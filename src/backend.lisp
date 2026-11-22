(in-package #:compute-backend-podman)

;;; Rootless podman backend. Every podman call goes through process-protocol
;;; (BACKEND's process backend, else *PROCESS-BACKEND*, else a private
;;; process-backend-uiop instance).
;;;
;;; One-shot: RUN-SANDBOXED backend spec → `podman run --rm …`.
;;; Warm session (0.3.0): OPEN-SANDBOX → `podman run -d … --entrypoint sleep
;;; IMAGE infinity`; RUN-SANDBOXED session spec → `podman exec`;
;;; CLOSE-SANDBOX → `podman rm -f`. Copy: `podman cp`. Build: `podman build`.

(defclass podman-compute-backend (compute-protocol:compute-backend)
  ((default-image :initarg :default-image :accessor podman-default-image
                  :initform "sbcl")
   (program :initarg :program :accessor podman-program :initform "podman"
            :documentation "The podman executable (name looked up on PATH, or a path).")
   (process-backend :initarg :process-backend :accessor podman-process-backend
                    :initform nil
                    :documentation "process-protocol backend for podman calls. NIL:
PROCESS-PROTOCOL:*PROCESS-BACKEND*, else a process-backend-uiop instance.")
   (kill-on-timeout :initarg :kill-on-timeout :accessor podman-kill-on-timeout
                    :initform t
                    :documentation "T: when a session command passes its wall clock, also
kill every process in the container except its init (`kill -s KILL -- -1`),
since killing the `podman exec` client leaves the command running."))
  (:documentation
   "Rootless podman backend. One-shot `podman run --rm`, warm sessions on
`podman exec`, `podman cp`, `podman build`. Every call goes through
process-protocol."))

(defun make-podman-compute-backend (&key (default-image "sbcl") (program "podman")
                                      process-backend (kill-on-timeout t))
  (make-instance 'podman-compute-backend :default-image default-image
                                         :program program
                                         :process-backend process-backend
                                         :kill-on-timeout kill-on-timeout))

(defun use-podman-compute-backend (&rest args)
  (setf compute-protocol:*compute-backend*
        (apply #'make-podman-compute-backend args)))

;;; ------------------------------------------------------------------
;;; Designators

(defun %namestring (x)
  (etypecase x
    (string x)
    (pathname (uiop:native-namestring x))
    (symbol (string x))))

(defun %env-name (key)
  (etypecase key
    (string key)
    (symbol (symbol-name key))))

(defun %container-path (x)
  "Container-side path: always POSIX, whatever the host (podman on Windows takes
   a native host path but the destination is inside a Linux VM)."
  (etypecase x
    (string x)
    (pathname (namestring x))
    (symbol (string x))))

(defun %trim-separator (path)
  "PATH without a trailing / or \\ (a directory pathname's), except a root."
  (let ((trimmed (string-right-trim "/\\" path)))
    (if (or (zerop (length trimmed)) (char= (char trimmed (1- (length trimmed))) #\:))
        path
        trimmed)))

(defun mount-volume-arg (mount)
  "Turn a mount designator (see COMPUTE-PROTOCOL:PARSE-MOUNT) into
`src:dst:ro` or `src:dst:rw` — host-native src, POSIX dst."
  (multiple-value-bind (src dst mode) (compute-protocol:parse-mount mount)
    (format nil "~a:~a:~(~a~)" (%trim-separator (%namestring src))
            (%trim-separator (%container-path dst)) mode)))

(defun %image (spec default-image)
  (let ((rt (compute-protocol:sandbox-spec-runtime spec)))
    (cond
      ((stringp rt) rt)
      (default-image default-image)
      (t "sbcl"))))

(defun %command-strings (spec)
  (mapcar (lambda (arg) (if (stringp arg) arg (princ-to-string arg)))
          (compute-protocol:sandbox-spec-command spec)))

(defun %env-args (spec)
  (loop for (k . v) in (compute-protocol:sandbox-spec-env spec)
        append (list "-e" (format nil "~a=~a" (%env-name k) v))))

(defun %workdir-args (spec)
  (let ((dir (compute-protocol:sandbox-spec-directory spec)))
    (when dir (list "-w" (%container-path dir)))))

(defun %input-args (spec)
  (when (compute-protocol:sandbox-spec-input spec) (list "-i")))

;;; ------------------------------------------------------------------
;;; Egress

(defun podman-egress-proxy-needed-p (spec)
  "T when SPEC carries a SANDBOX-NETWORK-POLICY (sidecar proxy required).
CNI / `--network none` do not enforce host/port."
  (compute-protocol:sandbox-network-policy-p
   (compute-protocol:sandbox-spec-network
    (compute-protocol:coerce-sandbox-spec spec))))

(defun assert-egress-allowed (spec host port)
  "Signal COMPUTE-PROTOCOL:SANDBOX-DENIED unless SPEC allows HOST:PORT.

:allow permits any destination. A SANDBOX-NETWORK-POLICY matches when an
egress rule has host equal to HOST or \"*\" and port equal to PORT.
:none and unmatched rules are denied. CNI does not enforce this — a
sidecar proxy must call this helper before opening a connection."
  (check-type host string)
  (check-type port integer)
  (let* ((spec (compute-protocol:coerce-sandbox-spec spec))
         (network (compute-protocol:sandbox-spec-network spec)))
    (cond
      ((eq network :allow)
       t)
      ((compute-protocol:sandbox-network-policy-p network)
       (unless (some (lambda (rule)
                       (and (or (string= (compute-protocol:egress-rule-host rule) host)
                                (string= (compute-protocol:egress-rule-host rule) "*"))
                            (= (compute-protocol:egress-rule-port rule) port)))
                     (compute-protocol:sandbox-network-policy-egress network))
         (error 'compute-protocol:sandbox-denied
                :spec spec
                :policy :egress
                :message (format nil "egress to ~a:~a is not allowed" host port)))
       t)
      (t
       (error 'compute-protocol:sandbox-denied
              :spec spec
              :policy :egress
              :message (format nil "egress to ~a:~a is not allowed" host port))))))

;;; ------------------------------------------------------------------
;;; argv builders (pure: they never run podman)

(defun %container-options (spec)
  "Options shared by `podman run` (one-shot and warm): network, limits,
mounts, env, working directory."
  (let ((network (compute-protocol:sandbox-spec-network spec))
        (mem (compute-protocol:sandbox-spec-memory-limit spec))
        (cpu (compute-protocol:sandbox-spec-cpu-limit spec)))
    (append
     (unless (eq network :allow) (list "--network" "none"))
     (when (compute-protocol:sandbox-network-policy-p network)
       (list "--annotation" "compute-protocol.egress-proxy=1"))
     (when mem (list "--memory" (princ-to-string mem)))
     (when cpu (list "--cpus" (princ-to-string cpu))))))

(defun %mount-args (spec)
  (loop for mount in (compute-protocol:sandbox-spec-mounts spec)
        append (list "-v" (mount-volume-arg mount))))

(defun podman-argv (spec &key default-image (program "podman"))
  "Build the one-shot `podman run --rm` argv for SPEC. Does not invoke podman.

:none and a SANDBOX-NETWORK-POLICY start with `--network none`. :allow
omits that flag. A policy also adds annotation
`compute-protocol.egress-proxy=1` as a marker for a sidecar that must
call ASSERT-EGRESS-ALLOWED. Podman/CNI do not filter host/port.
Mounts are `-v src:dst:ro|rw`; :DIRECTORY → `-w`; :INPUT → `-i`."
  (let ((spec (compute-protocol:coerce-sandbox-spec spec)))
    (append
     (list program "run" "--rm")
     (%input-args spec)
     (%container-options spec)
     (let ((wall (compute-protocol:sandbox-spec-wall-clock spec)))
       (when wall (list "--timeout" (princ-to-string (ceiling wall)))))
     (%mount-args spec)
     (%env-args spec)
     (%workdir-args spec)
     (list (%image spec default-image))
     (%command-strings spec))))

(defun podman-open-argv (spec &key default-image name (program "podman"))
  "Build the `podman run -d` argv that starts a warm-session container for SPEC:
SPEC's network, limits, mounts, env and :DIRECTORY (`-w`), entrypoint
`sleep infinity` so the container idles until `podman rm`. NAME → `--name`.
SPEC's command and wall clock are not used here (each RUN-SANDBOXED on the
session brings its own)."
  (let ((spec (compute-protocol:coerce-sandbox-spec spec)))
    (append
     (list program "run" "-d" "--rm")
     (when name (list "--name" name))
     (%container-options spec)
     (%mount-args spec)
     (%env-args spec)
     (%workdir-args spec)
     (list "--entrypoint" "sleep" (%image spec default-image) "infinity"))))

(defun podman-exec-argv (container spec &key (program "podman"))
  "Build the `podman exec` argv running SPEC's command in CONTAINER (id or
name): :INPUT → `-i`, :DIRECTORY → `-w`, :ENV → `-e K=V`."
  (let ((spec (compute-protocol:coerce-sandbox-spec spec)))
    (unless (compute-protocol:sandbox-spec-command spec)
      (error 'compute-protocol:compute-error
             :spec spec :message "podman exec needs :command"))
    (append
     (list program "exec")
     (%input-args spec)
     (%workdir-args spec)
     (%env-args spec)
     (list container)
     (%command-strings spec))))

(defun podman-remove-argv (container &key (program "podman"))
  "`podman rm -f -t 0 CONTAINER`: stop at once (the idle `sleep` ignores
SIGTERM) and remove."
  (list program "rm" "-f" "-t" "0" container))

(defun podman-kill-argv (container &key (program "podman"))
  "`podman exec CONTAINER sh -c 'kill -s KILL -- -1'`: kill every process in
CONTAINER except its init (the idle `sleep`)."
  (list program "exec" container "sh" "-c" "kill -s KILL -- -1 2>/dev/null; exit 0"))

(defun podman-copy-argv (source destination &key (program "podman"))
  "`podman cp SOURCE DESTINATION`. A container side is `ID:PATH`."
  (list program "cp" source destination))

(defun podman-build-argv (context-dir &key tag dockerfile (program "podman"))
  "`podman build -t TAG [-f DOCKERFILE] CONTEXT-DIR`."
  (append (list program "build")
          (when tag (list "-t" tag))
          (when dockerfile (list "-f" (%namestring dockerfile)))
          (list (string-right-trim "/\\" (%namestring context-dir)))))

;;; ------------------------------------------------------------------
;;; Running podman through process-protocol

(defvar *fallback-process-backend* nil)

(defun %process-backend (backend)
  (or (and backend (podman-process-backend backend))
      process-protocol:*process-backend*
      *fallback-process-backend*
      (setf *fallback-process-backend*
            (process-backend-uiop:make-uiop-process-backend))))

(defun %program (backend)
  (if backend (podman-program backend) "podman"))

(defun %run-podman (backend argv &key input timeout spec)
  "Run ARGV → (values exit-code stdout stderr), or :TIMEOUT as the exit code
when TIMEOUT seconds pass (the podman client is killed). A podman that cannot
be started → COMPUTE-PROTOCOL:COMPUTE-ERROR."
  (handler-case
      (process-protocol:run argv
                            :backend (%process-backend backend)
                            :input input
                            :output :string
                            :error-output :string
                            :timeout timeout)
    (process-protocol:process-timeout-error ()
      (values :timeout "" ""))
    (process-protocol:process-error (c)
      (error 'compute-protocol:compute-error
             :spec spec
             :message (format nil "cannot run ~a: ~a" (first argv) c)))))

(defun %check-ok (argv code out err what &optional spec)
  (unless (eql code 0)
    (error 'compute-protocol:compute-error
           :spec spec
           :message (format nil "~a failed (exit ~a): ~{~a~^ ~}~@[~%~a~]"
                            what code argv
                            (let ((e (string-trim '(#\Space #\Newline #\Return) err)))
                              (if (plusp (length e)) e
                                  (let ((o (string-trim '(#\Space #\Newline #\Return) out)))
                                    (and (plusp (length o)) o)))))))
  out)

(defun %run-with-wall-clock (backend spec argv &key on-timeout)
  "Run ARGV under SPEC's wall clock → SANDBOX-RESULT. Past it: ON-TIMEOUT,
then COMPUTE-PROTOCOL:SANDBOX-TIMEOUT with restarts EXTEND-LIMIT (run again
with more seconds), ABORT-EXECUTION and ABORT (→ NIL)."
  (let ((limit (compute-protocol:sandbox-spec-wall-clock spec))
        (input (compute-protocol:sandbox-spec-input spec)))
    (loop
      (multiple-value-bind (code out err)
          (%run-podman backend argv :input input :timeout limit :spec spec)
        (unless (eq code :timeout)
          (return (compute-protocol:make-sandbox-result
                   :exit-code code
                   :stdout out
                   :stderr err
                   :artifacts (compute-protocol:sandbox-spec-artifacts spec))))
        (when on-timeout (funcall on-timeout))
        (let ((extra
                (restart-case
                    (error 'compute-protocol:sandbox-timeout
                           :spec spec
                           :limit limit
                           :message "wall-clock limit exceeded")
                  (compute-protocol:extend-limit (seconds)
                    :report "Extend the wall-clock limit and run again"
                    :interactive (lambda ()
                                   (format *query-io* "Additional seconds: ")
                                   (force-output *query-io*)
                                   (list (read *query-io*)))
                    seconds)
                  (compute-protocol:abort-execution ()
                    :report "Abort sandbox execution"
                    (return nil))
                  (abort ()
                    :report "Abort sandbox execution"
                    (return nil)))))
          (setf limit (+ (or limit 0) extra)))))))

(defun podman-available-p (&optional backend)
  "T when `podman info` succeeds. Never signals."
  (ignore-errors
   (eql 0 (%run-podman backend (list (%program backend) "info") :timeout 60))))

;;; ------------------------------------------------------------------
;;; One-shot

(defmethod compute-protocol:run-sandboxed ((backend podman-compute-backend) spec)
  (let* ((spec (compute-protocol:coerce-sandbox-spec spec))
         (argv (podman-argv spec :default-image (podman-default-image backend)
                                 :program (podman-program backend))))
    (multiple-value-bind (code out err)
        (%run-podman backend argv :input (compute-protocol:sandbox-spec-input spec)
                                  :spec spec)
      (compute-protocol:make-sandbox-result
       :exit-code code
       :stdout out
       :stderr err
       :artifacts (compute-protocol:sandbox-spec-artifacts spec)))))

;;; ------------------------------------------------------------------
;;; Warm sessions

(defclass podman-sandbox-session (compute-protocol:sandbox-session)
  ((container-id :initarg :container-id :reader podman-session-container-id)
   (name :initarg :name :reader podman-session-name)
   (closed-p :initform nil :accessor podman-session-closed-p))
  (:documentation "A warm session: one container, alive from OPEN-SANDBOX to
CLOSE-SANDBOX. RUN-SANDBOXED on it is `podman exec`."))

(defun %session-name ()
  (format nil "compute-~(~36r~)-~(~36r~)"
          (get-universal-time)
          (random (expt 36 6) (make-random-state t))))

(defmethod compute-protocol:open-sandbox ((backend podman-compute-backend) spec)
  (let* ((spec (compute-protocol:coerce-sandbox-spec spec))
         (name (%session-name))
         (argv (podman-open-argv spec :default-image (podman-default-image backend)
                                      :name name
                                      :program (podman-program backend))))
    (multiple-value-bind (code out err) (%run-podman backend argv :spec spec)
      (%check-ok argv code out err "podman run -d" spec)
      (let ((id (string-trim '(#\Space #\Newline #\Return #\Tab) out)))
        (make-instance 'podman-sandbox-session
                       :backend backend
                       :spec spec
                       :container-id (if (plusp (length id)) id name)
                       :name name)))))

(defun %open-session (session)
  (when (podman-session-closed-p session)
    (error 'compute-protocol:compute-error
           :spec (compute-protocol:sandbox-session-spec session)
           :message (format nil "podman session ~a is closed"
                            (podman-session-name session))))
  session)

(defmethod compute-protocol:run-sandboxed ((session podman-sandbox-session) spec)
  (%open-session session)
  (let* ((backend (compute-protocol:sandbox-session-backend session))
         (spec (if spec
                   (compute-protocol:coerce-sandbox-spec spec)
                   (compute-protocol:sandbox-session-spec session)))
         (id (podman-session-container-id session))
         (program (podman-program backend))
         (argv (podman-exec-argv id spec :program program)))
    (%run-with-wall-clock
     backend spec argv
     :on-timeout (lambda ()
                   (when (podman-kill-on-timeout backend)
                     (ignore-errors
                      (%run-podman backend (podman-kill-argv id :program program)
                                   :timeout 30)))))))

(defmethod compute-protocol:close-sandbox ((session podman-sandbox-session))
  "`podman rm -f`. Idempotent; never signals."
  (unless (podman-session-closed-p session)
    (setf (podman-session-closed-p session) t)
    (let ((backend (compute-protocol:sandbox-session-backend session)))
      (ignore-errors
       (%run-podman backend (podman-remove-argv (podman-session-container-id session)
                                                :program (podman-program backend))
                    :timeout 120))))
  nil)

;;; ------------------------------------------------------------------
;;; Copy

(defmethod compute-protocol:sandbox-copy-to ((session podman-sandbox-session)
                                            host-path sandbox-path)
  (%open-session session)
  (let* ((backend (compute-protocol:sandbox-session-backend session))
         (argv (podman-copy-argv (%namestring host-path)
                                 (format nil "~a:~a" (podman-session-container-id session)
                                         (%container-path sandbox-path))
                                 :program (podman-program backend))))
    (multiple-value-bind (code out err) (%run-podman backend argv)
      (%check-ok argv code out err "podman cp"))
    sandbox-path))

(defmethod compute-protocol:sandbox-copy-from ((session podman-sandbox-session)
                                              sandbox-path host-path)
  (%open-session session)
  (let* ((backend (compute-protocol:sandbox-session-backend session))
         (argv (podman-copy-argv (format nil "~a:~a" (podman-session-container-id session)
                                         (%container-path sandbox-path))
                                 (%namestring host-path)
                                 :program (podman-program backend))))
    (multiple-value-bind (code out err) (%run-podman backend argv)
      (%check-ok argv code out err "podman cp"))
    host-path))

(defun podman-copy-to (session host-path container-path)
  "Copy HOST-PATH into SESSION's container at CONTAINER-PATH (`podman cp`).
→ CONTAINER-PATH. Same as COMPUTE-PROTOCOL:SANDBOX-COPY-TO."
  (compute-protocol:sandbox-copy-to session host-path container-path))

(defun podman-copy-from (session container-path host-path)
  "Copy CONTAINER-PATH out of SESSION's container to HOST-PATH (`podman cp`).
→ HOST-PATH. Same as COMPUTE-PROTOCOL:SANDBOX-COPY-FROM."
  (compute-protocol:sandbox-copy-from session container-path host-path))

;;; ------------------------------------------------------------------
;;; Build

(defun podman-build-image (context-dir &key tag dockerfile backend timeout)
  "Build an image from CONTEXT-DIR (its Dockerfile, or DOCKERFILE) with
`podman build -t TAG` → TAG. TAG defaults to a fresh
\"localhost/compute-build:<random>\". BACKEND (a PODMAN-COMPUTE-BACKEND)
gives the program and process backend. A failed build →
COMPUTE-PROTOCOL:COMPUTE-ERROR with podman's output; TIMEOUT seconds →
COMPUTE-PROTOCOL:SANDBOX-TIMEOUT."
  (let* ((tag (or tag (format nil "localhost/compute-build:~(~36r~)"
                              (random (expt 36 8) (make-random-state t)))))
         (argv (podman-build-argv context-dir :tag tag :dockerfile dockerfile
                                              :program (%program backend))))
    (multiple-value-bind (code out err) (%run-podman backend argv :timeout timeout)
      (when (eq code :timeout)
        (error 'compute-protocol:sandbox-timeout
               :limit timeout :message "podman build did not finish"))
      (%check-ok argv code out err "podman build"))
    tag))
