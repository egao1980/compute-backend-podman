(in-package #:compute-backend-podman)

(defclass podman-compute-backend (compute-protocol:compute-backend)
  ((default-image :initarg :default-image :accessor podman-default-image
                  :initform "sbcl"))
  (:documentation
   "Rootless podman backend. Builds `podman run` argv; drives it via
process-protocol when *process-backend* is bound, else UIOP."))

(defun make-podman-compute-backend (&key (default-image "sbcl"))
  (make-instance 'podman-compute-backend :default-image default-image))

(defun use-podman-compute-backend (&rest args)
  (setf compute-protocol:*compute-backend*
        (apply #'make-podman-compute-backend args)))

(defun %namestring (x)
  (etypecase x
    (string x)
    (pathname (uiop:native-namestring x))
    (symbol (string x))))

(defun %env-name (key)
  (etypecase key
    (string key)
    (symbol (symbol-name key))))

(defun mount-volume-arg (mount)
  "Turn a mount designator into `src:dst:ro`."
  (cond
    ((or (pathnamep mount) (stringp mount))
     (let ((p (%namestring mount)))
       (format nil "~a:~a:ro" p p)))
    ((and (consp mount) (consp (cdr mount)) (null (cddr mount)))
     (format nil "~a:~a:ro" (%namestring (first mount)) (%namestring (second mount))))
    ((and (consp mount) (atom (cdr mount)))
     (format nil "~a:~a:ro" (%namestring (car mount)) (%namestring (cdr mount))))
    (t
     (let ((p (%namestring mount)))
       (format nil "~a:~a:ro" p p)))))

(defun %image (spec default-image)
  (let ((rt (compute-protocol:sandbox-spec-runtime spec)))
    (cond
      ((stringp rt) rt)
      (default-image default-image)
      (t "sbcl"))))

(defun podman-argv (spec &key default-image)
  "Build `podman run` argv for SPEC. Does not invoke podman."
  (let* ((spec (compute-protocol:coerce-sandbox-spec spec))
         (image (%image spec default-image))
         (acc '()))
    (flet ((add (x) (push x acc)))
      (add "podman")
      (add "run")
      (add "--rm")
      (unless (eq (compute-protocol:sandbox-spec-network spec) :allow)
        (add "--network")
        (add "none"))
      (let ((mem (compute-protocol:sandbox-spec-memory-limit spec)))
        (when mem
          (add "--memory")
          (add (princ-to-string mem))))
      (let ((cpu (compute-protocol:sandbox-spec-cpu-limit spec)))
        (when cpu
          (add "--cpus")
          (add (princ-to-string cpu))))
      (let ((wall (compute-protocol:sandbox-spec-wall-clock spec)))
        (when wall
          (add "--timeout")
          (add (princ-to-string (ceiling wall)))))
      (dolist (mount (compute-protocol:sandbox-spec-mounts spec))
        (add "-v")
        (add (mount-volume-arg mount)))
      (dolist (pair (compute-protocol:sandbox-spec-env spec))
        (add "-e")
        (add (format nil "~a=~a" (%env-name (car pair)) (cdr pair))))
      (add image)
      (dolist (arg (compute-protocol:sandbox-spec-command spec))
        (add (if (stringp arg) arg (princ-to-string arg))))
      (nreverse acc))))

(defun %process-protocol-run ()
  (let ((pkg (find-package '#:process-protocol)))
    (when pkg
      (let ((star (find-symbol "*PROCESS-BACKEND*" pkg))
            (run (find-symbol "RUN" pkg)))
        (when (and star run (boundp star) (symbol-value star) (fboundp run))
          run)))))

(defun %to-string (x)
  (cond
    ((stringp x) x)
    ((null x) "")
    ((and (vectorp x) (plusp (length x)) (integerp (aref x 0)))
     (map 'string #'code-char x))
    ((and (vectorp x) (or (zerop (length x)) (characterp (aref x 0))))
     (coerce x 'string))
    (t (princ-to-string x))))

(defun %invoke (argv)
  (let ((run (%process-protocol-run)))
    (if run
        (multiple-value-bind (code out err)
            (funcall run argv)
          (values code (%to-string out) (%to-string err)))
        (multiple-value-bind (out err code)
            (uiop:run-program argv
                              :ignore-error-status t
                              :output '(:string :stripped nil)
                              :error-output '(:string :stripped nil))
          (values (or code 0) (or out "") (or err ""))))))

(defmethod compute-protocol:run-sandboxed ((backend podman-compute-backend) spec)
  (let* ((spec (compute-protocol:coerce-sandbox-spec spec))
         (argv (podman-argv spec :default-image (podman-default-image backend))))
    (multiple-value-bind (code out err)
        (%invoke argv)
      (compute-protocol:make-sandbox-result
       :exit-code code
       :stdout out
       :stderr err
       :artifacts (compute-protocol:sandbox-spec-artifacts spec)))))
