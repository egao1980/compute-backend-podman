(in-package #:compute-backend-podman/tests)

;;; Live lane: real podman. GATED — runs only on Unix when `podman info`
;;; succeeds and the test image is present locally (never pulls). Else every
;;; test is skipped with the reason, unless COMPUTE_PODMAN_LIVE=require (CI's
;;; podman job), which turns the reason into a failure.
;;; COMPUTE_PODMAN_TEST_IMAGE overrides the image.

(defun %live-image ()
  (or (uiop:getenv "COMPUTE_PODMAN_TEST_IMAGE") "docker.io/library/alpine:3.20"))

(defun %live-backend ()
  (compute-backend-podman:make-podman-compute-backend :default-image (%live-image)))

(defun %podman (&rest args)
  "Run podman ARGS through process-protocol → exit code."
  (values (ignore-errors
           (process-protocol:run (cons "podman" args)
                                 :backend (process-backend-uiop:make-uiop-process-backend)
                                 :output nil :error-output nil :timeout 120))))

(defvar *live-blocker* :unknown)

(defun %live-blocker ()
  (when (eq *live-blocker* :unknown)
    (setf *live-blocker*
          (cond ((not (uiop:os-unix-p)) "not a Unix host")
                ((not (compute-backend-podman:podman-available-p))
                 "podman info failed (no podman, machine or service)")
                ((not (eql 0 (%podman "image" "exists" (%live-image))))
                 (format nil "image ~a is not present locally" (%live-image)))
                (t nil))))
  *live-blocker*)

(defmacro live-lane (&body body)
  `(let ((blocker (%live-blocker)))
     (cond ((null blocker) ,@body)
           ((equal (uiop:getenv "COMPUTE_PODMAN_LIVE") "require")
            (ok nil (format nil "live lane required but blocked: ~a" blocker)))
           (t (skip (format nil "podman live lane: ~a" blocker))))))

(defun %live-tmp ()
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "podman-live-~(~36r~)/"
                                       (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defun %out (result) (compute-protocol:sandbox-result-stdout result))
(defun %code (result) (compute-protocol:sandbox-result-exit-code result))

(deftest live-one-shot-run-with-input
  (live-lane
   (let ((r (compute-protocol:run-sandboxed
             (%live-backend)
             (compute-protocol:make-sandbox-spec
              :command '("sh" "-c" "cat; pwd") :input (format nil "hi~%")
              :directory "/tmp"))))
     (ok (eql 0 (%code r)))
     (ok (equal (format nil "hi~%/tmp~%") (%out r))))))

(deftest live-warm-session
  (live-lane
   (let* ((host (%live-tmp))
          (ro (%live-tmp))
          (session (compute-protocol:open-sandbox
                    (%live-backend)
                    (compute-protocol:make-sandbox-spec
                     :mounts (list (list host "/logs" :rw) (list ro "/ro"))
                     :env '(("FROM_OPEN" . "yes"))
                     :directory "/"))))
     (unwind-protect
          (progn
            (ok (typep session 'compute-backend-podman:podman-sandbox-session))
            (testing "state survives between commands"
              (compute-protocol:run-sandboxed session '("sh" "-c" "echo kept > /tmp/state.txt"))
              (ok (equal (format nil "kept~%")
                         (%out (compute-protocol:run-sandboxed session '("cat" "/tmp/state.txt"))))))
            (testing ":directory, :env and :input on exec"
              (let ((r (compute-protocol:run-sandboxed
                        session
                        (compute-protocol:make-sandbox-spec
                         :command '("sh" "-c" "pwd; echo $FROM_OPEN $K; cat")
                         :directory "/tmp" :env '(("K" . "v")) :input "in"))))
                (ok (equal (format nil "/tmp~%yes v~%in") (%out r)))))
            (testing "exit code comes back"
              (ok (eql 3 (%code (compute-protocol:run-sandboxed session '("sh" "-c" "exit 3"))))))
            (testing "network :none leaves only lo"
              (ok (equal (format nil "lo~%")
                         (%out (compute-protocol:run-sandboxed session '("ls" "/sys/class/net"))))))
            (testing ":rw mount writes reach the host; :ro refuses"
              (compute-protocol:run-sandboxed session '("sh" "-c" "echo x > /logs/mark.txt"))
              (ok (probe-file (merge-pathnames "mark.txt" host)))
              (ng (eql 0 (%code (compute-protocol:run-sandboxed
                                 session '("sh" "-c" "echo x > /ro/no.txt"))))))
            (testing "copy in and out"
              (let ((in (merge-pathnames "in.txt" host))
                    (back (merge-pathnames "back.txt" ro)))
                (with-open-file (s in :direction :output :if-exists :supersede)
                  (write-string "copied" s))
                (compute-backend-podman:podman-copy-to session in "/tmp/in.txt")
                (ok (equal "copied" (%out (compute-protocol:run-sandboxed
                                           session '("cat" "/tmp/in.txt")))))
                (compute-protocol:sandbox-copy-from session "/tmp/in.txt" back)
                (ok (equal "copied" (uiop:read-file-string back)))
                (ok (signals (compute-protocol:sandbox-copy-from session "/no/such" back)
                             'compute-protocol:compute-error))))
            (testing "wall clock: sandbox-timeout, the command killed, session usable"
              (ok (signals (compute-protocol:run-sandboxed
                            session (compute-protocol:make-sandbox-spec
                                     :command '("sleep" "61") :wall-clock 1))
                           'compute-protocol:sandbox-timeout))
              (ok (null (handler-bind ((compute-protocol:sandbox-timeout
                                         (lambda (c) (declare (ignore c))
                                           (invoke-restart 'compute-protocol:abort-execution))))
                          (compute-protocol:run-sandboxed
                           session (compute-protocol:make-sandbox-spec
                                    :command '("sleep" "62") :wall-clock 1)))))
              (ng (search "sleep 6"
                          (%out (compute-protocol:run-sandboxed session '("ps" "-o" "args")))))
              (ok (eql 0 (%code (compute-protocol:run-sandboxed session '("true")))))))
       (compute-protocol:close-sandbox session)
       (uiop:delete-directory-tree host :validate t :if-does-not-exist :ignore)
       (uiop:delete-directory-tree ro :validate t :if-does-not-exist :ignore))
     (ok (compute-backend-podman:podman-session-closed-p session))
     (ok (null (compute-protocol:close-sandbox session)) "close is idempotent")
     (ng (eql 0 (%podman "container" "exists"
                         (compute-backend-podman:podman-session-container-id session)))
         "the container is gone"))))

(deftest live-with-sandbox-closes
  (live-lane
   (let (id)
     (compute-protocol:with-sandbox (s (%live-backend) '())
       (setf id (compute-backend-podman:podman-session-container-id s))
       (ok (eql 0 (%podman "container" "exists" id))))
     (ng (eql 0 (%podman "container" "exists" id))))))

(deftest live-build-image
  (live-lane
   (let* ((ctx (%live-tmp))
          (tag (format nil "localhost/compute-podman-test:~(~36r~)"
                       (random (expt 36 6) (make-random-state t)))))
     (with-open-file (s (merge-pathnames "Dockerfile" ctx) :direction :output)
       (format s "FROM ~a~%RUN echo built > /built.txt~%" (%live-image)))
     (unwind-protect
          (progn
            (ok (equal tag (compute-backend-podman:podman-build-image ctx :tag tag)))
            (ok (equal (format nil "built~%")
                       (%out (compute-protocol:run-sandboxed
                              (%live-backend)
                              (compute-protocol:make-sandbox-spec
                               :command '("cat" "/built.txt") :runtime tag)))))
            (with-open-file (s (merge-pathnames "Broken.Dockerfile" ctx) :direction :output)
              (format s "FROM ~a~%RUN exit 7~%" (%live-image)))
            (ok (signals (compute-backend-podman:podman-build-image
                          ctx :dockerfile (merge-pathnames "Broken.Dockerfile" ctx))
                         'compute-protocol:compute-error)))
       (%podman "rmi" "-f" tag)
       (uiop:delete-directory-tree ctx :validate t :if-does-not-exist :ignore)))))
