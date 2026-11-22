(in-package #:compute-backend-podman/tests)

;;; 0.3.0 argv (egao1980/compute-backend-podman#3) — podman not needed.

(defun %spec (&rest keys) (apply #'compute-protocol:make-sandbox-spec keys))

(deftest mount-volume-arg-modes
  (ok (string= "/src:/dst:rw"
               (compute-backend-podman:mount-volume-arg '("/src" "/dst" :rw))))
  (ok (string= "/src:/dst:ro"
               (compute-backend-podman:mount-volume-arg '("/src" "/dst" :ro))))
  (ok (string= "/src:/dst:ro"
               (compute-backend-podman:mount-volume-arg '("/src" . "/dst"))))
  (ok (signals (compute-backend-podman:mount-volume-arg '("/src" "/dst" :exec))
               'compute-protocol:compute-error)))

(deftest podman-argv-rw-mount-directory-input
  (let ((argv (compute-backend-podman:podman-argv
               (%spec :command '("cat") :runtime "alpine"
                      :mounts '(("/h" "/logs" :rw))
                      :directory "/app" :input "hi"))))
    (ok (equal '("podman" "run" "--rm" "-i") (subseq argv 0 4)))
    (ok (%has-seq argv '("-v" "/h:/logs:rw")))
    (ok (%has-seq argv '("-w" "/app" "alpine" "cat"))))
  (ng (find "-i" (compute-backend-podman:podman-argv (%spec :command '("true")))
            :test #'equal)))

(deftest podman-open-argv-warm-container
  (let ((argv (compute-backend-podman:podman-open-argv
               (%spec :command '("ignored") :runtime "alpine:3.20"
                      :network :none :cpu-limit 2 :memory-limit 1048576
                      :wall-clock 9
                      :mounts '(("/h/logs" "/logs" :rw))
                      :env '(("A" . "1")) :directory "/app")
               :name "compute-x")))
    (ok (equal '("podman" "run" "-d" "--rm" "--name" "compute-x") (subseq argv 0 6)))
    (ok (%has-seq argv '("--network" "none")))
    (ok (%has-seq argv '("--cpus" "2")))
    (ok (%has-seq argv '("--memory" "1048576")))
    (ok (%has-seq argv '("-v" "/h/logs:/logs:rw")))
    (ok (%has-seq argv '("-e" "A=1")))
    (ok (%has-seq argv '("-w" "/app")))
    (ok (equal '("--entrypoint" "sleep" "alpine:3.20" "infinity") (last argv 4)))
    (ng (find "--timeout" argv :test #'equal))
    (ng (find "ignored" argv :test #'equal)))
  (testing "network :allow omits --network none; default image"
    (let ((argv (compute-backend-podman:podman-open-argv
                 (%spec :network :allow) :default-image "debian")))
      (ng (%has-seq argv '("--network" "none")))
      (ok (%has-seq argv '("sleep" "debian" "infinity"))))))

(deftest podman-exec-argv-shape
  (ok (equal '("podman" "exec" "-i" "-w" "/app" "-e" "K=v" "cid" "bash" "-c" "echo hi")
             (compute-backend-podman:podman-exec-argv
              "cid" (%spec :command '("bash" "-c" "echo hi") :input "x"
                           :directory "/app" :env '(("K" . "v"))))))
  (ok (equal '("podman" "exec" "cid" "true")
             (compute-backend-podman:podman-exec-argv "cid" '("true"))))
  (ok (equal '("/opt/podman" "exec" "cid" "true")
             (compute-backend-podman:podman-exec-argv "cid" '("true") :program "/opt/podman")))
  (ok (signals (compute-backend-podman:podman-exec-argv "cid" (%spec))
               'compute-protocol:compute-error)))

(deftest podman-remove-kill-copy-build-argv
  (ok (equal '("podman" "rm" "-f" "-t" "0" "cid")
             (compute-backend-podman:podman-remove-argv "cid")))
  (ok (equal '("podman" "exec" "cid" "sh" "-c")
             (subseq (compute-backend-podman:podman-kill-argv "cid") 0 5)))
  (ok (equal '("podman" "cp" "/h/f" "cid:/app/f")
             (compute-backend-podman:podman-copy-argv "/h/f" "cid:/app/f")))
  (ok (equal '("podman" "build" "-t" "t:1" "-f" "/ctx/Dockerfile.x" "/ctx")
             (compute-backend-podman:podman-build-argv
              "/ctx/" :tag "t:1" :dockerfile "/ctx/Dockerfile.x")))
  (ok (equal '("podman" "build" "/ctx")
             (compute-backend-podman:podman-build-argv "/ctx"))))

(deftest session-class-and-closed-session
  (let ((s (make-instance 'compute-backend-podman:podman-sandbox-session
                          :backend (compute-backend-podman:make-podman-compute-backend)
                          :spec (%spec)
                          :container-id "cid" :name "compute-x")))
    (ok (compute-protocol:sandbox-session-p s))
    (ok (equal "cid" (compute-backend-podman:podman-session-container-id s)))
    (setf (compute-backend-podman:podman-session-closed-p s) t)
    (ok (null (compute-protocol:close-sandbox s)) "close of a closed session is a no-op")
    (ok (signals (compute-protocol:run-sandboxed s '("true"))
                 'compute-protocol:compute-error))
    (ok (signals (compute-backend-podman:podman-copy-to s "/h" "/c")
                 'compute-protocol:compute-error))))

(deftest missing-podman-is-compute-error
  (let ((b (compute-backend-podman:make-podman-compute-backend
            :program "/nonexistent/podman-for-tests")))
    (ok (signals (compute-protocol:run-sandboxed b (%spec :command '("true") :runtime "x"))
                 'compute-protocol:compute-error))
    (ng (compute-backend-podman:podman-available-p b))))
