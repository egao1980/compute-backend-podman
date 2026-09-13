(in-package #:compute-backend-podman/tests)

(defun %has-seq (argv tokens)
  (search tokens argv :test #'equal))

(deftest podman-argv-network-none-and-ro-mounts
  (let* ((spec (compute-protocol:make-sandbox-spec
                :command '("echo" "hi")
                :runtime "alpine:latest"
                :network :none
                :mounts (list #p"/tmp/ws")
                :memory-limit 134217728
                :cpu-limit 1
                :wall-clock 5
                :env '(("FOO" . "bar"))))
         (argv (compute-backend-podman:podman-argv spec)))
    (ok (equal "podman" (first argv)))
    (ok (equal "run" (second argv)))
    (ok (%has-seq argv '("--network" "none")))
    (ok (find "-v" argv :test #'string=))
    (ok (some (lambda (s) (and (stringp s) (search ":ro" s))) argv))
    (ok (some (lambda (s) (and (stringp s) (search "/tmp/ws" s))) argv))
    (ok (%has-seq argv '("--memory" "134217728")))
    (ok (%has-seq argv '("--cpus" "1")))
    (ok (%has-seq argv '("--timeout" "5")))
    (ok (find "alpine:latest" argv :test #'string=))
    (ok (%has-seq argv '("echo" "hi")))))

(deftest podman-argv-network-allow-omits-none
  (let ((argv (compute-backend-podman:podman-argv
               (compute-protocol:make-sandbox-spec
                :command '("true")
                :runtime "alpine"
                :network :allow))))
    (ng (%has-seq argv '("--network" "none")))))

(deftest mount-volume-arg-pairs
  (ok (string= "/src:/dst:ro"
               (compute-backend-podman:mount-volume-arg
                (list #p"/src" #p"/dst")))))
