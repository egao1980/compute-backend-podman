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
  ;; Host side is native (\src on Windows); container side stays POSIX.
  (ok (string= (format nil "~a:/dst:ro" (uiop:native-namestring #p"/src"))
               (compute-backend-podman:mount-volume-arg
                (list #p"/src" #p"/dst"))))
  (ok (string= (format nil "~a:/tmp/ws:ro" (uiop:native-namestring #p"/tmp/ws"))
               (compute-backend-podman:mount-volume-arg #p"/tmp/ws"))))

(defun %example-policy ()
  (compute-protocol:make-sandbox-network-policy
   :egress (list (compute-protocol:make-egress-rule :host "example.com" :port 443)
                 (compute-protocol:make-egress-rule :host "*" :port 80))))

(deftest podman-argv-policy-keeps-none-and-marker
  (let ((argv (compute-backend-podman:podman-argv
               (compute-protocol:make-sandbox-spec
                :command '("true")
                :runtime "alpine"
                :network (%example-policy)))))
    (ok (%has-seq argv '("--network" "none")))
    (ok (%has-seq argv '("--annotation" "compute-protocol.egress-proxy=1")))
    (ok (compute-backend-podman:podman-egress-proxy-needed-p
         (compute-protocol:make-sandbox-spec :network (%example-policy))))
    (ng (compute-backend-podman:podman-egress-proxy-needed-p
         (compute-protocol:make-sandbox-spec :network :none)))
    (ng (compute-backend-podman:podman-egress-proxy-needed-p
         (compute-protocol:make-sandbox-spec :network :allow)))))

(deftest assert-egress-allowed-allow
  (ok (compute-backend-podman:assert-egress-allowed
       (compute-protocol:make-sandbox-spec :network :allow)
       "evil.example" 22)))

(deftest assert-egress-allowed-policy-match
  (let ((spec (compute-protocol:make-sandbox-spec :network (%example-policy))))
    (ok (compute-backend-podman:assert-egress-allowed spec "example.com" 443))
    (ok (compute-backend-podman:assert-egress-allowed spec "anywhere.test" 80))))

(deftest assert-egress-allowed-deny
  (let ((spec (compute-protocol:make-sandbox-spec :network (%example-policy)))
        (none (compute-protocol:make-sandbox-spec :network :none)))
    (ok (signals (compute-backend-podman:assert-egress-allowed spec "example.com" 22)
                 'compute-protocol:sandbox-denied))
    (ok (signals (compute-backend-podman:assert-egress-allowed spec "other.example" 443)
                 'compute-protocol:sandbox-denied))
    (ok (signals (compute-backend-podman:assert-egress-allowed none "example.com" 443)
                 'compute-protocol:sandbox-denied))))
