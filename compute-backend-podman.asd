(defsystem "compute-backend-podman"
  :version "0.3.0"
  :description "Podman backend for compute-protocol (rootless podman run, warm sessions on podman exec, cp, build)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "compute-protocol" "0.4.0")
               (:version "process-protocol" "0.4.1")
               (:version "process-backend-uiop" "0.3.1"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "compute-backend-podman/tests"))))

(defsystem "compute-backend-podman/tests"
  :depends-on ("compute-backend-podman" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "session-test")      ; 0.3.0 argv: open / exec / rm / cp / build
               (:file "live-test")         ; gated on `podman info` + a local image
               (:file "demo-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
