(defsystem "compute-backend-podman"
  :version "0.2.0"
  :description "Podman backend for compute-protocol (rootless podman run argv)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("compute-protocol")
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
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
