(in-package #:compute-backend-podman/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (asdf:system-relative-pathname "compute-backend-podman" "examples/egress.lisp")))

(deftest egress-demo-runs
  (ok (eq t (compute-backend-podman/demo:run (make-broadcast-stream)))))
