(defpackage #:compute-backend-podman
  (:use #:cl)
  (:export #:podman-compute-backend
           #:podman-default-image
           #:make-podman-compute-backend
           #:use-podman-compute-backend
           #:podman-argv
           #:mount-volume-arg
           #:podman-egress-proxy-needed-p
           #:assert-egress-allowed))

(in-package #:compute-backend-podman)
