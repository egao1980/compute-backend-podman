;;;; Offline egress-proxy demo — assert-egress-allowed without running Podman.
;;;; CNI does not enforce host/port; a sidecar must call this helper.
;;;;   sbcl --load examples/egress.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :compute-backend-podman)
    (require :asdf)
    (asdf:load-system "compute-backend-podman")))

(defpackage #:compute-backend-podman/demo
  (:use #:cl)
  (:local-nicknames (#:compute #:compute-protocol)
                    (#:podman #:compute-backend-podman))
  (:export #:run))

(in-package #:compute-backend-podman/demo)

(defun run (&optional (stream *standard-output*))
  "Match example.com:443, deny :22. Returns T on the allowed check."
  (let* ((spec (compute:make-sandbox-spec
                :command '("echo" "hi")
                :network (compute:make-sandbox-network-policy
                          :egress (list (compute:make-egress-rule
                                         :host "example.com" :port 443)))))
         (argv (podman:podman-argv spec)))
    (format stream "~&; proxy-needed=~s argv has --network none=~s~%"
            (podman:podman-egress-proxy-needed-p spec)
            (search '("--network" "none") argv :test #'equal))
    (assert (podman:podman-egress-proxy-needed-p spec))
    (assert (search '("--network" "none") argv :test #'equal))
    (assert (podman:assert-egress-allowed spec "example.com" 443))
    (handler-case (podman:assert-egress-allowed spec "example.com" 22)
      (compute:sandbox-denied (c)
        (format stream "~&; denied example.com:22 policy=~s~%"
                (compute:sandbox-denied-policy c))
        (assert (eq :egress (compute:sandbox-denied-policy c)))))
    t))

#+sbcl
(when (and *load-truename*
           (equal (pathname-name *load-truename*) "egress")
           (find "examples/egress.lisp" sb-ext:*posix-argv* :test #'search))
  (run)
  (uiop:quit 0))
