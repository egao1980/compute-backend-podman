# compute-backend-podman

Rootless **podman** backend for [`compute-protocol`](https://github.com/egao1980/compute-protocol).

Builds `podman run` argv (`--network none` unless `:allow`, `--memory`, `--cpus`, `-v src:dst:ro`, `--timeout`) and drives it via `process-protocol` when `*process-backend*` is bound, else UIOP.

A `sandbox-network-policy` still starts with `--network none` plus annotation `compute-protocol.egress-proxy=1`. **Podman/CNI do not enforce host/port.** A sidecar proxy must call `assert-egress-allowed` (host equal or `"*"`, port equal) before opening a connection; `:allow` permits any destination. `podman-egress-proxy-needed-p` is T when the spec carries a policy.

Unit tests cover the argv builder only — podman does not need to be installed.

```lisp
(asdf:load-system "compute-backend-podman")

(compute-backend-podman:podman-argv
 (stack-compute:make-sandbox-spec
  :command '("echo" "hi")
  :runtime "alpine:latest"
  :mounts '(#p"/tmp/ws")
  :network :none
  :memory-limit 134217728
  :wall-clock 5))
;; → ("podman" "run" "--rm" "--network" "none" "--memory" "134217728"
;;    "--timeout" "5" "-v" "/tmp/ws:/tmp/ws:ro" "alpine:latest" "echo" "hi")
```

Kata / firecracker runtime is a podman config, not a second backend.

## License

MIT
