# compute-backend-podman

Rootless **podman** backend for [`compute-protocol`](https://github.com/egao1980/compute-protocol).

Builds `podman run` argv (`--network none` unless `:allow`, `--memory`, `--cpus`, `-v src:dst:ro|rw`, `-w`, `-i`, `--timeout`) and runs every podman call through `process-protocol` (the backend's `:process-backend`, else `*process-backend*`, else a `process-backend-uiop` instance).

A `sandbox-network-policy` still starts with `--network none` plus annotation `compute-protocol.egress-proxy=1`. **Podman/CNI do not enforce host/port.** A sidecar proxy must call `assert-egress-allowed` (host equal or `"*"`, port equal) before opening a connection; `:allow` permits any destination. `podman-egress-proxy-needed-p` is T when the spec carries a policy.

## Warm sessions, copy, build (0.3.0)

Terminal-Bench semantics (agent-eval E0): one container lives across several commands.

- `open-sandbox backend spec` → `podman-sandbox-session`: `podman run -d --rm --name compute-… [--network none] [--cpus] [--memory] [-v …] [-e …] [-w DIR] --entrypoint sleep IMAGE infinity`.
- `run-sandboxed session spec` → `podman exec [-i] [-w DIR] [-e K=V …] ID argv…`. `:input` is piped to stdin; past `:wall-clock` → `sandbox-timeout` (`extend-limit` / `abort-execution` / `abort`), the exec client is killed and, with `:kill-on-timeout t` (default), every process in the container but its init (`kill -s KILL -- -1`).
- `close-sandbox session` → `podman rm -f -t 0 ID`; idempotent. `with-sandbox` closes on every exit.
- Mounts: `(src dst :rw)` → `src:dst:rw`; default `:ro` (`compute-protocol:parse-mount`).
- Copy: `podman-copy-to session host-path container-path` / `podman-copy-from session container-path host-path` (`podman cp`; also `compute-protocol:sandbox-copy-to` / `-from`). Failure → `compute-error` with podman's stderr.
- Build: `podman-build-image context-dir &key tag dockerfile backend timeout` → the tag (`podman build -t`).
- `podman-available-p` → T when `podman info` succeeds.

Argv builders (`podman-argv`, `podman-open-argv`, `podman-exec-argv`, `podman-remove-argv`, `podman-copy-argv`, `podman-build-argv`) never run podman; unit tests cover them without podman. The live lane (`tests/live-test.lisp`) runs only when `podman info` works and the image (`COMPUTE_PODMAN_TEST_IMAGE`, default `docker.io/library/alpine:3.20`) is present locally; it never pulls. CI's `test-podman-live` job (ubuntu VM) pulls the image and sets `COMPUTE_PODMAN_LIVE=require`, so there a blocked lane fails instead of skipping.

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

Offline demo (`assert-egress-allowed` + argv, no Podman process):

```bash
sbcl --load examples/egress.lisp
```

Kata / firecracker runtime is a podman config, not a second backend.

## License

MIT
