#!/usr/bin/env bash
# Build and serve the ExecLab client.
#
#   app/ui/serve.sh [port]     # default port 8081
#
# The app is fully client-side; the server only delivers files.
set -euo pipefail

port="${1:-8081}"
cd "$(dirname "$0")/../.."

dune build app/ui/main.bc.js bin/server.exe
exec dune exec bin/server.exe -- "$port"
