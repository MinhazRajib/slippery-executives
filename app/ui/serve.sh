#!/usr/bin/env bash
# Build the ExecLab client and serve it locally.
#
#   app/ui/serve.sh [port]     # default port 8742
#
# The app is fully client-side (market data is baked into the JS bundle),
# so any static file server works.
set -euo pipefail

port="${1:-8742}"
cd "$(dirname "$0")/../.."

dune build app/ui/client/main.bc.js

site=$(mktemp -d)
cp app/ui/client/index.html "$site"/
cp _build/default/app/ui/client/main.bc.js "$site"/
chmod u+w "$site"/main.bc.js

echo "ExecLab on http://localhost:$port"
exec python3 -m http.server -d "$site" "$port"
