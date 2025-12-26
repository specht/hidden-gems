#!/usr/bin/env bash
set -euo pipefail

rsync -a --safe-links --no-owner --no-group --omit-dir-times \
      --chmod=Du=rwx,Fu=rwX /src/ /app/

source /home/runner/.cargo/env

cd /app
chmod +x start.sh
exec ./start.sh "$@"