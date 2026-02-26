#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

echo "[1/2] Running full pipeline..."
Rscript R/99_run/run_all.R

echo "[2/2] Running quality gates..."
python3 scripts/quality_gates.py --root "$ROOT"

echo "Done. Quality gates passed."
