#!/bin/bash
# post-merge.sh — Generate quality report after merging to main
# Triggered after a successful git merge.

PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TIMESTAMP=$(date +%Y-%m-%d)
BRANCH_NAME=$(git -C "$PROJECT_ROOT" branch --show-current 2>/dev/null || echo "unknown")
REPORT_DIR="${PROJECT_ROOT}/quality_reports/merges"

mkdir -p "$REPORT_DIR"

REPORT_FILE="${REPORT_DIR}/${TIMESTAMP}_${BRANCH_NAME}.md"

{
  echo "# Merge Quality Report"
  echo "- Date: ${TIMESTAMP}"
  echo "- Branch: ${BRANCH_NAME}"
  echo ""
  echo "## Files Changed"
  git -C "$PROJECT_ROOT" diff --stat HEAD~1 2>/dev/null || echo "(unable to diff)"
  echo ""
  echo "## Quality Gates"
  if [ -f "${PROJECT_ROOT}/scripts/quality_gates.py" ]; then
    python3 "${PROJECT_ROOT}/scripts/quality_gates.py" --root "$PROJECT_ROOT" 2>&1 || true
  else
    echo "(quality_gates.py not found)"
  fi
  echo ""
  echo "## Commit Log (this branch)"
  git -C "$PROJECT_ROOT" log --oneline -10 2>/dev/null
} > "$REPORT_FILE"

echo "Merge quality report saved to ${REPORT_FILE}"
