#!/bin/bash
# pre-compact.sh — Save critical context before auto-compression
# This hook runs before Claude's context window is compacted.
# It ensures key decisions and state survive the compression.

PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TIMESTAMP=$(date +%Y-%m-%d_%H%M%S)
LOG_DIR="${PROJECT_ROOT}/quality_reports/session_logs"

mkdir -p "$LOG_DIR"

# Save a snapshot of recent git activity (last 10 commits + current diff)
{
  echo "# Context Snapshot — ${TIMESTAMP}"
  echo ""
  echo "## Recent Commits"
  git -C "$PROJECT_ROOT" log --oneline -10 2>/dev/null || echo "(not a git repo)"
  echo ""
  echo "## Current Changes"
  git -C "$PROJECT_ROOT" diff --stat 2>/dev/null || echo "(no changes)"
  echo ""
  echo "## Staged Changes"
  git -C "$PROJECT_ROOT" diff --cached --stat 2>/dev/null || echo "(nothing staged)"
} > "${LOG_DIR}/context_snapshot_${TIMESTAMP}.md"

echo "Context snapshot saved to quality_reports/session_logs/context_snapshot_${TIMESTAMP}.md"
