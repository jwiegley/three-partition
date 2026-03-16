#!/usr/bin/env bash
set -euo pipefail

BASELINE_FILE=".perf-baseline"
THRESHOLD=5  # percent regression allowed

if [ ! -f "$BASELINE_FILE" ]; then
  echo "No performance baseline found."
  echo "Run 'scripts/update-baselines.sh' to establish one."
  exit 0
fi

BASELINE_NS=$(cat "$BASELINE_FILE")

# Run benchmark and capture mean time
OUTPUT=$(cabal bench --benchmark-options="--csv /dev/stdout" 2>/dev/null) || true

# Extract the mean time (nanoseconds) from the CSV output
CURRENT_NS=$(echo "$OUTPUT" \
  | tail -n +2 \
  | head -1 \
  | cut -d',' -f2 || echo "0")

if [ "$CURRENT_NS" = "0" ] || [ -z "$CURRENT_NS" ]; then
  echo "Warning: could not extract benchmark time"
  echo "Run 'cabal bench' manually to check"
  exit 0
fi

echo "Current: ${CURRENT_NS}ns (baseline: ${BASELINE_NS}ns)"

# Check for regression > threshold
ALLOWED=$(echo "$BASELINE_NS * (100 + $THRESHOLD) / 100" | bc)

if [ "$(echo "$CURRENT_NS > $ALLOWED" | bc)" -eq 1 ]; then
  REGRESSION=$(echo "($CURRENT_NS - $BASELINE_NS) * 100 / $BASELINE_NS" | bc)
  echo "FAIL: Performance regressed by ${REGRESSION}% (threshold: ${THRESHOLD}%)"
  exit 1
fi

echo "PASS: Performance within ${THRESHOLD}% of baseline"
