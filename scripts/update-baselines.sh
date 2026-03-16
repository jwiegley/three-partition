#!/usr/bin/env bash
set -euo pipefail

echo "=== Updating coverage baseline ==="

OUTPUT=$(cabal test --enable-coverage --test-show-details=direct 2>&1) || true

COVERAGE=$(echo "$OUTPUT" \
  | grep -oE '[0-9]+% expressions used' \
  | head -1 \
  | grep -oE '[0-9]+' || echo "0")

if [ "$COVERAGE" -gt 0 ]; then
  echo "$COVERAGE" > .coverage-baseline
  echo "Coverage baseline set to ${COVERAGE}%"
else
  echo "Warning: could not extract coverage, skipping baseline update"
fi

echo ""
echo "=== Updating performance baseline ==="

BENCH_OUTPUT=$(cabal bench --benchmark-options="--csv /dev/stdout" 2>/dev/null) || true

BENCH_NS=$(echo "$BENCH_OUTPUT" \
  | tail -n +2 \
  | head -1 \
  | cut -d',' -f2 || echo "0")

if [ "$BENCH_NS" != "0" ] && [ -n "$BENCH_NS" ]; then
  echo "$BENCH_NS" > .perf-baseline
  echo "Performance baseline set to ${BENCH_NS}ns"
else
  echo "Warning: could not extract benchmark time, skipping baseline update"
fi

echo ""
echo "Done. Commit .coverage-baseline and .perf-baseline to track baselines."
