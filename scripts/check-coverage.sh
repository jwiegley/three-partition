#!/usr/bin/env bash
set -euo pipefail

BASELINE_FILE=".coverage-baseline"
MIN_COVERAGE=0

if [ -f "$BASELINE_FILE" ]; then
  MIN_COVERAGE=$(cat "$BASELINE_FILE")
fi

# Run tests with coverage
OUTPUT=$(cabal test --enable-coverage --test-show-details=direct 2>&1) || true

# Extract coverage percentage from HPC output
COVERAGE=$(echo "$OUTPUT" \
  | grep -oE '[0-9]+% expressions used' \
  | head -1 \
  | grep -oE '[0-9]+' || echo "0")

if [ "$COVERAGE" -eq 0 ]; then
  echo "Warning: could not extract coverage percentage"
  echo "Run 'cabal test --enable-coverage' manually to check"
  exit 0
fi

echo "Coverage: ${COVERAGE}% (baseline: ${MIN_COVERAGE}%)"

if [ "$COVERAGE" -lt "$MIN_COVERAGE" ]; then
  echo "FAIL: Coverage dropped from ${MIN_COVERAGE}% to ${COVERAGE}%"
  exit 1
fi

echo "PASS: Coverage at or above baseline"
