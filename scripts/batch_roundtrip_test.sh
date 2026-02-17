#!/bin/bash
# Batch round-trip test: decompile each .RUN, recompile, compare bytes.
# Usage: ./scripts/batch_roundtrip_test.sh [dir] [timeout_secs]
#   dir          - directory of .RUN files (default: blank/)
#   timeout_secs - max seconds per file (default: 3)

set -euo pipefail

DIR="${1:-blank}"
TIMEOUT="${2:-3}"
CLI="src/CoTAS.Cli/bin/Release/net10.0/CoTAS.Cli"
OUTDIR="test/roundtrip"

if [ ! -f "$CLI" ]; then
    echo "ERROR: Build first: dotnet build -c Release src/CoTAS.Cli/CoTAS.Cli.csproj"
    exit 1
fi

# Clean and create output dirs
rm -rf "$OUTDIR"
mkdir -p "$OUTDIR/src" "$OUTDIR/run"

# Count files
FILE_COUNT=$(ls "$DIR"/*.RUN 2>/dev/null | wc -l | tr -d ' ')

IDENTICAL=0
DIFFS=0
FAIL=0
TIMEDOUT=0
TOTAL=0

echo "=== Batch Round-Trip Test ==="
echo "Source:  $DIR/*.RUN ($FILE_COUNT files)"
echo "Output:  $OUTDIR/src/ and $OUTDIR/run/"
echo "Timeout: ${TIMEOUT}s per file"
echo ""

for f in "$DIR"/*.RUN; do
    [ -f "$f" ] || continue
    TOTAL=$((TOTAL + 1))
    name=$(basename "$f")
    base="${name%.RUN}"

    printf "\r[%3d/%d] %-20s" "$TOTAL" "$FILE_COUNT" "$name"

    # Decompile
    if ! timeout "$TIMEOUT" "$CLI" --decompile "$f" > "$OUTDIR/src/$base.SRC" 2>/dev/null; then
        FAIL=$((FAIL + 1))
        printf " FAIL (decompile)\n"
        continue
    fi

    # Compile
    if ! timeout "$TIMEOUT" "$CLI" --compile "$OUTDIR/src/$base.SRC" -o "$OUTDIR/run/$base.RUN" >/dev/null 2>&1; then
        rc=$?
        if [ $rc -eq 124 ]; then
            TIMEDOUT=$((TIMEDOUT + 1))
            printf " ⏰ TIMEOUT\n"
        else
            FAIL=$((FAIL + 1))
            printf " FAIL (compile)\n"
        fi
        continue
    fi

    # Compare
    if cmp -s "$f" "$OUTDIR/run/$base.RUN"; then
        IDENTICAL=$((IDENTICAL + 1))
    else
        DIFFS=$((DIFFS + 1))
        osz=$(wc -c < "$f" | tr -d ' ')
        csz=$(wc -c < "$OUTDIR/run/$base.RUN" | tr -d ' ')
        printf " ❌ %s→%s\n" "$osz" "$csz"
    fi
done

printf "\r%*s\r" 60 ""
echo ""
echo "=== RESULTS ==="
printf "  ✅ Identical:  %d\n" "$IDENTICAL"
printf "  ❌ Diffs:      %d\n" "$DIFFS"
printf "  ❌ Failed:     %d\n" "$FAIL"
printf "  ⏰ Timeout:    %d\n" "$TIMEDOUT"
printf "  📁 Total:      %d\n" "$TOTAL"

if [ "$TOTAL" -gt 0 ]; then
    pct=$((IDENTICAL * 100 / TOTAL))
    printf "  📊 Match rate: %d%%\n" "$pct"
fi

