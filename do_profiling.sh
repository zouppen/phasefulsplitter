#!/bin/bash -eu

# Set up.
SECONDS=$1
RTS="-p -L40"
PLOT="-c"

shift #seconds out of argv

# Compile.
(cd src && ghc -threaded --make -O2 -prof -auto-all -caf-all -o parameter_analyzer ParameterAnalyzer.hs -fforce-recomp)

# Run.
echo  "Running analysis for $SECONDS secs..."
./prof-hc "$@" +RTS -hc $RTS &
./prof-hy "$@" +RTS -hy $RTS &
./prof-hd "$@" +RTS -hd $RTS &

# Sleep.
sleep "$SECONDS"

# Kill and wait.
set +E
kill -INT %1
kill -INT %2
kill -INT %3
wait
set -E

# Profit.
echo "Generating graphs..."
hp2ps $PLOT prof-hc.hp &
hp2ps $PLOT prof-hy.hp &
hp2ps $PLOT prof-hd.hp &
wait
cat prof-*.ps >prof.ps
ps2pdf prof.ps
echo "Graphs are generated."
