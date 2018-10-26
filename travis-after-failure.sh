#!/bin/sh

echo -n "Shell="
(ls -l $(which bash))

test -f ./libgeda/tests/test-suite.log && cat ./libgeda/tests/test-suite.log

test -f .libgeda/scheme/test-suite.log && cat ./libgeda/scheme/test-suite.log

test -f ./libgedacolor/tests/test-suite.log && cat ./libgedacolor/tests/test-suite.log

test -f ./libgedathon/tests/test-suite.log && cat libgedathon/tests/test-suite.log
test -f ./libgedathon/tests/run_tests.sh.log && cat ./libgedathon/tests/run_tests.sh.log

test -f ./libgedauio/tests/test-suite.log && cat ./libgedauio/tests/test-suite.log

test -f ./gschem/tests/test-suite.log && cat ./gschem/tests/test-suite.log

# Examples

test -f ./examples/CascodeAmp/test-suite.log && cat ./examples/CascodeAmp/test-suite.log

test -f ./examples/gTAG/test-suite.log && cat ./examples/gTAG/test-suite.log

test -f ./examples/lightning_detector/test-suite.log && cat ./examples/lightning_detector/test-suite.log

test -f ./examples/MSA-2643/test-suite.log && cat ./examples/MSA-2643/test-suite.log

test -f ./examples/TwoStageAmp/test-suite.log && cat ./examples/TwoStageAmp/test-suite.log
