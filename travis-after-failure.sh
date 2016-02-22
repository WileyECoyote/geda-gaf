#!/bin/sh

echo -n "Shell="
(ls -l $(which bash))

test -f ./libgeda/tests/test-suite.log && cat ./libgeda/tests/test-suite.log

test -f .libgeda/scheme/test-suite.log && cat ./libgeda/scheme/test-suite.log

test -f ./libgedacolor/tests/test-suite.log && cat ./libgedacolor/tests/test-suite.log

test -f ./libgedathon/tests/test-suite.log && cat libgedathon/tests/test-suite.log
test -f ./libgedathon/tests/run_tests.sh.log && cat ./libgedathon/tests/run_tests.sh.log

test -f ./gschem/tests/test-suite.log && cat ./gschem/tests/test-suite.log