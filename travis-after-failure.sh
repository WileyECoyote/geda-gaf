#!/bin/sh

test -f ./libgeda/tests/test-suite.log && cat ./libgeda/tests/test-suite.log

test -f ./libgedathon/tests/test-suite.log && cat libgedathon/tests/test-suite.log
test -f ./libgedathon/tests/run_tests.sh.log && cat ./libgedathon/tests/run_tests.sh.log

test -f ./gschem/tests/test-suite.log && cat ./gschem/tests/test-suite.log