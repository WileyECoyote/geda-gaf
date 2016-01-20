#!/bin/sh

test -f ./libgedathon/tests/test-suite.log && cat libgedathon/tests/test-suite.log
test -f ./libgedathon/tests/run_tests.sh.log && cat ./libgedathon/tests/run_tests.sh.log

