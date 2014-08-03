# $Id$
#

This directory contains a test scheme for libgedathon, the library is
excercised by executing scripts that created component symbols and a
schematic incorperating the newly created symbols. If a bug report is
filed, please add a test to show the bug has been fixed.

For now, the  resulting "lpfilter.sch" file is compared to the prebuilt
version. If the scripts exit with code 0, we passed. If the scripts exit
with any value other 0, we did not pass.

Use "make tester" to update the reference schematic.

Note: If "make tests" reports "ImportError: No module named geda" check
the PYTHONPATH environment variable. If you are root, then try as normal
user; the PYTHONPATH variable will not likely be exported when elevating
privileges





