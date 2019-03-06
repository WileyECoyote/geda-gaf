#!/bin/sh
#
# gEDA - GPL Electronic Design Automation
#
# Copyright (C) 2015-2016 Wiley Edward Hill <wileyhill@gmail.com>
# Copyright (C) 2015-2016 gEDA Contributors (see ChangeLog for details)
#
#-------------------------------------------------------------------
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write:
#
#            Free Software Foundation, Inc.
#            51 Franklin Street, Fifth Floor,
#            Boston, MA 02110-1301 USA
#
#-------------------------------------------------------------------
#
# Author: Wiley Edward Hill
# Date: 12/08/15
#
# Abstract: This scripts serves as a test for checking examples used
#           in the gEDA-gaf project. The script setups a  minimal
#           install environment using symbolic links in order to run
#           gnetlist, gsymcheck, and libgeda.
#           Recursively searches for symbol files and calls gsymcheck
#           for each sym file found under the current directory, if
#           gsymcheck returns an error the test fails.
#           Testing continues if all of the symbols pass, by running
#           gnetlist drc2 on the example file and then the GEDA net
#           list and BOM are extracted and compared to reference files
#           in the test subdirectory. After regenerating verify that
#           the generated BOM and NET results are correct.
#
VER=0.1.1

REGENERATE=false
DISTVBUILD=false

if [ "$1" = "-r" ] || [ "$1" = "--regen" ] ; then REGENERATE=true ; shift ; fi

if test ! -f ../../README ; then DISTVBUILD=true ; fi

BUILDDIR=$PWD

if [ -z $1 ] ; then
  schematic=${PWD##*/}
else
  schematic=$1
fi

if [ -z ${srcdir} ] ; then
  if [ -z $2 ] ; then
    SRCDIR=.
  else
    SRCDIR=$2
  fi
else
  SRCDIR=${srcdir}
fi

cd ${SRCDIR}

SRCDIR=$PWD

# ---------------------- Configuration constants -------------------

BOMBACKEND="bom2"
TMPGEDADIR="gEDA"

RPATH2LIBGEDA=${BUILDDIR}/../../libgeda/src/.libs
RPATH2LIBCAIRO=${BUILDDIR}/../../libgedacairo/src/.libs

CHECKSYM=gsymcheck
PATH2CHECKSYM=${BUILDDIR}/../../gsymcheck/src
SYMCHECKER=

CHECKNET=gnetlist
PATH2CHECKNET=${BUILDDIR}/../../gnetlist/src
NETLISTER=

# ----------------------- Functions constants ----------------------

# error exit handler when gnetlist is not found
do_exit_no_netlister ()
{
   echo "cannot check <${schematic}>, $CHECKNET is missing"
   echo "PATH2CHECKNET=$PATH2CHECKNET"
   exit 1;
}

# error exit handler when gsymcheck is not found
do_exit_no_symchecker ()
{
   echo "cannot check <${schematic}>, $CHECKSYM is missing"
   echo "PATH2CHECKSYM=$PATH2CHECKSYM"
   exit 1;
}

# checks for the built or installed gnetlist
do_get_netlister ()
{
   if [ -x $PATH2CHECKNET/$CHECKNET ] ; then
       NETLISTER=$PATH2CHECKNET/$CHECKNET
   else
       NETLISTER=$(which "${CHECKNET}")
   fi

   test ! -z ${NETLISTER} || do_exit_no_netlister
}

# checks for the built or installed gsymcheck
do_get_symbol_checker ()
{
   if [ -x $PATH2CHECKSYM/$CHECKSYM ] ; then
       SYMCHECKER=$PATH2CHECKSYM/$CHECKSYM
   else
       SYMCHECKER=$(which "${CHECKSYM}")
   fi

   test ! -z ${SYMCHECKER} || do_exit_no_symchecker
}

# Exports LD_LIBRARY_PATH= to the path of the in source build
# locations for libgeda and libgedacairo.
do_export_path2libraries()
{
   local CWDSAVE=$PWD
   local PATH2LIBGEDA
   local PATH2LIBCAIRO

   # Libgeda
   if [ -d $RPATH2LIBGEDA ] ; then
      cd $RPATH2LIBGEDA
      PATH2LIBGEDA=$PWD
      cd $CWDSAVE
   else
      echo "Error: not in the right place, cannot find libgeda"
      exit 0
   fi

   # Libgedacairo
   if [ -d $RPATH2LIBCAIRO ] ; then
      cd $RPATH2LIBCAIRO
      PATH2LIBCAIRO=$PWD
      cd $CWDSAVE
   else
      echo "Error: not in the right place, cannot find libgedacairo"
      exit 0
   fi

   export LD_LIBRARY_PATH=$PATH2LIBGEDA:$PATH2LIBCAIRO:$LD_LIBRARY_PATH

   test $VERBOSE && echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
}

# Creates a local gEDA directory and subdirectories and links to
# all pertinent files for gnetlist, libgeda, and symbol in this
# local gEDA directory. Exports GEDADATARC and GEDADATA both set
# this tmp directory. Specifically, links all files from:
#      ../../gnetlist/etc
#      ../../gnetlist/scheme
#      ../../libgeda/etc
#      ../../libgeda/scheme (including subdirectory: geda)
#      ../../libgeda/scheme/geda/core
#      ../../symbols
do_setup_geda_environment ()
{
  local CWDSAVE=$PWD

  cd ${BUILDDIR};

  # create temporary gEDA directory and required subdirs
  mkdir -m 777 -p ${TMPGEDADIR}
  rc=$?
  if test $rc -ne 0 ; then
     echo "Failed to create directory ${TMPGEDADIR}, check permissions"
     echo "mkdir returned $rc"
     exit 1
  fi

  cd ${TMPGEDADIR}
  export GEDADATARC=$PWD
  export GEDADATA=$PWD

  # Make all subdirectories under the gEDA directory
  mkdir -m 777 -p bitmaps
  mkdir -m 777 -p scheme/geda/core
  mkdir -m 777 -p scheme/gnetlist
  mkdir -m 777 -p gafrc.d

  test $VERBOSE && echo "GEDADATARC=$GEDADATARC"

  cd $CWDSAVE

  # Make links to rc files
  if [ -d $SRCDIR/../../gnetlist/etc ] ; then
     cd $SRCDIR/../../gnetlist/etc
     ln -s $PWD/system-* $GEDADATARC/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gnetlist etc directory"
    exit 0
  fi

  if [ -d $SRCDIR/../../libgeda/etc ] ; then
     cd $SRCDIR/../../libgeda/etc
     ln -s $PWD/system-gafrc $GEDADATARC/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda etc directory"
    exit 0
  fi

  # Make links to gnetlist scheme files in the source directory
  if [ -d $SRCDIR/../../gnetlist/scheme ] ; then
     cd $SRCDIR/../../gnetlist/scheme
     ln -s $PWD/*.scm $GEDADATARC/scheme/ 2>/dev/null
     ln -s $PWD/gnetlist/*.scm $GEDADATARC/scheme/gnetlist/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gnetlist scheme"
    exit 0
  fi

  # See libgedacolor in gschem/tests/runtest.sh if color maps are needed

  # Make links to gnetlist scheme files in the source directory
  if [ -d $SRCDIR/../../libgeda/scheme ] ; then
     cd $SRCDIR/../../libgeda/scheme
     ln -s $PWD/*.scm $GEDADATARC/scheme/ 2>/dev/null
     ln -s $PWD/geda/*.scm $GEDADATARC/scheme/geda/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda scheme src directory"
    exit 0
  fi

  # Make links to gnetlist scheme files in the build directory
  if [ -d $BUILDDIR/../../libgeda/scheme ] ; then
     cd $BUILDDIR/../../libgeda/scheme
     ln -s $PWD/geda/core/*.scm $GEDADATARC/scheme/geda/core/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda scheme build directory"
    exit 0
  fi

  # Make a link to geda-clib.scm and the symbols directory
  if [ -d $SRCDIR/../../symbols ] ; then
   cd $SRCDIR/../../symbols
   export SYMDIR=$PWD
   test $VERBOSE && echo "SYMDIR=$SYMDIR"
   cd $GEDADATARC/gafrc.d/
   ln -s $SYMDIR/geda-clib.scm geda-clib.scm 2>/dev/null
   cd ..
   ln -s $SYMDIR sym 2>/dev/null
   cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find symbols directory"
    exit 0
  fi

  cd $CWDSAVE
}

# Search example directory for sym files and gsymcheck all found
do_check_symbols ()
{
  find . -type f -iname '*.sym' -printf '%p\n' | xargs -0 | while IFS=' ' read sym;
  do
    test -z "$sym" && continue;
    test $VERBOSE && echo -n "Checking symbol: $(basename ${sym}) ..."
    $SYMCHECKER -q ${sym}
    if [ $? -ne 0 ] ; then
      echo "Failed ${sym}, see gsymcheck -v ${sym}"
      exit 1;
    fi
    test $VERBOSE && echo "passed"
  done

  return 0;
}

do_clean_up ()
{
  # Remove gEDA directory if not debugging
  test ! -z $DEBUG || rm -rf gEDA || : ;

  if $DISTVBUILD ; then
    rm -f "${BUILDDIR}/bom/${schematic}-bom.csv"
    rm -f "${BUILDDIR}/${schematic}-drc2.txt"
    rm -f "${BUILDDIR}/${schematic}-geda.net"
  fi;
}

# ------------------------------- Begin ----------------------------

if [ -z "${schematic}" ] ; then
  echo "Schematic not specified"
  exit 1;
fi

if [ ! -f ${schematic}.sch ] ; then
  echo "Are you sure? Did not find ${schematic}.sch"
  exit 1;
fi

if ! $REGENERATE ; then
  if [ ! -f "tests/${schematic}-geda.net" ] ; then
    echo "Reference is missing: tests/${schematic}-geda.net"
    exit 1;
  fi

  if [ ! -f "tests/${schematic}-bom.csv" ] ; then
    echo "Reference is missing: tests/${schematic}-bom.csv"
    exit 1;
  fi
  if test $VERBOSE ; then
    echo "Checking example ${schematic}"
    QV=v
  else
    QV=q
  fi
else
   test $VERBOSE && echo "Regenerating test results for example ${schematic}"
fi

test -z $DEBUG || set -x

do_export_path2libraries
do_setup_geda_environment

do_get_symbol_checker
do_get_netlister

if $REGENERATE ; then
    ${NETLISTER} -q -g ${BOMBACKEND} -o "tests/${schematic}-bom.csv" "${schematic}.sch"
    ${NETLISTER} -q -g geda -o "tests/${schematic}-geda.net" "${schematic}.sch"
    echo "Done!"
    exit 0;
fi

echo "Checking ${schematic}.sch"

do_check_symbols

${NETLISTER} -${QV} -g drc2 -o "${BUILDDIR}/${schematic}-drc2.txt" "${schematic}.sch"
test $? -eq 0 || exit 1;

# Note the next line exploits gnetlist ability to implicitly mkdir bom
${NETLISTER} -${QV} -g ${BOMBACKEND} -o "${BUILDDIR}/bom/${schematic}-bom.csv" "${schematic}.sch"
test $? -eq 0 || exit 1;

${NETLISTER} -${QV} -g geda -o "${BUILDDIR}/${schematic}-geda.net" "${schematic}.sch"
test $? -eq 0 || exit 1;

test -f "${BUILDDIR}/bom/${schematic}-bom.csv" || exit 1;

diff "tests/${schematic}-bom.csv" "${BUILDDIR}/bom/${schematic}-bom.csv"
test $? -eq 0 || exit 1;

test -f "${BUILDDIR}/${schematic}-geda.net" || exit 1;
diff "tests/${schematic}-geda.net" "${BUILDDIR}/${schematic}-geda.net"
result=$?;

do_clean_up

exit $result;
