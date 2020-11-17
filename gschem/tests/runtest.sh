#!/bin/bash

# Author: Wiley Edward Hill
# Date: 11/21/12
#
# Abstract: This scripts serves as a high-level script for executing
#           tests involving gschem, executing tests declared in the
#           schematic files and reporting the results. The script can
#           be invoked from the Makefile or from the command-line.

VER=0.1.9

ERR_FILE_NOT_FOUND=2
ERR_BAD_ARGS=65

APPLICATION=""
BUILDDIR="."
SRCDIR="."
ERRDIR=mismatched

#DEBUG=true
REGENERATE=false
#VERBOSE=true

TOTALTEST=0
PASSCOUNT=0
FAILCOUNT=0

PROGRAM="gschem"
TMPGEDADIR="gEDA"

# Show command line usage
show_help () {
   echo Usage:   `basename $0` '[-h] || [[-r || --regen] -v || --verbose] (no dir) input'
}

01_setup_path2libraries()
{
   mkdir -m 0775 -p "libs"

   LIBS=$(ls ../../libgeda*/src/.libs/*.so*)

   for link2lib in ${LIBS[*]} ; do
    if [ -L $link2lib ] ; then
       ln -s ../${link2lib} "libs/"
    fi
   done

   export LD_LIBRARY_PATH="$PWD/libs:$LD_LIBRARY_PATH"

   vecho "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
}

02_setup_geda_environment ()
{
  # Creates a local gEDA directory and subdirectories, copies all files
  # in ../etc and libgeda/etc into this directory, creates links to the
  # top-level BITMAPS, then creates a create scheme directory and copies
  # all files from:
  #      ../scheme
  #      ../scheme/gschem     (including subdirectory: gschem)
  #      ../../libgeda/scheme (including subdirectory: geda)

  local CWDSAVE=$PWD

  # create temporary gEDA directory and required subdirs
  mkdir -m 0777 -p ${TMPGEDADIR}
  rc=$?
  if test $rc -ne 0 ; then
     echo "Failed to create directory ${TMPGEDADIR} with appropriate permissions"
     echo "mkdir returned $rc"
     exit 1
  fi

  cd ${TMPGEDADIR}
  export GEDADATARC=$PWD
  export GEDADATA=$PWD

  # Make all subdirectories under the gEDA directory

  mkdir -m 0777 -p bitmaps/24x24
  mkdir -m 0777 -p bitmaps/26x26
  mkdir -m 0777 -p bitmaps/28x28
  mkdir -m 0777 -p scheme/geda
  mkdir -m 0777 -p scheme/gschem
  mkdir -m 0777 -p gafrc.d
  mkdir -m 0777 -p icons/hicolor/22x22/actions

  vecho "GEDADATARC=$GEDADATARC"

  cd $CWDSAVE

  # Create links to bitmaps

  # Note that the toplevel bitmaps are setup first because the images
  # from gschem/do not get sorted into subdirectories
  if [ -d $SRCDIR/../../bitmaps ] ; then
     cd $SRCDIR/../../bitmaps
     ln -s $PWD/*.png $GEDADATARC/bitmaps/
     ln -s $PWD/*.xpm $GEDADATARC/bitmaps/
     cd $GEDADATARC/bitmaps/
     mv *24x24.* 24x24/
     mv *26x26.* 26x26/
     mv *28x28.* 28x28/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find toplevel bitmaps"
    exit 0
  fi

  if [ -d $SRCDIR/../bitmap ] ; then
     cd $SRCDIR/../bitmap
     ln -s $PWD/*.png $GEDADATARC/bitmaps/
     ln -s $PWD/*.xpm $GEDADATARC/bitmaps/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gschem bitmaps"
    exit 0
  fi

  # Make links to rc files
  if [ -d $SRCDIR/../etc ] ; then
     cd $SRCDIR/../etc
     ln -s $PWD/gschem-* $GEDADATARC/
     ln -s $PWD/system-gschemrc $GEDADATARC/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gschem etc directory"
    exit 0
  fi

  if [ -d $SRCDIR/../../libgeda/etc ] ; then
     cd $SRCDIR/../../libgeda/etc
     ln -s $PWD/system-gafrc $GEDADATARC/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda etc directory"
    exit 0
  fi

  if [ -d $SRCDIR/../../libgedacolor/etc ] ; then
     cd $SRCDIR/../../libgedacolor/etc
     ln -s $PWD/display-* $GEDADATARC/
     ln -s $PWD/print-* $GEDADATARC/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgedacolor etc directory"
    exit 0
  fi

  # Create link to RC files
  if [ -d $SRCDIR/../scheme ] ; then
     cd $SRCDIR/../scheme
     ln -s $PWD/*.scm $GEDADATARC/scheme/
     ln -s $PWD/gschem/*.scm $GEDADATARC/scheme/gschem/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gschem scheme"
    exit 0
  fi

  if [ -d $SRCDIR/../../libgeda/scheme ] ; then
     cd $SRCDIR/../../libgeda/scheme
     ln -s $PWD/*.scm $GEDADATARC/scheme/
     ln -s $PWD/geda/*.scm $GEDADATARC/scheme/geda/
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda scheme directory"
    exit 0
  fi

  if [ -d $SRCDIR/../../symbols ] ; then
   cd $SRCDIR/../../symbols
   export SYMDIR=$PWD
   vecho "SYMDIR=$SYMDIR"
   cd $GEDADATARC/gafrc.d/
   ln -s $SYMDIR/geda-clib.scm geda-clib.scm
   cd ..
   ln -s $SYMDIR sym
   cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find symbols directory"
    exit 0
  fi

  if [ -d $SRCDIR/../data ] ; then
     cd $SRCDIR/../data
     ln -s $PWD/*22.png $GEDADATARC/icons/hicolor/22x22/actions/
     cd  $GEDADATARC/icons/hicolor/22x22/actions/
     for i in *.png ; do mv "$i" "${i/-22.png/.png}" ; done
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find toplevel bitmaps"
    exit 0
  fi

  cd $CWDSAVE
}

03_setup_guild_environment ()
{
  local CWDSAVE=$PWD

  export GUILE_AUTO_COMPILE=0

  if [ -d $BUILDDIR/../scheme ] ; then
   cd $BUILDDIR/../scheme
   export GUILE_LOAD_PATH=$PWD:$GUILE_LOAD_PATH
   cd $BUILDDIR/../../libgeda/scheme
   export GUILE_LOAD_PATH=$PWD:$GUILE_LOAD_PATH
   cd $CWDSAVE
   vecho "GUILE_LOAD_PATH=$GUILE_LOAD_PATH"
  else
    echo "Error: not in the right place, cannot find scheme directory"
    exit 0
  fi
}

04_setup_test_environment()
{
   mkdir -m 0777 -p "logs"

   test "-d logs/ & -f logs/*" && rm -rf logs/*

   # create temporary output directory and required subdirs
   mkdir -m 0777 -p ${RUNDIR}
   rc=$?
   if test $rc -ne 0 ; then
	  echo "Failed to create directory ${RUNDIR} with appropriate permissions"
	  echo "mkdir returned $rc"
	  exit 1
   fi

   TESTDIR=${RUNDIR}
   export TESTDIR

   export TEST_APP=${APPLICATION}

   if $REGENERATE ; then
     test -d "golden" || mkdir -m 0777 golden
     export "regenerating=true"
   else
     unset "regenerating"
   fi
}

do_process_input()
{
  local SUBTOTAL=0
  file="$1"

  echo "processing: ${file}" >>test-suite.log

  # Extract the TEST= attribute values from the file

  TESTS=$(grep TEST= ${file} | cut -d= -f2 )

  # Execute each of the "values" passing the file name

  for TEST in ${TESTS[@]} ; do

    if test ! -z ${DEBUG} ; then
      # Empty out the run directory in between each test
      test "-d ${RUNDIR}/ && -f ${RUNDIR}/*" && rm -rf ${RUNDIR}/*
    fi

    # Check if "TEST" is executable and Run test
    if [[ -x "$TEST" ]] ; then
      (./${TEST} ${file})
      if [ $? -ne 0 ] ; then
        FAILCOUNT=$(($FAILCOUNT + 1))
        echo $(basename ${file}) " Failed: ${TEST}"
      else
        PASSCOUNT=$(($PASSCOUNT + 1))
      fi
      SUBTOTAL=$(($SUBTOTAL + 1))
    else
      echo "Found bad or missing test $TEST"
    fi

    test "-f run/${LOG}" && cat run/${LOG} >> ${BUILDDIR}/${LOG}

  done

  return $SUBTOTAL
}

# ---------------- Process Command-Line Arguments  ----------------
#
# Too much to do to mess around here, just get it done!
if [ "$#" -eq 0 ] ; then show_help ; exit $ERR_BAD_ARGS ; fi
if [ "$1" = "-d" ] || [ "$1" = "--debug" ] ; then DEBUG=true ; shift ; fi
if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then show_help ; exit 0 ; fi
if [ "$1" = "-r" ] || [ "$1" = "--regen" ] ; then REGENERATE=true ; shift ; fi
if [ "$1" = "-v" ] || [ "$1" = "--verbose" ] ; then VERBOSE=true ; shift ; fi
if [ "$#" -lt 1 ] ; then show_help ; exit $ERR_BAD_ARGS ; fi
if [ "$#" -eq 1 ] ; then
  # Must be manual test mode
  INPUTS=${SRCDIR}/inputs/$1.sch
  if [ -f ${INPUTS} ] ; then
    BASE_NAME=`basename $INPUTS`
    if $REGENERATE ; then
      test ! -z ${VERBOSE} && echo "Regenerating single test:$BASE_NAME"
    else
      test ! -z ${VERBOSE} && echo "Running single test:$BASE_NAME"
    fi
  else
    echo "Did not understand $1"
    exit $ERR_BAD_ARGS
  fi
elif [ "$#" -eq 2 ] ; then
  BUILDDIR=$1
  SRCDIR=$2
fi

if test ! -f "${SRCDIR}/TEST_FUNCS" ; then
  echo "TEST_FUNCS is missing, aborting tests."
  exit 1;
fi

. ${SRCDIR}/TEST_FUNCS

if test ! -z ${DEBUG} ; then
  vecho "Debugging mode is active"
  vecho "VERBOSE mode is active"
fi

# ------------ Clean old tests files and directories ------------

# Clean up error remnants
if test -d ${ERRDIR} ; then
  vecho "Removing remnants: ${ERRDIR}"
  rm -rf ${ERRDIR}
fi

# Cleaning remnants if DEBUG was set on the previous run
test -d ${RUNDIR} && rm -rf ${RUNDIR};
test -d libs && rm -rf libs;
test -d gEDA && rm -rf gEDA;

# ------------------- Check for gschem Program ------------------
APPLICATION=${BUILDDIR}/../src/$PROGRAM

if test ! -x $APPLICATION ; then
  echo "Error: Can not find $APPLICATION"
  exit 1;
else

  appver=$($APPLICATION -q --version) 2>&1

  if $REGENERATE ; then
    echo "Current version: $PROGRAM $appver"
  else
    echo "Testing $PROGRAM version: $appver"
  fi

  # Note: Tested for gschem one level up but gschem will be ran
  # from the run directory, which will be 1 level down, so:
  APPLICATION="${BUILDDIR}/../../src/$PROGRAM"
fi

# --------------------- Set Test Directories --------------------

INPUTDIR="${SRCDIR}/inputs"
RUNDIR="${BUILDDIR}/run"

# Check that the inputs directory exists
if test ! -d $INPUTDIR ; then
  echo "Error: Can not find inputs"
  exit 1;
fi

# --------------------- Get test input files --------------------
#
# If BASE_NAME is nil then NOT single test mode
#
if test -z "$BASE_NAME" ; then
  # Remove any old files from the input directory
  rm -f $INPUTDIR/*~
  INPUTS=$INPUTDIR/*
fi

# Check that the inputs directory exists
if test ${#INPUTS[@]} -eq 0 ; then
  echo "Error: Input directory is empty"
  exit 1;
fi

# --------------------- Execute Setup Function ------------------
#
# Creates mini-installation directory and setup environment to use
# in-source files
#
01_setup_path2libraries
02_setup_geda_environment
03_setup_guild_environment
04_setup_test_environment

# Write header to log file
cat << \EOF > test-suite.log
==========================================================
 gschem regression test-suite gschem/tests/test-suite.log
==========================================================
EOF

# --------------- Process each of the input files ---------------
for file in $INPUTS ; do
  do_process_input $file
  TOTALTEST=$(($TOTALTEST + $?))
done

if test ! -z ${REGENERATE} ; then
  echo "TOTALTEST=$TOTALTEST PASSCOUNT=$PASSCOUNT FAILCOUNT=$FAILCOUNT"
else
  vecho "Regenerated $TOTALTEST test"
fi

# Clean up if not debugging
if test ! -z ${DEBUG} ; then
  image_func_config
else
  vecho "Removing temporary test directories"
  rm -rf ${RUNDIR}
  rm -rf libs
  rm -rf gEDA
fi

exit $FAILCOUNT
