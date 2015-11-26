#!/bin/bash

# Author: Wiley Edward Hill
# Date: 11/21/12
#
# Abstract: This scripts serves as a high-level script for executing
#           tests involving gschem, executing tests declared in the
#           schematic files and reporting the results. The script can
#           be invoked from the Makefile or from the command-line.

VER=0.0.8

ERR_FILE_NOT_FOUND=2
ERR_BAD_ARGS=65

APPLICATION=""
BUILDDIR="."
SRCDIR="."
ERRDIR=mismatched

DEBUG=false
REGENERATE=false
VERBOSE=false

TOTALTEST=0
PASSCOUNT=0
FAILCOUNT=0

PROGRAM="gschem"
. ./TEST_FUNCS

# Show command line usage
show_help (){
   echo Usage:   `basename $0` '[-h] || [[-r || --regen] -v || --verbose] (no dir) input'
}

do_setup_environment()
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

   if $DEBUG ; then
     export "debugging=true";
   else
     unset "debugging"
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

   if $VERBOSE ; then
     export "verbosity=1"
   else
     unset "verbosity"
   fi
}

do_process_input()
{
  local SUBTOTAL=0
  file="$1"

  # Extract the TEST= attribute values from the file

  TESTS=$(grep TEST= ${file} | cut -d= -f2 )

  # Exexute each of the "values" passing the file name

  for TEST in ${TESTS[@]} ; do

    if ! $DEBUG ; then
      # Empty out the run directory in between each test
      test "-d ${RUNDIR}/ && -f ${RUNDIR}/*" && rm -rf ${RUNDIR}/*
    fi

    # Check if "TEST" is executable and Run test
    if [[ -x "$TEST" ]] ; then
      (./${TEST} ${file})
      if [ $? -ne 0 ] ; then
        FAILCOUNT=$(($FAILCOUNT + 1))
      else
        PASSCOUNT=$(($PASSCOUNT + 1))
      fi
      SUBTOTAL=$(($SUBTOTAL + 1))
    else
      echo "Found bad or missing test $TEST"
    fi
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
  INPUTS=${SRCDIR}/inputs/$1*
  if [ -f ${INPUTS} ] ; then
    BASE_NAME=`basename $INPUTS`
    if $REGENERATE ; then
      vecho "Regenerating single test:$BASE_NAME"
    else
      vecho "Running single test:$BASE_NAME"
    fi
  else
    echo "Did not understand $1"
    exit $ERR_BAD_ARGS
  fi
elif [ "$#" -eq 2 ] ; then
  BUILDDIR=$1
  SRCDIR=$2
fi

if $DEBUG ; then
  vecho "Debugging mode is active"
fi

# Clean up remnants
if [ -d $ERRDIR ] ; then
  vecho "Removing remnants: ${ERRDIR}"
  rm -rf ${ERRDIR}
fi

INPUTDIR="${SRCDIR}/inputs"
RUNDIR="${BUILDDIR}/run"

# ------------------- Check for gschem Program ------------------
APPLICATION=${BUILDDIR}/../src/.libs/$PROGRAM

if test ! -x $APPLICATION ; then
  echo "Error: Can not find $APPLICATION"
  exit 1;
else
  APPLICATION=${BUILDDIR}/../../src/.libs/$PROGRAM
fi

# Check that the inputs directory exists
if test ! -d $INPUTDIR ; then
  echo "Error: Can not find inputs"
  exit 1;
fi

# --------------------- Get test input files --------------------
#
# If BASE_NAME is nil then not single test mode
#
if [ -z "$BASE_NAME" ] ; then
  # Remove any old files from the input directory
  rm -f $INPUTDIR/*~
  INPUTS=$INPUTDIR/*
fi

# Check that the inputs directory exists
if [ ${#INPUTS[@]} -eq 0 ] ; then
  echo "Error: Input directory is empty"
  exit 1;
fi

do_setup_environment

# --------------- Process each of the input files ---------------

for file in $INPUTS ; do
  do_process_input $file
  TOTALTEST=$(($TOTALTEST + $?))
done

if $REGENERATE ; then
  vecho "Regenerated $TOTALTEST test"
else
  echo "TOTALTEST=$TOTALTEST PASSCOUNT=$PASSCOUNT FAILCOUNT=$FAILCOUNT"
fi

# Clean up if not debugging
if ! $DEBUG ; then
  rm -rf ${RUNDIR}
else
  image_func_config
fi

exit $FAILCOUNT

