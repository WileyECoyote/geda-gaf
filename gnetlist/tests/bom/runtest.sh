#!/bin/sh

here=`pwd`
SRCDIR=${srcdir:-$here}
SRCDIR=`cd $SRCDIR && pwd`
BUILDDIR="${here}"

REGENERATE=
QUIET=
VERBOSE=

TOTALTEST=0
PASSCOUNT=0
FAILCOUNT=0

SCMDIR="${SRCDIR}/../../scheme"
GEDASCMDIR="${SRCDIR}/../../../libgeda/scheme"
GEDABUILTSCMDIR="${BUILDDIR}/../../../libgeda/scheme"
SCMLIBS="-L ${SCMDIR} -L ${GEDASCMDIR} -L ${GEDABUILTSCMDIR}"

GNETLIST="${BUILDDIR}/../../src/gnetlist -q ${SCMLIBS} -g bom"

TESTFILE="${SRCDIR}/../powersupply.sch"
OUTFILE="${BUILDDIR}/powersupply.bom"
REFFILE="${SRCDIR}/powersupply.bom"

PROGRAM="gnetlist"
TMPGEDADIR="gEDA"

#-------------------------------------------------------------------
#
# Verbose Message
#
vecho()
{
   if test ! -z "${1}" ; then
     if test ! -z ${VERBOSE} ; then
      echo "${1}"
     fi
   fi
}

# Show command line usage
show_help () {
   echo Usage:   `basename $0` '[-h] || [[-r || --regen] -v || --verbose] (no dir) input'
}

setup_geda_environment ()
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
  if [ -d $SRCDIR/../../etc ] ; then
     cd $SRCDIR/../../etc
     ln -s $PWD/system-* $GEDADATARC/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gnetlist etc directory"
    exit 1
  fi

  if [ -d $SRCDIR/../../../libgeda/etc ] ; then
     cd $SRCDIR/../../../libgeda/etc
     ln -s $PWD/print-* $GEDADATARC/ 2>/dev/null
     ln -s $PWD/system-gafrc $GEDADATARC/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda etc directory"
    exit 1
  fi

  # Make links to gnetlist scheme files in the source directory
  if [ -d $SRCDIR/../../scheme ] ; then
     cd $SRCDIR/../../scheme
     ln -s $PWD/*.scm $GEDADATARC/scheme/ 2>/dev/null
     ln -s $PWD/gnetlist/*.scm $GEDADATARC/scheme/gnetlist/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find gnetlist scheme"
    exit 1
  fi

  # Make links to gnetlist scheme files in the source directory
  if [ -d $SRCDIR/../../../libgeda/scheme ] ; then
     cd $SRCDIR/../../../libgeda/scheme
     ln -s $PWD/*.scm $GEDADATARC/scheme/ 2>/dev/null
     ln -s $PWD/geda/*.scm $GEDADATARC/scheme/geda/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda scheme src directory"
    exit 1
  fi

  # Make links to gnetlist scheme files in the build directory
  if [ -d $BUILDDIR/../../../libgeda/scheme ] ; then
     cd $BUILDDIR/../../../libgeda/scheme
     ln -s $PWD/geda/core/*.scm $GEDADATARC/scheme/geda/core/ 2>/dev/null
     cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find libgeda scheme build directory"
    exit 1
  fi

  # Make a link to geda-clib.scm and the symbols directory
  if [ -d $SRCDIR/../../../symbols ] ; then
   cd $SRCDIR/../../../symbols
   export SYMDIR=$PWD
   test $VERBOSE && echo "SYMDIR=$SYMDIR"
   cd $GEDADATARC/gafrc.d/
   ln -s $SYMDIR/geda-clib.scm geda-clib.scm 2>/dev/null
   cd ..
   ln -s $SYMDIR sym 2>/dev/null
   cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find symbols directory"
    exit 1
  fi
}

do_check_results()
{
  code=$1
  BOMFILE=$2

  if test $? -ne 0 ; then
    echo "FAIL: exited with an error"
    FAILCOUNT=`expr $FAILCOUNT + 1`
  else

    if test ! -f "${BOMFILE}" ; then
      echo "FAIL: ${BOMFILE} not generated"
      FAILCOUNT=`expr $FAILCOUNT + 1`
    else
      diff $EXTRADIFF ${REFFILE} "${BOMFILE}"
      if test $? -ne 0 ; then
        FAILCOUNT=`expr $FAILCOUNT + 1`
      else
        PASSCOUNT=`expr $PASSCOUNT + 1`
      fi
    fi
  fi
}

do_run_test()
{
  test -z ${QUIET} && echo "${GNETLIST} -O attribs=value,device -o ${OUTFILE}.1 ${TESTFILE}"
  ${GNETLIST} -O attribs=value,device -o "${OUTFILE}.1" ${TESTFILE}
  do_check_results $? ${OUTFILE}.1

  test -z ${QUIET} &&  echo "${GNETLIST} -O attrib_file=attribs2 -o ${OUTFILE}.2 ${TESTFILE}"
  ${GNETLIST} -O attribs=value,device -o "${OUTFILE}.2" ${TESTFILE}
  do_check_results $? ${OUTFILE}.2

  cp $BUILDDIR/attribs2 attribs

  test -z ${QUIET} && echo "${GNETLIST} -o ${OUTFILE}.3 ${TESTFILE}"
  ${GNETLIST} -O attribs=value,device -o "${OUTFILE}.3" ${TESTFILE}
  do_check_results $? ${OUTFILE}.3

  rm attribs

  if test -z ${DEBUG} ; then
    test -f "${OUTFILE}.1" && rm "${OUTFILE}.1"
    test -f "${OUTFILE}.2" && rm "${OUTFILE}.2"
    test -f "${OUTFILE}.3" && rm "${OUTFILE}.3"
  fi
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then show_help ; exit 0 ; fi
if [ "$1" = "-r" ] || [ "$1" = "--regen" ] ; then REGENERATE=true ; shift ; fi
if [ "$1" = "-v" ] || [ "$1" = "--verbose" ] ; then VERBOSE=true ; shift ; fi
if [ "$1" = "-q" ] || [ "$1" = "--quiet" ] ; then QUIET=true ; shift ; fi

if test ! -f ${TESTFILE} ; then
  echo "missing ${TESTFILE}, bom test aborted!"
  exit 1;
fi

setup_geda_environment

if test -z ${REGENERATE} ; then
  do_run_test
  echo "PASSCOUNT=$PASSCOUNT FAILCOUNT=$FAILCOUNT"
else
  ${GNETLIST} -O attribs=value,device -o powersupply.bom ${TESTFILE}
fi

# Clean up if not debugging
if test -z ${DEBUG} ; then
  vecho "Removing temporary test directories"
  rm -rf gEDA
fi

exit $FAILCOUNT
