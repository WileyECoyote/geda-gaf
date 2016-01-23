#!/bin/bash

let result=0;

BUILDDIR=$PWD;

if test -z ${srcdir} ; then
  SRCDIR="."
else
  SRCDIR=$srcdir
fi

cd ${SRCDIR}
SRCDIR=$PWD

MODULE=${BUILDDIR}/../module/.libs/geda.so

RPATH2LIBGEDA=${BUILDDIR}/../../libgeda/src/.libs
RPATH2LIBCAIRO=${BUILDDIR}/../../libgedacairo/src/.libs
RPATH2LIBTHON=${BUILDDIR}/../src/.libs

CHECKSYM=gsymcheck
PATH2CHECKSYM=${BUILDDIR}/../../gsymcheck/src
SYMCHECKER=

do_exit_no_symchecker ()
{
   echo "cannot check <libgedathon>, $CHECKSYM is missing"
   echo "PATH2CHECKSYM=$PATH2CHECKSYM"
   exit 1;
}

do_get_symbol_checker ()
{
   if test -x $PATH2CHECKSYM/$CHECKSYM ; then
       SYMCHECKER=$PATH2CHECKSYM/$CHECKSYM
   else
       SYMCHECKER=$(which "${CHECKSYM}")
   fi

   test ! -z ${SYMCHECKER} || do_exit_no_symchecker
}

do_export_module () {
   mkdir -p ${BUILDDIR}/geda  2>/dev/null
   cp "$MODULE" ${BUILDDIR}/geda/
   cp ../module/__init__.py ${BUILDDIR}/geda/
   export PYTHONPATH=${BUILDDIR}
   test $VERBOSE && echo  "PYTHONPATH=${PYTHONPATH}"
}

do_remove_module () {

   if test -d geda ; then
      rm -rf geda
   fi
}

do_err_exit () {
   echo "$1"
   do_remove_module
   exit 1;
}

do_export_path2libraries()
{
   local CWDSAVE=$PWD
   local PATH2LIBGEDA
   local PATH2LIBCAIRO
   local PATH2LIBTHON

   # Libgeda
   if test -d ${RPATH2LIBGEDA} ; then
      cd $RPATH2LIBGEDA
      PATH2LIBGEDA=$PWD
      cd $CWDSAVE
   else
      echo "Error: not in the right place, cannot find libgeda: $db"
      exit 1
   fi

   # Libgedacairo
   if test -d ${RPATH2LIBCAIRO} ; then
      cd $RPATH2LIBCAIRO
      PATH2LIBCAIRO=$PWD
      cd $CWDSAVE
   else
      echo "Error: not in the right place, cannot find libgedacairo"
      exit 1
   fi

   # Libgedathon
   if test -d ${RPATH2LIBTHON} ; then
      cd $RPATH2LIBTHON
      PATH2LIBTHON=$PWD
      cd $CWDSAVE
   else
      echo "Error: not in the right place, cannot find libgedathon"
      exit 1
   fi

   export LD_LIBRARY_PATH=$PATH2LIBGEDA:$PATH2LIBCAIRO:$PATH2LIBTHON:$LD_LIBRARY_PATH

   test $VERBOSE && echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
}

do_setup_geda_environment ()
{
  local CWDSAVE=$PWD

  if test -d ${SRCDIR}/../../symbols ; then
   cd ${SRCDIR}/../../symbols
   SYMDIR=$PWD
   cd ${BUILDDIR}
   ln -s -T $SYMDIR sym 2>/dev/null
   export GEDADATA=$PWD
   cd $CWDSAVE
  else
    echo "Error: not in the right place, cannot find symbols directory"
    exit 1
  fi

  test $VERBOSE && echo  "GEDADATA=${GEDADATA}"
}

do_setup_scripts ()
{
  local CWDSAVE=$PWD

  if test -d  ${SRCDIR}/../scripts ; then
   if test ! -f ${BUILDDIR}/../scripts/capacitor.py ; then
      cp ${SRCDIR}/../scripts/*.py ${BUILDDIR}/../scripts/
   fi
   path2scripts=../scripts
  else
     echo " cannot check <libgedathon>, directory containing scripts is missing"
     exit 1;
  fi
}

if test -f "$MODULE" ; then

   do_get_symbol_checker

   do_export_module
   do_export_path2libraries
   do_setup_geda_environment
   do_setup_scripts

   # Testing for the existence of the scripts is really only applicable when
   # "make check" is invoked directly from the tests/ directory, otherwise the
   # Makefile in ../scripts/ would have aborted "make check" before reaching
   # the tests/ directory

   if test ! -f "${path2scripts}/capacitor.py" ; then
     do_err_exit " can not check <libgedathon>, because capacitor.py is not in $path2scripts/"
   elif test ! -f "${path2scripts}/dual-opamp.py" ; then
     do_err_exit " can not check <libgedathon>, because dual-opamp.py is not in $path2scripts/"
   elif test ! -f "${path2scripts}/lpbf.py" ; then
     do_err_exit " can not check <libgedathon>, because lpbf.py is not in $path2scripts/"
   elif test ! -f "${path2scripts}/resistor.py" ; then
     do_err_exit " can not check <libgedathon>, because resistor.py is not in $path2scripts/"
   elif test ! -f ${SRCDIR}/lpfilter.sch ; then
     do_err_exit " can not check <libgedathon>, because lpfilter.sch is not in ${SRCDIR}/"
   else

     $path2scripts/lpbf.py ${BUILDDIR}

     cd ${BUILDDIR}

     if test ! -f "tmp/lpfilter.sch" ; then
       do_err_exit "<lpbf.py> did not produce tmp/lpfilter.sch"
     else

       # Check that each of the symbol files are compliant
       answer=$($SYMCHECKER -q tmp/sym/capacitor-py.sym)
       if test $? -ne 0 ; then
         echo "Failed capacitor-py.sym, see gsymcheck -v tmp/sym/capacitor-py.sym"
         ((result++))
       fi
       answer=$($SYMCHECKER -q tmp/sym/resistor-py.sym)
       if test $? -ne 0 ; then
         echo "Failed resistor-py.sym, see gsymcheck -v tmp/sym/resistor-py.sym"
         ((result++))
       fi
       answer=$($SYMCHECKER -q tmp/sym/dual-opamp-py.sym)
       if test $? -ne 0 ; then
         echo "Failed dual-opamp-py.sym, see gsymcheck -v tmp/sym/dual-opamp-py.sym"
         ((result++))
       fi
       answer=$(diff <(tail -n +2 "${SRCDIR}/lpfilter.sch") <(tail -n +2 "tmp/lpfilter.sch"))
       if test ! -z "$answer" ; then
         echo "Failed diff, lpfilter.sch and tmp/lpfilter.sch are suppose to be the exactly the same"
         echo "check $answer";
         ((result++))
       fi
     fi
   fi

   do_remove_module

else
   echo  "Module not in $MODULE"
   exit 1;
fi

# Clean up if not debugging
test $DEBUG || rm -rf sym

echo "Completed tests for libgedathon!"
exit $result;
