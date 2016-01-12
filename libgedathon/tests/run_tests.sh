#!/bin/bash

let result=0;

SRCDIR=$srcdir

MODULE="../module/.libs/geda.so"

RPATH2LIBGEDA=$SRCDIR/../../libgeda/src/.libs
RPATH2LIBCAIRO=$SRCDIR/../../libgedacairo/src/.libs
RPATH2LIBTHON=$SRCDIR/../src/.libs

CHECKSYM=gsymcheck
PATH2CHECKSYM=../../gsymcheck/src
SYMCHECKER=

do_export_module () {
   mkdir -p geda
   cp "$MODULE" ./geda/
   cp ../module/__init__.py ./geda/
   export PYTHONPATH=$PWD/
}

do_remove_module () {

   if [ -d geda ] ; then
      rm -rf geda
   fi
}

do_err_exit () {
   echo "$1"
   do_remove_module
   exit 1;
}

do_exit_no_symchecker ()
{
   echo "cannot check <libgedathon>, $CHECKSYM is missing"
   echo "PATH2CHECKSYM=$PATH2CHECKSYM"
   exit 1;
}

do_get_symbol_checker ()
{
   if [ -x $PATH2CHECKSYM/$CHECKSYM ] ; then
       SYMCHECKER=$PATH2CHECKSYM/$CHECKSYM
   else
       SYMCHECKER=$(which "${CHECKSYM}")
   fi

   test ! -z ${SYMCHECKER} || do_exit_no_symchecker
}

do_export_path2libraries()
{
   local CWDSAVE=$PWD
   local PATH2LIBGEDA
   local PATH2LIBCAIRO
   local PATH2LIBTHON

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

   # Libgedathon
   if [ -d $RPATH2LIBTHON ] ; then
      cd $RPATH2LIBTHON
      PATH2LIBTHON=$PWD
      cd $CWDSAVE
   else
      echo "Error: not in the right place, cannot find libgedathon"
      exit 0
   fi

   export LD_LIBRARY_PATH=$PATH2LIBGEDA:$PATH2LIBCAIRO:$PATH2LIBTHON:$LD_LIBRARY_PATH

   test $VERBOSE && echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
}

if [ -d "../scripts" ] ; then
   path2scripts="../scripts"
else
   echo " cannot check <libgedathon>, directory containing scripts is missing"
   exit 1;
fi

if [ -f "$MODULE" ] ; then

   do_get_symbol_checker

   do_export_module
   do_export_path2libraries

   # Testing for the existence of the scripts is really only applicable when
   # "make check" is invoked directly from the tests/ directory, otherwise the
   # Makefile in ../scripts/ would have aborted "make check" before reaching
   # the tests/ directory

   if [ ! -f "${path2scripts}/capacitor.py" ] ; then
     do_err_exit " can not check <libgedathon>, because capacitor.py is not in $path2scripts/"
   elif [ ! -f "${path2scripts}/dual-opamp.py" ] ; then
     do_err_exit " can not check <libgedathon>, because dual-opamp.py is not in $path2scripts/"
   elif [ ! -f "${path2scripts}/lpbf.py" ] ; then
     do_err_exit " can not check <libgedathon>, because lpbf.py is not in $path2scripts/"
   elif [ ! -f "${path2scripts}/resistor.py" ] ; then
     do_err_exit " can not check <libgedathon>, because resistor.py is not in $path2scripts/"
   elif [ ! -f "lpfilter.sch" ] ; then
     do_err_exit " can not check <libgedathon>, because lpfilter.sch is not in $PWD/"
   else

     $path2scripts/lpbf.py

     if [ ! -f "tmp/lpfilter.sch" ] ; then
       do_err_exit "<lpbf.py> did not produce tmp/lpfilter.sch"
     else

       # Check that each of the symbol files are compliant
       answer=$($SYMCHECKER -q tmp/sym/capacitor-py.sym)
       if [ $? -ne 0 ] ; then
         echo "Failed capacitor-py.sym, see gsymcheck -v tmp/sym/capacitor-py.sym"
         ((result++))
       fi
       answer=$($SYMCHECKER -q tmp/sym/resistor-py.sym)
       if [ $? -ne 0 ] ; then
         echo "Failed resistor-py.sym, see gsymcheck -v tmp/sym/resistor-py.sym"
         ((result++))
       fi
       answer=$($SYMCHECKER -q tmp/sym/dual-opamp-py.sym)
       if [ $? -ne 0 ] ; then
         echo "Failed dual-opamp-py.sym, see gsymcheck -v tmp/sym/dual-opamp-py.sym"
         ((result++))
       fi
       answer=$(diff <(tail -n +2 "lpfilter.sch") <(tail -n +2 "tmp/lpfilter.sch"))
       if [ ! -z "$answer" ] ; then
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

echo "Completed tests for libgedathon!"
exit $result;
