#!/bin/bash

let result=0;

MODULE="../module/.libs/geda.so"

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

if [ ! -f Makefile.am ] ; then
   echo "Skipping Libgedathon unit test for distcheck" >test-suite.log
   exit 0;
fi

ldconfig -p | grep libgeda
if [ $? != 0 ] ; then
   echo "Skipping Libgedathon unit test: libgeda is not installed" >test-suite.log
   exit 0;
fi

if [ -d "../scripts" ] ; then
   path2scripts="../scripts"
else
   echo " can not check <libgedathon>, can not find directory containing scripts"
   exit 1;
fi

if [ -f "$MODULE" ] ; then

   do_export_module

   # Testing for the existence if the scripts is really on applicable when the make check
   # is ran directly from the tests/ directory, otherwise the Makefile in ../scripts/ would
   # have exit "make check" before reaching the tests/ directory

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
       answer=$(`gsymcheck -q tmp/sym/capacitor-py.sym`)
       if [ $? -ne 0 ] ; then
         echo "Failed capacitor-py.sym, see gsymcheck -v tmp/sym/capacitor-py.sym"
         ((result++))
       fi
       answer=$(`gsymcheck -q tmp/sym/resistor-py.sym`)
       if [ $? -ne 0 ] ; then
         echo "Failed resistor-py.sym, see gsymcheck -v tmp/sym/resistor-py.sym"
         ((result++))
       fi
       answer=$(`gsymcheck -q tmp/sym/dual-opamp-py.sym`)
       if [ $? -ne 0 ] ; then
         echo "Failed dual-opamp-py.sym, see gsymcheck -v tmp/sym/dual-opamp-py.sym"
         ((result++))
       fi
       answer=$(diff "lpfilter.sch" "tmp/lpfilter.sch")
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
