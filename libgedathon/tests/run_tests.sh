#!/bin/bash

let result=0;

   if [ ! -f Makefile.am ] ; then
      echo "Skipping Libgedathon unit test for distcheck" >test-suite.log
      exit 0
   fi

   if [ -d "../scripts" ] ; then
     path2scripts="../scripts"
   else
     echo " can not check <libgedathon>, can not find directory containing scripts"
     exit 1;
   fi

   if [ ! -f "${path2scripts}/capacitor.py" ] ; then
     echo " can not check <libgedathon>, because capacitor.py is not in $path2scripts/"
     exit 1;
   elif [ ! -f "${path2scripts}/dual-opamp.py" ] ; then
     echo " can not check <libgedathon>, because dual-opamp.py is not in $path2scripts/"
     exit 1;
   elif [ ! -f "${path2scripts}/lpbf.py" ] ; then
     echo " can not check <libgedathon>, because lpbf.py is not in $path2scripts/"
     exit 1;
   elif [ ! -f "${path2scripts}/resistor.py" ] ; then
     echo " can not check <libgedathon>, because resistor.py is not in $path2scripts/"
     exit 1;
   elif [ ! -f "lpfilter.sch" ] ; then
     echo " can not check <libgedathon>, because lpfilter.sch is not in $PWD/"
     exit 1;
   else
     $path2scripts/lpbf.py
     if [ ! -f "tmp/lpfilter.sch" ] ; then
       echo "<lpbf.py> did not produce tmp/lpfilter.sch"
       exit 1;
     else
       answer=$(diff "lpfilter.sch" "tmp/lpfilter.sch")
       if [ ! -z "$answer" ] ; then
         echo "Failed diff, lpfilter.sch and tmp/lpfilter.sch are suppose to be the exactly the same"
         echo "check $answer";
         ((result++))
#       rm -rf $Arg.new
#       rm $Arg.org
       fi
     fi
   fi

echo "Completed tests for libgedathon scripts!"
exit $result;
