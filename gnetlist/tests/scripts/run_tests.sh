#!/bin/bash

let result=0;

path2scripts="../../scripts"

for Arg in $*; do
   if [ ! -f "${path2scripts}/annotate" ] ; then
     echo " can not check <annotate>, because it is not in $path2scripts/"
     exit 1;
   elif [ ! -f "${path2scripts}/unannotate" ] ; then
     echo " can not check <unannotate>, because it is not in $path2scripts/"
     exit 1;
   elif [ ! "$Arg" ] ; then
     echo "Need $Arg to check <annotate>, it is not in $PWD"
     exit 1;
   else
     $path2scripts/annotate "-b -i $Arg -o $Arg.new"
     if [ ! -f "$Arg.new" ] ; then
       echo "<annotate> did not produce $Arg.new"
       exit 1;
     fi
     $path2scripts/unannotate "-b -i $Arg.new -o $Arg.org"
     if [ ! -f "$Arg.org" ] ; then
       echo "<annotate> did not produce $Arg.org"
       exit 1;
     fi
     answer=$(diff "$Arg" "$Arg.org")
     if [ ! -z $answer ] ; then
       echo "Failed command mode, Check $Arg and $Arg.org, suppose to be the exactly the same"
       result++;
     else
       rm $Arg.new
       rm $Arg.org
     fi
     
     cat $Arg | $path2scripts/annotate > "$Arg.new"

     if [ ! -f "$Arg.new" ] ; then
       echo "<annotate> did not produce $Arg.new"
       exit 1;
     fi

     cat $Arg.new | $path2scripts/unannotate > "$Arg.org"

     if [ ! -f "$Arg.org" ] ; then
       echo "<annotate> did not produce $Arg.org"
       exit 1;
     fi

     answer=$(diff "$Arg" "$Arg.org")
     if [ ! -z $answer ] ; then
       echo "Failed piping mode, check $Arg and $Arg.org, suppose to be the exactly the same"
       ((result++))
     else
       rm $Arg.new
       rm $Arg.org
     fi
   fi

done
echo "Complete tests for netlist scripts!"
exit $result;
