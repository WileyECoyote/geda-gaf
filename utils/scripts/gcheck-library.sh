#!/bin/bash

# Author: Wiley Edward Hill
# Date: 10/20/12
#
# gEDA - GPL Electronic Design Automation
# gcheck-library - gEDA Symbol Library Batch Processor
#
# Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
#
#------------------------------------------------------------------
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
#------------------------------------------------------------------

VER=1.2.0

VERBOSE=false
QUIET=false
REPORT=true
CHECK_CLIB=false
CHECK_DOCS=false
FILTER="*.sym"
PROGRAM1="gsymcheck"
PROGRAM2="gaf"
PROGRAM3="curl"
OPT=""
FINAL_ARGUMENT="${@: -1}"

# directory names to be excluded when checking geda-clib.scm
EXCLUDE='! -name doc ! -name src ! -name model'

ERR_FILE_NOT_FOUND=2
ERR_BAD_ARGS=65

do_show_help()
{
   case $1 in
         "1") # Quick help
              echo Usage:   `basename $0` '[-h] | [-v] | [-r] | [-a,d,e,n] [-f <pattern>] [ Library ]' ;;
         "2") # Basic help
              echo
              echo "A script to assist in maintaining symbol libraries"
              echo Usage:   `basename $0` '[-options] [-d] [-f <pattern>] [ Library ]'
              echo
              echo "Use the following options to control behavior:"
              echo
              echo   '  -a | --all       file pattern is changed from *.sym to *'
              echo   '  -c | --clip      Check geda-clib.scm for errors and references'
              echo   '  -d | --docs      Check documentation references'
              echo   '  -f | --filter <pattern>   filter pattern is changed to pattern'
              echo   '  -n | --no_report do not perform report statistic'
              echo   '  -r | --report    report statistic only, do not perform checking'
              echo   ''
              echo   '  -h | --help      show this information'
              echo   '  -u | --usage     show basic usage format'
              echo   '  -v | --verbose   display extra information'
              echo   '       --version   Display version information'
              echo   ''
              echo ' Note: arguments are case sensitive. See documentation for more'
              echo ' detailed information.'
              echo ;;
          *)  echo "Help not avaliable for:$1" ;;
   esac
   exit 0
}

vecho()
{
   if [[ ! $1 = "" ]] ; then
     if $VERBOSE ; then
      echo $1
     fi
   fi
}

have_curl ()
{
  exist=$(which $PROGRAM2)
  if [[ ! $exist = "" ]] ; then
    CHECK_DOCS=true
  else
    echo "Documentation checks requested by could not find [$PROGRAM3]"
    echo "Skipping Checks for documentation references"
  fi
}

PREVIOUS_ARGUMENT=""
do_Assimilate_Arguments(){
   for Arg in $*; do
      case $Arg in
         --help | -h) do_show_help "2"                                ;;
        --usage | -u) do_show_help "1"                                ;;
      --verbose | -v) VERBOSE=true  ; OPT="-vv"                       ;;
           --version) echo `basename $0` "Version $VER" ; exit        ;;
        --quiet | -q) QUIET=true  ; OPT="-q"                          ;;
          --all | -a) FILTER="*"                                      ;;
         --clip | -c) CHECK_CLIB=true                                 ;;
         --docs | -d) have_curl                                       ;;
     --suppress | -s) OPT+=" -u "                                     ;;
    -n | --no_report) REPORT=false                                    ;;
       -r | --report) do_report_only                                  ;;
    	           *) case $PREVIOUS_ARGUMENT in
       --filter | -f) FILTER="$Arg"                                   ;;

                   *)                                                 ;;
                      esac                                            ;;
      esac
      PREVIOUS_ARGUMENT=$Arg
   done
}

do_report_stats() {
  if [ -d $1 ] ; then
    count=(`ls $1/$FILTER -1 2>/dev/null | grep -v ^l | wc -l`)
    echo "Count:$1    Symbols:$count"
  fi
  return $count;
}

do_report_only(){
  local result=0;
  if [ "$FINAL_ARGUMENT" = "" ] ; then
    SRC_DIRS=(`find * -maxdepth 0 -type d -printf "%f "`)
    for subdir in "${SRC_DIRS[@]}" ; do
      do_report_stats $subdir
    done
  else
    if [ -d $FINAL_ARGUMENT ] ; then
      do_report_stats $FINAL_ARGUMENT
      result=$?;
    else
      echo "Error, Could not access [$FINAL_ARGUMENT], recheck"
      result=-1;
    fi
  fi
  exit $result;
}

do_check_docs () {

  local filename;
  local files=$1/${FILTER}
  local result=0;
  local url

  for filename in $files ; do

    fname=`basename $filename`
    vecho "checking documentation for $fname"
    url=$(grep documentation= $filename | cut -f2- -d'=')
    if [ "x$url" != "x" ] ; then
       if ! curl --output /dev/null --location --silent --fail -r 0-0 "$url"; then
         echo "check $url in $fname"
         result=1;
       fi
    else
       echo `basename $filename` "does not appear to have a documentation attribute"
       result=1;
    fi
  done

  return $result;
}

do_check_all_containers () {
  local result=0;
  exist=$(which $PROGRAM1)
  if [[ ! $exist = "" ]] ; then
    SRC_DIRS=(`find * -maxdepth 0 -type d -printf "%f "`)
    for subdir in "${SRC_DIRS[@]}" ; do
      if [ "$(ls -A $subdir/$FILTER 2>/dev/null)" ] ; then
        $PROGRAM1 $OPT $subdir/$FILTER
        if [ $? -ne 0 ] ; then
          result=1;
        fi
      elif [ "$(ls -A $subdir/*/$FILTER 2>/dev/null)" ] ; then
        $PROGRAM1 $OPT $subdir/*/$FILTER
        if [ $? -ne 0 ] ; then
          result=1;
        fi
      else
        echo "$subdir/$FILTER has no symbol files"
      fi
      if $REPORT ; then
        do_report_stats $subdir
      fi
      if $CHECK_DOCS ; then
        do_check_docs $subdir
        if [ $? -ne 0 ] ; then
          result=1;
        fi
      fi
    done
  else
    echo "Error, recheck installation. Could not find [$PROGRAM1]"
  fi
  return $result;
}

do_check_clib_references () {

  local found=0;
  local clibfile=$1;
  local symdir=$2;
  local result=0;

  cd "$symdir"

  SYM_DIRS=(`find * -maxdepth 1 -type d $EXCLUDE -printf "%f "`)

  for subdir in "${SYM_DIRS[@]}" ; do
    vecho "Checking reference to $subdir"
    found=$(cat $clibfile | grep $subdir)
    if [[ -z $found ]] ; then
      missing=$(find * -maxdepth 1 -type d -name "$subdir")
      echo "$missing is not referenced by $filename"
      result=1;
    fi
  done
  return $result;
}

do_check_clib () {

  local result=0;
  local filename;

  exist=$(which $PROGRAM2)
  if [[ ! $exist = "" ]] ; then

    if [ -f "geda-clib.scm" ] ; then
      filename="$PWD/geda-clib.scm"
      result=$($PROGRAM2 "shell" "-e" "(load \"$filename\")" &> /dev/null)
      if [ $? -ne 0 ] ; then
         echo "$filename contains Errors!"
         result=1;
      else
         echo "$filename is Okay!"
         #savedir=$PWD
         cd ..
         #TODO maybe use @symbol_dirs@
         if [ -d "symbols" ] ; then
           do_check_clib_references $filename "symbols"
         elif [ -d "sym" ] ; then
           do_check_clib_references $filename "sym"
         else
           vecho "Not checking for symbol directory references"
         fi
         #cd "$savedir"
      fi
    else
      echo "Error, geda-clib.scm must be in the current directory"
    fi

  else
    echo "Error, recheck installation. Could not find [$PROGRAM2]"
  fi
  return $result;
}

# ----------------------- Parse Command Line ----------------------

do_Assimilate_Arguments $*

if $CHECK_CLIB ; then
  do_check_clib
else
  if [ "x$FINAL_ARGUMENT" != "x" ] && [ -d $FINAL_ARGUMENT ] ; then
    vecho "Checking:$FINAL_ARGUMENT/$FILTER"
    if $QUIET ; then
      $PROGRAM1 $OPT $FINAL_ARGUMENT/${FILTER}
    else
      if $REPORT ; then
        do_report_stats $FINAL_ARGUMENT
      fi
      $PROGRAM1 $OPT $FINAL_ARGUMENT/${FILTER}
      result=$?
    fi
    if $CHECK_DOCS ; then
       do_check_docs $FINAL_ARGUMENT
       if [ $? -ne 0 ] ; then
         result=1;
       fi
    fi
  else
    vecho "Looking in all containers for $FILTER"
    do_check_all_containers
  fi
fi

exit $result
