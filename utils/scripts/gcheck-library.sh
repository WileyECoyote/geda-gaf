#!/bin/bash

# Author: Wiley Edward Hill
# Date: 10/20/12
#
# gEDA - GPL Electronic Design Automation
# gcheck-library - gEDA Symbol Library Batch Processor
# Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
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

VER=1.0.0

VERBOSE=false
QUIET=false
REPORT=true

FILTER="*.sym"
PROGRAM="gsymcheck"
OPT=""
FINAL_ARGUMENT="${@: -1}"

ERR_FILE_NOT_FOUND=2
ERR_BAD_ARGS=65

do_show_help()
{
   case $1 in
         "1") # Quick help
              echo Usage:   `basename $0` '[-h] | [-v] | [-r] | [-a,e,n] [-f <pattern>] [ Library ]' ;;
         "2") # Basic help
              echo
              echo "A script to assist in maintaining symbol libraries"
              echo Usage:   `basename $0` '[-options] [-d] [-f <pattern>] [ Library ]'
              echo
              echo "Use the follow options to control behavior:"
              echo
              echo   '  -a | --all       file pattern is changed from *.sym to *'
              echo   '  -e | --extra     perform extra checks'
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
              echo ' detailed information. [dirname] &2>result.txt'
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
    -n | --no_report) REPORT=false                                  ;;
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
  count=(`ls $1/$FILTER -1 | grep -v ^l | wc -l`)
  echo "FINAL_ARGUMENT:$1    Symbols:$count"
fi
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
    else
      echo "Error, Could not access [$FINAL_ARGUMENT], recheck"
      result=1;
    fi
  fi
  exit $result;
}

do_check_all_containers (){
  exist=$(which $PROGRAM)
  SRC_DIRS=(`find * -maxdepth 0 -type d -printf "%f "`)
  if [[ ! $exist = "" ]] ; then
    for subdir in "${SRC_DIRS[@]}" ; do
    $PROGRAM $OPT $subdir/$FILTER
    if $REPORT ; then
      do_report_stats $subdir
    fi
    done
  else
    echo "Error, recheck installation. Could not find [$mand]"
  fi
}

# ----------------------- Parse Command Line ----------------------
do_Assimilate_Arguments $*
vecho "Checking:$FINAL_ARGUMENT/$FILTER"
if [ "$FINAL_ARGUMENT" != "" ] && [ -d $FINAL_ARGUMENT ] ; then
  if $QUIET ; then
    $PROGRAM $OPT $FINAL_ARGUMENT/${FILTER}
  else
    if $REPORT ; then
      do_report_stats $FINAL_ARGUMENT
    fi
    $PROGRAM $OPT $FINAL_ARGUMENT/${FILTER}
  fi
else
  do_check_all_containers
fi
exit 0

