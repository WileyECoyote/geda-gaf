#!/bin/bash

VER=1.0

# Author: Wiley Edward Hill
# Date: 01/16/14
#
# Abstract:

MINARGS=1          # Script requires at least one arguments.
ERR_BAD_ARGS=65
VERBOSE=false
QUIET=false

Extension="sym"

do_show_usage(){
  echo
  echo Usage:   `basename $0` '[-h, --help] | [--version] | [options] old_string, new_string value'
  echo
}

do_show_help(){

  do_show_usage

  echo "Abstract: This script is for 'batch' setting the values in resistor symbol files for the"
  echo "         geda-gaf project. The script creates a tmp directory in the current directory."
  echo "         In all files in the current directory, the text \"Ohms\" will be substitute with"
  echo "         the value argument and the results put in the tmp directory. After the values are"
  echo "         set in the files, the strings in the file names matching \"old_string\" will be"
  echo "         replaced with \"new_string\". This is useful to set both the values and the file"
  echo "         name of a collection of template symbols with placeholder strings in the file"
  echo "         name to identifiers, such as real part numbers"
  echo
  echo Example:   `basename $0` 'xxxxR00 180R00 180'
  echo
  echo "         will change  \"xxxxR00\" in the file names to \"180R00\" and the text \"Ohms\""
  echo "         would be replaced with \"180\"."
  echo
  echo "Options: -h, --help       Show this information"
  echo "         -q, --quiet      Suppress extraneous messages"
  echo "         -v, --verbose    Display extra information during processing"
  echo "             --version    Display software version information"
}

# Author: Wiley Edward Hill
# Date: 06/03/12
#
# Abstract: Simple function to remove unwanted strings from file names
do_rm_substr(){
  for f in *$1*; do mv "$f" ${f/$1/$2}; done
}

vecho()
{
   if [[ ! $1 = "" ]] ; then
     if $VERBOSE ; then
      echo $1
     fi
   fi
}

do_change_value(){

   FILES=*.$Extension

   for f in $FILES
   do
     vecho "Precessing file: $f"
     cat $f | sed -e s/$1/$2/g >"tmp/$f"
   done
}

if [ "$1" = "-v" ] || [ "$1" = "--verbose" ] ; then VERBOSE=true ; shift ; fi

do_Assimilate_Arguments(){

   for Arg in $*; do
      case $Arg in
         --help | -h) do_show_help  ; return 1                        ;;
        --usage | -u) do_show_usage ; return 1                        ;;
      --verbose | -v) VERBOSE=true  ; shift ;                         ;;
           --version) echo `basename $0` "Version $VER" ; return 1    ;;
        --quiet | -q) QUIET=true  ; shift ;                           ;;
                   *)                                                 ;;
      esac
   done

   return 0;
}

do_check_arguments(){

  if [[ $1 = "" ]] ; then
    do_show_usage
    return $ERR_BAD_ARGS
  fi
  if [[ $2 = "" ]] ; then
    do_show_usage
    return $ERR_BAD_ARGS
  fi
  if [[ $3 = "" ]] ; then
    do_show_usage
    return $ERR_BAD_ARGS
  fi
}
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-> MAIN <-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ----------------- Check for Empty Command Line ------------------
if [ "$#" -lt $MINARGS ] ; then do_show_usage; exit $ERR_BAD_ARGS ; fi

# ----------------------- Parse Command Line ----------------------
do_Assimilate_Arguments $*
if [ $? != 0 ]; then exit $?; fi

old_name=$1
new_name=$2
value=$3

do_check_arguments $old_name $new_name $value
if [ $? != 0 ]; then exit $?; fi

mkdir -p tmp

do_change_value "Ohms" $value
if [ $? != 0 ]; then exit $?; fi

cd tmp
do_rm_substr $old_name $new_name
cd ..
