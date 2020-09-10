#!/bin/bash

VER=1.0

# Author: Wiley Edward Hill
# Date: 02/25/13
#
# Abstract:

MINARGS=1          # Script requires at least one arguments.
ERR_BAD_ARGS=65
VERBOSE=false
PrefixLength=3
Suxfix=".sym"

TemplateFile=""
ParameterFile=""
Prefix=4

vecho()
{
   if [[ ! $1 = "" ]] ; then
     if $VERBOSE ; then
      echo $1
     fi
   fi
}

do_show_usage(){
   echo Usage:   `basename $0` '[-h, --help] | [--version] | [options] template[.sym] parameters[.spm]'
}

do_process(){
   echo "Using Parameter File: $ParameterFile"
   while read -u3 line
   do
      parameters=( $line )
      Capacitance=${parameters[0]}
      Voltage=${parameters[1]}
      PartNumber=${parameters[2]}
      FootPrint=${parameters[3]}
      vecho "Read Capacitance=$Capacitance, Voltage=$Voltage, PartNumber=$PartNumber, FootPrint=$FootPrint"
      if [ ! "${Capacitance:0:1}" = "#" ] && [ ! "$Capacitance" = "" ] ; then
         OutputFile=$Prefix"_"$Capacitance"_"$Voltage"_"$PartNumber$Suxfix
         Value=$Capacitance", "$Voltage
         cat $TemplateFile | sed -e s/"PART_NUMBER"/"$PartNumber"/g >$OutputFile.tp1
         cat $OutputFile.tp1 | sed -e s/"unknown"/"$FootPrint"/g >$OutputFile.tp2
         cat $OutputFile.tp2 | sed -e s/"CAPACITOR_VALUE"/"$Value"/g >$OutputFile
         # ------ Remove the tmp files
         if [ -f $OutputFile.tp1 ]; then
           rm $OutputFile.tp1
         fi
         if [ -f $OutputFile.tp2 ]; then
           rm $OutputFile.tp2
         fi
      fi
   done 3< <(cat $ParameterFile)
}

if [ "$#" -lt $MINARGS ] ; then do_show_usage; exit $ERR_BAD_ARGS ; fi

if [ "$1" = "--version" ] ; then echo `basename $0` "Version $VER" ; exit 0 ; fi
if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then do_show_usage ; exit 0 ; fi
if [ "$1" = "-q" ] || [ "$1" = "--quiet" ] ; then quiet=true ; shift ; fi
if [ "$1" = "-v" ] || [ "$1" = "--verbose" ] ; then VERBOSE=true ; shift ; fi

if [ -f $1 ] ; then
  TemplateFile="$1"
  shift ;
else
  if [ -f "$1.sym" ] ; then
    TemplateFile="$1.sym"
  else
    echo "Template File not found: $TemplateFile"
    exit $ERR_BAD_ARGS
  fi
fi

Prefix=`echo "${TemplateFile}" | cut -d'-' -f1` ; # trim away all but first word

if [ -f $1 ] ; then
  ParameterFile= "$1"
  shift ;
else
  if [  -f "$1.spm" ] ; then
    ParameterFile="$1.spm"
  else
    echo "Parameter File not found: $ParameterFile"
    exit $ERR_BAD_ARGS
  fi
fi

do_process


















