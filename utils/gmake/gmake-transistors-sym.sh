#!/bin/bash

VER=1.5

# Author: Wiley Edward Hill
# Date: 03/08/13
#

shopt -s nullglob

MINARGS=1          # Script requires at least one arguments.
ERR_FILE_NOT_FOUND=2
ERR_BAD_ARGS=65

VERBOSE=false
QUIET=false

PrefixLength=3
Extension=".sym"

TemplateFile=""
ParameterFile=""
Prefix=4

include_package=false
include_template=false
include_modifier=false

do_show_usage(){
   echo Usage:   `basename $0` '[-h, --help] | [--version] | [options] template[.sym] parameters[.spm]'
}

do_show_help(){
echo
echo Usage:   `basename $0` '[-options] template-symbol parameter-file'
echo
echo "Abstract: This script is for 'batch' production of transistor symbols"
echo "         for the geda-gaf project. The script uses template symbol file"
echo "         and a parameter file. Parameters read from the file are used"
echo "         in the output symbol file name or used as replacement values for"
echo "         pre-set variables in the symbol template file. The parameters are"
echo "         read and interpreted as follows:"
echo
echo "         1. Part Number string to be substituted for PART_NUMBER in the symbol file"
echo "         2. Voltage string is included in the file name and as part of the COMPONENT_VALUE"
echo "         3. Features string also included in the file name and in the COMPONENT_VALUE. This"
echo "            could be, for example, Wattage or Ampacity"
echo "         4. Package name,  if the -p option is present on the command line then the 4th field"
echo "            is interpreted  as the package ID and included in the file name"
echo "         5. FootPrint must be the next argument and is substituted for unknown in the template"
echo "         6. Optional symbol file modifier, if present, the value is used as a second suffix before"
echo "            the dash suffix. Normally this would be used to select the template with the correct pin"
echo "            -out, for example using ECB or BEC. The string is prefixed with a underscore composing"
echo "            the template file name."
echo "         7. Optional symbol file suffix, if this value is present in the parameter file, the"
echo "            string will be used between the template given on the commanline and the symbol"
echo "            file extension, in this case the name of the template given on the command line"
echo "            should only be the base file name. The string is prefixed with a dash composing"
echo "            the template file name. Note that if a modifier is not given then parameter in"
echo "            position 6 is assumed to be the symbol file suffix"
echo
echo "            Note on styles: The standard symbols include the technically correct symbols!"
echo "            Alternative styles can be used to provide a visual indication of something unusual"
echo "            about a component or group of components such as odd pin or package configuration"
echo "            When creating new styles a complementary symbol should also be created!"
echo
echo "Options: -h, --help       Show this information"
echo "         -v, --verbose    Display extra information during processing"
echo "             --version    Display software version information"
echo "         -p, --package    The packge type is included in the paratmeter data"
echo "         -t, --template   Include the template prefix and suffix in the output symbol name"
echo "         -m, --modifier   Suffix the template modifier in the output symbol name, like _ecb"
}
vecho()
{
   if [[ ! $1 = "" ]] ; then
     if $VERBOSE ; then
      echo $1
     fi
   fi
}

if [ "$1" = "-v" ] || [ "$1" = "--verbose" ] ; then VERBOSE=true ; shift ; fi

if [ "$1" = "-p" ] || [ "$1" = "--package" ] ; then include_package=true ; shift ; fi

do_Assimilate_Arguments(){

   for Arg in $*; do
      case $Arg in
         --help | -h) do_show_help ; exit 0                           ;;
        --usage | -u) do_show_usage ; exit 0                          ;;
      --verbose | -v) VERBOSE=true  ;                                 ;;
           --version) echo `basename $0` "Version $VER" ; exit 0      ;;
        --quiet | -q) QUIET=true  ;                                   ;;

      --package | -p) include_package=true ;                          ;;
     --template | -t) include_template=true ;                         ;;
     --modifier | -m) include_modifier=true ;                         ;;
                   *)                                                 ;;
      esac
   done

   return 0;
}

do_make_symbols(){
   vecho "Using Parameter File: $ParameterFile"
   while read -u3 line
   do
      parameters=( $line )
      PartNumber=${parameters[0]}
      Voltage=${parameters[1]}
      Features=${parameters[2]}
      if $include_package ; then
        Package=${parameters[3]}
        FootPrint=${parameters[4]}
        Template_file_modifier=${parameters[5]}
        Template_file_suffix=${parameters[6]}
      else
        Package=""
        FootPrint=${parameters[3]}
        Template_file_modifier=${parameters[4]}
        Template_file_suffix=${parameters[5]}
      fi

      vecho "Read PartNumber=$PartNumber, Voltage=$Voltage, Features=$Features, FootPrint=$FootPrint Template suffix1=$Template_file_modifier Template suffix2=$Template_file_suffix"

      # If not a comment and not a blank line
      if [ ! "${PartNumber:0:1}" = "#" ] && [ ! "$PartNumber" = "" ] ; then

         Value="$Voltage, $Features"

         FileNamePrefix=$PartNumber
         # IF not including we don't want underscores in the file name
         if $include_template; then
           FileNamePrefix=$FileNamePrefix"_"$Prefix
         fi

         FileNamePrefix=$FileNamePrefix"_"$Voltage"_"$Features

         if $include_package; then
            FileNamePrefix=$FileNamePrefix"_"$Package
         fi

         if [ "$Template_file_modifier" = "" ] ; then
           OutputFile=$FileNamePrefix$Extension
           cat $TemplateFile | sed -e s/"PART_NUMBER"/"$PartNumber"/g >$OutputFile.tp1
           cat $OutputFile.tp1 | sed -e s/"unknown"/"$FootPrint"/g >$OutputFile.tp2
           cat $OutputFile.tp2 | sed -e s/"COMPONENT_VALUE"/"$Value"/g >$OutputFile
           vecho "Mode 1: Created symbol:$OutputFile"
         else
           OutputFile=$FileNamePrefix
           if [ ! "$Template_file_suffix" = "" ] ; then
             TmpFileName=$TemplateFile"-"$Template_file_modifier"-"$Template_file_suffix$Extension
             if $include_modifier ; then
               OutputFile=$OutputFile"_"$Template_file_modifier"-"$Template_file_suffix
             else
               OutputFile=$OutputFile"-"$Template_file_suffix
             fi
           else
             TmpFileName=$TemplateFile"-"$Template_file_modifier$Extension
             if $include_modifier; then
               OutputFile=$FileNamePrefix"-"$Template_file_modifier
             fi
           fi

           OutputFile=$OutputFile$Extension

           if [ -f $TmpFileName ] ; then
             cat $TmpFileName    | sed -e s/"PART_NUMBER"/"$PartNumber"/g >$OutputFile.tp1
             cat $OutputFile.tp1 | sed -e s/"unknown"/"$FootPrint"/g >$OutputFile.tp2
             cat $OutputFile.tp2 | sed -e s/"COMPONENT_VALUE"/"$Value"/g >$OutputFile
             vecho "Mode 2:Created symbol:$OutputFile"
           else
              echo "Did not find template file:$TmpFileName"
              #could / should check for scond templates here because maybe we are suppose to
              # make these and not the first set
           fi
         fi
         # ------ Remove the tmp files
         if [ -f $OutputFile.tp1 ]; then
           rm -f $OutputFile*.tp1
         fi
         if [ -f $OutputFile.tp2 ]; then
           rm -f $OutputFile*.tp2
         fi
      fi
   done 3< <(cat $ParameterFile)
}

do_Reconciliation() {

  # clean command-line based on set parameters
  if $VERBOSE ; then shift ;fi
  if $QUIET ; then shift ;fi
  if $include_package ; then shift ;fi
  if $include_template ; then shift ;fi
  if $include_modifier ; then shift ;fi

  # Check the Template Arugment
  if [ -f $1 ] ; then
    TemplateFile="$1"
  else
    if [ -f "$1.sym" ] ; then
      TemplateFile="$1.sym"
      vecho "Using Template File: $TemplateFile"
    else
      files=$(ls $1*.sym 2>/dev/null)
      if [ ! -z "$files" ]; then
        # Assume for now that is the is symbol file exist
        TemplateFile="$1"
        vecho "Using $TemplateFile for the symbol template file name"
      fi
    fi
  fi

  if [ ! "$TemplateFile" = "" ] ; then
    shift
    Prefix=`echo "${TemplateFile}" | cut -d'-' -f1` ; # trim away all but first word
  else
    echo "Template File not found: $TemplateFile"
    do_show_usage
    return $ERR_BAD_ARGS
   fi

  vecho "Trimmed $Prefix from the symbol name to use in the output file name"

  if [ -f $1 ] ; then
    ParameterFile="$1"
    shift ;
  else
    if [  -f "$1.spm" ] ; then
      ParameterFile="$1.spm"
    else
      echo "Parameter File not found: $ParameterFile"
      do_show_usage
      return $ERR_BAD_ARGS
    fi
  fi

  return 0;
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-> MAIN <-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ----------------- Check for Empty Command Line ------------------
if [ "$#" -lt $MINARGS ] ; then do_show_usage; exit $ERR_BAD_ARGS ; fi

# ----------------------- Parse Command Line ----------------------
do_Assimilate_Arguments $*

if [ $? != 0 ]; then exit $?; fi

do_Reconciliation $*

if [ $? != 0 ]; then exit $?; fi

do_make_symbols


















