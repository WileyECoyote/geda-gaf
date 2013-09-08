#!/bin/bash
#  bash-wrapper.sh
#+ command wrapper that performs an operation on gEDA files.

#  gEDA - GPL Electronic Design Automation
#
#  Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
#

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#+ MA 02111-1301, USA

#  This is a Bash Wrapper encapsulating a utility command function. The
#  Wrapper provides system level interfacing for command-line options,
#  logging, I/O Management, debugging, and Error handling. The embedded
#+ function is defined in do_CommandFunction.
#
#+ Authur of embedded Command Utility: Unknown

set -f

# ------------------------- This Command  --------------------------

VER=1.1
TITLE="A script to unannotate gEDA schematics"
UFORMAT='[-options] [[-i] <inputfile[ext]> ] [-o <outputfile[ext]>]'
DEF_EXT="sch"

MINARGS=1          # Script requires no arguments.

# -------------------------- Dependencies --------------------------

# cat and sed

# ------------------------ Wrapper Variables  ----------------------

let ERR_FILE_NOT_FOUND=2
let ERR_FILE_ACCESS=70
let ERR_WRONG_ARGS=71
let ERR_BAD_ARGS=85

let ERR_NUM=0

ErrMessage=""
DEFAULT_LOGFILE="logfile.log"

AutoMode=false
BeQuite=false     # default noise
BeVerbose=false   # toggle babel on off
BatchMode=false
OverWrite=true
SimpleMode=false
DebugMode=false
MakeBackups=true

FILTER="*"
FILE_EXT=""
InputFile=""
OutputFile=""

BUFFERPID=`echo $$`;

declare -a on_exit_items;
declare -a input_items;

# ------------------------- Define General Functions ----------------------

# ------- Script Colorization Module -------

ESCAPE="\033"
ATT_NORMAL=0     # All attributes off (can be omitted )
ATT_BOLD=1       # Bold (appears bright)
ATT_UNDERSCORE=4 # Underscore (monomchrome adapters only!)
ATT_BLINKING=5   # Blinking
ATT_REVERSE=7    # Reverse video - black text on a white background
ATT_HIDDEN=8     # Hidden.

FG_BLACK=30
FG_RED=31
FG_GREEN=32
FG_YELLOW=33
FG_BLUE=34
FG_MAGENTA=35
FG_CYAN=36
FG_WHITE=37

BG_BLACK=40
BG_RED=41
BG_GREEN=42
BG_YELLOW=43
BG_BLUE=44
BG_MAGENTA=45
BG_CYAN=46
BG_WHITE=47

BLACK="$ESCAPE[$ATT_NORMAL;47;;30m\b"
RED="$ESCAPE[$ATT_NORMAL;40;;31m\b"
GREEN="$ESCAPE[$ATT_NORMAL;40;;32m\b"
YELLOW="$ESCAPE[$ATT_NORMAL;40;;33m\b"
BLUE="$ESCAPE[$ATT_NORMAL;40;;34m\b"
MAGENTA="$ESCAPE[$ATT_NORMAL;40;;35m\b"
CYAN="$ESCAPE[$ATT_NORMAL;40;;36m\b"
WHITE="$ESCAPE[$ATT_NORMAL;40;;37m\b"

BOLD='\033[1m'
BOLD_OFF='\033[00m'
NORMAL='\b\E[00m'

alias Reset="tput sgr0" # Reset console text attributes to normal without clearing screen.
alias BoldText="\033[1m$1\033[0m"

# ---------------------- Define Wrapper Functions ------------------

# Colorization from http://tldp.org/LDP/abs/html/colorizing.html - Spectacular!
#
# Function: cecho
#
# Purpose: Colorize text output to console so as to draw attention to
#          critical output and to visually enhance text output.
#
#  Author: Wiley Edward Hill
#
#    Date: 06/23/2112
#
function cecho () {           # Color-echo.
                              # Argument $1 = message
                              # Argument $2 = color
   if [[ $1 = "-n" ]] ; then  # passing no line-feed arguments to "echo"
      opt=-ne
      shift
   else
      opt=-e
   fi
   if [[ $1 = "" ]] ; then   # if no message then just do reset
      echo -ne "\E[0m"       # Reset to normal.
   else
      color=${2:-$WHITE}     # Defaults to white on black, if not specified.
      echo "$opt" "$color" "$1"
      if [ -z $3 ] ; then    # Any 3rd argument means no reset
         echo -ne "\E[0m"    # Reset to normal
      fi
   fi
   return
}

function decho() {           # Display messages only when in Debug mode
   if [[ ! $1 = "" ]] ; then
     if $DebugMode ; then
      echo $1 >&2
     fi
   fi
}


function vecho() {           # Display messages only when in Verbose mode
   if [[ ! $1 = "" ]] && [ -t 1 ] ; then
     if $BeVerbose ; then
      echo $1
     fi
   fi
}

# Display general messages but not in Quite mode unless Verbose mode enabled
function do_scream() {
  if [ -t 1 ] ; then
    if [[ ! $1 = "" ]] ; then
      if $BeVerbose ; then
        echo $*
      elif ! $BeQuite ; then
        echo $*
      fi
    elif [[ ! -z $ErrMessage ]] ; then
       echo "${ErrMessage}" 1>&2;
       ErrMessage=""
    else
       echo "Nothing to babel!"  # The function was called without an argument
    fi
  fi
}

# Format allows for contextual help but is not utilized in this module
function do_show_help() {

   case $1 in
         "1") # Quick help
              echo
              cecho "$TITLE" $BLUE false
              echo
              echo Usage:   `basename $0` "$UFORMAT"
              echo
              cecho ;;

         "2") # Basic help
              echo
              cecho "$TITLE" $GREEN
              echo
              echo Usage:   `basename $0` "$UFORMAT"
              echo
              echo "   -a | --all          Process all schematics in the current folder"
              echo "   -e | --extension    Change file extension of the input files, default=${DEF_EXT}"
              echo "   -f | --filter       Change the file filter, default=${FILTER}"
              echo '   -i | --input        Specify the name of the intput file, default=<show usage>'
              echo '   -o | --output       Specify the name of the output file, default=<input name>'
              echo '   -b | --no-backup    Do not make backup copies of input files'
              echo '   -t | --no-overwrite Do not overwrite any existing output files'
              echo
              echo '   -h | --help         Show this information'
              echo '   -x | --example      Show examples of usage'
              echo '   -u | --usage        Show basic command-line usage'
              echo '   -q | --quite        Surrpress output messages'

              echo '   -v | --verbose      Display extra information'
              echo '        --version      Display version information'
              echo
              echo 'Note: arguments are case sensitive.'
              echo  ;;

         "3") # Alternative syntax
              echo Usage:   `basename $0` '"Alternative syntax"'
              echo
              echo Example 1: `basename $0` '-v myfile  (verbose mode and process myfile)'
              echo Example 2: `basename $0` '-q -a      (No extra output, scan for schematic files)'
              echo Example 3: `basename $0` '--all -f "pnp*" (scan for schematic files being with pnp)'
              echo Example 4: 'cat afile.sch | ' `basename $0` '-q > newfile.sch (quitely pipe input and output)'
              echo Example 5: 'cat afile.sch | ' `basename $0` '| grep refdes (display annotated refdes)'
              echo
              echo ;;
   esac
   return 0;
}

function do_on_exit()
{
   for i in "${on_exit_items[@]}"
   do
      decho "on_exit: $i"
      eval $i
   done
}

function add_on_exit()
{
   local n=${#on_exit_items[*]}
   on_exit_items[$n]="$*"
   if [[ $n -eq 0 ]]; then
      decho "Setting trap"
      trap do_on_exit INT TERM EXIT # Bash pseudo-signals EXIT
   fi
}

function do_on_error()
{
  [ -n "$DebugMode" ] && set -x;
  exit 0;
}

# This is what we're wrapping: (yea, I know, it's a one-liner)

function do_CommandFunction () {

  # Note: We can't just redirect to output directly to $2 because $1 might be $2
  cat "$1" | sed "/refdes=\([A-Za-z][A-Za-z]*\)[0-9][0-9]*/s//refdes=\1?/g" > "$2.tmp"
  mv "$2.tmp" "$2"

}
# ---------------------- Wrapper Management Functions ------------------

function ParseCommandLine() {

  # Phase 1: Loop through all the arguments and set (most) options

   for Arg in $*; do
      case $Arg in
          --all | -a) AutoMode=true                                   ;;
        --debug | -d) DebugMode=true ; BeVerbose=true                 ;;
       --filter | -f) AutoMode=true                                   ;;
         --help | -h) do_show_help 2 ; return 0                       ;;
    --no-backup | -b) MakeBackups=false                               ;;
 --no-overwrite | -t) OverWrite=false                                 ;;
      --verbose | -v) BeVerbose=true                                  ;;
        --quite | -q) BeQuite=true                                   ;;
        --usage | -u) do_show_help 1 ; return 0                       ;;
      --example | -x) do_show_help 3 ; return 0                       ;;
      --version | -V) echo `basename $0` "Version $VER"; return 0     ;;
                    *) case $LAST_ARG in
                    --extension | -x) FILE_EXT="$Arg"                 ;;
                       --filter | -f) FILTER="$Arg"                   ;;
                        --input | -i) InputFile="$Arg" ;
                                      SimpleMode=true                 ;;
                       --output | -o) OutputFile="$Arg"
                                      SimpleMode=true                 ;;
                                   *)                                 ;;
                      esac                                            ;;
      esac
      LAST_ARG=$Arg
   done

 # Phase 2: For Mode > 0, assess mode status & re-evaluate as needed

 if $SimpleMode ; then
   if [ -z $InputFile ] ; then
     echo `basename $0` "Error: Output specified, but not input"
     ERR_NUM=$ERR_BAD_ARGS
     return 0
   fi
   return 1; # One means simple mode
 else
   if ! $AutoMode ; then
     while [ ! $#  = 0 ] ; do
       # if chars* [ argument 1 : starting at 0 : extract 1 char ] != hyphen;
       if [[ ${1:0:1} != '-' ]] ; then
         if [ -f $1 ] ; then
            input_items[ ${#input_items[@]} ]="$1"
         elif [ -f "$1.$DEF_EXT" ] ; then
            input_items[ ${#input_items[@]} ]="$1.$DEF_EXT"
         else
           do_scream "Peculiar sting on command-line: $1, maybe Typo?"
         fi
       fi
       shift
     done;
     if [ ${#input_items[@]} -gt 1 ] ; then
       BatchMode=true
       return 3; # Three means Batch mode
     else
       InputFile=${input_items[0]}
       SimpleMode=true;
       return 1;
     fi
   fi
 fi

 return 2; # Two means Auto mode

}

# Check for arguments when input is piped
function do_AssimulateArguments () {

  while [ ! $#  = 0 ] ; do
    case $1 in
        --debug | -d) DebugMode=true ; BeVerbose=true   ;;
    --no-backup | -b) MakeBackups=false                 ;;
 --no-overwrite | -t) OverWrite=false                   ;;
      --verbose | -v) BeVerbose=true                    ;;
                   *) 					;; # Ostrich approach
    esac
    shift
  done;
  BeQuite=true # This is implicit if piping input
  return 0;     # We should always return zero
}

# Called in all modes after parameters are assimulated
function do_Initialization () {

  if [ -z "$LOGFILE" ] ; then     # If not set, default to ...
    LOGFILE="$DEFAULT_LOGFILE"
  fi

  #trap do_on_error ERR # Bash pseudo-signals ERR

  add_on_exit "rm -fr $BUFFERPID.buf"
  add_on_exit "rm -fr $BUFFERPID.out"

  [ -z "$FILE_EXT" ]  && FILE_EXT="$DEF_EXT";
}

# This is called instead of the CommandFunction when debugging
function do_DummyCommand () {
  if [[ -t 1 ]] && [ -f $BUFFERPID.out ]; then
   cat "$BUFFERPID.out"
    sync
    rm $BUFFERPID.out
    echo  "Gonna process: $1, and pipe to $2"
  else
    echo  "Gonna do Something with: $1 and $2"
  fi
}

function do_Process () {
  if $DebugMode ; then
    do_DummyCommand "${1}" "${1}"
  else
    if $MakeBackups ; then
      cp -u "${1}" "${1}.sav"
    fi
    do_CommandFunction "${1}" "${1}"
  fi
}

# This function is called when multiable input file names were on
# the command-line, like <command> [options] input1 input2 input3 ...
function do_BatchProcessing () {

  set +f                         #**** <----- TURNING EXPANSION ON!
  for f in "${input_items[@]}" ; do
    do_Process "${f}"
  done
  return 0;
}

# This function is call when the --all or --filter option is used
function do_AutoProcessing () {

  vecho "FILTER=$FILTER"
  glob=$FILTER.$FILE_EXT
  set +f                         #**** <----- TURNING EXPANSION ON!
  vecho "glob=$glob"
  for f in ${glob} ; do
    do_Process "${f}"
  done
  return 0;
}

# This is used for SimpleMode and if data is piped (not Auto or Batch)
function do_ResolveOutputFileName () {

  if [[ ! -t 0 ]] || [[ ! -t 1 ]]; then
    OutputFile="$BUFFERPID.out"
  else
    if [[ -z $OutputFile ]] ; then
      if $OverWrite ; then
        OutputFile=$InputFile
      else
        OutputFile= "${InputFile%.[^.]*}.new"
      fi
    else
      # Add the extension to the output file name if is missing
      base="${OutputFile%.[^.]*}"
      extension_out="${OutputFile:${#base} + 1}"
      if [[ -z ${extension_out} ]] ; then
         OutputFile="$OutputFile.$FILE_EXT"
         vecho "writing output to $OutputFile"
      fi
      # if the name already exist and we were directed to not overwrite
      # then change the name
      if [ -f $OutputFile ] && ! $OverWrite ; then
        OutputFile="$base_new.$FILE_EXT"
      fi
    fi
  fi

  decho "_ResolveOutputFileName: OutputFile=<$OutputFile>"
}

# This function establishes the name of the input file, this is not
# used when the input data is streaming.
function do_ResolveFileNames () {

  if [[ -z $InputFile ]] ; then
     do_show_help 1
     return 0;
  elif [ ! -f ${InputFile} ] ; then
     if [ ! -f "$InputFile.$FILE_EXT" ] ; then
       do_scream "Can not find resolve $InputFile, did you type the file name correctly?"
       ERR_NUM=$ERR_FILE_NOT_FOUND
       return 0;
     else
       TmpFileName="$InputFile.$FILE_EXT"
       vecho "resolved input file: $TmpFileName"
     fi
  else
      TmpFileName="$InputFile"
  fi

  decho "_ResolveFileNames: TmpFileName=<$TmpFileName>"
  decho "_ResolveFileNames: InputFile=<$InputFile>"

  InputFile="$TmpFileName"

  do_ResolveOutputFileName

  return 1;
}

# This is used for SimpleMode and if data is piped (not Auto or Batch)
function do_PostProcess () {
  if [[ ! -t 1 ]] || [[ ! -t 0 ]] ; then
    if [ -f $OutputFile ]; then
      cat $OutputFile
      sync
      rm $OutputFile
    else
      echo "Error: <$OutputFile> is missing!"
    fi
  fi
}

# This "is" the SimpleMode handler
function do_ProcessFile () {
  # make a copy of the current schematics
  if [ -t 1 ] && $MakeBackups ; then
    if [ "${InputFile}" = "${OutputFile}" ] ; then
      cp -u $InputFile "$InputFile.sav"
    fi
  fi

  if $DebugMode ; then
    do_DummyCommand "$InputFile" "$OutputFile"
  else
    do_CommandFunction "$InputFile" "$OutputFile"
  fi

  do_PostProcess
}

# This is called to process piped data
function do_ProcessBuffer () {

  if $DebugMode ; then
    do_DummyCommand "$BUFFERPID.buf" "$OutputFile"
  else
    do_CommandFunction "$BUFFERPID.buf" "$OutputFile"
  fi
  do_PostProcess
  rm  $BUFFERPID.buf
}

# This is called to buffer piped data
function do_buffer_input () {
  printf "%s\n" "$1" >> "$BUFFERPID.buf"
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-> MAIN <-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
 
if [[ -t 0 ]] ; then          # Is we data from the terminal?
  if [ ! $# = 0 ] ; then      # Are there any command-line arguments?
    ParseCommandLine $*
    Mode=$?
    do_Initialization         # Setup error handlers
    case $Mode in
        0)                              ;; # is normal exit
        1) do_ResolveFileNames
           if [ ! $? -eq 0 ] ; then
             do_ProcessFile
           fi				;;
    	2) do_AutoProcessing 		;;
 	3) do_BatchProcessing		;;
	*) echo "What?" ; exit 0        ;;
    esac
  else
    do_show_help "1"
  fi
else                          # Input is comming from pipe
  if [ ! $# = 0 ] ; then      # Were there any arguments?
    do_AssimulateArguments $*
    if [ $? = 0 ] ; then
      # shift off all arguments so they don't end up leading stdin
      while [ ! $#  = 0 ] ; do
        shift
      done;
    fi
  fi
  do_Initialization
  while IFS= read -r -t 5 line ; do { # Keep reading from data stream
    if [[ $? -ge 128 ]] ; then
      ErrMessage="Input data pipe timed out, giving up"
      do_scream $ErrMessage    # Does not clear message
      kill $$;
    elif [[ ! -z $line  ]] ; then
      do_buffer_input "$line"
    fi
  }
  done < "${1:-/proc/${$}/fd/0}"
  if [ -f "$BUFFERPID.buf" ] ; then
    do_ResolveOutputFileName     # Find out where data gota go
    do_ProcessBuffer             # Process data in the buffer
  fi
fi

# Write to log if there was an error:
if [[ ! "${ErrMessage}" = "" ]] ; then
  echo "ErrMessage=$ErrMessage"
  echo "`basename $0` + `date` + `whoami` + $ErrMessage "$@"" >> $LOGFILE
fi

# We succeeded, so reset traps.
trap - ERR EXIT INT TERM
exit $ERR_NUM

