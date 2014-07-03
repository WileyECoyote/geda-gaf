#!/bin/sh

# By Braddock Gaskill (braddock@braddock.com), August 2004.  This
# software is hereby declared to be in the public domain by Braddock
# Gaskill, the author.
#

XOFFSET=40000
YOFFSET=33000
#XSCALE=10000
#YSCALE=10000
XSCALE=9000
YSCALE=9000

# ------------------------- Define General Functions ----------------------
vecho()
{
   if [[ ! $1 = "" ]] ; then
     if $Be_Verbose ; then
      echo $1
     fi
   fi
}

do_scream()
{
   if [[ ! $1 = "" ]] ; then
     if $Be_Verbose ; then
      echo $1
     elif ! $Be_Quite ; then
      echo $1
     fi
   else
    echo "Nothing to babel!"    # The function was called without an argument
   fi
}

do_show_help() 
{
   case $1 in
         "1") # Quick help
              echo
              cecho "A script to generate HTML files for PCB footprint Libraries" $BLUE false
              echo
              echo Usage:   `basename $0` '[-options] [[-l] Library]'
              echo
              cecho ;;
 
         "2") # Basic help 
              echo
              cecho "A script to generate HTML files for PCB footprint Libraries" $GREEN
              echo
              echo Usage:   `basename $0` '[-options] [[-l] Library]'
              echo
#             echo '   -s | --sdd          add package to repository'
              echo '   -e | --extension    Change file extension of the HTML files, default="html"'
              echo '   -f | --filter       Change the file filter, default="*.fp"'
              echo '   -n | --no-overwite  Do not overwrite any of the existing html files'
              echo '   -o | --output       Set the name of the HTML files, default="index.html"'
              echo '   -l | --library      Set the name of the library, default is to scan for "xxx.html"'
              echo
              echo '   -h | --help         Show this information'
              echo '   -u | --usage        Show basic command-line usage'
              echo '   -v | --verbose      Display extra information'
              echo '        --version      Display version information'
              echo
              echo Example 1: `basename $0` '-v stdlib  (verbose mode and process stdlib folder)'
              echo Example 2: `basename $0` '-q         (No extra output, scan for library folder)'
              echo
              echo 'Note: arguments are case sensitive.'
              echo  ;;
         "3") # Alternative syntax
              echo Usage:   `basename $0` '"plain english statements go here!"'
              echo
              echo 'Example 1 (formal explicit):' `basename $0` 'add package icedtea to distribution wheezy)'
              echo 'Example 2 (slang implicit):' `basename $0` ' remove banshee from natty'
              echo 'Example 2 (explicit):' `basename $0` 'list all packages in sid'
              echo
              echo ;;
   esac
}

FNAME="$1"
if [ -z "$FNAME" ]; then
    cat << EOF

$0 <inputfile.sch>

This script will read a gschem schematic and attempt to
extract the relative positions of the components in the schematic,
and generate corresponding MOVE instructions for Eagle.  You will
likely have to adjust XOFFSET, YOFFSET, XSCAL, and YSCALE at the
top of the script to obtain usable positions.

By Braddock Gaskill (braddock@braddock.com), August 2004

EOF
    exit -1
fi


tmpdir=/tmp/$$
mkdir -m 0700 -p $tmpdir
rc=$?
if test $rc -ne 0 ; then
	cat << EOF

$0: ERROR -- Failed to create $tmpdir with 0700 permissions.  mkdir returned $rc.

Make sure that $tmpdir does not already exist and that you have permissions to 
create it.

EOF
	exit 1
fi
tmpf=${tmpdir}/tmpf
grep -B1 refdes= "$FNAME" |sed 's/=/ /' | cut -d" " -f2,3 |grep -v '^--' >${tmpf}


while read; do
    # the directory on the client to backup
    X=`echo $REPLY | cut -d' ' -f1`
    Y=`echo $REPLY | cut -d' ' -f2`
    read;
    PART="$REPLY"
    X=`echo "scale=5; ($X - $XOFFSET) / $XSCALE" |bc`
    Y=`echo "scale=5; ($Y - $YOFFSET) / $YSCALE" |bc`
    echo "MOVE '$PART' ($X $Y);"
done < $tmpf
rm -fr "${tmpdir}"

