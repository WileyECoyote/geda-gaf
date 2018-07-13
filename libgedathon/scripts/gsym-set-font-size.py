#! /usr/bin/env python

# Set Visibility of Attributes (For now just to Defaults)

import os, sys, glob
from geda import geda
from geda.constants import *

#---------------------------------------------------------------------------
Version="0.2.0"
#---------------------------------------------------------------------------
VerboseMode=False
#---------------------------------------------------------------------------

#  This is the help string.
Usage =\
"""
gsym-set-font-size -- A gEDA-gaf Symbol File Utility
                      to set the font size of Text attributes in a symbol file
                      to default values unless otherwise specified

Usage: gsym-set-font-size [Options] -i <inputfile> [[-o] <outputfile> ]
"""

Help =\
"""
Options:

  -r, --recursive -- Process all symbol files in the current and all subordinated directories.
  -v, --verbose   -- Verbose mode.  Used in both archive and extract mode.
                     Spews lots of info about what the program is doing.

  The following options can be used to modify the default behavior of gsym-set-font-size
  The following list of attribute flags correspond to attributes set to 10 points by default,
  if these options are used the corresponding attributes will not be modified:

  -D, --device
  -R, --refdes
  -A, --author
  -U, --numslots
  -E, --description
  -C, --comment
  -N, --net
  -M, --netname
  -V, --value
  -L, --pinlabel

  The following list of attribute flags correspond to attributes set to 8 points by default,
  if these options are used the corresponding attributes will not be modified:

  -q, --pinseq
  -p, --pinnumber
  -t, --pintype

  The following list of attributes are set to 10 points, and no option is provided to
  change this behavior:

  "documentation", "symversion", "dist-license", "use-license", "file", "footprint",
  "footprints", "graphical", "pins", "slot", "slotdef", "spice-type", "electtype",
  "mechtype", "model-name"

Example: Set the font size of all attributes to default values in all symbol files
         in the current directory and in all subdirectories:

         gsym-set-font-size.py -r

Copyright (C) 2014-2015 by Wiley Edward Hill.  Released under GPL Version 2.

"""

#-------------------------------- Classes ----------------------------------
class ProgramParameters:
    """
    This class holds info about the environment and cmd line args passed to the
    program.  It has only one method:  the constructor, which gets the args
    and fills out the public vars.  The public vars are:
    """
    def __init__(self):

        global Version

        """
        Constructor: parse through cmd line args and fill out vars. The values
        here are the defaults
        """
        self.RecursiveMode   = False
        self.VerboseMode     = False
        self.InputFiles      = []
        self.UserDir         = os.path.abspath(os.getcwd())

        self.fix_device      = True
        self.fix_refdes      = True
        self.fix_author      = True
        self.fix_numslots    = True
        self.fix_description = True
        self.fix_comment     = True
        self.fix_net         = True
        self.fix_netname     = True
        self.fix_value       = True
        self.fix_pinlabel    = True

        self.fix_pinseq      = True
        self.fix_pinnumber   = True
        self.fix_pintype     = True

        if len(sys.argv) > 1:
            for arg in sys.argv[1:]:       # Skip OurSelf
                if arg in ('-V', '--version'):
                    print Version
                    sys.exit(0)
                if arg in ('-h', '--help'):
                    print Usage
                    print Help
                    sys.exit(0)
                if arg in ('-v', '--verbose'):
                    self.VerboseMode = True
                    continue
                if arg in ('-r', '--recursive'):
                    self.RecursiveMode = True
                    continue

                # The set to 10 points group
                if arg in ('-D', '--device'):
                    self.fix_device = False
                    continue
                if arg in ('-R', '--refdes'):
                    self.fix_refdes = False
                    continue
                if arg in ('-A', '--author'):
                    self.fix_author = False
                    continue
                if arg in ('-U', '--numslots'):
                    self.fix_numslots = False
                    continue
                if arg in ('-E', '--description'):
                    self.fix_description = False
                    continue
                if arg in ('-C', '--comment'):
                    self.fix_comment = False
                    continue
                if arg in ('-N', '--net'):
                    self.fix_net = False
                    continue
                if arg in ('-M', '--netname'):
                    self.fix_netname = False
                    continue
                if arg in ('-V', '--value'):
                    self.fix_value = False
                    continue
                if arg in ('-L', '--pinlabel'):
                    self.fix_pinlabel  = False
                    continue

                # The set to 8 points group

                if arg in ('-q', '--pinseq'):
                    self.fix_symversion = False
                    continue
                if arg in ('-p', '--pinnumber'):
                    self.fix_pinnumber = False
                    continue
                if arg in ('-t', '--pintype'):
                    self.fix_pintype = False
                    continue

                self.InputFiles.append(arg)


#---------------------------------------------------------------------------
# The ALWAYS turn OFF goup
def AlwaysSet10(Options, Text):

    list = [ "documentation", "symversion", "dist-license", "use-license", "file",
             "footprint", "footprints", "graphical", "pins", "slot", "slotdef",
             "spice-type", "electtype", "mechtype", "model-name"]

    status = 0
    string = Text.name()
    for name in list:
        if name == string:
            status = 1
            if not Text.size == 10:
                Text.size = 10
                status = 2
            break

    return status

#---------------------------------------------------------------------------

# The Set to 8 points group
def SetTextSize8 (Options, Text):
    if Text.name() == "pinseq":
        status = 1
        if Options.fix_pinseq and not Text.size == 8:
            status = 2
    elif Text.name() == "pinnumber":
        status = 1
        if Options.fix_pinnumber and not Text.size == 8:
            status = 2
    elif Text.name() == "pintype":
        status = 1
        if Options.fix_pintype and not Text.size == 8:
            status = 2
    else:
        status = 0

    if status == 2:
        Text.size = 8

    return status

#---------------------------------------------------------------------------
# The Set to 10 points group
def SetTextSize10 (Options, Text):
    if Text.name() == "device":
        status = 1
        if Options.fix_device and not Text.size == 10:
            status = 2
    elif Text.name() == "refdes":
        status = 1
        if Options.fix_refdes and not Text.size == 10:
            status = 2
    elif Text.name() == "author":
        status = 1
        if Options.fix_author and not Text.size == 10:
            status = 2
    elif Text.name() == "numslots":
        status = 1
        if Options.fix_numslots and not Text.size == 10:
            status = 2
    elif Text.name() == "description":
        status = 1
        if Options.fix_description and not Text.size == 10:
            status = 2
    elif Text.name() == "comment":
        status = 1
        if Options.fix_comment and not Text.size == 10:
            status = 2
    elif Text.name() == "net":
        status = 1
        if Options.fix_net and not Text.size == 10:
            status = 2
    elif Text.name() == "netname":
        status = 1
        if Options.fix_netname and not Text.size == 10:
            status = 2
    elif Text.name() == "value":
        status = 1
        if Options.fix_value and not Text.size == 10:
            status = 2
    elif Text.name() == "pinlabel":
        status = 1
        if Options.fix_pinlabel and not Text.size == 10:
            status = 2
    else:
        status = 0

    if status == 2:
        Text.size = 10

    return status

#---------------------------------------------------------------------------

def AnalyzeText(Options, Text):
    modified = False
    string = Text.string
    index = string.find("=")
    if index > 0:
        modified = SetTextSize10(Options, Text)
        if not modified:
            modified = SetTextSize8(Options, Text)
            if not modified:
                modified = AlwaysSet10(Options, Text)
        modified = modified - 1

    return modified

#---------------------------------------------------------------------------

def ProcessSymbol(Options, File):

    symbol = geda.open_page(File)

    if Options.VerboseMode:
        print "Processing: " + symbol.filename

    objects  = geda.get_objects(symbol)
    modified = 0
    """
      Loop thru all objects and get list of detached text attribute then loop
      again and skip if in list, Sub object (attrinutes) are to be handled by
      the objects. This is so that detached text gets added to the symbol sym
      groups.
    """
    for capsule in objects:
        object = geda.get_object(capsule)          # Retrieve object from GedaCapsule
        if capsule.type == OBJ_TEXT:
            result = AnalyzeText(Options, object)
            if result > 0:
                modified = modified + 1

    if modified:
        if Options.VerboseMode:
            print "Made " + str(modified) + " modifications to " + symbol.filename
        symbol.save() # Write the symbol to storage

    # Close the file
    symbol.close()

#---------------------------------------------------------------------------

def RecursiveGlob(path, *exts):
	""" Glob recursively a directory and all subdirectories for multiple file extensions
    Note: Glob is case-insensitive, i. e. for '\*.jpg' you will get files ending
    with .jpg and .JPG

    Parameters
    ----------
    path : str
        A directory name
    exts : tuple
        File extensions to glob for

    Returns
    -------
    files : list
        list of files matching extensions in exts in path and subfolders
	"""
	dirs = [a[0] for a in os.walk(path)]
	f_filter = [d+e for d in dirs for e in exts]
	return [f for files in [glob.iglob(files) for files in f_filter] for f in files]

#---------------------------------------------------------------------------
#  Main prog begins here

def main(argv):

	foundOurSelf = False

	Options = ProgramParameters()  #  Creates Options object for program varibles.

	InputFiles = Options.InputFiles

	files = []
	if len(InputFiles) == 0:
		# Get list of file OurSelf
		foundOurSelf = True
		if Options.RecursiveMode:
			files = RecursiveGlob(os.getcwd(), '/*.sym')
		else:
			files = glob.glob('*.sym')

	if len(files) > 0:
		for file in files:
			InputFiles.append(file)

	if len(InputFiles) == 0:
		print 'No symbols found or specifed, try using --help'
	elif not foundOurSelf :
		for file in InputFiles:
			if not os.path.isfile(file):
				print "Bad file name or option: " + file
				print Usage
			else:
				ProcessSymbol(Options, file)

	else:
		for symbol in InputFiles:
			ProcessSymbol(Options, symbol)

if __name__ == "__main__":
   main(sys.argv[1:])

# Done!

