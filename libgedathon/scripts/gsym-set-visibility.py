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
gsym-set-visibility -- A gEDA-gaf Symbol File Utility
                       to set the Visibility of Text attributes in a symbol file
                       to default values unless otherwise specified

Usage: gsym-set-visibility [Options] -i <inputfile> [[-o] <outputfile> ]
"""

Help =\
"""
Options:

  -r, --Recursive -- Process all symbol files in the current and all subordinated directories.
  -v, --verbose   -- Verbose mode.  Used in both archive and extract mode.
                     Spews lots of info about what the prog is doing.

  The following options can be used to modify the default behavior of gsym-set-visibility.py
  The following list of attribute flags correspond to attributes turned ON by default,
  if these options are used the corresponding attributes will not be modified:

  -R, --refdes    ON + SHOW-VALUE
  -p, --pinnumber ON + SHOW-VALUE
  -L, --pinlabel  ON + SHOW-VALUE

  The following list of attribute flags correspond to attributes turned OFF by default,
  if these options are used the corresponding attributes will not be modified:

  -a, --author
  -n, --numslots
  -d, --device
  -e, --description
  -o, --documentation
  -s, --symversion
  -l, --dist-license
  -u, --use-license
  -t, --pintype

   The following list of attribute flags correspond to attributes turned OFF by default
   if the SHOW-VALUE only is NOT set. If these options are used the corresponding attributes
   will not be modified:

  -C, --comment
  -F, --footprint
  -N, --net
  -E, --netname
  -V, --value
  -M, --model-name

  The following list of attributes are ALWAYS turned OFF, and no option is provided to
  change this behavior:

  "file", "footprints", "graphical", "pins", "slot", "slotdef", "spice-type",
  "pinseq", "electtype", "mechtype"

Example: Set visibility of all attributes in two symbols to default values:

    gsym-set-visibility.py -v MyOpamp.sym MyMicroController.sym

Example: Set the visibility of all attributes except the pin labels to default values
         in all symbol files in the current directory and in all subdirectories:

    gsym-set-visibility.py -r -L

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
        self.fix_pinnumber   = True
        self.fix_pinlabel    = True

        self.fix_author        = True
        self.fix_numslots      = True
        self.fix_description   = True
        self.fix_documentation = True
        self.fix_symversion    = True
        self.fix_dist_license  = True
        self.fix_use_license   = True
        self.fix_pintype       = True

        self.fix_comment       = True
        self.fix_footprint     = True
        self.fix_net           = True
        self.fix_netname       = True
        self.fix_value         = True
        self.fix_model_name    = True

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

                # The turn on (and set SHOW-VALUE) group
                if arg in ('-d', '--device'):
                    self.fix_device = False
                    continue
                if arg in ('-R', '--refdes'):
                    self.fix_refdes = False
                    continue
                if arg in ('-p', '--pinnumber'):
                    self.fix_pinnumber = False
                    continue
                if arg in ('-L', '--pinlabel'):
                    self.fix_pinlabel  = False
                    continue

                # The turn OFF goup
                if arg in ('-a', '--author'):
                    self.fix_author = False
                    continue
                if arg in ('-n', '--numslots'):
                    self.fix_numslots = False
                    continue
                if arg in ('-e', '--description'):
                    self.fix_description = False
                    continue
                if arg in ('-o', '--documentation'):
                    self.fix_documentation = False
                    continue
                if arg in ('-s', '--symversion'):
                    self.fix_symversion = False
                    continue
                if arg in ('-l', '--dist-license'):
                    self.fix_dist_license = False
                    continue
                if arg in ('-u', '--use-license'):
                    self.fix_use_license = False
                    continue
                if arg in ('-t', '--pintype'):
                    self.fix_pintype = False
                    continue

                # The turn OFF if the SHOW-VALUE is not set goup
                if arg in ('-C', '--comment'):
                    self.fix_comment = False
                    continue
                if arg in ('-F', '--footprint'):
                    self.fix_footprint = False
                    continue
                if arg in ('-N', '--net'):
                    self.fix_net = False
                    continue
                if arg in ('-E', '--netname'):
                    self.fix_netname = False
                    continue
                if arg in ('-V', '--value'):
                    self.fix_value = False
                    continue
                if arg in ('-M', '--model-name'):
                    self.fix_model_name = False
                    continue

                self.InputFiles.append(arg)


#---------------------------------------------------------------------------
# The ALWAYS turn OFF goup
def AlwaysTextOff(Options, Text):

    list = [ "file", "footprints", "graphical", "pins", "slot", "slotdef", "spice-type", "pinseq", "electtype", "mechtype"]

    status = 0
    string = Text.name()
    for name in list:
        if name == string:
            status = 1
            if Text.visible == 1:
                Text.visible = 0
                status = 2
            if name == "pinseq" and not Text.show == SHOW_VALUE:
                Text.show = SHOW_VALUE
                status = 2
            break

    return status

#---------------------------------------------------------------------------

# The turn OFF if the SHOW-VALUE is not set goup
def NotValueTextOff(Options, Text):
    if Text.name() == "comment":
        status = 1
        if Options.fix_comment and Text.visible == 1:
            status = 2
    elif Text.name() == "footprint":
        status = 1
        if Options.fix_footprint and Text.visible == 1:
            status = 2
    elif Text.name() == "net":
        status = 1
        if Options.fix_net and Text.visible == 1:
            status = 2
    elif Text.name() == "netname":
        status = 1
        if Options.fix_netname and Text.visible == 1:
            status = 2
    elif Text.name() == "value":
        status = 1
        if Options.fix_value and Text.visible == 1:
            status = 2
    elif Text.name() == "model-name":
        status = 1
        if Options.fix_model_name and Text.visible == 1:
            status = 2
    else:
        status = 0

    if status == 2:
        if Text.show == SHOW_VALUE:
            status = 1
        else:
            Text.visible = 0

    return status
#---------------------------------------------------------------------------

# The turn off (and set SHOW-NAME-VALUE) group
def TurnOffText(Options, Text):
    if Text.name() == "author":
        status = 1
        if Options.fix_author:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "numslots":
        status = 1
        if Options.fix_numslots:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "device":
        status = 1
        if Options.fix_device:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "description":
        status = 1
        if Options.fix_description:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "documentation":
        status = 1
        if Options.fix_documentation:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "symversion":
        status = 1
        if Options.fix_symversion:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "dist-license":
        status = 1
        if Options.fix_dist_license:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "use-license":
        status = 1
        if Options.fix_use_license:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_NAME_VALUE:
                status += 3
                show = SHOW_NAME_VALUE
    elif Text.name() == "pintype":
        status = 1
        if Options.fix_pintype:
            if Text.visible == 1:
                status = 2
            if not Text.show == SHOW_VALUE:
                status += 3
                show = SHOW_VALUE

    else:
        status = 0

    if status > 1:
        if status == 2:
            Text.visible = 0
        else:
            if status == 4:
                Text.show = show
            if status == 5:
                Text.visible = 0
                Text.show = show
            status =2

    return status

#---------------------------------------------------------------------------
# The turn on (and set SHOW-VALUE) group
def TurnOnText(Options, Text):
    if Text.name() == "refdes":
        status = 1
        if Options.fix_refdes and Text.visible == 0:
            status = 2
    elif Text.name() == "pinnumber":
        status = 1
        if Options.fix_pinnumber and Text.visible == 0:
            status = 2
    elif Text.name() == "pinlabel":
        status = 1
        if Options.fix_pinlabel and Text.visible == 0:
            status = 2
    else:
        status = 0

    if status == 2:
        Text.visible = 1
        Text.show    = SHOW_VALUE

    return status

#---------------------------------------------------------------------------

def AnalyzeText(Options, Text):
    modified = False
    string = Text.string
    index = string.find("=")
    if index > 0:
        modified = TurnOnText(Options, Text)
        if not modified:
            modified = TurnOffText(Options, Text)
            if not modified:
                modified = NotValueTextOff(Options, Text)
                if not modified:
                    modified = AlwaysTextOff(Options, Text)
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
            if AnalyzeText(Options, object):
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

