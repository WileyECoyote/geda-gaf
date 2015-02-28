#! /usr/bin/env python

# Set Visibility of Attributes (For now just to Defaults)

import os, sys, glob
from geda import geda
from geda.constants import *

#---------------------------------------------------------------------------
Version="0.1.0"
#---------------------------------------------------------------------------
VerboseMode=False
#---------------------------------------------------------------------------

#  This is the help string.
Usage =\
"""
gsym-snap-grid -- A gEDA-gaf Symbol File Utility
                  Round X-Y coordinates of attributes in a symbol file
                  to multibles of 25 or a specified value.

Usage: gsym-snap-grid [Options] -i <inputfile> [[-o] <outputfile> ]
"""

Help =\
"""
Options:

  -R, --Recursive -- Process all symbol files in the current and all subordinated directories.
  -v, --verbose   -- Verbose mode.  Used in both archive and extract mode.
                     Spews lots of info about what the prog is doing.

  The following options can be used to modify the default behavior of gsym-snap-grid

  -u, --up   Force rounding down, defaults to nearest.
  -d, --down Force rounding up, defaults to nearest.

  -m # Round to moduli specified by #, default is 25.
  -x   Only round X coordinates, mutually exclusive with -y
  -y   Only round Y coordinates, mutually exclusive with -x

Example: Snap all attribute coordinates values in all symbol files in the current
         directory and in all subdirectories to the nearest multiple of 50 and
         report modified values:

    gsym-snap-grid.py --verbose -R -m 50

Copyright (C) 2015 by Wiley Edward Hill. Released under GPL Version 3.

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

        self.rnd_up   = False
        self.rnd_down = False
        self.fix_x    = True
        self.fix_y    = True
        self.moduli   = 25

        if len(sys.argv) > 1:

            mflag = False

            for arg in sys.argv[1:]:       # Skip OurSelf

                if mflag:
                    self.moduli = int(arg)
                    mflag = False
                    continue

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

                if arg in ('-R', '--Recursive'):
                    self.RecursiveMode = True
                    continue

                if arg in ('-d', '--down'):
                    self.rnd_down = True
                    continue

                if arg in ('-u', '--up'):
                    self.rnd_up = True
                    continue

                if arg in ('-x', '-X'):
                    self.fix_y = False
                    continue

                if arg in ('-y', '-Y'):
                    self.fix_x = False
                    continue

                # The set to 10 points group
                if arg in ('-m', '-M', '--moduli'):
                    mflag = True
                    continue

                self.InputFiles.append(arg)

#---------------------------------------------------------------------------
def round_down(num, divisor):
    return num - (num%divisor)
#---------------------------------------------------------------------------
def round_up(num, divisor):
    return num + (num%divisor)
#---------------------------------------------------------------------------

def SnapText(Options, Text):
    modified = False
    if Options.fix_x:
        if not Text.x % Options.moduli == 0:
            modified = modified + 1
            low  = Text.x - Text.x % Options.moduli
            high = Text.x + Options.moduli - Text.x % Options.moduli
            if Options.VerboseMode:
                 print "Text: " + Text.string + " " + str(Text.x) + " is off X grid"
            if Options.rnd_down:
                Text.x = low
            elif Options.rnd_up:
                Text.x = high
            elif Text.x - low <= high - Text.x:
                Text.x = low
            else:
                Text.x = high

    if Options.fix_y:
        if not Text.y % Options.moduli == 0:
            modified = modified + 1
            low  = Text.y - Text.y % Options.moduli
            high = Text.y + Options.moduli - Text.y % Options.moduli
            if Options.VerboseMode:
                 print "Text: " + Text.string + " " + str(Text.y) + " is off Y grid"
            if Options.rnd_down:
                Text.y = low
            elif Options.rnd_up:
                Text.y = high
            elif Text.y - low <= high - Text.y:
                Text.y = low
            else:
                Text.y = high

    if modified and Options.VerboseMode:
        print "New postion: (" + str(Text.x) + "," + str(Text.y) + ")"

    return modified

#---------------------------------------------------------------------------

def ProcessSymbol(Options, File):

    symbol = geda.open_page(File)

    if Options.VerboseMode:
        print "Processing: " + symbol.filename()

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
            if SnapText(Options, object):
                modified = modified + 1

    if modified:
        if Options.VerboseMode:
            print "Made " + str(modified) + " modifications to " + symbol.filename()
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

	foundfiles = False
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
		foundfiles=True
		for file in files:
			InputFiles.append(file)

	if foundOurSelf and len(InputFiles) == 0:
		print 'No symbols found or specifed, try using --help'
	elif not foundfiles :
		if len(InputFiles) > 1:
			for file in InputFiles:
				if not os.path.isfile(file):
					print "Bad file name or option: " + file
					print Usage
	else:
		for symbol in InputFiles:
			ProcessSymbol(Options, symbol)

if __name__ == "__main__":
   main(sys.argv[1:])

# Done!

