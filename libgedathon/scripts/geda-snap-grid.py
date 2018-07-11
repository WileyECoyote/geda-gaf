#! /usr/bin/env python

# Set Visibility of Attributes (For now just to Defaults)

import os, sys, glob
from geda import geda
from geda.constants import *

#---------------------------------------------------------------------------
Version="0.4.1"
#---------------------------------------------------------------------------
VerboseMode=False
#---------------------------------------------------------------------------

#  This is the help string.
Usage =\
"""
geda-snap-grid -- A gEDA-gaf Schematics and Symbol cleaning utility

\tRound X-Y coordinates of attributes in schematic and symbol files to
\tmultiples of 25 or a specified value.

\tBy default, geda-snap-grid operates on both schematic and symbol files
\tunless the file names are specified on the command line, either with the
\t--input <inputfile> option or just listing the files. For both schematic
\tand symbol files, the extension must be specified when the names of files
\tare specified.

Usage: geda-snap-grid [Options] -i <inputfile> [[-o] <outputfile> ] [list of files]
"""

Help =\
"""
Options:

  The following options can be used to modify the default behavior of geda-snap-grid

  -R, --Recursive --  Process all files in the current and all subordinated directories.
  -v, --verbose   --  Verbose mode, spews lots of info about what the prog is doing.

  -a, --append <path> When processing schematic files this option can be used to specify
                      addition directories to be appended to the libraries search path.
                      This option is not needed for directories with the name "sym", if
                      such a directory exist, the directory will be appended automatically.

  -n, --no-path   --  By default, if a directory exist with the name "sym", then the
                      directory is appended to the libraries search path when processing
                      schematic files, this option inhibits this behavior.

  -c, --sch       --  Only process schematic files, ignored if a file name or names are specified.
  -s  --sym       --  Only process symbol files, ignored if a file name or names are specified.

  -u, --up   Force rounding up, defaults to nearest.
  -d, --down Force rounding down, defaults to nearest.

  -m # Round to moduli specified by #, default is 25.
  -x   Only round X coordinates, mutually exclusive with -y
  -y   Only round Y coordinates, mutually exclusive with -x

Example: Snap all attribute coordinates values in all files in the current
         directory and in all subdirectories to the nearest multiple of 50 and
         report modified values:

    geda-snap-grid.py --verbose -R -m 50

Copyright (C) 2015 by Wiley Edward Hill. Released under GPL Version 2.

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
        self.OnlySchematics  = False
        self.OnlySymbols     = False
        self.AutoAppendPath  = True
        self.InputFiles      = []
        self.InputFileName   = ""
        self.OutputFileName  = ""
        self.Path2Symbols    = ""
        self.UserDir         = os.path.abspath(os.getcwd())

        self.rnd_up   = False
        self.rnd_down = False
        self.fix_x    = True
        self.fix_y    = True
        self.moduli   = 25

        if len(sys.argv) > 1:

            mflag = False
            iflag = False
            oflag = False
            pflag = False

            for arg in sys.argv[1:]:       # Skip OurSelf

                if mflag:
                    self.moduli = int(arg)
                    mflag = False
                    continue

                if iflag:
                    self.InputFileName = arg
                    iflag = False
                    continue

                if oflag:
                    self.OutputFileName = arg
                    oflag = False
                    continue

                if pflag:
                    self.Path2Symbols = arg
                    pflag = False
                    continue

                if arg in ('-V', '--version'):
                    print Version
                    sys.exit(0)

                if arg in ('-h', '--help'):
                    print Usage
                    print Help
                    sys.exit(0)

                if arg == '--usage':
                    print Usage
                    sys.exit(0)

                if arg in ('-i', '--input'):
                    iflag = True
                    continue

                if arg in ('-o', '--output'):
                    oflag = True
                    continue

                if arg in ('-a', '--append'):
                    pflag = True
                    continue

                if arg in ('-n', '--no-path'):
                    self.AutoAppendPath = False
                    continue

                if arg in ('-v', '--verbose'):
                    self.VerboseMode = True
                    continue

                if arg in ('-R', '--Recursive'):
                    self.RecursiveMode = True
                    continue

                if arg in ('-c', '--sch'):
                    self.OnlySchematics = True
                    continue

                if arg in ('-s', '--sym'):
                    self.OnlySymbols = True
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

def ProcessFile(Options, File):

    gedafile = geda.open_page(File)

    if Options.VerboseMode:
        print "Processing: " + gedafile.name()

    objects  = geda.get_objects(gedafile)
    modified = 0
    """
      Loop thru all objects and get list of detached text attribute then loop
      again and skip if in list, Sub object (attrinutes) are to be handled by
      the objects. This is so that detached text gets added to the gedafile
      sym groups.
    """
    for capsule in objects:
        object = geda.get_object(capsule)          # Retrieve object from GedaCapsule
        if capsule.type == OBJ_TEXT:
            if SnapText(Options, object):
                modified = modified + 1

    if modified:
        if Options.OutputFileName:
            OutputFileName = Options.OutputFileName
            gedafile.filename = Options.OutputFileName # Write the file to storage
            print "Using " + OutputFileName + " for output file name"
        else:
            OutputFileName = gedafile.filename

        gedafile.save() # Write the file to storage

        if Options.VerboseMode:
            print "Made " + str(modified) + " modifications to " + OutputFileName

    # Close the file
    gedafile.close()

#---------------------------------------------------------------------------

def RecursiveGlob(path, *exts):
	""" Recursively Glob directory and all subdirectories for multiple file
	extensions. Note: Glob is case-insensitive, i. e. for '\*.sch' you will
	get files ending with .sch and .SCH

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

	# Resolve library search paths per Options
	if not Options.OnlySymbols:
		if Options.AutoAppendPath:
			if os.path.isdir("sym"):
				append_symbol_path("sym");
		if Options.Path2Symbols:
			if os.path.isdir(Options.Path2Symbols):
				append_symbol_path(Options.Path2Symbols);
			else:
				print Options.Path2Symbols + " does not exist"
				exit(1);

	# Rearrange input list if wild-card was used with -i
	if Options.InputFileName and Options.InputFiles:
		Options.InputFiles.insert(0, Options.InputFileName)
		Options.InputFileName=""

	if Options.InputFileName:
		ProcessFile(Options, Options.InputFileName)
	else:

		# Multi-file mode
		if Options.OutputFileName:
			print "output file can not be specified with multiple input files"

		InputFiles = Options.InputFiles

		files = []

		base = os.getcwd()

		if not InputFiles:
			# Get list of files OurSelf
			foundOurSelf = True
			if Options.OnlySymbols:
				if Options.RecursiveMode:
					files = RecursiveGlob(base, '/*.sym')
				else:
					files = glob.glob(os.path.join(base,'*.sym'))
			elif Options.OnlySchematics:
				if Options.RecursiveMode:
					files = RecursiveGlob(base, '/*.sch')
				else:
					files = glob.glob(os.path.join(base,'*.sch'))
			elif Options.RecursiveMode:
				files = RecursiveGlob(base,'/*.sym', '/*.sch')
			else:
				files = glob.glob(os.path.join(base,'*.sym'))
				files.extend(glob.glob(os.path.join(base,'*.sch')))

		if files:
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
				ProcessFile(Options, symbol)

if __name__ == "__main__":
   main(sys.argv[1:])

# Done!
