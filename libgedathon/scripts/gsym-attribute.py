#! /usr/bin/env python

# Set Visibility of Attributes (For now just to Defaults)

import os, sys, re, getopt, glob
from geda import geda
from geda.constants import *

#---------------------------------------------------------------------------
Version="0.2.2"
#---------------------------------------------------------------------------
VerboseMode=False
#---------------------------------------------------------------------------

#  This is the help string.
Usage =\
"""
gsym-attribute -- A gEDA-gaf Symbol File Attribute Manipulator Utility
                  to set or retrieve the value of a Text attribute in a
                  symbol file

Usage: gsym-attribute [Options] [-a] attribute -i <inputfile> [[-o] <outputfile> ]
"""

Help =\
"""
Options:

  -a, --attribute -- The Attribute to be queried or set
  -u, --value     -- Optional new value for the attribute
  -f, --force     -- When the force option is specified any existing file matching
                     the output name will be over-written, without any warning.
  -c, --color     -- Optional attribute color.
  -d, --hidden    -- Set the visibility flag to invisible When adding an attribute.
  -s, --size      -- Optional font size.
  -q, --quite     -- Suppress normal processing messages.
  -v, --verbose   -- Spews lots of info about what the prog is doing.

  -i, --input  <FILE> -- Specifies the name of the input sym file.

  -o, --output <FILE> -- Specifies the name of the output sym file. When the
                         output file name is not specified the input file will
                         be over written.

Example: Set the footprint in all symbol files in the current directory to SO8:

	        gsym-attribute.py -a footprint -u SO8

Example: Set the author in files in 24LC16BIMC-1.sym 24LC16BEMC-1.sym to Jethro Bodine

            gsym-attribute.py -v -a author -u "Jethro Bodine" 24LC16BIMC-1 24LC16BEMC-1

Example: Get the value of slotdef in the file dual-opamp-3.sym

	        gsym-attribute.py slotdef dual-opamp-3

Copyright (C) 2014-2015 by Wiley Edward Hill.  Released under GPL Version 2.

"""
#------------------------ Functions used by Classes ------------------------

def VMessage(Options, String):
    """
    This prints out String when the -v flag is set, otherwise is silent
    """
    if (Options.VerboseMode == "verbose"):
        print(String)
    return

#---------------------------------------------------------------------------

def QMessage(Options, String):
    """
    This prints out String when the -v flag is set, otherwise is silent
    """
    if Options.VerboseMode == "verbose" or not Options.QuiteMode:
        print(String)
    return

#---------------------------------------------------------------------------

def CheckFilename(Filename, suffix):
    """
    This checks a string to make sure that it is a valid filename.
    It currently doesn't do very much. . . .
    """
    if (re.search('\.' + suffix + '$', Filename)):
        return 1
    else:
        return 0

#-------------------------------- Classes ----------------------------------
class ProgramParameters:

    """
    This class holds info about the environment and cmd line args passed to the
    program.  It has only one method:  the constructor, which gets the args
    and fills out the public vars.  The public vars are:
    """
    def __init__(self):

        global Version

        valid_attributes =[ "device", "footprint", "numslots", "refdes",
                            "author", "dist-license", "use-license",
                            "description", "documentation", "symversion",
                            "graphical", "net", "netname", "slot", "slotdef",
                            "value", "comment", "footprints", "model-name",
                            "file", "pins", "spice-type"]
        """
        Constructor: parse through cmd line args and fill out vars. The values
        here are the defaults
        """
        self.ExitCode         = 0
        self.ForceMode        = False
        self.RecursiveMode    = False
        self.VerboseMode      = False
        self.QuiteMode        = False
        self.InputFiles       = []
        self.UserDir          = os.path.abspath(os.getcwd())
        self.InputFileName    = ""
        self.OutputFileName   = ""

        self.AttributeName    = ""
        self.AttributeValue   = ""
        self.AttributeColor   = 0
        self.AttributeSize    = None
        self.AttributeVisible = VISIBLE;

        #  Get ScratchDir, either from environment, or just use /tmp as default.
        for EnvVar in ["TMP", "TMPVAR", "TEMP"]:
            try:
                TempDir = os.environ[EnvVar]
            except:
                continue                      # Not present, continue looping
            else:
                self.ScratchDir = TempDir     # Got it!
                break
        else:
            self.ScratchDir = "/tmp"          # no env var set, use default

        #  Get and process command line args
        try:
            long_opt = ["force",
                        "help",
                        "attribute=",
                        "color=",
                        "hidden",
                        "input=",
                        "output=",
                        "size=",
                        "value=",
                        "quite",
                        "verbose",
                        "version"]
            OptList, Args = getopt.getopt(sys.argv[1:], 'fha:c:di:o:s:l:qvV', long_opt)
        except getopt.error:
            print Usage                # print out usage string if
                                       # user uses invalid flag.
            sys.exit(1)

        # First pass through args.  Get switch settings & set program modes.
        for Option, Value in OptList:

            if Option in ('-f', '--force'):
                self.ForceMode = True

            if Option in ('-h', '--help'):
                print Usage
                print Help
                sys.exit(0)

            if Option in ('-a', '--attribute'):
                self.AttributeName       = Value

            if Option in ('-c', '--color'):
                self.AttributeColor      = Value

            if Option in ('-d', '--hidden'):
                self.AttributeVisible    = INVISIBLE

            if Option in ('-i', '--input'):
                self.InputFileName       = Value

            if Option in ('-l', '--value'):
                self.AttributeValue      = Value

            if Option in ('-q', '--quite'):
                self.QuiteMode           = True

            if Option in ('-s', '--size'):
                self.AttributeSize       = Value

            if Option in ('-v', '--verbose'):
                self.VerboseMode         = True

            if Option in ('-V', '--version'):
                print Version
                sys.exit(0)

            #if os.path.isfile(Value):
            #    self.InputFiles.append(Value)

        # Second pass.  Do sanity checking and get configured filenames.
        for Option, Value in OptList:
            if Option in ('-o', '--output'):
                if CheckFilename(Value, "sym"):
                    self.OutputFileName = Value
                else:
                    print("Warning -- output file suffix is not \".sym\"")
                    Input = raw_input("Continue? [y/N] ")
                    if ((len(Input) == 0) or (Input[0] != "y") ):
                        sys.exit(1)
                    elif not self.ForceMode and os.path.isfile(Value):
                        print("Warning -- output file exist, continue will over write the file")
                        Input = raw_input("Continue? [y/N] ")
                        if ((len(Input) == 0) or (Input[0] != "y") ):
                            sys.exit(1)
                        else:
                            self.OutputFileName = Value
                    else:
                        self.OutputFileName = Value

        # Third step: Create list of files remaining on command line, and create output
        # base file name.
        self.CmdLineFileList = Args

        # Forth step: If the attribute name was not specified look for a valid name one in
        # the CmdLineFileList list and remove from list and use for the attribute name
        #
        if not self.AttributeName and len(self.CmdLineFileList) > 0:
            string = self.CmdLineFileList[0]
            if string:
                for attribute in valid_attributes:
                    if string == attribute:
                        self.AttributeName = string
                        self.CmdLineFileList.pop(0)
                        break

        self.OutputFileNameBase = re.sub('\.sym', '', self.OutputFileName)

        return

#---------------------------------------------------------------------------

def ProcessSymbol(Options, File):

    symbol = geda.open_page(File)

    if Options.VerboseMode:
        print "Processing: " + symbol.filename

    objects   = geda.get_objects(symbol)
    modified  = 0
    Attribute = None
    """
      Loop thru all objects and look for the attribute
    """
    for capsule in objects:
        object = geda.get_object(capsule)          # Retrieve object from GedaCapsule
        if capsule.type == OBJ_TEXT:
            string = object.string
            index = string.find("=")
            if index > 0:
                if object.name() == Options.AttributeName:
                    Attribute = object
                    break

    if not Attribute:                  # If not found and have value then create new
        if Options.AttributeValue:
            Attribute = geda.new_attrib(Options.AttributeName,
                                        Options.AttributeValue, 100, 100,
                                        Options.AttributeVisible, SHOW_NAME_VALUE)
            geda.add_object(symbol, Attribute)
            modified = 1
        else:                          # No value specified and attribute not found
            QMessage (Options, (Options.AttributeName + ' was not found in ' + symbol.filename))
            Options.ExitCode = 1;
    else:                              # Found the attribute
        if Options.AttributeValue:     # Check the value and set only if different
            if not Attribute.value == Options.AttributeValue:
                string = Options.AttributeName + '=' + Options.AttributeValue
                Attribute.string = string
                modified = 1
        else:                          # Get print the current value
            index = Attribute.string.find("=") + 1
            value = Attribute.string[index:]
            if Options.VerboseMode:
                print symbol.filename + '::' + value
            else:
                print value

    if Attribute and Options.AttributeSize:
        Attribute.size = Options.AttributeSize
        modified = 1

    if modified:
        QMessage (Options, ("saving file:" + symbol.filename))
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

    if not Options.AttributeName:
        print Usage
    else:

        if Options.InputFileName:
            FileName =  re.sub('\.sym', '', Options.InputFileName) + ".sym"
            if os.path.isfile(FileName):
                Options.InputFiles.append(FileName)
        else:
            for File in Options.CmdLineFileList:
                FileName =  re.sub('\.sym', '', File) + ".sym"
                if os.path.isfile(FileName):
                    Options.InputFiles.append(FileName)

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
            QMessage (Options, 'No symbols found or specifed, try using --help')
            Options.ExitCode = 1
        elif not foundfiles :
            if len(InputFiles) > 1:
                for file in InputFiles:
                    if not os.path.isfile(file):
                        QMessage (Options, "Bad file name or option: " + file)
                        print Usage
                        Options.ExitCode = 1

        for symbol_file in InputFiles:
            ProcessSymbol(Options, symbol_file)

    exit(Options.ExitCode)

if __name__ == "__main__":
   main(sys.argv[1:])

# Done!

