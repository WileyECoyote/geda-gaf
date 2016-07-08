#! /usr/bin/env python
#
# xml2gschem.py -- This program reads the XML representation of a schematic
# and translates it into the file format read by gschem.
# It requires that you have the XML tree processing package GedaPyxie installed
# in the same directory, or somewhere that it might be found.
# This prog works in the following way:
# 1.  It reads in the XML file & creates an xTree using methods from GedaPyxie.
# 2.  It starts at the top node of the tree, moves one down, and then calls the
#     recursive printing method.
# 3.  The recursive printing method prints and moves right in  a loop.  At each node,
#     it examines the type of element, and invokes a corresponding printing method
#     dependion upon the type of node.
# 4.  After printing every node, it looks down.  If there is an element node down,
#     it prints out a {, and calls itself.
#
# 2003.06.03 -- SDB
#
# 2015.09.15 -- WEH Extensive modification and additions; Added command-line
#               parser, help usage, and options, updated to write newer file
#               formats.

__version__ = "0.2"  # low version number signifies alpha quality

import os, sys, re, getopt, glob

from geda import GedaPyxie

#  This is the help string.
Usage =\
"""
xml2geda -- A gEDA-gaf Import Utility
            to import a gEDA schematic or symbol file from an XML file.

Usage: xml2geda [Options] -i <inputfile> [[-o] <outputfile> ]
"""

Help =\
"""
Options:

  -f, --force    -- When the force option is specified existing file matching
                    the output name will be over-written, without any warning.
  -h, --help     -- Display this help and exit.
  -q, --quite    -- Suppress normal processing messages.
  -v, --verbose  -- Spews lots of info about what the prog is doing.
  -V, --version  -- Output version information and exit.

  -i, --input  <infile>  -- Specifies the name of the input sym file.
  -o  --output <outfile> -- Specifies the name of the output sym file.
                            When not specified the base name of input
                            file with an "xml" extension.

Example: Set the footprint in all symbol files in the current directory to SO8:

            xml2geda -f simple.xml

Copyright (C) 2015 by gEDA Contributors.  Released under GPL Version 3.

"""

#------------------------ Functions used by Classes ------------------------

def VMessage(Options, String):
    """
    This prints out String when the -v flag is set, otherwise is silent
    """
    if (Options.VerboseMode):
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
    and fills out the public vars.
    """
    def __init__(self):

        global __version__

        """
        Constructor: parse through cmd line args and fill out vars. The values
        here are the defaults:
        """
        self.ExitCode         = 0
        self.ForceMode        = False
        self.VerboseMode      = False
        self.QuiteMode        = False
        self.UserDir          = os.path.abspath(os.getcwd())
        self.InputFileName    = ""
        self.OutputFileName   = ""

        #  Get and process command line args
        try:
            long_opt = ["force",
                        "help",
                        "input=",
                        "output=",
                        "quite",
                        "verbose",
                        "version"]
            OptList, Args = getopt.getopt(sys.argv[1:], 'fhi:o:qvV', long_opt)
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

            if Option in ('-i', '--input'):
                self.InputFileName = Value

            if Option in ('-o', '--output'):
                if CheckFilename(Value, "sch"):
                    self.OutputFileName = Value
                elif CheckFilename(Value, "sym"):
                    self.OutputFileName = Value
                else:
                    print("Warning -- output file suffix is not \".sch\" or \".sym\"")
                    Input = raw_input("Continue? [y/N] ")
                    if ((len(Input) == 0) or (Input[0] != "y") ):
                        QMessage(self, "Aborting")
                        sys.exit(1)
                    else:
                        self.OutputFileName = Value

            if Option in ('-q', '--quite'):
                self.QuiteMode = True

            if Option in ('-v', '--verbose'):
                self.VerboseMode = True

            if Option in ('-V', '--version'):
                print Version
                sys.exit(0)

        # Second pass.  Do sanity checking and get configured filenames.
        for Value in Args:

            if self.InputFileName and os.path.isfile(Value):
               self.OutputFileName = Value

            if not self.InputFileName:
               if os.path.isfile(Value):
                  self.InputFileName = Value
                  VMessage(self, "Input file found: " + Value)
               else:
                  print("Error -- input file not found:" + Value)
                  sys.exit(0)

        if self.InputFileName and not self.OutputFileName:
            if CheckFilename(self.InputFileName, "xml"):
                self.OutputFileNameBase = re.sub('\.xml', '', self.InputFileName)
            else:
                print("Error -- unknown input file type: " + self.InputFileName)
                sys.exit(0)

            self.OutputFileName = self.OutputFileNameBase + ".sch"

        if self.OutputFileName:
            if not self.ForceMode and os.path.isfile(self.OutputFileName):
                print("Warning -- output file exist, continue will over write the file")
                Input = raw_input("Continue? [y/N] ")
                if ((len(Input) == 0) or (Input[0] != "y") ):
                    QMessage(self, "Aborting")
                    sys.exit(1)

        return

#============================================================================
#  Define methods used to Print a Schematic tree object.
#============================================================================
def RecursivePrint(Options, GedaFile, Tree):

    # Extract the version string
    in_release_ver    = -1
    in_fileformat_ver = -1

    def PrintVersion(VersionNode):
        """
        Prints the version line
        """
        version = VersionNode.AttributeValues["version"]

        parts = version.split()

        if parts[0]:
            release_ver = int(parts[0])
            if parts[1]:
                fileformat_ver = int(parts[1])

            GedaFile.write( "v %s\n" % version )
        return

#-------------------------------------------------------------------------
    def PrintLine(LineNode):
        """
        This prints a line line
        """
        x1 = LineNode.AttributeValues["x1"]
        y1 = LineNode.AttributeValues["y1"]
        x2 = LineNode.AttributeValues["x2"]
        y2 = LineNode.AttributeValues["y2"]
        color = LineNode.AttributeValues["color"]
        width = LineNode.AttributeValues["width"]
        capstyle = LineNode.AttributeValues["capstyle"]
        dashstyle = LineNode.AttributeValues["dashstyle"]
        dashlength = LineNode.AttributeValues["dashlength"]
        dashspace  = LineNode.AttributeValues["dashspace"]

        GedaFile.write( "L %s %s %s %s %s %s %s %s %s %s\n" % (x1, y1, x2, y2, color, width, capstyle, dashstyle, dashlength, dashspace) )
        return


#-------------------------------------------------------------------------
    def PrintBox(BoxNode):
        """
        This prints a box line
        """
        x = BoxNode.AttributeValues["x"]
        y = BoxNode.AttributeValues["y"]
        box_width = BoxNode.AttributeValues["box_width"]
        box_height = BoxNode.AttributeValues["box_height"]
        color = BoxNode.AttributeValues["color"]
        width = BoxNode.AttributeValues["width"]
        capstyle = BoxNode.AttributeValues["capstyle"]
        dashstyle = BoxNode.AttributeValues["dashstyle"]
        dashlength = BoxNode.AttributeValues["dashlength"]
        dashspace = BoxNode.AttributeValues["dashspace"]
        filltype = BoxNode.AttributeValues["filltype"]
        fillwidth = BoxNode.AttributeValues["fillwidth"]
        angle1 = BoxNode.AttributeValues["angle1"]
        pitch1 = BoxNode.AttributeValues["pitch1"]
        angle2 = BoxNode.AttributeValues["angle2"]
        pitch2 = BoxNode.AttributeValues["pitch2"]

        GedaFile.write( "B %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n" % (x, y, box_width, box_height, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2))
        return


#-------------------------------------------------------------------------
    def PrintCircle(CircleNode):
        """
        This adds a circle line to the file
        """
        x = CircleNode.AttributeValues["x"]
        y = CircleNode.AttributeValues["y"]
        radius = CircleNode.AttributeValues["radius"]
        color = CircleNode.AttributeValues["color"]
        width = CircleNode.AttributeValues["width"]
        capstyle = CircleNode.AttributeValues["capstyle"]
        dashstyle = CircleNode.AttributeValues["dashstyle"]
        dashlength = CircleNode.AttributeValues["dashlength"]
        dashspace = CircleNode.AttributeValues["dashspace"]
        filltype = CircleNode.AttributeValues["filltype"]
        fillwidth = CircleNode.AttributeValues["fillwidth"]
        angle1 = CircleNode.AttributeValues["angle1"]
        pitch1 = CircleNode.AttributeValues["pitch1"]
        angle2 = CircleNode.AttributeValues["angle2"]
        pitch2 = CircleNode.AttributeValues["pitch2"]

        GedaFile.write( "V %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s\n" % (x, y, radius, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2) )
        return

#-------------------------------------------------------------------------
    def PrintArc(ArcNode):
        """
        This prints an arc line
        """

        x = ArcNode.AttributeValues["x"]
        y = ArcNode.AttributeValues["y"]
        radius = ArcNode.AttributeValues["radius"]
        startangle = ArcNode.AttributeValues["startangle"]
        sweepangle = ArcNode.AttributeValues["sweepangle"]
        color = ArcNode.AttributeValues["color"]
        width = ArcNode.AttributeValues["width"]
        capstyle = ArcNode.AttributeValues["capstyle"]
        dashstyle = ArcNode.AttributeValues["dashstyle"]
        dashlength = ArcNode.AttributeValues["dashlength"]
        dashspace = ArcNode.AttributeValues["dashspace"]

        GedaFile.write( "A %s %s %s %s %s %s %s %s %s %s %s\n" % (x, y, radius, startangle, sweepangle, color, width, capstyle, dashstyle, dashlength, dashspace) )
        return

#-------------------------------------------------------------------------
    def PrintText(TextNode):
        """
        This prints the text line, followed by the text itself.
        """

        x = TextNode.AttributeValues["x"]
        y = TextNode.AttributeValues["y"]
        color = TextNode.AttributeValues["color"]
        size = TextNode.AttributeValues["size"]
        visibility = TextNode.AttributeValues["visibility"]
        show_name_value = TextNode.AttributeValues["show_name_value"]
        angle = TextNode.AttributeValues["angle"]
        alignment = TextNode.AttributeValues["alignment"]

        GedaFile.write( "T %s %s %s %s %s %s %s %s 1\n" % (x, y, color, size, visibility, show_name_value, angle, alignment) )
        TextNode.Down()
        text = Tree.Data
        TextNode.Up()

        GedaFile.write( text )

        return

#-------------------------------------------------------------------------
    def PrintNet(NetNode):
        """
        This prints the net line
        """

        x1 = NetNode.AttributeValues["x1"]
        y1 = NetNode.AttributeValues["y1"]
        x2 = NetNode.AttributeValues["x2"]
        y2 = NetNode.AttributeValues["y2"]
        color = NetNode.AttributeValues["color"]

        GedaFile.write( "N %s %s %s %s %s\n" % (x1, y1, x2, y2, color) )
        return


#-------------------------------------------------------------------------
    def PrintBus(BusNode):
        """
        This prints a bus line
        """
        x1 = BusNode.AttributeValues["x1"]
        y1 = BusNode.AttributeValues["y1"]
        x2 = BusNode.AttributeValues["x2"]
        y2 = BusNode.AttributeValues["y2"]
        color = BusNode.AttributeValues["color"]
        ripperdir = BusNode.AttributeValues["ripperdir"]

        GedaFile.write( "U %s %s %s %s %s %s\n" % (x1, y1, x2, y2, color, ripperdir) )
        return

#-------------------------------------------------------------------------
    def PrintPin(PinNode):
        """
        This prints a pin line
        """
        x1 = PinNode.AttributeValues["x1"]
        y1 = PinNode.AttributeValues["y1"]
        x2 = PinNode.AttributeValues["x2"]
        y2 = PinNode.AttributeValues["y2"]
        color = PinNode.AttributeValues["color"]
        pintype = PinNode.AttributeValues["pintype"]
        whichend = PinNode.AttributeValues["whichend"]

        GedaFile.write( "P %s %s %s %s %s %s %s\n" % (x1, y1, x2, y2, color, pintype, whichend) )
        return

#-------------------------------------------------------------------------
    def PrintComponent(ComponentNode):
        """
        This prints the line associated with a component.
        """

        x = ComponentNode.AttributeValues["x"]
        y = ComponentNode.AttributeValues["y"]
        selectable = ComponentNode.AttributeValues["selectable"]
        angle = ComponentNode.AttributeValues["angle"]
        mirror = ComponentNode.AttributeValues["mirror"]
        basename = ComponentNode.AttributeValues["basename"]

        GedaFile.write( "C %s %s %s %s %s %s\n" % (x, y, selectable, angle, mirror, basename) )
        return


#-------------------------------------------------------------------------
    def PrintFont(FontNode):
        """
        This prints a font line
        """
        character = FontNode.AttributeValues["character"]
        width = FontNode.AttributeValues["width"]
        flag = FontNode.AttributeValues["flag"]

        GedaFile.write( "F %s %s %s\n" % (character, width, flag) )
        return

#=========================================================================
#  Now the main part of RecursivePrint
#=========================================================================
    while(1):

        NodeType = Tree.ElementTypeName

        if (NodeType == "version"):
            PrintVersion(Tree)
        elif (NodeType == "line"):
            PrintLine(Tree)
        elif (NodeType == "box"):
            PrintBox(Tree)
        elif (NodeType == "circle"):
            PrintCircle(Tree)
        elif (NodeType == "arc"):
            PrintArc(Tree)
        elif (NodeType == "text"):
            PrintText(Tree)
        elif (NodeType == "net"):
            PrintNet(Tree)
        elif (NodeType == "bus"):
            PrintBus(Tree)
        elif (NodeType == "pin"):
            PrintPin(Tree)
        elif (NodeType == "component"):
            PrintComponent(Tree)
        elif (NodeType == "font"):
            PrintFont(Tree)
        else:
            raise "Unknown node type!!!!!"

        if (Tree.HasDown() and (NodeType != "text") ):
            Tree.Down()
            GedaFile.write( "{\n" )
            RecursivePrint(Options, GedaFile, Tree)


        if (Tree.HasRight() ):
            Tree.Right()
        else:
            if (Tree.HasUp() ):
                Tree.Up()
                if (Tree.ElementTypeName != "Schematic"):
                    GedaFile.write( "}\n" )
                return
            else:
                break

#---------------------------------------------------------------------------
#  Main prog begins here

def main(argv):

    Options = ProgramParameters()      # Creates Options object for program varibles.

    if not Options.InputFileName:
        print Usage
    else:

        XMLFilename  = Options.InputFileName
        GedaFilename = Options.OutputFileName

        # Open output file.
        try:
            GedaFile = open(GedaFilename, "w")
        except IOError:
            print "Unable to open " + GedaFilename + ".  Exiting. . . ."

        VMessage(Options, "Processing " + XMLFilename)

#=========================================================================
#  Now read the XML file in as an xTree.  Then go down one node, and invoke
#  RecursivePrint.
#=========================================================================
        Tree = GedaPyxie.File2xTree(XMLFilename)
        Tree = GedaPyxie.NormalizeDataNodes(Tree)  # This gets rid of extraneous whitespace data nodes.

        if Tree.HasDown():
            Tree.Down()
            RecursivePrint(Options, GedaFile, Tree)
        else:
            raise "Unable to descend tree, is correct file?"

        QMessage (Options, "Conversion successful, write to file " + GedaFilename)

        GedaFile.close

if __name__ == "__main__":
    main(sys.argv[1:])
