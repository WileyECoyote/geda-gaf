#! /usr/bin/env python
#
# gschem2xml -- This program reads a symbol or a schematic file created by
# geda-gaf, and translates it into an XML representation using the GedaPyxie
# XML tree processing package installed in the Python library path.
#
# It works in the following way:
#
# 1.  Reads in the whole file (using readlines), creating a list of lines
# 2.  Iterates over each line in the list, examines each line, and depending
#     upon the object type (idendified by the first char in the line), calls
#     the appropriate handler.
# 3.  The handler for each object instantiates a node on an xTree with a
#     corresonding ElementTypeName, and all corresponding attributes.
# 4.  Attributes attached to a component as delinieated by the braces, "{" & "}"
#     are handled through recursion, attached attributes are attached to their
#     parent by creating a sub-tree beneath the parent node.
# 5.  When the entire gschem file has been iterated over, the xTree is written
#     out as an XML file using methods available in GedaPyxie.
#
# 2003.05.31 -- SDB
#
# 2015.09.15 -- WEH Extensive modification and additions; Added command-line
#               parser, help usage, and options, updated to read newer file
#               formats.

__version__ = "0.2"  # low version number signifies alpha quality

import os, sys, re, getopt, glob

from geda import GedaPyxie

#  This is the help string.
Usage =\
"""
geda2xml -- A gEDA-gaf Export Utility
            to export a gEDA schematic or symbol file to an XML file.

Usage: geda2xml [Options] -i <inputfile> [[-o] <outputfile> ]
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

	        geda2xml -f simple.sch

Copyright (C) 2015 by gEDA Contributors.  Released under GPL Version 3.

"""

#------------------------ Functions used by Classes ------------------------

def VMessage(Options, String):
    """
    This prints out String when the -v flag is set, otherwise is silent
    """
    if (Options.VerboseMode == True):
        print(String)
    return

#---------------------------------------------------------------------------

def QMessage(Options, String):
    """
    This prints out String when the -v flag is set, otherwise is silent
    """
    if Options.VerboseMode == True or not Options.QuiteMode:
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
    program. The class has only one method:  the constructor, which gets the args
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
                if CheckFilename(Value, "xml"):
                    self.OutputFileName = Value
                else:
                    print("Warning -- output file suffix is not \".xml\"")
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
            if CheckFilename(self.InputFileName, "sch"):
                self.OutputFileNameBase = re.sub('\.sch', '', self.InputFileName)
            elif CheckFilename(self.InputFileName, "sym"):
                self.OutputFileNameBase = re.sub('\.sym', '', self.InputFileName)
            else:
                print("Error -- unknown input file type: " + self.InputFileName)
                sys.exit(0)

            self.OutputFileName = self.OutputFileNameBase + ".xml"

        if self.OutputFileName:
            if not self.ForceMode and os.path.isfile(self.OutputFileName):
                print("Warning -- output file exist, continue will over write the file")
                Input = raw_input("Continue? [y/N] ")
                if ((len(Input) == 0) or (Input[0] != "y") ):
                    QMessage(self, "Aborting")
                    sys.exit(1)

        return

#============================================================================
#  Define methods used to manipulate Schematic tree object.
#============================================================================
class Schematic(GedaPyxie.xTree):

	def __init__(self):
		"""
		The init creates the top level node of the schematic object.
		"""
		GedaPyxie.xTree.__init__(self,GedaPyxie.xElement("Schematic"))

	def __repr__(self):
		return self.__repr__()

#-------------------------------------------------------------------------
	def MakeVersion(self,  Version):
		"""
		This adds a version tag to the tree.
                """
		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("version")
			self.Right()

		else:
			self.MakeElementDown("version")
			self.Down()

		self.AttributeValues["version"] = Quote(Version)
		return self.CurPos

#-------------------------------------------------------------------------
        def MakeLine(self, x1, y1, x2, y2, color, width, capstyle, dashstyle, dashlength, dashspace):
                """
		This adds a line tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("line")
			self.Right()

		else:
			self.MakeElementDown("line")
			self.Down()

		self.AttributeValues["x1"] = Quote(x1)
		self.AttributeValues["y1"] = Quote(y1)
		self.AttributeValues["x2"] = Quote(x2)
		self.AttributeValues["y2"] = Quote(y2)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["width"] = Quote(width)
		self.AttributeValues["capstyle"] = Quote(capstyle)
		self.AttributeValues["dashstyle"] = Quote(dashstyle)
		self.AttributeValues["dashlength"] = Quote(dashlength)
		self.AttributeValues["dashspace"] = Quote(dashspace)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeBox(self,  x, y, box_width, box_height, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2):
                """
		This adds a box tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("box")
			self.Right()

		else:
			self.MakeElementDown("box")
			self.Down()

		self.AttributeValues["x"] = Quote(x)
		self.AttributeValues["y"] = Quote(y)
		self.AttributeValues["box_width"] = Quote(box_width)
		self.AttributeValues["box_height"] = Quote(box_height)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["width"] = Quote(width)
		self.AttributeValues["capstyle"] = Quote(capstyle)
		self.AttributeValues["dashstyle"] = Quote(dashstyle)
		self.AttributeValues["dashlength"] = Quote(dashlength)
		self.AttributeValues["dashspace"] = Quote(dashspace)
		self.AttributeValues["filltype"] = Quote(filltype)
		self.AttributeValues["fillwidth"] = Quote(fillwidth)
		self.AttributeValues["angle1"] = Quote(angle1)
		self.AttributeValues["pitch1"] = Quote(pitch1)
		self.AttributeValues["angle2"] = Quote(angle2)
		self.AttributeValues["pitch2"] = Quote(pitch2)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeCircle(self, x, y, radius, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2):
                """
		This adds a circle tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("circle")
			self.Right()

		else:
			self.MakeElementDown("circle")
			self.Down()

		self.AttributeValues["x"] = Quote(x)
		self.AttributeValues["y"] = Quote(y)
		self.AttributeValues["radius"] = Quote(radius)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["width"] = Quote(width)
		self.AttributeValues["capstyle"] = Quote(capstyle)
		self.AttributeValues["dashstyle"] = Quote(dashstyle)
		self.AttributeValues["dashlength"] = Quote(dashlength)
		self.AttributeValues["dashspace"] = Quote(dashspace)
		self.AttributeValues["filltype"] = Quote(filltype)
		self.AttributeValues["fillwidth"] = Quote(fillwidth)
		self.AttributeValues["angle1"] = Quote(angle1)
		self.AttributeValues["pitch1"] = Quote(pitch1)
		self.AttributeValues["angle2"] = Quote(angle2)
		self.AttributeValues["pitch2"] = Quote(pitch2)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeArc(self,  x, y, radius, startangle, sweepangle, color, width, capstyle, dashstyle, dashlength, dashspace):
                """
		This adds an arc tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("arc")
			self.Right()

		else:
			self.MakeElementDown("arc")
			self.Down()

		self.AttributeValues["x"] = Quote(x)
		self.AttributeValues["y"] = Quote(y)
		self.AttributeValues["radius"] = Quote(radius)
		self.AttributeValues["startangle"] = Quote(startangle)
		self.AttributeValues["sweepangle"] = Quote(sweepangle)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["width"] = Quote(width)
		self.AttributeValues["capstyle"] = Quote(capstyle)
		self.AttributeValues["dashstyle"] = Quote(dashstyle)
		self.AttributeValues["dashlength"] = Quote(dashlength)
		self.AttributeValues["dashspace"] = Quote(dashspace)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeText(self, x, y, color, size, visibility, show_name_value, angle, alignment, text):
                """
		This adds a text tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("text")
			self.Right()

		else:
			self.MakeElementDown("text")
			self.Down()

		self.AttributeValues["x"] = Quote(x)
		self.AttributeValues["y"] = Quote(y)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["size"] = Quote(size)
		self.AttributeValues["visibility"] = Quote(visibility)
		self.AttributeValues["show_name_value"] = Quote(show_name_value)
		self.AttributeValues["angle"] = Quote(angle)
		self.AttributeValues["alignment"] = Quote(alignment)

		# Now insert text. . .  .
		self.MakeDataDown(text)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeNet(self, x1, y1, x2, y2, color):
                """
		This adds a net tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("net")
			self.Right()

		else:
			self.MakeElementDown("net")
			self.Down()

		self.AttributeValues["x1"] = Quote(x1)
		self.AttributeValues["y1"] = Quote(y1)
		self.AttributeValues["x2"] = Quote(x2)
		self.AttributeValues["y2"] = Quote(y2)
		self.AttributeValues["color"] = Quote(color)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeBus(self,  x1, y1, x2, y2, color, ripperdir):
                """
		This adds a bus tag to the tree.
                """
		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("bus")
			self.Right()

		else:
			self.MakeElementDown("bus")
			self.Down()

		self.AttributeValues["x1"] = Quote(x1)
		self.AttributeValues["y1"] = Quote(y1)
		self.AttributeValues["x2"] = Quote(x2)
		self.AttributeValues["y2"] = Quote(y2)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["ripperdir"] = Quote(ripperdir)


		return self.CurPos

#-------------------------------------------------------------------------
	def MakePin(self,  x1, y1, x2, y2, color, pintype, whichend):
                """
		This adds a pin tag to the tree.
                """

		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("pin")
			self.Right()

		else:
			self.MakeElementDown("pin")
			self.Down()

		self.AttributeValues["x1"] = Quote(x1)
		self.AttributeValues["y1"] = Quote(y1)
		self.AttributeValues["x2"] = Quote(x2)
		self.AttributeValues["y2"] = Quote(y2)
		self.AttributeValues["color"] = Quote(color)
		self.AttributeValues["pintype"] = Quote(pintype)
		self.AttributeValues["whichend"] = Quote(whichend)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeComponent(self,  x, y, selectable, angle, mirror, basename):
                """
		This adds a component tag to the tree.
                """
		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("component")
			self.Right()

		else:
			self.MakeElementDown("component")
			self.Down()

		self.AttributeValues["x"] = Quote(x)
		self.AttributeValues["y"] = Quote(y)
		self.AttributeValues["selectable"] = Quote(selectable)
		self.AttributeValues["angle"] = Quote(angle)
		self.AttributeValues["mirror"] = Quote(mirror)
		self.AttributeValues["basename"] = Quote(basename)

		return self.CurPos

#-------------------------------------------------------------------------
	def MakeFont(self,  character, width, flag):
                """
		This adds a font tag to the tree.
                """
		if self.HasDown():
			self.Down()
			while self.HasRight():
				self.Right()
			self.MakeElementRight("font")
			self.Right()

		else:
			self.MakeElementDown("font")
			self.Down()

		self.AttributeValues["character"] = Quote(character)
		self.AttributeValues["width"] = Quote(width)
		self.AttributeValues["flag"] = Quote(flag)

		return self.CurPos

#---------------------------------------------------------------------------
# These are global because CreateXMLTree is recursive and some entity types
# need to know the version to interpret the data correctly.
release_ver    = 0
fileformat_ver = 0

#=========================================================================
#  This is a recursive routine that creates the XML tree
#=========================================================================
def CreateXMLTree(schematic, i, Lines):

	global Options
	global release_ver
	global fileformat_ver

	# print "Entering CreateXMLTree,  i = %d" % i

	while (i < len(Lines)):

		Line = Lines[i]

		# print "In loop, i = %d, processing Line = %s" % (i, Line)

		# -----------  Get first char from line	 -------------
		CmdLett = Line[0]

		# -------- End of attached attributes --------
		if (CmdLett == "}"):
			# print "Leaving CreateXMLTree, returning i = %d" % i
			return i

		if (CmdLett == "]"):
			return i

		# -------- Version tag --------
		elif (CmdLett == "v"):
			parts = Line.split()
			if parts[1]:
				version     = parts[1]
				release_ver = int(parts[1])
			if parts[2]:
				version = version + " " + parts[2]
				fileformat_ver = int(parts[2])
			schematic.MakeVersion(version)
			schematic.Up()


		# -------- Line tag --------
		elif (CmdLett == "L"):
			(Cmd, x1, y1, x2, y2, color, width, capstyle, dashstyle, dashlength, dashspace) = Line.split()
			schematic.MakeLine(x1, y1, x2, y2, color, width, capstyle, dashstyle, dashlength, dashspace)

			# This checks to see if any attributes are attached to the line
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()


		# -------- Box tag --------
		elif (CmdLett == "B"):
			(Cmd, x, y, box_width, box_height, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2) = Line.split()
			schematic.MakeBox(x, y, box_width, box_height, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2)

			# This checks to see if any attributes are attached to the box
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()

		# -------- Circle tag --------
		elif (CmdLett == "V"):
			(Cmd, x, y, radius, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2) = Line.split()
			schematic.MakeCircle(x, y, radius, color, width, capstyle, dashstyle, dashlength, dashspace, filltype, fillwidth, angle1, pitch1, angle2, pitch2)

			# This checks to see if any attributes are attached to the circle
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)
			schematic.Up()


		# -------- Arc tag --------
		elif (CmdLett == "A"):
			(Cmd, x, y, radius, startangle, sweepangle, color, width, capstyle, dashstyle, dashlength, dashspace) = Line.split()
			Schematic.MakeArc(x, y, radius, startangle, sweepangle, color, width, capstyle, dashstyle, dashlength, dashspace)

			# This checks to see if any attributes are attached to the arc
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()


		# -------- Text tag --------
		elif (CmdLett == "T"):

			if fileformat_ver >= 1:
				(Cmd, x, y, color, size, visibility, show_name_value, angle, alignment, snum_lines) = Line.split()
				num_lines = int(snum_lines)
			elif release_ver < 20000220:
			# yes, above less than (not less than and equal) is correct.
			# The format change occurred in 20000220
				(Cmd, x, y, color, size, visibility, show_name_value, angle) = Line.split()
				alignment = 0   # older versions didn't have this, 0 = LOWER_LEFT
				num_lines = 1   # only support a single line
			else:
				(Cmd, x, y, color, size, visibility, show_name_value, angle, alignment) = Line.split()
				num_lines = 1

			i = i + 1
			TextString = Lines[i]

			# Read in additional (non-attribute) text lines if present
			while num_lines > 1:
				i = i + 1
				TextString = TextString + "\n" + Lines[i]
				num_lines  = num_lines - 1

			schematic.MakeText(x, y, color, size, visibility, show_name_value, angle, alignment, TextString)

			# This checks to see if any attributes are attached to the text
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i + 2, Lines)

			schematic.Up()

		# -------- Net tag --------
		elif (CmdLett == "N"):
			(Cmd, x1, y1, x2, y2, color) = Line.split()
			schematic.MakeNet(x1, y1, x2, y2, color)

			# This checks to see if any attributes are attached to the net
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()

		# -------- Bus tag --------
		elif (CmdLett == "U"):
			(Cmd, x1, y1, x2, y2, color, ripperdir) = string.Line()
			schematic.MakeBus(x1, y1, x2, y2, color, ripperdir)

			# This checks to see if any attributes are attached to the bus
			if (i < len(Lines)-1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()

		# -------- Pin tag --------
		elif (CmdLett == "P"):
			(Cmd, x1, y1, x2, y2, color, pintype, whichend) = Line.split()
			schematic.MakePin(x1, y1, x2, y2, color, pintype, whichend)

			# This processes any attributes are attached to the pin
			if (i < len(Lines) - 1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()

		# -------- Component tag --------
		elif (CmdLett == "C"):
			(Cmd, x, y, selectable, angle, mirror, basename) = Line.split()
			schematic.MakeComponent(x, y, selectable, angle, mirror, basename)

			# This processes any attributes are attached to the component and
			# embeded components
			if (i < len(Lines) - 1 ):
				NextLine = Lines[i+1]
				if (NextLine[0] == "["):
					i = CreateXMLTree(schematic, i+2, Lines)
				elif (NextLine[0] == "{"):
					i = CreateXMLTree(schematic, i+2, Lines)

			schematic.Up()

		elif (CmdLett == "{"):
			i = CreateXMLTree(schematic, i+1, Lines)

		# Ignore comments and empty lines
		elif (CmdLett == "#" or CmdLett == "\n"):
			i = i

		else:
			if (Options.verbose == True):
				print ("Unknown tag line <" + str(i) + "> in first position <" + CmdLett + ">")
			raise "CmdLett undefined!!!"

		#print ("line <" + str(i) + "> content <" + CmdLett + ">")
		i = i + 1

		#--- End of loop ---
#

#=========================================================================
#  This is a helper routine
#=========================================================================
def Quote(string):
	return '"'+string+'"'

#---------------------------------------------------------------------------
#  Main prog begins here

def main(argv):

    global Options

    if not Options.InputFileName:
        print Usage
    else:

        # instantate a schematic tree
        schematic = Schematic()        # Creates the top level element in the sch tree.

        GedaFilename = Options.InputFileName
        XMLFilename  = Options.OutputFileName

        try:
            GedaFile = open(GedaFilename, "r")
        except IOError:
            print "Unable to open " + GedaFilename + ".  Exiting. . . ."

        #======================================================================
        #  Read the whole input file at once, and dump it into the list called
        #  Lines. Then iterate over Lines and examine each line, determine its
        #  type, and then invoke the appropriate translator to XML.
        #======================================================================
        Lines = GedaFile.readlines()

        CreateXMLTree(schematic, 0, Lines)

        # print "Here is the final xTree . . . ."
        # Schematic.PrintXMLxTree()

        QMessage (Options, "Conversion successful, writing XML file " + XMLFilename)

        schematic.OutfileXMLSystem(XMLFilename)

        GedaFile.close

if __name__ == "__main__":
	Options = ProgramParameters()  # Creates Options object for program varibles.
	main(sys.argv[1:])

# Done!

