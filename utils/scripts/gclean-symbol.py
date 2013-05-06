#!/usr/bin/python

# Copyright (C) 2008-2013 Werner Hoch <werner.ho@gmx.de>
# Released under the terms of the GNU General Public License, version 2

# usage: gclean-symbol old_symbol_file [ROTATE]> beautyfied_symbol_file
#        ROTATE will rotate the labels of top and bottom pins

# missing features:
# * multiline text is not supported yet
# * getopt cli

# the script can be wrapped with a shellscript to improve the symbol repair workflow
shellscript = """
#!/bin/sh
# script to make a better symbol repair work flow
# it's neccesary that you have a temp directory r/ for the repaired symbols
# and gclean-symbol have to be in $PATH
# I use a symlink s/ to the original symbol path for short commands.

sym_name=$(basename "$1")

# check the old symbol
gsymcheck -vv "$1"

# show me the old symbol
gschem "$1" & >/dev/null

# let the script create a new skeleton of the symbol (add missing attributes)
./gclean-symbol "$1" "$2" >"r/""$sym_name"

# now edit the new sceleton (it's faster using xemacs than using gschem)
xemacs "r/""$sym_name"

# after editing the symbol look at the new symbol and do refinement
gschem "r/""$sym_name" >/dev/null

# after leaving gschem, check the new symbol
gsymcheck -vv "r/$sym_name"

# print a string to stdout that can be used to overwrite the old symbol
# with copy and paste.
# If the symbol is still bad, it can be edited with gschem or editor again.
echo
echo "   mv r/""$sym_name"" ""$1"
echo
"""

import re,sys

################# constants ########################################
VERSION="0.0.2"

PINTYPE=['in','out','io','oc','oe','pas','tp','tri','clk','pwr']

################ functions #####################################################
def makenice(filename, outfile):
    f=open(filename)
    file=[l.strip() for l in f.readlines()]
    pins=[]
    attr={}
    graphical_text=[]
    version=""
    nr =len(file)

    ## check for a valid symbol file
    ## only newer version are supported
    ##     if re.match("^v [0-9]{8} 1$",file[0]):
    ##        outfile.write("v 20070526 1\n")
    ##     else:
    ##        sys.stderr.write("ERROR: line 1: only newer file format versions are supported\n")

    i = 1
    while i < nr:
        if file[i][0] == 'T':
            if len(file[i+1].split("=")) == 2: ## attribute text
                t=file[i].split()
                if re.match('^refdes=',file[i+1]):
                    outfile.write(" ".join(t[0:3]) + ' 8 10 1 1 ' + " ".join(t[7:]) + "\n")
                    attr["refdes"]=1
                else:
                    outfile.write(" ".join(t[0:3]) + ' 5 10 0 0 ' + " ".join(t[7:]) + "\n")
                    attr[file[i+1].split("=")[0]] = 1
                outfile.write(file[i+1] + "\n")
            else:
                ## collect all graphical text elements and move them to the end
                ## of the file. Thus deleting is much easier with a text editor
                graphical_text.extend(file[i:i+2])
            i=i+2
            continue
        elif file[i][0] == "P":
            pin=["","","","",""]   ## Pin[pin,pinnumber,pinseq,pinlabel,pintype]
            pin[0]=file[i]
            outfile.write(pin[0] + "\n")
            i = i + 1
            if file[i][0] != "{":
                sys.stderr.write("ERROR: line %d: pin has no attributes\n" %i)
                escape=1
            else:
                escape=0
                i = i + 1
            outfile.write("{" + "\n")
            x,y,pos = getpinpos(pin[0])
            while file[i][0] != '}' and escape == 0:
                if re.match("^pinnumber=",file[i+1]) and file[i][0] == 'T':
                    if pos == "left":
                        pin[1] = 'T %i' %(x+200) + ' %i' %(y+50) + ' 5 8 1 1 0 6 1'
                    elif pos == "right":
                        pin[1] = 'T %i' %(x-200) + ' %i' %(y+50) + ' 5 8 1 1 0 0 1'
                    elif pos == "top":
                        pin[1] = 'T %i' %(x+50) + ' %i' %(y-200) + ' 5 8 1 1 0 0 1'
                        if ROTATE == 1:
                            pin[1] = 'T %i' %(x-50) + ' %i' %(y-200) + ' 5 8 1 1 90 0 1'
                    elif pos == "bottom":
                        pin[1] = 'T %i' %(x+50) + ' %i' %(y+100) + ' 5 8 1 1 0 0 1'
                        if ROTATE == 1:
                            pin[1] = 'T %i' %(x-50) + ' %i' %(y+200) + ' 5 8 1 1 90 6 1'
                    outfile.write(pin[1] + "\n" + file[i+1] + "\n")
                elif re.match("^pinseq=",file[i+1]) and file[i][0] == 'T':
                    if pos == "left":
                        pin[2] = 'T %i' %(x+200) + ' %i' %(y-50) + ' 5 8 0 1 0 8 1'
                    elif pos == "right":
                        pin[2] = 'T %i' %(x-200) + ' %i' %(y-50) + ' 5 8 0 1 0 2 1'
                    elif pos == "top":
                        pin[2] = 'T %i' %(x+50) + ' %i' %(y-200) + ' 5 8 0 1 0 2 1'
                        if ROTATE == 1:
                            pin[2] = 'T %i' %(x+50) + ' %i' %(y-200) + ' 5 8 0 1 90 2 1'
                    elif pos == "bottom":
                        pin[2] = 'T %i' %(x+50) + ' %i' %(y+100) + ' 5 8 0 1 0 2 1'
                        if ROTATE == 1:
                            pin[2] = 'T %i' %(x+50) + ' %i' %(y+200) + ' 5 8 0 1 90 8 1'
                    outfile.write(pin[2] + "\n" + file[i+1] + "\n")
                elif re.match("^pinlabel=",file[i+1]) and file[i][0] == 'T':
                    if pos == "left":
                        pin[3] = 'T %i' %(x+350) + ' %i' %y + ' 9 8 1 1 0 0 1'
                    elif pos == "right":
                        pin[3] = 'T %i' %(x-350) + ' %i' %y + ' 9 8 1 1 0 6 1'
                    elif pos == "top":
                        pin[3] = 'T %i' %x + ' %i' %(y-350) + ' 9 8 1 1 0 5 1'
                        if ROTATE == 1:
                            pin[3] = 'T %i' %x + ' %i' %(y-350) + ' 9 8 1 1 90 6 1'
                    elif pos == "bottom":
                        pin[3] = 'T %i' %x + ' %i' %(y+350) + ' 9 8 1 1 0 3 1'
                        if ROTATE == 1:
                            pin[3] = 'T %i' %x + ' %i' %(y+350) + ' 9 8 1 1 90 0 1'
                    outfile.write(pin[3] + "\n" + file[i+1] + "\n")
                elif re.match("^pintype=",file[i+1]) and file[i][0] == 'T':
                    if pos == "left":
                        pin[4] = 'T %i' %(x+350) + ' %i' %y + ' 5 8 0 1 0 2 1'
                    elif pos == "right":
                        pin[4] = 'T %i' %(x-350) + ' %i' %y + ' 5 8 0 1 0 8 1'
                    elif pos == "top":
                        pin[4] = 'T %i' %x + ' %i' %(y-500) + ' 5 8 0 1 0 5 1'
                        if ROTATE == 1:
                            pin[4] = 'T %i' %x + ' %i' %(y-350) + ' 5 8 0 1 90 8 1'
                    elif pos == "bottom":
                        pin[4] = 'T %i' %x + ' %i' %(y+500) + ' 5 8 0 1 0 3 1'
                        if ROTATE == 1:
                            pin[4] = 'T %i' %x + ' %i' %(y+350) + ' 5 8 0 1 90 2 1'
                    outfile.write(pin[4] + "\n" + file[i+1] + "\n")
                else:
                    sys.stderr.write("ERROR: line %d: pinsection wrong\n" %i)
                i = i + 2
            if pin[1] == "":
                if pos == "left":
                    pin[1] = 'T %i' %(x+200) + ' %i' %(y+50) + ' 5 8 1 1 0 6 1'
                elif pos == "right":
                    pin[1] = 'T %i' %(x-200) + ' %i' %(y+50) + ' 5 8 1 1 0 0 1'
                elif pos == "top":
                    pin[1] = 'T %i' %(x+50) + ' %i' %(y-200) + ' 5 8 1 1 0 0 1'
                    if ROTATE == 1:
                        pin[1] = 'T %i' %(x-50) + ' %i' %(y-200) + ' 5 8 1 1 90 0 1'
                elif pos == "bottom":
                    pin[1] = 'T %i' %(x+50) + ' %i' %(y+100) + ' 5 8 1 1 0 0 1'
                    if ROTATE == 1:
                        pin[1] = 'T %i' %(x-50) + ' %i' %(y+200) + ' 5 8 1 1 90 6 1'
                outfile.write(pin[1] + "\n" + "pinnumber=X" + "\n")
            if pin[2] == "":
                if pos == "left":
                    pin[2] = 'T %i' %(x+200) + ' %i' %(y-50) + ' 5 8 0 1 0 8 1'
                elif pos == "right":
                    pin[2] = 'T %i' %(x-200) + ' %i' %(y-50) + ' 5 8 0 1 0 2 1'
                elif pos == "top":
                    pin[2] = 'T %i' %(x+50) + ' %i' %(y-200) + ' 5 8 0 1 0 2 1'
                    if ROTATE == 1:
                        pin[2] = 'T %i' %(x+50) + ' %i' %(y-200) + ' 5 8 0 1 90 2 1'
                elif pos == "bottom":
                    pin[2] = 'T %i' %(x+50) + ' %i' %(y+100) + ' 5 8 0 1 0 2 1'
                    if ROTATE == 1:
                        pin[2] = 'T %i' %(x+50) + ' %i' %(y+200) + ' 5 8 0 1 90 8 1'
                outfile.write(pin[2] + "\n" + "pinseq=X" + "\n")
            if pin[3] == "":
                if pos == "left":
                    pin[3] = 'T %i' %(x+350) + ' %i' %y + ' 9 8 1 1 0 0 1'
                elif pos == "right":
                    pin[3] = 'T %i' %(x-350) + ' %i' %y + ' 9 8 1 1 0 6 1'
                elif pos == "top":
                    pin[3] = 'T %i' %x + ' %i' %(y-350) + ' 9 8 1 1 0 5 1'
                    if ROTATE == 1:
                        pin[3] = 'T %i' %x + ' %i' %(y-350) + ' 9 8 1 1 90 6 1'
                elif pos == "bottom":
                    pin[3] = 'T %i' %x + ' %i' %(y+350) + ' 9 8 1 1 0 3 1'
                    if ROTATE == 1:
                        pin[3] = 'T %i' %x + ' %i' %(y+350) + ' 9 8 1 1 90 0 1'
                outfile.write(pin[3] + "\n" + "pinlabel=X" + "\n")
            if pin[4] == "":
                if pos == "left":
                    pin[4] = 'T %i' %(x+350) + ' %i' %y + ' 5 8 0 1 0 2 1'
                elif pos == "right":
                    pin[4] = 'T %i' %(x-350) + ' %i' %y + ' 5 8 0 1 0 8 1'
                elif pos == "top":
                    pin[4] = 'T %i' %x + ' %i' %(y-500) + ' 5 8 0 1 0 5 1'
                    if ROTATE == 1:
                        pin[4] = 'T %i' %x + ' %i' %(y-350) + ' 5 8 0 1 90 8 1'
                elif pos == "bottom":
                    pin[4] = 'T %i' %x + ' %i' %(y+500) + ' 5 8 0 1 0 3 1'
                    if ROTATE == 1:
                        pin[4] = 'T %i' %x + ' %i' %(y+350) + ' 5 8 0 1 90 2 1'
                outfile.write(pin[4] + "\n" + "pintype=X" + "\n")
            if file[i][0] == "}":
                i = i + 1
            outfile.write("}" + "\n")

        elif file[i][0] in "ABLV":
            outfile.write(file[i] +"\n")
            i = i+ 1
        else:
            sys.stderr.write("ERROR: line %d: unknown file format error\n" % i)
            i = i + 1

    ## add attribute templates of missing attributes
    ay = 250
    for newattr in ["footprint", "numslots", "symversion", "use-license",
                    "dist-license", "author", "description", "documentation"]:
        if not attr.has_key(newattr):
            outfile.write("T 4000 %d 5 10 0 0 0 0 1" % ay + "\n")
            outfile.write(newattr + "=X" + "\n")
            ay += 200

    ## print all graphical text at the end of file
    outfile.write("\n".join(graphical_text))
    outfile.write("\n")

def getpinpos(pinstring):
    p = pinstring.split(" ")
    whichend = int(p[7])
    x1=int(p[1])
    x2=int(p[3])
    y1=int(p[2])
    y2=int(p[4])

    if whichend == 1:
        x1, y1, x2, y2 = x2, y2, x1, y1

    if x1 == x2: ## top or bottom pin
        if y1 < y2:
            pos = "bottom"
        else:
            pos = "top"
    else:  ## left or right pin
        if x1 < x2:
            pos ="left"
        else:
            pos = "right"

    return x1, y1, pos

########################## MAIN ################################################

try:
    file_in=sys.argv[1]
except:
    print "gclean-symbol:"
    print "Bad arguments, usage is: ", sys.argv[0] ,"infile [ROTATE]"
    sys.exit()

try:
    if sys.argv[2] == "ROTATE":
        ROTATE=1
    else:
        ROTATE=0
except:
    ROTATE=0

makenice(file_in, sys.stdout)
