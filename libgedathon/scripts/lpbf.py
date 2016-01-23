#! /usr/bin/env python

############################################################################

import os, sys
from geda import geda
from geda.functions import *

# Get & Save the current working directory
if len(sys.argv) > 1:
     directory = sys.argv[1]
     if not os.path.exists(directory):
          directory = os.getcwd()
else:
     directory = os.getcwd()

# Set directory and file name relative to the cwd
sym_dir   = directory + "/tmp/sym"
filename  = directory + "/tmp/lpfilter.sch"

# if tmp directory does not exist then create the directory
if not os.path.exists(directory + "/tmp"):
    os.makedirs(directory + "/tmp")

# Make the tmp directory the current directory
os.chdir(directory + "/tmp")

# if sym directory does not exist then create the directory
if not os.path.exists("sym"):
    os.makedirs("sym")

#print os.getcwd()

## Create the components needed for the filter ##
## ------------------------------------------- ##
# Create the dual-opamp symbol file
os.system("../../scripts/dual-opamp.py " + sym_dir)

# Create the capacitor symbol file
os.system("../../scripts/capacitor.py " + sym_dir)

# Create the resistor symbol file
os.system("../../scripts/resistor.py " + sym_dir)

# Add our custom symbols to the library

geda.append_symbol_path("./sym")
geda.declare_local_sym()

DefaultCapacitorSymbol("capacitor-py")
DefaultOpAmpSymbol("dual-opamp-py")
DefaultResistorSymbol("resistor-py")

## Create a new schmatic file for the filter ##
lpbf = geda.new_page(filename)

titleblock = geda.new_complex("title-B", 1000, 1000)
titleblock.locked = True
geda.add_object(lpbf, titleblock)

amp1 = AddComponent(lpbf, "dual-opamp-py", "U1", 7700, 7300)

amp2 = amp1.copy(4800, -200)
geda.set_attrib(amp2, "slot", "2", True)

# Add the input coupling cap
AddCapacitor(lpbf, 3000,  7700, ".01uF")

# Add pole cap's
AddCapacitor(lpbf,  6700, 8600, "20nF")
AddCapacitor(lpbf,  7000, 6300, "10nF", 0, 90)
AddCapacitor(lpbf, 11500, 8600, "20nF")
AddCapacitor(lpbf, 11800, 6100, "10nF", 0, 90)

# Add the output coupling cap
AddCapacitor(lpbf, 14600, 7300, "1uF")

# Add the bypass cap's
AddCapacitor(lpbf,  8900, 5200, ".01uF", 0, 270)
AddCapacitor(lpbf, 14300, 9800, ".01uF", 0, 270)

# Add filter resistors
AddResistor(lpbf, 4300,  7800, "560")
AddResistor(lpbf, 5600,  7800, "560")
AddResistor(lpbf, 9100,  7600, "560")
AddResistor(lpbf, 10400, 7600, "560")
AddResistor(lpbf, 7300,  5500, "150k",  0, 270)
AddResistor(lpbf, 8800,  7300, "22k", 0, 270)
AddResistor(lpbf, 12100, 5500, "18k", 0, 270)
AddResistor(lpbf, 13800, 7100, "22k", 0, 270)

#Add  power symbols from library
AddSource(lpbf, "12V-plus-1",  12800, 10300)
AddSource(lpbf, "12V-minus-1",  8400,  5100, 180)

#Add signal grounds from library
AddSource(lpbf, "gnd-1",  9000, 3700)
AddSource(lpbf, "gnd-1", 12100, 3700)

#Add grounds for bypass caps from library
AddSource(lpbf, "gnd-1",  7300, 3700)
AddSource(lpbf, "gnd-1", 14400, 8400)

#Add input and output symbols from library, both of which have a
#refdes=pinlabel attribute, so we will set to "Vin" and "Vout"
input=AddSource(lpbf, "in-1",   2300, 7800)

# 3rd argument is False because we do not need the attribute returned
geda.set_attrib(input, "refdes", "Vin", False)
geda.refresh_attribs(input)

output=AddSource(lpbf, "out-1", 15600, 7400)
geda.set_attrib(output, "refdes", "Vout", False)
geda.refresh_attribs(output)

AddNet(lpbf, 12200,  5500, 12200,  7300)
AddNet(lpbf, 12200,  7300, 12500,  7300)
AddNet(lpbf,  7400,  5500,  7400,  7500)
AddNet(lpbf,  7400,  7500,  7700,  7500)
AddNet(lpbf,  5200,  7900,  5600,  7900)
AddNet(lpbf,  6500,  7900,  7700,  7900)
AddNet(lpbf,  8700,  7700,  9100,  7700)
AddNet(lpbf, 10000,  7700, 10400,  7700)
AddNet(lpbf, 11300,  7700, 12500,  7700)
AddNet(lpbf, 11600,  7000, 11600,  7700)
AddNet(lpbf,  6800,  7200,  6800,  7900)
AddNet(lpbf, 11500,  8800, 10200,  8800)
AddNet(lpbf, 10200,  8800, 10200,  7700)
AddNet(lpbf,  6700,  8800,  5400,  8800)
AddNet(lpbf,  5400,  8800,  5400,  7900)
AddNet(lpbf,  7600,  8800,  8900,  8800)
AddNet(lpbf,  8900,  8800,  8900,  7700)
AddNet(lpbf, 12400,  8800, 13900,  8800)
AddNet(lpbf, 13900,  8800, 13900,  7500)
AddNet(lpbf,  3900,  7900,  4300,  7900)
AddNet(lpbf, 13500,  7500, 14600,  7500)
AddNet(lpbf, 13900,  7100, 13900,  7500)
AddNet(lpbf, 13900,  6200, 13900,  6000)
AddNet(lpbf, 13900,  6000, 12200,  6000)
AddNet(lpbf,  8900,  6400,  8900,  6000)
AddNet(lpbf,  8900,  6000,  7400,  6000)
AddNet(lpbf,  8900,  7300,  8900,  7700)
AddNet(lpbf,  7400,  4000,  7400,  4600)
AddNet(lpbf, 12200,  4600, 12200,  4000)
AddNet(lpbf,  6800,  6300,  6800,  4300)
AddNet(lpbf,  6800,  4300,  7400,  4300)
AddNet(lpbf, 11600,  6100, 11600,  4300)
AddNet(lpbf, 11600,  4300, 12200,  4300)
AddNet(lpbf,  8200,  5100,  8200,  7300)
AddNet(lpbf,  8200,  5500,  9100,  5500)
AddNet(lpbf, 13000,  7900, 13000, 10300)
AddNet(lpbf,  9100,  5500,  9100,  5200)
AddNet(lpbf,  9100,  4300,  9100,  4000)
AddNet(lpbf, 14500,  9800, 14500, 10000)
AddNet(lpbf, 14500, 10000, 13000, 10000)
AddNet(lpbf, 14500,  8900, 14500,  8700)
AddNet(lpbf, 15500,  7500, 15600,  7500)
AddNet(lpbf,  2900,  7900,  3000,  7900)

#objects = geda.get_objects(lpbf)
#print objects

lpbf.save()

lpbf.close()
os.chdir( directory )
