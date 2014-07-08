#! /usr/bin/env python

# Resistor Demo

import os, sys
from geda import geda
from geda.constants import *

if len(sys.argv) > 1:
     sym_path = sys.argv[1]
     if not os.path.exists(sym_path):
          sym_path = "/tmp/sch"
else:
     sym_path = "/tmp/sch"

filename = sym_path + "/resistor-py.sym"
resistor = geda.new_page(filename, True)

#x1, y1, x2, y2 whichend  number, label , etype, mtype, ntype
pin = geda.new_pin(0, 100, 100, 100, 0, 1, "1", PIN_ELECT_PAS, 0, 0)
geda.add_object(resistor, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinlabel":
        attrib.visible = 0
    if attrib.name() == "pinnumber":
        attrib.visible = 0

pin = geda.new_pin(800, 100, 900, 100, 1, 2, "2", PIN_ELECT_PAS, 0, 0)
geda.add_object(resistor, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinlabel":
        attrib.visible = 0
    if attrib.name() == "pinnumber":
        attrib.visible = 0

# Create and add the lines that form the resistor leads
#L 800 100 750 100 3 15 0 0 -1 -1
line = geda.new_line(100, 100, 150, 100)
geda.add_object(resistor, line)
line = geda.new_line(750, 100, 800, 100)
geda.add_object(resistor, line)

# Create and add the lines that form the resistor body
line = geda.new_line(150, 100, 200, 200)
geda.add_object(resistor, line)
line = geda.new_line(200, 200, 300, 0)
geda.add_object(resistor, line)
line = geda.new_line(300, 0, 400, 200)
geda.add_object(resistor, line)
line = geda.new_line(400, 200, 500, 0)
geda.add_object(resistor, line)
line = geda.new_line(500, 0, 600, 200)
geda.add_object(resistor, line)
line = geda.new_line(600, 200, 700, 0)
geda.add_object(resistor, line)
line = geda.new_line(700, 0, 750, 100)
geda.add_object(resistor, line)

# name, value, x, y, visible, show-option, align, angle
# Create and add standard symbol attributes
refdes = geda.new_attrib("refdes", "R?", 200, 300, VISIBLE, SHOW_VALUE)
geda.add_object(resistor, refdes)

value = geda.new_attrib("value", "Ohms", 600, 300, VISIBLE, SHOW_VALUE)
geda.add_object(resistor, value)

y = 500
device = geda.new_attrib("device", "resistor", 0, y)
geda.add_object(resistor, device)

y = y + 150
descr = geda.new_attrib("description", "resistor", 0, y)
geda.add_object(resistor, descr)

y = y + 150
foot = geda.new_attrib("footprint", "0603", 0, y)
geda.add_object(resistor, foot)

y = y + 150
slots = geda.new_attrib("numslots", "0", 0, y)
geda.add_object(resistor, slots)

y = y + 150
symver = geda.new_attrib("symversion", "0.1", 0, y)
geda.add_object(resistor, symver)

# Write the page to storage
resistor.save()

# Close the file
resistor.close()

# Done!

