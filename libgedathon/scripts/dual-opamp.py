#! /usr/bin/env python

# Dual Opamp Demo

import os, sys
from geda import geda
from geda.constants import *

if len(sys.argv) > 1:
     sym_path = sys.argv[1]
     if not os.path.exists(sym_path):
          sym_path = "/tmp/sch"
else:
     sym_path = "/tmp/sch"

filename = sym_path + "/dual-opamp-py.sym"
opamp = geda.new_page(filename, True)

# Create and add standard symbol attributes
attribute = geda.new_attrib("author", "Python", 0, 1900)
geda.add_object(opamp, attribute)

attribute = geda.new_attrib("device", "LF353", 675, 800)
geda.add_object(opamp, attribute)

attribute = geda.new_attrib("numslots", "2", 0, 1600)
geda.add_object(opamp, attribute)

attribute = geda.new_attrib("slotdef", "1:1,2,3,4,8", 0, 1450)
geda.add_object(opamp, attribute)

attribute = geda.new_attrib("slotdef", "2:5,6,7,4,8", 0, 1300)
geda.add_object(opamp, attribute)

attribute = geda.new_attrib("slot", "1", 0, 1150)
geda.add_object(opamp, attribute)

attribute = geda.new_attrib("footprint", "DIP8", 0, 1750)
geda.add_object(opamp, attribute)

# Create and add the lines that form the opamp symbol
line = geda.new_line(200, 0, 200, 800)
geda.add_object(opamp, line)
line = geda.new_line(200, 800, 800, 400)
geda.add_object(opamp, line)
line = geda.new_line(800, 400, 200, 0)
geda.add_object(opamp, line)

# Add the plus-minus symbol for the inputs
line = geda.new_line(250, 600, 350, 600)
geda.add_object(opamp, line)
line = geda.new_line(300, 650, 300, 550)
geda.add_object(opamp, line)
line = geda.new_line(250, 200, 350, 200)
geda.add_object(opamp, line)

# Add the pins. Note: pin.auto_attributes defaults to True, so the pinlabel,
# pinseq, pinnum, and pintype attributes will automatically be created for us
# when the pins are added to the Page object. For this Demo-example, we "fix"
# the attributes afterwards ...

# syntax: new_pin x1, y1, x2, y2,  whichend, number, label, etype, mtype, ntype
sequence = 1
# Create and add the output Pin object
pin = geda.new_pin(800, 400, 1000, 400, 1, 1, "OUT", PIN_ELECT_OUT)
pin.sequence = sequence
geda.add_object(opamp, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinlabel":
        attrib.visible = 0
        break

sequence = sequence + 1

# Create and add inverting input Pin object
pin = geda.new_pin(200, 200, 0, 200, 1, 2, "IN-", PIN_ELECT_IN)
pin.sequence = sequence
geda.add_object(opamp, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinlabel":
        attrib.visible = 0
        break

# Create and add non-inverting input Pin object
pin = geda.new_pin(200, 600, 0, 600, 1, 3, "IN+", PIN_ELECT_IN)
sequence = sequence + 1
pin.sequence = sequence
geda.add_object(opamp, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinlabel":
        attrib.visible = 0
        break

# Create and add the power pins
pin = geda.new_pin(500, 200, 500, 0, 1, 4, "V-", PIN_ELECT_PWR)
sequence = sequence + 1
pin.sequence = sequence
geda.add_object(opamp, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinnumber":
        attrib.x = attrib.x - 50
        attrib.y = attrib.y - 150 
    else:
        if attrib.name() == "pinlabel":
            attrib.y     = attrib.y - 150
            attrib.x     = attrib.x + 50
            attrib.angle = 0

pin = geda.new_pin(500, 600, 500, 800, 1, 8, "V+", PIN_ELECT_PWR)
sequence = sequence + 1
pin.sequence = sequence
geda.add_object(opamp, pin)
butes = geda.get_attribs(pin)
for attrib in butes:
    if attrib.name() == "pinnumber":
        attrib.y = attrib.y + 100
    else:
        if attrib.name() == "pinlabel":
            attrib.y     = attrib.y + 100
            attrib.x     = attrib.x + 250
            attrib.angle = 0

text = geda.new_text("LM353", 250, 350, 8)
geda.add_object(opamp, text)

attribute = geda.new_attrib("refdes", "U?", 800, 750, VISIBLE, SHOW_VALUE)
geda.add_object(opamp, attribute)

# Write the page to storage
opamp.save()

# Close the file
opamp.close()

# Done!

