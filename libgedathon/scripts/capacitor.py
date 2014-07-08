#! /usr/bin/env python

# Capacitor Demo

import os, sys
from geda import geda
from geda.constants import *

if len(sys.argv) > 1:
     sym_path = sys.argv[1]
     if not os.path.exists(sym_path):
          sym_path = "/tmp/sch"
else:
     sym_path = "/tmp/sch"

filename = sym_path + "/capacitor-py.sym"
capacitor = geda.new_page(filename, True)

pin = geda.new_pin(0, 200, 200, 200, 0, 1, "1", PIN_ELECT_PAS, 0, 0)

#auto attributes are not really intended for passives, so turn off.
pin.auto_attributes = False

x = ATTRIBUTE_OFFSET
y1 = 200 + ATTRIBUTE_OFFSET
y2 = 200 - ATTRIBUTE_OFFSET

# also, the default attribute properties are not optimized for passives, so
# over-ride as needed...
pinlabel = geda.new_attrib("pinlabel", "1", x, y1, INVISIBLE, SHOW_VALUE)
geda.add_object(pin, pinlabel)
pinseq = geda.new_attrib("pinseq",     "1", x, y1, INVISIBLE, SHOW_NAME_VALUE)
geda.add_object(pin, pinseq)
pinnum = geda.new_attrib("pinnumber",  "1", x, 200, INVISIBLE)
geda.add_object(pin, pinnum)
pintype = geda.new_attrib("pintype", "pas", x, y2, INVISIBLE, SHOW_VALUE)
geda.add_object(pin, pintype)

# Add the Pin object to the page, attributes will be added automatically
geda.add_object(capacitor, pin)

# Create the second Pin object
pin = geda.new_pin(900, 200, 700, 200, 0, 2, "2", PIN_ELECT_PAS, 0, 0)
pin.auto_attributes = False

# Add the Pin object to the page
geda.add_object(capacitor, pin)

x = 900 - ATTRIBUTE_OFFSET
y1 = 200 + ATTRIBUTE_OFFSET
y2 = 200 - ATTRIBUTE_OFFSET

pinlabel = geda.new_attrib("pinlabel", "2", x, y1, INVISIBLE, SHOW_VALUE, LOWER_RIGHT)
geda.add_object(pin, pinlabel)
pinseq = geda.new_attrib("pinseq",     "2", x, y1, INVISIBLE, SHOW_NAME_VALUE)
geda.add_object(pin, pinseq)
pinnum = geda.new_attrib("pinnumber",  "2", x, 200, INVISIBLE, SHOW_VALUE)
geda.add_object(pin, pinnum)
pintype = geda.new_attrib("pintype", "pas", x, y2, INVISIBLE, SHOW_VALUE)
geda.add_object(pin, pintype)

# Create and add the lines that form the capacitor symbol
line = geda.new_line(400, 400, 400, 0)
geda.add_object(capacitor, line)
line = geda.new_line(500, 400, 500, 0)
geda.add_object(capacitor, line)
line = geda.new_line(700, 200, 500, 200)
geda.add_object(capacitor, line)
line = geda.new_line(400, 200, 200, 200)
geda.add_object(capacitor, line)

# Create and add standard symbol attributes
device = geda.new_attrib("device", "CAPACITOR", 200, 700)
geda.add_object(capacitor, device)

value = geda.new_attrib("value", "uF", 600, 500, VISIBLE, SHOW_VALUE)
geda.add_object(capacitor, value)

refdes = geda.new_attrib("refdes", "C?", 200, 500, VISIBLE, SHOW_VALUE)
geda.add_object(capacitor, refdes)

descr = geda.new_attrib("description", "capacitor", 200, 1300)
geda.add_object(capacitor, descr)

slots = geda.new_attrib("numslots", "0", 200, 1100)
geda.add_object(capacitor, slots)

symver = geda.new_attrib("symversion", "0.1", 200, 900)
geda.add_object(capacitor, symver)

foot = geda.new_attrib("footprint", "0402", 600, 100)
geda.add_object(capacitor, foot)

# Write the page to storage
capacitor.save()

# Close the file
capacitor.close()

# Done!

