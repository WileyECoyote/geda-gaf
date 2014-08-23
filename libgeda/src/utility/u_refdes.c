/* -*- u_refdes.c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: April, 06, 2014
 */

#include <ctype.h>         /* isdigit */

#include <geda_struct.h>
#include "libgeda.h"

static
GedaRefDes StdRefDes[] = {
                             {"A",     "Directive"},
                             {"ANT",   "Antanna"},
                             {"AT",    "Attenuator"},
                             {"BAT",   "Battery"},
                             {"C",     "Capacitor"},
                             {"CB",    "Circuit Breaker"},
                             {"CN",    "Capacitor network"},
                             {"D",     "Diode"},
                             {"DS",    "Display"},
                             {"F",     "Fuse"},
                             {"FAN",   "Fan"},
                             {"FB",    "Ferrite Bead"},
                             {"G",     "Generator"},
                             {"I",     "Current Source"},
                             {"J",     "Jack (for plug)"},
                             {"L",     "Inductor"},
                             {"LED",   "Light Emitting Diode"},
                             {"M",     "Motor"},
                             {"MIC",   "Microphone"},
                             {"P",     "Plug (into jack)"},
                             {"PS",    "Power Supply"},
                             {"Q",     "Transistor"},
                             {"R",     "Resistor"},
                             {"RL",    "Relay"},
                             {"RN",    "Resistor Network"},
                             {"RT",    "Thermistor"},
                             {"RV",    "Varistor"},
                             {"SPK",   "Speaker"},
                             {"SW",    "Switch"},
                             {"TC",    "Thermocouple"},
                             {"TP",    "Test Point"},
                             {"T",     "Transformer"},
                             {"U",     "Integrated Circuit"},
                             {"V",     "Voltage Source"},
                             {"VR",    "Variable Resistor"},
                             {"Y",     "Crystal"},
                             {"X",     "Transducer"},
                             {"Z",     "Zener Diode"},
                             { '\0'}
};

static
GedaRefDes SpiceRefDes[] = {
                             {"A",     "directive"},
                             {"C",     "CAPACITOR"},
                             {"D",     "DIODE"},
                             {"E",     "SPICE-vscs"},
                             {"F",     "SPICE-cccs"},
                             {"G",     "SPICE-vccs"},
                             {"H",     "SPICE-ccvs"},
                             {"I",     "CURRENT_SOURCE"},
                             {"J",     "FET_TRANSISTOR"},
                             {"L",     "INDUCTOR"},
                             {"M",     "MOS_TRANSISTOR"},
                             {"Q",     "TRANSISTOR"},
                             {"R",     "RESISTOR"},
                             {"U",     "IC"},
                             {"V",     "VOLTAGE_SOURCE"},
                             {"X",     "IC"},
                             {"Z",     "MESFET"},
                             { '\0'}
};

static
GedaRefDes IeeeRefDes[] = {
                             {"A",     "Separable assembly or sub-assembly"},
                             {"AT",    "Attenuator or isolator"},
                             {"BR",    "Bridge rectifier"},
                             {"BT",    "Battery"},
                             {"C",     "Capacitor"},
                             {"CN",    "Capacitor network"},
                             {"D",     "Diode (including LEDs)"},
                             {"DL",    "Delay line"},
                             {"DS",    "Display"},
                             {"F",     "Fuse"},
                             {"FB",    "Ferrite bead"},
                             {"FD",    "Fiducial"},
                             {"FL",    "Filter"},
                             {"G",     "generator or oscillator"},
                             {"GN",    "General network"},
                             {"HY",    "circulator or directional coupler"},
                             {"J",     "Receptacle Jack"},
                             {"JP",    "Link (Jumper)"},
                             {"K",     "Relay or contactor"},
                             {"L",     "Inductor or coil or ferrite bead"},
                             {"LS",    "Loudspeaker or buzzer"},
                             {"M",     "Motor"},
                             {"MK",    "Microphone"},
                             {"MP",    "Mechanical part, screws and fasteners"},
                             {"P",     "Plug"},
                             {"PS",    "Power supply"},
                             {"Q",     "Transistor (all types)"},
                             {"R",     "Resistor"},
                             {"RN",    "Resistor network"},
                             {"RT",    "Thermistor"},
                             {"RV",    "Varistor"},
                             {"S",     "Switch (all types)"},
                             {"T",     "Transformer"},
                             {"TC",    "Thermocouple"},
                             {"TUN",   "Tuner"},
                             {"TP",    "Test point"},
                             {"U",     "Inseparable assembly"},
                             {"V",     "Vacuum tube"},
                             {"VR",    "Variable resistor"},
                             {"X",     "Receptacle socket"},
                             {"XA",    "Receptacle socket for printed circuit assembly"},
                             {"XF",    "Receptacle socket for fuse holder"},
                             {"XV",    "Receptacle socket for vacuum tube"},
                             {"Y",     "Crystal or oscillator"},
                             {"Z",     "Zener diode"},
                             { '\0'}
};

const GedaRefDes*
u_refdes_get_standard_designators()
{
  return StdRefDes;
}

const GedaRefDes*
u_refdes_get_spice_designators()
{
  return SpiceRefDes;
}

const GedaRefDes*
u_refdes_get_ieee_designators()
{
  return IeeeRefDes;
}

/*! \brief Reset the refdes number back to a question mark
 *
 *  \par If this text object represents a refdes attribute,
 *  then this function resets the refdes number back to the
 *  question mark. In other cases, this function does
 *  nothing.
 *
 *  \param [in] object      The text object
 */
void u_refdes_reset(Object *object)
{
  int   len;
  int   index;
  char  buffer[16] = "refdes=\0";
  char *ptr;

  g_return_if_fail (GEDA_IS_TEXT(object));

  len   = 0;
  index = 7;

  ptr   = object->text->string;

  if ( strncmp ( ptr, &buffer[0], index) == 0 ) {

    len = strlen (object->text->string);

    for ( ; index < len; index++) {
      if ( isdigit(ptr[index]) ) {
        GEDA_FREE (object->text->string);
        buffer[index] = '?';
        buffer[++index] = '\0';
        object->text->string= strdup(&buffer[0]);
        o_text_recreate (object);
        break;
      }
      else
        buffer[index] = ptr[index];
    }
  }
}

/*! \brief Return first numeris portionof a refdes
 *
 *  \par This function accepts both plain text and GedaText objects
 *  Text Objects should be a reference designator attribute, other
 *  wise just pass in the raw text. The return pointer point to a
 *  char in the passed string and should not be freed directly.
 *
 *  \param [in] text Either Text object or a string.
 *
 *  \returns pointer to the first numeric text character in the text
 *
 *  example 1:
 *
 *       text_digits = u_refdes_return_numeric (object);
 *
 *  example 2:
 *
 *       text_digits = u_refdes_return_numeric (attrib->text->string);
 *
 *  \sa u_refdes_reset
 */
char *u_refdes_return_numeric(void *text)
{
  char *ptr;
  char *str_out = NULL;

  if (text != NULL) {

    if (GEDA_IS_TEXT(text)) {
      ptr   = ((Object*)text)->text->string;
    }
    else {
      ptr = text;
    }

    while ( *ptr && !isdigit(*ptr) ) ptr++;

    if (isdigit(*ptr)) {
      str_out = ptr;
    }
  }

  return str_out;
}