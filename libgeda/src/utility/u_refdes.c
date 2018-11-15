/* -*- u_refdes.c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: April, 06, 2014
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <ctype.h>         /* isdigit */

#include <libgeda_priv.h>

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

/*! U0501
 * \brief Return refdes prefixes Associated with IEEE standards
 * \par Function Description
 *  Returns a pointer to IeeeRefDes.
 */
const GedaRefDes *geda_utility_refdes_get_ieee(void)
{
  return IeeeRefDes;
}

/*! U0502
 * \brief Return refdes prefixes Associated with SPICE components
 * \par Function Description
 *  Returns a pointer to SpiceRefDes.
 */
const GedaRefDes *geda_utility_refdes_get_spice(void)
{
  return SpiceRefDes;
}

/*! U0503
 * \brief Return gEDA standard refdes prefixes
 * \par Function Description
 *  Returns a pointer to StdRefDes.
 */
const GedaRefDes *geda_utility_refdes_get_standard(void)
{
  return StdRefDes;
}

/*! U0504
 * \brief Reset the refdes number back to a question mark
 * \par Function Description
 *  If this text object represents a refdes attribute, then
 *  this function resets the refdes number back to a question
 *  mark if the length of the string is less than the buffer
 *  size. This is likely the desirable result since a refdes
 *  attribute larger than 32 characters is likely to be used
 *  for a inter-page connection and not a normal refdes.
 *  In other cases, this function does nothing.
 *
 * \param [in] object      The text object
 */
void geda_utility_refdes_reset(GedaObject *object)
{
  int   index;
  char  buffer[32] = "refdes=\0";
  char *ptr;

  g_return_if_fail (GEDA_IS_TEXT(object));

  index = 7;

  ptr   = object->text->string;

  if (strncmp (ptr, &buffer[0], index) == 0) {

    int len = strlen (object->text->string);

    if (len < sizeof(buffer) - 1) {
      for ( ; index < len; index++) {
        if (isdigit(ptr[index]) ) {
          if (index == 7) { /* first char after "=" can not be a digit */
            break;
          }
          GEDA_FREE (object->text->string);
          buffer[index] = '?';
          buffer[++index] = '\0';
          object->text->string = strdup(&buffer[0]);
          geda_text_object_update_disp_string(object);
          break;
        }
        else {
          buffer[index] = ptr[index];
        }
      }
    }
  }
}

/*! U0505
 * \brief Return first numeric portion of a refdes
 * \par This function accepts both plain text and GedaText objects
 *  Text Objects should be a reference designator attribute, other
 *  wise just pass in the raw text. The return pointer points to a
 *  char in the passed string and should not be freed directly.
 *
 * \param [in] text Either Text object or a string.
 *
 * \returns pointer to the first numeric text character in the text
 *
 *  example 1:
 *
 *       text_digits = geda_utility_refdes_return_numeric (object);
 *
 *  example 2:
 *
 *       text_digits = geda_utility_refdes_return_numeric (attrib->text->string);
 *
 * \sa geda_utility_refdes_reset
 */
char *geda_utility_refdes_return_numeric(const void *text)
{
  if (text != NULL) {

    char *ptr;

    if (GEDA_IS_TEXT(text)) {
      ptr = ((GedaObject*)text)->text->string;
    }
    else {
      ptr = (char*)text;
    }

    if (ptr) {

      if (strlen(ptr) > 1) {

        while ( *ptr && !isdigit(*ptr) ) ptr++;

        if (isdigit(*ptr)) {
          return ptr;
        }
      }
    }
  }
  return NULL;
}

/*! U0506
 * \brief Return Character portion of a refdes
 * \par This function accepts both plain text and GedaText objects
 *  Text Objects should be a reference designator attribute, other
 *  wise just pass in the raw text. The returned string is a newly
 *  allocated array of the character portion of the refdes.
 *
 * \param [in] text Either Text object or a string.
 *
 * \returns pointer to the first numeric text character in the text
 *
 *  example 1:
 *
 *       prefix = geda_utility_refdes_return_prefix (object);
 *
 *  example 2:
 *
 *       prefix = geda_utility_refdes_return_prefix (attrib->text->string);
 *
 * \remarks Caller should GEDA_FREE returned pointer.
 *
 * \sa geda_utility_refdes_reset
 */
char *geda_utility_refdes_return_prefix(const void *text)
{
  if (text != NULL) {

    char *ptr;

    if (GEDA_IS_TEXT(text)) {
      ptr = ((GedaObject*)text)->text->string;
    }
    else {
      ptr = (char*)text;
    }

    if (ptr) {

      ptr = strstr(ptr, "=");

      if (ptr) {

        if (strlen(ptr) > 1) {

          char buffer[6];
          int  index;

          ptr++;
          index = 0;

          do {
            buffer[index] = *ptr++;
            index++;
          } while (*ptr && !isdigit(*ptr));

          buffer[index] = '\0';

          return strdup(&buffer[0]);
        }
      }
    }
  }
  return NULL;
}
