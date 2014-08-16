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

#include <geda_struct.h>

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