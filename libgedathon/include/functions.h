/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file functions.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 17, 2013
 */

#ifndef FUNCTION

#ifndef MODULE_WAIT_INTERVAL
#define MODULE_WAIT_INTERVAL      100   /* microseconds */
#define MAX_WAIT_FOR_MODULE        10
#endif

/* The next block is only seen by the MODULE file on the first pass */
#ifdef FIRST_PASS_FUNCTIONS  /* Macro Definition doubles as inclusion flag */

#define DEFAULT_CAPACITOR_SYMBOL    "capacitor-1"
#define DEFAULT_ELECTROLYTIC_SYMBOL "electrolytic-2"
#define DEFAULT_INDUCTOR_SYMBOL     "inductor-1"
#define DEFAULT_OPAMP_SYMBOL        "opamp-1"
#define DEFAULT_RESISTOR_SYMBOL     "resistor-1"
#define DEFAULT_TITLEBLOCK_SYMBOL   "title-B"
#define DEFAULT_TRANSFORMER_SYMBOL  "transformer-1"

#ifndef QUOTE_SYMBOL
#define QUOTE_SYMBOL(symbol) #symbol
#define ADD_QUOTE(...) QUOTE_SYMBOL(__VA_ARGS__)
#define ASSOCIATED_FUNCTION(function) ADD_QUOTE(function), do_##function
#endif

static PyObject *do_unknown(PyObject *self, PyObject *args, PyObject *kwds);

const char DefaultCapacitorSymbol_docs[]    = N_("Get or Set the default symbol for capacitors\n");
const char DefaultElectrolyticSymbol_docs[] = N_("Get or Set the default symbol for electrolytic capacitors\n");
const char DefaultInductorSymbol_docs[]     = N_("Get or Set the default symbol for inductors\n");
const char DefaultOpAmpSymbol_docs[]        = N_("Get or Set the default operational amplifier symbol\n");
const char DefaultResistorSymbol_docs[]     = N_("Get or Set the default symbol for resistors\n");
const char DefaultTitleblockSymbol_docs[]   = N_("Get or Set the default titleblock symbol\n");
const char DefaultTransformerSymbol_docs[]  = N_("Get or Set the default transformer symbol\n");

const char AddArc_docs[]          = "(Page, x, y, radius, start-angle, end-angle [, color])\n";
const char AddAttribute_docs[]    = "(page, name, value, x, y [, visible [, show [, alignment [, angle]]]])";
const char AddBox_docs[]          = "(Page, upper_x, upper_y, lower_x, lower_y [, color])\n";
const char AddBus_docs[]          = "(Page, x1, y1, x2, y2, [, name [, color]])\n";
const char AddComponent_docs[]    = "(Page, name, refdes, x, y [, angle [, mirror [, embed]]])\n";
const char AddCapacitor_docs[]    = "(Page, x, y, value [,refdes [, angle [, mirror [, embed]]]])\n";
const char AddCircle_docs[]       = "(Page, x, y, radius [, color])\n";
const char AddElectrolytic_docs[] = "(Page, x, y, value [,refdes [, angle [, mirror [, embed]]]])\n";
const char AddInductor_docs[]     = "(Page, x, y, value [,refdes [, angle [, mirror [, embed]]]])\n";
const char AddLine_docs[]         = "(Page, x1, y1, x2, y2, [, color]])\n";
const char AddNet_docs[]          = "(Page, x1, y1, x2, y2, [, name [, color]])\n";
const char AddOpAmp_docs[]        = "(Page, x, y, [refdes [, slot [, angle [, mirror [, embed]]]]])\n";
const char AddPath_docs[]         = "(Page, path-string [, color])\n";
const char AddPicture_docs[]      = "(Page, name, x1, y1, x2, y2 [, angle [, mirror [, embed]]])\n";
const char AddPin_docs[]          = "(Page, x1, y1, x2, y2, [, name [, color]])\n";
const char AddResistor_docs[]     = "(Page, x, y, value [,refdes [, angle [, mirror [, embed]])\n";
const char AddSource_docs[]       = "(Page, name, x, y [, angle [, mirror [, embed]])\n";
const char AddText_docs[]         = "(page, string, x, y [, size [, alignment [, angle]]])";
const char AddTitleblock_docs[]   = "(page, x, y [, embed])";
const char AddTransformer_docs[]  = "(Page, x, y, [,refdes [, angle [, mirror [, embed]]]])\n";

/* define Macro for declarations because FUNCTION was not defined */
#define FUNCTION(function) FIRST_PASS_FUNCTIONS(function);

        FUNCTION ( DefaultCapacitorSymbol )
        FUNCTION ( DefaultElectrolyticSymbol )
        FUNCTION ( DefaultInductorSymbol )
        FUNCTION ( DefaultOpAmpSymbol )
        FUNCTION ( DefaultResistorSymbol )
        FUNCTION ( DefaultTitleblockSymbol )
        FUNCTION ( DefaultTransformerSymbol )

        FUNCTION ( AddArc)
        FUNCTION ( AddAttribute )
        FUNCTION ( AddBox )
        FUNCTION ( AddBus )
        FUNCTION ( AddComponent )
        FUNCTION ( AddCapacitor )
        FUNCTION ( AddCircle )
        FUNCTION ( AddElectrolytic )
        FUNCTION ( AddInductor )
        FUNCTION ( AddLine )
        FUNCTION ( AddNet )
        FUNCTION ( AddOpAmp )
        FUNCTION ( AddPath )
        FUNCTION ( AddPicture )
        FUNCTION ( AddPin )
        FUNCTION ( AddResistor )
        FUNCTION ( AddSource )
        FUNCTION ( AddText )
        FUNCTION ( AddTitleblock )
        FUNCTION ( AddTransformer )

#undef  FUNCTION

#endif

/* Both the Library and the Module see this on the first load */
/* Define a flag so that closing brace and semicolon is added on this pass */
#define _MAKE_FUNCTION_ENUM_

/* MACRO Enumerate function base name on this pass */
#define FUNCTION(function, aflag ) e##function,

enum {
        function_unknown,

#endif  /* METHOD was not defined when the header was loaded so enumerate */

     FUNCTION ( DefaultCapacitorSymbol,     METH_VARARGS )
     FUNCTION ( DefaultElectrolyticSymbol,  METH_VARARGS )
     FUNCTION ( DefaultInductorSymbol,      METH_VARARGS )
     FUNCTION ( DefaultOpAmpSymbol,         METH_VARARGS )
     FUNCTION ( DefaultResistorSymbol,      METH_VARARGS )
     FUNCTION ( DefaultTitleblockSymbol,    METH_VARARGS )
     FUNCTION ( DefaultTransformerSymbol,   METH_VARARGS )

     FUNCTION ( AddArc,                     METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddAttribute,               METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddBox,                     METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddBus,                     METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddComponent,               METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddCapacitor,               METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddCircle,                  METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddElectrolytic,            METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddInductor,                METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddLine,                    METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddNet,                     METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddOpAmp,                   METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddPath,                    METH_VARARGS )
     FUNCTION ( AddPicture,                 METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddPin,                     METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddResistor,                METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddSource,                  METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddText,                    METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddTitleblock,              METH_VARARGS|METH_KEYWORDS )
     FUNCTION ( AddTransformer,             METH_VARARGS|METH_KEYWORDS )

#ifdef _MAKE_FUNCTION_ENUM_
     FUNCTION_COUNT,
};
#undef _MAKE_FUNCTION_ENUM_
#endif /* _MAKE_FUNCTION_ENUM_ */
#undef FUNCTION
