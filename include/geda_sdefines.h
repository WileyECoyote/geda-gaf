/* -*- C header file: geda_sdefines.h indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/** \file geda_sdefines.h
 *
 *  \brief Global String Defines
 *   W.E.Hill Oct 16, 2013 relocated "these" defines from gschem, and
 *   on August 05, 2015 from libgeda, so all gaf's can reference.
 *
 *   \defgroup geda-global-strings Global String Constants
 * @{\par This group defines global string constants
 *   \ingroup (geda-globals)
 */

/*!
 *  \def SCHEMATIC_FILE_SUFFIX      File extension chanaraters for Schematic
 *  \def SCHEMATIC_FILE_DOT_SUFFIX  Schematic extension
 *  \def SCHEMATIC_FILTER           Filter string used to filter Schematic
 *  \def SYMBOL_FILE_SUFFIX         File extension chanaraters for Symbols
 *  \def SYMBOL_FILE_DOT_SUFFIX     Symbol extension
 *  \def SYMBOL_FILTER              Filter string used to filter Symbols
 */
#define SCHEMATIC_FILE_SUFFIX      "sch"
#define SCHEMATIC_FILE_DOT_SUFFIX  ".sch"
#define SCHEMATIC_FILTER           "*.sch"

#define SYMBOL_FILE_SUFFIX         "sym"
#define SYMBOL_FILE_DOT_SUFFIX     ".sym"
#define SYMBOL_FILTER              "*.sym"

/** @} endgroup geda-global-strings */