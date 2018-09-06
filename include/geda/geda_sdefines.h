/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_sdefines.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/* ------------------------------------------------------------------
 * WEH | 10/16/13 | Relocated defines from gschem.
 * ------------------------------------------------------------------
 * WEH | 08/05/15 | Relocated defines from libgeda.
*/
/** \file geda_sdefines.h
 *
 *  \brief Global gEDA String Defines
 *
 *   \defgroup geda-global-strings Global String Constants
 * @{\ingroup geda-globals
 *   \par
 *    This group defines global string constants used in the gEDA suite.
 */

#ifndef GEDA_STR_DEFINES_H
#define GEDA_STR_DEFINES_H

#define DEFAULT_FONT_NAME          "Arial"

/*!
 *  \def SCHEMATIC_FILE_SUFFIX      File extension characters for Schematic
 *  \def SCHEMATIC_FILE_DOT_SUFFIX  Schematic extension
 *  \def SCHEMATIC_FILTER           Filter string used to filter Schematic
 *  \def SYMBOL_FILE_SUFFIX         File extension characters for Symbols
 *  \def SYMBOL_FILE_DOT_SUFFIX     Symbol extension
 *  \def SYMBOL_FILTER              Filter string used to filter Symbols
 */
#define SCHEMATIC_FILE_SUFFIX      "sch"
#define SCHEMATIC_FILE_DOT_SUFFIX  ".sch"
#define SCHEMATIC_FILTER           "*.sch"

#define SYMBOL_FILE_SUFFIX         "sym"
#define SYMBOL_FILE_DOT_SUFFIX     ".sym"
#define SYMBOL_FILTER              "*.sym"

/*!
 *  \def IMAGE_FILTER_PNG    File extension for Portable Network Graphics files
 *  \def IMAGE_FILTER_JPG    File extension for Joint Photographic Experts Group files
 *  \def IMAGE_FILTER_JPEG   File extension for Joint Photographic Experts Group files
 *  \def IMAGE_FILTER_GIF    File extension for Graphics Interchange File Format
 *  \def IMAGE_FILTER_BMP    File extension for Windows raw bitmap files
 *  \def IMAGE_FILTER_ICO    File extension for Icon image files
 *  \def IMAGE_FILTER_TIF    File extension for Tagged Image File Format
 *  \def IMAGE_FILTER_TIFF   File extension for Tagged Image File Format
 *  \def IMAGE_FILTER_XPM    File extension for X Windows Pixel Map files
 *  \def IMAGE_FILTER_PNM    File extension for Portable anymap format
 *  \def IMAGE_FILTER_PBM    File extension for Portable bitmap format
 *  \def IMAGE_FILTER_PPM    File extension for Portable pixmap format
 *  \def IMAGE_FILTER_PGM    File extension for Portable graymap format
 *  \def IMAGE_FILTER_RAS    File extension for Raster Image Files
 */

#define IMAGE_FILTER_PNG           "*.png"
#define IMAGE_FILTER_JPG           "*.jpg"
#define IMAGE_FILTER_JPEG          "*.jpeg"
#define IMAGE_FILTER_GIF           "*.gif"
#define IMAGE_FILTER_BMP           "*.bmp"
#define IMAGE_FILTER_ICO           "*.ico"
#define IMAGE_FILTER_TIF           "*.tif"
#define IMAGE_FILTER_TIFF          "*.tiff"
#define IMAGE_FILTER_XPM           "*.xpm"
#define IMAGE_FILTER_PNM           "*.pnm"
#define IMAGE_FILTER_PBM           "*.pbm"
#define IMAGE_FILTER_PPM           "*.ppm"
#define IMAGE_FILTER_PGM           "*.pgm"
#define IMAGE_FILTER_RAS           "*.ras"

/** @} endgroup geda-global-strings */

#endif /* GEDA_STR_DEFINES_H */