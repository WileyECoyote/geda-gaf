/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_file_filter.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 Wiley Edward Hill
 * Copyright (C) 2013-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 *  Date: October, 18, 2013
 *  Contributing Author: Wiley Edward Hill
 *
 * \remark Filter functionality is implemented in geda_file_chooser.c,
 *         enumerators that were here were relocated to <geda_file_chooser.h>
 *         so that applications would not need to include this file.
 */

typedef struct _filter_reg_t GedaFileFilterDataDef;

struct _filter_reg_t {
  IDE_FILTER id;
  const char *name;
  const char *const *pattern;

};

/*! \def GEDA_FILTER_SCHEMATIC #SCHEMATIC_FILE_SUFFIX */
#define GEDA_FILTER_SCHEMATIC  \
  { FILTER_SCHEMATIC,  "Schematics", (const char* const []) \
  {SCHEMATIC_FILTER, NULL}, }

  /*! \def GEDA_FILTER_SYMBOL #SYMBOL_FILE_SUFFIX */
#define GEDA_FILTER_SYMBOL     \
  { FILTER_SYMBOL,     "Symbols",    (const char* const []) \
  { SYMBOL_FILTER, NULL} }

/*! \def GEDA_FILTER_GSCHEM #SCHEMATIC_FILE_SUFFIX & #SYMBOL_FILE_SUFFIX */
#define GEDA_FILTER_GSCHEM       \
  { FILTER_GSCHEM,     "Schematics and symbols", (const char * const []) \
  { SCHEMATIC_FILTER, SYMBOL_FILTER, NULL} }

/*! \def GEDA_FILTER_PNG #PNG_FILE_SUFFIX */
#define GEDA_FILTER_PNG  \
  { FILTER_PNG,        IMAGE_FILTER_PNG, (const char* const []) \
  { IMAGE_FILTER_PNG, NULL}, }

/*! \def GEDA_FILTER_JPG #JPG_FILE_SUFFIX */
#define GEDA_FILTER_JPG  \
  { FILTER_JPG,       IMAGE_FILTER_JPG, (const char* const []) \
  { IMAGE_FILTER_JPG, IMAGE_FILTER_JPEG, NULL}, }

/*! \def GEDA_FILTER_GIF #GIF_FILE_SUFFIX */
#define GEDA_FILTER_GIF  \
  { FILTER_GIF,        IMAGE_FILTER_GIF, (const char* const []) \
  { IMAGE_FILTER_GIF,  NULL}, }

/*! \def GEDA_FILTER_BMP #BMP_FILE_SUFFIX */
#define GEDA_FILTER_BMP  \
  { FILTER_BMP,        IMAGE_FILTER_BMP, (const char* const []) \
  { IMAGE_FILTER_BMP,  NULL}, }

/*! \def GEDA_FILTER_ICO #ICO_FILE_SUFFIX */
#define GEDA_FILTER_ICO  \
  { FILTER_ICO,        IMAGE_FILTER_ICO, (const char* const []) \
  { IMAGE_FILTER_ICO,  NULL}, }

/*! \def GEDA_FILTER_TIF #TIF_FILE_SUFFIX */
#define GEDA_FILTER_TIF  \
  { FILTER_TIF,        IMAGE_FILTER_TIFF, (const char* const []) \
  { IMAGE_FILTER_TIF,  IMAGE_FILTER_TIFF, NULL}, }

/*! \def GEDA_FILTER_XPM #XPM_FILE_SUFFIX */
#define GEDA_FILTER_XPM  \
  { FILTER_XPM,        IMAGE_FILTER_XPM, (const char* const []) \
  { IMAGE_FILTER_XPM,  NULL}, }

/*! \def GEDA_FILTER_PNM #PNM_FILE_SUFFIX */
#define GEDA_FILTER_PNM \
  { FILTER_PNM,        IMAGE_FILTER_PNM, (const char* const []) \
  { IMAGE_FILTER_PNM,  IMAGE_FILTER_PBM, IMAGE_FILTER_PPM,      \
    IMAGE_FILTER_PGM,  NULL}, }

/*! \def GEDA_FILTER_RAS #RAS_FILE_SUFFIX */
#define GEDA_FILTER_RAS  \
  { FILTER_RAS,        IMAGE_FILTER_RAS, (const char* const []) \
  { IMAGE_FILTER_RAS,  NULL}, }

/*! \def GEDA_FILTER_IMAGES #ALL_IMAGE_FILE_SUFFIXES */
#define GEDA_FILTER_IMAGES  \
  { FILTER_IMAGES,     "All Images", (const char* const [])  \
  { IMAGE_FILTER_PNG, IMAGE_FILTER_JPG, IMAGE_FILTER_JPEG, IMAGE_FILTER_GIF,  \
    IMAGE_FILTER_BMP, IMAGE_FILTER_ICO, IMAGE_FILTER_TIF,  IMAGE_FILTER_TIFF, \
    IMAGE_FILTER_XPM, IMAGE_FILTER_PNM, IMAGE_FILTER_PBM,  IMAGE_FILTER_PPM,  \
    IMAGE_FILTER_PGM, IMAGE_FILTER_RAS, NULL}, }

#define GEDA_FILTER_NONE       \
  { FILTER_NONE,      "All files", (const char * const []) \
  { "*", NULL} }

#define GEDA_NO_MORE_FILTERS   \
  { 0,  NULL, NULL }
