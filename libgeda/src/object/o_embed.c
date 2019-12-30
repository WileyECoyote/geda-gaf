/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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

/*! \file o_embed.c
 *  \brief functions to embed and unembed symbols
 */

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>

/*!
 * \brief embed an object into a schematic
 * \par Function Description
 *  This functions embeds an object \a o_current into a
 *  libgeda. Currently complex objects are just marked to
 *  be embedded later. Picture objects are embedded immediatly.
 *
 * \param o_current The GedaObject to embed
 */
bool geda_object_embed(GedaObject *o_current)
{
  int page_modified = 0;

  /* check o_current is a complex and is not already embedded */
  if (GEDA_IS_COMPLEX(o_current) && !geda_complex_object_is_embedded (o_current))
  {
    /* set the embedded flag */
    geda_complex_set_is_embedded(o_current->complex, 1);

    geda_log (_("Component [%s] has been embedded\n"),
                 o_current->complex->filename);
    page_modified = 1;
  }
  else {

    /* If o_current is a picture and is not already embedded */
    if (GEDA_IS_PICTURE(o_current) && !geda_picture_object_is_embedded(o_current))
    {
      page_modified = geda_picture_object_embed (o_current);
    }
  }

  if (page_modified) {
    geda_page_set_changed (geda_object_get_page (o_current), TRUE);
  }
  return (page_modified);
}

/*!
 * \brief unembed an object from a schematic
 * \par Function Description
 *  This functions unembeds an object \a o_current from a libgeda
 *  structure. Complex objects are just marked to be not embedded.
 *  Picture objects are unembedded immediately.
 *
 * \param o_current The GedaObject to unembed
 *
 * \retval  0 on success
 *         -1 o_current is not a valid object
 *          1 o_current is valid but is not embedded
 *          2 Could not find the component in library
 */
int geda_object_unembed(GedaObject *o_current)
{
  int page_modified = 0;
  int result;

  /* check o_current is an embedded complex */
  if (GEDA_IS_COMPLEX(o_current)) {

    if (geda_complex_object_is_embedded (o_current)) {

      const CLibSymbol *sym;

      /* search for the symbol in the library */
      sym = geda_struct_clib_get_symbol_by_name (o_current->complex->filename);

      if (sym == NULL) {
        /* symbol not found in the symbol library: signal an error */
        geda_log_w (_("Could not find component [%s], while trying to "
        "unembed. Component is still embedded\n"),
        o_current->complex->filename);
        result = 2;
      }
      else {
        /* clear the embedded flag */
        o_current->complex->is_embedded = FALSE;

        geda_log (_("Component [%s] has been successfully unembedded\n"),
                  o_current->complex->filename);

        page_modified = 1;
      }
    }
    else {
      result = 1;
    }
  }
  else if (GEDA_IS_PICTURE(o_current)) {

    if (geda_picture_object_is_embedded (o_current)) {

      /* object is a picture and the picture is embedded */
      result = !geda_picture_object_unembed (o_current);

      if (!result) {
        page_modified = 1;
      }
    }
    else {
      result = 1;
    }
  }
  else {
    result = -1;
  }

  if (page_modified) {
    geda_page_set_changed (geda_object_get_page (o_current), TRUE);
    result = 0;
  }

  return result;
}
