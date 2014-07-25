/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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

#include <config.h>

#include <stdio.h>

#include "libgeda_priv.h"

/*! \brief embed an object into a schematic
 *  \par Function Description
 *  This functions embeds an object \a o_current into a
 *  libgeda. Currently complex objects are just marked to
 *  be embedded later. Picture objects are embedded immediatly.
 *
 *  \param toplevel  The GedaToplevel object
 *  \param o_current The Object to embed
 */
void o_embed(GedaToplevel *toplevel, Object *o_current)
{
  Page *page = geda_object_get_page (o_current);
  int page_modified = 0;

  /* check o_current is a complex and is not already embedded */
  if (GEDA_IS_COMPLEX(o_current) && !o_complex_is_embedded (o_current))
  {
    /* set the embedded flag */
    o_current->complex->is_embedded = TRUE;

    u_log_message (_("Component [%s] has been embedded\n"),
                   o_current->complex->filename);
    page_modified = 1;
  }

  /* If it's a picture and it's not embedded */
  if ( (o_current->type == OBJ_PICTURE) &&
       !o_picture_is_embedded (o_current) ) {
    o_picture_embed (o_current);

    page_modified = 1;
  }

  if (page_modified && page != NULL) {
    /* page content has been modified */
    page->CHANGED = 1;
  }
}

/*! \brief unembed an object from a schematic
 *  \par Function Description
 *  This functions unembeds an object \a o_current from a
 *  libgeda structure. Complex objects are just marked to
 *  be not embedded. Picture objects are unembeded immediatly.
 *
 *  \param toplevel  The GedaToplevel object
 *  \param o_current The Object to unembed
 */
void o_unembed(GedaToplevel *toplevel, Object *o_current)
{
  const CLibSymbol *sym;
  Page *page = geda_object_get_page (o_current);
  int page_modified = 0;

  /* check o_current is an embedded complex */
  if (GEDA_IS_COMPLEX(o_current) && o_complex_is_embedded (o_current))
  {

    /* search for the symbol in the library */
    sym = s_clib_get_symbol_by_name (o_current->complex->filename);

    if (sym == NULL) {
      /* symbol not found in the symbol library: signal an error */
      u_log_message (_("Could not find component [%s], while trying to "
                       "unembed. Component is still embedded\n"),
                     o_current->complex->filename);

    } else {
      /* clear the embedded flag */
      o_current->complex->is_embedded = FALSE;

      u_log_message (_("Component [%s] has been successfully unembedded\n"),
                     o_current->complex->filename);

      page_modified = 1;
    }
  }

  /* If it's a picture and it's embedded */
  if ( GEDA_IS_PICTURE(o_current) &&
       o_picture_is_embedded (o_current))
  {
    o_picture_unembed (o_current);
    page_modified = 1;
  }

  if (page_modified && page != NULL) {
    page->CHANGED = 1;
  }
}
