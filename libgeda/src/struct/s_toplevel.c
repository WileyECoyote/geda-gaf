/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>

#include "libgeda_priv.h"

/*!
 *  \brief Create a TOPLEVEL object
 *  \par Function Description
 *  Create and return an empty TOPLEVEL object with sensible defaults
 *  for its properties.
 *
 *  \returns the newly created TOPLEVEL.
 *
 *  \todo rethink block below that is set in gschem but used in libgeda.
 */
GedaToplevel *s_toplevel_new (void)
{
  GedaToplevel *toplevel = geda_toplevel_new();
  return toplevel;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_toplevel_release (GedaToplevel *toplevel)
{
  geda_toplevel_weakref_notify (toplevel);
  geda_toplevel_unref (toplevel);
}

/*! \brief Set the font-renderer-specific bounds function.
 *  \par Function Description
 *  Set the function to be used to calculate text bounds for #Text
 *  Objects for all pages associated with the given #GedaToplevel.
 *  This allows a global page renderer function to be defined. If the
 *  function is not defined the renderer, and neither a Page level or
 *  Text Object level render is defined then the bounds of text can
 *  not be determined and world_get_text_bounds will return FALSE.
 *  A renderer must be define for at least one level. The order of
 *  precedence is Object level, then the Page level and lastly the
 *  GedaToplevel. Note that any previous setting is erased and
 *  passing NULL will disable rendering at this level.
 *
 *  \sa s_page_set_bounds_func
 *  \sa o_text_set_rendered_bounds_func
 *
 *  \param [in] toplevel  The GedaToplevel for which the render
 *                        function should be associated.
 *  \param [in] func      Function to use.
 *  \param [in] user_data User data to be passed to the function.
 */
void
s_toplevel_set_rendered_bounds_func(GedaToplevel *toplevel,
                                    RenderedBoundsFunc func, void *user_data)
{
  g_return_if_fail(GEDA_IS_TOPLEVEL(toplevel));

  toplevel->rendered_text_bounds_func = func;
  toplevel->rendered_text_bounds_data = user_data;
}