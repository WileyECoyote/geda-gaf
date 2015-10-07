/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
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

/*! \brief Get a list of symbols in-use.
 *  \par Function Description
 *
 *  Retrieves a list of all symbols currently in use by the component
 *  library module and returns the list.
 *
 *  \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free'd.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to s_clib_free() or s_clib_refresh().
 *
 *  \param toplevel #GedaToplevel structure.
 *
 *  \return GList of symbols.
 */
GList *s_toplevel_get_symbols (const GedaToplevel *toplevel)
{
  g_return_val_if_fail ((toplevel != NULL), NULL);

  return s_clib_get_symbols (toplevel);
}

/*! \brief Releases resource associated with GedaToplevel object
 *  \par Function Description
 *  Decrements the reference count of the toplevel object by one.
 */
void
s_toplevel_release (GedaToplevel *toplevel)
{
  geda_toplevel_weakref_notify (toplevel);
  geda_toplevel_unref (toplevel);
}

/*! \brief Set Backup Loader Query Function in GedaToplevel object
 *  \par Function Description
 *  Sets function to be call when a files is requested to be loaded
 *  and a newer backup file is detected.
 *
 *  \param [in] toplevel  The GedaToplevel object being set
 *  \param [in] func      Function to call if a backup is newer.
 *  \param [in] ...       Optional data to be passed to the function.
 */
void
s_toplevel_set_backup_loader_query_func (GedaToplevel *toplevel, void *func, ...)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    va_list argp;
    va_start (argp, func);
    toplevel->load_newer_backup_func = func;
    toplevel->load_newer_backup_data = va_arg(argp, void*);
    va_end (argp);
  }
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