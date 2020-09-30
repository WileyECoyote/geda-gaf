/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
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

#include "../../../config.h"
#include <libgeda_priv.h>

/*!
 * \brief Get a list of the currently selected objects.
 * \par Function Description
 *  Retrieves a list of all currently selected component on the
 *  current/active page of \a toplevel, which could be a NULL or
 *  empty GList.
 *
 * \param toplevel #GedaToplevel structure.
 *
 * \return GList of selected components.
 *
 * \sa geda_struct_page_get_selection
 */
GList*
geda_toplevel_struct_get_selection (const GedaToplevel *toplevel)
{
  Page *page;

  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  page = geda_toplevel_get_current_page ((GedaToplevel*)toplevel);

  if (!page)
    return NULL;

  return geda_page_get_selection (page);
}

/*!
 * \brief Get a list of symbols in-use.
 * \par Function Description
 *  Retrieves a list of all symbols currently in use by the component
 *  library module and returns the list.
 *
 * \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free'd.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to geda_struct_free() or geda_struct_refresh().
 *
 * \param toplevel #GedaToplevel structure.
 *
 * \return GList of symbols.
 */
GList*
geda_toplevel_struct_get_symbols (const GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  return geda_struct_clib_get_symbols (toplevel);
}

/*!
 * \brief Search for a page given the filename
 * \par Function Description
 *  Searches in \a toplevel's list of pages for a page with a filename
 *  equal to \a filename.
 *
 * \param toplevel  The GedaToplevel object
 * \param filename  The filename string to search for
 *
 * \return Page pointer to a matching page, NULL otherwise.
 *
 * \sa geda_struct_page_search
 */
Page*
geda_toplevel_struct_get_page_by_name (const GedaToplevel *toplevel,
                                       const char         *filename)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  if (filename) {

    const GList *iter;

    for (iter = geda_toplevel_get_pages(toplevel); iter; NEXT(iter)) {

      Page *page = (Page*)iter->data;

      if (g_ascii_strcasecmp (page->filename, filename) == 0) {
        return page;
      }
    }
  }
  return NULL;
}

/*!
 * \brief Get list of Pages in GedaToplevel object.
 * \par Function Description
 *  Retrieves the list of all loaded pages from the toplevel object.
 *  Where \a toplevel has already been validated, callers might consider
 *  using the geda_toplevel_get_pages macro.
 *
 * \param toplevel #GedaToplevel structure.
 *
 * \return GList of Page objects.
 *
 * \sa geda_toplevel_get_page_list
 */
GList*
geda_toplevel_struct_get_pages (const GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  return geda_list_get_glist(toplevel->pages);
}

/*!
 * \brief Releases resource associated with GedaToplevel object
 * \par Function Description
 *  Decrements the reference count of the toplevel object by one.
 */
void
geda_toplevel_struct_release (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {

    /* Delete all pages */
    geda_struct_page_delete_list (toplevel);

    /* Delete the page list */
    GEDA_UNREF (toplevel->pages);

    geda_toplevel_weakref_notify (toplevel);
    geda_toplevel_unref (toplevel);
  }
}

/*!
 * \brief Set the font-renderer-specific bounds function.
 * \par Function Description
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
 * \sa geda_struct_page_set_bounds_func
 * \sa geda_text_object_set_rendered_bounds_func
 *
 * \param [in] toplevel  The GedaToplevel for which the render
 *                       function should be associated.
 * \param [in] func      Function to use.
 * \param [in] user_data User data to be passed to the function.
 */
void
geda_toplevel_struct_set_rbounds_func(GedaToplevel *toplevel,
                                      RenderedBoundsFunc func, void *user_data)
{
  g_return_if_fail(GEDA_IS_TOPLEVEL(toplevel));

  toplevel->rendered_text_bounds_func = func;
  toplevel->rendered_text_bounds_data = user_data;
}
