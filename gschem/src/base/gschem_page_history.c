/* -*- C indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id gschem_page_history.c $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 *
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 * Date Contributed: March, 31, 2016
 */
/*!
 * \file gschem_page_history.c
 *
 * \brief Maintains History of Page Change
 * \par
 *  The GschemPageHistory class is used to implement browser-like
 *  page forward and back buttons.
 *
 * \todo maintains unlimited history, connect signal and limit.
 */

#include <gschem.h>

static Page*
gschem_page_history_pop_back (GschemPageHistory *history)
{
  GList *back = geda_list_get_glist(history->pages_back);

  if (back && back->data) {

    Page *page = back->data;

    /* only removes the first occurence */
    geda_list_remove (history->pages_back, page);

    return GEDA_IS_PAGE(page) ? page : NULL;
  }

  return NULL;
}

static Page*
gschem_page_history_pop_forward (GschemPageHistory *history)
{
  GList *forw = geda_list_get_glist(history->pages_forw);

  if (forw && forw->data) {

    Page *page = forw->data;

    geda_list_remove (history->pages_forw, page);

    return page;
  }

  return NULL;
}

/*!
 * \brief Add a Page to the Back History
 * \par Function Description
 *  Adds \a page to the top of the pages-back history stack if
 *  \a page is not the page on top of the stack. This is not
 *  just a feature, this is instrumental to the operation of
 *  the page history logic.
 *
 * \param [in] history GschemPageHistory object,
 * \param [in] page    Page to be removed from history.
 */
void
gschem_page_history_push_back (GschemPageHistory *history, Page *page)
{
  if (GEDA_IS_PAGE(page)) {

    /* Do not prepend if page is the previous page! */
    GList *back = geda_list_get_glist(history->pages_back);

    if (!back || back->data != page) {
      geda_list_prepend (history->pages_back, page);
    }
  }
}

static void
gschem_page_history_push_forward (GschemPageHistory *history, Page *page)
{
  if (GEDA_IS_PAGE(page)) {
    geda_list_prepend (history->pages_forw, page);
  }
}

/*!
 * \brief Release resources allocated by GschemPageHistory
 * \par Function Description
 *  Unreferences the two GedaList used to track history and
 *  then frees the structure containing the two pointers.
 *
 * \param [in] history GschemPageHistory object to be destroyed.
 */
void
gschem_page_history_free (GschemPageHistory *history)
{
  if (history) {
    if (GEDA_IS_LIST(history->pages_forw)) {
      g_object_unref(history->pages_forw);
    }
    history->pages_forw = NULL;

    if (GEDA_IS_LIST(history->pages_back)) {
      g_object_unref(history->pages_back);
    }
    history->pages_back = NULL;
    GEDA_FREE(history);
  }
}

/*!
 * \brief Get Page Back in History
 * \par Function Description
 *  Returns the page on top the pages-back history stack
 *  after removing the current page, which is pushed to
 *  the list of pages forward.
 *
 * \param [in] history GschemPageHistory object.
 *
 * \returns pointer to the previous page in history.
 *
 * \sa gschem_page_history_get_forward.
 */
Page*
gschem_page_history_get_back (GschemPageHistory *history)
{
  GList *back = geda_list_get_glist(history->pages_back);

  if (back && back->next) { /* Only pop if history beyond */

    Page *page;

    page = gschem_page_history_pop_back (history);

    gschem_page_history_push_forward (history, page);

    back = geda_list_get_glist(history->pages_back);

    page = back->data;

    return page;
  }

  return NULL;
}

/*!
 * \brief Get Page forward in History
 * \par Function Description
 *  Returns the last page added to the page forward stack.
 *  Pages are automactically added to the forward stack when
 *  gschem_page_history_get_back is called.
 *
 * \param [in] history GschemPageHistory object.
 *
 * \returns pointer to the next page in history after going back.
 *
 * \sa gschem_page_history_get_back.
 */
Page*
gschem_page_history_get_forward (GschemPageHistory *history)
{
  GList *forw = geda_list_get_glist(history->pages_forw);

  if (forw && forw->data) {

    Page *page;

    page = gschem_page_history_pop_forward (history);

    return GEDA_IS_PAGE(page) ? page : NULL;
  }

  return NULL;
}

/*!
 * \brief Create a new GschemPageHistory
 * \par Function Description
 *  Allocates resources for a new GschemPageHistory, if successful
 *  returns the object.
 *
 * \returns a GschemPageHistory object.
 */
GschemPageHistory*
gschem_page_history_new ()
{
  GschemPageHistory *history = GEDA_MEM_ALLOC0(sizeof(GschemPageHistory));

  if (history) {
    history->pages_forw = geda_list_new();
    history->pages_back = geda_list_new();
    return history;
  }
  return NULL;
}

/*!
 * \brief Remove a Page from History
 * \par Function Description
 *  This function is called when the page is closed to removes all
 *  references to \a page from the history.
 *
 * \param [in] history GschemPageHistory object,
 * \param [in] page    Page to be removed from history.
 */
void
gschem_page_history_remove_page (GschemPageHistory *history, Page *page)
{
  if (page) {
    while (geda_list_is_in_list (history->pages_back, page)) {
      geda_list_remove(history->pages_back, page);
    }
    while (geda_list_is_in_list (history->pages_forw, page)) {
      geda_list_remove(history->pages_forw, page);
    }
  }
}

/*!
 * \brief Add a list of pages to the Page History
 * \par Function Description
 *  This function allows a list of pages to be added to the history
 *  list of pages and is used when gschem is started with a list of
 *  documents to open so gschem_page_history_push_back does not need
 *  to be called for each document.
 *
 * \param [in] history GschemPageHistory object,
 * \param [in] pages   List of pages to be add to history.
 */
void
gschem_page_history_seed_back (GschemPageHistory *history, GList *pages)
{
  GList *iter;

  geda_list_remove_all(history->pages_back);

  for (iter = pages; iter; iter = iter->next) {
    gschem_page_history_push_back(history, iter->data);
  }
}
