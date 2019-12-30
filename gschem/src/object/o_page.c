/* -*- C indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: o_page.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: March 25, 2015
 */
/*!
 * \file o_page.c
 * \brief Low-level module for manipulating the drawing order
 */
/** \defgroup Draw-Order-Operations Draw Order Operations
 *  @{
 *  \ingroup Editing-Operations
 *  \par This group contains routines for Draw-Order operations
 *   the draw order is only the apparent change, this module only
 *   manipulated that object appear the the page list, and in
 *   effect the file.
 */

#include <gschem.h>

/** \defgroup Draw-Order-Internal Draw-Order Module Helper Functions
 *  @{
 */

/*!
 * \brief Interogate List for Change
 * \ingroup Draw-Order-Internal
 * \par Function Description
 *  Compares each data item in list arguments and returns True is there
 *  is any difference.
 *
 * \param [in] list1 Pointer to start of the first to compare,
 * \param [in] list2 Pointer to start of the second to compare,
 *
 * \returns True is the two lists differ, otherwise False.
 */
static bool list_was_modified (GList *list1, GList *list2)
{
  int modified = FALSE;
  GList *iter1 = list1;
  GList *iter2 = list2;

  while (iter1 && iter2) {

    if (iter1->data != iter2->data) {
      modified = TRUE;
      break;
    }
    NEXT(iter1);
    NEXT(iter2);
  }

  return modified;
}

/*!
 * \brief Temporarily remove all title-blocks and title-block attributes
 * \ingroup Draw-Order-Internal
 * \par Function Description
 *  This function attempts to locate title blocks by searching for "title"
 *  in the name of Complex objects, if found the object and all associated
 *  children are removed from \a page_list and returned as a separate list.
 *
 * \param [in,out] page_list List to search for title block objects,
 *
 * \returns List containing object of interest or NULL if none were found.
 */
static GList *remove_title_blocks(GList **page_list)
{
  GList *list   = *page_list;
  GList *iter   =  list;
  GList *titles =  NULL;

  while (iter) {

    GedaObject *object;
    const char *filename;

    object = iter->data;
    filename = geda_complex_object_get_filename(object);

    if (filename) {

      if (geda_utility_string_stristr(filename, "title") >= 0) {

        const GList *children;

        list     = g_list_remove (list, object);
        titles   = g_list_prepend (titles, object);
        children = geda_object_get_attached(object);

        if (children) {

          const GList *iter2 = children;

          while (iter2) {
            object = iter2->data;
            list   = g_list_remove (list, object);
            titles = g_list_prepend (titles, object);
            iter2  = iter2->next;
          }
        }
        iter = list; /* start over from beginning */
        continue;
      }
    }
    iter = iter->next;
  }
 *page_list = list;
  return titles;
}

/*!
 * \brief Add title-blocks and attributes back to page list
 * \ingroup Draw-Order-Internal
 * \par Function Description
 *  This function undoes what remove_title_blocks did, prepending the
 *  removed objects for title-blocks.
 *
 * \param [in] page_list   List to receive the title block objects,
 * \param [in] titleblocks List returned from remove_title_blocks
 *
 * \returns Composited List.
 */
static GList *restore_title_blocks(GList *page_list, GList *titleblocks)
{
  GList  *iter = titleblocks;

  while (iter) {
    GedaObject *object = iter->data;
    page_list = g_list_prepend(page_list, object);
    iter = iter->next;
  }
  return page_list;
}

/**  @} endgroup Draw-Order-Internal */

/** \defgroup Draw-Order-Public Draw-Order Public Functions
*  @{
*/

/*!
 * \brief Draw Objects after other Objects
 * \ingroup Draw-Order-Public
 * \par Function Description
 *  This function operates on two selections, to first retrieved from
 *  primary_selection, the second is the current selection. Every member
 *  of the former is  removed from the page list and added back after the
 *  last member of the second list.
 *
 * \param [in] w_current   Pointer to GschemToplevel structure.
 * \todo unstable
 */
void o_page_draw_after (GschemToplevel *w_current)
{
  if (o_select_is_selection(w_current)) {

    Page   *page      = gschem_toplevel_get_current_page(w_current);

    GList  *page_list = geda_struct_page_get_objects (page);
    GList  *list      = geda_list_get_glist (geda_struct_page_get_selection (page));
    GList  *set2      = g_list_copy(list);
    GList  *set1      = g_list_copy(w_current->primary_selection);
    GList  *iter;

    o_select_unselect_all(w_current);

    /* Remove duplicate selection in after and remove from page */
    for (iter = set1; iter; iter = iter->next) {
      GedaObject *object;
      object = iter->data;
      set2   = g_list_remove(set2, object);
    }

    /* Is there still a set 2 or was second selection the
     * same as the first selection? */
    if (g_list_length(set2)) {

      GList  *old_list  = g_list_copy(page_list);
      GList  *last      = NULL;
      GList  *tail      = NULL;

      for (iter = set1; iter; iter = iter->next) {
        GedaObject *object;
        object    = iter->data;
        page_list = g_list_remove(page_list, object);
      }

      last = g_list_last (page_list);

      /* Traverse page backwards looking for occurence of member set2 */
      for (iter = last; iter; iter = iter->prev) {
        GedaObject *object = iter->data;
        if (g_list_find(set2, object)) {
          tail = iter->next;
          break;
        }
      }

      if (tail){
        for (iter = set1; iter; iter = iter->next) {
          GedaObject *object = iter->data;
          page_list = g_list_insert_before (page_list, tail, object);
        }
      }
      else {
        page_list = g_list_concat(page_list, set1);
      }

      page->_object_list = page_list;

      if (list_was_modified(old_list, page_list)) {
        geda_page_set_changed (page, TRUE);
        o_invalidate_all (w_current);
        o_undo_savestate (w_current, UNDO_ALL);
      }

      g_list_free(old_list);

    }

    g_list_free(set1);
    g_list_free(set2);

    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
  }
  gschem_toplevel_free_primary(w_current);
}

/*!
 * \brief Draw Objects Before other Objects
 * \ingroup Draw-Order-Public
 * \par Function Description
 *  This function operates on two selections, to first retrieved from
 *  primary_selection, the second is the current selection. Every member
 *  of the former is removed from the page list and added back before
 *  the first member of the second list.
 *
 * \param [in] w_current   Pointer to GschemToplevel structure.
 */
void o_page_draw_before (GschemToplevel *w_current)
{
  if (o_select_is_selection(w_current)) {

    Page   *page      = Current_Page;

    GList  *page_list = geda_struct_page_get_objects (page);
    GList  *set1      = g_list_copy(w_current->primary_selection);
    GList  *set2      = g_list_copy(geda_list_get_glist (Current_Selection));
    GList  *iter;

    o_select_unselect_all(w_current);

    /* Look for duplicate selection in and remove from set2 */
    for (iter = set1; iter; iter = iter->next) {
      GedaObject *object = iter->data;
      set2           = g_list_remove(set2, object);
    }

    /* Is there still a set 2 or was second selection the
     * same as the first selection? */
    if (g_list_length(set2)) {

      GList  *old_list  = g_list_copy(page_list);
      GList  *leader    = NULL;

      for (iter = set1; iter; iter = iter->next) {
        GedaObject *object = iter->data;
        page_list      = g_list_remove(page_list, object);
      }

      /* Traverse page looking for first occurence of member set2 */
      for (iter = page_list; iter; iter = iter->next) {
        GedaObject *object = iter->data;
        if (g_list_find(set2, object)) {
          leader = iter;
          break;
        }
      }

      for (iter = set1; iter; iter = iter->next) {
        GedaObject *object = iter->data;
        page_list = g_list_insert_before (page_list, leader, object);
      }

      page->_object_list = page_list;

      if (list_was_modified(old_list, page_list)) {
        geda_page_set_changed (page, TRUE);
        o_invalidate_all(w_current);
        o_undo_savestate(w_current, UNDO_ALL);
      }

      g_list_free(old_list);

    }

    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);

    g_list_free(set1);
    g_list_free(set2);
  }

  gschem_toplevel_free_primary(w_current);
}

/*!
 * \brief Draw Objects First
 * \ingroup Draw-Order-Public
 * \par Function Description
 *  This function operates on the object list. Every member is removed
 *  from the page list and added back to the front of the list after
 *  removing any titleblock objects. Title block objects are prepended
 *  back to the beginning, regardless of where they were found in the
 *  page list.
 *
 * \param [in] w_current   Pointer to GschemToplevel structure.
 * \param [in] object_list List of objects to be drawn first
 */
void o_page_draw_first (GschemToplevel *w_current, GList *object_list)
{
  Page  *page      = Current_Page;
  GList *page_list = geda_struct_page_get_objects (page);
  GList *tblocks   = remove_title_blocks(&page_list);
  GList *list      = g_list_copy(object_list);
  GList *iter      = g_list_reverse(list);

  o_select_unselect_all(w_current);

  while (iter) {

    GedaObject *object;

    object    = iter->data;
    page_list = g_list_remove (page_list, object);
    page_list = g_list_prepend (page_list, object);
    iter      = iter->next;
  }

  page_list = restore_title_blocks(page_list, tblocks);

  page->_object_list = page_list;

  geda_page_set_changed (page, TRUE);

  o_invalidate_all (w_current);
  o_undo_savestate (w_current, UNDO_ALL);

  g_list_free(list);
  g_list_free(tblocks);
}

/*!
 * \brief Draw Objects First
 * \ingroup Draw-Order-Public
 * \par Function Description
 *  This function operates on the current selection, moving every member
 *  to the end of the end of the page list.
 *
 * \param [in] w_current   Pointer to GschemToplevel structure.
 * \param [in] object_list List of objects to be drawn last
 *
 * \todo Check relocated objects for children.
 */
void o_page_draw_last (GschemToplevel *w_current, GList *object_list)
{
  Page  *page      = Current_Page;
  GList *page_list = geda_struct_page_get_objects (page);
  GList *iter      = object_list;

  while (iter) {

    GedaObject *object;

    object    = iter->data;
    page_list = g_list_remove (page_list, object);
    page_list = g_list_append (page_list, object);
    iter      = iter->next;
  }

  page->_object_list = page_list;

  geda_page_set_changed (page, TRUE);
  o_invalidate_list (w_current, object_list);
  o_undo_savestate (w_current, UNDO_ALL);
}

/** @} endgroup Draw-Order-Public */
/** @} endgroup Draw-Order-Operations */
