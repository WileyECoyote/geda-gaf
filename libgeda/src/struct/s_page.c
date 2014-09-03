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

/*! \file s_page.c
 *  \brief The page system
 *
 *  libgeda can handle multiple schematic or symbol pages. libgeda keeps
 *  track of the currently opened pages with a managed _GedaList.
 *  The currently used page is refered with an extra pointer.
 *
 *  Each page carries a list of the objects that are on the page.
 *  The first and the last element are referenced by the head and tail
 *  pointers.
 *
 *  \image html s_page_overview.png
 *  \image latex s_page_overview.pdf "page overview" width=14cm
 */

#include <config.h>

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"


/* Called just before adding an Object from a Page.
 * or after appending an OBJECT to a PAGE. */
static void object_added (Page *page, Object *object)
{
  /* Set up object parent pointer */
#ifndef NDEBUG
  if (object->page != NULL) {
    g_critical ("Object %p already has parent page %p!", object, object->page);
  }
#endif
  object->page = page;

  o_notify_emit_pre_change (object);

  /* Add object to tile system. */
  s_tile_add_object        (object);

  /* Update object connection tracking */
  s_conn_update_object     (object);

  page->CHANGED = 1;

  o_notify_emit_change     (object);

}

/* Called just before removing an Object from a Page. */
static void
pre_object_removed (Page *page, Object *object)
{
  if (GEDA_IS_OBJECT(object)) {

    o_notify_emit_pre_change (object);

    /* Clear object parent pointer */
    #ifdef DEBUG
    if (object->page == NULL) {
      g_critical ("Object %p has NULL parent page!\n", object);
    }
    #endif
    object->page = NULL;

    /* Clear page's object_lastplace pointer if set */
    if (page->object_lastplace == object) {
      page->object_lastplace = NULL;
    }

    /* Remove object from connection system */
    s_conn_remove_object (object);

    /* Remove object from tile system */
    s_tile_remove_object (object);
  }
  else
    BUG_MSG("Object is not a GedaObject");
}

static Page *
s_page_new_common(Page *page)
{
  page->up = -2;

  /* Init tile array */
  s_tile_init (page);

  /* new selection mechanism */
  page->selection_list = o_selection_new();

  /* init undo struct pointers */
  s_undo_init(page);

  /* Backup variables */
  g_get_current_time (&page->last_load_or_save_time);
  page->ops_since_last_backup    = 0;
  page->saved_since_first_loaded = 0;
  page->do_autosave_backup       = 0;

  return page;
}

/*! \brief create a new page object
 *  \par Function Description
 *  Creates a new page and add it to <B>toplevel</B>'s list of pages.
 *
 *  It initializes the#Page structure and set its <B>filename</B>
 *  to <B>filename</B>. <B>toplevel</B>'s current page is not changed by
 *  this function.
 */
Page *s_page_new (GedaToplevel *toplevel, const char *filename)
{
  Page *page;

  /* Create a blank Page object */
  page = geda_page_new();

  if (filename != NULL) {
    if (g_path_is_absolute (filename)) {
      page->filename = u_string_strdup (filename);
    }
    else {
      char *pwd = getcwd(0,0);
      page->filename = g_build_filename (pwd, filename, NULL);
      free (pwd);
    }
  }
  else {
    page->filename = u_string_strdup (toplevel->untitled_name);
  }

  page->width  = toplevel->width;
  page->height = toplevel->height;

  page->left   = 0;
  page->right  = toplevel->width;
  page->top    = 0;
  page->bottom = toplevel->height;

  /* append page to page list of toplevel */
  geda_list_add( toplevel->pages, page );
  geda_page_set_toplevel(page, toplevel);

  return s_page_new_common(page);
}

Page*
s_page_new_with_notify (GedaToplevel *toplevel, const char *filename)
{
  Page *page;

  /* Create a blank Page object */
  page = geda_page_new_with_notify();

  if (filename != NULL) {
    if (g_path_is_absolute (filename)) {
      page->filename = u_string_strdup (filename);
    }
    else {
      char *pwd = getcwd(0,0);
      page->filename = g_build_filename (pwd, filename, NULL);
      free (pwd);
    }
  }
  else {
    page->filename = u_string_strdup (toplevel->untitled_name);
  }

  page->width  = toplevel->width;
  page->height = toplevel->height;

  page->left   = 0;
  page->right  = toplevel->width;
  page->top    = 0;
  page->bottom = toplevel->height;

  /* append page to page list of toplevel */
  geda_list_add( toplevel->pages, page );
  geda_page_set_toplevel(page, toplevel);

  return s_page_new_common(page);
}

/*! \brief Get File Extension of the File Assocatiacted with Page.
 *
 * \par Function Description
 * Returns a pointer to extension of the filename associated with Page.
 * The string is owned by the Page and must not be changed!
 *
 * \param [in] page Pointer to a Page data structure.
 *
 * \return point to the page file name extension or NULL if there
 *         either no file name or no DOT suffix used in the name.
 *
 */
const char *s_page_get_file_extension (Page *page)
{
  if (page != NULL && page->filename != NULL) {
    return f_get_filename_ext(page->filename);
  }
  return NULL;
}

/*! \brief Get is Page a Symbol file.
 *
 * \par Function Description
 * Returns true if the filename assocaited with page ends in ".sym"!
 *
 * \param [in] page Pointer to a Page data structure.
 *
 * \return bool TRUE if Page is data from a Symbol file
 *
 */
bool s_page_is_symbol_file (Page *page) {
  const char *ext;
  bool isSymbol = FALSE;
  if (page != NULL) {
    if ((ext = s_page_get_file_extension(page)) != NULL) {
      if (strcmp (ext, SYMBOL_FILE_SUFFIX) == 0) {
        isSymbol = TRUE;
      }
    }
  }
  return isSymbol;
}
/*! \brief delete a page and it's contents
 *
 *  \par Function Description
 *  Deletes a single page <B>page</B> from the <B>toplevel</B> list of
 *  pages. If the page being deleted is the current <B>page</B>, this
 *  function sets the field <B>page_current</B> of the GedaToplevel
 *  struct to NULL. (WEH: which is probably not a great idea, lastpage)
 *
 * \sa #s_page_delete_list() to delete all pages of a <B>toplevel</B>
 *
 * \param [in] toplevel   Toplevel Object to which page belongs.
 * \param [in] page       Page to be removed.
 *
 */
void s_page_delete (GedaToplevel *toplevel, Page *page)
{
  Page *tmp;
  char *backup_filename;
  char *real_filename;

#if DEBUG
  printf("s_page_delete: Freeing page: %s\n", page->filename);
#endif

  /* We need to temporarily make the page being deleted current because
   * various functions called below (some indirectly) assume they are
   * deleting objects from the current page.
   *
   * These functions are known to include:
   *   s_object_release ()
   */

  /* save page_current and switch to page */
  if (page == toplevel->page_current) {
    tmp = NULL;
  }
  else {
    tmp = toplevel->page_current;
    s_page_goto (toplevel, page);
  }

  /* Get the real filename and file permissions */
  real_filename = f_file_follow_symlinks (page->filename, NULL);

  if (real_filename == NULL) {
    u_log_message (_("s_page_delete: Can't get the real filename of %s."),
                   page->filename);
  }
  else {
    backup_filename = f_get_autosave_filename (real_filename);

    /* Delete the backup file */
    if ( (g_file_test (backup_filename, G_FILE_TEST_EXISTS)) &&
      (!g_file_test(backup_filename, G_FILE_TEST_IS_DIR)) )
    {
      if (unlink(backup_filename) != 0) {
        u_log_message(_("s_page_delete: Unable to delete backup file %s."),
                      backup_filename);
      }
    }

    GEDA_FREE (backup_filename);
  }

  GEDA_FREE(real_filename);

  /* Free the selection object */
  GEDA_UNREF( page->selection_list );

  /* then delete objects of page */
  s_page_delete_objects (page);

  /* Free the objects in the place list. */
  s_object_release_objects (page->place_list);
  page->place_list = NULL;

  s_tile_free_all (page);

  /* free current page undo structs */
  s_undo_free_all (page);

  /* ouch, deal with parents going away and the children still around */
  page->up = -2;
  GEDA_FREE (page->filename);
  page->filename = NULL;

  geda_list_remove( toplevel->pages, page );

#if DEBUG
  s_tile_print (toplevel, page);
#endif

  //geda_page_weakref_notify (page);

  geda_page_unref( page );

  /* restore page_current */
  if (tmp != NULL) {
    s_page_goto (toplevel, tmp);
  }
  else {
    /* page was page_current */
    toplevel->page_current = NULL;
    /* page_current must be updated by calling function */
  }
}

/*! \brief Deletes the list of pages of <B>toplevel</B>.
 *  \par Function Description
 *  Deletes the list of pages of <B>toplevel</B>.
 *  This function should only be called when you are finishing up.
 *
 *  \param toplevel  The GedaToplevel object.
 */
void s_page_delete_list(GedaToplevel *toplevel)
{
  GList *list_copy, *iter;
  Page *page;

  /* s_page_delete removes items from the page list, so make a copy */
  list_copy = g_list_copy (geda_list_get_glist (toplevel->pages));

  for (iter = list_copy; iter != NULL; NEXT(iter)) {
    page = (Page *)iter->data;
    o_notify_change_remove_all(page);
    s_page_delete (toplevel, page);
  }

  g_list_free (list_copy);

  /* reset toplevel fields */
  toplevel->page_current = NULL;
}

/*! \brief Get the current page
 *  \par Function Description
 *  This function returns the current Page object.
 *
 * \sa s_page_set_current, s_page_goto
 *
 * \param [in,out] toplevel This toplevel
 */
Page * s_page_get_current (GedaToplevel *toplevel )
{
  g_return_val_if_fail (toplevel != NULL, FALSE);

  return toplevel->page_current;

}

/*! \brief Set the current page
 *  \par Function Description
 *  Changes the current page in \a toplevel to the page \a page.
 *
 * \sa s_page_get_current, s_page_goto
 *
 * \param [in,out] toplevel This toplevel
 * \param [in] page The new current page
 */
bool s_page_set_current (GedaToplevel *toplevel, Page *page)
{
  g_return_val_if_fail (toplevel != NULL, FALSE);

  toplevel->page_current = page;

  return TRUE;
}

/*! \brief changes the current page in toplevel
 *  \par Function Description
 *  Calls s_page_set_current to change the current page
 * referenced by \a toplevel and changes the current
 * working directory to the directory associated with the
 * page.
 *
 *  \param toplevel  The GedaToplevel object
 *  \param page      The Page to go to
 *
 *  \returns True on success, otherwise FALSE
 */
bool s_page_goto (GedaToplevel *toplevel, Page *page)
{
  char *dirname;
  bool  result;

  result = FALSE;
  if (s_page_set_current (toplevel, page)) {
    dirname = g_path_get_dirname(page->filename);
    if (!chdir (dirname)) {
      result = TRUE;
    }
    GEDA_FREE (dirname);
  }
  return result;
}

/*! \brief Search for pages by filename.
 *  \par Function Description
 *  Searches in \a toplevel's list of pages for a page with a filename
 *  equal to \a filename.
 *
 *  \param toplevel  The GedaToplevel object
 *  \param filename  The filename string to search for
 *
 *  \return Page pointer to a matching page, NULL otherwise.
 */
Page *s_page_search (GedaToplevel *toplevel, const char *filename)
{
  const GList *iter;
  Page *page;

  for ( iter = geda_list_get_glist(toplevel->pages); iter != NULL; NEXT(iter))
  {
    page = (Page *)iter->data;
    if ( g_ascii_strcasecmp( page->filename, filename ) == 0 )
      return page;
  }
  return NULL;
}

/*! \brief Search for a page given its page id in a page list.
 *  \par Function Description
 *  This functions returns the page that have the page id \a pid in
 *  the list of pages starting at \a page_list, or NULL if there is no
 *  such page.
 *
 *  \param [in] list      The list of page to search the page in.
 *  \param [in] pid       The ID of the page to find.
 *  \returns A pointer on the page found or NULL if not found.
 */
Page *s_page_search_by_page_id (PageList *list, int pid)
{
  const GList *iter;

  for ( iter = geda_list_get_glist (list); iter != NULL; NEXT(iter))
  {
    Page *page = (Page *)iter->data;
    if (page->pid == pid) {
      return page;
    }
  }

  return NULL;
}

/*! \brief Print full GedaToplevel structure.
 *  \par Function Description
 *  This function prints the internal structure of <B>toplevel</B>'s
 *  list of pages.
 *
 *  \param [in] toplevel  The GedaToplevel object to print.
 */
void s_page_print_all (GedaToplevel *toplevel)
{
  const GList *iter;
  Page *page;

  for ( iter = geda_list_get_glist( toplevel->pages ); iter != NULL; NEXT(iter))
  {
    page = (Page *)iter->data;
    printf ("FILENAME: %s\n", page->filename);
    print_struct_forw (page->_object_list);
  }
}

/*! \brief Saves all the pages of a GedaToplevel object.
 *  \par Function Description
 *  Saves all the pages in the <B>toplevel</B> parameter.
 *
 *  \param [in] toplevel  The GedaToplevel to save pages from.
 *  \return The number of failed tries to save a page.
 */
int s_page_save_all (GedaToplevel *toplevel)
{
  const GList *iter;
  Page *p_current;
  int status = 0;

  for ( iter = geda_list_get_glist( toplevel->pages ); iter != NULL; NEXT(iter))
  {
    p_current = (Page *)iter->data;

    if (f_save (toplevel, p_current, p_current->filename, NULL)) {
       u_log_message (_("Saved [%s]\n"), p_current->filename);
      /* reset the CHANGED flag of p_current */
      p_current->CHANGED = 0;

    } else {
      u_log_message (_("Could NOT save [%s]\n"), p_current->filename);
      /* increase the error counter */
      status++;
    }

  }

  return status;
}

/*! \brief Check if CHANGED flag is set for any page in list.
 *  \par Function Description
 *  This function checks the CHANGED flag for all pages in the <B>list</B>
 *  object.
 *
 *  \param [in] list  PageList to check CHANGED flag in.
 *  \return 1 if any page has the CHANGED flag set, 0 otherwise.
 */
bool s_page_check_changed (PageList *list)
{
  const GList *iter;
  Page *p_current;

  for ( iter = geda_list_get_glist( list ); iter != NULL; NEXT(iter))
  {
    p_current = (Page *)iter->data;
    if (p_current->CHANGED) {
      return TRUE;
    }
  }

  return FALSE;
}

/*! \brief Reset the CHANGED flag of all pages.
 *  \par Function Description
 *  This function resets the CHANGED flag of each page following \a head.
 *
 *  \param [in,out] list  Page list to set CHANGED flags in.
 */
void s_page_clear_changed (PageList *list)
{
  const GList *iter;
  Page *p_current;

  for ( iter = geda_list_get_glist( list ); iter != NULL; NEXT(iter))
  {
    p_current = (Page *)iter->data;
    p_current->CHANGED = 0;
  }
}

/*! \brief Autosave initialization function.
 *  \par Function Description
 *  This function sets up the autosave callback function.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 */
void s_page_autosave_init(GedaToplevel *toplevel)
{
  if (toplevel->auto_save_interval != 0) {

    /* 1000 converts seconds into milliseconds */
    toplevel->auto_save_timeout =
      g_timeout_add(toplevel->auto_save_interval*1000,
                    (GSourceFunc) s_page_autosave,
                    toplevel);
  }
}

/*! \brief Autosave callback function.
 *  \par Function Description
 *  This function is a g_timeout callback functions that is called
 *  every "interval" milliseconds and check and sets a flag to save
 *  a backup copy of the opened pages.
 *
 *  Applications can check the flag to determine if a backup should be
 *  created, the check could be done in an Undo function so that files
 *  that change get backed-up and opened files with no user activity
 *  are not.
 *
 *  The reason for using the auto_save_interval as the return value is
 *  so that if the applications wants to kill the source, maybe because
 *  the user changed the settings, then the accessible variable can be
 *  set to zero, and when the function returns the zero to glib, the
 *  source will be destroyed.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *
 *  \return The auto_save_interval setting.
 */
int s_page_autosave (GedaToplevel *toplevel)
{
  const GList *iter;
        Page  *p_current;

  if (toplevel == NULL) {
    u_log_message (_("Disabling auto save timer, no toplevel"));
    return 0;
  }

  /* Do nothing if the interval is 0, we will not be called again */
  if (toplevel->auto_save_interval != 0) {

    if ( toplevel->pages != NULL) {

      for ( iter = geda_list_get_glist(toplevel->pages); iter != NULL; NEXT(iter))
      {
        p_current = (Page *)iter->data;

        if (p_current->CHANGED) {
          if (p_current->ops_since_last_backup != 0) {
            /* Real autosave is done in o_undo_savestate */
            p_current->do_autosave_backup = 1;
          }
        }
      }
    }
  }
  return toplevel->auto_save_interval;
}

/*! \brief Append an Object to the Page
 *
 *  \par Function Description
 *  Links the passed Object to the end of the Page's
 *  linked list of objects and then calls object_added.
 *
 *  \param [in] page      The Page the object is being added to.
 *  \param [in] object    The Object being added to the page.
 */
void s_page_append_object (Page *page, Object *object)
{
  if(GEDA_IS_OBJECT(object)){
    page->_object_list = g_list_append (page->_object_list, object);
    object_added (page, object);
  }
  else {
    BUG_MSG("Objects is not a GedaObject");
  }
}

/*! \brief Append a GList of Objects to the Page
 *
 *  \par Function Description
 *  Links the passed Object GList to the end of the Page's
 *  object_list.
 *
 *  \param [in] page      The Page the objects are being added to.
 *  \param [in] obj_list  The Object list being added to the page.
 */
void s_page_append_list (Page *page, GList *obj_list)
{
  GList  *iter;
  Object *object;

  page->_object_list = g_list_concat (page->_object_list, obj_list);
  for (iter = obj_list; iter != NULL; iter = iter->next) {
    object = iter->data;
    object_added (page, object);
  }
}

/*! \brief Remove an Object from the Page
 *  \par Function Description
 *  Removes the passed Object from the Page's
 *  linked list of objects.
 *
 *  \param [in] page      The Page the object is being removed from.
 *  \param [in] object    The Object being removed from the page.
 */
void s_page_remove_object (Page *page, Object *object)
{
  pre_object_removed (page, object);
  page->_object_list = g_list_remove (page->_object_list, object);
  o_notify_emit_change (object);
  page->CHANGED = 1;
}

/*! \brief Replace an Object in a Page, in the same list position.
 *
 * \par Function Description
 * Removes \a object1 from \a page's linked list of objects, and puts
 * \a object2 in the position thus vacated. If \a object1 is not in \a
 * page, object2 is appended to \a page.
 *
 * \param [in] page      The Page to be modified.
 * \param [in] object1   The Object being removed from the page.
 * \param [in] object2   The Object being added to the page.
 */
void
s_page_replace_object (Page *page, Object *object1, Object *object2)
{
  GList *iter = g_list_find (page->_object_list, object1);

  /* If object1 not found, append object2 */
  if (iter == NULL) {
    s_page_append_object (page, object2);
    return;
  }

  pre_object_removed (page, object1);
  iter->data = object2;
  object_added (page, object2);
}

/*! \brief Remove and free all Objects from the Page
 *
 *  \par Function Description
 *  Removes and releases all Objects from the Page.
 *
 *  \param [in] page      The Page being cleared.
 */
void s_page_delete_objects (Page *page)
{
  GList *objects = page->_object_list;
  GList *iter;

  for (iter = objects; iter != NULL; NEXT(iter)) {
    if (GEDA_IS_OBJECT(iter->data)) {
       pre_object_removed (page, iter->data);
    }
  }
  page->_object_list = NULL;

  s_object_release_objects (objects);
}

/*! \brief Return an Object on the Page by ID
 *
 *  \par Function Description
 *  An accessor for the Page's GList of objects.
 *
 *  \note The Object is owned by the Page, and must not be
 *        free'd or modified by the caller.
 *
 *  \sa s_page_get_objects
 *
 *  \param [in] page      The Page to get objects on.
 *  \param [in] sid       The Page Id.
 *
 *  \returns a pointer to the Page's GList of objects
 */
Object *s_page_get_object (Page *page, int sid)
{
  return geda_page_get_object(page, sid);
}

/*! \brief Return a GList of Objects on the Page
 *
 *  \par Function Description
 *  An accessor for the Page's GList of objects.
 *
 *  \note This GList is owned by the Page, and must not be
 *        free'd or modified by the caller.
 *
 *  \sa s_page_get_object
 *
 *  \param [in] page      The Page to get objects on.
 *  \returns a pointer to the Page's GList of objects
 */
GList *s_page_get_objects (Page *page)
{
  return page->_object_list;
}

/*! \brief Find the objects in a given region
 *
 *  \par Function Description
 *  Finds the objects which are inside, or intersect
 *  the passed box shaped region.
 *
 *  \param [in] page      The Page to find objects on
 *  \param [in] rects     The RECTANGLE regions to check
 *  \param [in] n_rects   The number of regions
 *  \return The GList of Objects in the region
 */
GList*
s_page_objects_in_regions (Page *page, RECTANGLE *rects, int n_rects)
{
  GList *iter;
  GList *list = NULL;
  int i;

  if (GEDA_IS_PAGE(page)) {

    for (iter = page->_object_list; iter != NULL; NEXT(iter)) {
      Object *object = iter->data;
      int left, top, right, bottom;
      if (world_get_single_object_bounds (object, &left, &top, &right, &bottom))
      {
        for (i = 0; i < n_rects; i++) {
          if (right  >= rects[i].lower_x &&
            left   <= rects[i].upper_x &&
            top    <= rects[i].upper_y &&
            bottom >= rects[i].lower_y) {
            list = g_list_prepend (list, object);
          break;
            }
        }
      }
    }

    list = g_list_reverse (list);
  }
  else {
    BUG_MSG("Invalid Page object");
  }
  return list;
}

/*! \brief Find the objects in a given region
 *
 *  \par Function Description
 *  Finds the objects which are inside, or intersect
 *  the passed box shaped region.
 *
 *  \param [in] page      The Page to find objects on.
 *  \param [in] min_x     The smaller X coordinate of the region.
 *  \param [in] min_y     The smaller Y coordinate of the region.
 *  \param [in] max_x     The larger  X coordinate of the region.
 *  \param [in] max_y     The larger  Y coordinate of the region.
 *
 *  \return The GList of Objects in the region.
 */
GList*
s_page_objects_in_region (Page *page, int min_x, int min_y, int max_x, int max_y)
{
  RECTANGLE rect;

  rect.lower_x = min_x;
  rect.lower_y = min_y;
  rect.upper_x = max_x;
  rect.upper_y = max_y;

  return s_page_objects_in_regions (page, &rect, 1);
}

/*! \brief Set the font-renderer-specific bounds function.
 *  \par Function Description
 *  Set the function to be used to calculate text bounds for #Text
 *  Object associated with the page. This allow a per page object renderer
 *  function to be defined. If the function is not defined the renderer for
 *  the Toplevel will be used instead, if a Toplevel renderer is defined.
 *
 *  \param [in] page      The Page to associate this function with to render text.
 *  \param [in] func      Function to use.
 *  \param [in] user_data User data to be passed to the function.
 */
void
s_page_set_bounds_func(Page *page, RenderedBoundsFunc func, void *user_data)
{
  g_return_if_fail(GEDA_IS_PAGE(page));

  page->rendered_text_bounds_func = func;
  page->rendered_text_bounds_data = user_data;
}
