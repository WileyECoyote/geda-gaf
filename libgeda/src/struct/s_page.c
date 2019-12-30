/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
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

/*! \file s_page.c
 *  \brief The page system
 *  In Libgeda, a page is synonymous with an open document object.
 *  Multiple schematic or symbol pages can be open at the same time.
 *  The toplevel contains a managed GedaList of all currently opened
 *  pages, aka documents. The toplevel also contains a pointer that
 *  can be used to referred to an "active" page. In Libgeda this is
 *  is called the current page. Each page object contains a list of
 *  the objects that are on the page and other information relevant
 *  to the specific page.
 *
 *  \image html s_page_overview.png
 *  \image latex s_page_overview.pdf "page overview" width=14cm
 */

#include <config.h>

#include <sys/types.h>

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda_priv.h>

/* Called just before adding an GedaObject from a Page.
 * or after appending an OBJECT to a PAGE. */
static void object_added (Page *page, GedaObject *object)
{
  /* Set up object parent pointer */
#ifdef DEBUG_PAGE
  if (object->page != NULL) {
    g_critical ("GedaObject %p already has parent page %p!", object, object->page);
  }
#endif

  geda_object_set_page(object, page);

  geda_object_bounds(object);

  geda_object_notify_emit_pre_change (object);

  /* Add object to tile system. */
  geda_struct_tile_add_object        (object);

  /* Update object connection tracking */
  geda_struct_conn_update_object     (object);

  page->CHANGED = 1;

  geda_object_notify_emit_change     (object);

}

/* Called just before removing an Object from a Page. */
static void
pre_object_removed (Page *page, GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {

    geda_object_notify_emit_pre_change (object);

    /* Clear object parent pointer */
#ifdef DEBUG
    if (object->page == NULL) {
      g_critical ("GedaObject %p has NULL parent page!\n", object);
    }
#endif

    object->page = NULL;

    /* Clear page's object_lastplace pointer if set */
    if (page->object_lastplace == object) {
      page->object_lastplace = NULL;
    }

    /* Remove object from connection system */
    geda_struct_conn_remove_object (object);

    /* Remove object from tile system */
    geda_struct_tile_remove_object (object);

  }
  else {
    BUG_MSG("Object is not a GedaObject");
  }
}

static Page *
geda_struct_page_new_common(GedaToplevel *toplevel, Page *page)
{
  page->hierarchy_up = -2;

  page->width  = toplevel->width;
  page->height = toplevel->height;

  page->left   = 0;
  page->right  = toplevel->width;
  page->top    = 0;
  page->bottom = toplevel->height;

  geda_toplevel_add_page (toplevel, page); /* Adds reference to page */
  geda_page_set_toplevel (page, toplevel); /* Adds reference to toplevel */

  /* Init tile array */
  geda_struct_tile_init (page);

  /* new selection mechanism */
  page->selection_list = geda_object_selection_new();

  /* init undo struct pointers */
  geda_struct_undo_init(page);

  /* Backup variables */
  time (&page->last_load_or_save_time);

  page->ops_since_last_backup    = 0;
  page->saved_since_first_loaded = 0;
  page->do_autosave_backup       = 0;

  return page;
}

/*!
 * \brief create a new page object
 * \par Function Description
 *  Creates a new page and adds the page to <B>toplevel</B>'s list of pages.
 *  Initializes the <B>filename</B> to given value \a <B>filename</B>. The
 *  <B>toplevel</B>'s current page is not changed by this function.
 */
Page *geda_struct_page_new (GedaToplevel *toplevel, const char *filename)
{
  Page *page;

  /* Create a blank Page object */
  page = geda_page_new();

  if (filename != NULL) {

    if (geda_file_get_is_path_absolute (filename)) {

      page->filename = geda_utility_string_strdup (filename);

    }
    else {

      char *pwd = getcwd(0,0);

      page->filename = geda_strconcat (pwd, DIR_SEPARATOR_S, filename, NULL);

      free (pwd);
    }
  }
  else {
    page->filename = geda_toplevel_get_untitled_name (toplevel);
  }

  return geda_struct_page_new_common(toplevel, page);
}

/*!
 * \brief Create a New Page with Notification
 * \par Function Description
 *  Creates a new Page with geda_page_new_with_notify. The new page
 *  object is full initialize and ready to use. If filename is NULL
 *  the default untitled name is assigned, If filename is absolute
 *  the it will be assigned directly, otherwise the filename will
 *  be assumed to be relative to current working directory.
 */
Page *geda_struct_page_new_with_notify (GedaToplevel *toplevel, const char *filename)
{
  Page *page;

  /* Create a blank Page object */
  page = geda_page_new_with_notify();

  if (filename != NULL) {
    if (geda_file_get_is_path_absolute (filename)) {
      page->filename = geda_utility_string_strdup (filename);
    }
    else {
      char *pwd = getcwd(0,0);
      page->filename = g_build_filename (pwd, filename, NULL);
      free (pwd);
    }
  }
  else {
    page->filename = geda_utility_string_strdup (toplevel->untitled_name);
  }

  return geda_struct_page_new_common(toplevel, page);
}

/*!
 * \brief Autosave initialization function.
 * \par Function Description
 *  This function sets up the autosave callback function.
 *
 * \param [in] toplevel  The GedaToplevel object.
 */
void geda_struct_page_autosave_init(GedaToplevel *toplevel)
{
  if (toplevel->auto_save_interval != 0) {

    /* 1000 converts seconds into milliseconds */
    toplevel->auto_save_timeout =
      g_timeout_add(toplevel->auto_save_interval*1000,
                    (GSourceFunc) geda_struct_page_autosave,
                    toplevel);
  }
}

/*!
 * \brief Autosave callback function.
 * \par Function Description
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
int geda_struct_page_autosave (GedaToplevel *toplevel)
{
  if (toplevel == NULL) {
    u_log_message (_("Disabling auto save timer, no toplevel"));
    return 0;
  }

  /* Do nothing if the interval is 0, we will not be called again */
  if (toplevel->auto_save_interval != 0) {

    if ( toplevel->pages != NULL) {

      const GList *iter;

      for ( iter = geda_toplevel_get_pages(toplevel); iter != NULL; NEXT(iter))
      {
        Page *p_current = (Page*)iter->data;

        if (geda_page_get_changed (p_current) > 0) {

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

/*!
 * \brief Check if CHANGED flag is set for any page in list.
 * \par Function Description
 *  This function checks the CHANGED flag for all pages in the <B>list</B>
 *  object.
 *
 * \param [in] list  PageList to check CHANGED flag in.
 *
 * \return 1 if any page has the CHANGED flag set, 0 otherwise.
 */
bool geda_struct_page_check_changed (PageList *list)
{
  const GList *iter;

  for ( iter = geda_list_get_glist(list); iter != NULL; NEXT(iter))
  {
    Page *p_current = iter->data;

    if (p_current->CHANGED) {
      return TRUE;
    }
  }

  return FALSE;
}

/*!
 * \brief Reset the CHANGED flag of all pages.
 * \par Function Description
 *  This function resets the CHANGED flag of each page following \a head.
 *
 * \param [in,out] list  Page list to set CHANGED flags in.
 */
void geda_struct_page_clear_changed (PageList *list)
{
  const GList *iter;

  for (iter = geda_list_get_glist(list); iter != NULL; NEXT(iter))
  {
    Page *p_current = iter->data;
    p_current->CHANGED = 0;
  }
}

/*!
 * \brief delete a page and it's contents
 * \par Function Description
 *  Deletes a single page <B>page</B> from the <B>toplevel</B> list of
 *  pages. If the page being deleted is the current <B>page</B>, this
 *  function sets the field <B>page_current</B> of the GedaToplevel
 *  struct to NULL. (WEH: which is probably not a great idea, lastpage)
 *
 * \sa #geda_struct_page_delete_list() to delete all pages of a <B>toplevel</B>
 *
 * \param [in] toplevel   Toplevel GedaObject to which page belongs.
 * \param [in] page       Page to be removed.
 * \param [in] previous   If True the previous page will be set current
 *                        if the page being deleted is the current page.
 */
void geda_struct_page_delete (GedaToplevel *toplevel, Page *page, int previous)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  /* We need to temporarily make the page being deleted current because
   * various functions called below (some indirectly) assume they are
   * deleting objects from the current page.
   *
   * These functions are known to include:
   *   geda_struct_object_release ()
   */

  if (GEDA_IS_PAGE(page)) {

    Page *old_current;
    Page *page_up;
    char *saved_cwd;

    if (!previous) {
      saved_cwd = getcwd(0,0);
    }
    else {
      saved_cwd = NULL;
    }

    /* save page_current and switch to page */
    if (page == toplevel->page_current) {
      old_current = NULL;
    }
    else {
      if (previous) {
        old_current = geda_toplevel_get_current_page(toplevel);
      }
      else {
        old_current = NULL;
      }
      geda_struct_page_goto (page);
    }

    geda_remove_backup_file(page->filename);

    /* Free the selection list */
    geda_list_unref (page->selection_list);
    page->selection_list = NULL;

    /* then delete objects of page */
    geda_struct_page_delete_objects (page);

    /* Release objects in the place list. */
    geda_struct_object_release_objects (page->place_list);
    page->place_list = NULL;

    geda_struct_tile_free_all (page);

    /* free current page undo structs */
    geda_struct_undo_free_all (page);

    /* ouch, deal with parents going away and the children still around */
    page->hierarchy_up = -2;

    GEDA_FREE (page->filename);

#if DEBUG
    geda_struct_tile_print (toplevel, page);
#endif

    if (previous) {
      page_up = geda_toplevel_get_page_up(toplevel);
    }
    else {
      page_up = NULL;
    }

    /*geda_page_weakref_notify (page);*/

    geda_toplevel_remove_page(toplevel, page); /* remove reference on page */

    /* This should destroy the Page object */
    geda_page_unref (page); /* Removes internal reference to toplevel */

    /* restore page_current */
    if (old_current != NULL) {

      /* page_current was not the page deleted */
      geda_struct_page_goto (old_current);
    }
    else {

      /* page was page_current, so check previous flag */
      if (previous) {

        /* If page was not the first page */
        if (page_up) {
          geda_toplevel_set_current_page(toplevel, page_up);
        }
        else {

          /* The deleted page was the first of multiple pages so
           * set the current page to the next valid page in the
           * list of pages */
          GList *iter;

          iter = geda_toplevel_get_pages(toplevel);

          while (iter) {

            page = iter->data;

            if (GEDA_IS_PAGE(page)) {
              geda_toplevel_set_current_page(toplevel, page);
              break;
            }
            iter = iter->next;
          }
        }
      }
      if (saved_cwd) {
        if (!chdir(saved_cwd));
      }
    }
    if (saved_cwd) {
      free(saved_cwd);
    }
  }
}

/*!
 * \brief Deletes the list of pages in <B>toplevel</B>.
 * \par Function Description
 *  Deletes the list of pages of <B>toplevel</B>.
 *  This function should only be called when you are finishing up.
 *
 * \param toplevel  The GedaToplevel object.
 */
void geda_struct_page_delete_list (GedaToplevel *toplevel)
{
  GList *list_copy, *iter;

  /* geda_struct_page_delete removes items from the page list, so make a copy */
  list_copy = g_list_copy (geda_toplevel_get_pages (toplevel));

  for (iter = list_copy; iter != NULL; NEXT(iter)) {

    Page *page = iter->data;

    if (GEDA_IS_PAGE (page)) {

      geda_object_notify_change_remove_all(page);

      geda_struct_page_delete (toplevel, page, FALSE);

    }
  }

  g_list_free (list_copy);

}

/*!
  \brief Get the current page
* \par Function Description
*  This function returns the current Page object.
*
* \sa geda_struct_page_set_current, geda_struct_page_goto
*
* \param [in,out] toplevel This toplevel
*/
Page *geda_struct_page_get_current (GedaToplevel *toplevel)
{
  return geda_toplevel_get_current_page(toplevel);
}

/*!
 * \brief Set the current page
 * \par Function Description
 *  Changes the current page in \a toplevel to the page \a page.
 *
 * \sa geda_struct_page_get_current, geda_struct_page_goto
 *
 * \param [in,out] toplevel This toplevel
 * \param [in] page The new current page
 *
 * \returns TRUE if \a page was set as the current page
 */
bool geda_struct_page_set_current (GedaToplevel *toplevel, Page *page)
{
   return geda_toplevel_set_current_page(toplevel, page);
}

/*!
 * \brief Get File Extension of the File Assocatiacted with Page.
 * \par Function Description
 * Returns a pointer to extension of the filename associated with Page.
 * The string is owned by the Page and must not be changed!
 *
 * \param [in] page Pointer to a Page data structure.
 *
 * \return point to the page file name extension or NULL if there
 *         either no file name or no DOT suffix used in the name.
 *
 * \sa geda_struct_page_is_symbol_file
 */
const char *geda_struct_page_get_file_extension (Page *page)
{
  if (page != NULL && page->filename != NULL) {
    return geda_file_get_filename_ext(page->filename);
  }
  return NULL;
}

/*!
 * \brief Get the Current Selection for a given Page
 * \par Function Description
 *  Returns the selection GedaList member given the Page
 *  object or NULL if \a page is not a GedaPage object.
 *
 * \param page Pointer to a Geda Page object
 *
 * \returns Pointer to selection GedaList of the page
 *
 * \sa geda_page_get_selection_list geda_toplevel_struct_get_selection
 */
SELECTION *geda_struct_page_get_selection (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->selection_list : NULL;
}

/*!
 * \brief Changes set current page in toplevel and change directory
 * \par Function Description
 *  Calls geda_toplevel_set_current_page to set the current page
 *  referenced by \a toplevel if and only if \a toplevel is valid
 *  GedaToplevel. Page must be in the toplevels list of pages.
 *  If \a page is valid, the current working directory is set to
 *  the directory associated with the file.
 *
 * \param page      The Page to go to
 *
 * \returns True on success, otherwise FALSE
 */
bool geda_struct_page_goto (Page *page)
{
  bool  success;

  if (GEDA_IS_PAGE(page)) {

    char *target_dirname;

#ifdef HAVE_ERRNO_H
    int   sav_err = 0;
#endif

    target_dirname = geda_get_dirname(page->filename);

    if (!chdir (target_dirname)) {

      if (GEDA_IS_TOPLEVEL(page->toplevel)) {

        success = TRUE;            /* Assume success */

        /* Check if page is already the current page, set if not */
        if (page->toplevel->page_current != page) {

          success = geda_toplevel_set_current_page(page->toplevel, page);
        }
      }
      else {
        success = FALSE;  /* Page has invalid pointer to toplevel */
      }
    }
    else {

#ifdef HAVE_ERRNO_H
      sav_err = errno;   /* GEDA_FREE will overwrite the error */
#endif

      success = FALSE;
    }

    GEDA_FREE (target_dirname);

#ifdef HAVE_ERRNO_H
    errno = sav_err;
#endif

  }
  else {
    success = FALSE;  /* Invalid pointer to Page */
  }

  return success;
}

/*!
 * \brief Get is Page a Symbol file.
 * \par Function Description
 *  Returns true if the filename associated with page ends in ".sym"!
 *
 * \param [in] page Pointer to a Page data structure.
 *
 * \return bool TRUE if Page is data from a Symbol file
 *
 */
bool geda_struct_page_is_symbol_file (Page *page) {

  if (page != NULL) {

    const char *ext;

    if ((ext = geda_struct_page_get_file_extension(page)) != NULL) {

      return (strcmp (ext, SYMBOL_FILE_SUFFIX) == 0);

    }
  }

  return FALSE;
}

/*!
 * \brief Print full GedaToplevel structure.
 * \par Function Description
 *  This function prints the internal structure of <B>toplevel</B>'s
 *  list of pages.
 *
 * \param [in] toplevel  The GedaToplevel object to print.
 */
void geda_struct_page_print_all (GedaToplevel *toplevel)
{
  const GList *iter;

  for (iter = geda_toplevel_get_pages(toplevel); iter; iter = iter->next)
  {
    Page *page = (Page*)iter->data;
    printf ("FILENAME: %s\n", page->filename);
    geda_struct_print_forw (page->_object_list);
  }
}

static int geda_struct_page_comparator(const void *pg1, const void *pg2)
{
   const Page *page1 = pg1;
   const Page *page2 = pg2;

   return page1->pid - page2->pid;
}

/*!
 * \brief Resequence the Page List by Page Id.
 * \par Function Description
 *  Sorts the page list in <B>toplevel</B> by page Id, which is
 *  equivalent to the order the pages were loaded.
 *
 * \param [in] toplevel  The GedaToplevel.
 */
void geda_struct_page_resequence_by_ids (GedaToplevel *toplevel)
{
  if (g_list_length(geda_toplevel_get_pages(toplevel)) > 1) {

    toplevel->pages->glist = g_list_sort (toplevel->pages->glist,
                                          geda_struct_page_comparator);
  }
}

/*!
 * \brief Saves all the pages of a GedaToplevel object.
 * \par Function Description
 *  Saves all the pages in the <B>toplevel</B> parameter.
 *
 * \param [in] toplevel  The GedaToplevel to save pages from.
 *
 * \return The number of failed tries to save a page.
 */
int geda_struct_page_save_all (GedaToplevel *toplevel)
{
  const GList *iter;
  const GList *list;
  int status = 0;

  list = geda_toplevel_get_pages(toplevel);

  for ( iter = list; iter; iter = iter->next) {

    Page *p_current;

    p_current = (Page*)iter->data;

    if (geda_save_file (toplevel, p_current, p_current->filename, NULL)) {

      u_log_message ("%s \"%s\"\n", _("Saved"), p_current->filename);

      geda_page_set_changed (p_current, 0); /* reset CHANGED flag */
    }
    else {

      u_log_message ("%s \"%s\"\n", _("Could NOT save"), p_current->filename);

      status++; /* increment the error counter */
    }
  }

  return status;
}

/*!
 * \brief Saves all unsaved pages of a GedaToplevel object.
 * \par Function Description
 *  Saves all pages in given <B>toplevel</B> for which the CHANGED flag
 *  is set.
 *
 * \param [in] toplevel  The GedaToplevel to save pages from.
 *
 * \return The number of failed tries to save a page.
 */
int geda_struct_page_save_all_changed (GedaToplevel *toplevel)
{
  const GList *iter;
  const GList *list;
  int status = 0;

  list = geda_toplevel_get_pages(toplevel);

  for ( iter = list; iter; iter = iter->next) {

    Page *p_current;

    p_current = (Page*)iter->data;

    if (p_current && p_current->CHANGED) {

      if (geda_save_file (toplevel, p_current, p_current->filename, NULL)) {

        u_log_message ("%s \"%s\"\n", _("Saved"), p_current->filename);

        geda_page_set_changed(p_current, 0); /* reset CHANGED flag */

      }
      else {

        u_log_message ("%s \"%s\"\n", _("Could NOT save"), p_current->filename);

        status++; /* increment the error counter */
      }
    }
  }

  return status;
}

/*!
 * \brief Search for pages by filename.
 * \par Function Description
 *  Searches in \a toplevel's list of pages for a page with a filename
 *  equal to \a filename.
 *
 * \param toplevel  Must be a valid GedaToplevel object
 * \param filename  The filename string to search for
 *
 * \return Page pointer to a matching page, NULL otherwise.
 *
 * \sa geda_toplevel_struct_get_page_by_name
 */
Page *geda_struct_page_search (GedaToplevel *toplevel, const char *filename)
{
  if (filename) {

    const GList *iter;
    const GList *list;

    list = geda_toplevel_get_pages(toplevel);

    for (iter = list; iter; iter = iter->next)   {

      Page *page = (Page*)iter->data;

      if (g_ascii_strcasecmp (page->filename, filename) == 0)
        return page;
    }
  }
  return NULL;
}

/*!
 * \brief Search for a page given its page id in a page list.
 * \par Function Description
 *  This functions returns the page that have the page id \a pid in
 *  the list of pages starting at \a page_list, or NULL if there is no
 *  such page.
 *
 * \param [in] list      The list of page to search the page in.
 * \param [in] pid       The ID of the page to find.
 *
 * \returns A pointer on the page found or NULL if not found.
 */
Page *geda_struct_page_search_by_page_id (PageList *list, int pid)
{
  const GList *iter;

  for (iter = geda_list_get_glist (list); iter != NULL; NEXT(iter))
  {
    Page *page = (Page*)iter->data;

    if (page->pid == pid) {
      return page;
    }
  }

  return NULL;
}

/*!
 * \brief Set the font-renderer-specific bounds function.
 * \par Function Description
 *  Set the function to be used to calculate text bounds for #Text
 *  GedaObjects associated with the page. This allows a per page
 *  renderer function to be defined. If the function is not defined
 *  the renderer for the Toplevel will be used instead, provided a
 *  Toplevel renderer is defined.
 *
 * \param [in] page      Page to associate the render text function.
 * \param [in] func      Function to use.
 * \param [in] user_data User data to be passed to the function.
 */
void geda_struct_page_set_bounds_func(Page *page, RenderedBoundsFunc func, void *user_data)
{
  if (GEDA_IS_PAGE(page)) {
    page->rendered_text_bounds_func = func;
    page->rendered_text_bounds_data = user_data;
  }
  else {
    BUG_PMSG("Invalid page pointer", page);
  }
}

/*!
 * \brief Append an GedaObject to the Page
 * \par Function Description
 *  Links the passed GedaObject to the end of the Page's linked list
 *  of objects and then calls object_added. Notice that only the
 *  given object is added, descendents of \a object are not added.
 *
 * \param [in] page      The Page the object is being added to.
 * \param [in] object    The GedaObject being added to the page.
 */
void geda_struct_page_append_object (Page *page, GedaObject *object)
{
  if (GEDA_IS_PAGE(page)) {

    if (GEDA_IS_OBJECT(object)) {

      geda_page_add_object(page, object);

      object_added (page, object);
    }
    else {
      BUG_PMSG("Object is not a GedaObject", object);
    }

  }
  else {
    BUG_PMSG("Invalid page pointer", page);
  }
}

/*!
 * \brief Append a GList of Objects to the Page
 * \par Function Description
 *  Links the passed GedaObject GList to the end of the Page's
 *  object_list.
 *
 * \param [in] page      The Page the objects are being added to.
 * \param [in] obj_list  The Object list being added to the page.
 *
 * \todo The list of objects is not checked
 */
void geda_struct_page_append_list (Page *page, const GList *obj_list)
{
  if (GEDA_IS_PAGE(page)) {

    const GList  *iter;

    for (iter = obj_list; iter != NULL; iter = iter->next) {

      GedaObject *object = iter->data;

      geda_page_add_object(page, object);

      object_added (page, object);
    }
  }
  else {
    BUG_PMSG("Invalid page pointer", page);
  }
}

/*!
 * \brief Remove an GedaObject from the Page
 * \par Function Description
 *  Removes the passed GedaObject from the Page's
 *  linked list of objects.
 *
 * \param [in] page      The Page the object is being removed from.
 * \param [in] object    The GedaObject being removed from the page.
 */
void geda_struct_page_remove_object (Page *page, GedaObject *object)
{
  if (GEDA_IS_PAGE(page)) {

    if (GEDA_IS_OBJECT(object)) {

      pre_object_removed (page, object);

      geda_page_remove_object (page, object);

      geda_object_notify_emit_change (object);

      page->CHANGED = 1;
    }
  }
  else {
    BUG_PMSG("Invalid page pointer", page);
  }
}

/*!
 * \brief Replace an GedaObject in a Page, in the same list position.
 * \par Function Description
 * Removes \a object1 from \a page's linked list of objects, and puts
 * \a object2 in the position thus vacated. If \a object1 is not in \a
 * page, object2 is appended to \a page.
 *
 * \param [in] page      The Page to be modified.
 * \param [in] object1   The GedaObject being removed from the page.
 * \param [in] object2   The GedaObject being added to the page.
 */
void geda_struct_page_replace_object (Page *page, GedaObject *object1, GedaObject *object2)
{
  if (GEDA_IS_PAGE(page)) {

    GList *iter = g_list_find (page->_object_list, object1);

    /* If object1 not found, append object2 */
    if (iter == NULL) {
      geda_struct_page_append_object (page, object2);
    }
    else {

      pre_object_removed (page, object1);

      if (GEDA_IS_OBJECT(object2)) {
        iter->data = object2;
        object_added (page, object2);
      }
      else {
        BUG_PMSG("Object is not a GedaObject", object2);
      }
    }
  }
  else {
    BUG_PMSG("Invalid page pointer", page);
  }
}

/*!
 * \brief Remove and free all GedaObjects from the Page
 * \par Function Description
 *  Removes and releases all GedaObjects from the Page.
 *
 * \param [in] page      The Page being cleared.
 */
void geda_struct_page_delete_objects (Page *page)
{
  if (GEDA_IS_PAGE(page)) {

    GList *objects = page->_object_list;
    GList *iter;

    for (iter = objects; iter != NULL; NEXT(iter)) {
      pre_object_removed (page, iter->data);
    }

    geda_struct_object_release_objects (objects);

    g_list_free(page->_object_list);
    page->_object_list = NULL;
  }
  else {
    BUG_PMSG("Invalid page pointer", page);
  }
}

/*!
 * \brief Return an GedaObject on the Page by ID
 * \par Function Description
 *  An accessor for the Page's GList of objects.
 *
 * \note The GedaObject is owned by the Page, and must not be
 *       free'd or modified by the caller.
 *
 * \sa geda_struct_page_get_objects
 *
 * \param [in] page      The Page to get objects on.
 * \param [in] sid       The Page Id.
 *
 * \returns a pointer to the Page's GList of objects
 */
GedaObject *geda_struct_page_get_object (Page *page, int sid)
{
  return geda_page_get_object(page, sid);
}

/*!
 * \brief Return a GList of Objects on the Page
 * \par Function Description
 *  An accessor for the Page's GList of objects.
 *
 * \note This GList is owned by the Page, and must not be
 *       free'd or modified by the caller.
 *
 * \sa geda_struct_page_get_object
 *
 * \param [in] page      The Page to get objects on.
 *
 * \returns a pointer to the Page's GList of objects
 */
GList *geda_struct_page_get_objects (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->_object_list : NULL;
}

/*!
 * \brief Find the objects in a given region
 * \par Function Description
 *  Finds the objects which are inside, or intersect
 *  the passed box shaped region.
 *
 * \param [in] page      The Page to find objects on
 * \param [in] rects     The RECTANGLE regions to check
 * \param [in] n_rects   The number of regions
 *
 * \return The GList of Objects in the region
 */
GList *geda_struct_page_objects_in_regions (Page *page, RECTANGLE *rects, int n_rects)
{
  if (GEDA_IS_PAGE(page)) {

    GList *iter, *list = NULL;

    for (iter = page->_object_list; iter != NULL; NEXT(iter)) {

      GedaObject *object = iter->data;
      int left, top, right, bottom;

      if (geda_object_get_bounds (object, &left, &top, &right, &bottom)) {

        int i;

        for (i = 0; i < n_rects; i++) {

          if (right  >= rects[i].lower_x &&
              left   <= rects[i].upper_x &&
              top    <= rects[i].upper_y &&
              bottom >= rects[i].lower_y)
          {
            list = g_list_prepend (list, object);
            break;
          }
        }
      }
    }

    list = g_list_reverse (list);

    return list;
  }
  return NULL;
}

/*!
 * \brief Find the objects in a given region
 * \par Function Description
 *  Finds the objects which are inside, or intersect
 *  the passed box shaped region.
 *
 * \param [in] page      The Page to find objects on.
 * \param [in] min_x     The smaller X coordinate of the region.
 * \param [in] min_y     The smaller Y coordinate of the region.
 * \param [in] max_x     The larger  X coordinate of the region.
 * \param [in] max_y     The larger  Y coordinate of the region.
 *
 * \return The GList of Objects in the region.
 */
GList *geda_struct_page_objects_in_region (Page *page, int min_x, int min_y, int max_x, int max_y)
{
  RECTANGLE rect;

  rect.lower_x = min_x;
  rect.lower_y = min_y;
  rect.upper_x = max_x;
  rect.upper_y = max_y;

  return geda_struct_page_objects_in_regions (page, &rect, 1);
}
