/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
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

#include <libgeda_priv.h>

#include <geda_debug.h>

/*! \brief */
static int page_control_counter=0;

static char *get_parent_path_filespec (Page *parent, const char *filename, char **filespec)
{
  char *cwd;
  char *fspec;
  char *pfile;
  char *ppath;

  cwd   = g_get_current_dir ();
  pfile = parent->filename;
  ppath = geda_file_path_get_dirname(pfile);

  if (strcmp (cwd, ppath) == 0) {
    GEDA_FREE(cwd);
  }

  errno = 0;

  fspec = g_build_filename(ppath, filename, NULL);

  access(fspec, F_OK | R_OK);

  if (errno) {
    errno = 0;
    GEDA_FREE (fspec);
    *filespec = NULL;
  }
  else {
    if(g_chdir(ppath)) {
      *filespec = NULL;
    }
    else {
      *filespec = fspec;
    }
  }

  GEDA_FREE(ppath);

  return cwd;
}

/*!
 * \brief Search and Load for Schematic Source Files
 * \par Function Description
 *  This function searches the associated source file referred by the
 *  <B>filename</B> and loads it.  If the <B>flag</B> is set to
 *  <B>HIERARCHY_NORMAL_LOAD</B> and the page is already in the list of
 *  pages it will return the <B>pid</B> of that page.
 *  If the <B>flag</B> is set to <B>HIERARCHY_FORCE_LOAD</B> then this
 *  function will load the page again with a new page id. The second case
 *  is mainly used by gnetlist where pushed down schematics MUST be unique.
 *
 * \param [in] toplevel      The GedaToplevel object.
 * \param [in] filename      Schematic file name.
 * \param [in] parent        The parent page of the schematic.
 * \param [in] page_control
 * \param [in] flag          sets whether to force load
 * \param [in] err           Gerror object
 *
 * \return The page loaded, or NULL if failed.
 *
 * \internal
 *  This function finds the associated source file and loads the page
 *  but only works for schematic files though this is basically push
 *  flag can either be HIERARCHY_NORMAL_LOAD or HIERARCHY_FORCE_LOAD
 *  flag is mainly used by gnetlist where pushed down schematics MUST
 *  be unique
 */
Page*
geda_struct_hierarchy_down_single(GedaToplevel *toplevel, const char *filename,
                                  Page *parent, int page_control, int flag,
                                  GError **err)
{
  char *cwd;
  char *string = NULL;
  Page *found  = NULL;

  g_return_val_if_fail ((toplevel != NULL), NULL);
  g_return_val_if_fail ((filename != NULL), NULL);
  g_return_val_if_fail ((parent != NULL), NULL);

  cwd = get_parent_path_filespec (parent, filename, &string);

  Page *exit_function () {
    GEDA_FREE (string);
    if (cwd) {
      g_chdir(cwd);
      GEDA_FREE (cwd);
    }
    return found;
  }

  if (!string) {
    string = geda_struct_slib_search_for_file(filename);
  }

  if (!string) {

    g_set_error (err, EDA_ERROR, EDA_ERROR_NOLIB,
                 _("Schematic not found in source library."));
  }
  else {

    switch (flag) {
      case HIERARCHY_NORMAL_LOAD:
      {
        char *filename = geda_file_sys_normalize_name (string, NULL);

        found = geda_struct_page_search (toplevel, filename);
        GEDA_FREE (filename);

        if (found) {

          Page *forbear;

          /* check whether this page is in the parents list */
          for (forbear = parent;
               forbear != NULL && found->pid != forbear->pid && forbear->hierarchy_up >= 0;
               forbear = geda_struct_page_search_by_page_id (toplevel->pages, forbear->hierarchy_up))
               {
                 ; /* void */
               }
               if (forbear != NULL && found->pid == forbear->pid) {

                 g_set_error (err, EDA_ERROR, EDA_ERROR_LOOP,
                              _("Hierarchy contains a circular dependency."));
                              return NULL;  /* error signal */
               }
               geda_struct_page_goto (found);
               if (page_control != 0) {
                 found->page_control = page_control;
               }
               found->hierarchy_up = parent->pid;
               return exit_function();
        }

        found = geda_struct_page_new_with_notify (toplevel, string);

        geda_file_open (toplevel, found, found->filename, NULL);
      }
      break;

      case HIERARCHY_FORCE_LOAD:
      {
        found = geda_struct_page_new_with_notify (toplevel, string);
        geda_file_open (toplevel, found, found->filename, NULL);
      }
      break;

      default:
        BUG_IMSG("Invalid flag", flag);
        return exit_function();
    }

    if (page_control == 0) {
      page_control_counter++;
      found->page_control = page_control_counter;
    }
    else {
      found->page_control = page_control;
    }

    found->hierarchy_up = parent->pid;
  }

  return exit_function();
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
Page *geda_struct_hierarchy_down_symbol (GedaToplevel     *toplevel,
                                         const CLibSymbol *symbol,
                                         Page             *parent)
{
  Page *page;
  char *filename;

  filename = geda_struct_clib_symbol_get_filename (symbol);
  page     = geda_struct_page_search (toplevel, filename);

  if (page) {
    /* change link to parent page since we can come here from
     * any parent and must come back to the same page */
    page->hierarchy_up = parent->pid;
    geda_struct_page_goto (page);
    GEDA_FREE (filename);
  }
  else {

    page = geda_struct_page_new_with_notify (toplevel, filename);
    GEDA_FREE(filename);

    geda_struct_page_goto (page);

    geda_file_open(toplevel, page, page->filename, NULL);

    page->hierarchy_up = parent->pid;
    page_control_counter++;
    page->page_control = page_control_counter;
  }

  return page;
}

/*!
 * \brief Search for the parent page of a page in hierarchy.
 * \par Function Description
 *  This function searches the parent page of page \a page in the
 *  hierarchy and checks all the pages in the list \a page_list.
 *  The function returns a pointer to the page if found, NULL
 *  otherwise.
 *
 * \note
 *  The page \a current_page must be in the list \a page_list.
 *
 * \param [in] page_list    The list of pages in which to search.
 * \param [in] current_page The reference page for the search.
 *
 * \returns A pointer on the page found or NULL if not found.
 */
Page *geda_struct_hierarchy_find_up_page (PageList *page_list, Page *current_page)
{
  g_return_val_if_fail (GEDA_IS_PAGE(current_page), NULL);

  if (current_page->hierarchy_up < 0) {
    u_log_message(_("There are no schematics above the current one!\n"));
    return NULL;
  }

  return geda_struct_page_search_by_page_id (page_list, current_page->hierarchy_up);
}

/*!
 * \brief Load a hierarchical subpage
 * \par Function Description
 *  Implements geda_struct_hierarchy_down_single() without
 *  making the page the current page.
 *
 *  - Ensures a duplicate page is not loaded
 *  - Does not change the current page
 *  - Does not modify the most recent "up" page
 *
 * \param [in]  page
 * \param [in]  filename
 * \param [out] err
 *
 * \return A pointer to the subpage or NULL if an error occurred.
 */
Page *geda_struct_hierarchy_load_subpage (Page       *page,
                                          const char *filename,
                                          GError    **err)
{
  char *string;
  Page *subpage = NULL;

  g_return_val_if_fail (filename != NULL, NULL);
  g_return_val_if_fail (page != NULL, NULL);

  char *cwd = g_get_current_dir ();

  string = g_build_filename(cwd, filename, NULL);
  GEDA_FREE (cwd);

  errno = 0;
  access(string, F_OK | R_OK);

  if (errno) {
    errno = 0;
    GEDA_FREE (string);
    string = geda_struct_slib_search_for_file(filename);
  }

  if (string == NULL) {

    g_set_error (err, EDA_ERROR, EDA_ERROR_NOLIB,
                 _("Schematic not found in source library."));
    return NULL;
  }
  else {

    char *normalized = geda_normalize_filename (string, err);

    subpage = geda_struct_page_search (page->toplevel, normalized);

    if (subpage == NULL) {

      int success;

      subpage = geda_struct_page_new (page->toplevel, string);
      success = geda_file_open (page->toplevel, subpage, subpage->filename, err);

      if (!success) {
        geda_struct_page_delete (page->toplevel, subpage, FALSE);
        subpage = NULL;
      }
      else {
        subpage->page_control = ++page_control_counter;
      }
    }

    g_free (normalized);
  }

  return subpage;
}

/*!
 * \brief Find page hierarchy below a page.
 * \par Function Description
 *  This function traverses the hierarchy tree of pages and returns a
 *  flat list of pages that are below \a p_current. There are two \a
 *  flags that can be used to control the way that the return value is
 *  constructed: <B>HIERARCHY_NODUPS</B> returns a list without
 *  duplicate pages, and <B>HIERARCHY_POSTORDER</B> traverses the
 *  hierarchy tree and returns a postorder list instead of preorder.
 *
 * \param toplevel The GedaToplevel structure.
 * \param p_current The Page to traverse hierarchy for.
 * \param flags Flags controlling form of return value.
 * \return A GList of Page pointers.
 *
 * \warning Caller must destroy returned GList with g_list_free().
 */
GList *geda_struct_hierarchy_traverse_pages (GedaToplevel *toplevel,
                                                     Page *p_current,
                                                      int  flags)
{

  static GList *pages = NULL;
  const  GList *object_list;
  const  GList *iter;

  g_return_val_if_fail ((toplevel != NULL), NULL);
  g_return_val_if_fail ((p_current != NULL), NULL);

  /* init static variables the first time*/
  if (!(flags & HIERARCHY_INNERLOOP)) {
    pages = NULL;
  }

  /* preorder traversing */
  if (!(flags & HIERARCHY_POSTORDER)) {

    /* check whether we already visited this page */
    if ((flags & HIERARCHY_NODUPS) &&
        (g_list_find (pages, p_current) != NULL))
    {
      return pages;  /* drop the page subtree */
    }
    pages = g_list_append (pages, p_current);
  }

  object_list = geda_struct_page_get_objects (p_current);

  /* walk throught the page objects and search for underlaying schematics */
  for (iter = object_list; iter != NULL ; iter = g_list_next (iter)) {

    GError     *err;
    Page       *child_page;
    GedaObject *o_current;
    char       *filename;

    o_current = (GedaObject*)iter->data;

    /* only complex symbols can contain attributes */
    if (o_current->type != OBJ_COMPLEX)
      continue;

    /* Look outside symbol */
    filename = geda_attrib_search_attached_by_name (o_current, "source", 0);

    /* If not outside, then look inside symbol */
    if (filename == NULL) {
      filename = geda_attrib_search_inherited_by_name (o_current, "source", 0);
    }

    /* If no source attribute found then next component */
    if (filename == NULL)
      continue;

    err = NULL;

    /* we got a schematic source attribute, load the page and dive into it */
    child_page =
    geda_struct_hierarchy_down_single (toplevel, filename, p_current, 0,
                                       HIERARCHY_NORMAL_LOAD, &err);
    if (child_page != NULL) {
      /* call the recursive function */
      geda_struct_hierarchy_traverse_pages (toplevel, child_page,
                                            flags | HIERARCHY_INNERLOOP);
    }
    else {
      u_log_message ("%s '%s': %s\n", _("Failed to descend hierarchy into"),
                     filename, err->message);
      g_error_free (err);
    }

    GEDA_FREE (filename);
  }

  /* postorder traversing */
  if (flags & HIERARCHY_POSTORDER) {

    /* check whether we already visited this page */
    if ((flags & HIERARCHY_NODUPS) &&
        (g_list_find (pages, p_current) != NULL))
    {
      return pages;  /* don't append it */
    }
    pages = g_list_append (pages, p_current);
  }

  return pages;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Test function which only prints the name of a page and its number.
 */
int geda_struct_hierarchy_print_page (Page *p_current, void * data)
{
  printf("pagefilename: %s pageid: %d\n", p_current->filename, p_current->pid);
  return 0;
}

/*!
 * \brief Search for a page preceding a given page in hierarchy.
 * \par Function Description
 *  This function searches the previous sibling of page \a page in the
 *  hierarchy and checks all the pages preceding \a page in the list
 *  \a page_list.
 *
 * \note
 *  The page \a current_page must be in the list \a page_list.
 *
 * \param [in] page_list    The list of pages in which to search.
 * \param [in] current_page The reference page for the search.
 *
 * \returns A pointer on the page found or NULL if not found.
 */
Page *geda_struct_hierarchy_find_prev_page (PageList *page_list, Page *current_page)
{
  const GList *iter;

  iter = g_list_find (geda_list_get_glist (page_list), current_page);

  for (iter = g_list_previous (iter); iter != NULL; iter = g_list_previous (iter)) {

    Page *page = (Page*)iter->data;

    if (page->page_control == current_page->page_control) {
      return page;
    }
  }

  return NULL;
}

/*!
 * \brief Search for a page following a given page in hierarchy.
 * \par Function Description
 *  This function searches the next sibling of page \a page in the
 *  hierarchy and checks all the pages following \a page in the list
 *  \a page_list.
 *
 * \note The page \a current_page must be in the list \a page_list.
 *
 * \param [in] page_list    The list of pages in which to search.
 * \param [in] current_page The reference page for the search.
 *
 * \returns A pointer to the page found or NULL if not found.
 */
Page *geda_struct_hierarchy_find_next_page (PageList *page_list, Page *current_page)
{
  const GList *iter;

  iter = g_list_find (geda_list_get_glist (page_list), current_page);

  for (iter = g_list_next (iter); iter != NULL; iter = g_list_next (iter)) {

    Page *page = (Page*)iter->data;

    if (page->page_control == current_page->page_control) {
      return page;
    }
  }

  return NULL;
}
