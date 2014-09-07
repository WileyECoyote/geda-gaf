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

#include <geda_standard.h>

#include "libgeda_priv.h"

/*! \brief Returns the Last Undo Record given some Record
 *  \par Function Description
 *  This function returns the last record of the chain of
 *  Undo records associated with \a head.
 *
 *  \remarks should probably just make static
 */
UNDO *s_undo_return_tail(UNDO *head)
{
  UNDO *u_current=NULL;
  UNDO *ret_struct=NULL;

  u_current = head;
  while ( u_current != NULL ) { /* goto end of list */
    ret_struct = u_current;
    u_current = u_current->next;
  }

  return(ret_struct);
}

/*! \brief Returns the First Undo Record given some Record
 *  \par Function Description
 *  The function starts at the given record, \a tail and
 *  transverses upward until the top record is found.
 *
 *  \returns the real HEAD of the undo stack.
 *
 *  \remarks Another no so usefull function. This function
 *           is not used
 */
UNDO *s_undo_return_head(UNDO *tail)
{
  UNDO *u_current=NULL;
  UNDO *ret_struct=NULL;

  u_current = tail;
  while ( u_current != NULL ) { /* goto end of list */
    ret_struct = u_current;
    u_current = u_current->prev;
  }

  return(ret_struct);
}

/*! \todo Finish function documentation!!!
 *  \brief Allocate and Iniclize a New Empty Undo Record
 *  \par Function Description
 *  This function is not used
 *
 *  \returns An Empty UNDO structure, the structure should be
 *           freed when no longer needed.
 */
UNDO *s_undo_new_head(void)
{
  UNDO *u_new;

  u_new = (UNDO *) GEDA_MEM_ALLOC(sizeof(UNDO));
  u_new->type = -1;
  u_new->filename = NULL;
  u_new->object_list = NULL;
  u_new->left = u_new->right = u_new->top = u_new->bottom = -1;

  u_new->page_control = 0;
  u_new->up = -2;

  u_new->prev = NULL;
  u_new->next = NULL;

  return(u_new);
}

/*! \todo Finish function documentation!!!
 *  \brief Frees Allocation of the Given Undo Record
 *  \par Function Description
 *  This function is not used
 *
 *  \param [in] u_head Pointer to UNDO struture to be freed
 */
void s_undo_destroy_head(UNDO *u_head)
{
  GEDA_FREE(u_head);
}

/*! \brief Creates and Returns a New Disk Type Undo record
 *  \par Function Description
 *   Allocates an Undo structure and poplates values with data
 *   from the given \a page for a Disk Memory type record. The
 *   new record becomes tail and is returned.
 *
 *  \param [in] page  A GedaPage Object
 *  \param [in] type  integer <B>\a flag</B> can be one of the
 *                    following values:
 *  \par
 *  <DL>
 *    <DT>UNDO_ALL</DT>
 *    <DT>UNDO_VIEWPORT_ONLY</DT>
 *  </DL>
 */
UNDO *s_undo_add_disk (int type, char *filename, Page *page)
{
  UNDO *head;
  UNDO *tail;
  UNDO *u_new;
  UNDO *u_ret;

  head = page->undo_tos;

  u_new = (UNDO *) GEDA_MEM_ALLOC(sizeof(UNDO));

  u_new->filename     = u_string_strdup (filename);

  u_new->object_list  = NULL;

  u_new->type         = type;
  u_new->modified     = page->CHANGED;

  u_new->left         = page->left;
  u_new->top          = page->top;
  u_new->right        = page->right;
  u_new->bottom       = page->bottom;

  u_new->page_control = page->page_control;
  u_new->up           = page->up;

  if (head == NULL) {
    u_new->prev = NULL; /* setup previous link */
    u_new->next = NULL;
    u_ret       = u_new;
  }
  else {
    tail        = s_undo_return_tail(head);
    u_new->prev = tail; /* setup previous link */
    u_new->next = NULL;
    tail->next  = u_new;
    u_ret       = tail->next;
  }
  return u_ret;
}

/*! \brief Creates and Returns a New Memory Type Undo record
 *  \par Function Description
 *   Allocates an Undo structure and poplates values with data
 *   from the given \a page for a Memory type Undo record. The
 *   new record becomes tail and is returned.
 *
 *  \param [in] page  A GedaPage Object
 *  \param [in] type  integer <B>\a flag</B> can be one of the
 *                    following values:
 *  \par
 *  <DL>
 *    <DT>UNDO_ALL</DT>
 *    <DT>UNDO_VIEWPORT_ONLY</DT>
 *  </DL>
 */
UNDO *s_undo_add_memory (int type, Page *page)
{
  UNDO *head;
  UNDO *tail;
  UNDO *u_new;
  UNDO *u_ret;

  head = page->undo_tos;

  u_new = (UNDO *) GEDA_MEM_ALLOC(sizeof(UNDO));

  u_new->filename     = NULL;

  u_new->object_list  = o_glist_copy_all (s_page_get_objects (page), NULL);

  u_new->type         = type;
  u_new->modified     = page->CHANGED;

  u_new->left         = page->left;
  u_new->top          = page->top;
  u_new->right        = page->right;
  u_new->bottom       = page->bottom;

  u_new->page_control = page->page_control;
  u_new->up           = page->up;

  if (head == NULL) {
    u_new->prev = NULL; /* setup previous link */
    u_new->next = NULL;
    u_ret       = u_new;
  }
  else {
    tail        = s_undo_return_tail(head);
    u_new->prev = tail; /* setup previous link */
    u_new->next = NULL;
    tail->next  = u_new;
    u_ret       = tail->next;
  }
  return u_ret;
}

/*! \brief Creates and Returns a New Undo record
 *  \par Function Description
 *   This function is obsolete and is not used, performance test indicate
 *   passing a pointer to the page as is done in s_undo_add_memory and
 *   s_undo_add_disk, is more efficient the passing 9 separate arguments
 *   that are all members of the Page structure.
 */
UNDO *s_undo_add (UNDO *head, int type, char *filename, GList *object_list,
                  int left, int top, int right, int bottom, int page_control,
                  int up)
{
  UNDO *tail;
  UNDO *u_new;

  u_new = (UNDO *) GEDA_MEM_ALLOC(sizeof(UNDO));

  u_new->filename = u_string_strdup (filename);

  u_new->object_list = object_list;

  u_new->type = type;
  u_new->modified = 1;

  u_new->left = left;
  u_new->top = top;
  u_new->right = right;
  u_new->bottom = bottom;

  u_new->page_control = page_control;
  u_new->up = up;

  if (head == NULL) {
    u_new->prev = NULL; /* setup previous link */
    u_new->next = NULL;
    return(u_new);
  } else {
    tail = s_undo_return_tail(head);
    u_new->prev = tail; /* setup previous link */
    u_new->next = NULL;
    tail->next = u_new;
    return(tail->next);
  }
}

/*! \brief Print all Undo records for debugging purposes
 *  \par Function Description
 *   Does not print every field of the record but does manage to
 *   convolute the terminal.
 */
void s_undo_print_all( UNDO *head )
{
  UNDO *u_current;

  u_current = head;

  printf("START printing undo ********************\n");
  printf("BOTTOM\n");
  while(u_current != NULL) {

    if (u_current->filename) printf("%s\n", u_current->filename);

    if (u_current->object_list) {
      print_struct_forw (u_current->object_list);
    }

    printf("\t%d %d %d %d\n", u_current->left, u_current->top,
           u_current->right, u_current->bottom);
    u_current = u_current->next;
  }
  printf("TOS\n");
  printf("Number of levels: %d\n", s_undo_levels(head));
  printf("DONE printing undo ********************\n");
  printf("\n");

}

/*! \brief Real Release all Memory Allocated for a Given Head
 *  \par Function Description
 *   s_undo_destroy_all starts at the bottom of the stack and
 *   works upward, releasing all objects (MEMORY) and freeing
 *   all filename (DISK) until the top of the stack is reached
 *   as indicated by a NULL pointer to the previous record.
 */
void s_undo_destroy_all(UNDO *head)
{
  UNDO *u_current;
  UNDO *u_prev;

  u_current = s_undo_return_tail(head);

  while (u_current != NULL) {
    u_prev = u_current->prev;
    GEDA_FREE(u_current->filename);

    if (u_current->object_list) {
      s_object_release_objects (u_current->object_list);
      u_current->object_list = NULL;
    }

    GEDA_FREE(u_current);
    u_current = u_prev;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief Removes head from the Undo stack
 *  \par Function Description
 *  Fortunately, this function is not used.
 */
void s_undo_remove(UNDO *head, UNDO *u_tos)
{
  UNDO *u_current;

  if (u_tos == NULL) {
    fprintf(stderr, "Got NULL for u_tos in s_undo_remove\n");
    return;
  }

  u_current = head;

  while (u_current != NULL) {
    if (u_current == u_tos) {
      if (u_current->next)
        u_current->next->prev = u_current->prev;
      else
        u_current->next = NULL;

      if (u_current->prev)
        u_current->prev->next = u_current->next;
      else
        u_current->prev = NULL;

      GEDA_FREE(u_current->filename);

      if (u_current->object_list) {
        s_object_release_objects (u_current->object_list);
        u_current->object_list = NULL;
      }

      GEDA_FREE(u_current);
      return;
    }
    u_current = u_current->next;
  }
}

/*! \brief Remove all Undo record After the given Record
 *  \par Function Description
 *   Free memory or releases object belonging to records older
 *   than \a head, essentially, the record \a head will become
 *   tail.
 */
void s_undo_remove_rest(UNDO *head)
{
  UNDO *u_current;
  UNDO *u_next;

  u_current = head;

  while (u_current != NULL) {
    u_next = u_current->next;

    if (u_current->filename) {
      unlink(u_current->filename);
      GEDA_FREE(u_current->filename);
    }

    if (u_current->object_list) {
      s_object_release_objects (u_current->object_list);
      u_current->object_list = NULL;
    }

    GEDA_FREE(u_current);
    u_current = u_next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_undo_levels(UNDO *head)
{
  UNDO *u_current;
  int count = 0;

  u_current = head;
  while (u_current != NULL) {
    if (u_current->filename || u_current->object_list) {
      count++;
    }

    u_current = u_current->next;
  }

  return(count);
}

/*! \brief Reset Page Changed Flags in the Undo System
 *  \par Function Description
 *  Called by f_save to reset the page changed flags in the Undo system.
 *  This is accomplished by setting all Undo records as modified, and
 *  then setting the current record as not modified. This results in a
 *  "saved" page floating around in the Undo stack if a file is saved
 *  within the limits of the Undo buffer window.
 *
 *  For example, deleting 3 objects, then saving the page, then deleting
 *  3 more object followed by 4 Undo's and 1 Redo will leave the user
 *  back on the page that was saved and the page->CHANGED flag will be
 *  set accordingly.
 */
void s_undo_update_modified (Page *p_current)
{
  UNDO *u_current;
  UNDO *u_iter;

  u_iter = p_current->undo_tos;

  while (u_iter != NULL) {
    u_iter->modified = 1;
    u_iter = u_iter->prev;;
  }

  u_current = p_current->undo_current;
  if (u_current) u_current->modified = 0;
}

/*! \brief Initialize Page Object's Undo
 *  \par Function Description
 *  Fortunately, the complier is intelligent enough to optimize
 *  out this function!
 */
void s_undo_init(Page *p_current)
{
  p_current->undo_tos     = NULL;
  p_current->undo_bottom  = NULL;
  p_current->undo_current = NULL;
}

/*! \brief Release all Memory Allocated by the Undo System for Page
 *  \par Function Description
 *  This function is called when a page is destroyed to free all
 *  memory allocated by this module that is referenced by the
 *  given Page object. The functions calls s_undo_destroy_all
 *  to do the actual work.
 */
void s_undo_free_all(Page *p_current)
{
  s_undo_destroy_all(p_current->undo_bottom);
  p_current->undo_bottom   = NULL;
  p_current->undo_tos      = NULL;
  p_current->undo_current  = NULL;
}
