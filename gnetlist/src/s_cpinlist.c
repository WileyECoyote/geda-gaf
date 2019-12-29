/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_cpinlist.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */

#include "../../config.h"

#include <gnetlist.h>
#include <geda_debug.h>

/*!
 * \brief Add a Pin List record to Pin List
 * \par Function Description
 *  Allocates and initializes a CPINLIST record structure. The
 *  record is appended to the Pin List given by \a ptr and links
 *  the previous record.
 *
 * \note \a ptr can be NULL.
 *
 * \returns new node
 */
CPINLIST *s_cpinlist_add(CPINLIST *ptr)
{
  CPINLIST *new_node;

  new_node = (CPINLIST *) GEDA_MEM_ALLOC(sizeof(CPINLIST));

  /* setup node information */
  new_node->plid = 0;
  new_node->node_type  = PIN_NET_NODE;
  new_node->pin_number = NULL;
  new_node->pin_label  = NULL;
  new_node->net_name   = NULL;
  new_node->nets       = NULL;

  /* Setup link list stuff */
  new_node->next       = NULL;

  if (ptr == NULL) {
    new_node->prev = NULL;	/* setup previous link */
    return (new_node);
  }
  else {
    new_node->prev = ptr;	/* setup previous link */
    ptr->next = new_node;
    return (ptr->next);
  }
}

/*!
 * \brief Release memory for CPINLIST Record Structures
 * \par Function Description
 *  Iterates \a pinlist and calls s_net_destroy_or_report for each
 *  found NET, frees internal strings for the pin label and the pin
 *  number and adds the net_name pointer to \a list before releasing
 *  the CPINLIST structure.
 *
 * \param [in]  pinlist  Pointer to linked list of CPINLIST structures.
 * \param [out] list     List to be appended with the pointers of all
 *                       net-names encountered (but not removed).
 */
void s_cpinlist_destroy_or_report(CPINLIST *pinlist, GedaList *list)
{
  CPINLIST *pl_iter;

  /* Get a pointer to the first record */
  pl_iter = s_cpinlist_return_head(pinlist);

  while (pl_iter != NULL) {

    CPINLIST *pl_current = pl_iter;

    if (pl_current->nets) {
      s_net_destroy_or_report (pl_current->nets, list);
      pl_current->nets = NULL;
    }

    GEDA_FREE(pl_current->pin_number);
    GEDA_FREE(pl_current->pin_label);

    geda_list_add_unique (list, pl_current->net_name);

    pl_iter = pl_current->next;
    GEDA_FREE(pl_current);
  }
}

/*!
 * \brief Netlister: Print list of Pins
 * \par Function Description
 *  Used by s_netlist_print() during verbose mode to iterate the
 *  pin list and write out pin information, followed by a call to
 *  s_net_print to output net list information for each pin in
 *  in the list pointed to by \a ptr.
 */
void s_cpinlist_print(CPINLIST *ptr)
{
  CPINLIST *pl_current;

  pl_current = ptr;

  while (pl_current != NULL) {

    if (pl_current->plid != -1) {

      if (pl_current->pin_number) {
        printf("	pin %s", pl_current->pin_number);
      }
      else {
        printf("	pin ?");
      }

      if (pl_current->pin_label) {
        printf(" (%s)", pl_current->pin_label);
      }
      else {
        printf(" ()");
      }

      if (pl_current->net_name) {
        printf(" %s", pl_current->net_name);
      }
      else {
        printf(" Null net name");
      }

      printf("\n");


      if (pl_current->nets) {
        s_net_print(pl_current->nets);
      }
    }

    pl_current = pl_current->next;
  }
}

/*!
 * \brief Return first node in a Pin list
 * \par Function Description
 *  hack rename this to be s_return_head
 *  update object_head or any list of that matter
 */
CPINLIST *s_cpinlist_return_head(CPINLIST * tail)
{
  CPINLIST *pl_current = NULL;
  CPINLIST *ret_struct = NULL;

  pl_current = tail;
  while (pl_current != NULL) {  /* goto end of list */
    ret_struct = pl_current;
    pl_current = pl_current->prev;
  }

  return (ret_struct);
}

/*!
 * \brief Return last record in a linked list
 * \par Function Description
 *  This is a hack, rename this to be s_return_tail
 *  update object_tail or any list of that matter.
 */
CPINLIST *s_cpinlist_return_tail(CPINLIST * head)
{
  CPINLIST *pl_current = NULL;
  CPINLIST *ret_struct = NULL;

  pl_current = head;

  while (pl_current != NULL) {  /* goto end of list */
    ret_struct = pl_current;
    pl_current = pl_current->next;
  }

  return (ret_struct);
}

/*!
 * \brief Search or Pin Number in List of Pins
 * \par Function Description
 *  Searches pin list \a ptr looking for a record containing \a pin_number.
 *
 * \returns pointer to pinlist record containing \a pin_number or NULL if
 *          the number was not found.
 */
CPINLIST *s_cpinlist_search_pin(CPINLIST *ptr, char *pin_number)
{
  CPINLIST *pl_current = NULL;

  pl_current = ptr;

  if (pl_current == NULL) {
    return (NULL);
  }

  while (pl_current != NULL) {

    if (pl_current->plid != -1 && (pl_current->pin_number != NULL)) {

      if (strcmp(pl_current->pin_number, pin_number) == 0) {

#if DEBUG
        printf("equal: %s %s\n", pl_current->pin_number, pin_number);
#endif

        return (pl_current);
      }
    }

    pl_current = pl_current->next;
  }

  return (NULL);
}
