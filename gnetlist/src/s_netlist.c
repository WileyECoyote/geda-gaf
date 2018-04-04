/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_netlist.c
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
#include <gettext.h>
#include <geda_debug.h>

/* used by the extract functions below */
#define DELIMITERS ",; "

/*! \brief Add a NETLIST record to NETLIST List
 *  \par Function Description
 *   Allocates and initializes a NETLIST record structure. The record
 *   is appended to the NETLIST List pointed to by \a ptr by adding
 *   links to the previous record if \a ptr is not NULL. If \a ptr is
 *   NULL then the ->prev member is set to NULL to indicate this is
 *   the first record.
 *
 *  \returns new node
 */
NETLIST *s_netlist_add(NETLIST *ptr)
{
  NETLIST *new_node;

  new_node = (NETLIST *) GEDA_MEM_ALLOC(sizeof(NETLIST));

  /* setup node information */
  new_node->nlid                = 0;
  new_node->cpins               = NULL;
  new_node->component_uref      = NULL;
  new_node->object_ptr          = NULL;
  new_node->hierarchy_tag       = NULL;
  new_node->composite_component = FALSE;

  /* Setup link list stuff */
  new_node->next = NULL;

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

/*! \brief Release memory for NETLIST Record Structures
 *  \par Function Description
 *   Iterates \a netlist and calls s_cpinlist_destroy_or_report for each
 *   found CPINLIST, frees internal strings for the component_uref and
 *   the and hierarchy_tag, and releases each NETLIST structure.
 *
 *  \param [in]  netlist  Either the regular or the graphical net list.
 *  \param [out] strings  List to be appended with the pointers of all
 *                        net-names encountered (but not removed).
 */
void s_netlist_destroy_or_report(NETLIST *netlist, GedaList *strings)
{
  NETLIST  *nl_iter;

  /* Get a pointer to the first record */
  nl_iter = s_netlist_return_head(netlist);

  while (nl_iter != NULL) {

    NETLIST *nl_current = nl_iter;

    GEDA_FREE(nl_current->component_uref);
    GEDA_FREE(nl_current->hierarchy_tag);

    if (nl_current->cpins) {
      s_cpinlist_destroy_or_report(nl_current->cpins, strings);
      nl_current->cpins = NULL;
    }

    nl_iter = nl_current->next;
    GEDA_FREE(nl_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_netlist_name_named_nets (GedaToplevel *pr_current,
                                NETLIST *named_netlist,
                                NETLIST *unnamed_netlist)
{

  NETLIST  *nl_current;
  CPINLIST *pl_current;
  NET      *n_current;
  char     *net_name;

  if (verbose_mode) {
    printf("\n- %s\n", _("Staring post processing"));
    printf("- %s:\n", _("Naming nets of graphical objects"));
  }

  /* this pass gives all nets a name, whether specified or creates a */
  /* name */
  nl_current = unnamed_netlist;

  while (nl_current != NULL) {

    if (nl_current->cpins) {

      pl_current = nl_current->cpins;

      while (pl_current != NULL) {

        if (pl_current->plid != -1) {
          verbose_print("p");
        }

        if (pl_current->plid != -1 && pl_current->nets) {

          verbose_print("n");
          net_name = NULL;

          n_current = pl_current->nets;

          while (n_current != NULL) {

            GEDA_FREE (n_current->net_name);

            n_current->net_name = s_netlist_netname_of_netid(pr_current,
                                                             named_netlist,
                                                             n_current->nid);
            if (n_current->net_name != NULL) {
              net_name = n_current->net_name;
            }
            n_current = n_current->next;
          }
          if (net_name != NULL) {
            pl_current->net_name = geda_utility_string_strdup(net_name);
          }
        }
        pl_current = pl_current->next;
      }
    }
    nl_current = nl_current->next;
  }

  verbose_done();

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_netlist_netname_of_netid (GedaToplevel *pr_current,
                                  NETLIST      *netlist_head,
                                  int           net_id)
{
  NETLIST  *nl_current;
  CPINLIST *pl_current;
  NET      *n_current;

  nl_current = netlist_head;

  /* walk through the list of components, and through the list
   * of individual pins on each, looking for the net identifier
   */
  while (nl_current != NULL) {

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {

      if (pl_current->net_name) {

        n_current = pl_current->nets;

        while (n_current != NULL) {

          if (n_current->nid == net_id) {
            return (geda_utility_string_strdup(n_current->net_name));
          }
          n_current = n_current->next;
        }
      }
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }
  return NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief Post Traversal Processing to rename nets
 *  \par Function Description
 *
 *  \param [in] pr_current GedaToplevel structure; toplevel,
 *  \param [in] head       Pointer to Global netlist in globals.c
 *
 *   \sa s_traverse_start
 */
void s_netlist_post_process(GedaToplevel *pr_current, NETLIST *head)
{
  NETLIST  *nl_current;
  CPINLIST *pl_current;

  nl_current = head;

  if (verbose_mode) {
    printf("\n- %s\n", _("Staring post processing"));
    printf("- %s:\n", _("Naming nets"));
  }

  /* this pass gives all nets a name, whether specified or
   * creates a name */
  nl_current = head;

  while (nl_current != NULL) {

    if (nl_current->cpins) {

      pl_current = nl_current->cpins;

      while (pl_current != NULL) {

        if (pl_current->plid != -1) {
          verbose_print("p");
        }

        if (pl_current->plid != -1 && pl_current->nets) {

          GEDA_FREE(pl_current->net_name);

          verbose_print("n");

          /* only name nets of components which have a uref */
          if (nl_current->component_uref) {

            char *net_name;

            net_name = s_net_name(pr_current, head, pl_current->nets,
                                              nl_current->hierarchy_tag,
                                              pl_current->node_type);

            pl_current->net_name = net_name;

            /* also put this name in the first node of the nets linked list */
            if (pl_current->net_name && pl_current->nets) {

              if (pl_current->nets->next) {
                GEDA_FREE(pl_current->nets->next->net_name);
                pl_current->nets->next->net_name = geda_utility_string_strdup (net_name);

              }
            }
          }
        }

        pl_current = pl_current->next;
      }
    }
    nl_current = nl_current->next;
  }

  verbose_done();

  if (verbose_mode) {
    printf("- %s:\n", _("Renaming nets"));
  }

  s_rename_all(pr_current, head);

  verbose_done();

  if (verbose_mode) {
    printf("- %s:\n", ("Resolving hierarchy"));
  }

  s_hierarchy_post_process(pr_current, head);

  verbose_done();

  if (pr_current->hierarchy_uref_mangle == FALSE) {

    if (verbose_mode) {
      printf("- %s:\n", _("Removing refdes mangling"));
    }
    s_hierarchy_remove_uref_mangling(pr_current, head);
  }

  verbose_done();
}

/*! \brief Print Net List
 *  \par Function Description
 *   Prints the net-list pointed to by \a nl_current
 */
void s_netlist_print(NETLIST *nl_current)
{
  NETLIST *nl_iter = NULL;

  nl_iter = nl_current;

  if (nl_iter == NULL) {
    return;
  }

  while (nl_iter != NULL) {

    if (nl_iter->nlid != -1) {

      const char *_component = _("component");

      if (nl_iter->component_uref) {
        printf("%s %s \n", _component, nl_iter->component_uref);
      }
      else {
        printf("%s %s \n", _component, _("SPECIAL"));
      }

      if (nl_iter->hierarchy_tag) {

        const char *msg = _("Hierarchy tag");

        printf("%s: %s\n", msg, nl_iter->hierarchy_tag);
      }

      if (nl_iter->cpins) {
        s_cpinlist_print(nl_iter->cpins);
      }

      printf("\n");
    }

    nl_iter = nl_iter->next;
  }
  printf("\n");
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * hack rename this to be s_return_head
 * update object_tail or any list of that matter
 */

NETLIST *s_netlist_return_head(NETLIST *tail)
{
  NETLIST *nl_current = NULL;
  NETLIST *ret_struct = NULL;

  nl_current = tail;

  while (nl_current != NULL) {  /* goto end of list */
    ret_struct = nl_current;
    nl_current = nl_current->prev;
  }

  return (ret_struct);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * hack rename this to be s_return_tail
 * update object_tail or any list of that matter
 */
NETLIST *s_netlist_return_tail(NETLIST * head)
{
  NETLIST *nl_current = NULL;
  NETLIST *ret_struct = NULL;

  nl_current = head;

  while (nl_current != NULL) {  /* goto end of list */
    ret_struct = nl_current;
    nl_current = nl_current->next;
  }

  return (ret_struct);
}
