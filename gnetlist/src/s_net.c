/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_net.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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

static int unnamed_net_counter = 1;
static int unnamed_bus_counter = 1;
static int unnamed_pin_counter = 1;

#define MAX_UNNAMED_NETS 99999999
#define MAX_UNNAMED_PINS 99999999

/*! \brief Add a NET record to NET List
 *  \par Function Description
 *   Allocates and initializes a NET record structure. The record
 *   is appended to the NET List given by \a ptr by adding links
 *   to the previous record if \a ptr is not NULL.
 *
 *  \note \a ptr can be NULL.
 *
 *  \returns new node or NULL if an error occurred allocating memory
 */
NET *s_net_add(NET *ptr)
{
  NET *new_node;

  new_node = (NET*)GEDA_MEM_ALLOC(sizeof(NET));

  if (new_node) {

    /* setup node information */
    new_node->net_name              = NULL;
    new_node->pin_label             = NULL;
    new_node->net_name_has_priority = FALSE;
    new_node->nid                   = 0;
    new_node->connected_to          = NULL;

    /* Setup link list stuff */
    new_node->next = NULL;

    if (ptr == NULL) {
      new_node->prev = NULL;        /* setup previous link */
      return (new_node);
    }
    else {
      new_node->prev = ptr;         /* setup previous link */
      ptr->next = new_node;
      return (ptr->next);
    }
  }

  return (NULL);
}

/*! \brief Release memory for a Net Record Structure
 *  \par Function Description
 *   Frees internal strings for the pin label and the connected_to
 *   and adds the pointer to net_name to \a list before releasing
 *   the NET structure for each NET in the linked list pointed to
 *   by \a ptr.
 *
 *  \param [in]  ptr   Pointer to linked list of NET structures.
 *  \param [out] list  List to be appended with the pointers of
 *                     net-names encountered (but not removed).
 */
void s_net_destroy_or_report(NET *ptr, GedaList *list)
{
  NET *iter;

  iter = s_net_return_head(ptr);

  while(iter != NULL) {

    NET *node = iter;

    GEDA_FREE(node->pin_label);
    GEDA_FREE(node->connected_to);

    geda_list_add_unique (list, node->net_name);

    iter = node->next;
    GEDA_FREE(node);
  }
}

/*! \brief Find a NET given the node Id
 *  \par Function Description
 *   Called by s_net_name
 */
int s_net_find(NET *net_head, NET *node)
{
  NET *n_current;

  n_current = net_head;

  while (n_current != NULL) {

    if (n_current->nid == node->nid) {
      return (TRUE);
    }

    n_current = n_current->next;
  }
  return (FALSE);
}

/*!
 * \brief Write a Net Record to standard out
 * \par Function Description
 *  Write the name and the id of the objects connected in the
 *  given net to standard-out.This is used if during verbose
 *  mode.
 */
void s_net_print(NET *n_current)
{
  while (n_current != NULL) {

    if (n_current->nid != -1) {

#if DEBUG
      if (n_current->net_name) {
        printf("\t%s [%d]\n", n_current->net_name, n_current->nid);
      }
#endif

      if (n_current->connected_to) {
        printf("\t\t%s [%d]\n", n_current->connected_to, n_current->nid);
      }
    }

    n_current = n_current->next;
  }
}


/*!
 * \brief Get Connected String for Pin
 * \par Function Description
 *  If the value of the pin's parent refdes is not "none", attempts
 *  to create a string pair by prefixing the pin number with uref,
 *  separated by a space, where uref may include the hierarchy tag.
 *  If a refdes does not exist s_netattrib_pinnum_get_connected_string
 *  is called to return the pin number prefixed with PIN_NET_PREFIX,
 *  i.e. "__netattrib_power_pin".
 *
 *  Note:
 *  1. Flat schematics do not have a hierarchy tag.
 *  2. For hierarchical schematics, the refdes is the hierarchy tag for
 *     pins belonging to the source, which may include a hierarchy tag
 *     if the source is not on the top-level.
 *  3. Strings returned with PIN_NET_PREFIX do not include hierarchy tags.
 *
 *  Examples of returned strings:
 *
 *  <DL>
 *      <DT>Flat schematic or Components on top-level</DT>
 *          <DD>"R3 2"</DD><BR>
 *          <DD>"U1 8"</DD>
 *      <DT>Hierarchical schematics</DT>
 *          <DD>"S1 3"</DD><BR>
 *          <DD>"S1/C1 1"</DD><BR>
 *          <DD>"S1/S2/U5 4"</DD>
 *      <DT> Pins without a uref</DT>
 *          <DD>"__netattrib_power_pin 1"</DD>
 *  </DL>
 *
 *  \param [in] pr_current    GedaToplevel toplevel structure;
 *  \param [in] object        Is a Pin object
 *  \param [in] hierarchy_tag hierarchy tag string
 *
 *  \sa s_traverse_net
 */
char *s_net_return_connected_string(GedaToplevel *pr_current,
                                    GedaObject   *object,
                                    char         *hierarchy_tag)
{
  GedaObject *o_current;

  char *pinnum    = NULL;
  char *uref      = NULL;
  char *temp_uref = NULL;
  char *string;
  char *misc;

  o_current = object;

  pinnum = geda_attrib_search_object_by_name (o_current, "pinnumber", 0);

#if DEBUG
  printf("found pinnum: %s\n", pinnum);
#endif

  /* Resolve the uref */
  temp_uref = geda_attrib_search_object_by_name (o_current->parent_object, "refdes", 0);

  if (temp_uref) {
    if (geda_stricmp(temp_uref,"none") == 0) {
      uref = NULL;
    }
    else { /* apply the hierarchy name to the uref */
      uref = s_hierarchy_create_uref(pr_current, temp_uref, hierarchy_tag);
    }
    GEDA_FREE(temp_uref);
  }
  else {
    uref = NULL;
  }

  if (uref && pinnum) {
    string = geda_sprintf("%s %s", uref, pinnum);
  }
  else {

    if (pinnum) {

      /* Prefixes pinnumber with "__netattrib_power_pin " */
      string = s_netattrib_pinnum_get_connected_string (pinnum);

    }
    else {
      if (hierarchy_tag) {
        misc = s_hierarchy_create_uref(pr_current, "U?", hierarchy_tag);
        string = geda_sprintf("%s ?", misc);
        GEDA_FREE(misc);
      }
      else {
        string = geda_utility_string_strdup("U? ?");
      }

      fprintf(stderr, _("Missing Attributes (refdes and pin number)\n"));
    }
  }

  GEDA_FREE(pinnum);
  GEDA_FREE(uref);

  return (string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * hack rename this to be s_return_head
 * update object_tail or any list for that matter
 */
NET *s_net_return_head(NET *tail)
{
  NET *n_current = NULL;
  NET *ret_struct = NULL;

  n_current = tail;
  while (n_current != NULL) {   /* goto end of list */
    ret_struct = n_current;
    n_current = n_current->prev;
  }

  return (ret_struct);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * hack rename this to be s_return_tail
 * update object_tail or any list for that matter
 */
NET *s_net_return_tail(NET *head)
{
  NET *n_current = NULL;
  NET *ret_struct = NULL;

  n_current = head;
  while (n_current != NULL) {   /* goto end of list */
    ret_struct = n_current;
    n_current = n_current->next;
  }

  return (ret_struct);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] pr_current   GedaToplevel toplevel structure;
 *  \param [in] net_head     Pointer to first netlist record structure
 *
 *  \sa s_netlist_post_process s_net_name
 */
char *s_net_name_search(GedaToplevel *pr_current, NET *net_head)
{
  NET       *n_current;
  EdaConfig *cfg;
  char      *name = NULL;
  int        naming_priority;

  n_current = net_head;

  cfg = eda_config_get_context_for_file (NULL);
  naming_priority = eda_config_get_integer (cfg, "gnetlist", "net-naming-priority", NULL);

  while (n_current != NULL) {

    char *net_name = n_current->net_name;

    if (net_name != NULL) {

      if (name == NULL) {

        /* Note a copy is made here because s_netlist_post_process will
         * assign to pin and nets and when s_rename_all_lowlevel frees
         * the strings each pointer must be unique, otherwise, only the
         * first encounter will is renamed as s_rename_all_lowlevel will
         * not find the other instances */
        name = geda_utility_string_strdup (net_name);

      }
      else if (strcmp(name, net_name) != 0) {

#if DEBUG
        fprintf(stderr, "Found a net with two names!\n");
        fprintf(stderr, "Net called: [%s] and [%s]\n", name, net_name);
#endif

        /* only rename if this net name has priority  AND, we are
         * using net= attributes as the netnames which have priority */
        if (naming_priority == NETATTRIB_ATTRIBUTE) {

#if DEBUG
          printf("\nNETATTRIB_ATTRIBUTE\n");
#endif
          if (n_current->net_name_has_priority) {

#if DEBUG
            fprintf(stderr, "Net is now called: [%s]\n", net_name);

            /* this show how to rename nets */
            printf("\nRENAME all nets: %s -> %s\n", name, net_name);
#endif
            s_rename_add(name, net_name);
            GEDA_FREE(name);
            name = geda_utility_string_strdup (net_name);

          }
          else {

#if DEBUG
            printf
            ("\nFound a net name called [%s], but it doesn't have priority\n", net_name);
#endif

            /* Do the rename anyways, this might cause problems */
            /* this will rename net which have the same label= */
            if (!s_rename_search (name, net_name, TRUE)) {

              if (!quiet_mode) {
                const char *msg = _("Found duplicate net name, renaming [%s] to [%s]\n");
                fprintf(stderr, msg, name, net_name);
              }
              s_rename_add(name, net_name);
              GEDA_FREE(name);
              name = geda_utility_string_strdup (net_name); /* See note above */
            }
          }

        }
        else {	/* NETNAME_ATTRIBUTE */

#if DEBUG
          printf("\nNETNAME_ATTRIBUTE\n");
#endif

          /* here we want to rename the net */
          /* that has priority to the label */
          /* name */
          if (n_current->net_name_has_priority) {

#if DEBUG /* this shows how to rename nets */
            printf("\nRENAME all nets: %s -> %s (priority)\n", net_name, name);
#endif

            s_rename_add(net_name, name);

          }
          else {

#if DEBUG /* this shows how to rename nets */
            printf ("\nRENAME all nets: %s -> %s (not priority)\n", name, net_name);
#endif
            /* do the rename anyways, this might cause problems */
            /* this will rename net which have the same label= */
            if (!s_rename_search (name, net_name, TRUE)) {
              if (!quiet_mode) {
                fprintf(stderr,
                      _("Found duplicate net name, renaming [%s] to [%s]\n"),
                        name, net_name);
              }
              s_rename_add(name, net_name);
              GEDA_FREE(name);
              name = geda_utility_string_strdup (net_name); /* See note above */
            }
          }

#if DEBUG
          fprintf(stderr, "Net is now called: [%s]\n", name);
#endif

        }
      }
    }

    n_current = n_current->next;
  }

  if (name) {
    return (name);
  }
  else {
    return (NULL);
  }
}

/*!
 * \brief Resolve Net Name
 * \par Function Description
 *  If the net names exist, as determined by s_net_name_search,
 *  the existing net-name is returned, otherwise the node of each
 *  pin in the pin list is interrogated using s_net_find and if a
 *  match is found, s_net_name_search is called again to check if
 *  a net name exist for the node of the pin.
 *
 *  If no net-name exist for the node, a unique unnamed netname
 *  is created and assigned to node. For hierarchical schematics,
 *  the unnamed net may include the hierarchy_tag depending on the
 *  setting of hierarchy-netname-mangle.
 *
 *  \param [in] pr_current    GedaToplevel toplevel structure;
 *  \param [in] netlist_head  Pointer to first netlist record structure
 *  \param [in] net_head      Pointer to first net record structure
 *  \param [in] hierarchy_tag hierarchy tag string
 *  \param [in] node_type     The type of node
 */
char *s_net_name (GedaToplevel *pr_current, NETLIST *netlist_head,
                  NET *net_head, char *hierarchy_tag, int node_type)
{

  NET      *n_start;
  NETLIST  *nl_current;
  CPINLIST *pl_current;

  char *net_name;
  char *string            = NULL;
  char *temp;
  char *unnamed_string;
  int  *unnamed_counter;
  int   found             = 0;

  net_name = s_net_name_search(pr_current, net_head);

  if (net_name) {
    return (net_name);
  }

#if DEBUG
  printf("didn't find named net\n");
#endif

  /* didn't find a name */
  /* go looking for another net which might have already been named */
  /* ie you don't want to create a new unnamed net if the net has */
  /* already been named */
  nl_current = netlist_head;
  while (nl_current != NULL) {
    if (nl_current->cpins) {
      pl_current = nl_current->cpins;
      while (pl_current != NULL) {
        if (pl_current->nets) {
          n_start = pl_current->nets;
          if (n_start->next && net_head->next) {
            found = s_net_find(n_start->next, net_head->next);
            if (found) {
              net_name = s_net_name_search(pr_current, n_start);
              if (net_name) {
                return (net_name);
              }
            }
          }
        }

        pl_current = pl_current->next;
      }
    }
    nl_current = nl_current->next;
  }


#if DEBUG
  printf("didn't find previously named\n");
#endif

  /* AND we don't want to assign a dangling pin */
  /* which is signified by having only a head node */
  /* which is just a place holder */
  /* and the head node shows up here */

  if (net_head->nid  == -1 &&
      net_head->prev == NULL &&
      net_head->next == NULL)
  {
    string = geda_sprintf("unconnected_pin-%d", unnamed_pin_counter++);

    return (string);

  }

  switch (node_type) {
    case PIN_NET_NODE:
      unnamed_counter = &unnamed_net_counter;
      unnamed_string = pr_current->unnamed_netname;
      break;

    case PIN_BUS_NODE:
      unnamed_counter = &unnamed_bus_counter;
      unnamed_string = pr_current->unnamed_busname;
      break;

    default:
      fprintf (stderr, "%s: unhandled case node type %i\n", __func__, node_type);
      return NULL;
  }

  /* have we exceeded the number of unnamed nets? */
  if (*unnamed_counter < MAX_UNNAMED_NETS) {

    if (netlist_mode == SPICE) {
      string = geda_sprintf("%d", (*unnamed_counter)++);
    }
    else {

      temp = geda_sprintf ("%s%d", unnamed_string, (*unnamed_counter)++);

      if (hierarchy_tag) {
        string = s_hierarchy_create_netname (pr_current, temp, hierarchy_tag);
        GEDA_FREE (temp);
      }
      else {
        string = temp;
      }
    }
  }
  else {
    fprintf(stderr, _("Increase number of unnamed nets (s_net.c)\n"));
    return NULL;
  }

  return string;

}
