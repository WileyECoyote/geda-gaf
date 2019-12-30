/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_traverse.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2018 Ales Hvezda
 * Copyright (C) 1998-2018 gEDA Contributors (see ChangeLog for details)
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

//#define PERFORMANCE 1

#include "../../config.h"

#include <gnetlist.h>
#include <gettext.h>

#include <geda_debug.h>
#include <geda_diagnostics.h>

#if PERFORMANCE
#include <valgrind/callgrind.h>
#endif


/*!
 * Tracks which Objects have been visited so far, and how many times.
 * The keys of the table are the Object pointers, and the visit count
 * is stored directly in the value pointers.
 */
static GHashTable *visit_table;

/*!
 * \brief Clear Hash Table Callback
 * \par Function Description
 *  Trivial function used when clearing #visit_table.
 *  The function servers as a callback for g_hash_table_foreach_remove,
 *  g_hash_table_foreach_remove removes the entry when this callback
 *  returns TRUE.
 */
static bool returns_true (void * key, void * value, void * user_data)
{
  return TRUE;
}

/*!
 * \brief Get if Object has been Visited
 * \par Function Description
 *  Retrieve the current visit count for a particular Object, which would
 *  be 0 if \a obj is not in the hash table.
 */
static inline int is_visited(GedaObject *obj)
{
  void *val;
  void *orig_key;

  bool exist = g_hash_table_lookup_extended (visit_table,
                                             obj,
                                             &orig_key,
                                             &val);
  return exist ? (int)(long)(val) : 0;
}

/*!
 * \brief Increment the current visit count
 * \par Function Description
 *  Increment the current visit count for a particular Object.
 *
 *  \note g_hash_table_replace() will add the pointer to the table
 *        if the \a object is not found in the table.
 */
static inline int visit(GedaObject *obj)
{
  void *val = (void*)(long)(is_visited (obj) + 1);
  g_hash_table_replace (visit_table, obj, val);
  return (int)(long) (val);
}

/*!
 * \brief Clear Visit Hash Table
 * \par Function Description
 *  Reset all visit counts. Simply clears the hashtable completely.
 */
static inline void
s_traverse_clear_all_visited (const GList *obj_list)
{
  g_hash_table_foreach_remove (visit_table,
                               (GHRFunc) returns_true,
                               NULL);
}

/*!
 * \brief Initialize Traverse Module
 * \par Function Description
 *  Initializes netlist_head list with two initial NETLIST record
 *  structures, one for the netlist and one for graphical netlist.
 *  Awkwardly displays a legend if Verbose mode.
 */
static void s_traverse_init(void)
{
  /* NULL indicates no previous record in new NETLIST */
  netlist_head = s_netlist_add(NULL);
  netlist_head->nlid = -1;	/* head node */

  /* NULL indicates no previous record in new NETLIST */
  graphical_netlist_head = s_netlist_add(NULL);
  graphical_netlist_head->nlid = -1;	/* head node */

  if (verbose_mode) {
    printf  ( "\n\n------------------------------------------------------\n");
    printf(_("Verbose mode legend\n\n"));
    printf(_("n : Found net\n"));
    printf(_("C : Found component (starting to traverse component)\n"));
    printf(_("p : Found pin (starting to traverse or examine pin)\n"));
    printf(_("P : Found ending pin connection (end of this net)\n"));
    printf(_("R : Starting to rename a net\n"));
    printf(_("v : Found source attribute, traversing down\n"));
    printf(_("^ : Finished underlying source, going back up\n"));
    printf(_("u : Found a refdes which needs to be demangle\n"));
    printf(_("U : Found a connected_to refdes which needs to be demangle\n"));
    printf  ("------------------------------------------------------\n\n");
  }
}

/*!
 * \brief Traverse Netlist
 * \par Function Description
 *  Loops through open documents and assimilate a netlist and
 *  a second list of graphical entities. Prints the net-list
 *  when verbose mode.
 */
void s_traverse_process(GedaToplevel *pr_current)
{
  GList *iter;
  Page  *p_current;

  s_traverse_init();

  /* Initialize the hashtable which contains the visit
   *      count. N.b. no free functions are required. */
  visit_table = g_hash_table_new (g_direct_hash, g_direct_equal);

  for (iter  = geda_list_get_glist (pr_current->pages);
       iter != NULL;
       iter  = g_list_next (iter)) {

    p_current = (Page *)iter->data;

    /* Only traverse pages which are toplevel, ie not underneath */
    if (p_current->page_control == 0) {
      pr_current->page_current = p_current;
      s_traverse_sheet (pr_current, geda_struct_page_get_objects (p_current));
    }
  }

  /* All sheets have been read, do post processing work */
  s_netlist_post_process (pr_current, netlist_head);

  /* Match the graphical netlist with net names already assigned */
  s_netlist_name_named_nets (pr_current, netlist_head, graphical_netlist_head);

  if (verbose_mode) {
    printf ("\n%s:\n\n", _("Internal netlist representation:"));
    s_netlist_print (netlist_head);
  }

  g_hash_table_destroy(visit_table);
  visit_table = NULL;
}

/*!
 * \brief Traverse Toplevel Sheet
 * \par Function Description
 *  Called from s_traverse_process to traverse top-level sheets.
 *  If "traverse-hierarchy" is enabled s_hierarchy_traverse is
 *  called for each complex encountered to potentially traverse
 *  underlying schematics.
 *
 * \param [in] pr_current  Current GedaToplevel structure; toplevel,
 * \param [in] obj_list    List of all object on This sheet
 */
void s_traverse_sheet (GedaToplevel *pr_current, const GList *obj_list)
{

#if PERFORMANCE
  printf("%s processing <%s>\n",__func__, pr_current->page_current->filename);
  START_GEDA_PERFORMANCE
#endif

  const GList *iter;
  char        *net_name;
  char        *value;
  char        *temp_uref;

  if (verbose_mode) {
    printf("- %s\n", _("Starting internal netlist creation"));
  }

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current;
    NETLIST    *netlist;

    o_current = iter->data;
    netlist   = s_netlist_return_tail (netlist_head);

    if (o_current->type == OBJ_PLACEHOLDER) {
      const char *msg = _("WARNING: Found a placeholder/missing component, is symbol file missing");
      const char *fn  = geda_complex_get_filename(o_current->complex);
      printf("%s \"%s\"?\n", msg, fn);
    }

    if (o_current->type == OBJ_COMPLEX) {

      bool is_graphical = FALSE;

#if DEBUG
      printf("starting NEW component\n\n");
#endif

      verbose_print(" C");

      /* look for special graphical tag */
      value = geda_attrib_search_object_by_name (o_current, "graphical", 0);

      if (value) {

        if (g_strcmp0 (value, "1") == 0) {

          /* traverse graphical elements, and add to the graphical netlist */
          netlist      = s_netlist_return_tail (graphical_netlist_head);
          is_graphical = TRUE;
        }
        GEDA_FREE(value);
      }

      netlist = s_netlist_add (netlist);
      netlist->nlid = o_current->sid;

      temp_uref = geda_attrib_search_object_by_name (o_current, "refdes", 0);

      if (temp_uref) {
        if (geda_utility_string_stricmp(temp_uref,"none") == 0) {
          GEDA_FREE(temp_uref);          /* Release and set to NULL */
        }
      }

      if (temp_uref) {
        netlist->component_uref =
        s_hierarchy_create_uref (pr_current, temp_uref, NULL);
        GEDA_FREE(temp_uref);
      }
      else {
        netlist->component_uref = NULL;
      }

      netlist->object_ptr = o_current;

      if (!netlist->component_uref) {

        /* search object for net attribute, noting that symbol may
         * not be a normal component but a power or gnd symbol */
        value = geda_attrib_search_object_by_name (o_current, "net", 0);

        /* nope net attribute not found */
        if ((!value) && (!is_graphical)) {

          const char *filename;
          const char *msg;

          msg = _("Did not find refdes or any special attributes on component!");

          filename = geda_complex_object_get_filename (o_current);
          net_name = geda_attrib_search_object_by_name (o_current, "netname", 0);

          fprintf(stderr, "%s <%s>, <%s>\n", msg, filename, net_name);

          netlist->component_uref = geda_strdup("U?");
        }
        else {

#if DEBUG
          printf("found a power symbol <%s>\n", value);
#endif
          /* it's a power or some other special symbol */
          GEDA_FREE(value);
        }
      }

      /* Retrieve and store list of connected pins */
      netlist->cpins = s_traverse_component (pr_current, o_current, NULL);

      /* Deal with the net attribute */
      s_netattrib_handle (pr_current, o_current, netlist, NULL);

      /* Conditionally traverse any underlying schematics */
      if (pr_current->hierarchy_traversal) {
        s_hierarchy_traverse (pr_current, o_current, netlist);
      }
    }
  }

  verbose_done();

  STOP_GEDA_PERFORMANCE;
}

/*!
 * \brief Traverse Hierarchy Sheet
 * \par Function Description
 *  Called from s_hierarchy_traverse, possibly recursively. hierarchy_tag
 *  is the refdes of the source symbols, which can be combined using the
 *  hierarchy-uref-separator, such as "S1/S2", for nested hierarchies.
 *  This function is not called when net-listing flat schematics.
 *
 *  All objects on the sheet with a "graphical" attribute are appended
 *  to graphical_netlist_head, components with refdes value "none" are
 *  skipped, connected pins are stored to the netlist->cpins for every
 *  non-graphical component with a refdes value other than "none" and
 *  the component is appended netlist->component_uref along with the
 *  hierarchy_tag, which is either perpended or appended depending on
 *  the setting of hierarchy-uref-order.
 *
 *  Additionally, s_hierarchy_traverse is called to traverse any components
 *  with a "source" attribute, which could result in this function being
 *  called recursively.
 *
 *  \param [in] pr_current Current GedaToplevel structure; toplevel,
 *  \param [in] netlist    List of all object on This sheet
 */
void s_traverse_hierarchy_sheet (GedaToplevel *pr_current, NETLIST *netlist)
{
  const GList *iter;
  char        *net_name;
  char        *value;
  char        *temp_uref;
  bool         is_graphical;

  const GList *obj_list = geda_struct_page_get_objects (pr_current->page_current);
  char *hierarchy_tag = netlist->hierarchy_tag;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {

   GedaObject *o_current = iter->data;

    netlist = s_netlist_return_tail (netlist_head);

    if (o_current->type == OBJ_PLACEHOLDER) {

      const char *filename;

      filename = geda_complex_object_get_filename (o_current);

      printf("%s \"%s\"?\n",
             _("WARNING: Found a placeholder/missing component, is symbol file missing"),
             filename);
    }

    if (o_current->type == OBJ_COMPLEX) {

      is_graphical = FALSE;

#if DEBUG
      printf("starting NEW component\n\n");
#endif

      verbose_print(" C");

      /* look for special graphical tag */
      value = geda_attrib_search_object_by_name (o_current, "graphical", 0);

      if (value) {

        if (g_strcmp0 (value, "1") == 0) {

          /* traverse graphical elements, and add to the graphical netlist */
          netlist = s_netlist_return_tail (graphical_netlist_head);
          is_graphical = TRUE;
        }
        GEDA_FREE(value);
      }

      netlist       = s_netlist_add (netlist);
      netlist->nlid = o_current->sid;

      temp_uref = geda_attrib_search_object_by_name (o_current, "refdes", 0);

      if (temp_uref) {
        if (!geda_utility_string_stricmp(temp_uref,"none")) {
          GEDA_FREE(temp_uref);          /* Release and set to NULL */
        }
      }

      if (temp_uref) {
        netlist->component_uref =
        s_hierarchy_create_uref (pr_current, temp_uref, hierarchy_tag);
        GEDA_FREE(temp_uref);
      }
      else {

        if (hierarchy_tag) {
          netlist->component_uref = geda_strdup (hierarchy_tag);
        }
        else {
          netlist->component_uref = NULL;
        }
      }

      if (hierarchy_tag) {
        netlist->hierarchy_tag = geda_strdup (hierarchy_tag);
      }

      netlist->object_ptr = o_current;

      if (!netlist->component_uref) {

        /* search object for net attribute, noting that symbol may
         * not be a normal component but a power or gnd symbol */
        value = geda_attrib_search_object_by_name (o_current, "net", 0);

        /* nope net attribute not found */
        if ((!value) && (!is_graphical)) {

          const char *filename;

          filename = geda_complex_object_get_filename (o_current);
          net_name = geda_attrib_search_object_by_name (o_current, "netname", 0);

          fprintf(stderr, "%s <%s>, <%s>\n",
                _("Could not find refdes on component or any special attributes!"),
                  filename, net_name);

          netlist->component_uref = geda_strdup("U?");
        }
        else {

#if DEBUG
          printf("yeah... found a power symbol\n");
#endif
          /* it's a power or some other special symbol */
          netlist->component_uref = NULL;
          GEDA_FREE(value);
        }
      }

      /* Retrieve and store list of connected pins */
      netlist->cpins =
        s_traverse_component (pr_current, o_current, hierarchy_tag);

      /* here is where we deal with the net attribute */
      s_netattrib_handle (pr_current, o_current, netlist, hierarchy_tag);

      /* Conditionally traverse any underlying schematics */
      if (pr_current->hierarchy_traversal) {
        s_hierarchy_traverse (pr_current, o_current, netlist);
      }
    }
  }

  verbose_done();
}

/*!
 * \brief Traverse Component looking for Connected Pins
 * \par Function Description
 *  Compliles a double linked list of st_cpinlist structures,
 *  with one st_cpinlist allocation for each connected pin.
 *  The returned record will be empty if the \a component has
 *  no connected pins.
 *
 * \returns a pointer to st_cpinlist structure
 */
CPINLIST *s_traverse_component(GedaToplevel *pr_current,
                               GedaObject   *component,
                               char         *hierarchy_tag)
{
  CPINLIST *cpinlist_head = NULL;
  CPINLIST *cpins         = NULL;
  NET      *nets_head     = NULL;
  NET      *nets          = NULL;
  GList    *iter;

  cpinlist_head = cpins = s_cpinlist_add(NULL);

  if (cpins) {
    cpins->plid = -1;
  }

  for (iter  = component->complex->prim_objs;
       iter != NULL;
       iter  = g_list_next (iter))
  {
   GedaObject *o_current = iter->data;

    /* Ignore objects which are not net pins */
    if (o_current->type != OBJ_PIN)
      continue;

    /* and apparently we do not handle bus type pins */
    if (o_current->pin->node_type != PIN_NET_NODE)
      continue;

    /* add cpin node */
    cpins            = s_cpinlist_add(cpins);
    cpins->plid      = o_current->sid;
    cpins->node_type = o_current->pin->node_type;

    cpins->pin_number =
      geda_attrib_search_object_by_name (o_current, "pinnumber", 0);

    cpins->pin_label =
      geda_attrib_search_object_by_name (o_current, "pinlabel", 0);

    /* head nets node, is this really needed? */
    nets_head = nets = s_net_add(NULL);
    nets->nid = -1;

    /* This avoids us adding an unnamed net for an unconnected pin */
    if (geda_object_get_conn_list(o_current)) {

      Page *pcurrent;

  //CALLGRIND_START_INSTRUMENTATION;

      /* result of s_traverse_net() is not used, implicitly cast function value (void) */
      s_traverse_net (pr_current, nets, TRUE, o_current, hierarchy_tag, cpins->node_type);

  //CALLGRIND_STOP_INSTRUMENTATION;

  //CALLGRIND_DUMP_STATS;

      pcurrent = geda_toplevel_get_current_page (pr_current);

      s_traverse_clear_all_visited (geda_struct_page_get_objects (pcurrent));
    }

    cpins->nets = nets_head;
    /* s_net_print(nets); */
  }

  return (cpinlist_head);
}

/*!
 * \brief Get the Connection Type based on the Object Type
 * \par Function Description
 *  Helper for s_traverse_net to return the type of node
 *  associated with \a object.
 */
static int connection_type (GedaObject *object)
{
  switch (geda_get_object_type(object)) {
    case OBJ_PIN:  return object->pin->node_type;
    case OBJ_NET:  return PIN_NET_NODE;
    case OBJ_BUS:  return PIN_BUS_NODE;
    default:
      g_critical(_("Non-connectable object being queried for connection type\n"));
      return PIN_NET_NODE;
  }
}

/*!
 * \brief Traverse Nets
 * \par Function Description
 *  Creates a new NET record in \a nets.
 *
 * \param [in] pr_current     Current GedaToplevel structure; toplevel,
 * \param [in] nets           PIN netlist to which the new records are appended
 * \param [in] starting       True if toplevel, otherwise in recursion
 * \param [in] object         Object whos pins are to be interogated
 * \param [in] hierarchy_tag  The hierarchy (source) prefix or suffix
 * \param [in] type           The PIN_NODE type of interest
 */
NET *s_traverse_net (GedaToplevel *pr_current, NET *nets, int starting,
                     GedaObject *object, char *hierarchy_tag, int type)
{
        NET   *new_net;
        CONN  *c_current;
  const GList *cl_current;
        int    o_type;

  /* Add object to the visited hash table */
  visit (object);

  if (connection_type (object) != type)
    return nets;

  new_net = nets = s_net_add(nets);

  new_net->nid = object->sid;

  o_type = geda_get_object_type(object);

  /* pins are not allowed to have the netname attribute attached to them */
  if (o_type != OBJ_PIN) {

    char *temp = NULL;

    /* Ignore netname attributes on buses */
    if (o_type == OBJ_NET) {
      temp = geda_attrib_search_object_by_name (object, "netname", 0);
    }

    if (temp) { /* If a net WITH a "netname" attribute */

      new_net->net_name = s_hierarchy_create_netname(pr_current, temp,
                                                     hierarchy_tag);
      GEDA_FREE(temp);
    }
    else if (o_type == OBJ_NET) { /* If net WITHOUT a "netname" */

      /* search for the old label= attribute on nets */
      temp = geda_attrib_search_object_by_name (object, "label", 0);

      if (temp) { /* Accept the attribute here but issue notice */

        const char *msg1 = _("WARNING: Found label");
        const char *msg2 = _("label= is deprecated, please use netname");

        printf("%s=%s. %s=\n", msg1, temp, msg2);

        new_net->net_name = s_hierarchy_create_netname (pr_current, temp,
                                                        hierarchy_tag);
        GEDA_FREE(temp);
      }
    }

#if DEBUG
  printf("inside traverse: %s, new_net->net_name=%s\n", object->name, new_net->net_name);
#endif

  }
  else { /* Is a Pin object */

    const char *netattrib_pinnum;
          char *temp;

    verbose_print (starting ? "p" : "P");

    new_net->connected_to =
    s_net_return_connected_string (pr_current, object, hierarchy_tag);

    temp = geda_attrib_search_object_by_name (object, "pinlabel", 0);

    if (temp) {
      new_net->pin_label = temp;
    }

    /* net= new */
    netattrib_pinnum = s_netattrib_connected_string_get_pinnum (nets->connected_to);

    if (netattrib_pinnum != NULL && type == PIN_NET_NODE) {

#if DEBUG
      printf("%s: going to find netname %s \n", __func__, nets->connected_to);
#endif

      nets->net_name = s_netattrib_return_netname (pr_current, object,
                                                   nets->connected_to,
                                                   hierarchy_tag);
      nets->net_name_has_priority = TRUE;

      GEDA_FREE(nets->connected_to);     /* Release and set to NULL */
    }

#if DEBUG
    printf("traverse connected_to: %s, ", new_net->connected_to);
    printf("<%p>\n", new_net->connected_to);
#endif

    /* Terminate if we hit a pin which isn't the one we started with */
    if (!starting) {
      return nets;
    }
  }

  /* printf(_("Found net %s\n", object->name/0)); */
  verbose_print("n");

  /* This is not perfect yet and won't detect a loop... */
  if (is_visited(object) > 100) {
    fprintf(stderr, _("Found a possible net/pin infinite connection\n"));
    exit(-1);
  }

  cl_current = geda_object_get_conn_list(object);

  while (cl_current != NULL) {

   GedaObject *next_object;

    c_current = (CONN*) cl_current->data;

    next_object = c_current->other_object;

    if (next_object != NULL) {

      if (!is_visited(next_object) && next_object != object) {

        /* Recursively call ourself */
        nets = s_traverse_net (pr_current, nets, FALSE,
                               next_object, hierarchy_tag, type);
      }
    }
    cl_current = g_list_next(cl_current);
  }

  return (nets);
}
