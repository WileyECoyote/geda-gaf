/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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

#include <config.h>
#include <missing.h>

#include <gettext.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#include <geda_debug.h>

/*! Tracks which Objects have been visited so far, and how many times.
 *
 * The keys of the table are the Object pointers, and the visit count
 * is stored directly in the value pointers.
 */
static GHashTable *visit_table = NULL;

/*! Trivial function used when clearing#visit_table. */
static bool
returns_true (gpointer key, gpointer value, gpointer user_data)
{
  return TRUE;
}

/*! Retrieve the current visit count for a particular Object. */
static inline int
is_visited(Object *obj)
{
  gpointer val;
  gpointer orig_key;
  bool exist = g_hash_table_lookup_extended (visit_table,
                                                 obj,
                                                 &orig_key,
                                                 &val);
  return exist ? GPOINTER_TO_INT(val) : 0;
}

/*! Increment the current visit count for a particular Object. */
static inline int
visit(Object *obj)
{
  gpointer val = GINT_TO_POINTER(is_visited (obj) + 1);
  g_hash_table_replace (visit_table, obj, val);
  return GPOINTER_TO_INT (val);
}

/*! Reset all visit counts. Simply clears the hashtable completely. */
static inline void
s_traverse_clear_all_visited (const GList *obj_list)
{
  g_hash_table_foreach_remove (visit_table,
                               (GHRFunc) returns_true,
                               NULL);
}

void s_traverse_init(void)
{
  netlist_head = s_netlist_add(NULL);
  netlist_head->nlid = -1;	/* head node */

  graphical_netlist_head = s_netlist_add(NULL);
  graphical_netlist_head->nlid = -1;	/* head node */

  if (verbose_mode) {
    printf(_("\n\n------------------------------------------------------\n"));
    printf(_("Verbose mode legend\n\n"));
    printf(_("n : Found net\n"));
    printf(_("C : Found component (staring to traverse component)\n"));
    printf(_("p : Found pin (starting to traverse pin / or examining pin)\n"));
    printf(_("P : Found end pin connection (end of this net)\n"));
    printf(_("R : Starting to rename a net\n"));
    printf(_("v : Found source attribute, traversing down\n"));
    printf(_("^ : Finished underlying source, going back up\n"));
    printf(_("u : Found a refdes which needs to be demangle\n"));
    printf(_("U : Found a connected_to refdes which needs to be demangle\n"));
    printf(_("------------------------------------------------------\n\n"));

  }

  /* Initialise the hashtable which contains the visit
   *      count. N.b. no free functions are required. */
  visit_table = g_hash_table_new (g_direct_hash,
                                  g_direct_equal);
}

void s_traverse_start(GedaToplevel * pr_current)
{
  GList *iter;
  Page *p_current;

  for ( iter = geda_list_get_glist( pr_current->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (Page *)iter->data;

    /* only traverse pages which are toplevel, ie not underneath */
    if (p_current->page_control == 0) {
      pr_current->page_current = p_current;
      s_traverse_sheet (pr_current, s_page_get_objects (p_current), NULL);
    }
  }

  /* now that all the sheets have been read, go through and do the */
  /* post processing work */
  s_netlist_post_process(pr_current, netlist_head);

  /* Now match the graphical netlist with the net names already assigned */
  s_netlist_name_named_nets(pr_current, netlist_head,
                            graphical_netlist_head);

  if (verbose_mode) {
    printf(_("\nInternal netlist representation:\n\n"));
    s_netlist_print(netlist_head);
  }
}


void
s_traverse_sheet (GedaToplevel * pr_current, const GList *obj_list, char *hierarchy_tag)
{
  NETLIST *netlist;
  char    *net_name;
  char    *temp;
  char    *temp_uref;
  bool     is_graphical;
  bool     is_hierarchy;
  const GList *iter;
  GError      *err;
  EdaConfig   *cfg;

  is_graphical = FALSE;
  is_hierarchy = TRUE;
  err          = NULL;

  cfg = eda_config_get_context_for_file (NULL);
  is_hierarchy = eda_config_get_boolean (cfg, "gnetlist", "traverse-hierarchy", &err);
  if (err != NULL) {
    is_hierarchy = TRUE;
    g_clear_error (&err);
  }

  if (verbose_mode) {
    printf(_("- Starting internal netlist creation\n"));
  }

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    Object *o_current = iter->data;

    netlist = s_netlist_return_tail(netlist_head);

    if (o_current->type == OBJ_PLACEHOLDER) {
      printf(_("WARNING: Found a placeholder/missing component, is symbol file missing? [%s]\n"),
                o_current->complex->filename);
    }

    if (o_current->type == OBJ_COMPLEX) {

      is_graphical = FALSE;

#if DEBUG
      printf("starting NEW component\n\n");
#endif

      verbose_print(" C");

      /* look for special tag */
      temp = o_attrib_search_object_attribs_by_name (o_current, "graphical", 0);

      if (g_strcmp0 (temp, "1") == 0) {
        /* traverse graphical elements, but adding them to the
         *   graphical netlist */

        netlist = s_netlist_return_tail(graphical_netlist_head);
        is_graphical = TRUE;

      }
      GEDA_FREE(temp);

      netlist = s_netlist_add(netlist);
      netlist->nlid = o_current->sid;

      temp_uref = o_attrib_search_object_attribs_by_name (o_current, "refdes", 0);

      if (temp_uref) {
        if (stricmp(temp_uref,"none") == 0) {
          GEDA_FREE(temp_uref);
          temp_uref = NULL;
        }
      }

      if (temp_uref) {
        netlist->component_uref =
        s_hierarchy_create_uref(pr_current, temp_uref, hierarchy_tag);
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

        /* search of net attribute */
        /* maybe symbol is not a component */
        /* but a power / gnd symbol */
        temp = o_attrib_search_object_attribs_by_name (o_current, "net", 0);

        /* nope net attribute not found */
        if ( (!temp) && (!is_graphical) ) {
          net_name = o_attrib_search_object_attribs_by_name (o_current, "netname", 0);
          fprintf(stderr,
         _("Could not find refdes on component or any special attributes!<%s>, <%s>\n"),
            o_current->complex->filename, net_name);

          netlist->component_uref = geda_strdup("U?");
        } else {
#if DEBUG
          printf("yeah... found a power symbol\n");
#endif
          /* it's a power or some other special symbol */
          netlist->component_uref = NULL;
          GEDA_FREE(temp);
        }

      }

      netlist->cpins =
      s_traverse_component(pr_current, o_current, hierarchy_tag);

      /* here is where we deal with the net attribute */
      s_netattrib_handle(pr_current, o_current, netlist, hierarchy_tag);

      /* Conditionally traverse any underlying schematics */
      if (is_hierarchy) {
        s_hierarchy_traverse(pr_current, o_current, netlist);
      }
    }
  }

  verbose_done();
}

CPINLIST *s_traverse_component(GedaToplevel * pr_current, Object * component,
                               char *hierarchy_tag)
{
  CPINLIST *cpinlist_head = NULL;
  CPINLIST *cpins = NULL;
  NET      *nets_head = NULL;
  NET      *nets = NULL;
  GList    *iter;

  cpinlist_head = cpins = s_cpinlist_add(NULL);
  cpins->plid = -1;

  for (iter = component->complex->prim_objs; iter != NULL;
       iter = g_list_next (iter))
  {
    Object *o_current = iter->data;

    /* Ignore objects which aren't net pins */
    if (o_current->type != OBJ_PIN)
      continue;

    if (o_current->pin->node_type != PIN_NET_NODE)
      continue;

    /* add cpin node */
    cpins = s_cpinlist_add(cpins);
    cpins->plid = o_current->sid;
    cpins->node_type = o_current->pin->node_type;

    cpins->pin_number =
      o_attrib_search_object_attribs_by_name (o_current, "pinnumber", 0);

    cpins->pin_label =
      o_attrib_search_object_attribs_by_name (o_current, "pinlabel", 0);

    /* head nets node */
    /* is this really need */
    nets_head = nets = s_net_add(NULL);
    nets->nid = -1;

    /* This avoids us adding an unnamed net for an unconnected pin */
    if (o_current->conn_list != NULL) {

      /* result of s_traverse_net() is not used, explicitly cast function value (void) */
      s_traverse_net (pr_current, nets, TRUE,
                             o_current, hierarchy_tag, cpins->node_type);
      s_traverse_clear_all_visited (s_page_get_objects (pr_current->page_current));
    }

    cpins->nets = nets_head;
    /* s_net_print(nets); */
  }

  return (cpinlist_head);
}


static int connection_type (Object *object)
{
  switch (object->type) {
    case OBJ_PIN:  return object->pin->node_type;
    case OBJ_NET:  return PIN_NET_NODE;
    case OBJ_BUS:  return PIN_BUS_NODE;
    default:
      g_critical(_("Non-connectable object being queried for connection type\n"));
      return PIN_NET_NODE;
  }
}

NET *s_traverse_net (GedaToplevel *pr_current, NET *nets, int starting,
                     Object   *object, char *hierarchy_tag, int type)
{
  NET   *new_net;
  CONN  *c_current;
  GList *cl_current;
  char  *temp                   = NULL;
  const  char *netattrib_pinnum = NULL;

  visit (object);

  if (connection_type (object) != type)
    return nets;

  new_net = nets = s_net_add(nets);
  new_net->nid = object->sid;

  /* pins are not allowed to have the netname attribute attached to them */
  if (object->type != OBJ_PIN) {
    /* Ignore netname attributes on buses */
    if (object->type == OBJ_NET)
      temp = o_attrib_search_object_attribs_by_name (object, "netname", 0);

    if (temp) {
      new_net->net_name =
      s_hierarchy_create_netname(pr_current, temp,
                                 hierarchy_tag);
      GEDA_FREE(temp);
    } else if (object->type == OBJ_NET) {
      /* search for the old label= attribute on nets */
      temp = o_attrib_search_object_attribs_by_name (object, "label", 0);
      if (temp) {
        printf(_("WARNING: Found label=%s. label= is deprecated, please use netname=\n"), temp);
        new_net->net_name =
        s_hierarchy_create_netname(pr_current, temp,
                                   hierarchy_tag);
        GEDA_FREE(temp);
      }
    }
  }
#if DEBUG
  printf("inside traverse: %s, new_net->net_name=%s\n", object->name, new_net->net_name);
#endif

  if (object->type == OBJ_PIN) {

    verbose_print (starting ? "p" : "P");

    new_net->connected_to =
    s_net_return_connected_string (pr_current, object, hierarchy_tag);

    temp = o_attrib_search_object_attribs_by_name (object, "pinlabel", 0);

    if (temp) {
      new_net->pin_label = temp;
    }

    /* net= new */
    netattrib_pinnum = s_netattrib_connected_string_get_pinnum (nets->connected_to);
    if (netattrib_pinnum != NULL && type == PIN_NET_NODE) {

#if DEBUG
      printf("%s: going to find netname %s \n", __func__, nets->connected_to);
#endif
      nets->net_name =
      s_netattrib_return_netname (pr_current, object,
                                  nets->connected_to,
                                  hierarchy_tag);
      nets->net_name_has_priority = TRUE;
      GEDA_FREE(nets->connected_to);
      nets->connected_to = NULL;
    }
#if DEBUG
    printf("traverse connected_to: %s\n", new_net->connected_to);
#endif

    /* Terminate if we hit a pin which isn't the one we started with */
    if (!starting)
      return nets;
  }

  /*printf(_("Found net %s\n", object->name/0)); */
  verbose_print("n");

  /* this is not perfect yet and won't detect a loop... */
  if (is_visited(object) > 100) {
    fprintf(stderr, _("Found a possible net/pin infinite connection\n"));
    exit(-1);
  }

  cl_current = object->conn_list;
  while (cl_current != NULL) {

    c_current = (CONN *) cl_current->data;

    if (c_current->other_object != NULL) {
      if (!is_visited(c_current->other_object) &&
        c_current->other_object != object) {
        nets = s_traverse_net (pr_current, nets, FALSE,
                               c_current->other_object, hierarchy_tag, type);
        }

    }
    cl_current = g_list_next(cl_current);
  }

  return (nets);
}
