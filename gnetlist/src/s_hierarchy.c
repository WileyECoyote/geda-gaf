/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_hierarchy.c
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
/*! \file s_hierarchy.c
 *  \remarks
 *  This module only pertains to hierarchical schematics.
 */

#include "../../config.h"

#include <gnetlist.h>
#include <gettext.h>
#include <geda_debug.h>

/*!
 * \brief Traverse Hierarchy
 * \par Function Description
 *  Called by s_traverse_sheet or s_traverse_hierarchy_sheet if
 *  hierarchy-traversal is enabled to check \a o_current for any
 *  source= attributes, there can be more then one, and if found
 *  geda_struct_hierarchy_down_single is called to get the page
 *  object for the sheet/page. s_traverse_hierarchy_sheet is then
 *  called to traverse page.
 */
void s_hierarchy_traverse(GedaToplevel *pr_current,
                          GedaObject   *o_current,
                          NETLIST      *netlist)
{
  char *attrib;
  int   page_control   =-1;
  int   count          = 0;
  int   looking_inside = FALSE;
  int   loaded_flag    = FALSE;

  Page *p_current;
  Page *child_page;

  attrib = geda_attrib_search_attached_by_name (o_current, "source", 0);

  /* If above is null, then look inside symbol */
  if (attrib == NULL) {

    attrib = geda_attrib_search_inherited_by_name (o_current, "source", count);
    looking_inside = TRUE;

#if DEBUG
    printf("going to look inside now\n");
#endif
  }

  if (s_hierarchy_graphical_search(o_current, count)) {

    /* Do not bother traversing the hierarchy if symbol has a */
    /* graphical attribute attached to it. WEH: Is this really
     * the right thing to do? */
    GEDA_FREE(attrib);  /* Release memory and set attrib = NULL */
  }

  while (attrib) {

    /* Look for source=filename,filename, ... */
    int   pcount = 0;
    char *current_filename = geda_utility_string_split(attrib, ',', pcount);

    /* Loop over all filenames */
    while (current_filename != NULL) {

      geda_log("%s \"%s\"\n", _("Commence traversing source"), current_filename);

      /* Guts here for a single filename */
      p_current = pr_current->page_current;

#if DEBUG
      printf("Going down %s\n", current_filename);
#endif

      GError *err = NULL;

      child_page = geda_struct_hierarchy_down_single(pr_current,
                                                     current_filename,
                                                     pr_current->page_current,
                                                     page_control,
                                                     HIERARCHY_FORCE_LOAD,
                                                     &err);

      if (child_page == NULL) {
        fprintf(stderr, _("ERROR: Failed to load subcircuit '%s': %s\n"),
                current_filename, err->message);
        g_error_free (err);
        exit (2);

      }
      else {

        page_control = child_page->page_control;

        geda_struct_page_goto (child_page);

        loaded_flag = TRUE;

        verbose_print("v\n");
        verbose_reset_index();  /* sent CR/LF so reset to zero */

        netlist->composite_component = TRUE;

        netlist->hierarchy_tag = geda_strdup (netlist->component_uref);

        s_traverse_hierarchy_sheet (pr_current, netlist);

        verbose_print("^");
      }

      pr_current->page_current = p_current;

      GEDA_FREE(current_filename);
      pcount++;
      current_filename = geda_utility_string_split(attrib, ',', pcount);
    }

    GEDA_FREE(attrib);

    GEDA_FREE(current_filename);

    count++;

    /* Continue looking outside first */
    if (!looking_inside) {
      attrib = geda_attrib_search_attached_by_name (o_current, "source", count);
    }

    /* Did not find anything outside, so now look inside symbol */
    if (!looking_inside && attrib == NULL && !loaded_flag) {

      looking_inside = TRUE;

#if DEBUG
      printf("switching to go to look inside\n");
#endif

    }

    if (looking_inside) {

#if DEBUG
      printf("looking inside\n");
#endif

      attrib = geda_attrib_search_inherited_by_name (o_current, "source", count);
    }

    if (s_hierarchy_graphical_search(o_current, count)) {

      /* Do not bother looking further in the hierarchy if the symbol */
      /* has an graphical attribute attached to it. */
      if (attrib) {
        GEDA_FREE(attrib);
      }
    }
  }
}

/*!
 * \brief Remove Unreferenced connections in Hierarchical net list
 * \par Function Description
 *  Removes stuff from netlist connected to \a uref_disable.
 */
GList *s_hierarchy_remove_urefconn(NETLIST *head, char *uref_disable)
{
  NETLIST  *nl_current;
  CPINLIST *pl_current;
  NET      *n_current;
  GList    *removed;

  nl_current = head;
  removed    = NULL;

  while (nl_current != NULL) {

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {

      n_current = pl_current->nets;

      while (n_current != NULL) {

        if (n_current->connected_to != NULL) {

          char uref[80];

          /* Copy the old "uref pin" pair to buffer */
          strcpy(&uref[0], n_current->connected_to);

          int i = 0;

          /* Change the space to a null, do not the pin */
          while (uref[i]){
            if (uref[i] == ' ') {
              uref[i] = '\0';
              break;
            }
            i++;
          }

#if DEBUG
          printf("  looking at : %s %d\n", uref, /*pin*/ uref[i+1]);
#endif

          if (strcmp(uref_disable, uref) == 0) {

#if DEBUG
            printf("conn disabling %s\n", n_current->connected_to);
#endif

            GEDA_FREE(n_current->connected_to);
          }
        }
        n_current = n_current->next;
      }

      pl_current = pl_current->next;
    }

    if (nl_current->component_uref) {

      if (strcmp(nl_current->component_uref, uref_disable) == 0) {

#if DEBUG
        printf("refdes disabling, %s\n", nl_current->component_uref);
        printf(" %p\n", nl_current->component_uref);
#endif

        /* Save pointers to free later */
        if (!g_list_find(removed, nl_current->component_uref)) {
          removed = g_list_prepend(removed, nl_current->component_uref);
        }

        /* can't do frees, since some names are links */
        nl_current->component_uref = NULL;
      }
    }
    nl_current = nl_current->next;
  }

  return g_list_reverse(removed);
}

/*!
 * \brief Create a refdes relative to Hierarchy
 * \par Function Description
 *  Creates a string composed of \a hierarchy_tag and \a basename
 *  separated by the separator found in the toplevel variable
 *  hierarchy_uref_separator, with the order determined by the
 *  toplevel variable hierarchy_uref_order, provided both arguments
 *  are supplied, if \a hierarchy_tag is NULL a copy of \a basename
 *  is returned, if \a basename is NULL, then NULL is returned.
 *
 * \param [in] pr_current    GedaToplevel toplevel structure;
 * \param [in] basename      Is the value of refdes=,
 * \param [in] hierarchy_tag netlist->hierarchy_tag or NULL.
 *
 * \return Character string hierarchy reference.
 *
 * \note Caller should release the returned character string.
 *
 * \sa g_rc_hierarchy_uref_separator g_rc_hierarchy_uref_mangle i_vars_set
 */
char *s_hierarchy_create_uref(GedaToplevel *pr_current, char *basename,
                              char *hierarchy_tag)
{
  char *return_value = NULL;

  if (hierarchy_tag) {

    if (basename) {

      if (pr_current->hierarchy_uref_separator) {

        switch (pr_current->hierarchy_uref_order) {
          case (APPEND):
            return_value = geda_strconcat (hierarchy_tag,
                                           pr_current->hierarchy_uref_separator,
                                           basename, NULL);
            break;
          case (PREPEND):
            return_value = geda_strconcat (basename,
                                           pr_current->hierarchy_uref_separator,
                                           hierarchy_tag, NULL);

            break;
        }
      }
      else {
        switch (pr_current->hierarchy_uref_order) {
          case (APPEND):
            return_value = geda_strconcat (hierarchy_tag, basename, NULL);
            break;
          case (PREPEND):
            return_value = geda_strconcat (basename, hierarchy_tag, NULL);
            break;

        }
      }
    }
    else {
      return_value = NULL;
    }
  }
  else {
    if (basename) {
      return_value = geda_strdup (basename);
    }
    else {
      return_value = NULL;
    }
  }

  return (return_value);
}

/*!
 * \brief Hierarchy Setup Rename Nets
 * \par Function Description
 *  Searches \a head for \a uref after possibly hierarchical prefixing
 *  by s_hierarchy_create_uref and if the resulting composite is found
 *  s_rename_add is called to create a rename structure to replace what
 *  ever net_name is in the PIN list with \a new_name. All instances of
 *  component_uref are removed from the NET list and uniquely added to
 *  the \a removed list.
 *
 * \param [in]  pr_current GedaToplevel needed by s_hierarchy_create_uref
 * \param [in]  head       The Netlist
 * \param [in]  uref       To look for in the NET list
 * \param [in]  label      pinlabel= needed by s_hierarchy_create_uref
 * \param [in]  new_name   The net_name to store in the PIN list
 * \param [out] removed    A list to be appended with any pointer removed.
 */
static int
s_hierarchy_setup_rename(GedaToplevel *pr_current, NETLIST *head, char *uref,
                         char *label, char *new_name, GedaList *removed)
{
  NETLIST  *nl_current;
  CPINLIST *pl_current;
  char     *wanted_uref = NULL;
  bool      success     = FALSE;

  /* this is questionable, because I'm not sure if it's exactly the */
  /* same as the #if 0'ed out line */
  /* search for the uref which has the name: label/uref (or whatever the */
  /* hierarchy tag/separator order is) */
  wanted_uref = s_hierarchy_create_uref(pr_current, label, uref);

#if DEBUG
  printf("label: %s, uref: %s, wanted_uref: %s\n", label, uref,
         wanted_uref);
#endif

  nl_current = head;
  while (nl_current != NULL) {

    if (nl_current->component_uref) {

      if (strcmp(nl_current->component_uref, wanted_uref) == 0) {

        if (nl_current->cpins) {

          GList *tmp_list;
          char  *component_uref;

          /* skip over head of special io symbol */
          pl_current = nl_current->cpins->next;

#if DEBUG
          printf("net to be renamed: %s\n",
          pl_current->net_name);
          printf("%s -> %s\n", pl_current->net_name, new_name);
#endif

          s_rename_add(pl_current->net_name, new_name);

          component_uref = nl_current->component_uref;

#if DEBUG
          printf("Going to remove, %s,%p\n", component_uref, component_uref);
#endif

          tmp_list = s_hierarchy_remove_urefconn(head, component_uref);

          if (tmp_list) { /* Add list to master list and free list */
             geda_list_add_glist_unique(removed, tmp_list);
             g_list_free (tmp_list);
          }

          success = TRUE;
        }
      }
    }
    nl_current = nl_current->next;
  }

  GEDA_FREE(wanted_uref);

  return (success); /* Maybe should be geda_list_length(removed); */
}

/*!
 * \brief Hierarchy Post Process
 * \par Function Description
 *  Called by s_netlist_post_process to resolve hierarchy. Descends into
 *  nets associated with composited sheets and searches for pin labels
 *  with matching tags and utilizes s_hierarchy_setup_rename so that the
 *  net associated with the pin will be renamed with the source net name.
 *  After re-renaming, s_hierarchy_remove_compsite_all removes the child
 *  nets from the net list.
 */
void s_hierarchy_post_process(GedaToplevel *pr_current, NETLIST *head)
{
  NETLIST  *nl_current;
  CPINLIST *pl_current;
  GedaList *removed;

  s_rename_next_set();

  nl_current = head;

  removed = geda_list_new();

  while (nl_current != NULL) {

    if (nl_current->composite_component) {

#if DEBUG
      printf("Found composite %s\n", nl_current->component_uref);
#endif

      if (nl_current->cpins) {

        pl_current = nl_current->cpins;

        while (pl_current != NULL) {

          if (pl_current->plid != -1) {
            verbose_print("p");
          }

          if (pl_current->pin_label == NULL && pl_current->plid != -1) {

            const char *msg1 = "Found a pin";
            const char *msg2 = "on component";
            const char *msg3 = "without a label";

            fprintf(stderr, "%s [%s] %s [%s] %s!\n",     msg1,
                             nl_current->component_uref, msg2,
                             pl_current->pin_number,     msg3);
            }
            else if (pl_current->plid != -1) {

              bool  result;
              char *source_net_name;

#if DEBUG
              printf("# L: %s %s\n", pl_current->pin_number,
              pl_current->pin_label);
#endif

              /* get source net name, all nets are named already */
              source_net_name = s_net_name_search(pr_current, pl_current->nets);

#if DEBUG

              printf("name: %s\n", source_net_name);
              printf("Searching for: %s/%s\n", nl_current->component_uref,
                                               pl_current->pin_label);
#endif
              result = s_hierarchy_setup_rename(pr_current, head,
                                                nl_current->component_uref,
                                                pl_current->pin_label,
                                                source_net_name,
                                                removed);

              GEDA_FREE(source_net_name); /* s_rename_add made a copy */

              if (!result && !quiet_mode) {

                const char *msg1 = "Missing I/O symbol with refdes";
                const char *msg2 = "inside schematic for symbol";

                fprintf(stderr, "%s [%s] %s [%s]\n", msg1,
                                pl_current->pin_label, msg2,
                                nl_current->component_uref);
              }
            }
            pl_current = pl_current->next;
        }
      }
    }
    nl_current = nl_current->next;
  }

  s_rename_all(pr_current, head);

  s_hierarchy_remove_compsite_all(head, removed);

  geda_list_free_full(removed);

  geda_list_unref(removed);
}

/*!
 * \brief Hierarchy Remove All Composited
 * \par Function Description
 *  Remove references of composited components. These are the reference
 *  to the source symbols.
 */
void s_hierarchy_remove_compsite_all(NETLIST *head, GedaList *removed)
{
  NETLIST *nl_current;

  nl_current = head;

  while (nl_current != NULL) {

    if (nl_current->composite_component) {

      if (nl_current->component_uref != NULL) {

        GList *tmp_list;
        char  *u_ref = nl_current->component_uref;

#if DEBUG
        printf("Going to remove, %s, %p\n", u_ref, u_ref);
#endif

        tmp_list = s_hierarchy_remove_urefconn(head, u_ref);

        if (tmp_list) { /* Add list to master list and free list */

          geda_list_add_glist_unique(removed, tmp_list);
          g_list_free (tmp_list);
        }
      }
    }
    nl_current = nl_current->next;
  }
}

/*!
 * \brief Hierarchy Create Net Name
 * \par Function Description
 *  When netname-mangle is enabled this function prefixes or suffixes
 *  the net names with the reference of the source symbol given by \a
 *  hierarchy_tag, otherwise a copy of \a basename is returned with
 *  out modification.
 */
char *s_hierarchy_create_netname(GedaToplevel *pr_current,
                                         char *basename,
                                         char *hierarchy_tag)
{
  char *return_value = NULL;

  if (pr_current->hierarchy_netname_mangle == FALSE) {

    if (basename) {
      return (geda_strdup (basename));
    }
    else {
      return (NULL);
    }
  }

  if (hierarchy_tag) {

    if (basename) {

      if (pr_current->hierarchy_netname_separator) {
        switch (pr_current->hierarchy_netname_order) {
          case (APPEND):
            return_value =
            geda_strconcat (hierarchy_tag,
                             pr_current->hierarchy_netname_separator,
                             basename, NULL);
            break;

          case (PREPEND):
            return_value =
            geda_strconcat (basename,
                             pr_current->hierarchy_netname_separator,
                             hierarchy_tag, NULL);
            break;
        }
      }
      else {
        switch (pr_current->hierarchy_netname_order) {
          case (APPEND):
            return_value = geda_strconcat (hierarchy_tag, basename, NULL);
            break;

          case (PREPEND):
            return_value = geda_strconcat (basename, hierarchy_tag, NULL);
            break;
        }

      }
    }
    else {
      return_value = NULL;
    }
  }
  else {
    if (basename) {
      return_value = geda_strdup (basename);
    }
    else {
      return_value = NULL;
    }
  }

  return (return_value);
}

/*!
 * \brief Hierarchy Create a Net Attribute
 * \par Function Description
 *  When netattrib-mangle is enabled this function prefixes or suffixes
 *  the net attributes with the reference of the source symbol given by
 *  \a hierarchy_tag, otherwise a copy of \a basename is returned with
 *  out modification.
 */
char *s_hierarchy_create_netattrib(GedaToplevel *pr_current,
                                           char *basename,
                                           char *hierarchy_tag)
{
  char *return_value = NULL;

  if (pr_current->hierarchy_netattrib_mangle == FALSE) {

    if (basename) {
      return (geda_strdup (basename));
    }
    else {
      return (NULL);
    }
  }

  if (hierarchy_tag) {

    if (basename) {

      if (pr_current->hierarchy_netattrib_separator) {

        switch (pr_current->hierarchy_netattrib_order) {
          case (APPEND):
            return_value =
            geda_strconcat (hierarchy_tag,
                            pr_current->hierarchy_netattrib_separator,
                            basename, NULL);
            break;
          case (PREPEND):
            return_value =
            geda_strconcat (basename,
                            pr_current->hierarchy_netattrib_separator,
                            hierarchy_tag, NULL);
            break;
        }
      }
      else {

        switch (pr_current->hierarchy_netattrib_order) {
          case (APPEND):
            return_value =
            geda_strconcat (hierarchy_tag, basename, NULL);
            break;
          case (PREPEND):
            return_value =
            geda_strconcat (basename, hierarchy_tag, NULL);
            break;
        }
      }
    }
    else {
      return_value = NULL;
    }
  }
  else {

    if (basename) {
      return_value = geda_strdup (basename);
    }
    else {
      return_value = NULL;
    }
  }

  return (return_value);
}

/*!
 * \brief Hierarchy Remove Mangling from Reference
 * \par Function Description
 *  Removes the hierarchy prefix from reference desginator.
 *  Example:
 *           Sheets_18/J4 18 -> J4 18
 */
void s_hierarchy_remove_uref_mangling(GedaToplevel *pr_current, NETLIST *head)
{
  NETLIST  *nl_current;
  CPINLIST *pl_current;
  NET      *n_current;
  char     *new_uref = NULL;

  nl_current = head;

  while (nl_current != NULL) {

    if (nl_current->component_uref) {
      verbose_print("u");
      new_uref = s_hierarchy_return_baseuref(pr_current,
                                             nl_current->component_uref);
      GEDA_FREE(nl_current->component_uref);
      nl_current->component_uref = new_uref;
    }

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {

      n_current = pl_current->nets;

      while (n_current != NULL) {

        if (n_current->connected_to) {

          char *pin = NULL;
          char  uref[80];

          /* Copy the old "uref pin" pair to buffer */
          strcpy(&uref[0], n_current->connected_to);

          int i = 0;

          /* Change the space to a null and get pointer to pin */
          while (uref[i]){
            if (uref[i] == ' ') {
              pin = &uref[i + 1];
              uref[i] = '\0';
              break;
            }
            i++;
          }

          verbose_print("U");

          /* Retrieve the new reference */
          new_uref = s_hierarchy_return_baseuref(pr_current, uref);

          /* Build the new "uref pin" pair */
          strcpy(&uref[0], new_uref);
          strcat(&uref[0], " ");
          strcat(&uref[0], pin);

          GEDA_FREE(new_uref);
          GEDA_FREE(n_current->connected_to);
          n_current->connected_to = geda_strdup(&uref[0]);
        }
        n_current = n_current->next;
      }

      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }
}

/*!
 * \brief Hierarchy Get the Base reference
 * \par Function Description
 *  Helper for s_hierarchy_remove_uref_mangling, returns the base
 *  refdes with the hierarchy prefix or suffixes removed.
 *
 *  Examples:
 *
 *  1. non-hierarchy:     _U1_        -> _U1_
 *  2. hierarchy:         _S1/V1_     -> V1
 *  3. nested hierarchy:  _S1/S1/R2_  -> R2
 */
char *s_hierarchy_return_baseuref(GedaToplevel *pr_current, char *uref)
{
  char *return_value = NULL;

  /* use hierarchy separator */

  if (uref == NULL) {
    return (NULL);
  }

#if DEBUG
  printf("Got uref: _%s_\n", uref);
#endif

  if (pr_current->hierarchy_uref_order == APPEND) {

    char *start_of_base = strrchr(uref, '/');	/* separator is always '/' */

    if (start_of_base == NULL) {
      return (geda_strdup (uref));
    }

    return_value = geda_strdup (start_of_base + 1);

  }
  else if (pr_current->hierarchy_uref_order == PREPEND) {

    char *end_of_base = strchr(uref, '/');

    if (end_of_base == NULL) {
      return (geda_strdup (uref));
    }

    return_value = geda_strndup(uref, end_of_base - uref);
  }

#if DEBUG
  printf("new uref return_value = %s\n\n\n", return_value);
#endif

  /* Return base refdes */
  return (return_value);
}

/*!
 * \brief Has Graphical attribute set
 * \par Function Description
 *  Checks for existence of a graphical attribute and if found
 *  check if the value of the attribute is non zero, which is
 *  interpreted as meaning the object is a graphical object.
 */
int s_hierarchy_graphical_search (GedaObject* o_current, int count)
{
  bool  result;
  char *graphical_attrib;

  graphical_attrib = geda_attrib_search_object_by_name (o_current,
                                                        "graphical", count);
  if (graphical_attrib) {
    char value = graphical_attrib[0];
    result = value != '0';
    GEDA_FREE (graphical_attrib);
  }
  else {
    result = 0;
  }

  return result;
}
