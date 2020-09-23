/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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

#include "../../config.h"
#include "../../version.h" /* For g_get_version */

#include <gnetlist.h>
#include <libgeda/libgedaguile.h>
#include <gettext.h>
#include <geda_debug.h>

/** \defgroup gnetlist-SCM-API C Based API for Scheme Routines
 *  @{
 *
 *  \par This group contains SCM-API functions.
 */

/*!
 * \brief Get unique list of all packages
 * \par Function Description
 *  This function returns a unique list of packages, duplicated values
 *  are only listed once.
 *
 *  Alias gnetlist:get-packages
 *
 * \param [in] level string Not used
 *
 * \return list of packages
 */
SCM g_get_packages(SCM level)
{
  SCM list = SCM_EOL;
  GHashTable *ht;

  NETLIST *nl_current = NULL;

  /* build a hash table */
  ht = g_hash_table_new (g_str_hash, g_str_equal);

  for (nl_current = netlist_head; nl_current != NULL; nl_current = nl_current->next)
  {

    if (nl_current->component_uref != NULL) {

      /* add component_uref in the hash table */
      /* uniqueness of component_uref is guaranteed by the hashtable */
      if (g_hash_table_lookup (ht, nl_current->component_uref) == NULL) {
        g_hash_table_insert (ht,
                             nl_current->component_uref,
                             nl_current->component_uref);
        list = scm_cons(scm_from_utf8_string (nl_current->component_uref), list);
      }
    }
  }
  g_hash_table_destroy (ht);

  return list;
}

/*!
 * \brief Get non-unique list of all packages
 * \par Function Description
 * This function returns a non unique list of packages, duplicated values
 * are listed as many times as they appear.
 *
 *  Alias gnetlist:get-non-unique-packages
 *
 * \param [in] scm_level string Not used
 *
 * \return list of packages
 */
SCM g_get_non_unique_packages(SCM scm_level)
{
  SCM list = SCM_EOL;

  NETLIST *nl_current = NULL;

  for (nl_current = netlist_head; nl_current != NULL; nl_current = nl_current->next)
  {
    if (nl_current->component_uref != NULL) {
      list = scm_cons (scm_from_utf8_string (nl_current->component_uref),
                       list);
    }
  }

  return list;
}

/*!
 * \brief Get all Pins for a given refdes
 * \par Function Description
 *  Given a refdes, returns all pins belonging to the object.
 *
 *  Alias gnetlist:get-pins
 *
 * \param [in] scm_uref Object to query for pins
 *
 * \return list of pins
 */
SCM g_get_pins(SCM scm_uref)
{
  SCM list = SCM_EOL;

  NETLIST  *nl_current;
  CPINLIST *pl_current;
  char     *uref;
  SCM_ASSERT(scm_is_string (scm_uref), scm_uref, SCM_ARG1, "gnetlist:get-pins");

  uref = scm_to_utf8_string (scm_uref);

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for the first instance */
  /* through the entire list */
  while (nl_current != NULL) {

    if (nl_current->component_uref) {

      if (strcmp(nl_current->component_uref, uref) == 0) {

        pl_current = nl_current->cpins;

        while (pl_current != NULL) {

          if (pl_current->pin_number) {
            list = scm_cons (scm_from_utf8_string (pl_current->pin_number), list);
          }
          pl_current = pl_current->next;
        }
      }
    }
    nl_current = nl_current->next;
  }

  free (uref);

  return (list);
}

/*!
 * \brief Get list of all nets
 * \par Function Description
 *  list of net names for the set of schematics. Duplicated values are
 *  listed as many times as they appear, once per segment?.
 *  Alias gnetlist:get-all-nets
 *
 * \param [in] scm_level string Not used
 *
 * \return list of net names
 */
SCM g_get_all_nets(SCM scm_level)
{
  SCM list = SCM_EOL;
  NETLIST *nl_current;
  CPINLIST *pl_current;
  char *net_name;

  nl_current = netlist_head;

  /* walk through the list of components, and through the list
   * of individual pins on each, adding net names to the list
   * being careful to ignore duplicates, and unconnected pins
   */
  while (nl_current != NULL) {

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {
      if (pl_current->net_name) {

        net_name = pl_current->net_name;

        /* filter off unconnected pins */
        if (strncmp(net_name, "unconnected_pin", 15) != 0) {

#if DEBUG
          printf("Got net: `%s'\n", net_name);
          printf("pin %s\n", pl_current->pin_number);
#endif

          /* add the net name to the list */
          list = scm_cons (scm_from_utf8_string (net_name),
                           list);
        }
      }
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }

  return list;
}

/*!
 * \brief Get unique list of all nets
 * \par Function Description
 *  Gets a list of unique net names for the set of schematics.
 *  Duplicated values are only listed once.
 *
 *  Alias gnetlist:get-all-unique-nets
 *
 * \param [in] scm_level string Not used
 *
 * \return list of net names
 */
SCM g_get_all_unique_nets(SCM scm_level)
{

  SCM list = SCM_EOL;
  SCM x    = SCM_EOL;
  NETLIST *nl_current;
  CPINLIST *pl_current;
  char *net_name;

  SCM_ASSERT(scm_is_string (scm_level), scm_level, SCM_ARG1,
             "gnetlist:get-all-unique-nets");

  nl_current = netlist_head;

  /* walk through the list of components, and through the list
   * of individual pins on each, adding net names to the list
   * being careful to ignore duplicates, and unconnected pins
   */
  while (nl_current != NULL) {

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {

      if (pl_current->net_name) {

        net_name = pl_current->net_name;
        /* filter off unconnected pins */
        if (strncmp(net_name, "unconnected_pin", 15) != 0) {
          /* add the net name to the list */
          /*printf("Got net: `%s'\n",net_name); */

          x = scm_from_utf8_string (net_name);
          if (scm_is_false (scm_member (x, list))) {
            list = scm_cons (x, list);
          }
        }
      }
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }

  return list;
}

/*!
 * \brief Get All connection for a given Netname
 * \par Function Description
 *  Given a net name, returns list of all connections to the named net.
 *  Each element of the list is itself a two element list of the form
 *  (refdes pinnumber).
 *
 *  Alias gnetlist:get-all-connections
 *
 * \param [in] scm_netname string net name
 *
 * \return list of connections
 */
SCM g_get_all_connections(SCM scm_netname)
{
  SCM connlist = SCM_EOL;
  SCM pairlist = SCM_EOL;

  NETLIST *nl_current;

  char *wanted_net_name;

  SCM_ASSERT(scm_is_string(scm_netname), scm_netname, SCM_ARG1,
             "gnetlist:get-all-connections");

  wanted_net_name = scm_to_utf8_string (scm_netname);

  nl_current = netlist_head;

  /* walk through the list of components, and through the list
   * of individual pins on each, adding net names to the list
   * being careful to ignore duplicates, and unconnected pins
   */
  while (nl_current != NULL) {

    CPINLIST *pl_current;

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {

      if (pl_current->net_name) {

        char *net_name;

        net_name = pl_current->net_name;

        /* filter off unconnected pins */
        if (strcmp(net_name, wanted_net_name) == 0) {

#if DEBUG
          printf("found net: `%s'\n", net_name);
#endif

          NET *n_current;

          /* add the net name to the list */
          n_current = pl_current->nets;

          while (n_current != NULL) {

            if (n_current->connected_to) {

              char *pin;
              char *uref;

              pin =(char*)GEDA_MEM_ALLOC(sizeof(char) *strlen(n_current->connected_to));

              uref =(char*)GEDA_MEM_ALLOC(sizeof(char) *strlen(n_current->connected_to));

              sscanf(n_current->connected_to, "%s %s", uref, pin);

              pairlist = scm_list_n (scm_from_utf8_string (uref),
                                     scm_from_utf8_string (pin),
                                     SCM_UNDEFINED);

              if (scm_is_false (scm_member(pairlist, connlist))) {
                connlist = scm_cons (pairlist, connlist);
              }

              GEDA_FREE(uref);
              GEDA_FREE(pin);
            }
            n_current = n_current->next;
          }
        }
      }
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }

  free (wanted_net_name);

  return connlist;
}

/*  Reports suggest that get_nets does not reliably list all connections,
 *  possibly due to a problem with net= connections. Most (all?) existing
 *  back ends that use this appear to use only the netname.
 */
/*!
 *  \brief Get Netname connected to a Pin belonging to UREF
 *  \par Function Description
 *  Given a uref and a pin number return a list of:
 *  (netname (uref pin) (uref pin) ... )
 *
 *  This is apparently intended to yield the netname connected to the given
 *  pin along with all pins connected to that net, including the pin in the
 *  initial query.
 *
 *  \todo Validate this function
 *
 *  Alias gnetlist:get-nets
 *
 * \param [in] scm_uref string reference designator
 * \param [in] scm_pin  string pin number
 *
 * \return list of net connections, (netname (uref pin) (uref pin) ... )
 */
SCM g_get_nets(SCM scm_uref, SCM scm_pin)
{
  SCM outerlist        = SCM_EOL;
  SCM pinslist         = SCM_EOL;

  NETLIST  *nl_current = NULL;

  NET  *n_current;
  char *wanted_uref    = NULL;
  char *wanted_pin     = NULL;
  char *net_name       = NULL;

  SCM_ASSERT(scm_is_string (scm_uref), scm_uref, SCM_ARG1, "gnetlist:get-nets");

  SCM_ASSERT(scm_is_string (scm_pin), scm_pin, SCM_ARG2, "gnetlist:get-nets");

  scm_dynwind_begin (0);

  wanted_uref = scm_to_utf8_string (scm_uref);
  wanted_pin = scm_to_utf8_string (scm_pin);

  scm_dynwind_end ();

  nl_current = netlist_head;

  /* search through the entire list for the first instance */
  for (nl_current = netlist_head; nl_current != NULL; nl_current = nl_current->next)
  {
    CPINLIST *pl_current;

    if (!nl_current->component_uref) continue;

    if (strcmp (nl_current->component_uref, wanted_uref) != 0) continue;

    for (pl_current = nl_current->cpins; pl_current != NULL; pl_current = pl_current->next)
    {

      if (!pl_current->pin_number) continue;

      if (strcmp(pl_current->pin_number, wanted_pin) != 0) continue;

      if (pl_current->net_name) {
        net_name = pl_current->net_name;
      }

      for (n_current = pl_current->nets; n_current != NULL; n_current = n_current->next)
      {
        SCM   pairlist;
        char *pin;
        char *uref;

        if (!n_current->connected_to) continue;

        pin = (char*) GEDA_MEM_ALLOC(sizeof(char) * strlen(n_current->connected_to));
        uref = (char*) GEDA_MEM_ALLOC(sizeof(char) * strlen(n_current->connected_to));

        sscanf(n_current->connected_to, "%s %s", uref, pin);

        pairlist = scm_list_n (scm_from_utf8_string (uref),
                               scm_from_utf8_string (pin),
                               SCM_UNDEFINED);

        pinslist = scm_cons (pairlist, pinslist);

        GEDA_FREE(uref);
        GEDA_FREE(pin);
      }
    }
  }

  if (net_name != NULL) {
    outerlist = scm_cons (scm_from_utf8_string (net_name), pinslist);
  }
  else {
    outerlist = scm_cons (scm_from_utf8_string ("ERROR_INVALID_PIN"), outerlist);
    fprintf(stderr, _("Invalid refdes ('%s') and pin ('%s') passed to get-nets\n"),
            wanted_uref, wanted_pin);
  }

  free (wanted_uref);
  free (wanted_pin);

  return (outerlist);
}


/*!
 * \brief Get Nets of a given Net UREF
 * \par Function Description
 *  Given a uref, return a list of pairs, each pair contains the pin
 *  number and the name of the net connected to that pin.
 *
 *  Alias gnetlist:get-pin-nets
 *
 * \param [in] scm_uref string reference designator
 *
 * \return list containing connections ((pinnumber . netname) ... )
 */
SCM g_get_pins_nets(SCM scm_uref)
{
  SCM pinslist         = SCM_EOL;
  SCM pairlist         = SCM_EOL;

  NETLIST  *nl_current = NULL;
  CPINLIST *pl_current = NULL;

  char *wanted_uref = NULL;
  char *net_name    = NULL;
  char *pin         = NULL;

  SCM_ASSERT(scm_is_string (scm_uref),
             scm_uref, SCM_ARG1, "gnetlist:get-pins-nets");

  wanted_uref = scm_to_utf8_string (scm_uref);

  /* search for the any instances */
  /* through the entire list */
  for (nl_current = netlist_head; nl_current != NULL;
       nl_current = nl_current->next)
  {

    /* is there a uref? */
    if (nl_current->component_uref) {

      /* is it the one we want ? */
      if (strcmp(nl_current->component_uref, wanted_uref) == 0) {

        for (pl_current = nl_current->cpins; pl_current != NULL;
             pl_current = pl_current->next) {

          /* is there a valid pin number and a valid name ? */
          if (pl_current->pin_number) {

            if (pl_current->net_name) {

              /* yes, add it to the list */
              pin = pl_current->pin_number;
              net_name = pl_current->net_name;

              pairlist = scm_cons (scm_from_utf8_string (pin),
                                   scm_from_utf8_string (net_name));
              pinslist = scm_cons (pairlist, pinslist);
            }
          }
        }
      }
    }
  }

  free (wanted_uref);

  return (scm_reverse (pinslist)); /* pins are in reverse order on way out */
}


/*!
 * \brief Get attribute value(s) from a package with given uref.
 * \par Function Description
 *  This function returns the values of a specific attribute type
 *  attached to the symbol instances with the given refdes.
 *
 *  Every first attribute value found is added to the return list. A
 *  Scheme false value is added if the instance has no such attribute.
 *
 * \note The order of the values in the return list is the order of
 *  symbol instances within gnetlist (the first element is the value
 *  associated with the first symbol instance).
 *
 *  Alias gnetlist:get-all-package-attributes
 *
 * \param [in] scm_uref           Package reference.
 * \param [in] scm_wanted_attrib  Attribute name.
 *
 * \return A list of attribute values as strings and FALSE.
 */
SCM g_get_all_package_attributes(SCM scm_uref, SCM scm_wanted_attrib)
{
  SCM ret = SCM_EOL;
  NETLIST *nl_current;
  char *uref;
  char *wanted_attrib;

  SCM_ASSERT(scm_is_string (scm_uref),
             scm_uref, SCM_ARG1, "gnetlist:get-all-package-attributes");

  SCM_ASSERT(scm_is_string (scm_wanted_attrib),
             scm_wanted_attrib, SCM_ARG2, "gnetlist:get-all-package-attributes");

  uref          = scm_to_utf8_string (scm_uref);
  wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for uref instances and through the entire list */
  while (nl_current != NULL) {

    if (nl_current->component_uref) {

      if (strcmp(nl_current->component_uref, uref) == 0) {

        char *value;

        value = geda_attrib_search_object_by_name (nl_current->object_ptr,
                                                   wanted_attrib, 0);

        ret = scm_cons (value ? scm_from_utf8_string (value) : SCM_BOOL_F, ret);

        GEDA_FREE (value);
      }
    }

    nl_current = nl_current->next;
  }

  free (uref);
  free (wanted_attrib);

  return scm_reverse_x (ret, SCM_EOL);
}

/*!
 * \brief Get Attribute Value by Pin Sequence
 * \par Function Description
 *  This API function provides a method to retrieve an attribute value
 *  given a uref and pinseq number. Returns the value associated with
 *  the first instance.
 *
 *  Alias gnetlist:get-attribute-by-pinseq
 *
 * \param [in] scm_uref          string reference designator
 * \param [in] scm_pinseq        string pin sequence
 * \param [in] scm_wanted_attrib string attribute name (ex. "pinlabel").
 *
 * \return value of the attribute or "unknown" if attribute was not found
 */
SCM g_get_attribute_by_pinseq(SCM scm_uref, SCM scm_pinseq,
                              SCM scm_wanted_attrib)
{
  SCM scm_return_value;
  NETLIST *nl_current;
  char *uref;
  char *pinseq;
  char *wanted_attrib;
  char *return_value = NULL;
  GedaObject *o_pin_object;

  SCM_ASSERT(scm_is_string (scm_uref),
             scm_uref, SCM_ARG1, "gnetlist:get-attribute-by-pinseq");

  SCM_ASSERT(scm_is_string (scm_pinseq),
             scm_pinseq, SCM_ARG2, "gnetlist:get-attribute-by-pinseq");


  SCM_ASSERT(scm_is_string (scm_wanted_attrib),
             scm_wanted_attrib, SCM_ARG3, "gnetlist:get-attribute-by-pinseq");

  scm_dynwind_begin (0);

  uref = scm_to_utf8_string (scm_uref);
  scm_dynwind_free (uref);

  pinseq = scm_to_utf8_string (scm_pinseq);
  scm_dynwind_free (pinseq);

  wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);
  scm_dynwind_free (wanted_attrib);

#if DEBUG
  printf("gnetlist:g_netlist.c:g_get_attribute_by_pinseq -- \n");
  printf("  wanted uref = %s\n", uref);
  printf("  wanted_pin_seq = %s\n", pinseq);
  printf("  wanted_attrib = %s\n", wanted_attrib);
#endif

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for the first instance */
  /* through the entire list */
  while (nl_current != NULL) {

    if (nl_current->component_uref) {

      if (strcmp(nl_current->component_uref, uref) == 0) {

        o_pin_object =
          geda_complex_object_find_pin_by_attribute (nl_current->object_ptr,
                                                     "pinseq", pinseq);

        if (o_pin_object) {
          return_value =
          geda_attrib_search_object_by_name (o_pin_object, wanted_attrib, 0);
          if (return_value) {
            break;
          }
        }

        /* Don't break until we search the whole netlist to handle slotted */
        /* parts.   4.28.2007 -- SDB. */
      }
    }
    nl_current = nl_current->next;
  }

  scm_dynwind_end ();

  if (return_value) {
    scm_return_value = scm_from_utf8_string (return_value);
  }
  else {
    scm_return_value = scm_from_utf8_string ("unknown");
  }

#if DEBUG
  printf("gnetlist:g_netlist.c:g_get_attribute_by_pinseq -- ");
  printf("return_value: %s\n", return_value ? return_value : "NULL");
#endif

  return (scm_return_value);
}

/*!
 * \brief Get Attribute by Pin Number
 * \par Function Description
 *  This API function takes a pin number and returns the appropriate
 *  attribute on that pin scm_pin is the value associated with the
 *  pinnumber= attribute and uref
 *
 *  Alias gnetlist:get-attribute-by-pinnumber
 *
 * \param [in] scm_uref          string reference designator
 * \param [in] scm_pin           string pin number
 * \param [in] scm_wanted_attrib string attribute name (ex. "pintype").
 *
 * \return value of the attribute or "unknown" if attribute was not found
 */
SCM g_get_attribute_by_pinnumber(SCM scm_uref, SCM scm_pin,
                                 SCM scm_wanted_attrib)
{
  SCM scm_return_value;

  NETLIST    *nl_current;
  GedaObject *pin_object;

  char *uref;
  char *pin;
  char *wanted_attrib;
  char *return_value = NULL;
  bool  return_pwr   = FALSE;

  SCM_ASSERT(scm_is_string (scm_uref),
             scm_uref, SCM_ARG1, "gnetlist:get-attribute-by-pinnumber");

  SCM_ASSERT(scm_is_string (scm_pin),
             scm_pin, SCM_ARG2, "gnetlist:get-attribute-by-pinnumber");

  SCM_ASSERT(scm_is_string (scm_wanted_attrib),
             scm_wanted_attrib, SCM_ARG3, "gnetlist:get-attribute-by-pinnumber");

  scm_dynwind_begin (0);

  uref = scm_to_utf8_string (scm_uref);
  scm_dynwind_free (uref);

  pin = scm_to_utf8_string (scm_pin);
  scm_dynwind_free (pin);

  wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);
  scm_dynwind_free (wanted_attrib);

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for the first instance */
  /* through the entire list */
  while (nl_current != NULL) {

    if (nl_current->component_uref) {

      if (strcmp(nl_current->component_uref, uref) == 0) {

        pin_object =
          geda_complex_object_find_pin_by_attribute (nl_current->object_ptr,
                                                     "pinnumber", pin);

        if (pin_object) {

          /* only look for the first occurance of wanted_attrib */
          return_value = geda_attrib_search_object_by_name (pin_object,
                                                            wanted_attrib, 0);

          if (return_value) {
#if DEBUG
            printf("GOT return_value=<%s>\n", return_value);
#endif
            break;
          }

        }
        else if (strcmp("pintype", wanted_attrib) == 0) {

          if (nl_current->cpins) {

            CPINLIST *pinobject = s_cpinlist_search_pin(nl_current->cpins, pin);

            if (pinobject) {

              return_pwr = TRUE;
#if DEBUG

              printf("pintype 'pwr' for artificial pin '%s' of '%s'\n",
                     pin, uref);
#endif
              /* uref was found, but does not have pin, no need to continue */
              break;
            }
          }
        }
      }
    }
    nl_current = nl_current->next;
  }

  if (return_value) {
    scm_return_value = scm_from_utf8_string (return_value);
    GEDA_FREE(return_value);
  }
  else {

    if (return_pwr) {
      scm_return_value = scm_from_utf8_string ("pwr");
    }
    else {

      GedaObject *object;

      object = s_netlist_find_object (graphical_netlist_head, uref);

      if (object) {

        pin_object =
          geda_complex_object_find_pin_by_attribute (object, "pinnumber", pin);

        if (pin_object) {

          /* only look for the first occurance of wanted_attrib */
          return_value = geda_attrib_search_object_by_name (pin_object,
                                                            wanted_attrib, 0);

          if (verbose_mode) {
            const char *msg2 = _("retrieved from a graphical object");
            fprintf (stderr, "%s %s \"%s\" %s %s.\n", _("Pin"), pin, wanted_attrib, msg2, uref);
          }
        }
      }

      if (!return_value) {
        scm_return_value = scm_from_utf8_string ("unknown");
      }
      else {
        scm_return_value = scm_from_utf8_string (return_value);
        GEDA_FREE(return_value);
      }
    }
  }

  scm_dynwind_end ();

  return (scm_return_value);
}

/*!
 * \brief Get Top-Level Attributes
 * \par Function Description
 *  returns value of the named attribute at top level, that is, an attribute
 *  present in one of the schematics unattached to any object.
 *
 *  Alias gnetlist:get-toplevel-attribute
 *
 * \param [in] scm_wanted_attrib string attribute name (ex. "model").
 *
 * \return value of the attribute or "not found" if attribute was not found
 */
SCM g_get_toplevel_attribute(SCM scm_wanted_attrib)
{
  const GList *p_iter;
  char *wanted_attrib;
  char *attrib_value = NULL;
  SCM scm_return_value;

  GedaToplevel *toplevel;

  SCM_ASSERT(scm_is_string (scm_wanted_attrib),
             scm_wanted_attrib, SCM_ARG1, "gnetlist:get-toplevel-attribute");

  toplevel = edascm_c_current_toplevel ();

  wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);

  for (p_iter = geda_list_get_glist (toplevel->pages); p_iter != NULL; NEXT(p_iter))
  {
    Page *p_current = p_iter->data;

    /* only look for first occurrance of the attribute on each page */
    attrib_value =
    geda_attrib_search_floating_by_name (geda_struct_page_get_objects (p_current),
                                         wanted_attrib, 0);

    /* Stop when we find the first one */
    if (attrib_value != NULL)
      break;
  }

  free (wanted_attrib);

  if (attrib_value != NULL) {
    scm_return_value = scm_from_utf8_string (attrib_value);
    GEDA_FREE (attrib_value);
  }
  else {
    scm_return_value = scm_from_utf8_string ("not found");
  }

  return (scm_return_value);
}

/*!
 * \brief Returns verbosity level for messages
 * \par Function Description
 *  If the "-q" gnetlist command-line option was specified, returns -1.
 *  If the "-v" gnetlist command-line option was specified, returns 1.
 *  Otherwise, returns 0.
 */
SCM g_get_verbosity (void)
{
  if (verbose_mode) {
    return scm_from_int (1);
  }
  else if (quiet_mode) {
    return scm_from_int (-1);
  }
  else {
    return scm_from_int (0);
  }
}

/*!
 * \brief Returns the gnetlist Dotted Version
 * \par Function Description
 *  Returns the gnetlist dotted version as a string.
 */
SCM g_get_version (void)
{
  return scm_from_locale_string (PACKAGE_DOTTED_VERSION);
}

/*!
 * \brief Obtain a list of `-O' backend arguments
 * \par Function Description
 *  Returns a list of arguments passed to the gnetlist backend via the
 *  `-O' gnetlist command-line option.
 */
SCM g_get_backend_arguments(void)
{
  SCM result = SCM_EOL;
  GSList *iter;

  for (iter = backend_params; iter != NULL; iter = g_slist_next (iter)) {
    result = scm_cons (scm_from_locale_string ((char *) iter->data), result);
  }

  return scm_reverse_x (result, SCM_UNDEFINED);
}

/*!
 * \brief Get input files from command line
 * \par Function Description
 *  This function returns a list of the files named on the command line.
 *
 * \return A list of filenames as strings.
 */
SCM g_get_input_files(void)
{
  GedaToplevel *toplevel;
  GList        *p_iter;
  SCM           list;

  list     = SCM_EOL;
  toplevel = edascm_c_current_toplevel ();
  p_iter   = geda_toplevel_get_pages (toplevel);

  while (p_iter != NULL) {
    const char *filename = p_iter->data;
    list = scm_cons (scm_from_locale_string (filename), list);
    NEXT(p_iter);
  }

  return scm_reverse_x (list, SCM_EOL);
}

/*!
 * \brief Get graphical objects connected to given net
 * \par Function Description
 *  given a net name, an attribute, and a wanted attribute, return all
 *  the given attributes of all the graphical objects connected to that
 *  net name.
 *
 * \param [in] scm_netname          Name of the net to search which
 * \param [in] scm_has_attribute    Must possess this attribute
 * \param [in] scm_wanted_attribute Attribute to search for
 *
 * \returns list of values from wanted attributes if found
 */
SCM g_graphical_objs_in_net_with_attrib_get_attrib (SCM scm_netname, SCM scm_has_attribute, SCM scm_wanted_attribute)
{
  SCM list = SCM_EOL;
  NETLIST  *nl_current;

  char *net_name;
  char *has_attrib;
  char *wanted_attrib;

  scm_dynwind_begin (0);

  SCM_ASSERT(scm_is_string (scm_netname), scm_netname, SCM_ARG1,
             "gnetlist:graphical-objs-in-net-with-attrib-get-attrib");

  SCM_ASSERT(scm_is_string (scm_has_attribute),
             scm_has_attribute, SCM_ARG2,
             "gnetlist:graphical-objs-in-net-with-attrib-get-attrib");

  SCM_ASSERT(scm_is_string (scm_wanted_attribute),
             scm_wanted_attribute, SCM_ARG3,
             "gnetlist:graphical-objs-in-net-with-attrib-get-attrib");

  net_name      = scm_to_utf8_string (scm_netname);
  wanted_attrib = scm_to_utf8_string (scm_wanted_attribute);
  has_attrib    = scm_to_utf8_string (scm_has_attribute);

  scm_dynwind_end ();

  nl_current    = graphical_netlist_head;

  /* walk through list of components, and through the list
   * of individual pins on each, adding net names to the list
   * being careful to ignore duplicates, and unconnected pins
   */
  while (nl_current != NULL) {

    CPINLIST *pl_current;

    pl_current = nl_current->cpins;

    while (pl_current != NULL) {

      if (pl_current->net_name) {

        char *current_name = pl_current->net_name;

        if (strcmp(current_name, net_name) == 0) {

          char *has_attrib_name;
          char *has_attrib_value;

          if (geda_attrib_string_get_name_value (has_attrib, &has_attrib_name, &has_attrib_value) != 0)
          {
            char *attrib_value =
            geda_attrib_search_object_by_name (nl_current->object_ptr,
                                               has_attrib_name, 0);

            if (((has_attrib_value == NULL) && (attrib_value == NULL)) ||
                ((has_attrib_value != NULL) && (attrib_value != NULL) &&
                 (strcmp(attrib_value, has_attrib_value) == 0)))
            {
              char *wanted_value =

              geda_attrib_search_object_by_name (nl_current->object_ptr,
                                                 wanted_attrib, 0);
              if (wanted_value) {
                list = scm_cons (scm_from_utf8_string (wanted_value), list);
              }
              GEDA_FREE (wanted_value);
            }

            GEDA_FREE (attrib_value);
            GEDA_FREE (has_attrib_name);
            GEDA_FREE (has_attrib_value);
          }
        }
      }
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }

  free (has_attrib);
  free (wanted_attrib);
  free (net_name);

  return list;
}

/*
 * This function is in s_rename.c:  SCM g_get_renamed_nets(SCM scm_level)
 */
/** @} endgroup gnetlist-SCM-API */