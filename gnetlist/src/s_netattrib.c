/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_netattrib.c
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

#include <config.h>
#include <gnetlist.h>
#include <gettext.h>
#include <geda_debug.h>

/*! \def PIN_NET_PREFIX Used by the connected string functions */
#define PIN_NET_PREFIX "__netattrib_power_pin "

/*! \def DELIMITERS used by search and extract functions below */
#define DELIMITERS ",; "

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_netattrib_pinnum_get_connected_string (const char *pinnum)
{
  return geda_sprintf (PIN_NET_PREFIX "%s", pinnum);
}

/*! \brief Get the Pin Number of connected Net Attribute string
 *  \par Function Description
 *  This function returns the pin number as a string of a connected
 *  net attribute. If \a str does not begin with the PIN_NET_PREFIX
 *  NULL is returned:
 *
 *  example 1:  __netattrib_power_pin 1    returns "1"
 *  example 2:  S2/C20 2                   returns NULL
 */
const char *s_netattrib_connected_string_get_pinnum (const char *str)
{
  int prefix_len = (sizeof PIN_NET_PREFIX) - 1;

  if (strncmp (str, PIN_NET_PREFIX, prefix_len) != 0) {
    return NULL;
  }

  return str + prefix_len;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_netattrib_check_connected_string (const char *str)
{
  if (s_netattrib_connected_string_get_pinnum (str) == NULL) return;

  fprintf (stderr,
         _("ERROR: `%s' is reserved for internal use.\n"), PIN_NET_PREFIX);
           exit (1);
}

/*! \brief Get the Net Name from a Net Attribute
 *  \par Function Description
 *   Replaces the colon in a copy of the netname with a NULL,
 *   truncating the pin number portion of the string.
 *
 *  example:
 *            SCI_RX:1 returns SCI_RX
 *
 * \note caller should GEDA_FREE returned string
 *
 * things to do here :
 * write the net alias function
 *
 * \todo Actually validate the value and report errors, just about any
 *       string that is passed to this routine is accepted, including
 *       "GND::1".
 */
char *s_netattrib_extract_netname(char *value)
{
  char *return_value;
  int i = 1;

  /* a bit larger than needed ... */
  return_value = geda_utility_string_strdup (value);

  while (value[i] != ':' && value[i] != '\0') {
    return_value[i] = value[i];
    i++;
  }

  if (value[i] != ':') {
    fprintf(stderr, _("Found malformed net attribute\n"));
    GEDA_FREE(return_value);
    return_value = geda_utility_string_strdup ("unknown");
  }
  else {
    return_value[i] = '\0'; /* Replace colon with NULL */
  }

  return (return_value);

}

/*! \brief Create pins for Net Attributes
 *  \par Function Description
 *  Symbols with net attributes, for example, net=VCC:14, normally do not
 *  have an actual pin defined. This routine creates these virtual pins
 *  after checking that a real pin does not exist. If a pin is defined
 *  then a notice is issued and the existing pin information replaced
 *  using the net name found in the net attribute.
 *
 *  \param [in]     pr_current    Current GedaToplevel structure; toplevel,
 *  \param [in]     o_current     Complex, presumably with a least one pin
 *  \param [in,out] netlist       The net list
 *  \param [in]     value         Is value of net attribute, like GND:4
 *  \param [in]     hierarchy_tag refdes of symbol with source or NULL
 *
 * \note 1 The hierarchy_tag is the string before renaming occurs, and the
 *         tag may be replaced later, for eample here tag could be S1 and
 *         this might get changed to another tag, like S4, later.
 *
 * \note 2 If this function creates a cpinlist list, it will not have a head
 *         node.
 */
void
s_netattrib_create_pins(GedaToplevel *pr_current,GedaObject *o_current,
                        NETLIST *netlist, char *value, char *hierarchy_tag)
{
  char *char_ptr;
  char *current_pin;
  char *net_name;
  char *start_of_pinlist;

  char_ptr = strchr(value, ':');

  if (char_ptr == NULL) {
    return;
  }

  net_name = s_netattrib_extract_netname(value);

  /* skip over first : */
  start_of_pinlist = char_ptr + 1;
  current_pin      = strtok(start_of_pinlist, DELIMITERS);

  while (current_pin) {

    CPINLIST *cpinlist_tail;
    NETLIST  *netlist_tail;

    netlist_tail  = s_netlist_return_tail(netlist);
    cpinlist_tail = s_cpinlist_return_tail(netlist_tail->cpins);

    if (netlist->component_uref) {

      CPINLIST *old_cpin;

      old_cpin = s_cpinlist_search_pin(netlist_tail->cpins, current_pin);

      if (old_cpin) {

        if (old_cpin->nets != NULL) {

          if (old_cpin->nets->net_name) {
            if (!quiet_mode) {
              fprintf(stderr, _("Found a cpinlist head with a netname! [%s]\n"),
              old_cpin->nets->net_name);
            }
            GEDA_FREE(old_cpin->nets->net_name);
          }

          old_cpin->nets->net_name =
          s_hierarchy_create_netattrib(pr_current, net_name, hierarchy_tag);

          GEDA_FREE(old_cpin->nets->connected_to);

          old_cpin->nets->connected_to =
          geda_sprintf("%s %s", netlist->component_uref, current_pin);

          old_cpin->nets->net_name_has_priority = TRUE;

          old_cpin->nets->nid = o_current->sid;

        }
        else {
          BUG_MSG("Check old_cpin->nets");
        }
      }
      else {

        CPINLIST *new_cpin;

        new_cpin             = s_cpinlist_add(cpinlist_tail);

        new_cpin->pin_number = geda_utility_string_strdup (current_pin);
        new_cpin->net_name   = NULL;
        new_cpin->plid       = o_current->sid;

        new_cpin->nets       = s_net_add(NULL);

        new_cpin->nets->net_name_has_priority = TRUE;

        GEDA_FREE(new_cpin->nets->net_name);

        new_cpin->nets->net_name =
        s_hierarchy_create_netattrib(pr_current, net_name, hierarchy_tag);

        new_cpin->nets->connected_to =
        geda_sprintf("%s %s", netlist->component_uref, current_pin);

        new_cpin->nets->nid = o_current->sid;

#if DEBUG
        char *connected_to = new_cpin->nets->connected_to;
        printf("Finished creating: %s, %p\n", connected_to, connected_to);
        printf("netname: %s %s\n", new_cpin->nets->net_name, hierarchy_tag);
#endif

      }
    }
    /* else { no uref, which means this is a special component } */

    current_pin = strtok(NULL, DELIMITERS);
  }
  GEDA_FREE(net_name);
}

/*! \brief Process Net Attributes
 *  \par Function Description
 *  Searches \a o_current for inherited and attached net
 *  attributes and calls s_netattrib_create_pins for each
 *  attribute that is found.
 */
void
s_netattrib_handle (GedaToplevel *pr_current,GedaObject *o_current,
                    NETLIST *netlist, char *hierarchy_tag)
{
  char *value;
  int   counter;

  /* Look inside the component */
  for (counter = 0; ;) {

    value = geda_attrib_search_inherited_by_name (o_current, "net", counter);
    if (value == NULL)
      break;

    counter++;

    s_netattrib_create_pins (pr_current, o_current,
                             netlist, value, hierarchy_tag);
    GEDA_FREE (value);

  }

  /* Look outside the component */
  for (counter = 0; ;) {

    value = geda_attrib_search_attached_by_name (o_current, "net", counter);

    if (value == NULL)
      break;

    counter++;

    s_netattrib_create_pins (pr_current, o_current,
                             netlist, value, hierarchy_tag);
    GEDA_FREE (value);
  }
}

/*! \brief Search Net Attributes given a Pin Number and return Net Name
 *  \par Function Description
 *  An object can have multiple net= attributes, either attached or
 *  floating. This function searches complex object types for the
 *  net=netname:x attribute with an x==wanted_pin, and if found
 *  returns the net-name portion of the attribute as extracted by
 *  s_netattrib_extract_netname function.
 *
 *  \sa s_netattrib_extract_netname
 *
  * \note caller should GEDA_FREE returned string
 */
char *s_netattrib_net_search (GedaObject *o_current, const char *wanted_pin)
{
  char *value             = NULL;
  char *char_ptr          = NULL;
  char *start_of_pinlist  = NULL;
  char *return_value      = NULL;
  const char *current_pin = NULL;
  int   counter;

  if (o_current == NULL || o_current->complex == NULL)
    return NULL;

  /* for now just look inside the component */
  for (counter = 0; ;) {

    char *net_name;

    value = geda_attrib_search_inherited_by_name (o_current, "net", counter);
    if (value == NULL)
      break;

    counter++;

    char_ptr = strchr (value, ':');

    if (char_ptr == NULL) {
      fprintf (stderr,
     _("Got an invalid net= attrib [net=%s]\nMissing : in net= attrib\n"),
              value);
      GEDA_FREE (value);
      return NULL;
    }

    net_name = s_netattrib_extract_netname (value);

    /* Check if net attribute malformed */
    if (!net_name) {
      GEDA_FREE (value);
      break;
    }

    start_of_pinlist = char_ptr + 1;

    current_pin = strtok (start_of_pinlist, DELIMITERS);

    while (current_pin && !return_value) {
      if (strcmp (current_pin, wanted_pin) == 0) {
        return_value = net_name;
      }
      current_pin = strtok (NULL, DELIMITERS);
    }

    GEDA_FREE (value);
  }

  /* now look outside the component */
  for (counter = 0; ;) {

    char *net_name;

    value = geda_attrib_search_attached_by_name (o_current, "net", counter);

    if (value == NULL)
      break;

    counter++;

    char_ptr = strchr (value, ':');
    if (char_ptr == NULL) {
      fprintf (stderr,
          _("Got an invalid net= attrib [net=%s]\nMissing : in net= attrib\n"),
              value);
      GEDA_FREE (value);
      return NULL;
    }

    net_name = s_netattrib_extract_netname (value);

    if (!net_name) {
      GEDA_FREE (value);
      break;
    }

    start_of_pinlist = char_ptr + 1;

    current_pin = strtok (start_of_pinlist, DELIMITERS);

    while (current_pin) {

      if (strcmp (current_pin, wanted_pin) == 0) {
        GEDA_FREE (return_value);
        GEDA_FREE (value);
        return net_name;
      }
      current_pin = strtok (NULL, DELIMITERS);
    }

    GEDA_FREE (value);
  }

  return return_value;
}

/*! \todo Finish function documentation!!!
 *  \brief Get net-name associated with an Object's pin
 *  \par Function Description
 *
 */
char *s_netattrib_return_netname(GedaToplevel *pr_current,GedaObject *o_pin,
                                 char *pinnumber, char *hierarchy_tag)
{
  const char *pin_num;
        char *netname;
        char *temp_netname;

  pin_num = s_netattrib_connected_string_get_pinnum (pinnumber);

  if (pin_num == NULL) return NULL;

  /* use hierarchy tag here to make this net unique */
  temp_netname = s_netattrib_net_search(o_pin->parent_object, pin_num);

  netname =
  s_hierarchy_create_netattrib(pr_current, temp_netname, hierarchy_tag);

  GEDA_FREE (temp_netname);

#if DEBUG
  printf("netname: %s\n", netname);
#endif

  return (netname);
}
