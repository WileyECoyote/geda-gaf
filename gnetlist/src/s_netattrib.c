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
#include "gnetlist.h"
#include <gettext.h>
#include <geda_debug.h>

/* Used by the connected string functions */
#define PIN_NET_PREFIX "__netattrib_power_pin "

/* used by the extract functions below */
#define DELIMITERS ",; "

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_netattrib_pinnum_get_connected_string (const char *pinnum)
{
  return u_string_sprintf (PIN_NET_PREFIX "%s", pinnum);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * things to do here :
 * write the net alias function
 *
 * \note caller should GEDA_FREE returned string
 */
char *s_netattrib_extract_netname(char *value)
{
  char *return_value = NULL;
  int i = 0;

  /* a bit larger than needed ... */
  return_value = u_string_strdup (value);

  while (value[i] != ':' && value[i] != '\0') {
    return_value[i] = value[i];
    i++;
  }

  if (value[i] != ':') {
    fprintf(stderr, _("Found malformed net attribute\n"));
    return (u_string_strdup ("unknown"));
  }

  return_value[i] = '\0';

  return (return_value);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  if this function creates a cpinlist list, it will not have a head node
 */
void
s_netattrib_create_pins(GedaToplevel * pr_current, Object * o_current,
                        NETLIST * netlist, char *value, char *hierarchy_tag)
{
  NETLIST  *netlist_tail  = NULL;
  CPINLIST *cpinlist_tail = NULL;
  CPINLIST *new_cpin      = NULL;
  CPINLIST *old_cpin      = NULL;

  char *connected_to      = NULL;
  char *net_name          = NULL;
  char *start_of_pinlist  = NULL;
  char *char_ptr          = NULL;
  char *current_pin       = NULL;

  char_ptr = strchr(value, ':');

  if (char_ptr == NULL) {
    return;
  }

  net_name = s_netattrib_extract_netname(value);

  /* skip over first : */
  start_of_pinlist = char_ptr + 1;
  current_pin = strtok(start_of_pinlist, DELIMITERS);
  while (current_pin) {

    netlist_tail = s_netlist_return_tail(netlist);
    cpinlist_tail = s_cpinlist_return_tail(netlist_tail->cpins);

    if (netlist->component_uref) {

      old_cpin =
      s_cpinlist_search_pin(netlist_tail->cpins, current_pin);

      if (old_cpin) {

        if (old_cpin->nets != NULL) {

          if (old_cpin->nets->net_name) {
            fprintf(stderr, _("Found a cpinlist head with a netname! [%s]\n"),
            old_cpin->nets->net_name);
            GEDA_FREE(old_cpin->nets->net_name);
          }

          old_cpin->nets->net_name =
          s_hierarchy_create_netattrib(pr_current, net_name,
                                       hierarchy_tag);
          old_cpin->nets->net_name_has_priority = TRUE;
          connected_to = u_string_sprintf("%s %s",
          netlist->component_uref,
          current_pin);
          old_cpin->nets->connected_to = u_string_strdup(connected_to);
          old_cpin->nets->nid = o_current->sid;
          GEDA_FREE(connected_to);
        }
        else {
          BUG_MSG("Check old_cpin->nets");
        }
      }
      else {


        new_cpin = s_cpinlist_add(cpinlist_tail);

        new_cpin->pin_number = u_string_strdup (current_pin);
        new_cpin->net_name = NULL;

        new_cpin->plid = o_current->sid;

        new_cpin->nets = s_net_add(NULL);
        new_cpin->nets->net_name_has_priority = TRUE;
        new_cpin->nets->net_name =
        s_hierarchy_create_netattrib(pr_current, net_name,
                                     hierarchy_tag);

        connected_to = u_string_sprintf("%s %s",
                                       netlist->component_uref,
                                       current_pin);
        new_cpin->nets->connected_to = u_string_strdup(connected_to);
        new_cpin->nets->nid = o_current->sid;

#if DEBUG
        printf("Finished creating: %s\n", connected_to);
        printf("netname: %s %s\n", new_cpin->nets->net_name,
               hierarchy_tag);
#endif

        GEDA_FREE(connected_to);
      }

    } else {
      /* no uref, means this is a special component */
    }
    current_pin = strtok(NULL, DELIMITERS);
  }
  GEDA_FREE(net_name);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
s_netattrib_handle (GedaToplevel * pr_current, Object * o_current,
                    NETLIST * netlist, char *hierarchy_tag)
{
  char *value;
  int   counter;

  /* for now just look inside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_inherited_attribs_by_name (o_current,
                                                       "net", counter);
    if (value == NULL)
      break;

    counter++;

    s_netattrib_create_pins (pr_current, o_current,
                             netlist, value, hierarchy_tag);
    GEDA_FREE (value);
  }

  /* now look outside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_attached_attribs_by_name (o_current,
                                                      "net", counter);
    if (value == NULL)
      break;

    counter++;

    s_netattrib_create_pins (pr_current, o_current,
                             netlist, value, hierarchy_tag);
    GEDA_FREE (value);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_netattrib_net_search (Object * o_current, const char *wanted_pin)
{
  char *value             = NULL;
  char *char_ptr          = NULL;
  char *net_name          = NULL;
  char *start_of_pinlist  = NULL;
  char *return_value      = NULL;
  const char *current_pin = NULL;
  int   counter;

  if (o_current == NULL ||
      o_current->complex == NULL)
    return NULL;

  /* for now just look inside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_inherited_attribs_by_name (o_current,
                                                       "net", counter);
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
    value = o_attrib_search_attached_attribs_by_name (o_current,
                                                      "net", counter);
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

    start_of_pinlist = char_ptr + 1;
    current_pin = strtok (start_of_pinlist, DELIMITERS);
    while (current_pin) {
      if (strcmp (current_pin, wanted_pin) == 0) {
        GEDA_FREE (return_value);
        return net_name;
      }
      current_pin = strtok (NULL, DELIMITERS);
    }

    GEDA_FREE (value);
  }

  return return_value;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_netattrib_return_netname(GedaToplevel * pr_current, Object * o_current,
                                 char *pinnumber, char *hierarchy_tag)
{
  const char *current_pin;
  char *netname;
  char *temp_netname;

  current_pin = s_netattrib_connected_string_get_pinnum (pinnumber);
  if (current_pin == NULL) return NULL;

  /* use hierarchy tag here to make this net uniq */
  temp_netname = s_netattrib_net_search(o_current->parent_object,
                                        current_pin);

  netname =
  s_hierarchy_create_netattrib(pr_current, temp_netname,
                               hierarchy_tag);

#if DEBUG
  printf("netname: %s\n", netname);
#endif

  return (netname);
}
