/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: s_check.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#include <config.h>

#include "../include/gsymcheck.h"

/* TODO: Seems like it would just be easier to use g_list length rather than keeping count */
#define ADD_ERROR_MESSAGE(msg) s_current->error_messages = g_list_append(s_current->error_messages, msg); \
                               s_current->error_count++;
#define ADD_INFO_MESSAGE(msg)  s_current->info_messages = g_list_append(s_current->info_messages, message);
#define ADD_WARN_MESSAGE(msg)  s_current->warning_messages = g_list_append(s_current->warning_messages, message); \
                               s_current->warning_count++;
#define ADD_WARN_LOCATION(msg) s_current->warning_messages = g_list_append(s_current->warning_messages, message); \

/* Function prototypes */
static int  s_check_symbol(SYMCHECK *s_current, const GList *obj_list);
static bool s_check_list_has_item(char **list , const char *item);
static bool s_check_is_known_device(const char *device, SYMCHECK *s_current);
static bool s_check_is_valid_directive(const char *string);
static void s_check_symbol_structure(const GList *obj_list, SYMCHECK *s_current);
static void s_check_text (const GList *obj_list, SYMCHECK *s_current);
static void s_check_graphical(const GList *obj_list, SYMCHECK *s_current);
static void s_check_directive(const GList *obj_list, SYMCHECK *s_current);
static void s_check_description(const GList *obj_list, SYMCHECK *s_current);
static void s_check_device(const GList *obj_list, SYMCHECK *s_current);
static void s_check_pinseq(const GList *obj_list, SYMCHECK *s_current);
static void s_check_pintype(const GList *obj_list, SYMCHECK *s_current);
static void s_check_pinnumber(const GList *obj_list, SYMCHECK *s_current);
static void s_check_pin_ongrid(const GList *obj_list, SYMCHECK *s_current);
static void s_check_slotdef(const GList *obj_list, SYMCHECK *s_current);
static void s_check_oldpin(const GList *obj_list, SYMCHECK *s_current);
static void s_check_oldslot(const GList *obj_list, SYMCHECK *s_current);
static void s_check_nets_buses(const GList *obj_list, SYMCHECK *s_current);
static void s_check_connections(const GList *obj_list, SYMCHECK *s_current);
static bool s_check_missing_attribute(GedaObject *object, const char *attribute, SYMCHECK *s_current);
static void s_check_missing_attributes(const GList *obj_list, SYMCHECK *s_current);


/*! \brief Wrapper to execute s_check_symbol for each page
 *  \par Function Description
 *   Calls s_check_symbol for each in page in the top-level,
 *   accumulates and returns the results.
 *
 * \param [in] toplevel  The GedaToplevel object
 *
 * \returns total number findings
 */
int s_check_all(GedaToplevel *toplevel)
{
  SYMCHECK *s_current;

  int total = 0;

  s_current = s_symstruct_init();

  if (s_current) {

    GList *iter;
    GList *pages;

    pages = geda_toplevel_get_pages (toplevel);

    for (iter = pages; iter != NULL; iter = g_list_next(iter)) {

      GList *obj_list;
      Page  *page;

      page     = (Page*)iter->data;
      obj_list = geda_struct_page_get_objects (page);

      if (obj_list) {

        int result  = 0;

        if (verbose_mode && !quiet_mode) {
          u_log_message("\n%s: %s\n", _("Checking"), page->filename);
        }

        s_current->filename = page->filename;

        result = s_check_symbol (s_current, obj_list);

        total =  total + result;
      }
      s_symstruct_reset (s_current);
    }

    /* If there were no findings and not verbose then nothing was logged */
    if (!total && !verbose_mode && !quiet_mode) {
      /* If not quiet mode then tell user */
      u_log_message(_("No errors or warnings were detected.\n"));
    }

    s_symstruct_free (s_current);
  }

  return (total);
}

/*! \brief Check the Symbol
 *  \par Function Description
 *   Passes the object list and data structure for results to each of the
 *   checker (worker) functions and then prints the results and returns
 *   a yes/no result.
 *
 *  \note The check for graphical must be performed before the device
 *        check.
 *
 *  \param [in] s_current Pointer to SYMCHECK structure to store results
 *  \param [in] obj_list  List of all objects associated with the symbol
 *
 *  return
 */
static int
s_check_symbol (SYMCHECK *s_current, const GList *obj_list)
{
  int errors=0, warnings=0;

  /* check for graphical attribute */
  s_check_graphical (obj_list, s_current);

  /* check for directive attribute */
  s_check_directive (obj_list, s_current);

  /* overal symbol structure test */
  s_check_symbol_structure (obj_list, s_current);

  /* test all text elements */
  s_check_text (obj_list, s_current);

  /* check for description attribute */
  s_check_description (obj_list, s_current);

  /* check for device attribute */
  s_check_device (obj_list, s_current);

  /* check for missing attributes */
  s_check_missing_attributes (obj_list, s_current);

  /* check for whether all pins are on grid */
  s_check_pin_ongrid (obj_list, s_current);

  /* check for pinnumber attribute (and multiples) on all pins */
  s_check_pinnumber (obj_list, s_current);

  /* check for pinseq attribute (and multiples) on all pins */
  s_check_pinseq (obj_list, s_current);

  /* check for pintype attribute (and multiples) on all pins */
  s_check_pintype (obj_list, s_current);

  /* check for slotdef attribute on all pins (if numslots exists) */
  s_check_slotdef (obj_list, s_current);

  /* check for old pin#=# attributes */
  s_check_oldpin (obj_list, s_current);

  /* check for old pin#=# attributes */
  s_check_oldslot (obj_list, s_current);

  /* check for nets or buses within the symbol (completely disallowed) */
  s_check_nets_buses (obj_list, s_current);

  /* check for connections with in a symbol (completely disallowed) */
  s_check_connections (obj_list, s_current);

  errors   = s_current->error_count;
  warnings = s_current->warning_count;

  /* now report the info/warnings/errors to the user */
  if (!quiet_mode) {

    /* If not verbose mode then the file name was not printed,
     * so if there were any findings, print the file now...*/
    if (!verbose_mode && (errors > 0 || warnings > 0)) {
      u_log_message("%s: %s\n", _("File"), s_current->filename);
    }

    /* Print out warning messages stored in the structure */
    s_symstruct_print(s_current);

    if (warnings == 0) {

      if (verbose_mode) {
        u_log_message(_("No warnings detected\n"));
      }
    }
    else {

      if (warnings == 1) {
        u_log_message(_("1 warning found"));
      }
      else {
        u_log_message(_("%d warnings found"), warnings);
      }

      if (verbose_mode < 2) {
        u_log_message(" (%s)\n", _("use -vv to view details"));
      }
      else {
        u_log_message("\n");
      }
    }

    /* Print out error messages stored in the structure */
    if (errors == 0) {

      if (verbose_mode || warnings > 0) {

        if (!suppress_mode) {
          u_log_message(_("No errors detected\n"));
        }

        if (warnings && !verbose_mode) {
          u_log_message("\n");
        }
      }
    }
    else {

      if (errors == 1) {

        u_log_message(_("1 error found"));
      }
      else {
        u_log_message(_("%d errors found"), errors);
      }

      if (verbose_mode < 1) {
        u_log_message(_(" (use -v to view details)\n"));
      }

      u_log_message("\n");

    }
  }

  return (errors || warnings) ? 1 : 0;
}

/*!
 * \brief Check if Device Value is a Known type case sensitive
 * \par Function Description
 *  This function is called to perform the string comparisons for
 *  s_check_is_known_device when "strict" mode is someday active.
 *
 * \returns TRUE if a string matching \a item is found in \a list
 */
static bool s_check_list_has_item(char **list, const char *item)
{
  int cur;
  for (cur = 0; list[cur] != NULL; cur++) {
    if (strcmp(item, list[cur]) == 0)
      return TRUE;
  }
  return FALSE;
}

/*!
 * \brief Check if Device Value is a Known type
 * \par Function Description
 *  This function is used when checking device=xxx attributes.
 *  When the device value is not found in the file name then
 *  this function is called to check if the device is a known
 *  type, types listed in the structure "known_devices" and
 *  values containing the character sequence "SPICE" are not
 *  expected to be in the file name.
 *
 * \param [in] device    Attribute string to be checked
 * \param [in] s_current Pointer to current s_symcheck data structure
 *
 * \returns TRUE if device value is known
 */
static bool s_check_is_known_device (const char *device, SYMCHECK *s_current)
{
  bool  strict = FALSE;
  bool  known;

  if (geda_strncmpi(device, "SPICE", 5) == 0)
    return TRUE;

  if (!strict) {
    known = geda_glist_stri_inlist(s_current->known_devices, device);
  }
  else {
    known = geda_glist_str_inlist(s_current->known_devices, device);
  }

  return known;
}

/*!
 * \brief Check if Attribute string is a valid directive
 * \par Function Description
 *  Check if string contains "directive=" or "Directive="
 *  and is an attribute.
 *
 * \note Strings containing prefixes, such as "DRC_Directive="
 *       are valid.
 *
 * \param [in] string Attribute string to be checked
 *
 * \returns TRUE if string is a valid directive attribute
 */
static bool s_check_is_valid_directive(const char *string)
{
  const char *ptr;

  ptr = geda_string_istr(string, "Directive");

  /* If Directive followed by an EQUAL and not equal NULL */
  if (ptr && *ptr + 9 == ASCII_EQUAL_SIGN && *ptr + 10) {
    return TRUE;
  }

  return FALSE;
}

/*!
 * \brief Check a symbol attributes
 * \par Function Description
 *  Checks for "valid", "forbidden", obsolete, and improperly attached
 *  attributes.
 *
 * \param [in] obj_list  List of all object in the symbol.
 * \param [in] s_current Pointer to current s_symcheck data structure
 */
static void s_check_symbol_structure (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
        GList *found;

  char *message;

  char *valid_pin_attributes[] = {"pinlabel", "pintype", "pinseq",
                                  "pinnumber", "electtype", "mechtype",
                                  NULL};

  char *obsolete_attributes[]  = {"email", "label", "uref", NULL};
  char *forbidden_attributes[] = {"name", "netname", "type", NULL};
  /* pin# ?, slot# ? */

  char *redundant_attributes[] = {"documentation", "symversion",
                                  "dist-license", "use-license", NULL};
  found = NULL;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_TEXT) {

      char *name;
      char *value;

      const char *string = geda_text_object_get_string(o_current);

      geda_attrib_string_get_name_value(string, &name, &value);

      if (name != NULL && value != NULL) {

        if (s_current->has_directive) {

          if (s_check_is_valid_directive(string)) {

            GEDA_FREE(name);
            GEDA_FREE(value);

            if (found) {
              geda_glist_free_all(found);
              found = NULL;
            }
            continue;
          }
        }

        if (s_check_list_has_item(forbidden_attributes, name)) {
          message = geda_sprintf (_("Found forbidden %s= attribute: [%s=%s]\n"),
                                     name, name, value);
          ADD_ERROR_MESSAGE(message);
        }
        else if (s_check_list_has_item(obsolete_attributes, name)) {
          message = geda_sprintf (_("Found obsolete %s= attribute: [%s=%s]\n"),
                                     name, name, value);
          ADD_WARN_MESSAGE(message);
        }
        else if (s_check_list_has_item(valid_pin_attributes, name)) {
          if (o_current->attached_to == NULL
            || o_current->attached_to->type != OBJ_PIN)
          {
            const char *msg = _("Found misplaced pin attribute");
            message = geda_sprintf ("%s: [%s=%s]\n", msg, name, value);
            ADD_ERROR_MESSAGE(message);
          }
        }
        else if (!geda_glist_str_inlist(s_current->valid_attributes, name)) {
          message = geda_sprintf (_("Found unknown %s= attribute: [%s=%s]\n"),
                                     name, name, value);
          ADD_WARN_MESSAGE(message);
        }
        else if (o_current->attached_to != NULL) {
          /* Allow attributes to be attached to text objects */
          if (!GEDA_IS_TEXT(o_current->attached_to)) {
            const char *msg = _("Found incorrectly attached attribute");
            message = geda_sprintf ("%s: [%s=%s]\n", msg, name, value);
            ADD_ERROR_MESSAGE(message);
          }
        }

        if (!found) {
          found = g_list_prepend(NULL, geda_strdup(name));
        }
        else if (geda_glist_str_inlist(found, name)) {
            if (s_check_list_has_item(redundant_attributes, name)) {
              message = geda_sprintf (_("Found redundant %s= attribute: [%s=%s]\n"),
              name, name, value);
              ADD_WARN_MESSAGE(message);
            }
        }
        else {
          found = g_list_prepend(found, geda_strdup(name));
        }
        GEDA_FREE(name);
        GEDA_FREE(value);
      }
      else { /* object is not an attribute */
        if (o_current->show_name_value != SHOW_NAME_VALUE) {
          const char *msg = _("Found a simple text object with only SHOW_NAME or SHOW_VALUE set");
          message = geda_sprintf ("%s [%s]\n", msg, string);
          ADD_WARN_MESSAGE(message);
        }
      }
    }
  }
  if (found) {
    geda_glist_free_all(found);
  }
}

/*! \brief Check Texts attributes
 *  \par Function Description
 *  Checks the text of Text objects for un-escaped backslash characters
 *  and unbalanced over-bar markers.
 *
 * \param [in]     obj_list  List of all object in the symbol.
 * \param [in,out] s_current Pointer to current s_symcheck data structure
 */
static void s_check_text (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  const char  *text_string, *ptr;
  char        *message;
  gunichar     current_char;
  bool         overbar_started, escape, leave_parser;

  for (iter = obj_list; iter != NULL; iter = g_list_next(iter)) {

    GedaObject *o_current = iter->data;

    if (o_current->type != OBJ_TEXT)
      continue;

    overbar_started = escape = leave_parser = FALSE;
    text_string = geda_text_object_get_string(o_current);

    for (ptr = text_string;
         ptr != NULL && !leave_parser;
         ptr = g_utf8_find_next_char (ptr, NULL)) {

      current_char = g_utf8_get_char_validated (ptr, -1);

      /* state machine to interpret the string:
       * there are two independant state variables overbar_started and escape.
       */
      switch (current_char) {
        case '\0':
          /* end of the string */
          leave_parser = TRUE;
          break;
        case '\\':
          if (escape == TRUE) {
            escape = FALSE;
          } else {
            escape = TRUE;
          }
          break;
        case '_':
          if (escape == TRUE) {
            escape = FALSE;
            if (overbar_started == TRUE) {
              overbar_started = FALSE;
            } else {
              overbar_started = TRUE;
            }
          }
          break;
        default:
          if (escape == TRUE) {
            const char *msg = _("Found text with a '\\' in it: consider to escape it with");
            message = geda_sprintf ("%s '\\\\' [%s]\n", msg, text_string);
            ADD_WARN_MESSAGE(message);
            escape = FALSE;
          }
      }
    }

    if (escape == TRUE) {
      const char *msg = _("Found text with a trailing '\': consider to escape it with");
      message = geda_sprintf ("%s '\\\\' [%s]\n", msg, text_string);
      ADD_WARN_MESSAGE(message);
    }

    if (overbar_started == TRUE) {
      const char *msg = _("Found text with unbalanced overbar markers");
      message = geda_sprintf ("%s '\\_' [%s]\n", msg, text_string);
      ADD_WARN_MESSAGE(message);
    }
  }
}

/*! \brief Check if symbol is Graphical object
 *  \par Function Description
 *   Checks for the existence of graphical attribute and set flag
 *   in \a s_current if found. Does not set any error or warnings.
 *
 * \param [in]     obj_list  List of all object in the symbol.
 * \param [in,out] s_current Pointer to current s_symcheck data structure
 */
static void s_check_graphical (const GList *obj_list, SYMCHECK *s_current)
{
  char *temp;

  /* look for special graphical tag */
  temp = geda_attrib_search_floating_by_name (obj_list, "graphical", 0);

  if (temp) {
    s_current->graphical_symbol=TRUE;
    GEDA_FREE(temp);
  }
}

/*! \brief Check if object in list have a connection and report error
 *  \par Function Description
 *  symbol files should not have internal connections.
 *
 * \param [in]     obj_list  List of all object in the symbol.
 * \param [in,out] s_current Pointer to current s_symcheck data structure
 */
static void s_check_connections (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  char *message;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (geda_object_get_conn_list(o_current)) {

      message = geda_strdup (_("Found a connection inside a symbol\n"));
      ADD_ERROR_MESSAGE(message);
      s_current->found_connection++;
    }
  }
}

/*! \brief Check if symbol has a Directive
 *  \par Function Description
 *   Checks for the existence of directive attribute and set flag
 *   in \a s_current if found. Does not set any error or warnings.
 */
static void s_check_directive (const GList *obj_list, SYMCHECK *s_current)
{
  const char  *directive = "Directive";
  const GList *iter;

  /* look for special directive tag */

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (geda_object_get_is_valid_attribute(o_current)) {

      /* Check is attribute has directive */
      const char *string = geda_text_object_get_string(o_current);

      if (geda_stristr(string, directive) >= 0) {
        s_current->has_directive = TRUE;
      }
    }
  }
}

/*! \brief Performs checks of the description= attribute
 *  \par Function Description
 *   Checks for:
 *  <DL>
 *    <DT>Existence of Description attributes</DT>
 *    <DT>Duplicate description attribute values</DT>
 *  </DL>
 */
static void s_check_description (const GList *obj_list, SYMCHECK *s_current)
{
  char *message;
  char *string;
  int   counter;

  /* look for special description tag */
  for (counter = 0;
      (string = geda_attrib_search_floating_by_name (obj_list, "description", counter)) != NULL;
       counter++)
  {
    if (counter == 0) { /* collect the first appearance */
      s_current->description_attribute = geda_strdup (string);
      message = geda_sprintf ("%s=%s\n", _("Found description"), string);
      ADD_INFO_MESSAGE(message);
    }
    else { /* counter must be > 0 */

      if (strlen(string) == strlen(s_current->description_attribute)) {
        if (strcmp(s_current->description_attribute, string) == 0) {
          message = geda_sprintf (_("Found duplicate description=%s attributes\n"), string);
          ADD_ERROR_MESSAGE(message);
          s_current->duplicate_descr_attribute++;
        }
      }
    }
    GEDA_FREE(string);
  }

  if (counter == 0) {
    s_current->missing_descr_attrib = TRUE;
    message = geda_strdup (_("Missing description= attribute\n"));
    ADD_ERROR_MESSAGE(message);
  }
}

/*! \brief Performs checks of the device= attribute
 *  \par Function Description
 *   Checks for:
 *  <DL>
 *    <DT>Existence of device attributes</DT>
 *    <DT>Duplicate device attribute values</DT>
 *    <DT>Multiple device attributes</DT>
 *    <DT>device=none if graphical</DT>
 *    <DT>Present of device value in symbol filename</DT>
 *  </DL>
 */
static void s_check_device (const GList *obj_list, SYMCHECK *s_current)
{
  char *string;
  char *message;
  int   counter;
  bool  graphical;
  bool  not_directive;

  graphical     =  s_current->graphical_symbol;
  not_directive = !s_current->has_directive;

  /* search for device attributes */

  for (counter = 0;
      (string = geda_attrib_search_floating_by_name (obj_list, "device", counter)) != NULL;
       counter++)
  {
    if (counter == 0) { /* collect the first appearance */
      s_current->device_attribute = geda_strdup (string);
      message = geda_sprintf ("%s=%s\n", _("Found device"), string);
      ADD_INFO_MESSAGE(message);
    }
    else { /* counter must be > 0 */

      if (strlen(string) == strlen(s_current->device_attribute)) {
        if (strcmp(s_current->device_attribute, string) == 0) {
          message = geda_sprintf (_("Found duplicate device=%s attributes\n"), string);
          ADD_ERROR_MESSAGE(message);
          s_current->duplicate_device_attrib++;
        }
        else {
          if (not_directive) {
            message = geda_strdup (_("Found multiple device= attributes\n"));
            ADD_WARN_MESSAGE(message);
            s_current->multiple_device_attrib++;
          }
        }
      }
      else {
        if (not_directive) {
          message = geda_strdup (_("Found multiple device= attributes\n"));
          ADD_WARN_MESSAGE(message);
          s_current->multiple_device_attrib++;
        }
      }
    }
    GEDA_FREE(string);
  }

  if (counter == 0) {
    /* did not find device= attribute */
    message = geda_strdup (_("Missing device= attribute\n"));
    ADD_ERROR_MESSAGE(message);
    /* s_current->device_attribute was initialized to NULL */
  }
  else {

    string = s_current->device_attribute;

    /* check for device = none for graphical symbols */
    if (string && graphical && (strcmp(string, "none") == 0)) {
      s_current->device_attribute_incorrect=FALSE;
      message = geda_strdup (_("Found graphical symbol, device=none\n"));
      ADD_INFO_MESSAGE(message);
    }
    else if (graphical && not_directive) {
      /* If graphical "device" is not a "Directive" then might be an error */
      s_current->device_attribute_incorrect=TRUE;
      message = geda_strdup (_("Found graphical symbol, device= should be set to none\n"));
      ADD_WARN_MESSAGE(message);
    }

    /* If attribute is not graphical and does not contain a directive */
    if (string && !graphical && not_directive) {

      /* then is an ordinary device attribute, check the file name */
      if (geda_stristr(s_current->filename, string) < 0) {

        /* And if not a known device type */
        if (!s_check_is_known_device(string, s_current)) {
          s_current->device_attribute_incorrect=TRUE;
          message = geda_strconcat (_("Device not found in symbol filename"), " \"", string, "\"\n", NULL);
          ADD_WARN_MESSAGE(message);
        }
      }
    }

    /* string is not freed here, points to s_current->device_attribute */
  }
}

/*!
 * \brief  Performs checks of the pinnumber= attribute
 * \par Function Description
 *  Checks for:
 *  <DL>
 *    <DT>Existence of pin using number assigned by a net attribute</DT>
 *    <DT>Duplicate pinnumbers</DT>
 *    <DT>Multiple pinnumber attributes on the same pin</DT>
 *    <DT>Pins Missing a pinnumber attribute</DT>
 *  </DL>
 */
static void s_check_pinnumber (const GList *obj_list, SYMCHECK *s_current)
{
  int missing_pinnumber_attrib_sum=0;
  int multiple_pinnumber_attrib_sum=0;
  int counter=0;
  int i;

  char **pin_tokens;
  GList *net_numbers = NULL;
  GList *pin_numbers = NULL;
  GList *cur         = NULL;
  GList *cur2        = NULL;
  const GList *iter;
  char *message;
  char *net = NULL;

  /* collect all net pins */
  for (counter = 0; (net = geda_attrib_search_floating_by_name (obj_list, "net", counter)) != NULL; counter++)
  {
    char **net_tokens;

    message = geda_sprintf (_("Found net=%s attribute\n"), net);
    ADD_INFO_MESSAGE(message);

    net_tokens = g_strsplit(net,":", -1);
    /* length of net tokens have to be 2 */
    if (net_tokens[1] == NULL) {
      message = geda_sprintf (_("Bad net= attribute [net=%s]\n"), net);
      ADD_ERROR_MESSAGE(message);
      g_strfreev(net_tokens);
      continue;
    } else if (net_tokens[2] != NULL) { /* more than 2 tokens */
      message = geda_sprintf (_("Bad net= attribute [net=%s]\n"), net);
      ADD_ERROR_MESSAGE(message);
      g_strfreev(net_tokens);
      continue;
    }

    pin_tokens = g_strsplit(net_tokens[1],",",-1);

    for (i = 0; pin_tokens[i] != NULL; i++) {
      net_numbers = g_list_append(net_numbers, geda_strdup(pin_tokens[i]));
      message = geda_sprintf (_("Found pin number %s in net attribute\n"),
                              pin_tokens[i]);
      ADD_INFO_MESSAGE(message);
      s_current->numnetpins++;
    }

    GEDA_FREE(net);
    g_strfreev(net_tokens);
    g_strfreev(pin_tokens);
  }

  /* check for duplicate net pin numbers */
  net_numbers = g_list_sort(net_numbers, (GCompareFunc)strcmp);

  for (cur = net_numbers; cur != NULL && g_list_next(cur) != NULL; NEXT(cur)) {

    if (strcmp((char*)cur->data, (char*) cur->next->data) == 0) {
      const char *msg = _("Found duplicate pin in net= attributes");
      message = geda_sprintf ("%s [%s]\n", msg, cur->data);
      ADD_ERROR_MESSAGE(message);
    }

    if (strcmp((char*) cur->data, "0") == 0) {
      message = geda_strdup ("Found pinnumber 0 in net= attribute\n");
      ADD_ERROR_MESSAGE(message);
    }
  }

  /* collect all pin numbers */
  for (iter = obj_list; iter != NULL; NEXT(iter)) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      char *string;

      s_current->numpins++;

      missing_pinnumber_attrib_sum = 0;
      multiple_pinnumber_attrib_sum = 0;

      for (counter = 0; (string = geda_attrib_search_object_by_name (o_current, "pinnumber", counter)) != NULL; counter++)
      {

        message = geda_strdup (_("Found pinnumber 0 in net= attribute\n"));
        ADD_INFO_MESSAGE(message);

        if (counter == 0) { /* collect the first appearance */
          pin_numbers = g_list_append(pin_numbers, string);
        }
        if (counter >= 1) {
          message = geda_sprintf (_("Found multiple pinnumber=%s attributes on one pin\n"), string);
          ADD_ERROR_MESSAGE(message);
          multiple_pinnumber_attrib_sum++;
          GEDA_FREE(string);
        }
      }

      if (counter == 0) {
        message = geda_strdup (_("Missing pinnumber= attribute\n"));
        ADD_ERROR_MESSAGE(message);
        missing_pinnumber_attrib_sum++;
      }

      s_current->missing_pinnumber_attrib += missing_pinnumber_attrib_sum;
      s_current->multiple_pinnumber_attrib += multiple_pinnumber_attrib_sum;
    }
  }

  /* check for duplicate pinlabel numbers */
  pin_numbers = g_list_sort(pin_numbers, (GCompareFunc)strcmp);

  for (cur = pin_numbers; cur != NULL && cur->next != NULL; cur = cur->next)
  {
    if (strcmp((char*)cur->data, (char*) cur->next->data) == 0) {
      message = geda_sprintf (_("Found duplicate pinnumber=%s attribute in the symbol\n"), cur->data);
      ADD_ERROR_MESSAGE(message);
      s_current->duplicate_pinnumber_attrib++;
    }
    if (strcmp((char*)cur->data, "0") == 0) {
      message = geda_strdup (_("Found pinnumber=0 attribute\n"));
      ADD_ERROR_MESSAGE(message);
    }
  }

  /* Check for all pins that are in both lists and print a message.
   * Sometimes this is useful and sometimes it's an error. */

  cur  = net_numbers;
  cur2 = pin_numbers;

  while (cur != NULL && cur2 != NULL) {

    i = strcmp((char*)cur->data, (char*)cur2->data);

    if (i == 0) {

      if (!quiet_mode && verbose_mode > 0) {

        const char *msg = _("Notice: net attribute using defined pin number");
        u_log_message("%s [%s]\n", msg, (char*)cur->data);
      }
      cur = g_list_next(cur);
    } else if ( i > 0 ) {
      cur2 = g_list_next(cur2);

    } else { /* i < 0 */
      cur = g_list_next(cur);
    }
  }
  /* FIXME: this is not correct if a pinnumber is defined as pinnumber and
   *        inside a net. We have to calculate the union set */
  message = geda_sprintf (_("Found %d pins inside symbol\n"), s_current->numpins + s_current->numnetpins);
  ADD_INFO_MESSAGE(message);

  geda_glist_free_full(pin_numbers, g_free);
  geda_glist_free_full(net_numbers, g_free);
}

/*! \brief Performs checks of the pinseq= attribute
 *  \par Function Description
 *   Checks for:
 *  <DL>
 *    <DT>Pins missing a pinseq attributes</DT>
 *    <DT>pinseq attributes with value zero</DT>
 *    <DT>Duplicate pinseq attribute values</DT>
 *    <DT>Multiple pinseq attributes</DT>
 *  </DL>
 */
static void s_check_pinseq (const GList *obj_list, SYMCHECK *s_current)
{
  char *number;
  char *message;
  char *string;

  int found_first                = FALSE;
  int missing_pinseq_attrib_sum  = 0;
  int multiple_pinseq_attrib_sum = 0;
  int counter                    = 0;

         GList *found_numbers    = NULL;
         GList *ptr1             = NULL;
  const  GList *iter;

  /* Loop through object list and look for pins */
  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      missing_pinseq_attrib_sum = 0;
      multiple_pinseq_attrib_sum = 0;
      found_first = FALSE;
      counter = 0;

      string = geda_attrib_search_object_by_name(o_current, "pinseq", counter);

      if (!string) {

        message = geda_strdup (_("Missing pinseq= attribute\n"));
        ADD_ERROR_MESSAGE(message);
        missing_pinseq_attrib_sum++;
      }

      while (string) {

        message = geda_sprintf (_("Found pinseq=%s attribute\n"), string);
        ADD_INFO_MESSAGE(message);

        number = geda_strdup (string);

        if (strcmp(number, "0") == 0) {
          message = geda_strdup (_("Found pinseq=0 attribute\n"));
          ADD_ERROR_MESSAGE(message);
        }

        if (found_first) {
          message = geda_sprintf (
            _("Found multiple pinseq=%s attributes on one pin\n"), string);
            ADD_ERROR_MESSAGE(message);
            multiple_pinseq_attrib_sum++;
        }

        GEDA_FREE(string);

        /* this is the first attribute found */
        if (!found_first) {
          found_numbers = g_list_append(found_numbers, number);
          found_first=TRUE;
        }
        else {
          GEDA_FREE(number);
        }

        counter++;
        string = geda_attrib_search_object_by_name (o_current, "pinseq",
                                                    counter);
      }

      s_current->missing_pinseq_attrib += missing_pinseq_attrib_sum;
      s_current->multiple_pinseq_attrib += multiple_pinseq_attrib_sum;
    }
  }

  ptr1 = found_numbers;

  while (ptr1) {

    GList *ptr2;
    char  *string = (char*)ptr1->data;
    int    found  = 0;

    ptr2 = found_numbers;

    while(ptr2 && string) {

      char *current = (char*) ptr2->data;

      if (current && strcmp(string, current) == 0) {
        found++;
      }

      ptr2 = g_list_next(ptr2);
    }

    if (found > 1) {

      message = geda_sprintf (
        _("Found duplicate pinseq=%s attribute in the symbol\n"), string);
        ADD_ERROR_MESSAGE(message);
        s_current->duplicate_pinseq_attrib++;
    }

    ptr1 = g_list_next(ptr1);
  }

  ptr1 = found_numbers;

  while (ptr1) {
    GEDA_FREE(ptr1->data);
    ptr1 = g_list_next(ptr1);
  }

  g_list_free(found_numbers);
}

/*!
 * \brief Check Pin Type Attributes
 * \par Function Description
 *  Interates \a obj_list, checking all pins for valid pin type
 *  attribute value.
 */
static void s_check_pintype (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      int   counter;
      bool  done;

      counter = 0;
      done    = FALSE;

      do {

        char *pintype;

        pintype = geda_attrib_search_object_by_name (o_current, "pintype", counter);

        if (pintype != NULL) {

          char *message;

          message = geda_sprintf(_("Found pintype=%s attribute\n"), pintype);
          ADD_INFO_MESSAGE(message);

          if (geda_pin_lookup_etype(pintype) == PIN_ELECT_VOID) {

            const char *msg_ukn = _("Unknown pintype");
            const char *msg_rib = _("attribute");
            const char *msg_chk = _("check pin number");

            char *pin_num;

            pin_num = geda_attrib_search_object_by_name (o_current, "pinnumber", 0);

            if (pin_num) {
              message = geda_sprintf ("%s=%s %s, %s <%s>\n", msg_ukn, pintype, msg_rib, msg_chk, pin_num);
              GEDA_FREE(pin_num);
            }
            else {
              message = geda_sprintf ("%s=%s %s\n", msg_ukn, pintype, msg_rib);
            }
            ADD_ERROR_MESSAGE(message);
          }

          GEDA_FREE(pintype);
        }
        else {
          done = TRUE;
        }

        counter++;

      } while (!done);
    }
  }
}

/*!
 * \brief Check if Pins are on Grid
 * \par Function Description
 *  Check whether all pins are on 100 grid units.
 */
static void s_check_pin_ongrid (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      char *message;

      int x1, x2, y1, y2;

      x1 = o_current->line->x[0];
      y1 = o_current->line->y[0];
      x2 = o_current->line->x[1];
      y2 = o_current->line->y[1];

      if (x1 % 100 != 0 || y1 % 100 != 0) {
        message = geda_sprintf(_("Found offgrid pin at location (x1=%d,y1=%d)\n"), x1, y1);
        /* error if it is the whichend, warning if not */
        if (o_current->pin->whichend == 0) {
          ADD_ERROR_MESSAGE(message);
        }
        else {
          ADD_WARN_MESSAGE(message);
        }
      }

      if (x2 % 100 != 0 || y2 % 100 != 0) {
        message = geda_sprintf(_("Found offgrid pin at location (x2=%d,y2=%d)\n"), x2, y2);
        /* error when whichend, warning if not */
        if (o_current->pin->whichend != 0) {
          ADD_ERROR_MESSAGE(message);
        }
        else {
          ADD_WARN_MESSAGE(message);
        }
      }
    }
  }
}

/*!
 * \brief Check Slot Definition
 * \par Function Description
 *  Check for slotdef attribute on all pins if numslots exists.
 */
static void s_check_slotdef (const GList *obj_list, SYMCHECK *s_current)
{
  char *value;
  char *slotdef;
  char *temp    = NULL;
  char  numslots_str[10];
  int   slot;
  int   i,j;
  char *message;
  char  tempstr1[10];

  /*  pinlist will store the pin definitions for each slot */
  /* example: pinlist[0] = 3,2,8,4,1 ; pinlist[1] = 5,6,8,4,7 */
  char **pinlist     = NULL;
  bool error_parsing = FALSE;
  bool redundant     = FALSE;

  /* look for numslots to see if this symbol has slotting info */
  value = geda_attrib_search_floating_by_name (obj_list, "numslots", 0);

  if (!value) {
    /* Since there's no numslots= attribute, don't check slotting at all. */
    return;
  }

  /* The numslots member was initialized to -1 to check for redundancy */
  if (s_current->numslots != -1) {
    redundant = TRUE;
  }

  s_current->numslots = atoi(value);
  sprintf(numslots_str, "%d", s_current->numslots);
  GEDA_FREE(value);

  if (redundant) {
    message = geda_sprintf (_("Found redundant numslots=%s attribute\n"),
                               numslots_str);
    ADD_WARN_MESSAGE(message);
  }
  else {
    message = geda_sprintf (_("Found numslots=%s attribute\n"), numslots_str);
    ADD_INFO_MESSAGE(message);
  }

  if (s_current->numslots == 0) {
    message = geda_strdup (_("numslots set to zero, symbol does not have slots\n"));
    ADD_INFO_MESSAGE(message);
    return;
  }

  pinlist = (char**)GEDA_MEM_ALLOC0(sizeof(*pinlist) * s_current->numslots);

  i = 0;
  /* get the slotdef attribute */
  slotdef = geda_attrib_search_floating_by_name (obj_list, "slotdef", 0);

  while ((slotdef != NULL) && (!error_parsing)) {

    char *pins;
    char *slotnum;

    if (i > s_current->numslots-1) {

      sprintf(tempstr1, "%d", i + 1); /* i starts at zero */
      message = geda_sprintf (
        _("Found %s slotdef= attributes. Expecting %s slotdef= attributes\n"),
          tempstr1, numslots_str);
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
    }

    message = geda_sprintf (_("Found slotdef=%s attribute\n"), slotdef);
    ADD_INFO_MESSAGE(message);

    slotnum = geda_utility_string_split(slotdef, ':', 0);

    if (!slotnum) {

      message = geda_sprintf (_("Invalid slotdef=%s attributes\n"), slotdef);
      ADD_ERROR_MESSAGE(message);
      s_current->slotting_errors++;
      error_parsing = TRUE;
      continue;
    }

    if (strcmp(slotnum, "0") == 0) {
      const char *msg = _("Found a zero slot in slotdef");
      message = geda_sprintf ("%s=%s\n", msg, slotdef);
      ADD_ERROR_MESSAGE(message);
    }

    slot = atoi(slotnum);
    GEDA_FREE(slotnum);

    /* make sure that the slot # is less than the number of slots */
    if (slot > s_current->numslots) {
      sprintf(tempstr1, "%d", slot);
      message = geda_sprintf
      (_("Slot %s is larger then the maximum number of slots (%s)\n"),
       tempstr1, numslots_str);
      ADD_ERROR_MESSAGE(message);
      s_current->slotting_errors++;
    }

    /* skip over the : */
    pins = strchr(slotdef, ':');
    if (!pins) {
      message = geda_sprintf (_("Invalid slotdef=%s attributes\n"), slotdef);
      ADD_ERROR_MESSAGE(message);
      s_current->slotting_errors++;
      error_parsing = TRUE;
      continue;
    }

    pins++;  /* get past that : */
    if (!pins) {
      message = geda_sprintf (_("Invalid slotdef=%s attributes\n"), slotdef);
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
        error_parsing = TRUE;
        continue;
    }

    if (*pins == '\0') {
      message = geda_sprintf (_("Invalid slotdef=%s attributes\n"), slotdef);
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
        error_parsing = TRUE;
        continue;
    }

    if ((slot > 0) && (slot <= s_current->numslots)) {

      if (pinlist[slot-1]) {
        const char *msg = _("Duplicate slot number in slotdef");
        message = geda_sprintf ("%s=%s\n", msg, slotdef);
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
      }
      else {
        pinlist[slot-1] = geda_sprintf(",%s,", pins);
      }
    }

    j = 0;
    do {
      if (temp) {
        GEDA_FREE(temp);
        temp = NULL;
      }

      temp = geda_utility_string_split(pins, ',', j);

      if (!temp && j < s_current->numpins) {
        const char *msg = _("Not enough pins in slotdef");
        message = geda_sprintf ("%s=%s\n", msg, slotdef);
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
        break;
      }

      if (j > s_current->numpins) {
        const char *msg = _("Too many pins in slotdef");
        message = geda_sprintf ("%s=%s\n", msg, slotdef);
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
        GEDA_FREE(temp);
        temp = NULL;
        break;
      }

      if (temp && strcmp(temp, "0") == 0) {
        const char *msg = _("Found a zero pin in slotdef");
        message = geda_sprintf ("%s=%s\n", msg, slotdef);
        ADD_ERROR_MESSAGE(message);
      }

      j++;
    } while (temp);

    GEDA_FREE(temp);

    g_free(slotdef);

    i++;
    slotdef = geda_attrib_search_floating_by_name (obj_list, "slotdef", i);
  }

  if (!slotdef && i < s_current->numslots) {
    message = geda_sprintf (
      _("Missing slotdef= (there should be %s slotdef= attributes)\n"), numslots_str);
      ADD_ERROR_MESSAGE(message);
      s_current->slotting_errors++;
  }
  else {

    int  errors_found = 0;

    /* Validate that pinslist does not contain a null entry. If any entry */
    /* is null, that means the slotdef= attribute was malformed to start */
    /* with. */
    for (i = 0; i < s_current->numslots; i++) {
      if (pinlist[i] == NULL) {
        errors_found++;
      }
    }

    if (errors_found) {
      message = geda_sprintf(
        _("Malformed slotdef= (the format is #:#,#,#,...)\n"));
        ADD_ERROR_MESSAGE(message);
        s_current->slotting_errors++;
    }
    else {

      /* Compare each pin with the rest */
      s_current->numslotpins = 0;

      for (i = 0; i < s_current->numslots; i++) {

        int n,m;

        for (n = 1; n <= s_current->numpins; n++) {

          /* Get the number of one pin */
          char *pin = geda_utility_string_split(pinlist[i], ',', n);

          if (pin && *pin) {

            int match = FALSE;

            for (j = i - 1; j >= 0 && !match; j--) {

              for (m = 1; m <= s_current->numpins && !match; m++) {

                /* Get the number of the other pin */
                char *cmp = geda_utility_string_split(pinlist[j], ',', m);

                if (cmp && *cmp) {
                  match = (0 == strcmp (pin, cmp));
                  GEDA_FREE(cmp);
                }
              }
            }

            if (!match) {
              /* If they don't match, then increase the number of pins */
              s_current->numslotpins++;
            }
            GEDA_FREE(pin);
          }
        }
      }

      message = geda_sprintf (_("Found %d distinct pins in slots\n"),
                                   s_current->numslotpins);
      ADD_INFO_MESSAGE(message);
    }
  }

  GEDA_FREE(slotdef);
  if (pinlist) {
    /* Free the pinlist */
    for (i = 0; i < s_current->numslots; i++) {
      GEDA_FREE(pinlist[i]);
    }
    GEDA_FREE(pinlist);
  }

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void s_check_oldpin (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;

  int found_old      = FALSE;
  int number_counter = 0;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_TEXT) {

      const char *string = geda_text_object_get_string(o_current);

      if (strstr(string, "pin")) {

        /* skip over "pin" */
        const char *ptr = string + 3;

        found_old = FALSE;
        number_counter = 0;

        while (ptr && *ptr > '0' && *ptr < '9') {
          number_counter++;
          ptr++;
        }

        if (ptr && *ptr == '=') {
          found_old++;
        }

        if (!ptr)
          continue;

        /* found no numbers inbetween pin and = */
        if (number_counter == 0)
          continue;

        /* skip over = char */
        ptr++;

        while (*ptr > '0' && *ptr < '9') {
          ptr++;
        }

        if (*ptr == '\0') {
          found_old++;
        }

        /* 2 matches -> number found after pin and only numbers after = sign */
        if (found_old == 2) {

          char *message;

          message = geda_sprintf (_("Found old pin#=# attribute: %s\n"), string);

          ADD_ERROR_MESSAGE(message);
          s_current->found_oldpin_attrib += found_old;
        }
      }
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void s_check_oldslot (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  int   found_old      = FALSE;
  int   number_counter = 0;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_TEXT) {

      const char *string = geda_text_object_get_string(o_current);

      if (strstr(string, "slot")) {

        /* skip over "slot" */
        const char *ptr = string + 4;

        found_old = FALSE;
        number_counter = 0;

        while (ptr && *ptr > '0' && *ptr < '9') {
          number_counter++;
          ptr++;
        }

        if (ptr && *ptr == '=') {
          found_old++;
        }

        /* found no numbers inbetween pin and = */
        if (!ptr || number_counter == 0)
          continue;

        /* skip over = char */
        ptr++;

        while (((*ptr > '0') && (*ptr < '9')) || (*ptr == ',')) {
          ptr++;
        }

        if (*ptr == '\0') {
          found_old++;
        }

        /* 2 matches -> number found after slot and only numbers after = */
        if (found_old == 2) {

          const char *msg = _("Found old slot#=# attribute");
          char *message;

          message = geda_sprintf ("%s: %s\n", msg, string);

          ADD_ERROR_MESSAGE(message);
          s_current->found_oldslot_attrib += found_old;
        }
      }
    }
  }
}

/*!
 * \brief Check if a Specific Attribute is Missing
 * \par Function Description
 *  Helper for s_check_missing_attributes to check for pinlabel
 *  and pintype attributes.
 *
 * \param [in]     object     The Object to be checked
 * \param [in]     attribute  The attribute to search for.
 * \param [in,out] s_current  Pointer to current s_symcheck data structure
 */
static bool s_check_missing_attribute(GedaObject *object, const char *attribute, SYMCHECK *s_current)
{
  char *string;
  int   found;
  int   counter=0;
  char *message;

  string = geda_attrib_search_object_by_name (object, attribute, counter);

  if (!string) {
    message = geda_sprintf (_("Missing %s= attribute\n"), attribute);
    ADD_WARN_MESSAGE(message);
    found=FALSE;
  }
  else {

    /* this is the first time attribute was found */
    message = geda_sprintf (_("Found %s=%s attribute\n"), attribute, string);
    ADD_INFO_MESSAGE(message);
    GEDA_FREE(string);
    found = TRUE;
    counter++;
    string = geda_attrib_search_object_by_name (object, attribute, counter);
  }

  while (string) {

    message = geda_sprintf (_("Found multiple %s=%s attributes\n"), attribute, string);
    ADD_ERROR_MESSAGE(message);

    GEDA_FREE(string);
    counter++;
    string = geda_attrib_search_object_by_name (object, attribute, counter);
  }

  return found;
}

/*!
 * \brief Check for Missing Attributes
 * \par Function Description
 *  Use s_check_missing_attribute to search for pinlabel and pintype
 *  attributes. Reports an error if there are multiple footprint or
 *  refdes attributes, issues a warning for each of the following
 *  attributes when they are not found:
 *
 *  <DL>
 *    <DT>pinlabel</DT>
 *    <DT>pintype</DT>
 *    <DT>footprint</DT>
 *    <DT>refdes</DT>
 *  </DL>
 */
void s_check_missing_attributes (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  char *message;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      int missing;

      missing  = !s_check_missing_attribute(o_current, "pinlabel", s_current);
      missing += !s_check_missing_attribute(o_current, "pintype", s_current);

      if (missing) {

        char *pin;

        pin = geda_attrib_search_object_by_name (o_current, "pinnumber", 0);
        if (pin) {
          const char *msg = _("Check pin number");
          message = geda_sprintf ("%s=<%s>\n", msg, pin);
          ADD_WARN_LOCATION(message);
          GEDA_FREE(pin);
        }
        else {
          pin = geda_attrib_search_object_by_name (o_current, "pinseq", 0);
          if (pin) {
            const char *msg = _("Check pin sequence");
            message = geda_sprintf ("%s=<%s>\n",msg, pin);
            ADD_WARN_LOCATION(message);
            GEDA_FREE(pin);
          }
        }
      }
    }

    if (o_current->type == OBJ_TEXT) {

      const char *string = geda_text_object_get_string(o_current);

      if (strstr(string, "footprint=")) {
        message = geda_sprintf (_("Found %s attribute\n"), string);
        ADD_INFO_MESSAGE(message);;
        s_current->found_footprint++;
      }

      if (strstr(string, "refdes=")) {
        message = geda_sprintf (_("Found %s attribute\n"), string);
        ADD_INFO_MESSAGE(message);
        s_current->found_refdes++;
      }
    }
  }

  if (s_current->found_footprint == 0) {
    message = geda_strdup (_("Missing footprint= attribute\n"));
    ADD_WARN_MESSAGE(message);
  }

  if (s_current->found_footprint > 1) {
    message = geda_strdup (_("Multiple footprint= attributes found\n"));
    ADD_ERROR_MESSAGE(message);
  }

  if (s_current->found_refdes == 0) {
    message = geda_strdup (_("Missing refdes= attribute\n"));
    ADD_WARN_MESSAGE(message);
  }

  if (s_current->found_refdes > 1) {
    message = geda_strdup (_("Multiple refdes= attributes found\n"));
    ADD_ERROR_MESSAGE(message);
  }
}

/*! \brief Check for Net or Bus objects in list and report errors
 *  \par Function Description
 *   Net and Bus object are not allowed inside of symbols
 */
static void s_check_nets_buses (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;

  for (iter = obj_list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;
    char *message;

    if (o_current->type == OBJ_NET) {

      message = geda_strdup (_("Found a net inside a symbol\n"));
      ADD_ERROR_MESSAGE(message);
      s_current->found_net++;
    }

    if (o_current->type == OBJ_BUS) {

      message = geda_strdup (_("Found a bus inside a symbol\n"));
      ADD_ERROR_MESSAGE(message);
      s_current->found_bus++;
    }
  }
}
