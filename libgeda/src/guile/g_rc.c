/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
/*! \file g_rc.c
 *  \brief Process Scheme initialization file data.
 *
 * Contains functions to open, parse and manage Scheme initialization
 * (RC) files.
 */

#include <config.h>
#include <stdio.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <libgeda_priv.h>

/**   \defgroup Libgeda-RC-Handlers Libgeda RC Handlers
 *  @{\par
 *  Contains functions to parse initialization (RC) files.
 */

/*! \brief This function processes the component-group SCM list.
 *  \par Function Description
 *  This function reads the string list from the component-groups
 *  configuration parameter and converts the list into a GList.
 *  The GList is stored in the global default_component_groups variable.
 */
SCM g_rc_component_groups(SCM stringlist)
{
  int length, i;
  GList *list=NULL;
  char *attr;

  SCM_ASSERT(scm_list_p(stringlist), stringlist, SCM_ARG1, "scm_is_list failed");
  length = scm_ilength(stringlist);

  /* If the command is called multiple times, remove the old list before
     recreating it */
  g_list_foreach(default_component_groups, (GFunc)g_free, NULL);
  g_list_free(default_component_groups);

  scm_dynwind_begin(0);

  /* convert the scm list into a GList */
  for (i=0; i < length; i++) {
    char *str;
    SCM elem = scm_list_ref(stringlist, scm_from_int(i));

    SCM_ASSERT(scm_is_string(elem), elem, SCM_ARG1, "list element is not a string");

    str = scm_to_utf8_string(elem);
    attr = geda_utility_string_strdup(str);
    free(str);
    list = g_list_prepend(list, attr);
  }

  scm_dynwind_end();

  default_component_groups = g_list_reverse(list);

  return SCM_BOOL_T;
}

/*! \brief
 *  \par Function Description
 *
 *  \param [in] path
 *  \param [in] name Optional descriptive name for library directory.
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library(SCM path, SCM name)
{
  char *directory;
  char *temp;
  char *namestr = NULL;
  SCM   result;

  SCM_ASSERT (scm_is_string (path), path, SCM_ARG1, "component-library");

  scm_dynwind_begin (0);
  if (name != SCM_UNDEFINED) {
    namestr = scm_to_utf8_string (name);
    scm_dynwind_free(namestr);
  }

  /* take care of any shell variables */
  temp = scm_to_utf8_string (path);

  directory = geda_utility_expand_env_variable (temp);
  scm_dynwind_unwind_handler (g_free, directory, SCM_F_WIND_EXPLICITLY);
  free (temp);
  temp = NULL;

  /* Check if path is valid */
  if (!g_file_test (directory, G_FILE_TEST_IS_DIR)) {
    fprintf(stderr, "Check library path [%s]\n", directory);
    result = SCM_BOOL_F;
  }
  else {

    /* Check if path is absolute */
    if (f_get_is_path_absolute (directory)) {

      /* Check if scheme passed a NULL */
      if (namestr) {

        /* This should not freed */
        char *name = f_get_basename(directory);

        /* Check if scheme passed a zero length string,
         * aka a pointer to a NULL */
        if (!strlen(namestr)) {

          name = geda_utility_string_concat("Local/", name, NULL);

          s_clib_add_directory (directory, name);

          GEDA_FREE(name);
        }
        else {

          /* Check if scheme passed child dir with a leading slash */
          if (strcmp (namestr + 1, name) == 0 ) {

            name = geda_utility_string_concat("Local/", name, NULL);

            s_clib_add_directory (directory, name);

            GEDA_FREE(name);
          }
          else {
            s_clib_add_directory (directory, namestr);
          }
        }
      }
      else {
        s_clib_add_directory (directory, NULL);
      }
    }
    else {

      char *cwd = g_get_current_dir ();
      char *temp;

      switch (strlen(directory)) {
        case 1:
        case 2:
          /* Check if IS current directory */
          if (*directory == '.' && *directory + 1 == '/' ) {
            s_clib_add_directory (cwd, namestr);
            break;
          }

        default:

          /* Check if is below the current directory */
          if (strncmp(directory, "./", 2) == 0) {
            temp = g_build_filename (cwd, directory + 2, NULL);
          }
          else { /* Is above so normalize the path */
            temp = f_sys_normalize_name(directory, NULL);
            /*temp = g_build_filename (cwd, directory, NULL);*/
          }
          s_clib_add_directory (temp, namestr);
          GEDA_FREE(temp);
          break;
      }
      GEDA_FREE(cwd);
    }
    result = SCM_BOOL_T;
  }
  scm_dynwind_end();
  return result;
}

/*! \brief Guile callback for adding library commands.
 *  \par Function Description
 *  Callback function for the "component-library-command" Guile
 *  function, which can be used in the rc files to add a command to
 *  the component library.
 *
 *  \param [in] listcmd command to get a list of symbols
 *  \param [in] getcmd  command to get a symbol from the library
 *  \param [in] name    Optional descriptive name for component source.
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library_command (SCM listcmd, SCM getcmd,
                                    SCM name)
{
  const CLibSource *src;
  char *lcmdstr, *gcmdstr;
  char *tmp_str, *namestr;

  SCM_ASSERT (scm_is_string (listcmd), listcmd, SCM_ARG1,
              "component-library-command");
  SCM_ASSERT (scm_is_string (getcmd), getcmd, SCM_ARG2,
              "component-library-command");
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG3,
              "component-library-command");

  scm_dynwind_begin(0);

  /* take care of any shell variables */
  /*! \bug this may be a security risk! */
  tmp_str = scm_to_utf8_string (listcmd);
  lcmdstr = geda_utility_expand_env_variable (tmp_str);
  scm_dynwind_unwind_handler (g_free, lcmdstr, SCM_F_WIND_EXPLICITLY);
  free (tmp_str); /* this should stay as free (allocated from guile) */

  /* take care of any shell variables */
  /*! \bug this may be a security risk! */
  tmp_str = scm_to_utf8_string (getcmd);
  gcmdstr = geda_utility_expand_env_variable (tmp_str);
  scm_dynwind_unwind_handler (g_free, gcmdstr, SCM_F_WIND_EXPLICITLY);
  free (tmp_str); /* this should stay as free (allocated from guile) */

  namestr = scm_to_utf8_string (name);

  src = s_clib_add_command (lcmdstr, gcmdstr, namestr);

  free (namestr); /* this should stay as free (allocated from guile) */

  scm_dynwind_end();

  if (src != NULL) return SCM_BOOL_T;

  return SCM_BOOL_F;
}

/*! \brief Guile callback for adding library functions.
 *  \par Function Description
 *  Callback function for the "component-library-funcs" Guile
 *  function, which can be used in the rc files to add a set of Guile
 *  procedures for listing and generating symbols.
 *
 *  \param [in] listfunc A Scheme procedure which takes no arguments
 *                       and returns a Scheme list of component names.
 *  \param [in] getfunc A Scheme procedure which takes a component
 *                      name as an argument and returns a symbol
 *                      encoded in a string in gEDA format, or the \b
 *                      \#f if the component name is unknown.
 *  \param [in] name    A descriptive name for this component source.
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library_funcs (SCM listfunc, SCM getfunc, SCM name)
{
  char *namestr;

  SCM result = SCM_BOOL_F;

  SCM_ASSERT (scm_is_true (scm_procedure_p (listfunc)), listfunc, SCM_ARG1,
              "component-library-funcs");
  SCM_ASSERT (scm_is_true (scm_procedure_p (getfunc)), getfunc, SCM_ARG2,
              "component-library-funcs");
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG3,
              "component-library-funcs");

  namestr = scm_to_utf8_string (name);

  if (s_clib_add_scm (listfunc, getfunc, namestr) != NULL) {
    result = SCM_BOOL_T;
  }

  free (namestr);
  return result;
}

/*! \brief Handles the source-library SCM keyword.
 *  \par Function Description
 *   Sets a hokey path to search for schematics.
 *
 *  \param [in] path String to use as path to Source Library.
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if path is invalid.
 *
 *  \sa s_slib_add_entry
 */
SCM g_rc_source_library(SCM path)
{
  char *string;
  char *temp;

  SCM_ASSERT (scm_is_string (path), path, SCM_ARG1, "source-library");

  /* take care of any shell variables */
  temp   = scm_to_utf8_string (path);
  string = geda_utility_expand_env_variable (temp);
  free (temp);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
           _("Invalid path [%s] passed to source-library\n"),
             string);
    GEDA_FREE(string);
    return SCM_BOOL_F;
  }

  if (f_get_is_path_absolute (string)) {
    s_slib_add_entry (string);
  }
  else {
    char *cwd = g_get_current_dir ();
    char *temp;
    temp = g_build_filename (cwd, string, NULL);
    s_slib_add_entry (temp);
    GEDA_FREE(temp);
    GEDA_FREE(cwd);
  }

  GEDA_FREE(string);

  return SCM_BOOL_T;
}

/*! \brief Handles the source-library-search SCM keyword.
 *  \par Function Description
 *   Sets a hokey path to search for schematics.
 *
 *  \param [in] path String to use as path to search.
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if path is invalid.
 *
 *  \sa s_slib_add_entry
 */
SCM g_rc_source_library_search(SCM path)
{
  char *string;
  char *temp;
  GDir *dir;
  const char *entry;

  SCM_ASSERT (scm_is_string (path), path, SCM_ARG1, "source-library-search");

  /* take care of any shell variables */
  temp   = scm_to_utf8_string (path);
  string = geda_utility_expand_env_variable (temp);
  free (temp);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {

    fprintf (stderr, _("Invalid path [%s] passed to source-library-search\n"),
             string);
    GEDA_FREE(string);
    return SCM_BOOL_F;
  }

  dir = g_dir_open (string, 0, NULL);

  if (dir == NULL) {
    fprintf (stderr,
           _("Invalid path [%s] passed to source-library-search\n"),
             string);
    GEDA_FREE(string);
    return SCM_BOOL_F;
  }

  while ((entry = g_dir_read_name (dir))) {

    /* Skip . and .. and special case font */
    if ((g_ascii_strcasecmp (entry, ".")    != 0) &&
        (g_ascii_strcasecmp (entry, "..")   != 0) &&
        (g_ascii_strcasecmp (entry, "font") != 0))
    {
      char *fullpath = g_build_filename (string, entry, NULL);

      if (s_slib_unique_dir_exist (fullpath)) {

        if (f_get_is_path_absolute (fullpath)) {
          s_slib_add_entry (fullpath);
        }
        else {

          char *cwd  = g_get_current_dir ();
          char *temp = g_build_filename (cwd, fullpath, NULL);

          s_slib_add_entry (temp);

          GEDA_FREE(temp);
          GEDA_FREE(cwd);
        }
      }

      GEDA_FREE(fullpath);
    }
  }

  GEDA_FREE(string);
  g_dir_close(dir);

  return SCM_BOOL_T;
}

/*! \brief Handles the reset-component-library SCM keyword.
 *  \par Function Description
 *  When reset-component-library is process, all known component
 *  library paths are erased.
 *
 *  \returns SCM_BOOL_T always.
 *
 *  \sa s_clib_init
 */
SCM g_rc_reset_component_library(void)
{
  s_clib_init();

  return SCM_BOOL_T;
}

/*! \brief Handles the reset-source-library SCM keyword.
 *  \par Function Description
 *  When reset-source-library is process, resources used for library paths
 *  are released and all known component library paths erased.
 *
 *  \returns SCM_BOOL_T always.
 *
 *  \sa s_clib_init
 */
SCM g_rc_reset_source_library(void)
{
  s_slib_free(); /* Release resources */
  s_slib_init(); /* Sets pointers that were just freed to NULL */

  return SCM_BOOL_T;
}


/* Net Styles*/
/*! \brief This function processes the bus-style RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the bus-style RC entry. This function
 *       accepts either string or integer type arguments.
 *
 */
SCM g_rc_bus_style(SCM mode)
{
  if(scm_is_string(mode)) {

    static const vstbl_entry mode_table[] = {
     {STYLE_NONE , RC_STR_STYLE_NONE  },
     {STYLE_THIN , RC_STR_STYLE_THIN  },
     {STYLE_THICK, RC_STR_STYLE_THICK }
   };

   RETURN_G_RC_MODE("bus-style", default_bus_style, mode_table);

  }
  else {

    int val;

    if (scm_is_integer(mode)) {

      val = scm_to_int (mode);

      if(val < STYLE_NONE || val > STYLE_THICK) {
        fprintf (stderr, _("Bad value [%d], check bus-style entry in rc file\n"), val);
        fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_BUS_STYLE);
        val = DEFAULT_BUS_STYLE;
      }
    }
    else {
      fprintf (stderr, _("Invalid type assignment, check bus-style entry in rc file\n"));
      fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_BUS_STYLE);
      val = DEFAULT_BUS_STYLE;
    }
    default_bus_style = val;
  }
  return SCM_BOOL_T;
}

/*! \brief This function processes the line-style RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the line-style RC entry.  This function
 *       accepts either string or integer type arguments.
 *
 */
SCM g_rc_line_style(SCM mode)
{
  if(scm_is_string(mode)) {

    static const vstbl_entry mode_table[] = {
     {STYLE_NONE , RC_STR_STYLE_NONE  },
     {STYLE_THIN , RC_STR_STYLE_THIN  },
     {STYLE_THICK, RC_STR_STYLE_THICK }
   };

   RETURN_G_RC_MODE("line-style", default_line_style, mode_table);

  }
  else {

    int val;

    if (scm_is_integer(mode)) {

      val = scm_to_int (mode);

      if (val < STYLE_NONE || val > STYLE_THICK) {
        fprintf (stderr, _("Bad value [%d], check line-style entry in rc file\n"), val);
        fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_LINE_STYLE);
        val = DEFAULT_LINE_STYLE;
      }
    }
    else {
      fprintf (stderr, _("Invalid type assignment, check line-style entry in rc file\n"));
      fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_LINE_STYLE);
      val = DEFAULT_LINE_STYLE;
    }
    default_line_style = val;
  }
  return SCM_BOOL_T;
}

/*! \brief This function processes the net-style RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the net-style RC entry. This function
 *       accepts either string or integer type arguments.
 *
 */
SCM g_rc_net_style(SCM mode)
{
  if(scm_is_string(mode)) {

    static const vstbl_entry mode_table[] = {
      {STYLE_NONE , RC_STR_STYLE_NONE  },
      {STYLE_THIN , RC_STR_STYLE_THIN  },
      {STYLE_THICK, RC_STR_STYLE_THICK }
    };

    RETURN_G_RC_MODE("net-style", default_net_style, mode_table);

  }
  else {

    int val;

    if (scm_is_integer(mode)) {

      val = scm_to_int (mode);

      if (val < STYLE_NONE || val > STYLE_THICK) {
        fprintf (stderr, _("Bad value [%d], check net-style entry in rc file\n"), val);
        fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_NET_STYLE);
        val = DEFAULT_LINE_STYLE;
      }
    }
    else {
      fprintf (stderr, _("Invalid type assignment, check net-style entry in rc file\n"));
      fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_NET_STYLE);
      val = DEFAULT_NET_STYLE;
    }
    default_net_style = val;
  }
  return SCM_BOOL_T;
}

/*! \brief This function processes the pin-style RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the pin-style RC entry. This functions
 *       accepts either string or integer type arguments.
 *
 *  \returns SCM_BOOL_T always.
 */
SCM g_rc_pin_style(SCM mode)
{
  if(scm_is_string(mode)) {

    static const vstbl_entry mode_table[] = {
      {STYLE_NONE , RC_STR_STYLE_NONE  },
      {STYLE_THIN , RC_STR_STYLE_THIN  },
      {STYLE_THICK, RC_STR_STYLE_THICK }
    };

    RETURN_G_RC_MODE("pin-style", default_pin_style, mode_table);

  }
  else {

    int val;

    if (scm_is_integer(mode)) {

      val = scm_to_int (mode);

      if ((val < STYLE_NONE) || (val > STYLE_THICK)) {
        fprintf (stderr, _("Bad value [%d], check pin-style entry in rc file\n"), val);
        fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_PIN_STYLE);
        val = DEFAULT_LINE_STYLE;
      }
    }
    else {
      fprintf (stderr, _("Invalid type assignment, check pin-style entry in rc file\n"));
      fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_PIN_STYLE);
      val = DEFAULT_PIN_STYLE;
    }
    default_pin_style = val;
  }
  return SCM_BOOL_T;
}

/*! \brief This function processes the thick-bus-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thick-bus-width RC entry.
 */

SCM g_rc_thick_bus_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_BUS_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thick-bus-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THICK_BUS_WIDTH);
    val = DEFAULT_THICK_BUS_WIDTH;
  }

  default_thick_bus_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thick-line-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thick-line-width RC entry.
 */
SCM g_rc_thick_line_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_LINE_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thick-line-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THICK_LINE_WIDTH);
    val = DEFAULT_THICK_LINE_WIDTH;
  }

  default_thick_line_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thick-net-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thick-net-width RC entry.
 */
SCM g_rc_thick_net_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_NET_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thick-net-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THICK_NET_WIDTH);
    val = DEFAULT_THICK_NET_WIDTH;
  }

  default_thick_net_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thick-pin-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thick-pin-width RC entry.
 */
SCM g_rc_thick_pin_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_PIN_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thick-pin-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THICK_PIN_WIDTH);
    val = DEFAULT_THICK_PIN_WIDTH;
  }

  default_thick_pin_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thin-bus-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thin-bus-width RC entry.
 */
SCM g_rc_thin_bus_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_BUS_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thin-bus-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THIN_BUS_WIDTH);
    val = DEFAULT_THIN_BUS_WIDTH;
  }

  default_thin_bus_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thin-line-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thin-line-width RC entry.
 */
SCM g_rc_thin_line_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_LINE_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thin-line-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THIN_LINE_WIDTH);
    val = DEFAULT_THIN_LINE_WIDTH;
  }

  default_thin_line_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thin-net-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thin-net-width RC entry.
 */
SCM g_rc_thin_net_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_NET_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thin-net-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THIN_NET_WIDTH);
    val = DEFAULT_THIN_NET_WIDTH;
  }

  default_thin_net_width = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the thin-pin-width RC entry.
 *  \par Function Description
 *       C function to construct lisp algorithms to dynamically process
 *       configuration data for the thin-pin-width RC entry.
 */
SCM g_rc_thin_pin_width (SCM width)
{
  int val;

  if (scm_is_integer(width)) {

    val = scm_to_int (width);

    if (val < 0) {
      val = MIN_PIN_WIDTH; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check thin-pin-width entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_THIN_PIN_WIDTH);
    val = DEFAULT_THIN_PIN_WIDTH;
  }

  default_thin_pin_width = val;

  return SCM_BOOL_T;
}
/* End Style Related RC varibles */

/*! \brief Handles the always-promote-attributes SCM keyword.
 *  \par Function Description
 *  This function create a list of attribute string that are alway to be
 *  promoted when symbols file are loaded. A pointer to the list is stored
 *  to default_always_promote_attributes.
 *
 *  \param [in] attrlist Can be a space seperated list of string or a SCM list
 *
 *  \returns SCM_BOOL_T always.
 */
SCM g_rc_always_promote_attributes(SCM attrlist)
{
  GList *list=NULL;
  char  *attr;
  char **attr2;
  int i;

  g_list_foreach(default_always_promote_attributes, (GFunc)g_free, NULL);
  g_list_free(default_always_promote_attributes);

  if (scm_is_string (attrlist)) {
    char *temp;

    /* convert the space separated strings into a GList */
    temp = scm_to_utf8_string (attrlist);
    attr2 = g_strsplit(temp," ", 0);
    free (temp);

    for (i=0; attr2[i] != NULL; i++) {
      if (strlen(attr2[i]) > 0) {
        list = g_list_prepend(list, geda_utility_string_strdup(attr2[i]));
      }
    }
    g_strfreev(attr2);
  }
  else {

    int length;

    SCM_ASSERT(scm_list_p(attrlist), attrlist, SCM_ARG1, "always-promote-attributes");

    length = scm_ilength(attrlist);

    /* convert the scm list into a GList */
    for (i=0; i < length; i++) {

      char *temp;

      SCM_ASSERT(scm_is_string(scm_list_ref(attrlist, scm_from_int(i))),
                 scm_list_ref(attrlist, scm_from_int(i)), SCM_ARG1,
               _("always-promote-attribute: list element is not a string"));
      temp = scm_to_utf8_string (scm_list_ref (attrlist, scm_from_int (i)));
      attr = geda_utility_string_strdup(temp);
      free (temp);
      list = g_list_prepend(list, attr);
    }
  }

  default_always_promote_attributes = g_list_reverse(list);

  return SCM_BOOL_T;
}

/*! \brief Handles the attribute-promote SCM keyword.
 *  \par Function Description
 *  Uses MACRO to call g_rc_parse_mode to sets boolean configuration
 *  variable based on string argument.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 */
SCM g_rc_attribute_promotion(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("attribute-promotion",
                    default_attribute_promotion, mode_table);
}

/*! \brief Handles the keep-invisible SCM keyword.
 *  \par Function Description
 *  Uses MACRO to call g_rc_parse_mode to sets boolean configuration
 *  variable based on string argument.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 */
SCM g_rc_keep_invisible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("keep-invisible", default_keep_invisible, mode_table);
}

/*! \brief Handles the bitmap-directory SCM keyword.
 *  \par Function Description
 *  The value of the bitmap-directory keyword specifies where to search
 *  for bitmage images.
 *
 *  \param [in] path
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if path is invalid.
 *
 *  \sa f_get_bitmap_filespec
 */
SCM g_rc_bitmap_directory(SCM path)
{
  char *string;
  char *temp;

  SCM_ASSERT (scm_is_string (path), path, SCM_ARG1, "bitmap-directory");

  /* take care of any shell variables */
  temp   = scm_to_utf8_string (path);
  string = geda_utility_expand_env_variable (temp);
  free (temp);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr, _("Path invalid[%s], %s\n"), string, strerror (errno));
    GEDA_FREE(string);
    return SCM_BOOL_F;
  }

  GEDA_FREE(default_bitmap_directory);
  default_bitmap_directory = string;

  return SCM_BOOL_T;
}

/*! \brief Handles the log-directory SCM keyword.
 *  \par Function Description
 *  The value of the log-directory keyword specifies where to write log
 *  files.
 *
 *  \param [in] path
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if path is invalid.
 *
 *  \sa geda_utility_log_init
 */
SCM g_rc_log_directory(SCM path)
{
  char *string;
  char *temp;

  SCM_ASSERT (scm_is_string (path), path, SCM_ARG1, "log-directory");

  /* take care of any shell variables */
  temp   = scm_to_utf8_string (path);
  string = geda_utility_expand_env_variable (temp);
  free (temp);

  /* invalid path? */
  if (f_path_create (string, 0777 /*octal*/ ) != NO_ERROR) {
    fprintf (stderr, _("Path invalid[%s], %s\n"), string, strerror (errno));
    GEDA_FREE(string);
    return SCM_BOOL_F;
  }

  GEDA_FREE(default_log_directory);
  default_log_directory = string;

  return SCM_BOOL_T;
}

/*! \brief Add a directory to the Guile load path.
 *  \par Function Description
 *  Prepends \a s_path to the Guile system '%load-path', after
 *  expanding environment variables.
 *
 *  \param [in] s_path  Path to be added.
 *
 *  \returns SCM_BOOL_T.
 */
SCM g_rc_scheme_directory(SCM s_path)
{
  char *temp;
  char *expanded;
  SCM s_load_path_var;
  SCM s_load_path;

  SCM_ASSERT (scm_is_string (s_path), s_path,
              SCM_ARG1, "scheme-directory");

  /* take care of any shell variables */
  temp     = scm_to_utf8_string (s_path);
  expanded = geda_utility_expand_env_variable (temp);
  s_path   = scm_from_utf8_string (expanded);

  free (temp);
  GEDA_FREE (expanded);

  s_load_path_var = scm_c_lookup ("%load-path");
  s_load_path     = scm_variable_ref (s_load_path_var);

  scm_variable_set_x (s_load_path_var, scm_cons (s_path, s_load_path));

  scm_remember_upto_here_2 (s_load_path_var, s_load_path);
  scm_remember_upto_here_1 (s_path);

  return SCM_BOOL_T;
}

/*! \brief Handles the check-symbol-version SCM keyword.
 *  \par Function Description
 *  Uses MACRO to call g_rc_parse_mode to sets boolean configuration
 *  variable based on string argument that controls whether  symbol version
 *  checking is enabled or disabled.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 */
SCM g_rc_check_symbol_version(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("check-symbol-version",
                    default_check_symbol_version, mode_table);
}

/*! \brief Handles the log-time SCM keyword
 *  \par Function Description
 *  The log-time keyword specifies whether to prefix the current
 *  time of day to log entries when writing to log files.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 *
 *  \sa geda_utility_log_init
 */
SCM g_rc_log_time(SCM mode)
{
  SCM_ASSERT (scm_is_string (mode), mode, SCM_ARG1, "log-time");

  SCM ret_val;

  int prefix_time;

  static const vstbl_entry mode_table[] = {
    {TRUE , "enable" },
    {FALSE, "disable"},
  };

  ret_val = g_rc_parse_mode(mode, "log-time", &prefix_time, mode_table, 2);

  if (scm_is_true(ret_val)) {
    geda_utility_log_set_log_time(prefix_time);
  }

  return ret_val;
}

/*! \brief Enable the creation of backup files when saving
 *  \par Function Description
 *  Uses MACRO to call g_rc_parse_mode to sets boolean configuration
 *  variable based on string argument. If enabled then a backup file, of
 *  the form 'example.sch~', is created when saving a file.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 */
SCM g_rc_make_backup_files(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("make-backup-files",
                    default_make_backup_files, mode_table);
}

/*! \brief Handles the postsript-prolog SCM keyword.
 *  \par Function Description
 *  The value of the postscript-prolog keyword specifies the file name
 *  of the postscript prolog  file.
 *
 *  \param [in] scmsymname prolog file name.
 *
 *  \returns SCM_BOOL_T always.
 *
 *  \sa f_print_header
 */
SCM g_rc_postscript_prolog(SCM scmsymname)
{
  char *temp;

  SCM_ASSERT (scm_is_string (scmsymname), scmsymname,
              SCM_ARG1, "postsript-prolog");

  GEDA_FREE(default_postscript_prolog);

  /* take care of any shell variables */
  temp = scm_to_utf8_string (scmsymname);

  default_postscript_prolog = geda_utility_expand_env_variable (temp);

  free (temp);

  return SCM_BOOL_T;
}

/*! \brief Handles the promote-invisible SCM keyword.
 *  \par Function Description
 *  Uses MACRO to call g_rc_parse_mode to sets boolean configuration
 *  variable based on string argument. If enabled, then invisible floating
 *  attributes are promoted (attached to the outside of the component) if
 *  the text string is invisible.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 */
SCM g_rc_promote_invisible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("promote-invisible",
                    default_promote_invisible, mode_table);
}

/*! \brief Handles the untitled-name SCM keyword.
 *  \par Function Description
 *  Specify the default untitled basename.
 *
 *  \param [in] name
 *
 *  \returns SCM_BOOL_T always.
 */
SCM g_rc_untitled_name(SCM name)
{
  char *temp;

  SCM_ASSERT (scm_is_string (name), name, SCM_ARG1, "untitled-name");

  GEDA_FREE(default_untitled_name);

  temp = scm_to_utf8_string (name);

  default_untitled_name = geda_utility_string_strdup (temp);

  free (temp);

  return SCM_BOOL_T;
}

/*! \brief Handles the show-full-path SCM keyword.
 *  \par Function Description
 *  Uses MACRO to call g_rc_parse_mode to sets boolean configuration
 *  variable based on string argument. Application should check this
 *  setting and display the full path in the file name when enabled.
 *
 *  \param [in] mode string "enabled" or "disabled"
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F if mode is not a valid value.
 */
SCM g_rc_show_full_path(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("show-full-path",
                    default_show_full_path, mode_table);
}

/** @} endgroup Libgeda-RC-Handlers */
