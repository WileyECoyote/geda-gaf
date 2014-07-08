/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
/*! \file g_rc.c
 *  \brief Execute Scheme initialization files.
 *
 * Contains functions to open, parse and manage Scheme initialization
 * (RC) files.
 */

#include <config.h>
#include <missing.h>

#include <errno.h>
#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"
#include "libgedaguile.h"

SCM scheme_rc_config_fluid = SCM_UNDEFINED;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_mode_general(SCM scmmode,
                      const char *rc_name,
                      int *mode_var,
                      const vstbl_entry *table,
                      int table_size)
{
  SCM ret;
  int index;
  char *mode;

  SCM_ASSERT (scm_is_string (scmmode), scmmode,
              SCM_ARG1, rc_name);

  mode = scm_to_utf8_string (scmmode);

  index = vstbl_lookup_str(table, table_size, mode);
  /* no match? */
  if(index == table_size) {
    fprintf(stderr,
            "Invalid mode [%s] passed to %s\n",
            mode,
            rc_name);
    ret = SCM_BOOL_F;
  } else {
    *mode_var = vstbl_get_val(table, index);
    ret = SCM_BOOL_T;
  }

  free (mode);

  return ret;
}

/*! \brief Mark an RC file as loaded.
 * \par Function Description
 *   If the Scheme initialization file \a filename has not already been
 * loaded, mark the file as loaded and return TRUE, storing \a filename
 * in \a configuration context (\a filename should not subsequently be
 * freed). Otherwise, return FALSE, and set \a err appropriately.
 *
 * \note Should only be called by g_rc_parse_file().
 *
 * \param cfg       The configuration context to use while loading.
 * \param filename  The RC file name to test.
 * \param err       Return location for errors, or NULL.
 * \return TRUE if \a filename not already loaded, FALSE otherwise.
 */
static bool g_rc_try_mark_read (EdaConfig *cfg, char *filename, GError **err)
{
  GList *found = NULL;

  if (filename == NULL) {
    BUG_MSG("filename is NULL");
    return FALSE;
  }
  if (!EDA_IS_CONFIG (cfg)) {
    BUG_MSG("EDA_IS_CONFIG (cfg) failed");
    /**/
    return TRUE;
  }

  /* Test if marked read already */
  found = g_list_find_custom (cfg->RC_list, filename, (GCompareFunc) strcmp);

  if (found != NULL) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_RC_TWICE, _("RC file already loaded"));
    return FALSE;
  }

  cfg->RC_list = g_list_append (cfg->RC_list, filename);

  return TRUE;
}

/*! \brief Load an RC file.
 * \par Function Description
 * Load and run the Scheme initialization file \a rcfile, reporting
 * errors via \a err.
 *
 * \param rcfile    The filename of the RC file to load.
 * \param cfg       The configuration context to use while loading.
 * \param err       Return location for errors, or NULL;
 * \return TRUE on success, FALSE on failure.
 */
bool
g_rc_parse_file (const char *rcfile, EdaConfig *cfg, GError **err)
{
  bool    status     = FALSE;
  char   *name_norm  = NULL;
  GError *tmp_err    = NULL;

  if (rcfile == NULL) {
    BUG_MSG("rcfile is NULL.");
    return FALSE;
  }

  /* If no configuration file was specified, get the default
   * configuration file for the rc file. */
  if (cfg == NULL) {
    cfg = eda_config_get_context_for_path (rcfile);

    /* If the configuration wasn't loaded yet, attempt to load
     * it. Config loading is on a best-effort basis; if we fail, just
     * print a warning. WEH: changed previous conditional so that we
     * only do this if the cfg was NULL */
    if (!eda_config_is_loaded (cfg)) {
      eda_config_load (cfg, &tmp_err);
      if (tmp_err != NULL &&
        !g_error_matches (tmp_err, G_IO_ERROR, G_IO_ERROR_NOT_FOUND))
        g_warning (_("Failed to load config from '%s': %s\n"),
        eda_config_get_filename (cfg), tmp_err->message);
      g_clear_error (&tmp_err);
    }
  }

  /* If the fluid for storing the relevant configuration context for
   * RC file reading hasn't been created yet, create it. */
  if (scheme_rc_config_fluid == SCM_UNDEFINED)
    scheme_rc_config_fluid = scm_permanent_object (scm_make_fluid ());

  /* Normalise filename */
  name_norm = f_normalize_filename (rcfile, err);

  if (name_norm != NULL) {

    if (access(name_norm, R_OK) == 0) {

      scm_dynwind_begin (0);
      scm_dynwind_fluid (scheme_rc_config_fluid, edascm_from_config (cfg));

      /* Attempt to load the RC file, if the same file has not been loaded
       * already. If g_rc_try_mark_read() succeeds, it stores name_norm in
       * cfg, so we *don't* free it. */
      status = (g_rc_try_mark_read (cfg, name_norm, &tmp_err) &&
      g_read_scheme_file (name_norm, &tmp_err));

      scm_dynwind_end ();

      if (status) {
        u_log_message (_("Loaded RC file [%s]\n"), name_norm);
      }
      else {

        /* Copy tmp_err into err, with a prefixed message. */
        g_propagate_prefixed_error (err, tmp_err,
                                    _("Error encounted processing RC file [%s]: "),
                                    name_norm);
        GEDA_FREE (name_norm); /* was not successful so not stored */
      }
    }
    else {
      if(err != NULL) {
        g_set_error(err, G_FILE_ERROR,
                    g_file_error_from_errno (errno),
                    "accessing file %s", name_norm);
      }
      else {
        fprintf(stderr, "Error loading configuration %s, %s\n",
                name_norm, strerror(errno));
      }
      GEDA_FREE (name_norm);
    }
  }
  return status;
}

/*! \brief Load a system RC file.
 * \par Function Description
 * Attempts to load and run the system Scheme initialization file with
 * basename \a rcname.  The string "system-" is prefixed to \a rcname.
 * If \a rcname is NULL, the default value of "gafrc" is used.
 *
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
bool g_rc_parse_system (const char *rcname, GError **err)
{
  char *sysname = NULL;
  char *rcfile = NULL;
  bool status;

  /* Default to gafrc */
  rcname = (rcname != NULL) ? rcname : "gafrc";

  sysname = g_strdup_printf ("system-%s", rcname);

  rcfile = g_build_filename (f_path_sys_config (), sysname, NULL);

  status = g_rc_parse_file (rcfile, eda_config_get_system_context ("geda"), err);

  GEDA_FREE (rcfile);

  GEDA_FREE (sysname);

  return status;
}

/*! \brief Load a user RC file.
 * \par Function Description
 * Attempts to load the user Scheme initialization file with basename
 * \a rcname.  If \a rcname is NULL, the default value of "gafrc" is
 * used.
 *
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
bool
g_rc_parse_user (const char *rcname, GError **err)
{
  char *rcfile = NULL;
  bool  status;

  /* Default to gafrc */
  rcname = (rcname != NULL) ? rcname : "gafrc";

  rcfile = g_build_filename (f_path_user_config (), rcname, NULL);
  status = g_rc_parse_file (rcfile, eda_config_get_user_context (), err);
  GEDA_FREE (rcfile);
  return status;
}

/*! \brief Load a local RC file.
 * \par Function Description
 * Attempts to load the Scheme initialization file with basename \a
 * rcname corresponding to \a path, reporting errors via \a err.  If
 * \a path is a directory, looks for a file named \a rcname in that
 * directory. Otherwise, looks for a file named \a rcname in the same
 * directory as \a path. If \a path is NULL, looks in the current
 * directory. If \a rcname is NULL, the default value of "gafrc" is
 * used.
 *
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param path      The path to load a RC file for, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
bool
g_rc_parse_local (const char *rcname, const char *path, GError **err)
{
  bool  status;
  char *dir      = NULL;
  char *rcfile   = NULL;

  EdaConfig *cfg = NULL;

  /* Default to gafrc */
  rcname = (rcname != NULL) ? rcname : "gafrc";
  /* Default to cwd */
  path = (path != NULL) ? path : ".";

  /* If path isn't a directory, get the dirname. */
  if (g_file_test (path, G_FILE_TEST_IS_DIR)) {
    dir = g_strdup (path);
  }
  else {
    dir = g_path_get_dirname (path);
  }

  rcfile = g_build_filename (dir, rcname, NULL);

  cfg = eda_config_get_context_for_path(path);

  status = g_rc_parse_file (rcfile, cfg, err);

  GEDA_FREE (dir);
  GEDA_FREE (rcfile);
  return status;
}

static void g_rc_parse__process_error (GError **err, const char *pname)
{
  char *pbase;

  /* Take no chances; if err was not set for some reason, bail out. */
  if (*err == NULL) {
    const char *msgl =
      _("ERROR: An unknown error occurred while parsing configuration files.");
    u_log_message ("%s\n", msgl);
    fprintf(stderr, "%s\n", msgl);

  }
  else {
    /* Config files are allowed to be missing or skipped; check for
     * this. */
    if (g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOENT) ||
        g_error_matches (*err, EDA_ERROR, EDA_ERROR_RC_TWICE)) {
      return;
    }

    u_log_message (_("ERROR: %s\n"), (*err)->message);
    fprintf (stderr, _("ERROR: %s\n"), (*err)->message);
  }

  /* g_path_get_basename() allocates memory, but we don't care
   * because we're about to exit. */
  pbase = g_path_get_basename (pname);
  fprintf (stderr, _("ERROR: The %s log may contain more information.\n"),
           pbase);

  exit (1);
}

/*! \brief General RC Configuration file parsing function.
 * \par Function Description
 * Calls g_rc_parse_handler() with the default error handler. If any
 * error other than ENOENT occurs while loading or running a Scheme
 * initialisation file, prints an informative message and calls
 * exit(1).
 *
 * \bug libgeda shouldn't call exit() - this function calls
 *      g_rc_parse__process_error(), which does.
 *
 * \warning Since this function may not return, it should only be used
 * on application startup or when there is no chance of data loss from
 * an unexpected exit().
 *
 * \param [in] pname     The name of the application (usually argv[0]).
 * \param [in] rcname    Config file basename, or NULL.
 * \param [in] rcfile    Specific config file path, or NULL.
 */
bool
g_rc_parse (const char *pname, const char *rcname, const char *rcfile)
{
  g_rc_parse_handler (rcname, rcfile,
                     (ConfigParseErrorFunc) g_rc_parse__process_error,
                     (void *) pname);
  return TRUE;
}

#ifdef HANDLER_DISPATCH
#  error HANDLER_DISPATCH already defined
#endif
#define HANDLER_DISPATCH \
  do { if (err == NULL) break;  handler (&err, user_data);        \
       g_clear_error (&err); } while (0)

/*! \brief General RC file parsing function.
 * \par Function Description
 * Attempt to load system, user and local (current working directory)
 * configuration \a gafrc files. The second parameter is not used by
 * this function but serves a place holder to be consistence with the
 * other g_xxxx_parse_handler's; g_rcname_parse_handler and
 * g_rcfile_parse_handler.
 *
 * If an error occurs, calls \a handler with the provided \a user_data
 * and a GError.
 *
 * \see g_rc_parse().
 *
 * \param dummy     NULL.
 * \param handler   Handler function for config parse errors.
 * \param user_data Data to be passed to \a handler.
 */
void g_gafrc_parse_handler (const char *dummy,
                            ConfigParseErrorFunc handler, void *user_data)
{
  GError *err = NULL;

  /* Load RC files in order. */
  g_rc_parse_system (NULL, &err);       HANDLER_DISPATCH;
  g_rc_parse_user   (NULL, &err);       HANDLER_DISPATCH;
  g_rc_parse_local  (NULL, NULL, &err); HANDLER_DISPATCH;
}

/*! \brief General RC file parsing function.
 * \par Function Description
 * Attempt to load system, user and local (current working directory)
 * configuration files the basename \a rcname, if \a rcname is not NULL.
 *
 * If an error occurs, calls \a handler with the provided \a user_data
 * and a GError.
 *
 * \see g_rc_parse().
 *
 * \param rcname    Config file basename, or NULL.
 * \param handler   Handler function for config parse errors.
 * \param user_data Data to be passed to \a handler.
 */
void g_rcname_parse_handler (const char *rcname,
                             ConfigParseErrorFunc handler, void *user_data)
{
  GError *err = NULL;

  /* Application-specific rcname. */
  if (rcname != NULL) {
    g_rc_parse_system (rcname, &err);       HANDLER_DISPATCH;
    g_rc_parse_user   (rcname, &err);       HANDLER_DISPATCH; 
    g_rc_parse_local  (rcname, NULL, &err); HANDLER_DISPATCH;
  }

}
/*! \brief General RC file parsing function.
 * \par Function Description
 * Attempt to load configuration from \a rcfile if \a rcfile is not NULL.
 *
 * If an error occurs, calls \a handler with the provided \a user_data
 * and a GError.
 *
 * \see g_rc_parse().
 *
 * \param rcfile    Specific config file path, or NULL.
 * \param handler   Handler function for config parse errors.
 * \param user_data Data to be passed to \a handler.
 */
void
g_rcfile_parse_handler (const char *rcfile, ConfigParseErrorFunc handler, void *user_data)
{
  GError *err = NULL;

  /* Finally, optional additional RC file.  Specifically use the current
   * working directory's configuration context here, no matter where the
   * rc file is located on disk. */
  if (rcfile != NULL) {

    EdaConfig *cwd_cfg = eda_config_get_context_for_file (NULL);

    g_rc_parse_file (rcfile, cwd_cfg, &err); HANDLER_DISPATCH;

  }
}
#undef HANDLER_DISPATCH
/*! \brief General RC file parsing function.
 * \par Function Description
 * Attempt to load system, user and local (current working directory)
 * configuration files, starting with the default "gafrc" basename and
 * then with the basename \a rcname, if \a rcname is not NULL.
 * Additionally, attempt to load configuration from \a rcfile if \a
 * rcfile is not NULL.
 *
 * If an error occurs, calls \a handler with the provided \a user_data
 * and a GError.
 *
 * \see g_rc_parse().
 *
 * \param rcname    Config file basename, or NULL.
 * \param rcfile    Specific config file path, or NULL.
 * \param handler   Handler function for config parse errors.
 * \param user_data Data to be passed to \a handler.
 */
void g_rc_parse_handler (const char *rcname,
                         const char *rcfile, ConfigParseErrorFunc handler,
                         void *user_data)
{
  /* Load RC files in order. */
  /* First gafrc files. */
  g_gafrc_parse_handler (NULL, handler, user_data);

  /* Next application-specific rcname. */
  g_rcname_parse_handler (rcname, handler, user_data);

  /* Finally, optional additional RC file.  Specifically use the
   * current working directory's configuration context here, no matter
   * where the rc file is located on disk. */
  g_rcfile_parse_handler (rcfile, handler, user_data);
}
/*!
 * \brief Get the name of the RC filename being evaluated.
 * \par Function Description
 *
 * Creates a Guile stack object, extracts the topmost frame from that
 * stack and gets the sourcefile name.
 *
 * \returns If the interpreter can resolve the filename, returns a
 * Scheme object with the full path to the RC file, otherwise FALSE
 */
SCM
g_rc_rc_filename()
{
  SCM stack, frame, source;

  stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  if (scm_is_false (stack)) {
    return SCM_BOOL_F;
  }

  frame = scm_stack_ref (stack, scm_from_int(0));
  if (scm_is_false (frame)) {
    return SCM_BOOL_F;
  }

  source = scm_frame_source (frame);
  if (scm_is_false (source)) {
    return SCM_BOOL_F;
  }

  return scm_source_property (source, scm_sym_filename);
}

/*!
 * \brief Get a configuration context for the current RC file.
 * \par Function Description
 * Returns the configuration context applicable to the RC file being
 * evaluated.  This function is intended to support gEDA transition
 * from functions in RC files to static configuration files.
 *
 * \returns An EdaConfig smob.
 */
SCM
g_rc_rc_config()
{
  SCM cfg_s = scm_fluid_ref (scheme_rc_config_fluid);
  if (!scm_is_false (cfg_s)) return cfg_s;

  EdaConfig *cfg = eda_config_get_context_for_file (NULL);
  return edascm_from_config (cfg);
}