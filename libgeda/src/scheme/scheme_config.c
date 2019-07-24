/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2011-2015 Peter Brett <peter@peter-b.co.uk>
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

/*!
 * \file scheme_config.c
 * \brief Scheme API configuration procedures.
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

SCM_SYMBOL (system_error_sym,     "system-error");
SCM_SYMBOL (config_error_sym,     "config-error");
SCM_SYMBOL (unknown_encoding_sym, "unknown-encoding");
SCM_SYMBOL (parse_sym,            "parse");
SCM_SYMBOL (key_not_found_sym,    "key-not-found");
SCM_SYMBOL (group_not_found_sym,  "group-not-found");
SCM_SYMBOL (invalid_value_sym,    "invalid-value");

#define ASSERT_CFG_GROUP_KEY(subr) do { \
  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, subr); \
  SCM_ASSERT (scm_is_string (group_s), group_s, SCM_ARG2, subr); \
  SCM_ASSERT (scm_is_string (key_s), key_s, SCM_ARG3, subr); \
  } while (0);

/*! \brief Convert a GError to a Scheme error.
 *
 * Raise a Scheme exception for the given \a error, with the procedure
 * name \a subr. The \a error will be freed with g_clear_error(). Does
 * not return.
 *
 * The \a error will be converted to a Scheme error according to the
 * following rules:
 *
 * - If \a error is a GFileError, it will be converted to a
 *   system-error.
 *
 * - If \a error is an EdaConfigError, it will be converted to a
 *   config-error.
 *
 * - Otherwise, it will be converted to a misc-error.
 *
 * \bug For GFileErrors, the GLib error code will be returned rather
 *      than the system error code.
 *
 * param subr   name of failed procedure, or NULL.
 * param error  error to be converted to a Scheme exception.
 */
static void
error_from_gerror (const char *subr, GError **error)
{
  if (error == NULL || *error == NULL) {
    scm_misc_error (subr, "Unknown error", SCM_EOL);
  }

  GError *err = *error;

  scm_dynwind_begin (0);
  /* Make sure that the GError gets cleaned up when the non-local exit
   * occurs. */
  scm_dynwind_unwind_handler ((void (*)(void *)) g_clear_error, error,
                              SCM_F_WIND_EXPLICITLY);

  SCM rest;

  if (err->domain == EDA_ERROR || err->domain == G_FILE_ERROR) {
    /* File-related errors */
    scm_error (system_error_sym, subr, err->message, SCM_EOL,
               scm_list_1 (scm_from_int (err->code)));
  }

  if (err->domain == EDA_CONFIG_ERROR) {
    /* Configuration context-related errors */
    switch (err->code) {
      case EDA_CONFIG_ERROR_UNKNOWN_ENCODING:
        rest = scm_list_1 (unknown_encoding_sym);
        break;

      case EDA_CONFIG_ERROR_PARSE:
        rest = scm_list_1 (parse_sym);
        break;

      case EDA_CONFIG_ERROR_KEY_NOT_FOUND:
        rest = scm_list_1 (key_not_found_sym);
        break;

      case EDA_CONFIG_ERROR_GROUP_NOT_FOUND:
        rest = scm_list_1 (group_not_found_sym);
        break;

      case EDA_CONFIG_ERROR_INVALID_VALUE:
        rest = scm_list_1 (invalid_value_sym);
        break;

      default:
        rest = SCM_BOOL_F;
        break;
    }
    scm_error (config_error_sym, subr, err->message, SCM_EOL, rest);
  }

  /* All other errors */
  scm_misc_error (subr, err->message, SCM_EOL);

  scm_dynwind_end ();
  g_warn_if_reached ();
}

/*! \brief Get the default configuration context.
 * \par Function Description
 * Returns the configuration context for compiled-in default settings.
 *
 * \see eda_config_get_default_context().
 *
 * \note Scheme API: Implements the \%default-config-context procedure
 * in the (geda core config) module.
 *
 * \return an #EdaConfig smob for the default context.
 */
EDA_SCM_DEFINE (config_default_context, "%default-config-context", 0, 0, 0,
               (), "Get default configuration context.")
{
  return edascm_from_config (eda_config_get_default_context ());
}

/*! \brief Get the system configuration context.
 * \par Function Description
 * Returns the configuration context for system settings.
 *
 * \see eda_config_get_system_context().
 *
 * \note Scheme API: Implements the \%system-config-context procedure
 * in the (geda core config) module.
 *
 * \return an #EdaConfig smob for the system context.
 */
EDA_SCM_DEFINE (config_system_context, "%system-config-context", 0, 0, 0,
            (), "Get system configuration context.")
{
  return edascm_from_config (eda_config_get_system_context (NULL));
}

/*! \brief Get the user configuration context.
 * \par Function Description
 * Returns the configuration context for user settings.
 *
 * \see eda_config_get_user_context().
 *
 * \note Scheme API: Implements the \%user-config-context procedure
 * in the (geda core config) module.
 *
 * \return an #EdaConfig smob for the user context.
 */
EDA_SCM_DEFINE (config_user_context, "%user-config-context", 0, 0, 0,
               (), "Get user configuration context.")
{
  return edascm_from_config (eda_config_get_user_context ());
}

/*! \brief Get the configuration context for a path.
 * \par Function Description
 * Looks for a configuration file named "geda.conf" in \a path or a
 * parent directory.
 *
 * \see eda_config_get_context_for_path().
 *
 * \note Scheme API: Implements the \%path-config-context procedure in
 * the (geda core config) module.
 *
 * param [in] path_s Path to get context for, as a string.
 * \return an #EdaConfig smob for \a path.
 */
EDA_SCM_DEFINE (config_path_context, "%path-config-context", 1, 0, 0,
            (SCM path_s), "Get configuration context for a path.")
{
  EdaConfig *cfg;
  char      *path;
  SCM        result;

  SCM_ASSERT (scm_is_string (path_s), path_s, SCM_ARG1,
              scheme_config_path_context);

  scm_dynwind_begin (0);
  path = scm_to_utf8_string (path_s);
  scm_dynwind_free (path);

  cfg    = eda_config_get_context_for_file (path);
  result = edascm_from_config (cfg);

  scm_dynwind_end ();
  scm_remember_upto_here_1 (path_s);

  return result;
}

/*! \brief Get a configuration context's filename.
 * \par Function Description
 * Returns the underlying filename for the configuration context \a cfg,
 * or FALSE if it has no filename associated with it.
 *
 * \see eda_config_get_file().
 *
 * \note Scheme API: Implements the \%config-filename procedure in the
 * (geda core config) module.
 *
 * param cfg_s  #EdaConfig smob for configuration context.
 * \return string containing configuration filename.
 */
EDA_SCM_DEFINE (config_filename, "%config-filename", 1, 0, 0,
            (SCM cfg_s), "Get configuration filename.")
{
  EdaConfig  *cfg;
  const char *path;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_filename);

  scm_dynwind_begin (0);

  cfg  = edascm_to_config (cfg_s);

  path = eda_config_get_filename (cfg);

  SCM result = (path == NULL) ? SCM_BOOL_F : scm_from_utf8_string (path);

  scm_dynwind_end ();
  return result;
}

/*! \brief Load configuration parameters from file.
 * \par Function Description
 * Attempt to load configuration parameters for \a cfg_s from file.
 * Raises a system-error on failure.
 *
 * \see eda_config_load().
 *
 * \note Scheme API: Implements the \%config-load! procedure in the
 * (geda core config) module.
 *
 * param cfg_s  #EdaConfig smob for configuration context to load.
 * \return \a cfg_s.
 */
EDA_SCM_DEFINE (config_load_x, "%config-load!", 1, 0, 0,
            (SCM cfg_s), "Load configuration parameters from file.")
{
  EdaConfig *cfg;
  GError    *error;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_load_x);

  cfg   = edascm_to_config (cfg_s);
  error = NULL;

  if (!eda_config_load (cfg, &error)) {
    error_from_gerror (scheme_config_load_x, &error);
  }
  return cfg_s;
}

/*! \brief Test if configuration context has been loaded.
 * \par Function Description
 * Returns TRUE if \a cfg_s has been loaded from file at some point, and
 * FALSE otherwise.
 *
 * \see eda_config_is_loaded().
 *
 * \note Scheme API: Implements the \%config-loaded? procedure in the
 * (geda core config) module.
 *
 * param cfg_s  #EdaConfig smob of configuration context.
 * \return TRUE if \a cfg_s has been loaded; FALSE otherwise.
 */
EDA_SCM_DEFINE (config_loaded_p, "%config-loaded?", 1, 0, 0,
            (SCM cfg_s), "Test if configuration context has been loaded")
{
  EdaConfig *cfg;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_loaded_p);

  cfg = edascm_to_config (cfg_s);

  return eda_config_is_loaded (cfg) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*! \brief Save changes to a configuration context.
 * \par Function Description
 * Attempt to save configuration parameters for the context \a cfg_s
 * to its ssociated file. Raises a system-error on failure.
 *
 * \see eda_config_save().
 *
 * \note Scheme API: Implements the \%config-save! procedure in the
 * (geda core config) module.
 *
 * param cfg_s EdaConfig smob of configuration context.
 * \return \a cfg_s.
 */
EDA_SCM_DEFINE (config_save_x, "%config-save!", 1, 0, 0,
            (SCM cfg_s), "Save changes to a configuration context")
{
  EdaConfig *cfg;
  GError    *error;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_save_x);

  cfg   = edascm_to_config (cfg_s);
  error = NULL;

  if (!eda_config_save (cfg, &error)) {
    error_from_gerror (scheme_config_save_x, &error);
  }

  return cfg_s;
}

/*! \brief Test whether a configuration context was changed since last save/load.
 * \par Function Description
 * Determine whether the configuration context \a cfg has been altered
 * since it was last synchronised with the on-disk version by loading
 * or saving it.
 *
 * \see eda_config_is_changed().
 *
 * \note Scheme API: Implements the \%config-changed? procedure in the
 * (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * \return TRUE if \a cfg_s has unsaved changes, FALSE otherwise.
 */
EDA_SCM_DEFINE (config_changed_p, "%config-changed?", 1, 0, 0,
            (SCM cfg_s),
            "Test whether a configuration context was changed since last save/load.")
{
  EdaConfig *cfg;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_changed_p);

  cfg = edascm_to_config (cfg_s);

  return eda_config_is_changed (cfg) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*! \brief Get a configuration context's parent context.
 * \par Function Description
 * Return the parent context of the configuration context \a cfg, if
 * it has one, or FALSE otherwise.
 *
 * \see eda_config_get_parent().
 *
 * \note Scheme API: Implements the \%config-parent procedure in the
 * (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * \return parent context of \a cfg_s, or FALSE.
 */
EDA_SCM_DEFINE (config_parent, "%config-parent", 1, 0, 0,
               (SCM cfg_s), "Get a configuration context's parent context.")
{
  EdaConfig *cfg;
  EdaConfig *parent;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_parent);

  cfg    = edascm_to_config (cfg_s);
  parent = eda_config_get_parent (cfg);

  return (parent == NULL) ? SCM_BOOL_F : edascm_from_config (parent);
}

/*! \brief Set a configuration context's parent context.
 * \par Function Description
 * Set the parent context of the configuration context \a cfg to \a
 * parent.  If \a parent is FALSE, sets \a cfg as having no parent
 * context.
 *
 * \see eda_config_set_parent().
 *
 * \note Scheme API: Implements the \%set-config-parent! procedure in
 * the (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * param parent_s #EdaConfig smob of new parent context, or FALSE.
 * \return cfg_s.
 */
EDA_SCM_DEFINE (config_set_parent_x, "%set-config-parent!", 2, 0, 0,
               (SCM cfg_s, SCM parent_s),
              "Set a configuration context's parent context.")
{
  EdaConfig *cfg;
  EdaConfig *parent;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_set_parent_x);
  SCM_ASSERT (scm_is_false (parent_s) || EDASCM_CONFIGP (parent_s), cfg_s,
              SCM_ARG2, scheme_config_set_parent_x);

  cfg    = edascm_to_config (cfg_s);
  parent = EDASCM_CONFIGP (parent_s) ? edascm_to_config (parent_s) : NULL;

  eda_config_set_parent (cfg, parent);

  return cfg_s;
}

/*! \brief Test if a configuration context is trusted.
 * \par Function Description
 * Tests if \a cfg_s is a "trusted" configuration context
 * (i.e. if it is permitted as a source for risky configuration
 * parameters such as system commands).
 *
 * \see eda_config_is_trusted().
 *
 * \note Scheme API: Implements the \%config-trusted? procedure in
 * the (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * \return TRUE if \a cfg_s is trusted, FALSE otherwise.
 */
EDA_SCM_DEFINE (config_trusted_p, "%config-trusted?", 1, 0, 0,
            (SCM cfg_s), "Test if a configuration context is trusted.")
{
  EdaConfig *cfg;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_trusted_p);

  cfg = edascm_to_config (cfg_s);

  return eda_config_is_trusted (cfg) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*! \brief Set whether a configuration context is trusted.
 * \par Function Description
 * Set whether the configuration context \a cfg_s is trusted as a
 * source for risky configuration parameters.
 *
 * \see eda_config_set_trusted().
 *
 * \note Scheme API: Implements the \%set-config-trusted! procedure in
 * the (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * param trusted_s FALSE if \a cfg_s is not to be trusted.
 *
 * \return cfg_s
 */
EDA_SCM_DEFINE (config_set_trusted_x, "%set-config-trusted!", 2, 0, 0,
               (SCM cfg_s, SCM trusted_s),
               "Set whether configuration context is trusted.")
{
  EdaConfig *cfg;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_set_trusted_x);

  cfg = edascm_to_config (cfg_s);

  eda_config_set_trusted (cfg, scm_is_true (trusted_s));

  return cfg_s;
}

/*! \brief Get a list of available configuration groups.
 * \par Function Description.
 * Returns a list of the all groups available in \a cfg_s and its parent
 * contexts.
 *
 * \see eda_config_get_groups().
 *
 * \note Scheme API: Implements the \%config-groups procedure in
 * the (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * \return a list of available group names as strings.
 */
EDA_SCM_DEFINE (config_groups, "%config-groups", 1, 0, 0,
            (SCM cfg_s),
            "Get a list of available configuration groups.")
{
  EdaConfig  *cfg;
  SCM         lst_s;
  char      **groups;
  unsigned    len;
  unsigned    i;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, scheme_config_groups);

  cfg    = edascm_to_config (cfg_s);
  groups = eda_config_get_groups (cfg, &len);
  lst_s  = SCM_EOL;

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler ((void (*)(void *)) g_strfreev,
                              groups, SCM_F_WIND_EXPLICITLY);

  for (i = 0; i < len; i++) {
    lst_s = scm_cons (scm_from_utf8_string (groups[i]), lst_s);
  }

  scm_dynwind_end ();

  return scm_reverse_x (lst_s, SCM_EOL);
}

/*! \brief Test whether a configuration context has a particular group.
 * \par Function Description
 * Tests whether the configuration context \a cfg_s, or any of its
 * parent contexts, contains the \a group_s.
 *
 * \see eda_config_has_group().
 *
 * \note Scheme API: Implements the \%config-has-group? procedure in
 * the (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * param group_s Group name as a string.
 * \return TRUE if \a cfg_s or any ancestor contains \a group, FALSE
 * otherwise.
 */
EDA_SCM_DEFINE (config_has_group_p, "%config-has-group?", 2, 0, 0,
               (SCM cfg_s, SCM group_s),
               "Test whether a configuration context has a particular group.")
{
  EdaConfig *cfg;
  char      *group;
  bool       result;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1,
              scheme_config_has_group_p);
  SCM_ASSERT (scm_is_string (group_s), group_s, SCM_ARG2,
              scheme_config_has_group_p);

  cfg    = edascm_to_config (cfg_s);
  group  = scm_to_utf8_string (group_s);
  result = eda_config_has_group (cfg, group);

  free (group);

  return result ? SCM_BOOL_T : SCM_BOOL_F;
}

/*! \brief Get a list of available configuration keys.
 * \par Function Description.
 * Returns a list of the all keys available in \a cfg_s and \a
 * group_s.  If the \a group_s cannot be found, raises a
 * 'config-error'.
 *
 * \see eda_config_get_keys().
 *
 * \note Scheme API: Implements the \%config-keys procedure in
 * the (geda core config) module.
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * param group_s Group name as a string.
 * \return a list of available key names as strings.
 */
EDA_SCM_DEFINE (config_keys, "%config-keys", 2, 0, 0,
               (SCM cfg_s, SCM group_s),
               "Get a list of available configuration keys.")
{
  EdaConfig *cfg;
  char      *group;
  GError    *error;
  SCM        lst_s;
  char     **keys;
  unsigned   len;
  unsigned   i;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, scheme_config_keys);
  SCM_ASSERT (scm_is_string (group_s), group_s, SCM_ARG2, scheme_config_keys);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  error = NULL;
  lst_s = SCM_EOL;

  keys = eda_config_get_keys (cfg, group, &len, &error);

  free (group);

  if (keys == NULL) {
    error_from_gerror (scheme_config_keys, &error);
  }

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler ((void (*)(void *)) g_strfreev,
                              keys, SCM_F_WIND_EXPLICITLY);

  for (i = 0; i < len; i++) {
    lst_s = scm_cons (scm_from_utf8_string (keys[i]), lst_s);
  }

  scm_dynwind_end ();

  return scm_reverse_x (lst_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as a boolean.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * boolean.
 *
 * \see eda_config_get_boolean().
 *
 * \note Scheme API: Implements the \%config-boolean procedure in the
 * (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 *
 * \return configuration value as a boolean.
 */
EDA_SCM_DEFINE (config_boolean, "%config-boolean", 3, 0, 0,
               (SCM  cfg_s, SCM group_s, SCM key_s),
               "Get a configuration parameter's value as a boolean.")
{
  EdaConfig *cfg;
  GError    *error;
  char      *group;
  char      *key;
  bool       value;

  ASSERT_CFG_GROUP_KEY (scheme_config_boolean);

  scm_dynwind_begin (0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  error = NULL;
  value = eda_config_get_boolean (cfg, group, key, &error);

  if (error != NULL) {
    error_from_gerror  (scheme_config_boolean, &error);
  }

  scm_dynwind_end ();

  return value ? SCM_BOOL_T : SCM_BOOL_F;
}

/*! \brief Get a configuration parameter's value as a boolean list.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of booleans.
 *
 * \see eda_config_get_boolean_list().
 *
 * \note Scheme API: Implements the \%config-boolean-list procedure in
 * the (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as a list of booleans.
 */
EDA_SCM_DEFINE (config_boolean_list, "%config-boolean-list", 3, 0, 0,
           (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a boolean list.")
{
  ASSERT_CFG_GROUP_KEY (scheme_config_boolean_list);

  EdaConfig *cfg;
  char      *group;
  char      *key;
  unsigned   length;
  unsigned   i;
  GError    *error;
  bool      *value;

  scm_dynwind_begin(0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);

  scm_dynwind_free (group);

  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (key);

  error = NULL;
  value = eda_config_get_boolean_list (cfg, group, key, &length, &error);

  if (value == NULL) {
    error_from_gerror (scheme_config_boolean_list, &error);
  }

  scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);

  SCM value_s = SCM_EOL;

  for (i = 0; i < length; i++) {
    value_s = scm_cons (value[i] ? SCM_BOOL_T : SCM_BOOL_F, value_s);
  }

  scm_dynwind_end ();

  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as an integer.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * integer.
 *
 * \see eda_config_get_integer().
 *
 * \note Scheme API: Implements the \%config-int procedure in the
 * (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as an integer.
 */
EDA_SCM_DEFINE (config_int, "%config-int", 3, 0, 0,
               (SCM  cfg_s, SCM group_s, SCM key_s),
               "Get a configuration parameter's value as an integer.")
{
  EdaConfig *cfg;
  GError    *error;
  char      *group;
  char      *key;
  int        value;

  ASSERT_CFG_GROUP_KEY (scheme_config_int);

  scm_dynwind_begin (0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  error = NULL;
  value = eda_config_get_integer (cfg, group, key, &error);

  if (error != NULL) {
    error_from_gerror  (scheme_config_int, &error);
  }

  scm_dynwind_end ();

  return scm_from_int (value);
}

/*! \brief Get a configuration parameter's value as a real.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as an
 * inexact real number.
 *
 * \see eda_config_get_double().
 *
 * \note Scheme API: Implements the \%config-real procedure in the
 * (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as an inexact real.
 */
EDA_SCM_DEFINE (config_real, "%config-real", 3, 0, 0,
           (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a real.")
{
  EdaConfig *cfg;
  GError    *error;
  char      *group;
  char      *key;
  double     value;

  ASSERT_CFG_GROUP_KEY (scheme_config_real);

  scm_dynwind_begin (0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  error = NULL;
  value = eda_config_get_double (cfg, group, key, &error);

  if (error != NULL) {
    error_from_gerror (scheme_config_real, &error);
  }

  scm_dynwind_end ();

  return scm_from_double (value);
}

/*! \brief Get the originating context for a configuration parameter.
 * \par Function Description
 * Returns the configuration context (either \a cfg_s or one of its
 * parent contexts) in which the configuration parameter with the
 * given \a group_s and \a key_s has a value specified.  If the group
 * or key cannot be found, raises a 'config-error'.
 *
 * \see eda_config_get_source().
 *
 * \note Scheme API: Implements the \%config-source procedure in the
 * (geda core config module).
 *
 * param cfg_s #EdaConfig smob of configuration context.
 * param group_s Group name as a string.
 * param key_s Key name as a string.
 * \return #EdaConfig smob of originating context of parameter.
 */
EDA_SCM_DEFINE (config_source, "%config-source", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get the originating context for a configuration parameter")
{
  EdaConfig *cfg;
  EdaConfig *src;
  GError    *error;
  char      *group;
  char      *key;

  ASSERT_CFG_GROUP_KEY (scheme_config_source);

  scm_dynwind_begin (0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  error = NULL;
  src   = eda_config_get_source (cfg, group, key, &error);

  if (src == NULL) {
    error_from_gerror (scheme_config_source, &error);
  }

  scm_dynwind_end ();

  return edascm_from_config (src);
}

/*! \brief Get a configuration parameter's value as a string.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * string.
 *
 * \see eda_config_get_string().
 *
 * \note Scheme API: Implements the \%config-string procedure in the
 * (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as a string.
 */
EDA_SCM_DEFINE (config_string, "%config-string", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a string.")
{
  EdaConfig *cfg;
  GError    *error;
  char      *group;
  char      *key;
  char      *value;

  ASSERT_CFG_GROUP_KEY (scheme_config_string);

  scm_dynwind_begin (0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  error = NULL;
  value = eda_config_get_string (cfg, group, key, &error);

  if (value == NULL) {
    error_from_gerror  (scheme_config_string, &error);
  }

  scm_dynwind_unwind_handler ((void (*)(void *)) g_free, value,
                              SCM_F_WIND_EXPLICITLY);
  SCM value_s = scm_from_utf8_string (value);

  scm_dynwind_end ();

  return value_s;
}

/*! \brief Get a configuration parameter's value as a string list.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of strings.
 *
 * \see eda_config_get_string_list().
 *
 * \note Scheme API: Implements the \%config-string-list procedure in
 * the (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as a list of strings.
 */
EDA_SCM_DEFINE (config_string_list, "%config-string-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a string list.")
{
  EdaConfig *cfg;
  GError    *error;
  unsigned   length;
  int        i;
  char      *group;
  char      *key;
  char     **value;

  ASSERT_CFG_GROUP_KEY (scheme_config_string_list);

  scm_dynwind_begin (0);

  cfg    = edascm_to_config (cfg_s);
  group  = scm_to_utf8_string (group_s);
  key    = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  error = NULL;
  value = eda_config_get_string_list (cfg, group, key, &length, &error);

  if (value == NULL) {
    error_from_gerror  (scheme_config_string_list, &error);
  }

  scm_dynwind_unwind_handler ((void (*)(void *)) g_strfreev, value,
                               SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;
  for (i = 0; i < length; i++) {
    value_s = scm_cons (scm_from_utf8_string (value[i]), value_s);
  }

  scm_dynwind_end ();

  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as an integer list.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of integers.
 *
 * \see eda_config_get_int_list().
 *
 * \note Scheme API: Implements the \%config-int-list procedure in
 * the (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as a list of integers.
 */
EDA_SCM_DEFINE (config_int_list, "%config-int-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as an integer list.")
{
  ASSERT_CFG_GROUP_KEY (scheme_config_int_list);

  GError    *error;
  EdaConfig *cfg;
  char      *group;
  char      *key;;
  unsigned   length;
  unsigned   i;
  int       *value;

  scm_dynwind_begin (0);

  cfg   = edascm_to_config(cfg_s);
  group = scm_to_utf8_string(group_s);
  key   = scm_to_utf8_string(key_s);

  scm_dynwind_free(group);
  scm_dynwind_free(key);

  error = NULL;
  value = eda_config_get_int_list(cfg, group, key, &length, &error);

  if (value == NULL) {
    error_from_gerror(scheme_config_int_list, &error);
  }

  scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;

  for (i = 0; i < length; i++) {
    value_s = scm_cons (scm_from_int (value[i]), value_s);
  }

  scm_dynwind_end ();

  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as a list of reals.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of inexact real numbers.
 *
 * \see eda_config_get_double_list().
 *
 * \note Scheme API: Implements the \%config-real-list procedure in
 * the (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * \return configuration value as a list of inexact real numbers.
 */
EDA_SCM_DEFINE (config_real_list, "%config-real-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a list of reals.")
{
  ASSERT_CFG_GROUP_KEY (scheme_config_real_list);

  GError    *error;
  EdaConfig *cfg;
  char      *group;
  char      *key;;
  unsigned   length;
  unsigned   i;
  double    *value;

  scm_dynwind_begin (0);

  cfg   = edascm_to_config(cfg_s);
  group = scm_to_utf8_string(group_s);
  key   = scm_to_utf8_string(key_s);

  scm_dynwind_free(group);
  scm_dynwind_free(key);

  error = NULL;
  value = eda_config_get_double_list(cfg, group, key, &length, &error);

  if (value == NULL) {
    error_from_gerror(scheme_config_real_list, &error);
  }

  scm_dynwind_unwind_handler(g_free, value, SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;

  for (i = 0; i < length; i++) {
    value_s = scm_cons(scm_from_double (value[i]), value_s);
  }

  scm_dynwind_end ();

  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Set a configuration parameter's value.
 * \par Function Description
 * Set the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s to \a
 * value_s.  The supported types for \a value_s are strings, integers,
 * real numbers, and booleans, along with homogenous lists of strings,
 * integers, real numbers or booleans.
 *
 * \note Scheme API: Implements the \%set-config! procedure in the
 * (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param group_s  Group name as a string.
 * param key_s    Key name as a string.
 * param value_s  New value for parameter.
 * \return \a cfg_s.
 */
EDA_SCM_DEFINE (config_set_x, "%set-config!", 4, 0, 0,
               (SCM cfg_s, SCM group_s, SCM key_s, SCM value_s),
               "Set a configuration parameter's value.")
{
  EdaConfig *cfg;
  char      *group;
  char      *key;

  ASSERT_CFG_GROUP_KEY (scheme_config_set_x);

  scm_dynwind_begin (0);

  cfg   = edascm_to_config (cfg_s);
  group = scm_to_utf8_string (group_s);
  key   = scm_to_utf8_string (key_s);

  scm_dynwind_free (group);
  scm_dynwind_free (key);

  /* Figure out what value is */
  if (scm_is_string (value_s)) {

    char *value = scm_to_utf8_string (value_s);

    scm_dynwind_free (value);

    eda_config_set_string (cfg, group, key, value);
  }
  else if (scm_is_bool (value_s)) {

    bool value = scm_is_true (value_s);

    eda_config_set_boolean (cfg, group, key, value);
  }
  else if (scm_is_integer (value_s) && scm_is_true (scm_exact_p (value_s))) {

    int value = scm_to_int (value_s);

    eda_config_set_integer (cfg, group, key, value);
  }
  else if (scm_is_real (value_s)) {

    double value = scm_to_double (value_s);

    eda_config_set_double (cfg, group, key, value);

  }
  else if (scm_is_true (scm_list_p (value_s))) {

    /* Find out what sort of list it is, then process it accordingly. */
    SCM first_s = scm_car (value_s);
    int len = scm_to_int (scm_length (value_s));
    SCM curr_s;
    int i = 0;

    if (scm_is_string (first_s)) {

      char **value = GEDA_MEM_ALLOC0 ((sizeof(char*) * len) + 1);

      scm_dynwind_unwind_handler ((void (*)(void *)) g_strfreev, value,
                                  SCM_F_WIND_EXPLICITLY);

      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {

        char *str;

        str = scm_to_utf8_string (scm_car (curr_s));

        value [i++] = geda_utility_string_strdup (str);

        free (str);
      }

      eda_config_set_string_list (cfg, group, key,
                                  (const char * const *) value, len);

    }
    else if (scm_is_bool (first_s)) {

      bool *value = GEDA_MEM_ALLOC0 (sizeof(bool) * len);

      scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);

      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        value[i++] = scm_is_true (scm_car (curr_s));
      }

      eda_config_set_boolean_list (cfg, group, key, value, len);

    }
    else if (scm_is_integer (first_s) &&
             scm_is_true (scm_exact_p (first_s)))
    {
      int *value = GEDA_MEM_ALLOC0 (sizeof(int) * len);

      scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);

      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        value[i++] = scm_to_int (scm_car (curr_s));
      }

      eda_config_set_int_list (cfg, group, key, value, len);

    }
    else if (scm_is_real (first_s)) {

      double *value = GEDA_MEM_ALLOC0 (sizeof(double) * len);

      scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);

      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        value[i++] = scm_to_double (scm_car (curr_s));
      }

      eda_config_set_double_list (cfg, group, key, value, len);

    }
    else {
      scm_wrong_type_arg (scheme_config_set_x, SCM_ARG4, value_s);
    }
  }
  else {
    scm_wrong_type_arg (scheme_config_set_x, SCM_ARG4, value_s);
  }

  scm_remember_upto_here_1 (value_s);
  scm_dynwind_end ();

  return cfg_s;
}

/*! \brief Dispatch to a Scheme configuration change event handler.
 * \par Function Description
 * Dispatcher function used by the Scheme API to run Scheme procedures
 * when a configuration change occurs.
 */
static void
scheme_config_event_dispatcher (EdaConfig *cfg, const char *group,
                                const char *key, void *user_data)
{
  SCM proc_s = (SCM) user_data;
  SCM expr = scm_list_4 (proc_s,
                         edascm_from_config (cfg),
                         scm_from_utf8_string (group),
                         scm_from_utf8_string (key));

  g_evaluate_scm_protected (expr, scm_interaction_environment ());
  scm_remember_upto_here_1 (expr);
}

/*! \brief Add a configuration change event handler.
 * \par Function Description
 * Add \a proc_s as a function to be called when configuration is
 * modified in the context \a cfg.  \a proc_s will be called with the
 * following prototype:
 *
 * \code
 * (proc CFG GROUP KEY)
 * \endcode
 *
 * If \a proc_s causes an Scheme error to be raised, the error will be
 * caught and logged.
 *
 * \note Scheme API: Implements the \%add-config-event! procedure in the
 * (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param proc_s   Procedure to add as configuration change handler.
 * \return \a cfg_s.
 */
EDA_SCM_DEFINE (config_add_event_x, "%add-config-event!", 2, 0, 0,
            (SCM cfg_s, SCM proc_s),
            "Add a configuration change event handler.")
{
  unsigned int  signal_id;
  unsigned long handler_id;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, scheme_config_add_event_x);
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc_s)),
              proc_s, SCM_ARG2, scheme_config_add_event_x);

  EdaConfig *cfg = edascm_to_config (cfg_s);

  /* Test if proc_s was already connected. */
  signal_id = g_signal_lookup ("config-changed", EDA_TYPE_CONFIG);

  handler_id = g_signal_handler_find (cfg,
                                      G_SIGNAL_MATCH_FUNC |
                                      G_SIGNAL_MATCH_DATA |
                                      G_SIGNAL_MATCH_ID,
                                      signal_id,
                                      0,
                                      NULL,
                                      scheme_config_event_dispatcher,
                                      (void*)proc_s);
  if (handler_id) {
    return cfg_s;
  }

  /* Protect proc_s against garbage collection */
  g_signal_connect (cfg, "config-changed",
                    G_CALLBACK (scheme_config_event_dispatcher),
                    (void*)scm_gc_protect_object (proc_s));
  return cfg_s;
}

/*! \brief Remove a configuration change event handler.
 * \par Function Description
 * Stop \a proc_s from being called when configuration is modified in
 * the context \a cfg.
 *
 * \note Scheme API: Implements the \%remove-config-event! procedure
 * in the (geda core config) module.
 *
 * param cfg_s    #EdaConfig smob of configuration context.
 * param proc_s   Procedure to remove as configuration change handler.
 * \return \a cfg_s.
 */
EDA_SCM_DEFINE (config_remove_event_x, "%remove-config-event!", 2, 0, 0,
            (SCM cfg_s, SCM proc_s),
            "Remove a configuration change event handler.")
{
  EdaConfig    *cfg;
  unsigned int  signal_id;
  unsigned int  found;

  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, scheme_config_remove_event_x);
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc_s)),
              proc_s, SCM_ARG2, scheme_config_remove_event_x);

  cfg       = edascm_to_config (cfg_s);
  signal_id = g_signal_lookup ("config-changed", EDA_TYPE_CONFIG);
  found     = g_signal_handlers_disconnect_matched (cfg,
                                                    G_SIGNAL_MATCH_FUNC |
                                                    G_SIGNAL_MATCH_DATA |
                                                    G_SIGNAL_MATCH_ID,
                                                    signal_id,
                                                    0,
                                                    NULL,
                                                    scheme_config_event_dispatcher,
                                                    (void *) proc_s);
  g_warn_if_fail (found < 2);

  if (found) {
    scm_gc_unprotect_object (proc_s);
  }

  return cfg_s;
}

/*!
 * \brief Create the (geda core config) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core config) module. The module can
 * be accessed using (use-modules (geda core config)).
 */
static void
init_module_geda_core_config (void *nothing)
{
  /* Register the functions and symbols */
  #include "scheme_config.x"

  /* Add them to the module's public definitions. */
  scm_c_export (scheme_config_changed_p,
                scheme_config_default_context,
                scheme_config_system_context,
                scheme_config_user_context,
                scheme_config_path_context,
                scheme_config_filename,
                scheme_config_load_x,
                scheme_config_loaded_p,
                scheme_config_groups,
                scheme_config_has_group_p,
                scheme_config_keys,
                scheme_config_boolean,
                scheme_config_boolean_list,
                scheme_config_int,
                scheme_config_int_list,
                scheme_config_parent,
                scheme_config_real,
                scheme_config_real_list,
                scheme_config_save_x,
                scheme_config_set_x,
                scheme_config_set_parent_x,
                scheme_config_set_trusted_x,
                scheme_config_string,
                scheme_config_string_list,
                scheme_config_source,
                scheme_config_trusted_p,
                scheme_config_add_event_x,
                scheme_config_remove_event_x,
                NULL);
}

/*!
 * \brief Initialize the basic gEDA configuration manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #EdaConfig
 * smobs. Should only be called by edascm_init().
 */
void
edascm_init_config (void)
{
  /* Define the (geda core object) module */
  scm_c_define_module ("geda core config",
                       init_module_geda_core_config,
                       NULL);
}
