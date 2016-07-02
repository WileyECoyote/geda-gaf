/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 2011-2015 gEDA Contributors (see ChangeLog for details)
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
 */
/* 02/27/14 WEH Revamp: Eliminate GFile due to unnecessary dependencies,
 *          and assumes the installed target allows vfs, which mine does
 *          not and so had to fix (or break) depending on ones point of
 *          view.
 *
 */
/*! \file geda_config.c
 *  \brief Geda Configuration Class
 */

/** \defgroup geda-config Geda Configuration Class
 * @{
 * \brief Implmentation of #EdaConfig Class
 * \par
 *  This module implements EdaConfig system in libgeda. The EdaConfig class
 *  is derived from the GObject base class.
 *
 * \class EdaConfig geda_config.h "include/libgeda/geda_config.h"
 */
#include <config.h>

#include <errno.h>
#include <libgen.h>

#include <libgeda_priv.h>

enum _EdaConfigPropertyId {
  PROP_0,
  PROP_CONFIG_FILE,
  PROP_CONFIG_PARENT,
  PROP_CONFIG_TRUSTED,
};

/*! \private \memberof EdaConfig
 * Private data for configuration context. */
struct _EdaConfigData
{
  /* Accessed via properties */
  EdaConfig    *parent;
  unsigned long parent_handler_id;
  bool          trusted;
  char         *filename;

  /* Other private data */
  GKeyFile     *keyfile;
  bool          loaded;
  bool          changed;
};

static void eda_config_dispose       (GObject *object);
static void eda_config_finalize      (GObject *object);
static void eda_config_set_property  (GObject *object, unsigned int property_id, const GValue *value, GParamSpec *pspec);
static void eda_config_get_property  (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec);
static bool eda_config_is_descendent (EdaConfig *cfg, EdaConfig *parent);

static void cclosure_marshal_VOID__STRING_STRING (GClosure *closure,
                                                  GValue *return_value,
                                                  unsigned int n_param_values,
                                                  const GValue *param_values,
                                                  void *invocation_hint,
                                                  void *marshal_data);
static void default_config_changed_handler (EdaConfig *cfg, const char *group, const char *key);
static void parent_config_changed_handler  (EdaConfig *parent, const char *group, const char* key, EdaConfig *cfg);
static void propagate_key_file_error       (GError *src, GError **dest);

static GObjectClass *eda_config_parent_class = NULL;

/*! Set a property of an EdaConfig instance. */
static
void eda_config_set_property (GObject *object, unsigned int property_id,
                              const GValue *value, GParamSpec *pspec)
{
  EdaConfig     *config = EDA_CONFIG (object);
  EdaConfig     *parent;
  EdaConfigData *priv = config->priv;

  switch (property_id) {

  case PROP_CONFIG_FILE:
    if (priv->filename != NULL) {
      GEDA_FREE (priv->filename);
    }
    priv->filename = g_value_dup_string (value);
    break;

  case PROP_CONFIG_PARENT:
    /* Check if new parent is a child context of config (loops are not
     * permitted). */
    parent = g_value_get_object (value);
    if (parent != NULL) {
      g_return_if_fail (EDA_IS_CONFIG (parent));
      g_return_if_fail (!eda_config_is_descendent (parent, config));
    }

    if (priv->parent != NULL) {
      /* Disconnect parent signal handler, if still connected. */
      if (g_signal_handler_is_connected (priv->parent,
                                         priv->parent_handler_id)) {
        g_signal_handler_disconnect (priv->parent,
                                     priv->parent_handler_id);
      }
      GEDA_UNREF (priv->parent);
      priv->parent_handler_id = 0;
    }
    if (parent != NULL) {
      config->priv->parent = g_object_ref (parent);
      /* Connect signal handler to new parent. */
      priv->parent_handler_id =
        g_signal_connect_object (parent,
                                 "config-changed",
                                 (GCallback) parent_config_changed_handler,
                                 config,
                                 G_CONNECT_SWAPPED);
    } else {
      config->priv->parent = NULL;
    }
    break;

  case PROP_CONFIG_TRUSTED:
    config->priv->trusted = g_value_get_boolean (value);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

/*! Get a property of an EdaConfig instance. */
static
void eda_config_get_property (GObject *object, unsigned int property_id,
                              GValue *value, GParamSpec *pspec)
{
  EdaConfig *config = EDA_CONFIG (object);
  switch (property_id) {
  case PROP_CONFIG_FILE:
    g_value_set_string (value, config->priv->filename);
    break;
  case PROP_CONFIG_PARENT:
    g_value_set_object (value, config->priv->parent);
    break;
  case PROP_CONFIG_TRUSTED:
    g_value_set_boolean (value, config->priv->trusted);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

/*! Dispose of an EdaConfig instance. Drop all references to other
 * GObjects, but keep the instance otherwise intact. May be run multiple
 * times (due to reference loops).
 */
static void eda_config_dispose (GObject *object)
{
  EdaConfig *config = EDA_CONFIG (object);

  g_object_set (object, "parent", NULL, NULL);

  if (config->RC_list != NULL) {
    g_list_foreach(config->RC_list, (GFunc) g_free, NULL);
    g_list_free(config->RC_list);
    config->RC_list = NULL;
  }

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_config_parent_class)->dispose (object);
}

/*! Finalize an EdaConfig instance. Free all resources held by the
 * instance. */
static void eda_config_finalize (GObject *object)
{
  EdaConfig *config = EDA_CONFIG (object);

  if (config->priv->filename != NULL) {
    GEDA_FREE (config->priv->filename);
  }

  g_key_file_free (config->priv->keyfile);

  GEDA_FREE (config->priv);

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_config_parent_class)->finalize (object);
}

/*! \brief Initialisze EdaConfig class
 *
 *  \par Function Description
 *  GedaType class initializer for EdaConfigClass. We override the
 *  parent virtual class methods as needed and register GObject
 *  signals.
 *
 *  \param [in,out] class       A EdaConfigClass GedaObject
 *  \param [in]     class_data  A EdaConfigClass data structure (unused)
 */
static void eda_config_class_init(void *class, void *class_data)
{
  EdaConfigClass *config_class = (EdaConfigClass*)class;
  GObjectClass   *object_class = G_OBJECT_CLASS (class);
  GParamSpec     *pspec;

  /* Register functions with base class */
  object_class->dispose        = eda_config_dispose;
  object_class->finalize       = eda_config_finalize;
  object_class->set_property   = eda_config_set_property;
  object_class->get_property   = eda_config_get_property;

  config_class->config_changed = default_config_changed_handler;

  /* Register properties */
  pspec = g_param_spec_string ("file", _("Configuration file"),
                              _("Set underlying file for EdaConfig"),
                                "",
                                G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);

  g_object_class_install_property (object_class,
                                   PROP_CONFIG_FILE,
                                   pspec);

  pspec = g_param_spec_object ("parent",
                               "Configuration context parent",
                               "Set parent configuration context for EdaConfig",
                               EDA_TYPE_CONFIG,
                               G_PARAM_CONSTRUCT | G_PARAM_READWRITE);
  g_object_class_install_property (object_class,
                                   PROP_CONFIG_PARENT,
                                   pspec);

  pspec = g_param_spec_boolean ("trusted",
                                "Whether context is trusted",
                                "Set whether configuration context is trusted config source.",
                                FALSE /* default value */,
                                G_PARAM_CONSTRUCT | G_PARAM_READWRITE);
  g_object_class_install_property (object_class,
                                   PROP_CONFIG_TRUSTED,
                                   pspec);

  /* Create signals */
  g_signal_new ("config-changed", /* signal name */
                G_TYPE_FROM_CLASS (object_class), /* type */
                G_SIGNAL_RUN_FIRST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS, /* flags */
                G_STRUCT_OFFSET(EdaConfigClass, config_changed), /* class offset */
                NULL, /* accumulator */
                NULL, /* accumulator data */
                cclosure_marshal_VOID__STRING_STRING, /* c_marshaller */
                G_TYPE_NONE, /* return type */
                2, /* no. of params */
                G_TYPE_STRING, G_TYPE_STRING);
}

/*! Initialize EdaConfig instance. */
static void
eda_config_instance_init(GTypeInstance *instance, void *class)
{
  EdaConfig *config     = (EdaConfig*)instance;

  config->instance_type = eda_config_get_type();

  config->priv          = GEDA_MEM_ALLOC0(sizeof(EdaConfigData));

  config->priv->parent            = NULL;
  config->priv->keyfile           = g_key_file_new ();
  config->priv->loaded            = FALSE;
  config->priv->changed           = FALSE;
  config->priv->parent_handler_id = 0;

  config->RC_list                 = NULL;
}

/*! \brief Retrieve EdaConfig GedaConfigType identifier.
 *
 *  \par Function Description
 *  Function to retrieve EdaConfig GedaConfigType identifier. Upon first
 *  call, this registers the EdaConfig in the Type system. The value
 *  retained from the first execution is returned on subsequent calls.
 *
 *  \return the GedaConfigType identifier associated with EdaConfig.
 */
GedaConfigType eda_config_get_type (void)
{
  static volatile GedaConfigType eda_config_type = 0;

  if (g_once_init_enter (&eda_config_type)) {

    static const GTypeInfo info = {
      sizeof(EdaConfigClass),
      NULL,                      /* base_init           */
      NULL,                      /* base_finalize       */
      eda_config_class_init,     /* (GClassInitFunc)    */
      NULL,                      /* class_finalize      */
      NULL,                      /* class_data          */
      sizeof(EdaConfig),
      0,                         /* n_preallocs         */
      eda_config_instance_init   /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaConfigType type;

    string = g_intern_static_string ("EdaConfig");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&eda_config_type, type);
  }

  return eda_config_type;
}

bool is_a_eda_config (const EdaConfig *cfg)
{
  if (G_IS_OBJECT(cfg)) {
    return (eda_config_get_type() == cfg->instance_type);
  }
  return FALSE;
}

/*! \brief Create an #EdaConfigError from a GKeyFileError.
 *
 * Propagate an error returned by a GKeyFile function, converting any
 * GKeyFileError found into a #EdaConfigError.  The \a src error will
 * be freed.
 *
 * \note We do this so that we can move away from using a GKeyFile
 *       internally if we want to at some point.
 *
 * \param src   Error to propagate.
 * \param dest  Target GError to set with error information.
 */
static void propagate_key_file_error (GError *src, GError **dest)
{
  if (src != NULL) {

    if (dest == NULL) {
      g_error_free (src);
    }
    else {

      g_return_if_fail (*dest == NULL);
      g_propagate_error (dest, src);

      if ((*dest)->domain == G_KEY_FILE_ERROR) {

        int code;

        switch ((*dest)->code) {
          case G_KEY_FILE_ERROR_UNKNOWN_ENCODING:
            code = EDA_CONFIG_ERROR_UNKNOWN_ENCODING;
            break;
          case G_KEY_FILE_ERROR_PARSE:
            code = EDA_CONFIG_ERROR_PARSE;
            break;
          case G_KEY_FILE_ERROR_KEY_NOT_FOUND:
            code = EDA_CONFIG_ERROR_KEY_NOT_FOUND;
            break;
          case G_KEY_FILE_ERROR_GROUP_NOT_FOUND:
            code = EDA_CONFIG_ERROR_GROUP_NOT_FOUND;
            break;
          case G_KEY_FILE_ERROR_INVALID_VALUE:
            code = EDA_CONFIG_ERROR_INVALID_VALUE;
            break;
          case G_KEY_FILE_ERROR_NOT_FOUND:
          default:
            BUG_IMSG("unhandled case <%d>", (*dest)->code);
            return;
        }

        (*dest)->domain = EDA_CONFIG_ERROR;
        (*dest)->code = code;
      }
    }
  }
}

/*! \public \memberof EdaConfig
 *
 *  \brief Look for Directory Containing Configuration File.
 *  Recursively searches upwards from \a path, looking for a
 *  "geda.conf" file.  If the root directory is reached without finding
 *  a configuration file, returns the directory part of \a path (if \a
 *  path points to a regular file) or \a path itself (if \a path is a
 *  directory).
 *
 *  \returns path of directory containing \a filename or directory
 *           portion of \a path if the file was not found
 */
char *
eda_config_find_project_root (const char *path, const char *filename)
{
  char *root_path = f_path_get_dirname (path);
  char *proj_root = root_path;
  char *dir       = root_path;

  int   found     = 0;

  if (filename == NULL) {
    filename = LOCAL_CONFIG_FILE;
  }

  while (dir && strlen(dir) > 1){
      char *filespec = geda_utility_string_concat(dir, DIR_SEPARATOR_S, filename, NULL);
      if (g_file_test (filespec, G_FILE_TEST_EXISTS)) {
        free(filespec);
        found = 1;
        break;
      }
      free(filespec);
      dir = dirname (dir);
  }

  if (found) {
    proj_root = geda_utility_string_strdup (dir);
    free(root_path);
  }
  else {
    free(root_path); /* This version was modified by dirname() */
    proj_root = f_path_get_dirname (path);
  }
  return proj_root;
}

/*! \public \memberof EdaConfig
 * \brief Return the default configuration context.
 *
 * The default context is not associated with any physical path or
 * on-disk configuration file, and has no parent context. The default
 * context contains the default configuration used when no configuration
 * file can be loaded.
 *
 * Applications should normally populate the default context with
 * their built-in default configuration settings on start-up, before
 * loading any further configuration files.
 *
 * \return the default #EdaConfig configuration context.
 */
EdaConfig *
eda_config_get_default_context ()
{
  static volatile GedaType initialized = 0;
  static EdaConfig *config = NULL;

  if (g_once_init_enter (&initialized)) {

    config = g_object_new (EDA_TYPE_CONFIG, "trusted", TRUE, NULL);

    config->priv->filename = NULL;

    config->priv->loaded = TRUE;

    g_once_init_leave (&initialized, 1);

  }
  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return the system configuration context.
 *
 * The system context is used for system-wide configuration.  It is
 * located:
 *
 * -# By searching "${XDG_CONFIG_DIRS}" for a "gEDA/geda-system.conf"
 *    configuration file.
 * -# By checking "${sysconfdir}/gEDA" for a configuration file.
 *
 * Its parent context is the default context.
 *
 * \return the system #EdaConfig configuration context.
 */
EdaConfig *
eda_config_get_system_context (const char *context)
{
  static volatile GedaType initialized = 0;
  static EdaConfig *config = NULL;

  if (g_once_init_enter (&initialized)) {
    char *filename = NULL;
    char *filespec = NULL;

    if ( context == NULL) {
      filename=geda_utility_string_strdup(SYSTEM_CONFIG_FILE);
    }
    else {
      filename = geda_utility_string_concat(context, GEDA_CONFIG_SYS_SUFFIX, NULL);
    }

#if defined (_WIN32) || defined (GEDA_USE_XDG)

    /* Search for a system configuration file in XDG_CONFIG_DIRS */
    const char * const *sys_dirs;
    int i;

    sys_dirs = g_get_system_config_dirs ();

    for (i = 0; sys_dirs[i] != NULL; i++) {
      filespec = g_build_filename (sys_dirs[i], filename, NULL);
      if (g_file_test (filespec, G_FILE_TEST_EXISTS)) {
        /* Don't free if we found, it is the one were going to use */{

          if (sys_dirs[0] != NULL) {
            filespec = g_build_filename (sys_dirs[0], filename, NULL);
          }
          else
            filespec = g_build_filename (DEFAULT_CONFIG_DIR, filename, NULL);
        }
        break;
      }

      GEDA_FREE (filespec);
      filespec = NULL;
    }

#else
    filespec = g_build_filename (GEDA_CONFIG_SYSDIR, filename, NULL);
    if (!g_file_test (filespec, G_FILE_TEST_EXISTS)) {
      GEDA_FREE (filespec);
      filespec = NULL;
    }
#endif

    /* If we didn't find a configuration file, just use a filename in
     * the traditional location. */
    if (filespec == NULL) {
      filespec = g_build_filename (f_path_sys_config (), filename, NULL);
      if (!g_file_test (filespec, G_FILE_TEST_EXISTS)) {
        GEDA_FREE (filespec);
        filespec = NULL;
      }
    }

    /* Finally, fall back to default location */
#if defined (_WIN32) || defined (GEDA_USE_XDG)
    if (filespec == NULL) {
      if ( context == NULL) {
        filespec=g_build_filename (sys_dirs[0], GEDA_CONFIG_DIR, SYSTEM_CONFIG_FILE, NULL);
      }
      else {
        filespec=g_build_filename (sys_dirs[0], GEDA_CONFIG_DIR, filename, NULL);
      }
    }
#else
    if (filespec == NULL) {
      if ( context == NULL) {
        filespec=g_build_filename (GEDA_CONFIG_SYSDIR, SYSTEM_CONFIG_FILE, NULL);
      }
      else {
        filespec=g_build_filename (GEDA_CONFIG_SYSDIR, filename, NULL);
      }
    }
#endif

    g_return_val_if_fail (filespec != NULL, NULL);

    config = g_object_new (EDA_TYPE_CONFIG,
                           "file", filespec,
                           "parent", eda_config_get_default_context (),
                           "trusted", TRUE,
                           NULL);

    eda_config_load (config, NULL);

    GEDA_FREE (filespec);
    GEDA_FREE (filename);
    g_once_init_leave (&initialized, 1);
  }
  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return the user configuration context.
 *
 * The user context is used for user-specific configuration, and is
 * loaded from "${XDG_CONFIG_HOME}/gEDA/geda-user.conf". Its parent
 * context is the system context.
 *
 * \return the user #EdaConfig configuration context.
 */
EdaConfig *
eda_config_get_user_context (void)
{
  static volatile GedaType initialized = 0;
  static EdaConfig *config = NULL;

  const char *app_name = g_get_prgname();

  if ( app_name == NULL ) {
    app_name = DEFAULT_CONTEXT;
  }

  if (g_once_init_enter (&initialized)) {

    char *filename = NULL;
    char *tmpname;

    tmpname  = geda_utility_string_concat(app_name, GEDA_CONFIG_USER_SUFFIX, NULL);
    filename = g_build_filename(f_path_user_config(), tmpname, NULL);
    GEDA_FREE (tmpname);

    config = g_object_new (EDA_TYPE_CONFIG,
                           "file", filename,
                           "parent", eda_config_get_system_context (app_name),
                           "trusted", TRUE,
                           NULL);

    eda_config_load (config, NULL);

    GEDA_FREE (filename);
    g_once_init_leave (&initialized, 1);
  }
  return config;
}

static bool strhashcmp (const void *a, const void *b) {
  int answer = 0;
  if (((char*)a != '\0') && ((char*)b != '\0')) {
     answer = strcmp ((const char*) a, (const char*) b) == 0;
  }

  return answer;
}

/*! \public \memberof EdaConfig
 * \brief Return a local configuration context.
 *
 * Looks for a configuration file named "geda.conf".  If \a path is
 * not a directory, it is truncated and then a file named "geda.conf"
 * is looked for in that directory.  If none is found, the parent
 * directory is checked, and so on until a configuration file is found
 * or the root directory is reached.  If no configuration file was
 * found, the returned context will be associated with a "geda.conf"
 * in the same directory as \a path.  If \a path is NULL, the current
 * working directory is used.
 *
 * \warning Do not assume that the configuration file associated with
 * the context returned by eda_config_get_context_for_file() is
 * located in the directory specified by \a path.
 *
 * By default, the parent context of the returned #EdaConfig will be
 * the user context.
 *
 * Multiple calls to eda_config_get_context_for_file() with the same
 * \a path will return the same configuration context.
 *
 * \see eda_config_get_context_for_path().
 *
 * \param [in] path Path to search for configuration from.
 *
 * \return a local #EdaConfig configuration context for \a path.
 */
EdaConfig *
eda_config_get_context_for_file (const char *path)
{
  static volatile GedaType initialized  = 0;
  static GHashTable *local_contexts = NULL;

  char *ptr;
  char  dir[PATH_MAX];

  EdaConfig *config = NULL;

  /* Initialise global state */
  if (g_once_init_enter (&initialized)) {
    local_contexts = g_hash_table_new_full (g_str_hash,
                                           (GEqualFunc) strhashcmp,
                                            NULL,
                                            NULL);
    g_once_init_leave (&initialized, 1);
  }

#if HAVE_REALPATH

  ptr = realpath(path, &dir[0]);

#else

  if (g_path_is_absolute(path)) {
    ptr = strncpy(&dir[0], path, PATH_MAX);
  }
  else {
    ptr = NULL;
  }

#endif

  if (ptr != NULL) {

    char *file;
    char *root;

    /* Find the project root, and the corresponding configuration filename. */
    if (!g_file_test (ptr, G_FILE_TEST_IS_DIR)) {
      ptr = dirname (ptr);                        /* strip the filename */
    }

    /* traverse path looking for figs */
    root = eda_config_find_project_root (ptr, LOCAL_CONFIG_FILE);

    file = g_build_filename(root, DIR_SEPARATOR_S, LOCAL_CONFIG_FILE, NULL);

    /* If there is already a context available for this file, return that.
     * Otherwise, create a new context and record it in the global state. */
    config = g_hash_table_lookup (local_contexts, file);

    if (config == NULL) {

      /* Before creating a new fig, check for alternate (old) rc file */
      g_free (root);
      g_free (file);

      /* traverse path looking for figs */
      root = eda_config_find_project_root (ptr, LOCAL_CONFIG_FILE_ALT);

      file = g_build_filename(root, DIR_SEPARATOR_S, LOCAL_CONFIG_FILE_ALT, NULL);

      config = g_hash_table_lookup (local_contexts, file);

      if (config == NULL) {

        EdaConfig *cfg = eda_config_get_user_context ();

        config = g_object_new (EDA_TYPE_CONFIG,
                               "file", file,
                               "parent", cfg,
                               "trusted", FALSE,
                               NULL);
        g_hash_table_insert (local_contexts, file, config);
      }
      else {
        g_free (file);
      }
    }
    else {
      g_free (file);
    }
    g_free (root);

  }
  else {

    /* The file does not exist so return the default context */
    config = g_object_new (EDA_TYPE_CONFIG,
                           "file", NULL,
                           "parent", eda_config_get_default_context(),
                           "trusted", FALSE, NULL);
  }

  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return a local configuration context.
 *
 * Looks for a configuration file named "geda.conf".  If \a path is
 * not a directory, it is truncated and then a file named "geda.conf"
 * is looked for in that directory.  If none is found, the parent
 * directory is checked, and so on until a configuration file is found
 * or the root directory is reached.  If no configuration file was
 * found, the returned context will be associated with a "geda.conf"
 * in the same directory as \a path.
 *
 * \warning Do not assume that the configuration file associated with
 * the context returned by eda_config_get_context_for_path() is
 * located in the directory specified by \a path.
 *
 * By default, the parent context of the returned #EdaConfig will be
 * the user context.
 *
 * Multiple calls to eda_config_get_context_for_path() with the same
 * \a path will return the same configuration context.
 *
 * \see eda_config_get_context_for_file().
 *
 * \param [in] path    Path to search for configuration from.
 * \return a local #EdaConfig configuration context for \a path.
 */
EdaConfig *
eda_config_get_context_for_path (const char *path)
{
  EdaConfig *config = NULL;
  if (path != NULL) {
    config = eda_config_get_context_for_file (path);
  }
  else {
    BUG_MSG("NULL path");
  }

  return config;
}

/*! \public \memberof EdaConfig
 * \brief Return underlying filename for configuration context.
 *
 * Return the filename of the configuration file associated with the
 * context \a cfg.  May return NULL.  The return value is owned by the
 * API and should not be modified or freed.
 *
 * \see eda_config_get_file().
 *
 * \param cfg  Configuration context.
 * \return Filename of configuration file for \a cfg.
 */
const char*
eda_config_get_filename (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);
  return cfg->priv->filename;
}

/*! \public \memberof EdaConfig
 * \brief Load configuration parameters from file.
 *
 * Attempt to load configuration parameters for the context \a cfg
 * from its associated file.  Returns FALSE and generates an Error
 * or #EdaConfigError on error.  If \a cfg does not have an associated
 * file, does nothing, returns FALSE, and generates a
 * G_IO_ERROR_FAILED error.
 *
 * \see eda_config_is_loaded(), eda_config_get_file(),
 * eda_config_save().
 *
 * \param cfg    Configuration context.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
bool
eda_config_load (EdaConfig *cfg, GError **error)
{
  bool status = FALSE;

  const char *filename;

  g_return_val_if_fail (EDA_IS_CONFIG (cfg), TRUE);

  filename = eda_config_get_filename (cfg);

  if (filename == NULL) {
    if(error != NULL) {
       g_set_error (error,
                 G_IO_ERROR,
                 G_IO_ERROR_FAILED,
                 _("Undefined configuration filename"));
    }
    else {
      fprintf(stderr, "Error loading configuration, file name is undefined\n");
    }
  }
  else {

    int key_file_flags = G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS;
    GKeyFile *key_file = g_key_file_new ();

    if (access(filename, R_OK) == 0) {

      char  *buf;
      size_t len;

      if (g_file_get_contents (filename, &buf, &len, error)) {

        if (len != 0) { /* Don't load zero-length keyfiles */
          status = g_key_file_load_from_data (key_file, buf, len,
                                              key_file_flags, error);
        }
        else {
          status = TRUE;
        }

        GEDA_FREE (buf);

        if (!status) {
          g_key_file_free (cfg->priv->keyfile);
        }
        else {
          /* Substitute in new key file object, and reset loaded and changed flags. */
          if (cfg->priv->keyfile) {
            g_key_file_free(cfg->priv->keyfile);
          }
          cfg->priv->keyfile = key_file;
          cfg->priv->changed = FALSE;
          cfg->priv->loaded  = TRUE;
        }
      }
    }
    else {

      if (errno == ENOENT) {

        /* Substitute in new key file object, and reset loaded and changed flags. */
        if(cfg->priv->keyfile) {
          g_key_file_free(cfg->priv->keyfile);
        }
        cfg->priv->keyfile = key_file;
        cfg->priv->changed = FALSE;
        cfg->priv->loaded  = TRUE;

      }
      else {

        if(error != NULL) {

          g_set_error(error, G_FILE_ERROR,
                      g_file_error_from_errno (errno),
                     "accessing file %s", filename);
        }
        else {
          fprintf(stderr, "Error loading configuration %s, %s\n",
                  filename, strerror(errno));
        }
      }
    }
  }

  return status;
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration context has been loaded.
 *
 * Test whether the configuration context \a cfg has been successfully
 * loaded from disk.
 *
 * \param cfg  Configuration context.
 * \return TRUE if \a cfg has been loaded at some point, FALSE
 * otherwise.
 */
bool
eda_config_is_loaded (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), TRUE);
  return cfg->priv->loaded;
}

/*! \public \memberof EdaConfig
 * \brief Save changes to a configuration context.
 *
 * Attempt to save configuration parameters for the context \a cfg to
 * its associated file.  Returns FALSE and generates a GError on
 * error.  If \a cfg does not have an associated file, does nothing,
 * returns FALSE, and generates a G_IO_ERROR_FAILED error.
 *
 * \see eda_config_load(), eda_config_get_file().
 *
 * \param cfg    Configuration context.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
bool
eda_config_save (EdaConfig *cfg, GError **error)
{
  bool status = FALSE;

  g_return_val_if_fail (EDA_IS_CONFIG (cfg), TRUE);

  if (cfg->priv->filename == NULL) {
    if(error != NULL) {
      g_set_error (error,
                   G_IO_ERROR,
                   G_IO_ERROR_FAILED,
                   _("Undefined configuration filename"));
    }
    else {
      fprintf(stderr, "Error saving configuration, file name is undefined\n");
    }
  }
  else {

    char *dir;
    char *filename;
    char *scratch;

    filename = cfg->priv->filename;
    scratch  = geda_utility_string_strdup(filename);

    /* First try and make the directory, if necessary. */
    dir = dirname (scratch);

    if (dir != NULL) {

      FILE *fp;

      errno = 0;
      if (!g_file_test (filename, G_FILE_TEST_EXISTS)) {
        if (!g_file_test (dir, G_FILE_TEST_EXISTS)) {
          f_path_create (dir, S_IRWXU | S_IRWXG);
          g_set_error(error, G_FILE_ERROR, status,
                      _("file <%s> %s"), filename, strerror(errno));
        }
      }

      errno = 0;
      if ((access(filename, W_OK) == 0) || (errno == ENOENT)) {
        errno = 0;
        fp = fopen(filename, "w");
        if (fp) {
          status = TRUE;
        }
        else {
          status = FALSE;
        }
      }

      if (!errno && status) {

        char *data = g_key_file_to_data(cfg->priv->keyfile, NULL, NULL);

        fprintf(fp, "%s", data);
        fclose(fp);
        GEDA_FREE(data);
        cfg->priv->changed = FALSE;
      }
      else {
        if (fp) {
          fclose(fp);
        }
        if(error != NULL) {
          g_set_error(error, G_FILE_ERROR,
                      g_file_error_from_errno (errno),
                      strerror(errno), filename);
        }
        else {
          fprintf(stderr, "Error saving configuration to %s, %s\n",
                  filename, strerror(errno));
        }
      }
    }
    else {
      if(error != NULL) {
        g_set_error(error, G_FILE_ERROR,
                    g_file_error_from_errno (errno),
                    _("bad path in file %s"), filename);
      }
      else {
        fprintf(stderr, "Error saving configuration, bad path in %s\n", filename);
      }

    }
    GEDA_FREE(scratch);
  }

  return status;
}

/*! \public \memberof EdaConfig
 * \brief Test whether configuration was changed since last saved/loaded.
 *
 * Determine whether the configuration context \a cfg has been altered
 * since it was last synchronised with the on-disk version by loading
 * or saving it.
 *
 * \see eda_config_save(), eda_config_load().
 *
 * \param cfg  Configuration context.
 * \return TRUE if altered since last load/save, FALSE otherwise.
 */
bool
eda_config_is_changed (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  return cfg->priv->changed;
}

/*! \public \memberof EdaConfig
 * \brief Get a configuration context's parent context.
 *
 * Return the parent context of the context \a cfg, if it has one; if
 * not, returns NULL.
 *
 * \param cfg  Configuration context.
 * \return parent context of \a cfg, or NULL.
 */
EdaConfig*
eda_config_get_parent (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);
  return cfg->priv->parent;
}

/*! \private \memberof EdaConfig
 * \brief Test whether one configuration context is child of another.
 *
 * Returns TRUE if \a cfg is a descendent context of \a parent,
 * directly or indirectly.
 *
 * \param cfg     Configuration context.
 * \param parent  Context to check if ancestor of \a cfg.
 * \return TRUE if \a parent is ancestor of \a cfg, FALSE otherwise.
 */
static bool eda_config_is_descendent (EdaConfig *cfg, EdaConfig *parent)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  g_return_val_if_fail (EDA_IS_CONFIG (parent), FALSE);

  EdaConfig *iter = cfg;
  while (iter != NULL) {
    if (iter == parent) return TRUE;
    iter = eda_config_get_parent (iter);
  }
  return FALSE;
}

/*! \public \memberof EdaConfig
 * \brief Set a configuration context's parent context.
 *
 * Sets \a parent as the parent context of \a cfg.  If \a parent is
 * NULL, sets \a cfg as having no parent context.  Inheritance loops
 * are not permitted.
 *
 * \warning Normally, application code should avoid using this
 *          function; keeping to the default configuration inheritance
 *          structure is recommended in order to ensure consistent
 *          behaviour of all libgeda applications.
 *
 * \param cfg     Configuration context.
 * \param parent  New parent context for \a cfg.
 */
void
eda_config_set_parent (EdaConfig *cfg, EdaConfig *parent)
{
  g_return_if_fail (EDA_IS_CONFIG (cfg));
  g_object_set (cfg, "parent", parent, NULL);
}

/*! \public \memberof EdaConfig
 * \brief Test whether a context is trusted.
 *
 * Returns TRUE if \a cfg is a "trusted" configuration context
 * (i.e. if it is permitted as a source for risky configuration
 * parameters such as system commands).
 *
 * \param cfg  Configuration context.
 * \return TRUE if \a cfg is trusted.
 */
bool
eda_config_is_trusted (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  return cfg->priv->trusted;
}

/*! \public \memberof EdaConfig
 * \brief Set whether a context is trusted.
 *
 * Set whether the configuration context \a cfg is trusted as a source
 * for risky configuration parameters.
 *
 * \warning You should not set a configuration context as trusted
 *          unless you are certain that it originated from a safe
 *          source (e.g. by interacting with the user to verify it).
 *
 * \param cfg      Configuration context.
 * \param trusted  TRUE if \a cfg should be trusted; FALSE otherwise.
 */
void
eda_config_set_trusted (EdaConfig *cfg, bool trusted)
{
  g_return_if_fail (EDA_IS_CONFIG (cfg));
  g_object_set (cfg, "trusted", trusted, NULL);
}

/*! \public \memberof EdaConfig
 * \brief Get a configuration contexts first trusted ancestor.
 *
 * Returns the first trusted configuration context that \a cfg
 * inherits from, or \a cfg if \a cfg is trusted.  If no trusted
 * context is found, returns NULL.
 *
 * \param cfg  Configuration context.
 * \return first trusted ancestor of \a cfg, or NULL.
 */
EdaConfig *
eda_config_get_trusted_context (EdaConfig *cfg)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);
  EdaConfig *iter = cfg;
  while (iter != NULL) {
    if (eda_config_is_trusted (iter)) return iter;
    iter = eda_config_get_parent (iter);
  }
  return NULL;
}

/*! \brief Turn hashtable into key list.
 *
 * Convert a hashtable with string keys and empty values into an array
 * of string pointers.  Used by eda_config_get_groups() and
 * eda_config_get_keys().
 */
static
char **hash_table_keys_array (GHashTable *table, unsigned int *length)
{
  unsigned int len = g_hash_table_size (table);
  char **result    = GEDA_MEM_ALLOC0 (sizeof(char*) * (len + 1));

  GHashTableIter iter;
  void *key;
  int i = 0;

  g_hash_table_iter_init (&iter, table);

  while (g_hash_table_iter_next (&iter, &key, NULL)) {
    g_hash_table_iter_steal (&iter);
    result[i++] = (char *) key;
  }

  result[i] = NULL;

  g_hash_table_destroy (table);

  if (length != NULL)
    *length = len;

  return result;
}

/*! \public \memberof EdaConfig
 *  \brief Return a list of a configuration context's available groups.
 *  \par Function Description
 * Returns a list of the all groups available in \a cfg and its parent
 * contexts.  The value returned by eda_config_get_groups() is a
 * newly-allocated NULL-terminated array of strings.  Use g_strfreev()
 * to free it.  The \a length argument is an optional return location
 * for the number of groups returned.
 *
 * \see eda_config_has_group().
 *
 * \todo The current implementation is not enormously efficient; we
 * can do better!
 *
 * \param cfg     Configuration context.
 * \param length  Return location for number of groups.
 * \return a newly-allocated NULL-terminated array of strings.
 */
char **
eda_config_get_groups (EdaConfig *cfg, unsigned *length)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);

  GHashTable *group_table = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                   g_free, NULL);

  /* Build a hashtable with all groups in current and parent contexts
   * as keys. */
  EdaConfig *curr = cfg;

  while (curr != NULL) {

    size_t len;
    int    i;

    char **local_groups = g_key_file_get_groups (curr->priv->keyfile, &len);

    for (i = 0; i < len; i++) {
      g_hash_table_insert (group_table, local_groups[i], NULL);
    }
    GEDA_FREE (local_groups);
    /* Keys are now owned by hashtable, do not need to use g_strfreev */

    curr = eda_config_get_parent (curr);
  }

  return hash_table_keys_array (group_table, length);
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration context has a particular group.
 *
 * Tests whether the configuration context \a cfg, or any of its
 * parent contexts, contains the \a group.
 *
 * \see eda_config_get_keys().
 *
 * \param cfg    Configuration context.
 * \param group  Group to check for.
 * \return TRUE if \a cfg or any of its ancestors contains \a group,
 * otherwise FALSE.
 */
bool
eda_config_has_group (EdaConfig *cfg, const char *group)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  g_return_val_if_fail (group != NULL, FALSE);

  EdaConfig *curr;

  for (curr = cfg; curr != NULL; curr = eda_config_get_parent (curr)) {

    if (g_key_file_has_group (curr->priv->keyfile, group)) {
      return TRUE;
    }

  }
  return FALSE;
}

/*! \public \memberof EdaConfig
 * \brief Get the keys available in a particular configuration group.
 *
 * Get a list of all keys available in the specified \a group in the
 * configuration context \a cfg and its parent contexts.  The value
 * returned by eda_config_get_keys() is a newly-allocated
 * NULL-terminated array of strings.  Use g_strfreev() to free it.
 * The \a length argument is an optional return location for the
 * number of keys returned.  If an error occurs, returns NULL.
 *
 * \see eda_config_has_key().
 *
 * \param cfg     Configuration context.
 * \param group   Group to get key list for.
 * \param length  Return location for number of keys, or NULL.
 * \param error   Return location for error information.
 */
char **
eda_config_get_keys (EdaConfig *cfg, const char *group, unsigned *length,
                     GError **error)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), NULL);

  GHashTable *key_table = NULL;
  EdaConfig  *curr;

  for (curr = cfg; curr != NULL; curr = eda_config_get_parent (curr)) {

    size_t len;
    int    i;
    char **local_keys = g_key_file_get_keys (curr->priv->keyfile,
                                              group, &len, NULL);
    /* Skip files that don't provide the requested group */
    if (local_keys == NULL) continue;

    /* Create keytable if not already created. */
    if (key_table == NULL) {
      key_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    }

    for (i = 0; i < len; i++) {
      g_hash_table_insert (key_table, local_keys[i], NULL);
    }
    GEDA_FREE (local_keys); /* Keys are now owned by hashtable, don't
                            need to use g_strfreev(). */
  }

  /* If the hashtable was never created, it means that no matching
   * group was found. */
  if (key_table == NULL) {
    g_set_error (error, EDA_CONFIG_ERROR,
                 EDA_CONFIG_ERROR_GROUP_NOT_FOUND,
                 _("Configuration does not have group '%s'\n"),
                 group ? group : "(null)");
    return NULL;
  }

  return hash_table_keys_array (key_table, length);
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration context has a particular key.
 *
 * Tests whether the configuration context \a cfg, or any of its
 * parent contexts, contains the parameter specified by \a group and
 * \a key.  If \a group was not found, returns FALSE and sets \a error.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_get_keys().
 *
 * \param cfg    Configuration context.
 * \param group  Group to look for \a key in.
 * \param key    Key to check for.
 * \param error  Return location for error information.
 * \return TRUE if \a cfg or any of its ancestors contains \a group
 * and \a key, otherwise FALSE.
 */
bool
eda_config_has_key (EdaConfig *cfg, const char *group,
                    const char *key, GError **error)
{
  return (eda_config_get_source (cfg, group, key, error) != NULL);
}

/*! \public \memberof EdaConfig
 * \brief Obtain the originating context for a configuration
 * parameter.
 *
 * Returns the configuration context (either \a cfg or one of its
 * parent contexts) in which the configuration parameter with the
 * given \a group and \a key has a value specified.  If the group or
 * key cannot be found, returns FALSE and sets \a error.
 *
 * \see eda_config_is_inherited().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 */
EdaConfig *
eda_config_get_source (EdaConfig *cfg, const char *group,
                       const char *key, GError **error)
{
  g_return_val_if_fail (EDA_IS_CONFIG (cfg), FALSE);
  g_return_val_if_fail (group != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);

  if (!eda_config_has_group (cfg, group)) {
    g_set_error (error, EDA_CONFIG_ERROR,
                 EDA_CONFIG_ERROR_GROUP_NOT_FOUND,
                 _("Configuration does not have group '%s'"),
                 group);
    return NULL;
  }

  EdaConfig *curr;
  for (curr = cfg; curr != NULL; curr = eda_config_get_parent (curr)) {
    if (g_key_file_has_key (curr->priv->keyfile, group, key, NULL)) {
      return curr;
    }
  }

  g_set_error (error, EDA_CONFIG_ERROR,
               EDA_CONFIG_ERROR_KEY_NOT_FOUND,
               _("Configuration does not have key '%s'"),
               key);
  return NULL;
}

/*! \public \memberof EdaConfig
 * \brief Test whether a configuration parameter is inherited.
 *
 * Tests whether the value of the configuration parameter with the
 * given \a group and \a key is specified in the context \a cfg, or
 * whether it is inherited from a parent context of \a cfg.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_get_source().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 */
bool
eda_config_is_inherited (EdaConfig *cfg, const char *group,
                         const char *key, GError **error)
{
  return (eda_config_get_source (cfg, group, key, error) != cfg);
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a string.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a string.  If an
 * error occurs, NULL is returned and \a error is set.
 *
 * The returned string is owned by the caller, and should be freed
 * with GEDA_FREE() when no longer needed.
 *
 * \see eda_config_set_string().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as string, or NULL.
 */
char *
eda_config_get_string (EdaConfig *cfg, const char *group,
                       const char *key, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *sys_err = NULL;
  char   *result  =  g_key_file_get_string (cfg->priv->keyfile, group, key, &sys_err);
  propagate_key_file_error (sys_err, error);

  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a boolean.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a boolean.  If
 * an error occurs, FALSE is returned and \a error is set.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_set_boolean().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as a boolean.
 */
bool
eda_config_get_boolean (EdaConfig *cfg, const char *group,
                        const char *key, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return FALSE;

  GError *sys_err = NULL;
  bool result =
    g_key_file_get_boolean (cfg->priv->keyfile, group, key, &sys_err);
  propagate_key_file_error (sys_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as an integer.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as an integer.  If
 * an error occurs, 0 is returned and \a error is set.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_set_integer().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as a integer.
 */
int
eda_config_get_integer (EdaConfig *cfg, const char *group,
                        const char *key, GError **error)
{
  int result;
  GError *sys_err;

  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) {
    result = 0;
  }
  else {
    sys_err = NULL;
    result = g_key_file_get_integer (cfg->priv->keyfile, group, key, &sys_err);
    propagate_key_file_error (sys_err, error);
  }
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a double.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a double.  If
 * an error occurs, 0.0 is returned and \a error is set.
 *
 * \note This function does not folow the rules for GError strictly;
 *       the return value both carries meaning and signals an error.
 *       To use this function, you must pass a GError pointer in \a
 *       error, and check whether it is not NULL to see if an error
 *       occurred.
 *
 * \see eda_config_set_double().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param error  Return location for error information.
 * \return configuration value as a double.
 */
double
eda_config_get_double (EdaConfig *cfg, const char *group,
                       const char *key, GError **error)
{
  double result;
  GError *sys_err;

  cfg = eda_config_get_source (cfg, group, key, error);

  if (cfg == NULL) {
    result = 0.0;
  }
  else {
    sys_err = NULL;
    result = g_key_file_get_double (cfg->priv->keyfile, group, key, &sys_err);
    propagate_key_file_error (sys_err, error);
  }
  return result;
}


/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a string list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated NULL-terminated array of strings.  If an error
 * occurs, NULL is returned and \a error is set.  The returned value
 * should be freed with g_strfreev() when no longer needed.
 *
 * \see eda_config_set_string_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length, or NULL.
 * \param error  Return location for error information.
 * \return configuration value as an array of strings.
 */
char **
eda_config_get_string_list (EdaConfig *cfg, const char *group,
                            const char *key,size_t *length, GError **error)
{
  GError *sys_err;
  char **result;

  cfg = eda_config_get_source (cfg, group, key, error);

  if (cfg == NULL) {
    result = NULL;
  }
  else {
    sys_err = NULL;
    result =  g_key_file_get_string_list (cfg->priv->keyfile, group, key,
                                          length, &sys_err);
    propagate_key_file_error (sys_err, error);
  }
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a boolean list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated array of booleans.  If an error occurs, NULL is
 * returned and \a error is set.
 *
 * \see eda_config_set_boolean_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length.
 * \param error  Return location for error information.
 * \return configuration value as an array of booleans.
 */
bool *
eda_config_get_boolean_list (EdaConfig *cfg, const char *group,
                             const char *key, size_t *length, GError **error)
{
  GError *sys_err;
  bool   *result;

  cfg = eda_config_get_source (cfg, group, key, error);

  if (cfg == NULL) {
    result = NULL;
  }
  else {

    sys_err = NULL;
    result =   g_key_file_get_boolean_list (cfg->priv->keyfile, group, key,
                                            length, &sys_err);
    propagate_key_file_error (sys_err, error);
  }
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as an integer list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a newly-allocated
 * array of integers or  NULL if an error occurs, in which case \a error is
 * set. The returned list of integers should be freed with g_free() when no
 * longer needed.
 *
 * \see eda_config_set_int_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length.
 * \param error  Return location for error information.
 *
 * \return configuration value as an array of integers.
 */
int *
eda_config_get_int_list (EdaConfig  *cfg, const char *group,
                         const char *key, size_t *length, GError **error)
{
  GError *sys_err;
  int    *result;

  cfg = eda_config_get_source (cfg, group, key, error);

  if (cfg == NULL) {
    return NULL;
  }
  else {

    sys_err = NULL;
    result = g_key_file_get_integer_list (cfg->priv->keyfile, group, key,
                                          length, &sys_err);
    propagate_key_file_error (sys_err, error);
  }
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Get the value of a configuration parameter as a double list.
 *
 * Get the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg, as a
 * newly-allocated array of doubles.  If an error occurs, NULL is
 * returned and \a error is set.
 *
 * \see eda_config_set_double_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param length Return location for array length.
 * \param error  Return location for error information.
 * \return configuration value as an array of doubles.
 */
double *
eda_config_get_double_list (EdaConfig *cfg, const char *group,
                            const char *key, size_t *length, GError **error)
{
  cfg = eda_config_get_source (cfg, group, key, error);
  if (cfg == NULL) return NULL;

  GError *sys_err = NULL;
  double *result =
    g_key_file_get_double_list (cfg->priv->keyfile, group, key,
                                length, &sys_err);
  propagate_key_file_error (sys_err, error);
  return result;
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a string.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a string.
 *
 * \see eda_config_get_string().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_string (EdaConfig *cfg, const char *group,
                       const char *key, const char *value)
{
  g_key_file_set_string (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a boolean.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a boolean.
 *
 * \see eda_config_get_boolean().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_boolean (EdaConfig *cfg, const char *group,
                        const char *key, bool value)
{
  g_key_file_set_boolean (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from an integer.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from an integer.
 *
 * \see eda_config_get_integer().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_integer (EdaConfig *cfg, const char *group,
                    const char *key, int value)
{
  g_key_file_set_integer (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a double.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a double.
 *
 * \see eda_config_get_double().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param value  New value for parameter.
 */
void
eda_config_set_double (EdaConfig *cfg, const char *group,
                       const char *key, double value)
{
  g_key_file_set_double (cfg->priv->keyfile, group, key, value);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a string list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * strings.
 *
 * \see eda_config_get_string_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_string_list (EdaConfig *cfg, const char *group,
                            const char *key, const char * const list[],
                            int length)
{
  g_key_file_set_string_list (cfg->priv->keyfile, group, key,
                              list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a boolean list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * booleans.
 *
 * \see eda_config_get_boolean_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_boolean_list (EdaConfig *cfg, const char *group,
                             const char *key, bool list[], int length)
{
  g_key_file_set_boolean_list (cfg->priv->keyfile, group, key,
                               list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from an integer list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * integers.
 *
 * \see eda_config_get_int_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_int_list (EdaConfig *cfg, const char *group,
                         const char *key, int list[], int length)
{
  g_key_file_set_integer_list (cfg->priv->keyfile, group, key,
                               list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}


/*! \public \memberof EdaConfig
 * \brief Set the value of a configuration parameter from a double list.
 *
 * Set the value of the configuration parameter specified by \a group
 * and \a key in the configuration context \a cfg from a list of
 * doubles.
 *
 * \see eda_config_get_double_list().
 *
 * \param cfg    Configuration context.
 * \param group  Configuration group name.
 * \param key    Configuration key name.
 * \param list   List of new values for parameter.
 * \param length Number of values in \a list.
 */
void
eda_config_set_double_list (EdaConfig *cfg, const char *group,
                            const char *key, double list[], int length)
{
  g_key_file_set_double_list (cfg->priv->keyfile, group, key,
                              list, length);
  g_signal_emit_by_name (cfg, "config-changed", group, key);
}

/*! \brief Callback marshal function for config-changed signals.
 * \par Function Description
 * Based heavily on g_cclosure_marshal_VOID__STRING() from GObject.
 */
static
void cclosure_marshal_VOID__STRING_STRING (GClosure *closure,
                                           GValue *return_value,
                                           unsigned int n_param_values,
                                           const GValue *param_values,
                                           void *invocation_hint,
                                           void *marshal_data)
{
  typedef void (*MarshalFunc_VOID__STRING_STRING) (void *data1,
                                                   void *arg_1,
                                                   void *arg_2,
                                                   void *data2);
  register MarshalFunc_VOID__STRING_STRING callback;
  register GCClosure *cc = (GCClosure *) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);
  if (G_CCLOSURE_SWAP_DATA (closure)) {
    data1 = closure->data;
    data2 = g_value_peek_pointer (param_values + 0);
  } else {
    data1 = g_value_peek_pointer (param_values + 0);
    data2 = closure->data;
  }
  callback = (MarshalFunc_VOID__STRING_STRING) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            (void *) g_value_get_string (param_values + 1),
            (void *) g_value_get_string (param_values + 2),
            data2);
}

/*! \brief Default handler for config change signals.
 *
 * Sets the changed flag for \a cfg.
 *
 * \param cfg     Configuration context.
 * \param group   Configuration group name.
 * \param key     Configuration key name.
 * \param cfg     Child configuration context.
 */
static
void default_config_changed_handler (EdaConfig  *cfg, const char *group,
                                     const char *key)
{
  cfg->priv->changed = TRUE;
}

/*! \brief Emit config change signals for inherited configuration.
 *
 * Signal handler used by configuration contexts with parent contexts
 * to emit signals. When the value of a configuration parameter that
 * is inherited from the parent context is changed in the parent
 * context, re-emits the signal.
 *
 * \param parent  Parent configuration context.
 * \param group   Configuration group name.
 * \param key     Configuration key name.
 * \param cfg     Child configuration context.
 */
static
void parent_config_changed_handler (EdaConfig *cfg, const char *group,
                                    const char* key, EdaConfig *parent)
{
  if (eda_config_is_inherited (cfg, group, key, NULL)) {
    g_signal_emit_by_name (cfg, "config-changed", group, key);
  }
}

/*! \brief Get #EdaConfig error domain.
 * \par Function Description
 * Return the domain for errors relating to configuration contexts.
 *
 * \warning You should not call this function directly; use
 * EDA_CONFIG_ERROR instead.
 *
 * \return a GQuark representing the error domain.
 */
GQuark
eda_config_error_quark (void)
{
  return g_quark_from_static_string ("eda-config-error-quark");
}
/** @} endgroup geda-config */