/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_config.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
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

#ifndef __EDA_CONFIG_H__
#define __EDA_CONFIG_H__

/* \note
 * W.E.Hill Feb 24, 2014 relocated defines for configuration from
 * geda_config.c because it does not make sense for one library in
 * the suite to horde this information. Applications are allowed to
 * manage configuration data (but would be wise to NOT write to these
 * files). If Appilcations are to write configuration data it should
 * be to other files within this same sub-directory.
 */

/* These are used for testing without re-configuring */
//#define GEDA_USE_XDG 1
//#define GEDA_USE_HOME_ETC 1
//#define GEDA_USE_DOT_GEDA 1

/*! Default value for Non XDG system configuration found. */
#define DEFAULT_CONFIG_DIR        "/etc"

#define STD_USER_CONFIG_DIRS      ".config"

/*! Default value for XDG_CONFIG_DIRS if no system configuration found. */
#define XDG_CONFIG_DIRS_DEFAULT   "/etc/xdg/"

/*! Subdirectory of XDG directories (config, data, cache etc.) to check
 * for gEDA files. */
#define XDG_SUBDIR                GEDA_CONFIG_DIR

#if defined (GEDA_USE_HOME_ETC)
 #define USER_CONFIG_DIRS          DEFAULT_CONFIG_DIR
#else
 #define USER_CONFIG_DIRS          STD_USER_CONFIG_DIRS
#endif

#define GEDA_CONFIG_DIR           "gEDA"

#define GEDA_CONFIG_DOT           ".gEDA"

#define DEFAULT_CONTEXT           "geda"

/*! Filename for gEDA system configuration files */
#define SYSTEM_CONFIG_FILE        "geda-system.conf"

/*! Filename for gEDA user configuration files */
#define USER_CONFIG_FILE          "geda-user.conf"

/*! Filename for gEDA local configuration files */
#define LOCAL_CONFIG_FILE         "geda.conf"
#define LOCAL_CONFIG_FILE_ALT     "gafrc"

#define GEDA_CONFIG_SYS_SUFFIX    "-system.conf"
#define GEDA_CONFIG_USER_SUFFIX   "-user.conf"

/*! USER_CONFIG_DIR (with comma's) are pass to geda_strconcat */
#if defined (GEDA_USE_XDG)
  #define GEDA_CONFIG_SYSDIR      XDG_CONFIG_DIRS_DEFAULT
  #define USER_CONFIG_DIR         STD_USER_CONFIG_DIRS, XDG_SUBDIR
#elif defined (GEDA_USE_DOT_GEDA)
  #define GEDA_CONFIG_SYSDIR      DEFAULT_CONFIG_DIR, GEDA_CONFIG_DIR
  #define USER_CONFIG_DIR         GEDA_CONFIG_DOT
#else
  #define GEDA_CONFIG_SYSDIR      DEFAULT_CONFIG_DIR, GEDA_CONFIG_DIR
  #define USER_CONFIG_DIR         USER_CONFIG_DIRS, GEDA_CONFIG_DIR
#endif

/* ---------------------------------------------------------------- */

/*! \class EdaConfig geda_config.h "libgeda/geda_config.h"
 * \brief Hierarchical configuration context for gEDA configuration data.
 *
 * Configuration parameters in gEDA are evaluated within a configuration
 * context. Each context is associated with a configuration file (although
 * the file does not necessarily need to exist).
 *
 * Each configuration context may have a parent context. When retrieving
 * parameter values, the value of the parameter in the parent context will
 * be returned if the value was not set in the selected context.
 *
 * Configuration contexts are represented by instances of the #EdaConfig
 * class. Since this is a a subclass of GObject, the instances are reference
 * counted.  You can increment and decrement their reference counters using
 * g_object_ref() and g_object_unref().
 *
 * Normally, a configuration context should not be created directly; a
 * context can be obtained that is associated with a path using the function
 * eda_config_get_context_for_path().
 */

/*! Domain for errors relating to EdaConfig operations. */
#define EDA_CONFIG_ERROR eda_config_error_quark ()

/*! Error numbers for errors relating to EdaConfig operations. */
typedef enum {
  EDA_CONFIG_ERROR_UNKNOWN_ENCODING,
  EDA_CONFIG_ERROR_PARSE,
  EDA_CONFIG_ERROR_KEY_NOT_FOUND,
  EDA_CONFIG_ERROR_GROUP_NOT_FOUND,
  EDA_CONFIG_ERROR_INVALID_VALUE,
} EdaConfigError;

GQuark eda_config_error_quark (void);

/* ---------------------------------------------------------------- */
#if defined(__LP64__) || defined(_LP64)
# define GedaConfigType unsigned long
#else
# define GedaConfigType unsigned int
#endif

#define EDA_TYPE_CONFIG            (eda_config_get_type ())
#define EDA_CONFIG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_CONFIG, EdaConfig))
#define EDA_CONFIG_CLASS(class)    (G_TYPE_CHECK_CLASS_CAST ((class), EDA_TYPE_CONFIG, EdaConfigClass))
#define EDA_IS_CONFIG(obj)         (is_a_eda_config((EdaConfig*)(obj)))
#define EDA_IS_CONFIG_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class), EDA_TYPE_CONFIG))
#define EDA_CONFIG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_CONFIG, EdaConfigClass))

typedef struct _EdaConfig      EdaConfig;
typedef struct _EdaConfigClass EdaConfigClass;
typedef struct _EdaConfigData  EdaConfigData;

struct _EdaConfigClass
{
  GObjectClass parent_class;

  /* signals */
  void (*config_changed)(EdaConfig *cfg, const char *group, const char *key);

};

struct _EdaConfig
{
  GObject parent_instance;

  /* Private members */
  EdaConfigData *priv;

  GList *RC_list;            /* List of RC files which have been read in. */
};

#ifdef __cplusplus
extern "C" {
#endif

GedaConfigType eda_config_get_type (void) GEDA_CONST;
bool           is_a_eda_config     (const EdaConfig *cfg);

/* ---------------------------------------------------------------- */

char       *eda_config_find_project_root     (const char *path, const char *filename) GEDA_WARN_UNUSED_RESULT;

EdaConfig  *eda_config_get_context_for_file  (const char *file) GEDA_WARN_UNUSED_RESULT;
EdaConfig  *eda_config_get_context_for_path  (const char *path) GEDA_WARN_UNUSED_RESULT;

EdaConfig  *eda_config_get_default_context   (void);
EdaConfig  *eda_config_get_system_context    (const char *context);
EdaConfig  *eda_config_get_user_context      (void);

const char *eda_config_get_filename          (EdaConfig *cfg);
bool        eda_config_load                  (EdaConfig *cfg, GError **err);
bool        eda_config_is_loaded             (EdaConfig *cfg);

bool        eda_config_save                  (EdaConfig *cfg, GError **error);
bool        eda_config_is_changed            (EdaConfig *cfg);

EdaConfig  *eda_config_get_parent            (EdaConfig *cfg);
void        eda_config_set_parent            (EdaConfig *cfg, EdaConfig *parent);

bool        eda_config_is_trusted            (EdaConfig *cfg);
void        eda_config_set_trusted           (EdaConfig *cfg, bool trusted);

EdaConfig  *eda_config_get_trusted_context   (EdaConfig *cfg);

char      **eda_config_get_groups            (EdaConfig *cfg, unsigned   *length) GEDA_WARN_UNUSED_RESULT;
bool        eda_config_has_group             (EdaConfig *cfg, const char *group);
char      **eda_config_get_keys              (EdaConfig *cfg, const char *group, unsigned   *length, GError **error) GEDA_WARN_UNUSED_RESULT;
bool        eda_config_has_key               (EdaConfig *cfg, const char *group, const char *key, GError **err);
bool        eda_config_is_inherited          (EdaConfig *cfg, const char *group, const char *key, GError **err);

EdaConfig  *eda_config_get_source            (EdaConfig *cfg, const char *group, const char *key, GError **err);
void        eda_config_release_resources     (void);

/* ---------------------------------------------------------------- */

char    *eda_config_get_string       (EdaConfig *cfg, const char *group, const char *key, GError **err) GEDA_WARN_UNUSED_RESULT;
bool     eda_config_get_boolean      (EdaConfig *cfg, const char *group, const char *key, GError **err) GEDA_WARN_UNUSED_RESULT;
int      eda_config_get_integer      (EdaConfig *cfg, const char *group, const char *key, GError **err) GEDA_WARN_UNUSED_RESULT;
double   eda_config_get_double       (EdaConfig *cfg, const char *group, const char *key, GError **err) GEDA_WARN_UNUSED_RESULT;
char   **eda_config_get_string_list  (EdaConfig *cfg, const char *group, const char *key, unsigned int *length, GError **err) GEDA_WARN_UNUSED_RESULT;
bool    *eda_config_get_boolean_list (EdaConfig *cfg, const char *group, const char *key, unsigned int *length, GError **err) GEDA_WARN_UNUSED_RESULT;
int     *eda_config_get_int_list     (EdaConfig *cfg, const char *group, const char *key, unsigned int *length, GError **err) GEDA_WARN_UNUSED_RESULT;
double  *eda_config_get_double_list  (EdaConfig *cfg, const char *group, const char *key, unsigned int *length, GError **err) GEDA_WARN_UNUSED_RESULT;


void eda_config_set_string           (EdaConfig *cfg, const char *group, const char *key, const char *value);
void eda_config_set_boolean          (EdaConfig *cfg, const char *group, const char *key, bool value);
void eda_config_set_integer          (EdaConfig *cfg, const char *group, const char *key, int value);
void eda_config_set_double           (EdaConfig *cfg, const char *group, const char *key, double value);
void eda_config_set_string_list      (EdaConfig *cfg, const char *group, const char *key, const char  *const list[], int length);
void eda_config_set_boolean_list     (EdaConfig *cfg, const char *group, const char *key, bool list[],   int length);
void eda_config_set_int_list         (EdaConfig *cfg, const char *group, const char *key, int list[],    int length);
void eda_config_set_double_list      (EdaConfig *cfg, const char *group, const char *key, double list[], int length);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__EDA_CONFIG_H__ */