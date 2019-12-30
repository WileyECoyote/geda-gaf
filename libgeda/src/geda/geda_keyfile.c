/* -*- geda_keyfile.c -*-
 *
 * Copyright (C) 2016 Wiley Edward Hill
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 * Date: October, 31, 2012
 * Contributing Author: Wiley Edward Hill
 * Based heavily glib-2.32.4/glib/gkeyfile.c.
 */

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>

#include <errno.h>
#include <fcntl.h>
#include <locale.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if 0
#include <io.h>

#undef fstat
#define fstat(a,b) _fstati64(a,b)
#undef stat
#define stat _stati64

#ifndef S_ISREG
#define S_ISREG(mode) ((mode)&_S_IFREG)
#endif

#endif  /* G_OS_WIN23 */

/*! Size of input buffer when reading files */
#define KF_BUFFER_SIZE 128

/** \defgroup geda-keyfile GedaKeyFile
 * @{
 * \brief Implmentation module for #GedaKeyFile
 * \par
 * parses .ini-like config files
 *
 * #GedaKeyFile allows parsing, modifying or create files containing groups
 * of key-value pairs. Several freedesktop.org specifications use key files
 * now, e.g the
 * <a href="http://freedesktop.org/Standards/desktop-entry-spec">Desktop Entry Specification</a>
 * and the
 * <a href="http://freedesktop.org/Standards/icon-theme-spec">Theme Specification</a>
 *
 * The syntax of key files is described in detail in the
 * <a href="http://freedesktop.org/Standards/desktop-entry-spec">Desktop Entry Specification</a>,
 * here is a quick summary: Key files
 * consists of groups of key-value pairs, interspersed with comments.
 *
 * \code
 * # this is just an example
 * # there can be comments before the first group
 *
 * [First Group]
 *
 * Name=Key File Example
 *
 * # localized strings are stored in multiple key-value pairs
 * Welcome=Hello
 * Welcome[de]=Hallo
 * Welcome[fr_FR]=Bonjour
 * Welcome[it]=Ciao
 * Welcome[be@latin]=Hello
 *
 * [Another Group]
 *
 * Numbers=2;20;-200;0
 *
 * Booleans=true;false;true;true
 * \endcode
 *
 * Lines beginning with a '#' and blank lines are considered comments.
 * Comments before the first group are referred to as "top level" comments.
 * Top-level comments should not contain blank lines. If blanks lines are
 * presents, only the line or lines before the break will be returned.
 * This because a blank line is the only way to distinguish top level
 * comments from group 1 comment and vice versa.
 *
 * Groups are started with a header line containing the group name enclosed
 * in '[' and ']', and ended implicitly by the start of the next group or
 * the end of the file. Each key-value pair must be contained in a group.
 *
 * Key-value pairs generally have the form <b>key=value</b>, with the exception
 * of localized strings, which have the form <b>key[locale]=value</b>, with a
 * locale identifier of the form <b>lang_COUNTRY\@MODIFIER</b> where <b>COUNTRY</b>
 * and <b>MODIFIER</b> are optional. Spaces before and after the '=' character
 * are ignored. Newline, tab, carriage return and backslash characters in value
 * are escaped as \code \n, \t, \r, and \\, \endcode respectively. To preserve
 * leading spaces in values, these can also be escaped as \code \s \endcode.
 *
 * Key files can store strings, integers, booleans and lists of these.
 * Lists are separated by a separator character, typically ';' or ','.
 * To use the list separator character in a value in a list, it has to
 * be escaped by prefixing it with a backslash.
 *
 * This syntax is obviously inspired by the .ini files found on older Windows
 * systems, but there are some important differences:
 * <DL>
 *   <DT>.ini files use the ';' character to begin comments,
 *     key files use the '#' character.</DT>
 *   <DT>Key files do not allow for ungrouped keys meaning only
 *     comments can precede the first group.</DT>
 *   <DT>Key files are always encoded in UTF-8.</DT>
 *   <DT>Key and Group names are case-sensitive. For example, a
 *     group called <b>[GROUP]</b> is a different from
 *     <b>[group]</b>.</DT>
 *   <DT>.ini files don't have a strongly typed boolean entry type,
 *     they only have GetProfileInt(). In key files, only
 *     <b>true</b> and <b>false</b> (in lower case)
 *     are allowed.</DT>
 *  </DL>
 *
 * Note that in contrast to the
 * <a href="http://freedesktop.org/Standards/desktop-entry-spec">Desktop Entry Specification</a>,
 * groups in key files may contain the same key multiple times; the last
 * entry wins. Key files may also contain multiple groups with the same
 * name; they are merged together. Another difference is that keys and
 * group names in key files are not restricted to ASCII characters.
 */

typedef struct _GedaKeyFileGroup GedaKeyFileGroup;

/*!
 * GedaKeyFile:
 *
 * The GedaKeyFile struct contains private data and should not be accessed
 * directly.
 */
struct _GedaKeyFile
{
  GList      *groups;
  GHashTable *group_hash;

  GedaKeyFileGroup *start_group;
  GedaKeyFileGroup *current_group;

  char *parse_buffer; /* Holds up to one line of not-yet-parsed data */
  size_t buffer_size;

  char list_separator;

  GedaKeyFileFlags flags;

  char **locales;

  volatile int ref_count;
};

typedef struct _GedaKeyFilePair GedaKeyFilePair;

struct _GedaKeyFileGroup
{
  const char *name;  /* NULL for above first group (which will be comments) */

  GedaKeyFilePair *comment; /* Special comment that is stuck to the top of a group */

  GList *key_value_pairs;

  /* Used in parallel with key_value_pairs for
   * increased lookup performance
   */
  GHashTable *lookup_map;
};

struct _GedaKeyFilePair
{
  char *key;  /* NULL for comments */
  char *value;
};

static int    find_file_in_data_dirs                  (const char            *file,
                                                       const char           **data_dirs,
                                                       char                 **output_file,
                                                       GError               **error);

//static bool   geda_keyfile_load_from_fd               (GedaKeyFile           *key_file,
//                                                       int                    fd,
//                                                       GedaKeyFileFlags       flags,
//                                                       GError               **error);

static GList *geda_keyfile_lookup_group_node          (GedaKeyFile           *key_file,
                                                       const char            *group_name);
static GedaKeyFileGroup
             *geda_keyfile_lookup_group               (GedaKeyFile           *key_file,
                                                       const char            *group_name);

static GList *geda_keyfile_lookup_key_value_pair_node (GedaKeyFile           *key_file,
                                                       GedaKeyFileGroup      *group,
                                                       const char            *key);
static GedaKeyFilePair
             *geda_keyfile_lookup_key_value_pair      (GedaKeyFile           *key_file,
                                                       GedaKeyFileGroup      *group,
                                                       const char            *key);

static void   geda_keyfile_remove_group_node          (GedaKeyFile           *key_file,
                                                       GList                 *group_node);
static void   geda_keyfile_remove_key_value_pair_node (GedaKeyFile           *key_file,
                                                       GedaKeyFileGroup      *group,
                                                       GList                 *pair_node);

static void   geda_keyfile_add_key_value_pair         (GedaKeyFile           *key_file,
                                                       GedaKeyFileGroup      *group,
                                                       GedaKeyFilePair       *pair);
static void   geda_keyfile_add_key                    (GedaKeyFile           *key_file,
                                                       GedaKeyFileGroup      *group,
                                                       const char            *key,
                                                       const char            *value);
static void   geda_keyfile_add_group                  (GedaKeyFile           *key_file,
                                                       const char            *group_name);
static bool   geda_keyfile_is_group_name              (const char            *name);
static bool   geda_keyfile_is_key_name                (const char            *name);
static void   geda_keyfile_pair_free                  (GedaKeyFilePair       *pair);
static bool   geda_keyfile_line_is_comment            (const char            *line);
static bool   geda_keyfile_line_is_group              (const char            *line);
static bool   geda_keyfile_line_is_key_value_pair     (const char            *line);
static char  *geda_keyfile_parse_value_as_string      (GedaKeyFile           *key_file,
                                                       const char            *value,
                                                       GSList               **separators,
                                                       GError               **error);
static char  *geda_keyfile_parse_string_as_value      (GedaKeyFile           *key_file,
                                                       const char            *string,
                                                       bool                   escape_separator);
static int    geda_keyfile_parse_value_as_integer     (GedaKeyFile           *key_file,
                                                       const char            *value,
                                                       GError               **error);
static char  *geda_keyfile_parse_integer_as_value     (GedaKeyFile           *key_file,
                                                       int                    value);
static double geda_keyfile_parse_value_as_double      (GedaKeyFile           *key_file,
                                                       const char            *value,
                                                       GError               **error);
static bool   geda_keyfile_parse_value_as_boolean     (GedaKeyFile           *key_file,
                                                       const char            *value,
                                                       GError               **error);
static char  *geda_keyfile_parse_boolean_as_value     (GedaKeyFile           *key_file,
                                                       bool                   value);
static char  *geda_keyfile_parse_value_as_comment     (GedaKeyFile           *key_file,
                                                       const char            *value);
static char  *geda_keyfile_parse_comment_as_value     (GedaKeyFile           *key_file,
                                                       const char            *comment);
static void   geda_keyfile_parse_key_value_pair       (GedaKeyFile           *key_file,
                                                       const char            *line,
                                                       unsigned int            length,
                                                       GError               **error);
static void   geda_keyfile_parse_comment              (GedaKeyFile           *key_file,
                                                       const char            *line,
                                                       unsigned int           length,
                                                       GError               **error);
static void   geda_keyfile_parse_group                (GedaKeyFile           *key_file,
                                                       const char            *line,
                                                       unsigned int           length,
                                                       GError               **error);
//static char  *key_get_locale                          (const char            *key);
static void   geda_keyfile_parse_data                 (GedaKeyFile           *key_file,
                                                       const char            *data,
                                                       unsigned int           length,
                                                       GError               **error);
static void   geda_keyfile_flush_parse_buffer         (GedaKeyFile           *key_file,
                                                       GError               **error);

/* List of pointers to GedaKeyFile instances */
static GList *list_of_keyfiles = NULL;

GQuark
geda_keyfile_error (void)
{
  return g_quark_from_static_string ("geda-keyfile-error-quark");
}

static void
geda_keyfile_not_valid (const char *func, GedaKeyFile *key_file)
{
  fprintf(stderr, "libgeda <%s>: key_file ", func);

  if (!key_file) {
    fprintf(stderr, _("argument is NULL\n"));
  }
  else {
    const char *msg = _("is not valid\n");
    fprintf(stderr, "%s <%p>\n", msg, key_file);
  }
}

static void geda_keyfile_init (GedaKeyFile *key_file)
{
  key_file->current_group  = GEDA_MEM_ALLOC0 (sizeof(GedaKeyFileGroup));
  key_file->groups         = g_list_prepend (NULL, key_file->current_group);
  key_file->group_hash     = g_hash_table_new (g_str_hash, g_str_equal);
  key_file->start_group    = NULL;
  key_file->parse_buffer   = GEDA_MEM_ALLOC0 (KF_BUFFER_SIZE);
  key_file->buffer_size    = KF_BUFFER_SIZE;
  key_file->list_separator = ';';
  key_file->flags          = 0;
  key_file->locales        = g_strdupv ((char**)g_get_language_names ());
}

static void geda_keyfile_clear (GedaKeyFile *key_file)
{
  GList *tmp;

  if (key_file->locales) {
    g_strfreev (key_file->locales);
    key_file->locales = NULL;
  }

  GEDA_FREE (key_file->parse_buffer);
  key_file->buffer_size= 0;

  tmp = key_file->groups;

  while (tmp != NULL) {

    GList *group_node;

    group_node = tmp;
    tmp        = tmp->next;
    geda_keyfile_remove_group_node (key_file, group_node);
  }

  g_list_free(key_file->groups);
  key_file->groups = NULL;

  if (key_file->group_hash != NULL) {
    g_hash_table_destroy (key_file->group_hash);
    key_file->group_hash = NULL;
  }
}

/*!
 * \brief Determine if object is Geda KeyFile Object.
 * \par Function Description
 *  Returns true if the argument is a GedaKeyFile object. This
 *  function checks if the pointer is in the list of allocated
 *  GedaKeyFile objects and returns true if the address is in
 *  the list, otherwise FALSE is returned.
 *
 * \param [in] keyfile  Pointer to GedaKeyFile Object
 *
 * \return boolean.
 */
bool is_a_geda_keyfile (GedaKeyFile *keyfile)
{
  if (keyfile) {
    return g_list_find(list_of_keyfiles, keyfile) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Create a New Key File Object
 * \par Function Description
 *  Creates a new empty #GedaKeyFile object. To read an existing
 *  key file use one of:
 *
 *    geda_keyfile_load_from_file,
 *    geda_keyfile_load_from_data,
 *    geda_keyfile_load_from_dirs,
 *    geda_keyfile_load_from_data_dirs.
 *
 * \returns an empty #GedaKeyFile.
 */
GedaKeyFile *geda_keyfile_new (void)
{
  GedaKeyFile *key_file;

  key_file = GEDA_MEM_ALLOC0 (sizeof(GedaKeyFile));
  key_file->ref_count = 1;
  geda_keyfile_init (key_file);

  /* Append key_file to list of valid key_file objects */
  list_of_keyfiles = g_list_append(list_of_keyfiles, key_file);

  return key_file;
}

/*!
 * \brief Create a New Key File Object
 * \par Function Description
 *  Sets the character which is used to separate
 *  values in lists. Typically ';' or ',' are used
 *  as separators. The default list separator is ';'.
 *
 * \param [in] key_file  a #GedaKeyFile object
 * \param [in] separator the separator
 */
void geda_keyfile_set_list_separator (GedaKeyFile *key_file, char separator)
{
  g_return_if_fail (key_file != NULL);

  key_file->list_separator = separator;
}

/*! \internal
 * Iterates through all the directories in *dirs trying to open
 * file. The function returns a file descriptor to the open file
 * upon successfully locating and opening the file and outputs
 * the absolute path of the file in output_file. If the file is
 * not found or \a dirs is NULL, the function returns -1.
 */
static int find_file_in_data_dirs (const char   *file,
                                   const char  **dirs,
                                   char        **output_file,
                                   GError      **error)
{
  const char **data_dirs;
  const char  *data_dir;
  char        *path;
  int          fd;

  path = NULL;
  fd   = -1;

  if (dirs == NULL) {
   return fd;
  }

  data_dirs = dirs;

  while (data_dirs && (data_dir = *data_dirs) && fd == -1) {

    char *candidate_file;
    char *sub_dir;

    candidate_file = (char*)file;
    sub_dir        = geda_strdup ("");

    while (candidate_file != NULL && fd == -1) {

      char *p;

      path = g_build_filename (data_dir, sub_dir, candidate_file, NULL);

      fd = open (path, O_RDONLY, 0);

      if (fd == -1) {
        GEDA_FREE (path);
        path = NULL;
      }

      candidate_file = strchr (candidate_file, '-');

      if (candidate_file == NULL)
        break;

      candidate_file++;

      GEDA_FREE (sub_dir);
      sub_dir = geda_strndup (file, candidate_file - file - 1);

      for (p = sub_dir; *p != '\0'; p++)
      {
        if (*p == '-')
          *p = G_DIR_SEPARATOR;
      }
    }
    GEDA_FREE (sub_dir);
    data_dirs++;
  }

  if (fd == -1) {

    g_set_error_literal (error, GEDA_KEYFILE_ERROR,
                         GEDA_KEYFILE_ERROR_NOT_FOUND,
                         _("Valid key file could not be "
                           "found in search dirs"));
  }
  else if (output_file != NULL) {
    *output_file = geda_strdup (path);
  }

  GEDA_FREE (path);

  return fd;
}

/*!
 * \brief Load Key File from File
 * \par Function Description
 *  Loads a key file into an empty #GedaKeyFile structure.
 *  If the file could not be loaded then \a error is set to
 *  either a GFileError or #GedaKeyFileError.
 *
 * \param [in]  key_file  an empty #GedaKeyFile struct
 * \param [in]  file      the path of a filename to load, in the GLib filename encoding
 * \param [in]  flags     flags from #GedaKeyFileFlags
 * \param [out] error     Location for a GError, or %NULL
 *
 * \returns %TRUE if a key file could be loaded, %FALSE otherwise
 */
bool geda_keyfile_load_from_file (GedaKeyFile       *key_file,
                                  const char        *file,
                                  GedaKeyFileFlags   flags,
                                  GError           **error)
{
  bool result;

  g_return_val_if_fail (file != NULL, FALSE);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
    result = FALSE;
  }
  else {

    GError *key_file_error = NULL;
    char   *buffer;
    size_t length;

    if (geda_file_get_contents(file, &buffer, &length, &key_file_error)) {

      result = geda_keyfile_load_from_data (key_file,
                                            buffer, length,
                                            flags, error);
      GEDA_FREE(buffer);
    }
    else {
      if (error) {
        g_propagate_error (error, key_file_error);
      }
      result = FALSE;
    }
  }
  return result;
}

/*!
 * \brief Load Key File from data buffer
 * \par Function Description
 *  Loads a key file from memory into an empty #GedaKeyFile structure.
 *  If the object cannot be created then %error is set to a #GedaKeyFileError.
 *
 * \param [in]  key_file  an empty #GedaKeyFile struct
 * \param [in]  data      key file loaded in memory
 * \param [in]  length    the length of \a data in bytes (or -1 if data is nul-terminated)
 * \param [in]  flags     flags from #GedaKeyFileFlags
 * \param [out] error     Location for a GError, or %NULL
 *
 * \returns %TRUE if a key file could be loaded, %FALSE otherwise
 */
bool geda_keyfile_load_from_data (GedaKeyFile       *key_file,
                                  const char        *data,
                                  unsigned int       length,
                                  GedaKeyFileFlags   flags,
                                  GError           **error)
{
  GError *key_file_error = NULL;
  char list_separator;

  g_return_val_if_fail (key_file != NULL, FALSE);
  g_return_val_if_fail (data != NULL || length == 0, FALSE);

  if (length == -1) {
    length = strlen (data);
  }

  list_separator = key_file->list_separator;
  geda_keyfile_clear (key_file);
  geda_keyfile_init (key_file);
  key_file->list_separator = list_separator;
  key_file->flags = flags;

  geda_keyfile_parse_data (key_file, data, length, &key_file_error);

  if (key_file_error) {
    g_propagate_error (error, key_file_error);
    return FALSE;
  }

  geda_keyfile_flush_parse_buffer (key_file, &key_file_error);

  if (key_file_error) {
    g_propagate_error (error, key_file_error);
    return FALSE;
  }

  return TRUE;
}

/*!
 * \brief Load Key File from dir
 * \par Function Description
 * This function looks for a key file named \a file in the paths
 * specified in \a search_dirs, loads the file into \a key_file and
 * returns the file's full path in \a full_path. If the file could
 * not be loaded then an %error is set to either a GFileError or
 * #GedaKeyFileError.
 *
 * \param [out] key_file    an empty #GedaKeyFile struct
 * \param [in]  file        a relative path to a filename to open and parse
 * \param [in]  search_dirs %NULL-terminated array of directories to search
 * \param [out] full_path   return location for a string containing the full path of the file, or %NULL
 * \param [in]  flags       flags from #GedaKeyFileFlags
 * \param [out] error       Location for a GError, or %NULL
 *
 * \returns %TRUE if a key file could be loaded, %FALSE otherwise
 */
bool geda_keyfile_load_from_dirs (GedaKeyFile       *key_file,
                                  const char        *file,
                                  const char       **search_dirs,
                                  char             **full_path,
                                  GedaKeyFileFlags   flags,
                                  GError           **error)
{
  GError      *key_file_error = NULL;
  const char **data_dirs;
  char        *output_path;

  bool found_file;

  g_return_val_if_fail (key_file != NULL, FALSE);
  g_return_val_if_fail (!g_path_is_absolute (file), FALSE);
  g_return_val_if_fail (search_dirs != NULL, FALSE);

  found_file  = FALSE;
  data_dirs   = search_dirs;
  output_path = NULL;

  while (*data_dirs != NULL && !found_file) {

    int  fd;

    GEDA_FREE (output_path);

    fd = find_file_in_data_dirs (file, data_dirs, &output_path,
                                 &key_file_error);

    if (fd == -1) {
      if (key_file_error) {
        g_propagate_error (error, key_file_error);
      }
      break;
    }

    found_file = geda_keyfile_load_from_file (key_file,
                                              output_path, flags,
                                              &key_file_error);
    close (fd);

    if (key_file_error) {
      g_propagate_error (error, key_file_error);
      break;
    }
  }

  if (found_file && full_path) {
    *full_path = output_path;
  }
  else {
    GEDA_FREE(output_path);
  }

  return found_file;
}

/*!
 * \brief Load Key File from data dir
 * \par Function Description
 * This function looks for a key file named \a file in the paths
 * returned from g_get_user_data_dir() and g_get_system_data_dirs(),
 * loads the file into \a key_file and returns the file's full path in
 * \a full_path.  If the file could not be loaded then an %error is
 * set to either a GFileError or #GedaKeyFileError.
 *
 * \param [in]  key_file   an empty #GedaKeyFile struct
 * \param [in]  file       a relative path to a filename to open and parse
 * \param [out] full_path  return location for a string containing the full path
 *                         of the file, or %NULL
 * \param [in]  flags      flags from #GedaKeyFileFlags
 * \param [out] error      Location for a GError, or %NULL
 *
 * \returns %TRUE if a key file could be loaded, %FALSE othewise
 */
bool geda_keyfile_load_from_data_dirs (GedaKeyFile       *key_file,
                                       const char        *file,
                                       char             **full_path,
                                       GedaKeyFileFlags   flags,
                                       GError           **error)
{
  char **all_data_dirs;
  const char * user_data_dir;
  const char * const * system_data_dirs;
  unsigned int  i, j;
  bool found_file;

  g_return_val_if_fail (key_file != NULL, FALSE);
  g_return_val_if_fail (!g_path_is_absolute (file), FALSE);

  user_data_dir    = g_get_user_data_dir ();
  system_data_dirs = g_get_system_data_dirs ();

  all_data_dirs = GEDA_MEM_ALLOC (g_strv_length ((char**)system_data_dirs) + 2);

  i = 0;
  all_data_dirs[i++] = geda_strdup (user_data_dir);

  j = 0;
  while (system_data_dirs[j] != NULL) {
    all_data_dirs[i++] = geda_strdup (system_data_dirs[j++]);
  }

  all_data_dirs[i] = NULL;

  found_file = geda_keyfile_load_from_dirs (key_file,
                                            file,
                                            (const char **)all_data_dirs,
                                            full_path,
                                            flags,
                                            error);

  g_strfreev (all_data_dirs);

  return found_file;
}

/*!
 * \brief Add Reference to Key File Object
 * \par Function Description
 *  Increases the reference count of \a key_file.
 *
 * \param [in] key_file    a #GedaKeyFile object
 *
 * \returns the same \a key_file.
 */
GedaKeyFile *geda_keyfile_ref (GedaKeyFile *key_file)
{
  g_return_val_if_fail (key_file != NULL, NULL);

  g_atomic_int_inc (&key_file->ref_count);

  return key_file;
}

/*!
 * \brief Release Key File Object
 * \par Function Description
 * Clears all keys and groups from \a key_file, and decreases the
 * reference count by 1. If the reference count reaches zero,
 * frees the key file and all its allocated memory.
 *
 * \param [in] key_file    a #GedaKeyFile object
 */
void geda_keyfile_free (GedaKeyFile *key_file)
{
  g_return_if_fail (key_file != NULL);

  geda_keyfile_clear (key_file);
  geda_keyfile_unref (key_file);
}

/*!
 * \brief Unreference to Key File Object
 * \par Function Description
 * \param [in] key_file  a #GedaKeyFile object
 * Decreases the reference count of \a key_file by 1. If the reference count
 * reaches zero, frees the key file and associated memory.
 */
void geda_keyfile_unref (GedaKeyFile *key_file)
{
  g_return_if_fail (GEDA_IS_KEYFILE(key_file));

  if (g_atomic_int_dec_and_test (&key_file->ref_count)) {

    geda_keyfile_clear (key_file);

    /* Remove from list of valid keyfile objects */
    list_of_keyfiles = g_list_remove(list_of_keyfiles, key_file);

    GEDA_FREE (key_file);

    /* Release list of valid keyfile objects if empty */
    if (!g_list_length(list_of_keyfiles)) {
      g_list_free(list_of_keyfiles);
      list_of_keyfiles = NULL;
    }
  }
}

/*! \internal Returns true if locale is translatable.
 * If GEDA_KEYFILE_KEEP_TRANSLATIONS is not set, only returns
 * true for locales that match those in g_get_language_names().
 */
static bool geda_keyfile_locale_is_interesting (GedaKeyFile *key_file,
                                                const char  *locale)
{
  unsigned int i;

  if (key_file->flags & GEDA_KEYFILE_KEEP_TRANSLATIONS)
    return TRUE;

  for (i = 0; key_file->locales[i] != NULL; i++) {

    if (g_ascii_strcasecmp (key_file->locales[i], locale) == 0) {
      return TRUE;
    }
  }

  return FALSE;
}

static void
geda_keyfile_parse_line (GedaKeyFile *key_file,
                         const char  *line,
                         unsigned int  length,
                         GError      **error)
{
  GError *parse_error = NULL;
  char *line_start;

  g_return_if_fail (key_file != NULL);
  g_return_if_fail (line != NULL);

  line_start = (char *) line;

  while (g_ascii_isspace (*line_start)) {
    line_start++;
  }

  if (geda_keyfile_line_is_comment (line_start)) {

    geda_keyfile_parse_comment (key_file, line, length, &parse_error);
  }
  else if (geda_keyfile_line_is_group (line_start)) {

    geda_keyfile_parse_group (key_file, line_start,
                              length - (line_start - line),
                              &parse_error);
  }
  else if (geda_keyfile_line_is_key_value_pair (line_start)) {

    geda_keyfile_parse_key_value_pair (key_file, line_start,
                                       length - (line_start - line),
                                       &parse_error);
  }
  else {

    char *line_utf8 = geda_get_utf8 (line);

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_PARSE,
                 _("Key file contains line '%s' which is not "
                   "a key-value pair, group, or comment"),
                 line_utf8);
                 GEDA_FREE (line_utf8);

                 return;
  }

  if (parse_error) {
    g_propagate_error (error, parse_error);
  }
}

static void
geda_keyfile_parse_comment (GedaKeyFile *key_file,
                            const char  *line,
                            unsigned int    length,
                            GError      **error)
{
  GedaKeyFilePair *pair;

  if (!(key_file->flags & GEDA_KEYFILE_KEEP_COMMENTS))
    return;

  g_warn_if_fail (key_file->current_group != NULL);

  pair = GEDA_MEM_ALLOC (sizeof(GedaKeyFilePair));
  pair->key = NULL;
  pair->value = geda_strndup (line, length);

  key_file->current_group->key_value_pairs =
  g_list_prepend (key_file->current_group->key_value_pairs, pair);
}

static void
geda_keyfile_parse_group (GedaKeyFile *key_file,
                          const char  *line,
                          unsigned int    length,
                          GError      **error)
{
  char *group_name;
  const char *group_name_start, *group_name_end;

  /* advance past opening '['
   */
  group_name_start = line + 1;
  group_name_end = line + length - 1;

  while (*group_name_end != ']') {
    group_name_end--;
  }

  group_name = geda_strndup (group_name_start,
                             group_name_end - group_name_start);

  if (!geda_keyfile_is_group_name (group_name)) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_PARSE,  "%s '%s'",
                 _("Invalid group name"), group_name);
                 GEDA_FREE (group_name);
                 return;
  }

  geda_keyfile_add_group (key_file, group_name);
  GEDA_FREE (group_name);
}

static char *key_get_locale (const char *key)
{
  char *locale;

  locale = strstr (key, "[");

  if (locale) {

    if (strlen (locale) < 3) {
      locale = NULL;
    }
    else {
      locale = geda_strndup (locale + 1, strlen (locale) - 2);
    }
  }

  return locale;
}

static void
geda_keyfile_parse_key_value_pair (GedaKeyFile *key_file,
                                   const char  *line,
                                   unsigned int length,
                                   GError      **error)
{
  char *key, *value, *key_end, *value_start, *locale;
  unsigned int  key_len, value_len;

  if (key_file->current_group == NULL || key_file->current_group->name == NULL)
  {
    g_set_error_literal (error, GEDA_KEYFILE_ERROR,
                         GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND,
                         _("Key file does not start with a group"));
    return;
  }

  key_end = value_start = strchr (line, '=');

  g_warn_if_fail (key_end != NULL);

  key_end--;
  value_start++;

  /* Pull the key name from the line (chomping trailing whitespace)
   */
  while (g_ascii_isspace (*key_end)) {
    key_end--;
  }

  key_len = key_end - line + 2;

  g_warn_if_fail (key_len <= length);

  key = geda_strndup (line, key_len - 1);

  if (!geda_keyfile_is_key_name (key)) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_PARSE,  "%s '%s'",
                 _("Invalid key name"), key);
    GEDA_FREE (key);
    return;
  }

  /* Pull the value from the line; skip leading whitespace */
  while (g_ascii_isspace (*value_start)) {
    value_start++;
  }

  value_len = line + length - value_start + 1;

  value = geda_strndup (value_start, value_len);

  g_warn_if_fail (key_file->start_group != NULL);

  if (key_file->current_group
    && key_file->current_group->name
    && strcmp (key_file->start_group->name,
               key_file->current_group->name) == 0
               && strcmp (key, "Encoding") == 0)
  {
    if (g_ascii_strcasecmp (value, "UTF-8") != 0) {

      char *value_utf8 = geda_get_utf8 (value);

      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_UNKNOWN_ENCODING,  "%s '%s'",
                   _("Key file contains unsupported encoding"), value_utf8);

      GEDA_FREE (value_utf8);
      GEDA_FREE (key);
      GEDA_FREE (value);

      return;
    }
  }

  /* Is this key a translation? If so, is it one that we care about? */
  locale = key_get_locale (key);

  if (locale == NULL || geda_keyfile_locale_is_interesting (key_file, locale))
  {
    GedaKeyFilePair *pair;

    pair        = GEDA_MEM_ALLOC (sizeof(GedaKeyFilePair));
    pair->key   = key;
    pair->value = value;

    geda_keyfile_add_key_value_pair (key_file, key_file->current_group, pair);
  }
  else
  {
    GEDA_FREE (key);
    GEDA_FREE (value);
  }

  GEDA_FREE (locale);
}

static void geda_keyfile_parse_data (GedaKeyFile  *key_file,
                                     const char   *data,
                                     unsigned int  length,
                                     GError      **error)
{
  unsigned int  i;
  unsigned int  line_length;

  g_return_if_fail (key_file != NULL);
  g_return_if_fail (data != NULL || length == 0);

  line_length = i = 0;

  while (i < length) {

    if (data[i] == '\n') {

      GError *parse_error;
      char   *buffer;

      buffer      = key_file->parse_buffer;
      size_t len  = line_length;
      parse_error = NULL;

      if (len > 0 && (buffer[len - 1] == '\r')) {
        buffer[len - 1] = '\0';
        len--;
      }

      /* When a newline is encountered flush the parse buffer so that the
       * line can be parsed.  Note that completely blank lines won't show
       * up in the parse buffer, so they get parsed directly.
       */
      if (len > 0) {
        geda_keyfile_flush_parse_buffer (key_file, &parse_error);
      }
      else {
        geda_keyfile_parse_comment (key_file, "", 1, &parse_error);
      }

      if (parse_error) {
        g_propagate_error (error, parse_error);
        return;
      }
      i++;
    }
    else {

      const char   *start_of_line;
      const char   *end_of_line;

      start_of_line = data + i;
      end_of_line   = memchr (start_of_line, '\n', length - i);

      if (end_of_line == NULL) {
        end_of_line = data + length;
      }

      line_length = end_of_line - start_of_line;

      if (line_length > key_file->buffer_size) {

        char *buffer = (char*)realloc(key_file->parse_buffer, line_length + 1);

        if (buffer) {
          key_file->parse_buffer = buffer;
          key_file->buffer_size = line_length;
        }
        else {
          line_length = key_file->buffer_size - 1; /* Out of Memory */
        }
      }
      strncpy(key_file->parse_buffer, start_of_line, line_length);
      key_file->parse_buffer[line_length] = '\0';

      i += line_length;
    }
  }
}

static void
geda_keyfile_flush_parse_buffer (GedaKeyFile *key_file, GError **error)
{
  char *buffer = key_file->parse_buffer;

  if (buffer && *buffer) {

    GError *file_error = NULL;
    size_t  len        = strlen(buffer);

    geda_keyfile_parse_line (key_file, buffer, len, &file_error);

    memset(key_file->parse_buffer, 0, key_file->buffer_size);

    if (file_error) {
      g_propagate_error (error, file_error);
      return;
    }
  }
}

/*!
 * \brief Key File to data buffer
 * \par Function Description
 * This function outputs \a key_file as a string.
 *
 * Note that this function never reports an error,
 * so it is safe to pass %NULL as \a error.
 *
 * \param [in] key_file a GedaKeyFile object
 * \param [in] length   return location for the length of the returned string, or %NULL
 * \param [out] error   Location for a GError, or %NULL
 *
 * \returns a newly allocated string holding the contents of the GedaKeyFile
 */
char*
geda_keyfile_to_data (GedaKeyFile *key_file, unsigned int *length, GError **error)
{
  GString *data_string;
  GList   *group_node;
  GList   *key_file_node;

  g_return_val_if_fail (key_file != NULL, NULL);

  data_string = g_string_new (NULL);

  for (group_node = g_list_last (key_file->groups);
       group_node != NULL;
       group_node = group_node->prev) {

    GedaKeyFileGroup *group;

    group = (GedaKeyFileGroup*) group_node->data;

    /* separate groups by at least an empty line */
    if (data_string->len >= 2 &&
        data_string->str[data_string->len - 2] != '\n') {
      g_string_append_c (data_string, '\n');
    }

    if (group->comment != NULL) {
      g_string_append_printf (data_string, "%s\n", group->comment->value);
    }

    if (group->name != NULL) {
      g_string_append_printf (data_string, "[%s]\n", group->name);
    }

    for (key_file_node = g_list_last (group->key_value_pairs);
         key_file_node != NULL;
         key_file_node = key_file_node->prev)  {

      GedaKeyFilePair *pair;

      pair = (GedaKeyFilePair*) key_file_node->data;

      if (pair->key != NULL) {
        g_string_append_printf (data_string, "%s=%s\n", pair->key, pair->value);
      }
      else {
        g_string_append_printf (data_string, "%s\n", pair->value);
      }
    }
  }

  if (length) {
    *length = data_string->len;
  }

  return g_string_free (data_string, FALSE);
}

/*!
 * \brief Retrieves the Keys from a Key File Object
 * \par Function Description
 * Returns all keys for the group name \a group_name. The array of returned
 * keys will be %NULL-terminated, so \a length may optionally be %NULL. In
 * the event that the \a group_name cannot be found, %NULL is returned and
 * \a error is set to #GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  length      return location for the number of keys returned, or %NULL
 * \param [out] error       Location for a GError, or %NULL
 *
 * \returns a newly-allocated %NULL-terminated array of strings.
 *          Use g_strfreev() to free it.
 */
char **
geda_keyfile_get_keys (GedaKeyFile  *key_file,
                       const char   *group_name,
                       unsigned int *length,
                       GError       **error)
{
  GedaKeyFileGroup *group;
  GList            *tmp;
  char            **keys;
  unsigned int      i, num_keys;

  g_return_val_if_fail (key_file != NULL, NULL);
  g_return_val_if_fail (group_name != NULL, NULL);

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {

    if (error) {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                   _("Key file does not have group"),
                   group_name ? group_name : "(null)");
    }
    return NULL;
  }

  num_keys = 0;

  for (tmp = group->key_value_pairs; tmp; tmp = tmp->next) {

    GedaKeyFilePair *pair;

    pair = (GedaKeyFilePair*)tmp->data;

    if (pair->key) {
      num_keys++;
    }
  }

  /* Allocate array of pointers + 1 for sentinel NULL */
  keys = GEDA_MEM_ALLOC (sizeof(char*) * (num_keys + 1));

  i = num_keys - 1;

  for (tmp = group->key_value_pairs; tmp; tmp = tmp->next) {

    GedaKeyFilePair *pair;

    pair = (GedaKeyFilePair*)tmp->data;

    if (pair->key) {
      keys[i] = geda_strdup (pair->key);
      i--;
    }
  }

  keys[num_keys] = NULL;

  if (length) {
    *length = num_keys;
  }

  return keys;
}

/*!
 * \brief Get the Start Group in a Key File
 * \par Function Description
 *  Returns the name of the start group of the file. The returned
 *  string should be freed when no long needed.
 *
 * \param [in] key_file  a #GedaKeyFile object
 *
 * \returns The start group of the key file.
 */
char*
geda_keyfile_get_start_group (GedaKeyFile *key_file)
{
  g_return_val_if_fail (key_file != NULL, NULL);

  if (key_file->start_group) {
    return geda_strdup (key_file->start_group->name);
  }

  return NULL;
}

/*!
 * \brief Get the Groups in a Key File
 * \par Function Description
 *  Returns all groups in the key file loaded with \a key_file. The array
 *  of returned groups will be %NULL-terminated, so \a length may optionally
 *  be %NULL. The returned pointer should be freed using g_strfreev when no
 *  long needed. Optionally, each group name and the pointer to the array
 *  can be freed using g_free
 *
 * \param [in] key_file a #GedaKeyFile object
 * \param [in] length   return location for the number of returned groups, or %NULL
 *
 * \returns a newly-allocated %NULL-terminated array of strings.
 */
char **
geda_keyfile_get_groups (GedaKeyFile *key_file, unsigned int  *length)
{
  GList *group_node;
  char **groups;
  unsigned int  i, num_groups;

  g_return_val_if_fail (key_file != NULL, NULL);

  num_groups = g_list_length (key_file->groups);

  g_return_val_if_fail (num_groups > 0, NULL);

  group_node = g_list_last (key_file->groups);

  g_return_val_if_fail (((GedaKeyFileGroup*)group_node->data)->name == NULL, NULL);

  /* Only need num_groups instead of num_groups + 1 because the
   * first group of the file (last in the list) is always the
   * comment group at the top, which we skip.
   */
  groups = GEDA_MEM_ALLOC (sizeof(char*) * num_groups);

  i = 0;
  for (group_node = group_node->prev;
       group_node != NULL;
       group_node = group_node->prev) {

    GedaKeyFileGroup *group;

    group = (GedaKeyFileGroup*)group_node->data;

    g_warn_if_fail (group->name != NULL);

    groups[i++] = geda_strdup (group->name);
  }
  groups[i] = NULL;

  if (length) {
    *length = i;
  }

  return groups;
}

/*!
 * \brief Get a List of Groups in a Key File
 * \par Function Description
 *  Returns a list of all the groups in the key file \a key_file.
 *  The list should be freed with geda_glist_free_all.
 *
 * \param [in] key_file a #GedaKeyFile object
 *
 * \returns list of groups in the GedaKeyFile.
 */
GList*
geda_keyfile_get_group_list (GedaKeyFile *key_file)
{
  if (GEDA_IS_KEYFILE(key_file)) {

    GList *groups = NULL;
    GList *iter = g_list_last (key_file->groups);

    while (iter) {

      GedaKeyFileGroup *group;

      group = (GedaKeyFileGroup*)iter->data;

      if (group->name) {
        groups = g_list_append(groups, geda_strdup (group->name));
      }
      iter = iter->prev;
    }

    return groups;
  }
  geda_keyfile_not_valid(__func__, key_file);
  return NULL;
}


/*!
 * \brief Retrieve the Value of a Key in a Key File
 * \par Function Description
 * Returns the raw value associated with \a key under \a group_name.
 * Use geda_keyfile_get_string() to retrieve an unescaped UTF-8 string.
 *
 * In the event the key cannot be found, %NULL is returned and
 * \a error is set to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND.  In the
 * event that the \a group_name cannot be found, %NULL is returned
 * and \a error is set to #GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [out] error       Location for a GError, or %NULL
 *
 * \returns a newly allocated string or %NULL if the specified
 *          key cannot be found.
 */
char*
geda_keyfile_get_value (GedaKeyFile *key_file,
                        const char  *group_name,
                        const char  *key,
                        GError      **error)
{
  GedaKeyFileGroup *group;
  GedaKeyFilePair *pair;
  char *value = NULL;

  if ((key_file == NULL) || (group_name == NULL) || (key == NULL)) {

    const char *msg;

    if (key_file == NULL) {
      msg = _("ERROR: pointer to key file is NULL");
    }
    else if (group_name == NULL) {
      msg = _("ERROR: pointer to group name is NULL");
    }
    else {
      msg = _("ERROR: pointer to key is NULL");
    }

    g_set_error (error, EDA_ERROR, EDA_ERROR_NULL_POINTER, "libgeda: %s", msg);

    return NULL;
  }

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                 _("Key file does not have group"),
                 group_name ? group_name : "(null)");
    return NULL;
  }

  pair = geda_keyfile_lookup_key_value_pair (key_file, group, key);

  if (pair) {
    value = geda_strdup (pair->value);
  }
  else {
    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_KEY_NOT_FOUND, "%s '%s'",
                 _("Key file does not have key"), key);
  }

  return value;
}

/*!
 * \brief Set the Value of a Key in a Key File
 * \par Function Description
 * Associates a new value with \a key under \a group_name.
 * If \a key cannot be found then it is created. If \a group_name cannot
 * be found then it is created. To set an UTF-8 string which may contain
 * characters that need escaping (such as newlines or spaces), use
 * geda_keyfile_set_string().
 *
 * \param [in] key_file    a GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] value       a string
 */
void
geda_keyfile_set_value (GedaKeyFile *key_file,
                        const char  *group_name,
                        const char  *key,
                        const char  *value)
{
  g_return_if_fail (geda_keyfile_is_group_name (group_name));
  g_return_if_fail (geda_keyfile_is_key_name (key));
  g_return_if_fail (value != NULL);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
  }
  else {

    GedaKeyFileGroup *group;

    group = geda_keyfile_lookup_group (key_file, group_name);

    if (!group) {

      geda_keyfile_add_group (key_file, group_name);
      group = (GedaKeyFileGroup *) key_file->groups->data;

      geda_keyfile_add_key (key_file, group, key, value);
    }
    else {

      GedaKeyFilePair  *pair;

      pair = geda_keyfile_lookup_key_value_pair (key_file, group, key);

      if (!pair) {
        geda_keyfile_add_key (key_file, group, key, value);
      }
      else {
        GEDA_FREE (pair->value);
        pair->value = geda_strdup (value);
      }
    }
  }
}

/*!
 * \brief Retrieve a String from a Key File
 * \par Function Description
 *  Returns the string value associated with \a key under \a group_name.
 *  Unlike geda_keyfile_get_value(), this function handles escape sequences
 *  like "\s".
 *
 *  In the event the key cannot be found, %NULL is returned and \a error
 *  is set to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. In the event that the
 *  \a group_name cannot be found, %NULL is returned and \a error is set
 *  to #GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [out] error       Location for a GError, or %NULL
 *
 * \returns a newly allocated string or %NULL if the specified
 *          key cannot be found.
 */
char*
geda_keyfile_get_string (GedaKeyFile *key_file,
                         const char  *group_name,
                         const char  *key,
                         GError      **error)
{
  char   *value;
  char   *string_value;
  GError *key_file_error;

  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
    return NULL;
  }

  key_file_error = NULL;

  value = geda_keyfile_get_value (key_file, group_name, key, &key_file_error);

  if (key_file_error) {
    if (error) {
      g_propagate_error (error, key_file_error);
    }
    return NULL;
  }

  if (!g_utf8_validate (value, -1, NULL)) {
    if (error) {
      char *value_utf8 = geda_get_utf8 (value);
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_UNKNOWN_ENCODING,
                   _("Key file contains key '%s' with value '%s' "
                   "which is not UTF-8"), key, value_utf8);
                   GEDA_FREE (value_utf8);
                   GEDA_FREE (value);
    }
    return NULL;
  }

  string_value = geda_keyfile_parse_value_as_string (key_file, value, NULL,
                                                     &key_file_error);
  GEDA_FREE (value);

  if (key_file_error) {

    if (error) {

      if (g_error_matches (key_file_error,
                           GEDA_KEYFILE_ERROR,
                           GEDA_KEYFILE_ERROR_INVALID_VALUE))
      {
        g_set_error (error, GEDA_KEYFILE_ERROR,
                     GEDA_KEYFILE_ERROR_INVALID_VALUE,
                     _("Key file contains key '%s' "
                       "which has a value that cannot be interpreted."),
                     key);
                     g_error_free (key_file_error);
      }
      else {
        g_propagate_error (error, key_file_error);
      }
    }
    else {
      /* Clear the error set by geda_keyfile_parse_value_as_string
       * because no error argument was supplied */
      g_error_free (key_file_error);
    }
  }

  return string_value;
}

/*!
 * \brief Set a String in a Key File
 * \par Function Description
 * Associates a new string value with \a key under \a group_name.
 * If \a key cannot be found then it is created.
 * If \a group_name cannot be found then it is created.
 * Unlike geda_keyfile_set_value(), this function handles characters
 * that need escaping, such as newlines.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] string      Pointer to a string
 */
void
geda_keyfile_set_string (GedaKeyFile *key_file,
                         const char  *group_name,
                         const char  *key,
                         const char  *string)
{
  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
  }
  else {

    char *value;

    g_return_if_fail (string != NULL);

    value = geda_keyfile_parse_string_as_value (key_file, string, FALSE);

    geda_keyfile_set_value (key_file, group_name, key, value);

    GEDA_FREE (value);
  }
}

/*!
 * \brief Retrieve a List of Strings from a Key File
 * \par Function Description
 * Returns the values associated with \a key under \a group_name.
 *
 * In the event the key cannot be found, %NULL is returned and
 * \a error is set to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND.  In the
 * event that the \a group_name cannot be found, %NULL is returned
 * and \a \a error is set to #GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND.
 *
 * \param [in]  key_file    a GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [in]  length      return location for the number of returned strings, or %NULL
 * \param [out] error       Location for a GError, or %NULL
 *
 * \returns a %NULL-terminated string array or %NULL if the specified
 *          key cannot be found. The array should be freed with g_strfreev().
 */
char**
geda_keyfile_get_string_list (GedaKeyFile  *key_file,
                              const char   *group_name,
                              const char   *key,
                              unsigned int *length,
                              GError      **error)
{
  GError *key_file_error;
  GSList *p;
  GSList *pieces;
  char   *value, *string_value, **values;
  int     i, len;

  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
    return NULL;
  }

  if (length) {
    *length = 0;
  }

  key_file_error = NULL;

  value = geda_keyfile_get_value (key_file, group_name, key, &key_file_error);

  if (key_file_error) {
    if (error) {
      g_propagate_error (error, key_file_error);
    }
    return NULL;
  }

  if (!g_utf8_validate (value, -1, NULL)) {
    if (error) {
      char *value_utf8 = geda_get_utf8 (value);
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_UNKNOWN_ENCODING,
                   _("Key file contains key '%s' with value '%s' "
                   "which is not UTF-8"), key, value_utf8);
                   GEDA_FREE (value_utf8);
                   GEDA_FREE (value);
    }
    return NULL;
  }

  pieces = NULL;

  string_value = geda_keyfile_parse_value_as_string (key_file, value, &pieces, &key_file_error);
  GEDA_FREE (value);
  GEDA_FREE (string_value);

  if (key_file_error) {
    if (error) {
      if (g_error_matches (key_file_error,
        GEDA_KEYFILE_ERROR,
        GEDA_KEYFILE_ERROR_INVALID_VALUE))
      {
        g_set_error (error, GEDA_KEYFILE_ERROR,
                     GEDA_KEYFILE_ERROR_INVALID_VALUE,
                     _("Key file contains key '%s' "
                     "which has a value that cannot be interpreted."),
                     key);
                     g_error_free (key_file_error);
      }
      else {
        g_propagate_error (error, key_file_error);
      }
    }
    else {
      /* Clear the error set by geda_keyfile_parse_value_as_string
       * because no error argument was supplied */
      g_error_free (key_file_error);
    }
    geda_gslist_free_all (pieces);
    return NULL;
  }

  len    = g_slist_length (pieces);
  values = GEDA_MEM_ALLOC (sizeof(char*) * (len + 1));

  for (p = pieces, i = 0; p; p = p->next) {
    values[i++] = p->data;
  }

  values[len] = NULL;

  g_slist_free (pieces);

  if (length) {
    *length = len;
  }

  return values;
}

/*!
 * \brief Set a List of Strings in a Key File
 * \par Function Description
 *  Associates a list of string values for \a key under \a group_name.
 *  If \a key cannot be found then it is created.
 *  If \a group_name cannot be found then it is created.
 *
 * \param [in] key_file    a GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] list        an array of string values
 * \param [in] length      number of string values in \a list
 */
void
geda_keyfile_set_string_list (GedaKeyFile  *key_file,
                              const char   *group_name,
                              const char   *key,
                              const char   *const list[],
                              unsigned int  length)
{
  g_return_if_fail (list != NULL || length == 0);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
  }
  else {

    GString *value_list;
    unsigned int  i;

    value_list = g_string_sized_new (length * 128);

    for (i = 0; i < length && list[i] != NULL; i++) {

      char *value;

      value = geda_keyfile_parse_string_as_value (key_file, list[i], TRUE);
      g_string_append (value_list, value);
      g_string_append_c (value_list, key_file->list_separator);

      GEDA_FREE (value);
    }

    geda_keyfile_set_value (key_file, group_name, key, value_list->str);
    g_string_free (value_list, TRUE);
  }
}

/*!
 * \brief Set a Locale String in a Key File
 * \par Function Description
 *  Associates a string value for \a key and \a locale under \a group_name.
 *  If the translation for \a key cannot be found then it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] locale      a locale identifier
 * \param [in] string      a string
 */
void
geda_keyfile_set_locale_string (GedaKeyFile *key_file,
                                const char  *group_name,
                                const char  *key,
                                const char  *locale,
                                const char  *string)
{
  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
  }
  else {

    char *full_key, *value;

    g_return_if_fail (key != NULL);
    g_return_if_fail (locale != NULL);
    g_return_if_fail (string != NULL);

    value    = geda_keyfile_parse_string_as_value (key_file, string, FALSE);
    full_key = geda_sprintf ("%s[%s]", key, locale);

    geda_keyfile_set_value (key_file, group_name, full_key, value);

    GEDA_FREE (full_key);
    GEDA_FREE (value);
  }
}

/*!
 * \brief Retrieve a Locale String from a Key File
 * \par Function Description
 *  Returns the value associated with \a key under \a group_name
 *  translated in the given \a locale if available. If \a locale is
 *  %NULL then the current locale is assumed.
 *
 * If \a key cannot be found then %NULL is returned and \a error is set
 * to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. If the value associated
 * with \a key cannot be interpreted or no suitable translation can
 * be found then the untranslated value is returned.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [in]  locale      a locale identifier or %NULL
 * \param [out] error       Location for a GError, or %NULL
 *
 * \returns a newly allocated string or %NULL if the specified
 *          key cannot be found.
 */
char*
geda_keyfile_get_locale_string (GedaKeyFile *key_file,
                                const char  *group_name,
                                const char  *key,
                                const char  *locale,
                                GError      **error)
{
  char  *translated_value;
  char **languages;
  int    i;

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
    return NULL;
  }

  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

#if GLIB_CHECK_VERSION(2, 28, 0)

  bool free_languages;

  if (!locale) {
    languages = (char**)g_get_language_names ();
    free_languages = FALSE;
  }
  else {
    languages = g_get_locale_variants (locale);
    free_languages = TRUE;
  }

#else

  languages = (char**)g_get_language_names ();

#endif

  translated_value = NULL;

  for (i = 0; languages[i]; i++) {

    char *candidate_key;

    candidate_key = geda_sprintf ("%s[%s]", key, languages[i]);

    translated_value = geda_keyfile_get_string (key_file,
                                                group_name,
                                                candidate_key, NULL);
    GEDA_FREE (candidate_key);

    if (translated_value)
      break;
  }

  /* Fallback to untranslated key */
  if (!translated_value) {

    if (error) {

      GError *key_file_error = NULL;

      translated_value = geda_keyfile_get_string (key_file, group_name, key,
                                                  &key_file_error);

      if (!translated_value) {
        g_propagate_error (error, key_file_error);
      }
    }
    else {
      translated_value = geda_keyfile_get_string (key_file, group_name, key, NULL);
    }
  }

#if GLIB_CHECK_VERSION(2, 28, 0)
  if (free_languages) {
    g_strfreev (languages);
  }
#endif

  return translated_value;
}

/*!
 * \brief Retrieve a List of String from a Key File
 * \par Function Description
 *  Returns the values associated with \a key under \a group_name
 *  translated in the given \a locale if available. If \a locale is
 *  %NULL then the current locale is assumed.
 *
 *  If \a key cannot be found then %NULL is returned and \a error is set
 *  to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. If the values associated
 *  with \a key cannot be interpreted or no suitable translations
 *  can be found then the untranslated values are returned. The
 *  returned array is %NULL-terminated, so \a length may optionally
 *  be %NULL.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [in]  locale      a locale identifier or %NULL
 * \param [in]  length      return location for the number of returned strings or %NULL
 * \param [out] error       Location for a GError or %NULL
 *
 * \returns a newly allocated %NULL-terminated string array or %NULL
 *          if the key is not found. The string array should be freed
 *          with g_strfreev().
 */
char**
geda_keyfile_get_locale_string_list (GedaKeyFile  *key_file,
                                     const char   *group_name,
                                     const char   *key,
                                     const char   *locale,
                                     unsigned int *length,
                                     GError      **error)
{
  GError  *key_file_error;
  char   **values, *value;
  char     list_separator[2];

  unsigned int  len;

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
    return NULL;
  }

  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  key_file_error = NULL;

  value = geda_keyfile_get_locale_string (key_file, group_name,
                                          key, locale,
                                          &key_file_error);

  if (key_file_error) {
    if (error) {
      g_propagate_error (error, key_file_error);
    }
    else {
      /* Clear the error set by geda_keyfile_get_string
       * because no error argument was supplied */
      g_error_free (key_file_error);
    }
  }

  if (!value) {

    if (length) {
      *length = 0;
    }
    return NULL;
  }

  len = strlen (value);

  if (value[len - 1] == key_file->list_separator) {
    value[len - 1] = '\0';
  }

  list_separator[0] = key_file->list_separator;
  list_separator[1] = '\0';
  values = g_strsplit (value, list_separator, 0);

  GEDA_FREE (value);

  if (length) {
    *length = g_strv_length (values);
  }

  return values;
}

/*!
 * \brief Set a Locale String in a Key File
 * \par Function Description
 *  Associates a list of string values for \a key and \a locale under
 *  \a group_name.  If the translation for \a key cannot be found then
 *  it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] locale      a locale identifier
 * \param [in] list        a %NULL-terminated array of locale string values
 * \param [in] length      the length of \a list
 */
void
geda_keyfile_set_locale_string_list (GedaKeyFile        *key_file,
                                     const char         *group_name,
                                     const char         *key,
                                     const char         *locale,
                                     const char * const  list[],
                                     unsigned int        length)
{
  g_return_if_fail (key != NULL);
  g_return_if_fail (locale != NULL);
  g_return_if_fail (length != 0);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
  }
  else {

    GSList *iter;
    GSList *value_list;
    char   *full_key;
    char   *value;
    char   *string;

    unsigned int  i;
    unsigned int  size;

    value_list = NULL;
    size = 0;

    for (i = 0; i < length && list[i] != NULL; i++) {

      value  = geda_keyfile_parse_string_as_value (key_file, list[i], TRUE);
      size  += strlen(value);

      value_list = g_slist_append(value_list, value);
    }

    size   = size + length + 1;
    string = (char*)geda_calloc(size);

    for (iter = value_list; iter; iter = iter->next) {

      value = iter->data;

      strcat (string, value);
      strcat (string, &key_file->list_separator);

      geda_free (value);
    }

    g_slist_free(value_list);

    full_key = geda_sprintf ("%s[%s]", key, locale);

    geda_keyfile_set_value (key_file, group_name, full_key, string);

    geda_free (full_key);
    geda_free (string);
  }
}

/*!
 * \brief Retrieve a Boolean value from a Key File
 * \par Function Description
 * Returns the value associated with \a key under \a group_name as a
 * boolean.
 *
 * If \a key cannot be found then %FALSE is returned and \a error is set
 * to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. Likewise, if the value
 * associated with \a key cannot be interpreted as a boolean then %FALSE
 * is returned and \a error is set to #GEDA_KEYFILE_ERROR_INVALID_VALUE.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [out] error       Location for a GError
 *
 * \returns the value associated with the key as a boolean, or %FALSE
 *          if the key was not found or could not be parsed.
 */
bool
geda_keyfile_get_boolean (GedaKeyFile *key_file,
                          const char  *group_name,
                          const char  *key,
                          GError      **error)
{
  GError *key_file_error;
  char   *value;
  bool    bool_value;

  g_return_val_if_fail (group_name != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);

  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
    return FALSE;
  }

  key_file_error = NULL;

  value = geda_keyfile_get_value (key_file, group_name, key, &key_file_error);

  if (!value) {
    g_propagate_error (error, key_file_error);
    return FALSE;
  }

  bool_value = geda_keyfile_parse_value_as_boolean (key_file, value,
                                                    &key_file_error);
  GEDA_FREE (value);

  if (key_file_error) {

    if (g_error_matches (key_file_error,
                         GEDA_KEYFILE_ERROR,
                         GEDA_KEYFILE_ERROR_INVALID_VALUE))
    {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_INVALID_VALUE,
                   _("Key file contains key '%s' "
                     "which has a value that cannot be interpreted."),
                   key);
      g_error_free (key_file_error);
    }
    else {
      g_propagate_error (error, key_file_error);
    }
  }

  return bool_value;
}

/*!
 * \brief Set a Boolean value in a Key File
 * \par Function Description
 *  Associates a new boolean value with \a key under \a group_name.
 *  If \a key cannot be found then it is created.
 *
 * \param [in] key_file    a GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] value       %TRUE or %FALSE
 */
void
geda_keyfile_set_boolean (GedaKeyFile *key_file,
                          const char  *group_name,
                          const char  *key,
                          bool         value)
{
  if (!GEDA_IS_KEYFILE(key_file)) {
    geda_keyfile_not_valid(__func__, key_file);
  }
  else {

    char *result;

    result = geda_keyfile_parse_boolean_as_value (key_file, value);

    geda_keyfile_set_value (key_file, group_name, key, result);

    GEDA_FREE (result);
  }
}

/*!
 * \brief Retrieve a List of Boolean values from a Key File
 * \par Function Description
 *  Returns the values associated with \a key under \a group_name as
 *  booleans.
 *
 *  If \a key cannot be found then %NULL is returned and \a error is set to
 *  #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated
 *  with \a key cannot be interpreted as booleans then %NULL is returned
 *  and \a error is set to #GEDA_KEYFILE_ERROR_INVALID_VALUE.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [out] length      the number of booleans returned
 * \param [out] error       Location for a GError
 *
 * \returns the values associated with the key as a list of booleans, or %NULL
 *          if the key was not found or could not be parsed. The returned list
 *          of booleans should be freed with GEDA_FREE() when no longer needed.
 */
bool*
geda_keyfile_get_boolean_list (GedaKeyFile  *key_file,
                               const char   *group_name,
                               const char   *key,
                               unsigned int *length,
                               GError       **error)
{
  GError *key_file_error;
  char **values;
  bool *bool_values;
  unsigned int  i, num_bools;

  g_return_val_if_fail (key_file != NULL, NULL);
  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  if (length)
    *length = 0;

  key_file_error = NULL;

  values = geda_keyfile_get_string_list (key_file, group_name, key,
                                         &num_bools, &key_file_error);

  if (key_file_error) {
    g_propagate_error (error, key_file_error);
  }

  if (!values) {
    return NULL;
  }

  bool_values = GEDA_MEM_ALLOC (sizeof(bool) * (num_bools));

  for (i = 0; i < num_bools; i++) {

    bool_values[i] = geda_keyfile_parse_value_as_boolean (key_file,
                                                          values[i],
                                                          &key_file_error);

    if (key_file_error) {

      g_propagate_error (error, key_file_error);
      g_strfreev (values);
      GEDA_FREE (bool_values);

      return NULL;
    }
  }
  g_strfreev (values);

  if (length) {
    *length = num_bools;
  }

  return bool_values;
}

/*!
 * \brief Set a List of Boolean values in a Key File
 * \par Function Description
 *  Associates a list of boolean values with \a key under \a group_name.
 *  If \a key cannot be found then it is created. If \a group_name is %NULL,
 *  the start_group is used.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] list        an array of boolean values
 * \param [in] length      length of the array
 */
void
geda_keyfile_set_boolean_list (GedaKeyFile  *key_file,
                               const char   *group_name,
                               const char   *key,
                               bool          list[],
                               unsigned int  length)
{
  GSList *iter;
  GSList *value_list;
  char   *value;
  char   *string;

  unsigned int i;
  unsigned int size;

  g_return_if_fail (key_file != NULL);
  g_return_if_fail (list != NULL);

  value_list = NULL;
  size = 0;

  for (i = 0; i < length; i++) {

    value  = geda_keyfile_parse_boolean_as_value (key_file, list[i]);
    size  += strlen(value);

    value_list = g_slist_append(value_list, value);
  }

  size   = size + length + 1;
  string = (char*)geda_calloc(size);

  for (iter = value_list; iter; iter = iter->next) {

    value = iter->data;

    strcat (string, value);
    strcat (string, &key_file->list_separator);

    geda_free (value);
  }

  g_slist_free(value_list);


  geda_keyfile_set_value (key_file, group_name, key, string);
  geda_free (string);
}

/*!
 * \brief Retrieve an Integer from a Key File
 * \par Function Description
 * Returns the value associated with \a key under \a group_name as an
 * integer.
 *
 * If \a key cannot be found then 0 is returned and \a error is set to
 * #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated
 * with \a key cannot be interpreted as an integer then 0 is returned
 * and \a error is set to #GEDA_KEYFILE_ERROR_INVALID_VALUE.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [out] error       Location for a GError
 *
 * \returns the value associated with the key as an integer, or
 *          0 if the key was not found or could not be parsed.
 */
int
geda_keyfile_get_integer (GedaKeyFile *key_file,
                          const char  *group_name,
                          const char  *key,
                          GError      **error)
{
  GError *key_file_error;
  char *value;
  int int_value;

  g_return_val_if_fail (key_file != NULL, -1);
  g_return_val_if_fail (group_name != NULL, -1);
  g_return_val_if_fail (key != NULL, -1);

  key_file_error = NULL;

  value = geda_keyfile_get_value (key_file, group_name, key, &key_file_error);

  if (key_file_error) {
    g_propagate_error (error, key_file_error);
    return 0;
  }

  int_value = geda_keyfile_parse_value_as_integer (key_file, value,
                                                   &key_file_error);
  GEDA_FREE (value);

  if (key_file_error) {

    if (g_error_matches (key_file_error,
      GEDA_KEYFILE_ERROR,
      GEDA_KEYFILE_ERROR_INVALID_VALUE))
    {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_INVALID_VALUE,
                   _("Key file contains key '%s' in group '%s' "
                     "which has a value that cannot be interpreted."),
                   key, group_name);
                   g_error_free (key_file_error);
    }
    else {
      g_propagate_error (error, key_file_error);
    }
  }

  return int_value;
}

/*!
 * \brief Set an Integer value in a Key File
 * \par Function Description
 *  Associates a new integer value with \a key under \a group_name.
 *  If \a key cannot be found then it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] value       an integer value
 */
void
geda_keyfile_set_integer (GedaKeyFile *key_file,
                          const char  *group_name,
                          const char  *key,
                          int          value)
{
  char *result;

  g_return_if_fail (key_file != NULL);

  result = geda_keyfile_parse_integer_as_value (key_file, value);
  geda_keyfile_set_value (key_file, group_name, key, result);
  GEDA_FREE (result);
}

/*!
 * \brief Retrieve a 64 Bit Signed Integer from a Key File
 * \par Function Description
 * Returns the value associated with \a key under \a group_name as a signed
 * 64-bit integer. This is similar to geda_keyfile_get_integer() but can return
 * 64-bit results without truncation.
 *
 * \param [in] key_file    a non-%NULL #GedaKeyFile
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [out] error      Location for a GError
 *
 * \returns the value associated with the key as a signed 64-bit integer, or
 *          0 if the key was not found or could not be parsed.
 */
int64
geda_keyfile_get_int64 (GedaKeyFile *key_file,
                        const char  *group_name,
                        const char  *key,
                        GError      **error)
{
  char *s, *end;
  int64 v;

  g_return_val_if_fail (key_file != NULL, -1);
  g_return_val_if_fail (group_name != NULL, -1);
  g_return_val_if_fail (key != NULL, -1);

  s = geda_keyfile_get_value (key_file, group_name, key, error);

  if (s == NULL)
    return 0;

  v = g_ascii_strtoll (s, &end, 10);

  if (*s == '\0' || *end != '\0') {

    g_set_error (error, GEDA_KEYFILE_ERROR, GEDA_KEYFILE_ERROR_INVALID_VALUE,
                 _("Key '%s' in group '%s' has value '%s' "
                   "where %s was expected"),
                 key, group_name, s, "int64");
                 return 0;
  }

  GEDA_FREE (s);
  return v;
}

/*!
 * \brief Set a Signed 64 Bit Integer value in a Key File
 * \par Function Description
 *  Associates a new integer value with \a key under \a group_name.
 *  If \a key cannot be found then it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] value       an integer value
 */
void
geda_keyfile_set_int64 (GedaKeyFile    *key_file,
                        const char *group_name,
                        const char *key,
                        int64       value)
{
  char *result;

  g_return_if_fail (key_file != NULL);

  result = geda_sprintf ("%" G_GINT64_FORMAT, value);
  geda_keyfile_set_value (key_file, group_name, key, result);
  GEDA_FREE (result);
}

/*!
 * \brief Retrieve a 64 Bit Unsigned Integer from a Key File
 * \par Function Description
 *  Returns the value associated with \a key under \a group_name as an unsigned
 *  64-bit integer. This is similar to geda_keyfile_get_integer() but can return
 *  large positive results without truncation.
 *
 *
 * \param [in]  key_file    a non-%NULL #GedaKeyFile
 * \param [in]  group_name  a non-%NULL group name
 * \param [in]  key         a non-%NULL key
 * \param [out] error       Location for a GError
 *
 * \returns the value associated with the key as an unsigned 64-bit integer,
 *          or 0 if the key was not found or could not be parsed.
 */
uint64_t
geda_keyfile_get_uint64 (GedaKeyFile *key_file,
                         const char  *group_name,
                         const char  *key,
                         GError      **error)
{
  char *s, *end;
  uint64_t v;

  g_return_val_if_fail (key_file != NULL, -1);
  g_return_val_if_fail (group_name != NULL, -1);
  g_return_val_if_fail (key != NULL, -1);

  s = geda_keyfile_get_value (key_file, group_name, key, error);

  if (s == NULL) {
    return 0;
  }

  v = g_ascii_strtoull (s, &end, 10);

  if (*s == '\0' || *end != '\0') {

    g_set_error (error, GEDA_KEYFILE_ERROR, GEDA_KEYFILE_ERROR_INVALID_VALUE,
                 _("Key '%s' in group '%s' has value '%s' "
                 "where %s was expected"),
                 key, group_name, s, "uint64");
                 return 0;
  }

  GEDA_FREE (s);
  return v;
}

/*!
 * \brief Set a 64 Bit Unsigned Integer value in a Key File
 * \par Function Description
 * Associates a new integer value with \a key under \a group_name.
 * If \a key cannot be found then it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer Key name
 * \param [in] key         Pointer key name
 * \param [in] value       an integer value
 */
void
geda_keyfile_set_uint64 (GedaKeyFile *key_file,
                         const char  *group_name,
                         const char  *key,
                         uint64_t     value)
{
  char *result;

  g_return_if_fail (key_file != NULL);

  result = geda_sprintf ("%" G_GUINT64_FORMAT, value);
  geda_keyfile_set_value (key_file, group_name, key, result);
  GEDA_FREE (result);
}

/*!
 * \brief Retrieve a list of Integer from a Key File
 * \par Function Description
 * Returns the values associated with \a key under \a group_name as
 * integers.
 *
 * If \a key cannot be found then %NULL is returned and \a error is
 * set to #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. Likewise, if the values
 * associated with \a key cannot be interpreted as integers then %NULL
 * is returned and \a error is set to #GEDA_KEYFILE_ERROR_INVALID_VALUE.
 *
 * \param [in]  key_file    A #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [out] length      Number of integer values returned
 * \param [out] error       Location for a GError
 *
 * \returns the values associated with the key as a list of integers, or %NULL
 *          if the key was not found or could not be parsed. The returned list
 *          of integers should be freed with GEDA_FREE() when no longer needed.
 */
int *
geda_keyfile_get_integer_list (GedaKeyFile  *key_file,
                               const char   *group_name,
                               const char   *key,
                               unsigned int *length,
                               GError       **error)
{
  GError *key_file_error = NULL;
  char  **values;
  int    *int_values;
  unsigned int   i, num_ints;

  g_return_val_if_fail (key_file != NULL, NULL);
  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  if (length) {
    *length = 0;
  }

  values = geda_keyfile_get_string_list (key_file, group_name, key,
                                         &num_ints, &key_file_error);

  if (key_file_error)
    g_propagate_error (error, key_file_error);

  if (!values) {
    return NULL;
  }

  int_values = GEDA_MEM_ALLOC (sizeof(int) * (num_ints));

  for (i = 0; i < num_ints; i++) {

    int_values[i] = geda_keyfile_parse_value_as_integer (key_file,
                                                         values[i],
                                                         &key_file_error);

    if (key_file_error) {

      g_propagate_error (error, key_file_error);
      g_strfreev (values);
      GEDA_FREE (int_values);

      return NULL;
    }
  }
  g_strfreev (values);

  if (length) {
    *length = num_ints;
  }

  return int_values;
}

/*!
 * \brief Set a list of Integer values in a Key File
 * \par Function Description
 * Associates a list of integer values with \a key under \a group_name.
 * If \a key cannot be found then it is created.
 *
 * \param [in] key_file    A #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer Key name
 * \param [in] list        An array of integer values
 * \param [in] length      Number of integer values in the array
 */
void
geda_keyfile_set_integer_list (GedaKeyFile *key_file,
                               const char  *group_name,
                               const char  *key,
                               int          list[],
                               unsigned int length)
{
  GString *values;
  unsigned int  i;

  g_return_if_fail (key_file != NULL);
  g_return_if_fail (list != NULL);

  values = g_string_sized_new (length * 16);

  for (i = 0; i < length; i++) {

    char *value;

    value = geda_keyfile_parse_integer_as_value (key_file, list[i]);

    g_string_append (values, value);
    g_string_append_c (values, key_file->list_separator);

    GEDA_FREE (value);
  }

  geda_keyfile_set_value (key_file, group_name, key, values->str);
  g_string_free (values, TRUE);
}

/*!
 * \brief Retrieve a Double from a Key File
 * \par Function Description
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer Key name
 * \param [out] error      Location for a GError
 *
 * Returns the value associated with \a key under \a group_name as a
 * double. If \a group_name is %NULL, the start_group is used.
 *
 * If \a key cannot be found then 0.0 is returned and \a error is set to
 * #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. Likewise, if the value associated
 * with \a key cannot be interpreted as a double then 0.0 is returned
 * and \a error is set to #GEDA_KEYFILE_ERROR_INVALID_VALUE.
 *
 * \returns The value associated with the key as a double, or
 *          0.0 if the key was not found or could not be parsed.
 */
double
geda_keyfile_get_double  (GedaKeyFile *key_file,
                          const char  *group_name,
                          const char  *key,
                          GError      **error)
{
  GError *key_file_error;
  char   *value;
  double double_value;

  g_return_val_if_fail (key_file != NULL, -1);
  g_return_val_if_fail (group_name != NULL, -1);
  g_return_val_if_fail (key != NULL, -1);

  key_file_error = NULL;

  value = geda_keyfile_get_value (key_file, group_name, key, &key_file_error);

  if (key_file_error) {

    g_propagate_error (error, key_file_error);
    return 0;
  }

  double_value = geda_keyfile_parse_value_as_double (key_file, value,
                                                     &key_file_error);
  GEDA_FREE (value);

  if (key_file_error) {

    if (g_error_matches (key_file_error,
      GEDA_KEYFILE_ERROR,
      GEDA_KEYFILE_ERROR_INVALID_VALUE))
    {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_INVALID_VALUE,
                   _("Key file contains key '%s' in group '%s' "
                     "which has a value that cannot be interpreted."),
                   key, group_name);
                   g_error_free (key_file_error);
    }
    else {
      g_propagate_error (error, key_file_error);
    }
  }

  return double_value;
}

/*!
 * \brief Set a Double in a Key File
 * \par Function Description
 *  Associates a new double value with \a key under \a group_name.
 *  If \a key cannot be found then it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] value       Value of type double
 */
void
geda_keyfile_set_double  (GedaKeyFile    *key_file,
                          const char *group_name,
                          const char *key,
                          double      value)
{
  char result[G_ASCII_DTOSTR_BUF_SIZE];

  g_return_if_fail (key_file != NULL);

  g_ascii_dtostr (result, sizeof (result), value);
  geda_keyfile_set_value (key_file, group_name, key, result);
}

/*!
 * \brief Retrieve a  List of Doubles from a Key File
 * \par Function Description
 *  Returns the values associated with \a key under \a group_name as
 *  doubles.
 *
 *  If \a key cannot be found then %NULL is returned and \a error is set to
 *  #GEDA_KEYFILE_ERROR_KEY_NOT_FOUND. Likewise, if the values associated
 *  with \a key cannot be interpreted as doubles then %NULL is returned
 *  and \a error is set to #GEDA_KEYFILE_ERROR_INVALID_VALUE.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name
 * \param [out] length      the number of doubles returned
 * \param [out] error       Location for a GError
 *
 * \returns The values associated with the key as a list of doubles, or %NULL
 *          if the key was not found or could not be parsed. The returned list
 *          of doubles should be freed with GEDA_FREE() when no longer needed.
 */
double *
geda_keyfile_get_double_list  (GedaKeyFile *key_file,
                               const char  *group_name,
                               const char  *key,
                               unsigned int *length,
                               GError      **error)
{
  GError *key_file_error = NULL;
  char **values;
  double *double_values;
  unsigned int  i, num_doubles;

  g_return_val_if_fail (key_file != NULL, NULL);
  g_return_val_if_fail (group_name != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);

  if (length)
    *length = 0;

  values = geda_keyfile_get_string_list (key_file, group_name, key,
                                         &num_doubles, &key_file_error);

  if (key_file_error) {
    g_propagate_error (error, key_file_error);
  }

  if (!values)
    return NULL;

  double_values =  GEDA_MEM_ALLOC (sizeof(double) * num_doubles);

  for (i = 0; i < num_doubles; i++) {

    double_values[i] = geda_keyfile_parse_value_as_double (key_file,
                                                           values[i],
                                                           &key_file_error);

    if (key_file_error) {

      g_propagate_error (error, key_file_error);
      g_strfreev (values);
      GEDA_FREE (double_values);

      return NULL;
    }
  }
  g_strfreev (values);

  if (length)
    *length = num_doubles;

  return double_values;
}

/*!
 * \brief Set a List of Doubles in a Key File
 * \par Function Description
 *  Associates a list of double values with \a key under \a group_name.
 *  If \a key cannot be found then it is created.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 * \param [in] key         Pointer key name
 * \param [in] list        an array of double values
 * \param [in] length      number of double values in the array
 */
void
geda_keyfile_set_double_list (GedaKeyFile  *key_file,
                              const char   *group_name,
                              const char   *key,
                              double        list[],
                              unsigned int  length)
{
  GString *values;
  unsigned int  i;

  g_return_if_fail (key_file != NULL);
  g_return_if_fail (list != NULL);

  values = g_string_sized_new (length * 16);

  for (i = 0; i < length; i++) {

    char result[G_ASCII_DTOSTR_BUF_SIZE];

    g_ascii_dtostr( result, sizeof (result), list[i] );

    g_string_append (values, result);
    g_string_append_c (values, key_file->list_separator);
  }

  geda_keyfile_set_value (key_file, group_name, key, values->str);
  g_string_free (values, TRUE);
}

static bool
geda_keyfile_set_key_comment (GedaKeyFile *key_file,
                              const char  *group_name,
                              const char  *key,
                              const char  *comment,
                              GError      **error)
{
  GedaKeyFileGroup *group;
  GedaKeyFilePair *pair;
  GList *key_node, *tmp;

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                 _("Key file does not have group"),
                 group_name ? group_name : "(null)");

                 return FALSE;
  }

  /* First find the key the comments are supposed to be
   * associated with
   */
  key_node = geda_keyfile_lookup_key_value_pair_node (key_file, group, key);

  if (key_node == NULL) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_KEY_NOT_FOUND,
                 _("Key file does not have key '%s' in group '%s'"),
                   key, group->name);
                 return FALSE;
  }

  /* Then find all the comments already associated with the
   * key and free them
   */
  tmp = key_node->next;

  while (tmp != NULL) {

    GList *comment_node;

    pair = (GedaKeyFilePair*) tmp->data;

    if (pair->key != NULL)
      break;

    comment_node = tmp;
    tmp = tmp->next;
    geda_keyfile_remove_key_value_pair_node (key_file, group,
                                             comment_node);
  }

  if (comment == NULL) {
    return TRUE;
  }

  /* Now we can add our new comment */
  pair = GEDA_MEM_ALLOC (sizeof(GedaKeyFilePair));
  pair->key = NULL;
  pair->value = geda_keyfile_parse_comment_as_value (key_file, comment);

  key_node = g_list_insert (key_node, pair, 1);

  return TRUE;
}

static bool
geda_keyfile_set_group_comment (GedaKeyFile *key_file,
                                const char  *group_name,
                                const char  *comment,
                                GError      **error)
{
  GedaKeyFileGroup *group;

  g_return_val_if_fail (geda_keyfile_is_group_name (group_name), FALSE);

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                 _("Key file does not have group"),
                 group_name ? group_name : "(null)");

                 return FALSE;
  }

  /* First remove any existing comment
   */
  if (group->comment) {

    geda_keyfile_pair_free (group->comment);
    group->comment = NULL;
  }

  if (comment != NULL) {

    /* Now add new comment */
    group->comment = GEDA_MEM_ALLOC (sizeof(GedaKeyFilePair));
    group->comment->key = NULL;
    group->comment->value = geda_keyfile_parse_comment_as_value (key_file, comment);
  }

  return TRUE;
}

/* Does not use the Error argument and only returns TRUE */
static bool geda_keyfile_set_top_comment (GedaKeyFile *key_file,
                                          const char  *comment,
                                          GError      **error)
{
  GedaKeyFileGroup *group;
  GList            *group_node;

  /* The last group in the list should be the top (comments only)
   * group in the file
   */
  group_node = g_list_last (key_file->groups);
  group      = (GedaKeyFileGroup*) group_node->data;

  /* Note all keys must be comments at the top of
   * the file, so we can just free it all.
   */
  geda_glist_free_full (group->key_value_pairs, (GDestroyNotify)geda_keyfile_pair_free);
  group->key_value_pairs = NULL;

  if (comment != NULL) {

    GedaKeyFilePair  *pair;

    pair = GEDA_MEM_ALLOC (sizeof(GedaKeyFilePair));
    pair->key = NULL;
    pair->value = geda_keyfile_parse_comment_as_value (key_file, comment);

    group->key_value_pairs = g_list_prepend (group->key_value_pairs, pair);
  }

  return TRUE;
}

/*!
 * \brief Set a Comment in a Key File
 * \par Function Description
 *  Places a comment above \a key from \a group_name.
 *  If \a key is %NULL then \a comment will be written above \a group_name.
 *  If both \a key and \a group_name  are %NULL, then \a comment will be
 *  written above the first group in the file.
 *
 * \param [in]  key_file   a #GedaKeyFile object
 * \param [in]  group_name a group name, or %NULL
 * \param [in]  key        a key
 * \param [in]  comment    a comment
 * \param [out] error      Location for a GError
 *
 * \retval %TRUE if the comment was written, %FALSE otherwise
 */
bool geda_keyfile_set_comment (GedaKeyFile *key_file,
                               const char  *group_name,
                               const char  *key,
                               const char  *comment,
                               GError      **error)
{
  g_return_val_if_fail (key_file != NULL, FALSE);

  if (group_name != NULL && key != NULL) {

    if (!geda_keyfile_set_key_comment (key_file, group_name, key, comment, error))
      return FALSE;
  }
  else if (group_name != NULL) {

    if (!geda_keyfile_set_group_comment (key_file, group_name, comment, error))
      return FALSE;
  }
  else {

    if (!geda_keyfile_set_top_comment (key_file, comment, error))
      return FALSE;
  }

  return TRUE;
}

static char *geda_keyfile_get_key_comment (GedaKeyFile *key_file,
                                           const char  *group_name,
                                           const char  *key,
                                           GError      **error)
{
  GedaKeyFileGroup *group;
  GedaKeyFilePair  *pair;
  GList            *key_node;
  GList            *tmp;
  unsigned int      size;
  char             *string;

  g_return_val_if_fail (geda_keyfile_is_group_name (group_name), NULL);

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {

    if (error) {

      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                   _("Key file does not have group"),
                   group_name ? group_name : "(null)");
    }
    return NULL;
  }

  /* First find the key the comments are supposed to be
   * associated with
   */
  key_node = geda_keyfile_lookup_key_value_pair_node (key_file, group, key);

  if (key_node == NULL) {

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_KEY_NOT_FOUND,
                 _("Key file does not have key '%s' in group '%s'"),
                   key, group->name);
                 return NULL;
  }

  /* Then find all the comments already associated with the
   * key and concatentate them.
   */
  tmp = key_node->next;

  if (!key_node->next)
    return NULL;

  pair = (GedaKeyFilePair*) tmp->data;

  if (pair->key != NULL)
    return NULL;

  while (tmp->next) {

    pair = (GedaKeyFilePair*) tmp->next->data;

    if (pair->key != NULL)
      break;

    tmp = tmp->next;
  }

  size   = 0;
  string = NULL;

  while (tmp != key_node) { /* Not exactly correct */

    char *comment;

    pair = (GedaKeyFilePair*) tmp->data;

    comment = geda_keyfile_parse_value_as_comment (key_file, pair->value);

    if (comment) {

      size_t len = strlen(comment);

      size = size + len;

      if (string == NULL) {
        size++;
        string = (char*)malloc(size);
        strncpy(string, comment, len);
        string[len] = '\0';
      }
      else {

        char *buffer;

        buffer = (char*)realloc(string, size);

        if (!buffer)
          break;

        string = buffer;
        strncat(string, comment, len);
      }

      GEDA_FREE (comment);

      tmp = tmp->prev;
    }
    else {
      break;
    }
  }

  /* Strip trailing line-ending from the returned string */
  return geda_utility_string_remove_last_nl(string);
}

/*! \internal Helper function for:
 * geda_keyfile_get_top_comment   (top set)
 * geda_keyfile_get_group_comment (top unset)
 */
static char *get_group_comment (GedaKeyFile       *key_file,
                                GedaKeyFileGroup  *group,
                                bool               top,
                                GError           **error)
{
  unsigned int size;
  char        *string;
  GList       *tmp;

  tmp = group->key_value_pairs;

  /* Traverse downward=up, break on first, end and blanks if not top */
  while (tmp) {

    GedaKeyFilePair *pair;

    pair = (GedaKeyFilePair*) tmp->data;

    if (!top) {

      /* Looking for group comments so if blank line then stop so that
       * top level comments, if present, are excluded */
      char *comment;

      comment = geda_keyfile_parse_value_as_comment (key_file, pair->value);

      if (comment && (!strlen(comment))) {
        break;
      }
    }

    if (pair->key != NULL) {
      tmp = tmp->prev;
      break;
    }

    if (tmp->next == NULL) {
      break;
    }

    tmp = tmp->next;
  }

  size   = 0;
  string = NULL;

  while (tmp != NULL) {

    GedaKeyFilePair *pair;
    char            *comment;

    pair = (GedaKeyFilePair*)tmp->data;

    comment = geda_keyfile_parse_value_as_comment (key_file, pair->value);

    if (comment) {

      size_t len = strlen(comment);

      /* If looking for top comments and blank line then stop so that
       * the first group comments, if present, are not included */
      if (top && !len) {
        break;
      }

      size = size + len;

      if (string == NULL) {
        size++;
        string = (char*)malloc(size);
        strncpy(string, comment, len);
        string[len] = '\0';
      }
      else {

        char *buffer;

        buffer = (char*)realloc(string, size);

        if (!buffer)
          break;

        string = buffer;
        strncat(string, comment, len);
      }

      GEDA_FREE (comment);

      tmp = tmp->prev;
    }
    else {
      tmp = NULL;
    }
  }

  if (string != NULL) {

    /* Strip trailing line-ending from the returned string */
    return geda_utility_string_remove_last_nl(string);
  }

  if (error) {

    const char *group_name = group->name;

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_NO_COMMENT, "%s '%s'",
                 _("Group does not have comment"),
                   group_name ? group_name : "(null)");
  }
  return NULL;
}

static char *geda_keyfile_get_group_comment (GedaKeyFile *key_file,
                                             const char  *group_name,
                                             GError      **error)
{
  GedaKeyFileGroup *group;
  GList *group_node;

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {

    if (error) {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                   _("Key file does not have group"),
                   group_name ? group_name : "(null)");
    }
    return NULL;
  }

  if (group->comment) {
    return geda_strdup (group->comment->value);
  }

  group_node = geda_keyfile_lookup_group_node (key_file, group_name);
  group_node = group_node->next;
  group      = (GedaKeyFileGroup*)group_node->data;

  return get_group_comment (key_file, group, FALSE, error);
}

static char *geda_keyfile_get_top_comment (GedaKeyFile  *key_file,
                                           GError      **error)
{
  GedaKeyFileGroup *group;
  GList *group_node;

  /* The last group in the list should be the top (comments only)
   * group in the file
   */
  g_warn_if_fail (key_file->groups != NULL);

  group_node = g_list_last (key_file->groups);
  group      = (GedaKeyFileGroup *) group_node->data;

  g_warn_if_fail (group->name == NULL);

  return get_group_comment (key_file, group, TRUE, error);
}

/*!
 * \brief Retrieve a Comment from a Key File
 * \par Function Description
 *  Retrieves a comment above \a key from \a group_name. If \a key
 *  is %NULL then \a comment will be read from above \a group_name.
 *  If both \a key and \a group_name are %NULL, then \a comment will
 *  be read from above the first group in the file.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  a group name, or %NULL
 * \param [in]  key         Pointer key name, or %NULL
 * \param [out] error       Optional location for a GError
 *
 * \returns a comment that should be freed with GEDA_FREE()
 */
char *geda_keyfile_get_comment (GedaKeyFile *key_file,
                                const char  *group_name,
                                const char  *key,
                                GError      **error)
{
  g_return_val_if_fail (key_file != NULL, NULL);

  if (group_name != NULL && key != NULL) {
    return geda_keyfile_get_key_comment (key_file, group_name, key, error);
  }
  else if (group_name != NULL) {
    return geda_keyfile_get_group_comment (key_file, group_name, error);
  }

  return geda_keyfile_get_top_comment (key_file, error);
}

/*!
 * \brief Retrieve the Group comment in a Key File
 * \par Function Description
 *  Removes a comment above \a key from \a group_name.
 *  If \a key is %NULL then comment will be removed above \a group_name.
 *  If both \a key and \a group_name are %NULL, then, from some odd
 *  reason, the comment will be removed above the first group in the
 *  file. Does this make any sense?
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  a group name, or %NULL
 * \param [in]  key         a key
 * \param [out] error       Location for a GError
 *
 * \returns %TRUE if the comment was removed, %FALSE otherwise
 */
bool geda_keyfile_remove_comment (GedaKeyFile *key_file,
                                  const char  *group_name,
                                  const char  *key,
                                  GError      **error)
{
  g_return_val_if_fail (key_file != NULL, FALSE);

  if (group_name != NULL && key != NULL) {
    return geda_keyfile_set_key_comment (key_file, group_name, key, NULL, error);
  }
  else if (group_name != NULL) {
    return geda_keyfile_set_group_comment (key_file, group_name, NULL, error);
  }

  return geda_keyfile_set_top_comment (key_file, NULL, error);
}

/*!
 * \brief Determine if Key File contains Group
 * \par Function Description
 *  Checks whether the key file has the group \a group_name.
 *
 * \param [in] key_file    a #GedaKeyFile object
 * \param [in] group_name  Pointer to a group name
 *
 * \returns %TRUE if \a group_name is a part of \a key_file, otherwise %FALSE.
 */
bool geda_keyfile_has_group (GedaKeyFile *key_file, const char *group_name)
{
  g_return_val_if_fail (key_file != NULL, FALSE);
  g_return_val_if_fail (group_name != NULL, FALSE);

  return geda_keyfile_lookup_group (key_file, group_name) != NULL;
}

/* This code remains from a historical attempt to add a new public API
 * which respects the GError rules.
 */
static bool geda_keyfile_has_key_full (GedaKeyFile *key_file,
                                       const char  *group_name,
                                       const char  *key,
                                       bool        *has_key,
                                       GedaKeyFileGroup *group)
{
  GedaKeyFilePair *pair;

  pair = geda_keyfile_lookup_key_value_pair (key_file, group, key);

  if (has_key) {
    *has_key = pair != NULL;
  }
  return TRUE;
}

/*!
 * \brief Determine if a Key exist in the Key File
 * \par Function Description
 *  Checks whether the key file has the key \a key in the group \a group_name.
 *  The Error pointer is optional and is only set if \a group_name does not
 *  exist.
 *  Language bindings should use geda_keyfile_get_value() to test whether
 *  or not a key exists.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name name
 * \param [out] error       Location for a GError
 *
 * \returns %TRUE if \a key is a part of \a group_name, otherwise %FALSE
 */
bool geda_keyfile_has_key (GedaKeyFile *key_file,
                           const char  *group_name,
                           const char  *key,
                           GError      **error)
{
  bool has_key;

  g_return_val_if_fail (key_file   != NULL, FALSE);
  g_return_val_if_fail (group_name != NULL, FALSE);
  g_return_val_if_fail (key        != NULL, FALSE);

  GedaKeyFileGroup *group;

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {
    if (error) {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                   _("Key file does not have group"),
                   group_name ? group_name : "(null)");
    }
    return FALSE;
  }

  if (geda_keyfile_has_key_full (key_file, group_name, key, &has_key, group))
  {
    return has_key;
  }

  return FALSE;
}

static void geda_keyfile_add_group (GedaKeyFile *key_file, const char *group_name)
{
  GedaKeyFileGroup *group;

  g_return_if_fail (key_file != NULL);
  //g_return_if_fail (geda_keyfile_is_group_name (group_name));

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (group != NULL) {
    key_file->current_group = group;
    return;
  }

  group = GEDA_MEM_ALLOC0 (sizeof(GedaKeyFileGroup));

  group->name = geda_strdup (group_name);
  group->lookup_map = g_hash_table_new (g_str_hash, g_str_equal);
  key_file->groups = g_list_prepend (key_file->groups, group);
  key_file->current_group = group;

  if (key_file->start_group == NULL) {
    key_file->start_group = group;
  }

  g_hash_table_insert (key_file->group_hash, (gpointer)group->name, group);
}

static void geda_keyfile_pair_free (GedaKeyFilePair *pair)
{
  if (pair != NULL) {
    GEDA_FREE (pair->key);
    GEDA_FREE (pair->value);
    GEDA_FREE (pair);
  }
}

/* Be careful not to call this function on a node with data in the
 * lookup map without removing it from the lookup map, first.
 *
 * Some current cases where this warning is not a concern are
 * when:
 *   - the node being removed is a comment node
 *   - the entire lookup map is getting destroyed soon after
 *     anyway.
 */
static void geda_keyfile_remove_key_value_pair_node (GedaKeyFile      *key_file,
                                                     GedaKeyFileGroup *group,
                                                     GList            *pair_node)
{
  GedaKeyFilePair *pair;

  pair = (GedaKeyFilePair*) pair_node->data;

  group->key_value_pairs = g_list_remove_link (group->key_value_pairs, pair_node);

  g_warn_if_fail (pair->value != NULL);

  geda_keyfile_pair_free (pair);

  g_list_free_1 (pair_node);
}

static void geda_keyfile_remove_group_node (GedaKeyFile *key_file, GList *group_node)
{
  GedaKeyFileGroup *group;
  GList *tmp;

  group = (GedaKeyFileGroup*)group_node->data;

  if (group->name) {
    g_hash_table_remove (key_file->group_hash, group->name);
  }

  /* If the current group gets deleted make the current group the last
   * added group.
   */
  if (key_file->current_group == group) {

    /* groups should always contain at least the top comment group,
     * unless geda_keyfile_clear has been called
     */
    if (key_file->groups) {
      key_file->current_group = (GedaKeyFileGroup *) key_file->groups->data;
    }
    else {
      key_file->current_group = NULL;
    }
  }

  /* If the start group gets deleted make the start group the first
   * added group.
   */
  if (key_file->start_group == group) {

    tmp = g_list_last (key_file->groups);
    while (tmp != NULL) {

      if (tmp != group_node &&
        ((GedaKeyFileGroup *) tmp->data)->name != NULL)
        break;

      tmp = tmp->prev;
    }

    if (tmp) {
      key_file->start_group = (GedaKeyFileGroup *) tmp->data;
    }
    else {
      key_file->start_group = NULL;
    }
  }

  key_file->groups = g_list_remove_link (key_file->groups, group_node);

  tmp = group->key_value_pairs;
  while (tmp != NULL) {

    GList *pair_node;

    pair_node = tmp;
    tmp = tmp->next;
    geda_keyfile_remove_key_value_pair_node (key_file, group, pair_node);
  }

  g_warn_if_fail (group->key_value_pairs == NULL);

  if (group->comment) {

    geda_keyfile_pair_free (group->comment);
    group->comment = NULL;
  }

  if (group->lookup_map) {

    g_hash_table_destroy (group->lookup_map);
    group->lookup_map = NULL;
  }

  g_free ((char*)group->name);
  g_free (group);
  g_list_free_1 (group_node);
}

/*!
 * \brief Remove Group from a Key File
 * \par Function Description
 *  Removes the specified group, \a group_name, from the key file.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [out] error       Location for a GError or %NULL
 *
 * \returns %TRUE if the group was removed, %FALSE otherwise
 */
bool geda_keyfile_remove_group (GedaKeyFile *key_file,
                                const char  *group_name,
                                GError     **error)
{
  GList *group_node;

  g_return_val_if_fail (key_file != NULL, FALSE);
  g_return_val_if_fail (group_name != NULL, FALSE);

  group_node = geda_keyfile_lookup_group_node (key_file, group_name);

  if (!group_node) {

    if (error) {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                   _("Key file does not have group"), group_name);

    }
    return FALSE;
  }

  geda_keyfile_remove_group_node (key_file, group_node);

  return TRUE;
}

static void geda_keyfile_add_key_value_pair (GedaKeyFile      *key_file,
                                             GedaKeyFileGroup *group,
                                             GedaKeyFilePair  *pair)
{
  g_hash_table_replace (group->lookup_map, pair->key, pair);
  group->key_value_pairs = g_list_prepend (group->key_value_pairs, pair);
}

static void geda_keyfile_add_key (GedaKeyFile      *key_file,
                                  GedaKeyFileGroup *group,
                                  const char       *key,
                                  const char       *value)
{
  GedaKeyFilePair *pair;

  pair = GEDA_MEM_ALLOC0 (sizeof(GedaKeyFilePair));
  pair->key = geda_strdup (key);
  pair->value = geda_strdup (value);

  geda_keyfile_add_key_value_pair (key_file, group, pair);
}

/*!
 * \brief Remove a Key from in a Key File
 * \par Function Description
 *  Removes \a key in \a group_name from the key file.
 *
 * \param [in]  key_file    a #GedaKeyFile object
 * \param [in]  group_name  Pointer to a group name
 * \param [in]  key         Pointer key name name to remove
 * \param [out] error       Location for a GError or %NULL
 *
 * \returns %TRUE if the key was removed, %FALSE otherwise
 */
bool geda_keyfile_remove_key (GedaKeyFile *key_file,
                              const char  *group_name,
                              const char  *key,
                              GError      **error)
{
  GedaKeyFileGroup *group;
  GedaKeyFilePair *pair;

  g_return_val_if_fail (key_file != NULL, FALSE);
  g_return_val_if_fail (group_name != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);

  pair = NULL;

  group = geda_keyfile_lookup_group (key_file, group_name);

  if (!group) {
    if (error) {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND, "%s '%s'",
                   _("Key file does not have group"),
                   group_name ? group_name : "(null)");
    }
    return FALSE;
  }

  pair = geda_keyfile_lookup_key_value_pair (key_file, group, key);

  if (!pair) {
    if (error) {
      g_set_error (error, GEDA_KEYFILE_ERROR,
                   GEDA_KEYFILE_ERROR_KEY_NOT_FOUND,
                   _("Key file does not have key '%s' in group '%s'"),
                   key, group->name);
    }
    return FALSE;
  }

  group->key_value_pairs = g_list_remove (group->key_value_pairs, pair);
  g_hash_table_remove (group->lookup_map, pair->key);
  geda_keyfile_pair_free (pair);

  return TRUE;
}

static GList *geda_keyfile_lookup_group_node (GedaKeyFile *key_file,
                                              const char  *group_name)
{
  GList *tmp;

  for (tmp = key_file->groups; tmp != NULL; tmp = tmp->next) {

    GedaKeyFileGroup *group = (GedaKeyFileGroup *) tmp->data;

    if (group && group->name && strcmp (group->name, group_name) == 0)
      break;
  }

  return tmp;
}

static GedaKeyFileGroup *geda_keyfile_lookup_group (GedaKeyFile *key_file,
                                                    const char  *group_name)
{
  return (GedaKeyFileGroup*)g_hash_table_lookup (key_file->group_hash, group_name);
}

static GList *geda_keyfile_lookup_key_value_pair_node (GedaKeyFile      *key_file,
                                                       GedaKeyFileGroup *group,
                                                       const char       *key)
{
  GList *node;

  for (node = group->key_value_pairs; node != NULL; node = node->next)
  {
    GedaKeyFilePair *pair;

    pair = (GedaKeyFilePair*) node->data;

    if (pair->key && strcmp (pair->key, key) == 0)
      break;
  }

  return node;
}

static GedaKeyFilePair *geda_keyfile_lookup_key_value_pair (GedaKeyFile      *key_file,
                                                            GedaKeyFileGroup *group,
                                                            const char       *key)
{
  return (GedaKeyFilePair*) g_hash_table_lookup (group->lookup_map, key);
}

/* Lines starting with # or consisting entirely of whitespace are merely
 * recorded, not parsed. This function assumes all leading whitespace
 * has been stripped.
 */
static bool geda_keyfile_line_is_comment (const char *line)
{
  return (*line == '#' || *line == '\0' || *line == '\n');
}

static bool geda_keyfile_is_group_name (const char *name)
{
  char *p, *q;

  if (name == NULL)
    return FALSE;

  p = q = (char *) name;

  while (*q && *q != ']' && *q != '[' && !g_ascii_iscntrl (*q)) {
    q = g_utf8_find_next_char (q, NULL);
  }

  return (*q != '\0' || q == p) ? FALSE : TRUE;
}

static bool geda_keyfile_is_key_name (const char *name)
{
  char *p, *q;

  if (name == NULL) {
    return FALSE;
  }

  p = q = (char *) name;
  /* We accept a little more than the desktop entry spec says,
   * since gnome-vfs uses mime-types as keys in its cache.
   */
  while (*q && *q != '=' && *q != '[' && *q != ']')
    q = g_utf8_find_next_char (q, NULL);

  /* No empty keys, please */
  if (q == p) {
    return FALSE;
  }

  /* We accept spaces in the middle of keys to not break
   * existing apps, but we don't tolerate initial or final
   * spaces, which would lead to silent corruption when
   * rereading the file.
   */
  if (*p == ' ' || q[-1] == ' ') {
    return FALSE;
  }

  if (*q == '[') {

    q++;
    while (*q && (g_unichar_isalnum (g_utf8_get_char_validated (q, -1)) || *q == '-' || *q == '_' || *q == '.' || *q == '@'))
      q = g_utf8_find_next_char (q, NULL);

    if (*q != ']')
      return FALSE;

    q++;
  }

  return (*q != '\0') ? FALSE : TRUE;
}

/* A group in a key file is made up of a starting '[' followed by one
 * or more letters making up the group name followed by ']'.
 */
static bool geda_keyfile_line_is_group (const char *line)
{
  char *p;

  p = (char *) line;

  if (*p != '[') {
    return FALSE;
  }

  p++;

  while (*p && *p != ']')
    p = g_utf8_find_next_char (p, NULL);

  if (*p != ']') {
    return FALSE;
  }

  /* silently accept whitespace after the ] */
  p = g_utf8_find_next_char (p, NULL);

  while (*p == ' ' || *p == '\t') {
    p = g_utf8_find_next_char (p, NULL);
  }

  return (*p) ? FALSE : TRUE;
}

static bool geda_keyfile_line_is_key_value_pair (const char *line)
{
  char *p;

  p = (char *) g_utf8_strchr (line, -1, '=');

  if (!p)
    return FALSE;

  /* Key must be non-empty */
  return (*p == line[0]) ? FALSE : TRUE;
}

static char *geda_keyfile_parse_value_as_string (GedaKeyFile *key_file,
                                                 const char  *value,
                                                 GSList      **pieces,
                                                 GError      **error)
{
  char *string_value, *p, *q0, *q;

  string_value = GEDA_MEM_ALLOC0 (strlen(value) + 1);

  p = (char *) value;
  q0 = q = string_value;

  while (*p) {

    if (*p == '\\') {

      p++;

      switch (*p) {

        case 's':
          *q = ' ';
          break;

        case 'n':
          *q = '\n';
          break;

        case 't':
          *q = '\t';
          break;

        case 'r':
          *q = '\r';
          break;

        case '\\':
          *q = '\\';
          break;

        case '\0':
          g_set_error_literal (error, GEDA_KEYFILE_ERROR,
                               GEDA_KEYFILE_ERROR_INVALID_VALUE,
                               _("Key file contains escape character "
                                 "at end of line"));
          break;

        default:
          if (pieces && *p == key_file->list_separator)
            *q = key_file->list_separator;
          else {

            *q++ = '\\';
            *q = *p;

            if (*error == NULL) {

              char sequence[3];

              sequence[0] = '\\';
              sequence[1] = *p;
              sequence[2] = '\0';

              g_set_error (error, GEDA_KEYFILE_ERROR,
                           GEDA_KEYFILE_ERROR_INVALID_VALUE, "%s '%s'",
                           _("Key file contains invalid escape sequence"),
                           sequence);
            }
          }
          break;
      }
    }
    else {

      *q = *p;

      if (pieces && (*p == key_file->list_separator)) {

        *pieces = g_slist_prepend (*pieces, geda_strndup (q0, q - q0));
        q0 = q + 1;
      }
    }

    if (*p == '\0')
      break;

    q++;
    p++;
  }

  *q = '\0';

  if (pieces) {

    if (q0 < q) {
      *pieces = g_slist_prepend (*pieces, geda_strndup (q0, q - q0));
    }

    *pieces = g_slist_reverse (*pieces);
  }

  return string_value;
}

static char *geda_keyfile_parse_string_as_value (GedaKeyFile *key_file,
                                                 const char  *string,
                                                 bool         escape_separator)
{
  char *value, *p, *q;
  unsigned int  length;
  bool parsing_leading_space;

  length = strlen (string) + 1;

  /* Worst case would be that every character needs to be escaped.
   * In other words every character turns to two characters
   */
  value = GEDA_MEM_ALLOC(2 * length);

  p = (char*)string;
  q = value;
  parsing_leading_space = TRUE;

  while (p < (string + length - 1)) {

    char escaped_character[3] = { '\\', 0, 0 };

    switch (*p) {

      case ' ':
        if (parsing_leading_space) {
          escaped_character[1] = 's';
          strcpy (q, escaped_character);
          q += 2;
        }
        else {
         *q = *p;
          q++;
        }
        break;
      case '\t':
        if (parsing_leading_space) {
          escaped_character[1] = 't';
          strcpy (q, escaped_character);
          q += 2;
        }
        else {
        *q = *p;
          q++;
        }
        break;
      case '\n':
        escaped_character[1] = 'n';
        strcpy (q, escaped_character);
        q += 2;
        break;
      case '\r':
        escaped_character[1] = 'r';
        strcpy (q, escaped_character);
        q += 2;
        break;
      case '\\':
        escaped_character[1] = '\\';
        strcpy (q, escaped_character);
        q += 2;
        parsing_leading_space = FALSE;
        break;
      default:
        if (escape_separator && *p == key_file->list_separator) {
          escaped_character[1] = key_file->list_separator;
          strcpy (q, escaped_character);
          q += 2;
          parsing_leading_space = TRUE;
        }
        else {
         *q = *p;
          q++;
          parsing_leading_space = FALSE;
        }
        break;
    }
    p++;
  }
  *q = '\0';

  return value;
}

static int geda_keyfile_parse_value_as_integer (GedaKeyFile *key_file,
                                                const char  *value,
                                                GError      **error)
{
  char *eof_int;
  long  long_value;
  int int_value;

  errno = 0;
  long_value = strtol (value, &eof_int, 10);

  if (*value == '\0' || (*eof_int != '\0' && !g_ascii_isspace(*eof_int)))
  {
    char *value_utf8 = geda_get_utf8 (value);
    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_INVALID_VALUE,
                 _("Value '%s' cannot be interpreted as a number."), value_utf8);
                 GEDA_FREE (value_utf8);

                 return 0;
  }

  int_value = long_value;

  if (int_value != long_value || errno == ERANGE) {

    char *value_utf8 = geda_get_utf8 (value);

    g_set_error (error,
                 GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_INVALID_VALUE,
                 _("Integer value '%s' out of range"),
                   value_utf8);
                 GEDA_FREE (value_utf8);

                 return 0;
  }

  return int_value;
}

static char *geda_keyfile_parse_integer_as_value (GedaKeyFile *key_file,
                                                          int  value)

{
  return geda_sprintf ("%d", value);
}

static double geda_keyfile_parse_value_as_double  (GedaKeyFile  *key_file,
                                                   const char   *value,
                                                   GError      **error)
{
  char *end_of_valid_d;
  double double_value = 0;

  double_value = g_ascii_strtod (value, &end_of_valid_d);

  if (*end_of_valid_d != '\0' || end_of_valid_d == value)
  {
    char *value_utf8 = geda_get_utf8 (value);

    g_set_error (error, GEDA_KEYFILE_ERROR,
                 GEDA_KEYFILE_ERROR_INVALID_VALUE,
                 _("Value '%s' cannot be interpreted as a float number."),
                 value_utf8);
                 GEDA_FREE (value_utf8);
  }

  return double_value;
}

static bool geda_keyfile_parse_value_as_boolean (GedaKeyFile *key_file,
                                                 const char  *value,
                                                 GError     **error)
{
  char *value_utf8;

  if (strcmp (value, "true") == 0 || strcmp (value, "1") == 0) {
    return TRUE;
  }
  else if (strcmp (value, "false") == 0 || strcmp (value, "0") == 0) {
    return FALSE;
  }

  value_utf8 = geda_get_utf8 (value);
  g_set_error (error, GEDA_KEYFILE_ERROR,
               GEDA_KEYFILE_ERROR_INVALID_VALUE,
               _("Value '%s' cannot be interpreted as a boolean."), value_utf8);
  GEDA_FREE (value_utf8);

  return FALSE;
}

static char *geda_keyfile_parse_boolean_as_value (GedaKeyFile *key_file,
                                                         bool  value)
{
  if (value) {
    return geda_strdup ("true");
  }
  else {
    return geda_strdup ("false");
  }
}

static char *geda_keyfile_parse_value_as_comment (GedaKeyFile *key_file,
                                                   const char *value)
{
  GString *string;
  char **lines;
  unsigned int  i;

  string = g_string_sized_new (512);

  lines = g_strsplit (value, "\n", 0);

  for (i = 0; lines[i] != NULL; i++) {

    if (lines[i][0] != '#') {
      g_string_append_printf (string, "%s\n", lines[i]);
    }
    else {
      g_string_append_printf (string, "%s\n", lines[i] + 1);
    }
  }
  g_strfreev (lines);

  return g_string_free (string, FALSE);
}

static char *geda_keyfile_parse_comment_as_value (GedaKeyFile *key_file,
                                                   const char *comment)
{
  GString *string;
  char **lines;
  unsigned int  i;

  string = g_string_sized_new (512);

  lines = g_strsplit (comment, "\n", 0);

  for (i = 0; lines[i] != NULL; i++) {
    g_string_append_printf (string, "#%s%s", lines[i],
                            lines[i + 1] == NULL? "" : "\n");
  }

  g_strfreev (lines);

  return g_string_free (string, FALSE);
}

/** @} endgroup geda-keyfile */
