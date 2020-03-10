/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

/*! \file s_clib.c
 *  \brief The component library system
 *
 *  <B>clib</B> stands for component library.
 *
 *  The <b>component library</b> is made up of a number of
 *  <b>component sources</b>, each of which in turn makes available a
 *  number of component <b>symbols</b>.  Each source may be either a
 *  directory on disk containing symbol files, a command in the system
 *  PATH which can generate gEDA symbol data (e.g. from a database),
 *  or a Scheme function which can do likewise.  A component source is
 *  represented by a #CLibSource instance.
 *
 *  The component library system manages component sources and symbols,
 *  and abstracts the interface to the underlying storage.
 *
 *  To initialise the component library, geda_struct_clib_init() is called. To
 *  clean up when it is no longer needed, geda_struct_clib_free() should be
 *  called.
 *
 *  A directory which contains one or more symbol files in gEDA format
 *  may be used as a component source. Each symbol file should have a
 *  filename ending in ".sym" (case insensitive).  A component source
 *  based on a directory can be added using geda_struct_clib_add_directory().
 *  Symbol files with filenames starting with a period "." are ignored.
 *
 *  An executable program in the system search path may be used as a
 *  component source, and it must conform with the specification given
 *  on page \ref libcmds.  A component source based on a command may
 *  be added using geda_struct_clib_add_command().
 *
 *  Scheme functions may be used as a component source; for more
 *  information, please see page \ref libscms.  A component source
 *  based on Scheme functions may be added using geda_struct_clib_add_scm().
 *
 *  Each symbol is identified by its \b name, which is stored in the
 *  saved schematic file.  The name must be a valid for storage in a
 *  gEDA schematic file as the "basename" of a "component" object.
 *  For symbols from directory sources, the filename of the symbol is
 *  taken as the symbol name.  For a command source, the name may be
 *  any permissible string.  Guidelines to follow:
 *
 *    -# Do not begin a symbol name with "EMBEDDED"
 *    -# Do not use whitespace, or any of the characters "<tt>/:!*?</tt>".
 *    -# Try to use unique names.
 *
 *  The component database may be queried using geda_struct_clib_search().
 *  A null-terminated buffer containing symbol data (suitable for
 *  loading using geda_object_read_buffer()) may be obtained using
 *  geda_struct_clib_symbol_get_data(). If an exact symbol name is known,
 *  the symbol data may be requested directly using
 *  geda_struct_clib_symbol_get_data_by_name().
 *
 *
 * \section libcmds Library Commands
 *
 *  A program or set of programs can be used as a component source.
 *  The procedure used to add such a source from a gEDA rc file is:
 *
 * \code
 *  (component-library-command listcmd getcmd name)
 * \endcode
 *
 *  This is implemented by g_rc_component_library_command(), which is
 *  a wrapper for geda_struct_clib_add_command().
 *
 *  The list command will be executed with no further arguments, and
 *  should output a list of available symbol names on stdout. The get
 *  command will have a symbol name appended to it as the final
 *  argument, and should output gEDA symbol data on stdout.
 *
 *  If the command cannot successfully complete, it should exit with
 *  non-zero exit status.  Anything it has output on stdout will be
 *  ignored, and any stderr output displayed to the user.
 *
 * \section libscms Library Scheme Procedures
 *
 *  A set of Scheme procedures can be used as a component source.  The
 *  procedure used to add such a source from a gEDA rc file is:
 *
 * \code
 *  (component-library-funcs listfunc getfunc name)
 * \endcode
 *
 *  This is implemented by g_rc_component_library_funcs(), which is a
 *  wrapper for geda_struct_clib_add_scm().
 *
 *  \b listfunc and \b getfunc must both be Guile procedures. \b
 *  listfunc takes no arguments, and returns a list of symbol
 *  names. \b getfunc takes a symbol name as an argument, and returns
 *  gEDA symbol data in a string, or \b \#f if not known.
 */

#include <config.h>

#include <geda/geda_sdefines.h>  /* Explicitly include for doxygen, not preprocessor */

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#else
#define WIFSIGNALED(x) 0
#define WTERMSIG(x)    0
#define WIFEXITED(x)   1
#define WEXITSTATUS(x) 0
#endif

#include <dirent.h>
#include <libgen.h>

#include <libgeda_priv.h>
#include <libgeda/g_struct.h>

/* Constant definitions
 * ===================
 */

/*! Library command mode used to fetch list of symbols */
//#define CLIB_LIST_CMD       "list" /* WEH: Really? */

/*! Library command mode used to fetch symbol data */
//#define CLIB_DATA_CMD       "get"

/*! Maximum number of symbol cache entries */
#define CLIB_MAX_SYMBOL_CACHE 128

/*! When symbol cache gets full, remove down to this level */
#define CLIB_MIN_SYMBOL_CACHE 96

/* Static variables
 * ================
 */

/*! Holds the list of all known component sources */
static GList *clib_sources = NULL;

/*! Caches results of geda_struct_clib_search().  The key of the hashtable is a
 *  string describing the search that was carried out, and the value
 *  is a list of symbol pointers. */
static GHashTable *clib_search_cache = NULL;

/*! Caches symbol data.  The key of the hashtable is a symbol pointer,
 *  and the value is a #CacheEntry structure containing the data and
 *  the time it was last used. */
static GHashTable *clib_symbol_cache = NULL;

/* Local static functions
 * ======================
 */
static void free_symbol (void *data);
static void free_symbol_cache_entry (void *data);
static void free_source (CLibSource *source);

static int compare_source_name (const void *a, const void *b);
static int compare_symbol_name (const void *a, const void *b);
static void cache_find_oldest (void *key, void *value, void *user_data);

static char *run_source_command (const char *command);
static CLibSymbol *source_has_symbol (const CLibSource *source, const char *name);
static char *get_unique_source_name (const char *name);

static void refresh_directory (CLibSource *source);
static void refresh_command (CLibSource *source);
static void refresh_scm (CLibSource *source);

static char *get_data_directory (const CLibSymbol *symbol);
static char *get_data_command (const CLibSymbol *symbol);
static char *get_data_scm (const CLibSymbol *symbol);

/*! \brief Flush the symbol name lookup cache.
 *  \par Function Description
 *  Clears the hashtable which caches the results of geda_struct_clib_search().
 *  You should not ever need to call this, as all functions which
 *  invalidate the cache are supposed to make sure it is flushed.
 */
static void geda_struct_clib_flush_search_cache (void)
{
  if (clib_search_cache) {
    g_hash_table_remove_all (clib_search_cache);  /* Introduced in glib 2.12 */
  }
}

/*! \brief Flush the symbol data cache.
 *  \par Function Description
 *  Clears the hashtable which caches the results of geda_struct_clib_symbol_get_data().
 *  You should not ever need to call this, as all functions which
 *  invalidate the cache are supposed to make sure it is flushed.
 */
static void geda_struct_clib_flush_symbol_cache (void)
{
  if (clib_symbol_cache) {
    g_hash_table_remove_all (clib_symbol_cache);  /* Introduced in glib 2.12 */
  }
}

void geda_struct_clib_flush_cache (void)
{
  if (clib_search_cache) {
    g_hash_table_remove_all (clib_search_cache);  /* Introduced in glib 2.12 */
  }
  if (clib_symbol_cache) {
    g_hash_table_remove_all (clib_symbol_cache);  /* Introduced in glib 2.12 */
  }
}

/*! \brief Initialize the component library.
 *  \par Function Description
 *  Resets and initializes the component library.
 *
 *  \warning This function must be called before any other functions
 *  from s_clib.c.
 */
void geda_struct_clib_init (void)
{
  if (clib_sources != NULL) {
    geda_struct_clib_free ();
  }

  if (clib_search_cache != NULL) {
    geda_struct_clib_flush_search_cache();
  }
  else {
    clib_search_cache = g_hash_table_new_full ((GHashFunc) g_str_hash,
                                               (GEqualFunc)g_str_equal,
                                               (GDestroyNotify) g_free,
                                               (GDestroyNotify) g_list_free);
  }

  if (clib_symbol_cache != NULL) {
    geda_struct_clib_flush_symbol_cache();
  }
  else {
    clib_symbol_cache =
      g_hash_table_new_full ((GHashFunc)  g_direct_hash,
                             (GEqualFunc) g_direct_equal,
                             NULL,
                             (GDestroyNotify) free_symbol_cache_entry);
  }
}

/*! \brief Iterator callback for freeing a symbol.
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void free_symbol (void *data)
{
  CLibSymbol *symbol = data;

  if (symbol != NULL) {

    symbol->source = NULL;

    GEDA_FREE (symbol->name);
    GEDA_FREE (symbol);
  }
}

/*! \brief Iterator callback for freeing a symbol cache entry.
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void free_symbol_cache_entry (void *data)
{
  CacheEntry *entry = data;
  g_return_if_fail (entry != NULL);
  GEDA_FREE (entry->data);
  GEDA_FREE (entry);
}

/*! \brief Utility function to Release a Source.
 *  \par Function Description
 *  Utility function to release the
 *
 */
static void free_source (CLibSource *source)
{
  if (source != NULL) {

    if (source->name != NULL) {
      GEDA_FREE (source->name);
    }

    if (source->symbols != NULL) {
      geda_glist_free_full (source->symbols, free_symbol);
      source->symbols = NULL;
    }

    GEDA_FREE (source->category);
    GEDA_FREE (source->directory);
    GEDA_FREE (source->group);
    GEDA_FREE (source->get_cmd);
    GEDA_FREE (source->list_cmd);

    if (source->type == CLIB_SCM) {
      scm_gc_unprotect_object (source->list_fn);
      scm_gc_unprotect_object (source->get_fn);
    }

    GEDA_FREE(source);
  }
}

/*! \brief Free all memory used by the component library.
 *  \par Function Description
 *  Should be called at program exit to clean up any remaining data
 *  being used by the component library system.
 */
void geda_struct_clib_free (void)
{
  if (clib_sources != NULL) {

    GList *iter;

    for (iter = clib_sources; iter != NULL;  NEXT(iter)) {
      free_source((CLibSource*)iter->data);
    }
    g_list_free (clib_sources);
    clib_sources = NULL;
  }
}

/*! \brief Compare two component sources by name.
 *  \par Function Description
 *  Compare two component sources by name, case-insensitively.
 *  Typically used when calling g_list_sort(). Private function used
 *  only in s_clib.c. Argument order is as strcasecmp().
 *
 *  \param a First source to compare
 *  \param b Second source to compare
 *
 * \retval 0 if the names are equivalent, or
 *         1 if the names are NOT equivalent.
 */
static int compare_source_name (const void *a, const void *b)
{
  const CLibSource *src1 = a;
  const CLibSource *src2 = b;

  g_return_val_if_fail ((src1 != NULL), 0);
  g_return_val_if_fail ((src2 != NULL), 0);

  g_return_val_if_fail ((src1->name != NULL), 0);
  g_return_val_if_fail ((src2->name != NULL), 0);

  return strcasecmp(src1->name, src2->name);
}

/*! \brief Compare two component symbols by name.
 *  \par Function Description
 *  Compare two component symbols by name, case-insensitively.
 *  Typically used when calling g_list_sort(). Private function used
 *  only in s_clib.c.  Argument order is as strcasecmp().
 *
 *  \param a First symbol to compare
 *  \param b Second symbol to compare
 *
 * \retval 0 if the names are equivalent, or
 *         1 if the names are NOT equivalent.
 */
static int compare_symbol_name (const void *a, const void *b)
{
  const CLibSymbol *sym1 = a;
  const CLibSymbol *sym2 = b;

  g_return_val_if_fail ((sym1 != NULL), 0);
  g_return_val_if_fail ((sym2 != NULL), 0);

  g_return_val_if_fail ((sym1->name != NULL), 0);
  g_return_val_if_fail ((sym2->name != NULL), 0);

  return strcasecmp(sym1->name, sym2->name);
}

/*! \brief Iterator callback for finding oldest symbol cache entry
 *  \par Function Description
 *  Private function used only in s_clib.c.
 */
static void cache_find_oldest (void *key,
                               void *value,
                               void *user_data)
{
  CacheEntry *current = value;
  CacheEntry **oldest = user_data;

  if (current->accessed < (*oldest)->accessed) {
    *oldest = current;
  }
}

/*! \brief Execute a library command.
 *  \par Function Description
 *  Execute a library command, returning the standard output, or \b
 *  NULL if the command fails for some reason. The system \b PATH is
 *  used to find the program to execute.
 *  The command can write messages to the standard error output. They
 *  are forwarded to the libgeda logging mechanism.
 *
 *  Private function used only in s_clib.c.
 *
 *  \todo This is probably generally useful.
 *
 *  \param command  Command string to execute.
 *  \return The program's output, or \b NULL on failure.
 */
static char *run_source_command (const char *command)
{
  char *standard_output = NULL;
  char *standard_error  = NULL;
  char *err_msg         = NULL;
  int exit_status;

  GError *err  = NULL;
  bool success = FALSE;

  g_return_val_if_fail((command != NULL), NULL);

  g_spawn_command_line_sync (command,
                             &standard_output,
                             &standard_error,
                             &exit_status,
                             &err);

  if (err != NULL) {
    err_msg = geda_sprintf("[%s]: %s", command, err->message);
    g_error_free (err);
  }
  else if (WIFSIGNALED(exit_status)) {
    const char *msg = _("Uncaught signal");
    err_msg = geda_sprintf("[%s]: %s %i", command, msg, WTERMSIG(exit_status));
  }
  else if (WIFEXITED(exit_status) && WEXITSTATUS(exit_status)) {
    const char *msg = _("Error output was");
    err_msg = geda_sprintf("[%s]: %s \n%s\n", command, msg, standard_error);
  }
  else {
    success = TRUE;
  }

  /* forward library command messages */
  if (success && standard_error != NULL) {
    u_log_message ("%s", standard_error);
  }
  else {
    const char *msg = _("Library command failed");
    u_log_message ("%s %s\n", msg, err_msg);
    GEDA_FREE (err_msg);
  }

  GEDA_FREE (standard_error);

  if (success) {
    return standard_output;
  }

  GEDA_FREE (standard_output);
  return NULL;
}

/*! \brief Get a list of available component sources.
 *  \par Function Description
 *  Returns a copies of the current list of source directories.
 *
 *  \remarks The GList returned should be freed when no longer
 *  needed. The returned value is not guaranteed to remain valid over
 *  calls to geda_struct_clib_add_directory() or geda_struct_clib_add_command().
 *
 *  \return A \b GList of CLibSource.
 */
GList *geda_struct_clib_get_sources (const bool sorted)
{
  GList *list = g_list_copy(clib_sources);

  if (sorted) {
    list = g_list_sort (list, (GCompareFunc) compare_source_name);
  }
  return list;
}

/*!
 * \brief Find any symbols within a source with a given name.
 * \par Function Description
 *  Iterates through the symbol list of the given source, checking
 *  if there is already a symbol with the given name.  If there is
 *  such a symbol, it is returned.
 *
 * \param source The source to check.
 * \param name The symbol name to look for.
 *
 * \return The matching symbol, or \b NULL if no match was found.
 */
static CLibSymbol *source_has_symbol (const CLibSource *source,
                                      const char *name)
{
  GList      *symlist;

  for (symlist = g_list_first(source->symbols); symlist != NULL;
       symlist = g_list_next(symlist)) {

    CLibSymbol *symbol = (CLibSymbol*) symlist->data;

    if (strcmp (symbol->name, name) == 0) return symbol;
  }

  return NULL;
}

/*!
 * \brief Make sure a source name is unique.
 * \par Function Description
 *  Checks if a source already exists with the given \a name. If a
 *  source exist then appends a number to the source name. If \a name
 *  is not already in use, returns \a name unmodified. The return value
 *  is always a newly-allocated string, and should be freed.
 */
static char *get_unique_source_name (const char *name)
{
  char *newname = NULL;
  int   i = 0;

  if (geda_struct_clib_get_source_by_name (name) == NULL) {
    return geda_strdup (name);
  }

  do {
    GEDA_FREE (newname);
    i++;
    newname = geda_sprintf ("%s-%i", name, i);
  } while (geda_struct_clib_get_source_by_name (newname) != NULL);

  u_log_message (_("Library name [%s] already in use. Using [%s].\n"),
                 name, newname);

  return newname;
}

/*!
 * \brief Is Path a Symbol Library Source.
 * \par Function Description
 *  Compares the string argument to each of the strings in the
 *  list of source name. If a match is found to the string then
 *  then the source is currently a source and the function
 *  returns TRUE, if a match is not found then FALSE is
 *  returned.
 *
 * \param name The source name to look for in the source list.
 *
 * \return [bool] TRUE is the source was found, otherwise FALSE.
 */
bool geda_struct_clib_source_name_exist (const char *name)
{
  GList *sourcelist;

  bool result = FALSE;

  for (sourcelist = clib_sources; sourcelist != NULL;  NEXT(sourcelist))
  {
    CLibSource *source = (CLibSource*)sourcelist->data;

    if (strcmp (source->name, name) == 0) {
      result = TRUE;
      break;
    }
  }

  return result;
}

/*!
 * \brief Is Path a Symbol Library Source.
 * \par Function Description
 *  Compares the string argument to each of the strings in the
 *  list of sources directories designated. If a match is found
 *  to the string then the folder is currently a source folder
 *  and the function returns TRUE, if a match is not found then
 *  FALSE is returned.
 *
 * \param path The path name to look for in the source list.
 *
 * \return [bool] TRUE is the path was found, otherwise FALSE.
 */
bool geda_struct_clib_source_path_exist (const char *path)
{
  GList *sourcelist;

  bool result = FALSE;

  for (sourcelist = clib_sources; sourcelist != NULL;  NEXT(sourcelist))
  {
    CLibSource *source = (CLibSource*)sourcelist->data;

    if (strcmp (source->directory, path) == 0) {
      result = TRUE;
      break;
    }
  }

  return result;
}

/*!
 * \brief Rescan a directory for symbols.
 * \par Function Description
 *  Rescans a directory for symbols.
 *
 * \todo Does this need to do something more sane with subdirectories
 *       than just skipping them silently?
 *
 *  Private function used only in s_clib.c.
 */
static void refresh_directory (CLibSource *source)
{
  DIR *dirp;
  char tmpname[MAX_PATH];

  g_return_if_fail (source != NULL);
  g_return_if_fail (source->type == CLIB_DIR);

  /* Clear the current symbol list */
  geda_glist_free_full (source->symbols, free_symbol);
  source->symbols = NULL;

  /* Open the directory for reading. */
  dirp = opendir (source->directory);

  if (dirp != NULL) {

    const  char   *suffix;
           char   *tail;
    struct dirent *entry;

    strcpy(tmpname, source->directory);
    tail = &tmpname[0];

    while (*tail != '\0')
      tail++;

    *tail++ = DIR_SEPARATOR;

    /* get all the files within directory */
    while ((entry = readdir (dirp)) != NULL) {

      if (entry->d_name[0] == '.') {
        continue;
      }

      /* tag filename to end of directory */
      strcpy(tail, entry->d_name);

      /* skip subdirectories (for now) */
      if (g_file_test (&tmpname[0], G_FILE_TEST_IS_REGULAR)) {

        suffix = geda_file_get_filename_ext(entry->d_name);

        if (suffix &&
           !geda_utility_string_stricmp (suffix, SYMBOL_FILE_SUFFIX)) {

          /* skip filenames that we already know about. */
          if (source_has_symbol (source, entry->d_name) == NULL) {

            CLibSymbol *symbol;
            /* Create and add new symbol record */
            symbol         = GEDA_MEM_ALLOC0 (sizeof(CLibSymbol));
            symbol->source = source;
            symbol->name   = geda_strdup(entry->d_name);

            /* Prepend is faster, order does not matter. */
            source->symbols = g_list_prepend (source->symbols, symbol);
          }
          else {
            const char *msg1 = _("Duplicate symbol");
            const char *msg2 = _("location");
            u_log_message("%s: <%s>, %s <%s>\n", msg1, entry->d_name,
                                                 msg2, source->directory);
          }
        }
      }
    }
    closedir (dirp);
  }
  else {
    u_log_message ("%s [%s]: %s\n", _("Failed to open"),
                   source->directory, strerror(errno));
    return;
  }

  /* Now sort the list of symbols by name. */
  source->symbols = g_list_sort (source->symbols,
                                (GCompareFunc) compare_symbol_name);

  geda_struct_clib_flush_cache();
}

/*!
 * \brief Re-poll a library command for symbols.
 * \par Function Description
 *  Runs a library command, requesting a list of available symbols,
 *  and updates the source with the new list.
 *
 *  Private function used only in s_clib.c.
 */
static void refresh_command (CLibSource *source)
{
  TextBuffer *tb;
  CLibSymbol *symbol;
  char       *cmdout;


  g_return_if_fail (source != NULL);
  g_return_if_fail (source->type == CLIB_CMD);

  /* Clear the current symbol list */
  geda_glist_free_full (source->symbols, free_symbol);
  source->symbols = NULL;

  /* Run the command to get the list of symbols */
  cmdout = run_source_command (source->list_cmd);

  if (cmdout == NULL) return;

  /* Use a TextBuffer to help reading out the lines of the output */
  tb = geda_struct_textbuffer_new (cmdout, -1);

  while (1) {

    const char *line;
          char *name;

    line = geda_struct_textbuffer_next_line (tb);

    if (line == NULL) break;
    if (line[0] == '.') continue;  /* TODO is this sane? */

    name = geda_utility_string_remove_nl(geda_strdup(line));

    /* skip symbols already known about */
    if (source_has_symbol (source, name) != NULL) {
      GEDA_FREE (name);
      continue;
    }

    symbol         = GEDA_MEM_ALLOC0 (sizeof(CLibSymbol));
    symbol->source = source;
    symbol->name   = name;

    /* Prepend is faster, order does not matter. */
    source->symbols = g_list_prepend (source->symbols, symbol);
  }

  geda_struct_textbuffer_free (tb);
  GEDA_FREE (cmdout);

  /* Sort all symbols by name. */
  source->symbols = g_list_sort (source->symbols,
                                 (GCompareFunc) compare_symbol_name);

  geda_struct_clib_flush_cache();
}

/*!
 * \brief Re-poll a scheme procedure for symbols.
 * \par Function Description
 *  Calls a Scheme procedure to obtain a list of available symbols,
 *  and updates the source with the new list
 *
 *  Private function used only in s_clib.c.
 */
static void refresh_scm (CLibSource *source)
{
  SCM symlist;

  g_return_if_fail (source != NULL);
  g_return_if_fail (source->type == CLIB_SCM);

  /* Clear the current symbol list */
  geda_glist_free_full (source->symbols, free_symbol);
  g_list_free (source->symbols);
  source->symbols = NULL;

  symlist = scm_call_0 (source->list_fn);

  if (SCM_NCONSP (symlist) && (symlist != SCM_EOL)) {
    const char *msg1 = _("Failed to scan library");
    const char *msg2 = _("Scheme function returned non-list");
    u_log_message ("%s [%s]: %s\n", msg1, source->name, msg2);
    return;
  }

  while (symlist != SCM_EOL) {

    SCM symname = SCM_CAR (symlist);

    if (!scm_is_string (symname)) {
      const char *msg = _("Non-string symbol name while scanning library");
      u_log_message ("%s [%s]\n", msg, source->name);
    }
    else {

      CLibSymbol *symbol;
      char       *tmp;

      symbol         = GEDA_MEM_ALLOC0 (sizeof(CLibSymbol));
      symbol->source = source;

      /* Use free function on strings allocated by Guile. */
      tmp            = scm_to_utf8_string (symname);
      symbol->name   = geda_strdup(tmp);
      free (tmp);

      /* Prepend is faster, order does not matter. */
      source->symbols = g_list_prepend (source->symbols, symbol);
    }

    symlist = SCM_CDR (symlist);
  }

  /* Now sort the list of symbols by name. */
  source->symbols = g_list_sort (source->symbols,
                                 (GCompareFunc) compare_symbol_name);

  geda_struct_clib_flush_cache();
}

/*!
 * \brief Rescan all available component libraries.
 * \par Function Description
 *  Resets the list of symbols available from each source, and
 *  repopulates it from scratch. Useful e.g. for checking for new
 *  symbols.
 */
void geda_struct_clib_refresh (void)
{
  GList *sourcelist;

  for (sourcelist = clib_sources; sourcelist != NULL; NEXT(sourcelist)) {

    CLibSource *source = (CLibSource*)sourcelist->data;

    switch (source->type) {
      case CLIB_DIR:
        refresh_directory(source);
        break;

      case CLIB_CMD:
        refresh_command (source);
        break;

      case CLIB_SCM:
        refresh_scm (source);
        break;

      default:
        BUG_IMSG("source has bad source type", source->type);
        break;
    }
  }
}

/*!
 * \brief Get a named component source.
 * \par Function Description
 *  Iterates through the known component sources, checking if there is
 *  a source with the given \a name.
 *
 * \param name The source name to look for.
 *
 * \return The matching source, or \b NULL if no match was found.
 */
const CLibSource *geda_struct_clib_get_source_by_name (const char *name)
{
  GList *sourcelist;

  g_return_val_if_fail (name != NULL, NULL);

  for (sourcelist = clib_sources; sourcelist != NULL; NEXT(sourcelist))
  {
    CLibSource *source = (CLibSource*)sourcelist->data;

    if (name && strcmp (source->name, name) == 0) {
      return source;
    }
    else {
      BUG_MSG("NULL name in source list\n");
    }
  }

  return NULL;
}

/*!
 * \brief Add a directory of symbol files to the library
 * \par Function Description
 *  Adds a directory containing symbol files to the library. Only files
 *  ending with <b>#SYMBOL_FILE_DOT_SUFFIX</b> are considered to be symbol
 *  files. A \a name may be specified for the source; if \a name is \b NULL,
 *  the basename of the directory as returned by geda_file_get_basename_dup
 *  is used.
 *
 * \param directory The path of the directory to add.
 * \param name      A descriptive name for the directory.
 *
 * \return The #CLibSource associated with the directory.
 *
 *  name format options:
 *
 */
const CLibSource *geda_struct_clib_add_directory (const char *directory,
                                                  const char *name)
{
  CLibSource *source;

  char *tmpstr;
  char *category;
  char *group;
  char *pbuff;
  char *ptr_dir1;
  char *ptr_dir2;
  char *ptr_dir3;
  char *ptr;
  char  buffer[MAX_FILE];

  if (directory == NULL) {
    return NULL;
  }

  if (!g_file_test (directory, G_FILE_TEST_IS_DIR)) {
    return NULL;
  }

  if (geda_struct_clib_source_path_exist(directory)) {
    return NULL;
  }

  pbuff = memset (&buffer[0], '\0', MAX_FILE);

  strcpy (pbuff, directory);

  category = NULL;
  group    = NULL;

  /* get 1st level dir */
  ptr_dir1 = basename (pbuff);

  /* change the last slash to NULL */
  ptr = ptr_dir1 - 1;
 *ptr = '\0';

  /* get 2nd level dir */
  ptr_dir2 = basename (pbuff);

  /* change the last slash to NULL */
  ptr = ptr_dir2 - 1;
 *ptr = '\0';

  /* get 3rd level dir */
  ptr_dir3 = basename (pbuff);

  if ( strcmp(SYMBOL_FILE_SUFFIX, ptr_dir3 ) == 0) {
    group = geda_strdup (ptr_dir2);
  }
  else {
    if ( strcmp(SYMBOL_FILE_SUFFIX, ptr_dir2 ) == 0) {
      group = geda_strdup(ptr_dir1);
    }
    else {
      if ( strcmp(SYMBOL_FILE_SUFFIX, ptr_dir1 ) == 0) {
         if (name != NULL )  {
           group = geda_file_get_basename_dup (name);
         }
         else {
           group = geda_strdup ( ptr_dir2 );
         }
      }
      else {
        group = geda_strdup ( ptr_dir2 );
      }
    }
  }

  if (name != NULL) {

    const char *str;
          int   count;

    count = 0;

    for( str = name; *str != '\0'; str++) {
      if (*str == '/') ++count;
    }

    if (!count) {
      tmpstr   = geda_strdup (name);
    }
    else {
      str      = strstr(name, "/");
      category = geda_utility_string_strndup (name, str - name);
      tmpstr   = geda_strdup (str + 1);
    }
  }
  else {
    tmpstr = geda_file_get_basename_dup(directory);
  }

  if ( category == NULL) {
    category = geda_strdup("Standard");
  }

/*
  if (source_name_exist(tmpstr) ) {
    unique_name = geda_utility_string_concat(category, "-", tmpstr, NULL);
    GEDA_FREE (tmpstr);
    tmpstr = unique_name;
  }
  unique_name = get_unique_source_name (tmpstr);
*/

  source            = GEDA_MEM_ALLOC0 (sizeof(CLibSource));

  source->type      = CLIB_DIR;
  source->directory = geda_strdup (directory);
  source->name      = tmpstr;
  source->category  = category;
  source->group     = group;

  /* GEDA_FREE (tmpstr); */

  refresh_directory (source);

  /* Sources added later get scanned earlier */
  clib_sources = g_list_prepend (clib_sources, source);

#if DEBUG
  fprintf(stderr, "%s \t name %s \t directory %s \t category %s \t group %s\n",__func__,
       source->name, source->directory, source->category, source->group);
#endif

  return source;
}

/*!
 * \brief Add symbol-generating commands to the library
 * \par Function Description
 *  Adds a set of commands which can generate symbols to the
 *  library. \a list_cmd and \a get_cmd should be strings consisting
 *  of an executable name followed by any arguments required.
 *  Executables are resolved using the current PATH.  See page \ref
 *  libcmds for more information on library commands.
 *
 * \param list_cmd The executable & arguments used to list available
 *                 symbols.
 * \param get_cmd  The executable & arguments used to retrieve symbol
 *                 data.
 * \param name     A descriptive name for the component source.
 *
 * \return The CLibSource associated with the component source.
 */
const CLibSource *geda_struct_clib_add_command (const char *list_cmd,
                                                const char *get_cmd,
                                                const char *name)
{
  CLibSource *source;
  char *unique_name;

  const char *msg1 = _("Cannot add library");

  if (name == NULL) {
    u_log_message ("%s: %s\n", msg1, _("name not specified"));
    return NULL;
  }

  unique_name = get_unique_source_name (name);

  if (list_cmd == NULL || get_cmd == NULL) {
    const char *msg2 = _("both 'list' and 'get' commands must be specified");
    u_log_message ("%s [%s]: %s.\n", msg1, msg2, unique_name);
  }

  source           = GEDA_MEM_ALLOC0 (sizeof(CLibSource));
  source->type     = CLIB_CMD;
  source->name     = unique_name;

  source->list_cmd = geda_strdup (list_cmd);
  source->get_cmd  = geda_strdup (get_cmd);

  refresh_command (source);

  /* Sources added later get sacnned earlier */
  clib_sources = g_list_prepend (clib_sources, source);

  return source;
}

/*!
 * \brief Add symbol-generating Scheme procedures to the library.
 * \par Function Description
 *  Adds a source to the library based on Scheme procedures.  See page
 *  \ref libscms for more information. Two procedures are required: \a
 *  listfunc must return a Scheme list of symbol names, and \a getfunc
 *  must return a string containing symbol data when passed a symbol
 *  name.
 *
 * \param listfunc Scheme function returning a list of symbols.
 * \param getfunc  Scheme function returning symbol data.
 * \param name     Descriptive name for the component source.
 *
 * \return         The new CLibSource.
 */
const CLibSource *geda_struct_clib_add_scm (SCM listfunc, SCM getfunc, const char *name)
{
  CLibSource *source;
  char *unique_name;

  const char *msg1 = _("Cannot add library");

  if (name == NULL) {
    u_log_message ("%s: %s\n", msg1, _("name not specified"));
    return NULL;
  }

  unique_name = get_unique_source_name (name);

  if (scm_is_false (scm_procedure_p (listfunc)) &&
      scm_is_false (scm_procedure_p (getfunc)))
  {
    const char *msg2 = _("callbacks must be closures");
    u_log_message ("%s [%s]: %s.\n", msg1, msg2, unique_name);
    return NULL;
  }

  source           = GEDA_MEM_ALLOC0 (sizeof(CLibSource));
  source->type     = CLIB_SCM;
  source->name     = unique_name;
  source->list_fn  = scm_gc_protect_object (listfunc);
  source->get_fn   = scm_gc_protect_object (getfunc);

  refresh_scm (source);

  clib_sources     = g_list_prepend (clib_sources, source);

  return source;
}

/*!
 * \brief Get the name of a source.
 * \par Function Description
 *  Get the name of a source for use e.g. in displaying a GUI.
 *
 * \param source Source to be examined.
 *
 * \return Name of source.
 */
const char *geda_struct_clib_source_get_name (const CLibSource *source)
{
  if (source == NULL) return NULL;
  return source->name;
}

/*!
 * \brief Get a list of symbols available from a given source.
 * \par Function Description
 *  Get a \b GList containing all of the symbols available from \a
 *  source.
 *
 * \warning The returned \b GList will not be consistent over a call to
 *          geda_struct_clib_refresh and should be freed when no longer
 *          needed.
 *
 * \param source Source to be examined.
 *
 * \return A \b GList of #CLibSymbol.
 */
GList *geda_struct_clib_source_get_symbols (const CLibSource *source)
{
  if (source == NULL) return NULL;
  return g_list_copy(source->symbols);
}

/*!
 * \brief Get the name of a symbol.
 * \par Function Description
 *  Get the name of a symbol. The symbol name uniquely identifies the
 *  symbol to libgeda.
 *
 * \param symbol Symbol to be examined.
 *
 * \return Name of symbol.
*/
const char *geda_struct_clib_symbol_get_name (const CLibSymbol *symbol)
{
  if (symbol == NULL) return NULL;
  return symbol->name;
}

/*!
 * \brief Get a filename for editing a symbol.
 * \par Function Description
 *  Get the filename of the file a symbol was loaded from, if possible
 *  (e.g. to allow loading for user editing).
 *
 * \remarks The returned string should be freed when no longer needed.
 *
 * \param symbol Symbol to be examined.
 *
 * \return Filename of symbol.
 */
char *geda_struct_clib_symbol_get_filename (const CLibSymbol *symbol)
{
  if (symbol == NULL) return NULL;

  if (symbol->source->type != CLIB_DIR) return NULL;

  return g_build_filename(symbol->source->directory, symbol->name, NULL);
}

/*!
 * \brief Get the source to which a symbol belongs.
 * \par Function Description
 *  Get the source which a symbol is associated.
 *
 * \param symbol Symbol to be examined.
 *
 * \return Source which owns symbol.
 */
const CLibSource *geda_struct_clib_symbol_get_source (const CLibSymbol *symbol)
{
  if (symbol == NULL) return NULL;
  return symbol->source;
}

/*!
 * \brief Get symbol data from a directory source.
 * \par Function Description
 *  Get symbol data from a directory data source.  The return value
 *  should be free()'d when no longer needed.
 *
 *  Private function used only in s_clib.c.
 *
 * \param symbol Symbol to get data for.
 *
 * \return Allocated buffer containing symbol data.
 */
static char *get_data_directory (const CLibSymbol *symbol)
{
  char   *filename = NULL;
  char   *data     = NULL;
  GError *err      = NULL;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source->type == CLIB_DIR), NULL);

  filename = g_build_filename(symbol->source->directory,
                              symbol->name, NULL);

  geda_file_get_contents (filename, &data, NULL, &err);

  if (err != NULL) {
    const char *msg = _("Failed to load symbol from file");
    u_log_message ("%s [%s]: %s\n", msg, filename, err->message);
    g_error_free (err);
  }

  GEDA_FREE (filename);
  return data;
}

/*!
 * \brief Get symbol data from a library command.
 * \par Function Description
 *  Get symbol data from a library command. The return value should
 *  be free()'d when no longer needed.
 *
 *  Private function used only in s_clib.c.
 *
 * \param symbol Symbol to get data for.
 *
 * \return Allocated buffer containing symbol data.
 */
static char *get_data_command (const CLibSymbol *symbol)
{
  char *command;
  char *result;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source->type == CLIB_CMD), NULL);

  command = geda_sprintf ("%s %s", symbol->source->get_cmd, symbol->name);

  result = run_source_command (command);

  GEDA_FREE (command);

  return result;
}

/*!
 * \brief Get symbol data from a Scheme-based component source.
 * \par Function Description
 *  Get symbol data from a Scheme-based component source.  The return
 *  value should be free()'d when no longer needed.
 *
 *  Private function used only in s_clib.c.
 *
 * \param symbol Symbol to get data for.
 *
 * \return Allocated buffer containing symbol data.
 */
static char *get_data_scm (const CLibSymbol *symbol)
{
  SCM symdata;
  char *tmp;
  char *result;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source->type == CLIB_SCM), NULL);

  symdata = scm_call_1 (symbol->source->get_fn,
                        scm_from_utf8_string (symbol->name));

  if (!scm_is_string (symdata)) {
    const char *msg1 = _("Failed to load symbol data");
    const char *msg2 = _("from source");
    u_log_message ("%s [%s] %s [%s]\n", msg1, symbol->name,
                                        msg2, symbol->source->name);
    return NULL;
  }

  /* Need to make sure that the correct free() function is called
   * on strings allocated by Guile. */
  tmp = scm_to_utf8_string (symdata);
  result = geda_strdup(tmp);
  free (tmp);

  return result;
}

/*!
 * \brief Get symbol data.
 * \par Function Description
 *  Get the unparsed gEDA-format data corresponding to a symbol from
 *  the symbol's data source. The return value should be freed when
 *  no longer needed.
 *
 *  On failure, returns \b NULL (the error will be logged).
 *
 * \param symbol Symbol to get data for.
 *
 * \return Allocated buffer containing symbol data.
 */
char *geda_struct_clib_symbol_get_data (const CLibSymbol *symbol)
{
  CacheEntry *cached;
  char *data;
  void *symptr;
  int n;

  g_return_val_if_fail ((symbol != NULL), NULL);
  g_return_val_if_fail ((symbol->source != NULL), NULL);

  /* Trickery to bypass effects of const */
  symptr = (void *) symbol;

  /* First, try the cache. */
  cached = g_hash_table_lookup (clib_symbol_cache, symptr);
  if (cached != NULL) {
    cached->accessed = time(NULL);
    return geda_strdup(cached->data);
  }

  /* If the symbol was not found in the cache, get it directly. */
  switch (symbol->source->type) {

    case CLIB_DIR:
      data = get_data_directory (symbol);
      break;
    case CLIB_CMD:
      data = get_data_command (symbol);
      break;
    case CLIB_SCM:
      data = get_data_scm (symbol);
      break;
    default:
       BUG_IMSG("source has bad source type", symbol->source->type);
      return NULL;
  }

  if (data == NULL) return NULL;

  /* Cache the symbol data */
  cached           = g_new (CacheEntry, 1);
  cached->ptr      = (CLibSymbol *) symptr;
  cached->data     = geda_strdup (data);
  cached->accessed = time (NULL);

  g_hash_table_insert (clib_symbol_cache, symptr, cached);

  /* Clean out the cache if it is too full */
  n = g_hash_table_size (clib_symbol_cache);
  if (n > CLIB_MAX_SYMBOL_CACHE) {
    for ( ; n > CLIB_MIN_SYMBOL_CACHE; n--) {
      g_hash_table_foreach (clib_symbol_cache,
                            (GHFunc) cache_find_oldest,
                            &cached);
      g_hash_table_remove (clib_symbol_cache, cached->ptr);
    }
  }

  return data;
}

/*!
 * \brief Find all symbols matching a pattern.
 * \par Function Description
 *  Searches the library, returning all symbols whose
 *  names match \a pattern.
 *
 *  Two search modes are available: \b CLIB_EXACT, where \a pattern is
 *  compared to the symbol name using strcmp(), and \b CLIB_GLOB,
 *  where \a pattern is assumed to be a glob pattern (see the GLib
 *  documentation for details of the glob syntax applicable).
 *
 * \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free()'d.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to geda_struct_clib_free() or geda_struct_clib_refresh().
 *
 * \param pattern The pattern to match against.
 * \param mode    The search mode to use.
 *
 * \return A \b GList of matching #CLibSymbol structures.
 */
GList *geda_struct_clib_search (const char *pattern, const CLibSearchMode mode)
{
  GList      *result;
  GList      *sourcelist;
  GList      *symlist;
  CLibSource *source;
  char       *key;

  if (pattern == NULL) return NULL;

  result     = NULL;
  sourcelist = NULL;
  symlist    = NULL;

  /* Use different cache keys based on search mode */
  switch (mode) {
    case CLIB_GLOB: /* keytype = g */
      key = geda_sprintf("g%s", pattern);
      break;

    case CLIB_EXACT: /* keytype = s */
      key = geda_sprintf("s%s", pattern);
      break;

    default:
      BUG_IMSG ("Bad search mode", mode);
      return NULL;
  }

  /* Check to see if the query is already in the cache */
  result = (GList*)g_hash_table_lookup (clib_search_cache, key);

  if (result != NULL) {
    GEDA_FREE (key);
    result = g_list_copy (result);
  }
  else {

    GPatternSpec *globpattern = NULL;

    if (mode == CLIB_GLOB) {
      globpattern = g_pattern_spec_new(pattern);
    }

    /* Search each source */
    for (sourcelist = clib_sources; sourcelist != NULL; NEXT(sourcelist)) {

      source = (CLibSource*)sourcelist->data;

      /* Loop through each symbol in the source */
      for (symlist = source->symbols; symlist != NULL; NEXT(symlist)) {

        CLibSymbol *symbol = (CLibSymbol*)symlist->data;

        switch (mode) { /* TODO Eliminate this switch*/
          case CLIB_EXACT:
            if (strcmp (pattern, symbol->name) == 0) {
              result = g_list_prepend (result, symbol);
            }
            break;
          case CLIB_GLOB:
            if (g_pattern_match_string (globpattern, symbol->name)) {
              result = g_list_prepend (result, symbol);
            }
            break;
        }
      }
    }

    result = g_list_reverse (result);

    if (globpattern != NULL) {
      g_pattern_spec_free (globpattern);
    }

    g_hash_table_insert (clib_search_cache, key, g_list_copy (result));
    /* Do NOT free key here, it is stored by the hash table! */
  }
  return result;
}

/*!
 * \brief Invalidate all cached data about a symbol.
 * \par Function Description
 *  Removes all cached symbol data for \a symbol.
 *
 * \param symbol Symbol to flush cached data for.
 */
void
geda_struct_clib_symbol_invalidate_data (const CLibSymbol *symbol)
{
  g_hash_table_remove (clib_symbol_cache, (void *) symbol);
}

/*!
 * \brief Get symbol structure for a given symbol name.
 * \par Function Description
 *  Return the first symbol found with the given \a name.  If more
 *  than one matching symbol is found or no matches are found at all,
 *  emits a log message warning the user.
 *
 * \param name The symbol name to match against.
 *
 * \return The first matching symbol, or NULL if none found.
 */
const CLibSymbol *geda_struct_clib_get_symbol_by_name (const char *name)
{
  GList *symlist = NULL;
  const CLibSymbol *symbol;

  symlist = geda_struct_clib_search (name, CLIB_EXACT);

  if (symlist == NULL) {
    u_log_message (_("Component [%s] was not found in the component library\n"),
                   name);
    symbol = NULL;
  }
  else {

    if (g_list_next (symlist) != NULL) { /* More than one symbol */
      u_log_message (_("More than one component found with name [%s]\n"), name);
    }

    symbol = (CLibSymbol *) symlist->data;
    g_list_free (symlist);
  }
  return symbol;
}

/*!
 * \brief Get symbol data for a given symbol name.
 * \par Function Description
 *  Return the data for the first symbol found with the given name.
 *  This is a helper function for the schematic load system, as it
 *  will always want to load symbols given only their name.
 *
 *  On failure, returns \b NULL (the error will be logged).
 *
 * \param name The symbol name to match against.
 *
 * \return Allocated buffer containing symbol data.
 */
char *geda_struct_clib_symbol_get_data_by_name (const char *name)
{
  const CLibSymbol *symbol;

  symbol = geda_struct_clib_get_symbol_by_name (name);
  if (symbol == NULL) return NULL;
  return geda_struct_clib_symbol_get_data (symbol);
}

/*!
 * \brief Get a list of symbols used.
 * \par Function Description
 *  Scan a #GedaToplevel structure's object list looking for symbols, and
 *  return them in a list.
 *
 * \warning The #CLibSymbol instances in the \b GList returned belong
 *  to the component library, and should be considered constants; they
 *  should not be manipulated or free'd.  On the other hand, the \b
 *  GList returned must be freed with \b g_list_free() when no longer
 *  needed.  Note that the values returned will be invalidated by a
 *  call to geda_struct_clib_free() or geda_struct_clib_refresh().
 *
 * \bug Only includes components which are not embedded, but they
 *      should (probably) also appear in the list.
 *
 * \param toplevel #GedaToplevel structure to scan.
 *
 * \return GList of symbols.
 */
GList *geda_struct_clib_get_symbols (const GedaToplevel *toplevel)
{
  GList  *result  = NULL;
  GList  *iter    = NULL;
  GedaObject *object  = NULL;
  GList  *symlist = NULL;

  CLibSymbol  *sym = NULL;
  const GList *p_iter;
  const GList *o_iter;

  for (p_iter = geda_toplevel_get_pages(toplevel); p_iter != NULL; NEXT(p_iter))
  {
    Page *page = (Page*)p_iter->data;

    for (o_iter = geda_struct_page_get_objects (page); o_iter != NULL; NEXT(o_iter)) {

      object = (GedaObject *)o_iter->data;

      if (object->type != OBJ_COMPLEX) continue;

      if (object->complex->filename == NULL)  continue;

      /* Since we are not looking at embedded symbols, the first component
       * with the given name will be the one we need. N.b. we do not use
       * geda_struct_clib_get_symbol_by_name() because it's spammeh. */
      symlist = geda_struct_clib_search (object->complex->filename, CLIB_EXACT);

      if (symlist == NULL) continue;

      sym = (CLibSymbol *) symlist->data;
      g_list_free (symlist);

      /* We do the list insertion by evilly comparing pointers. This is
       * okay, because we always take the first symbol with the given
       * name, and symbol pointers don't change while this function
       * is running (we hope).  Note that this creates a sorted list.*/
      for (iter = result; iter != NULL; NEXT(iter)) {

        if (iter->data == sym) {
          break; /* Already in list */
        }

        if (compare_symbol_name (iter->data, sym) > 0) {
          /* not in list yet, and gone past point where it should go */
          result = g_list_insert_before (result, iter, sym);
          break;
        }
      }

      if (iter == NULL) {
        /* not in list yet, and at end of list */
        result = g_list_append (result, sym);
      }
    }
  }

  return result;
}
