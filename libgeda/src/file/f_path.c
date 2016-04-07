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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
#include <config.h>
#include <stdio.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <pwd.h>

#if MKDIR_TAKES_ONE_ARG /* MinGW32 */
#  include <io.h>
#endif

#include <libgeda_priv.h>

#ifdef OS_WIN32
#  ifndef STRICT
#    define STRICT
#    include <windows.h>
#    undef STRICT
#  endif
#  ifndef GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
#    define GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT 2
#    define GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS 4
#  endif
#endif

#ifdef OS_WIN32

/* Get a module handle for the libgeda DLL.
 *
 * Adapted from GLib, originally licensed under LGPLv2+.
 *
 */
static void *libgeda_module_handle ()
{
  typedef BOOL (WINAPI *t_GetModuleHandleExA) (DWORD, LPCTSTR, HMODULE *);
  static t_GetModuleHandleExA p_GetModuleHandleExA = NULL;
  static const void *address = (void (*)(void)) &libgeda_module_handle;
  static HMODULE hmodule = NULL;

  if (hmodule != NULL) return (void*) hmodule;

  if (p_GetModuleHandleExA == NULL) {
    p_GetModuleHandleExA =
      (t_GetModuleHandleExA) GetProcAddress (GetModuleHandle ("kernel32.dll"),
                                             "GetModuleHandleExA");
  }

  if (p_GetModuleHandleExA == NULL ||
      !(*p_GetModuleHandleExA) (GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT |
                                GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
                                address, &hmodule)) {
    MEMORY_BASIC_INFORMATION mbi;
    VirtualQuery (address, &mbi, sizeof (mbi));
    hmodule = (HMODULE) mbi.AllocationBase;
  }

  return (void) hmodule;
}

#endif /* G_OS_WIN32 */

static char *sys_data_path    = NULL;
static char *sys_doc_path     = NULL;
static char *sys_config_path  = NULL;
static char *user_config_path = NULL;


/*! \brief Free memory used by Libgeda Path Module.
 *  \par Function Description
 *  This function is public so that programs can use libgeda's path
 *  module without calling libgeda_init. Programs that do so should
 *  call this function before exiting.
 */
void f_path_free (void) {
  GEDA_FREE(sys_data_path);
  GEDA_FREE(sys_doc_path);
  GEDA_FREE(sys_config_path);
  GEDA_FREE(user_config_path);
}

/*! \brief Get the directory with the gEDA system data.
 *
 *  \par Function description
 *  Returns the path to be searched for gEDA data shared between all
 *  users. If the GEDADATA environment variable is set, returns its
 *  value; otherwise, uses a compiled-in path.
 *
 *  On Windows, the compiled in path is *not* used, as it might not
 *  match the path where the user has installed gEDA.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \return the gEDA shared data path, or NULL if none could be found.
 */
const char *f_path_sys_data () {

  /* If GEDADATA is set in the environment, use that path */
  if (sys_data_path == NULL) {
    const char *path;
    path = getenv ("GEDADATA");
    if (path != NULL) {
      sys_data_path = geda_utility_string_strdup(path);
    }
  }

  if (sys_data_path == NULL) {

# if defined (G_OS_WIN32)
    /* On Windows, guess the path from the location of the libgeda DLL. */
    char *directory =
      g_win32_get_package_installation_directory_of_module (libgeda_module_handle ());
    sys_data_path = g_build_filename (directory, "share", "gEDA", NULL);
    GEDA_FREE (directory);
# else
    /* On other platforms, use the compiled-in path */
    sys_data_path = geda_utility_string_strdup(GEDADATADIR);
# endif

    setenv ("GEDADATA", sys_data_path, FALSE);
  }

  return sys_data_path;
}

/*! \brief Get the directory with the gEDA system doc.
 *  \par Function description
 *  Returns the path to be searched for gEDA doc shared between all
 *  users. If the GEDADATA environment variable is set, returns its
 *  value; otherwise, uses a compiled-in path.
 *
 *  On UNIX platforms the XDG Base Directory may or may not return
 *  useful directories. On Windows, the compiled in path is *not* used,
 *  as it might not match the path where the user has installed gEDA.
 *
 *  \remark The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \return the gEDA shared doc path, or NULL if none could be found.
 */
const char *f_path_sys_doc () {

  /* If GEDADOC is set in the environment, use that path */
  if (sys_doc_path == NULL) {
    const char *path;
    path = getenv ("GEDADOC");
    if (path != NULL) {
      sys_doc_path = geda_utility_string_strdup(path);
    }
  }

  if (sys_doc_path == NULL) {
    sys_doc_path = geda_utility_string_strdup(GEDADOCDIR);
    setenv ("GEDADOC", sys_doc_path, FALSE);
  }
  return sys_doc_path;
}

/*! \brief Get the directory with the gEDA system configuration.
 *  \par Function description
 *  Returns the path to be searched for gEDA configuration shared
 *  between all users. If the GEDADATARC environment variable is set,
 *  returns its value; otherwise, use the XDG or a compiled-in path.
 *  Finally fallback to using the system data path.
 *
 *  \remark The returned string is owned by libgeda and should not be
 *  modified or free'd. Path strings are automatically freed when the
 *  library is released or by calling f_path_free().
 *
 *  \note On UNIX platforms the XDG Base Directory may or may not
 *        return useful directories. We only use for Windows which
 *        returns stable results.
 *
 *  \return the gEDA shared config path, or NULL if none could be
 *  found.
 */
const char *f_path_sys_config () {

  /* If GEDADATARC is set in the environment, use that path */
  if (sys_config_path == NULL) {
    const char *path;
    path = getenv ("GEDADATARC");
    if (path != NULL) {
      sys_config_path = geda_utility_string_strdup(path);
    }
  }

  if (sys_config_path == NULL) {

#if defined (GEDA_USE_XDG) && defined(_WIN32)

    const char * const *sys_dirs = g_get_system_config_dirs();
    sys_config_path = g_build_filename (sys_dirs[0], GEDA_CONFIG_DIR, NULL);

#elif defined (GEDARCDIR) && !defined(_WIN32)
    /* If available, use the rc directory set during configure. */
    sys_config_path = geda_utility_string_strdup(GEDARCDIR);
#else
    /* Otherwise, just use the data directory */
    sys_config_path = geda_utility_string_strdup(f_path_sys_data ());
#endif

  }

  if (sys_config_path != NULL) {
    setenv("GEDADATARC", sys_config_path, FALSE);
  }

  return sys_config_path;
}

/*! \brief Get the directory with the gEDA user configuration.
 *  \par Function description
 *  Returns the path to be searched for the current user's gEDA
 *  configuration. Currently defaults to a directory ".gEDA" in the
 *  user's home directory.
 *
 *  \note On UNIX platforms the XDG Base Directory may or may not
 *        return useful directories. We only use for Windows which
 *        returns stable results.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 */
const char *f_path_user_config () {

  if (user_config_path == NULL) {

    const char *homedir;
    const char *configdir;

#if defined (GEDA_USE_XDG) && defined(_WIN32)

    configdir = GEDA_CONFIG_DIR;
    homedir   = g_get_user_config_dir();

#else

    configdir = USER_CONFIG_DIR;

#ifdef OS_LINUX

    homedir = getenv ("HOME");

#else

    homedir = (char*)g_get_home_dir();

#endif

    if (homedir == NULL) {

      struct passwd *pw = getpwuid(getuid());

      if (pw) {
        homedir = pw->pw_dir;
      }
    }

#endif

    user_config_path =
    geda_utility_string_concat(homedir, DIR_SEPARATOR_S, configdir, DIR_SEPARATOR_S, GEDA_CONFIG_DIR, NULL);

    if (user_config_path == NULL) {  /* Otherwise, just use the data directory */
      user_config_path = geda_utility_string_strdup(f_path_sys_data ());
    }
  }
  return user_config_path;
}

/* ------------------ Directory Utility Functions --------------- */

/** \defgroup libgeda-dir-utilities Libgeda Directory Utilities
 *  @{ \par This Group contains utilities to manipulate directories.
*/

#if defined (HAVE_MKDIR) || defined (HAVE__MKDIR)

  #if MKDIR_TAKES_ONE_ARG

    /* MinGW32, mkdir under MinGW only takes one argument */
    #define MKDIR(a, b) mkdir(a)

  #else

    #if HAVE__MKDIR
      /* plain Windows 32 */
      #define MKDIR(a, b) _mkdir(a)
    #else
      #define MKDIR(a, b) mkdir(a, b)
    #endif

  #endif

/*! \brief Make a Directory
 *  \par Function description
 *  This is an internal function used by the f_path_create below.
 *  This function creates a new non-nested directory entry with the
 *  given permission attribute.
 *
 *  \Author Jonathan Leffler
 *  \copyright (C) JLSS 1990-2012
 *
 *  \param path Pointer to string path to be created
 *  \param mode is valid mode_t permission integer
 *
 *  \retval NO_ERROR on success or -1 if and error was encountered
 */
static int f_create_dir(const char *path, mode_t mode)
{
    struct stat stat_buf;
    int         status = NO_ERROR;

    if (stat(path, &stat_buf) != 0) {
        /* Directory does not exist. EEXIST for race condition */
        if (mkdir(path, mode) != 0 && errno != EEXIST)
            status = -1;
    }
    else if (!S_ISDIR(stat_buf.st_mode)) {
        errno = ENOTDIR;
        status = -1;
    }

    return (status);
}

/*! \brief Create a Directory Path
 *  \par Function description
 *  Ensure all directories in path exist, these algorithm takes the
 *  pessimistic view and works top-down to ensure each directory in
 *  path exists, rather than optimistically creating the last element
 *  and working backwards.
 *
 *  \Author Jonathan Leffler
 *  \copyright (C) JLSS 1990-2012
 *
 *  \param path Pointer to string path to be created
 *  \param mode valid mode_t integer permission attribute
 *
 *  \retval NO_ERROR on success or -1 if and error was encountered,
 *          if \a path is NULL then NO_ERROR is returned.
 *
 *  \remark WEH Tweeked for libgeda
 */
int f_path_create(const char *path, mode_t mode)
{
  char           *pp;
  char           *sp;
  int             status;
  char           *copypath;

  status = NO_ERROR;

  if (path) {

    copypath = geda_utility_string_strdup(path);
    pp       = copypath;

    while (status == NO_ERROR && (sp = strchr(pp, '/')) != 0) {

      if (sp != pp) {

        /* Neither root nor double slash in path */
        *sp = '\0';
        status = f_create_dir(copypath, mode);
        *sp = '/';
      }
      pp = sp + 1;
    }
    if (status == NO_ERROR) {
      status = f_create_dir(path, mode);
    }
    GEDA_FREE(copypath);
  }

  return (status);
}
#else

int f_path_create(const char *path, mode_t mode)
{

#if (( GLIB_MAJOR_VERSION >= 2 ) && ( GLIB_MINOR_VERSION >= 8 ))
  return g_mkdir_with_parents (path, mode);
#else
  #error "Don't know how to create a directory on this system."
#endif

}
#endif

/*! \brief Gets Directory Component of a File Name
 *  \par Function Description
 *   Returns directory portion of \a filepath. If \a filepath is
 *  a directory a copy of \a filepath is returned. If \a filepath
 *  has no directory components "." is returned. The returned
 *  string should be freed when no longer needed.
 *
 *  \param [in] filepath The filepath to search.
 *
 *  \returns directory components of \a filepath.
 */
char*
f_path_get_dirname (const char *filepath)
{
  register char *path;

  if (filepath == NULL) {
    path = NULL;
  }
  else if (g_file_test (filepath, G_FILE_TEST_IS_DIR)) {
    path = geda_utility_string_strdup(filepath);
  }
  else {

    register unsigned int  len;

    path = strrchr (filepath, G_DIR_SEPARATOR);

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)

    const char *ptr;

    ptr = strrchr (filepath, '/');

    if (path == NULL || (ptr != NULL && ptr > path)) {
      path = ptr;
    }

#endif

    if (!path) {

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)

      if (isalpha (filepath[0]) && filepath[1] == ':') {

        char root_path[4];

        root_path[0] = filepath[0];
        root_path[1] = ':';
        root_path[2] = '.';
        root_path[3] = '\0';

      }

#else

      char root_path[2];

      root_path[0] = '.';
      root_path[1] = '\0';

#endif

      return geda_utility_string_strdup (root_path);
    }

    while (path > filepath && G_IS_DIR_SEPARATOR (*path)) path--;

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)

    /* path points to the char before the last slash.
     *
     * In case filepath is the root of a drive (X:\) or a child of the
     * root of a drive (X:\foo), include the slash.
     *
     * In case filepath is the root share of an UNC path
     * (\\server\share), add a slash, returning \\server\share\ .
     *
     * In case filepath is a direct child of a share in an UNC path
     * (\\server\share\foo), include the slash after the share name,
     * returning \\server\share\ .
     */
    if (path == filepath + 1 && isalpha (filepath[0]) && filepath[1] == ':') {
      path++;
    }
    else if (G_IS_DIR_SEPARATOR (filepath[0]) &&
             G_IS_DIR_SEPARATOR (filepath[1]) &&
             filepath[2] &&
            !G_IS_DIR_SEPARATOR (filepath[2]) &&
             path >= filepath + 2)
    {
      ptr = filepath + 2;

      while (*ptr && !G_IS_DIR_SEPARATOR (*ptr)) ptr++;

      if (ptr == path + 1) {
        len = (unsigned int) strlen (filepath) + 1;
        path = GEDA_MEM_ALLOC (len + 1);
        strcpy (path, filepath);
        path[len-1] = G_DIR_SEPARATOR;
        path[len] = 0;
        return path;
      }

      if (G_IS_DIR_SEPARATOR (*ptr)) {

        ptr++;
        while (*ptr && !G_IS_DIR_SEPARATOR (*ptr)) ptr++;

        if (ptr == path + 1) {
          path++;
        }
      }
    }

#endif

    len = (unsigned int) 1 + path - filepath;

    path = GEDA_MEM_ALLOC (len + 1);
    memmove (path, filepath, len);
    path[len] = 0;
  }
  return path;
}

/** @} endgroup libgeda-dir-utilities */
