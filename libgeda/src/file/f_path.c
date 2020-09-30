/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
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
#include "../../../config.h"
#include <stdio.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#if MKDIR_TAKES_ONE_ARG /* MinGW32 */
#  include <io.h>
#endif

#include <libgeda_priv.h>

#ifdef OS_WIN32
#  include <ctype.h>    /* for isalpha */
#  ifndef STRICT
#    define STRICT
#    include <windows.h>
#    undef STRICT
#  endif
#  ifndef GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
#    define GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT 2
#    define GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS 4
#  endif
#else
#include <pwd.h>
#endif

static const char DOC_ENV_STR[]    = "GEDADOC";
static const char DATA_ENV_STR[]   = "GEDADATA";
static const char RCDATA_ENV_STR[] = "GEDADATARC";

static char *sys_data_path    = NULL;
static char *sys_doc_path     = NULL;
static char *sys_config_path  = NULL;
static char *user_config_path = NULL;
static char *user_cache_path  = NULL;

/* ------------------ Directory Utility Functions --------------- */

/** \defgroup libgeda-dir-utilities Libgeda Directory Utilities
 *  @{ \par This group contains utilities to manipulate directories.
*/

#if defined (HAVE_MKDIR) || defined (HAVE__MKDIR)

  /*! @cond MKDIR */

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

  /*! @endcond */

/*!
 * \brief Make a Directory
 * \par Function description
 *  This is an internal function used by the geda_file_path_create below.
 *  This function creates a new non-nested directory entry with the
 *  given permission attribute.
 *
 * \author Jonathan Leffler
 * \copyright (C) JLSS 1990-2012
 *
 * \param path Pointer to string path to be created
 * \param mode is valid mode_t permission integer
 *
 * \retval NO_ERROR on success or -1 if and error was encountered
 */
static int f_create_dir(const char *path, mode_t mode)
{
  struct stat stat_buf;
  int status = NO_ERROR;

  if (stat(path, &stat_buf) != 0) {

    /* Directory does not exist. EEXIST for race condition */
    if (MKDIR(path, mode) != 0 && errno != EEXIST) {
      status = -1;
    }
  }
  else if (!S_ISDIR(stat_buf.st_mode)) {
    errno = ENOTDIR;
    status = -1;
  }

  return (status);
}

/*! F0301
 * \brief Create a Directory Path
 * \par Function description
 *  Ensure all directories in path exist, these algorithm takes the
 *  pessimistic view and works top-down to ensure each directory in
 *  path exists, rather than optimistically creating the last element
 *  and working backwards.
 *
 * \author Jonathan Leffler
 * \copyright (C) JLSS 1990-2012
 *
 * \param path Pointer to string path to be created
 * \param mode valid mode_t integer permission attributes
 *
 * \retval NO_ERROR on success or -1 if an error was encountered or
 *         the path already exist with permissions less than \a mode,
 *         or EINVAL if \a path is NULL.
 *
 * \remark WEH Tweeked for libgeda
 */
int geda_file_path_create(const char *path, mode_t mode)
{
  int status;

  status = NO_ERROR;

  if (path) {

    /* Check if dir already exist */
    if (g_file_test(path, G_FILE_TEST_IS_DIR)) {

      status = geda_file_sys_ckmod (path, mode);

    }
    else {

      char *copypath;
      char *pp;
      char *sp;

      copypath = geda_strdup(path);

#if defined (OS_WIN32_NATIVE)

       geda_utility_string_strstr_rep (copypath, "/", DIR_SEPARATOR_S);

#endif

      pp  = copypath;

      while (status == NO_ERROR && (sp = strchr(pp, DIR_SEPARATOR)) != 0) {

        if (sp != pp) {

          /* Neither root nor double slash in path */
          *sp = '\0';
           status = f_create_dir(copypath, mode);
          *sp = DIR_SEPARATOR;
        }
        pp = sp + 1;
      }

      if (status == NO_ERROR) {

        status = f_create_dir(copypath, mode);

      }

      GEDA_FREE(copypath);
    }
  }
  else {
    errno = status = EINVAL;
  }

  return (status);
}

#else /* Not defined (HAVE_MKDIR) || defined (HAVE__MKDIR) */

int geda_file_path_create(const char *path, mode_t mode)
{

#if (( GLIB_MAJOR_VERSION >= 2 ) && ( GLIB_MINOR_VERSION >= 8 ))
  return g_mkdir_with_parents (path, mode);
#else
  #error "Don't know how to create a directory on this system."
#endif

}

#endif

/*!
 * \brief Free memory used by Libgeda Path Module.
 * \par Function Description
 *  This function is public so that programs can use libgeda's path
 *  module without calling libgeda_init. Programs that do so should
 *  call this function before exiting.
 */
void geda_file_path_free (void) {
  GEDA_FREE(sys_data_path);
  GEDA_FREE(sys_doc_path);
  GEDA_FREE(sys_config_path);
  GEDA_FREE(user_config_path);
  GEDA_FREE(user_cache_path);
}

/*! F0303 alias geda_get_dirname
 * \brief Gets Directory Component of a File Name
 * \par Function Description
 *  Returns directory portion of \a filespec. If \a filespec is
 *  a directory a copy of \a filespec is returned. If \a filespec
 *  has no directory components "." is returned. The returned
 *  string should be freed when no longer needed.
 *
 * \param [in] filespec The file path to search.
 *
 * \returns directory components of \a filespec.
 */
char *geda_file_path_get_dirname (const char *filespec)
{
  register char *path;

  if (filespec == NULL) {
    path = NULL;
  }
  else if (g_file_test (filespec, G_FILE_TEST_IS_DIR)) {
    path = geda_strdup(filespec);
  }
  else {

    char *filepath;
    unsigned int  len;

    filepath = geda_strdup(filespec);

#if defined (OS_WIN32_NATIVE)

    const char *ptr;

   /* Replace any forward slashes with DIR_SEPARATOR */
    if (strrchr (filepath, '/')) {

      int i;

      len = strlen(filepath);
      ptr = strrchr (filepath, '/');

      for (i = 0; i < len; i++) {
        if (filepath[i] == '/') {
          filepath[i] = DIR_SEPARATOR;
        }
      }
    }

#endif

    path = strrchr (filepath, DIR_SEPARATOR);

    if (!path) {

      char root_path[4];

      root_path[0] = '.';
      root_path[1] = '\0';

#if defined (OS_WIN32_NATIVE)

      if (isalpha (filepath[0]) && filepath[1] == ':') {

        root_path[0] = filepath[0];
        root_path[1] = ':';
        root_path[2] = '.';
        root_path[3] = '\0';

      }

#endif

      GEDA_FREE(filepath);
      return geda_strdup (root_path);
    }

    while (path > filepath && G_IS_DIR_SEPARATOR (*path)) path--;

#if defined (OS_WIN32_NATIVE)

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
    GEDA_FREE(filepath);
  }

  return path;
}

/** @} endgroup libgeda-dir-utilities */

#ifdef OS_WIN32

/* Get a module handle for the libgeda DLL.
 *
 * Adapted from GLib, originally licensed under LGPLv2+.
 *
 */
static void *libgeda_module_handle (void)
{
  typedef BOOL (WINAPI *t_GetModuleHandleExA) (DWORD, LPCTSTR, HMODULE*);

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
    VirtualQuery (address, &mbi, sizeof(mbi));
    hmodule = (HMODULE) mbi.AllocationBase;
  }

  return (void*) hmodule;
}

#endif /* OS_WIN32 */

/*!
 * \brief Get the directory with the gEDA system data.
 * \par Function description
 *  Returns the path to be searched for gEDA data shared between all
 *  users. If the GEDADATA environment variable is set, returns its
 *  value; otherwise, uses a compiled-in path.
 *
 *  On Windows, the compiled in path is *not* used, as it might not
 *  match the path where the user has installed gEDA.
 *
 * \warning The returned string is owned by libgeda and should not be
 *          modified or free'd.
 *
 * \return the gEDA shared data path, or NULL if none could be found.
 */
const char *geda_file_path_sys_data (void) {

  /* If GEDADATA is set in the environment, use that path */
  if (sys_data_path == NULL) {

    const char *path;

    path = getenv (DATA_ENV_STR);

    if (path != NULL) {
      sys_data_path = geda_strdup(path);
    }
  }

  if (sys_data_path == NULL) {

#if defined (OS_WIN32)

    /* On Windows, guess the path from the location of the libgeda DLL. */

    char *directory;

    directory = g_win32_get_package_installation_directory_of_module (libgeda_module_handle ());
    sys_data_path = g_build_filename (directory, "share", "gEDA", NULL);

    GEDA_FREE (directory);

#else

    /* On other platforms, use the compiled-in path */
    sys_data_path = geda_strdup(GEDADATADIR);

#endif

    setenv (DATA_ENV_STR, sys_data_path, FALSE);
  }

  return sys_data_path;
}

/*!
 * \brief Get the directory with the gEDA system doc.
 * \par Function description
 *  Returns the path to be searched for gEDA doc shared between all
 *  users. If the GEDADATA environment variable is set, returns its
 *  value; otherwise, uses a compiled-in path.
 *
 *  On UNIX platforms the XDG Base Directory may or may not return
 *  useful directories. On Windows, the compiled in path is *not* used,
 *  as it might not match the path where the user has installed gEDA.
 *
 * \remark The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 * \return the gEDA shared doc path, or NULL if none could be found.
 */
const char *geda_file_path_sys_doc (void) {

  /* If GEDADOC is set in the environment, use that path */
  if (sys_doc_path == NULL) {

    const char *path;

    path = getenv (DOC_ENV_STR);

    if (path != NULL) {
      sys_doc_path = geda_strdup(path);
    }
  }

  if (sys_doc_path == NULL) {
    sys_doc_path = geda_strdup(GEDADOCDIR);
    setenv (DOC_ENV_STR, sys_doc_path, FALSE);
  }
  return sys_doc_path;
}

/*!
 * \brief Get the directory with the gEDA system configuration.
 * \par Function description
 *  Returns the path to be searched for gEDA configuration shared
 *  between all users. If the GEDADATARC environment variable is set,
 *  returns its value; otherwise, use the XDG or a compiled-in path.
 *  Finally fallback to using the system data path.
 *
 * \remark The returned string is owned by libgeda and should not be
 *  modified or free'd. Path strings are automatically freed when the
 *  library is released or by calling geda_file_path_free().
 *
 * \note On UNIX platforms the XDG Base Directory may or may not
 *        return useful directories. We only use for Windows which
 *        returns stable results.
 *
 * \return the gEDA shared config path, or NULL if none could be found.
 */
const char *geda_file_path_sys_config (void) {

  /* If GEDADATARC is set in the environment, use that path */
  if (sys_config_path == NULL) {

    const char *path;

    path = getenv (RCDATA_ENV_STR);

    if (path != NULL) {
      sys_config_path = geda_strdup(path);
    }
  }

  /* If GEDADATARC was not specified in the environment */
  if (sys_config_path == NULL) {

#if defined (GEDA_USE_XDG)

    const char * const *sys_dirs = g_get_system_config_dirs();
    sys_config_path = g_build_filename (sys_dirs[0], GEDA_CONFIG_DIR, NULL);

#elif defined (GEDARCDIR) && !defined(OS_WIN32)

    /* If available, use the rc directory set during configure. */
    sys_config_path = geda_strdup(GEDARCDIR);

#else

    /* Otherwise, just use the data directory */
    sys_config_path = geda_strdup(geda_sys_data_path ());

#endif

  }

  if (sys_config_path != NULL) {
    setenv(RCDATA_ENV_STR, sys_config_path, FALSE);
  }

  return sys_config_path;
}

/*!
 * \brief Get the directory with the gEDA user configuration.
 * \par Function description
 *  Returns the path to be searched for the current user's gEDA
 *  configuration. Currently defaults to a directory ".gEDA" in the
 *  user's home directory.
 *
 * \note On UNIX platforms the XDG Base Directory may or may not
 *       return useful directories. We only use for Windows which
 *       returns stable results.
 *
 * \warning The returned string is owned by libgeda and should not be
 *          modified or free'd.
 */
const char *geda_file_path_user_config (void) {

  if (user_config_path == NULL) {

    const char *homedir;
    const char *configdir;

#if defined (GEDA_USE_XDG)

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

#if defined OS_WIN32

      homedir = (char*)g_get_home_dir();

#else

      struct passwd *pw = getpwuid(getuid());

      if (pw) {
        homedir = pw->pw_dir;
      }

#endif

    }

#endif

    user_config_path =
    geda_strconcat(homedir, DIR_SEPARATOR_S, configdir,
                            DIR_SEPARATOR_S, GEDA_CONFIG_DIR, NULL);

    /* As a last resort just use the data directory */
    if (user_config_path == NULL) {
      user_config_path = geda_strdup(geda_sys_data_path ());
    }
  }
  return user_config_path;
}

/*!
 * \brief Get the user gEDA cache directory
 * \par Function description
 *  Returns the path to the current user's gEDA cache directory.
 *  On UNIX platforms this typically returns the equivalent of
 *  ~/.cache/gEDA.
 *
 * \warning The returned string is owned by libgeda and should
 *          not be modified or free'd.
 */
const char *geda_file_path_user_cache (void) {

  if (user_cache_path == NULL) {

    const char *homedir;
    const char *cachedir;

#if defined (GEDA_USE_XDG)

    cachedir = STD_USER_CACHE_DIR;
    homedir  = g_get_user_cache_dir();

#else

    cachedir = STD_USER_CACHE_DIR;

#ifdef OS_LINUX

    homedir = getenv ("HOME");

#else

    homedir = (char*)g_get_home_dir();

#endif

    if (homedir == NULL) {

#if defined OS_WIN32

      homedir = (char*)g_get_home_dir();

#else

      struct passwd *pw = getpwuid(getuid());

      if (pw) {
        homedir = pw->pw_dir;
      }

#endif

    }

#endif

    user_cache_path =
    geda_strconcat(homedir, DIR_SEPARATOR_S, cachedir,
                            DIR_SEPARATOR_S, GEDA_CONFIG_DIR, NULL);

    /* As a last resort just use the config directory */
    if (user_cache_path == NULL) {
      user_cache_path = geda_strdup(geda_user_config_path ());
    }
  }

  return user_cache_path;
}
