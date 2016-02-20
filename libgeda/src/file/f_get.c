/* -*- C header file: f_get.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 *
*/

#include <config.h>

#include <stdio.h>
#include <dirent.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#include <libgeda_priv.h>
#include <version.h>

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h> /* for GetFullPathName */
#  include <io.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

/*! \brief Get the autosave filename for a file
 *  \par Function description
 *  Returns the expected autosave filename for the \a filename passed.
 *
 *  \remarks Returned allocation should be freed when no longer needed.
 *
 *  \param [in] filename The filename to create an autosave filename for.
 *  \return A newly allocated string buffer.
 */
char *f_get_autosave_filename (const char *filename)
{
  char *autosave_name;

  if (filename == NULL) {
    autosave_name = NULL;
  }
  else {

   const char *old_basename;
         char *new_basename;
         char *path_spec;

    old_basename  = f_get_basename(filename);
    path_spec     = f_path_get_dirname(filename);
    new_basename  = geda_utility_string_sprintf(AUTOSAVE_BACKUP_FILENAME_STRING, old_basename);
    autosave_name = g_build_filename(path_spec, new_basename, NULL);

    GEDA_FREE(new_basename);
    GEDA_FREE(path_spec);
  }

  return autosave_name;
}

/*! \brief Return pointer to base file name
 *  \par Function description
 *  Returns a pointer to the characters after the right most
 *  seperator or NULL if no sting was passed. The returned
 *  pointer points to the given string, and not reallocated.
 *
 *  \param [in] path The path to search.
 *  \return offset if found, otherwise NULL.
 */
char *f_get_basename(const char *path)
{
  if (path) {
    char *base = strrchr(path, DIR_SEPARATOR);
    return base ? base+1 : (char*)path;
  }
  return NULL;
}

/*! \brief Get full path of given bitmap file name
 *  \par Function description
 *  Prepends the path to the bitmaps directory to \a filename.
 *  User path specified paths using the bitmap-directory keyword
 *  are given precedence, otherwise the path is the default path,
 *  which is the path returned by f_path_sys_data suffix suffixed
 *  with "bitmaps".
 *
 *  \param [in] filename The file name to prepend the path to.
 *
 *  \return string with file name and path for the specified file.
 */
char *f_get_bitmap_filespec (const char *filename)
{
  char *filespec;

  if (filename) {

    const char *directory;
    const char *seperator;

    /* initialize variables */
    directory = default_bitmap_directory;
    filespec  = NULL;
    seperator = DIR_SEPARATOR_S;

    if (directory) {
      /* default_bitmap_directory was checked by g_rc, we double check here
       * because the directory could have been removed */
      if (g_file_test (directory, G_FILE_TEST_IS_DIR)) {
        filespec = geda_utility_string_concat (directory, seperator, filename, NULL);
      }
      else {
        fprintf (stderr, "Path invalid[%s], %s\n", directory, strerror(errno));
      }
    }

    if (!filespec) {

      const char *base;
      const char *subfolder;

      base      = f_path_sys_data();
      subfolder = "bitmap";
      filespec  = geda_utility_string_concat (base, seperator, subfolder,
                                         seperator, filename, NULL);
    }

    /* Check to see of file is accessible */
    if (filespec) {
      if ((access (filespec, R_OK)) != 0) {
        /* Does not point to accessible file, release string */
        GEDA_FREE(filespec);
        filespec = NULL;
      }
    }
  }
  else {
    return NULL;
  }
  return filespec;
}

/*! \brief Get full path of given Data File
 *
 *  \par Function description
 *  Checks for the existence of a file with the given name in the
 *  current, user data, and system data directories. Testing stops
 *  if a file is found. The full filename is returned or NULL if
 *  a file is not found.
 *
 *  \note \a filename could have a subfolder prefix.
 *
 *  \param [in] filename The data file to find.
 *
 *  \return string with file name and path for the specified file.
 */
char *f_get_data_filespec (const char *filename)
{
  char *filespec;

  if (filename) {

          char *cwd;
    const char *directory;
    const char *seperator;

    /* initialize variables */
    cwd       = getcwd(0,0);
    filespec  = NULL;
    seperator = DIR_SEPARATOR_S;

    /* Look for file in current directory */
    if (!cwd) {
      fprintf (stderr, "System error getting cwd: [%s]\n", strerror(errno));
    }
    else {

      filespec = geda_utility_string_concat (cwd, seperator, filename, NULL);
      free (cwd);

      if ((access (filespec, R_OK)) != 0) {
        /* Does not point to accessible file, release string */
        GEDA_FREE(filespec);
      }
    }

    /* Look for file in user config/data directory */
    if (!filespec) {

      directory = f_path_user_config();
      filespec  = geda_utility_string_concat (directory, seperator, filename, NULL);

      if ((access (filespec, R_OK)) != 0) {
        /* Does not point to accessible file, release string */
        GEDA_FREE(filespec);
      }
    }

    /* Look for file in system data directory */
    if (!filespec) {

      directory = f_path_sys_data();
      filespec  = geda_utility_string_concat (directory, seperator, filename, NULL);

      if ((access (filespec, R_OK)) != 0) {
        /* Does not point to accessible file, release string */
        GEDA_FREE(filespec);
      }
    }
  }
  else {
    return NULL;
  }

  return filespec;
}

/*! \brief Get list of file in Given directory
 *  \par Function Description
 *  This function collect the names of files contained in the
 *  specified path using the optional extension filter. The
 *  list of files is return in a single linked list.
 *
 *  \param [in] path    Path to directory to examine
 *  \param [in] filter  Optional file extension to use as filter
 *
 * \retval Returns GSList of files or NULL if no matching files
*/
GSList *f_get_dir_list_files(char *path, char *filter)
{
        GSList *files = NULL;
  const char   *real_filter;
        DIR    *dirp;

  real_filter = filter;

  if (*real_filter == 0x2E ) real_filter++; /* skip over Period  */

  dirp = opendir (path);

  if (dirp != NULL) {

    struct      dirent *ent;

    /* get all the files within directory */
    while ((ent = readdir (dirp)) != NULL) {

      char   *filename;

      if (real_filter) {

        const char *suffix = f_get_filename_ext(ent->d_name);

        if ( suffix && strcmp (suffix, real_filter) == 0) {
          filename = geda_utility_string_strdup(ent->d_name);
          files = g_slist_prepend(files, filename);
        }
      }
      else {
        filename = geda_utility_string_strdup(ent->d_name);
        files = g_slist_prepend(files, filename);
      }
    }
    closedir (dirp);
  }
  else { /* could not open directory */
    u_log_message(_("%s: error accessing: %s\n"), __func__, path);
  }

  return g_slist_reverse(files);
}

static bool
get_contents_stdio (const char *filename, FILE *f, char **contents,
                    size_t     *length,   GError        **err)
{
  bool   ret_val = FALSE;
  char  *str     = NULL;
  char  *tmp;

  char   buf[DISK_BUFFER_SIZE];

  size_t bytes;
  size_t total_bytes     = 0;
  size_t total_allocated = 0;

  while (!feof (f)) {

    int save_errno;

    bytes = fread (buf, 1, sizeof (buf), f);
    save_errno = errno;

    while ((total_bytes + bytes + 1) > total_allocated) {

      if (str)
        total_allocated *= 2;
      else
        total_allocated = MIN (bytes + 1, sizeof (buf));

      tmp = g_try_realloc (str, total_allocated);

      if (tmp == NULL) {
        g_free (str);
        g_set_error (err, G_FILE_ERROR, ENOMEM,
                   _("Could not allocate %lu bytes to read file \"%s\""),
                    (unsigned long)total_allocated,
                    filename);
        goto error;
      }

      str = tmp;
    }

    if (ferror (f)) {
      g_set_error (err, G_FILE_ERROR, save_errno,
                 _("Error reading file '%s': %s"),
                   filename,
                   strerror(save_errno));
      g_free (str);
      goto error;
    }

    memcpy (str + total_bytes, buf, bytes);

    if (total_bytes + bytes < total_bytes) {
      g_set_error (err, G_FILE_ERROR, ENOMEM,
                   _("File \"%s\" is too large"),
                   filename);
      g_free (str);
      goto error;
    }

    total_bytes += bytes;
  }

  if (total_allocated == 0) {
    str = g_new (char, 1);
    total_bytes = 0;
  }

  str[total_bytes] = '\0';

  if (length) {
    *length = total_bytes;
  }

  *contents = str;

  ret_val = TRUE;

error:

  fclose (f);

  return ret_val;
}

#ifndef OS_WIN32_NATIVE

static bool
get_contents_regfile (const char   *filename,
                      struct stat  *stat_buf,
                      int           fd,
                      char        **contents,
                      unsigned int *length,
                      GError      **err)
{
  bool   ret_val = FALSE;
  char  *buf;

  unsigned long int alloc_size;
  unsigned long int size;

  size = (unsigned long int)stat_buf->st_size;

  alloc_size = size + 1;

  buf = g_try_malloc (alloc_size);

  if (buf == NULL) {

    g_set_error (err, G_FILE_ERROR, ENOMEM,
               _("Could not allocate %ld byte to read file \"%s\""),
                (unsigned long int) alloc_size,
                 filename);
  }
  else {

    unsigned long int bytes_read;

    bytes_read = 0;
    errno      = 0;

    while (bytes_read < size) {

      gssize rc;

      rc = read (fd, buf + bytes_read, size - bytes_read);

      if (rc < 0) {

        if (errno != EINTR) {

          int save_errno = errno;

          g_free (buf); /* This could modify errno */

          g_set_error (err, G_FILE_ERROR, save_errno,
                     _("Failed to read from file '%s': %s"),
                        filename,
                        strerror(save_errno));

                       goto error;
        }
      }
      else if (rc == 0) {
        break;
      }
      else {
        bytes_read += rc;
      }
    }

    buf[bytes_read] = '\0';

    if (length) {
      *length = bytes_read;
    }

    *contents = buf;

     ret_val = TRUE;
  }

error:

  close (fd);

  return ret_val;
}
#endif

bool
f_get_file_contents(const char *filename, char **contents, size_t *length, GError **err)
{
  bool retval = FALSE; /* Assume failure */

  if (filename != NULL && contents != NULL) {

    FILE *file;

    if (length)
      *length = 0;      /* Assume zero bytes will be read */

   *contents = NULL;    /* Null caller's pointer */
    errno    = 0;       /* Ensure error is reset */

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)

    file = fopen (filename, "rb");
    if (file == NULL)

#else

    int fd;

    struct stat stat_buf;

    fd = open (filename, O_RDONLY|O_BINARY);

    if (fd < 0)

#endif

    {
      g_set_error (err, G_FILE_ERROR, errno,
                 _("Failed to open file '%s': %s"), filename, strerror (errno));
    }

#if !defined (OS_WIN32_NATIVE) && !defined(__MINGW32__)

    else if (fstat (fd, &stat_buf) < 0) {

      int save_errno;

      save_errno = errno;

      close (fd);

      g_set_error (err, G_FILE_ERROR, save_errno,
                 _("Failed to get attributes of file '%s': fstat() failed: %s"),
                    filename, strerror (errno));
    }
    else if (stat_buf.st_size > 0 && S_ISREG (stat_buf.st_mode)) {

      fsync(fd);
      unsigned int *len = (unsigned int*)length;
      retval = get_contents_regfile (filename, &stat_buf,
                                     fd, contents, len, err);
    }
    else {

      fsync(fd);
      file = fdopen (fd, "r");

      if (file == NULL) {

        g_set_error (err, G_FILE_ERROR, errno,
                   _("Failed to open file '%s': fdopen() failed: %s"),
                      filename, strerror (errno));
      }

#else
    else {
#endif
      retval = get_contents_stdio (filename, file, contents, length, err);
    }

#if !defined (OS_WIN32_NATIVE) && !defined(__MINGW32__)
  close (fd);
#endif

  }

  return retval;
}

/*! \brief Return pointer filename extension
 *  \par Function description
 *  Returns a pointer to the characters after the period or
 *  NULL if the is no period in the filename.
 *
 *  \param [in] filename The filename to search.
 *
 *  \return offset if found, otherwise NULL.
 */
const char *f_get_filename_ext(const char *filename)
{
    const char *dot = strrchr(filename, '.');
    if(!dot || dot == filename) return NULL;
    return dot + 1;
}

/*! \brief Get the file header string.
 *  \par Function Description
 *  This function returns the PACKAGE_DATE_VERSION and
 *  #FILEFORMAT_VERSION formatted as a gEDA file header.
 *
 *  \remarks <em>Do not</em> free the returned string.
 */
const char *f_get_format_header()
{
  static char *header = NULL;

  if (header == NULL)
    header = geda_utility_string_sprintf("v %s %u\n", PACKAGE_DATE_VERSION,
                             FILEFORMAT_VERSION);

  return header;
}

/*! \brief Get is Path absolute
 *  \par Function description
 *  Determines if the path of the given file name is absolute or relative.
 *
 *  \param [in] filename The filename to interrogate.
 *
 *  \returns TRUE if \a filename is absolute
 */
bool f_get_is_path_absolute (const char *filename)
{
  bool result;

  if (filename == NULL) {
    result = FALSE;
  }
  else {

    if (G_IS_DIR_SEPARATOR (filename[0])) {
      result = TRUE;
    }
    else {
      result = FALSE;
    }

#ifdef OS_WIN32
    /* Recognize drive letter on native Windows */
    if (isalpha (filename[0]) && filename[1] == ':') {
      if (G_IS_DIR_SEPARATOR (filename[2])) {
        result = TRUE;
      }
    }
#endif

  }

  return result;
}