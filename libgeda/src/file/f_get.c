/* -*- C header file: f_get.c indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * Copyright (C) 2013-2017 Wiley Edward Hill
 * Copyright (C) 2013-2017 gEDA Contributors (see ChangeLog for details)
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

#include "../../../config.h"

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

#ifdef OS_WIN32
#include <ctype.h>  /* for isalpha */
#endif

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h> /* for GetFullPathName */
#  include <io.h>
#endif

#include <libgeda_priv.h>
#include <version.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

/*!
 * \brief Get the autosave filename for a file
 * \par Function description
 *  Returns the expected autosave filename for the \a filename passed.
 *
 * \remarks Returned allocation should be freed when no longer needed.
 *
 * \param [in] filename The filename to create an autosave filename.
 *
 * \return A newly allocated string buffer.
 */
char *geda_file_get_autosave_filename (const char *filename)
{
  char *autosave_name;

  if (filename == NULL) {
    autosave_name = NULL;
  }
  else {

   const char *old_basename;
         char *new_basename;
         char *path_spec;

    old_basename  = geda_file_get_basename(filename);
    path_spec     = geda_get_dirname(filename);
    new_basename  = geda_sprintf(AUTOSAVE_BACKUP_FILENAME_STRING, old_basename);
    autosave_name = g_build_filename(path_spec, new_basename, NULL);

    GEDA_FREE(new_basename);
    GEDA_FREE(path_spec);
  }

  return autosave_name;
}

/*!
 * \brief Return pointer to base file name
 * \par Function description
 *  Returns a pointer to the characters after the right most
 *  seperator or NULL if no sting was passed. The returned
 *  pointer points to the given string, and not reallocated.
 *
 * \param [in] path The path to search.
 * \return offset if found, otherwise NULL.
 */
const char *geda_file_get_basename(const char *path)
{
  if (path) {

    const char *base;

#if defined (OS_WIN32_NATIVE)

    int i, len;

    base = NULL;
    len  = strlen(path);

    for (i = len; i > 0; i--) {

      if (path[i] == '/') {
        base = &path[i];
        break;
      }
      else if (path[i] == DIR_SEPARATOR) {
        base = &path[i];
        break;
      }
    }

#else

    base = strrchr(path, DIR_SEPARATOR);

#endif

    return base ? base + 1 : path;
  }
  return NULL;
}

/*!
 * \brief Return copy base file name
 * \par Function description
 *  Convenience function to combine geda_file_get_basename and
 *  geda_strdup.
 *
 * \param [in] path The path to search.
 * \return offset if found, otherwise NULL.
 */
char *geda_file_get_basename_dup(const char *path)
{
  const char *base = geda_file_get_basename(path);

  if (base) {
    return geda_strdup(base);
  }
  return NULL;
}

/*!
 * \brief Get full path of given bitmap file name
 * \par Function description
 *  Prepends the path to the bitmaps directory to \a filename.
 *  User path specified paths using the bitmap-directory keyword
 *  are given precedence, otherwise the path is the default path,
 *  which is the path returned by geda_sys_data_path suffix suffixed
 *  with "bitmaps".
 *
 * \param [in] filename The file name to prepend the path to.
 *
 * \return string with file name and path for the specified file.
 */
char *geda_file_get_bitmap_filespec (const char *filename)
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
        filespec = geda_strconcat (directory, seperator, filename, NULL);
      }
      else {
        const char *msg = _("Invalid path");
        fprintf (stderr, "%s '%s', %s\n", msg, directory, strerror(errno));
      }
    }

    if (!filespec) {

      const char *base;
      const char *subfolder;

      base      = geda_sys_data_path();
      subfolder = "bitmaps";
      filespec  = geda_strconcat (base, seperator, subfolder,
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

static bool get_contents_stdio (const char *filename, FILE *f, char **contents,
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

    bytes = fread (buf, 1, sizeof(buf), f);
    save_errno = errno;

    while ((total_bytes + bytes + 1) > total_allocated) {

      if (str)
        total_allocated *= 2;
      else
        total_allocated = MIN (bytes + 1, sizeof(buf));

      tmp = g_try_realloc (str, total_allocated);

      if (tmp == NULL) {
        g_free (str);
        const char *msg1 = _("Could not allocate");
        const char *msg2 = _("bytes to read file");
        g_set_error (err, EDA_ERROR, ENOMEM, "%s %lu %s '%s'",
                     msg1, (unsigned long)total_allocated, msg2, filename);
        goto error;
      }

      str = tmp;
    }

    if (ferror (f)) {
      g_set_error (err, EDA_ERROR, save_errno, "%s '%s': %s",
                 _("Error reading file"),
                   filename,
                   strerror(save_errno));
      g_free (str);
      goto error;
    }

    memcpy (str + total_bytes, buf, bytes);

    if (total_bytes + bytes < total_bytes) {
      g_set_error (err, EDA_ERROR, ENOMEM,
                   _("File '%s' is too large"), filename);
      g_free (str);
      goto error;
    }

    total_bytes += bytes;
  }

  if (total_allocated == 0) {
    str = g_malloc (sizeof(char));
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

static bool get_contents_regfile (const char   *filename,
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
    const char *msg1 = _("Could not allocate");
    const char *msg2 = _("bytes to read file");
    g_set_error (err, EDA_ERROR, ENOMEM, "%s %ld %s '%s'",
                 msg1, (unsigned long int)alloc_size, msg2, filename);
  }
  else {

    unsigned long int bytes_read;

    bytes_read = 0;
    errno      = 0;

    while (bytes_read < size) {

      ssize_t rc;

      rc = read (fd, buf + bytes_read, size - bytes_read);

      if (rc < 0) {

        if (errno != EINTR) {

          g_set_error (err, EDA_ERROR, errno, "%s '%s': %s",
                     _("Failed to read from file"),
                        filename, strerror(errno));
          g_free (buf); /* This could modify errno */
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

/*!
 * \brief Get the contents of a file
 * \par Function description
 *  Returns the entire contents of the file with name \a filename.
 *  If successful, the address of the dynamically allocated buffer
 *  is saved to \a contents and the size of the buffer is written
 *  to \a length.
 *
 * \param [in]  filename The filename of the file whose contents are to be retrieved.
 * \param [out] contents Pointer to location receive the address if successful.
 * \param [out] length   Size of the buffer if successful.
 * \param [out] err      Pointer to location of error record if not successful.
 *
 * \retval TRUE on success, FALSE if an error occured.
 */
bool geda_file_get_contents(const char  *filename,
                                  char **contents,
                                size_t  *length,
                                GError **err)
{
  bool retval = FALSE; /* Assume failure */

  if (filename != NULL && contents != NULL) {

    FILE *file;

    if (length) {
      *length = 0;      /* Assume zero bytes will be read */
    }

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
      g_set_error (err, EDA_ERROR, errno, "%s '%s': %s",
                 _("Failed to open file"), filename, strerror (errno));
    }

#if !defined (OS_WIN32_NATIVE) && !defined(__MINGW32__)

    else if (fstat (fd, &stat_buf) < 0) {

      const char *msg1 = _("Failed to get attributes of file");
      const char *msg2 = _("fstat failure");

      g_set_error (err, EDA_ERROR, errno, "%s '%s': %s %s",
                   msg1, filename, msg2, strerror (errno));
      close (fd);
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
        const char *msg1 = _("Failed to open file");
        const char *msg2 = _("fdopen failure");
        g_set_error (err, EDA_ERROR, errno, "%s '%s': %s %s",
                     msg1, filename, msg2, strerror (errno));
      }

#endif
    else {

        retval = get_contents_stdio (filename, file, contents, length, err);

#if !defined (OS_WIN32_NATIVE) && !defined(__MINGW32__)
        close (fd);
      }
#endif

    }
  }
  else if (err) {

    const char *msg;

    if (filename == NULL) {
      msg = _("ERROR: pointer to filename is NULL");
    }
    else {
      msg = _("ERROR: pointer to a buffer is NULL");
    }
    g_set_error (err, EDA_ERROR, EDA_ERROR_NULL_POINTER,
                 "libgeda <%s> %s.\n", msg, __func__);
  }

  return retval;
}

/*!
 * \brief Get full path of given Data File
 * \par Function description
 *  Checks for the existence of a file with the given name in the
 *  current, user data, and system data directories. Testing stops
 *  if a file is found. The full filename is returned or NULL if
 *  a file is not found.
 *
 * \note \a filename could have a subfolder prefix.
 *
 * \param [in] filename The data file to find.
 *
 * \return string with file name and path for the specified file.
 */
char *geda_file_get_data_filespec (const char *filename)
{
  char *filespec;

  if (filename) {

          char *cwd;
    const char *directory;

    /* initialize variables */
    cwd       = getcwd(0,0);
    filespec  = NULL;

    /* Look for file in current directory */
    if (!cwd) {
      perror (_("System error reading the current directory"));
    }
    else {

      filespec = g_build_filename (cwd, filename, NULL);
      free (cwd);

      if ((access (filespec, R_OK)) != 0) {
        /* Does not point to accessible file, release string */
        GEDA_FREE(filespec);
      }
    }

    /* Look for file in user config/data directory */
    if (!filespec) {

      directory = geda_user_config_path();
      filespec  = g_build_filename (directory, filename, NULL);

      if ((access (filespec, R_OK)) != 0) {
        /* Does not point to accessible file, release string */
        GEDA_FREE(filespec);
      }
    }

    /* Look for file in system data directory */
    if (!filespec) {

      directory = geda_sys_data_path();
      filespec  = g_build_filename (directory, filename, NULL);

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

/*!
 * \brief Get list of file in Given directory
 * \par Function Description
 *  This function collect the names of files contained in the
 *  specified path using the optional extension filter. The
 *  list of files is return in a single linked list.
 *
 * \param [in] path    Path to directory to examine
 * \param [in] filter  Optional file extension to use as filter
 * \param [in] err     Optional location of error record if not successful
 *
 * \returns GSList of files or NULL if no matching files
 */
GSList *geda_file_get_dir_list_files(char *path, char *filter, GError **err)
{
  GSList *files = NULL;

  if (!path) {

    if (err) {

      const char *msg = _("ERROR: pointer to path is NULL");

      g_set_error (err, EDA_ERROR, EDA_ERROR_NULL_POINTER,
                   "libgeda <%s> %s.\n",__func__,  msg);
    }
  }
  else {

    const char *real_filter;
    DIR        *dirp;

    real_filter = filter;

    if (real_filter && *real_filter == 0x2E ) {
      real_filter++; /* skip over Period  */
    }

    dirp = opendir (path);

    if (dirp != NULL) {

      struct dirent *ent;

      /* get all the files within directory */
      while ((ent = readdir (dirp)) != NULL) {

        char   *filename;

        if (real_filter) {

          const char *suffix = geda_file_get_filename_ext(ent->d_name);

          if (suffix && strcmp (suffix, real_filter) == 0) {
            filename = geda_strdup(ent->d_name);
            files = g_slist_prepend(files, filename);
          }
        }
        else {
          filename = geda_strdup(ent->d_name);
          files = g_slist_prepend(files, filename);
        }
      }
      closedir (dirp);
    }
    else { /* could not open directory */

      if (err) {

        const char *msg = _("error accessing");

        g_set_error (err, EDA_ERROR, errno, "%s '%s': %s\n",
                     msg, path, strerror (errno));
      }
    }
  }

  if (files) {
    return g_slist_reverse(files);
  }
  return NULL;
}

/*!
 * \brief Return pointer filename extension
 * \par Function description
 *  Returns a pointer to the characters after the period or
 *  NULL if the is no period in the filename.
 *
 * \param [in] filename The filename to search.
 *
 * \return offset if found, otherwise NULL.
 */
const char *geda_file_get_filename_ext(const char *filename)
{
  if (filename && *filename) {
    const char *dot = strrchr(filename, '.');
    if (!dot || dot == filename) return NULL;
    if (IS_DIR_SEPARATOR(*(dot-1))) return NULL;
    return dot + 1;
  }
  return NULL;
}

/*!
 * \brief Get the file header string.
 * \par Function Description
 *  This function returns the PACKAGE_DATE_VERSION and
 *  #FILEFORMAT_VERSION formatted as a gEDA file header.
 *
 * \remarks <em>Do not</em> free the returned string.
 */
const char *geda_file_get_format_header(void)
{
  static char *header = NULL;

  if (header == NULL) {
    header = geda_sprintf("v %s %u\n", PACKAGE_DATE_VERSION,
                                       FILEFORMAT_VERSION);
  }
  return header;
}

/*!
 * \brief Get the name of a file without the path or extension
 * \par Function description
 *  Returns the base file without the path or the extension as
 *  a newly allocated string.
 *
 * \param [in] filespec The filename to interrogate, with or without a path.
 *
 * \retval name of file without an extension
 */
char *geda_file_get_name (const char *filespec)
{
  char *fname;

  fname = geda_file_get_basename_dup(filespec);

  if (fname) {

    const char *dot = strrchr(fname, '.');

    if (dot) {

      char *tmp;
      int   len;

      len = (strlen(fname) - (strlen(dot)));
      tmp = geda_strndup(fname, len);

      GEDA_FREE(fname);

      fname = tmp;
    }
  }
  else {
    return NULL;
  }
  return fname;
}

/*!
 * \brief Get is Path absolute
 * \par Function description
 *  Determines if the path of the given file name is absolute or relative.
 *
 * \param [in] filename The filename to interrogate.
 *
 * \retval TRUE if \a filename is absolute
 */
bool geda_file_get_is_path_absolute (const char *filename)
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
