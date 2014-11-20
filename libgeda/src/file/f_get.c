/* -*- C header file: f_get.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
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
  #include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include "libgeda_priv.h"
#include "version.h"

# if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h> /* for GetFullPathName */
#  include <io.h>
# endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

/*! \brief Get the autosave filename for a file
 *  \par Function description
 *  Returns the expected autosave filename for the \a filename passed.
 *
 *  \warning The result should be freed when no longer needed.
 *
 *  \param [in] filename The filename to create an autosave filename for.
 *  \return A newly allocated string buffer.
 */
char *f_get_autosave_filename (const char *filename)
{
  char       *autosave_name, *path_spec, *new_basename;
  const char *old_basename;

  if (filename == NULL) {
    autosave_name = NULL;
  }
  else {
    old_basename  = f_get_basename(filename);
    path_spec     = g_path_get_dirname(filename);
    new_basename  = g_strdup_printf(AUTOSAVE_BACKUP_FILENAME_STRING, old_basename);
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

/*! \brief Get list of file in Given directory
 *  \par Function Description
 *  This function collect the names of files contain in the
 *  specified path using the optional extension filter. The
 *  list of file is return in a single linked list.
 *
 * \retval Returns GSList of files or NULL if no matching files
*/

GSList *f_get_dir_list_files(char *path, char *filter)
{
        GSList *files = NULL;
        char   *filename;
  const char   *real_filter;
  const char   *suffix;

  DIR        *dirp;
  struct      dirent *ent;

  real_filter = filter;

  if (*real_filter == 0x2E ) real_filter++; /* skip over Period  */

  dirp = opendir (path);
  if (dirp != NULL) {

    /* get all the files within directory */
    while ((ent = readdir (dirp)) != NULL) {
      if (real_filter) {
        suffix = f_get_filename_ext(ent->d_name);
        if ( suffix && strcmp (suffix, real_filter) == 0) {
          filename = u_string_strdup(ent->d_name);
          files = g_slist_prepend(files, filename);
        }
      }
      else {
        filename = u_string_strdup(ent->d_name);
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
  bool   ret_val         = FALSE;

  char   buf[DISK_BUFFER_SIZE];
  char  *str             = NULL;
  char  *tmp;
  int    save_errno;
  size_t bytes;
  size_t total_bytes     = 0;
  size_t total_allocated = 0;

  while (!feof (f))
  {

    bytes = fread (buf, 1, sizeof (buf), f);
    save_errno = errno;

    while ((total_bytes + bytes + 1) > total_allocated)
    {
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
get_contents_regfile (const char  *filename,
                      struct stat *stat_buf,
                      int          fd,
                      char       **contents,
                      size_t      *length,
                      GError      **err)
{
  bool   ret_val = FALSE;
  char  *buf;
  int    save_errno;

  size_t bytes_read;
  size_t size;
  size_t alloc_size;

  size = stat_buf->st_size;
  alloc_size = size + 1;

  buf = g_try_malloc (alloc_size);

  if (buf == NULL) {

    g_set_error (err, G_FILE_ERROR, ENOMEM,
               _("Could not allocate %ld byte to read file \"%s\""),
                (unsigned long) alloc_size,
                 filename);
  }
  else {
    bytes_read = 0;

    while (bytes_read < size)
    {
      gssize rc;

      rc = read (fd, buf + bytes_read, size - bytes_read);

      if (rc < 0) {

        if (errno != EINTR) {
          save_errno = errno;

          g_free (buf);
          g_set_error (err, G_FILE_ERROR, save_errno,
                     _("Failed to read from file '%s': %s"),
                        filename,
                        strerror(save_errno));

                       goto error;
        }
      }
      else if (rc == 0)
        break;
      else
        bytes_read += rc;
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

bool
f_get_file_contents(const char *filename, char **contents, size_t *length, GError **err)
{
  *contents = NULL;
  if (length)
    *length = 0;

  struct stat stat_buf;

  int save_errno;
  bool retval = FALSE;
  FILE *f;

  if ( filename != NULL && contents != NULL) {

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)

    f = g_fopen (filename, "rb");
    if (f == NULL) {

#else

    int fd;
    fd = open (filename, O_RDONLY|O_BINARY);
    if (fd < 0) {

#endif
      g_set_error (err, G_FILE_ERROR, errno,
                 _("Failed to open file '%s': %s"),
                    filename, strerror (errno));
    }

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)
    else {
#endif
    else if (fstat (fd, &stat_buf) < 0) {

      save_errno = errno;

      close (fd);

      g_set_error (err, G_FILE_ERROR, save_errno,
                 _("Failed to get attributes of file '%s': fstat() failed: %s"),
                    filename, strerror (errno));
    }
    else if (stat_buf.st_size > 0 && S_ISREG (stat_buf.st_mode)) {

      fsync(fd);
      retval = get_contents_regfile (filename, &stat_buf,
                                     fd, contents, length, err);
    }
    else {

      fsync(fd);
      f = fdopen (fd, "r");

      if (f == NULL) {

        g_set_error (err, G_FILE_ERROR, errno,
                   _("Failed to open file '%s': fdopen() failed: %s"),
                      filename, strerror (errno));
      }
#endif

      retval = get_contents_stdio (filename, f, contents, length, err);
    }
  }
  return retval;
}

/*! \brief Return pointer filename extension
 *  \par Function description
 *  Returns a pointer to the characters after the period or
 *  NULL if the is no period in the filename.
 *
 *  \param [in] filename The filename to search.
 *  \return offset if found, otherwise NULL.
 */
const char *f_get_filename_ext(const char *filename) {
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
    header = g_strdup_printf("v %s %u\n", PACKAGE_DATE_VERSION,
                             FILEFORMAT_VERSION);

  return header;
}
