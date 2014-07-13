/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file f_utilities.c
 *  \brief utilility file functions
 */

/*! TODO: review function s_slib_getbasename could other modules use this
 *       function, if so then should relocate to this module */
#include <config.h>
#if defined(_LINUX)
 #include <sys/sendfile>
#endif

#ifdef HAVE_FCNTL_H
  #include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
  #include <unistd.h>
#endif
#include <malloc.h>
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

#include <glib.h>

#include <errno.h>
extern int errno;

#include <defines.h>
#include "libgeda_priv.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*! /comment: The functions in f_basic.c are (mostly) application
 *  specific so this file was created for basic non-application
 *  specific routines.
 */

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

const char *geda_basename(const char *path)
{
  if (path) {
    char *base = strrchr(path, DIR_SEPARATOR);
    return base ? base+1 : path;
  }
  return NULL;
}

/* warning: MUST not be const char */
void f_remove_extension(char *filename) {

  int i = 0;
  int n = 0;
  int len = 0;

  while(filename[len] != '\0') { len++; } /* get length of filename */

  for(i = len; i > -1; i--) {             /* look for extension working backwards */
     if(filename[i] == '.') {
       n = i;                             /* char # of exension */
       break;
    }
  }
  if (n > 0)
    for(i = n; i < len; i++) {            /* starting with the '.'  */
      filename[i] = '\0';
    }
}



int f_copy(const char *source, const char *target)
{
  int input = 0;
  int output = 0;

  size_t nread;

  char *buffer;
  char *ptr_out;

  int error_exit( int TheError ) {
    u_log_message(_("File error: \"%s\", %s\n"), source, strerror(TheError));
    if (buffer > 0) free(buffer);
    if (input >= 0) close(input);
    if (output >= 0) close(output);
    errno = TheError;
    return -1;
  }

  buffer = malloc(DISK_BUFFER_SIZE);

  if(!buffer) {
    u_log_message(_("File error(strerror): Memory Allocation Error!\n"));
    return -1;
  }

  /* Check to see if inpit is readable */
  if(access(source, R_OK) != 0) {
    u_log_message(_("File error(strerror[source]): \"%s\", %s\n"), source, strerror( errno ));
    return -1;
  }

  input = open(source, O_RDONLY);
  if (input < 0) {
    u_log_message(_("File error(strerror)[source]: \"%s\", %s\n"), source, strerror( errno ));
    return -1;
  }

#if defined(_LINUX)
  if (input > 0)
    if (lockf(input, F_LOCK, 0) == -1) {
      return -1; /* FAILURE */
      u_log_message(_("File lock error(strerror): \"%s\", %s\n"), source, strerror( errno ));
    }
    /* else he input is locked */
  else {
    u_log_message(_("File lock error(strerror): \"%s\", %s\n"), source, strerror( errno ));
    return -1; /* FAILURE */
  }
#endif

  output = open(target, O_WRONLY | O_CREAT | O_EXCL, 0666);

  if (output < 0) return error_exit(errno);

  while (nread = read(input, buffer, DISK_BUFFER_SIZE), nread > 0)
  {
    ptr_out = buffer;
    size_t nwritten;

    do {
          nwritten = write(output, ptr_out, nread);

          if (nwritten >= 0) {
            nread -= nwritten;
            ptr_out += nwritten;
          }
          else if (errno != EINTR)
          {
            return error_exit(errno);
          }
    } while (nread > 0);
  }

  if (nread == 0) {
    if (close(output) < 0) {
      output = -1;
      return error_exit(errno);
    }
    close(input);
  }

#if defined(_LINUX)
  /* Sanity-Check for Lock */
  if (lockf(input, T_LOCK, 0) == -1 ) { /* if this is locked -1 is returned! */
    lockf(input, F_ULOCK, 0);
  }
  else
    u_log_message("File system Error: attempting to lock/unlock file[%s]\n",source);
#endif
  free(buffer);
  return 0; /* Success! */

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
  const char   *suffix;

  DIR        *dirp;
  struct      dirent *ent;

  dirp = opendir (path);
  if (dirp != NULL) {

    /* get all the files within directory */
    while ((ent = readdir (dirp)) != NULL) {
      if (filter) {
        suffix = f_get_filename_ext(ent->d_name);
        if ( suffix && strcmp (suffix, filter) == 0) {
          filename = geda_strdup(ent->d_name);
          files = g_slist_append(files, filename);
        }
      }
      else {
        filename = geda_strdup(ent->d_name);
        files = g_slist_append(files, filename);
      }
    }
    closedir (dirp);
  }
  else { /* could not open directory */
    u_log_message(_("%s: error accessing: %s\n"), __func__, path);
  }

  return files;
}

/*! \brief Remove File
 *  \par Function Description
 *  This function calls the standard remove function after setting
 *  the system error number to 0.
 *
 * \retval Returns result of remove function
*/
int f_file_remove (const char *pathname)
{
  int result;

  errno = 0;

  if (pathname != NULL) {
    result = remove(pathname);
  }
  else {
    result = -1;
  }
  return result;
}