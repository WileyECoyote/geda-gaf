/* -*- C header file: f_file.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
/*! \file f_file.c
 *  \brief utilility file functions
 */

/*! TODO: review function s_slib_getbasename. Could other modules use this
 *       function? if so then should relocate to this module */

#include <config.h>

#ifdef HAVE_FCNTL_H
  #include <fcntl.h>
#endif

#include <stdio.h>
#ifdef HAVE_UNISTD_H
  #include <unistd.h>
#endif

#include <sys/param.h>      /* MAXPATHLEN */

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include "libgeda_priv.h"

/*! /comment: The functions in f_basic.c are (mostly) application
 *  specific so this file was created for basic non-application
 *  specific routines.
 */

int f_file_copy(const char *source, const char *target)
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

/*! \brief Follow symlinks until a real file is found
 *  \par Function Description
 *  Does readlink() recursively until we find a real filename.
 *
 *  \param [in]     filename  The filename to search for.
 *  \param [in,out] err       GError structure for error reporting,
 *                            or NULL to disable error reporting.
 *  \return The newly-allocated path to real file on success, NULL
 *          otherwise.
 *
 *  \note Originally taken from gedit's source code.
 *  TODO: Get rid of MAXPATHLEN
 */
char *f_file_follow_symlinks (const char *filename, GError **err)
{
  char *followed_filename = NULL;
  int link_count = 0;
  GError *tmp_err = NULL;

  if (filename == NULL) {
    g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_INVAL,
                 "%s", strerror (EINVAL));
    return NULL;
  }

  if (strlen (filename) + 1 > MAXPATHLEN) {
    g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_NAMETOOLONG,
                 "%s", strerror (ENAMETOOLONG));
    return NULL;
  }

  followed_filename = g_strdup (filename);

#ifdef __MINGW32__
  /* MinGW does not have symlinks */
  return followed_filename;
#else

  while (link_count < MAX_LINK_LEVEL) {
    struct stat st;
    char *linkname = NULL;

    if (lstat (followed_filename, &st) != 0) {
      /* We could not access the file, so perhaps it does not
       * exist.  Return this as a real name so that we can
       * attempt to create the file.
       */
      return followed_filename;
    }

    if (!S_ISLNK (st.st_mode)) {
      /* It's not a link, so we've found what we're looking for! */
      return followed_filename;
    }

    link_count++;

    linkname = g_file_read_link (followed_filename, &tmp_err);

    if (linkname == NULL) {
      g_propagate_error(err, tmp_err);
      GEDA_FREE (followed_filename);
      return NULL;
    }

    /* If the linkname is not an absolute path name, append
     * it to the directory name of the followed filename.  E.g.
     * we may have /foo/bar/baz.lnk -> eek.txt, which really
     * is /foo/bar/eek.txt.
     */

    if (!g_path_is_absolute(linkname)) {
      char *dirname = NULL;
      char *tmp = NULL;

      dirname = g_path_get_dirname(followed_filename);

      tmp = g_build_filename (dirname, linkname, NULL);
      GEDA_FREE (followed_filename);
      GEDA_FREE (dirname);
      GEDA_FREE (linkname);
      followed_filename = tmp;
    } else {
      GEDA_FREE (followed_filename);
      followed_filename = linkname;
    }
  }

  /* Too many symlinks */
  g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_LOOP,
               _("%s: %s"), strerror (EMLINK), followed_filename);
  GEDA_FREE (followed_filename);
  return NULL;

#endif /* __MINGW32__ */
}

/*! \brief Remove File
 *  \par Function Description
 *  This function calls the standard remove function after setting
 *  the system error number to 0.
 *
 * \retval Returns result of remove = zero on success -1 if error
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

/* warning: MUST not be const char */
void f_file_remove_extension(char *filename) {

  int i   = 0;
  int n   = 0;
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
