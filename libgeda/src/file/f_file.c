/* -*- C header file: f_file.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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

#include <config.h>

#include <sys/file.h>

#include <stdio.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <sys/param.h>      /* MAXPATHLEN */

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#include "libgeda_priv.h"

/*! /comment: The functions in f_basic.c are mostly application
 *  specific so this file was created for basic non-application
 *  specific routines.
 */

/*! \brief Copy a file
 *  \par Function Description
 *  This function copies the file specified by \a source to \a target
 *  if possible. If and error is encountered, a message is written to
 *  the log and -1 is returned. If successful, the function returns 0.
 *
 *  \param [in]  source   Name of source file to be copied
 *  \param [out] target   Name of target or destination file
 *
 *  \returns 0 on success, -1 on failure.
 */
int f_file_copy(const char *source, const char *target)
{
  int input  = -1;
  int output = -1;
  int status;

  unsigned long int nread;

  char *buffer;
  char *ptr_out;

  const char *err_file = _("File error");
  const char *log_3SQ2 = "%s: \"%s\", %s\n";

#if defined(OS_LINUX)

  const char *err_lock = _("File lock error");
  const char *err_sys  = _("%s: attempting to unlock file \"%s\"\n");

  void unlock_input(int input) {
    /* Unlock the input file */
    if (flock(input, LOCK_UN) == -1) {
      fprintf(stderr, err_sys, err_file, source);
    }
  }

#define unlock_file(fd) unlock_input(fd)

#else

#define unlock_file(fd)

#endif

  int error_exit (const char *msg, int TheError ) {
    u_log_message(log_3SQ2, msg, source, strerror(TheError));
    if (buffer >  0) free(buffer);
    if (input  >= 0) close(input);
    if (output >= 0) close(output);
    errno = TheError;
    return -1;
  }

  status = errno = 0;
  buffer = malloc(DISK_BUFFER_SIZE);

  if(!buffer) {
    fprintf(stderr, _("%s: Memory Allocation Error!\n"), __func__);
    status = -1;
  }
  else if(access(source, R_OK) != 0) { /* Check to see if input is readable */
    u_log_message(log_3SQ2, err_file, source, strerror(errno));
    free(buffer);
    status = -1;
  }
  else {

    input = open(source, O_RDONLY);

    if (input < 0) {
      u_log_message(log_3SQ2, err_file, source, strerror(errno));
      status = -1;
    }
    else {

#if defined(OS_LINUX)

      /* Lock the input file, to prevent processes from writting to
       * file until we are done */
      if (flock(input, LOCK_EX) == -1) {
        return error_exit(err_lock, errno);
      }

#endif

      output = open(target, O_WRONLY | O_CREAT | O_EXCL, 0666);

      if (output < 0) {
        status  = errno;
        unlock_file(input);
        return error_exit(err_file, status);
      };

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
          else if (errno != EINTR) {
            status = errno;
            unlock_file(input);
            return error_exit(err_file, status);
          }
        } while (nread > 0);
      }

      if (nread == 0) {
        if (close(output) < 0) {
          status = errno;
          unlock_file(input);
          output = -1;
          return error_exit(err_file, status);
        }
      }

      unlock_file(input);
      close(input);
      status = errno;
    }
    free(buffer);
  }

  return status; /* Success! */
}

/*! \brief Compare file modification time to a given time
 *  \par Function Description
 *  Compares the file system modification time to the given
 *  \a time and returns the difference in seconds. Note that
 *  the modification is subtracted from the reference time,
 *  so a positive return value means the file is older than
 *  the reference time by the returned number of seconds.
 *
 *  \param [in] filename File to check
 *  \param [in] ref_time Time to compare too
 *
 *  \returns difference in seconds or -1 if an error occurred
 *
 *  example:
 *
 *    time_t now;
 *    int secs = f_file_cmp_mod_time(filename, time(&now));
 *    printf( "%s is %d seconds old\n",filename, secs);
 */
int f_file_cmp_mod_time (const char *filename, time_t ref_time)
{
  int    result;
  struct stat file_stat;

  errno = 0;

  if (stat (filename, &file_stat) != 0) {

    if (errno == ENOENT) {
      /* The file does not exist. */
      result =  -1;
    }
    else {
      fprintf (stderr, "%s: %s\n", __func__, strerror(errno));
      result = -1;
    }
  }
  else {

    result = difftime (ref_time, file_stat.st_mtime);
  }

  return result;
}

/*! \brief Follow symlinks until a real file is found
 *  \par Function Description
 *  Does readlink() recursively until we find a real filename.
 *
 *  \param [in]     filename  The filename to search for.
 *  \param [in,out] err       GError structure for error reporting,
 *                            or NULL to disable error reporting.
 *  \returns The newly-allocated path to real file on success, NULL
 *           otherwise.
 *
 *  \note Originally taken from gedit's source code.
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

  if (strlen (filename) + 1 > MAX_PATH) {
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

    if (!f_get_is_path_absolute(linkname)) {

      char *dirname = NULL;
      char *tmp     = NULL;

      dirname = f_path_get_dirname(followed_filename);
      tmp     = g_build_filename (dirname, linkname, NULL);

      GEDA_FREE (followed_filename);
      GEDA_FREE (dirname);
      GEDA_FREE (linkname);
      followed_filename = tmp;
    }
    else {
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
 * \returns result of remove = zero on success -1 if error
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

/*! \brief Remove Extension from a file name
 *  \par Function Description
 *  This function replaces the first dot/period from the end of a string
 *  and all characters after, with a NULL.
 *
 * \returns non-zero if successful
 *
 * \warning MUST not be const char
*/
bool f_file_remove_extension(char *filename) {

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

  return n;
}
