/* -*- C header file: f_sys.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
/*! \file f_sys.c
 *  \brief file utilility functions
 */

#include "../../../config.h"

#include <sys/file.h>

#include <stdio.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

//#include <sys/param.h>      /* MAXPATHLEN */

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#if defined (OS_WIN32_NATIVE) || defined(__MINGW32__)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h> /* for GetFullPathName */
#endif

#include <libgeda_priv.h>
#include <geda/geda_stat.h>

/** \defgroup libgeda-sys-file-utililities Libgeda File Utilities
 *  @{ \par
 *  Functions in f_file.c are mostly application specific so this
 *  module was created for basic non-application specific routines.
*/

/*!
 * \brief Copy a file
 * \par Function Description
 *  This function copies the file specified by \a source to \a target
 *  if possible. If and error is encountered, a message is written to
 *  the log and -1 is returned. If successful, the function returns 0.
 *
 * \param [in]  source   Name of source file to be copied
 * \param [out] target   Name of target or destination file
 *
 * \returns 0 on success, -1 on failure.
 */
int geda_file_copy(const char *source, const char *target)
{
  int input  = -1;
  int output = -1;
  int status;

  char *buffer;
  char *ptr_out;

  const char *err_file = _("File error");
  const char *log_3SQ2 = "%s: \"%s\", %s\n";

#if defined(OS_LINUX)

  const char *err_lock = _("File lock error");

  void unlock_input(int input) {
    /* Unlock the input file */
    if (flock(input, LOCK_UN) == -1) {

      const char *err_sys  = _("attempting to unlock file");

      geda_utility_log_system("%s: %s \"%s\"\n", err_file, err_sys, source);
    }
  }

#define unlock_file(fd) unlock_input(fd)

#else

#define unlock_file(fd)

#endif

  int inline error_exit (const char *msg, int TheError ) {
    u_log_message(log_3SQ2, msg, source, strerror(TheError));
    if (input  >= 0) close(input);
    if (output >= 0) close(output);
    errno = TheError;
    return -1;
  }

  status = errno = 0;
  buffer = malloc(DISK_BUFFER_SIZE);

  if (!buffer) {
    geda_utility_log_system(_("%s: Memory Allocation Error!\n"), __func__);
    status = -1;
  }
  else if (access(source, R_OK) != 0) { /* Check to see if input is readable */
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

        unsigned long int nread;

#if defined(OS_LINUX)

      /* Lock the input file, to prevent processes from writting to
       * file until we are done */
      if (flock(input, LOCK_EX) == -1) {
        free(buffer);
        return error_exit(err_lock, errno);
      }

#endif

#if defined (OS_WIN32_NATIVE)

       char *new_target = geda_strdup (target);

       geda_utility_string_strstr_rep (new_target, "/", DIR_SEPARATOR_S);

       output = open (target, O_WRONLY | O_CREAT | O_EXCL, 0666);

       GEDA_FREE (new_target);

#else

      output = open(target, O_WRONLY | O_CREAT | O_EXCL, 0666);

#endif

      if (output < 0) {
        status  = errno;
        unlock_file(input);
        free(buffer);
        return error_exit(err_file, status);
      };

      while (nread = read(input, buffer, DISK_BUFFER_SIZE), nread > 0)
      {
        ptr_out = buffer;

        do {

          size_t nwritten;

          nwritten = write(output, ptr_out, nread);

          if (nwritten) {
            nread -= nwritten;
            ptr_out += nwritten;
          }
          else if (errno != EINTR) {
            status = errno;
            unlock_file(input);
            free(buffer);
            return error_exit(err_file, status);
          }
        } while (nread > 0);
      }

      if (nread == 0) {
        if (close(output) < 0) {
          status = errno;
          unlock_file(input);
          output = -1;
          free(buffer);
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int geda_file_sys_ckmod(const char *path, mode_t mode)
{
  int status;

  struct stat stat_buf;

  errno = NO_ERROR;

  if (stat(path, &stat_buf) == 0) {

    unsigned int p_user,  m_user;
    unsigned int p_group, m_group;
    unsigned int p_other, m_other;

    status = NO_ERROR;

    p_user  = (stat_buf.st_mode & S_IRWXU) >> 6;
    p_group = (stat_buf.st_mode & S_IRWXG) >> 3;
    p_other =  stat_buf.st_mode & S_IRWXO;

    m_user  = (mode & S_IRWXU) >> 6;
    m_group = (mode & S_IRWXG) >> 3;
    m_other =  mode & S_IRWXO;

    if (p_user < m_user || p_group < m_group || p_other < m_other) {
      errno = EPERM;
      status = -1;
    }
  }
  else {
    status = -1;
  }

  return status;
}

/*!
 * \brief Compare file modification time to a given time
 * \par Function Description
 *  Compares the file system modification time to the given
 *  \a time and returns the difference in seconds. Note that
 *  the modification is subtracted from the reference time,
 *  so a positive return value means the file is older than
 *  the reference time by the returned number of seconds.
 *
 * \param [in] filename File to check
 * \param [in] ref_time Time to compare too
 *
 * \returns difference in seconds or -1 if an error occurred
 *
 *  example:
 *
 *    time_t now;
 *    int secs = geda_file_sys_cmp_mod_time(filename, time(&now));
 *    printf( "%s is %d seconds old\n",filename, secs);
 */
int geda_file_sys_cmp_mod_time (const char *filename, time_t ref_time)
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
      geda_utility_log_system ("%s: %s\n", __func__, strerror(errno));
      result = -1;
    }
  }
  else {

    result = difftime (ref_time, file_stat.st_mtime);
  }

  return result;
}

/*!
 * \brief Follow symlinks until a real file is found
 * \par Function Description
 *  Attempts to resolve \a filename to the real name of the file but does
 *  not guarantee the existence of the file. In other words, if a string
 *  is returned, the string will be the name of a file, or the target of
 *  one or more symbolic links. If \a filename is a real file then string
 *  is a copy of \a filename. In all cases, returned pointers should be
 *  freed when no longer needed.
 *
 * \param [in]     filename  The filename to search for.
 * \param [in,out] err       GError structure for error reporting,
 *                           or NULL to disable error reporting.
 *
 * \returns The newly-allocated path to real file on success, NULL
 *          otherwise.
 *
 * \note Originally taken from gedit's source code.
 */
char *geda_file_sys_follow_symlinks (const char *filename, GError **err)
{
  char *followed_filename;

  if (filename == NULL || *filename == 0) {
    g_set_error (err, EDA_ERROR, EINVAL, "%s", strerror (EINVAL));
    return NULL;
  }

  if (strlen (filename) + 1 > MAX_PATH) {
    g_set_error (err, EDA_ERROR, ENAMETOOLONG, "%s", strerror (ENAMETOOLONG));
    return NULL;
  }

  followed_filename = g_strdup (filename);

#ifdef __MINGW32__

  /* MinGW does not have symlinks */
  return followed_filename;

#else

  int link_count = 0;

  while (link_count < MAX_LINK_LEVEL) {

    char   *linkname;
    GError *tmp_err;
    struct  stat st;

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

    tmp_err  = NULL;
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

    if (!geda_file_get_is_path_absolute(linkname)) {

      char *dirname = NULL;
      char *tmp     = NULL;

      dirname = geda_get_dirname(followed_filename);
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
  g_set_error (err, EDA_ERROR, EDA_ERROR_LOOP, "%s: %s",
               strerror (EMLINK), followed_filename);
  GEDA_FREE (followed_filename);
  return NULL;

#endif /* __MINGW32__ */

}

/*!
 * \brief Builds an absolute pathname.
 * \par Function Description
 *  This function derives an absolute pathname for the pathname
 *  pointed to by \a name with '.' and '..' resolved. It does not
 *  resolve symbolic links.
 *
 *  It returns NULL and sets the \a error (if not NULL) if it failed
 *  to build the pathname or the pathname does not exists.
 *
 * \note
 *  The code for this function is derived from the realpath() of
 *  the GNU C Library (Copyright (C) 1996-2002, 2004, 2005, 2006 Free
 *  Software Foundation, Inc. / LGPL 2.1 or later).
 *
 *  The part for the resolution of symbolic links has been discarded
 *  and it has been adapted for glib and for use on Windows.
 *
 * \param [in]     name  A character string containing the pathname
 *                       to resolve.
 * \param [in,out] error Return location for a GError, or NULL.
 *
 * \return A newly-allocated string with the resolved absolute
 *         pathname on success, NULL otherwise.
 */
char *geda_file_sys_normalize_name (const char *name, GError **error)
{
  char *result;

  if (name == NULL) {
    g_set_error (error, EDA_ERROR, EINVAL, "%s", strerror (EINVAL));
    return NULL;
  }

  if (*name == '\0') {
    g_set_error (error, EDA_ERROR, ENOENT, "%s", strerror (ENOENT));
    return NULL;
  }

#if defined (OS_WIN32_NATIVE)

  /* Windows method (modified) from libiberty's lrealpath.c, GPL V2+
   *
   * We assume we don't have symlinks and just canonicalize to a
   * Windows absolute path.  GetFullPathName converts ../ and ./ in
   * relative paths to absolute paths, filling in current drive if
   * one is not given or using the current directory of a specified
   * drive (eg, "E:foo"). It also converts all forward slashes to
   * back slashes.
   */

  char buf[MAX_PATH];

  DWORD len = GetFullPathName (name, MAX_PATH, buf, NULL);

  if (len == 0 || len > MAX_PATH - 1) {
    result = g_strdup (name);
  }
  else {
    /* The file system is case-preserving but case-insensitive,
     * canonicalize to lowercase, using the codepage associated
     * with the process locale.  */
    //CharLowerBuff (buf, len);
    result = g_strdup (buf);
  }

  /* Test that the file actually exists, and fail if it does not.
   * This is consistent with behaviour on POSIX platforms. */
  if (!g_file_test (result, G_FILE_TEST_EXISTS)) {
    g_set_error (error, EDA_ERROR, ENOENT, "%s", strerror (ENOENT));
    GEDA_FREE (result);
    return NULL;
  }

#else

  if (!g_file_test(name, G_FILE_TEST_EXISTS)) {

    g_set_error (error, EDA_ERROR, ENOENT, "%s", strerror (ENOENT));
    return NULL;
  }

  result = realpath(name, NULL);

#endif

  return result;
}

/*!
 * \brief Remove File
 * \par Function Description
 *  This function calls the standard remove function after setting
 *  the system error number to 0.
 *
 * \returns result of remove = zero on success -1 if error
*/
int geda_file_sys_remove (const char *pathname)
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

/*!
 * \brief Remove Extension from a file name
 * \par Function Description
 *  This function replaces the first dot/period from the end of a string
 *  and all characters after, with a NULL.
 *
 * \returns non-zero if successful
 *
 * \warning MUST not be const char
*/
bool geda_file_sys_remove_extension(char *filename) {

  int i   = 0;
  int n   = 0;
  int len = 0;

  while (filename[len] != '\0') {      /* get length of filename */
    len++;
  }

  for (i = len; i > -1; i--) {         /* look for extension working backwards */
     if (filename[i] == '.') {
       n = i;                          /* char # of exension */
       break;
    }
  }

  if (n > 0) {
    for (i = n; i < len; i++) {        /* starting with the '.'  */
      filename[i] = '\0';
    }
  }

  return n;
}

/** @} endgroup libgeda-sys-file-utililities */