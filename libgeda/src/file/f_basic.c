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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/*! \file f_basic.c
 *  \brief file related functions
 */

#include <config.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include "libgeda_priv.h"

/*! \brief Check if a file has an active autosave file
 *  \par Function Description
 *  Checks whether an autosave file exists for the \a filename passed
 *  which has a modification time newer than the file itself.  If the
 *  check fails, sets \a err with the reason.  N.b. if the autosave
 *  file exists but it was not possible to get its modification time,
 *  returns TRUE.
 *
 *  \param [in]  filename File to check
 *  \param [out] err      GError structure for error reporting, or
 *                        NULL to disable error reporting
 *
 *  \returns TRUE if autosave active, FALSE otherwise
 */
bool f_has_active_autosave (const char *filename, GError **err)
{
  GFileError g_errcode = 0;
  bool  result         = FALSE;
  int   file_err       = 0;
  int   save_errno     = 0;
  char *auto_filename;

  struct stat file_stat, auto_stat;

  auto_filename = f_get_autosave_filename (filename);

  if (stat (filename, &file_stat) != 0) {
    file_err = errno;
  }

  if (stat (auto_filename, &auto_stat) != 0) {
    save_errno = errno;
  }

  if (save_errno == ENOENT) {
    /* The autosave file does not exist. */
    result = FALSE;
  }
  else {

    if (save_errno) {
      g_set_error (err, G_FILE_ERROR, save_errno,
                   _("Failed to stat [%s]: %s"),
                   auto_filename, strerror (save_errno));
                   result = TRUE;
    }
    else {

      if (file_err == ENOENT) {
        /* The autosave file exists, but the actual file does not. */
        result = TRUE;
      }
      else {
        if (file_err) {
          g_errcode = g_file_error_from_errno (file_err);
          g_set_error (err, G_FILE_ERROR, g_errcode,
                       _("Failed to stat [%s]: %s"),
                       auto_filename, strerror (file_err));
                       result = TRUE;
        }
        else {

          /* If we got this far, both files exist and we have
           * managed to get their stat info. */
          if (difftime (file_stat.st_mtime, auto_stat.st_mtime) < 0) {
            result = TRUE;
          }
        }
      }
    }
  }

  GEDA_FREE (auto_filename);
  return result;
}

/*! \brief Opens the schematic file
 *  \par Function Description
 *  Opens the schematic file and carries out a number of actions depending
 *  on the GedaToplevel::open_flags.  If F_OPEN_RC bit is set in open_flags,
 *  executes RC files found in the target directory. If F_OPEN_CHECK_BACKUP
 *  is set, warns user if a backup is found for the file being loaded, and
 *  possibly prompts user for whether to load the backup instead.
 *  If F_OPEN_RESTORE_CWD is set, does not change the working directory to
 *  that of the file being loaded.
 *
 *  \param [in,out] toplevel  The GedaToplevel object to load the schematic into.
 *  \param [in]     page      A Page object to be associated with the file
 *  \param [in]     filename  A character string containing the file name
 *                            to open.
 *  \param [in,out] err       GError structure for error reporting, or
 *                            NULL to disable error reporting
 *
 *  \return 0 on failure, 1 on success.
 */
int
f_open(GedaToplevel *toplevel, Page *page, const char *filename, GError **err)
{
  GError *tmp_err            = NULL;
  GList  *objects            = NULL;
  char   *full_filename      = NULL;
  char   *full_rcfilename    = NULL;
  char   *file_directory     = NULL;
  char   *saved_cwd          = NULL;
  char   *backup_filename    = NULL;
  char    load_backup_file   = 0;
  int     opened             = FALSE;
  int     flags;

  unsigned int mem_needed;

  const char *log_auto_back;
  const char *log_undetemine;
  const char *log_back_newer;
  const char *log_situation;
  const char *err_corrective;

  log_auto_back  = _("\nWARNING: Found an autosave backup file:\n \"%s\".\n\n");
  log_undetemine = _("Could not detemine which file is newer, so you should do this manually.\n");
  log_back_newer = _("The backup copy is newer than the schematic, so it seems you should load it instead of the original file.\n");
  log_situation  = _("This situation may have when an application crashed or was forced to exit abruptly.\n");
  err_corrective = _("\nRun the application to correct this situation or manually delete the backup file.\n\n");

  int inline f_open_exit (int status) {
    if (saved_cwd != NULL) {
      free(saved_cwd);
    }
    return status;
  }

  flags     = toplevel->open_flags;
  saved_cwd = NULL;

  /* Save the cwd so we can restore it later. */
  if (flags & F_OPEN_RESTORE_CWD) {
    saved_cwd = getcwd(0,0);
  }

  /* get full, absolute path to file */
  full_filename = f_file_normalize_name (filename, &tmp_err);

  if (full_filename == NULL) {
    g_set_error (err, G_FILE_ERROR, tmp_err->code,
               _("Cannot find file %s: %s"),
                 filename, tmp_err->message);
    g_error_free(tmp_err);
    return f_open_exit(0);
  }

  /* write full, absolute filename into page->filename */
  GEDA_FREE(page->filename);
  page->filename = u_string_strdup(full_filename);

  /* Before we open the page, let's load the corresponding gafrc. */
  /* First cd into file's directory. */
  file_directory = f_get_dirname (full_filename);

  if (file_directory) {

    if (chdir (file_directory)) {
      /* Error occurred with chdir */
      fprintf(stderr, _("ERROR, <libgeda>: Could not changed current directory to %s:%s"),
              file_directory, strerror (errno));
      return f_open_exit(0);
    }

    /* Now open RC and process file */
    if (flags & F_OPEN_RC) {

      g_rc_parse_local ("gafrc", file_directory, &tmp_err);

      if (tmp_err != NULL) {
        /* RC files are allowed to be missing or skipped; check for this. */
        if (!g_error_matches (tmp_err, G_FILE_ERROR, G_FILE_ERROR_NOENT) &&
            !g_error_matches (tmp_err, EDA_ERROR,    EDA_ERROR_RC_TWICE))
        {
          u_log_message ("%s\n", tmp_err->message);
        }
        g_error_free (tmp_err);
        tmp_err = NULL;
      }
    }

    GEDA_FREE (file_directory);
  }

  if (flags & F_OPEN_CHECK_BACKUP) {

    char       *message;
    const char *str;

    /* Check for a newer autosave backup file */
    bool active_backup = f_has_active_autosave (full_filename, &tmp_err);
    backup_filename    = f_get_autosave_filename (full_filename);

    if (tmp_err != NULL) {
      fprintf (stderr, "%s\n", tmp_err->message);
    }

    if (active_backup) {

      mem_needed = strlen(log_auto_back) + strlen(backup_filename);

      if (tmp_err != NULL) {
        str = log_undetemine;
        mem_needed = mem_needed + strlen(log_undetemine);
      }
      else {
        str = log_back_newer;
        mem_needed = mem_needed + strlen(log_back_newer);
      }

      mem_needed = mem_needed + strlen(log_situation);

      message = malloc(mem_needed + 100);

      if(!message) {
        fprintf(stderr, _("%s: Memory Allocation Error!\n"), __func__);
      }
      else {

        sprintf(message, log_auto_back, backup_filename);
        strcat(strcat(message, str), log_situation);

        if (toplevel->load_newer_backup_func == NULL) {
          fprintf (stderr, "%s%s", message, err_corrective);
        }
        else {

          /* Ask the user which file should be loaded */
          load_backup_file = toplevel->load_newer_backup_func(message,
                             toplevel->load_newer_backup_data);
        }

        free (message);
      }
    }
    if (tmp_err != NULL) g_error_free (tmp_err);
  }

  /* Now that we have set the current directory and read
   * the RC file, it's time to read in the file. */
  if (load_backup_file == 1) {
    /* Load the backup file */
    objects = o_read (toplevel, NULL, backup_filename, &tmp_err);
  }
  else {
    /* Load the original file */
    objects = o_read (toplevel, NULL, full_filename, &tmp_err);
    if (load_backup_file == 2) {
      remove(backup_filename);
    }
  }

  if (tmp_err == NULL) {
    s_page_append_list (page, objects);
    if (load_backup_file == 0) {
      /* If it's not the backup file */
      page->CHANGED=0; /* added 4/7/98 */
    }
    else {
      /* We are loading the backup file, so gschem should ask
       *    the user if save it or not when closing the page. */
      page->CHANGED=1;
    }
    opened = TRUE;
  }
  else {
    g_propagate_error (err, tmp_err);
  }

  GEDA_FREE(full_filename);
  GEDA_FREE(full_rcfilename);
  GEDA_FREE (backup_filename);

  /* Reset current directory to the orginal location */
  if (flags & F_OPEN_RESTORE_CWD) {
    if (chdir (saved_cwd)) {
      fprintf(stderr, _("ERROR, <libgeda>: Could not restore current directory to %s:%s"),
              saved_cwd, strerror (errno));
    }
  }

  return f_open_exit(opened);
}

/*! \brief Closes the schematic file
 *  \par Function Description
 *  Does nothing
 *
 *  \param [in,out] toplevel  The GedaToplevel object with schematic to be closed.
 */
void f_close(GedaToplevel *toplevel)
{

}

/*! \brief Save Schematic or Symbol file
 *  \par Function Description
 *  This function saves the current file in the toplevel object.
 *
 *  \param [in,out] toplevel  The GedaToplevel object containing the file.
 *  \param [in]     page      A Page object to be associated with the file
 *  \param [in]     filename  The file name to save the schematic or symbol.
 *  \param [in,out] err       GError structure for error reporting, or
 *                            NULL to disable error reporting
 *  \return 1 on success, 0 on failure.
 */
bool
f_save(GedaToplevel *toplevel, Page *page, const char *filename, GError **err)
{
  char   *backup_filename;
  char   *real_filename;
  char   *only_filename;
  char   *dirname;

  const char *err_not_real;
  const char *err_not_saved;
  const char *err_read_only;

  const char *log_set_back;
  const char *log_not_back;

  GError *tmp_err;
  struct  stat st_ActiveFile;
  int     result;

  err_not_real  = _("Can't get the real filename of %s: %s");
  err_not_saved = _("Could NOT save file: %s");
  err_read_only = _("File %s is read-only");

  log_set_back  = _("Could not set previous backup file [%s] read-write:%s\n");
  log_not_back  = _("Can not create backup file: %s: %s\n");

  tmp_err       = NULL;
  result        = 1;

  /* Get the real filename and file permissions */
  real_filename = f_file_follow_symlinks (filename, &tmp_err);

  if (real_filename == NULL) {
    g_set_error (err, tmp_err->domain, tmp_err->code, err_not_real,
                 filename, tmp_err->message);
    result = 0;
  }
  /* Check to see if filename is writable */
  else if (g_file_test(filename, G_FILE_TEST_EXISTS))  {

    errno = 0;
    access(filename, W_OK);
    if (errno == EACCES) {
      g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_PERM, err_read_only, filename);
      result = 0;
    }
  }

  if (result) {

    /* Get the files original permissions */
    if (stat (real_filename, &st_ActiveFile) != 0) {

      /* if problem then save default values */
      st_ActiveFile.st_mode = 0666 & ~umask(0);
    }

    /* Get the directory in which the real filename lives */
    dirname = f_get_dirname (real_filename);
    only_filename = g_path_get_basename(real_filename);

    /* Do a backup if it's not an undo file backup and it was never saved.
     * Only do a backup if backup files are enabled */
    if (page->saved_since_first_loaded == 0 &&
      toplevel->make_backup_files == TRUE)
    {
      if ( (g_file_test (real_filename, G_FILE_TEST_EXISTS)) &&
        (!g_file_test(real_filename, G_FILE_TEST_IS_DIR)) )
      {
        backup_filename = u_string_sprintf("%s%c%s~", dirname, DIR_SEPARATOR,
                                          only_filename);

        /* Make the backup file read-write before saving a new one */
        if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
           (!g_file_test (backup_filename, G_FILE_TEST_IS_DIR)))
        {
          if (chmod(backup_filename, S_IREAD|S_IWRITE) != 0) {
            u_log_message (log_set_back, backup_filename, strerror (errno));
          }
          else { /* delete backup from previous session */
            f_file_remove (backup_filename);
          }
        }

        if (f_file_copy(real_filename, backup_filename) != 0) {
          u_log_message (log_not_back, backup_filename, strerror (errno));
        }
        else {
          /* Make backup readonly so a 'rm *' will ask user before deleting */
          chmod(backup_filename, 0444 & ~umask(0));
        }
        GEDA_FREE(backup_filename);
      }
    }

    /* If there is not an existing file with that name, compute the
     * permissions and uid/gid that we will use for the newly-created file.
     */

    GEDA_FREE (dirname);
    GEDA_FREE (only_filename);

    if (o_save (s_page_get_objects (page), real_filename, &tmp_err)) {

      page->saved_since_first_loaded = 1;

      /* Restore permissions. */
      chmod (real_filename, st_ActiveFile.st_mode);

#ifdef HAVE_CHOWN
      if (chown (real_filename, st_ActiveFile.st_uid, st_ActiveFile.st_gid)) {
        /* Either the current user has permissioin to change ownership
         * or they didn't. */
      }
#endif

      /* Reset the last saved timer */
      time (&page->last_load_or_save_time);

      page->ops_since_last_backup = 0;
      page->do_autosave_backup = 0;
      page->CHANGED=0; /* WEH: added 11/17/13, really */

      s_undo_update_modified(toplevel->page_current);

      GEDA_FREE (real_filename);
      result = 1;
    }
    else {
      g_set_error (err, tmp_err->domain, tmp_err->code,
                   err_not_saved,
                   tmp_err->message);
      g_clear_error (&tmp_err);
      GEDA_FREE (real_filename);
      result = 0;
    }
  }

  return result;
}

/*! \brief Builds an absolute pathname.
 *  \par Function Description
 *  This function derives an absolute pathname for the pathname
 *  pointed to by \a name with '.' and '..' resolved. It does not
 *  resolve symbolic links.
 *
 *  It returns NULL and sets the \a error (if not NULL) if it failed
 *  to build the pathname or the pathname does not exists.
 *
 *  \note
 *  The code for this function is derived from the realpath() of
 *  the GNU C Library (Copyright (C) 1996-2002, 2004, 2005, 2006 Free
 *  Software Foundation, Inc. / LGPL 2.1 or later).
 *
 *  The part for the resolution of symbolic links has been discarded
 *  and it has been adapted for glib and for use on Windows.
 *
 *  \param [in]     name  A character string containing the pathname
 *                        to resolve.
 *  \param [in,out] error Return location for a GError, or NULL.
 *  \return A newly-allocated string with the resolved absolute
 *  pathname on success, NULL otherwise.
 */
char *f_file_normalize_name (const char *name, GError **error)
{

#if defined (OS_WIN32_NATIVE)
    char buf[MAX_PATH];
#else
    GString    *rpath;
    const char *start, *end;
#endif

  if (name == NULL) {
    g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_INVAL,
                 "%s", strerror (EINVAL));
    return NULL;
  }

  if (*name == '\0') {
    g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_NOENT,
                 "%s", strerror (ENOENT));
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
  DWORD len = GetFullPathName (name, MAX_PATH, buf, NULL);
  char *result;

  if (len == 0 || len > MAX_PATH - 1) {
    result = g_strdup (name);
  }
  else {
    /* The file system is case-preserving but case-insensitive,
     * canonicalize to lowercase, using the codepage associated
     * with the process locale.  */
    CharLowerBuff (buf, len);
    result = g_strdup (buf);
  }

  /* Test that the file actually exists, and fail if it doesn't.  We
   * do this to be consistent with the behaviour on POSIX platforms. */
  if (!g_file_test (result, G_FILE_TEST_EXISTS)) {
    g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_NOENT,
                 "%s", strerror (ENOENT));
    GEDA_FREE (result);
    return NULL;
  }
  return result;

#else
#define ROOT_MARKER_LEN 1 /* What the ?*/

  rpath = g_string_sized_new (strlen (name));

  /* if relative path, prepend current dir */
  if (!f_get_is_path_absolute (name)) {

    char *cwd = g_get_current_dir ();

    g_string_append (rpath, cwd);
    GEDA_FREE (cwd);

    if (!G_IS_DIR_SEPARATOR (rpath->str[rpath->len - 1])) {
      g_string_append_c (rpath, DIR_SEPARATOR);
    }
  }
  else {
    g_string_append_len (rpath, name, ROOT_MARKER_LEN);
    /* move to first path separator */
    name += ROOT_MARKER_LEN - 1;
  }

  for (start = end = name; *start != '\0'; start = end) {
    /* skip sequence of multiple path-separators */
    while (G_IS_DIR_SEPARATOR (*start)) {
      ++start;
    }

    /* find end of path component */
    for (end = start; *end != '\0' && !G_IS_DIR_SEPARATOR (*end); ++end);

    if (end - start == 0) {
      break;
    }
    else if (end - start == 1 && start[0] == '.') {
      /* nothing */;
    }
    else if (end - start == 2 && start[0] == '.' && start[1] == '.') {
      /* back up to previous component, ignore if at root already.  */
      if (rpath->len > ROOT_MARKER_LEN) {
        while (!G_IS_DIR_SEPARATOR (rpath->str[(--rpath->len) - 1]));
        g_string_set_size (rpath, rpath->len);
      }
    }
    else {
      /* path component, copy to new path */
      if (!G_IS_DIR_SEPARATOR (rpath->str[rpath->len - 1])) {
        g_string_append_c (rpath, DIR_SEPARATOR);
      }

      g_string_append_len (rpath, start, end - start);

      if (!g_file_test (rpath->str, G_FILE_TEST_EXISTS)) {
        g_set_error (error,G_FILE_ERROR, G_FILE_ERROR_NOENT,
                     "%s", strerror (ENOENT));
        goto error;
      }
      else if (!g_file_test (rpath->str, G_FILE_TEST_IS_DIR) &&
                 *end != '\0') {
        g_set_error (error,G_FILE_ERROR, G_FILE_ERROR_NOTDIR,
                     "%s", strerror (ENOTDIR));
        goto error;
      }
    }
  }

  if (G_IS_DIR_SEPARATOR (rpath->str[rpath->len - 1]) &&
      rpath->len > ROOT_MARKER_LEN) {
    g_string_set_size (rpath, rpath->len - 1);
  }

  return g_string_free (rpath, FALSE);

error:
  g_string_free (rpath, TRUE);
  return NULL;
#undef ROOT_MARKER_LEN
#endif
}

void f_set_backup_loader_query_func (GedaToplevel *toplevel, void *func, ...)
{
  if (toplevel) {
    va_list argp;
    va_start (argp, func);
    toplevel->load_newer_backup_func = func;
    toplevel->load_newer_backup_data = va_arg(argp, void*);
    va_end (argp);
  }
}

/*! \brief Remove backup file
 *  \par Function Description
 *  This function deletes files created by the autosave sub-system, if
 *  the such a file exist for \a filename.
 *
 *  \param [in]     filename  The file name of the schematic or symbol.
 *
 *  \todo implement err argument?
 *   param [in,out] err       GError structure for error reporting, or
 *                            NULL to disable error reporting
 */
void f_remove_backup_file (const char *filename)
{
  char *backup_filename;
  char *real_filename;

  /* Get the real filename and file permissions */
  real_filename = f_file_follow_symlinks (filename, NULL);

  if (real_filename == NULL) {
    u_log_message (_("%s: Can not get the real filename of %s."),
                      __func__, filename);
  }
  else {

    backup_filename = f_get_autosave_filename (real_filename);

    /* Delete the backup file */
    if ((g_file_test(backup_filename, G_FILE_TEST_EXISTS)) &&
       (!g_file_test(backup_filename, G_FILE_TEST_IS_DIR)))
    {
      if (unlink(backup_filename) != 0) {
        u_log_message(_("%s: Unable to delete backup file %s."),
                      __func__, backup_filename);
      }
    }

    GEDA_FREE (backup_filename);
  }

  GEDA_FREE(real_filename);
}
