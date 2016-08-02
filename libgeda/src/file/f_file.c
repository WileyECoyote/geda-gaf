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

/*! \file f_file.c
 *  \brief file related functions
 */

#include <config.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <libgeda_priv.h>

/*!
 * \brief Closes the schematic file
 * \par Function Description
 *  Does nothing
 *
 * \param [in,out] toplevel  The GedaToplevel object with schematic to be closed.
 */
void
f_close(GedaToplevel *toplevel)
{

}

/*!
 * \brief Check if a file has an active autosave file
 * \par Function Description
 *  Checks whether an autosave file exists for the \a filename passed
 *  which has a modification time newer than the file itself.  If the
 *  check fails, sets \a err with the reason.  N.b. if the autosave
 *  file exists but it was not possible to get its modification time,
 *  returns TRUE.
 *
 * \param [in]  filename File to check
 * \param [out] err      GError structure for error reporting, or
 *                       NULL to disable error reporting
 *
 * \returns TRUE if autosave active, FALSE otherwise
 */
bool
f_has_active_autosave (const char *filename, GError **err)
{
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

           g_set_error (err, G_FILE_ERROR, file_err,
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

/*!
 * \brief Opens the schematic file
 * \par Function Description
 *  Opens the schematic file and carries out a number of actions depending
 *  on the GedaToplevel::open_flags.  If F_OPEN_RC bit is set in open_flags,
 *  executes RC files found in the target directory. If F_OPEN_CHECK_BACKUP
 *  is set, warns user if a backup is found for the file being loaded, and
 *  possibly prompts user for whether to load the backup instead.
 *  If F_OPEN_RESTORE_CWD is set, does not change the working directory to
 *  that of the file being loaded.
 *
 * \param [in,out] toplevel  The GedaToplevel object to load the schematic into.
 * \param [in]     page      A Page object to be associated with the file
 * \param [in]     filename  A character string containing the file name
 *                           to open.
 * \param [in,out] err       GError structure for error reporting, or
 *                            NULL to disable error reporting
 *
 * \return 0 on failure, 1 on success.
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

  flags     = geda_toplevel_get_file_open_flags(toplevel);
  saved_cwd = NULL;

  /* Save the cwd so we can restore it later. */
  if (flags & F_OPEN_RESTORE_CWD) {
    saved_cwd = getcwd(0,0);
  }

  /* get full, absolute path to file */
  full_filename = f_sys_normalize_name (filename, &tmp_err);

  if (full_filename == NULL) {
    g_set_error (err, G_FILE_ERROR, tmp_err->code,
               _("Cannot find file %s: %s"), filename, tmp_err->message);
    g_error_free(tmp_err);
    return f_open_exit(0);
  }

  /* write full, absolute filename into page->filename */
  GEDA_FREE(page->filename);
  page->filename = geda_utility_string_strdup(full_filename);

  /* Before we open the page, load the corresponding gafrc. */
  /* First change into file's directory. */
  file_directory = f_path_get_dirname (full_filename);

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

    /* Check for a newer autosave backup file */
    bool active_backup = f_has_active_autosave (full_filename, &tmp_err);
    backup_filename    = f_get_autosave_filename (full_filename);

    if (tmp_err != NULL) {
      fprintf (stderr, "%s\n", tmp_err->message);
    }

    if (active_backup) {

      const char   *str;
      char         *message;
      unsigned int  mem_needed;

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

      if(!message) { /* Should this be translated? */
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
    objects = geda_object_read (toplevel, NULL, backup_filename, &tmp_err);
  }
  else {

    /* Load the original file */
    objects = geda_object_read (toplevel, NULL, full_filename, &tmp_err);

    if (load_backup_file == 2) {
      remove(backup_filename);
    }
  }

  if (tmp_err == NULL) {

    s_page_append_list (page, objects);

    if (load_backup_file == 0) {
      /* If it's not the backup file */
      page->CHANGED = 0; /* added 4/7/98 */
    }
    else {
      /* We are loading the backup file, so gschem should ask the
       * user whether to save the page or not when closing the page. */
      page->CHANGED = 1;
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

/*!
 * \brief Retreive the File Open Flags from Toplevel
 * \par Function Description
 *  Returns the open_flags without checking toplevel.
 *
 * \param [in,out] toplevel  The GedaToplevel object.
 *
 * \return Returns file open_flags.
 */
int
f_open_flags (GedaToplevel *toplevel)
{
  return toplevel->open_flags;
}

/*!
 * \brief Remove backup file
 * \par Function Description
 *  This function deletes files created by the autosave sub-system, if
 *  the such a file exist for \a filename.
 *
 * \param [in]     filename  The file name of the schematic or symbol.
 *
 * \todo implement err argument?
 *  param [in,out] err       GError structure for error reporting, or
 *                           NULL to disable error reporting
 */
void
f_remove_backup_file (const char *filename)
{
  char *real_filename;

  /* Get the real filename and file permissions */
  real_filename = f_sys_follow_symlinks (filename, NULL);

  if (real_filename == NULL) {
    u_log_message (_("%s: Can not get the real filename of %s."),
                      __func__, filename);
  }
  else {

    char *backup_filename = f_get_autosave_filename (real_filename);

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

static int
f_file_size(const char *filename)
{
  struct stat st;
  stat(filename, &st);
  return st.st_size;
}

/*!
 * \brief Save Schematic or Symbol file
 * \par Function Description
 *  This function saves the current file in the toplevel object.
 *
 * \param [in,out] toplevel  The GedaToplevel object containing the file.
 * \param [in]     page      A Page object to be associated with the file
 * \param [in]     filename  The file name to save the schematic or symbol.
 * \param [in,out] err       GError structure for error reporting, or
 *                           NULL to disable error reporting
 * \return 1 on success, 0 on failure.
 */
bool
f_save(GedaToplevel *toplevel, Page *page, const char *filename, GError **err)
{
  const char *err_not_real;
  const char *err_not_saved;
  const char *err_read_only;

  const char *log_set_back;
  const char *log_not_back;

  char   *real_filename;

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
  real_filename = f_sys_follow_symlinks (filename, &tmp_err);

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

    char *only_filename;
    char *dirname;

    /* Get the files original permissions */
    if (stat (real_filename, &st_ActiveFile) != 0) {

      /* if problem then save default values */
      st_ActiveFile.st_mode = 0666 & ~umask(0);
    }

    /* Get the directory in which the real filename lives */
    dirname = f_path_get_dirname (real_filename);
    only_filename = g_path_get_basename(real_filename);

    /* Do a backup if it's not an undo file backup and it was never saved.
     * Only do a backup if backup files are enabled */
    if (page->saved_since_first_loaded == 0 &&
        geda_toplevel_get_make_backups(toplevel))
    {
      if ((g_file_test (real_filename, G_FILE_TEST_EXISTS)) &&
         (!g_file_test (real_filename, G_FILE_TEST_IS_DIR)) &&
           f_file_size (real_filename))
      {
        char *backup_filename;

        backup_filename = geda_sprintf("%s%c%s~", dirname, DIR_SEPARATOR,
                                       only_filename);

        /* Make the backup file read-write before saving a new one */
        if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
           (!g_file_test (backup_filename, G_FILE_TEST_IS_DIR)))
        {
          if (chmod(backup_filename, S_IREAD|S_IWRITE) != 0) {
            u_log_message (log_set_back, backup_filename, strerror (errno));
          }
          else { /* delete backup from previous session */
            f_sys_remove (backup_filename);
          }
        }

        if (f_sys_copy(real_filename, backup_filename) != 0) {
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

    if (geda_object_save (s_page_get_objects (page), real_filename, &tmp_err)) {

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
      page->do_autosave_backup    = 0;
      page->CHANGED               = 0; /* WEH: added 11/17/13, really */

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
