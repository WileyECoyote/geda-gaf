/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
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

#include "../../../config.h"

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <libgeda_priv.h>

/** \defgroup libgeda-file-functions Libgeda File Functions
 *  @{ \par
 *  This group contains basic functions to maintaining geda
 *  schematic and symbol files.
*/

/*!
 * \brief Closes the schematic file
 * \par Function Description
 *  Does nothing
 *
 * \param [in,out] toplevel  The GedaToplevel object with schematic to be closed.
 */
void geda_file_close(GedaToplevel *toplevel)
{

}

/*! F0105
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
bool geda_file_has_active_autosave (const char *filename, GError **err)
{
  bool result;

  if (!filename) {

    result = FALSE;
  }
  else {

    int   file_err       = 0;
    int   save_errno     = 0;
    char *auto_filename;

    struct stat file_stat, auto_stat;

    auto_filename = geda_file_get_autosave_filename (filename);

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
        g_set_error (err, EDA_ERROR, save_errno, "%s [%s]: %s",
                   _("Failed to get file statistics"),
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

            g_set_error (err, EDA_ERROR, file_err, "%s [%s]: %s",
                       _("Failed to get file statistics"),
                         auto_filename, strerror (file_err));
            result = TRUE;
          }
          else {

            /* If we got this far, both files exist and we have
             * managed to get their stat info. */
            if (difftime (file_stat.st_mtime, auto_stat.st_mtime) < 0) {
              result = TRUE;
            }
            else {
              result = FALSE;
            }
          }
        }
      }
    }

    GEDA_FREE (auto_filename);
  }
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
int geda_file_open(GedaToplevel *toplevel, Page *page, const char *filename, GError **err)
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

  int inline geda_file_open_exit (int status) {
    if (saved_cwd != NULL) {
      free(saved_cwd);
    }
    return status;
  }

  if (!GEDA_IS_PAGE(page)) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_INVALID_PAGE,
               _("Error: Invalid page object"));
    return 0;
  }

  flags     = geda_toplevel_get_file_open_flags(toplevel);
  saved_cwd = NULL;

  /* Save the cwd so we can restore it later. */
  if (flags & F_OPEN_RESTORE_CWD) {
    saved_cwd = getcwd(0,0);
  }

  /* get full, absolute path to file */
  full_filename = geda_file_sys_normalize_name (filename, &tmp_err);

  if (full_filename == NULL) {

    /*const char *msg = _("Cannot find file");

    g_set_error (err, EDA_ERROR, tmp_err->code, "%s %s: %s",
                 msg, filename, tmp_err->message);*/

    g_set_error (err, EDA_ERROR, tmp_err->code, "%s", tmp_err->message);
    g_error_free(tmp_err);
    return geda_file_open_exit(0);
  }

  /* write full, absolute filename into page->filename */
  GEDA_FREE(page->filename);
  page->filename = geda_utility_string_strdup(full_filename);

  /* Before we open the page, load the corresponding gafrc. */
  /* First change into file's directory. */
  file_directory = geda_get_dirname (full_filename);

  if (file_directory) {

    if (chdir (file_directory)) {

      /* Error occurred with chdir */
      const char *msg = _("ERROR: Could not change current directory to");

      fprintf(stderr, "<libgeda> %s %s: %s", msg, file_directory, strerror (errno));

      return geda_file_open_exit(0);
    }

    /* Now open RC and process file */
    if (flags & F_OPEN_RC) {

      g_rc_parse_local ("gafrc", file_directory, &tmp_err);

      if (tmp_err != NULL) {

        /* RC files are allowed to be missing or skipped; check for this. */
        if (!g_error_matches (tmp_err, EDA_ERROR, ENOENT) &&
            !g_error_matches (tmp_err, EDA_ERROR, EDA_ERROR_RC_TWICE))
        {
          geda_log ("%s\n", tmp_err->message);
        }
        g_error_free (tmp_err);
        tmp_err = NULL;
      }
    }

    GEDA_FREE (file_directory);
  }

  if (flags & F_OPEN_CHECK_BACKUP) {

    /* Check for a newer autosave backup file */
    bool active_backup = geda_file_has_active_autosave (full_filename, &tmp_err);
    backup_filename    = geda_file_get_autosave_filename (full_filename);

    if (tmp_err != NULL) {
      fprintf (stderr, "%s\n", tmp_err->message);
    }

    if (active_backup) {

      const char *log_auto_back;
      const char *log_do_manual;
      const char *log_back_newer;
      const char *log_situation;
      const char *err_corrective;
      const char *str;
      char       *message;

      log_auto_back  = _("\nWARNING: Found an autosave backup file:\n\"%s\".\n\n");
      log_do_manual  = _("Could not determine which file is newer, so you should do this manually.\n");
      log_back_newer = _("The backup copy is newer than the schematic, it seems you should load it instead of the original file.\n");
      log_situation  = _("This situation may have occurred when an application crashed or was forced to exit abruptly.\n");
      err_corrective = _("Run the application to correct this situation or manually delete the backup file.");

      register unsigned int mem_needed = strlen(log_auto_back)
                                       + strlen(backup_filename)
                                       + strlen(log_situation);

      if (tmp_err != NULL) {
        mem_needed += strlen(str = log_do_manual);
      }
      else {
        mem_needed += strlen(str = log_back_newer);
      }

      message = malloc(mem_needed + 1);

      if(!message) { /* Should this be translated? */
        fprintf(stderr, "%s %s\n", __func__, _("Memory allocation error!"));
      }
      else {

        sprintf(message, log_auto_back, backup_filename);
        strcat(strcat(message, str), log_situation);

        if (toplevel->load_newer_backup_func == NULL) {
          fprintf (stderr, "%s\n%s\n\n", message, err_corrective);
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

    const char *log_msg = _("Loading backup file");

    geda_log ("%s \"%s\".\n", log_msg, backup_filename);

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

    geda_struct_page_append_list (page, objects);

    g_list_free(objects);

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
  GEDA_FREE(backup_filename);

  /* Reset current directory to the orginal location */
  if (flags & F_OPEN_RESTORE_CWD) {
    if (chdir (saved_cwd)) {
      const char *msg = _("ERROR: Could not restore current directory to");
      fprintf(stderr, "<libgeda> %s %s: %s", msg,  saved_cwd, strerror (errno));
    }
  }

  return geda_file_open_exit(opened);
}

/*!
 * \brief Retrieve the File Open Flags from Toplevel
 * \par Function Description
 *  Returns the open_flags without checking toplevel.
 *
 * \param [in,out] toplevel  The GedaToplevel object.
 *
 * \return Returns file open_flags.
 */
int geda_file_open_flags (GedaToplevel *toplevel)
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
void geda_file_remove_backup (const char *filename)
{
  if (filename) {

    char *real_filename;

    /* Get the real filename and file permissions */
    real_filename = geda_file_sys_follow_symlinks (filename, NULL);

    if (real_filename == NULL) {
      const char *log_msg = _("Cannot get the real filename of");
      geda_log ("%s: %s.\n", log_msg, filename);
    }
    else {

      char *backup_filename = geda_file_get_autosave_filename (real_filename);

      /* Delete the backup file */
      if (g_file_test(backup_filename, G_FILE_TEST_IS_REGULAR)) {

#if defined (OS_WIN32_NATIVE)

        if (chmod(backup_filename, 666)) {
          fprintf (stderr, "%s: %s, %s\n", __func__, backup_filename, strerror (errno));
        }

#endif

        if (unlink(backup_filename) != 0) {
          const char *log_msg = _("Unable to delete backup file");
          geda_log ("%s %s, %s.\n", log_msg, filename, strerror (errno));
        }
      }

      GEDA_FREE (backup_filename);
    }

    GEDA_FREE(real_filename);
  }
}

static int f_file_size(const char *filename)
{
  struct stat st;
  stat(filename, &st);
  return st.st_size;
}

/*!
 * \brief Save Schematic or Symbol file
 * \par Function Description
 *  This function saves the \a page to a file with name \a filename.
 *
 * \param [in,out] toplevel  GedaToplevel object.
 * \param [in]     page      A Page object associated with the file
 * \param [in]     filename  The file name to save the schematic or symbol.
 * \param [in,out] err       GError structure for error reporting, or
 *                           NULL to disable error reporting
 * \return 1 on success, 0 on failure.
 */
bool geda_file_save(GedaToplevel *toplevel, Page *page, const char *filename, GError **err)
{
  GError *tmp_err;
  char   *real_filename;
  bool    success;

  tmp_err = NULL;
  success = 1;

  /* Get the real filename and file permissions */
  real_filename = geda_file_sys_follow_symlinks (filename, &tmp_err);

  if (real_filename == NULL) {

    const char *err_not_real = _("Cannot get the real filename of");

    g_set_error (err, tmp_err->domain, tmp_err->code, "%s %s: %s",
                 err_not_real, filename, tmp_err->message);
    g_error_free (tmp_err);
    success = 0;
  }
  /* Check to see if filename is writable */
  else if (access(filename, R_OK) == 0)  {

    errno = 0;
    access(filename, W_OK);

    if (errno == EACCES) {

      const char *err_read_only = _("File [%s] is read-only");

      g_set_error (err, EDA_ERROR, EACCES, err_read_only, filename);
      GEDA_FREE (real_filename);
      success = 0;
    }
  }

  if (success) {

    if (!GEDA_IS_PAGE(page))  {

      g_set_error_literal (err, EDA_ERROR, EDA_ERROR_INVALID_PAGE,
                           "Invalid or corrupt Page object");
      success = 0;
    }
    else {

      const char *only_filename;
      char *dirname;

      struct stat st_ActiveFile;

      /* Get the files original permissions */
      if (stat (real_filename, &st_ActiveFile) != 0) {

        /* if problem then save default values */
        st_ActiveFile.st_mode = 0666 & ~umask(0);
      }

      /* Get the directory in which the real filename lives */
      dirname       = geda_get_dirname (real_filename);
      only_filename = geda_file_get_basename(real_filename);

      /* Do a backup if page is not an undo file backup and the page has
       * been never saved. Only do a backup if backup files are enabled,
       * noting that geda_toplevel_get_make_backups returns FALSE if the
       * toplevel argument is invalid, which was not checked */
      if (page->saved_since_first_loaded == 0 &&
          geda_toplevel_get_make_backups(toplevel))
      {
        if ((access(real_filename, R_OK) == 0) &&
          (!g_file_test (real_filename, G_FILE_TEST_IS_DIR)) &&
          f_file_size (real_filename))
        {
          char *backup_filename;

          backup_filename = geda_sprintf("%s%c%s~", dirname, DIR_SEPARATOR,
                                         only_filename);

          /* Make the backup file read-write before saving a new one */
          if ((access(backup_filename, R_OK) == 0) &&
            (!g_file_test (backup_filename, G_FILE_TEST_IS_DIR)))
          {
            if (chmod(backup_filename, S_IREAD|S_IWRITE) != 0) {

              const char *log_set_back = _("Could not set previous backup file");
              const char *read_write   = _("read-write");

              geda_log ("%s [%s] %s:%s\n", log_set_back, backup_filename, read_write, strerror (errno));
            }
            else { /* delete backup from previous session */
              geda_file_sys_remove (backup_filename);
            }
          }

          if (geda_file_copy(real_filename, backup_filename) != 0) {

            const char *log_not_back = _("Cannot create backup file");

            geda_log ("%s: %s: %s\n", log_not_back, backup_filename, strerror (errno));
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

      if (geda_object_save (geda_struct_page_get_objects (page), real_filename, &tmp_err))
      {
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

        geda_struct_undo_update_modified(page);

        success = 1;
      }
      else {

        const char *err_not_saved = _("Could NOT save file");

        g_set_error (err, tmp_err->domain, tmp_err->code, "%s: %s",
                     err_not_saved, tmp_err->message);
        g_clear_error (&tmp_err);
        success = 0;
      }
    }
    GEDA_FREE (real_filename);
  }

  return success;
}

/** @} endgroup libgeda-file-functions */
