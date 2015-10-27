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

/*! \file o_save.c
 *  \brief functions for saving object data
 */

#include <config.h>
#include <stdio.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include "libgeda_priv.h"

/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] toplevel  GedaToplevel object to search for autosave's.
 */
void o_save_auto_backup(GedaToplevel *toplevel)
{
  GError   *err;
  GList    *iter;
  Page     *p_save, *p_current;
  char     *backup_filename;
  char     *real_filename;
  char     *only_filename;
  char     *dirname;
  mode_t    saved_umask;
  mode_t    mask;
  struct    stat st;
  int       count;

  count = 0;

  /* save current page */
  p_save = toplevel->page_current;

  for (iter = geda_toplevel_get_pages(toplevel); iter != NULL; NEXT(iter))
  {
    p_current = (Page *)iter->data;

    if (p_current->do_autosave_backup == 0) {
      continue;
    }

    if (p_current->ops_since_last_backup != 0) {

      count++;

      /* make p_current the current page of toplevel */
      toplevel->page_current = p_current;
      s_page_goto (p_current);

      /* Get the real filename and file permissions */
      real_filename = f_file_follow_symlinks (p_current->filename, NULL);

      if (real_filename == NULL) {
        u_log_message (_("%s: Can't get real filename of %s."),
                       __func__, p_current->filename);
      }
      else {

        /* Get the directory in which the real filename lives */
        dirname         = f_path_get_dirname (real_filename);
        only_filename   = g_path_get_basename(real_filename);

        backup_filename = u_string_sprintf("%s%c"AUTOSAVE_BACKUP_FILENAME_STRING,
                                            dirname, DIR_SEPARATOR, only_filename);

        /* If there is not an existing file with that name, compute the
         * permissions and uid/gid that we will use for the newly-created file.
         */

        if (stat (real_filename, &st) != 0) {

#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
            struct stat dir_st;
            int result;
#endif

            /* Use default permissions */
            saved_umask = umask(0);
            st.st_mode = 0666 & ~saved_umask;
            umask(saved_umask);

#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
            st.st_uid = getuid ();

            result = stat (dirname, &dir_st);

            if (result == 0 && (dir_st.st_mode & S_ISGID))
              st.st_gid = dir_st.st_gid;
            else
              st.st_gid = getgid ();
#endif

        }
        GEDA_FREE (dirname);
        GEDA_FREE (only_filename);
        GEDA_FREE (real_filename);

        /* Make the backup file writable before saving a new one */
        if (g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
           (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR)))
        {
          saved_umask = umask(0);
          if (chmod(backup_filename, (S_IWRITE|S_IWGRP|S_IWOTH) &
                    ((~saved_umask) & 0777)) != 0) {
            u_log_message (_("Could NOT set previous backup file [%s] read-write\n"),
                           backup_filename);
          }
          umask(saved_umask);
        }

        if (o_save (s_page_get_objects (toplevel->page_current), backup_filename, &err))
        {
          u_log_message (_("Automatic backup file saved [%s]\n"), backup_filename);

          p_current->ops_since_last_backup = 0;
          p_current->do_autosave_backup = 0;

          /* Make the backup file readonly so a 'rm *' command will ask
             the user before deleting it */
          saved_umask = umask(0);
          mask = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
          mask = (~mask)&0777;
          mask &= ((~saved_umask) & 0777);
          if (chmod(backup_filename,mask) != 0) {
            u_log_message (_("Could NOT set backup file [%s] readonly\n"),
                           backup_filename);
          }
          umask(saved_umask);
        }
        else {
          u_log_message (_("Could NOT save backup file [%s]: %s\n"),
                            backup_filename, err->message);
          g_clear_error (&err);
        }
        GEDA_FREE (backup_filename);
      }
    }
  }

  /* Restore current page if any backups were performed */
  if (count) {
    toplevel->page_current = p_save;
    s_page_goto (p_current);
  }
}

/*! \brief Save a series of objects into a string buffer
 *  \par Function Description
 *  This function recursively saves a set of objects into a buffer in
 *  libgeda format.  User code should not normally call this function;
 *  they should call o_save_buffer() instead.
 *
 *  With save_attribs passed as FALSE, attribute objects are skipped over,
 *  and saved separately - after the objects they are attached to. When
 *  we recurse for saving out those attributes, the function must be called
 *  with save_attribs passed as TRUE.
 *
 *  \param [in] object_list   The head of a GList of objects to save.
 *  \param [in] save_attribs  Should attribute objects encounterd be saved?
 *  \returns a buffer containing schematic data or NULL on failure.
 */
char *o_save_objects (const GList *object_list, bool save_attribs)
{
  Object  *o_current;
  const    GList *iter;
  char    *out;
  GString *acc;
  bool     already_wrote = FALSE;

  acc = g_string_new("");

  iter = object_list;

  while ( iter != NULL ) {
    o_current = (Object *)iter->data;

    if (save_attribs || o_current->attached_to == NULL) {

      switch (o_current->type) {

        case(OBJ_LINE):
          out = o_line_save(o_current);
          break;

        case(OBJ_NET):
          out = o_net_save(o_current);
          break;

        case(OBJ_BUS):
          out = o_bus_save(o_current);
          break;

        case(OBJ_BOX):
          out = o_box_save(o_current);
          break;

        case(OBJ_CIRCLE):
          out = o_circle_save(o_current);
          break;

        case(OBJ_COMPLEX):
          out = o_complex_save(o_current);
          g_string_append_printf(acc, "%s\n", out);
          already_wrote = TRUE;
          GEDA_FREE(out); /* need to free here because of the above flag */

          if (o_complex_is_embedded(o_current)) {
            g_string_append(acc, "[\n");

            out = o_save_objects(o_current->complex->prim_objs, FALSE);
            g_string_append (acc, out);
            GEDA_FREE(out);

            g_string_append(acc, "]\n");
          }
          break;

        case(OBJ_PLACEHOLDER):  /* new type by SDB 1.20.2005 */
          out = o_complex_save(o_current);
          break;

        case(OBJ_TEXT):
          out = o_text_save(o_current);
          break;

        case(OBJ_PATH):
          out = o_path_save(o_current);
          break;

        case(OBJ_PIN):
          out = o_pin_save(o_current);
          break;

        case(OBJ_ARC):
          out = o_arc_save(o_current);
          break;

        case(OBJ_PICTURE):
          out = o_picture_save(o_current);
          break;

        default:
          /*! \todo Maybe we can continue instead of just failing completely?
           *  In any case, failing gracefully is better than killing the
           *  program, which is what this used to do... */
          g_critical (_("%s: object %p has unknown type '%c'\n"),
                      __func__, o_current, o_current->type);
          /* Dump string built so far */
          g_string_free(acc, TRUE);
          return NULL;
      }

      /* output the line */
      if (!already_wrote) {
        g_string_append_printf(acc, "%s\n", out);
        GEDA_FREE(out);
      }
      else {
        already_wrote = FALSE;
      }

      /* save any attributes */
      if (o_current->attribs != NULL) {
        g_string_append (acc, "{\n");
        out = o_save_objects (o_current->attribs, TRUE);
        g_string_append (acc, out);
        GEDA_FREE(out);

        g_string_append (acc, "}\n");
      }
    }

    iter = g_list_next (iter);
  }

  return g_string_free(acc, FALSE);
}

/*! \brief "Save" a file into a string buffer
 *
 *  \par Function Description
 *  This function saves a whole schematic into a buffer in libgeda
 *  format. The buffer should be freed when no longer needed.
 *
 *  \param [in] object_list The head of a GList of Objects to save.
 *
 *  \returns a buffer containing schematic data or NULL on failure.
 */
char *o_save_buffer (const GList *object_list)
{
  GString *acc;
  char    *buffer;

  acc = g_string_new (f_get_format_header());

  buffer = o_save_objects (object_list, FALSE);
  g_string_append (acc, buffer);
  GEDA_FREE (buffer);

  return g_string_free(acc, FALSE);
}

/*! \brief Save a file
 *  \par Function Description
 *  This function saves the data in a libgeda format to a file
 *
 *  \param [in] object_list The head of a GList of Objects to save.
 *  \param [in] filename    The filename to save the data to.
 *  \param [in,out] err     GError structure for error reporting.
 *
 *  \return 1 on success, 0 on failure.
 */
bool
o_save (const GList *object_list, const char *filename, GError **err)
{
  char *buffer;
  char *path;

  int   result;

  errno = 0;
  path  = f_path_get_dirname(filename);

  /* Check to see if real filename is writable */
  if (access(path, W_OK) != 0) {
    g_set_error (err, G_FILE_ERROR, errno, _("[%s]: because %s"),
                 path, strerror(errno));
    result = 0;
  }
  else {

    FILE  *output;

    output = fopen (filename, "w" );

    buffer = o_save_buffer (object_list);

    fputs(buffer, output);

    fclose(output);

    GEDA_FREE (buffer);

    result = 1;
  }
  GEDA_FREE (path);
  return result;
}