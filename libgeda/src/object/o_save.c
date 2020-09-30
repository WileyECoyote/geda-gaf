/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
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

#include "../../../config.h"

#include <stdio.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <libgeda_priv.h>
#include <geda/geda_stat.h>

#define IO_BUFFER_SLICE_SIZE 2048

/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] toplevel  GedaToplevel object to search for autosave's.
 */
void
geda_object_save_auto_backup(GedaToplevel *toplevel)
{
  GList *iter;
  int    count;

  count = 0;

  for (iter = geda_toplevel_get_pages(toplevel); iter != NULL; NEXT(iter))
  {
    Page *p_current = (Page*)iter->data;

    if (p_current->do_autosave_backup == 0) {
      continue;
    }

    if (p_current->ops_since_last_backup != 0) {

      char *real_filename;

      /* Get the real filename and file permissions */
      real_filename = geda_file_sys_follow_symlinks (p_current->filename, NULL);

      count++;

      if (real_filename == NULL) {
        geda_log_s ("%s %s.", _("Cannot get the real filename of"), p_current->filename);
      }
      else {
              GError *err;
              char   *dirname;
        const char   *only_filename;
              char   *backup_filename;
              mode_t  saved_umask;
              mode_t  mask;
              struct  stat st;

        /* Get the directory in which the real filename lives */
        dirname         = geda_get_dirname (real_filename);
        only_filename   = geda_file_get_basename(real_filename);

        backup_filename = geda_sprintf("%s%c"AUTOSAVE_BACKUP_FILENAME_STRING,
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
        GEDA_FREE (real_filename);

        /* Make the backup file writable before saving a new one */
        if (g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
           (!g_file_test (backup_filename, G_FILE_TEST_IS_DIR)))
        {
          saved_umask = umask(0);
          if (chmod(backup_filename, (S_IWRITE|S_IWGRP|S_IWOTH) &
                    ((~saved_umask) & 0777)) != 0) {
            geda_log_w (_("Could NOT set previous backup file [%s] read-write\n"),
                           backup_filename);
          }
          umask(saved_umask);
        }

        err = NULL;

        if (geda_object_save (geda_struct_page_get_objects (p_current), backup_filename, &err)) {

          geda_log_v ("%s [%s]\n", _("Automatic backup file saved"), backup_filename);

          p_current->ops_since_last_backup = 0;
          p_current->do_autosave_backup = 0;

          /* Make the backup file readonly so a 'rm *' command will ask
             the user before deleting it */
          saved_umask = umask(0);
          mask  = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
          mask  = (~mask)&0777;
          mask &= ((~saved_umask) & 0777);
          if (chmod(backup_filename,mask) != 0) {
            geda_log_w (_("Could NOT set backup file [%s] readonly\n"),
                           backup_filename);
          }
          umask(saved_umask);
        }
        else {
          geda_log_w ("%s [%s]: %s\n", _("Could NOT save backup file"),
                            backup_filename, err->message);
          g_clear_error (&err);
        }
        GEDA_FREE (backup_filename);
      }
    }
  }
}

/*!
 * \brief Save a series of objects into a string buffer
 * \par Function Description
 *  Recursively saves a set of objects into a buffer in libgeda format.
 *  User code should not normally call this function; use geda_object_save_buffer()
 *  instead.
 *
 *  If \a save_attribs is FALSE, attribute objects are skipped over and are
 *  saved separately - after the objects they are attached to. When recursing
 *  to save out skipped attributes, this function should be called with
 *  \a save_attribs TRUE.
 *
 * \param [in] object_list   The head of a GList of objects to save.
 * \param [in] save_attribs  Should encountered attribute objects be saved?
 *
 * \returns a buffer containing schematic data or NULL on failure.
 */
char *
geda_object_save_objects (const GList *object_list, bool save_attribs)
{
  const  GList *iter;
  char  *out;
  char  *acc           = GEDA_MEM_ALLOC(IO_BUFFER_SLICE_SIZE);
  int    allocated     = IO_BUFFER_SLICE_SIZE;
  int    acc_size      = 0;
  bool   already_wrote = FALSE;

  void string_append (char *str) {

    int len, new_size;

    len      = strlen(str);
    new_size = acc_size + len;

    if (new_size >= allocated) {

      char *buffer;

      /* Check if what is being added is large than the slice size */
      if (len > IO_BUFFER_SLICE_SIZE) {

        /* Request allocate len + 1/2 the slice */
        allocated = allocated + new_size + IO_BUFFER_SLICE_SIZE / 2;
      }
      else {

        /* Need less than slice so request a slice */
        allocated = allocated + IO_BUFFER_SLICE_SIZE;
      }

      /* Request increase allocation */
      buffer = (char*)realloc(acc, allocated);

      if (!buffer) {
        return;
      }

      /* Update pointer is case block was relocated */
      acc = buffer;
    }

    /* Append argument starting at acc_size */
    strncat(&acc[acc_size], str, len); /* use offset forward */

    /* Update the index to the new tail */
    acc_size = new_size;

    return;
  }

  if (!acc) {
    return NULL;
  }

  acc[0] = '\0';
  iter   = object_list;

  while ( iter != NULL ) {

    GedaObject *o_current = GEDA_OBJECT(iter->data);

    if (!o_current)
      continue;

    if (save_attribs || o_current->attached_to == NULL) {

      switch (o_current->type) {

        case(OBJ_ARC):
          out = geda_arc_object_to_buffer(o_current);
          break;

        case(OBJ_BOX):
          out = geda_box_object_to_buffer(o_current);
          break;

        case(OBJ_COMPLEX):
          out = geda_complex_object_to_buffer(o_current);
          string_append(out);
          string_append("\n");
          already_wrote = TRUE;
          GEDA_FREE(out); /* need to free here because of the above flag */

          if (geda_complex_object_is_embedded(o_current)) {

            string_append("[\n");
            out = geda_object_save_objects(o_current->complex->prim_objs, FALSE);
            if (out) {
              string_append (out);
              GEDA_FREE(out);
            }
            string_append("]\n");
          }
          break;

        case(OBJ_PICTURE):
          out = geda_picture_object_save(o_current);
          break;

        case(OBJ_PATH):
          out = geda_path_object_to_buffer(o_current);
          break;

        case(OBJ_LINE):
          out = geda_line_object_to_buffer(o_current);
          break;

        case(OBJ_NET):
          out = geda_net_object_to_buffer(o_current);
          break;

        case(OBJ_PIN):
          out = geda_pin_object_save(o_current);
          break;

        case(OBJ_TEXT):
          out = geda_text_object_save(o_current);
          break;

        case(OBJ_BUS):
          out = geda_bus_object_to_buffer(o_current);
          break;

        case(OBJ_CIRCLE):
          out = geda_circle_object_to_buffer(o_current);
          break;

        case(OBJ_PLACEHOLDER):  /* new type by SDB 1.20.2005 */
          out = geda_complex_object_to_buffer(o_current);
          break;

        default:
          /*! Continue instead of just failing completely! Save as much of
           *  the user's data as possible, in any case, failing gracefully
           *  is better than killing the program, which is what this used
           *  to do... */
          g_critical ("%s: object %p has unknown type '%c'\n",
                      __func__, o_current, o_current->type);
          continue;
      }

      /* output the line */
      if (!already_wrote) {
        string_append(out);
        string_append ("\n");
        GEDA_FREE(out);
      }
      else {
        already_wrote = FALSE;
      }

      /* save any attributes */
      if (o_current->attribs != NULL) {

        string_append ("{\n");
        out = geda_object_save_objects (o_current->attribs, TRUE);
        if (out) {
          string_append (out);
          GEDA_FREE(out);
        }
        string_append ("}\n");
      }
    }

    iter = g_list_next (iter);
  }
  return acc;
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
char *
geda_object_save_buffer (const GList *object_list)
{
  const char *header;
        char *acc;
        char *buffer;
        int   size;

  header = geda_file_get_format_header();
  buffer = geda_object_save_objects (object_list, FALSE);
  size   = strlen(header) + strlen(buffer);
  acc    = GEDA_MEM_ALLOC(size + 1);

  if (!acc) {
    geda_log_s ("%s: %s", __func__, _("Memory allocation error."));
    acc = NULL;
  }
  else {
    acc = strcat (strcpy (acc, header), buffer);
  }

  GEDA_FREE (buffer);

  return acc;
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
geda_object_save (const GList *object_list, const char *filename, GError **err)
{
  char *path;
  int   result;

  errno  = 0;
  result = 0;
  path   = geda_get_dirname(filename);

  /* Check to see if path is writable */
  if (access(path, W_OK) != 0) {
    g_set_error (err, EDA_ERROR, errno, "[%s]: %s %s",
                 path, _("because"), strerror(errno));
  }
  else {

    FILE *output;

    errno  = 0;
    output = fopen (filename, "w" );

    if (!output) {
      g_set_error (err, EDA_ERROR, errno, "%s <%s>: %s", _("file"),
                   filename, strerror(errno));
    }
    else {

      char *buffer = geda_object_save_buffer (object_list);

      if (buffer) {

        fputs(buffer, output);

        GEDA_FREE (buffer);

        result = 1;
      }
      fclose(output);
    }
  }
  GEDA_FREE (path);
  return result;
}
