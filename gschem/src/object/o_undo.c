/* -*- C o_undo.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
/*!
 * \file o_undo.c
 * \brief A dialog box for editing Attribute Text objects
 *  Gschem Undo system works in conjunction with the libgeda s_undo module
 *  to provide two post operative non-intelligent mechanisms to recreate
 *  data. A pre-operative system scheduled for 2.0.11 has been pulled-in
 *  and is now scheduled for the 2.0.10 release.
 *
 *  \remarks Just band-aid until new system implemented. WEH band-aid seems
 *           to work well, maybe push new system back out to 2.0.11
 */

#include <gschem.h>
#include <geda_debug.h>

/** \defgroup gschem-undo-module Undo Module
 *  @{ \par This group contains Routines for Undo and Redo.
 */

static int undo_file_index=0;

static char* tmp_path = NULL;

#define UNDO_FILE_PATTERN "%s%cgschem.save%d_%d.sch"

/*! \def UNDO_PADDING
 *   UNDO_PADDING is the additional number of levels or history at which
 *   point the undo stack will be trimmed, and is used as safety to prevent
 *   running out of entries to free */
#define UNDO_PADDING  5

/*! \brief Inititialize Undo System
 *  \par Function Description
 *  This function obtains the process ID and temporary directory path
 *  for later use when using the "disk" type undo, and retrieves user
 *  settings affecting the UNDO system. Undo configuration settings
 *  are saved by x_settings_save_settings.
 */
void o_undo_init(GschemToplevel *w_current)
{
  EdaConfig  *cfg = eda_config_get_user_context ();

  prog_pid = getpid();

  i_var_restore_global_boolean(cfg, "undo-control",  &w_current->undo_control,  TRUE);
  i_var_restore_global_integer(cfg, "undo-levels",   &w_current->undo_levels,   DEFAULT_UNDO_LEVELS);
  i_var_restore_global_boolean(cfg, "undo-panzoom",  &w_current->undo_panzoom,  FALSE);
  i_var_restore_global_boolean(cfg, "undo-preserve", &w_current->undo_preserve, TRUE);
  i_var_restore_global_integer(cfg, "undo-type",     &w_current->undo_type,     UNDO_DISK);

  if (tmp_directory != NULL) {

    const char *msg_cl_tmp;

    msg_cl_tmp  = _("Undo: using temporary directory specified on command-line");

    tmp_path = tmp_directory;
    geda_log_v("%s: <%s>\n", msg_cl_tmp, tmp_path);
  }
  else {

    const char *tmp_tmp;

    tmp_path = NULL;
    tmp_tmp = getenv("TMP");

    if (tmp_tmp != NULL) {
      if ((access(tmp_tmp, R_OK) == 0) && (access(tmp_tmp, W_OK) == 0)) {
        tmp_path = geda_utility_string_strdup(tmp_tmp);
      }
    }
    if (tmp_path == NULL) {
      tmp_tmp = getenv("TMPDIR");
      if ((access(tmp_tmp, R_OK) == 0) && (access(tmp_tmp, W_OK) == 0)) {
        tmp_path = geda_utility_string_strdup(tmp_tmp);
      }
    }
    if (tmp_path == NULL) {
      tmp_tmp = getenv("TEMP");
      if ((access(tmp_tmp, R_OK) == 0) && (access(tmp_tmp, W_OK) == 0)) {
        tmp_path = geda_utility_string_strdup(tmp_tmp);
      }
    }
    if (tmp_path == NULL) {
      tmp_path = geda_utility_string_strdup(g_get_tmp_dir());
    }
  }

  if (w_current->undo_type == UNDO_DISK) {

    if ((access(tmp_path, R_OK) != 0) || (access(tmp_path, W_OK) != 0)) {

      const char *msg_not_rw;
      const char *msg_use_mem;
            char *errmsg;
            char *title;

      msg_not_rw  = _("Directory is not read/writable, check permissions");
      msg_use_mem = _("Auto switching Undo system to type Memory");

      title  = geda_sprintf ("<b>%s</b>\n", msg_use_mem);
      errmsg = geda_sprintf ("%s: %s\n", msg_not_rw, tmp_path);

      titled_pango_warning_dialog (msg_use_mem,  errmsg, _("Gschem Undo System"));

      GEDA_FREE(errmsg);
      GEDA_FREE(title);

      w_current->undo_type = UNDO_MEMORY;
    }
  }
}

/*! \brief Finalize the Undo System
 *  \par Function Description
 *  This function is called by gschem::gschem_quit() at program shutdown
 *  in order to release memory allocated by the Undo system and remove the
 *  temporary files. The individual Undo structures associated with each
 *  page were freed when the pages were destroyed so the only memory to
 *  potentially release is the path to the tmp directory.
 *
 */
void o_undo_finalize(void)
{
  int i;

  for (i = 0 ; i < undo_file_index; i++) {

    char *filename;

    filename = geda_sprintf(UNDO_FILE_PATTERN, tmp_path,
                            DIR_SEPARATOR, prog_pid, i);
    unlink(filename);
    GEDA_FREE(filename);
  }

  GEDA_FREE(tmp_path);
}


/*! \brief Undo Save State
 *  \par Function Description
 *   This is a multipurpose function with two main purposes:
 *
 *   1. o_undo_savestate is part of the automatic backup system. In this
 *      role, calls geda_object_save_auto_backup to perform any required
 *      backups. This is done first. Note that o_auto_save_backups could
 *      perform backups on any number of files before returning.
 *
 *      \note WEH: geda_object_save_auto_backup uses o_save, same as us.
 *
 *      \remarks: o_undo_savestate does not currently check if the backup
 *      system is enabled via the auto_save_interval variable, nor does
 *      geda_object_save_auto_backup, nor should they check. o_undo_save
 *      state blindly calls geda_object_save_auto_backup, which backs up
 *      all files flaged by timer geda_struct_page_autosave with do_autosave_backup
 *      AND marked here with ops_since_last_backup if interval is non zero,
 *      but only after the call to geda_object_save_auto_backup.
 *      Sounds hokey huh? The scheme works because a change was just made
 *      or we wouldn't be here, the file will be flaged the next time the
 *      timer counts down, and backed-up the next time a change is made,
 *      that is, o_undo_savestate is called. Any files flaged before the
 *      auto_save_interval setting is changed still get backed-up. So is
 *      not all bad but what if Wiley makes an important change and then
 *      goes for donuts and coffee and forgets to save and comes back to
 *      find his puter won't come out of sleep mode?
 *
 *   2. AFTER satisfying 1 above, the function checks if UNDO control
 *      is enabled and performs the necessary operations to push the
 *      current state onto an undo buffer, aka either the entire file
 *      is saved to the tmp directory or a copy of all of the objects
 *      is saved in memory and referenced in a glist. TODO: In this
 *      capacity the UNDO system is inefficient and makes no attempt
 *      to determine what has been changed in the data.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] flag      integer <B>\a flag</B> can be one of the
 *                        following values:
 *  \par
 *  <DL>
 *    <DT>UNDO_ALL</DT>
 *    <DT>UNDO_VIEWPORT_ONLY</DT>
 *  </DL>
 */
void o_undo_savestate(GschemToplevel *w_current, int flag)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GError       *err      = NULL;

  Page         *p_current;
  UNDO         *u_current;
  UNDO         *u_current_next;

  /* save auto save backups if necessary */
  geda_object_save_auto_backup(w_current->toplevel);

  i_status_update_title (w_current);

  if (w_current->undo_control == FALSE) {
    return;
  }

  p_current = gschem_toplevel_get_current_page (w_current);

  if (p_current == NULL) {
    BUG_MSG("Could not get current page");
  }
  else {

    char *filename = NULL;
    int levels;

    if (flag == UNDO_ALL) {

      /* Temporary until can fix */
      x_pagesel_update (w_current);

      /* Increment the number of operations since last backup if
       *      auto-save is enabled */
      if (toplevel->auto_save_interval != 0) {
        p_current->ops_since_last_backup++;
      }

      /* HACK */
      /* Before we save the undo state, consolidate nets as necessary */

      /* This is where the net consolidation call would have been
       * triggered before it was removed from geda_object_save_buffer().
       */
      if (geda_toplevel_get_net_consolidate(toplevel) == TRUE) {
        geda_net_object_consolidate (toplevel, p_current);
      }

      if (w_current->undo_type == UNDO_DISK) {

        filename = geda_sprintf(UNDO_FILE_PATTERN,
                                tmp_path, DIR_SEPARATOR,
                                prog_pid, undo_file_index++);

        if (!geda_object_save (geda_struct_page_get_objects (p_current), filename, &err))
        {
          const char *file_err_msg = _("Undo: encountered an error, file");
          const char *sys_err_msg1 = _("Undo: system error");
          const char *sys_err_msg2 = _("switching to MEMORY mode");

          /* Error recovery sequence, the last disk operation failed
           * so log the event and switched to type Memory. We do not,
           * and likely can not, remove any existing undo files.*/
          u_log_message("%s=<%s> %s\n", file_err_msg, filename, err->message);
          u_log_message("%s: <%d>, %s\n", sys_err_msg1, err->code, sys_err_msg2);
          g_clear_error (&err);
          w_current->undo_type = UNDO_MEMORY;
          geda_struct_undo_free_all (p_current);
        }
      }
    }

    /* Clear Anything above current */
    if (p_current->undo_current) {
      geda_struct_undo_remove_rest(p_current->undo_current->next);
      p_current->undo_current->next = NULL;
    }
    else { /* undo current is NULL */
      geda_struct_undo_remove_rest(p_current->undo_bottom);
      p_current->undo_bottom = NULL;
    }

    p_current->undo_tos = p_current->undo_current;

    if (w_current->undo_type == UNDO_DISK) {
      p_current->undo_tos = geda_struct_undo_add_disk (flag, filename, p_current);
    }
    else if (w_current->undo_type == UNDO_MEMORY) {
      p_current->undo_tos = geda_struct_undo_add_memory(flag, p_current);
    }

    p_current->undo_current = p_current->undo_tos;

    if (p_current->undo_bottom == NULL) {
      p_current->undo_bottom = p_current->undo_tos;
    }

#if DEBUG_UNDO
    printf("\n\n---Undo----\n");
    geda_struct_undo_print_all (p_current->undo_bottom);
    printf("BOTTOM: %s\n",      p_current->undo_bottom->filename);
    printf("TOS: %s\n",         p_current->undo_tos->filename);
    printf("CURRENT: %s\n",     p_current->undo_current->filename);
    printf("----\n");
#endif

    GEDA_FREE(filename);

    /* Now go through and see if we need to free/remove some undo levels */
    /* so we stay within the limits */

    /* only check history every 10 undo savestates */
    if (undo_file_index % 10) {
      return;
    }

    levels = geda_struct_undo_levels(p_current->undo_bottom);

#if DEBUG_UNDO
    printf("levels: %d\n", levels);
#endif

    if (levels >= w_current->undo_levels + UNDO_PADDING) {
      levels = levels - w_current->undo_levels;

#if DEBUG_UNDO
      printf("Trimming: %d levels\n", levels);
#endif

      u_current = p_current->undo_bottom;

      while (levels > 0) {

        /* Because we use a pad we are always guaranteed to never */
        /* exhaust the list */

        if (u_current == NULL) {
          break;
        }
        u_current_next = u_current->next;

        if (u_current->filename) {
#if DEBUG_UNDO
          printf("Freeing: %s\n", u_current->filename);
#endif
          unlink(u_current->filename);
          GEDA_FREE(u_current->filename);
        }

        if (u_current->object_list) {
          geda_struct_object_release_objects (u_current->object_list);
          u_current->object_list = NULL;
        }

        u_current->next = NULL;
        u_current->prev = NULL;
        GEDA_FREE(u_current);

        u_current = u_current_next;
        levels--;
      }

      if (u_current == NULL) {
        BUG_MSG("u_current == NULL");
      }
      else {
        u_current->prev = NULL;
        p_current->undo_bottom = u_current;

#if DEBUG_UNDO
        printf("New current is: %s\n", u_current->filename);
#endif
      }
    }

#if DEBUG_UNDO
    printf("\n\n---Undo----\n");
    geda_struct_undo_print_all(p_current->undo_bottom);
    printf("BOTTOM: %s\n",     p_current->undo_bottom->filename);
    printf("TOS: %s\n",        p_current->undo_tos->filename);
    printf("CURRENT: %s\n",    p_current->undo_current->filename);
    printf("----\n");
#endif
  }
}

void o_undo_savestate_object(GschemToplevel *w_current,
                             int flag, GedaObject *object)
{
  w_current->which_object = object;
  o_undo_savestate(w_current, flag);
}

/*! \brief Find file name associated wth prev Head
 *  \par Function Description
 *  This function is only used for UNDO_DISK
 */
char *o_undo_find_prev_filename(UNDO *start)
{
  UNDO *u_current;

  u_current = start->prev;

  while(u_current) {
    if (u_current->filename) {
      return(u_current->filename);
    }
    u_current = u_current->prev;
  }

  return(NULL);
}

/*! \brief Finds object list associated prev Head when using Undo Memory
 *  \par Function Description
 *  This function is only used for UNDO_MEMORY
 */
GList *o_undo_find_prev_object_head (UNDO *start)
{
  UNDO *u_current;

  u_current = start->prev;

  while(u_current) {
    if (u_current->object_list) {
      return u_current->object_list;
    }
    u_current = u_current->prev;
  }

  return(NULL);
}

/*! \brief Undo
 *  \par Function Description
 *   The function restores the current page to the previous state.
 *
 *  <B>type</B> can be one of the following values:
 *  \par
 *  <DL>
 *    <DT>UNDO_ACTION</DT>
 *    <DT>REDO_ACTION</DT>
 *  </DL>
 */
void o_undo_callback(GschemToplevel *w_current, int type)
{
  GedaToplevel *toplevel = w_current->toplevel;

  UNDO  *u_current;
  UNDO  *u_next;
  UNDO  *save_bottom;
  UNDO  *save_tos;
  UNDO  *save_current;

  int   find_prev_data=FALSE;
  int   restored;

  volatile int left;
  volatile int right;
  volatile int top;
  volatile int bottom;

  const char *disk_err_msg;

  disk_err_msg = _("An error occurred during an UNDO disk operation");

  if (w_current->undo_control == FALSE) {
    q_log_message(_("Undo/Redo disabled\n"));
    return;
  }

  if (Current_Page->undo_current == NULL) {
    return;
  }

  if (type == UNDO_ACTION) {
    u_current = Current_Page->undo_current->prev;
  }
  else {
    u_current = Current_Page->undo_current->next;
  }

  if (u_current == NULL) {
    return;
  }

  u_next = Current_Page->undo_current;

  if (u_next->type == UNDO_ALL && u_current->type == UNDO_VIEWPORT_ONLY) {

#if DEBUG_UNDO
    printf("Type: %d\n", u_current->type);
    printf("Current is an undo all, next is viewport only!\n");
#endif

    find_prev_data = TRUE;

    if (w_current->undo_type == UNDO_DISK) {
      u_current->filename = o_undo_find_prev_filename(u_current);
    }
    else {
      u_current->object_list = o_undo_find_prev_object_head (u_current);
    }
  }

  /* save UNDO structures so they do not get released */
  save_bottom                = Current_Page->undo_bottom;
  save_tos                   = Current_Page->undo_tos;
  save_current               = Current_Page->undo_current;
  Current_Page->undo_bottom  = NULL;
  Current_Page->undo_tos     = NULL;
  Current_Page->undo_current = NULL;

  if (w_current->undo_preserve) {
      left   = Current_Page->left;
      right  = Current_Page->right;
      top    = Current_Page->top;
      bottom = Current_Page->bottom;
  }

  geda_notify_list_freeze (Current_Page->change_notify_funcs);

  /* Clear the selection list, o_select_unselect_all is not used
   * here because all of the objects are soon to be wiped-out */
  geda_list_remove_all(toplevel->page_current->selection_list);

  if ((w_current->undo_type == UNDO_DISK && u_current->filename) ||
      (w_current->undo_type == UNDO_MEMORY && u_current->object_list))
  {

    geda_struct_place_free_place_list (toplevel);

    geda_struct_page_delete_objects (Current_Page);

  }

  restored = FALSE;

  if (w_current->undo_type == UNDO_DISK && u_current->filename) {

    GError *err          = NULL;

    char *save_filename  = geda_utility_string_strdup (Current_Page->filename);
    int   save_logging   = logging;
    int   old_flags      = toplevel->open_flags;

    logging              = FALSE;   /* temporarily disable logging */
    toplevel->open_flags = F_OPEN_RESTORE_CWD;

    if (geda_open_file(toplevel, Current_Page, u_current->filename, &err)) {
      restored = TRUE;
    }
    else {
      char *title  = geda_sprintf ("<b>%s.</b>.", _("Undo error"));
      char *errmsg = geda_sprintf ("%s: %s.", disk_err_msg, err->message);
      titled_pango_error_dialog(title, errmsg, _("Undo failed"));
      GEDA_FREE(errmsg);
      GEDA_FREE(title);
      g_error_free(err);
      return;
    }

    /* set filename right */
    GEDA_FREE(Current_Page->filename);
    Current_Page->filename = save_filename;

    /* restore logging */
    logging = save_logging;

    toplevel->open_flags = old_flags;
  }
  else if (w_current->undo_type == UNDO_MEMORY && u_current->object_list) {

    geda_struct_page_append_list (Current_Page,
                        geda_copy_list (u_current->object_list, NULL));
    restored = TRUE;
  }

  if (restored) {
      i_window_set_viewport_size(w_current);
      Current_Page->page_control = u_current->page_control;
      Current_Page->hierarchy_up = u_current->hierarchy_up;
      Current_Page->CHANGED      = u_current->modified;
  }

  /* if not undo_panzoom then preserve based on user preference */
  if (!w_current->undo_panzoom && w_current->undo_preserve ) {
    x_window_setup_page(w_current, Current_Page, left, right, top, bottom);
  }
  else {

    x_window_setup_page(w_current, Current_Page,
                        u_current->left, u_current->right,
                        u_current->top,  u_current->bottom);
  }

  x_scrollbars_update(w_current);

  x_pagesel_update (w_current);

  /* double check this */
  x_multiattrib_update (w_current);

  geda_notify_list_thaw (Current_Page->change_notify_funcs);

  /* Let the caller to decide if redraw or not */
  o_invalidate_all (w_current);

  i_status_update_sensitivities (w_current);

  /* The page status may have changed */
  i_status_update_title (w_current);

  /* restore saved undo structures */
  Current_Page->undo_bottom  = save_bottom;
  Current_Page->undo_tos     = save_tos;
  Current_Page->undo_current = save_current;

  if (type == UNDO_ACTION) {
    if (Current_Page->undo_current) {
      Current_Page->undo_current = Current_Page->undo_current->prev;
      if (Current_Page->undo_current == NULL) {
        Current_Page->undo_current = Current_Page->undo_bottom;
      }
    }
  }
  else { /* type is REDO_ACTION */
    if (Current_Page->undo_current) {
      Current_Page->undo_current = Current_Page->undo_current->next;
      if (Current_Page->undo_current == NULL) {
        Current_Page->undo_current = Current_Page->undo_tos;
      }
    }
  }

  /* don't have to free data here since filename, object_list are */
  /* just pointers to the real data (lower in the stack) */
  if (find_prev_data) {
    u_current->filename    = NULL;
    u_current->object_list = NULL;
  }

#if DEBUG_UNDO
  printf("\n\n---Undo----\n");
  geda_struct_undo_print_all(Current_Page->undo_bottom);
  printf("TOS: %s\n",        Current_Page->undo_tos->filename);
  printf("CURRENT: %s\n",    Current_Page->undo_current->filename);
  printf("----\n");
#endif
}

/*! \brief Remove Last Undo Record
 *  \par Function Description
 *  This function supports drag-move and rotations during move
 *  operations by allowing o_move_end to essentially undo the
 *  previous Undo.
 */
void o_undo_remove_last_undo(GschemToplevel *w_current)
{
  if (Current_Page->undo_current == NULL) {
    return;
  }

  if (Current_Page->undo_current) {
    Current_Page->undo_current =
        Current_Page->undo_current->prev;
    if (Current_Page->undo_current == NULL) {
      Current_Page->undo_current =
          Current_Page->undo_bottom;
    }
  }
}
/** @} endgroup gschem-undo-module */