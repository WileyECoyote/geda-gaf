/* -*- C o_undo.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 *  \remarks Just band-aid until new system implemented
 */

#include <gschem.h>
#include <geda_debug.h>

/** \defgroup gschem-undo-module Undo Module
 *  @{ \par This Group contains Routines for Undo and Redo.
 */

static int undo_file_index=0;

static char* tmp_path = NULL;

#define UNDO_FILE_PATTERN "%s%cgschem.save%d_%d.sch"

/* this is additional number of levels (or history) at which point the */
/* undo stack will be trimmed, it's used a safety to prevent running out */
/* of entries to free */
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

  const char *msg_cl_tmp;
  const char *msg_not_rw;
  const char *msg_use_mem;
  const char *tmp_tmp;

  msg_cl_tmp  = _("Undo: using tmp directory specified on command-line: <%s>");
  msg_not_rw  = _("Directory: %s is not read/writable, check permissions.");
  msg_use_mem = _("<b>Auto switching Undo system to type Memory</b>");

  prog_pid = getpid();

  i_var_restore_global_boolean(cfg, "undo-control",  &w_current->undo_control,  TRUE);
  i_var_restore_global_integer(cfg, "undo-levels",   &w_current->undo_levels,   DEFAULT_UNDO_LEVELS);
  i_var_restore_global_boolean(cfg, "undo-panzoom",  &w_current->undo_panzoom,  FALSE);
  i_var_restore_global_boolean(cfg, "undo-preserve", &w_current->undo_preserve, TRUE);
  i_var_restore_global_boolean(cfg, "undo-type",     &w_current->undo_type,     UNDO_DISK);

  if (tmp_directory != NULL) {
    tmp_path = tmp_directory;
    v_log_message(msg_cl_tmp, tmp_path);
  }
  else {

    tmp_path = NULL;
    tmp_tmp = getenv("TMP");

    if (tmp_tmp != NULL) {
      if ((access(tmp_tmp, R_OK) == 0) && (access(tmp_tmp, W_OK) == 0)) {
        tmp_path = u_string_strdup(tmp_tmp);
      }
    }
    if (tmp_path == NULL) {
      tmp_tmp = getenv("TMPDIR");
      if ((access(tmp_tmp, R_OK) == 0) && (access(tmp_tmp, W_OK) == 0)) {
        tmp_path = u_string_strdup(tmp_tmp);
      }
    }
    if (tmp_path == NULL) {
      tmp_tmp = getenv("TEMP");
      if ((access(tmp_tmp, R_OK) == 0) && (access(tmp_tmp, W_OK) == 0)) {
        tmp_path = u_string_strdup(tmp_tmp);
      }
    }
    if (tmp_path == NULL) {
      tmp_path = u_string_strdup(g_get_tmp_dir());
    }
  }
  if (w_current->undo_type == UNDO_DISK) {
    if ((access(tmp_path, R_OK) != 0) || (access(tmp_path, W_OK) != 0)) {
      char *errmsg = u_string_sprintf (msg_not_rw, tmp_path);
      titled_pango_warning_dialog (msg_use_mem,  errmsg, _("Gschem Undo System"));
      GEDA_FREE(errmsg);
      w_current->undo_type = UNDO_MEMORY;
    }
  }
}

/*! \brief Finalize the Undo System
 *  \par Function Description
 *  This function is called by gschem::gschem_quit() at program
 *  shutdown in order to release memory allocated by the Undo
 *  system and remove the temporary files. The individual Undo
 *  structures associated with each page were freed when the
 *  pages were destroyed so the only memory to potentially
 *  release is the path to the tmp directory.
 *
 */
void o_undo_finalize(void)
{
  int i;
  char *filename;

  for (i = 0 ; i < undo_file_index; i++) {
    filename = u_string_sprintf(UNDO_FILE_PATTERN, tmp_path,
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
 *   1. o_undo_savestate is part of the automatic backup system
 *      In this role, calls o_save_auto_backup to perform any
 *      required backups. This is done first. Note that o_auto
 *      save_backups could perform backups on any number of
 *      files before returning.
 *
 *      \note WEH: o_save_auto_backup uses o_save, same as us.
 *
 *      TODO:o_undo_savestate does not currently check if the backup
 *      system is enabled via the auto_save_interval variable, nor does
 *      o_save_auto_backup, nor should they check. o_undo_savestate
 *      blindly calls o_save_auto_backup, which backs up all files flaged
 *      by timer s_page_autosave with do_autosave_backup AND marked here
 *      with ops_since_last_backup if interval is none zero, but after the
 *      call to o_save_auto_backup.
 *      Sounds hokey huh? The scheme works because the change was just made
 *      or we wouldn't be here, the file will be flaged the next time the
 *      timer counts down, and backed-up the next time a change is made,
 *      i mean o_undo_savestate is called. Any files flaged before the
 *      auto_save_interval setting is changed still get backed-up. So is
 *      not all bad but what if Wiley makes an important change and then
 *      goes for donuts and coffee and forgets to save and comes back to
 *      find his puter won't come out of sleep mode?
 *
 *   2. AFTER satisfying 1 above, the function checks if UNDO coontrol
 *      is enabled and performs the necessary operations to push the
 *      current state onto an undo buffer, aka either the entire file
 *      is saved to the tmp directory or a copy of all of the objects
 *      are saved in memory and referenced in a glist. TODO: In this
 *      capacity the UNDO system is inefficient and makes no attempt
 *      to determine what has be changed in the data.
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
  GedaToplevel *toplevel     = w_current->toplevel;
  GError       *err          = NULL;
  char         *filename     = NULL;
  UNDO         *u_current;
  UNDO         *u_current_next;
  int           levels;

  const char *file_err_msg;
  const char *sys_err_msg;
  const char *int_err_msg;

  file_err_msg = _("Undo: encountered an error: file=<%s> %s\n");
  sys_err_msg  = _("Undo: system error: <%d>, switching to MEMORY mode\n");
  int_err_msg  = _("Internal Error: <%s> line <%d> u_current == NULL\n");

  /* save auto save backups if necessary */
  o_save_auto_backup(w_current->toplevel);

  if (w_current->undo_control == FALSE) {
    return;
  }

  if (w_current->toplevel == NULL) {
    BUG_MSG("toplevel == NULL");
  }
  else if (Current_Page == NULL) {
    BUG_MSG("w_current->toplevel->current_page == NULL");
  }
  else {

    if (flag == UNDO_ALL) {

      /* Increment the number of operations since last backup if
       *      auto-save is enabled */
      if (toplevel->auto_save_interval != 0) {
        Current_Page->ops_since_last_backup++;
      }

      /* HACK */
      /* Before we save the undo state, consolidate nets as necessary */

      /* This is where the net consolidation call would have been
       * triggered before it was removed from o_save_buffer().
       */
      if (toplevel->net_consolidate == TRUE)
        o_net_consolidate (toplevel, Current_Page);
    }

    if (w_current->undo_type == UNDO_DISK && flag == UNDO_ALL) {

      filename = u_string_sprintf(UNDO_FILE_PATTERN,
                                  tmp_path, DIR_SEPARATOR,
                                  prog_pid, undo_file_index++);

      if (!o_save (s_page_get_objects (Current_Page), filename, &err)) {
          /* Error recovery sequence, the last disk operation failed
           * so log the event and switched to type Memory. We do not,
           * and likely can not, remove any existing undo files.*/
          u_log_message(file_err_msg, filename, err->message);
          u_log_message(sys_err_msg, err->code);
          g_clear_error (&err);
          w_current->undo_type = UNDO_MEMORY;
          s_undo_free_all (Current_Page);
      }

    }

    /* Clear Anything above current */
    if (Current_Page->undo_current) {
      s_undo_remove_rest(Current_Page->undo_current->next);
      Current_Page->undo_current->next = NULL;
    }
    else { /* undo current is NULL */
      s_undo_remove_rest(Current_Page->undo_bottom);
      Current_Page->undo_bottom = NULL;
    }

    Current_Page->undo_tos = Current_Page->undo_current;

    if (w_current->undo_type == UNDO_DISK) {
      Current_Page->undo_tos = s_undo_add_disk(flag,
                                               filename,
                                               Current_Page);
    }
    else if (w_current->undo_type == UNDO_MEMORY) {
      Current_Page->undo_tos = s_undo_add_memory(flag, Current_Page);
    }

    Current_Page->undo_current = Current_Page->undo_tos;

    if (Current_Page->undo_bottom == NULL) {
      Current_Page->undo_bottom = Current_Page->undo_tos;
    }

#if DEBUG
    printf("\n\n---Undo----\n");
    s_undo_print_all       (Current_Page->undo_bottom);
    printf("BOTTOM: %s\n",  Current_Page->undo_bottom->filename);
    printf("TOS: %s\n",     Current_Page->undo_tos->filename);
    printf("CURRENT: %s\n", Current_Page->undo_current->filename);
    printf("----\n");
#endif

    GEDA_FREE(filename);

    /* Now go through and see if we need to free/remove some undo levels */
    /* so we stay within the limits */

    /* only check history every 10 undo savestates */
    if (undo_file_index % 10) {
      return;
    }

    levels = s_undo_levels(Current_Page->undo_bottom);

#if DEBUG
    printf("levels: %d\n", levels);
#endif

    if (levels >= w_current->undo_levels + UNDO_PADDING) {
      levels = levels - w_current->undo_levels;

#if DEBUG
      printf("Trimming: %d levels\n", levels);
#endif

      u_current = Current_Page->undo_bottom;

      while (levels > 0) {

        /* Because we use a pad we are always guaranteed to never */
        /* exhaust the list */

        if (u_current == NULL) {
          break;
        }
        u_current_next = u_current->next;

        if (u_current->filename) {
#if DEBUG
          printf("Freeing: %s\n", u_current->filename);
#endif
          unlink(u_current->filename);
          GEDA_FREE(u_current->filename);
        }

        if (u_current->object_list) {
          s_object_release_objects (u_current->object_list);
          u_current->object_list = NULL;
        }

        u_current->next = NULL;
        u_current->prev = NULL;
        GEDA_FREE(u_current);

        u_current = u_current_next;
        levels--;
      }

      if (u_current == NULL) {
        u_log_message(int_err_msg, __func__, __LINE__);
      }
      else {
        u_current->prev = NULL;
        Current_Page->undo_bottom = u_current;

#if DEBUG
        printf("New current is: %s\n", u_current->filename);
#endif
      }
    }

#if DEBUG
    printf("\n\n---Undo----\n");
    s_undo_print_all(Current_Page->undo_bottom);
    printf("BOTTOM: %s\n", Current_Page->undo_bottom->filename);
    printf("TOS: %s\n", Current_Page->undo_tos->filename);
    printf("CURRENT: %s\n", Current_Page->undo_current->filename);
    printf("----\n");
#endif
  }
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
 *   The function restores the current page to the previous
 *   state, wiping out any change-notify-funcs in the process.
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
  Page  *p_new;
  UNDO  *u_current;
  UNDO  *u_next;
  UNDO  *save_bottom;
  UNDO  *save_tos;
  UNDO  *save_current;

  int   find_prev_data=FALSE;
  int   pid;

  /* The following varible are initialse to suppress errently gcc warning */
  int left    = left;
  int right   = right;
  int top     = top;
  int bottom  = bottom;

  char *save_filename;
  char *tmp_filename;

  const char *disk_err_msg;

  disk_err_msg = _("An error occurred during an UNDO disk operation: %s.");

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

  u_next = Current_Page->undo_current;

  if (u_current == NULL) {
    return;
  }

  if (u_next->type == UNDO_ALL && u_current->type == UNDO_VIEWPORT_ONLY) {

#if DEBUG
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

  /* save filename */
  save_filename = u_string_strdup (Current_Page->filename);

  /* save UNDO structures so they do not get released */
  save_bottom                = Current_Page->undo_bottom;
  save_tos                   = Current_Page->undo_tos;
  save_current               = Current_Page->undo_current;
  Current_Page->undo_bottom  = NULL;
  Current_Page->undo_tos     = NULL;
  Current_Page->undo_current = NULL;

  /* Set the appropriate file name */
  if (w_current->undo_type == UNDO_DISK && u_current->filename) {
    tmp_filename = u_current->filename;
  }
  else if (w_current->undo_type == UNDO_MEMORY && u_current->object_list) {
    tmp_filename = save_filename;
  }
  else {
    tmp_filename = NULL;
  }

  GedaNotifyList *ptr_notify_funcs;

  if (w_current->undo_preserve) {
      left   = Current_Page->left;
      right  = Current_Page->right;
      top    = Current_Page->top;
      bottom = Current_Page->bottom;
  }

  /* Destory the current page and create a new one */
  if (tmp_filename) {

    ptr_notify_funcs = Current_Page->change_notify_funcs;

    g_object_ref (ptr_notify_funcs);

    geda_notify_list_freeze (ptr_notify_funcs);

    pid = Current_Page->pid;

    s_page_delete (toplevel, Current_Page);

    p_new = s_page_new (toplevel, tmp_filename);

    p_new->pid = pid;

    s_page_goto (toplevel, p_new);

    p_new->change_notify_funcs = ptr_notify_funcs;

  }

  /* temporarily disable logging */
  int  save_logging;
  int  restored;

  save_logging = logging;
  logging      = FALSE;
  restored     = FALSE;

  if (w_current->undo_type == UNDO_DISK && u_current->filename) {

    GError *err          = NULL;
    int old_flags        = toplevel->open_flags;
    toplevel->open_flags = 0;

    if (f_open(toplevel, Current_Page, u_current->filename, &err)) {
      restored = TRUE;
    }
    else {
      char *errmsg = u_string_sprintf (disk_err_msg, err->message);
      titled_pango_error_dialog(_("<b>Undo error.</b>"), errmsg, _("Undo failed"));
      GEDA_FREE(errmsg);
      g_error_free(err);
      return;
    }
    toplevel->open_flags = old_flags;
  }
  else if (w_current->undo_type == UNDO_MEMORY && u_current->object_list) {

    s_page_delete_objects (Current_Page);

    s_page_append_list (Current_Page,
                        o_glist_copy_all (u_current->object_list, NULL));
    restored = TRUE;
  }

  if (restored) {
      x_manual_resize(w_current);
      Current_Page->page_control = u_current->page_control;
      Current_Page->up           = u_current->up;
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

  /* restore logging */
  logging = save_logging;

  /* set filename right */
  GEDA_FREE(Current_Page->filename);
  Current_Page->filename = save_filename;

  /* final redraw */
  x_pagesel_update (w_current);
  x_multiattrib_update (w_current);

  if (tmp_filename) {
    geda_notify_list_thaw (ptr_notify_funcs);
  }

  /* Let the caller to decide if redraw or not */
  o_invalidate_all (w_current);
  i_status_update_sensitivities(w_current);

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

#if DEBUG
  printf("\n\n---Undo----\n");
  s_undo_print_all(Current_Page->undo_bottom);
  printf("TOS: %s\n", Current_Page->undo_tos->filename);
  printf("CURRENT: %s\n", Current_Page->undo_current->filename);
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