/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <geda_debug.h>

static int undo_file_index=0;

static char* tmp_path = NULL;

/* this is additional number of levels (or history) at which point the */
/* undo stack will be trimmed, it's used a safety to prevent running out */
/* of entries to free */
#define UNDO_PADDING  5

/*! \brief Inititialize Undo System
 *  \par Function Description
 *  This function obtains the process ID and temporary directory path for
 *  later use, and retrieves user settings affecting the UNDO system.
 */
void o_undo_init(GschemToplevel *w_current)
{
  EdaConfig *cfg = eda_config_get_user_context ();

  prog_pid = getpid();

  i_var_restore_global_boolean(cfg, "undo-control", &w_current->undo_control, TRUE);
  i_var_restore_global_integer(cfg, "undo-levels",  &w_current->undo_levels,  DEFAULT_UNDO_LEVELS);
  i_var_restore_global_boolean(cfg, "undo-panzoom", &w_current->undo_panzoom, FALSE);
  i_var_restore_global_boolean(cfg, "undo-type",    &w_current->undo_type,    UNDO_DISK);

  tmp_path = u_string_strdup (g_getenv("TMP"));
  if (tmp_path == NULL) {
     tmp_path = u_string_strdup ("/tmp");
  }
#if DEBUG
  printf("%s\n", tmp_path);
#endif

}

/*! \brief Undo Save State
 *  \par Function Description
 *   This function is called to push the current state onto the
 *   undo buffer.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] flag      integer <B>\a flag</B> can be one of the
 *                       following values:
 *  <DL>
 *    <DD>UNDO_ALL</DD>
 *    <DD>UNDO_VIEWPORT_ONLY</DD>
 *  </DL>
 */
void o_undo_savestate(GschemToplevel *w_current, int flag)
{
  GedaToplevel *toplevel     = w_current->toplevel;
  GError       *err          = NULL;
  GList        *object_list  = NULL;
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

  /* save autosave backups if necessary */
  o_autosave_backups(w_current);

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

      filename = g_strdup_printf("%s%cgschem.save%d_%d.sch",
                                 tmp_path, DIR_SEPARATOR,
                                 prog_pid, undo_file_index++);

      if (!o_save (s_page_get_objects (Current_Page), filename, &err)) {
          u_log_message(file_err_msg, filename, err->message);
          u_log_message(sys_err_msg, err->code);
          g_clear_error (&err);
          w_current->undo_type = UNDO_MEMORY;
          s_undo_free_all (Current_Page);
          object_list = o_glist_copy_all (s_page_get_objects (Current_Page),
                                          object_list);
      }

    }
    else if (w_current->undo_type == UNDO_MEMORY && flag == UNDO_ALL) {
      object_list = o_glist_copy_all (s_page_get_objects (Current_Page),
                                      object_list);
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

    Current_Page->undo_tos = s_undo_add(Current_Page->undo_tos,
                                        flag, filename, object_list,
                                        Current_Page->left,
                                        Current_Page->top,
                                        Current_Page->right,
                                        Current_Page->bottom,
                                        Current_Page->page_control,
                                        Current_Page->up);

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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  <B>type</B> can be one of the following values:
 *  <DL>
 *    <DT>*</DT><DD>UNDO_ACTION
 *    <DT>*</DT><DD>REDO_ACTION
 *  </DL>
 */
void o_undo_callback(GschemToplevel *w_current, int type)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Page *p_new;
  UNDO *u_current;
  UNDO *u_next;
  UNDO *save_bottom;
  UNDO *save_tos;
  UNDO *save_current;
  int save_logging;
  int find_prev_data=FALSE;
  char *save_filename;
  char *tmp_filename;

  if (w_current->undo_control == FALSE) {
    q_log_message(_("Undo/Redo disabled\n"));
    return;
  }

  if (Current_Page->undo_current == NULL) {
    return;
  }

  if (type == UNDO_ACTION) {
    u_current = Current_Page->undo_current->prev;
  } else {
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
    } else {
      u_current->object_list = o_undo_find_prev_object_head (u_current);
    }
  }

  /* save filename */
  save_filename = u_string_strdup (Current_Page->filename);

  /* save structure so it's not nuked */
  save_bottom = Current_Page->undo_bottom;
  save_tos = Current_Page->undo_tos;
  save_current = Current_Page->undo_current;
  Current_Page->undo_bottom = NULL;
  Current_Page->undo_tos = NULL;
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

  /* Distroy the current page and create a new one */
  if (tmp_filename) {

    s_page_delete (toplevel, Current_Page);

    p_new = s_page_new (toplevel, tmp_filename);

    s_page_goto (toplevel, p_new);

    /* Damage notifications should invalidate the object on screen */
    o_add_change_notify (p_new,
                        (ChangeNotifyFunc) o_invalidate_object,
                        (ChangeNotifyFunc) o_invalidate_object, w_current);
  }

  /* temporarily disable logging */
  save_logging = logging;
  logging = FALSE;

  if (w_current->undo_type == UNDO_DISK && u_current->filename) {

    GError *err = NULL;
    int old_flags = toplevel->open_flags;
    toplevel->open_flags = 0;

    if(f_open(toplevel, Current_Page, u_current->filename, &err)) {
      x_manual_resize(w_current);
      Current_Page->page_control = u_current->page_control;
      Current_Page->up = u_current->up;
      Current_Page->CHANGED=1;
    }
    else {
      char *errmsg = g_strdup_printf ( _("An error occurred during an UNDO disk operation: %s."), err->message);
      titled_pango_error_dialog ( _("<b>Undo error.</b>"), errmsg, _("Undo failed") );
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

    x_manual_resize(w_current);
    Current_Page->page_control = u_current->page_control;
    Current_Page->up = u_current->up;
    Current_Page->CHANGED=1;
  }

  /* do misc setups */
  x_window_setup_page(w_current, Current_Page,
                      u_current->left, u_current->right,
                      u_current->top, u_current->bottom);

  x_hscrollbar_update(w_current);
  x_vscrollbar_update(w_current);

  /* restore logging */
  logging = save_logging;

  /* set filename right */
  GEDA_FREE(Current_Page->filename);
  Current_Page->filename = save_filename;

  /* final redraw */
  x_pagesel_update (w_current);
  x_multiattrib_update (w_current);

  /* Let the caller to decide if redraw or not */
  o_invalidate_all (w_current);
  i_status_update_sensitivities(w_current);

  /* restore saved undo structures */
  Current_Page->undo_bottom  = save_bottom;
  Current_Page->undo_tos     = save_tos;
  Current_Page->undo_current = save_current;

  if (type == UNDO_ACTION) {
    if (Current_Page->undo_current) {
      Current_Page->undo_current =
          Current_Page->undo_current->prev;
      if (Current_Page->undo_current == NULL) {
        Current_Page->undo_current =
            Current_Page->undo_bottom;
      }
    }
  }
  else { /* type is REDO_ACTION */
    if (Current_Page->undo_current) {
      Current_Page->undo_current =
          Current_Page->undo_current->next;
      if (Current_Page->undo_current == NULL) {
        Current_Page->undo_current =
            Current_Page->undo_tos;
      }
    }
  }

  /* don't have to free data here since filename, object_list are */
  /* just pointers to the real data (lower in the stack) */
  if (find_prev_data) {
    u_current->filename = NULL;
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_undo_finalize(void)
{
  int i;
  char *filename;

  for (i = 0 ; i < undo_file_index; i++) {
    filename = g_strdup_printf("%s%cgschem.save%d_%d.sch", tmp_path,
                               DIR_SEPARATOR, prog_pid, i);
    unlink(filename);
    GEDA_FREE(filename);
  }

  GEDA_FREE(tmp_path);
  tmp_path = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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
