/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_diagnostics.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * 02110-1301 USA
 *
 * Date: July, 13, 2014
 * Contributing Author: Wiley Edward Hill
 *
 */

#include "x_dialog.h"
#include <geda_dialog_controls.h>
#include <geda_widgets.h>

static void
print_page_info(GtkWidget *button, GschemToplevel *w_current)
{
  geda_page_debug_print (Current_Page);
}

static int
o_diagnostics_notifier_one(void *wc, Object *object)
{
  if (object)
    printf( "%s object <%s>\n", __func__, object->name);
  else
    printf( "%s object is NULL\n", __func__);
  return FALSE;
}

static int
o_diagnostics_notifier_two(void *wc, Object *object)
{
 if (object)
    printf( "%s object <%s>\n", __func__, object->name);
  else
    printf( "%s object is NULL\n", __func__);
  return FALSE;
}

static void
add_page_object_notifiers(GtkWidget *button, GschemToplevel *w_current)
{
  o_notify_change_add (Current_Page,
                       o_diagnostics_notifier_one,
                       o_diagnostics_notifier_two, w_current);
}
static void
remove_page_object_notifiers(GtkWidget *button, GschemToplevel *w_current)
{
  o_notify_change_remove (Current_Page,
                          o_diagnostics_notifier_one,
                          o_diagnostics_notifier_two, w_current);
}

/*
static void
o_diagnostics_notify_attribute (GschemToplevel *w_current, Object *object)
{
 if (object)
    printf( "%s object type = <%c>\n", __func__, object->type);
  else
    printf( "%s object is NULL\n", __func__);
}
*/
static float test_draw_time(GschemToplevel *w_current, int attempts)
{

  GdkDisplay   *display;
  struct rusage before;
  struct rusage after;
  float a_cputime, b_cputime, e_cputime;
  float a_systime, b_systime, e_systime;
  int i;

  display = gdk_display_get_default();
  gdk_display_flush(display);

  gtk_window_present (GTK_WINDOW(w_current->main_window));

  getrusage(RUSAGE_SELF, &before);
  for( i = 0; i < attempts; i++) {
    gdk_window_invalidate_rect (w_current->window, NULL, FALSE);
    gdk_window_process_updates(w_current->window, FALSE);
  }
  getrusage(RUSAGE_SELF, &after);

  a_cputime = after.ru_utime.tv_sec + after.ru_utime.tv_usec / 1000000.0;
  b_cputime = before.ru_utime.tv_sec + before.ru_utime.tv_usec / 1000000.0;
  e_cputime = a_cputime - b_cputime;
  a_systime = after.ru_stime.tv_sec + after.ru_stime.tv_usec / 1000000.0;
  b_systime = before.ru_stime.tv_sec + before.ru_stime.tv_usec / 1000000.0;
  e_systime = a_systime - b_systime;

  printf("Redraw %d times, CPU time (secs): user=%.4f; system=%.4f\n",
         attempts, e_cputime, e_systime);

  return e_cputime;
}

static void
test_undo_randomly_delete(GschemToplevel *w_current, int how_many)
{
  GList  *objects;
  Object *object;
  int     index;
  int     total;
  int     num;

  const char *symver = "symver";

  objects = g_list_copy (s_page_get_objects(Current_Page));

  for (index = 0; index < how_many; index++) {
     total  = g_list_length(objects);
     num    = m_random_number(0, total);
     object = g_list_nth_data (objects, num);
     if (GEDA_IS_OBJECT(object)) {
       if (object->type == OBJ_TEXT &&
          (strncmp(object->text->string, symver, 6) == 0))
       {
         /* Almost random, we skip deleting symbol versions attributes
          * because this causes errors, which makes us look bad */
         index--;
       }
       else {
       o_delete(w_current, object);
       objects = g_list_remove (objects, object);
       o_undo_savestate (w_current, UNDO_ALL);
       }
     }
     else {
       index--; /* make sure we delete how ever how_many*/
     }
  }
  g_list_free(objects);
}

/* When conducting Undo performance tests, no other files should be
 * open, otherwise automatic backups could interfere with the tests
 */
static float test_undo_time(GschemToplevel *w_current, int attempts)
{
  struct rusage before;
  struct rusage after;
  float a_cputime, b_cputime, e_cputime;
  float a_systime, b_systime, e_systime;
  bool  old_do_autosave_backup;
  int i;

  /* Temporarily disable backups for the current page */
  old_do_autosave_backup = Current_Page->do_autosave_backup;

  getrusage(RUSAGE_SELF, &before);
  for( i = 0; i < attempts; i++) {
    o_undo_callback(w_current, UNDO_ACTION);
  }
  getrusage(RUSAGE_SELF, &after);

  /* Restore the backup flag to the previous value */
  Current_Page->do_autosave_backup = old_do_autosave_backup;

  a_cputime = after.ru_utime.tv_sec + after.ru_utime.tv_usec / 1000000.0;
  b_cputime = before.ru_utime.tv_sec + before.ru_utime.tv_usec / 1000000.0;
  e_cputime = a_cputime - b_cputime;
  a_systime = after.ru_stime.tv_sec + after.ru_stime.tv_usec / 1000000.0;
  b_systime = before.ru_stime.tv_sec + before.ru_stime.tv_usec / 1000000.0;
  e_systime = a_systime - b_systime;

  printf("Undo %d times, CPU time (secs): user=%.4f; system=%.4f\n",
         attempts, e_cputime, e_systime);

  return e_cputime;
}

int gschem_diagnostics_dialog (GschemToplevel *w_current)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *widget;
  GtkWidget *table;
  GdkColor   bg_color;
  char      *string;
  int r;

  struct rusage usage;

  bg_color.red   = 0xEEEE;
  bg_color.green = 0xEBEB;
  bg_color.blue  = 0xE7E7;

  gschem_threads_enter();

  dialog =  gtk_dialog_new_with_buttons ("GSCHEM Internal Diagnostics",
                                         (GtkWindow*) w_current->main_window,
                                         GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                         "Toggle rusage", TOGGLE_RUSAGE_DATA,
                                         "Redraw Tests", RUN_REDRAW_TESTS,
                                         "Undo Tests", RUN_UNDO_TESTS,
                                         GTK_STOCK_CLOSE,  CLOSE_PERFORMANCE,
                                         NULL);

  vbox = GTK_DIALOG(dialog)->vbox;

  table = gtk_table_new (4, 6, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  /* Row 1 */
  widget = geda_aligned_label_new (_("Program:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 0,1,0,1, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 1,4,0,1, GTK_FILL,0,0,0);

  SetEntryText(widget, getenv("_"));

  widget = geda_aligned_label_new (_("Process ID:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 4,5,0,1, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 5,6,0,1, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", prog_pid);
  SetEntryText(widget, string);
  GEDA_FREE(string);

  /* Row 2 */
    widget = geda_aligned_label_new (_("Threads:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), widget, 0,1,1,2, GTK_FILL,0,0,0);

  /* Row 3 */
  if (getrusage (RUSAGE_SELF, &usage) == 0) {

    widget = geda_aligned_label_new (_("Max resident Size:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), widget, 0,1,2,3, GTK_FILL,0,0,0);

    widget = geda_visible_entry_new ( DISABLE, DISABLE);
    gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
    geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    gtk_table_attach(GTK_TABLE(table), widget, 1,2,2,3, GTK_FILL,0,0,0);

    string = u_string_sprintf("%ld", usage.ru_maxrss);
    SetEntryText(widget, string);
    GEDA_FREE(string);
  }

  widget = geda_aligned_label_new (_("Max Threads:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 2,3,2,3, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 3,4,2,3, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", MAX_THREADS);
  SetEntryText(widget, string);
  GEDA_FREE(string);

  widget = geda_aligned_label_new (_("Max Unused:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 4,5,2,3, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 5,6,2,3, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", MAX_THREADS_UNUSED);
  SetEntryText(widget, string);
  GEDA_FREE(string);

  /* Row 4 */
  int nt = g_thread_pool_get_num_threads (CommandPool);
  int up = g_thread_pool_unprocessed (CommandPool);
  int it = g_thread_pool_get_max_idle_time()/MAX_THREADS_IDLE_TIME;

  widget = geda_aligned_label_new (_("Number of threads:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 0,1,3,4, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 1,2,3,4, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", nt);
  SetEntryText(widget, string);
  GEDA_FREE(string);

  widget = geda_aligned_label_new (_("Pending:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 2,3,3,4, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 3,4,3,4, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", up);
  SetEntryText(widget, string);
  GEDA_FREE(string);

  widget = geda_aligned_label_new (_("Max Idle:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), widget, 4,5,3,4, GTK_FILL,0,0,0);

  widget = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(widget), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(widget), 0.5);
  geda_entry_widget_modify_color (widget, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), widget, 5,6,3,4, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", it);
  SetEntryText(widget, string);
  GEDA_FREE(string);

  /* -------------------------- Table 2 -------------------------- */

  GtkWidget *frame;
  GtkWidget *alignment;

  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME, "label", "Debug data", NULL));
  gtk_box_pack_start(GTK_BOX(vbox), frame, FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        "right-padding",
                                         DIALOG_H_SPACING,
                                        "left-padding",
                                         DIALOG_H_SPACING,
                                        "xscale", 0.0,
                                        "yscale", 0.0,
                                        "xalign", 0.5,
                                        "yalign", 0.5,
                                        NULL));

  gtk_container_add (GTK_CONTAINER (frame), alignment);

  /* Create a second table and put in the alignment */
  table = gtk_table_new (3, 3, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add (GTK_CONTAINER (alignment), table);

  widget = gtk_button_new_with_label ("Print Page Info");
  gtk_table_attach(GTK_TABLE(table), widget, 0,1,0,1, GTK_FILL,0,0,0);
  g_signal_connect(widget, "clicked", G_CALLBACK(print_page_info), w_current);

  widget = gtk_button_new_with_label ("Add Notify Funcs");
  gtk_table_attach(GTK_TABLE(table), widget, 0,1,1,2, GTK_FILL,0,0,0);
  g_signal_connect(widget, "clicked", G_CALLBACK(add_page_object_notifiers), w_current);

  widget = gtk_button_new_with_label ("Remove Notify Funcs");
  gtk_table_attach(GTK_TABLE(table), widget, 0,1,2,3, GTK_FILL,0,0,0);
  g_signal_connect(widget, "clicked", G_CALLBACK(remove_page_object_notifiers), w_current);

  gtk_widget_show_all (dialog);

  r = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  gschem_threads_leave();
  return r;
}