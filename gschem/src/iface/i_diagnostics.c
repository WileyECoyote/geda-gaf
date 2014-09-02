#include "x_dialog.h"
#include <geda_dialog_controls.h>
#include <geda_widgets.h>

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
          (strncmp(object->text->string, symver, sizeof(symver)) == 0))
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
  /*gtk_window_iconify (GTK_WINDOW(w_current->main_window));*/

  getrusage(RUSAGE_SELF, &before);
  for( i = 0; i < attempts; i++) {
    o_undo_callback(w_current, UNDO_ACTION);
  }
  getrusage(RUSAGE_SELF, &after);

  /*gtk_window_deiconify (GTK_WINDOW(w_current->main_window));*/

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
  GtkWidget *entry;
  GtkWidget *label;
  GtkWidget *vbox;
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
  label = geda_aligned_label_new (_("Program:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 1,4,0,1, GTK_FILL,0,0,0);

  SetEntryText(entry, getenv("_"));

  label = geda_aligned_label_new (_("Process ID:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 4,5,0,1, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 5,6,0,1, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", prog_pid);
  SetEntryText(entry, string);
  GEDA_FREE(string);

  /* Row 2 */
    label = geda_aligned_label_new (_("Threads:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  /* Row 3 */
  if (getrusage (RUSAGE_SELF, &usage) == 0) {

    label = geda_aligned_label_new (_("Max resident Size:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

    entry = geda_visible_entry_new ( DISABLE, DISABLE);
    gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
    geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    gtk_table_attach(GTK_TABLE(table), entry, 1,2,2,3, GTK_FILL,0,0,0);

    string = u_string_sprintf("%ld", usage.ru_maxrss);
    SetEntryText(entry, string);
    GEDA_FREE(string);
  }

  label = geda_aligned_label_new (_("Max Threads:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 2,3,2,3, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 3,4,2,3, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", MAX_THREADS);
  SetEntryText(entry, string);
  GEDA_FREE(string);

  label = geda_aligned_label_new (_("Max Unused:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 4,5,2,3, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 5,6,2,3, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", MAX_THREADS_UNUSED);
  SetEntryText(entry, string);
  GEDA_FREE(string);

  /* Row 4 */
  int nt = g_thread_pool_get_num_threads (CommandPool);
  int up = g_thread_pool_unprocessed (CommandPool);
  int it = g_thread_pool_get_max_idle_time()/MAX_THREADS_IDLE_TIME;

  label = geda_aligned_label_new (_("Number of threads:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 1,2,3,4, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", nt);
  SetEntryText(entry, string);
  GEDA_FREE(string);

  label = geda_aligned_label_new (_("Pending:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 2,3,3,4, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 3,4,3,4, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", up);
  SetEntryText(entry, string);
  GEDA_FREE(string);

  label = geda_aligned_label_new (_("Max Idle:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 4,5,3,4, GTK_FILL,0,0,0);

  entry = geda_visible_entry_new ( DISABLE, DISABLE);
  gtk_entry_set_has_frame (GTK_ENTRY(entry), FALSE);
  gtk_entry_set_alignment (GTK_ENTRY(entry), 0.5);
  geda_entry_widget_modify_color (entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
  gtk_table_attach(GTK_TABLE(table), entry, 5,6,3,4, GTK_FILL,0,0,0);

  string = u_string_sprintf("%i", it);
  SetEntryText(entry, string);
  GEDA_FREE(string);

  gtk_widget_show_all (dialog);

  r = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  gschem_threads_leave();
  return r;
}