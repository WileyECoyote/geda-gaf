/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
 * Copyright (C) 2012-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * \mainpage
 *
 * \section sdb_notes SDB's original comment in gattrib.c
 *
 * In the spirit of open source/free software, major sections of gattrib's
 * code were borrowed from other sources, and hacked together by SDB in
 * Dec. 2003.  Particularly rich sources for code were gEDA/gnetlist, and
 * the gtkextra program testgtksheet.c. Thanks to their authors for
 * providing the foundation upon which gattrib is built.
 *
 * Of course, I *did* write major portions of the code too . . . . .
 * Some documentation about the internal operation of this program can be
 * found in the "NOTES" file  in the gattrib top-level directory.
 * -- SDB  December 2003 -
 *
 * \section ml_notes Architecture
 *
 * (extracted from SDB's mailing list posting:
 *  http://osdir.com/ml/cad.geda.devel/2007-06/msg00282.html - believed to
 *  still be relevant)
 *
 * gattrib has three major components:
 *
 * -# It manipulates objects held in the GedaToplevel data structure. It
 *    does this by importing structs and functions from libgeda.
 * -# Gattrib defines its own layer of structs, notably PageDataSet,
 *    which holds a table of attrib name=value pairs, and also holds a
 *    couple of linked lists corresponding to all component's refdeses, and
 *    to all attribute names found in the design. This stuff is native to
 *    gattrib.
 * -# Gattrib uses a spreadsheet widget called GtkSheet. This stuff
 *    came from the GtkExtra project, which at one time offered a bunch of
 *    interesting widgets for graphing and visualization. I think they're
 *    still around; you can do a Google search for them. I stole the two
 *    .h files defining the spreadsheet widgets, and also stole code from
 *    the program itself to implement the run-time functions which deal with
 *    the spreadsheet.
 *
 * When run, gattrib does this:
 *
 * -# Uses libgeda functions to read in your design, and fill up the
 *    GedaToplevel struct.
 * -# Loops over everything in GedaToplevel and fills out the refdes
 *    list and the attribute name list. It sticks these into a STRING_LIST
 *    which is associated with the PageDataSet struct.
 * -# With all the refdeses and all the attribute names in list, gattrib
 *    then creates a TABLE data struct (a member of PageDataSet), and loops
 *    over each cell in the TABLE. For each cell, it queries GedaToplevel
 *    for the corresponding name=value pair, and sticks the value in the
 *    TABLE.
 * -# After the data is loaded into the tables, a GtkSheet is creates and
 *    populated with the data in the TABLE.
 * -# Then it turns over control to the user, who can manipulate the GtkSheet.
 *    As the user adds and deletes values from the GtkSheet, the values are
 *    stored locally there. The GtkSheet is the master repository of all
 *    attributes at that point; the other data structures are not updated.
 *
 *    Saving out a design is similar, except the process runs in reverse:
 *
 * -# The program loops over all cells in GtkSheet, and sticks the
 *    values found into PageDataSet. Handling issues like added/deleted
 *    columns happens first at the GtkSheet, and then to PageDataSet and
 *    GedaToplevel. I've kind of forgotten how I implemented these feaures,
 *    however. :-S
 * -# Then, the program loops over the cells in PageDataSet, and updates
 *    the attributes in GedaToplevel using functions from libgeda, as well as
 *    by reaching directly into the GedaToplevel data structure (a software
 *    engineering no-no). If a previously existing attrib has been removed,
 *    then it is removed from GedaToplevel. If a new attrib has been attached
 *    to a component, then it is added to GedaToplevel.
 * -# Then the design is saved out using the save function from
 *    libgeda.
 *
 * Therefore, think of PageDataSet and the other gattrib data structures
 * as a thin layer between GtkSheet and GedaToplevel. The gattrib data
 * structures are used basically for convenience while trying to build or
 * update either of the two other, more important data structures.
 *
 */
#define GLIB_DISABLE_DEPRECATION_WARNINGS
#include "../../config.h"
#include "../../version.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*------------------------------------------------------------------
 * Includes originally from testgtksheet -- stuff needed to deal with
 * spreadsheet widget.
 *------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include <glib.h>
#include <glib-object.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific includes -- stuff dealing with gattrib data structs.
 *------------------------------------------------------------------*/
#include "../include/gattrib.h"  /* include Gattrib specific headers  */

#include <geda_keysyms.h>
#include <libgedauio.h>

typedef struct {
  void (*func)(void*);
  void* arg;
} geda_atexit_struct;

static GList *exit_functions = NULL;

/*!
 * \brief Register a function to be called on program exit
 * \par Function Description
 *  This function registers a function to be called on
 *  program exit. Multiple functions will be executed in
 *  the order they are registered.
 *
 * \param [in] func  pointer to the function to be registered
 * \param [in] data  an arbitrary argument provided to the function
 *                   when it is called
 */
void geda_atexit(geda_atexit_func func, void* data)
{
  geda_atexit_struct *p;

  p = g_malloc(sizeof(geda_atexit_struct));
  p->func = func;
  p->arg = data;
  exit_functions = g_list_append(exit_functions, p);
}

/*------------------------------------------------------------------*/

/*!
 * \brief Save user config on exit.
 * \par Function Description
 *  Try to save the user configuration to disk when gattrib exits.
 */
void gattrib_save_user_config (void)
{
  EdaConfig *cfg = eda_config_get_user_context ();
  GError    *err = NULL;

  eda_config_save (cfg, &err);

  if (err != NULL) {
    fprintf (stderr, "%s '%s': %s.", _("Failed to save user configuration to"),
             eda_config_get_filename (cfg), err->message);
    g_clear_error (&err);
  }
}

/*------------------------------------------------------------------*/

/*!
 * \brief Quit the program.
 * \par Function Description
 *  Unconditionally quit gattrib. Flushes caches and I/O channels,
 *  calls the GTK function to quit the application then calls exit()
 *  with the appropriate return code.
 *
 * \param return_code Value to pass to the exit() system call.
 */
int gattrib_quit(int return_code)
{
  GList *list;

  /* Call all registered functions in order */
  list = exit_functions;

  while (list != NULL) {

    geda_atexit_struct *p;

    p = list->data;
    p->func(p->arg);
    GEDA_FREE(p);

    list = g_list_next(list);
  }

  g_list_free(exit_functions);

  /* Currently the search history is not retained between sessions */
  if (search_history) {
    geda_glist_free_all(search_history);
  }

  s_toplevel_close(sheet_head);

  x_gtksheet_destroy_all();

  x_window_release_all();

  x_find_save_search_setting();

  gattrib_save_user_config();

  i_vars_release_all();

  /* Shutdown libgeda */
  libgeda_release();

  /* s_rename_destroy_all(); */

#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_quit, calling gtk_main_quit()\n");
#endif

  gtk_main_quit();

#if DEBUG_GEDA_LABEL
  /* This can be helpful in identifying unreleased resources */
  geda_label_report_instances();
#endif

  exit(return_code);
}

/*------------------------------------------------------------------*/

/*!
 * \brief Callback to quit the program.
 * \par Function Description
 *  This is called when the user quits the program using the UI.
 *  The callback is attached to the GTK window_delete event in
 *  x_window_init() and attached to the File->Quit menu item in
 *  x_window_create_menu().  On execution, the function checks
 *  for unsaved changes before calling gattrib_quit() to quit
 *  the program.
 *
 * \return value 0 to the shell to denote a successful quit.
 */
bool gattrib_really_quit(void)
{
  if (sheet_head->CHANGED == TRUE) {
    x_dialog_unsaved_data();
  }
  else {
    gattrib_quit(0);
  }

  return TRUE;
}

/*------------------------------------------------------------------*/

/*!
 * \brief The "real" main for gattrib.
 * \par Function Description
 *  This is the main program body for gattrib. A pointer to this
 *  function is passed to scm_boot_guile() at startup.
 *
 *  This function:
 *  - initialises threading, if the underlying GTK library is threaded.
 *    However, gattrib itself is not threaded.
 *  - initialises libgeda;
 *  - parses the command line;
 *  - starts logging;
 *  - registers the Scheme functions with Guile;
 *  - parses the RC files;
 *  - initialises the GTK UI;
 *  - populates the spreadsheet data structure;
 *  - calls gtk_main() to start the event loop.
 *
 * \param closure
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
void gattrib_main(void *closure, int argc, char *argv[])
{
  /* GedaToplevel *pr_current is a global   */
  /* PageDataSet *sheet_head is a global */
  /* GtkWidget *main_window is a global */

  int argv_index;
  GSList *file_list = NULL;

#if !GLIB_CHECK_VERSION(2, 44, 0)
  geda_utility_program_mem_set_vtable();
#endif

#if ((GLIB_MAJOR_VERSION >= 2 ) && (GLIB_MINOR_VERSION <= 33 ))
  g_slice_set_config(G_SLICE_CONFIG_ALWAYS_MALLOC, 1);
#endif

#ifdef HAVE_GTHREAD

  /* Gattrib is not threaded, but some of GTK's file chooser
   * backends uses threading so we need to call g_thread_init().
   * GLib requires threading be initialized before any other GLib
   * functions are called. Do it now if its not already setup.  */

#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_thread_init (NULL);
#endif

#ifndef OS_WIN32_NATIVE

  if (g_thread_supported ()) {
    gdk_threads_init();
    gdk_threads_enter ();
  }

#endif /* ! OS_WIN32_NATIVE*/
#endif /* HAVE_GTHREAD */

#if ENABLE_NLS

  /* This should be equivalent to gdk_set_locale() */
  setlocale (LC_ALL, "");

  /* This must be the same for all locales, use decimal point instead
   * of comma, use "C" or "POSIX" */
  setlocale(LC_NUMERIC, "C");

  /* Prevent gtk_init() and gtk_init_check() from automatically
   * calling setlocale (LC_ALL, "") which would undo our LC_NUMERIC. */
  gtk_disable_setlocale();

# if DEBUG

  fprintf(stderr, "Configured locale directory: %s\n", LOCALEDIR);
  fprintf(stderr, "Current locale settings: %s\n", setlocale(LC_ALL, NULL));

# endif

#endif  /* ENABLE_NLS */

  /* This is called before libgeda_init so g_get_prgname returns "gattrib" */
  gtk_init(&argc, &argv);

  /* Initialize gEDA stuff */
  libgeda_init(argc, argv);
  libgedauio_setup_locale();

  export_mode  = 0;
  verbose_mode = FALSE;
  quiet_mode   = FALSE;

  /* Note that argv_index holds index to first non-flag command line option
   * (that is, to the first file name) */
  argv_index = parse_commandline(argc, argv);

  /* register guile (scheme) functions, this is necessary to parse RC file */
  g_register_funcs();

  /* ----- Read in RC files.   ----- */
  g_rc_parse (argv[0], "gattribrc", NULL);

  /* ----------  create log file right away ---------- */
  geda_utility_log_init ("gattrib");
  geda_log ("gEDA/gattrib version %s%s.%s\n", PREPEND_VERSION_STRING,
                                              PACKAGE_DOTTED_VERSION,
                                              PACKAGE_DATE_VERSION);

  /* Start creation of new project: (GedaToplevel *pr_current) */
  pr_current = geda_toplevel_new(); /* geda_toplevel_new is in Libgeda */

  i_vars_set(pr_current);

  x_window_init();

  /* Construct the list of filenames from the command line.
   * argv_index holds the position of the first filename  */
  while (argv_index < argc) {

    char *filename = geda_normalize_filename(argv[argv_index], NULL);

    if (filename != NULL) {
      file_list = g_slist_append(file_list, filename);
    }
    else {
      fprintf(stderr, "%s \"%s\"\n", _("Could not find file"), argv[argv_index]);
    }
    argv_index++;
  }

  /* ---------- Initialize PageDataSet data structure ---------- */
  sheet_head = s_sheet_data_new();   /* sheet_head is declared in globals.h */

  if (file_list) { /* do we need to call g here? */

    /* Attempt to Load the files */
    if (x_fileselect_load_files(file_list)) {
      /* Sort, Load Tables, Verify Design- Really? */
      s_toplevel_init_data_set(pr_current, sheet_head);
    }
    else {

    /* There was at least 1 error opening files, and we do not know
       which one, so clear all and start a blank page */
       geda_struct_page_delete (pr_current, pr_current->page_current, FALSE);
       x_window_blank_document(pr_current, sheet_head);
    }

    geda_gslist_free_all(file_list);
  }
  else {
    x_window_blank_document(pr_current, sheet_head);
  }

  /* create a new gtksheet having dimensions sheet_head->comp_count */
  x_gtksheet_init(sheet_head);

  /* -------------- Complete Remaining Windows Stuff ------------- */
  x_window_finalize_startup((GtkWindow*)main_window, sheet_head);

  x_find_restore_search_setting();

  if (export_mode) {

    const char *filename;
    char *basename;

    filename = geda_page_get_filename(pr_current->page_current);
    basename = geda_get_basename_dup(filename);
    filename = geda_strsubst(basename, ".sch", ".csv");

    f_export_components(filename);

    if (verbose_mode) {
       fprintf(stderr,"%s: \"%s\"\n",  _("Exported schematic to"), filename);
    }

    GEDA_FREE(basename);
  }
  else {

      /* enter main loop */
      gtk_main();

  }

  exit(0);
}

/*!
 * \brief Entry point to gattrib
 * \par Function Description
 *  This is a wrapper which invokes guile to the real main
 *  program, gattrib_main, after possibly setting up NLS.
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
int main(int argc, char *argv[])
{

#if ENABLE_NLS

  setlocale(LC_ALL, "");
  setlocale(LC_NUMERIC, "C");
  bindtextdomain(GATTRIB_GETTEXT_DOMAIN, LOCALEDIR);
  textdomain(GATTRIB_GETTEXT_DOMAIN);
  bind_textdomain_codeset(GATTRIB_GETTEXT_DOMAIN, "UTF-8");

#endif

  /* Initialize the Guile Scheme interpreter. This function does not
   * return but calls exit(0) on completion.
   */
  scm_boot_guile( argc, argv, gattrib_main, NULL);

  exit(0);   /* This is not real exit point.  Real exit is in gattrib_quit. */
}
