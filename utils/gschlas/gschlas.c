/*!
 * \file gschlas.c
 *
 * \brief Main Module for gschlas Program
 *
 * <hr>
 *
 * <h1><b>Copyright.</b></h1>\n
 * gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 *
 * Copyright (C) 2002-2015 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors (see ChangeLog for details)
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
 * along with this program; if  not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include "../include/common.h"

/*! \brief Cleanup gschlas on exit.
 *  \par Function Description
 *  This function is called at exit to call the necessary subroutines
 *  to release program resource.
 */
void gschlas_quit(void)
{
  libgeda_release();
}

/*! \brief The "real" main for gschlas.
 *
 * This is the main program body for gschlas. A pointer to this
 * function is passed to scm_boot_guile() at startup.
 *
 * This function:
 * - initialises libgeda;
 * - parses the command line;
 * - starts logging;
 * - registers the Scheme functions with Guile;
 * - parses the RC files;
 *
 * \param closure
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
void
main_prog(void *closure, int argc, char *argv[])
{
  int argv_index;

  argv_index = parse_commandline(argc, argv);

  if (embed_mode + unembed_mode == 1) {

    GedaToplevel *pr_current;

    char *cwd;
    int   index;

    cwd = g_get_current_dir();

    libgeda_init(argc, argv);

#if defined(__MINGW32__) && defined(DEBUG)
      fprintf(stderr, "This is the MINGW32 port.\n");
#endif

    if (!quiet_mode) {
      u_log_message ("gEDA/gschlas version %s%s.%s\ngEDA/gschlas comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\nThis is free software, and you are welcome to redistribute it under certain\nconditions; please see the COPYING file for more details.\n\n",
                      PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
                      PACKAGE_DATE_VERSION);
    }

    /* register guile (scheme) functions */
    g_register_funcs();

    g_rc_parse (argv[0], "gschlasrc", NULL);

    pr_current = geda_toplevel_new ();

    i_vars_set(pr_current);

    /* create log file right away even if logging is enabled */
    geda_utility_log_init ("gschlas");

    index = argv_index;

    while (argv[index] != NULL) {

      char       *filename;
      GError     *err = NULL;

      if (f_get_is_path_absolute(argv[index])) {

        /* Path is already absolute so no need to do any concat of cwd */
        filename = geda_utility_string_strdup (argv[index]);
      }
      else {
        filename = g_build_filename (cwd, argv[index], NULL);
      }

      s_page_goto (s_page_new (pr_current, filename));

      if (!f_open (pr_current, pr_current->page_current,
                   pr_current->page_current->filename, &err))
      {
        fprintf(stderr, "%s\n", err->message);
        g_error_free (err);
        /* Not being able to load a file is apparently a fatal error */
        exit(2);
      }
      else {
        g_message ("Loaded file [%s]\n", filename);
      }

      index++;
      GEDA_FREE (filename);
    }

    if (argv[argv_index] == NULL) {
      fprintf(stderr, "\nERROR! You must specify at least one filename\n\n");
      usage(argv[0]);
    }

    GEDA_FREE(cwd);

#if DEBUG
    s_page_print_all(pr_current);
#endif

    if (!quiet_mode) u_log_message("\n");

    if (embed_mode) {
      s_util_embed(pr_current, TRUE);
    }
    else if (unembed_mode) {
      s_util_embed(pr_current, FALSE);
    }

    /* save all the opened files */
    s_page_save_all(pr_current);

    s_page_delete_list (pr_current);

    gschlas_quit();
  }
  else {
    if (embed_mode && unembed_mode) {
      fprintf(stderr, "Cannot specify both -e and -u at the same time\n");
    }
    else {
      fprintf(stderr, "Must specify whether to embed or unembed\n");
    }
  }
  exit(0);
}

/*! \brief Entry point to gschlas
 *
 * This is just a wrapper which invokes the guile stuff, and
 * points to the real main program main_prog().
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
int main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
