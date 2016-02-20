/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: gsymcheck.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
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
 * MA 02111-1301 USA
 */

#include <config.h>

#include <libgeda/libgeda.h>

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

/*! \brief Cleanup gsymcheck on exit.
 *  \par Function Description
 *  This function is used to release system resource allocated during
 *  the program runtime.
 */
void
gsymcheck_quit(void)
{
  /* Shutdown libgeda */
  libgeda_release();
}

/*! \brief Main Scheme(GUILE) program function.
 *  \par Function Description
 *  This function is the main program called from scm_boot_guile.
 *  The function handles initializing all libraries and gsymcheck
 *  variables.
 */
static void main_prog(void *closure, int argc, char *argv[])
{
  int i;
  int argv_index;
  int exit_status;
  char *cwd;

  GedaToplevel *pr_current;

  argv_index = parse_commandline(argc, argv);
  cwd = g_get_current_dir();

  libgeda_init(argc, argv);

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif

  log_destiny=-1; /* don't output to the screen for now */

  /* register guile (scheme) functions */
  g_register_funcs();

  g_rc_parse (argv[0], "gsymcheckrc", rc_filename);

  /* create log file right away even if logging is enabled */
  geda_utility_log_set_update_func(s_log_update);
  geda_utility_log_init ("gsymcheck");

  pr_current = geda_toplevel_new ();
  i_vars_set(pr_current);

  i = argv_index;
  while (argv[i] != NULL) {

    char   *filename;
    GError *err = NULL;
    Page   *page;

    if (f_get_is_path_absolute(argv[i])) {

      /* Path is already absolute so no need to do any concat of cwd */
      filename = geda_utility_string_strdup (argv[i]);
    }
    else {
      filename = g_build_filename (cwd, argv[i], NULL);
    }

    page = s_page_new (pr_current, filename);
    s_page_goto (page);

    if (!f_open (pr_current, page, page->filename, &err)) {

      /* Not being able to load a file is apparently a fatal error */
      log_destiny = STDOUT_TTY;
      fprintf(stderr, "%s\n", err->message);
      g_error_free (err);
      exit(2);
    }
    else {
      g_message (_("Loaded file [%s]\n"), filename);
    }
    i++;
    GEDA_FREE (filename);
  }

  if (argv[argv_index] == NULL) {
    fprintf(stderr, _("\nERROR! You must specify at least one filename\n\n"));
    usage(argv[0]);
  }

  GEDA_FREE(cwd);

  log_destiny=STDOUT_TTY;

#if DEBUG
  s_page_print_all(pr_current);
#endif

  exit_status = s_check_all(pr_current);

  if (!exit_status  && !quiet_mode && !verbose_mode) {
    u_log_message("\n");
  }

  s_page_delete_list(pr_current);

  gsymcheck_quit();

  exit(exit_status);
}

/*! \brief Main executable entrance point.
 *  \par Function Description
 *  This is the main function for gsymcheck. The function sets up the
 *  Scheme(GUILE) environment and passes control to the #main_prog
 *  function via scm_boot_guile.
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
int main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
