/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2012 Ales Hvezda
 * Copyright (C) 2002-2012 gEDA Contributors (see ChangeLog for details)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if  not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301 USA
 */

#include "common.h"

void gschlas_quit(void)
{
  s_clib_free();
  s_slib_free();
}

void
main_prog(void *closure, int argc, char *argv[])
{
  int i;
  int argv_index;
  char *cwd;

  GedaToplevel *pr_current;

  argv_index = parse_commandline(argc, argv);
  cwd = g_get_current_dir();

  libgeda_init();

  /* create log file right away */
  /* even if logging is enabled */
  u_log_init ("gschlas");

  log_destiny=STDOUT_TTY;

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif

  log_destiny=-1; /* don't output to the screen for now */

  /* register guile (scheme) functions */
  g_register_funcs();

  pr_current = geda_toplevel_new ();
  g_rc_parse (argv[0], "gschlasrc", rc_filename);
  i_vars_set(pr_current);

  i = argv_index;
  while (argv[i] != NULL) {

    char *filename;
    GError *err = NULL;

    if (g_path_is_absolute(argv[i]))
    {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = geda_strdup (argv[i]);
    } else {
      filename = g_build_filename (cwd, argv[i], NULL);
    }

    s_page_goto (pr_current, s_page_new (pr_current, filename));

    if (!f_open (pr_current, pr_current->page_current,
                 pr_current->page_current->filename, &err)) {
      /* Not being able to load a file is apparently a fatal error */
      log_destiny = STDOUT_TTY;
      g_warning ("%s\n", err->message);
      g_error_free (err);
      exit(2);
    } else {
      g_message ("Loaded file [%s]\n", filename);
    }

    i++;
    GEDA_FREE (filename);
  }

  if (argv[argv_index] == NULL) {
    fprintf(stderr, "\nERROR! You must specify at least one filename\n\n");
    usage(argv[0]);
  }

  GEDA_FREE(cwd);

  log_destiny=STDOUT_TTY;

#if DEBUG
  s_page_print_all(pr_current);
#endif

  if (!quiet_mode) u_log_message("\n");

  if (embed_mode) {
    s_util_embed(pr_current, TRUE);
  }

  if (unembed_mode) {
    s_util_embed(pr_current, FALSE);
  }

  /* save all the opened files */
  s_page_save_all(pr_current);

  s_page_delete_list (pr_current);
  gschlas_quit();

  exit(0);
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
