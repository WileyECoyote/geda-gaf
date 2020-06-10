/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * gEDA/gaf command-line utility
 * Copyright (C) 2012-2015 Peter Brett <peter@peter-b.co.uk>
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
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <version.h>

#include <gaf_getopt.h>

/* Gettext translation */
#include "../include/gettext.h"

#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>

#define shell_short_options "e:hl:p:s:"

static struct gaf_option shell_long_options[] =
  {
    {"eval",   1, NULL, 'e'},
    {"help",   0, NULL, 'h'},
    {"load",   1, NULL, 'l'},
    {"path",   1, NULL, 'p'},
    {"scheme", 1, NULL, 's'},
    {NULL,     0, NULL, 0},
  };

/*!
 * \brief Print brief gaf shell usage information and exit.
 * \par Function Description
 *  Print brief help message describing gaf shell usage & command-line
 *  options.
 */
static void shell_usage (void)
{
  printf (_("Usage: gaf shell [OPTION ...]\n"
  "\n"
  "Shell for interactive processing of gEDA data using Scheme.\n"
  "\n"
  "  -e, --eval <EXPR>   evaluate Scheme expression EXPR, and exit\n"
  "  -h, --help          display usage information and exit\n"
  "  -l, --load <FILE>   load Scheme source code from FILE\n"
  "  -p, --path <DIR>    add DIR to the front of the Guile load path\n"
  "  -s, --scheme <FILE> load Scheme source code from FILE, and exit\n"
  "  --                  stop scanning arguments; run interactively\n"
  "\n"
  "The switch -e and -s stop argument processing, and pass all\n"
  "remaining arguments as the value of (command-line).\n"
  "\n"
  "\n"
  "Please report bugs to %s.\n"),
  PACKAGE_BUGREPORT);
  exit (0);
}

/* Some symbols we need */
SCM_SYMBOL (sym_begin,             "begin");
SCM_SYMBOL (sym_cons,              "cons");
SCM_SYMBOL (sym_eval_string,       "eval-string");
SCM_SYMBOL (sym_load,              "load");
SCM_SYMBOL (sym_load_path,         "%load-path");
SCM_SYMBOL (sym_quit,              "quit");
SCM_SYMBOL (sym_set_x,             "set!");

/* readline is not yet supported for MinGW builds */
#ifndef __MINGW32__

SCM_SYMBOL (sym_activate_readline, "activate-readline");
SCM_SYMBOL (sym_ice_9,             "ice-9");
SCM_SYMBOL (sym_readline,          "readline");
SCM_SYMBOL (sym_top_repl,          "top-repl");
SCM_SYMBOL (sym_use_modules,       "use-modules");

#endif /* __MINGW32__ */

static void cmd_shell_impl (void *data, int argc, char **argv)
{

  GedaToplevel *toplevel;
  SCM path_lst = SCM_EOL; /* We reverse! this before using it. */
  SCM run_lst = SCM_EOL;   /* We reverse! this before using it. */
  SCM scm_status;
  int status;
  int c, interactive = 1;

#include "shell.x"

  /* Parse command-line arguments */
  while ((c = gaf_getopt_long (argc, argv, shell_short_options,
                               shell_long_options, NULL)) != -1)
  {
    switch (c) {
      case 0:
        /* This is a long-form-only flag option, and has already been
         * dealt with by getopt_long(). */
        break;
      case 'h':
        shell_usage ();
        break;
      case 'e':
        interactive = 0;
        run_lst = scm_cons (scm_list_2 (sym_eval_string,
                                        scm_from_locale_string (gaf_optarg)),
                            run_lst);
        break;
      case 's':
        interactive = 0;
      case 'l':
        run_lst = scm_cons (scm_list_2 (sym_load,
                                        scm_from_locale_string (gaf_optarg)),
                            run_lst);
        break;
      case 'p':
        /* Add argument to %load-path */
        path_lst = scm_cons (scm_list_3 (sym_set_x,
                                         sym_load_path,
                                         scm_list_3 (sym_cons,
                                                     scm_from_locale_string (gaf_optarg),
                                                     sym_load_path)),
                             path_lst);
        continue;
      case '?':
        /* getopt_long already printed an error message */
        fprintf (stderr, _("\nRun `gaf shell --help' for more information.\n"));
        exit (1);
        break;
      default:
        fprintf (stderr, "%s: unhandled case <%d>\n", __func__, c);
    }
  }

  /* Set program arguments visible from Guile */
  scm_set_program_arguments (argc - gaf_optind, argv + gaf_optind, "gaf shell");

  /* First run the setup list */
  if (!scm_is_null (path_lst)) { /* if path_lst != SCM_EOL */
    /* Reverse lists */
    path_lst = scm_reverse_x (path_lst, SCM_UNDEFINED);
    path_lst = scm_cons (sym_begin, path_lst);
    scm_eval_x (path_lst, scm_current_module ());
  }

  libgeda_init (argc, argv);
  scm_dynwind_begin (0);
  toplevel = geda_toplevel_new ();
  edascm_dynwind_toplevel (toplevel);

  /* Interactive, so enable readline support and print an abbreviated
   * version message. */
  if (interactive) {

    /* Print version applicable */
    if (isatty (1) && isatty (0)) {
      printf ("gEDA %s (g%.7s)\n", PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
    }

#ifdef __MINGW32__
    fprintf (stderr, "readline is not yet supported for MinGW builds\n");
#else

    run_lst = scm_cons (scm_list_2 (sym_use_modules,
                                    scm_list_2 (sym_ice_9, sym_readline)),
                        run_lst);
    run_lst = scm_cons (scm_list_1 (sym_activate_readline), run_lst);
    run_lst = scm_cons (scm_list_1 (sym_top_repl), run_lst);

#endif /* __MINGW32__ */

  }
  else {
    run_lst = scm_cons (scm_list_1 (sym_quit), run_lst);
  }

  /* Reverse run list */
  run_lst = scm_reverse_x (run_lst, SCM_UNDEFINED);

  /* Now load rc files, if necessary */
  if (g_getenv ("GAF_INHIBIT_RCFILES") == NULL) {
    g_rc_parse ("gaf shell", NULL, NULL);
  }

  geda_iface_vars_set (toplevel); /* Ugh */

  /* Finally evaluate run list */
  run_lst = scm_cons (sym_begin, run_lst);

  scm_status = scm_eval_x (run_lst, scm_current_module ());

  status = scm_exit_status (scm_status);

  scm_dynwind_end ();

  scm_remember_upto_here_2 (path_lst, run_lst);

  exit (status);
}

/*! \brief Main function for "gaf shell" */
int cmd_shell (int argc, char **argv)
{
  scm_boot_guile (argc, argv, cmd_shell_impl, NULL); /* Doesn't return */
  return 0;
}
