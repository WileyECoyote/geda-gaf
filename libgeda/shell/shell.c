/*
 * geda-shell: Batch processing for gEDA
 * Copyright (C) 2010-2015 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* A simple batch processing interface to libgeda based on a Scheme
 * REPL (Read-Eval-Print Loop). */

#define WITHOUT_GDK_PIX_BUFFER 1

#include "../../config.h"

#include <version.h>
#include <ctype.h>
#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>

#include "../include/gettext_priv.h"

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#define GETOPT_OPTIONS "e:hil:p:s:V"

#ifdef HAVE_GETOPT_LONG
static struct option long_options[] =
  {
    {"eval",    1, NULL, 'e'},
    {"help",    0, NULL, 'h'},
    {"inhibit", 0, NULL, 'i'},
    {"load",    1, NULL, 'l'},
    {"path",    1, NULL, 'p'},
    {"scheme",  1, NULL, 's'},
    {"version", 0, NULL, 'V'},
    {NULL,      0, NULL, 0},
  };
#endif

/* Print help info and exit with exit_status */
static void
usage (int exit_status)
{
  printf ("Usage: gaf shell [OPTION ...]\n"
"\n"
"Shell for interactive processing of gEDA data using Scheme.\n"
"\n"
"  -e, --eval (EXPR)   evaluate Scheme expression EXPR, and exit\n"
"  -h, --help          display usage information and exit\n"
"  -i, --inhibit       Inhibit loading RC files.\n"
"  -l, --load <FILE>   load Scheme source code from FILE\n"
"  -p, --path <DIR>    add DIR to the front of the Guile load path\n"
"  -s, --scheme <FILE> load Scheme source code from FILE, and exit\n"
"  -V, --version       Show version information.\n"
"  --                  stop scanning arguments; run interactively\n"
"\n"
"Options -e, -s and -- switches stop argument processing, and pass all\n"
"remaining arguments as the value of (command-line).\n"
"\n"
"\n"
"Please report bugs to %s.\n",
PACKAGE_BUGREPORT);
  exit (exit_status);
}

/* Print version info and exit */
static void
version ()
{
  printf(
    "gEDA %s (g%.7s)\n"
    "Copyright (C) 1998-2015 gEDA developers\n"
    "This is free software, and you are welcome to redistribute it under\n"
    "certain conditions. For details, see the file `COPYING', which is\n"
    "included in the gEDA distribution.\n"
    "There is NO WARRANTY, to the extent permitted by law.\n",
    PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
    exit (0);
}

/* Some symbols we need */
SCM_SYMBOL (sym_load,              "load");
SCM_SYMBOL (sym_eval_string,       "eval-string");
SCM_SYMBOL (sym_set_x,             "set!");
SCM_SYMBOL (sym_load_path,         "%load-path");
SCM_SYMBOL (sym_cons,              "cons");
SCM_SYMBOL (sym_use_modules,       "use-modules");
SCM_SYMBOL (sym_ice_9,             "ice-9");
SCM_SYMBOL (sym_readline,          "readline");
SCM_SYMBOL (sym_activate_readline, "activate-readline");
SCM_SYMBOL (sym_top_repl,          "top-repl");
SCM_SYMBOL (sym_quit,              "quit");
SCM_SYMBOL (sym_begin,             "begin");

static void
shell_main (void *data, int argc, char **argv)
{
  SCM setup_lst = SCM_EOL; /* We reverse! this before using it. */
  SCM run_lst = SCM_EOL;   /* We reverse! this before using it. */
  SCM scm_status;
  int c;
  int interactive = 1;
  int inhibit_rc = 0;
  int status;
  GedaToplevel *toplevel;

  #include "shell.x"

  /* Parse command-line arguments */
  opterr = 0;

#ifdef HAVE_GETOPT_LONG
  while ((c = getopt_long (argc, argv, GETOPT_OPTIONS,
                           long_options, NULL)) != -1) {
#else
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {

#endif

    switch (c) {
      case 'e':
        /* We need to evaluate an expression */
        run_lst = scm_cons (scm_list_2 (sym_eval_string,
                                        scm_from_locale_string (optarg)),
                            run_lst);
        interactive = 0;
        goto endoptloop;
      case 'h':
        usage (0);
      case 'i':
        inhibit_rc = 1;
        break;

      case 'l':
        /* Same as -s, pretty much */
        run_lst = scm_cons (scm_list_2 (sym_load,
                                        scm_from_locale_string (optarg)),
                            run_lst);
        break;
      case 'p':
        /* Add argument to %load-path */
        setup_lst = scm_cons (scm_list_3 (sym_set_x,
                                          sym_load_path,
                                          scm_list_3 (sym_cons,
                                                      scm_from_locale_string (optarg),
                                                      sym_load_path)),
                              setup_lst);
        break;
      case 's':
        /* Construct an application of LOAD to the script name */
        run_lst = scm_cons (scm_list_2 (sym_load,
                                        scm_from_locale_string (optarg)),
                            run_lst);
        interactive = 0;
        goto endoptloop;
      case 'V':
        version();
      case '?':
        if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {
          fprintf (stderr,
                   "ERROR: -%c option requires an argument.\n\n", optopt);
          usage (1);
        }
        else if (isprint (optopt)) {
          fprintf (stderr, "ERROR: Unknown option -%c\n\n", optopt);
          usage (1);
        }
        else {
          fprintf (stderr,
                   "ERROR: Unknown option character `\\x%x'.\n\n", optopt);
          usage (1);
        }
      default:
        BUG_MSG ("Option not handled");
    }
  }

  endoptloop:
  /* Set program arguments visible from Guile */
  scm_set_program_arguments (argc - optind, argv + optind, "geda-shell");

  /* If interactive mode, load readline and run top REPL. */
  if (interactive) {

#ifdef __MINGW32__
    fprintf (stderr, "readline is not yet supported for MinGW builds\n");
#else

    run_lst = scm_cons (scm_list_2 (sym_use_modules,
                                    scm_list_2 (sym_ice_9, sym_readline)),
                        run_lst);
    run_lst = scm_cons (scm_list_1 (sym_activate_readline), run_lst);
    run_lst = scm_cons (scm_list_1 (sym_top_repl), run_lst);

    /* Print GPL bumf if necessary */
    if (isatty (1) && isatty (0)) {

      printf (
        "gEDA " PACKAGE_DOTTED_VERSION "\n"
        "Copyright (C) 1998-2017 gEDA developers\n"
        "This is free software, and you are welcome to redistribute it under\n"
        "certain conditions. For details, see the file `COPYING', which is\n"
        "included in the gEDA distribution.\n"
        "There is NO WARRANTY, to the extent permitted by law.\n"
      );
    }
#endif /* __MINGW32__ */
  }
  else {
    run_lst = scm_cons (scm_list_1 (sym_quit), run_lst);
  }

  /* Reverse lists */
  setup_lst = scm_reverse_x (setup_lst, SCM_UNDEFINED);
  run_lst = scm_reverse_x (run_lst, SCM_UNDEFINED);

  /* First run the setup list */
  if (setup_lst != SCM_EOL) {
    setup_lst = scm_cons (sym_begin, setup_lst);
    scm_eval_x (setup_lst, scm_current_module ());
  }

  /* Initialize libgeda */
  libgeda_init (argc, argv);
  scm_dynwind_begin (0);
  toplevel = geda_toplevel_new ();
  edascm_dynwind_toplevel (toplevel);

  /* Load rc files, unless command-line over-ride */
  if (!inhibit_rc) {
    g_rc_parse (argv[0], NULL, NULL);
  }

  geda_iface_vars_set (toplevel); /* Ugh */

  /* Finally evaluate run list */
  run_lst = scm_cons (sym_begin, run_lst);

  scm_status = scm_eval_x (run_lst, scm_current_module ());

  status = scm_exit_status (scm_status);

  scm_dynwind_end ();

  scm_remember_upto_here_2 (setup_lst, run_lst);

  exit (status);
}

/* This just starts guile, which calls shell_main back */
int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, shell_main, NULL);
  return 0;
}
