/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: parsecmd.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */

#define WITHOUT_GDK_PIX_BUFFER 1

#include "../../config.h"

#include <version.h>
#include <gnetlist.h>

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <gettext.h>
#include <geda_debug.h>

#define OPTIONS "c:g:hil:L:m:o:O:qr:vV"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

/* Added by SDB 3.3.2006.  */
#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"help", 0, 0, 'h'},
    {"list-backends", 0, &list_backends, TRUE},
    {"verbose", 0, 0, 'v'},
    {"version", 0, 0, 'V'},
    {0, 0, 0, 0}
  };
#endif

/*!
 * \brief Print brief usage information and exit.
 * \par Function Description
 *  Print brief help message describing gnetlist usage & command-line
 *  options.
 *
 * \note Does exit program.
 *
 * \param cmd First element of argv (name of program as run).
 */
void usage(char *cmd)
{
  printf (_(
    "Usage: %s [OPTION ...] [-g BACKEND] [ help | version ] [--] FILE ...\n"
    "\n"
    "Generate a netlist from one or more gEDA schematic FILEs.\n"
    "\n"
    "General options:\n"
    "  -q              Quiet mode.\n"
    "  -v, --verbose   Verbose mode.\n"
    "  -o <filename>   Filename for netlist data output.\n"
    "  -L <directory>  Add DIR to Scheme search path.\n"
    "  -g <backend>    Specify netlist backend to use.\n"
    "  -O <string>     Pass an option string to backend.\n"
    "  -l <file.scm>   Load Scheme file before loading backend.\n"
    "  -m <file.scm>   Load Scheme file after loading backend.\n"
    "  -c <expresion>  Evaluate Scheme expression at startup.\n"
    "  -i              Enter interactive Scheme REPL after loading.\n"
    "  -r <filename>   RC Filename, default is gnetlistrc.\n"
    "  --list-backends Print a list of available netlist backends.\n"
    "  -h, --help      Display usage and parameter information.\n"
    "  -V, --version   Show gnetlist or a backend version information.\n"
    "  --              Treat all remaining arguments as filenames.\n"
    "\n"
    "  Note: Help and version arguments are positional relative to the\n"
    "  backend argument, when specified after the backend, details specific\n"
    "  to the backend will be displayed.\n"
    "\n"
    "Report bugs at <https://bugs.launchpad.net/geda>\n"
    "gEDA homepage: <http://www.geda-project.org>\n"
    " PCB homepage: <http://pcb.geda-project.org>\n"),
    cmd);
}

/*!
 * \brief Print gnetlist version info and exit.
 * \par Function Description
 *  Print gEDA version, and copyright/warranty notices, and exit with
 *  exit status 0.
 */
static void version (void)
{
  if (!quiet_mode) {
    const char *string = _(
      "Copyright (C) 1998-2017 gEDA developers\n"
      "This is free software, and you are welcome to redistribute it under\n"
      "certain conditions. For details, see the file `COPYING', which is\n"
      "included in the gEDA distribution. There is NO WARRANTY; not even \n"
      "for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, to the extent\n"
      "permitted by law.\n");
    printf( "gEDA/gnetlist %s (%s) (g%.7s)\n%s",
      PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION, PACKAGE_GIT_COMMIT, string);
  }
  else {
    printf("%s\n", PACKAGE_DOTTED_VERSION);
  }
  exit (0);
}

/* from guile (libguile/gh_init.c) */
static SCM catch_handler (void *data, SCM tag, SCM throw_args)
{
  fprintf (stderr, "\n%s:\n", _("Received an error; tag is"));
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  return SCM_BOOL_F;
}

/*!
 * \brief Parse gnetlist command-line options.
 * \par Function Description
 *  Parse command line options, displaying usage message or version
 *  information as required.
 *
 * \param argc Number of command-line arguments.
 * \param argv Array of command-line arguments.
 * \param output_filename
 *
 * \return index into \a argv of first non-option argument.
 */
int parse_commandline (int argc, char *argv[], char **output_filename)
{
  int ch;
  int backend_flag  = FALSE;

  SCM sym_begin     = scm_from_utf8_symbol ("begin");
  SCM sym_cons      = scm_from_utf8_symbol ("cons");
  SCM sym_load      = scm_from_utf8_symbol ("load");
  SCM sym_set_x     = scm_from_utf8_symbol ("set!");
  SCM sym_load_path = scm_from_utf8_symbol ("%load-path");

#ifdef HAVE_GETOPT_LONG
  /* int option_index = 0; */

  while ((ch = getopt_long(argc, argv, OPTIONS, long_options, NULL /* &option_index */)) != -1)
  {

#else

  while ((ch = getopt(argc, argv, OPTIONS)) != -1) {

#endif
      switch (ch) {

        case 0:
          /* This is a long-form-only flag option, and has already been
           * dealt with by getopt_long(). */
          break;

        case 'v':
          verbose_mode = TRUE;
          break;

        case 'i':
          interactive_mode = TRUE;
          guile_proc = geda_strdup("shell");
          break;

        case 'r': /* Argument is filename of RC script to load */
           if (rc_filename) {
             const char *msg = _("WARNING: output already specified");
             fprintf(stderr, "%s <%s>\n", msg, rc_filename);
             g_free(rc_filename);
           }
          rc_filename = geda_utility_string_strdup(optarg);

        case 'q':
          quiet_mode = TRUE;
          break;

        case 'L':
          /* Argument is a directory to add to the Scheme load path.
           * Add the necessary expression to be evaluated before rc file
           * loading. */
          pre_rc_list =
          scm_cons (scm_list_3 (sym_set_x,
                                sym_load_path,
                                scm_list_3 (sym_cons,
                                            scm_from_locale_string (optarg),
                                            sym_load_path)),
                    pre_rc_list);
          break;

        case 'g': /* Argument is netlist backend to use */
          guile_proc = geda_utility_string_strdup(optarg);
          backend_flag = TRUE;
          break;

        case 'l':
          /* Argument is filename of a Scheme script to be run before
           * loading gnetlist backend. */
          pre_backend_list =
          scm_cons (scm_list_2 (sym_load, scm_from_locale_string (optarg)),
                    pre_backend_list);
          break;

        case 'm':
          /* Argument is filename of a Scheme script to be run after
           * loading gnetlist backend. */
          post_backend_list =
          scm_cons (scm_list_2 (sym_load, scm_from_locale_string (optarg)),
                    post_backend_list);
          break;

        case 'o':  /* Argument is the name of the output file */
           if (*output_filename) {
             fprintf(stderr, "%s <%s>\n", _("WARNING: output already specified"),
                    *output_filename);
             g_free(*output_filename);
           }
          *output_filename = geda_utility_string_strdup(optarg);
          break;

        case 'O': /* Argument is a parameter to pass to the backend */
          backend_params = g_slist_append(backend_params, optarg);
          break;

        case 'c': /* Argument is a scheme expression to be evaluated */
          scm_internal_catch (SCM_BOOL_T,
                             (scm_t_catch_body) scm_c_eval_string,
                             (void *) optarg,
                             (scm_t_catch_handler) catch_handler,
                             (void *) optarg);
          break;

        case 'h':
          usage(argv[0]);
          if (backend_flag) {
            char *help = "help";
            backend_params = g_slist_append(backend_params, help);
          }
          else {
            exit (0);
          }
          break;

        case 'V':
          if (!backend_flag) {
            version();
          }
          else {
            char *version = "version";
            backend_params = g_slist_append(backend_params, version);
          }
          break;

        case '?':
#ifndef HAVE_GETOPT_LONG
          if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {
            fprintf (stderr, _("ERROR: -%c option requires an argument.\n\n"),
                     optopt);
          }
          else if (isprint (optopt)) {
            fprintf (stderr, _("ERROR: Unknown option -%c.\n\n"), optopt);
          }
          else {
            fprintf (stderr, _("ERROR: Unknown option character `\\x%x'.\n\n"),
                     optopt);
          }
#endif
          fprintf (stderr, _("\nRun `%s --help' for more information.\n"), argv[0]);
          exit (1);
          break;

        default:
          break;
      }
    }

    if (quiet_mode) {
      verbose_mode = FALSE;
    }

    /* Make sure Scheme expressions can be passed straight to eval */
    pre_rc_list = scm_cons(sym_begin,
                           scm_reverse_x(pre_rc_list, SCM_UNDEFINED));

    scm_gc_protect_object (pre_rc_list);

    pre_backend_list = scm_cons (sym_begin,
                                 scm_reverse_x (pre_backend_list, SCM_UNDEFINED));

    scm_gc_protect_object (pre_backend_list);

    post_backend_list = scm_cons (sym_begin,
                                  scm_reverse_x (post_backend_list, SCM_UNDEFINED));

    scm_gc_protect_object (post_backend_list);

    return (optind);
}
