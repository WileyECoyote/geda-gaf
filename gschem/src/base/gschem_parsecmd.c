/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

#include <gschem.h>
#include <version.h>
#include <geda_debug.h>

/* Colon after a character means the argument expects a parameter strings */
#define GETOPT_OPTIONS "a:c:f:g:hlmno:pqr:s:t:vVx:z"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int   optind;
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#ifdef HAVE_GETOPT_LONG

struct option long_options[] =
  {
    {"render-adaptor", 1, 0, 'a'},
    {"config-file",    1, 0, 'c'},
    {"font",           1, 0, 'f'},
    {"guile",          1, 0, 'g'},
    {"help",           0, 0, 'h'},
    {"list",           0, 0, 'l'},
    {"safe-mode",      0, 0, 'm'},
    {"no-auto",        0, 0, 'n'},
    {"output",         1, 0, 'o'},
    {"version",        0, 0, 'V'},
    {"place",          0, 0, 'p'},
    {"quiet",          0, 0, 'q'},
    {"run",            1, 0, 'r'},
    {"start",          1, 0, 's'},
    {"title-block",    1, 0, 't'},
    {"undo-dir",       1, 0, 'u'},
    {"verbose",        0, 0, 'v'},
    {"minimized",      0, 0, 'z'},
    {0, 0, 0, 0}
  };

#endif

/*! Contains a Scheme expression arising from command-line arguments.
 *  This is evaluated after initializing gschem, but before loading
 *  any rc files. */
SCM s_pre_load_expr = SCM_EOL;

/*! Contains a Scheme expression arising from command-line arguments.
 *  This is evaluated after loading gschem and any schematic
 *  files specified on the command-line.
 */
SCM s_post_load_expr = SCM_EOL;

/*!
 * \brief Print brief help message and exit.
 * \par Function Description
 * Print brief help message describing gschem usage & command-line
 * options, then exit with exit status 0.
 *
 * \param cmd First element of argv (name of program as run).
 */
static void usage(char *cmd)
{
  printf(_(
    "Usage: %s [OPTION ...] [--] [FILE ...]\n"
    "\n"
    "Interactively edit gEDA schematics or symbols. If one or more FILEs\n"
    "are specified, open them for editing; otherwise, create a new, empty\n"
    "schematic.\n"
    "\n"
    "Options:\n"
    "  -a, --render-adaptor <DRV> Specify which render adaptor to use, Cario or X11.\n"
    "  -c, --config-file <FILE>   Additional configuration file to load.\n"
    "  -g, --guile <DIR>          Add DIR to the Guile path (pre-config).\n"
    "  -f, --font <NAME>          set font family name\n"
    "  -h, --help                 Help; this message.\n"
    "  -l, --list                 List saved session names.\n"
    "  -m, --safe-mode            Safe Mode.\n"
    "  -n, --no-auto              No auto load last document.\n"
    "  -o, --output <FILE>        Output filename (for printing and image output).\n"
    "  -p, --place                Automatically place the window.\n"
    "  -q, --quiet                Quiet mode.\n"
    "  -r, --run <FILE>           Scheme script to run at startup.\n"
    "  -s, --start <name>         Startup using the given session name.\n"
    "  -t, --title-block <FILE>   Start a new drawing using the specified title-block.\n"
    "  -u, --undo-dir <DIR>       Specify a temporary directory for Undo files.\n"
    "  -v, --verbose              Verbose mode.\n"
    "  -V, --version              Show version information.\n"
    "  -x (EXPR)                  Scheme expression to run at startup.\n"
    "  -z, --minimized            Hide the main window, used for scripting\n"
    "  --                         Treat all remaining arguments as filenames.\n"
    "\n"
    "Report bugs at <https://bugs.launchpad.net/geda>\n"
    "gEDA homepage: <http://www.geda-project.org>\n"),
    cmd);
  exit(0);
}

/*!
 * \brief Print version info and exit.
 * \par Function Description
 * Print gEDA version, and copyright/warranty notices, and exit with
 * exit status 0.
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
    printf( "gEDA/gschem %s (%s) (g%.7s)\n%s",
      PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION, PACKAGE_GIT_COMMIT, string);
  }
  else {
    printf("%s\n", PACKAGE_DOTTED_VERSION);
  }
  exit (0);
}

/*!
 * \brief Parse gschem command-line options.
 * \par Function Description
 *  Parse command line options, displaying usage message or version
 *  information as required.
 *
 * \param argc Number of command-line arguments.
 * \param argv Array of command-line arguments.
 *
 * \return index into \a argv of first non-option argument.
 */
int gschem_parse_commandline(int argc, char *argv[])
{
  GError *err;
  char   *str;
  int     ch;

  SCM sym_begin       = scm_from_utf8_symbol ("begin");
  SCM sym_cons        = scm_from_utf8_symbol ("cons");
  SCM sym_set_x       = scm_from_utf8_symbol ("set!");
  SCM sym_load        = scm_from_utf8_symbol ("primitive-load");
  SCM sym_load_path   = scm_from_utf8_symbol ("%load-path");
  SCM sym_eval_string = scm_from_utf8_symbol ("eval-string");

  override_autoload   = FALSE;
  iconify_main_window = FALSE;
  start_session       = NULL;
  comline_font        = NULL;
  comline_tblock      = NULL;

#ifdef HAVE_GETOPT_LONG
  while ((ch = getopt_long (argc, argv, GETOPT_OPTIONS, long_options, NULL)) != -1) {
#else
  while ((ch = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
#endif

    switch (ch) {

      case 'a':
        str = geda_utility_string_strdup (optarg);
        if (str) {
          if (!geda_utility_string_stricmp(str, RC_RENDERER_OPTION_CAIRO)) {
            default_render_adaptor = CAIRO_ADAPTOR;
          }
          else if (!geda_utility_string_stricmp(str, RC_RENDERER_OPTION_X11)) {
            default_render_adaptor = X11_ADAPTOR;
          }
          else {
            GEDA_FREE(str);
          }
        }
        break;

      case 'c':
        rc_filename = geda_utility_string_strdup (optarg);
        break;

      case 'f':
        comline_font = geda_utility_string_strdup (optarg);
        break;

      case 'g':
        /* Argument is a directory to add to the Scheme load path.
         * Add the necessary expression to be evaluated before rc file
         * loading. */
        s_pre_load_expr =
          scm_cons (scm_list_3 (sym_set_x,
                                sym_load_path,
                                scm_list_3 (sym_cons,
                                            scm_from_locale_string (optarg),
                                            sym_load_path)),
                    s_pre_load_expr);
        break;

      case 'h':
        usage(argv[0]);
        break;

      case 'l':
        i_sessions_list_sessions ();
        i_sessions_destroy_sessions();
        exit (0);

      case 'm':
        run_mode = 1;
        break;

      case 'n':
        override_autoload = 1;
        break;

      case 'o':
        output_filename = geda_utility_string_strdup (optarg);
        break;

      case 'p':
        auto_place_mode = TRUE;
        break;

      case 'q':
        quiet_mode = TRUE;
        break;

      case 'r':
        /* Argument is filename of a Scheme script to be run on gschem
         * load. Validate the file and add the necessary expression to
         * be evaluated after loading. */
        err = NULL;
        str = geda_normalize_filename (optarg, &err);
        if (str == NULL) {
           geda_log("%s <%s>: %s\n", _("error parsing"), optarg, err->message);
          g_clear_error(&err);
        }
        else {
          s_post_load_expr = scm_cons (scm_list_2 (sym_load,
                                                   scm_from_locale_string (str)),
                                       s_post_load_expr);
          GEDA_FREE(str);
        }
        break;

      case 's':
        start_session = geda_utility_string_strdup (optarg);
        break;

      case 't':
        comline_tblock = geda_utility_string_strdup (optarg);
        override_autoload = 1; /* do not auto load if tblock is not found */
        break;

      case 'u':
        tmp_directory = geda_utility_string_strdup (optarg);
        break;

      case 'v':
        verbose_mode = TRUE;
        break;

      case 'V':
        version ();
        break;

      case '?': {

#ifndef HAVE_GETOPT_LONG

        const char *_ERROR = _("ERROR");

        if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {

          const char *err_arg = _("option requires an argument");

          fprintf (stderr, "%s: -%c %s.\n\n", _ERROR, optopt, err_arg);
        }
        else if (isprint (optopt)) {

          const char *err_ukn = _("Unknown option");

          fprintf (stderr, "%s: %s -%c.\n\n", _ERROR, err_ukn, optopt);
        }
        else {

          const char *err_ukn = _("Unknown option character");

          fprintf (stderr, "%s: %s `\\x%x'.\n\n", _ERROR, err_ukn, optopt);
        }

#endif

        const char *err_info = _("help for more information");

        fprintf (stderr, "\n%s %s --%s.\n", _("Run"), argv[0], err_info);
        exit (1);
      }
      break;

      case 'x':
        /* Argument is a Scheme expression to be evaluated on gschem
         * load.  Add the necessary expression to be evaluated after
         * loading. */
        s_post_load_expr = scm_cons (scm_list_2 (sym_eval_string,
                           scm_from_locale_string (optarg)),
                           s_post_load_expr);
        break;

      case 'z':
        iconify_main_window = 1;
        break;

      default:
        fprintf (stderr, "<%s> unhandler case for <%c>.\n", __func__, ch);
    }
  }

  if (quiet_mode) {
    verbose_mode = FALSE;
  }

  /* Ensure Scheme expressions can be passed straight to eval */
  s_pre_load_expr = scm_cons (sym_begin, scm_reverse_x (s_pre_load_expr, SCM_UNDEFINED));
  scm_gc_protect_object (s_pre_load_expr);
  s_post_load_expr = scm_cons (sym_begin, scm_reverse_x (s_post_load_expr, SCM_UNDEFINED));
  scm_gc_protect_object (s_post_load_expr);

  return(optind);
}
