/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * gEDA/gaf command-line utility
 * Copyright (C) 2012-2016 Peter Brett <peter@peter-b.co.uk>
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

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

#include <glib.h>

/* Gettext translation */
#include <locale.h>
#include "../include/gettext.h"
#include "../include/builtins.h"

#include <libgeda/libgeda.h>         /* for geda_sprintf */

#define short_options "+hnV"

static struct option long_options[] =
  {
    {"help", 0, NULL, 'h'},
    {"no-rcfiles", 0, NULL, 'n'},
    {"version", 0, NULL, 'V'},
    {NULL, 0, NULL, 0},
  };

struct internal_command {
  char *name;
  int (*func)(int, char **);
};

static struct internal_command commands[] =
  {
    {"shell", cmd_shell},
    {"config", cmd_config},
    {"export", cmd_export},
    {NULL, NULL},
  };

/* Print help info and exit */
static void usage (void)
{
  printf (_("Usage: gaf [OPTION...] COMMAND [ARGS ...]\n\n"
            "gEDA/gaf command-line utility.\n\n"
            "General options:\n"
            "  -n, --no-rcfiles  inhibit loading of 'gafrc' files\n"
            "  -h, --help        display usage information and exit\n"
            "  -V, --version     display version information and exit\n\n"
            "Commonly-used commands (type `gaf <cmd> --help' for usage):\n"
            "  shell             Scheme REPL for interactive gEDA data processing\n"
            "  config            Edit gEDA configuration\n"
            "  export            Export gEDA files in various image formats.\n\n"
            "Please report bugs to %s.\n"),
             PACKAGE_BUGREPORT);
  exit (0);
}

/* Print version info and exit */
static void version (void)
{

  printf(_("gEDA/gaf %s (%s) (g%.7s)\n"
           "Copyright (C) 1998-2017 gEDA developers\n"
           "This is free software, and you are welcome to redistribute it under\n"
           "certain conditions. For details, see the file `COPYING', which is\n"
           "included in the gEDA distribution.\n"
           "There is NO WARRANTY, to the extent permitted by law.\n"),
           PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION, PACKAGE_GIT_COMMIT);
  exit (0);
}

static void gaf_show_run_help (const char *msg)
{
  const char *help_msg = _("Run `gaf --help' for more information");

  if (msg) {
    fprintf (stderr, "\n%s.\n\n%s.\n\n", msg, help_msg);
  }
  else {
    fprintf (stderr, "\n%s.\n\n", help_msg);
  }
}

int main (int argc, char **argv)
{
  int    c;
  char  *cmd;
  int    cmd_argc;
  char **cmd_argv;

  int (*cmd_func)(int, char **) = NULL;

  /* Possibly set up gettext */
#if ENABLE_NLS

  setlocale (LC_ALL, "");
  setlocale (LC_NUMERIC, "C");
  bindtextdomain ("geda-gaf", LOCALEDIR);
  textdomain ("geda-gaf");
  bind_textdomain_codeset ("geda-gaf", "UTF-8");

#endif

  while (-1 != (c = getopt_long (argc, argv, short_options,
                                 long_options, NULL))) {
    switch (c) {

    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 'V':
      version ();

    case 'h':
      usage ();

    case 'n':
      g_setenv ("GAF_INHIBIT_RCFILES", "1", 1);
      break;

    case '?':
      /* getopt_long already printed an error message */
      gaf_show_run_help (NULL);
      exit (1);
      break;

    default:
      fprintf (stderr, "%s: unhandled case <%d>\n", __func__, c);
    }
  }

  /* The next argument should be a command */
  if (optind == argc) {
    gaf_show_run_help (_("No command specified, nothing to do"));
    exit (1);
  }

  cmd = argv[optind];

  /* Look up the command */
  int i;
  for (i = 0; commands[i].name != NULL; i++) {
    if (strcmp (cmd, commands[i].name) == 0) {
      cmd_func = commands[i].func;
      break;
    }
  }

  if (cmd_func == NULL) {

    const char *msg = _("ERROR: Unrecognised command");
          char *str;

    str = geda_sprintf ("%s <%s>", msg, cmd);

    gaf_show_run_help (str);

    free(str);
    exit (1);
  }

  cmd_argc = argc - optind;
  cmd_argv = argv + optind;
  optind = 1;

  return cmd_func (cmd_argc, cmd_argv);
}
