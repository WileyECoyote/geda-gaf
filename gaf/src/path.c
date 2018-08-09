/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: path.c
 *
 * gEDA/gaf command-line utility
 *
 * Copyright (C) 2018 Wiley Edward Hill
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: August 10, 2018
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <version.h>

#include <gaf_getopt.h>

/* Gettext translation */
#include "../include/gettext.h"

#include <libgeda/libgeda.h>

#define path_short_options "hdosu"

static struct gaf_option path_long_options[] =
  {
    {"help",    0, NULL, 'h'},
    {"data",    0, NULL, 'd'},
    {"doc",     0, NULL, 'o'},
    {"system",  0, NULL, 's'},
    {"user",    0, NULL, 'u'},
  };
  
static void path_usage (void)
{
  printf (_("Usage: gaf path [OPTION] [which-path]\n"
            "\n"
            "Writes gEDA path strings to the standard output port.\n"
            "\n"
            "  -d, --data        system data directory\n"
            "  -o, --doc         system document directory\n"
            "  -s, --system      system configuration directory\n"
            "  -u, --user        user configuration directory\n"

            "  -h, --help        display usage information and exit\n"
            "\n"
            "If no path is specified the system data directory is written to stdout.\n"
            "\n"
            "Please report bugs to %s.\n"),
          PACKAGE_BUGREPORT);
  exit (0);
}


/*! \brief Main function for "gaf path" */
int cmd_path (int argc, char **argv)
{
  const char *str_out = NULL;
  int c;

   /* Note the libgeda path module does not require the library to be
    * initialized prior to usage.
    */
   
  /* Parse command-line arguments */
  while ((c = gaf_getopt_long (argc, argv, path_short_options,
                               path_long_options, NULL)) != -1) {
    switch (c) {

    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 'd':
      str_out = geda_file_path_sys_data();
      break;

    case 'o':
      str_out = geda_file_path_sys_doc();
      break;

    case 's':
      str_out =geda_file_path_sys_config();
      break;

    case 'u':
      str_out = geda_file_path_user_config();
      break;

    case 'h':
      path_usage (); /* does not return */
      break;

    case '?':
      /* getopt_long already printed an error message */
      str_out = _("\nRun `gaf path --help' for more information.\n");
      break;

    default:
      fprintf (stderr, "%s: unhandled case <%d>\n", __func__, c);
      exit (1);
    }
  }

  if (str_out) {
    printf ("%s", str_out);
  }
  else {
    printf ("%s", geda_file_path_sys_data());
  }

  geda_file_path_free();

  return 0;
}
