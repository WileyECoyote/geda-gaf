/* -*- test_parsecmd.h -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 5th, 2016
 */

/* Colon after a character means the argument expects a parameter strings */
#define GETOPT_OPTIONS "hqv"

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <libgen.h>

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int   optind;
#endif

#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"help",           0, 0, 'h'},
    {"quiet",          0, 0, 'q'},
    {"verbose",        0, 0, 'v'},
    {0, 0, 0, 0}
  };
#endif

static bool quiet_mode;
static bool verbose_mode;

/*!
 * \brief Print brief help message and exit.
 * \par Function Description
 * Print brief help message describing gschem usage & command-line
 * options, then exit with exit status 0.
 *
 * \param cmd First element of argv (name of program as run).
 */
static void
usage(char *cmd)
{
  printf(
    "Usage: %s [OPTION]\n"
    "\n"
    "Options:\n"
    "  -h, --help                 Help; this message.\n"
    "  -q, --quiet                Quiet mode.\n"
    "  -v, --verbose              Verbose mode.\n"
    "\n"
    "Report bugs at <https://bugs.launchpad.net/geda>\n"
    "gEDA homepage: <http://www.geda-project.org>\n",
    basename(cmd) + 3);
  exit(0);
}

static void
parse_commandline(int argc, char *argv[])
{
  int ch;

#ifdef HAVE_GETOPT_LONG
  while ((ch = getopt_long (argc, argv, GETOPT_OPTIONS, long_options, NULL)) != -1)
#else
  while ((ch = getopt (argc, argv, GETOPT_OPTIONS)) != -1)
#endif

  {
    switch (ch) {

      case 'h':
        usage(argv[0]);
        break;

      case 'q':
        quiet_mode = TRUE;
        break;

      case 'v':
        verbose_mode = TRUE;
        break;

      default:
        fprintf (stderr, "<parse_commandline> unhandler case for <%c>.\n", ch);
    }
  }

  if (quiet_mode) {
    verbose_mode = FALSE;
  }
}

void vmessage(const char *format, ...)
{
  if (verbose_mode) {

    va_list args;

    va_start (args, format);
      vprintf(format, args);
    va_end (args);
  }
}
