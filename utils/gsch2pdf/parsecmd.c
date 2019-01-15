/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/*
 * File: parsecmd.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsch2pdf - gEDA Schematic to PDF generator
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#include "../include/common.h"

#include "../../version.h"

#define OPTIONS "hquvV"

#ifndef OPTARG_IN_UNISTD
  extern char *optarg;
  extern int   optind;
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

int quiet_mode;
int verbose_mode;

#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"help",     0, 0, 'h'},
    {"quiet",    0, 0, 'q'},
    {"verbose",  0, 0, 'v'},
    {"version",  0, 0, 'V'}
  };
#endif

/*! \brief Print brief usage information and exit.
 * \par Function Description
 * Print brief help message describing gsch2pdf usage & command-line
 * options, then exit with exit_status 0.
 *
 * \param cmd First element of argv (name of program as run).
 */
void
usage(char *cmd)
{
  printf(_(
    "\n"
    "Usage: %s [OPTIONS] filename1 ... filenameN\n"
    "  -h, --help        Print usage\n"
    "  -q, --quiet       Quiet mode\n"
    "  -v, --verbose     Verbose mode (cumulative: errors, warnings, info)\n"
    "  -V, --version     Show version information.\n"
    "\n"
    "Generates a pdf file using the name base name of the input file.\n"
    "Each input file, filename1... filenameN, is converted to pdf format\n"
    "and printed as separate pages.\n"
    "\n"),
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
  if (!quiet_mode)
    printf(_(
      "gEDA/gsch2pdf %s (%s) (g%.7s)\n"
      "Copyright (C) 1998-2017 gEDA developers\n"
      "This is free software, and you are welcome to redistribute it under\n"
      "certain conditions. For details, see the file `COPYING', which is\n"
      "included in the gEDA distribution.\n"
      "There is NO WARRANTY, to the extent permitted by law.\n"),
      PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION, PACKAGE_GIT_COMMIT);
  else
    printf("%s\n", PACKAGE_DOTTED_VERSION);
  exit (0);
}

/*! \brief Parse gsch2pdf command-line options.
 * \par Function Description
 * Parse command line options, displaying usage message or version
 * information as required.
 *
 * \param argc Number of command-line arguments.
 * \param argv Array of command-line arguments.
 *
 * \return index into \a argv of first non-option argument.
 */
int parse_commandline(int argc, char *argv[])
{
  int ch;

#ifdef HAVE_GETOPT_LONG
  while ((ch = getopt_long (argc, argv, OPTIONS, long_options, NULL)) != -1) {
#else
  while ((ch = getopt (argc, argv, OPTIONS)) != -1) {
#endif

    switch (ch) {

      case 'h':
        usage(argv[0]);
        break;

      case 'q':
        quiet_mode=TRUE;
        break;

      case 'v':
        verbose_mode++;
        break;

      case 'V':
        version ();
        break;

#if 0
      case 'f':
        printf("f arg: %s\n", optarg);
        break;
#endif

      case '?':
      default:
        usage(argv[0]);
        break;
    }
  }

  return(optind);
}
