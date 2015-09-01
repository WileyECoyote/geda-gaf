/*!
 * \file parsecmd.c
 *
 * \brief Parse Command-line Arguments in gschlas Program
 *
 * <hr>
 *
 * <h1><b>Copyright.</b></h1>\n
 * gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 *
 * Copyright (C) 2002-2015 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors (see ChangeLog for details)
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
 * along with this program; if  not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include <config.h>

#include "common.h"

#define OPTIONS "hqveu"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"embed",   0, 0, 'e'},
    {"unembed", 0, 0, 'u'},
    {"quiet",   0, 0, 'q'},
    {"verbose", 0, 0, 'v'},
    {"help",    0, 0, 'h'}
  };
#endif

void usage(char *cmd)
{
    printf("Usage: %s [OPTIONS] filename1 ... filenameN\n\n", cmd);
    printf("  -e, --embed       Embed all components/pictures\n");
    printf("  -u, --unembed     Unembed all components/pictures\n");
    printf("  -q, --quiet       Quiet mode\n");
    printf("  -v, --verbose     Verbose mode on\n");
    printf("  -h, --help        This message\n");
    printf("\n");
    exit(0);
}

int parse_commandline(int argc, char *argv[])
{
  int ch;

#ifdef HAVE_GETOPT_LONG
  while ((ch = getopt_long (argc, argv, OPTIONS, long_options, NULL)) != -1)
#else
  while ((ch = getopt(argc, argv, OPTIONS)) != -1)
#endif

  {
    switch (ch) {

      case 'v':
        verbose_mode = TRUE;
        break;

      case 'q':
        quiet_mode = TRUE;
        break;

      case 'e':
        embed_mode = TRUE;
        break;

      case 'u':
        unembed_mode = TRUE;
        break;

      case 'h':
      case '?':
      default:
        usage(argv[0]);
        break;
    }
  }

  if (quiet_mode) {
    verbose_mode = FALSE;
  }

  return (optind);
}
