/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2016 Stuart D. Brorson.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*!
 * \file
 * \brief Functions to parse the command line.
 *
 * Functions to parse the command line and to provide usage
 * information.
 */

#include <geda/ansi.h>
#include <version.h>

#include <gattrib.h>

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif  /* Checking for getopt  */

#if !defined(HAVE_GETOPT_LONG) || !defined(HAVE_GETOPT_H)
/*! \brief Command line option string for getopt.
 *
 *  Command line option string for getopt. Defines "q" for quiet,
 *  "v" for verbose and "h" for help.
 */
#define OPTIONS "eqvh"
#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif
#endif   /* Checking for getopt_long  */

#include <geda_debug.h>

/*!
 * \brief Print usage message
 * \par Function Description
 *  Prints gattrib usage information to stdout.
 *
 * \param cmd Unused parameter.
 */
void usage(char *cmd)
{
    printf(_(
"\n"
"Gattrib:  The gEDA project\'s attribute editor.\n"
"Presents schematic attributes in easy-to-edit spreadsheet format.\n"
"\n"
"Usage: %s [OPTIONS] filename1 ... filenameN\n"
"\n"
"\t-e, --export-csv export file to comma separated values\n"
"\t-h, --help       This help menu\n"
"\t-q, --quiet      Enable quiet mode\n"
"\t-v, --verbose    Enable verbose mode\n"
"\t-V, --version    Show version information.\n"
"\n"
"  FAQ:\n"
"  *  What do the colors of the cell text mean?\n"
"     The cell text colors indicate the visibility of the attribute.\n"
"     " ATT_REVERSE "Black" RESET " = Visible attribute, value displayed only.\n"
"     " FG_GRAY   "Grey" RESET "  = Invisible attribute.\n"
"     " FG_L_RED  "Red"  RESET "   = Visible attribute, name displayed only.\n"
"     " FG_L_BLUE "Blue" RESET "  = Visible attribute, both name and value displayed.\n"
"\n"
"  *  What does the period (\".\") at the end of some component refdes mean?\n"
"     The period is placed after the refdes of slotted components.\n"
"     If slots are present on the component, then the different slots appear\n"
"     in different rows with the slot number after the period.  Example:  C101.2.\n"
"\n"
"Copyright (C) 2003-2017 Stuart D. Brorson. E-mail: sdb (AT) cloud9 (DOT) net.\n"
"\n"), cmd);
    exit(0);
}

/*!
 * \brief Print version info and exit.
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
    printf( "gEDA/gattrib %s (%s) (g%.7s)\n%s",
      PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION, PACKAGE_GIT_COMMIT, string);
  }
  else {
    printf("%s\n", PACKAGE_DOTTED_VERSION);
  }
  exit (0);
}

/*!
 * \brief Parse command line switches.
 * \par Function Description
 * Parse command line switches at startup. There are 5 commandline
 * switches:
 *
 * - export-csv
 * - help
 * - quiet
 * - verbose
 * - version
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments (array of strings)
 * \returns unknown - looks uninitialized in some circumstances.
 */
int parse_commandline(int argc, char *argv[])
{

#if defined(HAVE_GETOPT_LONG) && defined(HAVE_GETOPT_H)

  /* Use getopt_long if it is available */
  int option_index = 0;
  static struct option long_options[] = {
    {"export-csv", 1, 0, 'e'},
    {"help",       0, 0, 'h'},
    {"quiet",      0, 0, 'q'},
    {"verbose",    0, 0, 'v'},
    {"version",    0, 0, 'V'},
    {0, 0, 0, 0}
  };

  while (1) {

    int ch;

    ch = getopt_long(argc, argv, "ehqvV", long_options, &option_index);

    if (ch == -1) {
      break;
    }

#else

    /* Otherwise just use regular getopt */
  while ((ch = getopt(argc, argv, OPTIONS)) != -1) {

#endif

    switch (ch) {
      case 'e':
        export_mode = 1;
        break;

      case 'h':
        usage(argv[0]);
        break;

      case 'q':
        quiet_mode = TRUE;
        break;

      case 'v':
        verbose_mode = TRUE;
        break;

      case 'V':
        version();
        break;

      case '?':
      default:
        usage(argv[0]);
        break;
    }

    if (quiet_mode) {
      verbose_mode = FALSE;
    }
  }
  return (optind);
}

