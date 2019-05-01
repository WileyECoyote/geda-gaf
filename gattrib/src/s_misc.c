/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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

/*! \file
 *  \brief Miscellaneous STRING_LIST functions
 */

#include <gattrib.h>
#include <geda_debug.h>

/*------------------------------------------------------------------
 * The below fcns identical to those defined in
 * geda-gnetlist/src/s_misc.c
 *------------------------------------------------------------------*/
/*!
 * Running count of number of characters printed on current line.
 */
static int char_index = 0;

/*! \brief Print message in verbose mode
 *
 * Print the supplied message in verbose mode. Line wrap if necessary.
 *
 * Identical to that defined in gnetlist/src/s_misc.c
 * \param string String to be printed
 */
void verbose_print(char *string)
{
    if (verbose_mode) {
	printf("%s", string);
	char_index++;
	if ((char_index + 1) >= 78) {
	    printf("\n");
	    char_index = 0;
	}
    }
}

/*! \brief Print "DONE" message in verbose mode
 *
 * Prints the "DONE" message in verbose mode, wrapping before printing
 * if near the end of line.
 *
 * Identical to function defined in gnetlist/src/s_misc.c
 */
void verbose_done(void)
{
  if (verbose_mode) {

    const char *_Done =  _("DONE");

    if (char_index >= 70) {
      printf("\n%s\n",_Done);
    }
    else {
      printf(" %s\n", _Done);
    }

    char_index = 0;
  }
}

/*! \brief Reset the running character count
 *
 * Reset the current characted count.
 *
 * Identical to function defined in gnetlist/src/s_misc.c
 */

void verbose_reset_index(void)
{
    char_index = 0;
}


/*------------------------------------------------------------------
 * Gattrib specific utilities
 *------------------------------------------------------------------*/
char *s_misc_remaining_string(char *string, char delimiter, int count)
{
  int i;
  char *remaining;
  char *return_value;

  /* find count'th delimiter */
  remaining = string;
  for (i = 0; i < count; i++) {
    remaining = strchr(remaining, delimiter);
    if (!remaining) {
      return (NULL);
    }
    remaining++;
  }

  /* skip whitespace */
  while (*remaining == ' ') {
    remaining++;
  }
  if (!(*remaining)) {
    return (NULL);
  }

  /* copy remainder into allocated return string */
  return_value = geda_utility_string_strdup(remaining);

  /* return string */
  return (return_value);
}
