/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_misc.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2018 Ales Hvezda
 * Copyright (C) 1998-2018 gEDA Contributors (see ChangeLog for details)
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

#include "../../config.h"

#include <gnetlist.h>
#include <gettext.h>
#include <geda_debug.h>

static int char_index = 0;

/*!
 * \brief Verbose Print a String
 * \par Function Description
 *  Writes \a string to stdout if verbose and increments char_index
 *  If char_index is > 77, a line-feed/carriage return is also
 *  written char_index reset to 0.
 */
void verbose_print(char *string)
{
  if (verbose_mode) {

    printf("%s", string);

    char_index++;

    if ((char_index + 1) > 77) {
      printf("\n");
      char_index = 0;
    }
  }
}

/*!
 * \brief Verbose Done
 * \par Function Description
 *  Write "Done" to stdout if verbose, if the char_index greater than 69,
 *  the output is prepended with a line-feed/carriage return.
 */
void verbose_done(void)
{
  if (verbose_mode) {

    if (char_index > 69) {
      printf("\n");
    }

    printf("%s\n", _("DONE"));

    char_index = 0;
  }
}

/*!
 * \brief Reset the character Index to Zero
 * \par Function Description
 *  The char_index variable is used to control the number of characters
 *  written to the output before a line-feed/carriage return is sent to
 *  the output. Calling this function resets the index to zero.
 */
void verbose_reset_index(void)
{
    char_index = 0;
}
