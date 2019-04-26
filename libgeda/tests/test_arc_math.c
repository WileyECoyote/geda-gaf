/* -*- test_arc_math.c -*-
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
 *  Date Contributed: April, 26th, 2019
 */

#include "../../config.h"

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>

#include <math.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/math/m_arc.c"

/** \defgroup test-arc-math Test Arc Math Module
 * @{
 * \brief Group 4 src/math/m_arc.c geda_math_arc_
 *  Group 4 == Module/File No.
 */

/*
 *  Test Identifiers:  M  03  88  88
 *                     ^   ^   ^   ^
 *    group-code ______|   |   |   |
 *                         |   |   |
 *    Module/File No. _____|   |   |
 *                             |   |
 *    Function No._____________|   |
 *                                 |
 *    Tests Number ________________|
 *
 *  See tests/README for more details on the nomenclature for test identifiers.
 *
 *      M0301    geda_math_arc_chord
 *      M0302    geda_math_arc_length
 *      M0303    geda_math_arc_includes_point
 */

int check_math_arc_chord ()
{
  int result = 0;
  return result;
}

int check_math_arc_length ()
{
  int result = 0;
  return result;
}

int check_math_arc_includes_point ()
{
  int result = 0;
  return result;
}

int main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (setjmp(point) == 0) {
    result += check_math_arc_chord();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_chord in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_arc_length();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_length in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_arc_includes_point();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_includes_point in %s\n\n", MUT);
    return 1;
  }

  return result;
}
