/* -*- test_picture_object.c -*-
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
 *  Date Contributed: June, 28th, 2016
 */

#include "../../config.h"

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include "test-suite.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

/*! \file test_picture_object.c
 *  \brief Tests for o_picture_object.c module
 *  \par
 *  This module provides basic unit tests for functions in the
 *  o_picture_object module.
 */

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_picture_object.c"

#define TOBJECT "GedaPicture"
#define IMAGE_FILE "../docs/logo_256x101.png"
#define IMAGE_WIDTH  256
#define IMAGE_HEIGHT 101

/** \defgroup test-object-geda-picture Test GEDA Picture object Module
 * @{
 * \brief Group 16 src/object/o_picture_object.c geda_picture_object_
 *  Group 16 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  16  88  88
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
 *      O1601    geda_picture_object_copy
 *      O1602     geda_picture_object_embed
 *      O1603     geda_picture_object_export_object
 *      O1604     geda_picture_object_export_orginal
 *      O1605     geda_picture_object_export_pixbuf
 *      O1606    geda_picture_object_get_data
 *      O1607    geda_picture_object_get_effective_ratio
 *      O1608     geda_picture_object_get_fallback_pixbuf
 *      O1609     geda_picture_object_get_filename
 *      O1610     geda_picture_object_get_height
 *      O1611     geda_picture_object_get_mask_data
 *      O1612     geda_picture_object_get_nearest_point
 *      O1613     geda_picture_object_get_pixbuf
 *      O1614     geda_picture_object_get_pixbuf_fit
 *      O1615     geda_picture_object_get_position
 *      O1616     geda_picture_object_get_ratio
 *      O1617     geda_picture_object_get_rgb_data
 *      O1618     geda_picture_object_get_width
 *      O1619     geda_picture_object_is_embedded
 *      O1620     geda_picture_object_mirror
 *      O1621     geda_picture_object_modify
 *      O1623     geda_picture_object_modify_all
 *      O1623    geda_picture_object_new
 *      O1624     geda_picture_object_print
 *      O1625     geda_picture_object_read
 *      O1626     geda_picture_object_rotate
 *      O1627     geda_picture_object_save
 *      O1628     geda_picture_object_scale
 *      O1629     geda_picture_object_set_from_buffer
 *      O1630     geda_picture_object_set_from_file
 *      O1631     geda_picture_object_shortest_distance
 *      O1632     geda_picture_object_translate
 *      O1633     geda_picture_object_unembed
 */

static int remove_file = 0;

/* Copies the test image file from the source directory to the
 * build during checks from VPATH builds, aka distchecks */
int pretest(void)
{
  int result = 0;

  if (access(IMAGE_FILE, R_OK) != 0) {

    char *src_dir = getenv ("srcdir");

    if (src_dir) {
      char *source;
      source = g_build_filename(src_dir, IMAGE_FILE, NULL);
      result = geda_file_copy(source, IMAGE_FILE);
      g_free (source);
      remove_file = 1;
    }
  }

  return result;
}

/* Removes image file copied for VPATH build */
void posttest(void)
{
  /* The remove_file was set only if the file was copied during
   * a make check in a VPATH build, so remove the file if set */
  if (remove_file) {
    if(access(IMAGE_FILE, R_OK) == 0) {
      if (geda_file_sys_remove(IMAGE_FILE)) {
        fprintf(stderr,"Error removing file <%s>: %s\n", IMAGE_FILE, strerror(errno));
        exit (1);
      }
    }
    else {
      fprintf(stderr,"Error accessing file <%s>: %s\n", IMAGE_FILE, strerror(errno));
      exit (1);
    }
  }
}

int check_construction (void)
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int left = geda_random_number ( 0,     115000);
    int top  = geda_random_number ( 0,      75000);

    int right  = left + IMAGE_WIDTH;
    int bottom = top  - IMAGE_HEIGHT;

    GedaObject *object0 = geda_picture_object_new(NULL, 0, IMAGE_FILE,
                                                  left, top, right, bottom,
                                                  0, FALSE, FALSE);
    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O162301A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_PICTURE(object0->picture)) {
      fprintf(stderr, "FAILED: (O162301B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaPicture *picture  = object0->picture;
      int fail = 0;

      if (picture->upper_x - left) {
        fprintf(stderr, "FAILED: (O162301L) picture upper_x %d != %d\n", picture->upper_x, left);
        fail++;
      }

      if (picture->upper_y - top) {
        fprintf(stderr, "FAILED: (O162301T) picture upper_y %d != %d\n", picture->upper_y, top);
        fail++;
      }

      if (picture->lower_x - right) {
        fprintf(stderr, "FAILED: (O162301R) picture lower_x %d != %d\n", picture->lower_x, right);
        fail++;
      }

      if (picture->lower_y - bottom) {
        fprintf(stderr, "FAILED: (O162301B) picture lower_y %d != %d\n", picture->lower_y, bottom);
        fail++;
      }

      GedaObject *object1 = geda_picture_object_copy(object0);

      if (!GEDA_IS_OBJECT(object1)) {
        fprintf(stderr, "FAILED: (O160101A) geda_picture_object_copy\n");
        result++;
      }
      else {

        GedaPicture *picture2  = object1->picture;

        if (picture2->upper_x - left) {
          fprintf(stderr, "FAILED: (O160101L) picture left %d != %d\n", picture->upper_x, left);
          fail++;
        }

        if (picture2->upper_y - top) {
          fprintf(stderr, "FAILED: (O160101T) picture top %d != %d\n", picture->upper_y, top);
          fail++;
        }

        if (picture2->lower_x - right) {
          fprintf(stderr, "FAILED: (O160101R) picture right %d != %d\n", picture->lower_x, right);
          fail++;
        }

        if (picture2->lower_y - bottom) {
          fprintf(stderr, "FAILED: (O160101B) picture bottom %d != %d\n", picture->lower_y, bottom);
          fail++;
        }
      }

      if (fail) {
        result++;
        break;
      }

      g_object_unref (object1);
    }
    g_object_unref (object0);
  }
  return result;
}

int check_accessors (void)
{
  int result = 0;

  int left = 100;
  int top  = 200;

  int right  = left + IMAGE_WIDTH;
  int bottom = top  - IMAGE_HEIGHT;

  GedaObject *object0 = geda_picture_object_new(NULL, 0, IMAGE_FILE,
                                                left, top, right, bottom,
                                                0, FALSE, FALSE);
  if (!GEDA_IS_OBJECT(object0)) {
    fprintf(stderr, "FAILED: (O162302) New GedaObject Failed\n");
    result++;
  }

  /* === Function 06: geda_picture_object_get_data  === */

  const char *data;
  unsigned int len;

  data = geda_picture_object_get_data(object0, &len);

  if (!data) {
    fprintf(stderr, "FAILED: (O160601A) get_data NULL\n");
    result++;
  }
  else if (len != 8850) {
    fprintf(stderr, "FAILED: (O160601B) get_data len %u\n", len);
    result++;
  }

  /* === Function 07: geda_picture_object_get_effective_ratio  === */

  double ratio;

  ratio = geda_picture_object_get_effective_ratio(object0);

  if (abs((ratio - (IMAGE_WIDTH / IMAGE_HEIGHT))) > 0.000001) {
    fprintf(stderr, "FAILED: (O160701) get_effective_ratio <%f>\n", ratio);
    result++;
  }

  return result;
}

/** @} endgroup test-object-geda-picture */

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (pretest()) {
    fprintf(stderr, "Error during pretest, check %s\n\n", __FILE__);
    return 1;
  }

  if (setjmp(point) == 0) {
    result = check_construction();
  }
  else {
    fprintf(stderr, "Caught signal in constructors %s\n\n", __FILE__);
    return 1;
  }

  if (!result) {

    if (setjmp(point) == 0) {
      result = check_accessors();
    }
    else {
      fprintf(stderr, "Caught signal checking accessors in %s\n\n", MUT);
      return 1;
    }
  }

  posttest();
  return result;
}
