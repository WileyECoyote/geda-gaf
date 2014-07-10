/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
 * MA 02110-1301 USA
 */

/*! \file o_picture.c
 *  \brief functions for the picture object
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gio/gio.h>

#include "libgeda_priv.h"

/*! \brief Create picture Object from character string.
 *  \par Function Description
 *  Parses \a first_line and subsequent lines from \a tb, and returns
 *  a newly-created picture #Object.
 *
 *  \param [in]  first_line      Character string with picture description.
 *  \param [in]  tb              Text buffer to load embedded data from.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *
 *  \param [out] err           A GError obejct
 *
 *  \return A pointer to the new picture object, or NULL on error.
 */
Object *o_picture_read (const char *first_line,
                        TextBuffer *tb,
                        unsigned int release_ver,
                        unsigned int fileformat_ver,
                        GError **err)
{
  Object *new_obj;
  int x1, y1;
  int width, height, angle;
  int mirrored, embedded;
  int num_conv;
  char type;
  const char *line = NULL;
  char *filename;
  char *file_content = NULL;
  unsigned int file_length = 0;

  num_conv = sscanf(first_line, "%c %d %d %d %d %d %d %d\n",
                    &type, &x1, &y1, &width, &height, &angle, &mirrored, &embedded);

  if (num_conv != 8) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse picture definition"));
    return NULL;
  }

  if (width == 0 || height == 0) {
    u_log_message(_("Found a zero width/height picture [ %c %d %d %d %d ]\n"),
                  type, x1, y1, width, height);
  }

  if ( (mirrored > 1) || (mirrored < 0)) {
    u_log_message(_("Found a picture with a wrong 'mirrored' parameter: %d.\n"),
                  mirrored);
    u_log_message(_("Setting mirrored to 0\n"));
    mirrored = 0;
  }

  if ( (embedded > 1) || (embedded < 0)) {
    u_log_message(_("Found a picture with a wrong 'embedded' parameter: %d.\n"),
                  embedded);
    u_log_message(_("Setting embedded to 0\n"));
    embedded = 0;
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
      break;

    default:
      u_log_message(_("Found an unsupported picture angle [ %d ]\n"), angle);
      u_log_message(_("Setting angle to 0\n"));
      angle=0;
      break;

  }

  filename = g_strdup(s_textbuffer_next_line(tb));
  filename = remove_last_nl(filename);

  /* Handle empty filenames */
  if (strlen (filename) == 0) {
    u_log_message (_("Found an image with no filename."));
    GEDA_FREE (filename);
    filename = NULL;
  }

  if (embedded == 1) {
    GString *encoded_picture=g_string_new("");
    char finished = 0;

    /* Read the encoded picture */
    do {

      line = s_textbuffer_next_line(tb);
      if (line == NULL) break;

      if (g_ascii_strcasecmp(line, ".\n") != 0) {
        encoded_picture = g_string_append (encoded_picture, line);
      } else {
        finished = 1;
      }
    } while (finished == 0);

    /* Decode the picture */
    if (encoded_picture != NULL) {
      file_content = s_encoding_base64_decode(encoded_picture->str,
                                              encoded_picture->len,
                                              &file_length);
      g_string_free (encoded_picture, TRUE);
    }

    if (file_content == NULL) {
      u_log_message (_("Failed to load image from embedded data [%s]: %s\n"),
                     filename, _("Base64 decoding failed."));
      u_log_message (_("Falling back to file loading. Picture unembedded.\n"));
      embedded = 0;
    }
  }

  /* create the picture */
  /* The picture is described by its upper left and lower right corner */
  new_obj = o_picture_new (file_content, file_length, filename,
                           x1, y1+height, x1+width, y1,
                           angle, mirrored, embedded);

  GEDA_FREE (file_content);
  GEDA_FREE (filename);

  return new_obj;
}

/*! \brief Create a character string representation of a picture Object.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe
 *  the picture object <B>*object</B>.
 *
 *  \param [in] object  Picture Object to create string from.
 *  \return A pointer to the picture Object character string.
 *
 *  \note
 *  Caller must GEDA_FREE returned character string.
 *
 */
char *o_picture_save(Object *object)
{
  int           width, height, x1, y1;
  char         *encoded_picture=NULL;
  char         *out=NULL;
  unsigned int  encoded_picture_length;
  const char   *filename = NULL;

  g_return_val_if_fail(GEDA_IS_PICTURE(object), NULL);

  /* calculate the width and height of the box */
  width  = abs(object->picture->lower_x - object->picture->upper_x);
  height = abs(object->picture->upper_y - object->picture->lower_y);

  /* calculate the lower left corner of the box */
  x1 = object->picture->upper_x;
  y1 = object->picture->upper_y - height; /* move the origin to 0, 0*/

  #if DEBUG
  printf("picture: %d %d %d %d\n", x1, y1, width, height);
  #endif

  /* Encode the picture if it's embedded */
  if (o_picture_is_embedded (object)) {
    encoded_picture =
    s_encoding_base64_encode( (char *)object->picture->file_content,
                              object->picture->file_length,
                              &encoded_picture_length,
                              TRUE);
    if (encoded_picture == NULL) {
      u_log_message(_("ERROR: o_picture_save: unable to encode the picture.\n"));
    }
  }

  /* Cope with null filename */
  filename = o_picture_get_filename (object);
  if (filename == NULL) filename = "";

  if (o_picture_is_embedded (object) &&
    encoded_picture != NULL) {
    out = g_strdup_printf("%c %d %d %d %d %d %c %c\n%s\n%s\n%s",
                          object->type,
                          x1, y1, width, height,
                          object->picture->angle,
                          /* Convert the (0,1) chars to ASCII */
                          (object->picture->mirrored)+0x30,
                          '1',
                          filename,
                          encoded_picture,
                          ".");
    }
    else {
      out = g_strdup_printf("%c %d %d %d %d %d %c %c\n%s",
      object->type,
      x1, y1, width, height,
      object->picture->angle,
      /* Convert the (0,1) chars to ASCII */
      (object->picture->mirrored)+0x30,
      '0',
      filename);
    }
    GEDA_FREE(encoded_picture);

    return(out);
}


/*! \brief Create a picture object.
 *  \par Function Description
 *  This function creates a new object representing a picture.
 *
 *  The picture is described by its upper left corner (\a x1, \a y1)
 *  and its lower right corner (\a x2, \a y2).  The \a type parameter
 *  must be equal to #OBJ_PICTURE.
 *
 *  If \a file_content is non-NULL, it must be a pointer to a buffer
 *  containing raw image data.  If loading data from \a file_content
 *  is unsuccessful, and \a filename is non-NULL, an image will
 *  attempt to be loaded from \a filename.  Otherwise, the picture
 *  object will be initially empty.
 *
 *  \param [in]     file_content  Raw data of the image file, or NULL.
 *  \param [in]     file_length   Length of raw data buffer
 *  \param [in]     filename      File name backing this picture, or NULL.
 *  \param [in]     x1            Upper x coordinate.
 *  \param [in]     y1            Upper y coordinate.
 *  \param [in]     x2            Lower x coordinate.
 *  \param [in]     y2            Lower y coordinate.
 *  \param [in]     angle         Picture rotation angle.
 *  \param [in]     mirrored      Whether the image should be mirrored or not.
 *  \param [in]     embedded      Whether the embedded flag should be set or not.
 *  \return A pointer to a new picture #Object.
 */
Object *o_picture_new (const char *file_content, gsize file_length,
                       const char *filename,
                       int x1, int y1, int x2, int y2, int angle,
                       int mirrored, int embedded)
{
  Object  *new_obj;
  Picture *picture;

  /* create the object */
  new_obj = geda_picture_new();

  picture = GEDA_PICTURE(new_obj);

  /* describe the picture with its upper left and lower right corner */
  picture->upper_x = (x1 > x2) ? x2 : x1;
  picture->upper_y = (y1 > y2) ? y1 : y2;
  picture->lower_x = (x1 > x2) ? x1 : x2;
  picture->lower_y = (y1 > y2) ? y2 : y1;

  picture->pixbuf = NULL;
  picture->file_content = NULL;
  picture->file_length = 0;

  picture->angle = angle;
  picture->mirrored = mirrored;
  picture->embedded = embedded;

  /* Can not divide by zero */
  if ( (y1 - y2) != 0 ) {
    picture->ratio = abs ((x1 - x2) / (y1 - y2));
  }

  if ( filename ) {
    picture->filename = geda_strdup (filename);
  }

  if (file_content != NULL) {
    GError *error = NULL;
    if (!o_picture_set_from_buffer (new_obj, filename,
                                    file_content, file_length, &error)) {
      u_log_message (_("Failed to load buffer image [%s]: %s\n"),
                     filename, error->message);
      g_error_free (error);

      /* Force the data into the object anyway, so as to prevent data
       * loss of embedded images. */
      picture->file_content = g_memdup (file_content, file_length);
      picture->file_length = file_length;
    }
  }

  if (picture->pixbuf == NULL && filename != NULL) {
    GError *error = NULL;
    if (!o_picture_set_from_file (new_obj, filename, &error)) {
      u_log_message (_("Failed to load image from [%s]: %s\n"),
                     filename, error->message);
      g_error_free (error);
      s_object_release(new_obj);
      new_obj = NULL;
    }
  }

  return new_obj;

}

/*! \brief get the position of the left bottom point
 *  \par Function Description
 *  This function gets the position of the bottom left point of a picture object.
 *
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
bool o_picture_get_position (int *x, int *y, Object *object)
{
  *x = min(object->picture->lower_x, object->picture->upper_x);
  *y = min(object->picture->lower_y, object->picture->upper_y);
  return TRUE;
}


/*! \brief Get the width/height ratio of an image.
 *
 * \par Function Description

 * Returns the width/height ratio of picture \a object, taking the
 * image rotation into account.
 *
 * \param object    Picture #Object to inspect.
 * \return width/height ratio for \a object.
 */
float
o_picture_get_ratio (Object *object)
{

  g_return_val_if_fail (GEDA_IS_PICTURE(object), 1);

  /* The effective ratio varies depending on the rotation of the
   * image. */
  switch (object->picture->angle) {
  case 0:
  case 180:
    return object->picture->ratio;
  case 90:
  case 270:
    return 1.0 / object->picture->ratio;
  default:
    g_critical (_("Picture %p has invalid angle %i\n"), object,
                object->picture->angle);
  }
  return 0;
}

/*! \brief Modify the description of a picture Object.
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the picture. The new coordinates of the corner identified by
 *  <B>whichone</B> are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in,out] object     Picture Object to modify.
 *  \param [in]     x          New x coordinate.
 *  \param [in]     y          New y coordinate.
 *  \param [in]     whichone   Which picture parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>PICTURE_UPPER_LEFT
 *    <DT>*</DT><DD>PICTURE_LOWER_LEFT
 *    <DT>*</DT><DD>PICTURE_UPPER_RIGHT
 *    <DT>*</DT><DD>PICTURE_LOWER_RIGHT
 *  </DL>
 */
void
o_picture_modify(Object *object, int x, int y, int whichone)
{
  int tmp;
  float ratio = o_picture_get_ratio (object);

  /* change the position of the selected corner */
  switch(whichone) {
    case PICTURE_UPPER_LEFT:
      object->picture->upper_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y < object->picture->lower_y) {
        tmp = -tmp;
      }
      object->picture->upper_y = object->picture->lower_y + tmp;
      break;

    case PICTURE_LOWER_LEFT:
      object->picture->upper_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y > object->picture->upper_y) {
        tmp = -tmp;
      }
      object->picture->lower_y = object->picture->upper_y - tmp;
      break;

    case PICTURE_UPPER_RIGHT:
      object->picture->lower_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y < object->picture->lower_y) {
        tmp = -tmp;
      }
      object->picture->upper_y = object->picture->lower_y + tmp;
      break;

    case PICTURE_LOWER_RIGHT:
      object->picture->lower_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / ratio;
      if (y > object->picture->upper_y) {
        tmp = -tmp;
      }
      object->picture->lower_y = object->picture->upper_y - tmp;
      break;

    default:
      return;
  }

  /* need to update the upper left and lower right corners */
  if(object->picture->upper_x > object->picture->lower_x) {
    tmp                      = object->picture->upper_x;
    object->picture->upper_x = object->picture->lower_x;
    object->picture->lower_x = tmp;
  }

  if(object->picture->upper_y < object->picture->lower_y) {
    tmp                      = object->picture->upper_y;
    object->picture->upper_y = object->picture->lower_y;
    object->picture->lower_y = tmp;
  }

  /* recalculate the screen coords and the boundings */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Modify a picture object's coordinates.
 * \par Function Description
 * Modifies the coordinates of all four corners of a picture \a
 * object.  The picture is adjusted to fit the rectangle enclosed by
 * the points (\a x1, \a y1) and (\a x2, \a y2), and scaled as large
 * as possible to still fit within that rectangle.
 *
 * \param [in,out] object   picture #Object to be modified.
 * \param [in]     x1       x coordinate of first corner of box.
 * \param [in]     y1       y coordinate of first corner of box.
 * \param [in]     x2       x coordinate of second corner of box.
 * \param [in]     y2       y coordinate of second corner of box.
 */
void
o_picture_modify_all (Object *object, int x1, int y1, int x2, int y2)
{
  /* Normalise the requested rectangle. */
  object->picture->lower_x = (x1 > x2) ? x1 : x2;
  object->picture->lower_y = (y1 > y2) ? y2 : y1;
  object->picture->upper_x = (x1 > x2) ? x2 : x1;
  object->picture->upper_y = (y1 > y2) ? y1 : y2;

  /* recalculate the world coords and bounds */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Rotate picture Object using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the picture described by <B>*object</B> around
 *  the (<B>world_centerx</B>, <B>world_centery</B>) point by <B>angle</B>
 *  degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      world_centerx  Rotation center x coordinate in
 *                                  WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in
 *                                  WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Picture Object to rotate.
 */
void
o_picture_rotate_world( int world_centerx, int world_centery, int angle, Object *object)
{
  int newx1, newy1;
  int newx2, newy2;

  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;

  object->picture->angle = ( object->picture->angle + angle ) % 360;

  /* The center of rotation (<B>world_centerx</B>, <B>world_centery</B>) is
   * translated to the origin. The rotation of the upper left and lower
   * right corner are then performed. Finally, the rotated picture is
   * translated back to its previous location.
   */
  /* translate object to origin */
  object->picture->upper_x -= world_centerx;
  object->picture->upper_y -= world_centery;
  object->picture->lower_x -= world_centerx;
  object->picture->lower_y -= world_centery;

  /* rotate the upper left corner of the picture */
  rotate_point_90(object->picture->upper_x, object->picture->upper_y, angle,
                  &newx1, &newy1);

  /* rotate the lower left corner of the picture */
  rotate_point_90(object->picture->lower_x, object->picture->lower_y, angle,
                  &newx2, &newy2);

  /* reorder the corners after rotation */
  object->picture->upper_x = min(newx1,newx2);
  object->picture->upper_y = max(newy1,newy2);
  object->picture->lower_x = max(newx1,newx2);
  object->picture->lower_y = min(newy1,newy2);

  /* translate object back to normal position */
  object->picture->upper_x += world_centerx;
  object->picture->upper_y += world_centery;
  object->picture->lower_x += world_centerx;
  object->picture->lower_y += world_centery;

  /* recalc boundings and screen coords */
  object->w_bounds_valid_for = NULL;

}

/*! \brief Mirror a picture using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the picture from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The picture is first translated to the origin, then mirrored and
 *  finally translated back at its previous position.
 *
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Picture Object to mirror.
 */
void o_picture_mirror_world(int world_centerx, int world_centery,
                            Object *object)
{
  int newx1, newy1;
  int newx2, newy2;

  /* Set info in object. Sometimes it's necessary to change the
   * rotation angle as well as the mirror flag. */
  object->picture->mirrored = !object->picture->mirrored;
  switch (object->picture->angle) {
  case 90:
    object->picture->angle = 270;
    break;
  case 270:
    object->picture->angle = 90;
    break;
  }

  /* translate object to origin */
  object->picture->upper_x -= world_centerx;
  object->picture->upper_y -= world_centery;
  object->picture->lower_x -= world_centerx;
  object->picture->lower_y -= world_centery;

  /* mirror the corners */
  newx1 = -object->picture->upper_x;
  newy1 = object->picture->upper_y;
  newx2 = -object->picture->lower_x;
  newy2 = object->picture->lower_y;

  /* reorder the corners */
  object->picture->upper_x = min(newx1,newx2);
  object->picture->upper_y = max(newy1,newy2);
  object->picture->lower_x = max(newx1,newx2);
  object->picture->lower_y = min(newy1,newy2);

  /* translate back in position */
  object->picture->upper_x += world_centerx;
  object->picture->upper_y += world_centery;
  object->picture->lower_x += world_centerx;
  object->picture->lower_y += world_centery;

  /* recalc boundings and screen coords */
  object->w_bounds_valid_for = NULL;

}

/*! \brief Translate a picture position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the picture
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world units.
 *
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 *  \param [in,out] object     Picture Object to translate.
 */
void
o_picture_translate_world(int dx, int dy, Object *object)
{
  /* Do world coords */
  object->picture->upper_x = object->picture->upper_x + dx;
  object->picture->upper_y = object->picture->upper_y + dy;
  object->picture->lower_x = object->picture->lower_x + dx;
  object->picture->lower_y = object->picture->lower_y + dy;

  /* recalc the screen coords and the bounding picture */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Create a copy of a picture
 *
 *  \par Function Description
 *  This function creates a verbatim copy of the object pointed by
 *  <B>o_current</B> describing a picture.
 *
 *  \param [in]  o_current     Picture Object to copy.
 *  \return The new Object
 */
Object *o_picture_copy(Object *o_current)
{
  Object  *new_obj;
  Picture *new_picture;
  Picture *old_picture = GEDA_PICTURE(o_current);

  /* create the picture object */
  new_obj = geda_picture_new();
  new_picture = GEDA_PICTURE(new_obj);

  /* describe the picture with its upper left and lower right corner */
  new_picture->upper_x = old_picture->upper_x;
  new_picture->upper_y = old_picture->upper_y;
  new_picture->lower_x = old_picture->lower_x;
  new_picture->lower_y = old_picture->lower_y;

  if (old_picture->file_content != NULL) {
    new_picture->file_content = g_memdup (old_picture->file_content,
                                          old_picture->file_length);
  } else {
    new_picture->file_content = NULL;
  }

  new_picture->file_length = old_picture->file_length;
  new_picture->filename    = g_strdup (old_picture->filename);
  new_picture->ratio       = old_picture->ratio;
  new_picture->angle       = old_picture->angle;
  new_picture->mirrored    = old_picture->mirrored;
  new_picture->embedded    = old_picture->embedded;

  /* Get the picture data */
  new_picture->pixbuf = o_picture_get_pixbuf (o_current);

  return new_obj;
}

/*! \brief Test whether a picture object is embedded.
 * \par Function Description
 * Returns TRUE if the picture \a object will have its data embedded
 * in a schematic or symbol file; returns FALSE if its data will be
 * obtained from a separate file.
 *
 * \param object    The picture #Object to inspect.
 * \return TRUE if \a object is embedded.
 */
bool o_picture_is_embedded (Object *object)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);

  return object->picture->embedded;
}

/*! \brief Get RGB data from image.
 *  \par Function Description
 *  This function returns the RGB data of the given image.
 *  Function taken from the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 *  \param [in] image  GdkPixbuf image to read RGB data from.
 *  \return Array of rgb data from image.
 *
 *  \note
 *  Caller must GEDA_FREE returned uint8 array.
 */
static unsigned char*
o_picture_rgb_data(GdkPixbuf *image)
{
  int width = gdk_pixbuf_get_width(image);
  int height = gdk_pixbuf_get_height(image);
  int rowstride = gdk_pixbuf_get_rowstride(image);
  int size = height*rowstride;
  uint8 *rgb_pixels = g_malloc(size);

  if (gdk_pixbuf_get_has_alpha(image)) {
    uint8 *pixels = gdk_pixbuf_get_pixels(image);
    int i, j;
    for (i = 0; i < height; i++) {
      for (j = 0; j < width; j++) {
        rgb_pixels[i*rowstride+j*3] = pixels[i*rowstride+j*4];
        rgb_pixels[i*rowstride+j*3+1] = pixels[i*rowstride+j*4+1];
        rgb_pixels[i*rowstride+j*3+2] = pixels[i*rowstride+j*4+2];
      }
    }
    return rgb_pixels;
  }
  else {
    uint8 *pixels = gdk_pixbuf_get_pixels(image);

    g_memmove(rgb_pixels, pixels, height*rowstride);
    return rgb_pixels;
  }
}

/*! \brief Get mask data from image.
 *  \par Function Description
 *  This function returns the mask data of the given image.
 *  Function taken from the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 *  \param [in] image  GdkPixbuf image to get mask data from.
 *  \return Array of mask data from image.
 *
 *  \note
 *  Caller must GEDA_FREE returned uint8 array.
 */
static uint8 *
o_picture_mask_data(GdkPixbuf *image)
{
  uint8 *pixels;
  uint8 *mask;
  int i, size;

  if (!gdk_pixbuf_get_has_alpha(image)) {
    return NULL;
  }

  pixels = gdk_pixbuf_get_pixels(image);

  size = gdk_pixbuf_get_width(image)*
    gdk_pixbuf_get_height(image);

  mask = g_malloc(size);

  /* Pick every fourth byte (the alpha channel) into mask */
  for (i = 0; i < size; i++)
    mask[i] = pixels[i*4+3];

  return mask;
}

/*! \brief Print picture to Postscript document.
 *  \par Function Description
 *  This function prints a picture object. The picture is defined by the
 *  coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and its width
 *  and height given by the <B>width</B> and <B>height</B> parameters.
 *
 *  If the picture object was unable to be loaded, prints a crossed
 *  box of the same dimensions.
 *
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  Function based on the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  Picture Object to write to document.
 *  \param [in] origin_x   Page x coordinate to place picture Object.
 *  \param [in] origin_y   Page y coordinate to place picture Object.
 */
void o_picture_print(GedaToplevel *toplevel, FILE *fp, Object *o_current,
                      int origin_x, int origin_y)
{
  int x1, y1, x, y;
  int height, width;
  GdkPixbuf* image = o_picture_get_pixbuf (o_current);
  int img_width, img_height, img_rowstride;
  uint8 *rgb_data;
  uint8 *mask_data;

  /* calculate the width and height of the box */
  width  = abs(o_current->picture->lower_x - o_current->picture->upper_x);
  height = abs(o_current->picture->upper_y - o_current->picture->lower_y);

  /* calculate the origin of the box */
  x1 = o_current->picture->upper_x;
  y1 = o_current->picture->upper_y;

  /* If the image failed to load, try to get hold of the fallback
   * pixbuf. */
  if (image == NULL) image = o_picture_get_fallback_pixbuf (toplevel);
  /* If the image failed to load, draw a box in the default color with a
   * cross in it. */
  if (image == NULL) {
    int line_width = o_style_get_line_width(toplevel);
    //    int line_width = (toplevel->line_style == THICK) ? LINE_WIDTH : 2;
    o_box_print_solid (toplevel, fp, x1, y1, width, height,
                       DEFAULT_COLOR_INDEX, line_width, SQUARE_CAP, -1, -1, -1, -1);
    o_line_print_solid (toplevel, fp, x1, y1, x1+width, y1-height,
                        DEFAULT_COLOR_INDEX, line_width, ROUND_CAP, -1, -1, -1, -1);
    o_line_print_solid (toplevel, fp, x1+width, y1, x1, y1-height,
                        DEFAULT_COLOR_INDEX, line_width, ROUND_CAP, -1, -1, -1, -1);
    return;
  }

  img_width = gdk_pixbuf_get_width(image);
  img_rowstride = gdk_pixbuf_get_rowstride(image);
  img_height = gdk_pixbuf_get_height(image);

  rgb_data = o_picture_rgb_data(image);
  mask_data = o_picture_mask_data(image);

  fprintf(fp, "gsave\n");

  /* color output only */
  fprintf(fp, "/pix %i string def\n", img_width * 3);
  fprintf(fp, "%i %i 8\n", img_width, img_height);
  fprintf(fp, "%i %i translate\n", x1, y1);
  fprintf(fp, "%i %i scale\n", width, height);
  fprintf(fp, "[%i 0 0 -%i 0 0]\n", img_width, img_height);

  fprintf(fp, "{currentfile pix readhexstring pop}\n");
  fprintf(fp, "false 3 colorimage\n");
  fprintf(fp, "\n");

  if (mask_data) {
    for (y = 0; y < img_height; y++) {
      for (x = 0; x < img_width; x++) {
        int i = y*img_rowstride+x*3;
        int m = y*img_width+x;
        fprintf(fp, "%02x", 255-(mask_data[m]*(255-rgb_data[i])/255));
        fprintf(fp, "%02x", 255-(mask_data[m]*(255-rgb_data[i+1])/255));
        fprintf(fp, "%02x", 255-(mask_data[m]*(255-rgb_data[i+2])/255));
      }
      fprintf(fp, "\n");
    }
  } else {
    for (y = 0; y < img_height; y++) {
      for (x = 0; x < img_width; x++) {
        int i = y*img_rowstride+x*3;
        fprintf(fp, "%02x", (int)(rgb_data[i]));
        fprintf(fp, "%02x", (int)(rgb_data[i+1]));
        fprintf(fp, "%02x", (int)(rgb_data[i+2]));
      }
      fprintf(fp, "\n");
    }
  }
  fprintf(fp, "grestore\n");
  fprintf(fp, "\n");

  GEDA_UNREF (image);
  GEDA_FREE(rgb_data);
  GEDA_FREE(mask_data);
}


/*! \brief Embed the image file associated with a picture
 *
 * \par Function Description
 * Verify that a picture has valid data associated with it, and if so,
 * mark it to be embedded.
 *
 *  \param [in]     object       The picture Object to embed
 */
void o_picture_embed (Object *object)
{
  const char *filename;
  char       *basename;

  if (o_picture_is_embedded (object)) return;

  filename = object->picture->filename;

  if (object->picture->file_content == NULL) {
    u_log_message (_("Picture [%s] has no image data.\n"), filename);
    u_log_message (_("Falling back to file loading. Picture is still unembedded.\n"));
    object->picture->embedded = 0;
    return;
  }

  object->picture->embedded = 1;

  basename = g_path_get_basename (filename);
  u_log_message (_("Picture [%s] has been embedded\n"), basename);
  GEDA_FREE (basename);
}


/*! \brief Unembed a picture, reloading the image from disk
 * \par Function Description
 * Verify that the file associated with \a object exists on disk and
 * is usable, and if so, reload the picture and mark it as unembedded.
 *
 *  \param [in]     object       The picture Object to unembed
 */
void o_picture_unembed (Object *object)
{

  GError *err = NULL;
  const char *filename = o_picture_get_filename(object);
  char *basename;

  g_return_if_fail(GEDA_IS_PICTURE(object));

  if (!o_picture_is_embedded (object)) return;

  o_picture_set_from_file (object, filename, &err);

  if (err != NULL) {
    u_log_message (_("Failed to load image from file [%s]: %s\n"),
                   filename, err->message);
    u_log_message (_("Picture is still embedded.\n"));
    g_error_free (err);
    return;
  }

  object->picture->embedded = 0;

  basename = g_path_get_basename(filename);
  u_log_message (_("Picture [%s] has been unembedded\n"), basename);
  GEDA_FREE(basename);
}

/*! \brief Calculates the distance between the given point and the closest
 * point in the picture.
 *
 *  Interrior points within the picture return a distance of zero.
 *
 *  \param [in] object       A picture Object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double o_picture_shortest_distance (Object *object,
                                    int x, int y, int force_solid)
{
  double dx, dy;
  double x1, y1, x2, y2;

  g_return_val_if_fail (object->picture != NULL, G_MAXDOUBLE);

  x1 = (double)min (object->picture->upper_x, object->picture->lower_x);
  y1 = (double)min (object->picture->upper_y, object->picture->lower_y);
  x2 = (double)max (object->picture->upper_x, object->picture->lower_x);
  y2 = (double)max (object->picture->upper_y, object->picture->lower_y);

  dx = min (((double)x) - x1, x2 - ((double)x));
  dy = min (((double)y) - y1, y2 - ((double)y));

  dx = min (dx, 0);
  dy = min (dy, 0);

  return sqrt ((dx * dx) + (dy * dy));
}

/*! \brief Get a pixel buffer for a picture object.
 * \par Function Description
 * Returns a GdkPixbuf for the picture object \a object, or NULL if
 * the picture could not be loaded.
 *
 * The returned value should have its reference count decremented with
 * GEDA_UNREF() when no longer needed.
 *
 * \param object    The picture #Object to inspect.
 * \return A GdkPixbuf for the picture.
 */
GdkPixbuf *
o_picture_get_pixbuf (Object *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  if (object->picture->pixbuf != NULL) {
    return g_object_ref (object->picture->pixbuf);
  } else {
    return NULL;
  }
}

/*! \brief Get the raw image data from a picture object
 *
 * \par Function Description
 * Returns the raw image file data underlying the picture \a object,
 * or NULL if the picture could not be loaded.
 *
 * \param object    The picture #Object to inspect.
 * \param len       Location to store buffer length.
 * \return A read-only buffer of raw image data.
 */
const char *
o_picture_get_data (Object *object, size_t *len)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  *len = object->picture->file_length;
  return object->picture->file_content;
}

/*! \brief Set a picture object's contents from a buffer
 *
 * \par Function Description
 * Sets the contents of the picture \a object by reading image data
 * from a buffer.  The buffer should be in on-disk format.
 *
 * \param object   The picture #Object to modify.
 * \param filename The new filename for the picture.
 * \param data     The new image data buffer.
 * \param len      The size of the data buffer.
 * \param error    Location to return error information.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool
o_picture_set_from_buffer (Object *object,
                           const char *filename,
                           const char *data, size_t len,
                           GError **error)
{
  GdkPixbuf *pixbuf;
  GInputStream *stream;
  char *tmp;

  g_return_val_if_fail (GEDA_IS_PICTURE(object), FALSE);
  g_return_val_if_fail (data != NULL, FALSE);

  /* Check that we can actually load the data before making any
   * changes to the object. */
  stream = G_INPUT_STREAM (g_memory_input_stream_new_from_data (data, len, NULL));
  pixbuf = gdk_pixbuf_new_from_stream (stream, NULL, error);
  GEDA_UNREF (stream);
  if (pixbuf == NULL) return FALSE;

  if (object->picture->pixbuf != NULL) {
    GEDA_UNREF (object->picture->pixbuf);
  }
  object->picture->pixbuf = pixbuf;

  object->picture->ratio = (gdk_pixbuf_get_width(pixbuf) /
                            gdk_pixbuf_get_height(pixbuf));

  tmp = g_strdup (filename);
  GEDA_FREE (object->picture->filename);
  object->picture->filename = tmp;

  char *buf = g_realloc (object->picture->file_content,
                          len);
  /* It's possible that these buffers might overlap, because the
   * library user hates us. */
  memmove (buf, data, len);
  object->picture->file_content = buf;
  object->picture->file_length = len;

  return TRUE;
}

/*! \brief Set a picture object's contents from a file
 *
 * \par Function Description
 * Sets the contents of the picture \a object by reading image data
 * from a file.
 *
 * \param object   The picture #Object to modify.
 * \param filename The filename to load image data from.
 * \param error    Location to return error information.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool
o_picture_set_from_file (Object *object, const char *filename, GError **error)
{
  char *buf;
  size_t len;
  bool status;

  g_return_val_if_fail (filename != NULL, FALSE);

  if (!g_file_get_contents (filename, &buf, &len, error)) {
    return FALSE;
  }

  status = o_picture_set_from_buffer (object, filename, buf, len, error);
  GEDA_FREE (buf);
  return status;
}

/*! \brief Get the file name Associated with a Picture Object
 *
 * \par Function Description
 * Returns the filename associated with the picture \a object.
 *
 * \param object   The picture #Object to inspect
 *
 * \return the filename associated with \a object
 */
const char *
o_picture_get_filename (Object *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), NULL);

  return object->picture->filename;
}

/*! \brief Get fallback pixbuf for displaying pictures
 *
 * \par Function Description
 * Returns a pixbuf containing the fallback image to be used if a
 * picture object fails to load.  The returned pixbuf should be freed
 * with GEDA_UNREF() when no longer needed.
 *
 * \return a GdkPixbuf containing a warning image.
 */
GdkPixbuf *
o_picture_get_fallback_pixbuf (GedaToplevel *toplevel)
{
  static GdkPixbuf *pixbuf = NULL;
  static bool failed = FALSE;

  if (pixbuf == NULL && !failed) {
    char *filename;
    GError *error = NULL;

    filename = g_build_filename (toplevel->bitmap_directory,
                                 "gschem-warning.png", NULL);
    pixbuf = gdk_pixbuf_new_from_file (filename, &error);

    if (pixbuf == NULL) {
      g_warning ( _("Failed to load fallback image %s: %s.\n"),
                  filename, error->message);
      g_error_free (error);
      failed = TRUE;
    }
    GEDA_FREE (filename);
  }

  if (failed) return NULL;

  return g_object_ref (pixbuf);
}
