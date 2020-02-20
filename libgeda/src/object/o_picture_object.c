/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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

/*! \file o_picture_object.c
 *  \brief Functions for the Picture GedaObjects
 */

#include "../../../config.h"

#include <errno.h>
#include <math.h>
#include <stdio.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <libgeda_priv.h>
#include <warning_xpm.h>

/** \defgroup geda-picture-object-proc GedaPicture Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaPicture Objects
 */

/*!
 * \brief Create a copy of a picture
 * \par Function Description
 *  This function creates a verbatim copy of the object pointed by
 *  <B>\a o_current</B> describing a picture.
 *
 * \param [in]  o_current     Picture GedaObject to copy.
 * \return The new GedaObject
 */
GedaObject *geda_picture_object_copy(GedaObject *o_current)
{
  if (GEDA_IS_PICTURE(o_current)) {

    GedaObject  *new_obj;
    GedaPicture *new_picture;
    GedaPicture *old_picture;

    old_picture = GEDA_PICTURE(o_current);
    new_obj     = geda_picture_new();      /* create new picture object */
    new_picture = GEDA_PICTURE(new_obj);

    /* describe the picture with its upper left and lower right corner */
    new_picture->upper_x = old_picture->upper_x;
    new_picture->upper_y = old_picture->upper_y;
    new_picture->lower_x = old_picture->lower_x;
    new_picture->lower_y = old_picture->lower_y;

    if (old_picture->file_content != NULL) {
      new_picture->file_content = g_memdup (old_picture->file_content,
                                            old_picture->file_length);
    }
    else {
      new_picture->file_content = NULL;
    }

    new_picture->file_length = old_picture->file_length;
    new_picture->filename    = geda_utility_string_strdup (old_picture->filename);
    new_picture->ratio       = old_picture->ratio;
    new_picture->angle       = old_picture->angle;
    new_picture->mirrored    = old_picture->mirrored;
    new_picture->is_embedded = old_picture->is_embedded;

    /* Get the picture data */
    new_picture->pixbuf = geda_picture_object_get_pixbuf (o_current);

    return new_obj;
  }

  return NULL;
}

/*!
 * \brief Embed the image file associated with a picture
 * \par Function Description
 *  Verify that a picture has valid data associated with it, and if so,
 *  mark it to be embedded.
 *
 * \param [in] object  The picture Object to embed
 *
 * \return True on success, or False on failure
 */
bool geda_picture_object_embed (GedaObject *object)
{
  bool result;

  if (!geda_picture_object_is_embedded (object)) {

    const char *filename = object->picture->filename;

    if (object->picture->file_content == NULL) {

      geda_log_w (_("Picture [%s] has no image data.\n"), filename);
      geda_log_w (_("Falling back to file loading. Picture is still unembedded.\n"));
      object->picture->is_embedded = 0;
      result = FALSE;

    }
    else {

      const char *basename = geda_file_get_basename (filename);
      geda_log (_("Picture [%s] has been embedded\n"), basename);

      object->picture->is_embedded = 1;
      result = TRUE;
    }
  }
  else {
    result = FALSE;
  }

  return result;
}

static void geda_picture_object_add_if_writable (GdkPixbufFormat *data,
                                                 GSList          **list)
{
  if (gdk_pixbuf_format_is_writable (data)) {
    *list = g_slist_prepend (*list, data);
  }
}

/*!
 * \brief Real Export GdkPixbuf Buffer to a given file
 * \par Function Description
 *  This function creates an image file of the given type using \a pixbuf.
 *  Supported format \a type is system dependent and is determined at run
 *  time. The filename argument is not checked, the caller is responsible
 *  for ensuring the user has write-access to any path and file. Existing
 *  files are over-written.
 *
 * \param [in]  pixbuf    GdkPixbuf to export
 * \param [in]  filename  The name of the file to export
 * \param [in]  type      The type of image to generate, see note 3
 * \param [in]  varargs   Optional parameters as described below
 *
 * \return True on success, otherwise FALSE
 *
 * \note 1.) The image produced is based on the object setting for
 *           width, height, angle AND mirror, basically what is
 *           shown on the screen.
 *
 *        2.) The file extension is intentionally ignored.
 *
 *        3.) Image type are specified by using the string characters normally
 *            associated with the file extension, see gdk_pixbuf_get_formats.
 *            Additionally "jpg" and "tif" types are automatically associated
 *            with "jpeg" and "tiff", respectively.
 *
 *  Optional parameters can be passed to the back-ends, see the documentation
 *  for gdk_pixbuf_save. Options usually occur as key/value pairs of pointers
 *  but some are single pointer, these can be combined to ensure solo pointers
 *  are last on the stack or two singles can be paired. The point is, stack
 *  searching stops when a NULL is encountered and a NULL must be present at
 *  the end of the data.
 * \par
 *  Below is a summary of some of the advertised options:
 * \par
 *  <DL>
 *    <DT>"type       key          value          min     max</DT>
 *    <DT>"jpeg"   "quality",      "int"           0      100</DT>
 *    <DT>"png"    "compression",  "int"           0        9</DT>
 *    <DT>"png"    "tEXt::Str",     NULL          N/A     N/A</DT>
 *    <DT>"tiff"   "compression",  "int"           1        8</DT>
 *    <DT>"ico"    "depth",        "int"          16   24  32</DT>
 *    <DT>"ico"    "x_hot",        "int"          unk     unk</DT>
 *    <DT>"ico"    "y_hot",        "int"          unk     unk</DT>
 *  </DL>
 * \par
 *  jpeg, png, and tiff can also have embedded ICC color profiles,
 *  the value should be a pointer to the complete contents of the
 *  ICC base64 encoded file.
 *  <DL>
 *    <DT>"jpeg"   "icc-profile",  (char*data64)  N/A     N/A</DT>
 *    <DT>"png"    "icc-profile",  (char*data64)  N/A     N/A</DT>
 *    <DT>"tiff"   "icc-profile",  (char*data64)  N/A     N/A</DT>
 *  </DL>
 *
 *  Examples: See geda_picture_object_export and geda_picture_object_export_pixbuf
 *
 * \remarks
 *  During testing, the follow observation were made:
 * \par
 *  1.) While there was a difference in file size when using "png"
 *      "compression", the difference between 0 and 9 was not
 *      significant, suggesting gdk_pixbuf_save compression may not
 *      work properly for png types. The reduction was minimal, but
 *      present and all levels were readable by viewers.
 *
 *  2.) When testing the UTF-8 "tEXt::Str", using "tEXt::The quick
 *      brown fox", the argument was accepted by gdk_pixbuf_save
 *      but the text could not be found in the data (using Okteta).
 * \par
 *  3.) "tiff" "compression" codecs include 1 None, 2 Huffman, 5 LZW,
 *      7 JPEG and 8 Deflate. Using other values did not generate an
 *      error. Huffman (2) requires 4 channel data. The image file
 *      was actually larger using the LZW (5) option than with None,
 *      suggesting gdk_pixbuf_save tiff compression may not work
 *      properly. Only JPEG (7) had any significant reduction in size,
 *      (for all supported types) and this was dramatic, suggesting
 *      this is the ONLY compression back-end that does work properly,
 *      although jpeg file sizes were smaller when "quality" was
 *      reduced.
 * \par
 *  4.) If the "x_hot", and "y_hot" parameters are used with type
 *      "ico", the generated file is actually a "cur" type.
 */
static bool geda_picture_object_real_export_pixbuf (GdkPixbuf  *pixbuf,
                                                    const char *filename,
                                                    const char *type,
                                                    va_list     varargs)
{
  GSList     *formats;
  GSList     *iter;
  GSList     *writable;

  const char *argv;
  const char *real_type;

  char **keys;
  char **Vals;

  int  ecode;

  bool result;

  ecode = 0;

  if (!filename && !type) {
    result = FALSE;
  }
  else {

    if (!pixbuf) {
      result = FALSE;
    }
    else {

      int  argc;
      int  is_writable;
      int  i;

      keys = NULL;
      Vals = NULL;

      /* Find out how many options were passed */
      argc = 0;
      va_list varcnt;
      va_copy(varcnt, varargs);
      argv = va_arg (varcnt, char*);
      while (argv) {
        argc++;
        argv = va_arg (varcnt, const char *);
        /* could check if non null argv is something of interest */
      }
      va_end(varcnt);

      argc = argc / 2;  /* variable arguments must be pairs */

      if (argc > 0) {   /* allocate storage collect pairs */

        /* allocate memory for both arrays */
        keys = (char**)malloc(sizeof(char*) * argc + 1);
        Vals = (char**)malloc(sizeof(char*) * argc + 1);

        /* Save pointers on the stack to arrays */
        for ( i = 0; i < argc; i++) {
          keys[i] = va_arg (varargs, char *);
          Vals[i] = va_arg (varargs, char *);
        }

        /* Add sentinels to the two arrays */
        keys[i] = NULL;
        Vals[i] = NULL;
      }

      /* "jpg" will fail gdk_pixbuf_format_is_writable so we
       * substitue jpeg, even though we write to .jpg, not .jpeg */
      if (strcmp(type,"jpg") == 0) {
        real_type = "jpeg";
      } /*gdk_pixbuf_format_is_writable does like "tif" either */
      else if (strcmp(type,"tif") == 0) {
        real_type = "tiff";
      }
      else {
        real_type = type;
      }

      /* Get list of all support formats */
      formats = gdk_pixbuf_get_formats ();

      /* Reduce list to only writable formats */
      writable = NULL;
      g_slist_foreach(formats, (GFunc)geda_picture_object_add_if_writable, &writable);

      is_writable = 0;

      for (iter = writable; iter; iter = iter->next) {

        char **pattern;

        GdkPixbufFormat *format = (GdkPixbufFormat*) iter->data;

        pattern = gdk_pixbuf_format_get_extensions (format);

        for (i = 0; pattern[i] != NULL; i++) {

          if (strcmp(real_type, pattern[i])==0) {
            is_writable = 1;
            break;
          }
        }
        g_strfreev (pattern);

        if (is_writable) {
          /* No need to continue, we got the answer */
          break;
        }
      }

      if (!is_writable) {
        const char *msg = _("Can not export to type");
        geda_log ("%s %s\n", msg, real_type);
        result = FALSE;
      }
      else {

        GError *err = NULL;

        if (argc > 0) {
          gdk_pixbuf_savev (pixbuf, filename, real_type, keys, Vals, &err);
        }
        else {
          gdk_pixbuf_save (pixbuf, filename, real_type, &err, NULL);
        }

        if (err != NULL) {
          const char *msg = _("Failed to export");
          geda_log_w ("%s [%s]: %s\n", msg, filename, err->message);
          ecode = errno;
          g_error_free(err);
          result = FALSE;
        }
        else {
          result = TRUE;
        }
      }

      g_slist_free (formats);
      if (argc > 0) {
        free(keys);
        free(Vals);
      }
    }
  }

  errno = ecode;

  return result;
}

/*!
 * \brief Export Picture GedaObject to a given File and Type
 * \par Function Description
 *  This function creates an image file of the given type from the
 *  pixel buffer associated with \a o_current. Supported formats \a type
 *  is system dependent and is determined at run time. The filename
 *  argument is not checked, the caller is responsible for ensuring
 *  the user has write-access to the file and any path is valid.
 *
 * \param [in]  o_current Picture GedaObject to export
 * \param [in]  filename  The name of the file to export
 * \param [in]  type      type of image to generate
 *
 * \return True on success, otherwise FALSE
 *
 * \note The image produced is based on the object's width, height, angle
 *       AND mirror properties, basically what is shown on the screen.
 *
 * examples:
 *
 *  1.) geda_picture_object_export (object, name, "jpg", NULL); // simplest
 *  2.) geda_picture_object_export (object, name, "tif", "compression", "7", NULL);
 *
 */
bool geda_picture_object_export_object(GedaObject *o_current,
                                       const char *filename,
                                       const char *type, ...)
{
  GdkPixbuf *pixbuf;
  bool       result;


  /* This added a reference to pixbuf */
  pixbuf = geda_picture_object_get_pixbuf_fit (o_current, GDK_INTERP_BILINEAR);

  if (pixbuf) {
    va_list varargs;
    va_start (varargs, type);
    result = geda_picture_object_real_export_pixbuf (pixbuf, filename, type, varargs);
    va_end (varargs);
    GEDA_UNREF(pixbuf);
    GEDA_UNREF(pixbuf);
  }
  else {
    result = FALSE;
  }

  return result;
}

/*!
 * \brief Export Original Picture GedaObject Image to a given File and Type
 * \par Function Description
 *  This function creates an image file of the given type using \a pixbuf.
 *  Supported format \a type is system dependent and is determined at run
 *  time. The filename argument is not checked, the caller is responsible
 *  for ensuring the user has write-access to any path and file. Existing
 *  files are over-written.
 *
 * \param [in]  o_current Picture GedaObject to export
 * \param [in]  filename  The name of the file to export
 * \param [in]  type      type of image to generate
 *
 * \return True on success, otherwise FALSE
 *
 * \note The image produced is based on the original image's width,
 *       height properties, basically what was originally read in
 *       with no intentional adjustments.
 *
 * examples:
 *
 *  1.) geda_picture_object_export (object, name, "png", "tEXt::The quick brown fox", NULL);
 *  2.) geda_picture_object_export (object, name, type, "compression", "7", NULL); //type = "png" || "tif" || "tiff"
 *  3.) geda_picture_object_export (object, name, type, "x_hot", "50", "y_hot", "60", NULL);
 *
 * \sa geda_picture_object_export_object geda_picture_object_export_full
 */
bool geda_picture_object_export_orginal (GedaObject *o_current,
                                         const char *filename,
                                         const char *type, ...)
{
  GdkPixbuf *pixbuf;
  bool       result;

  pixbuf = geda_picture_object_get_pixbuf(o_current); /* This added a reference to pixbuf */

  if (pixbuf) {
    va_list varargs;
    va_start (varargs, type);
    result = geda_picture_object_real_export_pixbuf (pixbuf, filename, type, varargs);
    va_end (varargs);
    GEDA_UNREF(pixbuf);
  }
  else {
    result = FALSE;
  }

  return result;
}

/*!
 * \brief Export Picture GedaObject Image to a given File and Type
 * \par Function Description
 *  This function creates an image file of the given type using \a pixbuf.
 *  Supported format \a type is system dependent and is determined at run
 *  time. The filename argument is not checked, the caller is responsible
 *  for ensuring the user has write-access to any path and file. Existing
 *  files are over-written.
 *
 * \param [in]  pixbuf    The GdkPixbuf object to export
 * \param [in]  filename  The name of the file to export
 * \param [in]  type      type of image to generate
 *
 * \return True on success, otherwise FALSE
 *
 * \note The image produced is based on the original image's width,
 *       height properties, basically what was originally read in
 *       with no intentional adjustments.
 *
 * examples:
 *
 *  1.) geda_picture_object_export (pixbuf, name, "png", "tEXt::The quick brown fox", NULL);
 *  2.) geda_picture_object_export (pixbuf, name, type, "compression", "7", NULL); //type = "png" || "tif" || "tiff"
 *  3.) geda_picture_object_export (pixbuf, name, type, "x_hot", "50", "y_hot", "60", NULL);
 *
 * \sa geda_picture_object_export_object geda_picture_object_export_full
 */
bool geda_picture_object_export_pixbuf (GdkPixbuf  *pixbuf,
                                        const char *filename,
                                        const char *type, ...)
{
  bool result;

  if (pixbuf) {

    va_list varargs;

    GEDA_REF(pixbuf); /* Make sure the buffer exist until were done */
    va_start (varargs, type);
    result = geda_picture_object_real_export_pixbuf (pixbuf, filename, type, varargs);
    va_end (varargs);
    GEDA_UNREF(pixbuf);
  }
  else {
    result = FALSE;
  }

  return result;
}

/*!
 * \brief Get the raw image data from a picture object
 * \par Function Description
 *  Returns the raw image file data underlying the picture \a object,
 *  or NULL if the picture could not be loaded.
 *
 * \param object    The picture #GedaObject to inspect.
 * \param len       Location to store buffer length.
 * \return A read-only buffer of raw image data.
 */
const char *geda_picture_object_get_data (GedaObject *object, size_t *len)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  *len = object->picture->file_length;

  return object->picture->file_content;
}

/*!
 * \brief Get Effective Width/Height Ratio of an Image
 * \par Function Description

 * Returns the width/height ratio of picture \a object, taking the
 * image rotation into account.
 *
 * \param object    Picture #GedaObject to inspect
 *
 * \return width/height ratio for \a object.
 */
double geda_picture_object_get_effective_ratio (GedaObject *object)
{
  double anwser;

  if (GEDA_IS_PICTURE(object)) {

    int width  = geda_picture_object_get_width (object);
    int height = geda_picture_object_get_height(object);

        anwser = (double) width / height;

  }
  else {
    BUG_MSG ("Invald type of GedaObject, expecting Picture\n");
    anwser = 1.0;
  }

  return anwser;
}

/*!
 * \brief Get fallback pixbuf for displaying pictures
 * \par Function Description
 *  Returns a pixbuf containing the fallback image to be used if a
 *  picture object fails to load. The returned pixbuf should be freed
 *  with GEDA_UNREF() when no longer needed.
 *
 * \return a GdkPixbuf containing a warning image.
 */
GdkPixbuf *geda_picture_object_get_fallback_pixbuf (void)
{
  static GdkPixbuf *pixbuf = NULL;

  if (pixbuf == NULL) {

    pixbuf = gdk_pixbuf_new_from_xpm_data (warning_xpm);

  }

  return !pixbuf ? NULL : g_object_ref (pixbuf);
}

/*!
 * \brief Get the file name Associated with a Picture GedaObject
 * \par Function Description
 *  Returns the filename associated with the picture \a object.
 *
 * \param object   The picture #GedaObject to inspect
 *
 * \return the filename associated with \a object
 */
const char *geda_picture_object_get_filename (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), NULL);

  return object->picture->filename;
}

/*!
 * \brief Get Width of a Picture
 * \par Function Description
 *  This function returns current height if picture \a object
 *  in world units. No checking is performed.
 *
 * \param [in] object   The object to get the position.
 */
int geda_picture_object_get_height (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), -0);

  return object->picture->upper_y - object->picture->lower_y;
}

/*!
 * \brief Get Width of a Picture
 * \par Function Description
 *  This function returns current height if picture \a object
 *  in world units. No checking is performed.
 *
 * \param [in] object   The object to get the position.
 */
int geda_picture_object_get_lower_x (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), -0);

  return object->picture->lower_x;
}

/*!
 * \brief Get mask data from image
 * \par Function Description
 *  This function returns the mask data of the given image. Function
 *  taken from the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 * \remarks helper for geda_picture_object_get_mask_data and
 *          geda_picture_object_print
 *
 * \param [in] image  GdkPixbuf image to get mask data from.
 * \return Array of mask data from image.
 *
 * \note Caller must GEDA_FREE returned uint8 array.
 */
static uint8 *geda_picture_object_mask_data(GdkPixbuf *image)
{
  uint8 *pixels;
  uint8 *mask;
  int i, size;

  if (!gdk_pixbuf_get_has_alpha(image)) {
    return NULL;
  }

  pixels = gdk_pixbuf_get_pixels(image);

  size = gdk_pixbuf_get_width(image) * gdk_pixbuf_get_height(image);

  mask = GEDA_MEM_ALLOC(size);

  /* Pick every fourth byte (the alpha channel) into mask */
  for (i = 0; i < size; i++)
    mask[i] = pixels[i*4+3];

  return mask;
}

/*!
 * \brief Get mask data from a Picture object
 * \par Function Description
 *  This function returns the mask data of the given object.
 *
 * \param [in] object Picture object to get mask data from
 *
 * \return Array of mask data from image
 *
 * \note Caller must GEDA_FREE returned uint8 array.
 *
 * \sa geda_picture_object_mask_data geda_picture_object_get_rgb_data
 */
uint8 *geda_picture_object_get_mask_data(const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE (object), NULL);

  if (object->picture->pixbuf == NULL) {
    return geda_picture_object_mask_data(object->picture->pixbuf);
  }
  else {
    return NULL;
  }
}

/*!
 * \brief Get Point on a Picture Nearest a Given Point
 * \par Function Description
 *  This function is intended to locate a point on a Picture object given
 *  a point \a x, \a y, that is on or about the vicinity of \a object. If
 *  True is returned, <B>nx</B> and <B>ny</B> are set in world unit to a point
 *  on the picture that is the closest point on the picture to the point
 *  given by \a x, \a y.
 *
 * \param [in]  object  Pointer to a Picture object
 * \param [in]  x       Integer x of point near or on the picture
 * \param [in]  y       Integer y of point near or on the picture
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE is the results are valid or
 *          FALSE if \a object was not a Picture.
 */
bool geda_picture_object_get_nearest_point(const GedaObject *object, int x, int y, int *nx, int *ny)
{
  GedaPicture *picture;
  bool result;

  if (GEDA_IS_PICTURE(object)) {

    GedaObject *tmp = geda_box_new();

    picture = object->picture;

    tmp->box->upper_x = picture->upper_x;
    tmp->box->upper_y = picture->upper_y;
    tmp->box->lower_x = picture->lower_x;
    tmp->box->lower_y = picture->lower_y;

    result = geda_box_object_get_nearest_point (tmp, x, y, nx, ny);

    g_object_unref(tmp);
  }
  else { /* was not an GedaPicture object */
   *nx = x;
   *ny = y;
    result = FALSE;
  }

  return result;
}

/*!
 * \brief Get a pixel buffer for a picture object
 * \par Function Description
 *  Returns a GdkPixbuf for the picture object \a object, or NULL if
 *  the picture could not be loaded. The returned value should have
 *  its reference count decremented with GEDA_UNREF() when no longer
 *  needed.
 *
 * \param object  The picture #GedaObject to inspect.
 *
 * \return A GdkPixbuf for the picture.
 */
GdkPixbuf *geda_picture_object_get_pixbuf (GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE (object), NULL);

  if (object->picture->pixbuf != NULL) {
    return g_object_ref (object->picture->pixbuf);
  }
  else {
    return NULL;
  }
}

/*!
 * \brief Get a fitted pixel buffer for a picture object
 * \par Function Description
 *  Returns a GdkPixbuf for the picture object \a object, or NULL if
 *  the picture could not be loaded. The returned pixel buffer object
 *  size is set based on the object properties and page scaling. The
 *  GdkInterpType enumeration flag describes different interpolation
 *  modes used with the scaling functions. GDK_INTERP_NEAREST is the
 *  fastest scaling method, but results in poor quality when scaling
 *  down. If unsure, pick GDK_INTERP_BILINEAR, it has a good balance
 *  of speed and quality.
 *
 * \param [in] object  The picture #GedaObject to inspect.
 * \param [in] interp  GdkInterpType flag passed to gdk_pixbuf_scale_simple
 *
 *  <B>interp</B> can be one of the following values:
 * \par
 *  <DL>
 *    <DT>GDK_INTERP_NEAREST</DT>
 *    <DT>GDK_INTERP_TILES</DT>
 *    <DT>GDK_INTERP_BILINEAR</DT>
 *    <DT>GDK_INTERP_HYPER</DT>
 *  </DL>
 *
 * \return A GdkPixbuf for the picture.
 *
 * \note Use GEDA_UNREF() to decrement the buffer's reference count so
 *       that the associated memory is released when no longer needed.
 *
 * \sa geda_picture_object_export_object
 */
GdkPixbuf *geda_picture_object_get_pixbuf_fit (GedaObject *object, int interp)
{
  g_return_val_if_fail (GEDA_IS_PICTURE (object), NULL);

  Page        *page  = geda_object_get_page (object);
  GedaPicture *o_pic = object->picture;

  if (page && o_pic->pixbuf != NULL) {

    GdkPixbuf *pixbuf1;

    /* upper is considered the origin, world units */
    int width  = geda_picture_object_get_width (object);  /* o_pic->lower_x - o_pic->upper_x */
    int height = geda_picture_object_get_height (object); /* o_pic->upper_y - o_pic->lower_y */
    int angle  = o_pic->angle;
    int mirror = o_pic->mirrored;

    width  = width  * page->to_screen_x_constant;
    height = height * page->to_screen_y_constant;

    /* The object->picture->pixel is a pointer to the as read-in pixel
     * buffer and needs to be rescaled to the instance insertion size */
    if ((angle == 90) || (angle == 270)) {
      pixbuf1 = gdk_pixbuf_scale_simple (o_pic->pixbuf, height, width, interp);
    }
    else {
      pixbuf1 = gdk_pixbuf_scale_simple (o_pic->pixbuf, width, height, interp);
    }

    if (pixbuf1) {

      /* Adjust for rotation and mirroring */

      GdkPixbuf *pixbuf2;
      GdkPixbuf *pixbuf3;

      if (!angle && !mirror) {                            /* No adjustment required */
        pixbuf3 = g_object_ref (pixbuf1);
      }
      else if (angle && !mirror) {                        /* Rotation required  */
        pixbuf3 = gdk_pixbuf_rotate_simple (pixbuf1, angle);
      }
      else if (!angle && mirror) {                        /* Mirroring required */
        pixbuf3 = gdk_pixbuf_flip (pixbuf1, TRUE);
      }
      else /* (mirror && angle) note: do flip 1st */ {    /* Mirror and Rotate */
        pixbuf2 = gdk_pixbuf_flip (pixbuf1, TRUE);
        pixbuf3 = gdk_pixbuf_rotate_simple (pixbuf2, angle);
        g_object_unref (pixbuf2);
      }
      g_object_unref (pixbuf1);
      return g_object_ref(pixbuf3);
    }
  }

  return NULL;
}

/*!
 * \brief Get Picture position of the left bottom point
 * \par Function Description
 *  This function gets the position of the bottom left point of a picture object.
 *
 * \param [in]  object  Pointer to a #GedaPicture object
 * \param [out] x       pointer to save the x-position
 * \param [out] y       pointer to save the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_picture_object_get_position (GedaObject *object, int *x, int *y)
{
  *x = min(object->picture->lower_x, object->picture->upper_x);
  *y = min(object->picture->lower_y, object->picture->upper_y);

  return TRUE;
}

/*!
 * \brief Get Width/Height Ratio of an Image
 * \par Function Description
 * Returns the width/height ratio of picture \a object.
 *
 * \param object    Picture #GedaObject to inspect
 *
 * \return width/height ratio for \a object.
 */
double geda_picture_object_get_ratio (GedaObject *object)
{
  double anwser;

  if (GEDA_IS_PICTURE(object)) {

    int angle  = object->picture->angle;
    int height = geda_picture_object_get_height(object);
    int width  = geda_picture_object_get_width (object);

    /* The effective ratio varies depending on the rotation of the
     * image. */
    switch (angle) {
      default:
        BUG_IMSG ("Invalid picture angle %i, set to 0 degrees\n", angle);
        object->picture->angle = 0;
        /* fall-through*/
      case 0:
      case 180:
        anwser = (double) width / height;
        break;
      case 90:
      case 270:
        anwser = (double) height / width;
        break;
    }
  }
  else {
    BUG_MSG ("Invald type of GedaObject, expecting Picture\n");
    anwser = 1.0;
  }

  return anwser;
}

/*!
 * \brief Get RGB data from image
 * \par Function Description
 *  This function returns the RGB data of the given image. Function
 *  taken from the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 * \param [in] image  GdkPixbuf image to read RGB data from.
 *
 * \return Array of rgb data from image.
 *
 * \note Caller must GEDA_FREE returned uint8 array.
 */
static unsigned char *geda_picture_object_rgb_data (GdkPixbuf *image)
{
  int width         = gdk_pixbuf_get_width(image);
  int height        = gdk_pixbuf_get_height(image);
  int rowstride     = gdk_pixbuf_get_rowstride(image);
  int size          = height*rowstride;
  uint8 *rgb_pixels = GEDA_MEM_ALLOC(size);

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
    memmove(rgb_pixels, pixels, height*rowstride);
    return rgb_pixels;
  }
}

/*!
 * \brief Get RGB data from a Picture object
 * \par Function Description
 *  This function returns the RGB data of the given object..
 *
 * \param [in] object  Picture object to get RGB data from.
 *
 * \return Array of rgb data from image.
 *
 * \note Caller must GEDA_FREE returned data.
 *
 *  \sa geda_picture_object_rgb_data geda_picture_object_get_mask_data
 */
unsigned char *geda_picture_object_get_rgb_data (GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE (object), NULL);

  if (object->picture->pixbuf != NULL) {
    return geda_picture_object_rgb_data(object->picture->pixbuf);
  }
  else {
    return NULL;
  }
}

/*!
 * \brief Get Height of a Picture
 * \par Function Description
 *  This function returns current height if picture \a object
 *  in world units. No checking is performed.
 *
 * \param [in] object   The object to get the position.
 */
int  geda_picture_object_get_width(GedaObject *object)
{
  return object->picture->lower_x - object->picture->upper_x;
}

/*!
 * \brief Test whether a picture object is embedded
 * \par Function Description
 *  Returns TRUE if the picture \a object will have its data embedded
 *  in a schematic or symbol file; returns FALSE if its data will be
 *  obtained from a separate file.
 *
 * \param object    The picture #GedaObject to inspect.
 * \return TRUE if \a object is embedded.
 */
bool geda_picture_object_is_embedded (GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), FALSE);
  return (object->picture->is_embedded == 1);
}

/*!
 * \brief Mirror a picture
 * \par Function Description
 *  This function mirrors the picture from the point (<B>center_x</B>,
 *  <B>center_y</B>). The picture is first translated to the origin,
 *  then mirrored and finally translated back at its previous position.
 *
 * \param [in,out] object    Picture GedaObject to mirror.
 * \param [in]     center_x  Origin x coordinate.
 * \param [in]     center_y  Origin y coordinate.
 */
void geda_picture_object_mirror(GedaObject *object, int center_x, int center_y)
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

    default:
      break;
  }

  /* translate object to origin */
  object->picture->upper_x -= center_x;
  object->picture->upper_y -= center_y;
  object->picture->lower_x -= center_x;
  object->picture->lower_y -= center_y;

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
  object->picture->upper_x += center_x;
  object->picture->upper_y += center_y;
  object->picture->lower_x += center_x;
  object->picture->lower_y += center_y;

  /* invalidate bounding coordinates */
  object->bounds_valid = FALSE;
}

/*!
 * \brief Modify the description of a picture Object
 * \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the picture. The new coordinates of the corner identified by
 *  <B>whichone</B> are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate
 *  system. Screen coordinates and boundings are then updated.
 *
 * \param [in,out] object    Picture GedaObject to modify.
 * \param [in]     x         New x coordinate.
 * \param [in]     y         New y coordinate.
 * \param [in]     whichone  Which picture parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>PICTURE_UPPER_LEFT
 *    <DT>*</DT><DD>PICTURE_LOWER_LEFT
 *    <DT>*</DT><DD>PICTURE_UPPER_RIGHT
 *    <DT>*</DT><DD>PICTURE_LOWER_RIGHT
 *  </DL>
 */
void geda_picture_object_modify(GedaObject *object, int x, int y, int whichone)
{
  int tmp;
  double ratio = geda_picture_object_get_effective_ratio (object);

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

  /* invalidate the boundary */
  object->bounds_valid = FALSE;
}

/*!
 * \brief Modify a picture object's coordinates
 * \par Function Description
 *  Modifies the coordinates of all four corners of a picture \a
 *  object.  The picture is adjusted to fit the rectangle enclosed by
 *  the points (\a x1, \a y1) and (\a x2, \a y2), and scaled as large
 *  as possible to still fit within that rectangle.
 *
 * \param [in,out] object   picture #GedaObject to be modified.
 * \param [in]     x1       x coordinate of first corner of box.
 * \param [in]     y1       y coordinate of first corner of box.
 * \param [in]     x2       x coordinate of second corner of box.
 * \param [in]     y2       y coordinate of second corner of box.
 */
void geda_picture_object_modify_all (GedaObject *object, int x1, int y1, int x2, int y2)
{
  /* Normalize the requested rectangle. */
  object->picture->lower_x = (x1 > x2) ? x1 : x2;
  object->picture->lower_y = (y1 > y2) ? y2 : y1;
  object->picture->upper_x = (x1 > x2) ? x2 : x1;
  object->picture->upper_y = (y1 > y2) ? y1 : y2;

  /* invalidate the bounds */
  object->bounds_valid = FALSE;
}

/*!
 * \brief Create a picture object.
 * \par Function Description
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
 *  object will fallback to a warning image.
 *
 * \param [in] file_content  Raw data of the image file, or NULL.
 * \param [in] file_length   Length of raw data buffer
 * \param [in] filename      File name backing this picture, or NULL.
 * \param [in] x1            Upper x coordinate.
 * \param [in] y1            Upper y coordinate.
 * \param [in] x2            Lower x coordinate.
 * \param [in] y2            Lower y coordinate.
 * \param [in] angle         Picture rotation angle.
 * \param [in] mirrored      Whether the image should be mirrored or not.
 * \param [in] embedded      Whether the embedded flag should be set or not.
 *
 * \return A pointer to a new picture #GedaObject.
 */
GedaObject *geda_picture_object_new (const char   *file_content,
                                     unsigned int  file_length,
                                     const char   *filename,
                                     int x1, int y1, int x2, int y2,
                                     int angle, int mirrored, int embedded)
{
  GedaObject  *new_obj;
  GedaPicture *picture;

  /* create the object */
  new_obj = geda_picture_new();

  picture = GEDA_PICTURE(new_obj);

  /* describe the picture with its upper left and lower right corner */
  picture->upper_x      = (x1 > x2) ? x2 : x1;
  picture->upper_y      = (y1 > y2) ? y1 : y2;
  picture->lower_x      = (x1 > x2) ? x1 : x2;
  picture->lower_y      = (y1 > y2) ? y2 : y1;

  picture->pixbuf       = NULL;
  picture->file_content = NULL;
  picture->file_length  = 0;

  picture->angle        = angle;
  picture->mirrored     = mirrored;
  picture->is_embedded  = embedded;

  picture->height       = picture->upper_y - picture->lower_y;
  picture->width        = picture->lower_x - picture->upper_x;

  /* Can not divide by zero */
  if ((picture->lower_y - picture->upper_y) != 0) {
    picture->ratio = (double) (picture->lower_x - picture->upper_x) /
                              (picture->upper_y - picture->lower_y);
  }
  else {
    geda_log_w(_("Invalid; picture has no height\n"));
    geda_log_w (_("Setting aspect to 1.0\n"));
    picture->ratio = 1.0;
  }

  if (filename) {
    picture->filename = geda_utility_string_strdup (filename);
  }

  if (file_content != NULL) {

    GError *error = NULL;

    if (!geda_picture_object_set_from_buffer (new_obj, filename, file_content,
                                              file_length, &error))
    {
      const char *msg = _("Failed to load buffer image");
      geda_log_w ("%s [%s]: %s\n", msg, filename, error->message);
      g_error_free (error);

      /* Force the data into the object anyway, so as to prevent data
       * loss of embedded images. */
      picture->file_content = g_memdup (file_content, file_length);
      picture->file_length  = file_length;
    }
  }

  if (picture->pixbuf == NULL && filename != NULL) {

    GError *error = NULL;

    if (!geda_picture_object_set_from_file (new_obj, filename, &error)) {

      const char *msg = _("Failed to load image from");
      geda_log_w ("%s [%s]: %s\n", msg, filename, error->message);

      picture->pixbuf  = geda_picture_object_get_fallback_pixbuf();
      picture->missing = TRUE;
      g_error_free (error);
    }
  }
  else {
    picture->missing = FALSE;
  }

  return new_obj;
}

/*!
 * \brief Print picture to Postscript document
 * \par Function Description
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
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp         FILE pointer to Postscript document.
 * \param [in] o_current  Picture GedaObject to write to document.
 * \param [in] origin_x   Page x coordinate to place picture Object.
 * \param [in] origin_y   Page y coordinate to place picture Object.
 */
void geda_picture_object_print(GedaToplevel *toplevel, FILE *fp,
                               GedaObject   *o_current,
                               int origin_x, int origin_y)
{
  int x1, y1, x, y;
  int height, width;
  GdkPixbuf* image = geda_picture_object_get_pixbuf (o_current);
  int img_width, img_height, img_rowstride;
  uint8 *rgb_data;
  uint8 *mask_data;

  /* Calculate the width and height of the box */
  width  = abs(o_current->picture->lower_x - o_current->picture->upper_x);
  height = abs(o_current->picture->upper_y - o_current->picture->lower_y);

  /* Calculate the origin of the box */
  x1 = o_current->picture->upper_x;
  y1 = o_current->picture->upper_y;

  /* If the image failed to load, try to get hold of the fallback pixbuf. */
  if (image == NULL) {
    image = geda_picture_object_get_fallback_pixbuf ();
  }

  /* If the image failed to load, draw a box in the default color with a
   * cross in it. */
  if (image == NULL) {

    int line_width = geda_object_style_get_line_width(toplevel);

    geda_box_object_print_solid (toplevel, fp, x1, y1, width, height,
                                 DEFAULT_COLOR_INDEX,
                                 line_width, SQUARE_CAP, -1, -1, -1, -1);
    geda_line_object_print_solid (toplevel, fp, x1, y1, x1+width, y1-height,
                                  DEFAULT_COLOR_INDEX,
                                  line_width, ROUND_CAP, -1, -1, -1, -1);
    geda_line_object_print_solid (toplevel, fp, x1+width, y1, x1, y1-height,
                                  DEFAULT_COLOR_INDEX,
                                  line_width, ROUND_CAP, -1, -1, -1, -1);
    return;
  }

  img_width     = gdk_pixbuf_get_width(image);
  img_rowstride = gdk_pixbuf_get_rowstride(image);
  img_height    = gdk_pixbuf_get_height(image);

  rgb_data      = geda_picture_object_rgb_data(image);
  mask_data     = geda_picture_object_mask_data(image);

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
  }
  else {
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

/*!
 * \brief Create picture Object from character string
 * \par Function Description
 *  Parses \a first_line and subsequent lines from \a tb, and returns
 *  a newly-created picture #GedaObject.
 *
 * \param [in]  first_line      Character string with picture description.
 * \param [in]  tb              Text buffer to load embedded data from.
 * \param [in]  release_ver     libgeda release version number.
 * \param [in]  fileformat_ver  libgeda file format version number.
 *
 * \param [out] err           A GError obejct
 *
 * \return A pointer to the new picture object, or NULL on error.
 */
GedaObject *geda_picture_object_read (const char  *first_line,
                                      TextBuffer  *tb,
                                      unsigned int release_ver,
                                      unsigned int fileformat_ver,
                                      GError     **err)
{
  GedaObject *new_obj;

  char *file_content;
  char *filename;
  char *tmpstr;
  char  type;

  int   angle, height, width;
  int   embedded, mirrored;
  int   x1, y1;
  int   num_conv;

  unsigned int file_length;

  num_conv = sscanf(first_line, "%c %d %d %d %d %d %d %d\n", &type,
                    &x1, &y1, &width, &height, &angle, &mirrored, &embedded);

  if (num_conv != 8) {
    g_set_error(err,
                EDA_ERROR,
                EDA_ERROR_PARSE, _("Failed to parse picture definition"));
    return NULL;
  }

  if (width == 0 || height == 0) {

    const char *msg = _("Found a picture with zero width/height");

    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s: [%d x %d].\n", msg, width, height);
    }
  }

  if (mirrored != 0 && mirrored != 1) {

    const char *msg = _("Found a picture with an invalid 'mirrored' parameter");

    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s: %d.\n", msg, mirrored);
    }

    geda_log_w(_("Setting mirrored to 0\n"));
    mirrored = 0;
  }

  if ( (embedded > 1) || (embedded < 0)) {

    const char *msg = _("Found a picture with an invalid 'embedded' parameter");

    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s: %d.\n", msg, embedded);
    }

    geda_log_w(_("Setting embedded to 0\n"));
    embedded = 0;
  }

  if (angle != 0 && angle != 90 && angle != 180 && angle != 270) {
    const char *msg = _("Found an unsupported picture angle");
    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s: %d.\n", msg, angle);
    }
    geda_log_w (_("Setting angle to 0\n"));
    angle = 0;
  }

  tmpstr = geda_utility_string_strdup(geda_struct_textbuffer_next_line(tb));
  tmpstr = geda_utility_string_remove_last_nl(tmpstr);

  if (geda_file_get_is_path_absolute(tmpstr)) {

    /* Path is already absolute so no need to do anything */
    filename = tmpstr;
  }
  else {

    GError *err = NULL;

    /* Handle relative filenames, which will not work with UNDODISK.
     * File names in schematics would not normally be relative but
     * could happen if someone edited the file */
    filename = geda_file_sys_normalize_name (tmpstr, &err);

    if (err) {
      if (!embedded) {
        geda_log_w ("%s %s %s", _("Error"), tmpstr, err->message);
      }
      g_error_free (err);
      filename = tmpstr;
    }
    else {
      GEDA_FREE (tmpstr);
    }
  }

  /* Handle empty filenames */
  if (filename && strlen (filename) == 0) {
    geda_log_w (_("Image filename is missing."));
    GEDA_FREE (filename);
  }

  file_content = NULL;
  file_length  = 0;

  if (embedded == 1) {

    char     *encoded_picture = NULL;
    unsigned  size            = 0;
    bool      finished        = FALSE;

    /* Read the encoded picture */
    do {

      const char *line = geda_struct_textbuffer_next_line(tb);

      if (line == NULL) {
        break;
      }

      if (g_ascii_strcasecmp(line, ".\n") != 0) {

        size_t len = strlen(line);

        if (!encoded_picture) {
          encoded_picture = (char*)malloc(len + 1);
          strncpy(encoded_picture, line, len);
          size = ++len;
          encoded_picture[size - 1] = '\0';
        }
        else {

          char *buffer;

          size   = size + len;

          buffer = (char*)realloc(encoded_picture, size);

          if (!buffer)
            break;

          encoded_picture = buffer;
          strncat(encoded_picture, line, len);
        }
      }
      else {
        finished = TRUE;
      }
    } while (!finished);

    /* Decode the picture */
    if (encoded_picture != NULL) {
      file_content = geda_struct_encoding_base64_decode(encoded_picture,
                                                        size,
                                                        &file_length);
      free(encoded_picture);
    }

    if (file_content == NULL) {
      geda_log_w ("%s [%s]: %s\n", _("Failed to load image from embedded data"),
                  filename, _("Base64 decoding failed."));
      geda_log_w (_("Falling back to file loading. Picture unembedded.\n"));
      embedded = 0;
    }
  }

  /* create the picture */
  /* The picture is described by its upper left and lower right corner */
  new_obj = geda_picture_object_new (file_content, file_length, filename,
                                     x1, y1 + height, x1 + width, y1,
                                     angle, mirrored, embedded);

  GEDA_FREE (file_content);
  GEDA_FREE (filename);

  return new_obj;
}

/*!
 * \brief Rotate picture Object.
 * \par Function Description
 *  This function rotates the picture described by <B>*object</B> around
 *  the given (<B>x</B>, <B>y</B>) point by <B>angle</B> degrees.
 *
 * \param [in,out] object Picture GedaObject to rotate
 * \param [in]     x      Rotation center x coordinate
 * \param [in]     y      Rotation center y coordinate
 * \param [in]     angle  Rotation angle in degrees (See note below).
 */
void geda_picture_object_rotate(GedaObject *object, int x, int y, int angle)
{
  int newx1, newy1;
  int newx2, newy2;

  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if (angle < 0) angle = -angle;

  /* angle must be a 90 multiple or no rotation performed */
  if ((angle % 90) != 0) return;

  object->picture->angle = (object->picture->angle + angle) % 360;

  /* The center of rotation (<B>x</B>, <B>y</B>) is translated to the
   * origin. The rotation of the upper left and lower right corner are
   * then performed. Finally, the rotated picture is translated back to
   * its previous location.
   */
  /* translate object to origin */
  object->picture->upper_x -= x;
  object->picture->upper_y -= y;
  object->picture->lower_x -= x;
  object->picture->lower_y -= y;

  /* rotate the upper left corner of the picture */
  geda_math_rotate_point_90(object->picture->upper_x, object->picture->upper_y, angle,
                    &newx1, &newy1);

  /* rotate the lower left corner of the picture */
  geda_math_rotate_point_90(object->picture->lower_x, object->picture->lower_y, angle,
                    &newx2, &newy2);

  /* reorder the corners after rotation */
  object->picture->upper_x = min(newx1,newx2);
  object->picture->upper_y = max(newy1,newy2);
  object->picture->lower_x = max(newx1,newx2);
  object->picture->lower_y = min(newy1,newy2);

  /* translate object back to normal position */
  object->picture->upper_x += x;
  object->picture->upper_y += y;
  object->picture->lower_x += x;
  object->picture->lower_y += y;

  /* recalc boundings and screen coords */
  object->bounds_valid = FALSE;
}

/*!
 * \brief Create a character string representation of a picture Object
 * \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe
 *  the picture object <B>*object</B>.
 *
 * \note object was validated by geda_object_save_objects
 *
 * \param [in] object  Picture GedaObject to create string from.
 *
 * \return A pointer to the picture Object character string.
 *
 * \note returned character string should be freed with GEDA_FREE.
 */
char *geda_picture_object_save(GedaObject *object)
{
  char         *encoded_picture;
  char         *out;
  const char   *filename;
  unsigned int  encoded_picture_length;
  int           width, height, x1, y1;

  encoded_picture = NULL;
  filename        = NULL;
  out             = NULL;

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
  if (geda_picture_object_is_embedded(object)) {
    encoded_picture =
    geda_struct_encoding_base64_encode((char*)object->picture->file_content,
                                       object->picture->file_length,
                                       &encoded_picture_length,
                                       TRUE);
    if (encoded_picture == NULL) {
      geda_log_w (_("ERROR: unable to encode the picture.\n"));
    }
  }

  /* Cope with null filename */
  filename = geda_picture_object_get_filename (object);

  if (filename == NULL) filename = "";

  if (geda_picture_object_is_embedded(object) && encoded_picture != NULL) {
    out = geda_sprintf("%c %d %d %d %d %d %c %c\n%s\n%s\n%s", object->type,
                           x1, y1, width, height, object->picture->angle,
                           /* Convert the (0,1) chars to ASCII */
                          (object->picture->mirrored)+0x30, '1',
                           filename, encoded_picture, ".");
  }
  else {
    out = geda_sprintf("%c %d %d %d %d %d %c %c\n%s", object->type,
                           x1, y1, width, height, object->picture->angle,
                           /* Convert the (0,1) chars to ASCII */
                           (object->picture->mirrored)+0x30, '0',
                           filename);
  }

  GEDA_FREE(encoded_picture);

  return(out);
}


void geda_picture_object_scale (GedaObject *object, int x_scale, int y_scale)
{
  if (GEDA_IS_PICTURE(object)) {

    int lower_x, upper_x, lower_y, upper_y;
    int offset;

    if (x_scale) {

      int width;

      width = object->picture->upper_x - object->picture->lower_x;

      offset = ((width * x_scale) - width) >> 1;

      lower_x = object->picture->lower_x - offset;
      upper_x = object->picture->upper_x + offset;
    }
    else {
      lower_x = object->picture->lower_x;
      upper_x = object->picture->upper_x;
    }

    if (y_scale) {

      int height;

      height = (object->picture->upper_y - object->picture->lower_y);

      offset = ((height * y_scale) - height) >> 1;

      lower_y = object->picture->lower_y - offset;
      upper_y = object->picture->upper_y + offset;
    }
    else {
      lower_y = object->picture->lower_y;
      upper_y = object->picture->upper_y;
    }

    geda_picture_object_modify_all (object, upper_x, upper_y, lower_x, lower_y);

  }
  else {
    BUG_MSG ("Invald type of GedaObject, expecting Picture\n");
  }
}

/*!
 * \brief Set a picture object's contents from a buffer
 * \par Function Description
 *  Sets the contents of the picture \a object by reading image
 *  data from a buffer. The buffer should be in on-disk format.
 *
 * \note object->picture->ratio is set to the ratio of the buffer
 *       image and not the ratio of the as drawn image.
 *
 * \param object   The picture #GedaObject to modify.
 * \param filename The new filename for the picture.
 * \param data     The new image data buffer.
 * \param len      The size of the data buffer.
 * \param error    Location to return error information.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool geda_picture_object_set_from_buffer (GedaObject    *object,
                                          const char    *filename,
                                          const char    *data,
                                          unsigned int   len,
                                          GError       **error)
{
  GdkPixbuf *pixbuf;
  char      *tmp_name;

  int height;
  int width;

  g_return_val_if_fail (GEDA_IS_PICTURE(object), FALSE);
  g_return_val_if_fail (data != NULL, FALSE);

  /* Check that we can actually load the data before making any
   * changes to the object. */

#if HAVE_GDK_PIXBUF_LOADER_WRITE

  GdkPixbufLoader *loader;

  loader = gdk_pixbuf_loader_new ();

  if (gdk_pixbuf_loader_write (loader, (unsigned char*)data, len, error))
  {
    gdk_pixbuf_loader_close(loader, NULL);
    pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);

    if (pixbuf) {
      GEDA_REF (pixbuf);
    }
    else {
      g_set_error (error, GDK_PIXBUF_ERROR, GDK_PIXBUF_ERROR_FAILED,
                   "%s '%s'", _("Error processing image"), filename);
    }
  }
  else
  {
    pixbuf = NULL;
  }

  GEDA_UNREF (loader);

#else

  GInputStream *stream;

  stream = G_INPUT_STREAM (g_memory_input_stream_new_from_data (data, len, NULL));
  pixbuf = gdk_pixbuf_new_from_stream (stream, NULL, error);
  GEDA_UNREF (stream);

#endif

  if (pixbuf == NULL) {
    return FALSE;
  }

  /* If object already has pixbuf, then loose it */
  if (object->picture->pixbuf != NULL) {
    GEDA_UNREF (object->picture->pixbuf);
  }

  object->picture->pixbuf = pixbuf;

  width  = gdk_pixbuf_get_width(pixbuf);
  height = gdk_pixbuf_get_height(pixbuf);

  /* CAUTION! Do NOT put parenthesis around width / height
   * or GCC will generate code to perform integer division,
   * in effect, rounding the ratio to a whole number */
  object->picture->ratio = (double) width / height;

  tmp_name = geda_strdup (filename);
  GEDA_FREE (object->picture->filename);
  object->picture->filename = tmp_name;

  char *buf = GEDA_MEM_REALLOC (object->picture->file_content, len);

  /* It's possible that these buffers might overlap, because the
   * library user hates us. */
  memmove (buf, data, len);
  object->picture->file_content = buf;
  object->picture->file_length  = len;

  geda_struct_object_set_page_changed(object);

  return TRUE;
}

/*!
 * \brief Set a picture object's contents from a file
 * \par Function Description
 *  Sets the contents of the picture \a object by reading image data
 *  from a file.
 *
 * \param object   The picture #GedaObject to modify.
 * \param filename The filename to load image data from.
 * \param error    Location to return error information.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool geda_picture_object_set_from_file (GedaObject *object,
                                        const char *filename,
                                        GError    **error)
{
  char   *buf;
  size_t  len;
  bool    status;

  g_return_val_if_fail (filename != NULL, FALSE);

  if (!geda_file_get_contents(filename, &buf, &len, error)) {
    return FALSE;
  }

  status = geda_picture_object_set_from_buffer (object, filename, buf, len, error);

  GEDA_FREE (buf);

  return status;
}

/*!
 * \brief Closest distance between point and a picture
 * \par Function Description
 *  Interrior points within the picture return a distance of zero.
 *
 * \param [in] object       A picture Object.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point. With
 *         an invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_picture_object_shortest_distance (ConstObject *object,
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

#if HAVE_HYPOT
  return hypot (dx, dy);
#else
  return sqrt ((dx * dx) + (dy * dy));
#endif

}

/*!
 * \brief Translate a picture position by a delta
 * \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the picture
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world units.
 *
 * \param [in,out] object     Picture GedaObject to translate
 * \param [in]     dx         x distance to move
 * \param [in]     dy         y distance to move.
 */
void geda_picture_object_translate(GedaObject *object, int dx, int dy)
{
  /* Do world coords */
  object->picture->upper_x = object->picture->upper_x + dx;
  object->picture->upper_y = object->picture->upper_y + dy;
  object->picture->lower_x = object->picture->lower_x + dx;
  object->picture->lower_y = object->picture->lower_y + dy;

  /* recalc the screen coords and the bounding picture */
  object->bounds_valid = FALSE;
}

/*!
 * \brief Unembed a picture, reloading the image from disk
 * \par Function Description
 *  Verify that the file associated with \a object exists on disk and
 *  is usable, and if so, reload the picture and mark it as unembedded.
 *
 * \param [in] object  The picture Object to unembed
 *
 * \return True on success, or False on failure
 */
bool geda_picture_object_unembed (GedaObject *object)
{
  GError     *err      = NULL;
  const char *filename = geda_picture_object_get_filename(object);
  bool        result;

  if (GEDA_IS_PICTURE(object)) {

    if (geda_picture_object_is_embedded (object)) {

      geda_picture_object_set_from_file (object, filename, &err);

      if (err != NULL) {

        geda_log_w ("%s [%s]: %s\n", _("Failed to load image from file"),
                     filename, err->message);
        geda_log_v (_("Picture is still embedded.\n"));
        g_error_free (err);
        result = FALSE;

      }
      else {

        const char *basename = geda_file_get_basename(filename);

        geda_log (_("Picture [%s] has been unembedded\n"), basename);

        object->picture->is_embedded = 0;
        result = TRUE;

      }
    }
    else {
      result = FALSE;
    }
  }
  else {
    BUG_MSG("object is not a Picture");
    result = FALSE;
  }

  return result;
}

/** @} endgroup geda-picture-proc */
