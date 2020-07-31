/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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

/*! \file o_object.c
 *  \brief functions for the basic object type
 *
 *  This file contains the code used to manipulate <b>GedaObjects</b>.
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 *  The <b>GedaObject</b> be extended to become concrete objects like a line,
 *  a pin, text, a circle or a picture. These extentions are substructures
 *  in the object struct.
 *  The <b>Subobjects</b> are pictures (GedaPicture), paths (GedaPath),
 *  arcs (GedaArc), a lines (GedaLine), boxes (st_box), circles (GedaCircle),
 *  texts (GedaText) and a the complex type (GedaComplex).
 *
 *  Pins, nets and busses are types of line <b>GedaObjects</b>.
 *
 *  The <b>Complex GedaObjects</b> can be linked to many primary objects.
 *  If the <b>Complex GedaObject</b> is a symbol, then the complex symbol
 *  contains all the pins, the text and the graphics.
 *
 *  \image html o_object_relations.png
 *  \image latex o_object_relations.pdf "object relations" width=14cm
 */

//#define PERFORMANCE 1

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>

#include <geda_diagnostics.h>

/** \defgroup object-proc GedaObject Procedures
 * @{
 * \brief Procedures for Operations with #GedaObject Objects
 */

/*! \internal
 * Private function called by geda_xxx_object_read functions if the
 * input buffer contains and errors. If verbose is enable the entire
 * buffer and the passed message are written to the warning log and
 * FALSE is returned to indicate the caller should not show the data
 * buffer. When this function returns TRUE, the caller will display
 * the erroneous portion of the buffer or just a message depending
 * on the context.
 */
bool geda_object_show_buffer_err (const char *msg, const char *buf)
{
  if (geda_get_verbose_mode()) {
    char *line = geda_remove_last_newline((char*)buf);
    geda_log_w ("%s [ %s ]\n", msg, line);
    return FALSE;
  }

  return TRUE;
}

/*!
 * \brief Read a memory buffer
 * \par Function Description
 *  This function reads data in libgeda format from a memory buffer.
 *
 *  If the size argument is negative, the buffer is assumed to be
 *  null-terminated.
 *
 *  The name argument is used for debugging, and should be set to a
 *  meaningful string (e.g. the name of the file the data is from).
 *
 * \param [in,out] toplevel     Current GedaToplevel structure,
 * \param [in]     object_list  Object list to read data to,
 * \param [in]     buffer       Memory buffer to read from,
 * \param [in]     size         Size of the buffer,
 * \param [in]     name         Name to describe the data,
 * \param [in,out] err          GError structure for error reporting.
 *
 * \return GList of objects if successful read, or NULL on error.
 */
GList *geda_object_read_buffer (GedaToplevel *toplevel, GList    *object_list,
                                const char   *buffer,   const int size,
                                const char   *name,     GError  **err)
{
  GList *object_list_save      = NULL;
  GList *new_attrs_list        = NULL;
  GList *new_object_list       = NULL;

  unsigned int release_ver     = 0;
  unsigned int fileformat_ver  = 0;

  int pin_count                = 0;
  int itemsread                = 0;
  int embedded_level           = 0;
  GedaObject *last_complex     = NULL;
  GedaObject *new_obj          = NULL;

  TextBuffer *tb;
  const char *line;
  bool        is_ask;

  /*! \internal Subfunction to set err when bad marker encountered */
  void set_err_bad_marker(const char *msg1) {

    const char *msg2 = _("in file");

    int line_count = geda_struct_textbuffer_get_line_count(tb);

    g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
                 "%s <%d>, %s [%s] :\n>>\n%s<<\n",
                 msg1, line_count, msg2, name, line); /* line = bad text */
  }

  if (buffer == NULL) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_NULL_POINTER,
                 _("detected NULL pointer to buffer"));
    return NULL;
  }

  /* Check the buffer is valid UTF-8 */
  if (!g_utf8_validate (buffer, (size < 0) ? -1 : size, NULL)) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_UNKNOWN_ENCODING,
                 _("Schematic data is not valid UTF-8"));
    return NULL;
  }

  if (!GEDA_IS_TOPLEVEL(toplevel)) {
    g_set_error (err, EDA_ERROR, EINVAL, _("Invalid pointer to GedaToplevel"));
    return NULL;
  }

  tb   = geda_struct_textbuffer_new (buffer, size);

  line = geda_struct_textbuffer_next_line(tb);

  while (line) {

    char objtype;

    const char *ptr = line;

    /* Skip over leading spaces */
    while ((*ptr == SPACE) && (*ptr != ASCII_CR) && (*ptr != ASCII_NUL)) { ++ptr; }

    objtype = *ptr;

    /* Check the symbol version if */
    /* 0) symbol checking is enabled */
    /* 1) the last object read was a complex and */
    /* 2) the next object is not the start of attributes. */
    /* If the next object is the start of attributes, then check the */
    /* symbol version after the attributes have been read in, see the */
    /* STARTATTACH_ATTR case */
    if (toplevel->check_symbol_version) {
      if (last_complex && objtype != STARTATTACH_ATTR) {
        /* yes */
        /* verify symbol version (not file format but rather contents) */
        geda_complex_object_check_symbol_version(toplevel, last_complex);
        last_complex = NULL;  /* no longer need to check */
      }
    }

    switch (objtype) {

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER): /* Really? */
        new_obj = geda_complex_object_read (toplevel, line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;

        new_object_list = g_list_prepend (new_object_list, new_obj);

        /* last_complex is used for verifying symversion attribute */
        last_complex = new_obj;
        break;

      case(OBJ_NET):
        new_obj = geda_net_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_BOX):
        new_obj = geda_box_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        new_obj = geda_circle_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_LINE):
        new_obj = geda_line_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_TEXT):
        new_obj = geda_text_object_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        if (toplevel->rendered_text_bounds_func != NULL)
          geda_text_object_set_rendered_bounds_func (new_obj,
                                           toplevel->rendered_text_bounds_func,
                                           toplevel->rendered_text_bounds_data);
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PIN):
        new_obj = geda_pin_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        pin_count++;
        break;

      case(OBJ_ARC):
        new_obj = geda_arc_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_BUS):
        new_obj = geda_bus_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = geda_path_object_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PICTURE):
        new_obj = geda_picture_object_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(STARTATTACH_ATTR):
        /* first is the fp */
        /* 2nd is the object to get the attributes */
        if (new_obj != NULL) {
          geda_attrib_object_freeze_hooks (new_obj);
          new_attrs_list = geda_attrib_object_read (toplevel, new_obj, tb, release_ver, fileformat_ver, err);
          if (new_attrs_list == NULL) {
            goto error;
          }
          new_object_list = g_list_concat (new_attrs_list, new_object_list);
          geda_attrib_object_thaw_hooks (new_obj);

          /* by now we have finished reading all the attributes */
          /* did we just finish attaching to a complex object? */
          if (toplevel->check_symbol_version && last_complex) {
            /* yes */
            /* verify symbol version (not file format but rather contents) */
            geda_complex_object_check_symbol_version(toplevel, last_complex);
            last_complex = NULL;
          }

          /* slots only apply to complex objects */
          if (new_obj != NULL &&
              (new_obj->type == OBJ_COMPLEX ||
               new_obj->type == OBJ_PLACEHOLDER)) {
            geda_struct_slot_update_object (new_obj);
          }
          new_obj = NULL;
        }
        else {
          set_err_bad_marker(_("Read unexpected attach symbol start marker on line"));
          goto error2;
        }
        break;

      case(START_EMBEDDED):

        if (new_object_list != NULL) {

          new_obj = new_object_list->data;

          if (new_obj != NULL &&
             (new_obj->type == OBJ_COMPLEX ||
              new_obj->type == OBJ_PLACEHOLDER)) {

            object_list_save = new_object_list;
            new_object_list = new_obj->complex->prim_objs;
            embedded_level++;
          }
          else {
            set_err_bad_marker(_("Read unexpected embedded symbol start marker on line"));
            goto error2;
          }
        }
        else {
          set_err_bad_marker(_("Read unexpected embedded symbol start marker on line"));
          goto error2;
        }
        break;

      case(END_EMBEDDED):
        if (embedded_level > 0) {

          GList *pins = NULL;
          GList *iter;

          new_object_list = g_list_reverse (new_object_list);

          new_obj                     = object_list_save->data;
          new_obj->complex->prim_objs = new_object_list;
          new_object_list             = object_list_save;

          /* set the parent fields now and check for pin objects */
          for (iter = new_obj->complex->prim_objs; iter != NULL; iter = iter->next) {
            GedaObject *tmp = iter->data;
            tmp->parent_object = new_obj;
            if (GEDA_IS_PIN(tmp)) {
              pins = g_list_prepend(pins, tmp);
            }
          }

          /* Set the pin_objs list */
          if (pins) {
            new_obj->complex->pin_objs = g_list_reverse (pins);
          }

          new_obj->bounds_valid = FALSE;

          embedded_level--;
        }
        else {
          set_err_bad_marker(_("Read unexpected embedded symbol end marker on line"));
          goto error2;
        }
        break;

      case(ENDATTACH_ATTR):
        /* this case is never hit, since the } is consumed by geda_attrib_object_read */
        break;

      case(INFO_FONT):
        /* NOP */
        break;

      case(ASCII_CR):
      case(ASCII_LF):
      case(COMMENT):
        /* do nothing */
        break;

      case(VERSION_CHAR):

        itemsread = sscanf(line, "v %u %u\n", &release_ver, &fileformat_ver);

        if (itemsread == 0) {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, "failed to parse version from buffer.");
          goto error;
        }

        /* 20030921 was the last version which did not have a fileformat */
        /* version.  The below latter test should not happen, but it is here */
        /* just in in case. */
        if (release_ver <= VERSION_20030921 || itemsread == 1) {
          fileformat_ver = 0;
        }

        if (fileformat_ver == 0) {
          geda_log ("%s:\n[%s]\n", _("Read an old format sym/sch file!\n"
                                     "Please run g[sym|sch]update on"), name);
        }

        break;

      default:

        /* some upstream message handlers don't want non-ASCII message data,
         * so check line before returning and conditionally leave off line */
        is_ask = TRUE;
        while (*ptr != ASCII_NUL) {
          if ((*ptr < SPACE) && (*ptr != ASCII_CR || *ptr != ASCII_LF)) is_ask = FALSE;
          if ( *ptr > ASCII_TILDE) is_ask = FALSE;
          if (!is_ask) break;
          ++ptr;
        }
        if (is_ask) {
          set_err_bad_marker(_("Read garbage near line"));
        }
        else {
          /* The line contains data unsuitable for display, leave off */
          const char *msg1 = _("Read garbage near line");
          const char *msg2 = _("in file");
          int line_count   = geda_struct_textbuffer_get_line_count(tb);
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, "%s <%d> %s [%s]",
                       msg1, line_count, msg2, name);
        }
        new_obj = NULL;
        goto error2;
    }

    line = geda_struct_textbuffer_next_line(tb);
  } /* Wend !EOF */

  /* Was the very last thing we read a complex and has it not been checked */
  /* yet?  This would happen if the complex is at the very end of the file  */
  /* and had no attached attributes */
  if (toplevel->check_symbol_version && last_complex) {
    geda_complex_object_check_symbol_version(toplevel, last_complex);
    last_complex = NULL;  /* no longer need to check */
  }

  if (pin_count > 0) {
    if (release_ver <= VERSION_20020825) {
      geda_pin_object_update_whichend (new_object_list, pin_count);
    }
  }

  tb = geda_struct_textbuffer_free(tb);

  new_object_list = g_list_reverse(new_object_list);

  object_list = geda_glist_concat (object_list, new_object_list);

  return (object_list);

error:

   /* Note that the text buffer has not been released */
   g_prefix_error(err, " %s %d, ", _("On or about line"),
                  geda_struct_textbuffer_get_line_count(tb));

error2:

  geda_struct_object_release_objects(new_object_list);

  tb = geda_struct_textbuffer_free(tb);

  return NULL;
}

/*!
 * \brief Read a file
 * \par Function Description
 *  This function reads a file in libgeda format.
 *
 * \param [in,out] toplevel     The current GedaToplevel structure.
 * \param [in]     object_list  The object_list to read data to.
 * \param [in]     filename     The filename to read from.
 * \param [in,out] err          GError structure for error reporting, or
 *                              NULL to disable error reporting
 * \return object_list if successful read, or NULL on error.
 */
GList *geda_object_read (GedaToplevel *toplevel, GList *object_list, char *filename,
                  GError      **err)
{
  char  *buffer = (NULL);
  size_t size   = 0;

  /* Return NULL if error reporting is enabled and the return location
   * for an error is not NULL. */
  g_return_val_if_fail (err == (NULL) || *err == NULL, (NULL));

  if (!geda_file_get_contents (filename, &buffer, &size, err)){
    return (NULL);
  }

#if PERFORMANCE
  printf("%s processing <%s>\n",__func__, filename);
  START_GEDA_PERFORMANCE
#endif

  GList *result;

  /* Parse file contents */
  result = geda_object_read_buffer (toplevel, object_list, buffer, size, filename, err);

  STOP_GEDA_PERFORMANCE;

  GEDA_FREE (buffer);

  return result;
}

/*!
 * \brief Make a Copy a GedaObject
 * \par Function Description
 *  Returns a copy the object \a o_current. The new object inherits
 *  all of the properties of o_current such as selectable, visibility,
 *  show_name_value, etc.
 *
 * \param [in]  o_current The object to be copied
 *
 * \return pointer to a new GedaObject
 */
GedaObject *geda_object_copy (GedaObject *o_current)
{
  GedaObject *new_obj;

  g_return_val_if_fail (o_current != NULL, NULL);

  switch(o_current->type) {

    case(OBJ_LINE):
      new_obj = geda_line_object_copy (o_current);
      break;

    case(OBJ_NET):
      new_obj = geda_net_object_copy (o_current);
      break;

    case(OBJ_BUS):
      new_obj = geda_bus_object_copy (o_current);
      break;

    case(OBJ_BOX):
      new_obj = geda_box_object_copy (o_current);
      break;

    case(OBJ_PICTURE):
      new_obj = geda_picture_object_copy (o_current);
      break;

    case(OBJ_CIRCLE):
      new_obj = geda_circle_object_copy (o_current);
      break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
      new_obj = geda_complex_object_copy (o_current);
      break;

    case(OBJ_TEXT):
      new_obj = geda_text_object_copy (o_current);
      break;

    case(OBJ_PATH):
      new_obj = geda_path_object_copy (o_current);
      break;

    case(OBJ_PIN):
      new_obj = geda_pin_object_copy (o_current);
      break;

    case(OBJ_ARC):
      new_obj = geda_arc_object_copy (o_current);
      break;

    default:
      BUG_IMSG("Bad object type", o_current->type);
      return NULL;
  }

  if (new_obj) {

    new_obj->color             = o_current->color;
    new_obj->dont_redraw       = o_current->dont_redraw;
    new_obj->locked_color      = o_current->locked_color;
    new_obj->selectable        = o_current->selectable;
    new_obj->show_name_value   = o_current->show_name_value;
    new_obj->visibility        = o_current->visibility;

    /* Store a reference in the copied object to where it was copied.
     * Used to retain associations when copying attributes */
    o_current->copied_to = new_obj;
  }

  return new_obj;
}

/*!
 * \brief Mirrors an object
 * \par Function Description
 *  This function mirrors an object about the point
 *  (<B>center_wx</B>,<B>center_wy</B>).
 *
 * \param [in]     center_x  Origin x coordinate.
 * \param [in]     center_y  Origin y coordinate.
 * \param [in,out] object    The GedaObject to mirror.
 */
void geda_object_mirror (GedaObject *object, int center_x, int center_y)
{
  void (*topless) (GedaObject*, int, int) = NULL;

  switch (object->type) {
      case OBJ_LINE:    topless = geda_line_object_mirror;       break;
      case OBJ_NET:     topless = geda_net_object_mirror;        break;
      case OBJ_BUS:     topless = geda_bus_object_mirror;        break;
      case OBJ_BOX:     topless = geda_box_object_mirror;        break;
      case OBJ_PICTURE: topless = geda_picture_object_mirror;    break;
      case OBJ_CIRCLE:  topless = geda_circle_object_mirror;     break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: topless = geda_complex_object_mirror;    break;
      case OBJ_TEXT:    topless = geda_text_object_mirror;       break;
      case OBJ_PATH:    topless = geda_path_object_mirror;       break;
      case OBJ_PIN:     topless = geda_pin_object_mirror;        break;
      case OBJ_ARC:     topless = geda_arc_object_mirror;        break;
  }

  if (topless != NULL) {

    geda_object_notify_emit_pre_change (object);

    (*topless) (object, center_x, center_y);

    /* Update object to tile system. */
    geda_struct_tile_update_object(object);

    geda_object_notify_emit_change(object);

    geda_struct_object_set_page_changed (object);

  }
  else {
    BUG_IMSG("Bad object type", object->type);
  }
}

/*!
 * \brief Rotates an object
 * \par Function Description
 *  This function rotates the object <B>object</B> about the coordinates
 *  <B>center_wx</B> and <B>center_wy</B>, by <B>angle</B>degrees.
 *
 * \param [in] object    The object to rotate.
 * \param [in] center_x  X coordinate of rotation center
 * \param [in] center_y  Y coordinate of rotation center
 * \param [in] angle     Angle of rotation (degrees)
 */
void geda_object_rotate (GedaObject *object, int center_x, int center_y, int angle)
{
  void (*topless) (GedaObject *, int, int, int) = NULL;

  switch (object->type)
  {
    case OBJ_LINE:    topless = geda_line_object_rotate;       break;
    case OBJ_NET:     topless = geda_net_object_rotate;        break;
    case OBJ_BUS:     topless = geda_bus_object_rotate;        break;
    case OBJ_BOX:     topless = geda_box_object_rotate;        break;
    case OBJ_PICTURE: topless = geda_picture_object_rotate;    break;
    case OBJ_CIRCLE:  topless = geda_circle_object_rotate;     break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX: topless = geda_complex_object_rotate;    break;
    case OBJ_TEXT:    topless = geda_text_object_rotate;       break;
    case OBJ_PATH:    topless = geda_path_object_rotate;       break;
    case OBJ_PIN:     topless = geda_pin_object_rotate;        break;
    case OBJ_ARC:     topless = geda_arc_object_rotate;        break;
    default:
      break;
  }

  if (topless != NULL) {

    geda_object_notify_emit_pre_change (object);

    (*topless) (object, center_x, center_y, angle);

    /* Update object to tile system. */
    geda_struct_tile_update_object(object);

    geda_object_notify_emit_change(object);

    geda_struct_object_set_page_changed (object);
  }
  else {
    BUG_IMSG("Bad object type '%c'", object->type);
  }
}

/*!
 * \brief Translates an object
 * \par Function Description
 *  This function translates the object <B>object</B> by
 *  <B>dx</B> and <B>dy</B>.
 *
 * \param [in] object   The object to translate
 * \param [in] dx       Amount to horizontally translate object
 * \param [in] dy       Amount to vertically translate object
 */
void geda_object_translate (GedaObject *object, int dx, int dy)
{
  void (*topless) (GedaObject *, int, int) = NULL;

  switch (object->type) {
      case OBJ_LINE:    topless = geda_line_object_translate;    break;
      case OBJ_NET:     topless = geda_net_object_translate;     break;
      case OBJ_BUS:     topless = geda_bus_object_translate;     break;
      case OBJ_BOX:     topless = geda_box_object_translate;     break;
      case OBJ_PICTURE: topless = geda_picture_object_translate; break;
      case OBJ_CIRCLE:  topless = geda_circle_object_translate;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: topless = geda_complex_object_translate; break;
      case OBJ_TEXT:    topless = geda_text_object_translate;    break;
      case OBJ_PATH:    topless = geda_path_object_translate;    break;
      case OBJ_PIN:     topless = geda_pin_object_translate;     break;
      case OBJ_ARC:     topless = geda_arc_object_translate;     break;
      default:
        break;
  }

  if (topless != NULL) {

    geda_object_notify_emit_pre_change (object);

    (*topless) (object, dx, dy);

    /* Update object to tile system. */
    geda_struct_tile_update_object(object);

    geda_object_notify_emit_change(object);

    geda_struct_object_set_page_changed (object);
  }
  else {
    BUG_IMSG("Bad object type '%c'", object->type);
  }
}

/*!
 * \brief Update an object connections
 * \par Function Description
 *  Convienence function that combines
 *  <B>geda_struct_tile_update_object</B> and
 *  <B>geda_struct_conn_update_object</B>.
 *
 * \param [in] object The object to update.
 */
void geda_object_update (GedaObject *object)
{
  geda_struct_tile_update_object (object);
  geda_struct_conn_update_object (object);
}

/** @} endgroup object-proc */
