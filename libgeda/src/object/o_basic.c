/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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

/*! \file o_basic.c
 *  \brief functions for the basic object type
 *
 *  This file contains the code used to manipulate <b>Objects</b>.
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 *  The <b>Object</b> be extended to become concrete objects like a line,
 *  a pin, text, a circle or a picture. These extentions are substructures
 *  in the object struct.
 *  The <b>Subojects</b> are pictures (st_picture), paths (st_path), arcs (st_arc),
 *  a lines (st_line), boxes (st_box), circles (st_circle), texts (st_text) and
 *  a the complex type (st_complex).
 *
 *  Pins, nets and busses are types of line <b>Objects</b>.
 *
 *  The <b>Complex Objects</b> can be linked to many primary objects. If the <b>Complex
 *  Object</b> is a symbol, then the complex symbol contains all the pins,
 *  the text and the graphics.
 *
 *  \#include "version.h" image html o_object_relations.png
 *  \image latex o_object_relations.pdf "object relations" width=14cm
 */

#include <config.h>
#include <stdio.h>
#include "ascii.h"
#include "libgeda_priv.h"

/*! \brief Read a memory buffer
 *  \par Function Description
 *  This function reads data in libgeda format from a memory buffer.
 *
 *  If the size argument is negative, the buffer is assumed to be
 *  null-terminated.
 *
 *  The name argument is used for debugging, and should be set to a
 *  meaningful string (e.g. the name of the file the data is from).
 *
 *  \param [in,out] toplevel     Current GedaToplevel structure.
 *  \param [in]     object_list  Object list to read data to.
 *  \param [in]     buffer       Memory buffer to read from.
 *  \param [in]     size         Size of the buffer.
 *  \param [in]     name         Name to describe the data.
 *  \param [in,out] err          GError structure for error reporting.
 *
 *  \return GList of objects if successful read, or NULL on error.
 */
GList * o_read_buffer (GedaToplevel *toplevel, GList    *object_list,
                       const char   *buffer,   const int size,
                       const char   *name,     GError  **err)
{
  const char *line             = NULL;
  TextBuffer *tb               = NULL;

  char    objtype;
  GList  *object_list_save     = NULL;
  Object *new_obj              = NULL;
  GList  *new_attrs_list       = NULL;
  GList  *new_object_list      = NULL;
  GList  *iter;
  unsigned int release_ver     = 0;
  unsigned int fileformat_ver  = 0;

  int found_pin                = 0;
  int itemsread                = 0;
  int embedded_level           = 0;
  int line_count               = 0;
  Object* last_complex         = NULL;
  bool is_ask                  = TRUE;
  char* ptr;

  if (buffer == NULL) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_NULL_POINTER,
                 _("<o_read_buffer> detected NULL pointer to buffer "));
    return NULL;
  }

  /* Check the buffer is valid UTF-8 */
  if (!g_utf8_validate (buffer, (size < 0) ? -1 : size, NULL)) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_UNKNOWN_ENCODING,
                 _("Schematic data was not valid UTF-8"));
    return NULL;
  }

  tb = s_textbuffer_new (buffer, size);

  while (1) {

    line = s_textbuffer_next_line(tb);
    ++line_count;
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);

    /* Do we need to check the symbol version?  Yes, but only if */
    /* 1) the last object read was a complex and */
    /* 2) the next object isn't the start of attributes.  */
    /* If the next object is the start of attributes, then check the */
    /* symbol version after the attributes have been read in, see the */
    /* STARTATTACH_ATTR case */
    if (last_complex && objtype != STARTATTACH_ATTR)
    {
        /* yes */
        /* verify symbol version (not file format but rather contents) */
        o_complex_check_symversion(toplevel, last_complex);
        last_complex = NULL;  /* no longer need to check */
    }

    switch (objtype) {

      case(OBJ_LINE):
        if ((new_obj = o_line_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_NET):
        if ((new_obj = o_net_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_BUS):
        if ((new_obj = o_bus_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_BOX):
        if ((new_obj = o_box_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PICTURE):
        new_obj = o_picture_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        if ((new_obj = o_circle_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER): /* Really? */
        new_obj = o_complex_read (toplevel, line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;

        new_object_list = g_list_prepend (new_object_list, new_obj);

        /* last_complex is used for verifying symversion attribute */
        last_complex = new_obj;

        break;

      case(OBJ_TEXT):
        new_obj = o_text_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        if (toplevel->rendered_text_bounds_func != NULL)
          o_text_set_rendered_bounds_func (new_obj,
                                           toplevel->rendered_text_bounds_func,
                                           toplevel->rendered_text_bounds_data);
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = o_path_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PIN):
        if ((new_obj = o_pin_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        found_pin++;
        break;

      case(OBJ_ARC):
        if ((new_obj = o_arc_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(STARTATTACH_ATTR):
        /* first is the fp */
        /* 2nd is the object to get the attributes */
        if (new_obj != NULL) {
          o_attrib_freeze_hooks (new_obj);
          new_attrs_list = o_read_attribs (toplevel, new_obj, tb, release_ver, fileformat_ver, err);
          if (new_attrs_list == NULL)
            goto error;
          new_object_list = g_list_concat (new_attrs_list, new_object_list);
          o_attrib_thaw_hooks (new_obj);

          /* by now we have finished reading all the attributes */
          /* did we just finish attaching to a complex object? */
          if (last_complex)
          {
            /* yes */
            /* verify symbol version (not file format but rather contents) */
            o_complex_check_symversion(toplevel, last_complex);
            last_complex = NULL;
          }

          /* slots only apply to complex objects */
          if (new_obj != NULL &&
              (new_obj->type == OBJ_COMPLEX ||
               new_obj->type == OBJ_PLACEHOLDER)) {
            s_slot_update_object (new_obj);
          }
          new_obj = NULL;
        }
        else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Read unexpected attach "
                                                          "symbol start marker on line <%d>, in [%s] :\n>>\n%s<<\n"),
                       line_count, name, line);
          goto error;
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

            g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
                         _("Read unexpected embedded "
                           "symbol start marker on line <%d>, in [%s] :\n>>\n%s<<\n"),
                            line_count, name, line);
           goto error;
          }
        }
        else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
                       _("Read unexpected embedded "
                         "symbol start marker on line <%d>, in [%s] :\n>>\n%s<<\n"),
                          line_count, name, line);
          goto error;
        }
        break;

      case(END_EMBEDDED):
        if (embedded_level>0) {
          /* don't do this since objects are already
           * stored/read translated
           * o_complex_translate_world (toplevel, new_object_list->x,
           *                            new_object_list->y, new_object_list->complex);
           */
          new_object_list = g_list_reverse (new_object_list);

          new_obj = object_list_save->data;
          new_obj->complex->prim_objs = new_object_list;
          new_object_list = object_list_save;

          /* set the parent field now */
          for (iter = new_obj->complex->prim_objs;
               iter != NULL; iter = g_list_next (iter)) {
            Object *tmp = iter->data;
            tmp->parent_object = new_obj;
          }

          new_obj->w_bounds_valid_for = NULL;

          embedded_level--;
        } else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Read unexpected embedded "
                                                          "symbol end marker on line <%d>, in [%s] :\n>>\n%s<<\n"),
                       line_count, name, line);
          goto error;
        }
        break;

      case(ENDATTACH_ATTR):
        /* this case is never hit, since the } is consumed by o_read_attribs */
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
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, "Failed to parse version from buffer.");
          goto error;
        }

        /* 20030921 was the last version which did not have a fileformat */
        /* version.  The below latter test should not happen, but it is here */
        /* just in in case. */
        if (release_ver <= VERSION_20030921 || itemsread == 1) {
          fileformat_ver = 0;
        }

        if (fileformat_ver == 0) {
          u_log_message(_("Read an old format sym/sch file!\n"
                          "Please run g[sym|sch]update on:\n[%s]\n"), name);
        }

        break;

      default:

        /* some upstream message handlers don't want non-ASCII message data,
         * so check line before returning and conditionally leave off line */
        ptr = (char*)line;
        while ( *ptr != ASCII_NUL) {
          if (( *ptr < SPACE) && (*ptr != ASCII_CR || *ptr != ASCII_LF)) is_ask = FALSE;
          if ( *ptr > ASCII_TILDE) is_ask = FALSE;
          if (!is_ask) break;
          ++ptr;
        }
        if (is_ask)
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
                       _("Read garbage line <%d> in [%s] :\n>>\n%s<<\n"),
                       line_count, name, line);
        else
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
                       _("Read garbage line <%d> in [%s]"),
                       line_count, name);
        new_obj = NULL;
        goto error;
    }
  }

  /* Was the very last thing we read a complex and has it not been checked */
  /* yet?  This would happen if the complex is at the very end of the file  */
  /* and had no attached attributes */
  if (last_complex) {
    o_complex_check_symversion(toplevel, last_complex);
    last_complex = NULL;  /* no longer need to check */
  }

  if (found_pin) {
    if (release_ver <= VERSION_20020825) {
      o_pin_update_whichend (new_object_list, found_pin);
    }
  }

  tb = s_textbuffer_free(tb);

  new_object_list = g_list_reverse(new_object_list);

  object_list = g_list_concat (object_list, new_object_list);

  return(object_list);

 error:

  s_object_release_objects(new_object_list);

  return NULL;
}

/*! \brief Read a file
 *  \par Function Description
 *  This function reads a file in libgeda format.
 *
 *  \param [in,out] toplevel     The current GedaToplevel structure.
 *  \param [in]     object_list  The object_list to read data to.
 *  \param [in]     filename     The filename to read from.
 *  \param [in,out] err          GError structure for error reporting, or
 *                               NULL to disable error reporting
 *  \return object_list if successful read, or NULL on error.
 */
GList *o_read (GedaToplevel *toplevel, GList *object_list, char *filename,
               GError **err)
{
  char *buffer = NULL;
  size_t size  = 0;
  GList *result;

  /* Return NULL if error reporting is enabled and the return location
   * for an error isn't NULL. */
  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  if (!f_get_file_contents (filename, &buffer, &size, err)){
    return NULL;
  }

  /* Parse file contents */
  result = o_read_buffer (toplevel, object_list, buffer, size, filename, err);
  GEDA_FREE (buffer);

  return result;
}

/*! \todo Finish documentation!!!!
 *  \brief
 *  \par Function Description
 *  returns head !!!!!!!!!!!!!!!!!!!
 *  look at above.. this returns what was passed in!!!!
 *  copies selected to list_head (!! returns new list)
 *
 *  \param [in]  selected
 *  \return Object pointer.
 */
Object *o_object_copy (Object *selected)
{
  Object *new_obj;

  g_return_val_if_fail (selected != NULL, NULL);

  switch(selected->type) {

    case(OBJ_LINE):
      new_obj = o_line_copy (selected);
      break;

    case(OBJ_NET):
      new_obj = o_net_copy (selected);
      break;

    case(OBJ_BUS):
      new_obj = o_bus_copy (selected);
      break;

    case(OBJ_BOX):
      new_obj = o_box_copy (selected);
      break;

    case(OBJ_PICTURE):
      new_obj = o_picture_copy (selected);
      break;

    case(OBJ_CIRCLE):
      new_obj = o_circle_copy (selected);
      break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
      new_obj = o_complex_copy (selected);
      break;

    case(OBJ_TEXT):
      new_obj = o_text_copy (selected);
      break;

    case(OBJ_PATH):
      new_obj = o_path_copy (selected);
      break;

    case(OBJ_PIN):
      new_obj = o_pin_copy (selected);
      break;

    case(OBJ_ARC):
      new_obj = o_arc_copy (selected);
      break;

    default:
      g_critical ("o_list_copy_to: object %p has bad type '%c'\n",
                  selected, selected->type);
      return NULL;
  }


  new_obj->color             = selected->color;
  new_obj->dont_redraw       = selected->dont_redraw;
  new_obj->locked_color      = selected->locked_color;
  new_obj->selectable        = selected->selectable;
  new_obj->show_name_value   = selected->show_name_value;
  new_obj->visibility        = selected->visibility;

  /* Store a reference in the copied object to where it was copied.
   * Used to retain associations when copying attributes */
  selected->copied_to = new_obj;

  return new_obj;
}

/*! \brief Translates an object in world coordinates
 *  \par Function Description
 *  This function translates the object <B>object</B> by
 *  <B>dx</B> and <B>dy</B>.
 *
 *  \param [in] dx       Amount to horizontally translate object
 *  \param [in] dy       Amount to vertically translate object
 *  \param [in] object   The object to translate.
 */
void o_translate_world (int dx, int dy, Object *object)
{
  void (*topless) (int, int, Object*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    topless = o_line_translate_world;    break;
      case OBJ_NET:     topless = o_net_translate_world;     break;
      case OBJ_BUS:     topless = o_bus_translate_world;     break;
      case OBJ_BOX:     topless = o_box_translate_world;     break;
      case OBJ_PICTURE: topless = o_picture_translate_world; break;
      case OBJ_CIRCLE:  topless = o_circle_translate_world;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: topless = o_complex_translate_world; break;
      case OBJ_TEXT:    topless = o_text_translate_world;    break;
      case OBJ_PATH:    topless = o_path_translate_world;    break;
      case OBJ_PIN:     topless = o_pin_translate_world;     break;
      case OBJ_ARC:     topless = o_arc_translate_world;     break;
      default:
        break;
  }

  if (topless != NULL) {
    (*topless) (dx, dy, object);
  }
  else
    g_critical ("o_translate_world: object %p has bad type '%c'\n",
                  object, object->type);
}

/*! \brief Rotates an object in world coordinates
 *  \par Function Description
 *  This function rotates the object <B>object</B> about the coordinates
 *  <B>world_centerx</B> and <B>world_centery</B>, by <B>angle</B>degrees.
 *
 *  \param [in] w_centerx  X coordinate of rotation center (world coords)
 *  \param [in] w_centery  Y coordinate of rotation center (world coords)
 *  \param [in] angle          Angle of rotation (degrees)
 *  \param [in] object         The object to rotate.
 */
void o_rotate_world (int w_centerx, int w_centery, int angle, Object *object)
{
  void (*topless) (int, int, int, Object*) = NULL;

  switch (object->type)
  {
    case OBJ_LINE:    topless = o_line_rotate_world;       break;
    case OBJ_NET:     topless = o_net_rotate_world;        break;
    case OBJ_BUS:     topless = o_bus_rotate_world;        break;
    case OBJ_BOX:     topless = o_box_rotate_world;        break;
    case OBJ_PICTURE: topless = o_picture_rotate_world;    break;
    case OBJ_CIRCLE:  topless = o_circle_rotate_world;     break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX: topless = o_complex_rotate_world;    break;
    case OBJ_TEXT:    topless = o_text_rotate_world;       break;
    case OBJ_PATH:    topless = o_path_rotate_world;       break;
    case OBJ_PIN:     topless = o_pin_rotate_world;        break;
    case OBJ_ARC:     topless = o_arc_rotate_world;        break;
    default:
      break;
  }

  if (topless != NULL) {
    (*topless) (w_centerx, w_centery, angle, object);
  }
  else
    g_critical ("o_rotate_world: object %p has bad type '%c'\n",
                 object, object->type);
}


/*! \brief Mirrors an object in world coordinates
 *  \par Function Description
 *  This function mirrors an object about the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world units.
 *
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         The Object to mirror.
 */
void o_mirror_world (int world_centerx, int world_centery, Object *object)
{
  void (*topless) (int, int, Object*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    topless = o_line_mirror_world;       break;
      case OBJ_NET:     topless = o_net_mirror_world;        break;
      case OBJ_BUS:     topless = o_bus_mirror_world;        break;
      case OBJ_BOX:     topless = o_box_mirror_world;        break;
      case OBJ_PICTURE: topless = o_picture_mirror_world;    break;
      case OBJ_CIRCLE:  topless = o_circle_mirror_world;     break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: topless = o_complex_mirror_world;    break;
      case OBJ_TEXT:    topless = o_text_mirror_world;       break;
      case OBJ_PATH:    topless = o_path_mirror_world;       break;
      case OBJ_PIN:     topless = o_pin_mirror_world;        break;
      case OBJ_ARC:     topless = o_arc_mirror_world;        break;
  }

  if (topless != NULL) {
    (*topless) (world_centerx, world_centery, object);
  }
  else
    g_critical ("o_mirror_world: object %p has bad type '%c'\n",
                 object, object->type);
}

/*! \brief Scale a set of lines.
 *  \par Function Description
 *  This function takes a list of lines and scales them
 *  by the values of x_scale and y_scale.
 *
 *  \param [in,out]  list  The list with lines to scale.
 *  \param [in]   x_scale  The x scale value for the lines.
 *  \param [in]   y_scale  The y scale value for the lines.
 *
 */
void o_scale (GList *list, int x_scale, int y_scale)
{
  Object *o_current;
  GList *iter;

  /* this is okay if you just hit scale and have nothing selected */
  if (list == NULL) {
    return;
  }

  iter = list;
  while (iter != NULL) {
    o_current = (Object *)iter->data;
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_scale_world(x_scale, y_scale, o_current);
        break;
    }
    iter = g_list_next (iter);
  }
}
