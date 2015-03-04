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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file o_complex_basic.c
 *  \brief Functions for complex objects
 *
 *  Complex objects are collections of primary objects.
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include "libgeda_priv.h"

/*! \brief Queries the bounds of a complex object
 *
 *  \par Function Description
 *  This function returns the bounding box of the complex object
 *  <B>object</B>.
 *
 *  \param [in]  object   The complex object.
 */
int
o_complex_get_bounds(Object *object)
{
  g_return_val_if_fail (GEDA_IS_COMPLEX(object), FALSE);
  return o_get_bounds_list (object->complex->prim_objs,
                            &object->left, &object->top,
                            &object->right, &object->bottom);

}

/*! \brief Get the position of complex base point
 *
 *  \par Function Description
 *  This function gets the position of the base point of a complex object.
 *
 *  \param [out] x       Pointer to the x-position,
 *  \param [out] y       Pointer to the y-position,
 *  \param [in]  object  The object to get the position.
 *
 *  \return TRUE if successfully determined the position, FALSE otherwise.
 */
bool
o_complex_get_position (int *x, int *y, Object *object)
{
  g_return_val_if_fail(GEDA_IS_COMPLEX(object), FALSE);
  *x = object->complex->x;
  *y = object->complex->y;
  return TRUE;
}

/*! \brief check whether an object is a attributes
 *
 *  \par Function Description
 *  This function checks if an object should be promoted.
 *  An attribute object is promotable if it is promoted by default, or the user
 *  has configered it to promote an attribute.
 *
 *  \param [in] toplevel  The GedaToplevel object,
 *  \param [in] object    The attribute object to check.
 *
 *  \return TRUE if the object is a eligible attribute, FALSE otherwise.
 */
static int o_complex_is_eligible_attribute (GedaToplevel *toplevel, Object *object)
{
  char *name = NULL;
  const char *symversion = "symversion=";
  bool  answer = FALSE;

  if (GEDA_IS_TOPLEVEL(toplevel))  {

    if (GEDA_IS_TEXT(object))  {

      int promotableAttribute = FALSE;

      if(object->text->string == NULL) {
        BUG_MSG("GedaText object string value = NULL");
      }
      else if (strncmp(object->text->string, symversion, sizeof(symversion)) == 0) {
        /* always promote symversion= attribute, even if it is invisible */
        answer = TRUE;
      }
      else {

        /* check list against attributes which can be promoted */
        if (toplevel->always_promote_attributes != NULL) {

          if (o_attrib_get_name_value (object, &name, NULL)) {
            if (g_list_find_custom(toplevel->always_promote_attributes,
                name, (GCompareFunc) strcmp) != NULL) {
              /* Name of the attribute was in the always promote attributes list */
              promotableAttribute = TRUE;
            }

            GEDA_FREE(name);
            if (promotableAttribute) {
              answer = TRUE;
            }
          }
        }

        if (!answer) {

          /* object is invisible and we do not want to promote invisible text */
          if ((!o_get_is_visible (object)) &&
              (toplevel->promote_invisible == FALSE)) {
            answer = FALSE; /* attribute not eligible for promotion */
          }
          else {
            /* yup, attribute can be promoted */
            answer = TRUE;
          }
        }
      }
    }
    else {
      BUG_MSG("Object is not a GedaText object");
    }
  }
  else {
    BUG_MSG("Invalid pointer to Toplevel");
  }
  return answer;
}

/*! \brief Get the embedded state of a Complex object
 *  \par Function Description
 *  Returns the status of the complex object.
 *
 *  \param object  The object to check
 *
 *  \return 1 if embedded, 0 otherwise
 */
int o_complex_is_embedded(Object *object)
{
  g_return_val_if_fail(GEDA_IS_COMPLEX(object), 0);
  return object->complex->is_embedded;
}

/*! \brief Get Point on a Complex Nearest a Given Point
 *  \par Function Description
 *  Recursively calls o_get_nearest_point on the closest sub-object of
 *  the complex and returns the results of the function corresponding
 *  to the appropriate type of object for the selected sub-object.
 *
 *  \param [in] object   Pointer to a Box object
 *  \param [in] x        Integer pointer
 *  \param [in] y        Integer pointer
 *  \param [out] nx      Integer pointer
 *  \param [out] ny      Integer pointer
 *
 *  \returns TRUE is the results are valid, FALSE if \a object was not a Complex.
 */
bool o_complex_get_nearest_point (Object *object, int x, int y, int *nx, int *ny)
{
  bool    result;
  Object *closest = NULL;

  if (GEDA_IS_COMPLEX(object)) {

    GList *iter;
    double shortest = G_MAXDOUBLE;

    for (iter = object->complex->prim_objs; iter != NULL; NEXT(iter)) {

      Object *obj = iter->data;

      bool do_check;

      do_check = obj->type == OBJ_LINE ||
                 obj->type == OBJ_ARC  ||
                 obj->type == OBJ_CIRCLE;

      if (do_check) {

        double distance;

        distance = o_get_shortest_distance_full (obj, x, y, TRUE);

        if (distance < shortest) {
          shortest = distance;
          closest  = obj;
        }
      }

      if (shortest == 0.0) {
       *nx      = x;
       *ny      = y;
        result  = TRUE;
        closest = NULL;
        break;
      }
    }
  }

  if (closest) {
    result = o_get_nearest_point(closest, x, y, nx, ny);
  }
  else { /* was not an Complex */
    result = FALSE;
  }

  if (!result) {
    *nx = x;
    *ny = y;
  }

  return result;
}

/*! \brief Get attributes eligible for promotion from inside a complex
 *
 *  \par Function Description
 *  Returns a GList of Objects which are eligible for promotion from
 *  within the passed complex Object.
 *
 *  If detach is TRUE, the function removes these attribute objects
 *  from the prim_objs of the complex.  If detach is FALSE, the
 *  Objects are left in place.
 *
 *  \param [in] toplevel The toplevel environment,
 *  \param [in] object   The complex object being modified,
 *  \param [in] detach   Should the attributes be detached?
 *
 *  \returns Linked list of Objects to promote.
 */
GList *o_complex_get_promotable (GedaToplevel *toplevel, Object *object, int detach)
{
  GList  *promoted = NULL;
  GList  *attribs;
  GList  *iter;
  Object *ptr;

  if (toplevel == NULL)
    return NULL;

  if (!toplevel->attribute_promotion) /* controlled through rc file */
    return NULL;

  attribs = o_attrib_find_floating_attribs (object->complex->prim_objs);

  for (iter = attribs; iter != NULL; iter = iter->next) {

    ptr = iter->data;

    /* Is it an attribute we want to promote? */
    if (o_complex_is_eligible_attribute(toplevel, ptr)) {

      if (detach) {
        ptr->parent_object = NULL;
        object->complex->prim_objs =
        g_list_remove (object->complex->prim_objs, ptr);
      }

      promoted = g_list_prepend (promoted, ptr);
    }
  }

  g_list_free (attribs);

  return g_list_reverse (promoted);
}

/*! \brief Promote attributes from a complex Object
 *
 *  \par Function Description
 *  Selects promotable attributes from \a object, and returns a new
 *  GList containing them (suitable for appending to a #Page).
 *
 *  \param [in] toplevel The #GedaToplevel environment,
 *  \param [in] object   The complex#Object to promote from.
 *
 *  \return A GList of promoted attributes.
 */
GList *o_complex_promote_attribs (GedaToplevel *toplevel, Object *object)
{
  GList *promoted   = NULL;
  GList *promotable = NULL;
  GList *iter       = NULL;

  promotable = o_complex_get_promotable (toplevel, object, FALSE);

  /* Run through the attributes deciding if we want to keep them (in
   * which case we copy them and make them invisible) or if we want to
   * remove them. */
  if (toplevel->keep_invisible) {
    for (iter = promotable; iter != NULL; iter = iter->next) {
      Object *o_kept = (Object *) iter->data;
      Object *o_copy = o_copy_object (o_kept);
      o_set_visibility (o_kept, INVISIBLE);
      o_copy->parent_object = NULL;
      promoted = g_list_prepend (promoted, o_copy);
    }
    promoted = g_list_reverse (promoted);
  }
  else {

    for (iter = promotable; iter != NULL; iter = iter->next) {

      GList  *from_list = object->complex->prim_objs;
      Object *o_removed = (Object *) iter->data;

      o_removed->parent_object = NULL;
      object->complex->prim_objs = g_list_remove (from_list, o_removed);
    }

    promoted = promotable;
    /* Invalidate the object's bounds since we may have
     * stolen objects from inside it. */
    o_set_bounds_invalid (object);
  }

  /* Attach promoted attributes to the original complex object */
  o_attrib_attach_list (promoted, object, TRUE);

  g_list_free (promotable);

  return promoted;
}

/*! \brief Delete or hide promotable from the passed Object
 *
 *  \par Function Description
 *  Deletes or hides promotable attributes from \a Object. This is used
 *  when loading symbols while loading a schematic from disk. The schematic
 *  will already contain local copies of symbol's promotable objects, so we
 *  delete or hide the symbol's copies.
 *
 *  Deletion / hiding is dependant on the setting of
 *  toplevel->keep_invisible. If true, attributes eligible for
 *  promotion are kept in memory but flagged as invisible.
 *
 *  \param [in] toplevel The toplevel environment,
 *  \param [in] object   The complex object being altered.
 */
static void
o_complex_remove_promotable_attribs (GedaToplevel *toplevel, Object *object)
{
  GList *promotable, *iter;

  promotable = o_complex_get_promotable (toplevel, object, FALSE);

  if (promotable == NULL)
    return;

  for (iter = promotable; iter != NULL; iter = iter->next) {

    Object *a_object = iter->data;

    if (toplevel->keep_invisible == TRUE) {   /* Hide promotable attributes */
      o_set_visibility (a_object, INVISIBLE);
    }
    else {                                    /* Delete promotable attributes */
      GList *from_list = object->complex->prim_objs;
      object->complex->prim_objs = g_list_remove (from_list, a_object);
      s_object_release (a_object);
    }
  }

  o_set_bounds_invalid (object);
  g_list_free (promotable);
}

static Object*
create_placeholder(GedaToplevel *toplevel, Complex *complex, int x, int y, int angle, int mirror)
{
  Object      *new_prim_obj;
  LINE_OPTIONS line_options;

  char *not_found_text = NULL;

  int left   = 0;
  int top    = 0;
  int right  = 0;
  int bottom = 0;

  int x_offset, y_offset;

  line_options.line_end    = END_ROUND;
  line_options.line_type   = TYPE_SOLID;
  line_options.line_width  = 50;
  line_options.line_space  = -1;
  line_options.line_length = -1;

  /* Mark the origin of the missing component */
  new_prim_obj = o_line_new(DETACHED_ATTRIBUTE_COLOR, x - 50, y, x + 50, y);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = o_line_new(DETACHED_ATTRIBUTE_COLOR, x, y + 50, x, y - 50);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  /* Add some useful text */
  not_found_text = u_string_sprintf (_("Component not found:\n %s"), complex->filename);

  new_prim_obj = o_text_new(DETACHED_ATTRIBUTE_COLOR,
                            x + NOT_FOUND_TEXT_X,
                            y + NOT_FOUND_TEXT_Y, LOWER_LEFT, 0,
                            10, VISIBLE, SHOW_NAME_VALUE,
                            not_found_text);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);
  GEDA_FREE(not_found_text);

  /* figure out where to put the hazard triangle, we could only be here during
   * a read failure, there is no page so we can not use normal bounds routines
   * instead we will try ...*/
  geda_toplevel_set_bounds (toplevel, new_prim_obj);

  x_offset = (right - left) / 4;
  y_offset = bottom - top + 100;  /* 100 is just an additional offset */

  /* add hazard triangle */
  new_prim_obj = o_line_new(DETACHED_ATTRIBUTE_COLOR,
                            x + NOT_FOUND_TEXT_X + x_offset,
                            y + NOT_FOUND_TEXT_Y + y_offset,
                            x + NOT_FOUND_TEXT_X + x_offset + 600,
                            y + NOT_FOUND_TEXT_Y + y_offset);

  o_set_line_options(new_prim_obj, &line_options);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = o_line_new(DETACHED_ATTRIBUTE_COLOR,
                            x + NOT_FOUND_TEXT_X + x_offset,
                            y + NOT_FOUND_TEXT_Y + y_offset,
                            x + NOT_FOUND_TEXT_X + x_offset + 300,
                            y + NOT_FOUND_TEXT_Y + y_offset + 500);

  o_set_line_options(new_prim_obj, &line_options);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = o_line_new(DETACHED_ATTRIBUTE_COLOR,
                            x + NOT_FOUND_TEXT_X + x_offset + 300,
                            y + NOT_FOUND_TEXT_Y + y_offset + 500,
                            x + NOT_FOUND_TEXT_X + x_offset + 600,
                            y + NOT_FOUND_TEXT_Y + y_offset);

  o_set_line_options(new_prim_obj, &line_options);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = o_text_new(DETACHED_ATTRIBUTE_COLOR,
                            x + NOT_FOUND_TEXT_X + x_offset + 270,
                            y + NOT_FOUND_TEXT_Y + y_offset + 90,
                            LOWER_LEFT, 0, 18,
                            VISIBLE, SHOW_NAME_VALUE,
                            "!");

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  complex->prim_objs = g_list_reverse(complex->prim_objs);

  return (Object*)complex;
}

/*! \brief Create a New Complex Object
 *
 *  \par Function Description
 *  Creates and initialize a new complex object.
 *
 *  \return a new complex object
 */
Object *o_complex_new(GedaToplevel *toplevel, int x, int y, int angle,
                      int mirror, const CLibSymbol *clib, const char *basename,
                      int selectable)
{
  Object  *new_obj;
  Complex *complex;

  GList *iter;
  char *buffer;

  buffer  = NULL;
  new_obj = geda_complex_new();
  complex =  GEDA_COMPLEX(new_obj);

  if (clib != NULL) {
    complex->filename = u_string_strdup (s_clib_symbol_get_name (clib));
  }
  else {
    complex->filename = u_string_strdup (basename);
  }

  /* get the symbol data */
  if (clib != NULL) {
    buffer = s_clib_symbol_get_data (clib);
  }

  if (clib == NULL || buffer == NULL) {
    new_obj->type = OBJ_PLACEHOLDER;
    return create_placeholder(toplevel, complex, x, y, angle, mirror);
  }
  else {

    GError * err = NULL;

    new_obj->selectable = selectable;

    complex->angle = angle;
    complex->mirror = mirror;
    complex->x = x;
    complex->y = y;

    /* add connections till translated */
    complex->prim_objs = o_read_buffer (toplevel, NULL, buffer, -1, complex->filename, &err);

    if (err) {

      g_error_free(err);
      /* If reading fails, change object to a placeholder type */
      new_obj->type = OBJ_PLACEHOLDER;
      return create_placeholder(toplevel, complex, x, y, angle, mirror);
    }
    else {

      /* Mirror, rotate and translate children */
      if (mirror) { /* children if required */
        for (iter = complex->prim_objs; iter != NULL; iter = iter->next) {
          Object *sub_object = iter->data;
          o_mirror_object (sub_object, 0, 0);
          o_rotate_object(sub_object, 0, 0, angle);
          o_translate_object(sub_object, x, y);
        }
      }
      else {
        for (iter = complex->prim_objs; iter != NULL; iter = iter->next) {
          Object *sub_object = iter->data;
          o_rotate_object(sub_object, 0, 0, angle);
          o_translate_object(sub_object, x, y);
        }
      }
    }

    GEDA_FREE (buffer);

  }

  complex->pin_objs = NULL;

  /* set the parent field now and check for pins */
  for (iter = complex->prim_objs; iter != NULL; iter = iter->next) {

    Object *sub_object = iter->data;

    sub_object->parent_object = new_obj;

    if (sub_object->type == OBJ_PIN) {
      complex->pin_objs = g_list_append(complex->pin_objs, sub_object);
    }
  }

  return new_obj;
}

/*! \brief Create a new embedded object
 *
 *  \par Function Description
 *  This function creates a new embedded object.
 *
 *  \param [in]  x         The x location of the complex object
 *  \param [in]  y         The y location of the complex object
 *  \param [in]  angle     The rotation angle
 *  \param [in]  mirror    The mirror status
 *  \param [in]  basename  The basic name the embedded was created of
 *  \param [in]  selectable whether the object can be selected with the mouse
 *
 *  \return a new complex object
 */
Object *o_complex_new_embedded(int x, int y, int angle, int mirror,
                               const char *basename, int selectable)
{
  Object  *new_obj;
  Complex *complex;

  new_obj = geda_complex_new();
  complex = GEDA_COMPLEX(new_obj);

  complex->x = x;
  complex->y = y;

  complex->angle = angle;
  complex->mirror = mirror;

  complex->filename = u_string_strdup(basename);

  complex->is_embedded = TRUE;

  new_obj->selectable = selectable;

  complex->prim_objs = NULL;

  /* don't have to translate/rotate/mirror here at all since the */
  /* object is in place */
  return new_obj;
}

/*! \brief Read Complex object from a char buffer
 *
 *  \par Function Description
 *   This function reads a complex object from the buffer \a buf.
 *   If the complex object was read successfully, a new object is
 *   allocated and appended to the \a object_list.
 *
 *  \param [in] toplevel       The GedaToplevel object
 *  \param [in] buf            Text buffer (usually a line of a schematic file)
 *  \param [in] release_ver    The release number gEDA
 *  \param [in] fileformat_ver An integer value of the file format
 *
 *  \param [out] err           A GError object
 *
 *  \return The object list, or NULL on error.
 */
Object *o_complex_read (GedaToplevel *toplevel,
                        const char buf[], unsigned int release_ver,
                        unsigned int fileformat_ver, GError **err)
{
  Object *new_obj;
  char type;
  int x1, y1;
  int angle;

  char *basename = GEDA_MEM_ALLOC (1 + strlen (buf));

  int selectable;
  int mirror;

  if (sscanf(buf, "%c %d %d %d %d %d %s\n",
    &type, &x1, &y1, &selectable, &angle, &mirror, basename) != 7) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse complex object"));
    return NULL;
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
      break;

    default:
      u_log_message(_("Found a component with an invalid rotation [ %c %d %d %d %d %d %s ]\n"), type, x1, y1, selectable, angle, mirror, basename);
      u_log_message (_("Setting angle to 0\n"));
      angle = 0;
  }

  switch(mirror) {

    case(0):
    case(1):

      break;

    default:
      u_log_message(_("Found a component with an invalid mirror flag [ %c %d %d %d %d %d %s ]\n"), type, x1, y1, selectable, angle, mirror, basename);
      u_log_message (_("Setting mirror to 0\n"));
      mirror = 0;
  }

  if (strncmp(basename, "EMBEDDED", 8) == 0) {

    new_obj = o_complex_new_embedded(x1, y1, angle, mirror, basename + 8,
                                     selectable);
  }
  else {

    const CLibSymbol *clib = s_clib_get_symbol_by_name (basename);

    new_obj = o_complex_new(toplevel, x1, y1, angle, mirror, clib, basename, selectable);

    /* Delete or hide attributes eligible for promotion inside the complex */
    if (new_obj) {
      o_complex_remove_promotable_attribs (toplevel, new_obj);
    }
  }

  GEDA_FREE (basename);

  return new_obj;
}

/*! \brief Create a string representation of the complex object
 *
 *  \par Function Description
 *  This function takes a complex \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a complex Object
 *
 *  \return the string representation of the complex Object
 */
char *o_complex_save(Object *object)
{
  int      selectable;
  char    *buf = NULL;
  char    *basename;
  Complex *complex;

  g_return_val_if_fail(GEDA_IS_COMPLEX(object), NULL);

  complex = GEDA_COMPLEX(object);

  basename = u_string_sprintf ("%s%s", complex->is_embedded ? "EMBEDDED" : "", complex->filename);

  selectable = (object->selectable) ? 1 : 0;

  /* Force the object type to be output as OBJ_COMPLEX for both
   * these object types. */
  buf = u_string_sprintf("%c %d %d %d %d %d %s", OBJ_COMPLEX,
                         complex->x, complex->y,
                         selectable, complex->angle,
                         complex->mirror, basename);
  GEDA_FREE (basename);

  return(buf);
}

/*! \brief Create a copy of a COMPLEX object
 *
 *  \par Function Description
 *  This function creates a copy of the complex object \a o_current.
 *
 *  \param [in] o_current    The object that is copied
 *
 *  \return a new COMPLEX object
 */
Object *o_complex_copy(Object *o_current)
{
  Object  *o_new;
  Complex *new_complex;
  Complex *old_complex;

  GList *iter;
  GList *pins = NULL;

  g_return_val_if_fail(GEDA_IS_COMPLEX(o_current), NULL);

  o_new       = geda_complex_new();
  new_complex = GEDA_COMPLEX(o_new);
  old_complex = GEDA_COMPLEX(o_current);

  new_complex->filename    = u_string_strdup(old_complex->filename);
  new_complex->is_embedded = old_complex->is_embedded;
  new_complex->x           = old_complex->x;
  new_complex->y           = old_complex->y;
  new_complex->angle       = old_complex->angle;
  new_complex->mirror      = old_complex->mirror;

  /* Copy contents and set the parent pointers on the copied objects. */
  new_complex->prim_objs = o_list_copy_all (old_complex->prim_objs, NULL);

  for (iter = new_complex->prim_objs; iter != NULL; NEXT (iter)) {

    Object *child = (Object*) iter->data;

    if (GEDA_IS_OBJECT(child)) {
      child->parent_object = o_new;
      if(GEDA_IS_PIN(child)) {
        pins = g_list_append(pins, child);
      }
    }
    else {
      BUG_MSG("Invalid pointer attached to complex");
    }
  }

  new_complex->pin_objs = pins;

  /* Recalculate bounds */
  o_new->w_bounds_valid_for = NULL;

  s_slot_update_object (o_new);

  return o_new;
}

/*! \brief Reset the refdes number back to a question mark
 *
 *  \par This function finds the refdes attribute inside this
 *  object and resets the refdes number back to a question mark.
 *
 *  \param [in] object      The complex containing text objects
 */
void o_complex_reset_refdes(Object *object)
{
  GList *iter = object->attribs;

  while (iter != NULL) {
    Object *attrib = (Object*) iter->data;

    if (attrib->type == OBJ_TEXT) {
      u_refdes_reset(attrib);
    }

    iter = iter->next;
  }
}

/*! \brief Mirror a Complex Object
 *
 *  \par Function Description
 *  This function mirrors a complex from the point
 *  (<B>center_x</B>,<B>center_y</B>) in world unit.
 *
 *  \param [in,out] object    Complex Object to mirror
 *  \param [in]     center_x  Origin x coordinate in WORLD units
 *  \param [in]     center_y  Origin y coordinate in WORLD units
 */
void o_complex_mirror(Object *object, int center_x, int center_y)
{
  int x, y;

  g_return_if_fail( GEDA_IS_COMPLEX(object) );

  x = 2 * center_x - object->complex->x;
  y = object->complex->y;

  o_complex_translate(object, -object->complex->x, -object->complex->y);

  o_list_mirror (object->complex->prim_objs, 0, 0);

  switch(object->complex->angle) {
    case(90):
      object->complex->angle = 270;
      break;

    case(270):
      object->complex->angle = 90;
      break;
  }

  object->complex->mirror = !object->complex->mirror;

  o_complex_translate(object, x, y);
}

/*! \brief Rotates a complex object in world coordinates
 *
 *  \par Function Description
 *   This function rotates a complex \a object around the
 *   (\a center_x,\a center_y) point by \a angle degrees.
 *   The center of rotation is in world units.
 *
 *  \param [in,out] object    Complex object to rotate
 *  \param [in]     center_x  X coordinate of rotation center (world coords)
 *  \param [in]     center_y  Y coordinate of rotation center (world coords)
 *  \param [in]     angle     Rotation angle in degrees
 */
void o_complex_rotate(Object *object, int center_x, int center_y, int angle)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (GEDA_IS_COMPLEX(object));

  x = object->complex->x + (-center_x);
  y = object->complex->y + (-center_y);

  m_rotate_point_90(x, y, angle, &newx, &newy);

  x = newx + (center_x);
  y = newy + (center_y);

  o_complex_translate(object, -object->complex->x, -object->complex->y);

  o_list_rotate (object->complex->prim_objs, 0, 0, angle);

  object->complex->x = 0;
  object->complex->y = 0;

  o_complex_translate(object, x, y);

  object->complex->angle = (object->complex->angle + angle ) % 360;
}

/*! \brief Translate a complex object
 *
 *  \par Function Description
 *  This function changes the position of a complex \a object.
 *
 *  \param [in,out] object  The complex Object to be translated
 *  \param [in]     dx      The x-distance to move the object
 *  \param [in]     dy      The y-distance to move the object
 */
void o_complex_translate(Object *object, int dx, int dy)
{
  g_return_if_fail (GEDA_IS_COMPLEX(object));

  object->complex->x = object->complex->x + dx;
  object->complex->y = object->complex->y + dy;

  o_list_translate (object->complex->prim_objs, dx, dy);

  object->w_bounds_valid_for = NULL;
}

/*! \brief Find a pin with a particular attribute
 *
 *  \par Function Description
 *  Search for a pin inside the given complex which has an attribute
 *  matching those passed.
 *
 *  \param [in] object  Complex Object to search
 *  \param [in] name    The attribute name to search for
 *  \param [in] wanted  The attribute value to search for
 *
 *  \return The pin Object with the given attribute, NULL otherwise.
 */
Object*
o_complex_find_pin_by_attribute (Object *object, char *name, char *wanted)
{
  GList *list;
  GList *iter;
  char  *value;
  int    found;

  g_return_val_if_fail (GEDA_IS_COMPLEX(object), NULL);

  list = object->complex->prim_objs;

  for (iter = list; iter != NULL; iter = iter->next) {

    Object *o_current = iter->data;

    if (o_current->type != OBJ_PIN)
      continue;

    value = o_attrib_search_object_attribs_by_name (o_current, name, 0);
    found = (value != NULL && strcmp (value, wanted) == 0);
    GEDA_FREE (value);

    if (found)
      return o_current;
  }

  return NULL;
}

/*! \brief check the symversion of a complex object
 *
 *  \par Function Description
 *  This function compares the symversion of a symbol with it's
 *  earlier saved symversion in a schematic.
 *  Major symversion changes are added to the toplevel object
 *  (toplevel->major_changed_refdes), minor changes are reported
 *  to the messaging system.
 *
 *  \param [in] toplevel  Optional pointer to GedaToplevel toplevel
 *  \param [in] object    The complex Object
 */
void o_complex_check_symversion(GedaToplevel *toplevel, Object* object)
{
  char *inside         = NULL;
  char *outside        = NULL;
  double inside_value  = -1.0;
  double outside_value = -1.0;
  char *err_check      = NULL;
  int inside_present   = FALSE;
  int outside_present  = FALSE;

  double inside_major, inside_minor;
  double outside_major, outside_minor;

  g_return_if_fail (GEDA_IS_COMPLEX(object));

  const char *refdes;
  const char *match;
  const char *newer;
  const char *oddity;
  const char *older;
  const char *parse;

  const char *clash_msg;
  const char *major_msg;
  const char *minor_msg;
  const char *parse_msg;
  const char *warn_msg;

  match     = _("mismatch");
  newer     = _("newer");
  oddity    = _("oddity");
  older     = _("older");
  parse     = _("parse error");

  clash_msg = _("\tInstantiated symbol <%s> is %s than the version in library\n");
  major_msg = _("\tMAJOR VERSION CHANGE (instantiated %.3f, library %.3f, %s)!\n");
  minor_msg = _("\tMinor version change (instantiated %.3f, library %.3f)\n");
  parse_msg = _("\tCould not parse symbol file\n");
  warn_msg  = _("WARNING: Symbol version %s on refdes %s:\n");

  /* first look on the inside for the symversion= attribute */
  inside = o_attrib_search_inherited_attribs_by_name (object, "symversion", 0);

  /* now look for the symversion= attached to object */
  outside = o_attrib_search_attached_attribs_by_name (object, "symversion", 0);

  /* get the uref for future use */
  refdes = o_get_object_attrib_value(object, "refdes");

  if (!refdes) {
    refdes = "unknown";
  }

  if (inside) {

    inside_value = strtod(inside, &err_check);

    if (inside_value == 0 && inside == err_check) {

      u_log_message(warn_msg, parse, refdes);

      if (inside) {

        u_log_message("\t%s symversion=%s\n", parse_msg, inside);
      }
      else {
        u_log_message("\t%s\n",parse_msg);
      }

      goto done;
    }
    inside_present = TRUE;
  }
  else {
    inside_present = FALSE;  /* attribute not inside */
  }

  if (outside) {

    outside_value = strtod(outside, &err_check);

    if (outside_value == 0 && outside == err_check) {

      u_log_message(warn_msg, parse, refdes);
      u_log_message(_("\tCould not parse attached symversion=%s\n"), outside);
      goto done;

    }
    outside_present = TRUE;
  }
  else {
    outside_present = FALSE;  /* attribute not outside */
  }

#if DEBUG
  printf("%s:\n\tinside: %.1f outside: %.1f\n\n", object->name,
         inside_value, outside_value);
#endif

  /* if neither symbol nor library have version then skip */
  if (inside_present || outside_present) {

    /* No symversion inside, but a version is outside, is a weird case */
    if (!inside_present && outside_present) {

      u_log_message(warn_msg, oddity, refdes);
      u_log_message(_("\tsymversion=%s attached to instantiated symbol,"
      " but version not found inside symbol file\n"), outside);

    }
    else {

      /* inside & not outside is a valid case, means symbol in library is */
      /* newer also if inside_value is greater than outside_value, than the */
      /* symbol in library is newer */
      if ((inside_present && !outside_present) ||
         ((inside_present && outside_present) &&
          (inside_value > outside_value)))
      {
        u_log_message(warn_msg, match, refdes);
        u_log_message(clash_msg, object->complex->filename, older);

        /* break up the version values into major.minor numbers */
        inside_major = floor(inside_value);
        inside_minor = inside_value - inside_major;

        if (outside_present) {
          outside_major = floor(outside_value);
          outside_minor = outside_value - outside_major;
        }
        else {
          /* symversion was not attached to the symbol, set all to zero */
          outside_major = 0.0;
          outside_minor = 0.0;
          outside_value = 0.0;
        }

#if DEBUG
        printf("i: %f %f %f\n", inside_value, inside_major, inside_minor);
        printf("o: %f %f %f\n", outside_value, outside_major, outside_minor);
#endif

        if (inside_major > outside_major) {

          Page *page;
          char *refdes_copy;

          u_log_message (major_msg, outside_value, inside_value, refdes);

          if (GEDA_IS_PAGE(object->page)) {
            page = object->page;
          }
          else if (GEDA_IS_PAGE(toplevel->page_current)) {
            page = toplevel->page_current;
          }
          else {
            page = NULL;
          }

          if (page) {
            /* Add the refdes to the major_changed_refdes GList */
            /* if a page was found */
            refdes_copy = u_string_concat (refdes, " (",
                                                  object->complex->filename,
                                                  ")", NULL);
            page->major_changed_refdes =
            g_list_append(page->major_changed_refdes, refdes_copy);
          }

          /* don't bother checking minor changes if there are major ones*/
        }
        else if (inside_minor > outside_minor) {
          u_log_message (minor_msg,  outside_value, inside_value);
        }
      }
      else {
        /* outside value is greater than inside value, this is weird case */
        if ((inside_present && outside_present) && (outside_value > inside_value))
        {
          u_log_message(warn_msg, oddity, refdes);
          u_log_message(clash_msg, object->complex->filename, newer);
        }
      }
    }
  }

done:

  GEDA_FREE(inside);
  GEDA_FREE(outside);
}

/*! \brief Get Shortest distance from Complex to point
 *
 *  \par Function Description
 *   Calculates the distance between the given point and the closest
 *   point on an object within the complex object.
 *
 *  \note When querying the distance to our child objects, we always
 *        force treating them as solid filled.
 *        We ignore the force_solid argument to this function.
 *
 *  \param [in] object       A complex  Object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *
 *  \return The shortest distance from the object to the point. If the
 *          distance cannot be calculated, this function returns a really
 *          large number (G_MAXDOUBLE).  With an invalid parameter, this
 *          function returns G_MAXDOUBLE.
 */
double o_complex_shortest_distance(Object *object, int x, int y, int force_solid)
{
  Box    line_bounds;
  GList *iter;
  double distance;
  double shortest_distance = G_MAXDOUBLE;
  int    found_line_bounds = 0;

  g_return_val_if_fail (GEDA_IS_COMPLEX(object), G_MAXDOUBLE);

  for (iter = object->complex->prim_objs; iter != NULL; NEXT(iter)) {

    Object *obj = iter->data;
    int left, top, right, bottom;

    /* Collect the bounds of any lines and arcs in the symbol */
    if ((obj->type == OBJ_LINE || obj->type == OBJ_ARC) &&
         o_get_bounds(obj, &left, &top, &right, &bottom))
    {
      if (found_line_bounds) {
        line_bounds.lower_x = min (line_bounds.lower_x, left);
        line_bounds.lower_y = min (line_bounds.lower_y, top);
        line_bounds.upper_x = max (line_bounds.upper_x, right);
        line_bounds.upper_y = max (line_bounds.upper_y, bottom);
      }
      else {
        line_bounds.lower_x = left;
        line_bounds.lower_y = top;
        line_bounds.upper_x = right;
        line_bounds.upper_y = bottom;
        found_line_bounds = 1;
      }
    }
    else {
      distance = o_get_shortest_distance_full (obj, x, y, TRUE);
      shortest_distance = min (shortest_distance, distance);
    }

    if (shortest_distance == 0.0)
      return shortest_distance;
  }

  if (found_line_bounds) {
    distance = m_box_shortest_distance (&line_bounds, x, y, TRUE);
    shortest_distance = min (shortest_distance, distance);
  }

  return shortest_distance;
}
