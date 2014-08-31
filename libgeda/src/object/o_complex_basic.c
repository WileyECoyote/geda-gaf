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

/*! \brief Return the bounds of the given object
 *
 *  \par Given an object, calculate the bounds coordinates.
 *
 *  \param [in] o_current The object to look the bounds for.
 *  \param [out] rleft   pointer to the left coordinate of the object.
 *  \param [out] rtop    pointer to the top coordinate of the object.
 *  \param [out] rright  pointer to the right coordinate of the object.
 *  \param [out] rbottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the object
 *  \retval 0 No bound was found
 *  \retval 1 Bound was found
 */
int world_get_single_object_bounds(Object *o_current, int *rleft, int *rtop,
                                   int *rright, int *rbottom)
{
  int result = 0;

  if (GEDA_IS_OBJECT(o_current)) {

    result = geda_object_bounds(o_current);

    if (result) {

      *rleft   = o_current->left;
      *rtop    = o_current->top;
      *rright  = o_current->right;
      *rbottom = o_current->bottom;
    }
  }
  else {
    BUG_MSG("Invalid argument, is not a GedaObject");
  }
  return result;
}

/*! \brief Return the bounds of the given GList of objects.
 *
 *  \par Given a list of objects, calculates the bounds coordinates.
 *
 *  \param [in]  list   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the list of objects
 *  \retval 0 No bounds were found
 *  \retval 1 Bound was found
 */

int world_get_object_glist_bounds(const GList *list, int *left, int *top, int *right, int *bottom)
{
  const GList *s_current;
  Object      *o_current;

  int rleft   = 0;
  int rtop    = 0;
  int rright  = 0;
  int rbottom = 0;
  int found   = 0;

  s_current = g_list_first((GList *)list);

  /* Find the first object with bounds, and set the bounds variables, then expand as necessary */
  while ( s_current != NULL ) {

    o_current = GEDA_OBJECT(s_current->data);

    if (GEDA_IS_OBJECT(o_current)) {
      if ( world_get_single_object_bounds(o_current, &rleft, &rtop, &rright, &rbottom) ) {
        if ( found ) {
          *left   = min( *left, rleft );
          *top    = min( *top, rtop );
          *right  = max( *right, rright );
          *bottom = max( *bottom, rbottom );
        }
        else {
          *left   = rleft;
          *top    = rtop;
          *right  = rright;
          *bottom = rbottom;
          found   = 1;
        }
      }
    }
    else {
      BUG_MSG("oops, world_get_object_glist_bounds found bad object");
      break;
    }
    NEXT(s_current);
  }

  return found;
}

/*! \brief Queries the bounds of a complex object
 *
 *  \par Function Description
 *  This function returns the bounding box of the complex object
 *  <B>object</B>.
 *
 *  \param [in]  object   The complex object.
 */
int
world_get_complex_bounds(Object *object)
{
  g_return_val_if_fail (GEDA_IS_COMPLEX(object), FALSE);
  return world_get_object_glist_bounds (object->complex->prim_objs,
                                       &object->left, &object->top,
                                       &object->right, &object->bottom);

}

/*! \brief get the position of the complex base point
 *  \par Function Description
 *  This function gets the position of the base point of a complex object.
 *
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
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
 *  \par Function Description
 *  This function checks if an object should be promoted.
 *  An attribute object is promotable if it is promoted by default, or the user
 *  has configered it to promote an attribute.
 *
 *  \param [in] toplevel  The GedaToplevel object
 *  \param [in] object    The attribute object to check
 *  \return TRUE if the object is a eligible attribute, FALSE otherwise
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

/*! \brief get the embedded state of an complex object
 *  \par Function Description
 *  Checks and returns the status of the complex object.
 *
 *  \param object  The object to check
 *  \return 1 if embedded, 0 otherwise
 */
int o_complex_is_embedded(Object *object)
{

  g_return_val_if_fail(GEDA_IS_COMPLEX(object), 0);

  return object->complex->is_embedded;

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
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The complex object being modified.
 *  \param [in]  detach   Should the attributes be detached?
 *  \returns              A linked list of Objects to promote.
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

  for (iter = attribs; iter != NULL; iter = g_list_next (iter)) {

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
 *  \par Function Description
 *  Selects promotable attributes from \a object, and returns a new
 *  GList containing them (suitable for appending to a #Page).
 *
 *  \param [in]  toplevel The #GedaToplevel environment.
 *  \param [in]  object   The complex #Object to promote from.
 *
 *  \return A GList of promoted attributes.
 */
GList *o_complex_promote_attribs (GedaToplevel *toplevel, Object *object)
{
  GList *promoted = NULL;
  GList *promotable = NULL;
  GList *iter = NULL;

  promotable = o_complex_get_promotable (toplevel, object, FALSE);

  /* Run through the attributes deciding if we want to keep them (in
   * which case we copy them and make them invisible) or if we want to
   * remove them. */
  if (toplevel->keep_invisible) {
    for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
      Object *o_kept = (Object *) iter->data;
      Object *o_copy = o_object_copy (o_kept);
      o_set_visibility (o_kept, INVISIBLE);
      o_copy->parent_object = NULL;
      promoted = g_list_prepend (promoted, o_copy);
    }
    promoted = g_list_reverse (promoted);
  }
  else {
    for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
      Object *o_removed = (Object *) iter->data;
      o_removed->parent_object = NULL;
      object->complex->prim_objs =
        g_list_remove (object->complex->prim_objs, o_removed);
    }
    promoted = promotable;
    /* Invalidate the object's bounds since we may have
     * stolen objects from inside it. */
    o_bounds_invalidate (object);
  }

  /* Attach promoted attributes to the original complex object */
  o_attrib_attach_list (promoted, object, TRUE);

  g_list_free (promotable);

  return promoted;
}


/*! \brief Delete or hide promotable from the passed Object
 *
 *  \par Function Description
 *  Deletes or hides promotable attributes from the passed Object.
 *  This is used when loading symbols while loading a schematic from
 *  disk. The schematic will already contain local copies of symbol's
 *  promotable objects, so we delete or hide the symbol's copies.
 *
 *  Deletion / hiding is dependant on the setting of
 *  toplevel->keep_invisible. If true, attributes eligible for
 *  promotion are kept in memory but flagged as invisible.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The complex object being altered.
 */
static void o_complex_remove_promotable_attribs (GedaToplevel *toplevel, Object *object)
{
  GList *promotable, *iter;

  promotable = o_complex_get_promotable (toplevel, object, FALSE);

  if (promotable == NULL)
    return;

  for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
    Object *a_object = iter->data;
    if (toplevel->keep_invisible == TRUE) {   /* Hide promotable attributes */
      o_set_visibility (a_object, INVISIBLE);
    }
    else {                                /* Delete promotable attributes */
      object->complex->prim_objs =
        g_list_remove (object->complex->prim_objs, a_object);
      s_object_release (a_object);
    }
  }

  o_bounds_invalidate (object);
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
  not_found_text = g_strdup_printf (_("Component not found:\n %s"), complex->filename);

  new_prim_obj = o_text_new(DETACHED_ATTRIBUTE_COLOR,
                            x + NOT_FOUND_TEXT_X,
                            y + NOT_FOUND_TEXT_Y, LOWER_LEFT, 0,
                            not_found_text, 8,
                            VISIBLE, SHOW_NAME_VALUE);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);
  GEDA_FREE(not_found_text);

  /* figure out where to put the hazard triangle, we could only be here during
   * a read failure, there is not page so we can not use normal bounds routines
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
                            LOWER_LEFT, 0, "!", 18,
                            VISIBLE, SHOW_NAME_VALUE);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  complex->prim_objs = g_list_reverse(complex->prim_objs);

  return (Object*)complex;
}

/* Done */
/*! \brief
 *  \par Function Description
 *
 */
Object *
o_complex_new(GedaToplevel *toplevel, int x, int y, int angle,
              int mirror, const CLibSymbol *clib, const char *basename,
              int selectable)
{
  Object  *new_obj;
  Complex *complex;

  GList *iter;
  char *buffer = NULL;

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

      if (mirror) {

        o_glist_mirror_world (0, 0, complex->prim_objs);
      }
      o_glist_rotate_world (0, 0, angle, complex->prim_objs);
      o_glist_translate_world (x, y, complex->prim_objs);
    }

    GEDA_FREE (buffer);

  }

  complex->pin_objs = NULL;
  /* set the parent field now and check for pins */
  for (iter = complex->prim_objs; iter != NULL; iter = g_list_next (iter)) {
    Object *sub_object = iter->data;
    sub_object->parent_object = new_obj;
    if (sub_object->type == OBJ_PIN) {
      complex->pin_objs = g_list_append(complex->pin_objs, sub_object);
    }
  }

  return new_obj;
}

/*! \brief create a new embedded object
 *  \par Function Description
 *  This function creates a new embedded object.
 *
 *  \param [in]  x         The x location of the complex object
 *  \param [in]  y         The y location of the complex object
 *  \param [in]  angle     The rotation angle
 *  \param [in]  mirror    The mirror status
 *  \param [in]  basename  The basic name the embedded was created of
 *  \param [in]  selectable whether the object can be selected with the mouse
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

/*! \brief read a complex object from a char buffer
 *  \par Function Description
 *  This function reads a complex object from the buffer \a buf.
 *  If the complex object was read successfully, a new object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] toplevel     The GedaToplevel object
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
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
 *  \par Function Description
 *  This function takes a complex \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a complex Object
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

  basename = g_strdup_printf ("%s%s", complex->is_embedded ? "EMBEDDED" : "", complex->filename);

  selectable = (object->selectable) ? 1 : 0;

  /* We force the object type to be output as OBJ_COMPLEX for both
   * these object types. */
  buf = g_strdup_printf("%c %d %d %d %d %d %s", OBJ_COMPLEX,
                        complex->x, complex->y,
                        selectable, complex->angle,
                        complex->mirror, basename);
  GEDA_FREE (basename);

  return(buf);
}

/*! \brief move a complex object
 *  \par Function Description
 *  This function changes the position of a complex \a object.
 *
 *  \param [in] dx           The x-distance to move the object
 *  \param [in] dy           The y-distance to move the object
 *  \param [in] object       The complex Object to be moved
 */
void
o_complex_translate_world(int dx, int dy, Object *object)
{
  g_return_if_fail (GEDA_IS_COMPLEX(object));

  object->complex->x = object->complex->x + dx;
  object->complex->y = object->complex->y + dy;

  o_glist_translate_world (dx, dy, object->complex->prim_objs);

  object->w_bounds_valid_for = NULL;
}

/*! \brief Create a copy of a COMPLEX object
 *  \par Function Description
 *  This function creates a copy of the complex object \a o_current.
 *
 *  \param [in] o_current    The object that is copied
 *  \return a new COMPLEX object
 */
Object *o_complex_copy(Object *o_current)
{
  Object *o_new;
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
  new_complex->prim_objs = o_glist_copy_all (old_complex->prim_objs, NULL);

  for (iter = new_complex->prim_objs; iter != NULL; NEXT (iter)) {
    Object *child = (Object*) iter->data;
    if (GEDA_IS_OBJECT(child)) {
      child->parent_object = o_new;
      if(GEDA_IS_PIN(child))
        pins = g_list_append(pins, child);
    }
    else {
      BUG_MSG("Invalid pointer attached to complex");
    }
  }

  new_complex->pin_objs = pins;

  /* Recalculate bounds */
  o_new->w_bounds_valid_for = NULL;

  /* Delete or hide attributes eligible for promotion inside the complex */
  //o_complex_remove_promotable_attribs (toplevel, o_new);

  s_slot_update_object (o_new);

  /* deal with stuff that has changed */

  /* here you need to create a list of attributes which need to be
   * connected to the new list, probably make an attribute list and
   * fill it with sid's of the attributes */

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

    iter = g_list_next (iter);
  }
}

/*! \brief Rotates a complex object in world coordinates
 * \par Function Description
 * This function rotates a complex \a object around the
 * (\a centerx,\a centery) point by \a angle degrees.
 * The center of rotation is in world units.
 *
 * \param [in] centerx  X coordinate of rotation center (world coords).
 * \param [in] centery  Y coordinate of rotation center (world coords).
 * \param [in] angle    Rotation angle in degrees.
 *
 * \param [in,out] object Complex object to rotate.
 *
 */
void
o_complex_rotate_world(int centerx, int centery, int angle, Object *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (object!=NULL);
  g_return_if_fail (GEDA_IS_COMPLEX(object));

  x = object->complex->x + (-centerx);
  y = object->complex->y + (-centery);

  m_rotate_point_90(x, y, angle, &newx, &newy);

  x = newx + (centerx);
  y = newy + (centery);

  o_complex_translate_world(-object->complex->x, -object->complex->y, object);

  o_glist_rotate_world (0, 0, angle, object->complex->prim_objs);

  object->complex->x = 0;
  object->complex->y = 0;

  o_complex_translate_world(x, y, object);

  object->complex->angle = (object->complex->angle + angle ) % 360;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_complex_mirror_world(int world_centerx, int world_centery, Object *object)
{
  int x, y;

  g_return_if_fail( object != NULL );
  g_return_if_fail( GEDA_IS_COMPLEX(object) );

  x = 2 * world_centerx - object->complex->x;
  y = object->complex->y;

  o_complex_translate_world(-object->complex->x, -object->complex->y, object);

  o_glist_mirror_world (0, 0, object->complex->prim_objs);

  switch(object->complex->angle) {
    case(90):
      object->complex->angle = 270;
      break;

    case(270):
      object->complex->angle = 90;
      break;
  }

  object->complex->mirror = !object->complex->mirror;

  o_complex_translate_world(x, y, object);
}


/*! \brief Find a pin with a particular attribute.
 *  \par Function Description
 *  Search for a pin inside the given complex which has an attribute
 *  matching those passed.
 *
 *  \param [in] object        complex Object whos pins to search.
 *  \param [in] name          the attribute name to search for.
 *  \param [in] wanted_value  the attribute value to search for.
 *  \return The pin Object with the given attribute, NULL otherwise.
 */
Object *o_complex_find_pin_by_attribute (Object *object, char *name, char *wanted_value)
{
  GList *iter;
  Object *o_current;
  char *value;
  int found;

  g_return_val_if_fail (GEDA_IS_COMPLEX(object), NULL);

  for (iter = object->complex->prim_objs; iter != NULL;
       iter = g_list_next (iter)) {
    o_current = iter->data;

    if (o_current->type != OBJ_PIN)
      continue;

    value = o_attrib_search_object_attribs_by_name (o_current, name, 0);
    found = (value != NULL && strcmp (value, wanted_value) == 0);
    GEDA_FREE (value);

    if (found)
      return o_current;
  }

  return NULL;
}


/*! \brief check the symversion of a complex object
 *  \par Function Description
 *  This function compares the symversion of a symbol with it's
 *  earlier saved symversion in a schematic.
 *  Major symversion changes are added to the toplevel object
 *  (toplevel->major_changed_refdes), minor changes are reported
 *  to the messaging system.
 *
 *  \param object    The complex Object
 */
void
o_complex_check_symversion(Object* object)
{
  char *inside = NULL;
  char *outside = NULL;
  char *refdes = NULL;
  double inside_value = -1.0;
  double outside_value = -1.0;
  char *err_check = NULL;
  int inside_present = FALSE;
  int outside_present = FALSE;
  double inside_major, inside_minor;
  double outside_major, outside_minor;

  g_return_if_fail (GEDA_IS_COMPLEX(object));

  /* first look on the inside for the symversion= attribute */
  inside = o_attrib_search_inherited_attribs_by_name (object, "symversion", 0);

  /* now look for the symversion= attached to object */
  outside = o_attrib_search_attached_attribs_by_name (object, "symversion", 0);

  /* get the uref for future use */
  refdes = o_attrib_search_object_attribs_by_name(object, "refdes", 0);
  if (!refdes)
  {
    refdes = u_string_strdup ("unknown");
  }

  if (inside)
  {
    inside_value = strtod(inside, &err_check);
    if (inside_value == 0 && inside == err_check)
    {
      if (inside)
      {
        u_log_message(_("WARNING: Symbol version parse error on refdes %s:\n"
                        "\tCould not parse symbol file symversion=%s\n"),
                      refdes, inside);
      } else {
        u_log_message(_("WARNING: Symbol version parse error on refdes %s:\n"
                        "\tCould not parse symbol file symversion=\n"),
                      refdes);
      }
      goto done;
    }
    inside_present = TRUE;
  }
  else {
    inside_present = FALSE;  /* attribute not inside */
  }

  if (outside)
  {
    outside_value = strtod(outside, &err_check);
    if (outside_value == 0 && outside == err_check)
    {
      u_log_message(_("WARNING: Symbol version parse error on refdes %s:\n"
                      "\tCould not parse attached symversion=%s\n"),
                    refdes, outside);
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

  /* symversion= is not present anywhere */
  if (!inside_present && !outside_present)
  {
    /* symbol is legacy and versioned okay */
    goto done;
  }

  /* No symversion inside, but a version is outside, this is a weird case */
  if (!inside_present && outside_present)
  {
    u_log_message(_("WARNING: Symbol version oddity on refdes %s:\n"
                    "\tsymversion=%s attached to instantiated symbol, "
                    "but no symversion= inside symbol file\n"),
                  refdes, outside);
    goto done;
  }

  /* inside & not outside is a valid case, means symbol in library is newer */
  /* also if inside_value is greater than outside_value, then symbol in */
  /* library is newer */
  if ((inside_present && !outside_present) ||
      ((inside_present && outside_present) && (inside_value > outside_value)))
  {

    u_log_message(_("WARNING: Symbol version mismatch on refdes %s (%s):\n"
                    "\tSymbol in library is newer than "
                    "instantiated symbol\n"),
                  refdes, object->complex->filename);

    /* break up the version values into major.minor numbers */
    inside_major = floor(inside_value);
    inside_minor = inside_value - inside_major;

    if (outside_present)
    {
      outside_major = floor(outside_value);
      outside_minor = outside_value - outside_major;
    } else {
      /* symversion was not attached to the symbol, set all to zero */
      outside_major = 0.0;
      outside_minor = 0.0;
      outside_value = 0.0;
    }

#if DEBUG
    printf("i: %f %f %f\n", inside_value, inside_major, inside_minor);
    printf("o: %f %f %f\n", outside_value, outside_major, outside_minor);
#endif

    if (inside_major > outside_major)
    {
      Page * page;
      g_return_if_fail(GEDA_IS_PAGE(object->page));
      page = object->page;

      char* refdes_copy;
      u_log_message(_("\tMAJOR VERSION CHANGE (file %.3f, "
                      "instantiated %.3f, %s)!\n"),
                    inside_value, outside_value, refdes);

      /* add the refdes to the major_changed_refdes GList */
      /* make sure refdes_copy is freed somewhere */
      refdes_copy = g_strconcat (refdes, " (",
                                 object->complex->filename,
                                 ")", NULL);
      page->major_changed_refdes =
        g_list_append(page->major_changed_refdes, refdes_copy);

      /* don't bother checking minor changes if there are major ones*/
      goto done;
    }

    if (inside_minor > outside_minor)
    {
      u_log_message(_("\tMinor version change (file %.3f, "
                      "instantiated %.3f)\n"),
                    inside_value, outside_value);
    }

    goto done;
  }

  /* outside value is greater than inside value, this is weird case */
  if ((inside_present && outside_present) && (outside_value > inside_value))
  {
    u_log_message(_("WARNING: Symbol version oddity on refdes %s:\n"
                    "\tInstantiated symbol is newer than "
                    "symbol in library\n"),
                  refdes);
    goto done;
  }

  /* if inside_value and outside_value match, then symbol versions are okay */

done:
  GEDA_FREE(inside);
  GEDA_FREE(outside);
  GEDA_FREE(refdes);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on an object within the complex object.
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
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this function returns
 *  G_MAXDOUBLE.
 */
double
o_complex_shortest_distance (Object *object, int x, int y, int force_solid)
{
  double shortest_distance = G_MAXDOUBLE;
  double distance;
  int found_line_bounds = 0;
  Box line_bounds;
  GList *iter;

  g_return_val_if_fail (GEDA_IS_COMPLEX(object), G_MAXDOUBLE);

  for (iter = object->complex->prim_objs; iter != NULL; NEXT(iter))
  {
    Object *obj = iter->data;
    int left, top, right, bottom;

    /* Collect the bounds of any lines and arcs in the symbol */
    if ((obj->type == OBJ_LINE || obj->type == OBJ_ARC) &&
         world_get_single_object_bounds(obj, &left, &top, &right, &bottom))
    {
      if (found_line_bounds) {
        line_bounds.lower_x = min (line_bounds.lower_x, left);
        line_bounds.lower_y = min (line_bounds.lower_y, top);
        line_bounds.upper_x = max (line_bounds.upper_x, right);
        line_bounds.upper_y = max (line_bounds.upper_y, bottom);
      } else {
        line_bounds.lower_x = left;
        line_bounds.lower_y = top;
        line_bounds.upper_x = right;
        line_bounds.upper_y = bottom;
        found_line_bounds = 1;
      }
    } else {
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
