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

/*! \file o_complex_object.c
 *  \brief Functions for complex objects
 *
 *  GedaComplex objects are collections of primary objects.
 */

/** \defgroup geda-complex-object-proc GedaComplex Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaComplex Objects
 */

#include "../../../config.h"

#include <stdio.h>
#include <math.h>

#include <libgeda_priv.h>

static int
o_complex_is_eligible_attribute (GedaToplevel *toplevel, GedaObject *object);

static void
geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void geda_complex_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_COMPLEX);
}

/*!
 * \brief Create a copy of a COMPLEX object
 * \par Function Description
 *  This function creates a copy of the complex object \a o_current.
 *
 * \param [in] o_current    The object that is copied
 *
 * \return a new COMPLEX object
 */
GedaObject *geda_complex_object_copy(GedaObject *o_current)
{
  if (GEDA_IS_COMPLEX(o_current)) {

    GedaObject  *o_new;
    GedaComplex *new_complex;
    GedaComplex *old_complex;

    GList *iter;
    GList *pins = NULL;

    o_new       = geda_complex_new();
    new_complex = GEDA_COMPLEX(o_new);
    old_complex = GEDA_COMPLEX(o_current);

    new_complex->filename    = geda_strdup(old_complex->filename);
    new_complex->is_embedded = old_complex->is_embedded;
    new_complex->x           = old_complex->x;
    new_complex->y           = old_complex->y;
    new_complex->angle       = old_complex->angle;
    new_complex->mirror      = old_complex->mirror;

    /* Copy contents and set the parent pointers on the copied objects. */
    new_complex->prim_objs = geda_object_list_copy_all (old_complex->
                                                        prim_objs, NULL);

    for (iter = new_complex->prim_objs; iter != NULL; NEXT (iter)) {

      GedaObject *child = (GedaObject*)iter->data;

      if (GEDA_IS_OBJECT(child)) {
        child->parent_object = o_new;
        if (GEDA_IS_PIN(child)) {
          pins = g_list_append(pins, child);
        }
      }
      else {
        geda_object_error(__func__, child, GEDA_OBJECT_ALL);
      }
    }

    new_complex->pin_objs = pins;

    /* Recalculate bounds */
    o_new->bounds_valid = FALSE;

    geda_struct_slot_update_object (o_new);

    return o_new;
  }

  geda_complex_object_error(__func__, o_current);

  return NULL;
}

/*!
 * \brief check the symversion of a complex object
 * \par Function Description
 *  This function compares the symversion of a symbol with it's
 *  earlier saved symversion in a schematic.
 *  Major symversion changes are added to the toplevel object
 *  (toplevel->major_changed_refdes), minor changes are reported
 *  to the messaging system.
 *
 * \param [in] toplevel  Optional pointer to GedaToplevel toplevel
 * \param [in] object    The complex Object
 */
void geda_complex_object_check_symbol_version(GedaToplevel *toplevel,
                                              GedaObject   *object)
{
  char  *inside        = NULL;
  char  *outside       = NULL;
  double inside_value  = -1.0;
  double outside_value = -1.0;
  char  *err_check     = NULL;

  int inside_present   = FALSE;
  int outside_present  = FALSE;

  const char *schematic;
  const char *refdes;

  Page *page;

  void write_log_instantiated_msg (const char *str1) {

    const char *str2 = _("instantiated");
    const char *str3 = _("library");
    const char *str4 = _("check");

    geda_log_w ("\t%s (%s %.3f, %s %.3f, %s %s)!\n",
                str1, str2, outside_value, str3, inside_value, str4, refdes);
  }

  /*
   * "\t<schematic> Instantiated symbol <fname>
   *  is older/newer than the version in library\n" */
  void write_log_warn_clash (const char *str4) {

    const char *str1 = _("Instantiated symbol");
    const char *str2 = object->complex->filename;
    const char *str3 = _("is");
    const char *str5 = _("than the version in library");

    geda_log_w ("\t<%s> %s <%s> %s %s %s\n",
                schematic, str1, str2, str3, str4, str5);
  }

  void write_log_warn_msg (const char *str2) {

    const char *str1 = _("WARNING: Symbol version");
    const char *str3 = _("on refdes");

    geda_log_w ("%s %s %s %s:\n", str1, str2, str3, refdes);
  }

  if (!GEDA_IS_COMPLEX(object)) {
    geda_complex_object_error(__func__, object);
    return;
  }

  /* When loading multiple documents it is helpful to include the file name
   * when reporting symbol version conflicts, to do that, get the page: */
  if (GEDA_IS_PAGE(object->page)) {
    page = object->page;
  }
  else if (GEDA_IS_PAGE(toplevel->page_current)) {
    page = toplevel->page_current;
  }
  else {
    page = NULL;
  }

  /* Retrieve the file name from the page */
  if (page) {
    /* The path can clutter the message so just use the base name */
    schematic = geda_file_get_basename(page->filename); /* Do not free */
  }
  else {
    schematic = _("unknown file");
  }

  /* look for the symversion= attached to object */
  outside = geda_attrib_search_attached_by_name (object, "symversion", 0);

  /* Check is version checking is enabled for this refdes */
  if (outside && outside[0] == '-') {
    GEDA_FREE(outside);
    return;
  }

  /* look on the inside for the symversion= attribute */
  inside = geda_attrib_search_inherited_by_name (object, "symversion", 0);

  /* get the uref for future use */
  refdes = geda_object_get_attrib_value(object, "refdes");

  if (!refdes) {
    refdes = "unknown";
  }

  if (inside) {

    inside_value = strtod(inside, &err_check);

    if (inside_value == 0 && inside == err_check) {

      write_log_warn_msg(_("parse error"));

      const char *parse_msg = _("Could not parse symbol file");

      if (g_utf8_validate(inside, -1, NULL)) {
        geda_log_w ("\t%s\nsymversion=%s\n", parse_msg, inside);
      }
      else {
        geda_log_w ("\t%s\n",parse_msg);
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
      write_log_warn_msg(_("parse error"));
      geda_log_w ("\t%s symversion=%s\n", _("Could not parse attached"), outside);
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

      /* Do not report symversion oddity if is a placeholder */
      if (object->type != OBJ_PLACEHOLDER) {
        write_log_warn_msg(_("oddity"));
        geda_log_w ("\tsymversion=%s %s\n", outside,
                  _("attached to instantiated symbol,"
                    " but version not found inside symbol file"));
      }

    }
    else {

      /* inside & not outside is a valid case, means symbol in library is */
      /* newer also if inside_value is greater than outside_value, than the */
      /* symbol in library is newer */
      if ((inside_present && !outside_present) ||
         ((inside_present && outside_present) &&
          (inside_value > outside_value)))
      {

        double inside_major, inside_minor;
        double outside_major, outside_minor;

        write_log_warn_msg(_("mismatch"));
        write_log_warn_clash(_("older"));

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

          write_log_instantiated_msg (_("MAJOR VERSION CHANGE"));

          if (page) {

            char *refdes_copy;

            /* Add the refdes to the major_changed_refdes GList */
            /* if a page was found */
            refdes_copy = geda_utility_string_concat (refdes,
                                     " (", object->complex->filename, ")",
                                           NULL);
            page->major_changed_refdes =
            g_list_append(page->major_changed_refdes, refdes_copy);
          }

          /* don't bother checking minor changes if there are major ones*/
        }
        else if (inside_minor > outside_minor) {
          write_log_instantiated_msg (_("Minor version change"));
        }
      }
      else {
        /* outside value is greater than inside value, this is weird case */
        if ((inside_present && outside_present) && (outside_value > inside_value))
        {
          write_log_warn_msg(_("oddity"));
          write_log_warn_clash(_("newer"));
        }
      }
    }
  }

done:

  GEDA_FREE(inside);
  GEDA_FREE(outside);
}

static GedaObject *o_complex_create_placeholder(GedaToplevel *toplevel,
                                                GedaComplex  *complex,
                                                int x, int y, int angle,
                                                int mirror)
{
  GedaObject      *new_prim_obj;
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
  new_prim_obj = geda_line_object_new(DETACHED_ATTRIBUTE_COLOR, x - 50, y, x + 50, y);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = geda_line_object_new(DETACHED_ATTRIBUTE_COLOR, x, y + 50, x, y - 50);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  /* Add some useful text */
  not_found_text = geda_sprintf ("%s:\n %s", _("Component not found"), complex->filename);

  new_prim_obj = geda_text_object_new(DETACHED_ATTRIBUTE_COLOR,
                                      x + NOT_FOUND_TEXT_X,
                                      y + NOT_FOUND_TEXT_Y, LOWER_LEFT, 0,
                                      10, VISIBLE, SHOW_NAME_VALUE,
                                      not_found_text);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);
  GEDA_FREE(not_found_text);

  /* figure out where to put the hazard triangle, we could only be here during
   * a read failure, there is no page so we can not use normal bounds routines
   * instead we will try ...*/
  geda_toplevel_set_text_bounds (toplevel, new_prim_obj);

  x_offset = (right - left) / 4;
  y_offset = bottom - top + 100;  /* 100 is just an additional offset */

  /* add hazard triangle */
  new_prim_obj = geda_line_object_new(DETACHED_ATTRIBUTE_COLOR,
                                      x + NOT_FOUND_TEXT_X + x_offset,
                                      y + NOT_FOUND_TEXT_Y + y_offset,
                                      x + NOT_FOUND_TEXT_X + x_offset + 600,
                                      y + NOT_FOUND_TEXT_Y + y_offset);

  geda_set_object_line_options(new_prim_obj, &line_options);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = geda_line_object_new(DETACHED_ATTRIBUTE_COLOR,
                                      x + NOT_FOUND_TEXT_X + x_offset,
                                      y + NOT_FOUND_TEXT_Y + y_offset,
                                      x + NOT_FOUND_TEXT_X + x_offset + 300,
                                      y + NOT_FOUND_TEXT_Y + y_offset + 500);

  geda_set_object_line_options(new_prim_obj, &line_options);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = geda_line_object_new(DETACHED_ATTRIBUTE_COLOR,
                                      x + NOT_FOUND_TEXT_X + x_offset + 300,
                                      y + NOT_FOUND_TEXT_Y + y_offset + 500,
                                      x + NOT_FOUND_TEXT_X + x_offset + 600,
                                      y + NOT_FOUND_TEXT_Y + y_offset);

  geda_set_object_line_options(new_prim_obj, &line_options);

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  new_prim_obj = geda_text_object_new(DETACHED_ATTRIBUTE_COLOR,
                                      x + NOT_FOUND_TEXT_X + x_offset + 270,
                                      y + NOT_FOUND_TEXT_Y + y_offset + 90,
                                      LOWER_LEFT, 0, 18,
                                      VISIBLE, SHOW_NAME_VALUE,
                                      "!");

  complex->prim_objs = g_list_prepend (complex->prim_objs, new_prim_obj);

  complex->prim_objs = g_list_reverse(complex->prim_objs);

  return (GedaObject*)complex;
}

/*!
 * \brief Queries the bounds of a complex object
 * \par Function Description
 *  This function returns the bounding box of the complex <B>object</B>.
 *
 * \param [in]  object   The complex object.
 *
 * \see geda_object_get_bounds_list
 */
int geda_complex_object_get_bounds(GedaObject *object)
{
  if (GEDA_IS_COMPLEX(object)) {
    return geda_object_get_bounds_list (object->complex->prim_objs,
                                        &object->left, &object->top,
                                        &object->right, &object->bottom);
  }
  geda_complex_object_error(__func__, object);
  return FALSE;
}

/*!
 * \brief Get the file name Associated with a Complex GedaObject
 * \par Function Description
 *  Returns the filename associated with the GedaComplex \a object.
 *
 * \param object   The GedaComplex object to inspect
 *
 * \return the filename associated with \a object
 */
const char *geda_complex_object_get_filename (GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_COMPLEX(object), NULL);

  return object->complex->filename;
}

/*!
 * \brief Get Point on a GedaComplex Nearest a Given Point
 * \par Function Description
 *  Recursively calls geda_object_get_nearest_point on the closest
 *  sub-object of the complex and returns the results of the function
 *  corresponding to the appropriate type of object for the selected
 *  sub-object.
 *
 * \param [in]  object  Pointer to a GedaComplex object
 * \param [in]  x       Integer pointer
 * \param [in]  y       Integer pointer
 * \param [out] nx      Integer pointer
 * \param [out] ny      Integer pointer
 *
 * \returns TRUE is the results are valid or
 *          FALSE if \a object was not a GedaComplex.
 */
bool geda_complex_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny)
{
  bool result;
  GedaObject *closest = NULL;

  if (GEDA_IS_COMPLEX(object)) {

    GList *iter;
    double shortest = G_MAXDOUBLE;

    for (iter = object->complex->prim_objs; iter != NULL; NEXT(iter)) {

      GedaObject *obj = iter->data;

      bool do_check;

      do_check = obj->type == OBJ_LINE   ||
                 obj->type == OBJ_ARC    ||
                 obj->type == OBJ_BOX    ||
                 obj->type == OBJ_CIRCLE ||
                 obj->type == OBJ_PIN;

      if (do_check) {

        double distance;

        distance = geda_object_get_shortest_distance_full (obj, x, y, TRUE);

        if (distance < shortest) {
          shortest = distance;
          closest  = obj;
        }
      }

      if (shortest == 0.0) {
       *nx      = x;
       *ny      = y;
        closest = NULL;
        break;
      }
    }
  }
  else {
    geda_complex_object_error(__func__, object);
  }

  if (closest) {
    result = geda_object_get_nearest_point(closest, x, y, nx, ny);
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

/*!
 * \brief Get list of pin objects from a Complex object
 * \par Function Description
 *  Returns a GList of GedaPin objects associated with \a object.
 *
 * \param [in] object A GedaComplex object
 *
 * \returns GList of pins belonging to the complex, do not free the list.
 */
GList *geda_complex_object_get_pin_objs(GedaObject *object)
{
  if (!GEDA_IS_COMPLEX(object)) {
    geda_complex_object_error(__func__, object);
    return NULL;
  }
  return object->complex->pin_objs;
}

/*!
 * \brief Get the position of complex base point
 * \par Function Description
 *  This function returns the position of the base point of a complex object.
 *
 * \param [in] object  GedaComplex object whose position is to be returned
 * \param [out] x      Pointer to save the x-position,
 * \param [out] y      Pointer to save the y-position,
 *
 * \return TRUE if successfully determined the position, FALSE otherwise.
 */
bool geda_complex_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_COMPLEX(object)) {
    *x = object->complex->x;
    *y = object->complex->y;
    return TRUE;
  }

  geda_complex_object_error(__func__, object);

  return FALSE;
}

/*!
 * \brief Get list of objects from a Complex object
 * \par Function Description
 *  Returns a GList of GedaObjects associated with \a object.
 *
 * \param [in] object A GedaComplex object
 *
 * \returns GList of subobjects belonging to the complex.
 */
GList *geda_complex_object_get_prim_objs (GedaObject *object)
{
  if (!GEDA_IS_COMPLEX(object)) {
    geda_complex_object_error(__func__, object);
    return NULL;
  }

  return object->complex->prim_objs;
}

/*!
 * \brief Get attributes eligible for promotion from inside a complex
 * \par Function Description
 *  Returns a GList of Objects which are eligible for promotion from
 *  within the passed complex Object.
 *
 *  If detach is TRUE, the function removes these attribute objects
 *  from the prim_objs of the complex.  If detach is FALSE, the
 *  GedaObjects are left in place.
 *
 * \param [in] toplevel The toplevel environment,
 * \param [in] object   The complex object being modified,
 * \param [in] detach   Should the attributes be detached?
 *
 * \returns Linked list of Objects to promote.
 */
GList *geda_complex_object_get_promotable (GedaToplevel *toplevel,
                                           GedaObject   *object,
                                           int           detach)
{
  if (GEDA_IS_COMPLEX(object)) {

    GList  *promoted = NULL;
    GList  *attribs;
    GList  *iter;

    if (toplevel == NULL)
      return NULL;

    if (!toplevel->attribute_promotion) /* controlled through rc file */
      return NULL;

    attribs = geda_object_list_find_floating (object->complex->prim_objs);

    for (iter = attribs; iter != NULL; iter = iter->next) {

      GedaObject *ptr = iter->data;

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
  else {
    geda_complex_object_error(__func__, object);
  }

  return FALSE;
}

/*!
 * \brief Find a pin with a particular attribute
 * \par Function Description
 *  Search for a pin inside the given complex which has an attribute
 *  matching those passed.
 *
 * \param [in] object  Complex GedaObject to search
 * \param [in] name    The attribute name to search for
 * \param [in] wanted  The attribute value to search for
 *
 * \return The pin Object with the given attribute, NULL otherwise.
 */
GedaObject *geda_complex_object_find_pin_by_attribute (GedaObject *object, char *name, char *wanted)
{
  if (GEDA_IS_COMPLEX(object)) {

    GList *list;
    GList *iter;
    char  *value;
    int    found;

    list = object->complex->prim_objs;

    for (iter = list; iter != NULL; iter = iter->next) {

      GedaObject *o_current = iter->data;

      if (o_current->type != OBJ_PIN)
        continue;

      value = geda_attrib_search_object_by_name (o_current, name, 0);
      found = (value != NULL && strcmp (value, wanted) == 0);
      GEDA_FREE (value);

      if (found)
        return o_current;
    }
  }
  else {
    geda_complex_object_error(__func__, object);
  }

  return NULL;
}

/*!
 * \brief check whether an object is a attributes
 * \par Function Description
 *  This function checks if an object should be promoted. An attribute
 *  object is promotable if it is promoted by default, or the user has
 *  configered it to promote an attribute.
 *
 * \param [in] toplevel  The GedaToplevel object,
 * \param [in] object    The attribute object to check.
 *
 * \return TRUE if the object is a eligible attribute, FALSE otherwise.
 */
static int o_complex_is_eligible_attribute (GedaToplevel *toplevel, GedaObject *object)
{
  char *name = NULL;

  bool  answer = FALSE;

  if (GEDA_IS_TOPLEVEL(toplevel))  {

    if (GEDA_IS_TEXT(object))  {

      const char *symversion = "symversion=";
      int promotableAttribute = FALSE;

      if(object->text->string == NULL) {
        BUG_MSG("GedaText object string value = NULL");
      }
      else if (strncmp(object->text->string, symversion, 11) == 0) {
        /* always promote symversion= attribute, even if it is invisible */
        answer = TRUE;
      }
      else {

        /* check list against attributes which can be promoted */
        if (toplevel->always_promote_attributes != NULL) {

          if (geda_attrib_object_get_name_value (object, &name, NULL)) {
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
          if ((!geda_object_get_is_visible (object)) &&
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
      geda_object_error(__func__, object, GEDA_OBJECT_TEXT);
    }
  }
  else {
    BUG_MSG("Invalid pointer to Toplevel");
  }

  return answer;
}

/*!
 * \brief Get the embedded state of a GedaComplex object
 * \par Function Description
 *  Returns the status of the complex object.
 *
 * \param object  The object to check
 *
 * \return 1 if embedded, 0 otherwise
 */
int geda_complex_object_is_embedded(GedaObject *object)
{
  if (GEDA_IS_COMPLEX(object)) {
    return object->complex->is_embedded;
  }

  geda_complex_object_error(__func__, object);

  return 0;
}

/*!
 * \brief Mirror a GedaComplex GedaObject
 * \par Function Description
 *  This function mirrors a complex from the point
 *  (<B>center_x</B>,<B>center_y</B>) in world unit.
 *
 * \param [in,out] object    Complex GedaObject to mirror
 * \param [in]     center_x  Origin x coordinate
 * \param [in]     center_y  Origin y coordinate
 */
void geda_complex_object_mirror(GedaObject *object, int center_x, int center_y)
{
  if (GEDA_IS_COMPLEX(object)) {

    GedaComplex *complex = object->complex;

    int x, y;

    x = 2 * center_x - complex->x;
    y = complex->y;

    geda_complex_object_translate(object, -complex->x, -complex->y);

    geda_object_list_mirror (complex->prim_objs, 0, 0);

    switch(complex->angle) {
      case(90):
        complex->angle = 270;
        break;

      case(270):
        complex->angle = 90;
        break;
    }

    complex->mirror = !complex->mirror;

    geda_complex_object_translate(object, x, y);
  }
  else {
    geda_complex_object_error(__func__, object);
  }
}

void geda_complex_object_modify (GedaObject *object, int x, int y)
{
  if (GEDA_IS_COMPLEX(object)) {

    GedaComplex *complex = object->complex;

    complex->x = x;
    complex->y = y;
  }
  else {
    geda_complex_object_error(__func__, object);
  }
}

/*!
 * \brief Create a New GedaComplex GedaObject
 * \par Function Description
 *  Creates and initialize a new complex object.
 *
 * \return a new complex object
 */
GedaObject *geda_complex_object_new(GedaToplevel *toplevel,
                                    int x, int y,
                                    int angle, int mirror,
                                    const CLibSymbol *clib,
                                    const char *basename,
                                    int selectable)
{
  GedaObject  *new_obj;
  GedaComplex *complex;

  GList *iter;
  char  *buffer;

  buffer  = NULL;
  new_obj = geda_complex_new();
  complex = GEDA_COMPLEX(new_obj);

  if (clib != NULL) {
    complex->filename = geda_strdup (geda_struct_clib_symbol_get_name (clib));
  }
  else {
    complex->filename = geda_strdup (basename);
  }

  /* get the symbol data */
  if (clib != NULL) {
    buffer = geda_struct_clib_symbol_get_data (clib);
  }

  new_obj->selectable = selectable;

  complex->angle  = angle;
  complex->mirror = mirror;
  complex->x      = x;
  complex->y      = y;

  if (clib == NULL || buffer == NULL) {
    new_obj->type = OBJ_PLACEHOLDER;
    return o_complex_create_placeholder(toplevel, complex, x, y, angle, mirror);
  }
  else {

    GError *err = NULL;

    /* add connections till translated */
    complex->prim_objs = geda_object_read_buffer (toplevel,
                                                  NULL,
                                                  buffer,
                                                  -1,
                                                  complex->filename,
                                                  &err);

    if (err) {
      g_error_free(err);
      /* If reading fails, change object to a placeholder type */
      new_obj->type = OBJ_PLACEHOLDER;
      return o_complex_create_placeholder(toplevel, complex, x, y, angle, mirror);
    }
    else {

      /* Mirror, rotate and translate children */
      if (mirror) { /* children if required */
        for (iter = complex->prim_objs; iter != NULL; iter = iter->next) {
          GedaObject *sub_object = iter->data;
          geda_object_mirror (sub_object, 0, 0);
          geda_object_rotate(sub_object, 0, 0, angle);
          geda_object_translate(sub_object, x, y);
        }
      }
      else {
        for (iter = complex->prim_objs; iter != NULL; iter = iter->next) {
          GedaObject *sub_object = iter->data;
          geda_object_rotate(sub_object, 0, 0, angle);
          geda_object_translate(sub_object, x, y);
        }
      }
    }

    GEDA_FREE (buffer);

  }

  complex->pin_objs = NULL;

  /* set the parent field now and check for pins */
  for (iter = complex->prim_objs; iter != NULL; iter = iter->next) {

    GedaObject *sub_object = iter->data;

    sub_object->parent_object = new_obj;

    if (sub_object->type == OBJ_PIN) {
      complex->pin_objs = g_list_append(complex->pin_objs, sub_object);
    }
  }

  return new_obj;
}

/*!
 * \brief Create a new embedded object
 * \par Function Description
 *  This function creates a new embedded object.
 *
 * \param [in]  x         The x location of the complex object
 * \param [in]  y         The y location of the complex object
 * \param [in]  angle     The rotation angle
 * \param [in]  mirror    The mirror status
 * \param [in]  basename  The basic name the embedded was created of
 * \param [in]  selectable whether the object can be selected with the mouse
 *
 * \return a new complex object
 */
GedaObject *geda_complex_object_new_embedded(int x,
                                             int y,
                                             int angle,
                                             int mirror,
                                             const char *basename,
                                             int selectable)
{
  GedaObject  *new_obj;
  GedaComplex *complex;

  new_obj = geda_complex_new();
  complex = GEDA_COMPLEX(new_obj);

  complex->x = x;
  complex->y = y;

  complex->angle = angle;
  complex->mirror = mirror;

  complex->filename = geda_utility_string_strdup(basename);

  complex->is_embedded = TRUE;

  new_obj->selectable = selectable;

  complex->prim_objs = NULL;

  /* Don't have to translate/rotate/mirror here at all since
   * the object is in place */
  return new_obj;
}

/*!
 * \brief Promote attributes from a complex Object
 * \par Function Description
 *  Selects promotable attributes from \a object, and returns a new
 *  GList containing them (suitable for appending to a #Page).
 *
 * \param [in] toplevel The #GedaToplevel environment,
 * \param [in] object   The complex#GedaObject to promote from.
 *
 * \return A GList of promoted attributes.
 */
GList *geda_complex_object_promote_attribs (GedaToplevel *toplevel,
                                            GedaObject   *object)
{
  GList *promoted   = NULL;
  GList *promotable = NULL;
  GList *iter       = NULL;

  if (!GEDA_IS_COMPLEX(object)) {
    geda_complex_object_error(__func__, object);
    return NULL;
  }

  promotable = geda_complex_object_get_promotable (toplevel, object, FALSE);

  /* Run through the attributes deciding if we want to keep them (in
   * which case we copy them and make them invisible) or if we want
   * to remove them. */
  if (toplevel->keep_invisible) {

    for (iter = promotable; iter != NULL; iter = iter->next) {
      GedaObject *o_kept = (GedaObject *) iter->data;
      GedaObject *o_copy = geda_object_copy (o_kept);
      geda_set_object_visibility (o_kept, INVISIBLE);
      o_copy->parent_object = NULL;
      promoted = g_list_prepend (promoted, o_copy);
    }
    promoted = g_list_reverse (promoted);
  }
  else {

    for (iter = promotable; iter != NULL; iter = iter->next) {

      GList  *from_list = object->complex->prim_objs;
      GedaObject *o_removed = (GedaObject *) iter->data;

      o_removed->parent_object = NULL;
      object->complex->prim_objs = g_list_remove (from_list, o_removed);
    }

    promoted = promotable;
    /* Invalidate the object's bounds since we may have
     * stolen objects from inside it. */
    geda_set_object_bounds_invalid (object);
  }

  /* Attach promoted attributes to the original complex object */
  geda_attrib_object_attach_list (object, promoted, TRUE);

  g_list_free (promotable);

  return promoted;
}

/*!
 * \brief Delete or hide promotable attributes from the passed GedaObject
 * \par Function Description
 *  Deletes or hides promotable attributes from \a GedaObject. This is used
 *  when loading symbols while loading a schematic from disk. The schematic
 *  will already contain local copies of symbol's promotable objects, so we
 *  delete or hide the symbol's copies.
 *
 *  Deletion / hiding is dependant on the setting of
 *  toplevel->keep_invisible. If true, attributes eligible for
 *  promotion are kept in memory but flagged as invisible.
 *
 * \param [in] toplevel The toplevel environment,
 * \param [in] object   The complex object being altered.
 */
static void o_complex_remove_promotable (GedaToplevel *toplevel, GedaObject *object)
{
  GList *promotable, *iter;

  promotable = geda_complex_object_get_promotable (toplevel, object, FALSE);

  if (promotable == NULL)
    return;

  for (iter = promotable; iter != NULL; iter = iter->next) {

    GedaObject *a_object = iter->data;

    if (toplevel->keep_invisible == TRUE) {   /* Hide promotable attributes */
      geda_set_object_visibility (a_object, INVISIBLE);
    }
    else {                                    /* Delete promotable attributes */
      GList *from_list = object->complex->prim_objs;
      object->complex->prim_objs = g_list_remove (from_list, a_object);
      geda_struct_object_release (a_object);
    }
  }

  geda_set_object_bounds_invalid (object);
  g_list_free (promotable);
}

/*!
 * \brief Read Complex object from a char buffer
 * \par Function Description
 *  This function reads a complex object from the buffer \a buf.
 *  If the complex object was read successfully, a new object is
 *  allocated and appended to the object_list in \a toplevel.
 *
 * \param [in] toplevel       The GedaToplevel object
 * \param [in] buf            Text buffer (usually a line of a schematic file)
 * \param [in] release_ver    The release number gEDA
 * \param [in] fileformat_ver An integer value of the file format
 *
 * \param [out] err           A GError object
 *
 * \return The object list, or NULL on error.
 */
GedaObject *geda_complex_object_read (GedaToplevel *toplevel,
                                      const char   buf[],
                                      unsigned int release_ver,
                                      unsigned int fileformat_ver,
                                      GError       **err)
{
  GedaObject *new_obj;
  char type;

  int angle;
  int mirror;
  int selectable;
  int x1, y1;

  char *basename = GEDA_MEM_ALLOC (1 + strlen (buf));

  if (sscanf(buf, "%c %d %d %d %d %d %s\n",
    &type, &x1, &y1, &selectable, &angle, &mirror, basename) != 7) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse complex object"));
    return NULL;
  }

  if (angle != 0 && angle != 90 && angle != 180 && angle != 270) {
    const char *msg = _("Found a component with an invalid rotation");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, angle);
    }
    geda_log_w (_("Setting angle to 0\n"));
    angle = 0;
  }

  if (mirror != 0 && mirror != 1) {
    const char *msg = _("Found a component with an invalid mirror flag");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, mirror);
    }
    geda_log_w (_("Setting mirror to 0\n"));
    mirror = 0;
  }

  /* Do not load symbol recursively, resolves bug 732326 */
  Page *current_page;

  current_page = geda_toplevel_get_current_page(toplevel);

  if (current_page != NULL) {

    char *current_file = geda_page_get_filename_dup(current_page);

    if (strcmp(basename, geda_get_basename(current_file)) == 0) {
      fprintf(stderr, "libgeda: refusing to recursively read <%s>\n", basename);
      g_set_error(err, EDA_ERROR, EDA_ERROR_LOOP, _("Invalid complex object"));
      GEDA_FREE (current_file);
      return NULL;
    }

    GEDA_FREE (current_file);
  }

  if (strncmp(basename, "EMBEDDED", 8) == 0) {

    new_obj = geda_complex_object_new_embedded(x1, y1, angle, mirror, basename + 8,
                                               selectable);
  }
  else {

    const CLibSymbol *clib = geda_struct_clib_get_symbol_by_name (basename);

    new_obj = geda_complex_object_new(toplevel, x1, y1, angle, mirror, clib, basename, selectable);

    /* Delete or hide attributes eligible for promotion inside the complex */
    if (new_obj) {
      o_complex_remove_promotable (toplevel, new_obj);
    }
  }

  GEDA_FREE (basename);

  return new_obj;
}

/*!
 * \brief Reset the refdes number back to a question mark
 * \par Function Description
 *  This function finds the refdes attribute inside this
 *  object and resets the refdes number back to a question mark.
 *
 * \param [in] object      The complex containing text objects
 */
void geda_complex_object_reset_refdes(GedaObject *object)
{
  if (GEDA_IS_COMPLEX(object)) {

    GList *iter = object->attribs;

    while (iter != NULL) {
      GedaObject *attrib = (GedaObject*) iter->data;

      if (attrib->type == OBJ_TEXT) {
        geda_utility_refdes_reset(attrib);
      }

      iter = iter->next;
    }
  }
  else {
    geda_complex_object_error(__func__, object);
  }
}

/*!
 * \brief Rotates a complex object in world coordinates
 * \par Function Description
 *  This function rotates a complex \a object around the
 *  (\a center_x,\a center_y) point by \a angle degrees.
 *  The center of rotation is in world units.
 *
 * \param [in,out] object    Complex object to rotate
 * \param [in]     center_x  X coordinate of rotation center (world coords)
 * \param [in]     center_y  Y coordinate of rotation center (world coords)
 * \param [in]     angle     Rotation angle in degrees
 */
void geda_complex_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  if (GEDA_IS_COMPLEX(object)) {

    int x, y;
    int newx, newy;

    x = object->complex->x + (-center_x);
    y = object->complex->y + (-center_y);

    geda_math_rotate_point_90(x, y, angle, &newx, &newy);

    x = newx + (center_x);
    y = newy + (center_y);

    geda_complex_object_translate(object, -object->complex->x, -object->complex->y);

    geda_object_list_rotate (object->complex->prim_objs, 0, 0, angle);

    object->complex->x = 0;
    object->complex->y = 0;

    geda_complex_object_translate(object, x, y);

    object->complex->angle = (object->complex->angle + angle ) % 360;
  }
  else {
    geda_complex_object_error(__func__, object);
  }
}

/*!
 * \brief Create a string representation of the complex object
 * \par Function Description
 *  This function takes a complex \a object and return a string
 *  according to the file format definition.
 *
 * \note object was validated by geda_object_save_objects.
 *
 * \param [in] object  a complex Object
 *
 * \return the string representation of the complex Object
 */
char *geda_complex_object_to_buffer(GedaObject *object)
{
  if (GEDA_IS_COMPLEX(object)) {

    GedaComplex *complex;
    char        *basename;
    char        *buf = NULL;
    int          selectable;

    complex = GEDA_COMPLEX(object);

    basename = geda_sprintf ("%s%s", complex->is_embedded ? "EMBEDDED" : "",
                                     complex->filename);

    selectable = (object->selectable) ? 1 : 0;

    /* Force the object type to be output as OBJ_COMPLEX for both
     * these object types. */
    buf = geda_sprintf("%c %d %d %d %d %d %s", OBJ_COMPLEX,
    complex->x, complex->y,
    selectable, complex->angle,
    complex->mirror, basename);
    GEDA_FREE (basename);

    return(buf);
  }

  geda_complex_object_error(__func__, object);

  return NULL;
}

/*!
 * \brief Get Shortest distance from Complex to point
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on an object within the complex object.
 *
 * \note When querying the distance to our child objects, we always
 *       force treating them as solid filled.
 *       We ignore the force_solid argument to this function.
 *
 * \param [in] object       A complex Object.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point. If the
 *         distance cannot be calculated, this function returns a really
 *         large number (G_MAXDOUBLE).  With an invalid parameter, this
 *         function returns G_MAXDOUBLE.
 */
double geda_complex_object_shortest_distance(ConstObject *object, int x, int y, int force_solid)
{

  GedaBox line_bounds;
  GList  *iter;
  double  distance;
  double  shortest_distance;
  int     found_line_bounds;

  if (!GEDA_IS_COMPLEX(object)) {
    geda_complex_object_error(__func__, object);
    return G_MAXDOUBLE;
  }

  shortest_distance = G_MAXDOUBLE;
  found_line_bounds = 0;

  for (iter = object->complex->prim_objs; iter != NULL; NEXT(iter)) {

    GedaObject *obj = iter->data;
    int left, top, right, bottom;

    /* Collect the bounds of any lines and arcs in the symbol */
    if ((obj->type == OBJ_LINE || obj->type == OBJ_ARC) &&
         geda_object_get_bounds(obj, &left, &top, &right, &bottom))
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
      distance = geda_object_get_shortest_distance_full (obj, x, y, TRUE);
      shortest_distance = min (shortest_distance, distance);
    }

    if (shortest_distance == 0.0)
      return shortest_distance;
  }

  if (found_line_bounds) {
    distance = geda_math_box_shortest_distance (&line_bounds, x, y, TRUE);
    shortest_distance = min (shortest_distance, distance);
  }

  return shortest_distance;
}

/*!
 * \brief Translate a complex object
 * \par Function Description
 *  This function changes the position of a complex \a object.
 *
 * \param [in,out] object  The complex Object to be translated
 * \param [in]     dx      The x-distance to move the object
 * \param [in]     dy      The y-distance to move the object
 */
void geda_complex_object_translate(GedaObject *object, int dx, int dy)
{
  if (GEDA_IS_COMPLEX(object)) {

    object->complex->x = object->complex->x + dx;
    object->complex->y = object->complex->y + dy;

    geda_object_list_translate (object->complex->prim_objs, dx, dy);

    object->bounds_valid = FALSE;
  }
  else {
    geda_complex_object_error(__func__, object);
  }
}

/** @} endgroup geda-complex-object-proc */
