/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2015 Peter Brett <peter@peter-b.co.uk>
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

/*!
 * \file scheme_complex.c
 * \brief Scheme API complex object manipulation procedures.
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

/*! \brief Create a new complex object.
 * \par Function Description
 * Creates a new, empty complex object, with the given \a basename and
 * with all other parameters set to default values.  It is initially set
 * to be embedded.
 *
 * \note Scheme API: Implements the %make-complex procedure in the
 * (geda core complex) module.
 *
 * \return a newly-created complex object.
 */
EDA_SCM_DEFINE (complex_make, "%make-complex", 1, 0, 0,
               (SCM basename_s), "Create a new complex object.")
{
  SCM_ASSERT (scm_is_string (basename_s), basename_s, SCM_ARG1, scheme_complex_make);

  char *tmp = scm_to_utf8_string (basename_s);
  GedaObject *obj = geda_complex_object_new_embedded (0, 0, 0, FALSE, tmp, TRUE);
  free (tmp);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Instantiate a complex object from the component library.
 * \par Function Description

 * Searches the component library for a component with the given \a
 * basename.  If found, creates a new complex object by instantiating
 * that library component.  It is initially set to be unembedded.  If
 * no match is found for \a basename in the library, returns
 * SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %make-complex/library procedure in
 * the (geda core complex) module.
 *
 * param basename component name to search for in the component
 *                 library.
 * \return a newly-created complex object.
 */
EDA_SCM_DEFINE (complex_make_library, "%make-complex/library", 1, 0, 0,
               (SCM basename_s),
               "Instantiate a complex object from the component library.")
{
  SCM_ASSERT (scm_is_string (basename_s), basename_s, SCM_ARG1,
              scheme_complex_make_library);

  char *basename;
  const CLibSymbol *clib;

  basename = scm_to_utf8_string (basename_s);

  clib = geda_struct_clib_get_symbol_by_name (basename);

  SCM result = SCM_BOOL_F;

  if (clib != NULL) {

    scm_dynwind_begin (0);
    scm_dynwind_unwind_handler (free, basename, SCM_F_WIND_EXPLICITLY);

    GedaToplevel *toplevel;
    GedaObject   *obj;

    toplevel = edascm_c_current_toplevel();
    obj      = geda_complex_object_new (toplevel, 0, 0, 0, FALSE, clib, basename, TRUE);
    result   = edascm_from_object (obj);

    /* At the moment, the only pointer to the object is owned by the smob. */
    edascm_c_set_gc (result, TRUE);

    scm_dynwind_end ();
  }

  return result;
}

/*! \brief Set complex object parameters.
 * \par Function Description
 * Modifies the complex object \a complex_s by setting its parameters
 * to new values. The modifying is performed in the following sequence:
 * 1. Mirroring.
 * 2. Rotation.
 * 3. Translating to new position.
 *
 * \note Scheme API: Implements the %set-complex! procedure in the
 * (geda core complex) module.
 *
 * param complex_s the complex object to modify.
 * param x_s       the new x-coordinate of the complex object.
 * param y_s       the new y-coordinate of the complex object.
 * param angle_s   the new rotation angle.
 * param mirror_s  whether the complex object should be mirrored.
 * param locked_s  whether the complex object should be locked.
 *
 * \return the modified \a complex_s.
 */
EDA_SCM_DEFINE (complex_set_x, "%set-complex!", 6, 0, 0,
               (SCM complex_s, SCM x_s, SCM y_s, SCM angle_s, SCM mirror_s,
                SCM locked_s), "Set complex object parameters")
{
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, scheme_complex_set_x);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, scheme_complex_set_x);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, scheme_complex_set_x);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s, SCM_ARG4, scheme_complex_set_x);

  GedaObject *obj = edascm_to_object (complex_s);

  geda_object_notify_emit_pre_change (obj);

  int x = scm_to_int (x_s);
  int y = scm_to_int (y_s);

  /* Mirroring */
  if (obj->complex->mirror != scm_is_true (mirror_s)) {
    geda_object_mirror (obj, x, y);
  }

  /* Angle */
  int angle = scm_to_int (angle_s);
  switch (angle) {
  case 0:
  case 90:
  case 180:
  case 270:
    /* These are all fine. */
    break;
  default:
    /* Otherwise, not fine. */
    scm_misc_error (scheme_complex_set_x,
                    _("Invalid complex angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
  }

  /* The angle to rotate by is the difference between the
   * angle we want and the current angle */
  angle -= obj->complex->angle;
  if (angle < 0) {
    angle += 360;
  }

  geda_object_rotate (obj, x, y, angle);

  geda_object_translate (obj, x - obj->complex->x, y - obj->complex->y);

  obj->selectable = scm_is_false (locked_s);

  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed (obj);

  return complex_s;
}

/*! \brief Get complex object parameters.
 * \par Function Description
 * Retrieves the parameters of a complex object. The return value is a
 * list of parameters:
 *
 * -# Basename
 * -# Base x-coordinate.
 * -# Base y-coordinate.
 * -# Rotation angle.
 * -# Whether object is mirrored.
 * -# Whether object is locked.
 *
 * \note Scheme API: Implements the %complex-info procedure in the
 * (geda core complex) module.
 *
 * param complex_s the complex object to inspect.
 * \return a list of complex object parameters.
 */
EDA_SCM_DEFINE (complex_info, "%complex-info", 1, 0, 0,
               (SCM complex_s), "Get complex object parameters.")
{
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, scheme_complex_info);

  GedaObject *obj = edascm_to_object (complex_s);

  return scm_list_n (scm_from_utf8_string (obj->complex->filename),
                     scm_from_int (obj->complex->x),
                     scm_from_int (obj->complex->y),
                     scm_from_int (obj->complex->angle),
                     obj->complex->mirror ? SCM_BOOL_T : SCM_BOOL_F,
                     obj->selectable ? SCM_BOOL_F : SCM_BOOL_T,
                     SCM_UNDEFINED);
}

/*! \brief Get the contents of a complex object.
 * \par Function Description
 * Retrieves a list of the primitive objects that make up a complex object.
 *
 * \note Scheme API: Implements the %complex-contents procedure in the
 * (geda core complex) module.
 *
 * param complex_s a complex object.
 * \return a list of primitive objects.
 */
EDA_SCM_DEFINE (complex_contents, "%complex-contents", 1, 0, 0,
               (SCM complex_s), "Get complex object contents.")
{
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, scheme_complex_contents);

  GedaObject *obj = edascm_to_object (complex_s);

  return edascm_from_object_glist (obj->complex->prim_objs);
}

/*! \brief Add a primitive object to a complex object.
 * \par Function Description
 * Adds \a obj_s to \a complex_s.  If \a obj_s is already attached to
 * another complex object or to a #Page, or if \a obj_s is itself a
 * complex object, throws a Scheme error.  If \a obj_s is already
 * attached to \a complex_s, does nothing.
 *
 * \note Scheme API: Implements the %complex-append! procedure of the
 * (geda core complex) module.
 *
 * param complex_s complex object to modify.
 * param obj_s     primitive object to add.
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (complex_append_x, "%complex-append!", 2, 0, 0,
               (SCM complex_s, SCM obj_s),
               "Add a primitive object to a complex object")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, scheme_complex_append_x);

  SCM_ASSERT ((EDASCM_OBJECTP (obj_s) &&
               !edascm_is_object_type (obj_s, OBJ_COMPLEX) &&
               !edascm_is_object_type (obj_s, OBJ_PLACEHOLDER)),
              obj_s, SCM_ARG2, scheme_complex_append_x);

  GedaObject *parent = edascm_to_object (complex_s);
  GedaObject *child  = edascm_to_object (obj_s);

  /* Check that object is not already attached to a page */
  if (geda_object_get_page (child) != NULL) {
    scm_error (edascm_object_state_sym, scheme_complex_append_x,
               _("Object ~A is already on a page"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not already attached to a different complex. */
  if (child->parent_object != NULL && child->parent_object != parent)
  {
    scm_error (edascm_object_state_sym, scheme_complex_append_x,
               _("Object ~A is already attached to something"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (child->parent_object != parent) {

    /* GedaObject cleanup now managed by C code. */
    edascm_c_set_gc (obj_s, 0);

    /* Don't need to emit change notifications for the child because
     * it's guaranteed not to be present in a page at this point. */
    geda_object_notify_emit_pre_change (parent);

    parent->complex->prim_objs = g_list_append (parent->complex->prim_objs,
                                                child);
    child->parent_object = parent;

    parent->bounds_valid = FALSE;

    /* We may need to update connections */
    geda_struct_tile_update_object (child);
    geda_struct_conn_update_object (child);

    geda_object_notify_emit_change (parent);

    geda_struct_object_set_page_changed (parent);
  }

  return complex_s;
}

/*! \brief Remove a primitive object from a complex object.
 * \par Function Description
 * Removes \a obj_s from \a complex_s.  If \a obj_s is attached to a
 * #Page or to a complex object other than \a complex_s, throws a
 * Scheme error.  If \a obj_s is unattached, does nothing.
 *
 * \note Scheme API: Implements the %complex-remove! procedure of the
 * (geda core complex) module.
 *
 * param complex_s complex object to modify.
 * param obj_s     primitive object to remove.
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (complex_remove_x, "%complex-remove!", 2, 0, 0,
               (SCM complex_s, SCM obj_s),
               "Remove a primitive object from a complex object")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, scheme_complex_remove_x);
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s, SCM_ARG2, scheme_complex_remove_x);

  GedaObject *parent     = edascm_to_object (complex_s);
  GedaObject *child      = edascm_to_object (obj_s);
  Page       *child_page = geda_object_get_page (child);

  /* Check that object is not attached to a different complex. */
  if ((child->parent_object != NULL) && (child->parent_object != parent)) {
    scm_error (edascm_object_state_sym, scheme_complex_remove_x,
               _("Object ~A is attached to a different complex"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached to a page. */
  if ((child->parent_object == NULL) && (child_page != NULL)) {
    scm_error (edascm_object_state_sym, scheme_complex_remove_x,
               _("Object ~A is attached to a page"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached as an attribute. */
  if (child->attached_to != NULL) {
    scm_error (edascm_object_state_sym, scheme_complex_remove_x,
               _("Object ~A is attached as an attribute"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object doesn't have attributes. */
  if (child->attribs != NULL) {
    scm_error (edascm_object_state_sym, scheme_complex_remove_x,
               _("Object ~A has attributes"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (child->parent_object == NULL) {
    return obj_s;
  }

  /* Don't need to emit change notifications for the child because
   * only the parent will remain in the page. */
  geda_object_notify_emit_pre_change (parent);

  parent->complex->prim_objs = g_list_remove_all (parent->complex->prim_objs,
                                                  child);
  child->parent_object = NULL;

  /* We may need to update connections */
  geda_struct_tile_remove_object (child);
  geda_struct_conn_remove_object (child);

  geda_object_notify_emit_change (parent);

  geda_struct_object_set_page_changed (parent);

  /* GedaObject cleanup now managed by Guile. */
  edascm_c_set_gc (obj_s, 1);

  return complex_s;
}

/*!
 * \brief Create the (geda core complex) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core complex) module. The module can
 * be accessed using (use-modules (geda core complex)).
 */
static void
init_module_geda_core_complex (void *nothing)
{
  /* Register the functions and symbols */
  #include "scheme_complex.x"

  /* Add them to the module's public definitions. */
  scm_c_export (scheme_complex_append_x,
                scheme_complex_make,
                scheme_complex_make_library,
                scheme_complex_set_x,
                scheme_complex_info,
                scheme_complex_contents,
                scheme_complex_remove_x,
                NULL);
}

/*!
 * \brief Initialise the basic gEDA complex object manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with complex #GedaObject
 * smobs. Should only be called by edascm_init().
 */
void
edascm_init_complex (void)
{
  /* Define the (geda core object) module */
  scm_c_define_module ("geda core complex",
                       init_module_geda_core_complex,
                       NULL);
}
