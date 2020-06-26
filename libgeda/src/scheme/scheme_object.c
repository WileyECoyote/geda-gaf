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
 * \file scheme_object.c
 * \brief Scheme API object manipulation procedures.
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

#ifndef  SCM_ARG8
 #define SCM_ARG8 8
 #define SCM_ARG9 9
#endif

extern void edascm_init_make_object(void);

SCM_GLOBAL_SYMBOL (edascm_object_state_sym, "object-state");

SCM_SYMBOL (wrong_type_arg_sym , "wrong-type-arg");
SCM_SYMBOL (arc_sym ,            "arc");
SCM_SYMBOL (box_sym ,            "box");
SCM_SYMBOL (bus_sym ,            "bus");
SCM_SYMBOL (circle_sym ,         "circle");
SCM_SYMBOL (complex_sym ,        "complex");
SCM_SYMBOL (line_sym ,           "line");
SCM_SYMBOL (net_sym ,            "net");
SCM_SYMBOL (path_sym ,           "path");
SCM_SYMBOL (picture_sym ,        "picture");
SCM_SYMBOL (pin_sym ,            "pin");
SCM_SYMBOL (text_sym ,           "text");

SCM_SYMBOL (lower_left_sym ,     "lower-left");
SCM_SYMBOL (middle_left_sym ,    "middle-left");
SCM_SYMBOL (upper_left_sym ,     "upper-left");
SCM_SYMBOL (lower_center_sym ,   "lower-center");
SCM_SYMBOL (middle_center_sym ,  "middle-center");
SCM_SYMBOL (upper_center_sym ,   "upper-center");
SCM_SYMBOL (lower_right_sym ,    "lower-right");
SCM_SYMBOL (middle_right_sym ,   "middle-right");
SCM_SYMBOL (upper_right_sym ,    "upper-right");

SCM_SYMBOL (name_sym ,           "name");
SCM_SYMBOL (value_sym ,          "value");
SCM_SYMBOL (both_sym ,           "both");

SCM_SYMBOL (none_sym,            "none");
SCM_SYMBOL (square_sym ,         "square");
SCM_SYMBOL (round_sym ,          "round");

SCM_SYMBOL (solid_sym ,          "solid");
SCM_SYMBOL (dotted_sym ,         "dotted");
SCM_SYMBOL (dashed_sym ,         "dashed");
SCM_SYMBOL (center_sym ,         "center");
SCM_SYMBOL (phantom_sym ,        "phantom");

SCM_SYMBOL (hollow_sym ,         "hollow");
SCM_SYMBOL (mesh_sym ,           "mesh");
SCM_SYMBOL (hatch_sym ,          "hatch");

SCM_SYMBOL (moveto_sym ,         "moveto");
SCM_SYMBOL (lineto_sym ,         "lineto");
SCM_SYMBOL (curveto_sym ,        "curveto");
SCM_SYMBOL (closepath_sym ,      "closepath");

/*!
 * \brief Convert a Scheme object list to a GList.
 * \par Function Description
 *  Takes a Scheme list of GedaObject smobs, and returns a GList
 *  containing the objects. If \a objs is not a list of GedaObject smobs,
 *  throws a Scheme error.
 *
 * \warning If the GedaObject structures in the GList are to be stored by
 *          C code and later free()'d directly, the smobs must be marked as
 *          unsafe for garbage collection (by calling edascm_c_set_gc()).
 *
 *  param [in] objs a Scheme list of GedaObject smobs.
 *  param [in] subr the name of the Scheme subroutine (used for error messages).
 *
 * \return a GList of GedaObject.
 */
GList *
edascm_to_object_glist (SCM objs, const char *subr)
{
  GList *result = NULL;
  SCM lst;

  SCM_ASSERT (scm_is_true (scm_list_p (objs)), objs, SCM_ARGn, subr);

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler ((void (*)(void *))g_list_free, result, 0);

  for (lst = objs; lst != SCM_EOL; lst = SCM_CDR (lst)) {
    SCM smob = SCM_CAR (lst);
    result = g_list_prepend (result, (void *) edascm_to_object (smob));
  }

  scm_remember_upto_here_1 (lst);

  scm_dynwind_end ();

  return g_list_reverse (result);
}

/*!
 * \brief Convert a GList of objects into a Scheme list.
 * \par Function Description
 *  Takes a GList of GedaObject and returns a Scheme list of corresponding
 *  object smobs.
 *
 * \warning If the GedaObject structures are to be subsequently managed
 *          only by Scheme, the smobs in the returned list must be marked as
 *          safe for garbage collection (by calling edascm_c_set_gc()).
 *
 *  param [in] objs a GList of GedaObject instances.
 *
 * \return a Scheme list of smobs corresponding to each GedaObject.
 */
SCM
edascm_from_object_glist (const GList *objs)
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *iter = (GList *) objs;

  while (iter != NULL) {
    lst = scm_cons (edascm_from_object (iter->data), lst);
    iter = g_list_next (iter);
  }

  rlst = scm_reverse (lst);

  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*!
 * \brief Test if an object smob is of a particular type.
 * \par Function Description
 *  Checks if \a smob contains an GedaObject of the given \a type. This is
 *  intended to be used by C-based Scheme procedures for working with
 *  particular object types.
 *
 *  param [in] smob Scheme value to check type for.
 *  param [in] type Type to check against (e.g. OBJ_LINE).
 *
 * \return non-zero if \a smob is an GedaObject smob of \a type.
 */
int edascm_is_object_type (SCM smob, int type)
{
  if (!EDASCM_OBJECTP(smob)) {
    return 0;
  }

  GedaObject *obj = edascm_to_object (smob);

  return (obj->type == type);
}

/* ----------------------- Scheme GedaObject API ---------------------- */

/*!
 * \brief Copy an object.
 * \par Function Description
 *  Returns a copy of the GedaObject contained in smob \a obj_s as a new
 *  smob.
 *
 * \note Scheme API: Implements the %copy-object procedure in the
 *       (geda core object) module.
 *
 *  param [in] obj_s an GedaObject smob.
 *
 * \return a new GedaObject smob containing a copy of the GedaObject in \a obj_s.
 */
EDA_SCM_DEFINE (object_copy, "%copy-object", 1, 0, 0,
               (SCM obj_s), "Copy an object.")
{
  SCM result;
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s, SCM_ARG1, scheme_object_copy);

  GedaObject *obj = edascm_to_object (obj_s);

  result = edascm_from_object (geda_object_copy (obj));

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);
  return result;
}

/*!
 * \brief Mirror an object.
 * \par Function Description
 *  Mirrors \a obj_s in the line x = \a x_s.
 *
 * \note Scheme API: Implements the %mirror-object! procedure of the
 *      (geda core object) module.
 *
 *  param obj_s    GedaObject smob for object to translate.
 *  param x_s      x-coordinate of centre of rotation.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_mirror_x, "%mirror-object!", 2, 0, 0,
               (SCM obj_s, SCM x_s),
               "Mirror an object.")
{
  /* Check argument types */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, scheme_object_mirror_x);
  SCM_ASSERT (scm_is_integer (x_s), x_s,
              SCM_ARG2, scheme_object_mirror_x);

  GedaObject *obj = edascm_to_object (obj_s);
  int x = scm_to_int (x_s);

  geda_object_notify_emit_pre_change (obj);
  geda_object_mirror (obj, x, 0);
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Get the bounds of a list of objects
 * \par Function Description
 *  Returns the bounds of the objects in the variable-length argument
 *  list \a rst_s. The bounds are returned as a pair structure of the
 *  form:
 *
 * \code
 * ((left . top) . (right . bottom))
 * \endcode
 *
 *  If \a rst_s is empty, or none of the objects has any bounds
 *  (e.g. because they are all empty components and/or text strings),
 *  returns SCM_BOOL_F.
 *
 * \warning This function always returns the actual bounds of the
 *          objects, not the visible bounds.
 *
 * \note Scheme API: Implements the %object-bounds procedure in the
 *      (geda core object) module.  The procedure takes any number of
 *       GedaObject smobs as arguments.
 *
 *  param [in] rst_s Variable-length list of GedaObject arguments.
 *
 * \return bounds of objects or SCM_BOOL_F.
 */
EDA_SCM_DEFINE (object_bounds, "%object-bounds", 0, 0, 1,
               (SCM rst_s), "Get the bounds of a list of objects")
{
  GList *obj_list;

  int left, top, right, bottom;
  int success;

  obj_list = edascm_to_object_glist (rst_s, scheme_object_bounds);

  if (obj_list != NULL) {

    GedaObject *o_current;
    GList      *iter;
    Page       *page;

    o_current = (GedaObject*)obj_list->data;
    page      = geda_object_get_page(o_current);

    for (iter = obj_list; iter != NULL; iter = g_list_next(iter)) {
      geda_object_set_bounds_valid(iter->data, FALSE);
    }

    if (page == NULL) {

      success = geda_object_get_bounds_list (obj_list, &left, &top, &right, &bottom);
    }
    else if (page->show_hidden_text) {

      success = geda_object_get_bounds_list (obj_list, &left, &top, &right, &bottom);
    }
    else {

      page->show_hidden_text = TRUE;

      success = geda_object_get_bounds_list (obj_list, &left, &top, &right, &bottom);

      page->show_hidden_text = FALSE;
    }
  }
  else {
    success  = FALSE;
  }

  SCM result = SCM_BOOL_F;

  if (success) {
    result = scm_cons (scm_cons (scm_from_int (min(left, right)),
                                 scm_from_int (max(top, bottom))),
                       scm_cons (scm_from_int (max(left, right)),
                                 scm_from_int (min(top, bottom))));
  }

  scm_remember_upto_here_1 (rst_s);

  return result;
}


/*!
 * \brief Get the stroke properties of an object.
 * \par Function Description
 *  Returns the stroke settings of the object \a obj_s.  If \a obj_s is
 *  not a line, box, circle, arc, or path, throws a Scheme error.  The
 *  return value is a list of parameters:
 *
 * -# stroke width
 * -# cap style (a symbol: none, square or round)
 * -# dash style (a symbol: solid, dotted, dashed, center or phantom)
 * -# up to two dash parameters, depending on dash style:
 *    -# For solid lines, no parameters.
 *    -# For dotted lines, dot spacing.
 *    -# For other styles, dot/dash spacing and dash length.
 *
 * \note Scheme API: Implements the %object-stroke procedure in the
 *       (geda core object) module.
 *
 *  param obj_s object to get stroke settings for.
 *
 * \return a list of stroke parameters.
 */
EDA_SCM_DEFINE (object_stroke, "%object-stroke", 1, 0, 0,
               (SCM obj_s), "Get the stroke properties of an object.")
{
  SCM_ASSERT ((edascm_is_object_type (obj_s, OBJ_LINE)   ||
               edascm_is_object_type (obj_s, OBJ_BOX)    ||
               edascm_is_object_type (obj_s, OBJ_CIRCLE) ||
               edascm_is_object_type (obj_s, OBJ_ARC)    ||
               edascm_is_object_type (obj_s, OBJ_PATH)),
               obj_s, SCM_ARG1, scheme_object_stroke);

  GedaObject *obj = edascm_to_object (obj_s);

  int end, type, width, length, space;

  geda_object_get_line_options (obj, (LINE_END*) &end,
                                     (LINE_TYPE*) &type,
                                     &width, &length, &space);

  SCM width_s  = scm_from_int (width);
  SCM length_s = scm_from_int (length);
  SCM space_s  = scm_from_int (space);

  SCM cap_s;
  switch (end) {
    case END_NONE:   cap_s = none_sym;   break;
    case END_SQUARE: cap_s = square_sym; break;
    case END_ROUND:  cap_s = round_sym;  break;
    default:
      scm_misc_error (scheme_object_stroke,
                      _("Object ~A has invalid stroke cap style ~A"),
                      scm_list_2 (obj_s, scm_from_int (end)));
  }

  SCM dash_s;
  switch (type) {
    case TYPE_SOLID:   dash_s = solid_sym;   break;
    case TYPE_DOTTED:  dash_s = dotted_sym;  break;
    case TYPE_DASHED:  dash_s = dashed_sym;  break;
    case TYPE_CENTER:  dash_s = center_sym;  break;
    case TYPE_PHANTOM: dash_s = phantom_sym; break;
    default:
      scm_misc_error (scheme_object_stroke,
                      _("Object ~A has invalid stroke dash style ~A"),
                      scm_list_2 (obj_s, scm_from_int (type)));
  }

  switch (type) {
    case TYPE_DASHED:
    case TYPE_CENTER:
    case TYPE_PHANTOM:
      return scm_list_5 (width_s, cap_s, dash_s, space_s, length_s);

    case TYPE_DOTTED:
      return scm_list_4 (width_s, cap_s, dash_s, space_s);

    default:
      return scm_list_3 (width_s, cap_s, dash_s);
  }
}

/*!
 * \brief Get the fill properties of an object.
 * \par Function Description
 *  Returns the fill settings of the object \a obj_s.  If \a obj_s is
 *  not a box, circle, or path, throws a Scheme error.  The return
 *  value is a list of parameters:
 *
 * -# fill style (a symbol: hollow, solid, mesh or hatch)
 * -# up to five fill parameters, depending on fill style:
 *   -# none for hollow or solid fills
 *   -# line width, line angle, and line spacing for hatch fills.
 *   -# line width, first angle and spacing, and second angle and
 *      spacing for mesh fills.
 *
 * \note Scheme API: Implements the %object-fill procedure in the
 *       (geda core object) module.
 *
 *  param obj_s object to get fill settings for.
 *
 * \return a list of fill parameters.
 */
EDA_SCM_DEFINE (object_fill, "%object-fill", 1, 0, 0,
               (SCM obj_s), "Get the fill properties of an object.")
{
  SCM_ASSERT ((edascm_is_object_type (obj_s, OBJ_BOX)    ||
               edascm_is_object_type (obj_s, OBJ_CIRCLE) ||
               edascm_is_object_type (obj_s, OBJ_PATH)),
               obj_s, SCM_ARG1, scheme_object_fill);

  GedaObject *obj = edascm_to_object (obj_s);

  int type, width, pitch1, angle1, pitch2, angle2;
  geda_object_get_fill_options (obj, (OBJECT_FILLING *) &type, &width, &pitch1, &angle1,
                      &pitch2, &angle2);

  SCM width_s  = scm_from_int (width);
  SCM pitch1_s = scm_from_int (pitch1);
  SCM angle1_s = scm_from_int (angle1);
  SCM pitch2_s = scm_from_int (pitch2);
  SCM angle2_s = scm_from_int (angle2);

  SCM type_s;
  switch (type) {
  case FILLING_HOLLOW: type_s = hollow_sym; break;
  case FILL_SOLID:     type_s = solid_sym;  break;
  case FILLING_MESH:   type_s = mesh_sym;   break;
  case FILLING_HATCH:  type_s = hatch_sym;  break;
  default:
    scm_misc_error (scheme_object_fill,
                    _("Object ~A has invalid fill style ~A"),
                    scm_list_2 (obj_s, scm_from_int (type)));
  }

  if (type == FILLING_MESH) {
    return scm_list_n (type_s, width_s, pitch1_s, angle1_s, pitch2_s, angle2_s,
                       SCM_UNDEFINED);
  }
  else if (type == FILLING_HATCH) {
    return scm_list_4 (type_s, width_s, pitch1_s, angle1_s);
  }

  return scm_list_1 (type_s);
}

/*!
 * \brief Set the fill properties of an object.
 * \par Function Description
 *  Updates the fill settings of the object \a obj_s.  If \a obj_s is
 *  not a box, circle, or path, throws a Scheme error.  The optional
 *  parameters \a width_s, \a angle1_s, \a space1_s, \a angle2_s and
 *  space2_s
 *
 * \note Scheme API: Implements the %object-fill procedure in the
 *       (geda core object) module.
 *
 *  param obj_s object to set fill settings for.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_set_fill_x, "%set-object-fill!", 2, 5, 0,
               (SCM obj_s, SCM type_s, SCM width_s, SCM space1_s, SCM angle1_s,
                SCM space2_s, SCM angle2_s),
               "Set the fill properties of an object.")
{
  SCM_ASSERT ((edascm_is_object_type (obj_s, OBJ_BOX)    ||
               edascm_is_object_type (obj_s, OBJ_CIRCLE) ||
               edascm_is_object_type (obj_s, OBJ_PATH)),
               obj_s, SCM_ARG1, scheme_object_set_fill_x);

  GedaObject *obj = edascm_to_object (obj_s);
  FILL_OPTIONS fill_options;

  if (type_s == hollow_sym) {
    fill_options.fill_type = FILLING_HOLLOW;
  }
  else if (type_s == solid_sym) {
    fill_options.fill_type = FILL_SOLID;
  }
  else if (type_s == hatch_sym) {
    fill_options.fill_type = FILLING_HATCH;
  }
  else if (type_s == mesh_sym) {
    fill_options.fill_type = FILLING_MESH;
  }
  else {
    scm_misc_error (scheme_object_set_fill_x, _("Invalid fill style ~A."),
                    scm_list_1 (type_s));
  }

  switch (fill_options.fill_type) {
  case FILLING_MESH:
    if (space2_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_fill_x,
                      _("Missing second space parameter for fill style ~A."),
                      scm_list_1 (space2_s));
    }
    SCM_ASSERT (scm_is_integer (space2_s), space2_s,
                SCM_ARG6, scheme_object_set_fill_x);
    fill_options.fill_pitch2 = scm_to_int (space2_s);

    if (angle2_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_fill_x,
                      _("Missing second angle parameter for fill style ~A."),
                      scm_list_1 (angle2_s));
    }
    SCM_ASSERT (scm_is_integer (angle2_s), angle2_s,
                SCM_ARG7, scheme_object_set_fill_x);
    fill_options.fill_angle2 = scm_to_int (angle2_s);

    /* This case intentionally falls through */
  case FILLING_HATCH:
    if (width_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_fill_x,
                      _("Missing stroke width parameter for fill style ~A."),
                      scm_list_1 (width_s));
    }
    SCM_ASSERT (scm_is_integer (width_s), width_s,
                SCM_ARG3, scheme_object_set_fill_x);
    fill_options.fill_width = scm_to_int (width_s);

    if (space1_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_fill_x,
                      _("Missing space parameter for fill style ~A."),
                      scm_list_1 (space1_s));
    }
    SCM_ASSERT (scm_is_integer (space1_s), space1_s,
                SCM_ARG4, scheme_object_set_fill_x);
    fill_options.fill_pitch1 = scm_to_int (space1_s);

    if (angle1_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_fill_x,
                      _("Missing angle parameter for fill style ~A."),
                      scm_list_1 (angle1_s));
    }
    SCM_ASSERT (scm_is_integer (angle1_s), angle1_s,
                SCM_ARG5, scheme_object_set_fill_x);
    fill_options.fill_angle1 = scm_to_int (angle1_s);
    /* This case intentionally falls through */
    break;

  case FILLING_HOLLOW:
  case FILL_SOLID:
  case FILLING_VOID:
    break;
  }

  geda_object_notify_emit_pre_change (obj);

  geda_set_object_fill_options (obj, &fill_options);
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed (obj);

  return obj_s;
}

/*!
 * \brief Get the color of an object.
 * \par Function Description
 *  Returns the colormap index of the color used to draw the GedaObject
 *  smob \a obj_s. Note that the color may not be meaningful for some
 *  object types.
 *
 * \note Scheme API: Implements the %object-color procedure in the
 *       (geda core object) module.
 *
 *  param [in] obj_s GedaObject smob to inspect.
 *
 * \return The colormap index used by \a obj_s.
 */
EDA_SCM_DEFINE (object_color, "%object-color", 1, 0, 0,
               (SCM obj_s), "Get the color of an object.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, scheme_object_color);

  GedaObject *obj = edascm_to_object (obj_s);
  return scm_from_int (obj->color);
}

/*!
 * \brief Set the color of an object.
 * \par Function Description
 *  Set the colormap index of the color used to draw the GedaObject smob
 *  \a obj_s to \a color_s. Note that the color may not be meaningful
 *  for some object types.
 *
 * \note Scheme API: Implements the %set-object-color! procedure in
 *       the (geda core object) module.
 *
 *  param obj_s   GedaObject smob to modify.
 *  param color_s new colormap index to use for \a obj_s.
 *
 * \return the modified \a obj_s.
 */
EDA_SCM_DEFINE (object_set_color_x, "%set-object-color!", 2, 0, 0,
               (SCM obj_s, SCM color_s), "Set the color of an object.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, scheme_object_set_color_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s,
              SCM_ARG2, scheme_object_set_color_x);

  GedaObject *obj = edascm_to_object (obj_s);

  geda_object_notify_emit_pre_change (obj);
  geda_set_object_color (obj, scm_to_int (color_s));
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Check Whether an Object is Locked.
 * \par Function Description
 *  Check the state of an object's selectable flag: if it is set, the
 *  object is considered to be unlocked, otherwise it is locked.
 *
 *  \note Scheme API: Implements the %object-selectable? procedure in
 *        the (geda core object) module.
 *
 * param obj_s  #GedaObject smob to inspect.
 *
 * \return Boolean value indicating whether \a obj_s is selectable.
 */
EDA_SCM_DEFINE (object_selectable, "%object-selectable?", 1, 0, 0,
               (SCM obj_s), "Check whether an object is locked.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, scheme_object_selectable);

  GedaObject *obj = edascm_to_object (obj_s);

  return scm_from_bool (obj->selectable);
}

/*!
 * \brief Lock or Unlock an Object
 * \par Function Description
 *  Sets the object's selectable flag: locked objects cannot be selected.
 *
 * \note Scheme API: Implements the %set-object-selectable! procedure
 *       in the (geda core object) module.
 *
 * param obj_s         #GedaObject smob to modify.
 * param selectable_s  boolean: object's selectable flag.
 *
 * \return the object (\a obj_s).
 */
EDA_SCM_DEFINE (object_set_selectable_x, "%set-object-selectable!", 2, 0, 0,
               (SCM obj_s, SCM selectable_s), "Lock or unlock an object.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, scheme_object_set_selectable_x);
  SCM_ASSERT (scm_is_bool (selectable_s), selectable_s,
              SCM_ARG2, scheme_object_set_selectable_x);

  GedaObject *obj = edascm_to_object (obj_s);

  int selectable = scm_is_true (selectable_s);

  if (obj->selectable != selectable) {

    obj->selectable = selectable;

    geda_struct_object_set_page_changed (obj);
  }

  return obj_s;
}

/*!
 * \brief Get line parameters.
 * \par Function Description
 *  Retrieves the parameters of a line object. The return value is a
 *  list of parameters:
 *
 * -# X-coordinate of start of line
 * -# Y-coordinate of start of line
 * -# X-coordinate of end of line
 * -# Y-coordinate of end of line
 * -# Colormap index of color to be used for drawing the line
 *
 *  This function also works on net, bus and pin objects.  For pins,
 *  the start is the connectable point on the pin.
 *
 *  param line_s the line object to inspect.
 *
 * \return a list of line parameters.
 */
EDA_SCM_DEFINE (object_line_info, "%line-info", 1, 0, 0,
               (SCM line_s), "Get line parameters.")
{
  SCM_ASSERT ((edascm_is_object_type (line_s, OBJ_LINE) ||
               edascm_is_object_type (line_s, OBJ_NET)  ||
               edascm_is_object_type (line_s, OBJ_BUS)  ||
               edascm_is_object_type (line_s, OBJ_PIN)),
               line_s, SCM_ARG1, scheme_object_line_info);

  GedaObject *obj = edascm_to_object (line_s);

  SCM      x1 = scm_from_int (obj->line->x[0]);
  SCM      y1 = scm_from_int (obj->line->y[0]);
  SCM      x2 = scm_from_int (obj->line->x[1]);
  SCM      y2 = scm_from_int (obj->line->y[1]);
  SCM   color = scm_from_int (obj->color);

  /* Swap ends according to pin's whichend flag. */
  if ((obj->type == OBJ_PIN) && obj->pin->whichend) {
    SCM s;
    s = x1; x1 = x2; x2 = s;
    s = y1; y1 = y2; y2 = s;
  }

  return scm_list_n (x1, y1, x2, y2, color, SCM_UNDEFINED);
}

/*!
 * \brief Set line parameters.
 * \par Function Description
 *  Modifies a line object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-line! procedure in the (geda
 *       core object) module.
 *
 *  This function also works on net, bus and pin objects.  For pins,
 *  the start is the connectable point on the pin.
 *
 *  param line_s the line object to modify.
 *  param x1_s   the new x-coordinate of the start of the line.
 *  param y1_s   the new y-coordinate of the start of the line.
 *  param x2_s   the new x-coordinate of the end of the line.
 *  param y2_s   the new y-coordinate of the end of the line.
 *  param color  the colormap index of the color to be used for
 *               drawing the line.
 *
 * \return the modified line object.
 */
EDA_SCM_DEFINE (object_set_line_x, "%set-line!", 6, 0, 0,
            (SCM line_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM color_s),
            "Set line parameters.")
{
  SCM_ASSERT ((edascm_is_object_type (line_s, OBJ_LINE) ||
               edascm_is_object_type (line_s, OBJ_NET)  ||
               edascm_is_object_type (line_s, OBJ_BUS)  ||
               edascm_is_object_type (line_s, OBJ_PIN)),
               line_s, SCM_ARG1, scheme_object_set_line_x);

  SCM_ASSERT (scm_is_integer (x1_s),    x1_s,    SCM_ARG2, scheme_object_set_line_x);
  SCM_ASSERT (scm_is_integer (y1_s),    y1_s,    SCM_ARG3, scheme_object_set_line_x);
  SCM_ASSERT (scm_is_integer (x2_s),    x2_s,    SCM_ARG4, scheme_object_set_line_x);
  SCM_ASSERT (scm_is_integer (y2_s),    y2_s,    SCM_ARG5, scheme_object_set_line_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG6, scheme_object_set_line_x);

  GedaObject *obj = edascm_to_object (line_s);

  int x1 = scm_to_int (x1_s);
  int y1 = scm_to_int (y1_s);
  int x2 = scm_to_int (x2_s);
  int y2 = scm_to_int (y2_s);

  geda_object_notify_emit_pre_change (obj);

  switch (obj->type) {
  case OBJ_NET:
    geda_struct_conn_remove_object (obj); /* We may need to update connectivity. */
    geda_net_object_modify (obj, x1, y1, 0);
    geda_net_object_modify (obj, x2, y2, 1);
    geda_struct_conn_update_object (obj);
    break;

  case OBJ_PIN:
    geda_struct_conn_remove_object (obj);
    /* Swap ends according to pin's whichend flag. */
    geda_pin_object_modify (obj, x1, y1, obj->pin->whichend ? 1 : 0);
    geda_pin_object_modify (obj, x2, y2, obj->pin->whichend ? 0 : 1);
    geda_struct_conn_update_object (obj);
    break;

  case OBJ_LINE:
    geda_line_object_modify (obj, x1, y1, LINE_END1);
    geda_line_object_modify (obj, x2, y2, LINE_END2);
    break;

  case OBJ_BUS:
    geda_struct_conn_remove_object (obj);
    geda_bus_object_modify (obj, x1, y1, 0);
    geda_bus_object_modify (obj, x2, y2, 1);
    geda_struct_conn_update_object (obj);
    break;

  default:
    return line_s;
  }

  geda_set_object_color (obj, scm_to_int (color_s));
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed (obj);

  return line_s;
}

/*!
 * \brief Get the type of a pin object.
 * \par Function Description
 *  Returns a symbol describing the pin type of the pin object \a
 *  pin_s.
 *
 * \note Scheme API: Implements the %pin-type procedure in the (geda
 *       core object) module.
 *
 * \return the symbol 'pin or 'bus.
 */
EDA_SCM_DEFINE (object_pin_type, "%pin-type", 1, 0, 0,
            (SCM pin_s), "Get the type of a pin object.")
{
  SCM_ASSERT (edascm_is_object_type (pin_s, OBJ_PIN), pin_s,
              SCM_ARG1, scheme_object_pin_type);

  GedaObject *obj = edascm_to_object (pin_s);
  SCM result;

  switch (obj->pin->node_type) {
  case PIN_NET_NODE:
    result = net_sym;
    break;
  case PIN_BUS_NODE:
    result = bus_sym;
    break;
  default:
    scm_misc_error (scheme_object_pin_type,
                    _("Object ~A has invalid pin type."),
                    scm_list_1 (pin_s));
  }

  return result;
}

/*!
 * \brief Set box parameters.
 * \par Function Description
 *  Modifies a box object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-box! procedure in the (geda
 *       core object) module.
 *
 * param box_s  the box object to modify.
 * param x1_s   the new x-coordinate of the top left of the box.
 * param y1_s   the new y-coordinate of the top left of the box.
 * param x2_s   the new x-coordinate of the bottom right of the box.
 * param y2_s   the new y-coordinate of the bottom right of the box.
 * param color  the colormap index of the color to be used for
 *               drawing the box.
 *
 * \return the modified box object.
 */
EDA_SCM_DEFINE (object_set_box_x, "%set-box!", 6, 0, 0,
           (SCM box_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM color_s),
           "Set box parameters.")
{
  SCM_ASSERT (edascm_is_object_type (box_s, OBJ_BOX), box_s,
              SCM_ARG1, scheme_object_set_box_x);
  SCM_ASSERT (scm_is_integer (x1_s),    x1_s,    SCM_ARG2, scheme_object_set_box_x);
  SCM_ASSERT (scm_is_integer (y1_s),    y1_s,    SCM_ARG3, scheme_object_set_box_x);
  SCM_ASSERT (scm_is_integer (x2_s),    x2_s,    SCM_ARG4, scheme_object_set_box_x);
  SCM_ASSERT (scm_is_integer (y2_s),    y2_s,    SCM_ARG5, scheme_object_set_box_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG6, scheme_object_set_box_x);

  GedaObject *obj = edascm_to_object (box_s);
  geda_box_object_modify_all (obj,
                              scm_to_int (x1_s), scm_to_int (y1_s),
                              scm_to_int (x2_s), scm_to_int (y2_s));

  geda_set_object_color (obj, scm_to_int (color_s));

  geda_struct_object_set_page_changed (obj);

  return box_s;
}

/*!
 * \brief Get box parameters.
 * \par Function Description
 *  Retrieves the parameters of a box object. The return value is a
 *  list of parameters:
 *
 * -# X-coordinate of top left of box
 * -# Y-coordinate of top left of box
 * -# X-coordinate of bottom right of box
 * -# Y-coordinate of bottom right of box
 * -# Colormap index of color to be used for drawing the box
 *
 *  param box_s the box object to inspect.
 *
 * \return a list of box parameters.
 */
EDA_SCM_DEFINE (object_box_info, "%box-info", 1, 0, 0,
               (SCM box_s), "Get box parameters.")
{
  SCM_ASSERT (edascm_is_object_type (box_s, OBJ_BOX), box_s,
              SCM_ARG1, scheme_object_box_info);

  GedaObject *obj = edascm_to_object (box_s);

  return scm_list_n (scm_from_int (obj->box->upper_x),
                     scm_from_int (obj->box->upper_y),
                     scm_from_int (obj->box->lower_x),
                     scm_from_int (obj->box->lower_y),
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*!
 * \brief Set circle parameters.
 * \par Function Description
 *  Modifies a circle object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-circle! procedure in the
 *      (geda core object) module.
 *
 *  param circle_s the circle object to modify.
 *  param x_s    the new x-coordinate of the center of the circle.
 *  param y_s    the new y-coordinate of the center of the circle.
 *  param r_s    the new radius of the circle.
 *  param color  the colormap index of the color to be used for
 *               drawing the circle.
 *
 * \return the modified circle object.
 */
EDA_SCM_DEFINE (object_set_circle_x, "%set-circle!", 5, 0, 0,
               (SCM circle_s, SCM x_s, SCM y_s, SCM r_s, SCM color_s),
               "Set circle parameters")
{
  SCM_ASSERT (edascm_is_object_type (circle_s, OBJ_CIRCLE), circle_s,
              SCM_ARG1, scheme_object_set_circle_x);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, scheme_object_set_circle_x);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, scheme_object_set_circle_x);
  SCM_ASSERT (scm_is_integer (r_s),     r_s,     SCM_ARG4, scheme_object_set_circle_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG5, scheme_object_set_circle_x);

  GedaObject *obj = edascm_to_object (circle_s);
  geda_circle_object_modify (obj, scm_to_int(x_s), scm_to_int(y_s), CIRCLE_CENTER);
  geda_circle_object_modify (obj, scm_to_int(r_s), 0, CIRCLE_RADIUS);
  geda_set_object_color (obj, scm_to_int (color_s));

  geda_struct_object_set_page_changed ( obj);

  return circle_s;
}

/*!
 * \brief Get circle parameters.
 * \par Function Description
 *  Retrieves the parameters of a circle object. The return value is a
 *  list of parameters:
 *
 * -# X-coordinate of center of circle
 * -# Y-coordinate of center of circle
 * -# Radius of circle
 * -# Colormap index of color to be used for drawing the circle
 *
 *  param circle_s the circle object to inspect.
 *
 * \return a list of circle parameters.
 */
EDA_SCM_DEFINE (object_circle_info, "%circle-info", 1, 0, 0,
                (SCM circle_s), "Get circle parameters.")
{
  SCM_ASSERT (edascm_is_object_type (circle_s, OBJ_CIRCLE),
              circle_s, SCM_ARG1, scheme_object_circle_info);

  GedaObject *obj = edascm_to_object (circle_s);

  return scm_list_n (scm_from_int (obj->circle->center_x),
                     scm_from_int (obj->circle->center_y),
                     scm_from_int (obj->circle->radius),
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*!
 * \brief Set arc parameters.
 * \par Function Description
 *  Modifies a arc object by setting its parameters to new values.
 *
 * \note Scheme API: Implements the %set-arc! procedure in the
 *       (geda core object) module.
 *
 *  param arc_s         the arc object to modify.
 *  param x_s           the new x-coordinate of the center of the arc.
 *  param y_s           the new y-coordinate of the center of the arc.
 *  param r_s           the new radius of the arc.
 *  param start_angle_s the start angle of the arc.
 *  param arc_sweep_s   the start angle of the arc.
 *  param color_s       the colormap index of the color to be used for
 *                      drawing the arc.
 *
 * \return the modified arc object.
 */
EDA_SCM_DEFINE (object_set_arc_x, "%set-arc!", 7, 0, 0,
               (SCM arc_s, SCM x_s, SCM y_s, SCM r_s, SCM start_angle_s,
                SCM arc_sweep_s, SCM color_s),
               "Set arc parameters")
{
  SCM_ASSERT (edascm_is_object_type (arc_s, OBJ_ARC), arc_s,
              SCM_ARG1, scheme_object_set_arc_x);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, scheme_object_set_arc_x);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, scheme_object_set_arc_x);
  SCM_ASSERT (scm_is_integer (r_s),     r_s,     SCM_ARG4, scheme_object_set_arc_x);
  SCM_ASSERT (scm_is_integer (color_s), color_s, SCM_ARG7, scheme_object_set_arc_x);
  SCM_ASSERT (scm_is_integer (start_angle_s),
                                  start_angle_s, SCM_ARG5, scheme_object_set_arc_x);
  SCM_ASSERT (scm_is_integer (arc_sweep_s),
                                  arc_sweep_s, SCM_ARG6, scheme_object_set_arc_x);

  GedaObject *obj = edascm_to_object (arc_s);

  geda_object_notify_emit_pre_change (obj);

  geda_arc_object_modify (obj, scm_to_int(x_s), scm_to_int(y_s), ARC_CENTER);
  geda_arc_object_modify (obj, scm_to_int(r_s), 0, ARC_RADIUS);
  geda_arc_object_modify (obj, scm_to_int(start_angle_s), 0, ARC_START_ANGLE);
  geda_arc_object_modify (obj, scm_to_int(arc_sweep_s), 0, ARC_END_ANGLE);

  geda_set_object_color (obj, scm_to_int (color_s));

  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return arc_s;
}

/*!
 * \brief Get arc parameters.
 * \par Function Description
 *  Retrieves the parameters of a arc object. The return value is a
 *  list of parameters:
 *
 * -# X-coordinate of center of arc
 * -# Y-coordinate of center of arc
 * -# Radius of arc
 * -# Start angle of arc
 * -# End angle of arc
 * -# Colormap index of color to be used for drawing the arc
 *
 * \note Scheme API: Implements the %arc-info procedure in the
 *       (geda core object) module.
 *
 *  param arc_s the arc object to inspect.
 *
 * \return a list of arc parameters.
 */
EDA_SCM_DEFINE (object_arc_info, "%arc-info", 1, 0, 0, (SCM arc_s), "Get arc parameters.")
{
  SCM_ASSERT (edascm_is_object_type (arc_s, OBJ_ARC),
              arc_s, SCM_ARG1, scheme_object_arc_info);

  GedaObject *obj = edascm_to_object (arc_s);

  return scm_list_n (scm_from_int (obj->arc->x),
                     scm_from_int (obj->arc->y),
                     scm_from_int (obj->arc->radius),
                     scm_from_int (obj->arc->start_angle),
                     scm_from_int (obj->arc->arc_sweep),
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*!
 * \brief Set text parameters.
 * \par Function Description
 *  Modifies a text object by setting its parameters to new values.
 *
 *  The alignment \a align_s should be a symbol of the form "x-y" where
 *  x can be one of "lower", "middle", or "upper", and y can be one of
 *  "left", "center" or "right". \a show_s determines which parts of an
 *  attribute-formatted string should be shown, and should be one of
 *  the symbols "name", "value" or "both".
 *
 * \note Scheme API: Implements the %set-text! procedure in the
 *       (geda core object) module.
 *
 *  param text_s    the text object to modify.
 *  param x_s       the new x-coordinate of the anchor of the text.
 *  param y_s       the new y-coordinate of the anchor of the text.
 *  param align_s   the new alignment of the text on the anchor.
 *  param angle_s   the angle the text in degrees (0, 90, 180 or 270).
 *  param string_s  the new string to display.
 *  param size_s    the new text size.
 *  param visible_s the new text visibility (SCM_BOOL_T or SCM_BOOL_F).
 *  param show_s    the new attribute part visibility setting.
 *  param color_s   the colormap index of the color to be used for
 *                  drawing the text.
 *
 * \remark WEH: May need to rethink this, 10 parameters may be a bit
 *              much,even for Schema
 *
 * \return the modified text object.
 */
EDA_SCM_DEFINE (object_set_text_x, "%set-text!", 10, 0, 0,
               (SCM text_s, SCM x_s, SCM y_s, SCM align_s, SCM angle_s,
                SCM string_s, SCM size_s, SCM visible_s, SCM show_s, SCM color_s),
               "Set text parameters")
{
  SCM_ASSERT (edascm_is_object_type (text_s, OBJ_TEXT), text_s,
              SCM_ARG1, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_integer (x_s), x_s,
              SCM_ARG2, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_integer (y_s), y_s,
              SCM_ARG3, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_symbol (align_s),  align_s,
              SCM_ARG4, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s,
              SCM_ARG5, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_string (string_s), string_s,
              SCM_ARG6, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_integer (size_s),  size_s,   SCM_ARG7, scheme_object_set_text_x);

  SCM_ASSERT (scm_is_symbol (show_s),    show_s,
              9, scheme_object_set_text_x);
  SCM_ASSERT (scm_is_integer (color_s),  color_s,
              10, scheme_object_set_text_x);

  GedaObject *obj = edascm_to_object (text_s);

  /* Alignment. Sadly we can't switch on pointers. :-( */
  int align;
  if      (align_s == lower_left_sym)    { align = LOWER_LEFT;    }
  else if (align_s == middle_left_sym)   { align = MIDDLE_LEFT;   }
  else if (align_s == upper_left_sym)    { align = UPPER_LEFT;    }
  else if (align_s == lower_center_sym)  { align = LOWER_MIDDLE;  }
  else if (align_s == middle_center_sym) { align = MIDDLE_MIDDLE; }
  else if (align_s == upper_center_sym)  { align = UPPER_MIDDLE;  }
  else if (align_s == lower_right_sym)   { align = LOWER_RIGHT;   }
  else if (align_s == middle_right_sym)  { align = MIDDLE_RIGHT;  }
  else if (align_s == upper_right_sym)   { align = UPPER_RIGHT;   }
  else {
    scm_misc_error (scheme_object_set_text_x,
                    _("Invalid text alignment ~A."),
                    scm_list_1 (align_s));
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
    scm_misc_error (scheme_object_set_text_x,
                    _("Invalid text angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
  }

  /* Visibility */
  int visibility;
  if (scm_is_false (visible_s)) {
    visibility = INVISIBLE;
  } else {
    visibility = VISIBLE;
  }

  /* Name/value visibility */
  int show;

  if (show_s == name_sym) {
    show = SHOW_NAME;
  }
  else if (show_s == value_sym)
  {
    show = SHOW_VALUE;
  }
  else if (show_s == both_sym)
  {
    show = SHOW_NAME_VALUE;
  }
  else {
    scm_misc_error (scheme_object_set_text_x,
                    _("Invalid text name/value visibility ~A."),
                    scm_list_1 (show_s));
  }

  geda_object_notify_emit_pre_change (obj);

  /* Actually make changes */
  obj->text->x = scm_to_int (x_s);
  obj->text->y = scm_to_int (y_s);
  obj->text->alignment = align;
  obj->text->angle = angle;

  obj->text->size = scm_to_int (size_s);
  obj->visibility = visibility;
  obj->show_name_value = show;

  /* Color */
  geda_set_object_color (obj, scm_to_int (color_s));

  char *tmp = scm_to_utf8_string (string_s);

  geda_text_object_set_string (obj, tmp);

  free (tmp);

  geda_object_notify_emit_change (obj);

  return text_s;
}

/*!
 * \brief Get text parameters.
 * \par Function Description
 *  Retrieves the parameters of a text object. The return value is a
 *  list of parameters:
 *
 * -# X-coordinate of anchor of text
 * -# Y-coordinate of anchor of text
 * -# Alignment of text
 * -# Angle of text
 * -# The string contained in the text object
 * -# Size of text
 * -# Text visibility
 * -# Which part(s) of an text attribute are shown
 * -# Colormap index of color to be used for drawing the text
 *
 * \note Scheme API: Implements the %text-info procedure in the
 *       (geda core object) module.
 *
 *  param text_s the text object to inspect.
 *
 * \return a list of text parameters.
 */
EDA_SCM_DEFINE (object_text_info, "%text-info", 1, 0, 0,
               (SCM text_s), "Get text parameters.")
{
  SCM_ASSERT (edascm_is_object_type (text_s, OBJ_TEXT),
              text_s, SCM_ARG1, scheme_object_text_info);

  GedaObject *obj = edascm_to_object (text_s);
  SCM align_s, visible_s, show_s;

  switch (obj->text->alignment) {
  case LOWER_LEFT:    align_s = lower_left_sym;    break;
  case MIDDLE_LEFT:   align_s = middle_left_sym;   break;
  case UPPER_LEFT:    align_s = upper_left_sym;    break;
  case LOWER_MIDDLE:  align_s = lower_center_sym;  break;
  case MIDDLE_MIDDLE: align_s = middle_center_sym; break;
  case UPPER_MIDDLE:  align_s = upper_center_sym;  break;
  case LOWER_RIGHT:   align_s = lower_right_sym;   break;
  case MIDDLE_RIGHT:  align_s = middle_right_sym;  break;
  case UPPER_RIGHT:   align_s = upper_right_sym;   break;
  default:
    scm_misc_error (scheme_object_text_info,
                    _("Text object ~A has invalid text alignment ~A"),
                    scm_list_2 (text_s, scm_from_int (obj->text->alignment)));
  }

  switch (obj->visibility) {
  case VISIBLE:   visible_s = SCM_BOOL_T; break;
  case INVISIBLE: visible_s = SCM_BOOL_F; break;
  default:
    scm_misc_error (scheme_object_text_info,
                    _("Text object ~A has invalid visibility ~A"),
                    scm_list_2 (text_s, scm_from_int (obj->visibility)));
  }

  switch (obj->show_name_value) {
  case SHOW_NAME:       show_s = name_sym;  break;
  case SHOW_VALUE:      show_s = value_sym; break;
  case SHOW_NAME_VALUE: show_s = both_sym;  break;
  default:
    scm_misc_error (scheme_object_text_info,
                    _("Text object ~A has invalid text attribute visibility ~A"),
                    scm_list_2 (text_s, scm_from_int (obj->show_name_value)));
  }

  return scm_list_n (scm_from_int (obj->text->x),
                     scm_from_int (obj->text->y),
                     align_s,
                     scm_from_int (obj->text->angle),
                     scm_from_utf8_string (geda_text_object_get_string (obj)),
                     scm_from_int (obj->text->size),
                     visible_s,
                     show_s,
                     scm_from_int (obj->color),
                     SCM_UNDEFINED);
}

/*!
 * \brief Get objects that are connected to an object.
 * \par Function Description
 *  Returns a list of all objects directly connected to \a obj_s.  If
 *  \a obj_s is not included in a page, throws a Scheme error.  If \a
 *  obj_s is not a pin, net, bus, or complex object, returns the empty
 *  list.
 *
 * \note Scheme API: Implements the %object-connections procedure of
 *       the (geda core object) module.
 *
 *  param obj_s GedaObject smob for object to get connections for.
 *
 * \return a list of GedaObject smobs.
 */
EDA_SCM_DEFINE (object_connections, "%object-connections", 1, 0, 0,
               (SCM obj_s), "Get objects that are connected to an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s, SCM_ARG1, scheme_object_connections);

  GedaObject *obj = edascm_to_object (obj_s);

  if (geda_object_get_page (obj) == NULL) {
    scm_error (edascm_object_state_sym, scheme_object_connections,
               _("Object ~A is not included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  GList *list = geda_struct_conn_return_others (NULL, obj);
  SCM result = edascm_from_object_glist (list);
  g_list_free (list);

  return result;
}

/*!
 * \brief Get the complex object that contains an object.
 * \par Function Description
 *  Returns the complex object that contains the object \a obj_s.  If
 *  \a obj_s is not part of a component, returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %object-complex procedure of the
 *       (geda core object) module.
 *
 *  param obj_s GedaObject smob for object to get component of.
 *
 * \return the GedaObject smob of the containing component, or SCM_BOOL_F.
 */
EDA_SCM_DEFINE (object_complex, "%object-complex", 1, 0, 0,
           (SCM obj_s), "Get containing complex object of an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, scheme_object_complex);

  GedaObject *obj = edascm_to_object (obj_s);
  GedaObject *parent = geda_object_get_parent (obj);

  if (parent == NULL) return SCM_BOOL_F;

  return edascm_from_object (parent);
}

/*!
 * \brief Set the stroke properties of an object.
 * \par Function Description
 *  Updates the stroke settings of the object \a obj_s.  If \a obj_s is
 *  not a line, box, circle, arc, or path, throws a Scheme error.  The
 *  optional parameters \a space_s and \a length_s can be set to
 *  SCM_UNDEFINED if not required by the dash style \a dash_s.
 *
 * \note Scheme API: Implements the %object-stroke procedure in the
 *       (geda core object) module.
 *
 *  param obj_s object to set stroke settings for.
 *  param width_s new stroke width for \a obj_s.
 *  param cap_s new stroke cap style for \a obj_s.
 *  param dash_s new dash style for \a obj_s.
 *  param space_s dot/dash spacing for dash styles other than solid.
 *  param length_s dash length for dash styles other than solid or
 *                 dotted.
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_set_stroke_x, "%set-object-stroke!", 4, 2, 0,
               (SCM obj_s, SCM width_s, SCM cap_s, SCM dash_s, SCM space_s,
                SCM length_s), "Set the stroke properties of an object.")
{
  SCM_ASSERT ((edascm_is_object_type (obj_s, OBJ_LINE)   ||
               edascm_is_object_type (obj_s, OBJ_BOX)    ||
               edascm_is_object_type (obj_s, OBJ_CIRCLE) ||
               edascm_is_object_type (obj_s, OBJ_ARC)    ||
               edascm_is_object_type (obj_s, OBJ_PATH)),
              obj_s, SCM_ARG1, scheme_object_set_stroke_x);

  GedaObject *obj = edascm_to_object (obj_s);
  LINE_OPTIONS line_options;

  SCM_ASSERT (scm_is_integer (width_s), width_s,
              SCM_ARG2, scheme_object_set_stroke_x);
  SCM_ASSERT (scm_is_symbol (cap_s), cap_s,
              SCM_ARG3, scheme_object_set_stroke_x);
  SCM_ASSERT (scm_is_symbol (dash_s), dash_s,
              SCM_ARG4, scheme_object_set_stroke_x);

  line_options.line_width = scm_to_int (width_s);

  if      (scm_is_eq (cap_s, none_sym))   { line_options.line_end = END_NONE;   }
  else if (scm_is_eq (cap_s, square_sym)) { line_options.line_end = END_SQUARE; }
  else if (scm_is_eq (cap_s, round_sym))  { line_options.line_end = END_ROUND;  }
  else {
    scm_misc_error (scheme_object_set_stroke_x,
                    _("Invalid stroke cap style ~A."),
                    scm_list_1 (cap_s));
  }

  if      (scm_is_eq (dash_s, solid_sym))   { line_options.line_type = TYPE_SOLID;   }
  else if (scm_is_eq (dash_s, dotted_sym))  { line_options.line_type = TYPE_DOTTED;  }
  else if (scm_is_eq (dash_s, dashed_sym))  { line_options.line_type = TYPE_DASHED;  }
  else if (scm_is_eq (dash_s, center_sym))  { line_options.line_type = TYPE_CENTER;  }
  else if (scm_is_eq (dash_s, phantom_sym)) { line_options.line_type = TYPE_PHANTOM; }
  else {
    scm_misc_error (scheme_object_set_stroke_x,
                    _("Invalid stroke dash style ~A."),
                    scm_list_1 (dash_s));
  }

  switch (line_options.line_type) {
  case TYPE_DASHED:
  case TYPE_CENTER:
  case TYPE_PHANTOM:
    if (length_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_stroke_x,
                      _("Missing dash length parameter for dash style ~A."),
                      scm_list_1 (length_s));
    }
    SCM_ASSERT (scm_is_integer (length_s), length_s,
                SCM_ARG6, scheme_object_set_stroke_x);
    line_options.line_length = scm_to_int (length_s);

    /* This case intentionally falls through */
  case TYPE_DOTTED:
    if (space_s == SCM_UNDEFINED) {
      scm_misc_error (scheme_object_set_stroke_x,
                      _("Missing dot/dash space parameter for dash style ~A."),
                      scm_list_1 (space_s));
    }
    SCM_ASSERT (scm_is_integer (space_s), space_s,
                SCM_ARG5, scheme_object_set_stroke_x);
    line_options.line_space = scm_to_int (space_s);
   case TYPE_SOLID:
   case TYPE_ERASE:
     break;
  }

  geda_object_notify_emit_pre_change (obj);
  geda_set_object_line_options (obj, &line_options);
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Get the type of an object.
 * \par Function Description
 *  Returns a symbol describing the type of the GedaObject smob \a obj_s.
 *
 * \note Scheme API: Implements the %object-type procedure in the
 *       (geda core object) module.
 *
 *  param [in] obj_s an GedaObject smob.
 *
 * \return a Scheme symbol representing the object type.
 */
EDA_SCM_DEFINE (object_type, "%object-type", 1, 0, 0,
               (SCM obj_s), "Get an object smob's type")
{
  SCM result;

  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s, SCM_ARG1, scheme_object_type);

  GedaObject *obj = edascm_to_object (obj_s);

  switch (obj->type) {
    case OBJ_LINE:    result = line_sym;       break;
    case OBJ_NET:     result = net_sym;        break;
    case OBJ_BUS:     result = bus_sym;        break;
    case OBJ_BOX:     result = box_sym;        break;
    case OBJ_PICTURE: result = picture_sym;    break;
    case OBJ_CIRCLE:  result = circle_sym;     break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX: result = complex_sym;    break;
    case OBJ_TEXT:    result = text_sym;       break;
    case OBJ_PATH:    result = path_sym;       break;
    case OBJ_PIN:     result = pin_sym;        break;
    case OBJ_ARC:     result = arc_sym;        break;
    default:
      scm_misc_error (scheme_object_type, _("Object ~A has bad type '~A'"),
      scm_list_2 (obj_s,
                  scm_integer_to_char (scm_from_int (obj->type))));
  }

  return result;
}

/*!
 * \brief Get the internal id of an object.
 * \par Function Description
 *  Returns an internal id number of the #GedaObject smob \a obj_s.
 *
 * \note Scheme API: Implements the %object-id procedure in the
 *       (geda core object) module.
 *
 * param [in] obj_s an #GedaObject smob.
 *
 * \return a Scheme symbol representing the object type.
 */
EDA_SCM_DEFINE (object_id, "%object-id", 1, 0, 0,
               (SCM obj_s), "Get an object smob's id")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, scheme_object_id);

  GedaObject *obj = edascm_to_object (obj_s);

  return scm_from_int (obj->sid);
}

/*!
 * \brief Get the number of elements in a path.
 * \par Function Description
 *  Retrieves the number of path elements in the path object \a obj_s.
 *
 * \note Scheme API: Implements the %path-length procedure in the
 *       (geda core object) module.
 *
 *  param obj_s GedaObject smob for path object to inspect.
 *
 * \return The number of path elements in \a obj_s.
 */
EDA_SCM_DEFINE (object_path_length, "%path-length", 1, 0, 0,
            (SCM obj_s), "Get number of elements in a path object.")
{
  /* Ensure that the argument is a path object */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, scheme_object_path_length);

  GedaObject *obj = edascm_to_object (obj_s);
  return scm_from_int (obj->path->num_sections);
}

/*!
 * \brief Get one of the elements from a path.
 * \par Function Description
 *  Retrieves a path element at index \a index_s from the path object
 *  \a obj_s.  If \a index_s is not a valid index, raises a Scheme
 *  "out-of-range" error.
 *
 *  The return value is a list.  The first element in the list is a
 *  symbol indicating the type of path element ("moveto", "lineto",
 *  "curveto" or "closepath"), and the remainder of the list contains
 *  zero or more control point coordinates, depending on the type of
 *  path element.  Each element is evaluated relative to the current
 *  path position.
 *
 * - moveto: x and y coordinates of position to step to.
 * - lineto: x and y coordinates of straight line endpoint.
 * - curveto: coordinates of first Bezier control point; coordinates
 *   of second control point; and coordinates of curve endpoint.
 * - closepath: No coordinate parameters.
 *
 *  All coordinates are absolute.
 *
 * \note Scheme API: Implements the %path-ref procedure in the (geda
 *       core object) module.
 *
 *  param obj_s   GedaObject smob of path object to get element from.
 *  param index_s Index of element to retrieve from \a obj_s
 *
 * \return A list containing the requested path element data.
 */
EDA_SCM_DEFINE (object_path_ref, "%path-ref", 2, 0, 0,
           (SCM obj_s, SCM index_s),
           "Get a path element from a path object.")
{
  /* Ensure that the arguments are a path object and integer */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, scheme_object_path_ref);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, scheme_object_path_ref);

  GedaObject *obj = edascm_to_object (obj_s);
  int     idx = scm_to_int (index_s);

  /* Check index is valid for path */
  if ((idx < 0) || (idx >= obj->path->num_sections)) {
    scm_out_of_range (scheme_object_path_ref, index_s);
  }

  PATH_SECTION *section = &obj->path->sections[idx];

  switch (section->code) {
  case PATH_MOVETO:
  case PATH_MOVETO_OPEN:
    return scm_list_3 (moveto_sym,
                       scm_from_int (section->x3),
                       scm_from_int (section->y3));
  case PATH_LINETO:
    return scm_list_3 (lineto_sym,
                       scm_from_int (section->x3),
                       scm_from_int (section->y3));
  case PATH_CURVETO:
    return scm_list_n (curveto_sym,
                       scm_from_int (section->x1),
                       scm_from_int (section->y1),
                       scm_from_int (section->x2),
                       scm_from_int (section->y2),
                       scm_from_int (section->x3),
                       scm_from_int (section->y3),
                       SCM_UNDEFINED);
  case PATH_END:
    return scm_list_1 (closepath_sym);
  default:
    scm_misc_error (scheme_object_path_ref,
                    _("Path object ~A has invalid element type ~A at index ~A"),
                    scm_list_3 (obj_s, scm_from_int (section->code), index_s));
  }

}

/*!
 * \brief Remove an element from a path.
 * \par Function Description
 *  Removes the path element at index \a index_s from the path object
 *  \a obj_s. If \a index_s is not a valid index, raises a Scheme
 *  "out-of-range" error.
 *
 * \note Scheme API: Implements the %path-remove! procedure in the
 *       (geda core object) module.
 *
 *  param obj_s   GedaObject smob of path object to remove element from.
 *  param index_s Index of element to remove from \a obj_s.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_path_remove_x, "%path-remove!", 2, 0, 0,
           (SCM obj_s, SCM index_s),
           "Remove a path element from a path object.")
{

  /* Ensure that the arguments are a path object and integer */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, scheme_object_path_remove_x);
  SCM_ASSERT (scm_is_integer (index_s), index_s,
              SCM_ARG2, scheme_object_path_remove_x);

  GedaObject *obj = edascm_to_object (obj_s);
  int idx = scm_to_int (index_s);

  if ((idx < 0) || (idx >= obj->path->num_sections)) {
    /* Index is valid for path */
    scm_out_of_range (scheme_object_path_remove_x, index_s);

  }

  geda_object_notify_emit_pre_change (obj);

  if (idx + 1 == obj->path->num_sections) {

    /* Section is last in path */
    obj->path->num_sections--;

  }
  else {

    /* Remove section at index by moving all sections above index one
     * location down. */
    memmove (&obj->path->sections[idx], &obj->path->sections[idx+1],
             sizeof(PATH_SECTION) * (obj->path->num_sections - idx - 1));
    obj->path->num_sections--;
  }

  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Insert an element into a path.
 * \par Function Description
 *  Inserts a path element into the path object \a obj_s at index \a
 *  index_s.  The type of element to be inserted is specified by the
 *  symbol \a type_s, and the remaining optional integer arguments
 *  provide as many absolute coordinate pairs as are required by that
 *  element type:
 *
 * - "closepath" elements require no coordinate arguments;
 * - "moveto" and "lineto" elements require one coordinate pair, for
 *   the endpoint;
 * - "curveto" elements require the coordinates of the first control
 *   point, coordinates of the second control point, and coordinates
 *   of the endpoint.
 *
 *  If the index is negative, or is greater than or equal to the number
 *  of elements currently in the path, the new element will be appended
 *  to the path.
 *
 * \note Scheme API: Implements the %path-insert! procedure of the
 *       (geda core object) module.
 *
 *  param obj_s   GedaObject smob for the path object to modify.
 *  param index_s Index at which to insert new element.
 *  param type_s  Symbol indicating what type of element to insert.
 *  param x1_s    X-coordinate of first coordinate pair.
 *  param y1_s    Y-coordinate of first coordinate pair.
 *  param x2_s    X-coordinate of second coordinate pair.
 *  param y2_s    Y-coordinate of second coordinate pair.
 *  param x3_s    X-coordinate of third coordinate pair.
 *  param y3_s    Y-coordinate of third coordinate pair.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_path_insert_x, "%path-insert", 3, 6, 0,
            (SCM obj_s, SCM index_s, SCM type_s,
             SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM x3_s, SCM y3_s),
            "Insert a path element into a path object.")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s, SCM_ARG1, scheme_object_path_insert_x);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, scheme_object_path_insert_x);
  SCM_ASSERT (scm_is_symbol (type_s), type_s, SCM_ARG3, scheme_object_path_insert_x);

  GedaObject *obj  = edascm_to_object (obj_s);
  GedaPath   *path = obj->path;

  PATH_SECTION section = {0, 0, 0, 0, 0, 0, 0};

  /* Check & extract path element type. */
  if (type_s == closepath_sym) {
    section.code = PATH_END;
  }
  else if (type_s == moveto_sym) {
    section.code = PATH_MOVETO;
  }
  else if (type_s == lineto_sym) {
    section.code = PATH_LINETO;
  }
  else if (type_s == curveto_sym) {
    section.code = PATH_CURVETO;
  }
  else {
    scm_misc_error (scheme_object_path_insert_x,
                    _("Invalid path element type ~A."),
                    scm_list_1 (type_s));
  }

  /* Check the right number of coordinates have been provided. */
  switch (section.code) {
    case PATH_CURVETO:
      SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG4, scheme_object_path_insert_x);
      section.x1 = scm_to_int (x1_s);

      SCM_ASSERT (scm_is_integer (y1_s), y1_s, SCM_ARG5, scheme_object_path_insert_x);
      section.y1 = scm_to_int (y1_s);

      SCM_ASSERT (scm_is_integer (x2_s), x2_s, SCM_ARG6, scheme_object_path_insert_x);
      section.x2 = scm_to_int (x2_s);

      SCM_ASSERT (scm_is_integer (y2_s), y2_s, SCM_ARG7, scheme_object_path_insert_x);
      section.y2 = scm_to_int (y2_s);

      SCM_ASSERT (scm_is_integer (x3_s), x3_s, SCM_ARG8, scheme_object_path_insert_x);
      section.x3 = scm_to_int (x3_s);

      SCM_ASSERT (scm_is_integer (y3_s), y3_s, SCM_ARG9, scheme_object_path_insert_x);
      section.y3 = scm_to_int (y3_s);

      break;
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG4, scheme_object_path_insert_x);
      section.x3 = scm_to_int (x1_s);
      SCM_ASSERT (scm_is_integer (y1_s), y1_s, SCM_ARG5, scheme_object_path_insert_x);
      section.y3 = scm_to_int (y1_s);
      break;
    case PATH_END:
      break;
  }

  /* Start making changes */
  geda_object_notify_emit_pre_change (obj);

  /* Make sure there's enough space for the new element */
  if (path->num_sections == path->num_sections_max) {
    path->sections = g_realloc (path->sections,
                                (path->num_sections_max <<= 1) * sizeof(PATH_SECTION));
  }

  /* Move path contents to make a gap in the right place. */
  int idx = scm_to_int (index_s);

  if ((idx < 0) || (idx > path->num_sections)) {
    idx = path->num_sections;
  }
  else {
    memmove (&path->sections[idx+1], &path->sections[idx],
             sizeof(PATH_SECTION) * (path->num_sections - idx));
  }

  path->num_sections++;
  path->sections[idx] = section;

  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Get picture object parameters.
 * \par Function Description
 *  Retrieves the parameters of a picture object.  The return value is
 *  a list of parameters:
 *
 * -# Filename of picture.
 * -# X-coordinate of top left of picture.
 * -# Y-coordinate of top left of picture.
 * -# X-coordinate of bottom right of picture.
 * -# Y-coordinate of bottom right of picture.
 * -# Rotation angle.
 * -# Whether object is mirrored.
 *
 * \note Scheme API: Implements the %picture-info procedure in the
 *       (geda core object) module.
 *
 *  param obj_s the picture object to inspect.
 *
 * \return a list of picture object parameters.
 */
EDA_SCM_DEFINE (object_picture_info, "%picture-info", 1, 0, 0,
               (SCM obj_s), "Get picture object parameters")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PICTURE), obj_s,
              SCM_ARG1, scheme_object_picture_info);

  GedaObject *obj = edascm_to_object (obj_s);
  const char *filename = geda_picture_object_get_filename (obj);

  SCM filename_s = SCM_BOOL_F;

  if (filename != NULL) {
    filename_s = scm_from_utf8_string (filename);
  }

  return scm_list_n (filename_s,
                     scm_from_int (obj->picture->upper_x),
                     scm_from_int (obj->picture->upper_y),
                     scm_from_int (obj->picture->lower_x),
                     scm_from_int (obj->picture->lower_y),
                     scm_from_int (obj->picture->angle),
                     (obj->picture->mirrored ? SCM_BOOL_T : SCM_BOOL_F),
                     SCM_UNDEFINED);
}

/*!
 * \brief Set picture object parameters.
 * \par Function Description
 *  Sets the parameters of the picture object \a obj_s.
 *
 * \note Scheme API: Implements the %set-picture! procedure in the
 *       (geda core object) module.
 *
 *  param obj_s       the picture object to modify
 *  param x1_s  the new x-coordinate of the top left of the picture.
 *  param y1_s  the new y-coordinate of the top left of the picture.
 *  param x2_s  the new x-coordinate of the bottom right of the picture.
 *  param y2_s  the new y-coordinate of the bottom right of the picture.
 *  param angle_s     the new rotation angle.
 *  param mirror_s    whether the picture object should be mirrored.
 *
 * \return the modify \a obj_s.
 */
EDA_SCM_DEFINE (object_set_picture_x, "%set-picture!", 7, 0, 0,
           (SCM obj_s, SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM angle_s,
            SCM mirror_s), "Set picture object parameters")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PICTURE), obj_s,
              SCM_ARG1, scheme_object_set_picture_x);
  SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG2, scheme_object_set_picture_x);
  SCM_ASSERT (scm_is_integer (y1_s), x1_s, SCM_ARG3, scheme_object_set_picture_x);
  SCM_ASSERT (scm_is_integer (x2_s), x1_s, SCM_ARG4, scheme_object_set_picture_x);
  SCM_ASSERT (scm_is_integer (y2_s), x1_s, SCM_ARG5, scheme_object_set_picture_x);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s, SCM_ARG6, scheme_object_set_picture_x);

  GedaObject *obj = edascm_to_object (obj_s);

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
    scm_misc_error (scheme_object_set_picture_x,
                  _("Invalid picture angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
  }

  geda_object_notify_emit_pre_change (obj);

  obj->picture->angle = scm_to_int (angle_s);
  obj->picture->mirrored = scm_is_true (mirror_s);
  geda_picture_object_modify_all (obj,
                        scm_to_int (x1_s), scm_to_int (y1_s),
                        scm_to_int (x2_s), scm_to_int (y2_s));

  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Set a picture object's data from a vector.
 * \par Function Description
 *  Sets the image data for the picture object \a obj_s from the vector
 *  \a data_s, and set its \a filename.  If the contents of \a data_s
 *  could not be successfully loaded as an image, raises an error.  The
 *  contents of \a data_s should be image data encoded in on-disk
 *  format.
 *
 * \note Scheme API: Implements the %set-picture-data/vector!
 *       procedure in the (geda core object) module.
 *
 *  param obj_s       The picture object to modify.
 *  param data_s      Vector containing encoded image data.
 *  param filename_s  New filename for \a obj_s.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_set_picture_data_x, "%set-picture-data/vector!", 3, 0, 0,
           (SCM obj_s, SCM data_s, SCM filename_s),
            "Set a picture object's data from a vector.")
{
  SCM vec_s = scm_any_to_s8vector (data_s);

  /* Check argument types */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PICTURE), obj_s,
              SCM_ARG1, scheme_object_set_picture_data_x);
  SCM_ASSERT (scm_is_true (scm_s8vector_p (vec_s)), data_s, SCM_ARG2,
              scheme_object_set_picture_data_x);
  SCM_ASSERT (scm_is_string (filename_s), filename_s, SCM_ARG3,
              scheme_object_set_picture_data_x);

  scm_dynwind_begin (0);

  /* Convert vector to contiguous buffer */
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc; /* Assuming we have it */
  const scm_t_int8 *elt = scm_s8vector_elements (vec_s, &handle, &len, &inc);
  char *buf = GEDA_MEM_ALLOC (len);
  int i;

  scm_dynwind_unwind_handler (g_free, buf, SCM_F_WIND_EXPLICITLY);

  for (i = 0; i < len; i++, elt += inc) {
    buf[i] = (char) *elt;
  }
  scm_array_handle_release (&handle);

  bool status;
  GError *error = NULL;

  GedaObject *obj = edascm_to_object (obj_s);
  char *filename = scm_to_utf8_string (filename_s);
  scm_dynwind_unwind_handler (g_free, filename, SCM_F_WIND_EXPLICITLY);

  geda_object_notify_emit_pre_change (obj);

  status = geda_picture_object_set_from_buffer (obj, filename, buf, len, &error);

  if (!status) {
    scm_dynwind_unwind_handler ((void (*)(void *)) g_error_free, error,
                                SCM_F_WIND_EXPLICITLY);
    scm_misc_error (scheme_object_set_picture_data_x,
                    "Failed to set picture image data from vector: ~S",
                    scm_list_1 (scm_from_utf8_string (error->message)));
  }

  geda_struct_object_set_page_changed ( obj);

  geda_object_notify_emit_change (obj);

  scm_dynwind_end ();
  return obj_s;
}


/*!
 * \brief Translate an object.
 * \par Function Description
 *  Translates \a obj_s by \a dx_s in the x-axis and \a dy_s in the
 *  y-axis.
 *
 * \note Scheme API: Implements the %translate-object! procedure of the
 *       (geda core object) module.
 *
 *  param obj_s  GedaObject smob for object to translate.
 *  param dx_s   Integer distance to translate along x-axis.
 *  param dy_s   Integer distance to translate along y-axis.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_translate_x, "%translate-object!", 3, 0, 0,
           (SCM obj_s, SCM dx_s, SCM dy_s), "Translate an object.")
{
  /* Check argument types */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, scheme_object_translate_x);
  SCM_ASSERT (scm_is_integer (dx_s), dx_s,
              SCM_ARG2, scheme_object_translate_x);
  SCM_ASSERT (scm_is_integer (dy_s), dy_s,
              SCM_ARG3, scheme_object_translate_x);

  GedaObject *obj = edascm_to_object (obj_s);
  int dx = scm_to_int (dx_s);
  int dy = scm_to_int (dy_s);

  geda_object_notify_emit_pre_change (obj);
  geda_object_translate (obj, dx, dy);
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Rotate an object.
 * \par Function Description
 *  Rotates \a obj_s anti-clockwise by \a angle_s about the point
 *  specified by \a x_s and \a y_s.  \a angle_s must be an integer
 *  multiple of 90 degrees.
 *
 * \note Scheme API: Implements the %rotate-object! procedure of the
 *       (geda core object) module.
 *
 *  param obj_s    GedaObject smob for object to translate.
 *  param x_s      x-coordinate of centre of rotation.
 *  param y_s      y-coordinate of centre of rotation.
 *  param angle_s  Angle to rotate by.
 *
 * \return \a obj_s.
 */
EDA_SCM_DEFINE (object_rotate_x, "%rotate-object!", 4, 0, 0,
               (SCM obj_s, SCM x_s, SCM y_s, SCM angle_s),
               "Rotate an object.")
{
  /* Check argument types */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s, SCM_ARG1, scheme_object_rotate_x);
  SCM_ASSERT (scm_is_integer   (x_s), x_s, SCM_ARG2, scheme_object_rotate_x);
  SCM_ASSERT (scm_is_integer   (y_s), y_s, SCM_ARG3, scheme_object_rotate_x);
  SCM_ASSERT (scm_is_integer   (angle_s), angle_s, SCM_ARG4, scheme_object_rotate_x);

  GedaObject *obj = edascm_to_object (obj_s);
  int       x = scm_to_int (x_s);
  int       y = scm_to_int (y_s);
  int   angle = scm_to_int (angle_s);

  /* FIXME Work around horribly broken libgeda behaviour.  Some
   * libgeda functions treat a rotation of -90 degrees as a rotation
   * of +90 degrees, etc., which is not sane. */
  while (angle < 0) angle += 360;
  while (angle >= 360) angle -= 360;

  SCM_ASSERT (angle % 90 == 0, angle_s, SCM_ARG4, scheme_object_rotate_x);

  geda_object_notify_emit_pre_change (obj);
  geda_object_rotate (obj, x, y, angle);
  geda_object_notify_emit_change (obj);

  geda_struct_object_set_page_changed ( obj);

  return obj_s;
}

/*!
 * \brief Create the (geda core object) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core object) module. The module can
 * be accessed using (use-modules (geda core object)).
 */
static void
init_module_geda_core_object (void *nothing)
{
  /* Register the functions and symbols */
  #include "scheme_object.x"

  /* Define object character types */
  scm_c_define("OBJ_ARC",         SCM_MAKE_CHAR((unsigned char) OBJ_ARC));
  scm_c_define("OBJ_BOX",         SCM_MAKE_CHAR((unsigned char) OBJ_BOX));
  scm_c_define("OBJ_BUS",         SCM_MAKE_CHAR((unsigned char) OBJ_BUS));
  scm_c_define("OBJ_CIRCLE",      SCM_MAKE_CHAR((unsigned char) OBJ_CIRCLE));
  scm_c_define("OBJ_COMPLEX",     SCM_MAKE_CHAR((unsigned char) OBJ_COMPLEX));
  scm_c_define("OBJ_LINE",        SCM_MAKE_CHAR((unsigned char) OBJ_LINE));
  scm_c_define("OBJ_NET",         SCM_MAKE_CHAR((unsigned char) OBJ_NET));
  scm_c_define("OBJ_PATH",        SCM_MAKE_CHAR((unsigned char) OBJ_PATH));
  scm_c_define("OBJ_PICTURE",     SCM_MAKE_CHAR((unsigned char) OBJ_PICTURE));
  scm_c_define("OBJ_PIN",         SCM_MAKE_CHAR((unsigned char) OBJ_PIN));
  scm_c_define("OBJ_PLACEHOLDER", SCM_MAKE_CHAR((unsigned char) OBJ_PLACEHOLDER));
  scm_c_define("OBJ_TEXT",        SCM_MAKE_CHAR((unsigned char) OBJ_TEXT));

  /* Add them to the module's public definitions. */
  scm_c_export ("OBJ_ARC", "OBJ_BOX", "OBJ_BUS", "OBJ_CIRCLE",
                "OBJ_COMPLEX", "OBJ_LINE",  "OBJ_NET", "OBJ_PATH",
                "OBJ_PICTURE", "OBJ_PIN", "OBJ_PLACEHOLDER", "OBJ_TEXT",
                 NULL);

  /* Add the modules routines to public definitions. */
  scm_c_export (scheme_object_copy,
                scheme_object_mirror_x,
                scheme_object_rotate_x,
                scheme_object_translate_x,
                scheme_object_type,
                scheme_object_id,
                scheme_object_connections,
                scheme_object_complex,
                scheme_object_bounds,
                scheme_object_stroke,
                scheme_object_set_stroke_x,
                scheme_object_fill,
                scheme_object_set_fill_x,
                scheme_object_color,
                scheme_object_set_color_x,
                scheme_object_arc_info,
                scheme_object_set_arc_x,
                scheme_object_box_info,
                scheme_object_set_box_x,
                scheme_object_circle_info,
                scheme_object_set_circle_x,
                scheme_object_selectable,
                scheme_object_set_selectable_x,
                scheme_object_line_info,
                scheme_object_set_line_x,
                scheme_object_path_length,
                scheme_object_path_insert_x,
                scheme_object_path_ref,
                scheme_object_path_remove_x,
                scheme_object_picture_info,
                scheme_object_set_picture_x,
                scheme_object_set_picture_data_x,
                scheme_object_pin_type,
                scheme_object_text_info,
                scheme_object_set_text_x,
                NULL);

  edascm_init_make_object();
}

/*!
 * \brief Initialize the basic gEDA object manipulation procedures.
 * \par Function Description
 *  Registers some Scheme procedures for working with GedaObject
 *  smobs. Should only be called by edascm_init().
 */
void
edascm_init_object (void)
{
  /* Define the (geda core object) module */
  scm_c_define_module ("geda core object",
                       init_module_geda_core_object,
                       NULL);
}
