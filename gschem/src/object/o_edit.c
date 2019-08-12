/* -*- C o_edit.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
/*!
 * \file o_edit.c
 * \brief Low-level module for general editing
 */

#include <gschem.h>
#include <geda/geda_stat.h>

#include <ctype.h>
#include <libgen.h>
#include <sys/types.h>

#include <geda_debug.h>

/** \defgroup Editing-Operations Editing Operations
 *  @{
 *
 *  \par This group contains routines for basic editing operations.
 */

/*!
 * \brief Add a Title-Block symbol to a Page
 * \par Function Description
 *  This function attempts to insert the symbol for the given title-block
 *  into the schematic.
 */
bool o_edit_add_titleblock (GschemToplevel *w_current, Page *page, const char *tblock)
{
  bool              result;
  char             *sym_file;
  const char       *ext;
  const CLibSymbol *clib;

  ext = tblock;

  while (*ext) ext++;
  ext = ext - 4;

  if (strcmp(ext, SYMBOL_FILE_DOT_SUFFIX)) {
    sym_file = geda_strconcat(tblock, SYMBOL_FILE_DOT_SUFFIX, NULL);
  }
  else {
    sym_file = g_strdup(tblock);
  }

  clib = geda_struct_clib_get_symbol_by_name (sym_file);

  if (clib != NULL) {

    GedaObject *object;

    object = geda_complex_object_new (w_current->toplevel, 0, 0, 0,
                                      FALSE, clib, sym_file, FALSE);

    geda_struct_page_append_object (page, object);

    result = TRUE;
  }
  else {
    result = FALSE;
  }

  GEDA_FREE(sym_file);

  return result;
}

/*  break with the tradition here and input a list.
 *
 *  \todo probably should go back and do the same for o_copy o_move
 *        o_delete...
 */
/*!
 * \brief What happens when user "edits" an object
 * \par Function Description
 *  Which is also what happens when user double-clicks on an object.
 *  Calls an editing function based on the type of object.
 *
 * \todo This function is wrong, the functions accepts a list but
 *  calls func with first member of list, but does not check other
 *  members in the list, see geda_list_is_homogeneous_objects and
 *  the object argument to editors with multiple objects of the same
 *  type should be NULL.
 */
void o_edit_objects (GschemToplevel *w_current, GList *list, int who)
{
  GedaObject *o_current;
  Page       *page;
  const char *str;
  bool        isSymbol;

  if (list == NULL) {
    i_status_action_stop(w_current);
    return;
  }

  o_current = (GedaObject*) list->data;

  if (o_current == NULL) {
    BUG_MSG("unexpected NULL\n");
    return;
  }

  page     = gschem_toplevel_get_current_page(w_current);
  isSymbol = geda_struct_page_is_symbol_file (page);

  /* for now deal with only the first item */
  switch(o_current->type) {

    case(OBJ_ARC):
      x_dialog_edit_arc_angle (w_current, o_current);
      break;

    case OBJ_BOX:
      x_dialog_edit_fill_type (w_current);
      break;

    case OBJ_CIRCLE:
    case (OBJ_LINE):
      x_dialog_edit_line_type (w_current);
      break;

    case(OBJ_COMPLEX):
      x_multiattrib_open (w_current);
      break;

    case(OBJ_PLACEHOLDER):
    case(OBJ_NET):
    case(OBJ_BUS):
      x_attrib_add_dialog(w_current, o_current);
      break;

    case(OBJ_PICTURE):
      o_picture_exchange_file(w_current, o_current);
      break;

    case(OBJ_PIN):
      if(isSymbol) {
        x_dialog_edit_pin_type(w_current);
      }
      else {
        x_multiattrib_open (w_current);
      }
      break;

    case(OBJ_TEXT):
      str = geda_text_object_get_string (o_current);
      if (geda_attrib_object_get_name_value (o_current, NULL, NULL) &&
        /* attribute editor only accept 1-line values for attribute */
        geda_object_get_num_text_lines (str) == 1)
      {
        x_attrib_edit_dialog(w_current, o_current);
      }
      else {
        o_text_edit(w_current, o_current);
      }
      break;
  }
}

/*!
 * \brief Lock Selected Objects
 * \par Function Description
 *  Locks an object by setting the selectable property to FALSE. The color
 *  is also set to the locked_color. This locks the entire selected list
 *  including components, but does NOT change the color of primitives of
 *  the components. This cannot be called recursively.
 */
void o_edit_lock_selection (GschemToplevel *w_current)
{
  Page  *page      = gschem_toplevel_get_current_page(w_current);
  GList *s_current = geda_list_get_glist(Current_Selection);

  while(s_current != NULL) {

    GedaObject *object = (GedaObject*) s_current->data;

    if (object) {

      int color;

      geda_object_set_selectable(object, FALSE);

      color = geda_object_get_color(object);

      if (color != LOCK_COLOR) {
        geda_object_set_locked_color(object, color);
      }

      geda_object_set_color(object, LOCK_COLOR);

      geda_page_set_changed(page, TRUE);
    }

    NEXT(s_current);
  }

  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  o_undo_savestate(w_current, UNDO_ALL);
  i_status_update_sensitivities(w_current);
}

/*!
 * \brief Unlock current selection
 * \par Function Description
 *  Sets each object in the "selection" selectable, if the object
 *  selectable is set, then restores the objects color from the
 *  locked color property otherwise the color is restored to the
 *  value returned from geda_object_color_get_default().
 *
 * \note this cannot be called recursively
 */
void o_edit_unlock_selection(GschemToplevel *w_current)
{
  Page  *page      = gschem_toplevel_get_current_page(w_current);
  GList *s_current = geda_list_get_glist(Current_Selection);

  while (s_current != NULL) {

    GedaObject *object = (GedaObject*) s_current->data;

    if (object) {

      geda_object_set_selectable(object, TRUE);

      if (object->locked_color != LOCK_COLOR && object->locked_color > 0)
        object->color           = object->locked_color;
      else object->color        = geda_object_color_get_default(object->type);
      geda_page_set_changed(page, TRUE);
    }
    NEXT(s_current);
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*!
 * \brief Mirror list of objects
 * \par Function Description
 *  Wrapper for libgeda::geda_object_list_mirror, performs gschem
 *  tasks to ensure members of \a list are redrawn, run hooks and
 *  calls undo state.
 */
void o_edit_mirror_world(GschemToplevel *w_current, int centerx, int centery, GList *list)
{
  if (list == NULL) {
    i_status_set_state(w_current, SELECT);
  }
  else {

    o_invalidate_list (w_current, list);

    geda_mirror_list(list, centerx, centery);

    /* Objects will be in redraw by ChangeNotifyFunc */

    /* Run mirror-objects-hook */
    g_hook_run_object_list (w_current, MIRROR_OBJECTS_HOOK, list);

    o_undo_savestate(w_current, UNDO_ALL);
  }
  i_status_action_stop(w_current);
}

/** \defgroup Edit-Offset Implementation for Offset editing operations
 *  @{
 *  \par
 *  The Offset action has two similar but distinct modes of operation. The
 *  first mode is the "Hot" mode, which must be initiated from the keyboard
 *  like all other "Hot" actions. The Hot mode copies everything currently
 *  selected to the current pointer position relative to the bounds of the
 *  selection and this allows angular offsets.
 *  In the second mode of operation the user was asked to supply an offset
 *  distance and is then prompted to select the side to offset, similar to
 *  mirroring, the selected objects are then copied the supplied distance
 *  in the chosen direction.
 *
 *  This groups is contains the following functions:
 *
 *      1. o_edit_offset_hot      called by i_cmd_do_offset
 *      2. o_edit_set_offset      helper called by o_edit_offset_world
 *      3. o_edit_offset_world    called by x_event_button_pressed
*/

/*!
 * \brief Offset selected objects Hot mode
 * \par Function Description
 *  Create a copy of selected objects at the pointer position and offset
 *  the pointer cursor the same amount.
 */
void o_edit_offset_hot(GschemToplevel *w_current, int x, int y, GList *list)
{
  if (list == NULL) {
    i_status_set_state(w_current, SELECT);
  }
  else {

    GList *new_list = NULL;

    int left, right, top, bottom;

    if (geda_object_get_bounds_list (list, &left, &top, &right, &bottom)) {
      /* Save the bottom left corner to data structure */
      w_current->first_wx = left;
      w_current->first_wy = bottom > top ? top : bottom;
    }

    geda_struct_place_set_place_list(w_current->toplevel, list);

    i_status_action_start(w_current);

    w_current->second_wx = x;
    w_current->second_wy = y;

    o_place_end (w_current, FALSE, &new_list, COPY_OBJECTS_HOOK);

    o_select_unselect_all(w_current);
    o_select_add_list(w_current, new_list);
    g_list_free(new_list);

    o_undo_savestate(w_current, UNDO_ALL);

    int dx = x + x - w_current->first_wx;
    int dy = y + y - w_current->first_wy;

    i_window_set_pointer_position (w_current, dx, dy);
  }
  i_status_action_stop(w_current);
}

/*!
 * \brief Determine where to Offset a list of objects
 * \par Function Description
 *  Sets toplevel coordinates if x,y is outside the bounds of the
 *  list of objects, first_w[x,y] is set to the lower left bounds.
 *  The second_w[x,y] varibles are set based on the relationship
 *  of x,y to the point nearest a temporary created based on the
 *  boundary of the objects in the list.
 */
static bool
o_edit_set_offset(GschemToplevel *w_current, GList *list, int x, int y)
{
  int valid;
  int left, right, top, bottom;

  if (geda_object_get_bounds_list (list, &left, &top, &right, &bottom)) {

    int ymin, ymax;  /* Blame it on ass-backwards X11 */

    if (bottom > top) {
      ymin = top;
      ymax = bottom;
    }
    else {
      ymin = bottom;
      ymax = top;
    }

    if (!geda_object_get_is_inside_region (left, ymin, right, ymax, x, y)) {

      int nx, ny;                /* Point of bound to Nearest Target */

      /* Save the bottom left corner to data structure */
      w_current->first_wx = left;
      w_current->first_wy = ymin;

     GedaObject *tmp = geda_box_new();

      tmp->box->upper_x = left;
      tmp->box->upper_y = ymin;
      tmp->box->lower_x = right;
      tmp->box->lower_y = ymax;

      valid = geda_box_object_get_nearest_point (tmp, x, y, &nx, &ny);

      g_object_unref(tmp);

      /* Lateral gets the tie */
      if (x < nx && nx == left) {
        w_current->second_wx = left - w_current->offset;
        w_current->second_wy = ymin;
      }
      else if (x > nx && nx == right) {
        w_current->second_wx = left + w_current->offset;
        w_current->second_wy = ymin;
      }
      else if (y > ny && ny == top) {
        w_current->second_wx = left;
        w_current->second_wy = ymin + w_current->offset;
      }
      else if (y < ny && ny == bottom) {
        w_current->second_wx = left;
        w_current->second_wy = ymin - w_current->offset;
      }
      else {
        valid = FALSE;
      }
    }
    else {
      valid = FALSE;
    }
  }
  else {
    valid = FALSE;
  }

  return valid;
}

/*!
 * \brief Offset selected objects a Preset Distance
 * \par Function Description
 *  Places a copy of selected objects at a preset offset in the direction
 *  of the pointer position. The new objects replaces the old objects as
 *  the selection if the SHIFT key is down when then list is placed.
 */
void o_edit_offset_world(GschemToplevel *w_current, int x, int y, GList *list)
{
  if (list == NULL) {
    i_status_set_state(w_current, SELECT);
  }
  else {

    if (o_edit_set_offset(w_current, list, x, y)) {

      geda_struct_place_set_place_list(w_current->toplevel, list);

      i_status_action_start(w_current);

      if (!w_current->SHIFTKEY) {

        o_place_end (w_current, FALSE, NULL, COPY_OBJECTS_HOOK);

      }
      else { /* Shift is down, place and move selection to new objects */

        GList *list = NULL;

        o_place_end (w_current, FALSE, &list, COPY_OBJECTS_HOOK);

        o_select_unselect_all(w_current);
        o_select_add_list(w_current, list);
        g_list_free(list);

      }

      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
  i_status_action_stop(w_current);
}

/** @} endgroup Edit-Offset */

/*!
 * \brief Rotate all objects in list.
 * \par Function Description
 *  Given an object <B>list</B>, and the center of rotation
 *  (<B>centerx</B>,<B>centery</B>, this function traverses all the selection
 *  list, rotating each object through angle <B>angle</B>.
 *  The list contains a given object and all its attributes
 *  (refdes, pinname, pinlabel, ...).
 *  There is a second pass to run the rotate hooks of non-simple objects,
 *  like pin or complex objects, for example.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] centerx    Center x coordinate of rotation.
 *  \param [in] centery    Center y coordinate of rotation.
 *  \param [in] angle      Angle to rotate the objects through.
 *  \param [in] list       The list of objects to rotate.
 */
void o_edit_rotate_world(GschemToplevel *w_current,
                         int centerx, int centery, int angle, GList *list)
{
  /* Is okay if user hits rotate and has nothing selected */
  if (list == NULL) {
    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_list (w_current, list);

  geda_rotate_list(list, centerx, centery, angle);

  o_invalidate_list (w_current, list);

  /* Run rotate-objects-hook */
  g_hook_run_object_list (w_current, ROTATE_OBJECTS_HOOK, list);

  /* Don't save the undo state if we are inside an action, in which
   * case that action is responsible for saving the undo state. For
   * example, this is could be called when rotating the selection
   * while moving, so the Move mode would save the undo state */
  if (!w_current->inside_action) {
    o_undo_savestate(w_current, UNDO_ALL);
  }

  i_status_action_stop(w_current);
}

/*!
 * \brief Gschem Set Object Selectable
 * \par Function Description
 *  This function sets the object selectability to the given
 *  state and handles changing the color index of the object
 *  by setting the color to LOCK_COLOR when locking or to
 *  object->locked_color when unlocking.
 *
 * \param [in] w_current    The GschemToplevel object
 * \param [in] object       The Object being modified
 * \param [in] state        Whether to set object selectable or not
 *
 * \returns TRUE if the Object was hit, otherwise FALSE.
 */
void o_edit_set_selectable(GschemToplevel *w_current, GedaObject *object, bool state)
{
  if (GEDA_IS_OBJECT(object)) {

    bool current_state = geda_object_get_is_selectable(object);

    if (current_state != state) {

      Page *page = gschem_toplevel_get_current_page(w_current);

      geda_object_set_selectable(object, state);

      if (!state) {

        /* lock */
        if (object->color != LOCK_COLOR) {
          object->locked_color = object->color;
        }
        object->color = LOCK_COLOR;
      }
      else {

        /* Unlock, note 0 is BACKGROUND_COLOR */
        if (object->locked_color != LOCK_COLOR && object->locked_color > 0)
        {
          object->color = object->locked_color;
        }
        else
        {
          object->color = geda_object_color_get_default(object->type);
        }
      }

      geda_page_set_changed(page, TRUE);
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}

/* This is a utility function to report the number of objects whose
 * visibility was changed, this function is called by:
 *
 *      o_edit_show_inherited_attrib,
 *      o_edit_show_hidden_attrib,
 *      o_edit_show_netnames
 */
static void log_visibility (int set_hidden, int set_visible)
{
  if (set_hidden > 0) {
    geda_log_q("%d %s\n", set_hidden, _("attributes were hidden"));
  }

  if (set_visible > 0) {
    geda_log_q("%d %s\n", set_visible, _("hidden attributes were revealed"));
  }
}

/*!
 * \brief Reveal Complexes Inheritied Attributes attached to
 * \par Function Description
 *  This function causes hidden text for inherited attributes to be redrawn,
 *  This is accomplished by setting the object visibility to 2, which results
 *  in geda_object_get_is_visible returning true to the renderer, but is not saved when
 *  the schematic is saved. The function returns a list object that were set
 *  to be displayed, which does not include objects set to invisible, since
 *  these would not be redrawn. Instead, when text.object.visibility is set to
 *  0, the area is redrawn immediately by calling o_invalidate_force.
 */
static GList*
o_edit_show_inherited_attrib (GschemToplevel *w_current,  const GList *o_list)
{
  GList *iter        = (GList*)o_list;
  GList *redraw      = NULL;
  int    set_hidden  = 0;
  int    set_visible = 0;

  while (iter != NULL) {

   GedaObject *o_current = (GedaObject*)iter->data;

    if (!geda_object_get_selectable(o_current)) {
      NEXT(iter);
      continue;
    }

    if (o_current->type == OBJ_COMPLEX) {

      GList  *iter2;

      for(iter2 = o_current->complex->prim_objs; iter2; NEXT(iter2)) {

       GedaObject *sub_obj = iter2->data;

        if (sub_obj->type == OBJ_TEXT) {

          int visibility;

          visibility = geda_object_get_visibility(sub_obj);

          if (visibility == INVISIBLE) {

            geda_object_set_visibility(sub_obj, 2);
            redraw = g_list_prepend(redraw, sub_obj);
            ++set_visible;

          }
          else if (visibility == 2) {

            geda_object_set_visibility(sub_obj, INVISIBLE);

            /* Since now invisible, renderer won't return a bounds, so... */
            o_invalidate_force(w_current, sub_obj);
            ++set_hidden;

          }
        }
      }
    }

    NEXT(iter);
  }

  log_visibility (set_hidden, set_visible);
  return redraw;
}

/*!
 * \brief Reveal Hidden Attributes attached to Complexes
 * \par Function Description
 *  This function causes hidden text for invisible attributes to be redrawn,
 *  This is accomplished by setting the object visibility to 2, which results
 *  in geda_object_get_is_visible returning true to the renderer, but is not saved when
 *  the schematic is saved. The function returns a list of objects that were
 *  set to be displayed, which does not include objects set to invisible, since
 *  these would not be redrawn. Instead, when text.object.visibility is set to
 *  INVISIBLE, the area is redrawn immediately by calling o_invalidate_force.
 */
static GList*
o_edit_show_hidden_attrib (GschemToplevel *w_current,  const GList *o_list)
{
  GList *iter        = (GList*)o_list;
  GList *redraw      = NULL;
  int    set_hidden  = 0;
  int    set_visible = 0;

  while (iter != NULL) {

    GedaObject *o_current = (GedaObject*)iter->data;

    if (!geda_object_get_selectable(o_current)) {
      NEXT(iter);
      continue;
    }

    if (o_current->type == OBJ_COMPLEX) {

      GList  *iter2;

      for(iter2 = o_current->complex->prim_objs; iter2; NEXT(iter2)) {

        GedaObject *sub_obj = iter2->data;

        if (sub_obj->type == OBJ_PIN) {

          GList  *pa_iter;

          for( pa_iter = sub_obj->attribs; pa_iter; NEXT(pa_iter)) {

            GedaObject *p_attrib = pa_iter->data;

            if (p_attrib->type == OBJ_TEXT) {

              int visibility;

              if(strncmp(p_attrib->text->string, "pinseq", 6) == 0)
                continue;

              visibility = geda_object_get_visibility(p_attrib);

              if (visibility == INVISIBLE) {
                geda_object_set_visibility(p_attrib, 2);
                redraw = g_list_prepend(redraw, p_attrib);
                ++set_visible;
              }
              else if (visibility == 2) {
                geda_object_set_visibility(p_attrib, INVISIBLE);
                /* Invisible set, renderer won't return a bounds, so... */
                o_invalidate_force(w_current, p_attrib);
                ++set_hidden;
              }
            }
          }
        }
      }
    }

    if (o_current->type == OBJ_TEXT) {

      int visibility;

      /* If the parent is not selectable then don't display this attribute */

      GedaObject *o_parent = geda_object_get_attached_to(o_current);

      if (o_parent && !geda_object_get_selectable(o_parent)) {
        NEXT(iter);
        continue;
      }

      visibility = geda_object_get_visibility(o_current);

      if (visibility == INVISIBLE) {
        geda_object_set_visibility(o_current, 2);
        redraw = g_list_prepend(redraw, o_current);
        ++set_visible;
      }
      else if (visibility == 2) {
        geda_object_set_visibility(o_current, INVISIBLE);
        /* Since now invisible, renderer won't return a bounds, so... */
        o_invalidate_force(w_current, o_current);
        ++set_hidden;
      }
    }
    NEXT(iter);
  }

  log_visibility (set_hidden, set_visible);
  return redraw;
}

/*!
 * \brief Toggle Visibility of Hidden Attributes
 * \par Function Description
 *  The function causes the object visibility property of all
 *  attribute text objects in the given list to be modified,
 *  based on the current visibility and the inherited flag.
 *  This a wrapper for o_edit_show_hidden_attrib.
 *
 * \sa o_edit_show_hidden_attrib
 */
bool o_edit_show_hidden (GschemToplevel *w_current, const GList *o_list, int inherited)
{
  bool  result = FALSE;
  Page *page;

  if (o_list != NULL) {

    GList *modified;

    if (inherited)
      modified = o_edit_show_inherited_attrib(w_current, o_list);
    else
      modified = o_edit_show_hidden_attrib(w_current, o_list);

    if (modified) {
       o_invalidate_list(w_current, modified);
       g_list_free(modified);
    }

    o_invalidate_list(w_current, (GList*)o_list);
    result = TRUE;
  }

  page = gschem_toplevel_get_current_page(w_current);

  page->show_hidden_text = ! page->show_hidden_text;

  if (page->show_hidden_text) {
    q_log_message(_("Hidden text is now visible\n"));
  }
  else {
    q_log_message(_("Hidden text is now invisible\n"));
  }
  return result;
}

/*!
 * \brief Toggle Visibility of Hidden Netname Attribute
 * \par Function Description
 *  The function modifies the object visibilty property of all
 *  netname attribute text objects in the given list, searching
 *  each complex for a netname attribute. If the visibility is
 *  <b>INVISIBLE</b> the value is set to 2 and vise-versa. If the text
 *  is VISIBLE the attribute is not modified.
 *
 * \sa o_edit_show_hidden_attrib
 *
 * \param w_current  Pointer to GschemToplevel object
 * \param o_list     Pointer to a GList of object to check for
 *                   netname attributes
 */
void o_edit_show_netnames (GschemToplevel *w_current, const GList *o_list)
{
  GList *iter        = (GList*)o_list;
  GList *redraw      = NULL;
  int    set_hidden  = 0;
  int    set_visible = 0;
  char  *name;
  char  *value;

  while (iter != NULL) {

   GedaObject *o_current = (GedaObject*)iter->data;

    if (o_current->type == OBJ_TEXT) {

      GedaObject *o_parent = geda_object_get_attached_to(o_current);

      /* If the parent is not selectable then don't display this attribute */
      if (o_parent && geda_object_get_selectable(o_parent)) {

        if (geda_attrib_string_get_name_value(o_current->text->string, &name, &value)) {

          if (strcmp(name, "netname") == 0) {

            int visibility;

            visibility = geda_object_get_visibility(o_current);

            if (visibility == INVISIBLE) {
              geda_object_set_visibility(o_current, 2);
              redraw = g_list_prepend(redraw, o_current);
              ++set_visible;
            }
            else if (visibility == 2) {

              geda_object_set_visibility(o_current, INVISIBLE);

              /* Since now invisible, renderer won't return a bounds, so... */
              o_invalidate_force(w_current, o_current);
              ++set_hidden;
            }
          }
          GEDA_FREE(name);
          GEDA_FREE(value);
        }
      }
    }
    else if (o_current->type == OBJ_COMPLEX) {

     GedaObject *a_current = geda_attrib_first_attrib_by_name (o_current, "netname");

      if ( a_current != NULL) {

        int visibility;

        visibility = geda_object_get_visibility(a_current);

        if (visibility == INVISIBLE) {
          geda_object_set_visibility(a_current, 2);
          redraw = g_list_prepend(redraw, a_current);
          ++set_visible;
        }
        else if (visibility == 2) {

          geda_object_set_visibility(a_current, INVISIBLE);

          /* Since now invisible, renderer won't return a bounds, so... */
          o_invalidate_force(w_current, a_current);
          ++set_hidden;
        }
      }
    }
    NEXT(iter);
  }

  log_visibility (set_hidden, set_visible);
  o_invalidate_list(w_current, (GList*)redraw);
}

GedaObject *last_o = NULL;
int skiplast;

/*!
 * \brief Find Text
 * \par Function Description
 *  Searches for the string given by \a stext in the strings of each
 *  text object in \a o_list and optionally in the hierarchy below when
 *  the SEARCH_DESCEND bit is set in \a flags. If the SEARCH_HIDDEN
 *  bit is set in \a flags, hidden text objects will be included in
 *  the search.
 *
 * \returns 0 if found, 1 if NULL was passed or there was an error,
 *          or 2 if the end of the list is reached.
 *
 * \todo Only descends into the first source schematic
 */
int o_edit_find_text (GschemToplevel *w_current, const GList *o_list,
                      const char     *stext,       int flags, int skip)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int   count            = 0;
  int   page_control     = 0;
  int   text_screen_height;

  const GList *iter;

  skiplast = skip;
  iter     = o_list;

  while (iter != NULL) {

   GedaObject *o_current = (GedaObject*)iter->data;

    if (o_current->type == OBJ_TEXT) {

      int visible = geda_object_get_is_visible (o_current);

      if (visible || flags & SEARCH_HIDDEN) {

        const char *str = geda_text_object_get_string (o_current);

        /* replaced strcmp with strstr to simplify the search */
        if (strstr (str, stext)) {

          if (!skiplast) {

            Page *page;

            int x1, y1, x2, y2;

            if (!visible) {
              geda_object_set_visibility(o_current, VISIBLE);
            }

            if (!geda_object_get_bounds (o_current, &x1, &y1, &x2, &y2)) {
              BUG_MSG("world object bounds returned FALSE");
              return 1;
            }

            page = gschem_toplevel_get_current_page(w_current);

            i_zoom_world_extents(w_current,
                                 geda_struct_page_get_objects (page),
                                 I_PAN_DONT_REDRAW);

            text_screen_height = SCREENabs (w_current, y2 - y1);

            /* this code will zoom/pan till the text screen height is about */
            /* 50 pixels high, perhaps a future enhancement will be to make */
            /* this number configurable */
            while (text_screen_height < 50) {
              i_zoom_world(w_current, ZOOM_IN_DIRECTIVE, DONTCARE, I_PAN_DONT_REDRAW);
              text_screen_height = SCREENabs (w_current, y2 - y1);
            }

            i_pan_world_general(w_current, page, o_current->text->x, o_current->text->y, 1, 0);

            last_o = o_current;
            if (!visible) {
              geda_object_set_visibility(o_current, 2);
            }
            break;
          }

          if (last_o == o_current) {
            skiplast = 0;
          }
        }          /* endif (strstr(o_current->text->string,stext)) */
      }            /* endif visible || flags & SEARCH_HIDDEN */
    }              /* endif (o_current->type == OBJ_TEXT) */

    if ((flags & SEARCH_DESCEND) && (o_current->type == OBJ_COMPLEX)) {

      char *attrib;

      attrib = geda_attrib_search_attached_by_name (o_current, "source", count);

      /* if above is null, then look inside symbol */
      if (attrib == NULL) {
        attrib = geda_attrib_search_inherited_by_name (o_current, "source", count);
      }

      /* Check if a source attribute was found */
      if (attrib) {

        char *current_filename = geda_utility_string_split(attrib, ',', 0);

        GEDA_FREE (attrib);

        if (current_filename != NULL) {

          Page *parent     = toplevel->page_current;
          Page *child_page = geda_struct_hierarchy_down_single(toplevel,
                                                               current_filename,
                                                               parent,
                                                               page_control,
                                                               HIERARCHY_NORMAL_LOAD,
                                                               NULL);
          GEDA_FREE(current_filename);

          if (child_page != NULL) {

            GList *children;
            int rv;

            x_window_setup_page(w_current, child_page, w_current->world_left,
                                                       w_current->world_right,
                                                       w_current->world_top,
                                                       w_current->world_bottom);

            geda_struct_page_goto (child_page);

            geda_object_notify_change_add (child_page,
                                (ChangeNotifyFunc) o_invalidate_object,
                                (ChangeNotifyFunc) o_invalidate_object, w_current);

            children = geda_struct_page_get_objects (child_page);

            i_zoom_world_extents(w_current, children, I_PAN_DONT_REDRAW);

            page_control = child_page->page_control;

            rv = o_edit_find_text (w_current, children, stext, flags, skiplast);

            if (!rv) {
              x_window_set_current_page (w_current, child_page);
              return 0;
            }
            else {
              geda_struct_page_goto (parent);
              x_window_set_current_page (w_current, parent);
            }
          }
        }
      }
    }

    NEXT(iter);

    if (iter == NULL) {
      return 2;
    }
  }

  return (iter == NULL);
}


/*!
 * \brief Hide Specified Text
 * \par Function Description
 *  Searches string component of Text Objects and calls for
 *  geda_set_object_visibility string matching the given text.
 */
void o_edit_hide_specific_text (GschemToplevel *w_current,
                                const GList    *o_list,
                                const char     *stext)
{
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {

   GedaObject *o_current = (GedaObject*)iter->data;

    if (o_current->type == OBJ_TEXT) {

      const char *str = geda_text_object_get_string (o_current);

      if (!strncmp (stext, str, strlen (stext))) {
        if (geda_object_get_is_visible (o_current)) {
          geda_set_object_visibility (o_current, INVISIBLE);
          geda_text_object_recreate(o_current);
        }
      }
    }
    NEXT(iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
  o_invalidate_all (w_current);
}

/*!
 * \brief Show Specified Text
 * \par Function Description
 *  Searches string component of Text Objects and calls for
 *  geda_set_object_visibility string matching the given text.
 */
void o_edit_show_specific_text (GschemToplevel *w_current,
                                const GList    *o_list,
                                const char     *stext)
{
  const GList  *iter;

  iter     = o_list;

  while (iter != NULL) {

   GedaObject *o_current = (GedaObject*)iter->data;

    if (o_current->type == OBJ_TEXT) {

      const char *str = geda_text_object_get_string (o_current);

      if (!strncmp (stext, str, strlen (stext))) {
        if (!geda_object_get_is_visible (o_current)) {
          geda_set_object_visibility (o_current, VISIBLE);
          geda_text_object_recreate(o_current);
        }
      }
    }
    NEXT(iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*!
 * \brief Snap Selection to current Grid Snap Size
 * \par Function Description
 *  Checks the position of each object in the \a list and translates
 *  objects found to be off the current grid snap.
 *
 * \todo Consider launching a dialog to call this routine and pass
 *       list of objects and snap the user wants to use. Maybe just
 *       hack the old Grid snap dialog and hack the menus since the
 *       dialog does not work well inside an action.
 *
 */
void o_edit_snap (GschemToplevel *w_current, const GList *object_list)
{
  const GList *iter = object_list;
  bool  modified    = FALSE;

  while (iter) {

    int cur_x, cur_y;

    GedaObject *object = (GedaObject*)iter->data;

    if (geda_object_get_position(object, &cur_x, &cur_y)) {

      int dx, dy;

      dx = snap_grid (w_current, cur_x) - cur_x;
      dy = snap_grid (w_current, cur_y) - cur_y;

      if (dx || dy) {
        geda_object_translate(object, dx, dy);
        modified = TRUE;
      }
    }

    NEXT(iter);
  }

  if (modified) {
    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*!
 * \brief Update a component
 * \par Function Description
 *  Updates \a o_current to the latest version of the symbol available
 *  in the symbol library, while preserving any attributes set in the
 *  current schematic. On success, returns the new Object which
 *  replaces \a o_current on the page; \a o_current is deleted. On
 *  failure, returns NULL, and \a o_current is left unchanged.
 *
 * \param [in]     w_current The GschemToplevel object.
 * \param [in,out] o_current The Object to be updated.
 *
 * \return the new Object that replaces \a o_current.
 *
 * \note This function retains attribute positions. If an attribute
 *  position was what changed between symbols versions and the user
 *  desires to update the positions of attributes then the user can
 *  use "Attributes/Reset Position" after updating the component.
 *
 * \todo Consider launching a dialog to call this routine and pass
 *       list of attribute names the user wants restored, the dialog
 *       could also have a check button to restore positions.
 */
GedaObject *o_edit_update_component (GschemToplevel *w_current,
                                     GedaObject     *o_current)
{
  GedaToplevel *toplevel;
  GedaObject   *o_new;
  GedaObject   *attr_old;
  Page   *page;
  GList  *new_attribs;
  GList  *old_attribs;
  GList  *iter;

  const  CLibSymbol *clib;

  const char *keepers[] = { "pinnumber",
                            "pinlabel",
                            "pinseq",
                            "pintype",
                            "symversion",
                            NULL };

  char *fname = geda_complex_get_filename(o_current->complex);

  g_return_val_if_fail (fname != NULL, NULL);

  toplevel = gschem_toplevel_get_geda_toplevel(w_current);
  page     = geda_object_get_page (o_current);

  /* Force symbol data to be reloaded from source */
  clib = geda_struct_clib_get_symbol_by_name (fname);
  geda_struct_clib_symbol_invalidate_data (clib);

  if (clib == NULL) {
    const char *log_msg1 = _("Could not find symbol");
    const char *log_msg2 = _("in library. Update failed");
    geda_log ("%s \"%s\" %s.\n", log_msg1, fname, log_msg2);
    return NULL;
  }
  else {
    geda_log_q ("%s \"%s\".\n", _("Updating symbol"), fname);
  }

  /* Unselect the old object. */
  geda_object_selection_remove (page->selection_list, o_current);

  int a = geda_complex_get_angle(o_current->complex);
  int x = geda_complex_get_x(o_current->complex);
  int y = geda_complex_get_y(o_current->complex);
  int m = geda_complex_get_is_mirror(o_current->complex);

  /* Create new object and set embedded */
  o_new = geda_complex_object_new (toplevel, x, y, a, m, clib, fname, 1);

  if (geda_complex_object_is_embedded (o_current)) {
    geda_object_embed (o_new);
  }

  new_attribs = geda_complex_object_promote_attribs (toplevel, o_new);

  /* Cull any attributes from new COMPLEX that are already attached to
   * old COMPLEX. Note that the new_attribs list is kept consistent by
   * setting GList data pointers to NULL if their Objects are culled.
   * At the end, the new_attribs list is updated by removing all list
   * items with NULL data. This is slightly magic, but works. */
  for (iter = new_attribs; iter != NULL; NEXT(iter)) {

   GedaObject *attr_new = iter->data;

    char *name;
    char *new_value;

    if (attr_new->type != OBJ_TEXT) {
      BUG_MSG("type is not OBJ_TEXT");
    }
    else {

      char *old_value;

      geda_attrib_object_get_name_value (attr_new, &name, &new_value);

      old_value = geda_attrib_search_attached_by_name (o_current, name, 0);

      if (old_value != NULL) {
        int index = 0;
        do {
          if ( strcmp(name, keepers[index]) == 0 ) {
            attr_old = geda_find_attrib_by_name (o_current->attribs, name, 0);
            geda_attrib_object_set_value (attr_old, name,  new_value);
            break;
          }
          index++;
        } while (keepers[index]);

        attr_old = geda_find_attrib_by_name (o_current->attribs, name, 0);
        if (attr_old != NULL && (attr_old->text->size != attr_new->text->size))
        {
          attr_old->dont_redraw = TRUE;
          o_invalidate_object (w_current, attr_old);
          attr_old->text->size = attr_new->text->size;
          attr_old->dont_redraw = FALSE;
        }

        geda_attrib_object_remove (&o_new->attribs, attr_new);
        geda_struct_object_release (attr_new);
        iter->data = NULL;
      }

      GEDA_FREE (name);
      GEDA_FREE (old_value);
      GEDA_FREE (new_value);
    }
  }
  new_attribs = g_list_remove_all (new_attribs, NULL);

  /* Detach attributes from old Object and attach to newGedaObject */
  old_attribs = g_list_copy (o_current->attribs);
  geda_attrib_object_detach_all (o_current);
  geda_attrib_object_attach_list (o_new, old_attribs, 1);
  g_list_free (old_attribs);

  /* Add new attributes to page */
  geda_struct_page_append_list (page, new_attribs);

  /* Update pinnumbers for current slot */
  geda_struct_slot_update_object (o_new);

  /* Replace old Object with newGedaObject */
  geda_struct_page_replace_object (page, o_current, o_new);
  geda_struct_object_release (o_current);

  /* Select newGedaObject */
  geda_object_selection_add (page->selection_list, o_new);

  /* A redraw attributes in case a property (size) was restored */
  o_invalidate_list (w_current, o_new->attribs);

  /* mark the page as modified */
  o_undo_savestate (w_current, UNDO_ALL);

  return o_new;
}

/** @} endgroup Editing-Operations */
