/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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

#include <gschem.h>
#include <geda_debug.h>

/* No special type for attributes */
/* You can only edit text attributes */

/* be sure in o_copy o_move o_delete you maintain the attributes */
/* delete is a bare, because you will have to unattach the other end */
/* and in o_save o_read as well */
/* and in o_select when selecting objects, select the attributes */

/* there needs to be a modifier (in struct.h, such as a flag) which
 * signifies that this is an attribute */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Copy all attributes select to the selection list.
 *
 *  \todo get a better name
 */
void o_attrib_add_selected(GschemToplevel *w_current, SELECTION *selection,
                           Object *selected)
{
  Object *a_current;
  GList *a_iter;
  GList *selected_objects = NULL;

  if (selection == NULL) {
    u_log_message(_("Internal Error Detected: <o_attrib_add_selected> selection == NULL\n"));
    return;
  }

  for (a_iter = selected->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = a_iter->data;

    /* make sure object isn't selected already */
    if (!a_current->selected) {
      o_selection_add (selection, a_current);
      selected_objects = g_list_prepend (selected_objects, a_current);
    }
  }

  if (selected_objects != NULL) {
    /* Run select-objects-hook */
    g_run_hook_object_list (w_current, "%select-objects-hook",
                            selected_objects);
    g_list_free (selected_objects);
  }
}

/*! \brief Remove invisible attributes of an object from the selection list.
 *  \par Function Description
 *
 *  Remove all invisible attributes attached to the given object
 *  from the selection list. If hidden text is being shown, this
 *  function returns immediately.
 *
 *  \param [in]     w_current  The GschemToplevel object.
 *  \param [in,out] selection  The SELECTION list to remove from.
 *  \param [in]     object     Selected Object whose invisible attributes to remove.
 */
void o_attrib_deselect_invisible (GschemToplevel *w_current,
                                  SELECTION      *selection,
                                  Object         *object)
{
  Object *a_current;
  GList *a_iter;

  if (selection == NULL) {
    BUG_MSG("selection == NULL");
    return;
  }

  if (Current_Page->show_hidden_text) {
    return;
  }

  for (a_iter = object->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = a_iter->data;

    if (a_current->selected && !o_get_is_visible(a_current)) {
      o_selection_remove (selection, a_current);
    }
  }
}

/*! \brief Add invisible attributes of an object to the selection list.
 *  \par Function Description
 *
 *  Add all invisible attributes attached to the given object
 *  to the selection list. If hidden text is being shown, this
 *  function returns immediately.
 *
 *  \param [in]     w_current  The GschemToplevel object.
 *  \param [in,out] selection  The SELECTION list to add to.
 *  \param [in]     object     Selected Object whose invisible attributes to add.
 */
void o_attrib_select_invisible (GschemToplevel *w_current,
                                SELECTION      *selection,
                                Object         *object)
{
  Object *a_current;
  GList  *a_iter;

  if (selection == NULL) {
    BUG_MSG("selection == NULL");
    return;
  }

  if (Current_Page->show_hidden_text) {
    return;
  }

  for (a_iter = object->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = a_iter->data;

    if (!a_current->selected && !o_get_is_visible(a_current)) {
      o_selection_add (selection, a_current);
    }
  }
}

/*! \brief Change visibility status of attribute object.
 *  \par Function Description
 *  This function toggles the visibility status of the attribute \a
 *  object and updates it. The object is erased or redrawn if
 *  necessary.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The attribute object.
 */
void o_attrib_toggle_visibility(GschemToplevel *w_current, Object *object)
{
  GedaToplevel *toplevel = w_current->toplevel;

  g_return_if_fail (object != NULL && object->type == OBJ_TEXT);

  if (object->visibility == VISIBLE) {

    /* Must hide before changing or libgedacairo will not redraw */
    o_invalidate (w_current, object);

    o_set_visibility (object, INVISIBLE);

    if (Current_Page->show_hidden_text) {
      o_invalidate (w_current, object);
    }

  }
  else {

    o_set_visibility (object, VISIBLE);

    o_text_recreate(object);
  }

  toplevel->page_current->CHANGED = 1;
}

/*! \brief Set what part of an attribute is shown.
 *  \par Function Description
 *  This function changes what part (name, value or both) of an
 *  attribute is shown by its attribute object. The attribute object
 *  is erased, updated and finally redrawn.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The attribute object.
 *  \param [in] show_name_value  The new display flag for attribute.
 */
void o_attrib_toggle_show_name_value(GschemToplevel *w_current,
                                     Object *object, int show_name_value)
{
  GedaToplevel *toplevel = w_current->toplevel;

  g_return_if_fail (object != NULL && object->type == OBJ_TEXT);

  o_invalidate (w_current, object);
  object->show_name_value = show_name_value;
  o_text_recreate(object);

  toplevel->page_current->CHANGED = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * \note This function no longer returns NULL, instead the new object,
 * aka text item, is always return */
Object *o_attrib_add_attrib(GschemToplevel *w_current,
                            const char *text_string, int visibility,
                            int show_name_value, Object *parent)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Page *page;
  int world_x, world_y;
  int align;
  int angle;
  int color;
  int left, right, top, bottom;

  Object *new_obj;

  world_x = -1;
  world_y = -1;

  /* change later of needed */
  align   = LOWER_LEFT;
  angle   = 0;

  /* creating a toplevel or unattached attribute */
  if (parent) {

    color = ATTRIBUTE_COLOR;

    /* get coordinates of where to place the text object */
    switch(parent->type) {
      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        world_x = parent->complex->x;
        world_y = parent->complex->y;
        break;

      case(OBJ_ARC):
        world_x = parent->arc->x;
        world_y = parent->arc->y;
        break;

      case(OBJ_CIRCLE):
        world_x = parent->circle->center_x;
        world_y = parent->circle->center_y;
        break;

      case(OBJ_BOX):
        world_x = parent->box->upper_x;
        world_y = parent->box->upper_y;
        break;

      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_PIN):
      case(OBJ_BUS):
      {
        int dx = parent->line->x[1] - parent->line->x[0];
        int dy = parent->line->y[1] - parent->line->y[0];

        if (dy == 0) {
          if (dx > 0) {
            world_x = parent->line->x[0] + ( 2 * DEFAULT_ATTRIBUTE_OFFSET);
            world_y = parent->line->y[0] + DEFAULT_ATTRIBUTE_OFFSET;
          }
          else {
            world_x = parent->line->x[0] - ( 2 * DEFAULT_ATTRIBUTE_OFFSET);
            world_y = parent->line->y[0] + DEFAULT_ATTRIBUTE_OFFSET;

            align = LOWER_RIGHT;
          }
        }
        else if (dx == 0) {
          if (dy > 0) {
            world_x = parent->line->x[0] - DEFAULT_ATTRIBUTE_OFFSET;
            world_y = parent->line->y[0] + ( 2 * DEFAULT_ATTRIBUTE_OFFSET);

            angle = 90;
          }
          else {
            world_x = parent->line->x[0] - DEFAULT_ATTRIBUTE_OFFSET;
            world_y = parent->line->y[0] - ( 2 * DEFAULT_ATTRIBUTE_OFFSET);

            align = LOWER_RIGHT;
            angle = 90;
          }
        }
        else {
          world_x = parent->line->x[0];
          world_y = parent->line->y[0];
        }
      }
      break;

      case(OBJ_TEXT):
        world_x = parent->text->x;
        world_y = parent->text->y;
        color = DETACHED_ATTRIBUTE_COLOR;
        parent = NULL;
        break;
    }
  }
  else {

    color = DETACHED_ATTRIBUTE_COLOR;

    world_get_object_glist_bounds (s_page_get_objects (toplevel->page_current),
                                   &left, &top, &right, &bottom);

    /* this really is the lower left hand corner */
    world_x = left;
    world_y = top;

    /* printf("%d %d\n", world_x, world_y); */
  }

  /* first create text item */
  new_obj = o_text_new(color, world_x, world_y,
                       align, angle, text_string, /* zero is angle */
                       w_current->text_size, /* current text size */
                       visibility, show_name_value);


  /* now attach the attribute to the object (if parent is not NULL) */
  /* remember that parent contains the object to get the attribute */
  if (parent) {
    if (parent->page) {
      page = parent->page;
    }
    else {
      page = toplevel->page_current;
    }
    o_attrib_attach (new_obj, parent, FALSE);
  }
  else {
    page = toplevel->page_current;
    o_selection_add (Current_Selection, new_obj);
  }

  s_page_append_object (page, new_obj);

  /* handle slot= attribute, it's a special case */
  if (parent != NULL &&
    g_ascii_strncasecmp (text_string, "slot=", 5) == 0) {
    o_slot_end (w_current, parent, text_string);
  }

  /* Call add-objects-hook. */
  g_run_hook_object (w_current, "%add-objects-hook", new_obj);
  g_run_hook_object (w_current, "%select-objects-hook", new_obj);

  page->CHANGED = 1;

  return new_obj;
}
