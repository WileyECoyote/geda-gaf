/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2003-2016 Stuart D. Brorson.
 * Copyright (C) 2003-2016 gEDA Contributors (see ChangeLog for details)
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

/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions for manipulating Objects.
 * \par
 *  This file holds functions involved in manipulating the Object data
 *  structure.  Object is defined in libgeda.  An Object is a graphical
 *  primitive normally used in gschem.  Example Objects: some text,
 *  a component (complex), a pin, a line, etc.
 *
 *  The functions herein are functions which I wrote as wrappers to the
 *  fcns in libgeda.
 */

#include <gattrib.h>
#include <geda_debug.h>

/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define DEFAULT_TEXT_SIZE 10

/* ===================  Public Functions  ====================== */

/*------------------------------------------------------------------*/
/*!
 * \brief Add an attribute to an Object
 * \par Function Description
 *  This function adds a new attrib to o_current, when o_current is a
 *  component.
 *
 * \param toplevel         GedaToplevel structure
 * \param o_current        Pointer to object to add attribute to
 * \param new_attrib_name  Name of the attribute to add
 * \param new_attrib_value Value of the attribute to add
 * \param visibility       Is the attribute visible?
 * \param show_name_value  Control visibility of name and value.
 */
void
s_object_add_comp_attrib_to_object (GedaToplevel *toplevel,
                                    GedaObject   *o_current,
                                    char         *new_attrib_name,
                                    char         *new_attrib_value,
                                    int           visibility,
                                    int           show_name_value)
{
  /* One last sanity check, then add attrib */
  if (strlen(new_attrib_value) != 0) {

    char *name_value_pair;

    name_value_pair = geda_strconcat(new_attrib_name, "=",
                                     new_attrib_value, NULL);

    s_object_attrib_add_attrib_in_object (toplevel,
                                          name_value_pair,
                                          visibility,
                                          show_name_value, -1,
                                          o_current);
  }

  return;
}

/*------------------------------------------------------------------*/
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void
s_object_add_net_attrib_to_object (GedaToplevel *toplevel,
                                   GedaObject   *o_current,
                                   char         *new_attrib_name,
                                   char         *new_attrib_value)
{
  /* TBD */
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add a new attribute to an pin Object
 * \par Function Description
 *  This function adds a new text attribute to o_current. The function
 *  sets the color of the attribute to TEXT_COLOR if the name string
 *  is "pinlabel".
 *
 * \param toplevel         GedaToplevel structure
 * \param o_current        Pointer to pin object
 * \param new_attrib_name  Name of attribute to add
 * \param new_attrib_value Value of attribute to add
 *
 * \todo Do I really need separate fcns for comps, nets, and pins???
 */
void
s_object_add_pin_attrib_to_object (GedaToplevel *toplevel,
                                   GedaObject   *o_current,
                                   char         *new_attrib_name,
                                   char         *new_attrib_value)
{
  /* One last sanity check */
  if (strlen(new_attrib_value) != 0) {

    char *name_value_pair;
    int   color;
    int   visible;

    name_value_pair = geda_strconcat(new_attrib_name, "=",
                                     new_attrib_value, NULL);

    if (strncmp(new_attrib_name, "pinlabel", 8) == 0) {
      color   = TEXT_COLOR;
      visible = VISIBLE;
    }
    else {

      color = -1;

      if (strncmp(new_attrib_name, "pinnumber", 8) == 0) {
        visible = VISIBLE;
      }
      else {
        visible = INVISIBLE;
      }
    }

    s_object_attrib_add_attrib_in_object (toplevel,
                                          name_value_pair,
                                          visible,
                                          SHOW_NAME_VALUE, color,
                                          o_current);
  }

  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Replace attribute value in object
 * \par Function Description
 *  Find the instance of attrib_name on o_current, and replace the
 *  value with the new_attrib_value.
 *
 *  \param toplevel         GedaToplevel object
 *  \param o_current        object to operate on
 *  \param new_attrib_name  name of attribute to replace
 *  \param new_attrib_value value to set attribute to
 *  \param visibility       set visibility of attribute
 *  \param show_name_value  set visibility of attribute name and value
 */
void
s_object_replace_attrib_in_object(GedaToplevel *toplevel,
                                  GedaObject   *o_current,
                                  char         *new_attrib_name,
                                  char         *new_attrib_value,
                                  int           visibility,
                                  int           show_name_value)
{
  GList *a_iter;
  char  *old_attrib_text;
  char  *old_attrib_name;

  a_iter = o_current->attribs;

  while (a_iter != NULL) {

    GedaObject *a_current = a_iter->data;

    if (a_current->type == OBJ_TEXT && a_current->text != NULL) {

      /* found an attribute? */

      /* may need to check more thoroughly here. . . . */
      old_attrib_text = geda_strdup(a_current->text->string);
      old_attrib_name = geda_strsplit(old_attrib_text, '=', 0);

      if (strcmp(old_attrib_name, new_attrib_name) == 0) {

        /* Update the attrib string with the new name and value */
        geda_attrib_object_set_value(a_current, new_attrib_name, new_attrib_value);

        if (visibility != LEAVE_VISIBILITY_ALONE)
          geda_set_object_visibility (a_current, visibility);

        if (show_name_value != LEAVE_NAME_VALUE_ALONE)
          a_current->show_name_value = show_name_value;

        GEDA_FREE(old_attrib_text);
        GEDA_FREE(old_attrib_name);
        return;     /* we are done -- leave. */
      }
      else {
        GEDA_FREE(old_attrib_text);
        GEDA_FREE(old_attrib_name);
      }  /* if (strcmp . . . . */
    } /* if (a_current . . . . */

    a_iter = g_list_next (a_iter);
  } /* wend */

  /* if we get here, it is because we have failed to find the attrib on the
   * component, this is an error condition. */
  fprintf(stderr, "%s: %s <%s>.", __func__, _("failed to find attribute"), new_attrib_name);
  fprintf(stderr, " %s\n", _("Exiting..."));
  return;
}


/*------------------------------------------------------------------*/
/*!
 * \brief Remove attribute from object
 * \par Function Description
 *  Remove an attribute from an object.
 *
 * \param toplevel        GedaToplevel structure
 * \param o_current       Object to remove attribute from
 * \param new_attrib_name Name of attribute to remove
 */
void
s_object_release_attrib_in_object (GedaToplevel *toplevel,
                                   GedaObject   *o_current,
                                   char         *new_attrib_name)
{
  GList      *a_iter;
  GedaObject *a_current;
  char *old_attrib_text;
  char *old_attrib_name;

  a_iter = o_current->attribs;

  while (a_iter != NULL) {

    a_current = a_iter->data;

    if (a_current->type == OBJ_TEXT && a_current->text != NULL) {  /* found an attribute */

      /* may need to check more thoroughly here. . . . */
      old_attrib_text = geda_strdup(a_current->text->string);
      old_attrib_name = geda_strsplit(old_attrib_text, '=', 0);

      if (strcmp(old_attrib_name, new_attrib_name) == 0) {

        /* The attribute has been found. Delete it and then return. */

#ifdef DEBUG
        printf("%s: removing attrib with name = %s\n", __func__, old_attrib_name);
#endif

        s_object_delete_text_object (toplevel, a_current);

        GEDA_FREE(old_attrib_text);
        GEDA_FREE(old_attrib_name);
        return;     /* we are done -- leave. */
      }
      GEDA_FREE(old_attrib_text);
      GEDA_FREE(old_attrib_name);
    }
    a_iter = g_list_next (a_iter);
  }

  /* if we get here, it's because we have failed to find the attrib on the component.
   * This is an error condition. */
  BUG_MSG("failed to find the attrib: ");
  fprintf(stderr, "%s\n", new_attrib_name);
}

/*------------------------------------------------------------------*/

/*!
 * \brief Attach attribute to object.
 * \par Function Description
 *  Attach the name=value pair to the Object "object". This function was
 *  re-used from gschem/src/o_attrib.c:o_attrib_add_attrib and hacked for
 *  gattrib.
 *
 * \param toplevel          GedaToplevel to operate on
 * \param text_string       The new attribute text
 * \param visibility        Either VISIBLE or INVISIBLE
 * \param show_name_value   SHOW_NAME_VALUE, SHOW_VALUE | SHOW_NAME
 * \param color             Color index or -1 to use ATTRIBUTE_COLOR | DETACHED_ATTRIBUTE_COLOR
 * \param parent            Parent object or NULL
 *
 * \returns TRUE if the attribute was added, else FALSE
 */
void
s_object_attrib_add_attrib_in_object (GedaToplevel *toplevel,
                                      char         *text_string,
                                      int           visibility,
                                      int           show_name_value,
                                      int           color,
                                      GedaObject   *parent)
{
  Page *page;
  int world_x, world_y;
  int align;
  int angle;
  int left, right, top, bottom;

  GedaObject *new_obj;

  world_x = -1;
  world_y = -1;

  /* change later if needed */
  align   = LOWER_LEFT;
  angle   = 0;

  page = geda_toplevel_get_current_page (toplevel);

  /* creating a toplevel or unattached attribute */
  if (parent) {

    color = color < 0 ? ATTRIBUTE_COLOR : color;

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
        world_y = parent->text->y - ( 2 * DEFAULT_ATTRIBUTE_OFFSET);
        color = DETACHED_ATTRIBUTE_COLOR;
        break;
    }
  }
  else {

    color = color < 0 ? DETACHED_ATTRIBUTE_COLOR : color;

    geda_object_get_bounds_list (geda_struct_page_get_objects (page),
                             &left, &top, &right, &bottom);

    /* this really is the lower left hand corner */
    world_x = left;
    world_y = top;

  }

  /* first create text item */
  new_obj = geda_text_object_new(color, world_x, world_y,
                                 align, angle,          /* zero is angle */
                                 DEFAULT_TEXT_SIZE,     /* default text size */
                                 visibility,            /* we did not check */
                                 show_name_value, text_string);

  /* Attach the new attribute to the object if parent is not NULL */
  /* remember that parent contains the object to get the attribute */
  if (parent) {
    geda_struct_object_add_child (parent, new_obj);
  }
  else {
    geda_struct_page_append_object (page, new_obj);
  }

  /* handle slot= attribute, it's a special case */
  if (parent != NULL &&
    geda_strncmpi (text_string, "slot=", 5) == 0) {
    geda_struct_slot_update_object (parent);
  }
  else if (parent && parent->selected) {
    geda_object_selection_add (toplevel->page_current->selection_list, new_obj);
  }
}

/*------------------------------------------------------------------*/
/*!
 * \brief Delete text object
 * \par Function Description
 *  Delete the text object pointed to by \a text_object.
 *
 * \param toplevel    GedaToplevel to be operated on
 * \param text_object text object to be deleted
 */
void
s_object_delete_text_object (GedaToplevel *toplevel, GedaObject *text_object)
{
  geda_struct_page_remove_object (toplevel->page_current, text_object);
  geda_struct_object_release (text_object);
}

/*------------------------------------------------------------------*/
/*!
 * \brief Ensure object has a symbol file
 * \par Function Description
 *  This verifies that the object has a non-null symbol filename.
 *
 * \returns 0 = valid symbol file, 1 = no symbol file found.
 */
int s_object_has_sym_file(GedaObject *object)
{
  return geda_complex_get_filename(object->complex) ? 0 : 1;
}
