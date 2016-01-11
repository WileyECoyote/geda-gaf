/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2003-2015 Stuart D. Brorson.
 * Copyright (C) 2003-2015 gEDA Contributors (see ChangeLog for details)
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
 *
 * This file holds functions involved in manipulating the Object data
 * structure.  Object is defined in libgeda.  An Object is a graphical
 * primitive normally used in gschem.  Example Objects: some text,
 * a component (complex), a pin, a line, etc.
 *
 * The functions herein are functions which I wrote as wrappers to the
 * fcns in libgeda.
 */

#include <gattrib.h>
#include <geda_debug.h>

/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define DEFAULT_TEXT_SIZE 10

/* ===================  Public Functions  ====================== */

/*------------------------------------------------------------------*/
/*! \brief Add an attribute to an Object
 *
 * This fcn adds a new attrib to o_current, when o_current is a
 * component.  It does it in the following
 * way:
 * -# It creates an object -- "attrib_graphic" -- and fills it in.
 * -# It gets the position info from o_current's refdes attrib and
 *    calls s_object_attrib_add_attrib_in_object() to add position
 *    info and name=value string to attrib_graphic.
 * -# It calls o_attrib_add() to wrap attrib_graphic with (attribute
 *    Object )
 * \param toplevel GedaToplevel structure
 * \param o_current pointer to object to add attribute to
 * \param new_attrib_name name of the attribute to add
 * \param new_attrib_value value of the attribute to add
 * \param visibility Is the attribute visible?
 * \param show_name_value Control visibility of name and value.
 */
void
s_object_add_comp_attrib_to_object (GedaToplevel *toplevel,
                                    Object       *o_current,
                                    char         *new_attrib_name,
                                    char         *new_attrib_value,
                                    int           visibility,
                                    int           show_name_value)
{
  /* One last sanity check, then add attrib */
  if (strlen(new_attrib_value) != 0) {

    char *name_value_pair;

    name_value_pair = u_string_concat(new_attrib_name, "=",
                                      new_attrib_value, NULL);
    s_object_attrib_add_attrib_in_object (toplevel,
                                          name_value_pair,
                                          visibility,
                                          show_name_value,
                                          o_current);
  }

  return;

}

/*------------------------------------------------------------------*/
/*!
 * \todo This needs to be filled in.
 */
void
s_object_add_net_attrib_to_object (GedaToplevel *toplevel,
                                   Object       *o_current,
                                   char         *new_attrib_name,
                                   char         *new_attrib_value)
{
  /* TBD */
}

/*------------------------------------------------------------------*/
/*! \brief Add a new attribute to an pin Object
 *
 *  \par Function Description
 *
 *  This function adds a new attribute to o_current, when o_current is
 *  a pin using the following technique:
 *
 * -# creates an object -- "attrib_graphic" -- and fills it in.
 * -# gets the position info from o_current's refdes attrib and
 *    calls s_object_attrib_add_attrib_in_object() to add position
 *    info and name=value string to attrib_graphic.
 * -# calls o_attrib_add() to wrap attrib_graphic with (attribute Object )
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
                                   Object       *o_current,
                                   char         *new_attrib_name,
                                   char         *new_attrib_value)
{
  /* One last sanity check */
  if (strlen(new_attrib_value) != 0) {

    char *name_value_pair;

    name_value_pair = u_string_concat(new_attrib_name, "=",
                                      new_attrib_value, NULL);

    s_object_attrib_add_attrib_in_object (toplevel,
                                          name_value_pair,
                                          INVISIBLE,
                                          SHOW_NAME_VALUE,
                                          o_current);
  }

  return;
}

/*------------------------------------------------------------------*/
/*! \brief Replace attribute value in object
 *
 *  \par Function Description
 *
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
                                  Object       *o_current,
                                  char         *new_attrib_name,
                                  char         *new_attrib_value,
                                  int           visibility,
                                  int           show_name_value)
{
  GList *a_iter;
  char  *old_attrib_text;
  char  *old_attrib_name;
  char  *new_attrib_text;

  a_iter = o_current->attribs;

  while (a_iter != NULL) {

    Object *a_current = a_iter->data;

    if (a_current->type == OBJ_TEXT && a_current->text != NULL) {

      /* found an attribute? */

      /* may need to check more thoroughly here. . . . */
      old_attrib_text = u_string_strdup(a_current->text->string);
      old_attrib_name = u_string_split(old_attrib_text, '=', 0);

      if (strcmp(old_attrib_name, new_attrib_name) == 0) {

        /* create attrib=value text string & stuff it back into toplevel */
        new_attrib_text = u_string_concat(new_attrib_name, "=", new_attrib_value, NULL);
        GEDA_FREE(a_current->text->string);   /* remove old attrib string */
        a_current->text->string = u_string_strdup(new_attrib_text);   /* insert new attrib string */
        if (visibility != LEAVE_VISIBILITY_ALONE)
          o_set_visibility (a_current, visibility);
        if (show_name_value != LEAVE_NAME_VALUE_ALONE)
          a_current->show_name_value = show_name_value;
        GEDA_FREE(new_attrib_text);
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

  /* if we get here, it's because we have failed to find the attrib on the component.
   * This is an error condition. */
  fprintf(stderr,
          _("%s, we have failed to find the attrib %s on the component. Exiting...\n"),
            __func__, new_attrib_name);

  return;
}


/*------------------------------------------------------------------*/
/*!
 * \brief Remove attribute from object
 *
 * \par Function Description
 *
 *  Remove an attribute from an object.
 *
 * \param toplevel        GedaToplevel structure
 * \param o_current       Object to remove attribute from
 * \param new_attrib_name Name of attribute to remove
 */
void
s_object_release_attrib_in_object (GedaToplevel *toplevel,
                                   Object       *o_current,
                                   char         *new_attrib_name)
{
  GList *a_iter;
  Object *a_current;
  Object *attribute_object;
  char *old_attrib_text;
  char *old_attrib_name;

  a_iter = o_current->attribs;

  while (a_iter != NULL) {

    a_current = a_iter->data;

    if (a_current->type == OBJ_TEXT && a_current->text != NULL) {  /* found an attribute */

      /* may need to check more thoroughly here. . . . */
      old_attrib_text = u_string_strdup(a_current->text->string);
      old_attrib_name = u_string_split(old_attrib_text, '=', 0);

      if (strcmp(old_attrib_name, new_attrib_name) == 0) {

        /* We've found the attrib.  Delete it and then return. */

#ifdef DEBUG
        printf("%s: removing attrib with name = %s\n", __func__, old_attrib_name);
#endif

        attribute_object = a_current;
        s_object_delete_text_object_in_object (toplevel, attribute_object);

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
  BUG_MSG("failed to find the attrib");
  fprintf(stderr, "%s\n", new_attrib_name);
}

/*------------------------------------------------------------------*/
/*! \brief Attach attribute to object.
 *
 * \par Function Description
 *
 * Attach the name=value pair to the Object "object". This function was
 * re-used from gschem/src/o_attrib.c:o_attrib_add_attrib and hacked for
 * gattrib.
 *
 * \param toplevel          GedaToplevel to operate on
 * \param text_string
 * \param visibility
 * \param show_name_value
 * \param object
 *
 * \returns TRUE if the attribute was added, else FALSE
 */
bool
s_object_attrib_add_attrib_in_object (GedaToplevel *toplevel,
                                      char         *text_string,
                                      int           visibility,
                                      int           show_name_value,
                                      Object       *object)
{
  Object *o_current;
  Object *new_obj;
  int color;
  int left, right, top, bottom;
  int world_x = -1, world_y = -1;

  o_current = object;

  /* creating a toplevel or unattached attribute */
  if (o_current) {
    /* get coordinates of where to place the text object */
    switch (o_current->type) {
    case (OBJ_COMPLEX):
      world_x = o_current->complex->x;
      world_y = o_current->complex->y;
      color = ATTRIBUTE_COLOR;
      break;

    case (OBJ_NET):
      world_x = o_current->complex->x;
      world_y = o_current->complex->y;
      color = ATTRIBUTE_COLOR;
      break;

    default:
      fprintf(stderr, _("%s: trying to add attrib to non-complex or non-net!\n"), __func__);
      return FALSE;
    }
  }
  else {    /* This must be a floating attrib, but what is that !?!?!?!?!  */
    o_get_bounds_list (s_page_get_objects (toplevel->page_current),
                                   &left, &top, &right, &bottom);

    /* this really is the lower left hand corner */
    world_x = left;
    world_y = top;

    /* printf("%d %d\n", world_x, world_y); */
    color = DETACHED_ATTRIBUTE_COLOR;
  }

  /* first create text item */
#if DEBUG
  printf("===  %s: about to attach new text attrib with properties:\n", __func__);
  printf("     color = %d\n", color);
  printf("     text_string = %s \n", text_string);
  printf("     visibility = %d \n", visibility);
  printf("     show_name_value = %d \n", show_name_value);
#endif

  new_obj = o_text_new (color, world_x, world_y,
                        LOWER_LEFT, 0, /* zero is angle */
                        DEFAULT_TEXT_SIZE,
                        visibility, show_name_value,text_string);

  s_page_append_object(toplevel->page_current, new_obj);

  /* now toplevel->page_current->object_tail contains new text item */

  /* now attach the attribute to the object (if o_current is not NULL) */
  /* note that o_current contains the object to get the attribute */
  if (o_current) {
    o_attrib_attach (o_current, new_obj, FALSE);
  }

  o_selection_add (toplevel->page_current->selection_list, new_obj);

  return TRUE;
}

/*------------------------------------------------------------------*/
/*! \brief Delete text object
 *
 *  \par Function Description
 *
 *  Delete the text object pointed to by text_object. This function
 *  was shamelessly stolen from gschem/src/o_delete.c and hacked
 *  for gattrib by SDB.
 *
 *  \param toplevel    GedaToplevel to be operated on
 *  \param text_object text object to be deleted
 */
void
s_object_delete_text_object_in_object (GedaToplevel *toplevel,
                                       Object       *text_object)
{
  s_page_remove_object (toplevel->page_current, text_object);
  s_object_release (text_object);
}

/*------------------------------------------------------------------*/
/*! \brief Ensure object has a symbol file
 *
 *  \par Function Description
 *  This verifies that the object has a non-null symbol file.
 *
 * \returns 0 = valid symbol file, 1 = no symbol file found.
 */
int s_object_has_sym_file(Object *object)
{
  char *filename;

  filename = object->complex->filename;

  if (filename != NULL) {

#ifdef DEBUG
    printf("%s: object has sym file = %s.\n", __func__, filename);
#endif

    return 0;
  }
  else {

#ifdef DEBUG
    printf("%s:, found object with no attached symbol file.\n", __func__);
#endif

    return 1;
  }
}
