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

/*! \file o_attrib.c
 *  \brief utility functions for attributes
 *
 *  Attributes are normal text objects. An attribute is a text object
 *  that has a text string that is delimited by an equal "=" character.
 *  The part before the equal character is called <b>name</b> the
 *  part of the string behind the equal character is called <b>value</b>
 *
 *  Attributes are attached to <b>GedaObjects</b>. Each attribute has a reference
 *  to the object it is attached to. Each object that has attributes has a
 *  list of pionters to its attributes.
 *
 *  \image html geda_attrib_object_overview.png
 *  \image latex geda_attrib_object_overview.pdf "attribute overview" width=14cm
 *
 *  \note
 *  Be sure in o_copy o_move o_delete you maintain the attributes
 *  delete is a bare, because you will have to unattach the other end
 *  and in o_save o_read as well
 *  and in o_select when selecting objects, select the attributes
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda_priv.h>

/*! \brief Add an attribute to an existing attribute list.
 *  \par Function Description
 *  Add an attribute to an existing attribute list.
 *
 *  \param [in]  object The GedaObject that item is being added to.
 *  \param [in]  item   The attribute that is to be added to object.
 */
void
geda_attrib_object_add(GedaObject *object, GedaObject *item)
{
  /* Add link from item to attrib listing */
  item->attached_to = object;
  object->attribs   = g_list_append (object->attribs, item);
  geda_attrib_object_emit_changed (object);
}

/*! \brief Get List of Attributes Attached to GedaObject
 *  \par Function Description
 *  return the GedaObject-atrribs glist assocaiated with the given object.
 *
 *  \param [in]  object The GedaObject from which to get the attribute list.
 *
 *  \return List of attached attributes.
 */
GList*
geda_attrib_object_get_attached (const GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {
    return object->attribs;
  }
  return NULL;
}

/*! \brief Check whether a attrib is attached to another object
 *  \par Function Description
 *  This function checks whether the object \a attrib is attached to
 *  the \a object.
 *
 *  \param [in]  attrib     The attribute to be checket.
 *  \param [in]  object     The object where you want to add item as an attribute.
 *
 *  \return TRUE if attrib is an attribute of object, FALSE otherwise
 */
bool
geda_attrib_object_is_attached_to (const GedaObject *attrib, const GedaObject *object)
{
  if (attrib == NULL || object == NULL)
    return FALSE;

  if (attrib->attached_to == object)
    return TRUE;

  return FALSE;
}

/*! \brief Attach existing attribute to an object
 *
 *  \par Function Description
 *  Attach existing attribute to an object.
 *
 *  \param [out] object       The object where you want to add item as an attribute.
 *  \param [in]  attrib       The attribute to be added.
 *  \param [in]  set_color    Whether or not we should set the new attribute's color.
 */
void
geda_attrib_object_attach (GedaObject *object, GedaObject *attrib, int set_color)
{
  g_return_if_fail (attrib != NULL);
  g_return_if_fail (object != NULL);

  /* is the object already part of the list ? */
  if (g_list_find (object->attribs, attrib)) {
    fprintf(stderr, _("Attribute [%s] already attached\n"), attrib->text->string);
    return;
  }

  if (attrib->type != OBJ_TEXT) {
    fprintf(stderr, _("Attempt to attach non text object as an attribute!\n"));
    return;
  }

  if (attrib->attached_to != NULL && attrib->attached_to != object) {
   fprintf(stderr, _("Attempt to attach attribute [%s] to more than one object\n"),
                attrib->text->string);
    return;
  }

  geda_attrib_object_add (object, attrib);

  /* Only gets set if object is on a page */
  s_object_set_page_changed (object);

  if (set_color)
    o_set_color (attrib, ATTRIBUTE_COLOR);
}

/*! \brief Attach list of existing attributes to an object
 *
 *  \par Function Description
 *  Attach list of existing attributes to an object.
 *
 *  \param [out] object     The object where you want to add item as an attribute.
 *  \param [in]  attr_list  The list of attributes to be added.
 *  \param [in]  set_color  Whether or not we should set the new attribute's color.
 */
void
geda_attrib_object_attach_list (GedaObject *object, const GList *attr_list, int set_color)
{
  const GList *iter;

  for (iter = attr_list; iter != NULL; iter = iter->next)
    geda_attrib_object_attach (object, iter->data, set_color);
}

/*! \brief Detach an attribute from parent
 *
 *  \par Function Description
 *  Detaches \a attribute from the parent object.
 *  Currently in gschem, this would only apply to floating
 *  attributes because a non-floating can not be selected
 *  individually.
 *
 *  \param [in,out] attribute The Attribute to be detached.
 */
void geda_attrib_object_detach(GedaObject *attribute)
{
  if (attribute && attribute->attached_to != NULL) {

    Page   *page;
    GedaObject *parent;

    parent = attribute->attached_to;
    attribute->attached_to = NULL;

    o_set_color (attribute, DETACHED_ATTRIBUTE_COLOR);
    geda_attrib_object_emit_changed (attribute);

    parent->attribs = g_list_remove (parent->attribs, attribute);

    page = geda_object_get_page(parent);

    if (page && (GEDA_IS_PAGE(page))) {
      page->CHANGED = TRUE;
    }
  }
}

/*! \brief Detach all attribute items in a list
 *
 *  \par Function Description
 *  Detach all attributes from an object.
 *
 *  \param [in,out] object    The object whos attributes to detach.
 */
void geda_attrib_object_detach_all(GedaObject *object)
{
  if (object && object->attribs != NULL) {

    GList *a_iter;
    Page  *page;

    geda_attrib_object_freeze_hooks (object);

    for (a_iter = object->attribs; a_iter != NULL; NEXT (a_iter)) {

      GedaObject *attribute = a_iter->data;

      attribute->attached_to = NULL;
      o_set_color (attribute, DETACHED_ATTRIBUTE_COLOR);
      geda_attrib_object_emit_changed (object);
    }

    g_list_free (object->attribs);
    object->attribs = NULL;

    page = geda_object_get_page(object);

    if (page && (GEDA_IS_PAGE(page))) {
      page->CHANGED = TRUE;
    }
    geda_attrib_object_thaw_hooks (object);
  }
}

/*! \brief Create a new Attributes GedaObject
 *
 *  \par Function Description
 *  This function creates a new text object and attaches the
 *  attribute to the given parent object of the Parent argument
 *  is not NULL, if the parent object is on a page, then the
 *  new attribute will be appended to the same page.
 *
 *  \param [in] parent          GedaObject the new attribute is to be attached to.
 *  \param [in] name            Attribute name string.
 *  \param [in] value           Attribute value string.
 *  \param [in] visibility      Attribute GedaObject visibility.
 *  \param [in] show_name_value Show name value visibility flag.
 *
 *  \return [out] the new Text attribute GedaObject
 */
GedaObject *geda_attrib_object_new_attached(GedaObject *parent, const char *name,
                                              const char *value,
                                              int visibility,
                                              int show_name_value)
{
  int world_x, world_y;
  int align;
  int angle;
  int color;

  char   *text;
  GedaObject *new_obj;

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
    world_x = 0;
    world_y = 0;

  }

  /* first create text item */
  if ( name && value) {
    text = geda_utility_string_sprintf("%s=%s", name, value);
  }
  else if (name) {
    text = geda_utility_string_strdup(name);
  }
  else if (value) {
    text = geda_utility_string_strdup(value);
  }
  else {
    text = geda_utility_string_strdup("unknown=empty");
  }
  new_obj = o_text_new(color, world_x, world_y,
                       align, angle,               /* zero is angle */
                       DEFAULT_ATTRIBUTE_SIZE,     /* default text size */
                       visibility, show_name_value, text);

  /* attach the attribute to the object (if parent is not NULL) */
  if (parent) {
    if (parent->page) {
      s_page_append_object (parent->page, new_obj);
    }
    geda_attrib_object_attach (parent, new_obj, FALSE);
  }

  /* handle slot= attribute, it's a special case */
  if (parent != NULL &&
    g_ascii_strncasecmp (text, "slot=", 5) == 0) {
    s_slot_update_object (parent);
  }
  GEDA_FREE(text);
  return new_obj;
}

/*! \brief Print all attributes to a Postscript document
 *
 *  \par Function Description
 *  Print all attributes to a Postscript document.
 *
 *  \param [in] attributes  List of attributes to print.
 */
void
geda_attrib_object_print(const GList *attributes)
{
  const GList *a_iter = attributes;

  while (a_iter != NULL) {

    const GedaObject *attribute = a_iter->data;

    printf("Attribute points to: %s\n", attribute->name);

    if (attribute->text) {
      printf("\tText is: %s\n", attribute->text->string);
    }

    a_iter = a_iter->next;
  }
}

/*! \brief Remove an attribute item from an attribute list
 *
 *  \par Function Description
 *  This function removes the given attribute from an attribute list.
 *  This function should be used when detaching an attribute.
 *
 *  \param [in] list      The attribute list to remove attribute from.
 *  \param [in] remove    The GedaObject to remove from list.
 */
void
geda_attrib_object_remove(GList **list, GedaObject *remove)
{
  if (remove != NULL) {

    GedaObject *attached_to = remove->attached_to;

    remove->attached_to = NULL;

    *list = g_list_remove (*list, remove);

    geda_attrib_object_emit_changed (attached_to);
  }
  else
    BUG_MSG("can not remove NULL attribute");
}

/*! \brief Read attributes from a buffer.
 *  \par Function Description
 *  Read attributes from a TextBuffer.
 *
 *  \param [in]  toplevel               The GedaToplevel object.
 *  \param [in]  parent  GedaObject which gets these attribs.
 *  \param [in]  tb                     The text buffer to read from.
 *  \param [in]  release_ver            libgeda release version number.
 *  \param [in]  fileformat_ver         file format version number.
 *
 *  \param [out] err                    A GError object
 *
 *  \return GList of attributes read, or NULL on error.
 *
 *  \todo Bad Error recovery
 */
GList*
o_read_attribs (GedaToplevel *toplevel,
                GedaObject       *parent,
                TextBuffer   *tb,
                unsigned int  release_ver, unsigned int fileformat_ver,
                GError       ** err)
{
  GList      *object_list = NULL;
  GedaObject     *new_obj;
  const char *line = NULL;
  char        objtype;
  int         ATTACH=FALSE;

  while (1) {

    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);
    switch (objtype) {

      case(OBJ_LINE):
        if ((new_obj = o_line_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;


      case(OBJ_NET):
        if ((new_obj = o_net_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BUS):
        if ((new_obj = o_bus_read(line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BOX):
        if ((new_obj = geda_box_object_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        if ((new_obj = o_circle_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        if ((new_obj = o_complex_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = o_path_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PIN):
        if ((new_obj = o_pin_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_ARC):
        if ((new_obj = geda_arc_object_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_TEXT):
        new_obj = o_text_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;

        o_text_set_rendered_bounds_func (new_obj,
                                         toplevel->rendered_text_bounds_func,
                                         toplevel->rendered_text_bounds_data);


        if (parent->type == OBJ_PIN)
          o_pin_update_read_property(parent, new_obj);

        object_list = g_list_prepend (object_list, new_obj);
        ATTACH=TRUE;
        break;

      case(ENDATTACH_ATTR):
        return object_list;
        break;
    }

    if (ATTACH) {
      geda_attrib_object_attach (parent, new_obj, FALSE);
      ATTACH=FALSE;
    }
    else {
      fprintf(stderr, "Bad line:<%s>\n", line);
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("tried to attach a non-text object as an attribute"));
      goto error;
    }
  }

  /* The attribute list wasn't terminated, so it's a parse error! */
  g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
               _("unexpected end-of-file in attribute list"));

error:
  s_object_release_objects(object_list);
  return NULL;
}


/*! \brief Get name and value from an attribute 'name=value' string.
 *  \par Function Description
 *  This function parses the character string \a string expected to be
 *  an attribute string of the form 'name=value'.
 *
 *  It returns TRUE if function was able to parse the string into the
 *  name and value parts of an attribute. Otherwise the function returns
 *  FALSE, in that case \a *name_ptr and \a *value_ptr are set to NULL.
 *
 *  \a name_ptr and/or \a value_ptr can be NULL.
 *  If not NULL, the caller must GEDA_FREE these returned strings.
 *
 *  \note
 *  If you get an invalid attribute (improper) with a name and no
 *  value, then it is NOT an attribute. Also, there cannot be any
 *  spaces beside the equals sign
 *
 *  \param [in]  string     String to split into name/value pair.
 *  \param [out] name_ptr   The return location for the name, or NULL.
 *  \param [out] value_ptr  The return location for the value, or NULL.
 *
 *  \return TRUE on success, FALSE otherwise.
 */
bool
geda_attrib_object_string_get_name_value (const char *string,
                                      char **name_ptr, char **value_ptr)
{
  char *ptr, *prev_char, *next_char;

  if (name_ptr != NULL)
    *name_ptr = NULL;
  if (value_ptr != NULL)
    *value_ptr = NULL;

  g_return_val_if_fail (string != NULL, FALSE);

  ptr = g_utf8_strchr (string, -1, g_utf8_get_char ("="));
  if (ptr == NULL) {
    return FALSE;
  }

  prev_char = g_utf8_find_prev_char (string, ptr);
  next_char = g_utf8_find_next_char (ptr, NULL);

  if (prev_char == NULL || *prev_char == ' ' ||
      next_char == NULL || *next_char == ' ' || *next_char == '\0' ) {
    return FALSE;
  }

  if (name_ptr != NULL) {
    *name_ptr = g_strndup (string, (ptr - string));
  }

  if (value_ptr != NULL) {
    *value_ptr = geda_utility_string_strdup (next_char);
  }

  return TRUE;
}

/*! \brief Get name and value from an attribute GedaObject
 *  \par Function Description
 *  Calls geda_attrib_object_get_name_value to do the work
 *
 *  \param [in]  attrib     The attribute GedaObject whos name/value to return.
 *  \param [out] name_ptr   The return location for the name, or NULL.
 *  \param [out] value_ptr  The return location for the value, or NULL.
 *
 *  \return TRUE on success, FALSE otherwise.
 *
 *  \sa geda_attrib_object_string_get_name_value()
 */
bool
geda_attrib_object_get_name_value (const GedaObject *attrib, char **name_ptr, char **value_ptr)
{
  g_return_val_if_fail (attrib->type == OBJ_TEXT, FALSE);

  return geda_attrib_object_string_get_name_value (attrib->text->string,
                                         name_ptr, value_ptr);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_attrib_object_set_value (const GedaObject *attrib, const char *name_ptr, const char *value_ptr)
{

  GEDA_FREE(attrib->text->string);

  attrib->text->string = geda_utility_string_concat(name_ptr, "=", value_ptr, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_attrib_object_set_integer_value (const GedaObject *attrib, const char *name_ptr, int value)
{
  GEDA_FREE(attrib->text->string);
  attrib->text->string = geda_utility_string_sprintf("%s=%d", name_ptr, value, NULL);
}

/*! \brief Find all floating attributes in the given object list.
 *  \par Function Description
 *  Find all floating attributes in the given object list.
 *
 *  \param [in] list  GList of Objects to search for floating attributes.
 *
 *  \return GList of floating attributes from the input list
 *
 *  \note Caller must g_list_free returned list.
 */
GList*
geda_attrib_object_find_floating (const GList *list)
{
  GList *floating_attributes = NULL;
  const  GList *iter;

  for (iter = list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    /* Skip non text objects, attached attributes and text which doesn't
     * constitute a valid attributes (e.g. general text placed on the page)
     */
    if (o_current->type == OBJ_TEXT &&
        o_current->attached_to == NULL &&
        geda_attrib_object_get_name_value (o_current, NULL, NULL)) {

      floating_attributes = g_list_prepend (floating_attributes, o_current);
    }
  }

  return g_list_reverse (floating_attributes);
}

/*! \brief Find an attribute in a list.
 *  \par Function Description
 *  Case sensitive search for attribute by name. Counter is the n'th
 *  occurance of the attribute, and starts searching from zero.  Zero
 *  is the first occurance of an attribute.
 *
 *  \param [in] list     GList of attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] count    Which occurance to return.
 *
 *  \return The n'th attribute object in the given list with the given name.
 */
GedaObject*
geda_attrib_object_find_attrib_by_name (const GList *list, const char *name, int count)
{
  const GList *iter;
  char *found_name;
  int   internal_counter = 0;

  for (iter = list; iter != NULL; iter = iter->next) {

    GedaObject *attribute = iter->data;

    g_return_val_if_fail (attribute->type == OBJ_TEXT, NULL);

    if (!geda_attrib_object_get_name_value (attribute, &found_name, NULL))
      continue;

    if (strcmp (name, found_name) == 0) {
      if (internal_counter == count) {
        GEDA_FREE (found_name);
        return attribute;
      }
      internal_counter++;
    }

    GEDA_FREE (found_name);
  }

  return NULL;
}

/*! \brief Find first occurance of attribute object attached to an object.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  \param [in] object   GedaObject whose attributes are to searched.
 *  \param [in] name     Character string with attribute name to search for.
 *
 *  \return The n'th attribute object in the given list with the given name.
 */
GedaObject*
geda_attrib_object_first_attrib_by_name (const GedaObject *object, char *name)
{
  if (GEDA_IS_OBJECT(object)) {
    return geda_attrib_object_find_attrib_by_name (object->attribs, name, 0);
  }
  BUG_MSG("Invalid GEDA GedaObject");
  return NULL;
}
/*! \brief Search attribute list by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     GList of attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *
 *  \return Character string with attribute value, NULL otherwise.
 */
static char*
geda_attrib_object_search_attrib_list_by_name (const GList *list,
                                     const char  *name,
                                           int    counter)
{
  GedaObject *attrib;
  char *value = NULL;

  attrib = geda_attrib_object_find_attrib_by_name (list, name, counter);

  if (attrib != NULL)
    geda_attrib_object_get_name_value (attrib, NULL, &value);

  return value;
}

/*! \brief Search floating attribute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     GList of Objects to search for floating attributes.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \note Caller must release the returned character string.
 */
char*
geda_attrib_object_search_floating_by_name (const GList *list,
                                            const char  *name,
                                                  int    counter)
{
  char *result;
  GList *attributes;

  attributes = geda_attrib_object_find_floating (list);
  result = geda_attrib_object_search_attrib_list_by_name (attributes, name, counter);
  g_list_free (attributes);

  return result;
}

/*! \brief Search attached attributes by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] object   The GedaObject whos attached attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \note Caller must release the returned character string.
 */
char*
geda_attrib_object_search_attached_by_name (const GedaObject *object,
                                          const char   *name,
                                                int     counter)
{
  return geda_attrib_object_search_attrib_list_by_name (object->attribs, name, counter);
}

/*! \brief Search inherited attribute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] object   The GedaObject whos inherited attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \note Caller must release the returned character string.
 */
char*
geda_attrib_object_search_inherited_by_name (const GedaObject *object,
                                             const char       *name,
                                                   int         counter)
{
  g_return_val_if_fail (object->type == OBJ_COMPLEX ||
                        object->type == OBJ_PLACEHOLDER, NULL);

  return geda_attrib_object_search_floating_by_name (object->complex->prim_objs,
                                                     name, counter);
}

/*! \brief Search attributes of object by name
 *  \par Function Description
 *  Search for attribute by name. The search includes attributes directly
 *  attached and inherited attributes. Counter is the n'th occurance of the
 *  attribute, and starts searching from zero. Zero is the first occurance
 *  of an attribute.
 *
 *  \param [in] object  GedaObject who's attributes to search,
 *  \param [in] name    Character string with attribute name to search for,
 *  \param [in] counter Which occurance to return.
 *
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \note Caller should release the returned character string.
 */
char *geda_attrib_object_search_object_by_name (const GedaObject *object,
                                              const char   *name,
                                                    int     counter)
{
  char  *result;
  GList *attributes;

  attributes = geda_attrib_object_return_attribs (object);
  result     = geda_attrib_object_search_attrib_list_by_name (attributes, name, counter);
  g_list_free (attributes);

  return result;
}

/*! \brief Get all attached attributes of the specified GedaObject.
 *  \par Function Description
 *  This function returns all attributes of the specified object.
 *
 *  The returned GList should be freed using the g_list_free().
 *
 *  This function aggregates the attached and inherited attributes
 *  belonging to a given GedaObject. (inherited attributes are those
 *  which live as toplevel un-attached attributes inside in a
 *  complex Object's prim_objs).
 *
 *  \param [in] object       GedaObject whos attributes to return.
 *
 *  \return A GList of attributes belinging to the passed object.
 */
GList *geda_attrib_object_return_attribs (const GedaObject *object)
{
  GList  *a_iter;
  GList  *attribs = NULL;

  g_return_val_if_fail (object != NULL, NULL);

  /* Directly attached attributes */
  for (a_iter = object->attribs; a_iter != NULL; a_iter = a_iter->next)
  {
     GedaObject *attribute;

     if ((attribute = a_iter->data) != NULL) {
      if (attribute->type != OBJ_TEXT)
        continue;

      /* Don't add invalid attributes to the list */
      if (!geda_attrib_object_get_name_value (attribute, NULL, NULL))
        continue;

      attribs = g_list_prepend (attribs, attribute);
    }
  }

  attribs = g_list_reverse (attribs);

  /* Inherited attributes (inside complex objects) */
  if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER)
  {
    GList *inherited_attribs =
      geda_attrib_object_find_floating (object->complex->prim_objs);

    attribs = g_list_concat (attribs, inherited_attribs);
  }

  return attribs;
}

/*! \brief Query whether a given attribute GedaObject is "inherited"
 *  \par Function Description
 *  This function returns TRUE if the given attribute GedaObject is a
 *  toplevel un-attached attribute inside a complex's prim_objs.
 *
 *  \param [in] attrib       GedaObject who's status to query.
 *
 *  \return TRUE if the given attribute is inside a symbol
 */
int
geda_attrib_object_is_inherited (const GedaObject *attrib)
{
  return (attrib->attached_to == NULL && attrib->parent_object != NULL);
}

typedef struct {
  AttribsChangedFunc func;
  void *data;
} AttribsChangedHook;

void
geda_attrib_object_append_changed_hook (Page *page,
                                        AttribsChangedFunc func,
                                        void *data)
{
  AttribsChangedHook *new_hook;

  new_hook = GEDA_MEM_ALLOC0(sizeof(AttribsChangedHook));
  new_hook->func = func;
  new_hook->data = data;

  page->attribs_changed_hooks =
    g_list_append (page->attribs_changed_hooks, new_hook);
}

static void call_attribs_changed_hook (void *data, void *user_data)
{
  AttribsChangedHook *hook = data;
  GedaObject *object = user_data;

  hook->func (hook->data, object);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void geda_attrib_object_emit_changed (GedaObject *object)
{
  if (object->attrib_notify_freeze_count > 0) {
    object->attrib_notify_pending = 1;
    return;
  }

  object->attrib_notify_pending = 0;

  if (object->page != NULL) {
    g_list_foreach (object->page->attribs_changed_hooks,
                    call_attribs_changed_hook, object);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void geda_attrib_object_freeze_hooks (GedaObject *object)
{
  object->attrib_notify_freeze_count ++;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void geda_attrib_object_thaw_hooks (GedaObject *object)
{
  g_return_if_fail (object->attrib_notify_freeze_count > 0);

  object->attrib_notify_freeze_count --;

  if (object->attrib_notify_freeze_count == 0 &&
      object->attrib_notify_pending)
    geda_attrib_object_emit_changed (object);
}
