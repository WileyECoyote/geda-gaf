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

/*! \file o_attrib_object.c
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
 *  \image html o_attrib_overview.png
 *  \image latex o_attrib_overview.pdf "attribute overview" width=14cm
 *
 *  \note
 *  Be sure in o_copy, o_move, and o_delete to maintain the attributes
 *  delete is a bare, because you will have to unattach the other end
 *  and in o_save geda_object_read as well
 *  and in o_select when selecting objects, select the attributes
 */

/** \defgroup geda-attrib-object-proc Attribute Object Procedures
 * @{
 * \brief Procedures for Operations with Attribute Objects
 */

#include "../../../config.h"

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda_priv.h>

typedef struct {
  AttribsChangedFunc func;
  void *data;
} AttribsChangedHook;

static void call_attribs_changed_hook (void *data, void *user_data)
{
  AttribsChangedHook *hook = data;
  GedaObject *object = user_data;

  hook->func (hook->data, object);
}

/*!
 * \internal Emit Attribute Change
 * \par Function Description
 * \todo does not compress notifiees, so if refresh_connectivity_cache
 *       is in the list 10 times does refresh_connectivity_cache is
 *       called 10 time?
 */
static void geda_attrib_object_emit_changed (GedaObject *object)
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

static void geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

/*!
 * \internal Add an attribute to a list.
 * \par Function Description
 *  Add an attribute to an existing attribute list.
 *
 * \param [in]  object The GedaObject that item is being added to.
 * \param [in]  item   The attribute that is to be added to object.
 */
static void geda_attrib_object_real_add(GedaObject *object, GedaObject *item)
{
  /* Add link from item to attrib listing */
  item->attached_to = object;
  object->attribs   = g_list_append (object->attribs, item);
  geda_attrib_object_emit_changed (object);
}

/*! O0301
 * \brief Add an attribute to an existing attribute list.
 * \par Function Description
 *  Front-end to validate arguments for geda_attrib_object_real_add.
 *
 * \param [in]  object The GedaObject that item is being added to.
 * \param [in]  item   The attribute that is to be added to object.
 */
void geda_attrib_object_add(GedaObject *object, GedaObject *item)
{
  if (!GEDA_IS_OBJECT(object)) {
    geda_object_error (__func__, object, GEDA_OBJECT_ALL);
  }
  else if (!GEDA_IS_OBJECT(item)) {
    geda_object_error (__func__, item, GEDA_OBJECT_ALL);
  }
  else {
    geda_attrib_object_real_add (object, item);
  }
}

/*! O0302
 * \brief Append Attribute Change List
 * \par Function Description
 *  Appends \a func to the list of functions to be called when
 *  an attribute is modified passing \a data to the function.
 */
void geda_attrib_object_append_changed_hook (Page              *page,
                                             AttribsChangedFunc func,
                                             void              *data)
{
  if (func != NULL) {

    AttribsChangedHook *new_hook;

    new_hook = GEDA_MEM_ALLOC0(sizeof(AttribsChangedHook));
    new_hook->func = func;
    new_hook->data = data;

    page->attribs_changed_hooks =
      g_list_append (page->attribs_changed_hooks, new_hook);
  }
}

/*! O0303
 * \brief Attach existing attribute to an object
 * \par Function Description
 *  Attach an existing attribute to an object.
 *
 * \param [out] object     The object that attribute is to be attached to.
 * \param [in]  attrib     The attribute to be added.
 * \param [in]  set_color  Whether or not the new attribute's color should be set.
 */
void geda_attrib_object_attach (GedaObject *object, GedaObject *attrib, int set_color)
{
  if (!GEDA_IS_OBJECT(object)) {
    geda_object_error(__func__, object, GEDA_OBJECT_ALL);
  }
  else if (!GEDA_IS_OBJECT(attrib)) {
    geda_object_error(__func__, attrib, GEDA_OBJECT_ALL);
  }
  else {

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

    geda_attrib_object_real_add (object, attrib);

    /* Only gets set if object is on a page */
    geda_struct_object_set_page_changed (object);

    if (set_color) {
      geda_set_object_color (attrib, ATTRIBUTE_COLOR);
    }
  }
}

/*! O0304
 * \brief Attach list of existing attributes to an object
 * \par Function Description
 *  Attach list of existing attributes to an object.
 *
 * \param [out] object     The object where you want to add item as an attribute.
 * \param [in]  attr_list  The list of attributes to be added.
 * \param [in]  set_color  Whether or not we should set the new attribute's color.
 */
void geda_attrib_object_attach_list (GedaObject  *object,
                                     const GList *attr_list,
                                     int          set_color)
{
  const GList *iter;

  for (iter = attr_list; iter != NULL; iter = iter->next) {
    geda_attrib_object_attach (object, iter->data, set_color);
  }
}

/*! O0305
 * \brief Detach an attribute from parent
 * \par Function Description
 *  Detaches \a attribute from the parent object.
 *  Currently in gschem, this would only apply to floating
 *  attributes because a non-floating can not be selected
 *  individually.
 *
 * \param [in,out] attribute The Attribute to be detached.
 */
void geda_attrib_object_detach(GedaObject *attribute)
{
  if (GEDA_IS_OBJECT(attribute)) {

    if (attribute->attached_to != NULL) {

      GedaObject *parent;

      parent = attribute->attached_to;
      attribute->attached_to = NULL;

      geda_set_object_color (attribute, DETACHED_ATTRIBUTE_COLOR);
      geda_attrib_object_emit_changed (attribute);

      parent->attribs = g_list_remove (parent->attribs, attribute);

      geda_struct_object_set_page_changed (attribute);
    }
  }
  else {
    geda_object_error(__func__, attribute, GEDA_OBJECT_ALL);
  }
}

/*! O0306
 * \brief Detach all attribute items in a list
 * \par Function Description
 *  Detach all attributes from an object. The color of each attribute
 *  is set to DETACHED_ATTRIBUTE_COLOR and the page change flag is set
 *  if at least one of the attributes was on a page.
 *
 * \param [in,out] object The object whos attributes are to be detached.
 */
void geda_attrib_object_detach_all(GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {

    if (object->attribs != NULL) {

      GList *a_iter;
      Page  *page;

      page = NULL;

      for (a_iter = object->attribs; a_iter != NULL; NEXT (a_iter)) {

        GedaObject *attribute = a_iter->data;

        attribute->attached_to = NULL;
        geda_set_object_color (attribute, DETACHED_ATTRIBUTE_COLOR);

        geda_attrib_object_emit_changed (attribute);

        if (!page) {
          page = geda_object_get_page(attribute);
        }
      }

      g_list_free (object->attribs);
      object->attribs = NULL;

      if (page && (GEDA_IS_PAGE(page))) {
        page->CHANGED = TRUE;
      }

      geda_attrib_object_emit_changed (object);
    }
  }
  else {
    geda_object_error(__func__, object, GEDA_OBJECT_ALL);
  }
}

/*! O0307
 * \brief Find first occurance of attribute object attached to an object.
 * \par Function Description
 *  Search for attribute by name.
 *
 * \param [in] object GedaObject whose attributes are to searched.
 * \param [in] name   Character string with attribute name to search for.
 *
 * \return The n'th attribute object in the given list with the given name.
 */
GedaObject *geda_attrib_object_first_attrib_by_name (const GedaObject *object,
                                                     const char *name)
{
  if (GEDA_IS_OBJECT(object)) {
    return geda_find_attrib_by_name (object->attribs, name, 0);
  }
  geda_object_error(__func__, object, GEDA_OBJECT_ALL);
  return NULL;
}

/*! O0308
 * \brief Freeze Attribute Notification Hooks
 * \par Function Description
 *  Increments the freeze count notify count, effectively blocking
 *  notifications until the freeze count was has been reduce to zero.
 */
void geda_attrib_object_freeze_hooks (GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {
    object->attrib_notify_freeze_count ++;
  }
  else {
    geda_object_error(__func__, object, GEDA_OBJECT_ALL);
  }
}

/*! O0309
 * \brief Get name and value from an attribute GedaObject
 * \par Function Description
 *  Calls geda_attrib_object_string_get_name_value to do the work
 *
 * \param [in]  attrib     The attribute GedaObject whos name/value to return.
 * \param [out] name_ptr   The return location for the name, or NULL.
 * \param [out] value_ptr  The return location for the value, or NULL.
 *
 * \return TRUE on success, FALSE otherwise.
 *
 * \sa geda_attrib_object_string_get_name_value()
 */
bool geda_attrib_object_get_name_value (const GedaObject  *attrib,
                                              char       **name_ptr,
                                              char       **value_ptr)
{
  if (GEDA_IS_TEXT(attrib)) {

    return geda_attrib_object_string_get_name_value (attrib->text->string,
                                                     name_ptr, value_ptr);
  }
  geda_object_error(__func__, attrib, GEDA_OBJECT_TEXT);
  return FALSE;
}

/*! O0310
 * \brief Query whether an attrib is attached to a specific object
 * \par Function Description
 *  This function checks whether the object \a attrib is attached to
 *  the \a object.
 *
 * \param [in]  attrib  The attribute to be checked.
 * \param [in]  object  Object to query.
 *
 * \return TRUE if attrib is an attribute of object, otherwise FALSE
 */
bool geda_attrib_object_is_attached_to (const GedaObject *attrib, const GedaObject *object)
{
  if (GEDA_IS_TEXT(attrib)) {

    if (object == NULL)
      return FALSE;

    if (attrib->attached_to == object)
      return TRUE;
  }

  geda_object_error(__func__, attrib, GEDA_OBJECT_TEXT);

  return FALSE;
}

/*! O0311
 * \brief Query whether a given attribute GedaObject is "inherited"
 * \par Function Description
 *  This function returns TRUE if the given attribute GedaObject is a
 *  toplevel un-attached attribute inside a complex's prim_objs.
 *
 * \param [in] attrib  GedaObject who's status to query.
 *
 * \return TRUE if the given attribute is inside a symbol
 */
int geda_attrib_object_is_inherited (const GedaObject *attrib)
{
  if (GEDA_IS_OBJECT(attrib)) {

    return (attrib->attached_to == NULL && attrib->parent_object != NULL);
  }

  geda_object_error(__func__, attrib, GEDA_OBJECT_ALL);

  return FALSE;
}

/*! O0312
 * \brief Create a new Attributes GedaObject
 * \par Function Description
 *  This function creates a new text object and attaches the attribute
 *  to the given parent object if the \a parent argument is not NULL,
 *  if the parent object is on a page, then the new attribute will be
 *  appended to the same page.
 *
 * \param [in] parent          Object the new attribute is to be attached to.
 * \param [in] name            Attribute name string.
 * \param [in] value           Attribute value string.
 * \param [in] visibility      Attribute GedaObject visibility.
 * \param [in] show_name_value Show name value visibility flag.
 *
 * \return [out] the new Text attribute GedaObject
 */
GedaObject *geda_attrib_object_new_attached(GedaObject *parent,
                                            const char *name,
                                            const char *value,
                                            int         visibility,
                                            int         show_name_value)
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
  if (parent && GEDA_IS_OBJECT(parent)) {

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
    text = geda_sprintf("%s=%s", name, value);
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
  new_obj = geda_text_object_new(color, world_x, world_y,
                       align, angle,               /* zero is angle */
                       DEFAULT_ATTRIBUTE_SIZE,     /* default text size */
                       visibility, show_name_value, text);

  /* attach the attribute to the object (if parent is not NULL) */
  if (parent) {
    if (parent->page) {
      geda_struct_page_append_object (parent->page, new_obj);
    }
    geda_attrib_object_attach (parent, new_obj, FALSE);
  }

  /* handle slot= attribute, it's a special case */
  if (parent != NULL &&
    geda_utility_string_strncmpi (text, "slot=", 5) == 0) {
    geda_struct_slot_update_object (parent);
  }

  GEDA_FREE(text);

  return new_obj;
}

/*! O0313
 * \brief Print all attributes to a Postscript document
 * \par Function Description
 *  Print all attributes to a Postscript document.
 *
 * \param [in] attributes  List of attributes to print.
 */
void geda_attrib_object_print(const GList *attributes)
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

/*! O0314
 * \brief Read attributes from a buffer.
 * \par Function Description
 *  Read attributes from a TextBuffer.
 *
 * \param [in]  toplevel        The GedaToplevel object.
 * \param [in]  parent          GedaObject which gets these attribs.
 * \param [in]  tb              The text buffer to read from.
 * \param [in]  release_ver     libgeda release version number.
 * \param [in]  fileformat_ver  file format version number.
 *
 * \param [out] err                    A GError object
 *
 * \return GList of attributes read, or NULL on error.
 *
 * \todo Bad Error recovery
 */
GList *geda_attrib_object_read (GedaToplevel *toplevel,
                                GedaObject   *parent,
                                TextBuffer   *tb,
                                unsigned int  release_ver,
                                unsigned int  fileformat_ver,
                                GError      **err)
{
  GList      *object_list;
  GedaObject *new_obj;
  const char *line;
  char        objtype;
  int         ATTACH;

  ATTACH      = FALSE;
  object_list = NULL;
  line        = geda_struct_textbuffer_next_line (tb);

  while (line) {

    const char *ptr = line;

    /* Skip over leading spaces */
    while ((*ptr == SPACE) && (*ptr != ASCII_CR) && (*ptr != ASCII_NUL)) { ++ptr; }

    objtype = *ptr;

    switch (objtype) {

      case(OBJ_TEXT):
        new_obj = geda_text_object_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;

        geda_text_object_set_rendered_bounds_func (new_obj,
                                         toplevel->rendered_text_bounds_func,
                                         toplevel->rendered_text_bounds_data);

        if (parent->type == OBJ_PIN)
          geda_pin_object_update_read_property(parent, new_obj);

        object_list = g_list_prepend (object_list, new_obj);
        ATTACH=TRUE;
        break;

      case(ENDATTACH_ATTR):
        return object_list;
        break;

      case(OBJ_LINE):
        new_obj = geda_line_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_NET):
        new_obj = geda_net_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BUS):
        new_obj = geda_bus_object_read(line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BOX):
        new_obj = geda_box_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        new_obj = geda_circle_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        new_obj = geda_complex_object_read (toplevel, line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = geda_path_object_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PIN):
        new_obj = geda_pin_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_ARC):
        new_obj = geda_arc_object_read (line, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
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

    line = geda_struct_textbuffer_next_line (tb);
  }

  /* The attribute list wasn't terminated, so it's a parse error! */
  g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
               _("unexpected end-of-file in attribute list"));

error:
  geda_struct_object_release_objects(object_list);
  return NULL;
}

/*! O0315
 * \brief Remove an attribute item from an attribute list
 * \par Function Description
 *  This function removes the given attribute from an attribute list.
 *  This function should be used when detaching an attribute.
 *
 * \param [in] list    The attribute list to remove attribute from.
 * \param [in] remove  The GedaObject to remove from list.
 */
void geda_attrib_object_remove(GList **list, GedaObject *remove)
{
  if (GEDA_IS_OBJECT(remove)) {

    GedaObject *attached_to = remove->attached_to;

    remove->attached_to = NULL;

    *list = g_list_remove (*list, remove);

    geda_attrib_object_emit_changed (attached_to);
  }
  else {
    geda_object_error(__func__, remove, GEDA_OBJECT_ALL);
  }
}

/*!
 * \internal Get all attached attributes of the GedaObject
 * \par Function Description
 *  This function returns all attributes of the specified object.
 *  The returned GList should be freed using the g_list_free().
 *  This function aggregates the attached and inherited attributes
 *  belonging to a given GedaObject. (inherited attributes are those
 *  which live as toplevel un-attached attributes inside in a
 *  complex Object's prim_objs).
 *
 * \param [in] object GedaObject whos attributes are to be to returned.
 *
 * \return A GList of attributes belonging to the passed object.
 */
static GList *geda_attrib_object_real_return_attribs (const GedaObject *object)
{
  GList *attribs = NULL;
  GList *a_iter;

  /* Directly attached attributes */
  for (a_iter = object->attribs; a_iter != NULL; a_iter = a_iter->next)
  {
    GedaObject *attribute = a_iter->data;

    if (attribute != NULL) {

      if (attribute->type != OBJ_TEXT)
        continue;

      /* Don't add invalid attributes to the list */
      if (!geda_attrib_object_string_get_name_value (attribute->text->string, NULL, NULL))
        continue;

      attribs = g_list_prepend (attribs, attribute);
    }
  }

  attribs = g_list_reverse (attribs);

  /* Inherited attributes (inside complex objects) */
  if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER)
  {
    GList *inherited_attribs;

    inherited_attribs = geda_list_find_floating (object->complex->prim_objs);

    attribs = g_list_concat (attribs, inherited_attribs);
  }

  return attribs;
}

/*! O0316
 * \brief Get all attached attributes of the specified GedaObject
 * \par Function Description
 *  This function is a wrapper for geda_attrib_object_real_return_attribs
 *  for external intrfaces, internal functions having previously validated
 *  the \a object should call geda_attrib_object_real_return_attribs
 *  directly.
 *
 * \param [in] object GedaObject whos attributes are to be to returned.
 *
 * \return A GList of attributes belonging to the passed object.
 */
GList *geda_attrib_object_return_attribs (const GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {

    return geda_attrib_object_real_return_attribs (object);

  }
  else {

    geda_object_error(__func__, object, GEDA_OBJECT_ALL);

  }
  return NULL;
}

/*!
 * \brief Search attribute list by name
 * \internal
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 * \param [in] list     GList of attributes to search.
 * \param [in] name     Character string with attribute name to search for.
 * \param [in] counter  Which occurance to return.
 *
 * \return Character string with attribute value, NULL otherwise.
 *
 * called by:
 *
 *   geda_attrib_object_search_attached_by_name
 *   geda_attrib_object_search_floating_by_name
 *   geda_attrib_object_search_object_by_name
 */
static char *geda_attrib_object_search_attrib_list_by_name (const GList *list,
                                                            const char  *name,
                                                                  int    counter)
{
  GedaObject *attrib;
  char       *value = NULL;

  attrib = geda_object_list_find_attrib_by_name (list, name, counter);

  if (attrib != NULL) {
    /* Attribute was validated by _list_find_attrib_by_name */
    geda_attrib_string_get_name_value (attrib->text->string, NULL, &value);
  }

  return value;
}

/*! O0317
 * \brief Search attached attributes by name.
 * \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero. Zero is the first occurance of an attribute.
 *
 * \param [in] object  The GedaObject whos attached attributes to search.
 * \param [in] name    Character string with attribute name to search for.
 * \param [in] index   Which occurance to return.
 *
 * \return Character string with attribute value, NULL otherwise.
 *
 * \note Caller should release the returned character string.
 */
char *geda_attrib_object_search_attached_by_name (const GedaObject *object,
                                                  const char       *name,
                                                        int         index)
{
  if (GEDA_IS_OBJECT(object)) {

    return geda_attrib_object_search_attrib_list_by_name (object->attribs,
                                                          name, index);
  }
  geda_object_error(__func__, remove, GEDA_OBJECT_ALL);
  return NULL;
}

/*! O0318
 * \brief Search floating attribute by name
 * \par Function Description
 *  Search for attribute by name.
 *
 *  Count is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 * \param [in] list   GList of Objects to search for floating attributes.
 * \param [in] name   Character string with attribute name to search for.
 * \param [in] index  Which occurance to return.
 *
 * \return Character string with attribute value, NULL otherwise.
 *
 * \note Caller should release the returned character string.
 */
char *geda_attrib_object_search_floating_by_name (const GList *list,
                                                  const char  *name,
                                                        int    index)
{
  char  *result;
  GList *attributes;

  attributes = geda_object_list_find_floating (list);
  result     = geda_attrib_object_search_attrib_list_by_name (attributes,
                                                              name,
                                                              index);
  g_list_free (attributes);

  return result;
}

/*! O0319
 * \brief Search inherited attribute by name.
 * \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 * \param [in] object  The GedaObject whos inherited attributes to search.
 * \param [in] name    Character string with attribute name to search for.
 * \param [in] index   Which occurance to return.
 *
 * \return Character string with attribute value, NULL otherwise.
 *
 * \note Caller should release the returned character string.
 */
char *geda_attrib_object_search_inherited_by_name (const GedaObject *object,
                                                   const char       *name,
                                                         int         index)
{
  if (GEDA_IS_COMPLEX(object)) {

  return geda_attrib_search_floating_by_name (object->complex->prim_objs,
                                              name, index);
  }

  geda_object_error(__func__, object, GEDA_OBJECT_COMPLEX);

  return NULL;
}

/*! O0320
 * \brief Search attributes of object by name
 * \par Function Description
 *  Search for attribute by name. The search includes attributes directly
 *  attached and inherited attributes. Counter is the n'th occurance of the
 *  attribute, and starts searching from zero. Zero is the first occurance
 *  of an attribute.
 *
 * \param [in] object  GedaObject who's attributes to search,
 * \param [in] name    Character string with attribute name to search for,
 * \param [in] index   Which occurance to return.
 *
 * \return Character string with attribute value, NULL otherwise.
 *
 * \note Caller should release the returned character string.
 */
char *geda_attrib_object_search_object_by_name (const GedaObject *object,
                                                const char       *name,
                                                      int         index)
{
  if (GEDA_IS_OBJECT(object)) {

    char  *result;
    GList *attributes;

    attributes = geda_attrib_object_real_return_attribs (object);
    result     = geda_attrib_object_search_attrib_list_by_name (attributes,
                                                                name, index);
    g_list_free (attributes);

    return result;
  }

  geda_object_error(__func__, object, GEDA_OBJECT_ALL);

  return NULL;
}

/*! O0321
 * \brief Search attributes of object for string return list matching
 * \par Function Description
 *  Search attribute strings. The search includes attributes directly
 *  attached and inherited attributes. If the search mode flag is False,
 *  partial matches and case insentitive searches are performed. When
 *  mode flag is True the returned list only includes attributes with
 *  strings having an exact match.
 *
 * \param [in] object GedaObject who's attributes to search,
 * \param [in] text   String to search for,
 * \param [in] exact  Search mode flag,
 *
 * \return List of attributes with match string, or NULL if no matches.
 *
 * \note Caller should free the returned list of objects
 */
GList *geda_attrib_object_search_object_string (const GedaObject *object,
                                                const char       *text,
                                                      int         exact)
{
  g_return_val_if_fail (text != (NULL), (NULL));
  g_return_val_if_fail (*text != 0, (NULL));

  if (GEDA_IS_OBJECT(object)) {

    GList *butes;
    GList *list;
    GList *iter;

    butes = NULL;
    list  = geda_attrib_object_real_return_attribs (object);

    for (iter = list; iter != NULL; iter = iter->next) {

      GedaObject *attrib = iter->data;

      const char *string = geda_text_object_get_string(attrib);

      if (string) {
        if (exact) {
          if (strcmp (string, text) == 0) {
            butes = g_list_prepend(butes, attrib);
          }
        }
        else {
          if (geda_utility_string_stristr (string, text) >= 0) {
            butes = g_list_prepend(butes, attrib);
          }
        }
      }
    }
    g_list_free (list);

    return g_list_reverse(butes);
  }

  geda_object_error(__func__, object, GEDA_OBJECT_ALL);

  return NULL;
}

/*! O0322
 * \brief Set Attribute to Interger Value
 * \par Function Description
 *  Sets the value of \a attrib using the string representation
 *  of \a value. The name of the attribute must not be NULL.
 *
 * \param [in] attrib   Attribute whose value is to be set,
 * \param [in] name_ptr Ponter to string of attribute name,
 * \param [in] value    Integer value to be set.
 *
 * \sa geda_attrib_object_set_value
 */
void geda_attrib_object_set_integer_value (GedaObject *attrib,
                                           const char *name_ptr,
                                                  int  value)
{
  if (GEDA_IS_TEXT(attrib)) {

    if (name_ptr != NULL) {

      GEDA_FREE(attrib->text->string);

      attrib->text->string = geda_sprintf("%s=%d", name_ptr, value, NULL);
      attrib->bounds_valid = FALSE;

      geda_struct_object_set_page_changed (attrib);
      geda_attrib_object_emit_changed (attrib);
    }
    else {
      fprintf(stderr, _("Attribute name cannot be NULL!\n"));
    }
  }
  else {
    geda_object_error(__func__, attrib, GEDA_OBJECT_TEXT);
  }
}

/*! O0323
 * \brief  Set Attribute to String Value
 * \par Function Description
 *  Sets the attribute value of \a attrib to the given value.
 *
 * \param [in] attrib    Attribute whose value is to be set,
 * \param [in] name_ptr  Pointer to string of attribute name,
 * \param [in] value_ptr Pointer the new value.
 *
 * \sa geda_attrib_object_set_integer_value
 */
void geda_attrib_object_set_value (GedaObject *attrib,
                                   const char *name_ptr,
                                   const char *value_ptr)
{
  if (GEDA_IS_TEXT(attrib)) {
    GEDA_FREE(attrib->text->string);
    attrib->text->string = geda_strconcat(name_ptr, "=", value_ptr, NULL);
    attrib->bounds_valid = FALSE;
    geda_struct_object_set_page_changed (attrib);
    geda_attrib_object_emit_changed (attrib);
  }
  else {
    geda_object_error(__func__, attrib, GEDA_OBJECT_TEXT);
  }
}

/*! O0324
 * \brief Get name and value from an attribute 'name=value' string.
 * \par Function Description
 *  This function parses the character string \a string expected to be
 *  an attribute string of the form 'name=value' and returns TRUE if
 *  the function was able to parse the string into the name and value
 *  components. Otherwise the function returns FALSE, in which case
 *  \a name_ptr and \a value_ptr are both set to NULL. Note that either
 *  \a name_ptr and/or \a value_ptr can be NULL.The caller should
 *  GEDA_FREE returned strings.
 *
 * \note
 *  If you get an invalid attribute (improper) with a name and no
 *  value, then it is NOT an attribute. Also, there cannot be any
 *  spaces adjacent the equals sign.
 *
 * \param [in]  string     String to split into name/value pair.
 * \param [out] name_ptr   The return location for the name, or NULL.
 * \param [out] value_ptr  The return location for the value, or NULL.
 *
 * \return TRUE on success, FALSE otherwise.
 *
 * \todo Move this to string utilities because does not accept an object!
 */
bool geda_attrib_object_string_get_name_value (const char  *string,
                                                     char **name_ptr,
                                                     char **value_ptr)
{
  /* If return name address set pointer to NULL */
  if (name_ptr != NULL) {
    *name_ptr = NULL;
  }

  /* If return value address set pointer to NULL */
  if (value_ptr != NULL) {
    *value_ptr = NULL;
  }

  if (string != NULL) {

    char *ptr = g_utf8_strchr (string, -1, g_utf8_get_char ("="));

    if (ptr != NULL) {

      char *next_char;
      char *prev_char;

      next_char = g_utf8_find_next_char (ptr, NULL);
      prev_char = g_utf8_find_prev_char (string, ptr);

      if (prev_char != NULL && *prev_char != ' ' &&
          next_char != NULL && *next_char != ' ' && *next_char != '\0')
      {

        if (name_ptr != NULL) {
          *name_ptr = geda_utility_string_strndup (string, (ptr - string));
        }

        if (value_ptr != NULL) {
          *value_ptr = geda_utility_string_strdup (next_char);
        }

        return TRUE;
      }
    }
  }
  return FALSE;
}

/*! O0325
 * \brief Thaw Attribute Notification Hooks
 * \par Function Description
 *  Decreases the notify_freeze_count of \a object, if the count
 *  has been reduced to zero pending notifications are completed.
 *  Silently ignores error if freeze_count already zero and does
 *  not emit_changed signal on the object.
 */
void geda_attrib_object_thaw_hooks (GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {

    if (object->attrib_notify_freeze_count > 0) {

      object->attrib_notify_freeze_count --;

      if (object->attrib_notify_freeze_count == 0 &&
          object->attrib_notify_pending)
      {
        geda_attrib_object_emit_changed (object);
      }
    }
  }
  else {
    geda_object_error(__func__, object, GEDA_OBJECT_ALL);
  }
}

/** @} endgroup geda-attrib-object-proc */
