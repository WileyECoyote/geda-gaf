/* -*- C x_multiattrib.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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
 * \file x_multiattrib.c
 * \brief A dialog box for editing Object Attributes.
 */

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#include <geda_widgets.h>
#include <geda_keysyms.h>
#include <geda_debug.h>

#define ThisDialog multiattrib

/** \defgroup Multi-Attribute-Dialog Multi Attribute Dialog
 *  @{
 *  \ingroup Editing-Dialogs
 *  \image html multiattrib_dialog.png
 *  \image latex multiattrib_dialog.png
 *  \par
 */

/* Enumerate Control IDs */
typedef enum {
       ShowInherited,

} ControlID;

static WidgetStringData DialogStrings[] = {
  { "ShowInheritedSwitch",  "Show inherited attributes", "Enable or disable displaying of inherited attributes"},
        { NULL, NULL, NULL},
};

static void multiattrib_update (Multiattrib *multiattrib);

static const char mae_object_list[]  = "object-list";

static bool snv_shows_name (int snv)
{
  return snv == SHOW_NAME_VALUE || snv == SHOW_NAME;
}

static bool snv_shows_value (int snv)
{
  return snv == SHOW_NAME_VALUE || snv == SHOW_VALUE;
}

/*!
 * \brief Process the response returned by the multi-attribte dialog.
 * \par Function Description
 *  This function handles the response <B>arg1</B> of the multi-attribute
 *  editor dialog <B>dialog</B>.
 *
 * \param [in] dialog    The multi-attribute editor dialog.
 * \param [in] arg1      The response ID.
 * \param [in] user_data A pointer on the GschemToplevel environment.
 */
static void
multiattrib_callback_response (GtkDialog *dialog, int arg1, void *user_data)
{
/* GschemToplevel *w_current = GSCHEM_TOPLEVEL (user_data); */

  switch (arg1) {
      case GEDA_RESPONSE_CLOSE:
      case GEDA_RESPONSE_DELETE_EVENT:
        gtk_widget_destroy (GTK_WIDGET (dialog));
        break;
  }
}

/*!
 * \brief Open multiple attribute editor dialog.
 * \par Function Description
 *  Opens the multiple attribute editor dialog for objects in this <B>toplevel</B>.
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void x_multiattrib_open (GschemToplevel *w_current)
{
  if ( w_current->mawindow == NULL ) {

    Page      *page;
    SELECTION *selection;

    page      = gschem_toplevel_get_current_page (w_current);
    selection = geda_struct_page_get_selection (page);

    w_current->mawindow = g_object_new (TYPE_MULTIATTRIB,
                                        "parent",          w_current->main_window,
                                        "settings-name",   IDS_MULTI_ATTRBI,
                                        "gschem-toplevel", w_current,
                                        mae_object_list,   selection,
                                        NULL);

    g_signal_connect (w_current->mawindow, "response",
                      G_CALLBACK (multiattrib_callback_response),
                      w_current);

    gtk_widget_show (w_current->mawindow);
  }
  else {
    gtk_window_present (GTK_WINDOW(w_current->mawindow));
  }
}

/*!
 * \brief Close the multiattrib dialog.
 * \par Function Description
 *  Closes the multiattrib dialog associated with <B>w_current</B>.
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void x_multiattrib_close (GschemToplevel *w_current)
{
  if (w_current->mawindow != NULL) {

    gtk_widget_destroy (w_current->mawindow);

    w_current->mawindow = NULL;
  }
}

/*!
 * \brief Update the multiattrib editor dialog for a GschemToplevel.
 * \par Function Description
 *  If the GschemToplevel has an open multiattrib dialog, switch to
 *  watching the current page's SELECTION object for changes.
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void x_multiattrib_update (GschemToplevel *w_current)
{
  if (w_current->mawindow != NULL) {

    Page       *page;
    SELECTION  *selection;

    page = geda_toplevel_get_current_page (w_current->toplevel);

    selection = geda_struct_page_get_selection (page);

    g_object_set (w_current->mawindow, mae_object_list, selection, NULL);
  }
}

/*! \section celltextview-widget Cell TextView Widget Code.
 * This widget makes a 'GtkTextView' widget implements the 'GtkCellEditable'
 * interface. It can then be used to renderer multi-line texts inside
 * tree views ('GtkTreeView').
 */
static void celltextview_class_init             (CellTextViewClass    *class);
static void celltextview_instance_init          (CellTextView         *self);
static void celltextview_editable_instance_init (GtkCellEditableIface *iface);

enum {
    PROP_EDIT_CANCELED = 1
};

static void celltextview_set_property (GObject      *object,
                                       unsigned int  property_id,
                                       const GValue *value,
                                       GParamSpec   *pspec)
{
  CellTextView *celltextview = (CellTextView*) object;

  switch (property_id) {
      case PROP_EDIT_CANCELED:
        celltextview->editing_canceled = g_value_get_boolean (value);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void celltextview_get_property (GObject      *object,
                                       unsigned int  property_id,
                                       GValue       *value,
                                       GParamSpec   *pspec)
{
  CellTextView *celltextview = (CellTextView*) object;

  switch (property_id) {
      case PROP_EDIT_CANCELED:
        g_value_set_boolean (value, celltextview->editing_canceled);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*!
 * \brief Multi-Attribute Edit Cell Key Press Event
 * \par Function Description
 *  Terminate editing of the cell of either the ESC or the ENTER key
 *  is pressed while editing name or value cells.
 */
static bool celltextview_key_press_event (GtkWidget   *widget,
                                          GdkEventKey *key_event,
                                          void        *data)
{
  CellTextView *celltextview = (CellTextView*)widget;

  /* If the Escape key is pressed, we flag the edit as canceled */
  if (key_event->keyval == GDK_Escape)
      celltextview->editing_canceled = TRUE;

  /* ends editing of cell if one of these keys are pressed or editing is canceled */
  if (celltextview->editing_canceled == TRUE ||
      /* the Enter key without the Control modifier */
      (!(key_event->state & GDK_CONTROL_MASK) &&
       (key_event->keyval == GDK_Return ||
        key_event->keyval == GDK_KP_Enter))) {
    gtk_cell_editable_editing_done  (GTK_CELL_EDITABLE (celltextview));
    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (celltextview));
    return TRUE;
  }

  return FALSE;
}

/*!
 * \brief CellTextView Start Editing Virtual Method
 * \par Function Description
 *  Over-ride for the GtkCellEditableIface::start_editing method.
 *  Connects the Key Press Event handler.
 */
static void
celltextview_start_editing (GtkCellEditable *cell_editable, GdkEvent *event)
{
  g_signal_connect (cell_editable, "key_press_event",
                    G_CALLBACK (celltextview_key_press_event),
                    NULL);
}

/*!
 * \brief Function to retrieve CellTextView's Type identifier
 * \par Function Description
 *  Function to retrieve CellTextView's Type identifier. On the first
 *  call, this registers the CellTextView in the GType system.
 *  Subsequently the function returns the saved value from its first
 *  execution.
 *
 *  \return the Type identifier associated with CellTextView.
 */
GedaType celltextview_get_type()
{
  static volatile GedaType celltextview_type = 0;

  if (g_once_init_enter (&celltextview_type)) {

    static const GTypeInfo celltextview_info = {
      sizeof(CellTextViewClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) celltextview_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CellTextView),
      0,    /* n_preallocs */
      (GInstanceInitFunc) celltextview_instance_init
    };

    static const GInterfaceInfo cell_editable_info = {
      (GInterfaceInitFunc) celltextview_editable_instance_init,
      NULL, /* interface_finalize */
      NULL  /* interface_data */
    };

    GedaType type;

    type = g_type_register_static (GTK_TYPE_TEXT_VIEW,
                                   g_intern_static_string ("CellTextView"),
                                   &celltextview_info, 0);

    g_type_add_interface_static(type, GTK_TYPE_CELL_EDITABLE,
                                      &cell_editable_info);

    g_once_init_leave (&celltextview_type, type);
  }

  return celltextview_type;
}

/*!
 * \brief CellTextView Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the CellTextView
 *  instance. Overrides parents virtual class methods as needed
 *  and installs a "editing-canceled" property.
 *
 * \param [in]  class  CellTextView class being initialized
 */
static void celltextview_class_init(CellTextViewClass *class)
{
  GParamSpec   *params;
  GObjectClass *gobject_class = (GObjectClass*) class;

  gobject_class->get_property = celltextview_get_property;
  gobject_class->set_property = celltextview_set_property;

  params = g_param_spec_boolean ("editing-canceled",
                                 "",
                                 "",
                                 FALSE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_EDIT_CANCELED, params);

}

/*!
 * \brief Initialize new CellTextView data structure instance.
 * \par Function Description
 *  This function is called after the CellTextViewClass is created
 *  to initialize the data structure.
 *
 * \param [in] celltextview  An empty CellTextView data structure
 */
static void celltextview_instance_init (CellTextView *celltextview)
{
  celltextview->editing_canceled = FALSE;
}

/*!
 * \brief Cell Text View Editable Interface Class Initializer
 * \par Function Description
 *  Assigns celltextview_start_editing to over-ride the start_editing
 *  method of the GtkCellEditableIface class.
 */
static void celltextview_editable_instance_init(GtkCellEditableIface *iface)
{
  iface->start_editing = celltextview_start_editing;
}

/*! \section multi-line-text-cell-renderer Multi-line Text Cell Renderer
 * GTK has no multi-line text cell renderer. This code adds one to be used
 * in gschem code. It is inspired by the 'GtkCellRendererCombo' renderer
 * of GTK 2.4 (LGPL).
 */

static void cellrenderermultilinetext_class_init (CellRendererMultiLineTextClass *class);

#define CELL_RENDERER_MULTI_LINE_TEXT_PATH "cell-renderer-multi-line-text-path"

/*!
 * \internal Callback Cell Editing Done
 * \par Function Description
 *  If editing was not cancel then the text from the buffer is retrieved and
 *  passed to the "edited" call-back handler, either ma_callback_edited_name
 *  or ma_callback_edited_value. Decrements the reference count of the buffer
 *  created by multiline_text_start_editing.
 */
static void
multiline_text_editing_done(GtkCellEditable *cell_editable, void *user_data)
{
  CellRendererMultiLineText *cell = CELL_RENDERER_MULTI_LINE_TEXT (user_data);
  GtkTextBuffer *buffer;

  if (cell->focus_out_id > 0) {
    g_signal_handler_disconnect (cell_editable, cell->focus_out_id);
    cell->focus_out_id = 0;
  }

  buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (cell_editable));

  if (CELL_TEXT_VIEW (cell_editable)->editing_canceled) {
    g_signal_emit_by_name (cell, "editing-canceled");
  }
  else {

    GtkTextIter start;
    GtkTextIter end;
    char       *new_text;
    const char *path;

    gtk_text_buffer_get_start_iter (buffer, &start);
    gtk_text_buffer_get_end_iter   (buffer, &end);

    new_text = gtk_text_buffer_get_text (buffer, &start, &end, TRUE);

    path = GEDA_OBJECT_GET_DATA(cell_editable, CELL_RENDERER_MULTI_LINE_TEXT_PATH);

    g_signal_emit_by_name (cell, "edited", path, new_text);

    GEDA_FREE (new_text);
  }

  g_object_unref(buffer);
}

/*!
 * \internal Callback Start Cell Editing
 * \par Function Description
 *  Over-ride for the GtkCellRendererClass::start_editing method.
 *  Painfully creates a new GtkTextBuffer and a new GtkTextView,
 *  loading text from the GtkCellRendererText into the buffer.
 *  A copy of the path is stored in the textview object. Connects
 *  the handler to the "editing_done" signal.
 *
 * \returns the new CellTextView object.
 */
static GtkCellEditable*
multiline_text_start_editing(GtkCellRenderer      *cell,
                             GdkEvent             *event,
                             GtkWidget            *widget,
                             const char           *path,
                             GdkRectangle         *background_area,
                             GdkRectangle         *cell_area,
                             GtkCellRendererState  flags)
{
  GtkCellRendererText       *cell_text;
  CellRendererMultiLineText *cell_multilinetext;
  GtkWidget                 *textview;
  GtkTextBuffer             *buffer;

  cell_text = GTK_CELL_RENDERER_TEXT (cell);

  if (cell_text->editable == FALSE) {
    return NULL;
  }

  cell_multilinetext  = CELL_RENDERER_MULTI_LINE_TEXT (cell);

  buffer = g_object_new (GTK_TYPE_TEXT_BUFFER, NULL);

  gtk_text_buffer_set_text (buffer, cell_text->text, strlen (cell_text->text));

  textview = g_object_new (TYPE_CELL_TEXT_VIEW,
                           /* GtkTextView */
                           "buffer",   buffer,
                           "editable", TRUE,
                           /* GtkWidget */
                           "height-request", cell_area->height,
                           NULL);

  g_object_set_data_full ((GObject*)textview,
                          CELL_RENDERER_MULTI_LINE_TEXT_PATH,
                          geda_utility_string_strdup (path), g_free);

  gtk_widget_show (textview);

  g_signal_connect (textview, "editing_done",
                    G_CALLBACK (multiline_text_editing_done),
                    cell_multilinetext);

  return GTK_CELL_EDITABLE (textview);
}

#undef CELL_RENDERER_MULTI_LINE_TEXT_PATH

/*! \brief Retrieve CellRendererMultiLineText GedaType identifier.
 *  \par Function Description
 *  Function to retrieve CellRendererMultiLineText GedaType identifier.
 * On the first call this functions registers the EdaConfig in the Type
 * system. On subsequent calls, the saved value returned.
 * A CellRendererMultiLineText is derived from GtkCellRendererTextClass
 *
 *  \return the GedaType identifier associated with EdaConfig.
 */
GedaType cellrenderermultilinetext_get_type(void)
{
  static GedaType cellrenderermultilinetext_type = 0;

  if (!cellrenderermultilinetext_type) {
    static const GTypeInfo cellrenderermultilinetext_info = {
      sizeof(CellRendererMultiLineTextClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) cellrenderermultilinetext_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CellRendererMultiLineText),
      0,    /* n_preallocs */
      NULL, /* instance_init */
    };

    cellrenderermultilinetext_type = g_type_register_static (
      GTK_TYPE_CELL_RENDERER_TEXT,
      "CellRendererMultiLineText",
      &cellrenderermultilinetext_info, 0);
  }

  return cellrenderermultilinetext_type;
}

/*! \brief Initialize Cell Renderer Multi-line Text Class Instance
 *  \par Function Description
 *
 */
static void
cellrenderermultilinetext_class_init(CellRendererMultiLineTextClass *class)
{
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (class); */
  GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS (class);


  cell_class->start_editing = multiline_text_start_editing;
}

enum {
  PROP_OBJECT_LIST = 1
};

enum {
  COLUMN_INHERITED,
  COLUMN_NAME,
  COLUMN_VALUE,
  COLUMN_VISIBILITY,
  COLUMN_SHOW_NAME_VALUE,
  COLUMN_PRESENT_IN_ALL,
  COLUMN_IDENTICAL_VALUE,
  COLUMN_IDENTICAL_VISIBILITY,
  COLUMN_IDENTICAL_SHOW_NAME,
  COLUMN_IDENTICAL_SHOW_VALUE,
  COLUMN_ATTRIBUTE_GEDALIST,
  NUM_COLUMNS
};

static GObjectClass *multiattrib_parent_class = NULL;

static void multiattrib_class_init   (MultiattribClass *class);
static void multiattrib_init         (Multiattrib      *ThisDialog);
static void multiattrib_set_property (GObject          *object,
                                      unsigned int      property_id,
                                const GValue *          value,
                                      GParamSpec       *pspec);
static void multiattrib_get_property (GObject          *object,
                                      unsigned int      property_id,
                                      GValue           *value,
                                      GParamSpec       *pspec);

static void multiattrib_popup_menu   (Multiattrib      *ThisDialog,
                                      GdkEventButton   *event);

/*! \brief Returns TRUE/FALSE if the given object may have attributes attached.
 *
 *  \par Function Description
 *
 *  Returns TRUE/FALSE if the given object may have attributes attached.
 *
 *  \param [in] object  The Object to test.
 *
 *  \returns  TRUE/FALSE if the given object may have attributes attached.
 */
static bool is_multiattrib_object (GedaObject *object)
{
  if (object->type == OBJ_COMPLEX ||
      object->type == OBJ_PLACEHOLDER ||
      object->type == OBJ_NET ||
      object->type == OBJ_BUS ||
      object->type == OBJ_PIN) {
    return TRUE;
  }
  return FALSE;
}

/*! \brief Multi-attribute Dialog Add Attributes
 *  \par Function Description
 *
 */
static void multiattrib_action_add_attribute(Multiattrib *ThisDialog,
                                             const char  *name,
                                             const char  *value,
                                             int          visible,
                                             int          show_name_value)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (ThisDialog)->w_current;
  GList          *iter;
  char           *newtext;

  newtext = geda_sprintf ("%s=%s", name, value);

  if (!x_dialog_validate_attribute(GTK_WINDOW(ThisDialog), newtext)) {
    GEDA_FREE(newtext);
    return;
  }

  for (iter  = geda_list_get_glist (ThisDialog->object_list);
       iter != NULL;
       iter  = g_list_next (iter)) {

    GedaObject *object = (GedaObject*)iter->data;

    if (is_multiattrib_object (object)) {

      /* create a new attribute and link it */
      o_attrib_add_attrib (w_current, newtext,
                           visible, show_name_value, -1, object);
    }
  }

  o_undo_savestate (w_current, UNDO_ALL);

  GEDA_FREE (newtext);

}

/*! \brief Multi-attribute Dialog Dupilcate Attributes
 *  \par Function Description
 *
 */
static void multiattrib_action_duplicate_attributes(Multiattrib *ThisDialog,
                                                    GList       *attr_list)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (ThisDialog)->w_current;

  GList *iter;

  for (iter = attr_list; iter != NULL; NEXT (iter)) {

   GedaObject *o_attrib = (GedaObject*)iter->data;

    /* create a new attribute and link it */
    o_attrib_add_attrib (w_current,
                         geda_text_object_get_string (o_attrib),
                         geda_object_get_is_visible (o_attrib),
                         o_attrib->show_name_value,
                         o_attrib->color,
                         o_attrib->attached_to);
  }

  o_undo_savestate (w_current, UNDO_ALL);

}

/*!
 * \brief Multi-attribute Dialog Promote Attribute
 * \par Function Description
 *  Attaches an Attribute object, aka text, to an Object,
 *  presumablby a Complex. If the orginal Attribute was
 *  invisible, then a copy is made and the copy is attached
 *  to the Object instead.
 *
 * \note: This Attribute object is unselected (not added to selection).
 */
static void multiattrib_action_promote_attributes(Multiattrib *ThisDialog,
                                                  GList       *iter)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (ThisDialog)->w_current;
  GedaToplevel   *toplevel  = w_current->toplevel;
  GedaObject     *o_new;

  while (iter) {

   GedaObject *o_attrib = (GedaObject*)iter->data;

    if (geda_object_get_is_visible (o_attrib)) {

      /* If the attribute we're promoting is visible, don't clone its location */
      o_attrib_add_attrib (w_current,
                           geda_text_object_get_string (o_attrib),
                           VISIBLE,
                           o_attrib->show_name_value,
                           o_attrib->color,
                           o_attrib->attached_to);
    }
    else {

        /* make a copy of the attribute object */
        o_new = geda_object_copy (o_attrib);
        geda_struct_page_append_object (toplevel->page_current, o_new);

        /* add the attribute its parent */
        geda_attrib_object_attach (o_attrib->parent_object, o_new, TRUE);

        /* Call add-objects-hook */
        g_hook_run_object (w_current, ADD_OBJECT_HOOK, o_new);
    }
    NEXT(iter);
  };

  o_undo_savestate (w_current, UNDO_ALL);
}

/*! \brief Multi-attribute Dialog Delete Attributes
 *  \par Function Description
 *
 */
static void multiattrib_action_delete_attributes(Multiattrib *ThisDialog,
                                                 GList       *iter)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (ThisDialog)->w_current;

  while (iter) {
    o_delete (w_current, iter->data);  /* Delete the attribute */
    NEXT(iter);
  };

  o_undo_savestate (w_current, UNDO_ALL);
}

/*! \brief Multi-attribute Dialog Copy Attributes
 *  \par Function Description
 *
 */
static void
multiattrib_action_copy_attribute_to_all (Multiattrib *ThisDialog,
                                          GList       *attr_list)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (ThisDialog)->w_current;

  GList  *iter;
  GList  *objects_needing_add;
  int     visibility;

  objects_needing_add = g_list_copy (geda_list_get_glist (ThisDialog->object_list));

  /* Remove objects which already have this attribute from the list */
  for (iter = attr_list; iter != NULL; NEXT(iter)) {
    GedaObject *parent = ((GedaObject*)iter->data)->attached_to;
    objects_needing_add = g_list_remove (objects_needing_add, parent);
  }

  for (iter = objects_needing_add; iter != NULL; NEXT(iter)) {

    GedaObject *object = iter->data;

    if (is_multiattrib_object (object)) {

      /* Pick the first instance to copy from */
      GedaObject *attrib_to_copy = attr_list->data;

      visibility = geda_object_get_is_visible (attrib_to_copy)
                 ? VISIBLE : INVISIBLE;

      /* create a new attribute and link it */
      o_attrib_add_attrib (w_current,
                           geda_text_object_get_string (attrib_to_copy),
                           visibility,
                           attrib_to_copy->show_name_value,
                           attrib_to_copy->color,
                           object);
    }
  }

  g_list_free (objects_needing_add);

  o_undo_savestate (w_current, UNDO_ALL);
}

/*! \brief Multi-attribute Dialog Set Data Names
 *  \par Function Description
 *
 */
static void ma_column_set_data_name (GtkTreeViewColumn *tree_column,
                                     GtkCellRenderer   *cell,
                                     GtkTreeModel      *tree_model,
                                     GtkTreeIter       *iter,
                                     void              *data)
{
  Multiattrib *dialog = (Multiattrib*) data;
  bool         present_in_all;
  char        *name;
  int          inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_NAME, &name,
                      COLUMN_PRESENT_IN_ALL, &present_in_all,
                      -1);

  g_object_set (cell,
                "text", name,
                "foreground-gdk", inherited ? &dialog->insensitive_text_color :
                                  (!present_in_all ? &dialog->not_present_in_all_text_color : NULL),
                "editable", !inherited,
                NULL);
  GEDA_FREE (name);
}

/*! \brief Multi-attribute Dialog Set Data Values
 *  \par Function Description
 *
 */
static void ma_column_set_data_value(GtkTreeViewColumn *tree_column,
                                     GtkCellRenderer   *cell,
                                     GtkTreeModel      *tree_model,
                                     GtkTreeIter       *iter,
                                     void              *data)
{
  Multiattrib *dialog = (Multiattrib *) data;
  bool         identical_value;
  char        *value;
  int          inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_VALUE, &value,
                      COLUMN_IDENTICAL_VALUE, &identical_value,
                      -1);

  g_object_set (cell,
               "text", identical_value ? value : _("<various>"),
               "foreground-gdk", inherited ? &dialog->insensitive_text_color :
                               (!identical_value ? &dialog->not_identical_value_text_color : NULL),
               "editable", !inherited,
                NULL);
  GEDA_FREE (value);
}

/*! \brief Multi-attribute Dialog
 *  \par Function Description
 *
 */
static void ma_column_set_data_visible(GtkTreeViewColumn *tree_column,
                                       GtkCellRenderer   *cell,
                                       GtkTreeModel      *tree_model,
                                       GtkTreeIter       *iter,
                                       void              *data)
{
  bool visibility;
  bool identical_visibility;
  int  inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_VISIBILITY, &visibility,
                      COLUMN_IDENTICAL_VISIBILITY, &identical_visibility,
                      -1);

  g_object_set ( cell,
                "active",        visibility,
                "sensitive",    !inherited,
                "activatable",  !inherited,
                "inconsistent", !identical_visibility,
                NULL);
}

/*!
 * \brief Multi Attribute Show Name Column Data Function
 * \par Function Description
 *  Sets the following properties for a given cell in the
 *  Name column:
 *  \par
 *  <DL>
 *    <DT>active</DT>
 *    <DT>sensitive</DT>
 *    <DT>activatable</DT>
 *    <DT>inconsistent</DT>
 *  </DL>
 *
 * \param [in] tree_column Not so much of interest, unused
 * \param [in] cell        The cell to be set
 * \param [in] tree_model  The model containing the data
 * \param [in] iter        to the data in the model
 * \param [in] data        Unused pointer to dialog
 */
static void ma_column_set_data_show_name(GtkTreeViewColumn *tree_column,
                                         GtkCellRenderer   *cell,
                                         GtkTreeModel      *tree_model,
                                         GtkTreeIter       *iter,
                                         void              *data)
{
  bool identical_show_name;
  int  inherited;
  int  show_name_value;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_SHOW_NAME_VALUE, &show_name_value,
                      COLUMN_IDENTICAL_SHOW_NAME, &identical_show_name,
                      -1);

  g_object_set (cell,
                "active",       snv_shows_name(show_name_value),
                "sensitive",    !inherited,
                "activatable",  !inherited,
                "inconsistent", !identical_show_name,
                NULL);
}

/*!
 * \brief Multi Attribute Show Value Column Data Function
 * \par Function Description
 *  Sets the following properties for a given cell in the
 *  Value column:
 *  \par
 *  <DL>
 *    <DT>active</DT>
 *    <DT>sensitive</DT>
 *    <DT>activatable</DT>
 *    <DT>inconsistent</DT>
 *  </DL>
 *
 * \param [in] tree_column Not so much of interest, unused
 * \param [in] cell        The cell to be set
 * \param [in] tree_model  The model containing the data
 * \param [in] iter        to the data in the model
 * \param [in] data        Unused pointer to dialog
 */
static void
ma_column_set_data_show_value(GtkTreeViewColumn *tree_column,
                              GtkCellRenderer   *cell,
                              GtkTreeModel      *tree_model,
                              GtkTreeIter       *iter,
                              void              *data)
{
  bool identical_show_value;
  int  inherited;
  int  show_name_value;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_SHOW_NAME_VALUE, &show_name_value,
                      COLUMN_IDENTICAL_SHOW_VALUE, &identical_show_value,
                      -1);

  g_object_set (cell,
                "active",       snv_shows_value (show_name_value),
                "sensitive",    !inherited,
                "activatable",  !inherited,
                "inconsistent", !identical_show_value,
                NULL);
}

/*! \brief  Multi-attribute Dialog
 *  \par Function Description
 *
 */
static void ma_callback_edited_name(GtkCellRendererText *cellrenderertext,
                                    const char          *path,
                                    char                *new_name,
                                    void                *user_data)
{
  GschemToplevel *w_current;
  Multiattrib    *ThisDialog;
  GtkTreeModel   *model;
  GtkTreeIter     iter;
  GedaList       *attr_list;
  GList          *a_iter;
  char           *newtext;
  char           *value;

  ThisDialog = user_data;
  model      = gtk_tree_view_get_model (ThisDialog->treeview);

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  if (*new_name == '\0') {

    const char *warning;

    warning = _("Attribute name must not be empty. Please set a name.");

    titled_warning_dialog(_("Multi-Attribute Editor"), "%s", warning);

    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_VALUE, &value,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  newtext = geda_sprintf ("%s=%s", new_name, value);

  /* Check if the new attribute is valid */
  if (!x_dialog_validate_attribute(GTK_WINDOW(ThisDialog), newtext)) {
    GEDA_FREE (value);
    GEDA_FREE(newtext);
    return;
  }

  w_current  = GSCHEM_DIALOG (ThisDialog)->w_current;

  a_iter = geda_list_get_glist (attr_list);

  while (a_iter != NULL) {

    GedaObject *o_attrib = a_iter->data;

    int visibility = geda_object_get_is_visible (o_attrib) ? VISIBLE : INVISIBLE;

    /* actually modifies the attribute */
    o_text_change (w_current, o_attrib, newtext, visibility,
                              o_attrib->show_name_value);
    NEXT (a_iter);
  };

  /* Refresh entire model, some attribute names may consolidate into one row */
  multiattrib_update (ThisDialog);

  o_undo_savestate (w_current, UNDO_ALL);

  GEDA_UNREF (attr_list);
  GEDA_FREE (value);
  GEDA_FREE (newtext);
}

/*! \brief  Multi-attribute Dialog
 *  \par Function Description
 *
 */
static void ma_callback_edited_value(GtkCellRendererText *cell_renderer,
                                     char                *path,
                                     char                *new_value,
                                     void                *user_data)
{
  Multiattrib    *ThisDialog;
  GtkTreeModel   *model;
  GtkTreeIter     iter;

  ThisDialog = user_data;
  model      = gtk_tree_view_get_model (ThisDialog->treeview);

  if (gtk_tree_model_get_iter_from_string (model, &iter, path)) {

    GedaList *attr_list;
    char     *name;
    char     *old_value;

    gtk_tree_model_get (model, &iter,
                        COLUMN_NAME, &name,
                        COLUMN_VALUE, &old_value,
                        COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                        -1);

    /* If the edit did not change anything, don't adjust any attributes */
    if (strcmp (old_value, new_value) != 0) {

      char *newtext = geda_sprintf ("%s=%s", name, new_value);

      if (x_dialog_validate_attribute(GTK_WINDOW(ThisDialog), newtext)) {

        GschemToplevel *w_current;

        w_current = GSCHEM_DIALOG (ThisDialog)->w_current;

        GList *a_iter = geda_list_get_glist (attr_list);

        while (a_iter != NULL) {

          GedaObject *o_attrib;

          int visibility, show_name_value;

          o_attrib        = (GedaObject*)a_iter->data;
          visibility      = geda_object_get_visibility(o_attrib);
          show_name_value = geda_object_get_show_name_value(o_attrib);

          /* actually modifies the attribute */
          o_text_change (w_current, o_attrib,
                         newtext,   visibility, show_name_value);
          NEXT (a_iter);
        };

        /* Fixup the model to reflect the edit */
        gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                            COLUMN_VALUE, new_value,
                            COLUMN_IDENTICAL_VALUE, TRUE,
                            -1);

        o_undo_savestate (w_current, UNDO_ALL);
      }

      GEDA_FREE (newtext);
    }

    GEDA_UNREF (attr_list);
    GEDA_FREE (name);
    GEDA_FREE (old_value);
  }
}

/*! \brief  Multi-attribute Dialog
 *  \par Function Description
 *
 */
static void ma_callback_toggled_visible(GtkCellRendererToggle *cell_renderer,
                                        char                  *path,
                                        void                  *user_data)
{
  Multiattrib    *ThisDialog;
  GtkTreeModel   *model;
  GtkTreeIter     iter;
  GschemToplevel *w_current;
  bool            new_visibility;
  GedaList       *attr_list;
  GList          *a_iter;

  ThisDialog = user_data;
  model      = gtk_tree_view_get_model (ThisDialog->treeview);

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  w_current  = GSCHEM_DIALOG (ThisDialog)->w_current;

  gtk_tree_model_get (model, &iter, COLUMN_ATTRIBUTE_GEDALIST, &attr_list, -1);

  new_visibility = !gtk_cell_renderer_toggle_get_active (cell_renderer);

  a_iter = geda_list_get_glist (attr_list);

  while (a_iter != NULL) {

    GedaObject *o_attrib = (GedaObject*)a_iter->data;

    /* Modify the attribute */
    o_invalidate_object (w_current, o_attrib);
    geda_set_object_visibility (o_attrib, new_visibility ? VISIBLE : INVISIBLE);
    geda_text_object_recreate (o_attrib);

    NEXT (a_iter);
  };

  /* Fixup the model to reflect the edit */
  gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_VISIBILITY, new_visibility,
                      COLUMN_IDENTICAL_VISIBILITY, TRUE,
                      -1);

  o_undo_savestate (w_current, UNDO_ALL);

  GEDA_UNREF (attr_list);
}

/*! \brief  Multi-attribute Dialog
 *  \par Function Description
 *
 */
static void
ma_callback_toggled_show_name(GtkCellRendererToggle *cell_renderer,
                              char                  *path,
                              void                  *user_data)
{
  Multiattrib    *ThisDialog = (Multiattrib*)user_data;
  GtkTreeModel   *model;
  GtkTreeIter     iter;
  GschemToplevel *w_current;
  GedaList       *attr_list;
  GList          *a_iter;
  Page           *page;
  SELECTION      *selection;
  bool            new_name_visibility;
  int             new_snv;

  model     = gtk_tree_view_get_model (ThisDialog->treeview);
  w_current = GSCHEM_DIALOG (ThisDialog)->w_current;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter, COLUMN_ATTRIBUTE_GEDALIST, &attr_list, -1);

  new_name_visibility = !gtk_cell_renderer_toggle_get_active (cell_renderer);

  a_iter = geda_list_get_glist (attr_list);

  while (a_iter != NULL) {

    GedaObject *o_attrib   = (GedaObject*)a_iter->data;

    /* If we switch off the name visibility, but the value was not
     * previously visible, make it visible now */
    if (new_name_visibility) {

      bool value_visible;

      value_visible = snv_shows_value (o_attrib->show_name_value);

      new_snv = value_visible ? SHOW_NAME_VALUE : SHOW_NAME;
    }
    else {
      new_snv = SHOW_VALUE;
    }

    o_invalidate_object (w_current, o_attrib);

    /* actually modifies the attribute */
    o_attrib->show_name_value = new_snv;
    geda_text_object_recreate (o_attrib);

    NEXT (a_iter);
  };

  page      = gschem_toplevel_get_current_page (w_current);
  selection = geda_struct_page_get_selection (page);

  /* Request update of display for this row, recomputing the whole model
   * as the consistency for the show value column may be affected above */
  g_object_set (ThisDialog, mae_object_list, selection, NULL);

  o_undo_savestate (w_current, UNDO_ALL);

  GEDA_UNREF (attr_list);
}

/*! \brief  Multi-attribute Dialog
 *  \par Function Description
 *
 */
static void
ma_callback_toggled_show_value(GtkCellRendererToggle *cell_renderer,
                               char                  *path,
                               void                  *user_data)
{
  Multiattrib    *ThisDialog;
  GschemToplevel *w_current;
  GtkTreeModel   *model;
  GtkTreeIter     iter;
  GedaList       *attr_list;
  GList          *a_iter;
  bool            new_value_visibility;

  ThisDialog = user_data;
  w_current  = GSCHEM_DIALOG (ThisDialog)->w_current;
  model      = gtk_tree_view_get_model (ThisDialog->treeview);

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter, COLUMN_ATTRIBUTE_GEDALIST, &attr_list, -1);

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter, COLUMN_ATTRIBUTE_GEDALIST, &attr_list, -1);

  new_value_visibility = !gtk_cell_renderer_toggle_get_active (cell_renderer);

  a_iter = geda_list_get_glist (attr_list);

  while (a_iter != NULL) {

    GedaObject *o_attrib = (GedaObject*)a_iter->data;
    int     new_snv;

    /* If we switch off the name visibility, but the value was not
     * previously visible, make it so now */
    if (new_value_visibility) {
      bool name_visible = snv_shows_name (o_attrib->show_name_value);
      new_snv = name_visible ? SHOW_NAME_VALUE : SHOW_VALUE;
    }
    else {
      new_snv = SHOW_NAME;
    }

    o_invalidate_object (w_current, o_attrib);

    /* Modify the attribute */
    o_attrib->show_name_value = new_snv;
    geda_text_object_recreate (o_attrib);

    NEXT (a_iter);
  };

  /* Request update of display for this row, recomputing the whole model
   * as the consistency for the show name column may be affected above */
  multiattrib_update (ThisDialog);

  o_undo_savestate (w_current, UNDO_ALL);

  GEDA_UNREF (attr_list);
}

/*! \brief  Multi-attribute Dialog
 *  \par Function Description
 *
 */
static bool multiattrib_callback_key_pressed(GtkWidget   *widget,
                                             GdkEventKey *event,
                                             void        *user_data)
{
  if (event->state == 0 && (event->keyval == GDK_Delete ||
      event->keyval == GDK_KP_Delete))
  {

    Multiattrib  *ThisDialog = (Multiattrib*)user_data;
    GtkTreeModel *model;
    GtkTreeIter   iter;
    GedaList     *attr_list;
    int           inherited;

    /* delete the currently selected attribute */
    if (!gtk_tree_selection_get_selected (
      gtk_tree_view_get_selection (ThisDialog->treeview), &model, &iter))
    {
      /* nothing selected, nothing to do */
      return FALSE;
    }

    gtk_tree_model_get (model, &iter,
                        COLUMN_INHERITED, &inherited,
                        COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                        -1);

    /* We can't delete inherited attribtes */
    if (!inherited) {

      multiattrib_action_delete_attributes (ThisDialog,
                                            geda_list_get_glist (attr_list));
      /* update the treeview contents */
      multiattrib_update (ThisDialog);
    }

    GEDA_UNREF (attr_list);
  }
  return FALSE;
}

/*!
 * \brief Move edit focus to the cell pointed to by a mouse event.
 * \par Function Description
 *  The function reacts to a mouse <B>event</B> at a given x-y coords,
 *  by moving edit-focus to the cell at those coords.
 *
 * \param [in] multiattrib  The Multiattrib object.
 * \param [in] event        Mouse event.
 */
static void
multiattrib_edit_cell(Multiattrib *ThisDialog, GdkEventButton *event)
{
  GtkTreePath       *path;
  GtkTreeViewColumn *column;

  if (event != NULL &&
      gtk_tree_view_get_path_at_pos (ThisDialog->treeview,
                                     (int)event->x,
                                     (int)event->y,
                                     &path, &column, NULL, NULL)) {

    gtk_tree_view_set_cursor_on_cell(ThisDialog->treeview,
                                     path, column, NULL, TRUE);
  }
}

/*!
 * \brief  Multi-attribute Dialog Edit Cell on Double (left) click
 * \par Function Description
 *  This is a niffy over-ride function. Normally, edit-focus by click is
 *  handled for us, but this function is useful for overriding the default
 *  behavior of treating a double-click the same as a single-click, with
 *  edit-focus needing two consecutive double \a or two single clicks with
 *  a pause in between.  This can be unintuitive and time-wasting.
 */
static bool multiattrib_callback_button_pressed(GtkWidget      *widget,
                                                GdkEventButton *event,
                                                void           *user_data)
{
  Multiattrib *ThisDialog = (Multiattrib*)user_data;
  bool retval             = FALSE;

  if (event->type == GDK_BUTTON_PRESS  &&  event->button == 3) {
    multiattrib_popup_menu (ThisDialog, event);
    retval = TRUE;
  }
  else {
    if (event->type == GDK_2BUTTON_PRESS  &&  event->button == 1) {
      multiattrib_edit_cell (ThisDialog, event);
      retval = TRUE;
    }
  }
  return retval;
}

/*! \brief Multi-attribute Dialog Display Popup Menu
 *  \par Function Description
 *
 */
static bool
multiattrib_callback_popup_menu(GtkWidget *widget, void *user_data)
{
  Multiattrib *ThisDialog = (Multiattrib*)user_data;

  multiattrib_popup_menu (ThisDialog, NULL);

  return TRUE;
}

/*! \internal A good riddance */
static void
multiattrib_destroy_popup(Multiattrib *ThisDialog)
{
  if (ThisDialog->popup) {
    gtk_widget_destroy(ThisDialog->popup);
    ThisDialog->popup = NULL;
  }
}

/*! \brief Multi-attribute Dialog Display Popup Do Dupilcate Attributes
 *  \par Function Description
 *
 */
static void
multiattrib_callback_popup_duplicate(GedaMenuItem *menuitem, void *user_data)
{
  Multiattrib   *ThisDialog = (Multiattrib*)user_data;
  GtkTreeModel  *model;
  GtkTreeIter    iter;
  GedaList      *attr_list;

  if (!gtk_tree_selection_get_selected (
    gtk_tree_view_get_selection (ThisDialog->treeview), &model, &iter))
  {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter, COLUMN_ATTRIBUTE_GEDALIST, &attr_list, -1);

  multiattrib_action_duplicate_attributes (ThisDialog, geda_list_get_glist (attr_list));

  /* update the treeview contents */
  multiattrib_update (ThisDialog);
  multiattrib_destroy_popup(ThisDialog);
  GEDA_UNREF (attr_list);
}

/*! \brief Multi-attribute Dialog Display Popup Do Promote Attributes
 *  \par Function Description
 *
 */
static void
multiattrib_callback_popup_promote (GedaMenuItem *menuitem, void *user_data)
{
  Multiattrib  *ThisDialog = user_data;

  GtkTreeModel *model;
  GtkTreeIter   iter;
  GedaList     *attr_list;

  if (gtk_tree_selection_get_selected (
      gtk_tree_view_get_selection (ThisDialog->treeview), &model, &iter))
  {
    GschemToplevel *w_current;
    Page           *page;
    SELECTION      *selection;

    gtk_tree_model_get (model, &iter, COLUMN_ATTRIBUTE_GEDALIST, &attr_list, -1);

    multiattrib_action_promote_attributes (ThisDialog,
                                           geda_list_get_glist (attr_list));

    w_current = GSCHEM_DIALOG (ThisDialog)->w_current;
    page      = gschem_toplevel_get_current_page (w_current);
    selection = geda_struct_page_get_selection (page);

    /* update the tree-view contents with promoted attributes */
    g_object_set (ThisDialog, mae_object_list, selection, NULL);

    GEDA_UNREF (attr_list);
  }

  multiattrib_destroy_popup(ThisDialog);
}

/*! \brief  Multi-attribute Dialog Display Popup Do Delete Attributes
 *  \par Function Description
 *
 */
static void
multiattrib_callback_popup_delete(GedaMenuItem *menuitem, void *user_data)
{
  Multiattrib     *ThisDialog = (Multiattrib*)user_data;
  GtkTreeModel    *model;
  GtkTreeIter      iter;
  GedaList        *attr_list;

  if (!gtk_tree_selection_get_selected (
        gtk_tree_view_get_selection (ThisDialog->treeview),
        &model, &iter))
  {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  multiattrib_action_delete_attributes (ThisDialog,
                                        geda_list_get_glist (attr_list));

  /* update the treeview contents */
  multiattrib_update (ThisDialog);
  multiattrib_destroy_popup(ThisDialog);
  GEDA_UNREF (attr_list);
}

/*!
 * \brief Multi-Attribute Menu Option Copy to All
 * \par Function Description
 *  Handles the menu option "Copy to All". If selected, attributes are
 *  passed to multiattrib_action_copy_attribute_to_all to propagate
 *  to other objects in the selection.
 *
 * \todo This option should be greyed-out when there is only
 *       one attribute in the selection.
 */
static void
multiattrib_callback_popup_copy_to_all(GedaMenuItem *menuitem, void *user_data)
{
  Multiattrib  *ThisDialog = user_data;
  GtkTreeModel *model;
  GtkTreeIter   iter;
  GedaList     *attr_list;

  if (!gtk_tree_selection_get_selected (
         gtk_tree_view_get_selection (ThisDialog->treeview),
         &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);
  multiattrib_action_copy_attribute_to_all (ThisDialog,
                                            geda_list_get_glist (attr_list));
  GEDA_UNREF (attr_list);

  /* update the treeview contents */
  multiattrib_update (ThisDialog);
  multiattrib_destroy_popup(ThisDialog);
}

/*!
 * \brief  Multi-attribute Dialog Display Value Key Pressed
 * \par Function Description
 *  Terminates editing of cell if one of these keys are pressed:
 *  - the Return key without the Control modifier
 *  - the Tab key without the Control modifier
 */
static bool multiattrib_callback_value_key_pressed(GtkWidget   *widget,
                                                   GdkEventKey *event,
                                                   void        *user_data)
{
  Multiattrib *ThisDialog = (Multiattrib*)widget;
  bool retval = FALSE;

  if ((event->keyval == GDK_Return || event->keyval == GDK_KP_Enter) ||
      (event->keyval == GDK_Tab    || event->keyval == GDK_KP_Tab)   ||
      (event->keyval == GDK_ISO_Left_Tab))
  {
    /* Control modifier activated? */
    if (event->state & GDK_CONTROL_MASK) {
      /* yes the modifier in event structure and let event propagate */
      event->state ^= GDK_CONTROL_MASK;
      retval = FALSE;
    }
    else {
      /* change focus and stop propagation */
      g_signal_emit_by_name (ThisDialog,
                             "move_focus",
                             (event->state & GDK_SHIFT_MASK) ?
                             GTK_DIR_TAB_BACKWARD : GTK_DIR_TAB_FORWARD);
      retval = TRUE;
    }
  }

  return retval;
}

/*!
 * \brief Multi-attribute Dialog "grab-focus" signal handler
 * \par Function Description
 *  Select the text in the GtkTextView so it may be over-typed quickly
 */
static void multiattrib_callback_value_grab_focus (GtkWidget *widget,
                                                   void      *user_data)
{
  GtkTextView   *text_view = GTK_TEXT_VIEW (widget);
  GtkTextBuffer *text_buffer;
  GtkTextIter    start_iter, end_iter;

  text_buffer = gtk_text_view_get_buffer (text_view);
  gtk_text_buffer_get_iter_at_offset (text_buffer, &start_iter, 0);
  gtk_text_buffer_get_iter_at_offset (text_buffer, &end_iter, -1);
  gtk_text_buffer_select_range (text_buffer, &end_iter, &start_iter);
}

/*!
 * \brief Multi-attribute Dialog Add New Attribute
 * \par Function Description
 *  This a callback for the "Add" button. The string is retrieved
 *  from the combo entry, along with the value from the test buffer.
 *  multiattrib_action_add_attribute is called to add the attribute
 *  using the visibility retrieved from the check-box button.
 */
static void multiattrib_callback_button_add(GtkButton *button, void *user_data)
{
  Multiattrib   *ThisDialog = (Multiattrib*)user_data;

  GtkTextBuffer *buffer;
  GtkTextIter    start;
  GtkTextIter    end;

  bool           visible;
  const char    *name;
  char          *value;
  int            shownv;

  buffer = gtk_text_view_get_buffer (ThisDialog->textview_value);

  /* retrieve information from the Add/Edit frame */
  /*   - attribute's name */
  name = GetEntryText(ThisDialog->combo_entry);

  /*   - attribute's value */
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  value = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);

  /*   - attribute's visibility status */
  visible = GetToggleState ( ThisDialog->button_visible );

  /*   - visibility type */
  shownv = geda_option_menu_get_history (ThisDialog->optionmenu_shownv);

  if (name[0] == '\0' || name[0] == ' ') {
    /* name not allowed for an attribute */
    GEDA_FREE (value);
    return;
  }

  multiattrib_action_add_attribute (ThisDialog, name, value, visible, shownv);
  GEDA_FREE (value);

  /* Refresh entire model, some attribute names may consolidate into one row */
  multiattrib_update (ThisDialog);
}

/*!
 * \brief Multi-attribute Dialog Pre-load Attribute ComboBox
 * \par Function Description
 *  This function appends each attribute name string retrieved from
 *  LibGeda->geda_struct_attrib_get() to the Add Frame's Attribute Name
 *  Combo during construction of the dialog.
 */
static
GtkWidget *x_multiattrib_new_entry_combo(void)
{
  GtkWidget  *combo;
  GtkWidget  *combo_entry;
  const char *string;
  int i;

  combo =  geda_combo_box_text_new_with_entry ();

  /* load the combo's tree with our list of attributes names */
  i = 0;
  string =geda_struct_attrib_get(i);
  while (string != NULL) {
    geda_combo_widget_append_text(combo, string);
    i++;
    string = geda_struct_attrib_get(i);
  }

  /* Add completion to attribute combo box entry */
  GtkEntryCompletion *cmp = gtk_entry_completion_new();

  gtk_entry_completion_set_model(cmp,
                                 geda_combo_widget_get_model(combo));
  gtk_entry_completion_set_text_column(cmp, 0);
  gtk_entry_completion_set_inline_completion(cmp, TRUE);
  gtk_entry_completion_set_popup_single_match(cmp, FALSE);

  combo_entry = geda_get_child_widget(combo);
  gtk_entry_set_completion(GTK_ENTRY(combo_entry), cmp);

  SetWidgetTip(combo, _("Select an attribute to add"));

  return combo;
}

/*!
 * \brief Multi-attribute Dialog Pre-load Visibilty Options Menu
 * \par Function Description
 *  This function loads the Visibility Options strings into the
 *  optionmenu during construction of the dialog.
 */
static void multiattrib_init_visible_types(GedaOptionMenu *optionmenu)
{
  GtkWidget *menu, *item;

  menu = geda_menu_new ();
  item = geda_menu_item_new_with_label (_("Show Name & Value"));
  geda_menu_append (menu, item);
  item = geda_menu_item_new_with_label (_("Show Value only"));
  geda_menu_append (menu, item);
  item = geda_menu_item_new_with_label (_("Show Name only"));
  geda_menu_append (menu, item);

  geda_option_menu_set_menu (optionmenu, menu);
  SetWidgetTip (optionmenu, _("Select an attribute visibility option"));
}

/*!
 * \brief Multi-attribute Dialog Popup Context Sensitive Menu.
 * \par Function Description
 *  Pops up a context-sensitive menu.
 *  <B>event</B> can be NULL if the popup is triggered by a key
 *  binding instead of a mouse click.
 *
 * \param [in] multiattrib  The Multiattrib object.
 * \param [in] event        Mouse event.
 */
static void multiattrib_popup_menu(Multiattrib *ThisDialog, GdkEventButton *event)
{
  GtkTreePath *path;
  GtkWidget   *menu;

  struct menuitem_t {
    char *label;
    GCallback callback;
  };

  struct menuitem_t menuitems_inherited[] = {
    { N_("Promote"),   G_CALLBACK (multiattrib_callback_popup_promote) },
    { NULL,            NULL                                            }
  };

  struct menuitem_t menuitems_noninherited[] = {
    { N_("Duplicate"),   G_CALLBACK (multiattrib_callback_popup_duplicate)  },
    { N_("Delete"),      G_CALLBACK (multiattrib_callback_popup_delete)     },
    { N_("Copy to all"), G_CALLBACK (multiattrib_callback_popup_copy_to_all)},
    { NULL,            NULL                                                 }
  };

  struct menuitem_t *item_list;
  struct menuitem_t *tmp;

  GtkTreeModel      *model;
  GtkTreeIter        iter;
  GtkTreeSelection  *selection;
  int                inherited;

  selection = gtk_tree_view_get_selection (ThisDialog->treeview);

  if (event != NULL &&
    gtk_tree_view_get_path_at_pos (ThisDialog->treeview,
                                  (int)event->x,
                                  (int)event->y,
                                  &path, NULL, NULL, NULL))
  {
    gtk_tree_selection_unselect_all (selection);
    gtk_tree_selection_select_path (selection, path);
    gtk_tree_path_free (path);
  }

  /* if nothing is selected, nothing to do */
  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    return;

  gtk_tree_model_get (model, &iter, COLUMN_INHERITED, &inherited, -1);

  item_list = inherited ? menuitems_inherited : menuitems_noninherited;

  /* create the context menu */
  menu = geda_menu_new();

  for (tmp = item_list; tmp->label != NULL; tmp++) {

    GtkWidget *menuitem;
    if (strcmp (tmp->label, "-") == 0) {
      menuitem = geda_menu_separator_new ();
    }
    else {
      menuitem = geda_menu_item_new_with_label (_(tmp->label));
      g_signal_connect (menuitem, "activate", tmp->callback, ThisDialog);
    }
    geda_menu_append (menu, menuitem);
  }
  gtk_widget_show_all (menu);

  if (ThisDialog->popup) {
    gtk_widget_destroy(ThisDialog->popup);
  }

  /* Store a pointer to the popup for destruction */
  ThisDialog->popup = menu;

  /* make menu a popup menu */
  geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                   gdk_event_get_time ((GdkEvent*)event));
}

/*!
 * \brief GschemDialog "geometry_save" class method handler
 * \par Function Description
 *  Chain up to our parent's method to save the dialog's size and
 *  position, then save the dialog's current internal geometry.
 *
 * \param [in] dialog     The GschemDialog to save the geometry of.
 * \param [in] cfg        A Geda Configuration object.
 * \param [in] group_name The group name in the key file to store the data under.
 */
static void multiattrib_geometry_save (GschemDialog *dialog,
                                       EdaConfig    *cfg,
                                       char         *group_name)
{
  bool show_inherited;

  /* Call the parent's GschemDialog::geometry_save method */
  GSCHEM_DIALOG_CLASS (multiattrib_parent_class)->geometry_save (dialog, cfg, group_name);

  show_inherited = GET_SWITCH_STATE(MULTIATTRIB (dialog)->ShowInheritedSwitch);

  eda_config_set_boolean (cfg, group_name, "show_inherited", show_inherited);
}

/*!
 * \brief GschemDialog "geometry_restore" class method handler
 * \par Function Description
 *  Chain up to our parent's method to restore the dialog's size and
 *  position, then restore the dialog's current internal geometry.
 *
 * \param [in] dialog     The GschemDialog to restore the geometry of.
 * \param [in] cfg        A Geda Configuration object.
 * \param [in] group_name The group name in the key file to store the data under.
 */
static void
multiattrib_geometry_restore (GschemDialog *dialog, EdaConfig *cfg, char *group_name)
{
  bool show_inherited;
  GError *error = NULL;

  /* Call the parent's geometry_restore method */
  GSCHEM_DIALOG_CLASS (multiattrib_parent_class)->
    geometry_restore (dialog, cfg, group_name);

  show_inherited = eda_config_get_boolean (cfg, group_name, "show_inherited", &error);
  if (error != NULL) {
    show_inherited = TRUE;
    g_error_free (error);
  }

  SetSwitch ((MULTIATTRIB (dialog))->ShowInherited, show_inherited);
}

/*!
 * \brief Function to retrieve Multiattrib's GedaType identifier.
 * \par Function Description
 *  Function to retrieve Multiattrib's GedaType identifier.
 *  Upon first call, this registers Multiattrib in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 * \return the GedaType identifier associated with Multiattrib.
 */
GedaType multiattrib_get_type()
{
  static GedaType multiattrib_type = 0;

  if (!multiattrib_type) {
    static const GTypeInfo multiattrib_info = {
      sizeof(MultiattribClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) multiattrib_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Multiattrib),
      0,    /* n_preallocs */
      (GInstanceInitFunc) multiattrib_init,
    };

    multiattrib_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                               "Multiattrib",
                                               &multiattrib_info, 0);
  }

  return multiattrib_type;
}

/*!
 * \brief Update multiattrib editor dialog when its object list changes
 * \par Function Description
 *
 * \param [in] object_list  The GedaList object of we are watching/
 * \param [in] multiattrib  The multi-attribute editor dialog.
 */
static void
object_list_changed_cb (GedaList *object_list, Multiattrib *ThisDialog)
{
  multiattrib_update (ThisDialog);
}

/*!
 * \brief Update the dialog when the current object GedaList object is destroyed
 * \par Function Description
 *  This handler is called when the g_object_weak_ref() on the GedaList object
 *  we're watching expires. We reset our multiattrib->object_list pointer to NULL
 *  to avoid attempting to access the destroyed object. Note: The signal handlers
 *  were automatically disconnected during the destruction process.
 *
 * \param [in] data                  Pointer to the multi-attrib dialog
 * \param [in] where_the_object_was  Pointer to where the object was just destroyed
 */
static void
object_list_weak_ref_cb (void *data, GObject *where_the_object_was)
{
  Multiattrib *ThisDialog = (Multiattrib *)data;

  ThisDialog->object_list = NULL;
  multiattrib_update (ThisDialog);
}

/*!
 * \brief Connect signal handler and weak_ref on the GedaList object
 * \par Function Description
 *  Connect the "changed" signal and add a weak reference
 *  on the GedaList object we are going to watch.
 *
 * \param [in] multiattrib  The Multiattrib dialog.
 * \param [in] object_list  The GedaList object to watch.
 */
static void
connect_object_list (Multiattrib *ThisDialog, GedaList *object_list)
{
  ThisDialog->object_list = object_list;

  if (ThisDialog->object_list != NULL) {

    GEDA_WEAK_REF(ThisDialog->object_list, object_list_weak_ref_cb, ThisDialog);

    ThisDialog->object_list_changed_id =
      g_signal_connect (ThisDialog->object_list, "changed",
                        G_CALLBACK (object_list_changed_cb),
                        ThisDialog);

    /* Synthesize an object_list changed update to refresh the view */
    object_list_changed_cb (ThisDialog->object_list, ThisDialog);
  }
  else {
    /* Call an update to set the sensitivities */
    multiattrib_update (ThisDialog);
  }
}

/*!
 * \brief Disconnect Selection from multiattrib
 * \par Function Description
 *  If the dialog is watching a GedaList object, disconnect the
 *  "changed" signal and remove our weak reference on the object.
 *
 * \note The function is called when something is selected not
 *       when something is unselected.
 *
 * \param [in] multiattrib  The Multiattrib dialog.
 */
static void
disconnect_object_list (Multiattrib *ThisDialog)
{
  if (ThisDialog->object_list != NULL) {

    g_signal_handler_disconnect (ThisDialog->object_list,
                                 ThisDialog->object_list_changed_id);

    GEDA_WEAK_UNREF(ThisDialog->object_list, object_list_weak_ref_cb, ThisDialog);
  }
}

/* gobject_class->dispose */
static void
multiattrib_dispose (GObject *object)
{
  Multiattrib *ThisDialog = MULTIATTRIB(object);

  if (GTK_IS_TREE_VIEW (ThisDialog->treeview)) {

    GtkListStore *liststore;

    liststore = (GtkListStore*)gtk_tree_view_get_model (ThisDialog->treeview);

    gtk_list_store_clear (liststore);

    g_object_unref(liststore);

    ThisDialog->treeview = NULL;
  }

  ((GObjectClass*)multiattrib_parent_class)->dispose (object);
}

/*!
 * \brief GObject finalize handler
 * \par Function Description
 *  Just before the Multiattrib GObject is finalized, disconnect from
 *  the GedaList object being watched and then chain up to the parent
 *  class's finalize handler.
 *
 * \param [in] object  The GObject being finalized.
 */
static void
multiattrib_finalize (GObject *object)
{
  Multiattrib  *ThisDialog = MULTIATTRIB(object);

  /* Clear the existing list of attributes */
  disconnect_object_list (ThisDialog);

  multiattrib_destroy_popup(ThisDialog);

  ((GObjectClass*)multiattrib_parent_class)->finalize (object);
}

/*!
 * \brief GedaType class initializer for Multiattrib
 * \par Function Description
 *  Type class initializer for Multiattrib. We override our parent's
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  class       The MultiattribClass we are initializing
 */
static void multiattrib_class_init(MultiattribClass *class)
{
  GObjectClass      *gobject_class;
  GschemDialogClass *gschem_dialog_class;

  gobject_class        = (GObjectClass*)class;
  gschem_dialog_class  = (GschemDialogClass*)class;

  gschem_dialog_class->geometry_save    = multiattrib_geometry_save;
  gschem_dialog_class->geometry_restore = multiattrib_geometry_restore;

  gobject_class->set_property = multiattrib_set_property;
  gobject_class->get_property = multiattrib_get_property;
  gobject_class->dispose      = multiattrib_dispose;
  gobject_class->finalize     = multiattrib_finalize;

  multiattrib_parent_class = g_type_class_peek_parent (class);

  g_object_class_install_property ( gobject_class, PROP_OBJECT_LIST,
                                    g_param_spec_pointer (mae_object_list,
                                                          "",
                                                          "",
                                                          G_PARAM_READWRITE));
}

/*!
 * \brief Regenerate attribute list when the visibility
 *        setting  changes and toggle switch image
 * \par Function Description: This function changes images for
 *  show_inherited switch to the opposite state, i.e. if ON use
 *  OFF image and if OFF use ON image. The function then calls
 *  multiattrib_update to update the attribute list.
 */
static void multiattrib_show_inherited_toggled (GtkWidget   *widget,
                                                Multiattrib *ThisDialog)
{
  TOGGLE_SWITCH(widget);
  multiattrib_update (ThisDialog);
  return;
}

#define NAME_COLUMN_WIDTH   120
#define VALUE_COLUMN_WIDTH  220
#define TOGGLE_COLUMN_WIDTH  60

#define DataFunc          GtkTreeCellDataFunc
#define NameDataFunc      G_CALLBACK(ma_callback_edited_name)
#define ValueDataFunc     G_CALLBACK(ma_callback_edited_value)
#define VisibleDataFunc   G_CALLBACK(ma_callback_toggled_visible)
#define ShowNameDataFunc  G_CALLBACK(ma_callback_toggled_show_name)
#define ShowValueDataFunc G_CALLBACK(ma_callback_toggled_show_value)

#define NAME_COLUMN_DATA     "edited",       NameDataFunc, _("Name"),       \
        NAME_COLUMN_WIDTH,    FALSE, TRUE,   ma_column_set_data_name
#define VALUE_COLUMN_DATA    "edited",       ValueDataFunc,  _("Value"),    \
        VALUE_COLUMN_WIDTH,   TRUE,  TRUE,   ma_column_set_data_value
#define VISIBLE_COLUMN_DATA  "toggled",      VisibleDataFunc, _("Visible"), \
        TOGGLE_COLUMN_WIDTH,  FALSE,  FALSE, ma_column_set_data_visible
#define SHOWNAME_COLUMN_DATA "toggled",      ShowNameDataFunc, _("Name"),   \
        TOGGLE_COLUMN_WIDTH,  FALSE,  FALSE, ma_column_set_data_show_name
#define SHOWVALUE_COLUMN_DATA "toggled",     ShowValueDataFunc, _("Value"), \
        TOGGLE_COLUMN_WIDTH,  FALSE,  FALSE, ma_column_set_data_show_value

#define TreeViewSizing  GtkTreeViewColumnSizing
#define COLUMN_CAN_GROW GTK_TREE_VIEW_COLUMN_GROW_ONLY
#define COLUMN_FIXED    GTK_TREE_VIEW_COLUMN_FIXED

#define ColumnRendererType column_def[i].renderer_type
#define ColumnSignal       column_def[i].signal
#define ColumnNotifier     column_def[i].notify_func
#define ColumnTitle        column_def[i].title
#define ColumnMinWidth     column_def[i].min_width
#define ColumnSizing       column_def[i].sizing
#define ColumnExpandable   column_def[i].is_expandable
#define ColumnResizable    column_def[i].is_resizable
#define ColumnDataFunc     column_def[i].data_func

/*!
 * \brief GedaType instance initializer for Multiattrib
 * \par Function Description
 *  GedaType instance initializer for Multiattrib. Create
 *  and setup the widgets which make up the dialog.
 *
 * \param [in] multiattrib The Multiattrib we are initializing
 */
static void multiattrib_init(Multiattrib *ThisDialog)
{
  GtkWidget *frame, *label, *scrolled_win, *treeview;
  GtkWidget *table, *textview, *combo, *button;
  GtkWidget *attrib_vbox;
  GtkWidget *ShowInheritedSwitch=NULL;
  GedaOptionMenu    *optionm;
  GtkTreeModel      *store;
  GtkTreeSelection  *selection;
  GtkStyle          *style;
  int i;

  /* dialog initialization */
  g_object_set (ThisDialog, /* GtkContainer */
                /* GtkWindow */
                "title",           _("Edit Attributes"),
                "default-width",   320,
                "default-height",  350,
                "window-position", GTK_WIN_POS_MOUSE,
                "allow-grow",      TRUE,
                "allow-shrink",    FALSE,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  ThisDialog->object_list   = NULL;

  /* create the attribute list frame */
  frame = g_object_new (GTK_TYPE_FRAME,
                        /* GtkFrame */
                        "shadow", GTK_SHADOW_NONE,
                        NULL);

  ThisDialog->frame_attributes = frame;

  /*   - create the model for the treeview with pointer to attributes */
  store = (GtkTreeModel*)gtk_list_store_new (NUM_COLUMNS,
                                             G_TYPE_BOOLEAN,  /* COLUMN_INHERITED */
                                             G_TYPE_STRING,   /* COLUMN_NAME */
                                             G_TYPE_STRING,   /* COLUMN_VALUE */
                                             G_TYPE_BOOLEAN,  /* COLUMN_VISIBILITY */
                                             G_TYPE_INT,      /* COLUMN_SHOW_NAME_VALUE */
                                             G_TYPE_BOOLEAN,  /* COLUMN_PRESENT_IN_ALL */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_VALUE */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_VISIBILITY */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_SHOW_NAME */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_SHOW_VALUE */
                                             G_TYPE_OBJECT);  /* COLUMN_ATTRIBUTE_GEDALIST */

  /*   - create a scrolled window for the treeview */
  scrolled_win = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                               /* GtkContainer */
                               "border-width",      3,
                               /* GtkScrolledWindow */
                               "hscrollbar-policy",
                               GTK_POLICY_AUTOMATIC,
                               "vscrollbar-policy",
                               GTK_POLICY_AUTOMATIC,
                               "shadow-type",
                               GTK_SHADOW_ETCHED_IN,
                               NULL);

  /*   - create the treeview */
  treeview = g_object_new (GTK_TYPE_TREE_VIEW, /* GtkTreeView */
                           "model",      store,
                           "rules-hint", TRUE,
                           NULL);

  g_signal_connect (treeview, "key-press-event",
                    G_CALLBACK (multiattrib_callback_key_pressed),
                    ThisDialog);
  g_signal_connect (treeview, "button-press-event",
                    G_CALLBACK (multiattrib_callback_button_pressed),
                    ThisDialog);
  g_signal_connect (treeview, "popup-menu",
                    G_CALLBACK (multiattrib_callback_popup_menu),
                    ThisDialog);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));

  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  struct {
    GedaType       renderer_type;
    const char    *signal;
    void         (*notify_func)(void);
    const char    *title;
    int            min_width;
    int            is_expandable;
    int            is_resizable;
    DataFunc       data_func;
    TreeViewSizing sizing;
  } column_def[] = {{ CR_SINGLE_LINE, NAME_COLUMN_DATA, COLUMN_CAN_GROW  },
                    { CR_MULTI_LINE,  VALUE_COLUMN_DATA, COLUMN_CAN_GROW },
                    { CR_TOGGLE_CELL, VISIBLE_COLUMN_DATA, COLUMN_FIXED  },
                    { CR_TOGGLE_CELL, SHOWNAME_COLUMN_DATA, COLUMN_FIXED },
                    { CR_TOGGLE_CELL, SHOWVALUE_COLUMN_DATA, COLUMN_FIXED}
                   };

  /*  Add the columns to the treeview */
  for ( i = 0; i < G_N_ELEMENTS(column_def); i++) {

    GtkCellRenderer   *renderer;
    GtkTreeViewColumn *column;

    renderer = g_object_new (ColumnRendererType, NULL);

    g_signal_connect (renderer, ColumnSignal, ColumnNotifier, ThisDialog);

    column   = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                             "title",     ColumnTitle,
                             "min-width", ColumnMinWidth,
                             "expand",    ColumnExpandable,
                             "resizable", ColumnResizable,
                             "sizing",    ColumnSizing,
                             NULL);

    gtk_tree_view_column_pack_start (column, renderer, ColumnResizable);

    gtk_tree_view_column_set_cell_data_func (column,   renderer,
                                             ColumnDataFunc,
                                             ThisDialog, NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  }

  /*  - End columns of the treeview */

  /* add the treeview to the scrolled window */
  geda_container_add(scrolled_win, treeview);

  /* set treeview of multiattrib */
  ThisDialog->treeview = GTK_TREE_VIEW (treeview);

  attrib_vbox = gtk_vbox_new (FALSE, 0);

  /* Pack the vbox into the frame */
  geda_container_add(frame, attrib_vbox);

  /* add the scrolled window to box */
  gtk_box_pack_start (GTK_BOX (attrib_vbox), scrolled_win, TRUE, TRUE, 0);

  /* Create a new Toggle Switch widget */
  EDA_SWITCH(attrib_vbox, ShowInherited, 0, FALSE);

  /* Setup callback for Switch widget */
  GEDA_CALLBACK_SWITCH (ShowInherited, multiattrib_show_inherited_toggled, ThisDialog);

  /* Store pointer to widget in ThisDialog */
  ThisDialog->ShowInheritedSwitch = ShowInheritedSwitch;

  /* put the frame in the content area of the dialog */
  geda_container_add(GTK_DIALOG(ThisDialog)->vbox, frame);
  gtk_widget_show_all (frame);

  /* create the add/edit frame */
  frame = g_object_new (GTK_TYPE_FRAME,
                        "label", _("Add Attribute"),
                        NULL);
  ThisDialog->frame_add = frame;

  table = g_object_new (GTK_TYPE_TABLE,
                        /* GtkTable */
                        "n-rows",      4,
                        "n-columns",   2,
                        "homogeneous", FALSE,
                        NULL);

  /*   - the name entry: a Combo Box Entry */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        "yalign", 0.5,
                        /* GtkLabel */
                        "label",  _("Name:"),
                        NULL);

  gtk_table_attach (GTK_TABLE (table), label,
                    0, 1, 0, 1, 0, 0, 0, 0);

  combo = x_multiattrib_new_entry_combo();

  ThisDialog->combo_entry = geda_get_child_widget(combo);

  gtk_table_attach (GTK_TABLE (table), combo,
                    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, 0, 6, 3);

  /*   - the value entry: a GtkEntry */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        "yalign", 0.5,
                        /* GtkLabel */
                        "label",  _("Value:"),
                        NULL);

  scrolled_win = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                               /* GtkScrolledWindow */
                               "hscrollbar-policy",
                               GTK_POLICY_AUTOMATIC,
                               "vscrollbar-policy",
                               GTK_POLICY_AUTOMATIC,
                               "shadow-type",
                               GTK_SHADOW_IN,
                               NULL);

  const char *textview_tip;

  textview_tip  = _("Ctrl+Enter inserts new line; Ctrl+Tab inserts Tab");

  textview = g_object_new (GTK_TYPE_TEXT_VIEW, NULL);

  gtk_widget_set_tooltip_text (textview, textview_tip);

  g_signal_connect (textview, "key_press_event",
                    G_CALLBACK (multiattrib_callback_value_key_pressed),
                    ThisDialog);

  g_signal_connect (textview, "grab-focus",
                    G_CALLBACK (multiattrib_callback_value_grab_focus),
                    ThisDialog);

  /* Save the GTK_STATE_NORMAL color so we can work around GtkTextView's
   * stubborn refusal to draw with GTK_STATE_INSENSITIVE later on */
  style = gtk_widget_get_style (textview);

  ThisDialog->value_normal_text_color = style->text[ GTK_STATE_NORMAL ];

  /* Save this one so we can pick it as a sensible color to show the
   * inherited attributes dimmed.
   */
  style = gtk_widget_get_style (treeview);
  ThisDialog->insensitive_text_color = style->text[ GTK_STATE_INSENSITIVE ];

  gdk_color_parse ("grey", &ThisDialog->not_identical_value_text_color);
  gdk_color_parse ("red",  &ThisDialog->not_present_in_all_text_color);

  geda_container_add(scrolled_win, textview);
  SetWidgetTip( textview, _("Enter or type a value for the new attribute then press the Add button"));
  ThisDialog->textview_value = (GtkTextView*) (textview);
  gtk_table_attach (GTK_TABLE (table), label,
                    0, 1, 1, 2, 0, 0, 0, 0);
  gtk_table_attach (GTK_TABLE (table), scrolled_win,
                    1, 2, 1, 2, GTK_EXPAND | GTK_FILL, 0, 6, 3);

  /*   - the visible status */
  button = g_object_new (GTK_TYPE_CHECK_BUTTON,
                         /* GtkButton */
                         "label", _("Visible"),
                         "active", TRUE,
                         NULL);

  ThisDialog->button_visible = (GtkCheckButton*)button;
  SetWidgetTip( button, _("Enable or disable attribute visibility"));
  gtk_table_attach (GTK_TABLE (table), button,
                    0, 1, 2, 3, GTK_FILL, 0, 3, 0);

  /*   - the visibility type */
  optionm = g_object_new (GEDA_TYPE_OPTION_MENU, NULL);
  multiattrib_init_visible_types (optionm);
  ThisDialog->optionmenu_shownv = optionm;

  gtk_table_attach (GTK_TABLE (table), (GtkWidget*)optionm,
                    1, 2, 2, 3, GTK_EXPAND | GTK_FILL, 0, 6, 3);
  gtk_widget_show_all (table);

  /* create the add button */
  button = gtk_button_new_from_stock (GTK_STOCK_ADD);
  SetWidgetTip( button, _("Add new attribute to component"));
  g_signal_connect (button, "clicked",
                    G_CALLBACK (multiattrib_callback_button_add),
                    ThisDialog);
  gtk_table_attach (GTK_TABLE (table), button,
                    2, 3, 0, 3, 0, 0, 6, 3);

  /* add the table to the frame */
  geda_container_add(frame, table);

  /* pack the frame in the dialog */
  gtk_box_pack_start (GTK_BOX (((GtkDialog*)ThisDialog)->vbox), frame, FALSE, TRUE, 5);

  gtk_widget_show_all (frame);

  /* now add the close button to the action area */
  gtk_dialog_add_button ((GtkDialog*)ThisDialog,
                         GTK_STOCK_CLOSE, GEDA_RESPONSE_CLOSE);

  GList *focus_chain = NULL;
  focus_chain = g_list_append (focus_chain, ThisDialog->treeview);
  focus_chain = g_list_append (focus_chain, ThisDialog->ShowInheritedSwitch);
  focus_chain = g_list_append (focus_chain, combo);
  focus_chain = g_list_append (focus_chain, scrolled_win);
  focus_chain = g_list_append (focus_chain, ThisDialog->button_visible);
  focus_chain = g_list_append (focus_chain, optionm);
  focus_chain = g_list_append (focus_chain, button);
  geda_container_set_focus_chain (table, focus_chain);
  g_list_free (focus_chain);
}

/*!
 * \brief GObject property setter function
 * \par Function Description
 *  Setter function for Multiattrib's GObject property, "object-list".
 *
 * \param [in]  object       The GObject whose properties we are setting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [in]  value        The GValue the property is being set from
 * \param [in]  pspec        A GParamSpec describing the property being set
 */
static void multiattrib_set_property (GObject      *object,
                                      unsigned int  property_id,
                                const GValue       *value,
                                      GParamSpec   *pspec)
{
  Multiattrib *ThisDialog = MULTIATTRIB (object);

  switch(property_id)
  {
    case PROP_OBJECT_LIST:
      disconnect_object_list (ThisDialog);
      connect_object_list (ThisDialog, g_value_get_pointer (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*!
 * \brief GObject property getter function
 * \par Function Description
 *  Getter function for Multiattrib's GObject property, "object-list".
 *
 * \param [in]  object       The GObject whose properties we are getting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [out] value        The GValue in which to return the value of the property
 * \param [in]  pspec        A GParamSpec describing the property being got
 */
static void multiattrib_get_property (GObject     *object,
                                      unsigned int property_id,
                                      GValue      *value,
                                      GParamSpec  *pspec)
{
  Multiattrib *ThisDialog = MULTIATTRIB (object);

  switch(property_id)
  {
    case PROP_OBJECT_LIST:
      g_value_set_pointer (value, (void *)ThisDialog->object_list);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

typedef struct {
  bool  inherited;
  char *name;
  int   nth_with_name;
  char *value;
  bool  visibility;
  int   show_name_value;

  bool  present_in_all;
  bool  identical_value;
  bool  identical_visibility;
  bool  identical_show_name;
  bool  identical_show_value;

  GedaList *attribute_gedalist;
} MODEL_ROW;

/*!
 * \brief For a given Object, produce a GList of MODEL_ROW records
 * \par Function Description
 *  The main purpose of this function is to provide the "nth_with_name"
 *  count which we need to merge the attribute lists of various objects
 *  together.
 *
 * \param [in] multiattrib  The multi-attribute editor dialog (For libgeda API which needs a GedaToplevel)
 * \param [in] object       TheGedaObject * whos attributes we are processing
 * \returns  A GList of MODEL_ROW records detailing object's attributes.
 */
static GList *
object_attributes_to_model_rows (Multiattrib *ThisDialog, GedaObject *object)
{
  GList *model_rows = NULL;
  GList *a_iter;
  GList *object_attribs = geda_attrib_return_attribs (object);

  for (a_iter = object_attribs; a_iter != NULL; a_iter = g_list_next (a_iter))
  {
    GedaObject *a_current = a_iter->data;
    MODEL_ROW *m_row = GEDA_MEM_ALLOC0 (sizeof(MODEL_ROW));
    GList *m_iter;

    geda_attrib_object_get_name_value (a_current, &m_row->name, &m_row->value);

    m_row->inherited       = geda_attrib_is_inherited (a_current);
    m_row->visibility      = geda_object_get_is_visible (a_current);
    m_row->show_name_value = a_current->show_name_value;
    m_row->nth_with_name   = 0; /* Provisional value until we check below */

    /* The following fields are always true for a single GedaObject */
    m_row->present_in_all       = TRUE;
    m_row->identical_value      = TRUE;
    m_row->identical_visibility = TRUE;
    m_row->identical_show_name  = TRUE;
    m_row->identical_show_value = TRUE;

    m_row->attribute_gedalist = geda_list_new ();
    geda_list_add (m_row->attribute_gedalist, a_current);

    /* Search already processed attributes to see if we need to bump m_row->nth_with_name */
    for (m_iter = model_rows; m_iter != NULL; m_iter = g_list_next (m_iter))
    {
      MODEL_ROW *m_compare = m_iter->data;
      if (strcmp (m_compare->name, m_row->name) == 0 &&
          m_compare->inherited == m_row->inherited) {
        m_row->nth_with_name = m_row->nth_with_name + 1;
      }
    }

    model_rows = g_list_append (model_rows, m_row);
  }

  g_list_free (object_attribs);

  return model_rows;
}

/*!
 * \brief Produce GList of MODEL_ROW records for all attribute objects in our GedaList
 * \par Function Description
 *  This function produces a GList of MODEL_ROWs to the user can edit unattached
 *  attributes, or attributes which are selected separately from their owning
 *  object.
 *
 *  It is not expected this will be called when the GedaList the dialog is watching
 *  contains any higher level objects on which we could edit attributes.
 *
 * \param [in] multiattrib  The multi-attribute editor dialog
 * \returns  A GList of MODEL_ROW records detailing all lone selected attributes.
 */
static GList *
lone_attributes_to_model_rows (Multiattrib *ThisDialog)
{
  GList *o_iter;
  GList *model_rows = NULL;

  /* populate the store with attributes */
  for (o_iter = ThisDialog->object_list == NULL ? NULL : geda_list_get_glist (ThisDialog->object_list);
       o_iter != NULL;
       o_iter = g_list_next (o_iter)) {

    GedaObject *object = o_iter->data;
    MODEL_ROW *m_row;

    /* Consider a selected text object might be an attribute */
    if (object->type != OBJ_TEXT ||
        !geda_attrib_object_get_name_value (object, NULL, NULL))
      continue;

    /* We have an OBJ_TEXT which libgeda can parse as an attribute */

    ThisDialog->num_lone_attribs_in_list ++;

    m_row = GEDA_MEM_ALLOC0 (sizeof(MODEL_ROW));
    m_row->inherited = geda_attrib_is_inherited (object);
    geda_attrib_object_get_name_value (object, &m_row->name, &m_row->value);
    m_row->visibility = geda_object_get_is_visible (object);
    m_row->show_name_value = object->show_name_value;
    m_row->nth_with_name = 0; /* All selected attributes are treated individually */

    /* The following fields are always true for a single attribute */
    m_row->present_in_all = TRUE;
    m_row->identical_value = TRUE;
    m_row->identical_visibility = TRUE;
    m_row->identical_show_name = TRUE;
    m_row->identical_show_value = TRUE;

    m_row->attribute_gedalist = geda_list_new ();
    geda_list_add (m_row->attribute_gedalist, object);

    model_rows = g_list_append (model_rows, m_row);
  }

  return model_rows;
}

/*!
 * \brief Populate the multiattrib editor dialog's liststore
 * \par Function Description
 *  Consumes the GList of MODEL_ROW data, populating the dialog's liststore.
 *  The function frees / consumes the GList and MODEL_ROW data.
 *
 * \param [in] multiattrib  The multi-attribute editor dialog.
 * \param [in] model_rows   A GList of MODEL_ROW data.
 */
static void
multiattrib_populate_liststore (Multiattrib *ThisDialog, GList *model_rows)
{
  GtkListStore *liststore;
  GtkTreeIter tree_iter;
  GList *m_iter;

  /* Clear the existing list of attributes */
  liststore = (GtkListStore*)gtk_tree_view_get_model (ThisDialog->treeview);
  gtk_list_store_clear (liststore);

  for (m_iter = model_rows; m_iter != NULL; m_iter = g_list_next (m_iter))
  {

    MODEL_ROW *model_row = m_iter->data;

    int count = g_list_length (geda_list_get_glist (model_row->attribute_gedalist));

    model_row->present_in_all = (count == ThisDialog->total_num_in_list);

    gtk_list_store_append (liststore, &tree_iter);
    gtk_list_store_set (liststore,
                        &tree_iter,
                        COLUMN_INHERITED,            model_row->inherited,
                        COLUMN_NAME,                 model_row->name,
                        COLUMN_VALUE,                model_row->value,
                        COLUMN_VISIBILITY,           model_row->visibility,
                        COLUMN_SHOW_NAME_VALUE,      model_row->show_name_value,
                        COLUMN_PRESENT_IN_ALL,       model_row->present_in_all,
                        COLUMN_IDENTICAL_VALUE,      model_row->identical_value,
                        COLUMN_IDENTICAL_VISIBILITY, model_row->identical_visibility,
                        COLUMN_IDENTICAL_SHOW_NAME,  model_row->identical_show_name,
                        COLUMN_IDENTICAL_SHOW_VALUE, model_row->identical_show_value,
                        COLUMN_ATTRIBUTE_GEDALIST,   model_row->attribute_gedalist,
                        -1);
  }
}

static void
update_dialog_title (Multiattrib *ThisDialog, const char *complex_title_name)
{
  const char *title_string = _("Edit Attributes");
  char *title;

  unsigned len;

  unsigned sc, sl;  char *ss;
  unsigned pc, pl;  char *ps;
  unsigned nc, nl;  char *ns;
  unsigned bc, bl;  char *bs;
  unsigned ac, al;  char *as;

  sc = ThisDialog->num_complex_in_list;
  pc = ThisDialog->num_pins_in_list;
  nc = ThisDialog->num_nets_in_list;
  bc = ThisDialog->num_buses_in_list;
  ac = ThisDialog->num_lone_attribs_in_list;

  if (sc > 0) {
    if (sc > 1) {
      const char *str = _("symbols");
      ss = geda_sprintf(" %i %s (%s)", sc, str, complex_title_name);
    }
    else {
      const char *str = _("symbol");
      ss = geda_sprintf(" 1 %s (%s)", str, complex_title_name);
    }
    sl = strlen(ss);
  }
  else {
    sl = 0;
  }

  if (pc > 0) {
    if (pc > 1) {
      const char *str = _("pins");
      ps = geda_sprintf(" %i %s", pc, str);
    }
    else {
      const char *str = _("pin");
      ps  = geda_sprintf(" 1 %s", str);
    }
    pl = strlen(ps);
  }
  else {
    pl = 0;
  }

  if (nc > 0) {
    if (nc > 1) {
      const char *str = _("nets");
      ns = geda_sprintf(" %i %s", nc, str);
    }
    else {
      const char *str = _("net");
      ns = geda_sprintf(" 1 %s", str);
    }
    nl = strlen(ns);
  }
  else {
    nl = 0;
  }

  if (bc > 0) {
    if (bc > 1) {
      const char *str = _("buses");
      bs = geda_sprintf(" %i %s", bc, str);
    }
    else {
      const char *str = _("bus");
      bs = geda_sprintf(" 1 %s", str);
    }
    bl = strlen(bs);
  }
  else {
    bl = 0;
  }

  if (ac > 0) {
    if (ac > 1) {
      const char *str = _("attributes");
      as = geda_sprintf(" %i %s", ac, str);
    }
    else {
      const char *str = _("attribute");
      as = geda_sprintf(" 1 %s", str);
    }
    al = strlen(as);
  }
  else {
    al = 0;
  }

  len = strlen(title_string) + sl + pl + nl + bl + al;

  title = malloc(len + 1);  /* Add 1 for Terminator */

  strcpy (title, title_string);

  if (sl) {
    strncat (title, ss, sl);
    g_free(ss);
  }

  if (pl) {
    strncat (title, ps, pl);
    g_free(ps);
  }

  if (nl) {
    strncat (title, ns, nl);
    g_free(ns);
  }

  if (bl) {
    strncat (title, bs, bl);
    g_free(bs);
  }

  if (al) {
    strncat (title, as, al);
    g_free(as);
  }

  g_object_set (G_OBJECT (ThisDialog), "title", title, NULL);

  free (title);
}

static void free_row_record (void *data_record)
{
  MODEL_ROW *model_row = data_record;

  g_free(model_row->name);
  g_free(model_row->value);

  GEDA_UNREF (model_row->attribute_gedalist);
  model_row->attribute_gedalist = NULL;
  g_free(model_row);
}

/*!
 * \brief Update the multiattrib editor dialog's interface
 * \par Function Description
 *  Update the dialog to reflect the attributes of the currently selected
 *  object. If no (or multiple) objects are selected, the dialog's controls
 *  are set insensitive.
 *
 * \param [in] multiattrib  The multi-attribute editor dialog.
 */
static void
multiattrib_update (Multiattrib *ThisDialog)
{
  GList      *o_iter;
  GList      *model_rows;
  GtkStyle   *style;
  const char *complex_title_name;

  bool add_sensitive;
  bool list_sensitive;
  bool show_inherited;

  complex_title_name = NULL;
  model_rows         = NULL;
  show_inherited     = GET_SWITCH_STATE(ThisDialog->ShowInheritedSwitch);

  ThisDialog->total_num_in_list        = 0;
  ThisDialog->num_complex_in_list      = 0;
  ThisDialog->num_pins_in_list         = 0;
  ThisDialog->num_nets_in_list         = 0;
  ThisDialog->num_buses_in_list        = 0;
  ThisDialog->num_lone_attribs_in_list = 0;

  /* populate the store with attributes */
  for (o_iter = multiattrib->object_list == NULL ? NULL : geda_list_get_glist (multiattrib->object_list);
       o_iter != NULL; NEXT(o_iter))
  {

    GedaObject *object = o_iter->data;

    GList *object_rows;
    GList *or_iter;

    if (!is_multiattrib_object (object)) {
      continue;
    }

    /* Count the different objects we are editing */
    ThisDialog->total_num_in_list++;

    if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER) {

      const char *filename;

      ThisDialog->num_complex_in_list++;

      filename = geda_complex_get_filename(object->complex);

      if (filename) {
        if (complex_title_name == NULL) {
          complex_title_name = filename;
        }
        else if (strcmp (complex_title_name, filename) != 0) {
          complex_title_name = _("<various>");
        }
      }
    }

    if (object->type == OBJ_PIN) {
      ThisDialog->num_pins_in_list++;
    }

    if (object->type == OBJ_NET) {
      ThisDialog->num_nets_in_list++;
    }

    if (object->type == OBJ_BUS) {
      ThisDialog->num_buses_in_list++;
    }

    /* populate the store with any attributes from this object */
    object_rows = object_attributes_to_model_rows (ThisDialog, object);

    for (or_iter = object_rows; or_iter != NULL; or_iter = g_list_next (or_iter)) {

      MODEL_ROW *object_row = or_iter->data;
      MODEL_ROW *model_row;
      GList     *mr_iter;
      bool       found = FALSE;

      /* Skip over inherited attributes if we don't want to show them */
      if (!show_inherited && object_row->inherited) {
        free_row_record (object_row);
        continue;
      }

      /* Search our list of attributes to see if we already encountered */
      for (mr_iter = model_rows; mr_iter != NULL && found == FALSE; mr_iter = g_list_next (mr_iter)) {
        model_row = mr_iter->data;
        if (strcmp (model_row->name, object_row->name) == 0 &&
          model_row->nth_with_name == object_row->nth_with_name &&
          model_row->inherited == object_row->inherited) {
          found = TRUE;
        }
      }

      if (found) {
        /* Name matches a previously found attribute */
        /* Check if the rest of its properties match the one we have stored... */

        if (strcmp (model_row->value, object_row->value) != 0) {
          model_row->identical_value = FALSE;
        }

        if (model_row->visibility != object_row->visibility) {
          model_row->identical_visibility = FALSE;
        }

        if (snv_shows_name (model_row->show_name_value) !=
            snv_shows_name (object_row->show_name_value)) {
          model_row->identical_show_name = FALSE;
        }

        if (snv_shows_value (model_row->show_name_value) !=
            snv_shows_value (object_row->show_name_value)) {
          model_row->identical_show_value = FALSE;
        }

        /* Add the underlying attribute to the row's GedaList of attributes */
        geda_list_add_glist (model_row->attribute_gedalist,
                             geda_list_get_glist (object_row->attribute_gedalist));
        free_row_record (object_row);
      }
      else {
        /* The attribute name doesn't match any previously found attributes,
         * so add the model row entry describing it to the list. Note that
         * we do not free this record */
        model_rows = g_list_append (model_rows, object_row);
      }
    }

    /* delete the list of attribute objects */
    g_list_free (object_rows);

  }

  if (ThisDialog->total_num_in_list == 0) {

    /* If the selection contains no high level objects we can edit,
     * take a look and see whether there are any lone attributes
     * selected we can edit directly.
     */
    model_rows = lone_attributes_to_model_rows (ThisDialog);
    list_sensitive = (ThisDialog->num_lone_attribs_in_list > 0);
    add_sensitive = FALSE;
  }
  else {
    list_sensitive = TRUE;
    add_sensitive = TRUE;
  }

  multiattrib_populate_liststore (ThisDialog, model_rows);

  geda_glist_free_full (model_rows, free_row_record);

  /* Update window title to describe the objects we are editing. */
  update_dialog_title (ThisDialog, complex_title_name);

  /* Update sensitivities */
  gtk_widget_set_sensitive (ThisDialog->frame_attributes, list_sensitive);
  gtk_widget_set_sensitive (ThisDialog->frame_add, add_sensitive);

  /* Work around GtkTextView's stubborn indifference
   * to GTK_STATE_INSENSITIVE when rendering its text. */
  style = gtk_widget_get_style (GTK_WIDGET (ThisDialog->textview_value));
  gtk_widget_modify_text (GTK_WIDGET (ThisDialog->textview_value),
                          GTK_STATE_NORMAL,
                          add_sensitive ? &ThisDialog->value_normal_text_color
                          : &style->text[GTK_STATE_INSENSITIVE]);
}

/** @} endgroup Multi-Attribute-Dialog */

#undef ThisDialog
