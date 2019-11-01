/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_file_chooser.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: August, 05, 2014
 * Contributing Author: Wiley Edward Hill
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include <geda_file_chooser.h>
#include <geda_file_filter.h>
#include "../../include/geda_container.h"
#include "../../include/geda_marshal.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

#define ChooseClass GedaFileChooserClass

/**
 * \brief GedaFileChooser - A File Chooser Dialog
 * \par
 * A GedaFileChooser is a variant of GtkFileChooser.
 *
 * \defgroup GedaFileChooser File Chooser Dialog
 * @{
 */

enum {
  FILTER_CHANGED,
  GEOMETRY_SAVE,
  GEOMETRY_RESTORE,
  LAST_SIGNAL
};

enum {
  PROP_FILTER_INDEX = 1,
};

static unsigned int chooser_signals[LAST_SIGNAL] = { 0 };

static GtkFileChooserDialogClass *geda_file_chooser_parent_class = NULL;

/* List of pointers to GedaFileChooser instances */
static GList *list_of_choosers = NULL;

static GedaFileFilterDataDef filter_data[] = {
    GEDA_FILTER_NONE,
    GEDA_FILTER_SCHEMATIC,
    GEDA_FILTER_SYMBOL,
    GEDA_FILTER_GSCHEM,
    GEDA_NO_MORE_FILTERS
};

static void geda_file_chooser_filter_changed(GedaFileChooser *chooser)
{
    /* Do nothing here */
}

static void
chooser_update_filter_index(GtkWidget *button, GedaFileChooser *chooser)
{
  chooser->filter_index = gtk_combo_box_get_active (GTK_COMBO_BOX(button));
  g_signal_emit (chooser, chooser_signals[FILTER_CHANGED], 0);
}

/* GtkFileChooserDialog does not expose the combo button used for filter
 * selection, we should make our own chooser dialog, but until then, we
 * search all widgets looking for a combobox, the one used for the filter
 * is the only combobox used in the dialog */
static void FixGtkCrap(GtkWidget *widget, void *self)
{
  if (GTK_IS_COMBO_BOX(widget)) {
    (GEDA_FILE_CHOOSER(self))->filter_button = widget;
  }
  else if (GTK_IS_CONTAINER(widget)) {
     geda_container_forall (widget, FixGtkCrap, self);
  }
}

static void get_filter_button(GedaFileChooser *chooser)
{
  GList *children, *iter;

  /* Get all object inside the contents area of the dialog */
  children = geda_container_get_children (GTK_DIALOG (chooser)->vbox);

  /* For each container in the contents area to call look for combo box */
  for (iter = children; iter; iter = iter->next) {
    if (GTK_IS_CONTAINER(iter->data)) {
      geda_container_forall (iter->data, FixGtkCrap, chooser);
      if (chooser->filter_button) {
        break;
      }
    }
  }

  g_list_free (children);
}

/*! \brief Creates filter for Geda File Chooser.
 *  \par Function Description
 *  This function adds file filters to <B>filechooser</B>.
 *
 *  \param [in] filechooser The file chooser to apply filter to.
 */
static void
geda_file_chooser_setup_filters (GtkFileChooser *filechooser)
{
  GedaFileFilterDataDef *data;
  int i;

  for (data = filter_data; data->name != NULL; data++) {

    GtkFileFilter *filter;

    filter = gtk_file_filter_new ();

    gtk_file_filter_set_name(filter, data->name);

    for (i = 0; data->pattern[i]; i++) {
      const char *ext = data->pattern[i];
      gtk_file_filter_add_pattern (filter, ext);
    }
    g_object_set_data( G_OBJECT(filter), "id", (void*)(long)(data->id));
    gtk_file_chooser_add_filter (filechooser, filter);
  }
}

/*! \brief Get Current Filter Index of a Geda File Chooser
 *  \par Function Description
 *  This function return the current filters index of a #GedaFileChooser
 *  dialog.
 *
 *  \param [in] widget The file chooser widget.
 *
 *  \returns filter index integer value
 */
int geda_file_chooser_get_filter (GtkWidget *widget)
{
  return (GEDA_FILE_CHOOSER(widget)->filter_index);
}

/*! \brief Set the Filter Index of a Geda File Chooser
 *  \par Function Description
 *  This function sets the filters index of a #GedaFileChooser
 *  dialog.
 *
 *  \param [in] widget The file chooser widget.
 *  \param [in] index  The new index of the filter.
 *
 */
void geda_file_chooser_set_filter (GtkWidget *widget, int index)
{
  if (GEDA_IS_FILE_CHOOSER(widget)) {
    GedaFileChooser *chooser = (GedaFileChooser*)widget;
    g_signal_handler_block(chooser->filter_button, chooser->handler);
    gtk_combo_box_set_active(GTK_COMBO_BOX(chooser->filter_button), index);
    g_signal_handler_unblock(chooser->filter_button, chooser->handler);
    chooser->filter_index = index;
  }
  else {
    BUG_MSG("chooser is not a GedaFileChooser");
  }
}

static void look_for_entry(GtkWidget *widget, GedaFileChooser *self)
{
  if (GTK_IS_ENTRY(widget)) {
    self->entry = (GtkEntry*)widget;
  }
  else if (GTK_IS_CONTAINER(widget)) {
     geda_container_forall (widget, look_for_entry, self);
  }
}

static void
geda_file_chooser_find_entry (GedaFileChooser *chooser)
{
  GList *children, *iter;

  /* Get all objects inside the dialog */
  children = geda_container_get_children (chooser);

  for (iter = children; iter; iter = iter->next) {

    if (GTK_IS_CONTAINER(iter->data)) {

      geda_container_forall (iter->data, look_for_entry, chooser);

      if (chooser->entry != NULL) {
        break;
      }
    }
  }
  g_list_free (children);
}

static GObject *
geda_file_chooser_constructor (GType                  type,
                               unsigned int           n_properties,
                               GObjectConstructParam *properties)
{
  GObject *obj;

  /* Chain up to the parent constructor */
  obj = G_OBJECT_CLASS (geda_file_chooser_parent_class)->constructor (type, n_properties, properties);

  gtk_dialog_set_has_separator (GTK_DIALOG(obj), TRUE);

  get_filter_button((GedaFileChooser*)obj);

  return obj;
}

/*! \brief GObject finalize handler
 *
 *  \par Function Description
 *  Just before the GtkAction GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void geda_file_chooser_finalize (GObject *object)
{

  list_of_choosers = g_list_remove(list_of_choosers, object);

  if (!g_list_length(list_of_choosers)) {
    g_list_free(list_of_choosers);
    list_of_choosers = NULL;
  }

  (G_OBJECT_CLASS (geda_file_chooser_parent_class))->finalize (object);
}

/*! \brief GObject property getter function for a GedaFileChooser Object
 *
 *  \par Function Description
 *  Getter function for GedaFileChooser's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_file_chooser_get_property (GObject *object, unsigned int  property_id,
                                GValue  *value,  GParamSpec   *pspec)
{
  GedaFileChooser *chooser = GEDA_FILE_CHOOSER(object);

  switch (property_id) {

    case PROP_FILTER_INDEX:
      g_value_set_int (value, chooser->filter_index);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*! \brief GObject property setter for a GedaFileChooser Object
 *
 *  \par Function Description
 *  Setter function for GedaFileChooser's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_file_chooser_set_property (GObject *object, unsigned int  property_id,
                                const    GValue *value,  GParamSpec   *pspec)
{
  GedaFileChooser *chooser = GEDA_FILE_CHOOSER(object);

  switch (property_id) {

    case PROP_FILTER_INDEX:
      geda_file_chooser_set_filter((GtkWidget*)chooser, g_value_get_int (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*! \brief GedaFileChooser "geometry_restore" class method handler
 *  \par Function Description
 *  Restore dialog's last position and size from EdaConfig.
 *
 *  \param [in] chooser    The #GedaFileChooser Dialog to restore geometry.
 *  \param [in] group_name The group name in the key file to find the data under.
 */
static void
geda_file_chooser_geometry_restore (GedaFileChooser *chooser, char *group_name)
{
  EdaConfig *cfg;
  GtkWindow *window;
  int x, y, width, height;


  window = GTK_WINDOW(chooser);
  cfg    = eda_config_get_user_context();

  x      = eda_config_get_integer (cfg, group_name, "x", NULL);
  y      = eda_config_get_integer (cfg, group_name, "y", NULL);
  width  = eda_config_get_integer (cfg, group_name, "width",  NULL);
  height = eda_config_get_integer (cfg, group_name, "height", NULL);

  gtk_window_move (window, x, y);

  /* No need to call for a resize with a zero value */
  if ( width != 0 && height != 0) {
    gtk_window_resize (window, width, height);
  }
}

/*! \brief GedaFileChooser "geometry_save" class method handler
 *  \par Function Description
 *  Save the dialog's current position and size to EdaConfig.
 *
 *  \param [in] chooser    The #GedaFileChooser Dialog to save the geometry.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void
geda_file_chooser_geometry_save (GedaFileChooser *chooser, char *group_name)
{
  EdaConfig *cfg;
  GtkWindow *window;
  int x, y, width, height;

  window = GTK_WINDOW(chooser);
  cfg    = eda_config_get_user_context ();

  gtk_window_get_position (window, &x, &y);
  gtk_window_get_size (window, &width, &height);

  eda_config_set_integer (cfg, group_name, "x", x);
  eda_config_set_integer (cfg, group_name, "y", y);
  eda_config_set_integer (cfg, group_name, "width", width);
  eda_config_set_integer (cfg, group_name, "height", height);
}

/*! \brief GtkWidget show signal handler
 *
 *  \par Function Description
 *  Before the Dialog widget is shown, restore previously saved
 *  position and size.
 *
 *  \param [in] widget  The GtkWidget being shown.
 */
static void show_handler (GtkWidget *widget)
{
  char *group = FILE_CHOOSER_CONFIG_GROUP;

  /* Hack to fix BUG in GtkFileChooserDialog */
  gtk_window_set_resizable (GTK_WINDOW(widget), FALSE);

  /* Let Gtk show the window */
  GTK_WIDGET_CLASS (geda_file_chooser_parent_class)->show (widget);

  gtk_window_set_resizable (GTK_WINDOW(widget), TRUE);

  g_signal_emit (GEDA_FILE_CHOOSER (widget),
                 chooser_signals[ GEOMETRY_RESTORE ], 0, group);
}

/*! \brief GtkWidget unmap signal handler
 *
 *  \par Function Description
 *  Before the dialog widget is unmapped, save its current position
 *  and size.
 *
 *  \param [in] widget  The GtkWidget being unmapped.
 */
static void unmap_handler (GtkWidget *widget)
{
  char *group = FILE_CHOOSER_CONFIG_GROUP;

  g_signal_emit (GEDA_FILE_CHOOSER (widget),
                 chooser_signals[ GEOMETRY_SAVE ], 0, group);

  /* Let Gtk unmap the window */
  GTK_WIDGET_CLASS (geda_file_chooser_parent_class)->unmap (widget);
}

/*! \brief Type class initializer for GedaFileChooser
 *  \par Function Description
 *  Type class initializer for GedaFileChooser. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 * \param [in] class A GedaFileChooserClass Object
 * \param [in] data  A GedaFileChooser data structure
 */
static void
geda_file_chooser_class_init (void *class, void *data)
{
  GParamSpec     *params;
  GedaType        type;

  ChooseClass    *chooser_class   = (ChooseClass*) class;
  GObjectClass   *gobject_class   = (GObjectClass*) class;
  GtkWidgetClass *widget_class    = (GtkWidgetClass*) class;

  gobject_class->get_property     = geda_file_chooser_get_property;
  gobject_class->set_property     = geda_file_chooser_set_property;
  gobject_class->constructor      = geda_file_chooser_constructor;
  gobject_class->finalize         = geda_file_chooser_finalize;

  chooser_class->filter_changed   = geda_file_chooser_filter_changed;
  chooser_class->geometry_save    = geda_file_chooser_geometry_save;
  chooser_class->geometry_restore = geda_file_chooser_geometry_restore;

  widget_class->show              = show_handler;
  widget_class->unmap             = unmap_handler;

  geda_file_chooser_parent_class = g_type_class_peek_parent (class);

  params = g_param_spec_int ("filter-index",
                           _("filter index"),   /* nick name */
                           _("Set or retrieve index of the filter combo"), /* hint / blurb */
                              FILTER_SCHEMATIC, /* Min value */
                              FILTER_GSCHEM,    /* Max value */
                              FILTER_GSCHEM,    /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_FILTER_INDEX, params);

  type = geda_file_chooser_get_type();

  /*!
   * \brief GedaFileChooser::filter-changed:
   * \par
   * The GedaFileChooser::filter-changed signal is emitted when the user
   * changes the selection of the filter combo text box.
   *
   * param [in] chooser the object which received the signal.
   */

  chooser_signals[FILTER_CHANGED]     = g_signal_new ("filter-changed", type,
                                                      G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                                                      G_STRUCT_OFFSET (GedaFileChooserClass,
                                                                       filter_changed),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      geda_marshal_VOID__VOID,
                                                      G_TYPE_NONE, 0);

  /*!
   * \brief GedaFileChooser::geometry-restore:
   * \par
   *  The GedaFileChooser::geometry-restore signal cause the dialog to restore
   *  the dialog size and position with the signal is emitted on the dialog.
   *
   * param [in] chooser the object which received the signal.
   */
  chooser_signals[ GEOMETRY_RESTORE ] = g_signal_new ("geometry-restore", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GedaFileChooserClass,
                                                                       geometry_restore ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      geda_marshal_VOID__STRING,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);

  /*!
   * \brief GedaFileChooser::geometry-save:
   * \par
   *  The GedaFileChooser::geometry-save signal cause the dialog to save
   *  the dialog size and position with the signal is emitted on the dialog.
   *
   * param [in] chooser the object which received the signal.
   */
  chooser_signals[ GEOMETRY_SAVE ]    = g_signal_new ("geometry-save", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GedaFileChooserClass,
                                                                       geometry_save ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      geda_marshal_VOID__STRING,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);
}

/*! \brief Initialize new GedaFileChooser data structure instance.
 *
 *  \par Function Description
 *  This function is call after the GedaFileChooserClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaFileChooser data structure
 * \param [in] class     A GedaFileChooserClass Object
 */
static void
geda_file_chooser_instance_init (GTypeInstance *instance, void *class)
{
  GedaFileChooser *self = (GedaFileChooser*)instance;

  self->entry         = NULL;
  self->filter_button = NULL;

  /* Append instance to list of valid GedaFileChooser objects */
  list_of_choosers = g_list_append(list_of_choosers, instance);
}

    /*! \brief Function to retrieve GedaFileChooser's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaFileChooser Type identifier. When
 *  first called, the function registers a #GedaFileChooser in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaFileChooser and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaFileChooser.
 */
GedaType geda_file_chooser_get_type (void)
{
  static volatile GedaType geda_file_chooser_type = 0;

  if (g_once_init_enter (&geda_file_chooser_type)) {

    static const GTypeInfo info = {
      sizeof(GedaFileChooserClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_file_chooser_class_init,    /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaFileChooser),
      0,                               /* n_preallocs         */
      geda_file_chooser_instance_init  /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaFileChooser");
    type   = g_type_register_static (GTK_TYPE_FILE_CHOOSER_DIALOG,
                                     string, &info, 0);

    g_once_init_leave (&geda_file_chooser_type, type);
  }

  return geda_file_chooser_type;
}


/*!
 * \brief Check if an object is a GedaFileChooser
 * \par Function Description
 *  Determines if \a chooser is valid by verifying \a chooser
 *  is included in the hash table of GedaFileChooser objects.
 *
 * \return TRUE if \a chooser is a valid GedaFileChooser
 */
bool
is_a_geda_file_chooser (GedaFileChooser *chooser)
{
  if (chooser && list_of_choosers) {
    return g_list_find(list_of_choosers, chooser) ? TRUE : FALSE;
  }
  return FALSE;
}

/*! \brief Instantiate a New Geda File Chooser Dialog
 *  to provide a GedaFileChooser equivelant of the convenience function
 *  gtk_file_chooser_dialog_new(...)
 *
 *  \par Function Description
 *  Convenience function which creates a GedaFileChooser with buttons and options.
 *
 *  \param [in]  parent         The GtkWindow Widget which will parent this dialog
 *  \param [in]  chooser_action The #FileChooserAction to use when setting up the dialog
 *
 *  \return  The GedaFileChooser created.
 */
GtkWidget*
geda_file_chooser_new (void *parent, FileChooserAction chooser_action)
{
  GtkWidget *widget;

  widget = g_object_new (geda_file_chooser_get_type(),
                         "action", chooser_action,
                         "select-multiple",
                         (chooser_action == FILE_CHOOSER_ACTION_OPEN),
                         NULL);

  if (widget) {

    GedaFileChooser *chooser;
    GtkDialog       *dialog;
    const char      *second_button_text;
    const char      *title;

    chooser = (GedaFileChooser*)widget;
    dialog  = (GtkDialog*)widget;

    switch (chooser_action) {
      case FILE_CHOOSER_ACTION_OPEN:
        second_button_text =  _("_Open");
        title = _("Open...");
        break;

      case FILE_CHOOSER_ACTION_SAVE:
        second_button_text = _("_Save");
        title = _("Save As...");
        break;

      case FILE_CHOOSER_ACTION_SELECT_FOLDER:
        second_button_text = _("_Select Folder...");
        title = _("Select Folder");
        break;

      default:
        second_button_text = _("OOPS");
        title = NULL;
        break;
    }

    gtk_dialog_add_buttons (dialog,
                            _("_Cancel"),       GEDA_RESPONSE_CANCEL,
                            second_button_text, GEDA_RESPONSE_ACCEPT,
                            NULL);

    /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(dialog,
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_CANCEL,
                                            -1);

    gtk_window_set_title (GTK_WINDOW (dialog), title);

    if (parent != NULL) {
      gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));
    }

    geda_file_chooser_setup_filters (GTK_FILE_CHOOSER (dialog));

    if (chooser->filter_button) {
      chooser->handler = g_signal_connect_after(G_OBJECT(chooser->filter_button),
                                                "changed",
                                                G_CALLBACK (chooser_update_filter_index),
                                                chooser);
    }

    /* set default response signal, usually triggered by the "Return" key */
    gtk_dialog_set_default_response (dialog, GEDA_RESPONSE_ACCEPT);
  }
  else {
    widget = NULL;
  }

  return widget;
}

static GtkWidget*
geda_file_chooser_dialog_new_valist (const char        *title,
                                     void              *parent,
                                     FileChooserAction  action,
                                     const char        *first_button_text,
                                     va_list            varargs)
{
  GtkDialog  *result;
  const char *button_text = first_button_text;

  result = g_object_new (geda_file_chooser_get_type(),
                         "title", title,
                         "action", action,
                         NULL);

  if (parent) {
    gtk_window_set_transient_for ((GtkWindow*)result, (GtkWindow*)parent);
  }

  while (button_text) {

    int response_id = va_arg (varargs, int);

    gtk_dialog_add_button (result, button_text, response_id);
    button_text = va_arg (varargs, const char *);
  }

  return (GtkWidget*)result;
}

/*!
 * \brief Create a New GedaFileChooser specifying Buttons
 * \par Function Description
 *  Creates a new #GedaFileChooser. This function is analogous to
 *  gtk_dialog_new_with_buttons(). GEDA_RESPONSE_ACCEPT will be set
 *  as the default response if detected.
 *
 * \param [in] title              Title of the dialog, or %NULL
 * \param [in] parent            Transient parent of the dialog, or %NULL
 * \param [in] action            Open or save mode for the dialog
 * \param [in] first_button_text stock ID or text to go in the first button, or %NULL
 * \param [in] ...               response ID for the first button, then additional (button, id) pairs, ending with %NULL
 *
 * \return a new #GedaFileChooser
 */
GtkWidget*
geda_file_chooser_dialog_new_full (const char       *title,
                                   void             *parent,
                                   FileChooserAction action,
                                   const char       *first_button_text, ...)
{
  GtkWidget *result;
  va_list varargs;

  va_start (varargs, first_button_text);
  result = geda_file_chooser_dialog_new_valist (title, parent, action,
                                                first_button_text,
                                                varargs);
  va_end (varargs);

  return result;
}

/*!
 * \brief Get Geda File Chooser Entry Widget
 * \par Function Description
 *  This function returns a pointer to the internal GtkEntry widget
 *
 * \param [in] widget The file chooser widget.
 *
 * \returns GtkEntry object
 */
GtkEntry *geda_file_chooser_get_entry (GtkWidget *widget)
{
  if (GEDA_IS_FILE_CHOOSER(widget)) {

    GedaFileChooser *chooser = (GedaFileChooser*)widget;

    if (chooser->entry == NULL) {
      geda_file_chooser_find_entry (chooser);
    }

    return chooser->entry;
  }
  else {
    BUG_MSG ("Operative is not a GedaFileChooser");
  }

  return NULL;
}

/*!
 * \brief Retrieve the Entry Text from a GedaFileChooser Widget
 * \par Function Description
 *  Returns all text in the chooser's entry or NULL, returned
 *  pointer should be freed when no longer need.
 */
char *geda_file_chooser_get_entry_text(GtkWidget *despicable)
{
  if (GTK_IS_FILE_CHOOSER(despicable)) {

    GtkEntry *entry = geda_file_chooser_get_entry(despicable);

    if (GTK_IS_ENTRY(entry)) {

      if (gtk_entry_get_text_length (entry)) {
        return geda_strdup (gtk_entry_get_text(entry));
      }
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaFileChooser");
  }

  return NULL;
}

/*!
 * \brief Programmatically Set Entry Text on GedaFileChooser
 * \par Function Description
 *  Sets the text in the entry of the #GedaFileChooser to \a text or
 *  an empty string if \a text is NULL.
 */
void geda_file_chooser_set_entry_text(GtkWidget *despicable, const char *text)
{
  if (GEDA_IS_FILE_CHOOSER(despicable)) {

    GtkEntry *entry = geda_file_chooser_get_entry(despicable);

    if (GTK_IS_ENTRY(entry)) {

      if (text) {
         gtk_entry_set_text(entry, text);
      }
      else {
         gtk_entry_set_text(entry, "");
      }
    }
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

/*!
 * \brief Get the filename from a GedaFileChooser
 * \par Function Description
 *  Gets the filename for the currently selected file in the file selector.
 *  The filename is returned as an absolute path. If multiple files are
 *  selected, one of the filenames will be returned at random. If the file
 *  chooser is in folder mode, this function returns the selected folder.
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 *
 * \returns filename string, which should be freed, or NULL.
 */
char *geda_file_chooser_get_filename(GtkWidget *widget)
{
  char *name;

  if (GTK_IS_FILE_CHOOSER(widget)) {
    name = gtk_file_chooser_get_filename((GtkFileChooser*)widget);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    name = NULL;
  }
  return name;
}

/*!
 * \brief Set the GedaFileChooser Filename
 * \par Function Description
 *  Sets filename as the current filename for the file chooser,
 *  changes to the file’s parent folder and selects the file in
 *  list; all other files will be unselected. If the chooser is
 *  in GTK_FILE_CHOOSER_ACTION_SAVE mode, the file’s base name
 *  will also appear in the dialog’s file name entry.
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 * \param [in] name    Pointer to the filename to set as current
 */
bool geda_file_chooser_set_filename (GtkWidget *widget, const char *name)
{
  int result;

  if (GTK_IS_FILE_CHOOSER(widget)) {
    result = gtk_file_chooser_set_filename((GtkFileChooser*)widget, name);
    if (result) {
      geda_file_chooser_set_entry_text(widget, name);
    }
    else {
      geda_file_chooser_set_entry_text(widget, "");
    }
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    result = FALSE;
  }

  return result;
}

/*!
 * \brief Get a list filenames from a GedaFileChooser
 * \par Function Description
 *  Lists all the selected files and subfolders in the current folder.
 *  The returned names are full absolute paths. If files in the current
 *  folder cannot be represented as local filenames they will be ignored
 *  The returned list should be released using g_slist_free, and each
 *  filenames with g_free.
 *
 * \param [in] widget Pointer to a hideous GtkFileChooser widget
 *
 * \returns a GSList containing the filenames of all selected files and
 *          subfolders in the current folder.
 */
GSList *geda_file_chooser_get_filenames(GtkWidget *widget)
{
  GSList *list;

  if (GTK_IS_FILE_CHOOSER(widget)) {
    list = gtk_file_chooser_get_filenames((GtkFileChooser*)widget);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    list = NULL;
  }
  return list;
}

/*!
 * \brief Retrieve the current folder from a GedaFileChooser
 * \par Function Description
 *  Gets the current folder of the chooser as a local filename.
 *  Note that this is the folder the file chooser is currently
 *  displaying. The returned string should be relased with g_free.
 *
 * \param [in] widget Pointer to a hideous GtkFileChooser widget
 *
 * \returns the full path of the current folder, or NULL if the current
 *          path cannot be represented as a local filename.
 */
char *geda_file_chooser_get_current_folder(GtkWidget *widget)
{
  char *folder;

  if (GTK_IS_FILE_CHOOSER(widget)) {
    folder = gtk_file_chooser_get_current_folder((GtkFileChooser*)widget);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    folder = NULL;
  }
  return folder;
}

/*!
 * \brief Set the current folder of a GedaFileChooser
 * \par Function Description
 *  Sets the current folder for chooser from a local filename. The
 *  user will be shown the full contents of the current folder, plus
 *  user interface elements for navigating to other folders.
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 * \param [in] folder  Pointer to the location to set as current
 */
void geda_file_chooser_set_current_folder (GtkWidget *widget, const char *folder)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    gtk_file_chooser_set_current_folder((GtkFileChooser*)widget, folder);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

/*!
 * \brief Set the current name of a GedaFileChooser
 * \par Function Description
 * Sets the current name in the file selector, as if entered by the user.
 * Note that the name passed in here is a UTF-8 string rather than a filename.
 * This function is meant for such uses as a suggested name in a "Save As..."
 * dialog. You can pass "Untitled.sch" or a similarly suitable suggestion for
 * the name.
 *
 * If you want to preselect a particular existing file, you should use
 * geda_file_chooser_set_filename instead.
 *
 * \param [in] widget Pointer to a hideous GtkFileChooser widget
 * \param [in] name   Pointer to the name to set as current
 */
void geda_file_chooser_set_current_name (GtkWidget *widget, const char *name)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    gtk_file_chooser_set_current_name((GtkFileChooser*)widget, name);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

/*!
 * \brief Retrieve Pointer to Extra GedaFileChooser Widget
 * \par Function Description
 *  Retrieves the application-supplied extra widget or NULL if
 *  an extra widget is not present.
 */
GtkWidget *geda_file_chooser_get_extra_widget(GtkWidget *hideous)
{
  GtkWidget *extra;

  if (GTK_IS_FILE_CHOOSER(hideous)) {
    extra = gtk_file_chooser_get_extra_widget((GtkFileChooser*)hideous);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    extra = NULL;
  }
  return extra;
}

/*!
 * \brief Add extra widgets to an File Chooser Dialog
 * \par Function Description
 * Add extra widgets to a file chooser to provide options not present
 * in the default design.  For example, to add a toggle button to give
 * users the option to open a file in read-only mode.
 *
 * example:
 * \code
 *   GtkWidget *toggle;
 *
 *   ...
 *
 *   toggle = gtk_check_button_new_with_label ("Open file read-only");
 *   gtk_widget_show (toggle);
 *   gtk_file_chooser_set_extra_widget (file_chooser, toggle);
 * \endcode
 *
 * A container such as a GtkBox can be used to set more than one extra
 * widget in the file chooser, set the container as the whole extra
 * widget.
 */
void geda_file_chooser_set_extra_widget (GtkWidget *hideous, GtkWidget *extra)
{
  if (GTK_IS_FILE_CHOOSER(hideous)) {
    gtk_file_chooser_set_extra_widget((GtkFileChooser*)hideous, extra);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

#undef ChooseClass

/** @} end group GedaFileChooser */
