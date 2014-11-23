/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
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
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 *  Date: August, 05, 2014
 *  Contributing Author: Wiley Edward Hill
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gtk/gtk.h>

#include <libgeda/libgeda.h>

#include <geda_file_chooser.h>
#include <geda_file_filter.h>
#include <geda_debug.h>

#include "gettext.h"

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

static GtkEntry *chooser_entry;

static GedaFileFilterDataDef filter_data[] = {
    GEDA_FILTER_SCHEMATIC,
    GEDA_FILTER_SYMBOL,
    GEDA_FILTER_GSCHEM,
    GEDA_FILTER_NONE,
    GEDA_NO_MORE_FILTERS
};

/*! \brief Creates filter for Geda File Chooser.
 *  \par Function Description
 *  This function adds file filters to <B>filechooser</B>.
 *
 *  \param [in] filechooser The file chooser to apply filter to.
 */
static void
geda_file_chooser_setup_filters (GtkFileChooser *filechooser)
{
  GtkFileFilter         *filter;
  GedaFileFilterDataDef *data;
  int i;

  for (data = filter_data; data->name != NULL; data++) {
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name(filter, data->name);
    for (i = 0; data->pattern[i] != '\0'; i++) {
      const char *ext = data->pattern[i];
      gtk_file_filter_add_pattern (filter, ext);
    }
    g_object_set_data( G_OBJECT(filter), "id", GINT_TO_POINTER(data->id));
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
     gtk_container_forall ( GTK_CONTAINER (widget), FixGtkCrap, self);
  }
}

static void look_for_entry(GtkWidget *widget, void *self)
{
  if (GTK_IS_ENTRY(widget)) {
    chooser_entry = (GtkEntry*)widget;
  }
  else if (GTK_IS_CONTAINER(widget)) {
     gtk_container_forall ( GTK_CONTAINER (widget), look_for_entry, self);
  }
}

static void
geda_file_chooser_find_entry (GtkWidget *chooser)
{
  GList   *children, *iter;

  /* Get all objects inside the dialog */
  children = gtk_container_get_children (GTK_CONTAINER (chooser));

  for (iter = children; iter; iter = iter->next) {

    if (GTK_IS_CONTAINER(iter->data)) {

      gtk_container_forall ( GTK_CONTAINER (iter->data), look_for_entry, chooser);

      if (chooser_entry != NULL) {
        break;
      }
    }
  }
  g_list_free (children);
}

static GObject *
geda_file_chooser_constructor (GedaType               type,
                               unsigned int           n_properties,
                               GObjectConstructParam *properties)
{
  GObject *obj;
  GList   *children, *iter;


  /* Chain up to the parent constructor */
  obj = G_OBJECT_CLASS (geda_file_chooser_parent_class)->constructor (type, n_properties, properties);

  gtk_dialog_set_has_separator (GTK_DIALOG(obj), TRUE);

  /* Get all object inside the contents area of the dialog */
  children = gtk_container_get_children (GTK_CONTAINER (GTK_DIALOG (obj)->vbox));

  /* For each container in the contents area to call look for combo box */
  for (iter = children; iter; iter = iter->next) {
    if (GTK_IS_CONTAINER(iter->data)) {
      gtk_container_forall ( GTK_CONTAINER (iter->data), FixGtkCrap, obj);
      if ((GEDA_FILE_CHOOSER(obj))->filter_button) {
        break;
      }
    }
  }

  g_list_free (children);

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

  chooser_entry = NULL;

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

  switch (property_id)
    {
    case PROP_FILTER_INDEX:
      g_value_set_boolean (value, chooser->filter_index);
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
      geda_file_chooser_set_filter((GtkWidget*)chooser, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*! \brief GedaFileChooser "geometry_restore" class method handler
 *  \par Function Description
 *  Restore dialog's last position and size from the passed GKeyFile
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
  cfg    = eda_config_get_user_context ();

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
 *  Save the dialog's current position and size to the passed GKeyFile
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
  char *group = "file-chooser";

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
  char *group = "file-chooser";

  g_signal_emit (GEDA_FILE_CHOOSER (widget),
                 chooser_signals[ GEOMETRY_SAVE ], 0, group);

  /* Let Gtk unmap the window */
  GTK_WIDGET_CLASS (geda_file_chooser_parent_class)->unmap (widget);
}

/*! \brief Type class initialiser for GedaFileChooser
 *
 *  \par Function Description
 *  Type class initialiser for GedaFileChooser. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  class       The GedaFileChooserClass we are initialising
 */
static void
geda_file_chooser_class_init (GedaFileChooserClass *class)
{
  GParamSpec     *params;

  GObjectClass   *gobject_class  = (GObjectClass*) class;
  GtkWidgetClass *widget_class   = (GtkWidgetClass*) class;

  gobject_class->get_property    = geda_file_chooser_get_property;
  gobject_class->set_property    = geda_file_chooser_set_property;
  gobject_class->constructor     = geda_file_chooser_constructor;
  gobject_class->finalize        = geda_file_chooser_finalize;

  class->filter_changed          = geda_file_chooser_filter_changed;
  class->geometry_save           = geda_file_chooser_geometry_save;
  class->geometry_restore        = geda_file_chooser_geometry_restore;

  widget_class->show             = show_handler;
  widget_class->unmap            = unmap_handler;

  geda_file_chooser_parent_class = g_type_class_peek_parent (class);

  params = g_param_spec_int ("filter-index",
                           _("Set or retrieve index of the filter combo"), /* nick name */
                           _("IDE_FILTER"),     /* hint / blurb */
                              FILTER_SCHEMATIC, /* Min value, be zero */
                              FILTER_GSCHEM,    /* Max value */
                              FILTER_GSCHEM,    /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_FILTER_INDEX, params);

  /**
   * GedaFileChooser::filter-changed:
   * Chooser: The chooser on which the signal is emitted
   *
   * The  GedaFileChooser::filter-changed signal is emitted when the user
   * changes the selection of the filter combo text box.
   */

  chooser_signals[FILTER_CHANGED]     = g_signal_new ("filter-changed",
                                                      geda_file_chooser_get_type(),
                                                      G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                                                      G_STRUCT_OFFSET (GedaFileChooserClass,
                                                                       filter_changed),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      g_cclosure_marshal_VOID__VOID,
                                                      G_TYPE_NONE, 0);

  chooser_signals[ GEOMETRY_RESTORE ] = g_signal_new ("geometry-restore",
                                                      geda_file_chooser_get_type(),
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GedaFileChooserClass,
                                                                       geometry_restore ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      g_cclosure_marshal_VOID__STRING,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);

  chooser_signals[ GEOMETRY_SAVE ]    = g_signal_new ("geometry-save",
                                                      geda_file_chooser_get_type(),
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GedaFileChooserClass,
                                                                       geometry_save ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      g_cclosure_marshal_VOID__STRING,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);
}

/*! \brief Initialize GedaFileChooser data structure.
 *
 *  \par Function Description
 *  Function tois call after the GedaFileChooserClass is created
 *  to initialize the data structure.
 *
 * \param [in] self A GedaFileChooser object (structure)
 */
static void geda_file_chooser_init (GedaFileChooser *self)
{
  chooser_entry       = NULL;
  self->filter_button = NULL;
}

/*! \brief Function to retrieve GedaFileChooser's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GedaFileChooser's Type identifier. On the first
 *  call, this registers the GedaFileChooser in the GedaType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the GedaType identifier associated with GedaFileChooser.
 */
GedaType geda_file_chooser_get_type ()
{
  static GedaType geda_file_chooser_type = 0;

  if (!geda_file_chooser_type) {
    static const GTypeInfo geda_file_chooser_info = {
      sizeof(GedaFileChooserClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) geda_file_chooser_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(GedaFileChooser),
      0,    /* n_preallocs */
      (GInstanceInitFunc) geda_file_chooser_init, /* instance_init */
    };

    geda_file_chooser_type = g_type_register_static (GTK_TYPE_FILE_CHOOSER_DIALOG,
                                                     "GedaFileChooser",
                                                     &geda_file_chooser_info,
                                                     0);
  }
  return geda_file_chooser_type;
}

/*! \brief Instantiate a New Geda File Chooser Dialog
 *  to provide a GedaFileChooser equivelant of the convenience function
 *  gtk_file_chooser_dialog_new(...)
 *
 *  \par Function Description
 *  Convenience function which creates a GedaFileChooser with buttons and options.
 *
 *  \param [in]  parent             The GtkWindow Widget which will parent this dialog
 *  \param [in]  chooser_action     The #FileChooserAction to use when setting up the dialog
 *
 *  \return  The GedaFileChooser created.
 */
GtkWidget*
geda_file_chooser_new (GtkWidget *parent,
                       FileChooserAction chooser_action)
{
  GtkWidget       *widget;
  GtkDialog       *dialog;
  GedaFileChooser *chooser;

  const char *second_button_text;
  const char *title = NULL;

  widget = g_object_new (geda_file_chooser_get_type(),
                         "action", chooser_action,
                         "select-multiple",
                         (chooser_action == FILE_CHOOSER_ACTION_OPEN),
                         NULL);

  if ( G_IS_OBJECT(widget)) {

    chooser = (GedaFileChooser*)widget;
    dialog  = (GtkDialog*)widget;

    switch (chooser_action) {
      case FILE_CHOOSER_ACTION_OPEN:
        second_button_text =  _("_Open");
        title = _("Open...");
        break;

      case FILE_CHOOSER_ACTION_SAVE:
        second_button_text = _("_Save");
        title = _("Save As..");
        break;

      case FILE_CHOOSER_ACTION_SELECT_FOLDER:
        second_button_text = _("_Select Folder...");
        title = _("Select Folder");
        break;

      default:
        second_button_text = _("OOPS");
        break;
    }

    gtk_dialog_add_buttons (dialog,
                            _("_Cancel"),       GTK_RESPONSE_CANCEL,
                            second_button_text, GTK_RESPONSE_ACCEPT,
                            NULL);

    /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(dialog,
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_CANCEL,
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
    gtk_dialog_set_default_response (dialog, GTK_RESPONSE_ACCEPT);
  }
  else {
    widget = NULL;
  }

  return widget;
}

static GtkWidget *
geda_file_chooser_dialog_new_valist (const char        *title,
                                     GtkWindow         *parent,
                                     FileChooserAction  action,
                                     const char        *first_button_text,
                                     va_list            varargs)
{
  GtkWidget  *result;
  const char *button_text = first_button_text;
  int         response_id;

  result = g_object_new (geda_file_chooser_get_type(),
                         "title", title,
                         "action", action,
                         NULL);

  if (parent) {
    gtk_window_set_transient_for (GTK_WINDOW (result), parent);
  }

  while (button_text) {
      response_id = va_arg (varargs, gint);
      gtk_dialog_add_button (GTK_DIALOG (result), button_text, response_id);
      button_text = va_arg (varargs, const char *);
  }

  return result;
}

/*! \brief Create a New GedaFileChooser specifying Buttons
 *
 *  \par Function Description
 * Creates a new #GedaFileChooser. This function is analogous to
 * gtk_dialog_new_with_buttons().
 *
 * \param [in] title  (allow-none): Title of the dialog, or %NULL
 * \param [in] parent (allow-none): Transient parent of the dialog, or %NULL
 * \param [in] action Open or save mode for the dialog
 * \param [in] first_button_text (allow-none): stock ID or text to go in the first button, or %NULL
 * \param [in] ... response ID for the first button, then additional (button, id) pairs, ending with %NULL
 *
 * \return a new #GedaFileChooser
 *
 */
GtkWidget *
geda_file_chooser_dialog_new_full (const char       *title,
                                   GtkWindow        *parent,
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

/*! \brief Get Geda File Chooser Entry Widget
 *  \par Function Description
 *  This function returns a pointer to the internal GtkEntry widget
 *
 *  \param [in] widget The file chooser widget.
 *
 *  \returns GtkEntry object
 */
GtkEntry *geda_file_chooser_get_entry (GtkWidget *widget)
{
  if (chooser_entry == NULL) {
    geda_file_chooser_find_entry (widget);
  }
  return chooser_entry;
}

char*
geda_file_chooser_get_entry_text(GtkWidget *despicable)
{
  char       *name;
  GtkEntry   *entry;

  name = NULL;

  if (GTK_IS_FILE_CHOOSER(despicable)) {

    entry = geda_file_chooser_get_entry(despicable);

    if (GTK_IS_ENTRY(entry)) {

      if (gtk_entry_get_text_length (entry)) {
        name = u_string_strdup (gtk_entry_get_text(entry));
      }
    }
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    name = NULL;
  }
  return name;
}

char*
geda_file_chooser_get_filename(GtkWidget *hideous)
{
  char     *name;

  if (GTK_IS_FILE_CHOOSER(hideous)) {
    name = gtk_file_chooser_get_filename((GtkFileChooser*)hideous);

  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    name = NULL;
  }
  return name;
}

void
geda_file_chooser_set_filename (GtkWidget *hideous, const char *name)
{
  if (GTK_IS_FILE_CHOOSER(hideous)) {
    gtk_file_chooser_set_filename((GtkFileChooser*)hideous, name);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

GSList*
geda_file_chooser_get_filenames(GtkWidget *hideous)
{
  GSList *list;

  if (GTK_IS_FILE_CHOOSER(hideous)) {
    list = gtk_file_chooser_get_filenames((GtkFileChooser*)hideous);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    list = NULL;
  }
  return list;
}

char*
geda_file_chooser_get_current_folder(GtkWidget *hideous)
{
  char *folder;

  if (GTK_IS_FILE_CHOOSER(hideous)) {
    folder = gtk_file_chooser_get_current_folder((GtkFileChooser*)hideous);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
    folder = NULL;
  }
  return folder;
}

void
geda_file_chooser_set_current_folder (GtkWidget *hideous, const char *folder)
{
  if (GTK_IS_FILE_CHOOSER(hideous)) {
    gtk_file_chooser_set_current_folder((GtkFileChooser*)hideous, folder);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

void
geda_file_chooser_set_current_name (GtkWidget *hideous, const char *folder)
{
  if (GTK_IS_FILE_CHOOSER(hideous)) {
    gtk_file_chooser_set_current_name((GtkFileChooser*)hideous, folder);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

GtkWidget*
geda_file_chooser_get_extra_widget(GtkWidget *hideous)
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

void
geda_file_chooser_set_extra_widget (GtkWidget *hideous, GtkWidget *extra)
{
  if (GTK_IS_FILE_CHOOSER(hideous)) {
    gtk_file_chooser_set_extra_widget((GtkFileChooser*)hideous, extra);
  }
  else {
    BUG_MSG ("Operative is not a GtkFileChooser");
  }
}

/** @} end group GedaFileChooser */
