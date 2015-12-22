/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <gschem_dialog.h>
#include <gschem_xdefines.h>
#include <geda_marshal.h>
#include <geda_debug.h>

enum {
  GEOMETRY_SAVE,
  GEOMETRY_RESTORE,
  LAST_SIGNAL
};

static unsigned int  gschem_dialog_signals[ LAST_SIGNAL ] = { 0 };
static GObjectClass *gschem_dialog_parent_class = NULL;

/*! \brief GschemDialog "geometry_save" class method handler
 *  \par Function Description
 *  Save the dialog's current position and size to the passed GKeyFile
 *
 *  \param [in] dialog     The GschemDialog to save the position and size of.
 *  \param [in] cfg        A Geda Configuration object.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void geometry_save (GschemDialog *dialog, EdaConfig *cfg, char* group_name)
{
  int x, y, width, height;

  gtk_window_get_position (GTK_WINDOW (dialog), &x, &y);
  gtk_window_get_size (GTK_WINDOW (dialog), &width, &height);
  eda_config_set_integer (cfg, group_name, "x", x);
  eda_config_set_integer (cfg, group_name, "y", y);
  eda_config_set_integer (cfg, group_name, "width", width);
  eda_config_set_integer (cfg, group_name, "height", height);
}

/*! \brief GschemDialog "geometry_restore" class method handler
 *  \par Function Description
 *  Restore dialog's last position and size from the passed GKeyFile
 *
 *  \param [in] dialog     The GschemDialog to restore the position and size of.
 *  \param [in] cfg        A Geda Configuration object.
 *  \param [in] group_name The group name in the key file to find the data under.
 */
static void
geometry_restore (GschemDialog *dialog, EdaConfig *cfg, char* group_name)
{
  int x, y, width, height;

  x      = eda_config_get_integer (cfg, group_name, "x", NULL);
  y      = eda_config_get_integer (cfg, group_name, "y", NULL);
  width  = eda_config_get_integer (cfg, group_name, "width",  NULL);
  height = eda_config_get_integer (cfg, group_name, "height", NULL);

  gtk_window_move (GTK_WINDOW (dialog), x, y);

  /* No need to call for a resize with a zero value */
  if ( width != 0 && height != 0) {
    gtk_window_resize (GTK_WINDOW (dialog), width, height);
  }
}

/*! \brief Return True if dialog is derived from GschemDialog Class */
bool is_a_gschem_dialog (void *dialog)
{
  GschemDialog *ptr = (GschemDialog*)dialog;
  return ptr && (GSCHEM_TYPE_DIALOG == ptr->tail_marker);
}

/* Begin Call Back Selection Handler */

/*! \brief Update the editing dialog when the page's selection changes.
 *  \par Function Description
 *  When the page's selection changes this function identifies how
 *  many objects which can have attributes are currently selected. If
 *  this number is 1, the dialog is set to edit the attributes of the
 *  first selected object..
 *
 *  \param [in] selection  The SELECTION object of page being edited.
 *  \param [in] user_data  The dialog.
 */
static void gd_callback_selection_changed (SELECTION *selection, void * user_data)
{
  GschemDialog   *Dialog;
  GschemToplevel *w_current;

  if (GSCHEM_IS_DIALOG(user_data)) {

    Dialog    = GSCHEM_DIALOG(user_data);
    w_current = Dialog->w_current;

    if (w_current != NULL) {

      Dialog->tracker (w_current, o_select_return_first_object (w_current));

    }
    else {
      BUG_MSG("GedaToplevel not set in Dialog\n");
    }
  }
  else {
    BUG_MSG("Bad pointer to Dialog\n");
  }
}

/*! \brief Update the dialog when the current page's SELECTION object
 *         is destroyed
 *  \par Function Description
 *  This handler is called when the g_object_weak_ref() on the
 *  SELECTION object we are watching expires. We reset the
 *  Dialog->selection pointer to NULL to avoid attempting to
 *  access the destroyed object.
 *
 *  \note
 *  Our signal handlers were automatically disconnected during the
 *  destruction process.
 *
 *  \param [in] data                  Pointer to a dialog
 *  \param [in] where_the_object_was  Pointer to where the object was
 *                                    just destroyed
 */
static void gd_callback_selection_finalized (void *data, GObject *where_the_object_was)
{
  GschemDialog *Dialog;
  if (GSCHEM_IS_DIALOG(data)) {
    Dialog = (GschemDialog*)data;
    Dialog->selection = NULL;
    GEDA_OBJECT_SET_DATA (Dialog, NULL, DIALOG_SELECTION_TRACKER);
  }
  else {
    BUG_MSG("Bad pointer to Dialog\n");
  }
}

/*! \brief Add link between modeless dialog and current selection.
 *  \par Function Description
 *  This function connects a handler to the "changed" signal of
 *  current selection to let the dialog watch it. It also adds a weak
 *  reference on the selection.
 *
 *  \param [in] maybe  Pointer to a GschemDialog dialog.
 */
static void gd_connect_selection (void *maybe)
{
  GschemDialog *Dialog;

  if (GSCHEM_IS_DIALOG(maybe)) {

    Dialog = maybe;

    Dialog->selection = Dialog->w_current->toplevel->page_current->selection_list;

    if (Dialog->selection && G_IS_OBJECT(Dialog->selection)) {
      g_object_weak_ref (G_OBJECT (Dialog->selection), gd_callback_selection_finalized, Dialog);
      g_signal_connect (Dialog->selection, "changed", (GCallback)gd_callback_selection_changed, Dialog);
    }
    else {
      Dialog->selection = NULL;
    }

    GEDA_OBJECT_SET_DATA (Dialog, Dialog->selection, DIALOG_SELECTION_TRACKER);
  }
  else {
    BUG_MSG("Bad pointer to Dialog\n");
  }
}

/*! \brief Remove the link between Dialog and selection.
 *  \par Function Description
 *  If the dialog is watching a selection, this function disconnects
 *  the "changed" signal and removes the weak reference that was
 *  previously added on it.
 *
 *  \param [in] Dialog  The Multiattrib dialog.
 */
static void gd_disconnect_selection (GschemDialog *Dialog) {

  SELECTION *selection = Dialog->selection;

  if (selection && G_IS_OBJECT(selection)) {

    g_signal_handlers_disconnect_matched (selection,
                                          G_SIGNAL_MATCH_FUNC |
                                          G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL,
                                          gd_callback_selection_changed,
                                          Dialog);

    Dialog->selection = NULL;
  }

  /* get selection watched from dialog data */
  selection = (SELECTION*)GEDA_OBJECT_GET_DATA(Dialog, DIALOG_SELECTION_TRACKER);

  if (selection && G_IS_OBJECT(selection)) {

      g_object_weak_unref (G_OBJECT (selection),
                           gd_callback_selection_finalized,
                           Dialog);
  }

  /* reset Dialog data */
  GEDA_OBJECT_SET_DATA (Dialog, NULL, DIALOG_SELECTION_TRACKER);
}

/*! \brief GtkWidget show signal handler
 *
 *  \par Function Description
 *  Just before the Dialog widget is shown, call the hook
 *  to restore its previously saved position and size.
 *
 *  \param [in] widget  The GtkWidget being shown.
 */
static void show_handler (GtkWidget *widget)
{
  EdaConfig    *cfg;
  GschemDialog *dialog;
  char         *group;

  dialog = GSCHEM_DIALOG (widget);
  cfg    = eda_config_get_user_context ();
  group  = dialog->settings_name;

  if (group != NULL) {
    if ( cfg != NULL ) {
        g_signal_emit (dialog, gschem_dialog_signals[ GEOMETRY_RESTORE ], 0,
                       cfg, group);
    }
    else {
         BUG_MSG("cfg=NULL, could not restore dialog geometry\n");
    }
  }

  /* Let GTK show the window */
  GTK_WIDGET_CLASS (gschem_dialog_parent_class)->show (widget);
}

/*! \brief GtkWidget unmap signal handler
 *
 *  \par Function Description
 *  Just before the dialog widget is unmapped, call the hook
 *  to save its current position and size.
 *
 *  This typically happens when you call gtk_widget_destroy().
 *
 *  \param [in] widget  The GtkWidget being unmapped.
 */
static void unmap_handler (GtkWidget *widget)
{
  EdaConfig    *cfg;
  GschemDialog *dialog;
  char         *group_name;

  dialog     = GSCHEM_DIALOG (widget);
  cfg        = eda_config_get_user_context ();
  group_name = dialog->settings_name;

  if (group_name != NULL) {

    if( cfg != NULL )
      g_signal_emit (dialog, gschem_dialog_signals[ GEOMETRY_SAVE ], 0,
                     cfg, group_name);
  }

  /* Disconnect the update selection handler function
  if (dialog->tracker != NULL) {
    gd_disconnect_selection (dialog);
  }
  */
  /* Let GTK unmap the window */
  GTK_WIDGET_CLASS (gschem_dialog_parent_class)->unmap (widget);
}

/*! \brief Set w_current dialog pointer to NULL
 *  \par Function Description
 *  This function automatically sets the toplevel entry for a
 *  GschemDialog to NULL when the dialog is destroyed. Note
 *  that if the dialog code set the toplevel entry to NULL
 *  this function has no effect.
 *
 *  \note This is an on-instance bases so only one of the if's
 *        can be true for a given child instance.
 */
static void set_gschem_dialog_null(void *dialog)
{
    GschemToplevel *w_current = ((GschemDialog*)dialog)->w_current;

    if (dialog == w_current->sswindow) {      /* Snap Size */
        w_current->sswindow = NULL;
    }
    else if (dialog == w_current->tswindow) { /* Text size */
        w_current->tswindow = NULL;
    }
    else if (dialog == w_current->aawindow) { /* Arc Attrib */
        w_current->aawindow = NULL;
    }
    else if (dialog == w_current->clwindow) { /* Color Edit */
        w_current->clwindow = NULL;
    }
    else if (dialog == w_current->hpwindow) { /* Hatch Pattern */
        w_current->hpwindow = NULL;
    }
    else if (dialog == w_current->ltwindow) { /* Line Type */
        w_current->ltwindow = NULL;
    }
    else if (dialog == w_current->prwindow) { /* Prop edit */
        w_current->prwindow = NULL;
    }
    else if (dialog == w_current->ptwindow) { /* Pin Type */
        w_current->ptwindow = NULL;
    }
    else if (dialog == w_current->sewindow) { /* Slot Edit */
        w_current->sewindow = NULL;
    }
    else if (dialog == w_current->tewindow) { /* Text Edit */
        w_current->tewindow = NULL;
    }
    else if (dialog == w_current->ftwindow) { /* Find Text */
        w_current->ftwindow = NULL;
    }
    else if (dialog == w_current->htwindow) { /* Hide Text */
        w_current->htwindow = NULL;
     }
    else if (dialog == w_current->stwindow) { /* Show Text */
        w_current->stwindow = NULL;
    }
    else if (dialog == w_current->tiwindow) { /* Text Input */
        w_current->tiwindow = NULL;
    }
    else if (dialog == w_current->trwindow) { /* Translate */
        w_current->trwindow = NULL;
    }
    else if (dialog == w_current->hkwindow) { /* HotKeys */
        w_current->hkwindow = NULL;
    }
    else if (dialog == w_current->cowindow) { /* Coordinates */
        w_current->cowindow = NULL;
    }
    else if (dialog == w_current->aewindow) { /* Attribute Edit */
        w_current->aewindow = NULL;
    }
    else if (dialog == w_current->cpwindow) { /* Configuration Preferences */
        w_current->cpwindow = NULL;
    }
    else if (dialog == w_current->cswindow) { /* component select */
        w_current->cswindow = NULL;
    }
    else if (dialog == w_current->mawindow) { /* multi attribute */
        w_current->mawindow = NULL;
    }
    else if (dialog == w_current->pswindow) { /* page select */
        w_current->pswindow = NULL;
    }
}

/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GschemDialog GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void gschem_dialog_finalize (GObject *object)
{
  GschemDialog *dialog = GSCHEM_DIALOG (object);

  if (dialog->tracker != NULL) {
    gd_disconnect_selection (dialog);
  }

  /* Make sure w_current entry is set to NULL */
  set_gschem_dialog_null(dialog);

  GEDA_FREE (dialog->settings_name);

  G_OBJECT_CLASS (gschem_dialog_parent_class)->finalize (object);
}

/* End CallBack Selection Handler*/

/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GschemDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
gschem_dialog_set_property (GObject *object, unsigned int property_id,
                            const GValue *value, GParamSpec *pspec)
{
  GschemDialog *Dialog = GSCHEM_DIALOG (object);

  switch(property_id) {
    case PROP_SETTINGS_NAME:
      GEDA_FREE (Dialog->settings_name);
      Dialog->settings_name = g_value_dup_string (value);
      break;

    case PROP_GSCHEM_TOPLEVEL:
      Dialog->w_current = (GschemToplevel*)g_value_get_pointer (value);
      break;

    case PROP_PARENT_WINDOW:
      gschem_dialog_set_parent(Dialog, (GtkWindow*) g_value_get_pointer (value));
      break;

    case PROP_SELECTION_LIST:
      gd_disconnect_selection (Dialog);
      Dialog->selection = g_value_get_pointer(value);
      gd_connect_selection (Dialog);
      g_signal_emit_by_name (Dialog->selection, "changed", Dialog);
      break;

    case PROP_SELECTION_TRACKER:
      /* disconnect Dialog from any previous selection */
      gd_disconnect_selection (Dialog);
      Dialog->tracker = g_value_get_pointer(value);
      /* connect the Dialog to the selection of the current page */
      gd_connect_selection (Dialog);
      break;

    case PROP_TITLE:
      gtk_window_set_title (GTK_WINDOW (object), g_value_get_string (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GschemDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void gschem_dialog_get_property (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec)
{
  GschemDialog *Dialog = GSCHEM_DIALOG (object);

  switch(property_id) {
      case PROP_SETTINGS_NAME:
        g_value_set_string (value, Dialog->settings_name);
        break;

      case PROP_GSCHEM_TOPLEVEL:
        g_value_set_pointer (value, (void *)Dialog->w_current);
        break;

      case PROP_PARENT_WINDOW:
        g_value_set_pointer (value, Dialog->parent_window);
        break;

      case PROP_TITLE:
        g_value_set_string (value, gtk_window_get_title(GTK_WINDOW(object)));
       break;

      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

/*! \brief Type class initializer for GschemDialog
 *
 *  \par Function Description
 *  Type class initializer for GschemDialog. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The GschemDialogClass we are initialising
 */
static void gschem_dialog_class_init (GschemDialogClass *klass)
{
  GObjectClass     *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *gtkwidget_class = GTK_WIDGET_CLASS (klass);

  klass->geometry_save            = geometry_save;
  klass->geometry_restore         = geometry_restore;

  gtkwidget_class->show           = show_handler;
  gtkwidget_class->unmap          = unmap_handler;

  gobject_class->finalize         = gschem_dialog_finalize;
  gobject_class->set_property     = gschem_dialog_set_property;
  gobject_class->get_property     = gschem_dialog_get_property;

  gschem_dialog_parent_class      = g_type_class_peek_parent (klass);

  gschem_dialog_signals[ GEOMETRY_SAVE ] =
    g_signal_new ("geometry-save",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET( GschemDialogClass, geometry_save ),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  geda_marshal_VOID__POINTER_STRING,
                  G_TYPE_NONE,
                  2,    /* n_params */
                  G_TYPE_POINTER,
                  G_TYPE_STRING
                 );

  gschem_dialog_signals[ GEOMETRY_RESTORE ] =
    g_signal_new ("geometry-restore",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET( GschemDialogClass, geometry_restore ),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  geda_marshal_VOID__POINTER_STRING,
                  G_TYPE_NONE,
                  2,    /* n_params */
                  G_TYPE_POINTER,
                  G_TYPE_STRING
                 );

  g_object_class_install_property (
    gobject_class, PROP_SETTINGS_NAME,
    g_param_spec_string ("settings-name",
                         "",
                         "",
                         NULL,
                         G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));

  g_object_class_install_property (
    gobject_class, PROP_GSCHEM_TOPLEVEL,
    g_param_spec_pointer ("gschem-toplevel",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));

  g_object_class_install_property (
    gobject_class, PROP_PARENT_WINDOW,
    g_param_spec_pointer ("parent",
                        _("Parent Window"),
                        _("Parent window for this dialog"),
                          G_PARAM_READWRITE));

  g_object_class_install_property (
    gobject_class, PROP_SELECTION_LIST,
    g_param_spec_pointer (DIALOG_SELECTION_DATA,
                          "",
                          "",
                          G_PARAM_WRITABLE));

  g_object_class_install_property (
    gobject_class, PROP_SELECTION_TRACKER,
    g_param_spec_pointer (DIALOG_SELECTION_TRACKER,
                          "",
                          "",
                          G_PARAM_WRITABLE));

  g_object_class_install_property (
    gobject_class, PROP_TITLE,
    g_param_spec_string ("title",
                       _("Title"),
                       _("Dialog window title"),
                         NULL,
                         G_PARAM_READWRITE));

}

/*! \brief GedaType instance initializer for a GschemDialog object
 *
 *  \par Function Description
 *  GedaType instance initializer for an Object, initializes a new empty
 *  Object by setting the head and tail markers to the GedaType value.
 *
 *  \param [in]  instance  The Object being initialising.
 *  \param [in]  g_class   The class of the type the instance is created for.
 */
static void gschem_dialog_instance_init(GTypeInstance *instance, void *g_class)
{
  GschemDialog *dialog  = (GschemDialog*)instance;

  dialog->tail_marker   = GSCHEM_TYPE_DIALOG;

  /* Set properties here to apply regardless of how dialog created */
  g_object_set (dialog, "border-width", DIALOG_BORDER_WIDTH, NULL);
  g_object_set (GTK_DIALOG (dialog)->vbox, "spacing", DIALOG_V_SPACING, NULL);
}

/*! \brief Retrieve GschemDialog's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GattribDialog's Type identifier. On first call,
 *  this registers the GattribDialog in the GedaType system. Subsequently
 *  the function returns the saved value from its first execution.
 *
 *  \return GedaType identifier associated with GschemDialog.
 */
GedaType gschem_dialog_get_type (void)
{
  static GedaType gschem_dialog_type = 0;

  if (!gschem_dialog_type) {
    static const GTypeInfo gschem_dialog_info = {
      sizeof(GschemDialogClass),
      NULL,                        /* base_init */
      NULL,                        /* base_finalize */
      (GClassInitFunc) gschem_dialog_class_init,
      NULL,                        /* class_finalize */
      NULL,                        /* class_data */
      sizeof(GschemDialog),
      0,                           /* n_preallocs */
      gschem_dialog_instance_init  /* instance_init */
    };

    gschem_dialog_type = g_type_register_static (GTK_TYPE_DIALOG,
                                                 "GschemDialog",
                                                 &gschem_dialog_info, 0);
  }

  return gschem_dialog_type;
}

/*! \brief Add variable number of buttons to a GschemDialog
 *  \par Function Description
 *  Internal function which adds buttons to a pre-existing GtkDialog.
 *  This function was modified from GTK+-2.4.14 gtkdialog.c to support
 *  gschem_dialog_new_with_buttons(...)
 *
 *  \param [in]  dialog             The GtkDialog buttons are being added to
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  args               The va_list containging the remaining button strings
 */
static void gschem_dialog_add_buttons_valist (GtkDialog     *dialog,
                                              const char    *first_button_text,
                                              va_list         args)
{
  const char* text;
  int response_id;

  //g_return_if_fail (GTK_IS_DIALOG (dialog));

  if (first_button_text == NULL)
    return;

  text = first_button_text;
  response_id = va_arg (args, int);

  while (text != NULL) {

    gtk_dialog_add_button (dialog, text, response_id);

    text = va_arg (args, char*);
    if (text == NULL)
      break;
    response_id = va_arg (args, int);
  }
}

/*! \brief Internal GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to support gschem_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a blank GschemDialog with various options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name gschem should use to store this dialog's settings
 *  \param [in]  w_current          The GschemToplevel object this dialog is associated with
 *
 *  \return  The GschemDialog created.
 */
 GtkWidget* gschem_dialog_new_empty (const char            *title,
                                           GtkWindow       *parent,
                                           GtkDialogFlags   flags,
                                           const char      *settings_name,
                                           GschemToplevel  *w_current)
 {
   GschemDialog *dialog;

   dialog = g_object_new (GSCHEM_TYPE_DIALOG,
                          "settings-name",   settings_name,
                          "gschem-toplevel", w_current,
                          NULL);

   if (title)
     gtk_window_set_title (GTK_WINDOW (dialog), _(title));

   if (parent)
     gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);

   if (flags & GTK_DIALOG_MODAL)
     gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

   gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);

   if (flags & GTK_DIALOG_NO_SEPARATOR)
     gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

   gtk_window_set_type_hint (GTK_WINDOW (dialog), GDK_WINDOW_TYPE_HINT_DIALOG);

   return GTK_WIDGET (dialog);
 }


/*! \brief GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to provide a GschemDialog equivelant of the convenience function
 *  gtk_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a GschemDialog with buttons and options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name gschem should use to store this dialog's settings
 *  \param [in]  w_current          The GschemToplevel object this dialog is associated with
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  ...                A variable number of arguments with the remaining button strings
 *
 *  \return  The GschemDialog created.
 */
GtkWidget* gschem_dialog_new_with_buttons (const char *title, GtkWindow *parent, GtkDialogFlags flags,
                                           const char *settings_name, GschemToplevel *w_current,
                                           const char *first_button_text, ...)
{
  GschemDialog *dialog;
  va_list args;

  dialog = GSCHEM_DIALOG (gschem_dialog_new_empty (title, parent, flags, settings_name, w_current));

  va_start (args, first_button_text);

  gschem_dialog_add_buttons_valist (GTK_DIALOG (dialog),
                                    first_button_text,
                                    args);

  va_end (args);

  return GTK_WIDGET (dialog);
}

GtkWindow *gschem_dialog_get_parent(GschemDialog *dialog)
{
  if( !GSCHEM_IS_DIALOG(dialog))
    fprintf(stderr, "Error, <gschem_dialog_get_parent> object is not a GschemDialog\n");
  else
    return dialog->parent_window;
  return NULL;
}

void gschem_dialog_set_parent(GschemDialog *dialog, GtkWindow *parent)
{
  if( !GSCHEM_IS_DIALOG(dialog))
    fprintf (stderr, "Error, <gschem_dialog_set_parent> parameter 1 is not a GschemDialog\n");
  else if (!GTK_IS_WINDOW(parent))
    fprintf (stderr, "Error, <gschem_dialog_set_parent> parameter 2 is not a GtkWindow\n");
  else {
    if (dialog->parent_window) {
      gtk_window_set_transient_for (GTK_WINDOW (dialog), NULL);
    }
    dialog->parent_window = parent;
    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);
  }
}

GList *gschem_dialog_get_selected(GschemDialog *dialog)
{
  if (GSCHEM_IS_DIALOG(dialog) && dialog->selection)
    return dialog->selection->glist;
  return NULL;
}

const char *gschem_dialog_get_title(GschemDialog *dialog)
{
  return gtk_window_get_title(GTK_WINDOW(dialog));
}
void gschem_dialog_set_title(GschemDialog *dialog, const char*title)
{
  gtk_window_set_title (GTK_WINDOW (dialog), title);
}
