/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include "../include/gattrib.h"
#include "../include/gattrib_dialog.h"

#include <geda_uio_functions.h>
#include <geda/geda_stat.h>

/*!
 * \brief Create pixmap widget for dialogs boxes.
 * \par Function Description
 *  This is an internally used function to create pixmaps.
 *  The default bitmap directory is prefixed to the filename
 *  and if is valid then the image widget is created and returned. GtkWidget *widget,
 */
GtkWidget *create_pixmap (const char *filename)
{
  char *pathname = NULL;
  GtkWidget *pixmap;

  if (!filename || !filename[0])
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE ,
                                      GTK_ICON_SIZE_INVALID);

  pathname = g_build_filename (geda_sys_data_path (), "bitmaps", filename, NULL);

  if (!pathname) {

    geda_log_w("Could not find image at file: %s.\n", filename);
    return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE,
                                    GTK_ICON_SIZE_INVALID);
  }

  pixmap = gtk_image_new_from_file (pathname);

  GEDA_FREE (pathname);

  return pixmap;
}

/*!
 * \brief Retrieve a Geda switch image
 * \par Function Description
 *  This function creates is required by TOGGLE_SWITCH macro in
 *  include/geda/geda_dialog_controls.h to return the image widget
 *  based on WhichState.
 *
 * \returns: Newly created switch widget
 */
GtkWidget *get_geda_switch_image (bool WhichState)
{
   GtkWidget *image;

   if (WhichState) {
     image = create_pixmap (SWITCH_ON_IMAGE);
   }
   else {
     image = create_pixmap (SWITCH_OFF_IMAGE);
   }

   return image;
}

/*!
 * \brief Function to create a Geda switch image control.
 * \par Function Description
 *  This function creates a Check Box widget using an image, the checkbox
 *  indicator is disabled so only the images is displayed. This creates a
 *  control similar to a GTK3 Switch, using standard GTK2 controls. The
 *  On or Off images is controlled by the istate parameter.
 *
 * \returns Newly created switch widget
 */
GtkWidget*
create_geda_switch(GtkWidget *parent, GtkWidget *SwitchImage, bool istate)
{
  GtkWidget *widget;

  widget = gtk_check_button_new ();

  gtk_widget_show (widget);
  geda_container_add (parent, widget);
  gtk_widget_set_size_request (widget, -1, 30);

  /* turn off the indicator, i.e. box */
  gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (widget), FALSE);

  /* Set the value of the control, sets raised property */
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget), istate);

  SwitchImage = get_geda_switch_image( istate);
  gtk_widget_show (SwitchImage);
  geda_container_add (widget, SwitchImage);

  return widget;
}

enum {
  PROP_SETTINGS_NAME = 1,
  PROP_GATTRIB_GedaToplevel
};

enum {
  GEOMETRY_SAVE,
  GEOMETRY_RESTORE,
  LAST_SIGNAL
};

static unsigned int gattrib_dialog_signals[ LAST_SIGNAL ] = { 0 };
static GObjectClass *gattrib_dialog_parent_class = NULL;

static GedaKeyFile *dialog_geometry = NULL;

#define DIALOG_GEOMETRY_STORE "gattrib-dialog-geometry"

/*!
 * \brief Save all geometry data into a file.
 * \par Function Description
 *  This is called at program exit to save all window geometry data
 *  into a file
 *
 * \param [in] user_data unused
 */
static void save_geometry_to_file(void *user_data)
{
  char *data, *file;

  if ( dialog_geometry != NULL ) {

    data = geda_keyfile_to_data(dialog_geometry, NULL, NULL);
    file = g_build_filename(geda_user_config_path (), DIALOG_GEOMETRY_STORE,
                            NULL);
    g_file_set_contents(file, data, -1, NULL);
    GEDA_FREE(data);
    GEDA_FREE(file);
  }
}

/*!
 * \brief GattribDialog "geometry_save" class method handler
 * \par Function Description
 *  Save the dialog's current position and size to the passed GedaKeyFile
 *
 * \param [in] dialog     The GattribDialog to save the position and size of.
 * \param [in] key_file   The GedaKeyFile to save the geometry data to.
 * \param [in] group_name The group name in the key file to store the data under.
 */
static void geometry_save (GattribDialog *dialog, GedaKeyFile *key_file, char *group_name)
{
  int x, y, width, height;

  gtk_window_get_position ((GtkWindow*)dialog, &x, &y);
  gtk_window_get_size ((GtkWindow*)dialog, &width, &height);

  geda_keyfile_set_integer (key_file, group_name, "x", x);
  geda_keyfile_set_integer (key_file, group_name, "y", y);
  geda_keyfile_set_integer (key_file, group_name, "width",  width );
  geda_keyfile_set_integer (key_file, group_name, "height", height);
}

/*!
 * \brief GattribDialog "geometry_restore" class method handler
 * \par Function Description
 *  Restore dialog's last position and size from the passed GedaKeyFile.
 *
 * \param [in] dialog     The GattribDialog to restore the position and size of
 * \param [in] key_file   The GedaKeyFile to load the geometry data from
 * \param [in] group_name The group name in the key file to find the data under
 */
static void geometry_restore (GattribDialog *dialog, GedaKeyFile *key_file, char *group_name)
{
  int x, y, width, height;

  x      = geda_keyfile_get_integer (key_file, group_name, "x", NULL);
  y      = geda_keyfile_get_integer (key_file, group_name, "y", NULL);
  width  = geda_keyfile_get_integer (key_file, group_name, "width",  NULL);
  height = geda_keyfile_get_integer (key_file, group_name, "height", NULL);

  gtk_window_move ((GtkWindow*)dialog, x, y);
  gtk_window_resize ((GtkWindow*)dialog, width, height);
}

/*!
 * \brief Setup the GedaKeyFile for saving / restoring geometry
 * \par Function Description
 *  Check if the GedaKeyFile for saving / restoring geometry is open.
 *  If it doesn't exist, we create it here, and also install a hook
 *  to ensure its contents are saved at program exit.
 */
static inline void setup_keyfile ()
{
  if (!GEDA_IS_KEYFILE(dialog_geometry)) {

    char *file;

    dialog_geometry = geda_keyfile_new();

    file = g_build_filename (geda_user_config_path (),
                            DIALOG_GEOMETRY_STORE, NULL);

    /* Remember to save data on program exit */
    geda_atexit(save_geometry_to_file, NULL);

    if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
      geda_create_path (geda_user_config_path (), S_IRWXU | S_IRWXG);
      g_file_set_contents (file, "", -1, NULL);
    }

    if (!geda_keyfile_load_from_file (dialog_geometry, file, G_KEY_FILE_NONE, NULL)) {

      /* If verbose then let the user know what happened */
      if (verbose_mode) {
        fprintf(stderr,"%s: \"%s\"\n",  _("Could not load geometry from file"), file);
      }

      /* error opening key file, create an empty one and try again */
      g_file_set_contents (file, "", -1, NULL);

      if (!geda_keyfile_load_from_file (dialog_geometry, file, G_KEY_FILE_NONE, NULL)) {
        fprintf(stderr,"%s: \"%s\"\n",  _("error creating file"), file);
      }
    }
    GEDA_FREE (file);
  }
}

/*!
 * \brief Widget show signal handler
 * \par Function Description
 *  This function is called before the dialog widget is shown
 *  to restore the previously saved position and size.
 *
 * \param [in] widget  The GtkWidget being shown.
 */
static void show_handler (GtkWidget *widget)
{
  GattribDialog *dialog = (GattribDialog*)widget;
  char *group_name;

  group_name = dialog->settings_name;

  if (group_name != NULL) {

    setup_keyfile ();

    if ( GEDA_IS_KEYFILE(dialog_geometry) ) {

      if (geda_keyfile_has_group (dialog_geometry, group_name)) {
        g_signal_emit (dialog, gattrib_dialog_signals[ GEOMETRY_RESTORE ], 0,
                       dialog_geometry, group_name);
      }
    }
  }

  /* Let GTK show the window */
  ((GtkWidgetClass*)gattrib_dialog_parent_class)->show (widget);
}

/*!
 * \brief GtkWidget unmap signal handler
 * \par Function Description
 *  This function is called before the dialog widget is unmapped
 *  to save its current position and size.
 *
 *  This typically happens when you call gtk_widget_destroy().
 *
 * \param [in] widget  The GtkWidget being unmapped.
 */
static void unmap_handler (GtkWidget *widget)
{
  char *group_name;
  GattribDialog *dialog = (GattribDialog*)widget;

  group_name = dialog->settings_name;
  if (group_name != NULL) {

    g_signal_emit (dialog, gattrib_dialog_signals[ GEOMETRY_SAVE ], 0,
                   dialog_geometry, group_name);
  }

  /* Let GTK unmap the window */
  ((GtkWidgetClass*)gattrib_dialog_parent_class)->unmap (widget);
}

/*!
 * \brief GObject finalise handler
 * \par Function Description
 *  Just before the GattribDialog GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize
 *  handler.
 *
 * \param [in] object The GObject being finalized.
 */
static void gattrib_dialog_finalize (GObject *object)
{
  GattribDialog *dialog = GATTRIB_DIALOG (object);

  GEDA_FREE (dialog->settings_name);

  ((GObjectClass*)gattrib_dialog_parent_class)->finalize (object);
}

/*!
 * \brief GObject property setter function
 * \par Function Description
 *  Setter function for GattribDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  object       The GObject whose properties we are setting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [in]  value        The GValue the property is being set from
 * \param [in]  pspec        A GParamSpec describing the property being set
 */
static void gattrib_dialog_set_property (GObject *object, guint property_id, const GValue *value, GParamSpec *pspec)
{
  GattribDialog *dialog = GATTRIB_DIALOG (object);

  switch(property_id) {
    case PROP_SETTINGS_NAME:
      GEDA_FREE (dialog->settings_name);
      dialog->settings_name = geda_strdup (g_value_get_string (value));
      break;
    case PROP_GATTRIB_GedaToplevel:
      dialog->pr_current = (GedaToplevel*)g_value_get_pointer (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*!
 * \brief GObject property getter function
 * \par Function Description
 *  Getter function for GattribDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  object       The GObject whose properties we are getting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [out] value        The GValue in which to return the value of the property
 * \param [in]  pspec        A GParamSpec describing the property being got
 */
static void gattrib_dialog_get_property (GObject *object, guint property_id, GValue *value, GParamSpec *pspec)
{
  GattribDialog *dialog = GATTRIB_DIALOG (object);

  switch(property_id) {
      case PROP_SETTINGS_NAME:
        g_value_set_string (value, dialog->settings_name);
        break;
      case PROP_GATTRIB_GedaToplevel:
        g_value_set_pointer (value, (void *)dialog->pr_current);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*!
 * \brief Type class initializer for GattribDialog
 * \par Function Description
 *  Type class initializer for GattribDialog. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 * \param [in]  klass       GattribDialogClass class we are initializing
 * \param [in]  class_data  GattribDialog structure associated with the class
 */
static void gattrib_dialog_class_init(void *klass, void *class_data)
{
  GattribDialogClass *dialog_class;
  GObjectClass       *gobject_class;
  GtkWidgetClass     *gtkwidget_class;

  dialog_class    = (GattribDialogClass*) klass;
  gobject_class   = (GObjectClass*) klass;
  gtkwidget_class = (GtkWidgetClass*) klass;

  dialog_class->geometry_save    = geometry_save;
  dialog_class->geometry_restore = geometry_restore;

  gtkwidget_class->show        = show_handler;
  gtkwidget_class->unmap       = unmap_handler;

  gobject_class->finalize      = gattrib_dialog_finalize;
  gobject_class->set_property  = gattrib_dialog_set_property;
  gobject_class->get_property  = gattrib_dialog_get_property;

  gattrib_dialog_parent_class = g_type_class_peek_parent (klass);

  gattrib_dialog_signals[ GEOMETRY_SAVE ] =
    g_signal_new ("geometry-save",
                  GATTRIB_TYPE_DIALOG,
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET(GattribDialogClass, geometry_save),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  geda_marshal_VOID__POINTER_STRING,
                  G_TYPE_NONE,
                  2,    /* n_params */
                  G_TYPE_POINTER,
                  G_TYPE_STRING
                 );

  gattrib_dialog_signals[ GEOMETRY_RESTORE ] =
    g_signal_new ("geometry-restore",
                  GATTRIB_TYPE_DIALOG,
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET(GattribDialogClass, geometry_restore),
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
                         "settings name",
                         "The name of the setting",
                         NULL,
                         G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));
  g_object_class_install_property (
    gobject_class, PROP_GATTRIB_GedaToplevel,
    g_param_spec_pointer ("gattrib-toplevel",
                          "gattrib toplevel",
                          "gattrib toplevel structure",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));
}

/*!
 * \brief Function to retrieve GattribDialog's GedaType identifier
 * \par Function Description
 *  Function to retrieve GattribDialog's Type identifier. On first call,
 *  this registers the GattribDialog in the GType system. Subsequently
 *  the function returns the saved value from its first execution.
 *
 * \return GedaType identifier associated with GattribDialog.
 */
GedaType gattrib_dialog_get_type (void)
{
  static volatile GedaType gattrib_dialog_type = 0;

  if (g_once_init_enter (&gattrib_dialog_type)) {

    static const GTypeInfo info = {
      sizeof(GattribDialogClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      gattrib_dialog_class_init,       /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GattribDialog),
      0,                               /* n_preallocs         */
      NULL                             /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GattribDialog");
    type   = g_type_register_static (GTK_TYPE_DIALOG, string, &info, 0);

    g_once_init_leave (&gattrib_dialog_type, type);
  }

  return gattrib_dialog_type;
}

/*!
 * \brief va_list support for gattrib_dialog_new_with_buttons
 * \par Function Description
 *  Convenience function which adds buttons to a pre-existing GtkDialog
 *  Modified from internal GTK function in GTK+-2.4.14 gtkdialog.c
 *
 * \param [in]  dialog             The GtkDialog buttons are being added to
 * \param [in]  first_button_text  The text string for the first button
 * \param [in]  args               The va_list containging the remaining button strings
 */
static void gattrib_dialog_add_buttons_valist (GtkDialog     *dialog,
                                               const char    *first_button_text,
                                               va_list        args)
{
  const char *text;
  int response_id;

  g_return_if_fail (GTK_IS_DIALOG (dialog));

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

/*!
 * \brief Create a new empty GattribDialog
 * \par Function Description
 *  Convenience function which creates a blank GattribDialog with various
 *  options.
 *
 * \param [in]  title          The title text of the dialog
 * \param [in]  parent         The GtkWindow which will parent this dialog
 * \param [in]  flags          The GtkDialogFlags to use when setting up the dialog
 * \param [in]  settings_name  The name gattrib should use to store this dialog's settings
 *
 * \return  The GattribDialog created.
 */
GtkWidget *gattrib_dialog_new_empty (const char *title, GtkWindow *parent,
                                     GtkDialogFlags flags,
                                     const char *settings_name)
{
  GtkWindow *dialog;

  dialog = g_object_new (GATTRIB_TYPE_DIALOG,
                         "settings-name", settings_name,
                         "gattrib-toplevel", pr_current,
                         NULL);

  if (title) {
    gtk_window_set_title (dialog, _(title));
  }

  if (parent) {
    gtk_window_set_transient_for (dialog, parent);
  }

  if (flags & GTK_DIALOG_MODAL) {
    gtk_window_set_modal (dialog, TRUE);
  }

  if (flags & GTK_DIALOG_DESTROY_WITH_PARENT) {
    gtk_window_set_destroy_with_parent (dialog, TRUE);
  }

  if (flags & GTK_DIALOG_NO_SEPARATOR) {
    gtk_dialog_set_has_separator ((GtkDialog*)dialog, FALSE);
  }

  gtk_window_set_type_hint (dialog, GDK_WINDOW_TYPE_HINT_DIALOG);

  return (GtkWidget*)dialog;
}

/*!
 * \brief Create a new empty GattribDialog with buttons
 * \par Function Description
 *  Convenience function which creates a GattribDialog with buttons and options.
 *  Modified from GTK+-2.4.14 gtkdialog.c.
 *
 * \param [in]  title              The title text of the dialog
 * \param [in]  parent             The GtkWindow which will parent this dialog
 * \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 * \param [in]  settings_name      The name gattrib should use to store this dialog's settings
 * \param [in]  first_button_text  The text string for the first button
 * \param [in]  ...                A variable number of arguments with the remaining button strings
 *
 * \return  The GattribDialog created.
 */
GtkWidget *gattrib_dialog_new_with_buttons (const char *title, GtkWindow *parent, GtkDialogFlags flags,
                                            const char *settings_name, const char *first_button_text, ...)
{
  GattribDialog *dialog;
  va_list args;

  dialog = GATTRIB_DIALOG (gattrib_dialog_new_empty (title, parent, flags, settings_name));

  va_start (args, first_button_text);

  gattrib_dialog_add_buttons_valist (GTK_DIALOG (dialog),
                                     first_button_text,
                                     args);
  va_end (args);

  return GTK_WIDGET (dialog);
}
