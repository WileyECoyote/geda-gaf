/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include <config.h>
#include <glib.h>

#include <glib-object.h>
#include <glib/gstdio.h>

#include "gattrib.h"
#include <gdk/gdkkeysyms.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


#include "gattrib_dialog.h"
/*! \brief Create pixmap widget for dialogs boxes.
 *  \par Function Description
 *  This is an internally used function to create pixmaps.
 *  The default bitmap directory is prefixed to the filename
 *  and if is valid then the image widget is created and returned. GtkWidget *widget, 
 */

GtkWidget*
create_pixmap (const char *filename)
{
  char *pathname = NULL;
  GtkWidget *pixmap;

  if (!filename || !filename[0])
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE ,
                                      GTK_ICON_SIZE_INVALID);

  pathname = g_build_filename (f_path_sys_data (), "bitmap", filename, NULL);

  if (!pathname)
    {
      u_log_message("Could not find image at file: %s.\n", filename);
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE,
                                      GTK_ICON_SIZE_INVALID);
    }

  pixmap = gtk_image_new_from_file (pathname);
  GEDA_FREE (pathname);
  return pixmap;
}

GtkWidget* get_geda_switch_image (gboolean WhichState)
{
   GtkWidget* image;

   if (WhichState)
     image = create_pixmap (SWITCH_ON_IMAGE);
   else
     image = create_pixmap (SWITCH_OFF_IMAGE);

   return image;
}


/*! \brief Function to create a GTK switch image control.
 *  \par Function Description
 *  This function creates a Check Box widget using an image, the Check
 *  Box indicator is disabled so only the images is displayed. This creates
 *  a control similar to a GTK3 Switch, using standard GTK2 controls. The
 *  On or Off images is controlled by the istate variable.
 *
 *  Returns: Newly created widget
 */

GtkWidget*
create_geda_switch(GtkWidget *Dialog, GtkWidget *parent, GtkWidget *widget,
                   GtkWidget *SwitchImage, gboolean istate)
{
  widget = gtk_check_button_new ();
  gtk_widget_show (widget);
  gtk_box_pack_start (GTK_BOX (parent), widget, FALSE, FALSE, 0);
  gtk_widget_set_size_request (widget, -1, 30);

  /* turn off the indicator, ie box */
  gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (widget), FALSE);

  /* Set the value of the control, sets raised property */
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget), istate);

  SwitchImage = get_geda_switch_image( istate);
  gtk_widget_show (SwitchImage);
  gtk_container_add (GTK_CONTAINER (widget), SwitchImage);

  return widget;
}

/* Signal marshaller based on generated code from glib-genmarshal */
static void
gattrib_marshal_VOID__POINTER_STRING (GClosure     *closure,
                                     GValue       *return_value,
                                     guint         n_param_values,
                                     const GValue *param_values,
                                     gpointer      invocation_hint,
                                     gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__POINTER_STRING) (gpointer     data1,
                                                     gpointer     arg_1,
                                                     gpointer     arg_2,
                                                     gpointer     data2);
  register GMarshalFunc_VOID__POINTER_STRING callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
    data1 = closure->data;
    data2 = g_value_peek_pointer (param_values + 0);
  } else {
    data1 = g_value_peek_pointer (param_values + 0);
    data2 = closure->data;
  }
  callback = (GMarshalFunc_VOID__POINTER_STRING) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_value_get_pointer (param_values + 1),
            (char*)g_value_get_string (param_values + 2),
            data2);
}
/* End section based on generated code from glib-genmashal */


enum {
  PROP_SETTINGS_NAME = 1,
  PROP_GATTRIB_GedaToplevel
};


enum {
  GEOMETRY_SAVE,
  GEOMETRY_RESTORE,
  LAST_SIGNAL
};

static guint gattrib_dialog_signals[ LAST_SIGNAL ] = { 0 };
static GObjectClass *gattrib_dialog_parent_class = NULL;

static GKeyFile *dialog_geometry = NULL;

#define DIALOG_GEOMETRY_STORE "gattrib-dialog-geometry"


/*! \brief Save all geometry data into a file.
 *
 *  \par Function Description
 *  This is called at program exit to save all window geometry data into a file
 *
 *  \param [in] user_data unused
 */
static void save_geometry_to_file(gpointer user_data)
{
  char *data, *file;

  g_assert( dialog_geometry != NULL );

  data = g_key_file_to_data(dialog_geometry, NULL, NULL);
  file = g_build_filename(f_path_user_config (), DIALOG_GEOMETRY_STORE,
        NULL);
  g_file_set_contents(file, data, -1, NULL);
  GEDA_FREE(data);
  GEDA_FREE(file);
}


/*! \brief GattribDialog "geometry_save" class method handler
 *
 *  \par Function Description
 *  Save the dialog's current position and size to the passed GKeyFile
 *
 *  \param [in] dialog     The GattribDialog to save the position and size of.
 *  \param [in] key_file   The GKeyFile to save the geometry data to.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void geometry_save (GattribDialog *dialog, GKeyFile *key_file, char* group_name)
{
  int x, y, width, height;

  gtk_window_get_position (GTK_WINDOW (dialog), &x, &y);
  gtk_window_get_size (GTK_WINDOW (dialog), &width, &height);

  g_key_file_set_integer (key_file, group_name, "x", x);
  g_key_file_set_integer (key_file, group_name, "y", y);
  g_key_file_set_integer (key_file, group_name, "width",  width );
  g_key_file_set_integer (key_file, group_name, "height", height);
}


/*! \brief GattribDialog "geometry_restore" class method handler
 *
 *  \par Function Description
 *  Restore dialog's last position and size from the passed GKeyFile
 *
 *  \param [in] dialog     The GattribDialog to restore the position and size of.
 *  \param [in] key_file   The GKeyFile to load the geometry data from.
 *  \param [in] group_name The group name in the key file to find the data under.
 */
static void geometry_restore (GattribDialog *dialog, GKeyFile *key_file, char* group_name)
{
  int x, y, width, height;

  x      = g_key_file_get_integer (key_file, group_name, "x", NULL);
  y      = g_key_file_get_integer (key_file, group_name, "y", NULL);
  width  = g_key_file_get_integer (key_file, group_name, "width",  NULL);
  height = g_key_file_get_integer (key_file, group_name, "height", NULL);

  gtk_window_move (GTK_WINDOW (dialog), x, y);
  gtk_window_resize (GTK_WINDOW (dialog), width, height);
}


/*! \brief Setup the GKeyFile for saving / restoring geometry
 *
 *  \par Function Description
 *  Check if the GKeyFile for saving / restoring geometry is open.
 *  If it doesn't exist, we create it here, and also install a hook
 *  to ensure its contents are saved at program exit.
 */
static void setup_keyfile ()
{
  if (dialog_geometry != NULL)
    return;

  char *file = g_build_filename (f_path_user_config (),
                                  DIALOG_GEOMETRY_STORE, NULL);

  dialog_geometry = g_key_file_new();

  /* Remember to save data on program exit */
  geda_atexit(save_geometry_to_file, NULL);

  if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
    g_mkdir (f_path_user_config (), S_IRWXU | S_IRWXG);

    g_file_set_contents (file, "", -1, NULL);
  }

  if (!g_key_file_load_from_file (dialog_geometry, file, G_KEY_FILE_NONE, NULL)) {
    /* error opening key file, create an empty one and try again */
    g_file_set_contents (file, "", -1, NULL);
    if ( !g_key_file_load_from_file (dialog_geometry, file, G_KEY_FILE_NONE, NULL)) {
       GEDA_FREE (file);
       return;
    }
  }
  GEDA_FREE (file);
}


/*! \brief GtkWidget show signal handler
 *
 *  \par Function Description
 *  Just before the dialog widget is shown, call the hook
 *  to restore its previously saved position and size.
 *
 *  \param [in] widget  The GtkWidget being shown.
 */
static void show_handler (GtkWidget *widget)
{
  char *group_name;
  GattribDialog *dialog = GATTRIB_DIALOG( widget );

  group_name = dialog->settings_name;
  if (group_name != NULL) {

    setup_keyfile ();
    if ( dialog_geometry != NULL ); {
      if (g_key_file_has_group (dialog_geometry, group_name)) {
        g_signal_emit (dialog, gattrib_dialog_signals[ GEOMETRY_RESTORE ], 0,
                       dialog_geometry, group_name);
      }
    }
  }

  /* Let GTK show the window */
  GTK_WIDGET_CLASS (gattrib_dialog_parent_class)->show (widget);
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
  char *group_name;
  GattribDialog *dialog = GATTRIB_DIALOG (widget);

  group_name = dialog->settings_name;
  if (group_name != NULL) {

    g_assert( dialog_geometry != NULL );
    g_signal_emit (dialog, gattrib_dialog_signals[ GEOMETRY_SAVE ], 0,
                   dialog_geometry, group_name);
  }

  /* Let GTK unmap the window */
  GTK_WIDGET_CLASS (gattrib_dialog_parent_class)->unmap (widget);
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GattribDialog GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void gattrib_dialog_finalize (GObject *object)
{
  GattribDialog *dialog = GATTRIB_DIALOG (object);

  GEDA_FREE (dialog->settings_name);

  G_OBJECT_CLASS (gattrib_dialog_parent_class)->finalize (object);
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GattribDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void gattrib_dialog_set_property (GObject *object, guint property_id, const GValue *value, GParamSpec *pspec)
{
  GattribDialog *dialog = GATTRIB_DIALOG (object);

  switch(property_id) {
    case PROP_SETTINGS_NAME:
      GEDA_FREE (dialog->settings_name);
      dialog->settings_name = g_strdup (g_value_get_string (value));
      break;
    case PROP_GATTRIB_GedaToplevel:
      dialog->pr_current = (GedaToplevel*)g_value_get_pointer (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GattribDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void gattrib_dialog_get_property (GObject *object, guint property_id, GValue *value, GParamSpec *pspec)
{
  GattribDialog *dialog = GATTRIB_DIALOG (object);

  switch(property_id) {
      case PROP_SETTINGS_NAME:
        g_value_set_string (value, dialog->settings_name);
        break;
      case PROP_GATTRIB_GedaToplevel:
        g_value_set_pointer (value, (gpointer)dialog->pr_current);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief Type class initialiser for GattribDialog
 *
 *  \par Function Description
 *  Type class initialiser for GattribDialog. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The GattribDialogClass we are initialising
 */
static void gattrib_dialog_class_init (GattribDialogClass *klass)
{
  GObjectClass     *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *gtkwidget_class = GTK_WIDGET_CLASS (klass);

  klass->geometry_save         = geometry_save;
  klass->geometry_restore      = geometry_restore;

  gtkwidget_class->show        = show_handler;
  gtkwidget_class->unmap       = unmap_handler;

  gobject_class->finalize      = gattrib_dialog_finalize;
  gobject_class->set_property  = gattrib_dialog_set_property;
  gobject_class->get_property  = gattrib_dialog_get_property;

  gattrib_dialog_parent_class = g_type_class_peek_parent (klass);

  gattrib_dialog_signals[ GEOMETRY_SAVE ] =
    g_signal_new ("geometry-save",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET( GattribDialogClass, geometry_save ),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  gattrib_marshal_VOID__POINTER_STRING,
                  G_TYPE_NONE,
                  2,    /* n_params */
                  G_TYPE_POINTER,
                  G_TYPE_STRING
                 );

  gattrib_dialog_signals[ GEOMETRY_RESTORE ] =
    g_signal_new ("geometry-restore",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET( GattribDialogClass, geometry_restore ),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  gattrib_marshal_VOID__POINTER_STRING,
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
    gobject_class, PROP_GATTRIB_GedaToplevel,
    g_param_spec_pointer ("gattrib-toplevel",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));
}


/*! \brief Function to retrieve GattribDialog's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GattribDialog's Type identifier.
 *  Upon first call, this registers the GattribDialog in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the Type identifier associated with GattribDialog.
 */
unsigned int gattrib_dialog_get_type ()
{
  static unsigned int gattrib_dialog_type = 0;

  if (!gattrib_dialog_type) {
    static const GTypeInfo gattrib_dialog_info = {
      sizeof(GattribDialogClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) gattrib_dialog_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(GattribDialog),
      0,    /* n_preallocs */
      NULL, /* instance_init */
    };

    gattrib_dialog_type = g_type_register_static (GTK_TYPE_DIALOG,
                                                 "GattribDialog",
                                                 &gattrib_dialog_info, 0);
  }

  return gattrib_dialog_type;
}

/*! \brief Internal GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to support gattrib_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which adds buttons to a pre-existing GtkDialog
 *
 *  \param [in]  dialog             The GtkDialog buttons are being added to
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  args               The va_list containging the remaining button strings
 */
static void gattrib_dialog_add_buttons_valist (GtkDialog     *dialog,
                                               const char    *first_button_text,
                                               va_list        args)
{
  const char* text;
  int response_id;

  g_return_if_fail (GTK_IS_DIALOG (dialog));

  if (first_button_text == NULL)
    return;

  text = first_button_text;
  response_id = va_arg (args, int);

  while (text != NULL)
    {
      gtk_dialog_add_button (dialog, text, response_id);

      text = va_arg (args, char*);
      if (text == NULL)
        break;
      response_id = va_arg (args, int);
    }
}

/*! \brief Internal GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to support gattrib_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a blank GattribDialog with various options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name gattrib should use to store this dialog's settings
 *
 *  \return  The GattribDialog created.
 */
 GtkWidget* gattrib_dialog_new_empty (const char *title, GtkWindow *parent,
                                      GtkDialogFlags flags,
                                      const char *settings_name)
{
  GattribDialog *dialog;

  dialog = g_object_new (GATTRIB_TYPE_DIALOG,
                         "settings-name", settings_name,
                         "gattrib-toplevel", pr_current,
                         NULL);

  if (title)
    gtk_window_set_title (GTK_WINDOW (dialog), _(title));

  if (parent)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);

  if (flags & GTK_DIALOG_MODAL)
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  if (flags & GTK_DIALOG_DESTROY_WITH_PARENT)
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);

  if (flags & GTK_DIALOG_NO_SEPARATOR)
    gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

    gtk_window_set_type_hint (GTK_WINDOW (dialog), GDK_WINDOW_TYPE_HINT_DIALOG);

  return GTK_WIDGET (dialog);
}


/*! \brief GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to provide a GattribDialog equivelant of the convenience function
 *  gtk_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a GattribDialog with buttons and options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name gattrib should use to store this dialog's settings
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  ...                A variable number of arguments with the remaining button strings
 *
 *  \return  The GattribDialog created.
 */
GtkWidget* gattrib_dialog_new_with_buttons (const char *title, GtkWindow *parent, GtkDialogFlags flags,
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

