/* -*- C x_dialog.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2017 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_dialog.c
 * \brief A Collection of Utilities and Dialogs for Editing & Messages
 */

#include <ctype.h>

#include <gschem.h>
#include <version.h>
#include <x_dialog.h>
#include <geda_dialogs.h>
#include <geda_widgets.h>
#include <geda_debug.h>

const char *IDS_MESSEAGE_TITLES[] = {
  "Information", "Warning", "Confirmation", "Error", "gschem", /* Message Title Strings*/
  NULL
};


/**   \defgroup Dialog-Utilities Common Dialog Utlities
 *  @{\par This group contains utilities used by various Gschem dialogs
 */

/**   \defgroup General-Dialog-Utilities General Dialog Utilities
 *  @{\par This group contains utility functions used by various dialogs
 *    \ingroup Dialog-Utilities
 */

/**   \defgroup Atk-Dialog-Utilities Atk Dialog Utilities
 *  @{\par This group contains utility functions used by various dialogs
 *    \ingroup General-Dialog-Utilities
 */

/*! \brief Create AtkObject widget and Link Label with Widget
 *  \par Function Description
 *  This function obtains a new <b>Accessibility</b> object associate with linkto
 *  and associates the object with the label widget.
 */
AtkObject*
atk_widget_linked_label_new( GtkWidget *label, GtkWidget *linkto)
{
  AtkObject *atk_obj;

  atk_obj = gtk_widget_get_accessible (linkto);

  if (GTK_IS_ACCESSIBLE (atk_obj)) {
    /* Accessibility support is enabled.
       Make the label ATK_RELATON_LABEL_FOR for the size list as well. */

    AtkObject      *atk_label;
    AtkRelationSet *relation_set;
    AtkRelation    *relation;
    AtkObject      *obj_array[1];

    atk_label    = gtk_widget_get_accessible (label);

    relation_set = atk_object_ref_relation_set (atk_obj);

    relation     = atk_relation_set_get_relation_by_type (relation_set,
                                                          ATK_RELATION_LABELLED_BY);
    if (relation) {
      atk_relation_add_target (relation, atk_label);
    }
    else {
      obj_array[0] = atk_label;
      relation     = atk_relation_new (obj_array, 1, ATK_RELATION_LABELLED_BY);
      atk_relation_set_add (relation_set, relation);
    }
    GEDA_UNREF (relation_set);

    relation_set = atk_object_ref_relation_set (atk_label);
    relation     = atk_relation_set_get_relation_by_type (relation_set,
                                                          ATK_RELATION_LABEL_FOR);
    if (relation) {
      atk_relation_add_target (relation, atk_obj);
    }
    else {
      obj_array[0] = atk_obj;
      relation = atk_relation_new (obj_array, 1, ATK_RELATION_LABEL_FOR);
      atk_relation_set_add (relation_set, relation);
    }
    GEDA_UNREF (relation_set);
  }
  else
    atk_obj = NULL;

  return atk_obj;
}

/** @} endgroup Atk-Dialog-Utilities */

/*! \brief Create pixmap widget for dialogs boxes.
 *  \par Function Description
 *  This is an internally used function to create pixmaps. The
 *  default bitmap directory is prefixed to the filename and if
 *  is valid then the image widget is created and returned.
 *
 *  \param [in] filename File name of file containing image data
 */

GtkWidget *create_pixmap (const char *filename)
{
  char *pathname = NULL;
  GtkWidget *pixmap;

  if (!filename || !filename[0]) {
      u_log_message("Bad image file name.\n");
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE ,
                                      GTK_ICON_SIZE_INVALID);
  }

  pathname = geda_file_get_bitmap_filespec (filename);

  if (!pathname) {
      u_log_message("Could not find image file: %s.\n", filename);
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE,
                                      GTK_ICON_SIZE_INVALID);
  }
  pixmap = gtk_image_new_from_file (pathname);
  GEDA_FREE (pathname);
  return pixmap;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void x_dialog_set_icon (GtkWidget *dialog, const char *icon_name)
{
  /* GschemMainWindow *window = (GschemMainWindow*)instance; */
  GdkPixbuf *icon;
  GError    *error;
  char      *filename;

  error    = NULL;
  filename = geda_file_get_bitmap_filespec (icon_name);
  icon     = gdk_pixbuf_new_from_file (filename, &error);

  if (!error) {
    gtk_window_set_icon ((GtkWindow*)dialog, icon);
  }
  else if (verbose_mode) {
    fprintf(stderr, "%s is missing\n", filename);
    g_error_free(error);
  }
  GEDA_FREE(filename);
}

/** @} endgroup General-Dialog-Utilities */

/** \defgroup Dialog-Toggle-Switches Switch Manipulators for Dialogs
 *  @{
 *  \ingroup Dialog-Utilities
 *  \par
 *       Contains function change the images for checkboxes, aka Switches for
 *       for the on and off state.
 */

GtkWidget *get_geda_switch_image (bool WhichState)
{
   GtkWidget *image;

   if (WhichState) {
     image = create_pixmap (GEDA_BITMAP_SWITCH_ON);
   }
   else {
     image = create_pixmap (GEDA_BITMAP_SWITCH_OFF);
   }

   return image;
}

/*! \brief Function to create a Geda switch image control / widget.
 *  \par Function Description
 *  This function creates a Check Box widget using an image, the Check
 *  Box indicator is disabled so only the images is displayed. This creates
 *  a control similar to a GTK3 Switch, using a standard GTK2 widget. The
 *  On or Off images is controlled by the istate variable.
 *
 *  \returns Newly created widget
 */
GtkWidget*
create_geda_switch(GtkWidget *parent, GtkWidget *SwitchImage, bool istate)
{
  GtkWidget *widget;

  widget = gtk_check_button_new ();

  gtk_widget_show (widget);
  PACK_START (parent, widget, FALSE, FALSE, 0);
  gtk_widget_set_size_request (widget, -1, 30);

  /* turn off the indicator, ie box */
  gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (widget), FALSE);

  /* Set the value of the control, sets raised property */
  SetToggleState (widget, istate);

  /* Set the Widgets Name */
  gtk_widget_set_name (widget, "GedaToggleSwitch");

  SwitchImage = get_geda_switch_image (istate);
  gtk_widget_show (SwitchImage);
  geda_container_add (widget, SwitchImage);

  return widget;
}

/** @} endgroup Dialog-Toggle-Switches */

/** \defgroup Dialog-Radio-Bulbs Manipulator for Radio Controls on Dialogs
 *  @{
 *  \ingroup Dialog-Utilities
 *  \par
 *  The Bulb widgets are nothing more than ordinary radio buttons with
 *  their indicators set to invisible. Both ON and the Off images are
 *  embed into the widget when bulb widgets are created and then each
 *  image is set visible or invisible based on the state of the radio
 *  widgets. This groups contain functions to work with widgets. Note
 *  that the actual creation is done with a macro, see #EDA_BULB.
 */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GtkWidget *x_dialog_get_bulb_image (bool WhichState)
{
   GtkWidget *image;

   if (WhichState)
     image = create_pixmap (GEDA_BITMAP_BULB_ON );
   else
     image = create_pixmap (GEDA_BITMAP_BULB_OFF);

   return image;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Helper for Obsolete x_dialog_bulb_group_set_active
 */
void x_dialog_set_bulb_on( GtkWidget *widget) {

  GList *button   = geda_container_get_children (widget);
  GList *align    = geda_container_get_children (button->data);
  GList *lightbox = geda_container_get_children (align->data);

  GtkWidget *BulbOnImage  = lightbox->data;
  GtkWidget *BulbOffImage = lightbox->next->data;

  gtk_widget_show (BulbOnImage);
  gtk_widget_hide (BulbOffImage);

  g_list_free(lightbox);
  g_list_free(align);
  g_list_free(button);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * Helper for Obsolete x_dialog_bulb_group_set_active
 */
void x_dialog_set_bulb_off( GtkWidget *widget) {

  GList *button   = geda_container_get_children (widget);
  GList *align    = geda_container_get_children (button->data);
  GList *lightbox = geda_container_get_children (align->data);

  GtkWidget *BulbOnImage  = lightbox->data;
  GtkWidget *BulbOffImage = lightbox->next->data;

  gtk_widget_hide (BulbOnImage);
  gtk_widget_show (BulbOffImage);

  g_list_free(lightbox);
  g_list_free(align);
  g_list_free(button);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * Obsolete, use libgedauio GedaBulb
 */
void x_dialog_bulb_group_set_active(GSList *RadioGroupList, int value)
{
  int length;
  int index;
  unsigned int pos = (unsigned int)(long)value;
  int j;

  /* Get number of buttons in group */
  length = g_slist_length (RadioGroupList);

  /* new buttons are *prepended* to the list, so buttons added as
   * first have last position in the list and using glist reverse
   * confuses gtk */
  index = (length - 1) - pos;

  if (index < 0 || index >= length) { /* should not to happen */
    return;
  }

  for (j = 0; j < length; j++) {

    GtkToggleButton *button;

    button = GTK_TOGGLE_BUTTON (g_slist_nth_data (RadioGroupList, j));

    if (button == NULL)
      return;

    if ( j == index) {

      if (gtk_toggle_button_get_active (button) == FALSE) {
        gtk_toggle_button_set_active (button, TRUE);
      }
      x_dialog_set_bulb_on(GTK_WIDGET(button));
    }
    else {
      x_dialog_set_bulb_off(GTK_WIDGET(button));
    }
  }

  return;
}

/** @} endgroup Dialog-Radio-Bulbs */

/** \defgroup Text-View-Utilities Utility Functions for TextView Widgets
 *  @{
 *  \ingroup Dialog-Utilities
 *  \brief Generic utility functions for textview widgets.
 */

/*! \brief Selects all text in a TextView widget
 *  \par Function Description
 *  The function selects all the text in a TextView widget.
 *
 */
void select_all_text_in_textview(GtkTextView *textview)
{
  if (GTK_IS_TEXT_VIEW(textview)) {

    GtkTextBuffer *textbuffer;
    GtkTextIter start, end;

    textbuffer = gtk_text_view_get_buffer(textview);
    gtk_text_buffer_get_bounds (textbuffer, &start, &end);
    gtk_text_buffer_select_range(textbuffer, &start, &end);
  }
  else {
    BUG_MSG("parameter is not a textview widget");
  }
}

/*!
 * \brief Calculate the width of a TABs in pixels
 * \par Function Description
 *  This function actually allocates a pango layout containing a
 *  \a tab_size TAB characters in order to determine the width of
 *  in pixels.
 *
 * \param textview  Pointer to a text view widget
 * \param tab_size  Number of TAB characters to measure.
 *
 *  \returns the width of a TAB
 */
int
text_view_calculate_real_tab_width(GtkTextView *textview, int tab_size)
{
  PangoLayout *layout;
  char *tab_string;
  int tab_width = 0;

  if (tab_size == 0)
  return -1;

  tab_string = g_strnfill (tab_size, ' ');

  layout = gtk_widget_create_pango_layout (
                                           GTK_WIDGET (textview),
                                           tab_string);
  GEDA_FREE (tab_string);

  if (layout != NULL) {
    pango_layout_get_pixel_size (layout, &tab_width, NULL);
    GEDA_UNREF (G_OBJECT (layout));
  } else
  tab_width = -1;

  return tab_width;
}
/** @} endgroup Text-View-Utilities */
/** @} endgroup Dialog-Utilities */

/** \defgroup Standard-Dialogs Standard Program Dialogs
 *  @{
 *  \brief This group contains functions for Standard Dialogs
*/

/* Enumerate Control IDs */
typedef enum {
       ShowBinding,

} ControlID;

static WidgetStringData DialogStrings[] = {
  { "ShowBindingSwitch",  "Show binding", "Show or Hide the action column"},
  { NULL, NULL, NULL},
};

/** \defgroup About-Dialog About Dialog
 *  @{
 *  \image html about_dialog.png
 *  \image latex about_dialog.png
 */

/*!
 * \brief Callback Link on About Dialog
 * \par Function Description
 *  This is a callback for the link on the about dialog, the function
 *  simply passes the \a link to x_show_uri.
 */
static void dialog_link_cb(GtkAboutDialog *dialog, const char *link, void *data)
{
   x_show_uri(link);
}

#if HAVE_GNU_LIBC_VERSION_H
# include <gnu/libc-version.h>
#else

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
const char *gnu_get_libc_version(void)
{
   return "unknown";
}

#endif

/*! \brief Create the about dialog and show it
 *  \par Function Description
 *  This function creates and presents the about dialog.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void about_dialog (GschemToplevel *w_current)
{
        char *version_string;
        char *logo_file;
        char *comments;
  const char *copyright;
  const char *gEDA_str;
  const char *glib_str;
  const char *guile_str;
        char *guile_ver;
  GdkPixbuf  *logo;
  GError     *error = NULL;
  GtkWidget  *Dialog;

  version_string = geda_sprintf ("%s (%s)", PACKAGE_DOTTED_VERSION,
                                            PACKAGE_DATE_VERSION);

  logo_file =  geda_file_get_bitmap_filespec ("gschem_about_logo.png");

  logo = gdk_pixbuf_new_from_file (logo_file, &error);
  GEDA_FREE (logo_file);

  if (error != NULL && logo == NULL) {

    const char *logo_err = _("Could not load image file");

    u_log_message ("%s: %s\n%s\n", logo_err, logo_file, error->message);
    g_error_free (error);
  }

  gEDA_str  = _("gEDA: GPL Electronic Design Automation");
  glib_str  = _("glibc");
  guile_str = _("Guile");

  guile_ver = scm_to_utf8_string(scm_version());

  comments  = geda_strconcat (gEDA_str, "\n\n",
                              glib_str, " ", gnu_get_libc_version(), "\n",
                              guile_str," ", guile_ver, "\n", NULL);

  free(guile_ver);

  copyright = _("Copyright © 1998-2017 Ales Hvezda "
                "<ahvezda@geda.seul.org>\n"
                "Copyright © 1998-2017 gEDA Contributors "
                "(see ChangeLog for details)");

  Dialog = gtk_about_dialog_new ();

  g_object_set (Dialog, "version",   version_string,
                        "logo",      logo,
                        "title",    _("About gschem"),
                        "comments",  comments,
                        "copyright", copyright,
                        "website",   "http://geda-project.org/",
                         NULL);      /* End marker */

  /* About dialog URI calls maybe broken on Windows */
#if (GTK_MAJOR_VERSION == 2 && GTK_MINOR_VERSION < 24)

  /* deprecated since GTK 2.24 */
  gtk_about_dialog_set_url_hook(dialog_link_cb, NULL, NULL);

#else

  g_signal_connect(Dialog, "activate-link", G_CALLBACK(dialog_link_cb), NULL);

#endif

/* Strip ".exe" from the program name on Win32 platforms */
#if defined (OS_WIN32_NATIVE)

  const char *app_name = g_get_prgname();
        char *name;
        char *ptr;

  name = geda_strdup(app_name);

  ptr = strstr(name, ".exe");

 *ptr = '\0';

  gtk_about_dialog_set_program_name ((GtkAboutDialog*)Dialog, name);

  GEDA_FREE(name);

#endif

  gtk_dialog_run(GTK_DIALOG(Dialog));
  gtk_widget_destroy(Dialog);

  GEDA_FREE (version_string);
  GEDA_FREE (comments);
  if (logo) {
    GEDA_UNREF (logo);
  }
}

/******************* End of help/about dialog box **********************/

/** @} endgroup About-Dialog */

/** \defgroup Snap-Size-Dialog Snap Size Dialog
 *  @{
 *  \ingroup Settings-Dialogs
*/

/*! \brief response function for the snap size dialog
 *  \par Function Description
 *  This is the response function for the snap size dialog.
 *  If the user select the apply button, w_current->snap_size is set to
 *  given value.
 */
static void
snap_size_dialog_response(GtkWidget *Dialog, int response, void* data)
{
  GtkWidget *snap_size;
  int size;
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_ACCEPT:
    snap_size = GEDA_OBJECT_GET_DATA(Dialog, IDS_SNAP_SIZE);
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(snap_size));

    w_current->snap_size = size;
    i_status_update_grid_info (w_current);
    o_invalidate_all (w_current);
    break;

  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }

  /* clean up */
  i_status_set_state(w_current, SELECT);
  gtk_widget_destroy(Dialog);
}

/*! \brief Create the snap size dialog
 *  \par Function Description
 *  This function creates the snap size dialog.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void snap_size_dialog (GschemToplevel *w_current)
{
  GtkWidget *Dialog = w_current->sswindow;
  GtkWidget *snap_size;

  if (!Dialog) {

    GtkWidget *label;
    GtkWidget *vbox;

    Dialog = gschem_dialog_new_with_buttons(_("Snap Size"),
                                            w_current->main_window,
                                            GTK_DIALOG_MODAL,
                                            IDS_SNAP_SIZE,
                                            w_current,
                                            GTK_STOCK_CANCEL,
                                            GEDA_RESPONSE_REJECT,
                                            GTK_STOCK_OK,
                                            GEDA_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_MOUSE);

    g_signal_connect (Dialog, "response",
                      G_CALLBACK (snap_size_dialog_response),
                      NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(Dialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(Dialog)->vbox;

    label = geda_aligned_label_new (_("Enter new snap grid spacing:"), 0, 0);
    PACK_START(vbox, label, TRUE, TRUE, 0);

    snap_size = gtk_spin_button_new_with_range(MIN_SNAP_SIZE,MAX_SNAP_SIZE,5);
    gtk_editable_select_region( GTK_EDITABLE(snap_size), 0, -1);
    PACK_START(vbox, snap_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(snap_size), TRUE);
    gtk_widget_grab_focus(snap_size);

    SetWidgetTip(snap_size, _("Sets the default spacing\nwhich objects snaps to."));

    GEDA_HOOKUP_OBJECT(Dialog, snap_size, IDS_SNAP_SIZE);

    w_current->sswindow = Dialog;

    gtk_widget_show_all(Dialog);
  }

  else {  /* dialog already there */
    gtk_window_present(GTK_WINDOW(Dialog));
  }

  /* always set the current gschem value to the dialog entry */
  snap_size = GEDA_OBJECT_GET_DATA(Dialog, IDS_SNAP_SIZE);
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(snap_size), w_current->snap_size);
  gtk_editable_select_region(GTK_EDITABLE(snap_size), 0, -1);
}

/******************** End of Snap size dialog box **********************/

/** @} endgroup Snap-Size-Dialog */

/** \defgroup Text-Size-Dialog Text Size Dialog
 *  @{
 *  \ingroup Settings-Dialogs
*/

/*! \brief response function for the text size dialog
 *  \par Function Description
 *  This function takes the user input and applies it to the text object
 */
static void
text_size_dialog_response(GtkWidget *Dialog, int response, void* data)
{
  GtkWidget *text_size;
  int size;
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_ACCEPT:
    text_size = GEDA_OBJECT_GET_DATA(Dialog, IDS_TEXT_SIZE);
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(text_size));

    w_current->text_size = size;
    break;
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    /* void */
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }

  /* clean up */
  i_status_set_state(w_current, SELECT);
  gtk_widget_destroy(Dialog);
}

/*! \brief Create the text size dialog
 *  \par Function Description
 *  This function creates the text size dialog.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void text_size_dialog (GschemToplevel *w_current)
{
  GtkWidget *Dialog = w_current->tswindow;
  GtkWidget *text_size;

  if (!Dialog ) {

    GtkWidget *label;
    GtkWidget *vbox;
    GtkWidget *text_size;

    Dialog = gschem_dialog_new_with_buttons(_("Text Size"),
                                            w_current->main_window,
                                            GTK_DIALOG_MODAL,
                                            IDS_TEXT_SIZE, w_current,
                                            GTK_STOCK_CANCEL,
                                            GEDA_RESPONSE_REJECT,
                                            GTK_STOCK_OK,
                                            GEDA_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_MOUSE);

    g_signal_connect (Dialog, "response",
                      G_CALLBACK (text_size_dialog_response),
                      NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(Dialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(Dialog)->vbox;

    label = geda_aligned_label_new (_("Enter new text size:"), 0, 0);
    PACK_START(vbox, label, TRUE, TRUE, 0);

    text_size = gtk_spin_button_new_with_range(MIN_TEXT_SIZE,
                                               MAX_TEXT_SIZE,2);
    gtk_editable_select_region( GTK_EDITABLE(text_size), 0, -1);
    PACK_START(vbox, text_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(text_size), TRUE);
    gtk_widget_grab_focus(text_size);

    SetWidgetTip(text_size, _("Sets the default text font size."));

    GEDA_HOOKUP_OBJECT(Dialog, text_size, IDS_TEXT_SIZE);
    w_current->tswindow = Dialog;
    gtk_widget_show_all(Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }

  /* always set the current text size to the dialog */
  text_size = GEDA_OBJECT_GET_DATA(Dialog,IDS_TEXT_SIZE);
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(text_size), w_current->text_size);
  gtk_editable_select_region(GTK_EDITABLE(text_size), 0, -1);
}

/********************* End of Text size dialog box *********************/

/** @} endgroup Text-Size-Dialog */

/** @} endgroup Standard-Dialogs */

/** \defgroup Editing-Dialogs X-Dialogs for Editing Schematics
 *  @{
 *  \brief This group contains Dialogs routines for Editing Objects
 *  \details
 *  This section contains a collection of Mode-less Dialogs derived from
 * type GschemDialog and use the PROP_SELECTION_TRACKER property, where
 * as each dialog registers a callback function to be called when the
 * selection is changed so that the Dialog content can be updated based
 * on the current selection. This allows the Dialogs to stay open while
 * other editing is performed.
 */

static      GtkWidget *create_linetype_menu (GschemToplevel *w_current);
static int  x_dialog_edit_line_type_change  (GtkWidget *w, line_type_data *ld);
static void x_dialog_edit_line_type_ok      (GtkWidget *w, line_type_data *ld);

static      GtkWidget *create_menu_filltype (GschemToplevel *w_current);
static int  x_dialog_edit_fill_type_change  (GtkWidget *w, fill_type_data *fd);
static void x_dialog_edit_fill_type_ok      (GtkWidget *w, fill_type_data *fd);

/* string buffer used by dialogs: show_text, find_text and hide_text */
static char text_buffer[256] = "refdes=R";

/*!
 * \brief Load the text_buffer
 * \par Function Description
 *  Local function used in this module to load the text_buffer
 */
static void set_text_buffer(const char *string)
{
  if (string != NULL) {

    int length;

    memset(text_buffer, 0, sizeof(text_buffer)-1);

    length = strlen(string);

    memcpy(text_buffer, string, length);
    text_buffer[length] = '\0';
  }
  else {
    memset(text_buffer, 0, sizeof(text_buffer)-1);
  }
}

/** \defgroup Arc-Edit-Dialog Arc Edit Dialog
 *  @{ \memberof Editing-Dialogs
 */

/*! \brief Handle selection change event for x_dialog_edit_arc_angle
 *  \par Function Description
 *  Updates the comboboxes when the selection changes.
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to a selected Object.
 */
static void
x_dialog_edit_arc_angle_selection (GschemToplevel *w_current, GedaObject *object)
{
  GtkWidget *spin_radius, *spin_start, *spin_sweep;

  GtkWidget *Dialog = w_current->aawindow;

  spin_radius = GEDA_OBJECT_GET_DATA(Dialog, "radius");
  spin_start  = GEDA_OBJECT_GET_DATA(Dialog, "spin_start");
  spin_sweep  = GEDA_OBJECT_GET_DATA(Dialog, "spin_sweep");

  if (object == NULL) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_radius), w_current->distance);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep), 90);
  }
  else {
    if (object->type == OBJ_ARC) {
      gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_radius),
                                object->arc->radius);
      gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),
                                object->arc->start_angle);
      gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep),
                                object->arc->arc_sweep);

      gtk_widget_grab_focus(spin_radius);
    }
  }
}

/*! \brief Apply changes to selected objects
 *  \par Function Description
 *  This function applies the changes to the currently selected objects.
 */
static void
x_dialog_edit_arc_angle_apply(GtkWidget *Dialog, GschemToplevel *w_current)
{
  GtkWidget *spin_entry;
  GList     *s_current;
  int        modified;
  int        radius;
  int        start_angle;
  int        sweep_angle;

  if (w_current->event_state != ARCMODE) {
    s_current = gschem_dialog_get_selected ((GschemDialog*)Dialog);
  }
  else {
    s_current   = NULL;
  }

  modified    = 0;

  /* Get ptr to the spinner widgets */
  spin_entry  = GEDA_OBJECT_GET_DATA(Dialog,"radius");
  radius      = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_entry));
  spin_entry  = GEDA_OBJECT_GET_DATA(Dialog,"spin_start");
  start_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_entry));
  spin_entry  = GEDA_OBJECT_GET_DATA(Dialog,"spin_sweep");
  sweep_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_entry));

  if (s_current != NULL) {

    while (s_current != NULL) {

      GedaObject *object = (GedaObject*) s_current->data;

      if (object == NULL) {
        BUG_MSG("NULL object");
      }
      else {
        if(object->type == OBJ_ARC) {
          /* invalidate the old arc object */
          o_invalidate_object (w_current, object);
          geda_arc_object_modify(object, radius,      0, ARC_RADIUS);
          geda_arc_object_modify(object, start_angle, 0, ARC_START_ANGLE);
          geda_arc_object_modify(object, sweep_angle, 0, ARC_END_ANGLE);
          o_invalidate_object (w_current, object);
          modified++;
        }
      }
      NEXT(s_current);
    }
  }
  else {
    /* When arc is a new object */
    o_arc_end4(w_current, radius, start_angle, sweep_angle);
  }

  if (modified) {

    Page *p_current = gschem_toplevel_get_current_page(w_current);

    geda_page_set_changed(p_current, TRUE);
    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*! \brief response function for the arc angle dialog
 *  \par Function Description
 *  The response function of the arc angle dialog takes the content of
 *  the dialog and applies it on the current arc.
 *  If the dialog is closed or canceled the function destroys the dialog.
 */
static void
x_dialog_edit_arc_angle_response(GtkWidget *Dialog, int response, void* data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
    case GEDA_RESPONSE_REJECT:
    case GEDA_RESPONSE_DELETE_EVENT:
      gtk_widget_destroy(Dialog);
      break;

    case GEDA_RESPONSE_ACCEPT:
      x_dialog_edit_arc_angle_apply(Dialog, w_current);
      break;

    default:
      BUG_IMSG ("unhandled case", response);
  }
}

/*! \brief Creates the arc angle dialog
 *  \par Function Description
 *  This function creates the arc angle dialog. This dialog is also used
 *  both for creating and editing ARC's. Depending on the \a arc_object the
 *  entries are filled with the arc Object properties or with some standard
 *  values.
 *
 *  \param [in] w_current   The GschemToplevel object
 *  \param [in] arc_object  an arc Object if used to modify an arc
 *                          or NULL to create a new arc.
 *
 * TODO: When multi-selection and modless is applied then only change
 *       the changed value, maybe add check box next to each
 */
void x_dialog_edit_arc_angle (GschemToplevel *w_current, GedaObject *arc_object)
{
  GtkWidget *Dialog = w_current->aawindow;

  if (!Dialog) {

    GtkWidget *label = NULL;
    GtkWidget *vbox;
    GtkWidget *alignment, *table;
    GtkWidget *radius, *spin_start, *spin_sweep;

    Dialog = gschem_dialog_new_with_buttons(_("Arc Parameters"),
                                            w_current->main_window,
                                            GSCHEM_MODELESS_DIALOG,
                                            IDS_ARC_ANGLE, w_current,
                                            GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                            GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response(GTK_DIALOG(Dialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(Dialog)->vbox;

    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              0 /*DIALOG_INDENTATION */, 0);
    PACK_START(vbox, alignment, FALSE, FALSE, 0);

    table = gtk_table_new (2, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    geda_container_add(alignment, table);

    label = geda_aligned_label_new (_("Arc Radius:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

    radius = gtk_spin_button_new_with_range(1, 100000, 100);
    gtk_entry_set_activates_default(GTK_ENTRY(radius), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), radius, 1,2,0,1);
    SetWidgetTip(radius,  _("Sets the radius of the Arc."));

    label = geda_aligned_label_new (_("Start Angle:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

    spin_start = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_start), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_start, 1,2,1,2);
    SetWidgetTip(spin_start,  _("Sets the rotation of the arc."));

    label = geda_aligned_label_new(_("Degrees of Sweep:"), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

    spin_sweep = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_sweep), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_sweep, 1,2,2,3);
    SetWidgetTip(spin_sweep,  _("Sets the central angle of the arc."));

    HD_ACTION_SEPARATOR (vbox);

    GEDA_HOOKUP_OBJECT(Dialog, radius,    "radius");
    GEDA_HOOKUP_OBJECT(Dialog, spin_start,"spin_start");
    GEDA_HOOKUP_OBJECT(Dialog, spin_sweep,"spin_sweep");

    g_signal_connect (Dialog, "response",
                      G_CALLBACK (x_dialog_edit_arc_angle_response),
                      NULL);

    g_object_set (Dialog, DIALOG_SELECTION_TRACKER,
                  x_dialog_edit_arc_angle_selection, NULL);

    gtk_widget_show_all (Dialog);
    w_current->aawindow = Dialog;
  }
  else {  /* dialog already created */
    gtk_window_present (GTK_WINDOW(Dialog));
  }

  x_dialog_edit_arc_angle_selection (w_current, arc_object);

}

/********************** End of Arc dialog box ***************************/

/** @} end group Arc-Angle-Dialog */

/** \defgroup Common-Edit-Dialog Common Routines Editing Dialogs
 *  @{
 */

/*!
 * \brief Given color index, obtain a human readable name
 * \par Function Description
 *  Returns the string associated with the given \a index.
 */
static const char*
x_dialog_get_color_name (int index)
{
  switch(index) {
    case BACKGROUND_COLOR:         return _("Background");
    case PIN_COLOR:                return _("Pin");
    case NET_ENDPOINT_COLOR:       return _("Net endpoint");
    case GRAPHIC_COLOR:            return _("Graphic");
    case NET_COLOR:                return _("Net");
    case ATTRIBUTE_COLOR:          return _("Attribute");
    case LOGIC_BUBBLE_COLOR:       return _("Logic bubble");
    case DOTS_GRID_COLOR:          return _("Grid point");
    case DETACHED_ATTRIBUTE_COLOR: return _("Detached attribute");
    case TEXT_COLOR:               return _("Text");
    case BUS_COLOR:                return _("Bus");
    case SELECT_COLOR:             return _("Selection");
    case BOUNDINGBOX_COLOR:        return _("Bounding box");
    case ZOOM_BOX_COLOR:           return _("Zoom box");
    case STROKE_COLOR:             return _("Stroke");
    case LOCK_COLOR:               return _("Lock");
    case OUTPUT_BACKGROUND_COLOR:  return _("Output background");
    case JUNCTION_COLOR:           return _("Net junction");
    case MESH_GRID_MAJOR_COLOR:    return _("Mesh grid major");
    case MESH_GRID_MINOR_COLOR:    return _("Mesh grid minor");
    case FREESTYLE0_COLOR:         return _("User Style 0");
    case FREESTYLE1_COLOR:         return _("User Style 1");
    case FREESTYLE2_COLOR:         return _("User Style 2");
    case FREESTYLE3_COLOR:         return _("User Style 3");
    case FREESTYLE4_COLOR:         return _("User Style 4");
    case FREESTYLE5_COLOR:         return _("User Style 5");
    case FREESTYLE6_COLOR:         return _("User Style 6");
    case FREESTYLE7_COLOR:         return _("User Style 7");
    case FREESTYLE8_COLOR:         return _("User Style 8");
    case FREESTYLE9_COLOR:         return _("User Style 9");
    default:
      break;
  }
  return _("Unknown");
}

/*! \brief Cell layout data function for color combobox.
 *  \par Function Description
 *  Cell layout data function to support color swatches in the color
 *  combobox.
 *
 *  \param layout
 *  \param cell
 *  \param model
 *  \param iter
 *  \param data the current GschemToplevel pointer.
 */
static void
color_menu_swatch_layout_data (GtkCellLayout   *layout,
                               GtkCellRenderer *cell,
                               GtkTreeModel    *model,
                               GtkTreeIter     *iter,
                               void            *data)
{
  GdkColor *color;
  int index;

  /* Get the index of the color on this row */
  gtk_tree_model_get (model, iter, 1, &index, -1);

  color = geda_color_x11_color_from_index (index);

  /* Set the cell's background color */
  g_object_set (cell, "background-gdk", color, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void x_dialog_color_menu_view_changed (GedaComboBox *cbox,
                                       unsigned int  view,
                                       void         *data)
{
  EdaConfig  *cfg = eda_config_get_user_context ();
  int value       = view;
  const char *grp = WIDGET_CONFIG_GROUP;
  const char *key = "color-menu-view";

  /* Save user choice of view style - either List or Menu */
  eda_config_set_integer (cfg, grp, key, value);
}

/*!
 * \brief Create a ComboBox with the gschem colors.
 * \par Function Description
 *  Creates a GedaComboBox with the color list and swatches showing
 *  each of the available colors.
 *
 *  The backing GtkTreeModel is a GtkListStore with two columns, the
 *  first holding the user-friendly name of the color, and the other
 *  the color map index.
 *
 * \param [in] w_current    The current gschem context
 * \param [in] color_index  Value of current color index
 *
 * \internal Use on:
 *                   EditColor Dialog
 *                   EditText Dialog
 */
GtkWidget *create_color_menu (GschemToplevel *w_current, int color_index)
{
  EdaConfig       *cfg;
  GtkWidget       *cbox;
  GtkListStore    *store;
  GtkCellLayout   *layout;
  GtkCellRenderer *text_cell;
  GtkCellRenderer *color_cell;

  int i;
  int list_view;

  const char *grp;
  const char *str;

  GtkTreeIter iter;

  cfg = eda_config_get_user_context();
  grp = WIDGET_CONFIG_GROUP;
  str = "color-menu-view";             /* It's called "pre-reusing" a variable */
  i_var_restore_group_integer(cfg, grp, str, &list_view, GEDA_VIEW_MENU);

  /* The columns are: name of color, index of color. */
  store  = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
  cbox   = geda_combo_box_new_with_model (GTK_TREE_MODEL (store));
  layout = GTK_CELL_LAYOUT (cbox); /* For convenience */

  g_object_set (cbox, "list-view", list_view, NULL);

  /* Renders the color swatch. Since this won't contain text, set a
   * minimum width. */
  color_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (color_cell, "width", 25, NULL);
  gtk_cell_layout_pack_start (layout, color_cell, FALSE);
  gtk_cell_layout_set_cell_data_func (layout, color_cell,
                                      color_menu_swatch_layout_data,
                                      (void*) w_current,
                                      NULL);

  /* Renders the name of the color */
  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (layout, text_cell, TRUE);
  gtk_cell_layout_add_attribute (layout, text_cell, "text", 0);

  /* Populate the list */
  for (i = 0; i < MAX_COLORS; i++) {

    /* Skip 'invalid' colors. */
    if (!geda_color_x11_get_state(i))
      continue;

    str = x_dialog_get_color_name(i);
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, 0, str, 1, i, -1);

    if (i == color_index) {
      geda_combo_widget_set_active_iter(cbox, &iter);
    }
  }

  g_signal_connect (cbox, "view-changed",
                    G_CALLBACK (x_dialog_color_menu_view_changed),
                    w_current);

  return cbox;
}

/** @} end group Common-Edit-Dialog */

/** \defgroup Fill-Type-Dialog Fill Type Editing-Dialog
 *  @{ \memberof Editing-Dialogs
 */

/*!
 * \brief Create a menu with fill types for the line type dialog
 * \par Function Description
 *  This function creates a GedaMenu with the different fill types.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 */
static GtkWidget *create_menu_filltype (GschemToplevel *w_current)
{
  GtkWidget *menu;
  GSList    *group;

  struct fill_type {
    char *str;
    OBJECT_FILLING type;
  } types[] = { { N_("Hollow"),   FILLING_HOLLOW },
                { N_("Filled"),   FILL_SOLID },
                { N_("Mesh"),     FILLING_MESH },
                { N_("Hatch"),    FILLING_HATCH },
                { N_("*varies*"), FILLING_VOID } };

  int i;

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct fill_type); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));
    group    = geda_radio_menu_item_group (menuitem);

    geda_menu_append (menu, menuitem);

    GEDA_OBJECT_SET_DATA(menuitem, (void*)(long)(types[i].type), "filltype");

    gtk_widget_show (menuitem);
  }

  return menu;
}

/*!
 * \brief Get the filltype data from selected objects
 * \par Function Description
 *  Get filltype information over all selected objects. If an object
 *  property is different than other objects, then set the value of
 *  that property equal to -2 to indicate there are mixed values.
 *
 * \param [in]   selection the selection list
 * \param [out]  type      #OBJECT_FILLING type
 * \param [out]  width     fill width.
 * \param [out]  pitch1    cross hatch line distance
 * \param [out]  angle1    cross hatch angle
 * \param [out]  pitch2    cross hatch line distance
 * \param [out]  angle2    cross hatch angle
 *
 * \returns TRUE if filltype found, FALSE otherwise
 */
static bool selection_get_fill_type(GList *selection,
                                    OBJECT_FILLING *type, int *width,
                                    int *pitch1, int *angle1,
                                    int *pitch2, int *angle2)
{
  GList *iter;
  bool found = FALSE;
  OBJECT_FILLING otype;
  int owidth, opitch1, oangle1, opitch2, oangle2;

  for (iter = selection; iter != NULL; NEXT(iter)) {

    GedaObject *object = (GedaObject*) iter->data;

    if (geda_object_get_fill_options(object, &otype, &owidth, &opitch1,
                                             &oangle1, &opitch2, &oangle2))
    {

      if (found == FALSE) {  /* first object with filltype */
        found   = TRUE;
        *type   = otype;
        *width  = owidth;
        *pitch1 = opitch1;
        *angle1 = oangle1;
        *pitch2 = opitch2;
        *angle2 = oangle2;
      }
      else {
        /* indicate mixed values with the value LEAVE_ALONE */
        if (*type   != otype)   *type   = LEAVE_ALONE;
        if (*width  != owidth)  *width  = LEAVE_ALONE;
        if (*pitch1 != opitch1) *pitch1 = LEAVE_ALONE;
        if (*angle1 != oangle1) *angle1 = LEAVE_ALONE;
        if (*pitch2 != opitch2) *pitch2 = LEAVE_ALONE;
        if (*angle2 != oangle2) *angle2 = LEAVE_ALONE;
      }
    }
  }

  return found;
}

/*!
 * \brief Set the filltype in the filltype dialog
 * \par Function Description
 *  Set all widgets in the filltype dialog. Variables marked with the
 *  invalid value -2 are set to *varies*.
 *
 * \param [in]   fill_data dialog structure
 * \param [in]   type      OBJECT_FILLING type
 * \param [in]   width     fill width.
 * \param [in]   pitch1    cross hatch line distance
 * \param [in]   angle1    cross hatch angle
 * \param [in]   pitch2    cross hatch line distance
 * \param [in]   angle2    cross hatch angle
 */
static void x_dialog_edit_fill_type_set_values(fill_type_data *fill_data,
                                               OBJECT_FILLING type, int width,
                                               int pitch1, int angle1,
                                               int pitch2, int angle2)
{
  char *text;
  GtkWidget *menu, *menuitem;

  if (type == LEAVE_ALONE)
    type = FILLING_VOID;

  geda_option_widget_set_history(fill_data->fill_type, type);
  menu = geda_option_widget_get_menu(fill_data->fill_type);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active((GedaCheckMenuItem*)menuitem, TRUE);

  if (width == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", width);

  SetEntryText   ( fill_data->width_entry, text );
  EntrySelectAll ( fill_data->width_entry );
  GEDA_FREE(text);

  if (pitch1 == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", pitch1);

  SetEntryText   ( fill_data->pitch1_entry, text );
  EntrySelectAll ( fill_data->pitch1_entry );
  GEDA_FREE(text);

  if (angle1 == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", angle1);

  SetEntryText   ( fill_data->angle1_entry, text );
  EntrySelectAll ( fill_data->angle1_entry );
  GEDA_FREE(text);

  if (pitch2 == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", pitch2);

  SetEntryText   ( fill_data->pitch2_entry, text );
  EntrySelectAll ( fill_data->pitch2_entry );
  GEDA_FREE(text);

  if (angle2 == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", angle2);

  SetEntryText   ( fill_data->angle2_entry, text );
  EntrySelectAll ( fill_data->angle2_entry );
  GEDA_FREE(text);
}

/*!
 * \brief Callback function for the filltype menu in the filltype dialog
 * \par Function Description
 *  This function sets the entry activity according to the selected
 *  filltype of the filltype dialog.
 */
static int
x_dialog_edit_fill_type_change(GtkWidget *w, fill_type_data *fill_data)
{
  GtkWidget *menuitem;
  bool activate_width_entry;
  bool activate_anglepitch1_entries;
  bool activate_anglepitch2_entries;
  int type;

  activate_width_entry         = FALSE;
  activate_anglepitch1_entries = FALSE;
  activate_anglepitch2_entries = FALSE;

  menuitem = geda_menu_widget_get_active (
              geda_option_widget_get_menu (fill_data->fill_type));

  type = (int)(long)(
    GEDA_OBJECT_GET_DATA (menuitem, "filltype"));

  switch(type) {
  case(FILLING_HOLLOW):
  case(FILL_SOLID):
    activate_width_entry         = FALSE;
    activate_anglepitch1_entries = FALSE;
    activate_anglepitch2_entries = FALSE;
    break;

  case(FILLING_HATCH):
    activate_width_entry = TRUE;
    activate_anglepitch1_entries = TRUE;
    activate_anglepitch2_entries = FALSE;
    break;

  case(FILLING_MESH):
  case(FILLING_VOID):
    activate_width_entry         = TRUE;
    activate_anglepitch1_entries = TRUE;
    activate_anglepitch2_entries = TRUE;
    break;

  default:
    BUG_IMSG ("unhandled case for", type);
  }

  gtk_widget_set_sensitive (fill_data->width_entry,  activate_width_entry);
  gtk_widget_set_sensitive (fill_data->angle1_entry, activate_anglepitch1_entries);
  gtk_widget_set_sensitive (fill_data->pitch1_entry, activate_anglepitch1_entries);
  gtk_widget_set_sensitive (fill_data->angle2_entry, activate_anglepitch2_entries);
  gtk_widget_set_sensitive (fill_data->pitch2_entry, activate_anglepitch2_entries);

  return(0);
}

/*!
 * \brief Apply the settings of the filltype dialog to the selection
 * \par Function Description
 *  This function applies the settings of the filltype dialog to the
 *  selected objects
 */
static void
x_dialog_edit_fill_type_ok(GtkWidget *Dialog, fill_type_data *fill_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;
  GedaToplevel   *toplevel  = w_current->toplevel;

  GList *selection, *iter;
  const char *width_str, *angle1_str, *pitch1_str, *angle2_str, *pitch2_str;

  OBJECT_FILLING type;
  int width, angle1, pitch1, angle2, pitch2;

  OBJECT_FILLING otype;
  int owidth, oangle1, opitch1, oangle2, opitch2;

  FILL_OPTIONS fill_options;

  /* get the selection */
  if (!o_select_is_selection(w_current))
    return;

  selection = geda_list_get_glist(Current_Selection);

  /* get the new values from the text entries of the dialog */
  width_str  = GetEntryText ( fill_data->width_entry );
  angle1_str = GetEntryText ( fill_data->angle1_entry );
  pitch1_str = GetEntryText ( fill_data->pitch1_entry );
  angle2_str = GetEntryText ( fill_data->angle2_entry );
  pitch2_str = GetEntryText ( fill_data->pitch2_entry );

  type = (int)(long)(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_widget_get_menu (fill_data->fill_type)), "filltype"));

  if (type == FILLING_VOID)
    type = LEAVE_ALONE;

  /* convert the options to integers, if string is "*varies*"
   * then there are multiple object with different values and
   * the current value for each object should not be changed.
   * To indicate this, we set such fields to LEAVE_ALONE */
  width  = g_ascii_strcasecmp (width_str,
                         _("*varies*")) ? atoi (width_str)  : LEAVE_ALONE;
  angle1 = g_ascii_strcasecmp (angle1_str,
                         _("*varies*")) ? atoi (angle1_str) : LEAVE_ALONE;
  pitch1 = g_ascii_strcasecmp (pitch1_str,
                         _("*varies*")) ? atoi (pitch1_str) : LEAVE_ALONE;
  angle2 = g_ascii_strcasecmp (angle2_str,
                         _("*varies*")) ? atoi (angle2_str) : LEAVE_ALONE;
  pitch2 = g_ascii_strcasecmp (pitch2_str,
                         _("*varies*")) ? atoi (pitch2_str) : LEAVE_ALONE;

  for (iter = selection; iter != NULL; NEXT(iter)) {

    GedaObject *object = (GedaObject*) iter->data;

    if (!geda_object_get_fill_options(object, &otype, &owidth,
                            &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    /* if the field is set to LEAVE_ALONE then use the old value,
     * otherwise use the new value extracted from the dialog */
    fill_options.fill_type   = type   == LEAVE_ALONE ? otype   : type;
    fill_options.fill_width  = width  == LEAVE_ALONE ? owidth  : width;
    fill_options.fill_pitch1 = pitch1 == LEAVE_ALONE ? opitch1 : pitch1;
    fill_options.fill_angle1 = angle1 == LEAVE_ALONE ? oangle1 : angle1;
    fill_options.fill_pitch2 = pitch2 == LEAVE_ALONE ? opitch2 : pitch2;
    fill_options.fill_angle2 = angle2 == LEAVE_ALONE ? oangle2 : angle2;

    /* if the value retrieved from the dialog is -1 then substitute the
     * default value. Set non-required options to -1 */
    switch (type) {
    case (FILLING_HOLLOW):
    case (FILL_SOLID):
      fill_options.fill_pitch1 = -1;
      fill_options.fill_angle1 = -1;
      fill_options.fill_pitch2 = -1;
      fill_options.fill_angle2 = -1;
      break;

    case (FILLING_HATCH):
      if (fill_options.fill_width  == -1) fill_options.fill_width  = toplevel->default_fill_width;
      if (fill_options.fill_pitch1 == -1) fill_options.fill_pitch1 = toplevel->default_fill_pitch1;
      if (fill_options.fill_angle1 == -1) fill_options.fill_angle1 = toplevel->default_fill_angle1;
      fill_options.fill_pitch2 = -1;
      fill_options.fill_angle2 = -1;
      break;

    case (FILLING_MESH):
      if (fill_options.fill_width  == -1) fill_options.fill_width  = toplevel->default_fill_width;
      if (fill_options.fill_pitch1 == -1) fill_options.fill_pitch1 = toplevel->default_fill_pitch1;
      if (fill_options.fill_angle1 == -1) fill_options.fill_angle1 = toplevel->default_fill_angle1;
      if (fill_options.fill_pitch2 == -1) fill_options.fill_pitch2 = toplevel->default_fill_pitch2;
      if (fill_options.fill_angle2 == -1) fill_options.fill_angle2 = toplevel->default_fill_angle2;
      break;

    default:
      BUG_IMSG ("unhandled case for value", type);
    }

    geda_set_object_fill_options (object, &fill_options);
    o_invalidate_object (w_current, object);
  }

  o_undo_savestate(w_current, UNDO_ALL);
}

/*!
 * \brief response function for the filltype dialog
 * \par Function Description
 *  This function handles the user response to the filltype dialog.
 *  It destroys the dialog after that.
 */
static void
x_dialog_edit_fill_type_response(GtkWidget *Dialog, int response,
                                 fill_type_data *fill_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_grab_remove (Dialog);
    gtk_widget_destroy (Dialog);
    GEDA_FREE (fill_data);
    break;
  case GEDA_RESPONSE_ACCEPT:
    x_dialog_edit_fill_type_ok(Dialog, fill_data);
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }

  i_status_set_state (w_current, SELECT);

}

/*!
 * \brief Handle selection change event for x_dialog_edit_fill_type
 * \par Function Description
 *  Updates the fill_type dialog widgets when the selection changes.
 *  It uses the selection to set it's initial values.
 *
 * \param w_current pointer to GschemToplevel context
 * \param object    pointer to a selected Object.
 */
static void
x_dialog_fill_type_update_selection (GschemToplevel *w_current,
                                     GedaObject *object)
{
  GtkWidget *Dialog;

  fill_type_data *fill_data;

  /* Get ptr to the data structure */
  Dialog    = w_current->hpwindow;
  fill_data = GEDA_OBJECT_GET_DATA(Dialog, IDS_FILL_TYPE);

  if (o_select_is_selection(w_current)) {

    OBJECT_FILLING type = FILLING_VOID;

    int width=0, pitch1=0, angle1=0, pitch2=0, angle2=0;

    GList *selection = geda_list_get_glist(Current_Selection);

    if (selection_get_fill_type (selection, &type, &width,
                                 &pitch1, &angle1, &pitch2, &angle2))
    {

      x_dialog_edit_fill_type_set_values(fill_data, type, width,
                                         pitch1, angle1, pitch2, angle2);
      /* Set the widget activity according to the current filltype */
      x_dialog_edit_fill_type_change(fill_data->fill_type, fill_data);

      gtk_widget_grab_focus(fill_data->width_entry);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
GtkWidget *x_dialog_fill_type_create_dialog(GschemToplevel *w_current)
{
  GtkWidget *Dialog;
  GtkWidget *vbox;
  GtkWidget *optionmenu = NULL;
  GtkWidget *width_entry = NULL;
  GtkWidget *angle1_entry = NULL;
  GtkWidget *pitch1_entry = NULL;
  GtkWidget *angle2_entry = NULL;
  GtkWidget *pitch2_entry = NULL;
  GtkWidget *table;
  GtkWidget *label;

  fill_type_data *fill_data;
  Dialog = gschem_dialog_new_with_buttons(_("Edit Fill Type"),
                                          w_current->main_window,
         /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                          IDS_FILL_TYPE, w_current,
                                          GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                          GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                          NULL);
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_REJECT,
                                          -1);

  gtk_dialog_set_default_response(GTK_DIALOG(Dialog), GEDA_RESPONSE_ACCEPT);

  vbox = GTK_DIALOG(Dialog)->vbox;

  table = gtk_table_new (6, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  PACK_START(vbox, table, FALSE, FALSE, 0);

  label = geda_aligned_label_new (_("Fill Type:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Line Width:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Angle 1:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Pitch 1:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Angle 2:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,4,5, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Pitch 2:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,5,6, GTK_FILL,0,0,0);

  optionmenu = geda_option_menu_new ();
  geda_option_widget_set_menu(optionmenu, create_menu_filltype (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), optionmenu,  1,2,0,1);
  SetWidgetTip(optionmenu, _("Select fill pattern"));

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry, 1,2,1,2);
  SetWidgetTip(width_entry,  _("Set the width of the filler lines"));

  angle1_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(angle1_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), angle1_entry, 1,2,2,3);
  SetWidgetTip(angle1_entry, _("Primary angle for filler lines"));

  pitch1_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(pitch1_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), pitch1_entry, 1,2,3,4);
  SetWidgetTip(pitch1_entry,  _("Spacing for the primary filler lines"));

  angle2_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(angle2_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), angle2_entry, 1,2,4,5);
  SetWidgetTip(angle2_entry, _("Secondary angle for filler lines"));

  pitch2_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(pitch2_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), pitch2_entry, 1,2,5,6);
  SetWidgetTip(pitch2_entry, _("Spacing for the secondary filler lines"));

  HD_ACTION_SEPARATOR (vbox);

  fill_data = (fill_type_data*) GEDA_MEM_ALLOC (sizeof(struct st_fill_type_data));

  /* populate the data structure */
  fill_data->fill_type    = optionmenu;
  fill_data->width_entry  = width_entry;
  fill_data->angle1_entry = angle1_entry;
  fill_data->pitch1_entry = pitch1_entry;
  fill_data->angle2_entry = angle2_entry;
  fill_data->pitch2_entry = pitch2_entry;

  /* fill in the fields of the dialog */
  x_dialog_edit_fill_type_set_values(fill_data, FILLING_VOID, 0, 0, 0, 0, 0);

  g_signal_connect (optionmenu, "changed",
                    G_CALLBACK (x_dialog_edit_fill_type_change),
                    fill_data);

  g_signal_connect (Dialog, "response",
                    G_CALLBACK (x_dialog_edit_fill_type_response),
                    fill_data);

  g_object_set (Dialog, DIALOG_SELECTION_TRACKER,
                x_dialog_fill_type_update_selection,
                NULL);

  GEDA_OBJECT_SET_DATA(Dialog, fill_data, IDS_FILL_TYPE);

  return Dialog;
}

/*!
 * \brief Creates the fill type dialog
 * \par Function Description
 *  This function creates the fill type dialog.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_edit_fill_type(GschemToplevel *w_current)
{
  GtkWidget *Dialog;

  Dialog = w_current->hpwindow;

  if (!Dialog) {

    Dialog = x_dialog_fill_type_create_dialog(w_current);

    gtk_window_set_position(GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);

    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  w_current->main_window);

    w_current->hpwindow = Dialog;
    gtk_widget_show_all (Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }
  x_dialog_fill_type_update_selection (w_current, NULL);
}

/******************* End of Fill Type dialog box ************************/

/** @} end group Fill-Type-Dialog */

/** \defgroup Line-Type-Dialog Line Type Editing-Dialog
 *  @{ \memberof Editing-Dialogs
 */

/*!
 * \brief Create a line end type menu for the line type dialog
 * \par Function Description
 *  This function creates a GedaMenu with the different line end types.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 */
static GtkWidget *create_endtype_menu (GschemToplevel *w_current)
{
  GtkWidget *menu;
  GSList    *group;

  struct end_type {
    char *str;
    LINE_END end;
  } types[] = { { N_("None"),     END_NONE   },
                { N_("Square"),   END_SQUARE },
                { N_("Round"),    END_ROUND  },
                { N_("*varies*"), END_VOID   }
              };
  int i;

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct end_type); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));
    group    = geda_radio_menu_item_group (menuitem);

    geda_menu_append (menu, menuitem);
    GEDA_OBJECT_SET_DATA(menuitem,
                         (void*)(long) (types[i].end), "endtype");
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*!
 * \brief Create a line type menu for the line type dialog
 * \par Function Description
 *  This function creates a GedaMenu with the different linetypes.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 */
static GtkWidget *create_linetype_menu (GschemToplevel *w_current)
{
  GtkWidget *menu;
  GSList    *group;

  struct line_type {
    char *str;
    LINE_TYPE type;
  } types[] = { { N_("Solid"),       TYPE_SOLID   },
                { N_("Dotted"),      TYPE_DOTTED  },
                { N_("Dashed"),      TYPE_DASHED  },
                { N_("Center"),      TYPE_CENTER  },
                { N_("Phantom"),     TYPE_PHANTOM },
                { N_("*varies*"), TYPE_ERASE } };
  int i;

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct line_type); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));
    group    = geda_radio_menu_item_group (menuitem);

    geda_menu_append (menu, menuitem);
    GEDA_OBJECT_SET_DATA(menuitem, (void*)(long)(types[i].type), "linetype");
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*!
 * \brief get the linetype data from selected objects
 * \par Function Description
 *  Get linetype information over all selected objects.
 *  If a object property is different to the other objects, then
 *  return LEAVE_ALONE in that variable.
 *
 * \param [in]   selection the selection list
 * \param [out]  end       #LINE_END type
 * \param [out]  type      OBJECT_FILLING type
 * \param [out]  width     line width
 * \param [out]  length    length of each line
 * \param [out]  space     space between points and lines
 *
 * \returns TRUE if linetype found, FALSE otherwise
 */
static bool
selection_get_line_type(GList *selection, LINE_END *end, LINE_TYPE *type,
                        int *width, int *length, int *space)
{
  GList  *iter;

  LINE_END   oend;
  LINE_TYPE  otype;

  bool found = FALSE;
  int owidth = 0, olength = 0, ospace = 0;

  for (iter = selection; iter != NULL; NEXT(iter)) {

    GedaObject *object = iter->data;

    if (! geda_object_get_line_options(object,  &oend,    &otype,
                             &owidth, &olength, &ospace))
      continue;

    if (found == FALSE) {  /* first object with filltype */
       found  = TRUE;
      *end    = oend;
      *type   = otype;
      *width  = owidth;
      *length = olength;
      *space  = ospace;
    }
    else {
      /* indicate mixed values with the value LEAVE_ALONE = -2 */
      if (*end    != oend)    *end    = LEAVE_ALONE;
      if (*type   != otype)   *type   = LEAVE_ALONE;
      if (*width  != owidth)  *width  = LEAVE_ALONE;
      if (*length != olength) *length = LEAVE_ALONE;
      if (*space  != ospace)  *space  = LEAVE_ALONE;
    }
  }

  return found;
}

/*!
 * \brief set the linetype in the linetype dialog
 * \par Function Description
 *  Set all widgets in the linetype dialog. Variables marked with the
 *  invalid value LEAVE_ALONE (-2) are set to *varies*.
 *
 * \param [in]   line_data line dialog structure
 * \param [in]   end       #LINE_END type (currently not used)
 * \param [in]   type      #LINE_TYPE type
 * \param [in]   width     fill width.
 * \param [in]   length    length of each line
 * \param [in]   space     space between points and lines
 */
static void
x_dialog_edit_line_type_set_values(line_type_data *line_data,
                                   LINE_END end, LINE_TYPE type,
                                   int width, int length, int space)
{
  char *text;
  GtkWidget *menu, *menuitem;

  if (type == LEAVE_ALONE)
    type = TYPE_ERASE;

  geda_option_widget_set_history(line_data->line_type, type);
  menu = geda_option_widget_get_menu(line_data->line_type);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active(GEDA_CHECK_MENU_ITEM(menuitem), TRUE);

  if (end == LEAVE_ALONE)
    end = END_VOID;

  geda_option_widget_set_history(line_data->end_type, end);
  menu = geda_option_widget_get_menu(line_data->end_type);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active(GEDA_CHECK_MENU_ITEM(menuitem), TRUE);

  if (width == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", width);

  SetEntryText   ( line_data->width_entry, text );
  EntrySelectAll ( line_data->width_entry );
  GEDA_FREE(text);

  if (length == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", length);

  SetEntryText   ( line_data->length_entry, text );
  EntrySelectAll ( line_data->length_entry );

  GEDA_FREE(text);

  if (space == LEAVE_ALONE)
    text = geda_strdup(_("*varies*"));
  else
    text = geda_sprintf ("%d", space);
  SetEntryText   ( line_data->space_entry, text );
  EntrySelectAll ( line_data->space_entry );
  GEDA_FREE(text);
}

/*!
 * \brief Callback function for the linetype menu item in the line type dialog
 * \par Function Description
 *  This Function is called when the user changes the line type selection.
 *  It sets the dash space/length entries either active or inactive.
 */
static int
x_dialog_edit_line_type_change(GtkWidget *w, line_type_data *line_data)
{
  GtkWidget *menuitem;
  bool activate_length_entry, activate_space_entry;
  int type;

  menuitem = geda_menu_widget_get_active (
              geda_option_widget_get_menu (line_data->line_type));

  type = (int)(long)(GEDA_OBJECT_GET_DATA(menuitem, "linetype"));

  switch(type) {
      case(TYPE_SOLID):
        activate_length_entry = FALSE;
        activate_space_entry  = FALSE;
        break;
      case(TYPE_DOTTED):
        activate_length_entry = FALSE;
        activate_space_entry  = TRUE;
        break;
      case(TYPE_DASHED):
      case(TYPE_CENTER):
      case(TYPE_PHANTOM):
        activate_length_entry = TRUE;
        activate_space_entry  = TRUE;
        break;
      default:
        activate_length_entry = TRUE;
        activate_space_entry  = TRUE;
  }

  gtk_widget_set_sensitive (line_data->space_entry, activate_space_entry);
  gtk_widget_set_sensitive (line_data->length_entry, activate_length_entry);

  return(0);
}

/*!
 * \brief Worker function for the line type and width dialog
 * \par Function Description
 *  The function takes the properties of the dialog and applies
 *  them to the selected objects.
 */
static void
x_dialog_edit_line_type_ok(GtkWidget *Dialog, line_type_data *line_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;
  GedaToplevel   *toplevel  = w_current->toplevel;

  GList      *selection, *iter;
  const char *width_str, *length_str, *space_str;

  LINE_TYPE  type;
  LINE_END   end;
  int width, length, space;

  LINE_TYPE  otype;
  LINE_END   oend;
  int owidth, olength, ospace;

  LINE_OPTIONS line_options;

  /* get the selection */
  if (!o_select_is_selection(w_current))
    return;

  selection = geda_list_get_glist(Current_Selection);

  /* get the new values from the text entries of the dialog */
  width_str   = GetEntryText ( line_data->width_entry );
  length_str  = GetEntryText ( line_data->length_entry );
  space_str   = GetEntryText ( line_data->space_entry );

  type = (int)(long)(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_widget_get_menu (line_data->line_type)), "linetype"));

  if (type == TYPE_ERASE) {
    type = LEAVE_ALONE;
  }

  end = (int)(long)(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_widget_get_menu (line_data->end_type)), "endtype"));

  if (end == END_VOID) {
    end = LEAVE_ALONE;
  }

  /* convert the options to integers, if string is "*varies*"
   * then there are multiple object with different values and
   * the current value for each object should not be changed.
   * To indicate this, we set such fields to LEAVE_ALONE */
  width =  g_ascii_strcasecmp (width_str,
                         _("*varies*")) ? atoi (width_str)  : LEAVE_ALONE;
  length = g_ascii_strcasecmp (length_str,
                         _("*varies*")) ? atoi (length_str) : LEAVE_ALONE;
  space  = g_ascii_strcasecmp (space_str,
                         _("*varies*")) ? atoi (space_str)  : LEAVE_ALONE;

  for (iter = selection; iter != NULL; NEXT(iter)) {

     GedaObject *object = (GedaObject*) iter->data;

    if (! geda_object_get_line_options(object,  &oend,    &otype,
                             &owidth, &olength, &ospace))
      continue;

    /* if the field is set to LEAVE_ALONE then use the old value,
     * otherwise use the new value extracted from the dialog */

    line_options.line_type   = type   == LEAVE_ALONE ? otype   : type;
    line_options.line_end    = end    == LEAVE_ALONE ? oend    : end;
    line_options.line_width  = width  == LEAVE_ALONE ? owidth  : width;
    line_options.line_length = length == LEAVE_ALONE ? olength : length;
    line_options.line_space  = space  == LEAVE_ALONE ? ospace  : space;

    /* if the value retrieved from the dialog is -1 then substitute the
     * default value. Set non-required options to -1 */
    switch (type) {
    case (TYPE_SOLID):
      line_options.line_length = -1;
      line_options.line_space  = -1;
      break;

    case (TYPE_DOTTED):
      line_options.line_length = -1;
      if (line_options.line_space == -1) line_options.line_space = toplevel->default_line_space;
      break;

    case (TYPE_DASHED):
    case (TYPE_CENTER):
    case (TYPE_PHANTOM):
      if (line_options.line_length == -1) line_options.line_length = toplevel->default_line_length;
      if (line_options.line_space  == -1) line_options.line_space  = toplevel->default_line_space;
      break;

    default:
      BUG_IMSG("unhandlered case for type", type);
    }

    geda_set_object_line_options (object, &line_options);
    o_invalidate_object (w_current, object);
  }

  o_undo_savestate(w_current, UNDO_ALL);
}

/*!
 * \brief response function for the line type and width dialog
 * \par Function Description
 *  This function takes the user input and applies it to selected
 *  objects. After that it kills the dialog.
 */
void
x_dialog_edit_line_type_response(GtkWidget *Dialog, int response,
                                 line_type_data *line_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy (Dialog);
    GEDA_FREE (line_data);
    break;

  case GEDA_RESPONSE_ACCEPT:
    x_dialog_edit_line_type_ok(Dialog, line_data);
    break;

  default:
    BUG_IMSG ("unhandled case", response);

  }

  i_status_set_state (w_current, SELECT);

}

/*!
 * \brief Handle selection change event for x_dialog_edit_fill_type
 * \par Function Description
 *  Updates the fill_type dialog widgets when the selection changes.
 *  It uses the selection to set it's initial values.
 *
 * \param w_current pointer to GschemToplevel context
 * \param object    pointer to a selected Object.
 */
static void
x_dialog_line_type_update_selection (GschemToplevel *w_current,
                                     GedaObject     *object)
{
  GtkWidget *Dialog;

  LINE_END   end  = END_NONE;
  LINE_TYPE  type = TYPE_SOLID;

  int width = 1, length = -1, space = -1;

  line_type_data *line_data;

  /* Get ptr to the data structure */
  Dialog    = w_current->ltwindow;

  line_data = GEDA_OBJECT_GET_DATA (Dialog, IDS_LINE_TYPE);

  if (o_select_is_selection(w_current)) {

    GList *selection = geda_list_get_glist(Current_Selection);

    if (selection_get_line_type(selection, &end, &type, &width, &length, &space))
    {
      /* fill in the fields of the dialog */
      x_dialog_edit_line_type_set_values(line_data, end, type, width, length, space);

      /* calling it once will set the dash space/length activity */
      x_dialog_edit_line_type_change(line_data->line_type, line_data);

      gtk_widget_grab_focus(line_data->width_entry);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
GtkWidget *x_dialog_line_type_create_dialog(GschemToplevel *w_current)
{
  GtkWidget *Dialog;
  GtkWidget *vbox;
  GtkWidget *line_type    = NULL;
  GtkWidget *end_type     = NULL;
  GtkWidget *length_entry = NULL;
  GtkWidget *space_entry  = NULL;
  GtkWidget *width_entry  = NULL;
  GtkWidget *table;
  GtkWidget *label;

  line_type_data *line_data;

  Dialog = gschem_dialog_new_with_buttons(_("Edit Line Width & Type"),
                                          w_current->main_window,
         /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                          IDS_LINE_TYPE, w_current,
                                          GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                          GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_REJECT,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (Dialog), GEDA_RESPONSE_ACCEPT);

  vbox = GTK_DIALOG(Dialog)->vbox;

  table = gtk_table_new (5, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  PACK_START(vbox, table, FALSE, FALSE, 0);

  label = geda_aligned_label_new (_("Line Type:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("End Type:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Width:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Dash Length:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  label = geda_aligned_label_new (_("Dash Space:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,4,5, GTK_FILL,0,0,0);

  line_type = geda_option_menu_new ();
  geda_option_widget_set_menu(line_type, create_linetype_menu (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), line_type, 1,2,0,1);

  end_type = geda_option_menu_new ();
  geda_option_widget_set_menu(end_type, create_endtype_menu (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), end_type, 1,2,1,2);

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(width_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry, 1,2,2,3);
  SetWidgetTip(width_entry, _("Set width of the line"));

  length_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(length_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(length_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), length_entry, 1,2,3,4);
  SetWidgetTip(length_entry, _("Set \"dash\" length of the line"));

  space_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(space_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(space_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), space_entry, 1,2,4,5);
  SetWidgetTip(space_entry, _("Set spacing between dashes in the line"));

  HD_ACTION_SEPARATOR (vbox);

  line_data = (line_type_data*) GEDA_MEM_ALLOC (sizeof(struct st_line_type_data));

  /* populate the data structure */
  line_data->width_entry  = width_entry;
  line_data->line_type    = line_type;
  line_data->end_type     = end_type;
  line_data->length_entry = length_entry;
  line_data->space_entry  = space_entry;

  /* fill in the fields of the dialog */
  x_dialog_edit_line_type_set_values(line_data, END_NONE, TYPE_SOLID, 1, 1, 1);

  /* calling it once will set the dash space/length activity */
  x_dialog_edit_line_type_change(line_type, line_data);

  g_signal_connect(line_type, "changed",
                   G_CALLBACK (x_dialog_edit_line_type_change),
                   line_data);

  g_signal_connect (Dialog, "response",
                    G_CALLBACK (x_dialog_edit_line_type_response),
                    line_data);

  g_object_set (Dialog, DIALOG_SELECTION_TRACKER,
                x_dialog_line_type_update_selection,
                NULL);

  GEDA_OBJECT_SET_DATA(Dialog, line_data, IDS_LINE_TYPE);

  return Dialog;
}

/*!
 * \brief Creates the line type and width dialog
 * \par Function Description
 *  This function creates and sets up a dialog for manipulating
 *  properties of line objects.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_edit_line_type (GschemToplevel *w_current)
{
  GtkWidget *Dialog;
  Dialog = w_current->ltwindow;

  if (!Dialog) {

    Dialog = x_dialog_line_type_create_dialog(w_current);

    gtk_window_set_position(GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);

    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  w_current->main_window);

    w_current->ltwindow = Dialog;
    gtk_widget_show_all (Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }
  x_dialog_line_type_update_selection (w_current, NULL);
}

/***************** End of Line Type / Width dialog box ****************/

/** @} end group Line-Type-Dialog */


/** \defgroup Find-Text-Dialog Find Text Editing-Dialogs
 *  @{ \memberof Editing-Dialogs
 */

Page *remember_page;
Page *forget_page;

/*!
 * \brief response function for the find text dialog
 * \par Function Description
 *  This function calls o_edit_find_text to search schematics for
 *  the user input string.
 */
void x_dialog_find_text_response(GtkWidget *Dialog, int response,
                                 GschemToplevel *w_current)
{
  GedaToplevel    *toplevel = w_current->toplevel;
  GtkToggleButton *checkdescend;
  GtkToggleButton *checkhidden;
  GtkWidget       *textentry;
  const char      *string;
  int done=0, close=0;
  unsigned    search_flags;
  int         start_find;

  if (remember_page == NULL) {
    remember_page = geda_toplevel_get_current_page(toplevel);
    start_find = TRUE;
  }
  else {
    start_find = FALSE;
  }

  switch (response) {
    case GEDA_RESPONSE_ACCEPT:

      /* Get the stored pointer to objects */
      checkdescend = GEDA_OBJECT_GET_DATA(Dialog, "checkdescend");
      checkhidden  = GEDA_OBJECT_GET_DATA(Dialog, "checkhidden");
      textentry    = GEDA_OBJECT_GET_DATA(Dialog, IDS_FIND_TEXT);

      search_flags = 0;

      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (checkdescend))) {
        search_flags = SEARCH_DESCEND;
      }

      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (checkhidden))) {
        search_flags += SEARCH_HIDDEN;
      }

      /* Retrieve the text string from the Entry widget */
      string = GetEntryText( textentry );

      /* Check if new search string */
      if (strcmp(string, text_buffer)) {
        start_find = TRUE;
      }

      /* Save the string in the shared buffer */
      set_text_buffer(string);

      if (remember_page != geda_toplevel_get_current_page(toplevel)) {
        geda_struct_page_goto(remember_page);
      }

      done = o_edit_find_text (w_current,
                               geda_struct_page_get_objects (remember_page),
                               string,
                               search_flags,
                               !start_find);

      if (done) {
        o_invalidate_all (w_current);
        close = TRUE;
      }
      else {

        Page *current_page;

        current_page = geda_toplevel_get_current_page(toplevel);

        if (forget_page == NULL) {
          forget_page = current_page;
        }

        if (current_page == remember_page) {

          GtkWidget *checkascent;

          checkascent = GEDA_OBJECT_GET_DATA(Dialog, "checkascent");

          if (GetToggleState(checkascent)) {
            x_window_close_page (w_current, forget_page);
          }
        }
      }
      break;

    case GEDA_RESPONSE_REJECT:
    case GEDA_RESPONSE_DELETE_EVENT:
      close = TRUE;
      break;

    default:
      BUG_IMSG ("unhandled case", response);
      close = TRUE;
  }

  if (close) {
    gtk_widget_destroy(Dialog);
  }
}

/*!
 * \brief Find Text Dialog Descend into Hierarchy Check-box Callback
 *  Enable or disabled sensitivity of Close-on-Ascent checkbox
 *  based on the state of the check-box.
 *
 * \param [in] check_butt Pointer to the Descend CheckBox widget
 * \param [in] cb         Pointer to the Ascend CheckBox widget
 */
static void x_dialog_find_text_on_descend (GtkWidget *check_butt, GtkWidget *cb)
{
  bool state = GetToggleState(check_butt);

  gtk_widget_set_sensitive(cb, state);
}

/*!
 * \brief Create the text find dialog
 * \par Function Description
 *  This function creates the text find dialog.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 *
 * \note remember_page is also used as a flag to indicate the search
 *  should start from the beginning, as oppose to continue searching.
 */
void x_dialog_find_text(GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  GtkWidget  *textentry;
  GedaObject *object;

  remember_page = NULL;
  forget_page   = NULL;

  if ((object = o_select_return_first_object(w_current)) != NULL) {

    if (object->type == OBJ_TEXT) {

      const char *string = geda_text_object_get_string (object);

      set_text_buffer(string);
    }
  }

  if (w_current->ftwindow) {
    /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->ftwindow));
    ThisDialog = w_current->ftwindow;
  }
  else {

    GtkWidget  *vbox;
    GtkWidget  *alignment;
    GtkWidget  *checkascent;
    GtkWidget  *checkdescend;
    GtkWidget  *checkhidden;
    GtkWidget  *label;

    ThisDialog = gschem_dialog_new_with_buttons(_("Find Text"),
                            w_current->main_window,
       /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                      IDS_FIND_TEXT, w_current,
                          GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                           GTK_STOCK_FIND, GEDA_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                     GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    label = geda_aligned_label_new(_("Text to find:"), 0, 0);
    PACK_START(vbox, label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(FIND_DIALOG_MAX_ENTRY);
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    PACK_START(vbox, textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    checkhidden = gtk_check_button_new_with_label(_("Search hidden attributes"));
    PACK_START(vbox, checkhidden, TRUE, TRUE, 2);

    checkdescend = gtk_check_button_new_with_label(_("Descend into hierarchy"));
    PACK_START(vbox, checkdescend, TRUE, TRUE, 2);

    alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                              "left-padding",  25,
                              NULL);
    PACK_START(vbox, alignment, TRUE, TRUE, 0);

    checkascent = gtk_check_button_new_with_label(_("Close on ascent"));
    geda_container_add (alignment, checkascent);
    gtk_widget_set_sensitive(checkascent, FALSE);

    GEDA_HOOKUP_OBJECT(ThisDialog, textentry, IDS_FIND_TEXT);
    GEDA_HOOKUP_OBJECT(ThisDialog, checkhidden,  "checkhidden");
    GEDA_HOOKUP_OBJECT(ThisDialog, checkdescend, "checkdescend");
    GEDA_HOOKUP_OBJECT(ThisDialog, checkascent,  "checkascent");

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_find_text_response),
                      w_current);

    GTK_CALLBACK_TOGGLED (checkdescend,
                          x_dialog_find_text_on_descend,
                          checkascent);

    gtk_widget_show_all(ThisDialog);
  }

  /* always select the text string in the entry */
  textentry = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_FIND_TEXT);
  SetEntryText   (textentry, text_buffer);
  EntrySelectAll (textentry);
}

/*********** End of find text dialog box *******/

/** @} End Group Find-Text-Dialog */

/** \defgroup Edit-Hide-Text-Dialog Hide Text Editing-Dialogs
 *  @{ \memberof Editing-Dialogs
 */

/*********** Start of hide text dialog box *******/

/*! \brief Response function for the hide text dialog
 *  \par Function Description
 *  This is the response function of the hide text dialog. It takes the user input
 *  and hides all text elements that starts with the searchtext.
 */
void x_dialog_hide_text_response(GtkWidget *Dialog, int response,
                                 GschemToplevel *w_current)
{
  GtkWidget  *textentry;
  const char *string;

  switch (response) {
  case GEDA_RESPONSE_ACCEPT:

    /* Get the stored pointer to the entry object */
    textentry = GEDA_OBJECT_GET_DATA(Dialog, IDS_HIDE_TEXT);

    /* Retrieve the text string from the Entry widget */
    string = GetEntryText( textentry );

    /* Save the string in the shared buffer */
    set_text_buffer(string);

    o_edit_hide_specific_text (w_current,
                               geda_struct_page_get_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(Dialog);;
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }
}

/*! \brief Creates the hide text dialog
 *  \par Function Description
 *  This function creates the hide text dialog.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_hide_text(GschemToplevel * w_current)
{
  GtkWidget *ThisDialog = w_current->htwindow;
  GtkWidget *textentry;

  if (!ThisDialog) {

    GtkWidget *label;
    GtkBox    *vbox;

    ThisDialog = gschem_dialog_new_with_buttons(_("Hide Text"),
                            w_current->main_window,
      /* nonmodal Editing Dialog */     GSCHEM_MODELESS_DIALOG,
                                        IDS_HIDE_TEXT, w_current,
                            GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                            GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                                           NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = (GtkBox*)GTK_DIALOG(ThisDialog)->vbox;

    label = geda_aligned_label_new(_("Hide text starting with:"), 0, 0);
    PACK_START(vbox, label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(HIDE_DIALOG_MAX_ENTRY);
    PACK_START(vbox, textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GEDA_HOOKUP_OBJECT(ThisDialog, textentry, IDS_HIDE_TEXT);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_hide_text_response),
                      w_current);

    gtk_widget_show_all(ThisDialog);
    w_current->htwindow = ThisDialog;
  }

  else { /* dialog already created, just select it */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always select the text in the search entry */
  textentry = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_HIDE_TEXT);
  SetEntryText (textentry, text_buffer);
  EntrySelectAll (textentry);
}

/*********** End of hide text dialog box *******/

/** @} End Group Hide-Text-Dialog */

void x_dialog_patch_text(GschemToplevel *w_current)
{
  fprintf(stderr, "Weeeeee!\n");
}

/** \defgroup Show-Text-Dialog Show Text Dialog
 *  @{ \memberof Editing-Dialogs
 */

/*********** Start of show text dialog box *******/

/*! \brief Response function for the show text dialog
 *  \par Function Description
 *  This function takes the users input and searches all strings starting with
 *  the given search text and hides those text objects.
 */
void x_dialog_show_text_response(GtkWidget *Dialog, int response,
                                 GschemToplevel *w_current)
{
  GtkWidget  *textentry;
  const char *string;

  switch (response) {
  case GEDA_RESPONSE_ACCEPT:

    /* Get the stored pointer to the entry object */
    textentry = GEDA_OBJECT_GET_DATA(Dialog,IDS_SHOW_TEXT);

    /* Retrieve the text string from the Entry widget */
    string    = GetEntryText( textentry );

    /* Save the string in the shared buffer */
    set_text_buffer(string);

    o_edit_show_specific_text (w_current,
                               geda_struct_page_get_objects (Current_Page),
                               string);
    break;
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(Dialog);
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }
}

/*! \brief Create the show text dialog.
 *  \par Function Description
 *  This function creates the show text dialog.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_show_text(GschemToplevel * w_current)
{
  GtkWidget *ThisDialog = w_current->stwindow;
  GtkWidget *textentry;

  if (!ThisDialog) {

    GtkWidget *label;
    GtkWidget *vbox;

    ThisDialog = gschem_dialog_new_with_buttons(_("Show Text"),
                            w_current->main_window,
       /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                      IDS_SHOW_TEXT, w_current,
                          GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                          GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    label = geda_aligned_label_new(_("Show text starting with:"), 0, 0);
    PACK_START(vbox, label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(SHOW_TEXT_DIALOG_MAX_ENTRY);
    PACK_START(vbox, textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GEDA_HOOKUP_OBJECT(ThisDialog, textentry, IDS_SHOW_TEXT);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_show_text_response),
                      w_current);

    gtk_widget_show_all(ThisDialog);
    w_current->stwindow = ThisDialog;
  }

  else { /* dialog already created. Show it */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always select the text in the entry */
  textentry = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_SHOW_TEXT);
  SetEntryText   ( textentry, text_buffer );
  EntrySelectAll ( textentry );
}

/*********** End of show text dialog box *******/

/** @} End Group Show-Text-Dialog */

/** \defgroup Text-Input-Dialog Text-Input Dialog
 *  @{ \memberof Editing-Dialogs
 */

/*! \brief Apply function for the text entry dialog
 *  \par Function Description
 *  This function applies the text from the text entry dialog.
 */
void x_dialog_text_input_apply(GtkWidget *Dialog, GschemToplevel *w_current)
{
  GtkWidget     *tientry;
  GtkTextBuffer *textbuffer;
  GtkTextIter    start, end;
  char          *string;

  tientry    = GEDA_OBJECT_GET_DATA(Dialog, IDS_TEXT_INPUT);
  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tientry));

  gtk_text_buffer_get_bounds (textbuffer, &start, &end);

  string =  gtk_text_iter_get_text (&start, &end);

  if (string[0] != '\0' ) {

    char *tmp;

    switch(w_current->text_case) {
      case(LOWER_CASE):
        tmp = g_utf8_strdown (string, -1);
        break;

      case(UPPER_CASE):
        tmp = g_utf8_strup (string, -1);
        break;

      case(BOTH_CASES):
      default:
        tmp = NULL;
        break;
    }

    /* Select the text, so you can continue immediatly writing the next text */
    select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
    gtk_widget_grab_focus(tientry);

    o_text_prepare_place (w_current, tmp == NULL ? string : tmp);
    GEDA_FREE (string);
    GEDA_FREE (tmp);
  }
}

/*! \brief Response Function for the Text Entry dialog
 *  \par Function Description
 *  Callback function for the text entry dialog.
 */
void x_dialog_text_input_response(GtkWidget *Dialog, int response,
                                  GschemToplevel *w_current)
{
  switch(response) {
  case GEDA_RESPONSE_ACCEPT:
    x_dialog_text_input_apply(Dialog, w_current);
    break;
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    i_status_set_state(w_current, SELECT);
    gtk_widget_destroy(Dialog);
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }
}

/*! \brief create or present the text entry dialog
 *  \par Function Description
 *  This function creates or raises the modeless text entry dialog
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_text_input (GschemToplevel *w_current)
{
  GtkWidget *ThisDialog = w_current->tiwindow;
  GtkWidget *tientry;

  if (!ThisDialog) { /* dialog not created yet */

    GtkWidget *label;
    GtkWidget *viewport1;
    GtkWidget *scrolled_window;
    GtkWidget *vbox;

    PangoTabArray *tab_array;
    int real_tab_width;

    ThisDialog = gschem_dialog_new_with_buttons(_("Text Entry..."),
                                w_current->main_window,
           /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                         IDS_TEXT_INPUT, w_current,
                              GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                              GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                                              NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_window_set_position(GTK_WINDOW (ThisDialog), GTK_WIN_POS_NONE);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_text_input_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    label = geda_aligned_label_new (_("Enter text, click apply,\n"
                                      "move cursor into window, click to place text.\n"
                                      "Middle button to rotate while placing."), 0, 0);

    PACK_START(vbox, label, FALSE, FALSE, 0);

    viewport1 = gtk_viewport_new (NULL, NULL);
    gtk_widget_show (viewport1);

    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    geda_container_add (viewport1, scrolled_window);
    PACK_START(vbox, viewport1, TRUE, TRUE, 0);

    tientry = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(tientry), TRUE);
    select_all_text_in_textview(GTK_TEXT_VIEW(tientry));

    /* Set the tab width, using pango tab array */
    tab_array = pango_tab_array_new (1, TRUE);
    real_tab_width = text_view_calculate_real_tab_width(GTK_TEXT_VIEW(tientry),
                                                        DEFAULT_TAB_SIZE);
    if (real_tab_width >= 0) {
      pango_tab_array_set_tab (tab_array, 0, PANGO_TAB_LEFT, real_tab_width);
      /* printf("Real tab width: %i\n", real_tab_width);*/
      gtk_text_view_set_tabs (GTK_TEXT_VIEW (tientry),
                              tab_array);
    }
    else {
      fprintf(stderr, "%s: Impossible to set tab width.\n", __func__);
    }

    pango_tab_array_free (tab_array);
    geda_container_add(scrolled_window, tientry);

    GEDA_OBJECT_SET_DATA(ThisDialog, tientry, IDS_TEXT_INPUT);

    gtk_widget_show_all (ThisDialog);

    w_current->tiwindow = ThisDialog;
  }
  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(ThisDialog));
  }

  /* always select the text in the entry */
  tientry = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_TEXT_INPUT);
  select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
  gtk_widget_grab_focus(tientry);
}

/******************* End of Text Input dialog box ***********************/

/** @} End Group Text-Input-Dialog */

/** \defgroup Translate-Dialog Translate Dialog
 *  @{ \memberof Editing-Dialogs
 */

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  This function takes the user action and applies it.
 *  \todo improve error detection / use a spin button?
 */
void x_dialog_translate_response(GtkWidget      *Dialog,
                                 int             response,
                                 GschemToplevel *w_current)
{
  GtkWidget  *textentry;
  const char *string;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    /* void */
    break;

  case GEDA_RESPONSE_ACCEPT:

    textentry = GEDA_OBJECT_GET_DATA(Dialog, IDS_TRANSLATE);
    string    = geda_entry_widget_get_text(textentry);

    if (strlen(string) != 0) {

      GtkToggleButton *zoom_check_butt;
      const GList     *object_list;
            GSList    *scope;
      bool             selected, zoom_extents;
      int x, y;

      scope           = GEDA_OBJECT_GET_DATA(Dialog, "Scope");
      selected        = geda_bulb_group_get_active_index(scope);

      zoom_check_butt = GEDA_OBJECT_GET_DATA(Dialog, "zoom-check-butt");
      zoom_extents    = gtk_toggle_button_get_active(zoom_check_butt);

      if (selected) {
        object_list = geda_list_get_glist (Current_Selection);

      }
      else {
        GedaToplevel *toplevel = w_current->toplevel;
        object_list = geda_struct_page_get_objects (toplevel->page_current);
      }

      /* first zoom extents */
      if (zoom_extents) {
        i_zoom_world_extents (w_current, object_list, I_PAN_DONT_REDRAW);
      }

      if (geda_utility_string_parse_xy(string, &x, &y)) {

        if (y == 0 && !selected) {
          o_complex_translate_all(w_current, x, object_list);
        }
        else {
          o_complex_translate_list(w_current, object_list, x, y);
        }
      }

      if (zoom_extents) {
        i_zoom_world_extents (w_current, object_list, I_PAN_DONT_REDRAW);
      }
    }

    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }

  i_status_set_state(w_current, SELECT);
  gtk_widget_destroy(Dialog);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static inline
GtkWidget *x_dialog_translate_add_scope (GschemToplevel *w_current,
                                         GtkWidget      *Dialog)
{
  GtkWidget *alignment;
  GtkWidget *container;
  GtkWidget *widget;
  GSList    *group;

  container = gtk_hbox_new (FALSE, 0);

  widget = geda_bulb_new_visible_with_mnemonic(NULL, "_All");
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  geda_container_add (container, widget);

  widget = geda_bulb_new_with_mnemonic_from_widget(widget, "Selected", 1);
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  geda_container_add (container, widget);

  group = geda_bulb_get_group(widget);

  GEDA_OBJECT_SET_DATA(Dialog, group, "Scope");

  geda_bulb_group_set_active_index(group, o_select_is_selection(w_current));

  gtk_widget_show(container);

  alignment = gtk_alignment_new(0.5, 0, 0, 1);

  geda_container_add (alignment, container);

  return alignment;
}

/*! \brief Create the translate dialog
 *  \par Function Description
 *  Create the dialog to translate symbols.
 */
void x_dialog_translate (GschemToplevel *w_current)
{
  GtkWidget *ThisDialog = w_current->trwindow;

  if (!ThisDialog) {

    GtkWidget *label;
    GtkWidget *textentry;
    GtkWidget *vbox;
    GtkWidget *sbox;
    GtkWidget *zoom_check_butt;

    const char *entry_tip;
    const char *zoom_tip;

    entry_tip = _("Enter a single digit or a coordinate pair");
    zoom_tip  = _("Automatically zoom to the new extents after translation");

    ThisDialog = gschem_dialog_new_with_buttons(_("Translate"),
                                        w_current->main_window,
                                              GTK_DIALOG_MODAL,
                                      IDS_TRANSLATE, w_current,
                                              GTK_STOCK_CANCEL,
                                           GEDA_RESPONSE_REJECT,
                                                  GTK_STOCK_OK,
                                           GEDA_RESPONSE_ACCEPT,
                                                         NULL);

    /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    sbox = x_dialog_translate_add_scope (w_current, ThisDialog);
    geda_container_add (vbox, sbox);

    HD_SEPARATOR (vbox, Options);

    label = geda_aligned_label_new(_("Offset to translate?\n(0 for origin)"), 0, 0);
    PACK_START(vbox, label, TRUE, TRUE, 0);

    textentry = geda_entry_new_with_max_length (TRANSLATE_DIALOG_MAX_ENTRY);
    geda_entry_widget_set_text( textentry, "0" );
    EntrySelectAll(textentry);
    geda_entry_widget_set_activates_default(textentry, TRUE);
    PACK_START(vbox,textentry, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(textentry, entry_tip);

    HD_SEPARATOR (vbox, Options);

    zoom_check_butt = gtk_check_button_new_with_mnemonic (_("Auto Zoom Extents"));
    gtk_widget_show (zoom_check_butt);

    PACK_START (vbox, zoom_check_butt, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(zoom_check_butt, zoom_tip);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(zoom_check_butt), TRUE);

    GEDA_HOOKUP_OBJECT(ThisDialog, zoom_check_butt, "zoom-check-butt");
    GEDA_HOOKUP_OBJECT(ThisDialog, textentry, IDS_TRANSLATE);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_translate_response),
                      w_current);

    gtk_widget_grab_focus(textentry);
    gtk_widget_show_all (ThisDialog);
    w_current->trwindow = ThisDialog;
  }
  else  { /* dialog already created */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }
}

/***************** End of Translate dialog box ***********************/

/** @} End Group Translate-Dialog */

/** @} endgroup Editing-Dialogs */

/** \defgroup Systemic-Dialogs System Related Dialog
 *  @{
 * \brief This group contains functions for System Level Dialogs
*/

/** \defgroup Hotkeys-Dialog  Hotkeys Dialog
 *  @{ \memberof Systemic-Dialogs
 *     \brief Functions to create and support the Hotkeys Dialog
*/

/*! \brief Response function for the hotkey dialog
 *  \par Function Description
 *  This function destroys the hotkey dialog and does some cleanup.
 */
void x_dialog_hotkeys_response(GtkWidget *Dialog, int response,
                               GschemToplevel *w_current)
{
  GtkToggleButton *butt;
  EdaConfig *cfg;
  int show_bind;

  switch(response) {
  case GEDA_RESPONSE_CLOSE:
  case GEDA_RESPONSE_DELETE_EVENT:
    butt = GEDA_OBJECT_GET_DATA (w_current->hkwindow, WIDGET(ShowBinding));
    show_bind = gtk_toggle_button_get_active(butt);
    cfg = eda_config_get_user_context ();
    eda_config_set_boolean (cfg, IVAR_CONFIG_GROUP, "hotkey-show-bind", show_bind);
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }
  /* clean up */
  gtk_widget_destroy(Dialog);
}

/*! \brief Fix up displaying icons in list of hotkeys.
 * gschem incorporates both GTK's stock and theme icons. Each is identified
 * by a single icon name, which could be either a GTK stock icon or a theme
 * icon.  To determine which icon to show, we first check if there is a
 * matching stock icon, and if one does not exist, we fall back to looking
 * in the theme.
 *
 * The GtkCellRendererPixbuf does not provide this capability.  If its
 * "icon-name" property is set, it does not look at stock items, but if
 * its "stock-id" property is set, it ignores the "icon-name" even if
 * no matching stock item exists.
 *
 * This handler hooks into the "notify::stock-id" signal in order to
 * implement the desired fallback behaviour.
 */
static void
x_dialog_hotkeys_cell_stock_id_notify (GObject    *gobject,
                                       GParamSpec *pspec,
                                       bool       *user_data)
{
  char *stock_id = NULL;

  /* Decide whether the requested stock ID actually matches a stock item */
  g_object_get (gobject, "stock-id", &stock_id, NULL);

  if (stock_id != NULL) {

    GtkStockItem stock_info;

    const char *new_icon_name = NULL;
    const char *new_stock_id  = stock_id;

    if (!gtk_stock_lookup (stock_id, &stock_info) &&
        !gtk_icon_factory_lookup_default(stock_id))
    {
      new_icon_name = stock_id;
      new_stock_id  = NULL;
    }

    /* Fix up the cell renderer, making sure that this function does not
     * get called recursively.*/
    g_signal_handlers_block_by_func (gobject,
                                     x_dialog_hotkeys_cell_stock_id_notify,
                                     NULL);
    g_object_set (gobject,
                  "icon-name", new_icon_name,
                  "stock-id", new_stock_id,
                  NULL);

    g_signal_handlers_unblock_by_func (gobject,
                                       x_dialog_hotkeys_cell_stock_id_notify,
                                       NULL);

    g_free (stock_id);
  }
}

/*! \brief Regenerate attribute list when the visibility
 *         setting  changes and toggle switch image
 *  \par Function Description: This function changes images for
 *       show_inherited switch to the opposite state, i.e. if ON
 *       use OFF image and if OFF use ON image. The function then
 *       calls multiattrib_update to update the attribute list.
 */
static void hotkey_show_binding_toggled (GtkToggleButton *widget,
                                         void            *user_data)
{
  TOGGLE_SWITCH(widget);

  GtkTreeViewColumn *column = user_data;

  int state = gtk_toggle_button_get_active(widget);

  g_object_set (column, "visible", state, NULL);

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static void
hotkey_callback_close_clicked (GtkButton *CloseButt, void *user_data)
{
  gtk_dialog_response (GTK_DIALOG (user_data), GEDA_RESPONSE_CLOSE);
}

#define Sortable GtkTreeSortable

/*! \brief Creates the hotkeys dialog
 *  \par Function Description
 *  This function creates the hotkey dialog and displays the list of
 *  hotkeys in a list tree-view.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_hotkeys (GschemToplevel *w_current)
{
  GtkWidget *ThisDialog = w_current->hkwindow;

  if (!w_current->hkwindow) {

    GtkWidget *vbox,  *scrolled_win;
    GtkListStore      *key_store, *store;
    bool               show_bind;

    EdaConfig *cfg = eda_config_get_user_context ();

    show_bind = eda_config_get_boolean (cfg, IVAR_CONFIG_GROUP,
                                        "hotkey-show-bind", NULL);

    ThisDialog = gschem_dialog_new_empty (_("Hotkeys"),
                                           w_current->main_window,
         /* nonmodal Editing Dialog */     GSCHEM_MODELESS_DIALOG,
                                           IDS_HOTKEYS, w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GEDA_RESPONSE_ACCEPT);

    gtk_widget_set_usize(ThisDialog, 300,300);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    PACK_START (vbox, scrolled_win, TRUE, TRUE, 0);
    gtk_widget_show (scrolled_win);

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    /* the model */
    key_store = g_keys_to_new_list_store ();

    store = gtk_list_store_new (4, G_TYPE_STRING, G_TYPE_STRING,
                                   G_TYPE_STRING, G_TYPE_STRING);
    GtkTreeIter iter;

    if (gtk_tree_model_get_iter_first ((GtkTreeModel*)key_store, &iter)) {

      GtkWidget         *treeview;
      GtkCellRenderer   *renderer;
      GtkTreeViewColumn *column;

      do {
        GtkTreeIter iter2;
        const char *icon_id;
        char *action;
        char *binding;
        char *keys;
        char *ptr;

        gtk_tree_model_get ((GtkTreeModel*)key_store, &iter, 0, &binding, 1, &keys, -1);

        icon_id = i_command_get_action_icon(binding);

        ptr = action = geda_strdup(binding);

        action[0] = action[0] ^ 0x20;

        ptr++;
        while (*ptr) {
          if (*ptr == '-') {
            *ptr++ = ' ';
            *ptr   =  *ptr ^ 0x20;
          }
          ptr++;
        }

        if (icon_id) {
          gtk_list_store_insert_with_values (store, &iter2, -1,
                                             0, icon_id,
                                             1, action,
                                             2, keys,
                                             3, binding,
                                             -1);
        }
        else {
          gtk_list_store_insert_with_values (store, &iter2, -1,
                                             1, action,
                                             2, keys,
                                             3, binding,
                                             -1);
        }
        GEDA_FREE(action);
      } while (gtk_tree_model_iter_next ((GtkTreeModel*)key_store, &iter));

      g_object_unref(key_store);

      gtk_tree_sortable_set_sort_column_id ((Sortable*)store, 1, GTK_SORT_ASCENDING);

      /* the tree view */
      treeview = gtk_tree_view_new_with_model (GTK_TREE_MODEL(store));

      geda_container_add (scrolled_win, treeview);
      gtk_widget_show (treeview);

      /* -------------------- The Columns -------------------- */

      /* The first column contains the action's icon (if one was set) */
      renderer = gtk_cell_renderer_pixbuf_new ();
      column = gtk_tree_view_column_new_with_attributes (_("Icon"),
                                                         renderer,
                                                         "stock-id",
                                                         0, NULL);

      /* Fix things up to show stock icons *and* theme icons.  */
      g_signal_connect (renderer, "notify::stock-id",
                        G_CALLBACK (x_dialog_hotkeys_cell_stock_id_notify),
                        NULL);

      gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

      /* The second column contains the modified action text */
      renderer = gtk_cell_renderer_text_new ();
      column = gtk_tree_view_column_new_with_attributes (_("Action"),
                                                         renderer,
                                                         "text",
                                                         1,
                                                         NULL);

      gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

      /* Third column contains the action's keybinding */
      renderer = gtk_cell_renderer_text_new ();
      column = gtk_tree_view_column_new_with_attributes (_("Keystroke(s)"),
                                                         renderer,
                                                         "text",
                                                         2,
                                                         NULL);

      gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

      /* Forth column contains the action's keybinding */
      renderer = gtk_cell_renderer_text_new ();
      column = gtk_tree_view_column_new_with_attributes (_("Command"),
                                                         renderer,
                                                         "text",
                                                         3,
                                                         NULL);

      gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

      g_object_set (column, "visible", show_bind, NULL);

      g_object_set (treeview,
                    "enable-search", TRUE,
                    "headers-clickable", FALSE,
                    "search-column", 1,
                    NULL);

      g_signal_connect (ThisDialog, "response",
                        G_CALLBACK (x_dialog_hotkeys_response),
                        w_current);

      GtkWidget *ShowBindingSwitch  GEDA_UNUSED;
      GtkWidget *action_area;        /* GtkButtonBox to be removed */
      GtkWidget *action_hbox;        /* Replacement container */
      GtkWidget *alignment;
      GtkWidget *butt_hbox;
      GtkWidget *close_butt;
      GtkWidget *switch_vbox;

      /* Remove Gtk action area from the dialog and don't re-use it */
      action_area = GTK_DIALOG(ThisDialog)->action_area;
      geda_container_remove(vbox, action_area);

      /* Replace the action_area with the new container */
      action_hbox = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_end (GTK_BOX (vbox), action_hbox, FALSE, FALSE, 0);
      gtk_widget_show (action_hbox);

      /* Create and add an option toggle switch */
      switch_vbox = gtk_vbox_new(FALSE, 0);
      PACK_START (action_hbox, switch_vbox, FALSE, FALSE, 0);

      /* Setup the full-name option toggle switch */
      ShowBindingSwitch = NULL;

      /* Create a new Toggle Switch widget */
      EDA_SWITCH (switch_vbox, ShowBinding, 0, show_bind);
      gtk_widget_show_all(switch_vbox); /* set every widget in container visible */

      /* Setup callback for Toggle Switch widget */
      GEDA_CALLBACK_SWITCH (ShowBinding, hotkey_show_binding_toggled, column)

      /* Create and add alignment container to hold the button container */
      alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                            "right-padding", 0,
                                            "left-padding",  50,
                                            "xscale",        1.0,
                                            "yscale",        0.0,
                                            "xalign",        1.0,
                                            "yalign",        0.5,
                                            NULL));

      gtk_widget_show (alignment);
      gtk_box_pack_end (GTK_BOX (action_hbox), alignment, TRUE, TRUE, 0);

      /* Create a Horizontal Box for the button to go into */
      butt_hbox = gtk_hbox_new(FALSE, 0);
      gtk_widget_show (butt_hbox);
      geda_container_add (alignment, butt_hbox);

      /* Create and connect the Close a Button */
      close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);

      g_signal_connect (close_butt, "clicked",
                        G_CALLBACK (hotkey_callback_close_clicked),
                        ThisDialog);
      g_object_set (close_butt, "can-default", TRUE, NULL);

      gtk_widget_show (close_butt);
      gtk_box_pack_end (GTK_BOX (butt_hbox), close_butt, FALSE, FALSE,
                        DIALOG_H_SPACING);

      gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GEDA_RESPONSE_CLOSE);
      gtk_widget_grab_default (close_butt);
    }

    gtk_widget_show(ThisDialog);
    w_current->hkwindow = ThisDialog;
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }
}
#undef Sortable
/******************* End of help/keymapping dialog box ******************/

/** @} endgroup Hotkeys-Dialog */

/** \defgroup Raise-All-Dialogs Raise All Dialogs
 *  @{ \memberof Systemic-Dialogs
 *     \brief Raise all dialogs utility function
*/

/***************** Start of misc helper dialog boxes **************/

/*! \brief Raise All Dialogs
 *  \par Function Description
 *  This is a generic function called by x_event_expose if
 * w_current->raise_dialog_boxes is TRUE. This routine is
 * used to request that any open gschem dialog be raised
 * /brought to the foreground. Since the main window is
 * continuously being sent expose events, this could have
 * a dramatic impact on slower machines.
 */
void x_dialog_raise_all(GschemToplevel *w_current)
{
  if(w_current->sswindow) { /* Snap Size */
    gdk_window_raise(w_current->sswindow->window);
  }
  if(w_current->tswindow) { /* Text size */
    gdk_window_raise(w_current->tswindow->window);
  }
  if(w_current->aawindow) { /* Arc Attrib */
    gdk_window_raise(w_current->aawindow->window);
  }
  if(w_current->clwindow) { /* Color Edit */
    gdk_window_raise(w_current->clwindow->window);
  }
  if(w_current->hpwindow) { /* Hatch Pattern */
    gdk_window_raise(w_current->hpwindow->window);
  }
  if(w_current->ltwindow) { /* Line Type */
    gdk_window_raise(w_current->ltwindow->window);
  }
  if(w_current->ptwindow) { /* Pin Type */
    gdk_window_raise(w_current->ptwindow->window);
  }
  if(w_current->sewindow) { /* Slot Edit */
    gdk_window_raise(w_current->sewindow->window);
  }
  if(w_current->tewindow) { /* Text Edit */
    gdk_window_raise(w_current->tewindow->window);
  }
  if(w_current->ftwindow) { /* Find Text */
    gdk_window_raise(w_current->ftwindow->window);
  }
  if(w_current->htwindow) { /* Hide Text */
    gdk_window_raise(w_current->htwindow->window);
  }
  if(w_current->stwindow) { /* Show Text */
    gdk_window_raise(w_current->stwindow->window);
  }
  if(w_current->tiwindow) {
    gdk_window_raise(w_current->tiwindow->window);
  }
  if(w_current->trwindow) { /* Translate */
    gdk_window_raise(w_current->trwindow->window);
  }
  if(w_current->hkwindow) { /* HotKeys */
    gdk_window_raise(w_current->hkwindow->window);
  }
  if(w_current->cowindow) { /* Coordinates */
    gdk_window_raise(w_current->cowindow->window);
  }
  if(w_current->aewindow) { /* Attribute Edit */
    gdk_window_raise(w_current->aewindow->window);
  }
  if(w_current->cpwindow) { /* Configuration Preferences */
    gdk_window_raise(w_current->cpwindow->window);
  }
  if(w_current->cswindow) { /* component select */
    gdk_window_raise(w_current->cswindow->window);
  }
  if(w_current->mawindow) { /* multi attribute */
    gdk_window_raise(w_current->mawindow->window);
  }
  if(w_current->pswindow) { /* page select */
    gdk_window_raise(w_current->pswindow->window);
  }
  if(w_current->sowindow) { /* Script open */
    gdk_window_raise(w_current->sowindow->window);
  }
}

/** @} endgroup Raise-All-Dialogs */

/** \defgroup Symbol-Changed-Dialog Symbol Changed Dialog
 *  @{ \memberof Systemic-Dialogs
 *     \brief Functions to create and support the Symbol Changed Dialog
*/

/*! \brief Populate the the Symbol Change Dialog
 *  \par Function Description
 *  Called by x_dialog_symbol_changed to add a list of out-dated symbols
 *  to the message area of the Symbol Changed dialog. The function creates
 *  widgets as nessasary to present the listing.
 */
static void xd_add_changed_symbol_list (GschemToplevel   *w_current,
                                        GtkMessageDialog *dialog)
{
  GtkWidget *mess_area;
  GtkWidget *hbox, *vbox, *label;
  GtkWidget *tree_view, *scroll;

  GtkListStore      *list_store;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;

  char  *tmp;
  GList *changed;
  Page  *page;

  g_object_get ((GObject*)dialog, "message-area", &mess_area, NULL);

  /* This box contains the warning image and the vbox */
  hbox = g_object_new (GTK_TYPE_HBOX,
                       /* GtkContainer */
                       "border-width", 5,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing", 12,
                       NULL);

  PACK_START (mess_area, hbox, TRUE, TRUE, 0);

  /* This box contains the labels and list of changed symbols */
  vbox = g_object_new (GTK_TYPE_VBOX,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing", 12,
                       NULL);

  PACK_START (hbox, vbox, FALSE, FALSE, 0);

    /* Primary label */
  tmp = geda_strconcat ("<big><b>", _("Major symbol changes detected."),
                         "</b></big>", NULL);

  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        "yalign", 0.0,
                        "selectable", TRUE,
                        /* GtkLabel */
                        "wrap", TRUE,
                        "use-markup", TRUE,
                        "label", tmp,
                        NULL);

  geda_container_add (vbox, label);
  GEDA_FREE (tmp);

  /* Secondary label */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        "yalign", 0.0,
                        "selectable", TRUE,
                        /* GtkLabel */
                        "wrap", TRUE,
                        "use-markup", TRUE,
                        "label",
                        _("Changes have occurred to the symbols shown below.\n\n"
                        "Be sure to verify each of these symbols."),
                        NULL);


  geda_container_add (vbox, label);

  /* List of changed symbols */
  scroll = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                         /* GtkScrolledWindow */
                         "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                         "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                         "shadow-type",       GTK_SHADOW_IN,
                         NULL);

  PACK_START (vbox, scroll, TRUE, TRUE, 0);

  list_store = gtk_list_store_new (1, G_TYPE_STRING);
  page       = gschem_toplevel_get_current_page (w_current);
  changed    = geda_page_get_changed_refdes (page);

  while (changed) {

    char *value = (char *) changed->data;
    GtkTreeIter iter;

    gtk_list_store_append (list_store, &iter);
    gtk_list_store_set (list_store, &iter, 0, value, -1);

    NEXT(changed);
  }

  tree_view = g_object_new (GTK_TYPE_TREE_VIEW,
                            "enable-search", FALSE,
                            "headers-visible", FALSE,
                            "model", list_store,
                            NULL);

  geda_container_add (scroll, tree_view);

  renderer = gtk_cell_renderer_text_new ();

  column = gtk_tree_view_column_new_with_attributes (_("Symbol"),
                                                       renderer,
                                                       "text", 0,
                                                       NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

  gtk_widget_show_all (mess_area);
}

/*! \brief Annoyance Dialog
 *  \par Function Description
 *  Called when a symbol in a drawing being loaded is an older version
 *  than the same symbol in the library, based on the symversion in the
 *  symbol definition, to creates a message dialog notifying the user of
 *  such. This function only sets up the basic dialog, see the preceding
 *  function xd_add_changed_symbol_list.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 */
void x_dialog_symbol_changed(GschemToplevel *w_current)
{
  Page *page_current;

  page_current = gschem_toplevel_get_current_page (w_current);

  if (page_current) {

    if (geda_page_get_changed_refdes(page_current)) {

      GtkWidget *dialog;
      GtkWidget *close_butt;

      dialog = gtk_message_dialog_new (w_current->main_window,
                                       GTK_DIALOG_DESTROY_WITH_PARENT,
                                       GEDA_MESSAGE_INFO,
                                       GTK_BUTTONS_NONE,
                                       NULL);

      xd_add_changed_symbol_list (w_current, GTK_MESSAGE_DIALOG(dialog));

      gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);

      gtk_window_set_transient_for (GTK_WINDOW(dialog), w_current->main_window);

      /* Add the Close button to dialog action area */
      close_butt = gtk_button_new_from_stock ("gtk-close");
      gtk_widget_show (close_butt);
      gtk_dialog_add_action_widget (GTK_DIALOG (dialog), close_butt, GEDA_RESPONSE_CLOSE);
      gtk_widget_set_can_default(close_butt, TRUE);
      gtk_widget_set_tooltip_text (close_butt, _("Dismiss this dialog"));

      gtk_dialog_set_default_response(GTK_DIALOG(dialog), GEDA_RESPONSE_CLOSE);
      gtk_widget_grab_focus(close_butt);
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy (dialog);
    }
  }
  else {
    u_log_message("Null reference to Current Page\n");
  }
}

/****************** End of major symbol changed dialog box **************/

/** @} endgroup Symbol-Changed-Dialog */


/** \defgroup Invalid-Dialog Invalid attribute Dialog
 *  @{ \memberof Systemic-Dialogs
 *     \brief Creates the Invalid attribute Dialog
*/

/*! \brief Validate the input attribute
 *  \par Function Description
 *  This function validates the attribute and if it is not valid
 *  pops up an error message box. The function is used by both
 *  the single and the multi-attribute editors to validate text
 *  input.
 *
 *  \param parent The parent window which spawned this dialog box.
 *  \param attribute The attribute to be validated.
 *  \returns TRUE if the attribute is valid, FALSE otherwise.
 */
int x_dialog_validate_attribute(GtkWindow *parent, char *attribute)
{
  /* validate the new attribute */
  if (!geda_attrib_string_get_name_value (attribute, NULL, NULL)) {

    GtkWidget *message_box;

    const char *msg1 = _("invalid attribute value");
    const char *msg2 = _("Please correct in order to continue");
    const char *msg3 = _("The name and value must be non-empty.\nThe name cannot end with a space.\nThe value cannot start with a space");

    message_box = gtk_message_dialog_new_with_markup (parent,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_CLOSE,
                                  "<span weight=\"bold\" size=\"larger\">%s \"%s\" \n%s</span>\n\n%s.", msg1, attribute, msg2, msg3);

     gtk_window_set_title(GTK_WINDOW(message_box), _("Invalid Attribute"));
     gtk_dialog_run (GTK_DIALOG (message_box));
     gtk_widget_destroy (message_box);
     return FALSE;
  }
  return TRUE;
}

/** @} endgroup Invalid-Dialog */

/************** End of misc support functions for dialog boxes **********/

/** @} endgroup Systemic-Dialogs */

/** \defgroup Gschem-General-Dialogs General Dialogs
 *  @{ \par This group contains General Utility Dialogs
*/

/** \defgroup Confirmation-Dialog Confirmation Dialogs
 *  @{ \memberof Gschem-General-Dialogs
*/

/*! \brief General Purpose Confirmation Dialog
 *  \todo: derive this from gschem dialog class
 */
int x_dialog_confirmation (const char *msg, IDE_MESSAGE_TYPE context, bool thread)
{
  GtkWidget *dialog;
  int response;

  if (thread) {
    gschem_threads_enter();
  }

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   context,
                                   GTK_BUTTONS_NONE,
                                   "%s", msg);

  /* add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          GTK_STOCK_NO,         GEDA_RESPONSE_NO,
                          GTK_STOCK_YES,        GEDA_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_YES,
                                          GEDA_RESPONSE_NO,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GEDA_RESPONSE_YES);

  gtk_window_set_title(GTK_WINDOW(dialog), _(IDS_MESSEAGE_TITLES[GEDA_MESSAGE_QUESTON]));

  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (thread) {
    gschem_threads_leave();
  }
  return response;
}

/*! \brief General Purpose Confirmation Dialog with Cancel Option
 *  \remarks TODO: derive this from gschem dialog class
 */
int x_dialog_confirm_with_cancel (const char *msg, IDE_MESSAGE_TYPE context, bool thread)
{
  GtkWidget *dialog;
  int response;

  if (thread) {
    gschem_threads_enter();
  }

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   context,
                                   GTK_BUTTONS_NONE,
                                   "%s", msg);

  /* add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          GTK_STOCK_NO,         GEDA_RESPONSE_NO,
                          GTK_STOCK_CANCEL,     GEDA_RESPONSE_CANCEL,
                          GTK_STOCK_YES,        GEDA_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_YES,
                                          GEDA_RESPONSE_NO,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GEDA_RESPONSE_YES);

  gtk_window_set_title(GTK_WINDOW(dialog), _(IDS_MESSEAGE_TITLES[GEDA_MESSAGE_QUESTON]));

  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (thread) {
    gschem_threads_leave();
  }
  return response;
}

/****************** End of General confirm dialog box ********************/

/** @} endgroup Confirmation-Dialog */

/** \defgroup File-Select-File-Dialog Select File Dialog
 *  @{ \memberof Gschem-General-Dialogs
*/

/*! \brief  Multi-Purpose File Dialog
 *  \par Function Description
 *  The function creates a generic file open or save files.
 *
 *  \note \a w_current can be NULL, theGschemToplevel is used
 *        to store and retrive the last path member.
 *
 *  \warning
 *   Caller must GEDA_FREE returned character string.
 */
char *x_dialog_select_file (GschemToplevel *w_current,
                            const char     *msg,
                            const char     *templ,
                            int             flags)
{
  GtkWidget *dialog;
  char      *title;
  char      *result;
  char      *path;

  result  = NULL;

  /* Default to load if not specified.  Maybe this should cause an error. */
  if (! (flags & (FSB_LOAD | FSB_SAVE))) {
    flags = flags | FSB_LOAD;
  }

  if (flags & FSB_LOAD) {
    title = geda_sprintf("%s: Open", msg);
    dialog = geda_file_chooser_dialog_new_full (_(title),
                                          NULL,
                                          FILE_CHOOSER_ACTION_OPEN,
                                          GTK_STOCK_CANCEL, GEDA_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GEDA_RESPONSE_OK,
                                          NULL);
    /* Since this is a load dialog box, the file must exist! */
    flags = flags | FSB_MUST_EXIST;

  }
  else {
    title = geda_sprintf("%s: Save", msg);
    dialog = geda_file_chooser_dialog_new_full (_(title),
                                          NULL,
                                          FILE_CHOOSER_ACTION_SAVE,
                                          GTK_STOCK_CANCEL, GEDA_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GEDA_RESPONSE_OK,
                                          NULL);
  }

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_OK,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GEDA_RESPONSE_OK);

  path = gschem_toplevel_get_last_image_path (w_current);

  /* Pick the current default folder to look for files in */
  if (path && *path) {
    geda_file_chooser_set_current_folder (dialog, path);
  }
  else {
    char *cwd = g_get_current_dir();
    geda_file_chooser_set_current_folder (dialog, cwd);
    GEDA_FREE (cwd);
  }

  /* Pick the current template (*.rc) or default file name */
  if (templ && *templ) {
    if (flags & FSB_SAVE)  {
      geda_file_chooser_set_current_name (dialog, templ);
    }
    else {
      gtk_file_chooser_select_filename (GTK_FILE_CHOOSER (dialog), templ);
    }
  }

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GEDA_RESPONSE_OK) {

    result = geda_file_chooser_get_filename (dialog);

    if (result !=NULL) {
      char *file_path = geda_get_dirname(result);
      gschem_toplevel_set_last_image_path(w_current, file_path);
    }
  }

  gtk_widget_destroy (dialog);

  GEDA_FREE (title);

  return result;

}

/***************** End of General file select dialog box ****************/

/** @} endgroup File-Select-File-Dialog */

/** \defgroup Message-Dialogs Message Dialogs
 *  @{ \memberof Gschem-General-Dialogs
*/

/*! \brief General Purpose Message Dialog
 *  \remarks See Utility Macros defined in globals.h
 *  \remarks This dialog is not for messages with Pango markups,
 *           see x_dialog_message_with_markup.
 */
void x_dialog_show_message (const char *msg, IDE_MESSAGE_TYPE context, const char *title)
{
  if (msg) {

      GtkWidget *dialog;

      dialog = gtk_message_dialog_new (NULL,
                                       GTK_DIALOG_MODAL |
                                       GTK_DIALOG_DESTROY_WITH_PARENT,
                                       context,
                                       GTK_BUTTONS_OK,
                                       "%s", msg);

    if(title)
      gtk_window_set_title(GTK_WINDOW(dialog), _(title));
    else
      gtk_window_set_title(GTK_WINDOW(dialog), _(IDS_MESSEAGE_TITLES[context]));

    gtk_dialog_run (GTK_DIALOG (dialog));

    gtk_widget_destroy (dialog);
  }
}

/*! \brief General Purpose Pango Message Dialog
 *  \remarks See Utility Macros defined in globals.h
 */
void x_dialog_message_with_markup (const char *msg1, const char *msg2,
                                   IDE_MESSAGE_TYPE context, const char *title)
{
  GtkWidget *dialog;

  bool msg_1_has_markup = FALSE;
  bool msg_2_has_markup = FALSE;

  if (strstr(msg1, "</") && !(strstr(msg1, "</ho")))
    msg_1_has_markup = TRUE;

  if (strstr(msg2, "</") && !(strstr(msg2, "</ho")))
    msg_2_has_markup = TRUE;

  dialog = g_object_new (GTK_TYPE_MESSAGE_DIALOG,
                         "border-width",         DIALOG_BORDER_WIDTH,
                         "message-type",         context,
                         "text",                 msg1,
                         "use-markup",           msg_1_has_markup,
                         "secondary-text",       msg2,
                         "secondary-use-markup", msg_2_has_markup,
                         "buttons",              GTK_BUTTONS_OK,
                         NULL);
  if (title)
    gtk_window_set_title(GTK_WINDOW(dialog), _(title));
  else
    gtk_window_set_title(GTK_WINDOW(dialog), _(IDS_MESSEAGE_TITLES[context]));

  if (!gschem_threads_is_locked()) {
    gschem_threads_enter();
    gtk_dialog_run (GTK_DIALOG (dialog));
    gschem_threads_leave();
  }
  else {
    gtk_dialog_run (GTK_DIALOG (dialog));
  }

  gtk_widget_destroy (dialog);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
GList *x_dialog_sym_not_embedded(GschemToplevel *w_current, GList *syms)
{
  GList     *items;
  GList     *iter;

  items = NULL;

  if (syms && (g_list_length(syms) > 1)) {

    for (iter = syms; iter; iter = iter->next) {

      GedaObject *object = (GedaObject*)iter->data;

      if (object != NULL) {

        const char *filename;

        filename = NULL;

        if (object->type == OBJ_COMPLEX) {
          filename = geda_complex_object_get_filename(object);
        }
        else if (object->type == OBJ_PICTURE) {
          filename = geda_picture_object_get_filename(object);
        }

        if (filename) {

          GedaConfirmItem *item;

          item = geda_confirm_item_new (filename, object);

          if (item) {
            items = g_list_append(items, item);
          }
        }
      }
    }

    if (items) {

      GtkWidget *dialog;

      dialog = geda_confirm_dialog_items_new(items);

      gtk_dialog_run (GTK_DIALOG (dialog));

      gtk_widget_destroy (dialog);
    }
  }

  return NULL;
}

/******************* End of General message dialogs **********************/

/** @} endgroup Message-Dialogs */

/** @} endgroup Gschem-General-Dialogs */
