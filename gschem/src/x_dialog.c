/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
/*! \todo STILL NEED to clean up line lengths in aa and tr */
#include <config.h>
#include <version.h>
#include <missing.h>
#include <ascii.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#include <ctype.h>
#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include <geda_dialog_controls.h>
#include "x_dialog.h"
#include "widgets.h"

const char* IDS_MESSEAGE_TITLES[] = {
  "Information", "Warning", "Confirmation", "Error", "gschem", /* Message Title Strings*/
  NULL
};

static GtkWidget* create_menu_linetype (GSCHEM_TOPLEVEL *w_current);
static int x_dialog_edit_line_type_change (GtkWidget *w, line_type_data *ld);
static void x_dialog_edit_line_type_ok (GtkWidget *w, line_type_data *ld);

static GtkWidget* create_menu_filltype (GSCHEM_TOPLEVEL *w_current);
static int x_dialog_edit_fill_type_change(GtkWidget *w, fill_type_data *fd);
static void x_dialog_edit_fill_type_ok(GtkWidget *w, fill_type_data *fd);

/* string buffer used by dialogs: show_text, find_text and hide_text */
char General_textstring[256] = "refdes=R";

/*! \defgroup Dialog-Utilities
 *  @{ \par This Group contains utility functions used by various dialogs
*/

/*! \brief Create AtkObject widget and Link Label with Widget
 *  \par Function Description
 *  This function obtains a new accesible object associate with linkto
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
    g_object_unref (relation_set);

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
    g_object_unref (relation_set);
  }
  else
    atk_obj = NULL;

  return atk_obj;
}

/*! \brief Create pixmap widget for dialogs boxes.
 *  \par Function Description
 *  This is an internally used function to create pixmaps.
 *  The default bitmap directory is prefixed to the filename
 *  and if is valid then the image widget is created and returned.
 */

GtkWidget* create_pixmap (const char *filename)
{
  char *pathname = NULL;
  GtkWidget *pixmap;

  if (!filename || !filename[0]) {
      s_log_message("Bad image file name.\n");
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE ,
                                      GTK_ICON_SIZE_INVALID);
  }

  pathname = g_build_filename (s_path_sys_data (), "bitmap", filename, NULL);

  if (!pathname)
    {
      s_log_message("Could not find image at file: %s.\n", filename);
      return gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE,
                                      GTK_ICON_SIZE_INVALID);
    }

  pixmap = gtk_image_new_from_file (pathname);
  g_free (pathname);
  return pixmap;
}

/*! \brief Distroy Window Function
 *  \par Function Description
 *
 */
void destroy_window(GtkWidget *widget, GtkWidget **window)
{
  *window = NULL;
}

GtkWidget* get_geda_switch_image (bool WhichState)
{
   GtkWidget* image;

   if (WhichState)
     image = create_pixmap (GEDA_BITMAP_SWITCH_ON);
   else
     image = create_pixmap (GEDA_BITMAP_SWITCH_OFF);

   return image;
}

/*! \brief Function to create a GTK switch image control / widget.
 *  \par Function Description
 *  This function creates a Check Box widget using an image, the Check
 *  Box indicator is disabled so only the images is displayed. This creates
 *  a control similar to a GTK3 Switch, using a standard GTK2 widget. The
 *  On or Off images is controlled by the istate variable.
 *
 *  Returns: Newly created widget
 */
GtkWidget*
create_geda_switch(GtkWidget *Dialog, GtkWidget *parent, GtkWidget *widget,
                   GtkWidget *SwitchImage, bool istate)
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

/*! \brief Selects all text in a TextView widget
 *  \par Function Description
 *  The function selects all the text in a TextView widget.
 *
 */
void select_all_text_in_textview(GtkTextView *textview)
{
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  if (GTK_IS_TEXT_VIEW(textview)) {
    textbuffer = gtk_text_view_get_buffer(textview);
    gtk_text_buffer_get_bounds (textbuffer, &start, &end);
    gtk_text_buffer_select_range(textbuffer, &start, &end);
  }
  else
    g_warning("select_all_text: parameter is not a textview widget\n");
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int text_view_calculate_real_tab_width(GtkTextView *textview, int tab_size)
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
  g_free (tab_string);

  if (layout != NULL) {
    pango_layout_get_pixel_size (layout, &tab_width, NULL);
    g_object_unref (G_OBJECT (layout));
  } else
  tab_width = -1;

  return tab_width;

}

/*! @} endgroup Dialog-Utilities */

/*! \defgroup Standard-Dialogs
 *  @{ \par This Group contains Functions for Standard Dialogs
*/

/*!***** \section Help-About-Dialog ( Help About Standard-Dialogs ) *****/

static void dialog_link_cb(GtkAboutDialog *dialog, const char *link, gpointer data)
{
   x_show_uri(link);
}
#include <gnu/libc-version.h>
/*! \brief Create the about dialog and show it
 *  \par Function Description
 *  This function creates the about dialog.
 */
void about_dialog (GSCHEM_TOPLEVEL *w_current)
{
  char *version_string;
  char *logo_file;
  GdkPixbuf *logo;
  char *comments;
  char *copyright;
  GError *error = NULL;
  GtkWidget *Dialog;

  version_string = g_strdup_printf (_("%s (g%.7s)"),
                                    PACKAGE_DOTTED_VERSION,
                                    PACKAGE_GIT_COMMIT);

  logo_file = g_strconcat (w_current->toplevel->bitmap_directory,
                           G_DIR_SEPARATOR_S, "gschem_about_logo.png", NULL);

  logo = gdk_pixbuf_new_from_file (logo_file, &error);
  g_free (logo_file);

  if (error != NULL) {
    if (logo == NULL){
      s_log_message ("Could not load image at file: %s\n%s\n",
                   logo_file, error->message);
      g_error_free (error);
    }
  }

  char *gEDA_str = _("gEDA: GPL Electronic Design Automation\nglibc ");

  comments  = g_strconcat (gEDA_str, gnu_get_libc_version(), NULL);

  copyright = _("Copyright © 1998-2013 Ales Hvezda"
                " <ahvezda@geda.seul.org>\n"
                "Copyright © 1998-2013 gEDA Contributors"
                " (see ChangeLog for details)");

  Dialog = gtk_about_dialog_new ();

  g_object_set (GTK_OBJECT(Dialog), "version",    version_string,
                                    "logo",       logo,
                                    "title",      _("About gschem"),
                                    "comments",   comments,
                                    "copyright",  copyright,
                                    "website",    "http://geda-project.org/",
                                       NULL);     /* End marker */

  /* About dialog URI calls maybe broken on Windows */
#if (GTK_MAJOR_VERSION == 2 && GTK_MINOR_VERSION < 24)
  /* deprecated since GTK 2.24 */
  gtk_about_dialog_set_url_hook(dialog_link_cb, NULL, NULL);
 #else
  g_signal_connect(GTK_ABOUT_DIALOG(Dialog), "activate-link", G_CALLBACK(dialog_link_cb), NULL);
#endif

  gtk_dialog_run(GTK_DIALOG(Dialog));
  gtk_widget_destroy(Dialog);

  g_free (version_string);
  g_free (comments);
  if (logo) g_object_unref (logo);

}

/*!****************** End of help/about dialog box **********************/

/*!***** \section Snap-Size-Dialog ( Snap Size Standard-Dialogs ) ******/

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
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    snap_size = g_object_get_data(G_OBJECT(Dialog), IDS_SNAP_SIZE);
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(snap_size));

    w_current->snap_size = size;
    i_update_grid_info (w_current);
    o_invalidate_all (w_current);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    break;
  default:
    printf("snap_size_dialog_response(): strange signal %d\n",response);
  }

  /* clean up */
  i_set_state(w_current, SELECT);
  gtk_widget_destroy(Dialog);
  w_current->sswindow = NULL;
}

/*! \brief Create the snap size dialog
 *  \par Function Description
 *  This function creates the snap size dialog.
 */
void snap_size_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *snap_size;
  GtkWidget *Dialog;

  DECLARE_TOOPTIPS

  Dialog = w_current->sswindow;
  if (!Dialog) {
    Dialog = gschem_dialog_new_with_buttons(_("Snap Size"),
                                            GTK_WINDOW(w_current->main_window),
                                            GTK_DIALOG_MODAL,
                                            IDS_SNAP_SIZE,
                                            w_current,
                                            GTK_STOCK_CANCEL,
                                            GTK_RESPONSE_REJECT,
                                            GTK_STOCK_OK,
                                            GTK_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(Dialog), GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (Dialog), "response",
                      G_CALLBACK (snap_size_dialog_response),
                      NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(Dialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(Dialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(Dialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter new snap grid spacing:"));
    gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    snap_size = gtk_spin_button_new_with_range(MIN_SNAP_SIZE,MAX_SNAP_SIZE,5);
    gtk_editable_select_region( GTK_EDITABLE(snap_size), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), snap_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(snap_size), TRUE);
    gtk_widget_grab_focus(snap_size);

    gtk_widget_set_tooltip_text(snap_size,
            _("Sets the default spacing\n which objects snaps to."));

    GSCHEM_HOOKUP_OBJECT(Dialog, snap_size, IDS_SNAP_SIZE);

    w_current->sswindow = Dialog;

    gtk_widget_show_all(Dialog);
  }

  else {  /* dialog already there */
    gtk_window_present(GTK_WINDOW(Dialog));
  }

  /* always set the current gschem value to the dialog entry */
  snap_size = g_object_get_data(G_OBJECT(Dialog), IDS_SNAP_SIZE);
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(snap_size), w_current->snap_size);
  gtk_editable_select_region(GTK_EDITABLE(snap_size), 0, -1);
}

/*!******************* End of Snap size dialog box **********************/

/*!****** \section Text-Size-Dialog ( Text Size Standard-Dialogs ) *****/

/*! \brief response function for the text size dialog
 *  \par Function Description
 *  This function takes the user input and applies it to the text object
 */
static void
text_size_dialog_response(GtkWidget *Dialog, int response, void* data)
{
  GtkWidget *text_size;
  int size;
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    text_size = g_object_get_data(G_OBJECT(Dialog), IDS_TEXT_SIZE);
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(text_size));

    w_current->text_size = size;
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("text_size_dialog_response(): strange signal %d\n",response);
  }

  /* clean up */
  i_set_state(w_current, SELECT);
  gtk_widget_destroy(Dialog);
  w_current->tswindow = NULL;
}

/*! \brief Create the text size dialog
 *  \par Function Description
 *  This function creates the text size dialog.
 */
void text_size_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *text_size;
  GtkWidget *Dialog;
  DECLARE_TOOPTIPS

  Dialog = w_current->tswindow;
  if (!Dialog ) {
    Dialog = gschem_dialog_new_with_buttons(_("Text Size"),
                                            GTK_WINDOW(w_current->main_window),
                                            GTK_DIALOG_MODAL,
                                            IDS_TEXT_SIZE, w_current,
                                            GTK_STOCK_CANCEL,
                                            GTK_RESPONSE_REJECT,
                                            GTK_STOCK_OK,
                                            GTK_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(Dialog), GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (Dialog), "response",
                      G_CALLBACK (text_size_dialog_response),
                      NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(Dialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(Dialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(Dialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter new text size:"));
    gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    text_size = gtk_spin_button_new_with_range(MIN_TEXT_SIZE,
                                               MAX_TEXT_SIZE,2);
    gtk_editable_select_region( GTK_EDITABLE(text_size), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), text_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(text_size), TRUE);
    gtk_widget_grab_focus(text_size);

    gtk_widget_set_tooltip_text(text_size, _("Sets the default text font size."));

    GSCHEM_HOOKUP_OBJECT(Dialog, text_size, IDS_TEXT_SIZE);
    w_current->tswindow = Dialog;
    gtk_widget_show_all(Dialog);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }

  /* always set the current text size to the dialog */
  text_size = g_object_get_data(G_OBJECT(Dialog),IDS_TEXT_SIZE);
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(text_size), w_current->text_size);
  gtk_editable_select_region(GTK_EDITABLE(text_size), 0, -1);
}

/*!******************** End of Text size dialog box *********************/

/*! @} endgroup Standard-Dialogs */

/*! \defgroup Editing-Dialogs ( X-Dialogs )
 *  @{
 *  \par This Group contains Dialogs for Editing Objects
 *  \details
 *  This section contains a collection of Mode-less Dialogs derived
 * from type GschemDialog and use the PROP_SELECTION_TRACKER property.
 * These dialog register a callback function to be called when the
 * selection is changed so that the contents of the Dialog can be
 * updated based on the current selection. This allow the Dialogs to
 * stay open while other editing is performed.
 */

/*!******** \section Arc-Edit-Dialog ( Edit Arc Editing-Dialogs ) *******/

/*! \brief Handle selection change event for x_dialog_edit_arc_angle
 *  \par Function Description
 *  Updates the combobox when the selection changes.
 *
 *  \param w_current pointer to GSCHEM_TOPLEVEL context
 *  \param object    pointer to a selected OBJECT.
 */
static void
x_dialog_edit_arc_angle_selection (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  GtkWidget *radius, *spin_start, *spin_sweep;

  GtkWidget *Dialog = w_current->aawindow;

  radius     = g_object_get_data(G_OBJECT(Dialog), "radius");
  spin_start = g_object_get_data(G_OBJECT(Dialog), "spin_start");
  spin_sweep = g_object_get_data(G_OBJECT(Dialog), "spin_sweep");

  if (object == NULL) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(radius), w_current->distance);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep), 90);
  }
  else {
    if (object->type == OBJ_ARC) {
      gtk_spin_button_set_value(GTK_SPIN_BUTTON(radius),
                                object->arc->width / 2);
      gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),
                                object->arc->start_angle);
      gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep),
                                object->arc->end_angle);

      gtk_widget_grab_focus(radius);
    }
  }
}

/*! \brief Apply changes to selected objects
 *  \par Function Description
 *  This function applies the changes to the currently selected objects.
 */
static void
x_dialog_edit_arc_angle_apply(GtkWidget *Dialog, GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *spinentry;
  int radius, start_angle, sweep_angle;
  GList  *s_current = NULL;
  OBJECT *object = NULL;

  s_current = geda_list_get_glist( Current_Selection );

  /* Get ptr to the spinner widgets */
  spinentry = g_object_get_data(G_OBJECT(Dialog),"radius");
  radius = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
  spinentry = g_object_get_data(G_OBJECT(Dialog),"spin_start");
  start_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
  spinentry = g_object_get_data(G_OBJECT(Dialog),"spin_sweep");
  sweep_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));

  if(s_current != NULL) {
    while(s_current != NULL) {

      object = (OBJECT *) s_current->data;
      if (object == NULL) {
        fprintf(stderr, _("ERROR: NULL object in x_dialog_edit_arc_angle_apply!\n"));
      }
      else {
        if(object->type == OBJ_ARC) {
          o_arc_modify(w_current->toplevel, object,
                       radius, 0, ARC_RADIUS);
          o_arc_modify(w_current->toplevel, object,
                       start_angle, 0, ARC_START_ANGLE);
          o_arc_modify(w_current->toplevel, object,
                       sweep_angle, 0, ARC_END_ANGLE);
        }
      }
      s_current = g_list_next(s_current);
    }
  }
  else {
    o_arc_end4(w_current, radius, start_angle, sweep_angle);
  }
  w_current->toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);

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
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(Dialog);
    w_current->aawindow = NULL;
    break;
  case GTK_RESPONSE_ACCEPT:
    x_dialog_edit_arc_angle_apply(Dialog, w_current);
    break;
  default:
    printf("x_dialog_edit_arc_angle_response(): strange signal %d\n",response);
  }
}

/*! \brief Creates the arc angle dialog
 *  \par Function Description
 *  This function creates the arc angle dialog. This dialog is also used
 *  both for creating and editing ARC's. Depending on the \a arc_object the
 *  entries are filled with the arc OBJECT properties or with some standard
 *  values.
 *
 *  \param [in] w_current   The GSCHEM_TOPLEVEL object
 *  \param [in] arc_object  an arc OBJECT if used to modify an arc
 *                          or NULL to create a new arc.
 *
 * TODO: When multi-selection and modless is applied then only change
 *       the changed value, maybe add check box next to each
 */
void x_dialog_edit_arc_angle (GSCHEM_TOPLEVEL *w_current, OBJECT *arc_object)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *alignment, *table;
  GtkWidget *radius, *spin_start, *spin_sweep;
  DECLARE_TOOPTIPS

  GtkWidget *Dialog = w_current->aawindow;

  if (!Dialog) {
    Dialog = gschem_dialog_new_with_buttons(_("Arc Params"),
                                            GTK_WINDOW(w_current->main_window),
                                            GSCHEM_MODELESS_DIALOG,
                                            IDS_ARC_ANGLE, w_current,
                                            GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                            GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(Dialog),
                        GTK_WIN_POS_MOUSE);



    gtk_dialog_set_default_response(GTK_DIALOG(Dialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(Dialog), DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(Dialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              0 /*DIALOG_INDENTATION */, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

    table = gtk_table_new (2, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add(GTK_CONTAINER(alignment), table);

    label = gtk_label_new (_("Arc Radius:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

    radius = gtk_spin_button_new_with_range(1, 100000, 100);
    gtk_entry_set_activates_default(GTK_ENTRY(radius), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), radius, 1,2,0,1);
    gtk_widget_set_tooltip_text(radius,  _("Sets the radius of the Arc."));

    label = gtk_label_new (_("Start Angle:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

    spin_start = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_start), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_start, 1,2,1,2);
    gtk_widget_set_tooltip_text(spin_start,  _("Sets the rotation of the arc."));

    label = gtk_label_new(_("Degrees of Sweep:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

    spin_sweep = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_sweep), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_sweep, 1,2,2,3);
    gtk_widget_set_tooltip_text(spin_sweep,  _("Sets the central angle of the arc."));

    GSCHEM_HOOKUP_OBJECT(Dialog, radius,    "radius");
    GSCHEM_HOOKUP_OBJECT(Dialog, spin_start,"spin_start");
    GSCHEM_HOOKUP_OBJECT(Dialog, spin_sweep,"spin_sweep");

    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  GTK_WINDOW(w_current->main_window));

    g_signal_connect (G_OBJECT (Dialog), "response",
                      G_CALLBACK ( x_dialog_edit_arc_angle_response),
                      NULL);

    g_object_set (G_OBJECT (Dialog), DIALOG_DATA_SELECTION,
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

/*!*** \section Common-Edit-Dialog ( Common Routines Editing-Dialogs ) **/

const char* IDS_COLOR_STRINGS[] = {
  "Background", "Pin", "Net endpoint", "Graphic", "Net", "Attribute",
  "Logic bubble", "Grid point", "Detached attribute", "Text", "Bus",
  "Selection", "Bounding box", "Zoom box", "Stroke", "Lock",
  "Output background", "Net junction", "Mesh grid major",
  "Mesh grid minor", "Unknown",
  NULL
};

/*! \brief Cell layout data function for color combobox.
 *  \par Function Description
 *  Cell layout data function to support color swatches in the color
 *  combobox.
 *
 *  \param layout
 *  \param cell
 *  \param model
 *  \param iter
 *  \param data the current #GSCHEM_TOPLEVEL pointer.
 */
static void
color_menu_swatch_layout_data (GtkCellLayout *layout,
                               GtkCellRenderer *cell,
                               GtkTreeModel *model,
                               GtkTreeIter *iter,
                               void* data)
{
  /* GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) data; */
  GValue v = {0, };
  int index;

  /* Get the index of the color on this row */
  gtk_tree_model_get_value (model, iter, 1, &v);
  index = g_value_get_int (&v);

  /* Set the cell's background color */
  g_object_set (cell, "background-gdk", x_get_color (index), NULL);
}

/*! \brief Create a ComboBox with the gschem colors.
 *  \par Function Description
 *  Creates a #GtkComboBox with the color list and swatches showing
 *  each of the available colors.
 *
 *  The backing #GtkTreeModel is a #GtkListStore with two columns, the
 *  first holding the user-friendly name of the color, and the other
 *  the color map index.
 *
 *  \param [in] w_current    The current gschem context.
 */
GtkWidget *create_color_menu (GSCHEM_TOPLEVEL *w_current, int color_index)
{
  GtkListStore    *store;
  GtkComboBox     *cbox;
  GtkCellLayout   *layout;
  GtkCellRenderer *text_cell;
  GtkCellRenderer *color_cell;

  int i;
  const char *str;

  GtkTreeIter iter;

  /* The columns are: name of color, index of color. */
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
  cbox = GTK_COMBO_BOX (gtk_combo_box_new_with_model (GTK_TREE_MODEL (store)));
  layout = GTK_CELL_LAYOUT (cbox); /* For convenience */

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
    if (!x_color_display_enabled(i)) continue;
    str = _(IDS_COLOR_STRINGS[i]);
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, 0, str, 1, i, -1);
    if (i == color_index)
      gtk_combo_box_set_active_iter (cbox, &iter);
  }

  return GTK_WIDGET (cbox);
}

/*!****** \section Fill-Type-Dialog ( Fill Type Editing-Dialogs ) *******/

/*! \brief Create a menu with fill types for the line type dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different fill types.
 */
static GtkWidget *create_menu_filltype (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GSList *group;
  struct fill_type {
    char *str;
    OBJECT_FILLING type;
  } types[] = { { N_("Hollow"), FILLING_HOLLOW },
                { N_("Filled"), FILLING_FILL },
                { N_("Mesh"),   FILLING_MESH },
                { N_("Hatch"),  FILLING_HATCH },
                { N_("*unchanged*"), FILLING_VOID } };

  int i;

  menu  = gtk_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof (types) / sizeof (struct fill_type); i++) {
    GtkWidget *menuitem;

    menuitem = gtk_radio_menu_item_new_with_label (group, _(types[i].str));
    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
    gtk_menu_append (GTK_MENU (menu), menuitem);
    gtk_object_set_data (GTK_OBJECT(menuitem), "filltype",
                         GINT_TO_POINTER (types[i].type));
    gtk_widget_show (menuitem);
  }

  return menu;
}

/*! \brief Get the filltype data from selected objects
 *  \par Function Description
 *  Get filltype information over all selected objects.
 *  If a object property is different to the other objects, then
 *  return -2 in that variable.
 *  \param [in]   selection the selection list
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     fill width.
 *  \param [out]  pitch1    cross hatch line distance
 *  \param [out]  angle1    cross hatch angle
 *  \param [out]  pitch2    cross hatch line distance
 *  \param [out]  angle2    cross hatch angle
 *  \returns TRUE if filltype found, FALSE otherwise
 */
static bool selection_get_fill_type(GList *selection,
                                    OBJECT_FILLING *type, int *width,
                                    int *pitch1, int *angle1,
                                    int *pitch2, int *angle2)
{
  GList *iter;
  OBJECT *object;
  bool found = FALSE;
  OBJECT_FILLING otype;
  int owidth, opitch1, oangle1, opitch2, oangle2;


  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_fill_options(object, &otype, &owidth,
                             &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    if (found == FALSE) {  /* first object with filltype */
      found = TRUE;
      *type = otype;
      *width = owidth;
      *pitch1 = opitch1;
      *angle1 = oangle1;
      *pitch2 = opitch2;
      *angle2 = oangle2;
    } else {
      /* indicate different values with the value -2 */
      if (*type != otype) *type = -2;
      if (*width != owidth) *width = -2;
      if (*pitch1 != opitch1) *pitch1 = -2;
      if (*angle1 != oangle1) *angle1 = -2;
      if (*pitch2 != opitch2) *pitch2 = -2;
      if (*angle2 != oangle2) *angle2 = -2;
    }
  }

  return found;
}

/*! \brief Set the filltype in the filltype dialog
 *  \par Function Description
 *  Set all widgets in the filltype dialog. Variables marked with the
 *  invalid value -2 are set to *unchanged*.
 *  \param [in]   fill_type_data dialog structure
 *  \param [in]   type      OBJECT_FILLING type
 *  \param [in]   width     fill width.
 *  \param [in]   pitch1    cross hatch line distance
 *  \param [in]   angle1    cross hatch angle
 *  \param [in]   pitch2    cross hatch line distance
 *  \param [in]   angle2    cross hatch angle
 */
static void x_dialog_edit_fill_type_set_values(fill_type_data *fill_data,
                                        OBJECT_FILLING type, int width,
                                        int pitch1, int angle1,
                                        int pitch2, int angle2)
{
  char *text;
  GtkWidget *menu, *menuitem;

  if (type == -2)
    type = FILLING_VOID;
  gtk_option_menu_set_history(GTK_OPTION_MENU(fill_data->fill_type), type);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(fill_data->fill_type));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);

  if (width == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", width);
  gtk_entry_set_text (GTK_ENTRY (fill_data->width_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_data->width_entry),
                           0, strlen (text));
  g_free(text);

  if (pitch1 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", pitch1);
  gtk_entry_set_text (GTK_ENTRY (fill_data->pitch1_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_data->pitch1_entry),
                           0, strlen (text));
  g_free(text);

  if (angle1 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", angle1);
  gtk_entry_set_text (GTK_ENTRY (fill_data->angle1_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_data->angle1_entry),
                           0, strlen (text));
  g_free(text);

  if (pitch2 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", pitch2);
  gtk_entry_set_text (GTK_ENTRY (fill_data->pitch2_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_data->pitch2_entry),
                           0, strlen (text));
  g_free(text);

  if (angle2 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", angle2);
  gtk_entry_set_text (GTK_ENTRY (fill_data->angle2_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_data->angle2_entry),
                           0, strlen (text));
  g_free(text);
}

/*! \brief Callback function for the filltype menu in the filltype dialog
 *  \par Function Description
 *  This function sets the entry activity according to the selected
 *  filltype of the filltype dialog.
 */
static int
x_dialog_edit_fill_type_change(GtkWidget *w, fill_type_data *fill_data)
{
  //struct fill_type_data *fill_type_data = (struct fill_type_data*) data;
  GtkWidget *menuitem;
  bool activate_width_entry;
  bool activate_anglepitch1_entries;
  bool activate_anglepitch2_entries;
  int type;

  activate_width_entry         = FALSE;
  activate_anglepitch1_entries = FALSE;
  activate_anglepitch2_entries = FALSE;

  menuitem = gtk_menu_get_active (
    GTK_MENU (gtk_option_menu_get_menu (
                GTK_OPTION_MENU (fill_data->fill_type))));

  type = GPOINTER_TO_INT(
    gtk_object_get_data (GTK_OBJECT (menuitem), "filltype"));

  switch(type) {
  case(FILLING_HOLLOW):
  case(FILLING_FILL):
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
    s_log_message ("Internal Error: <%s><x_dialog_edit_fill_type_change>"
                    "unhandlered case for <%d>, line %d.\n",
                   __FILE__, type, __LINE__);
  }

  gtk_widget_set_sensitive (fill_data->width_entry,
                            activate_width_entry);
  gtk_widget_set_sensitive (fill_data->angle1_entry,
                            activate_anglepitch1_entries);
  gtk_widget_set_sensitive (fill_data->pitch1_entry,
                            activate_anglepitch1_entries);
  gtk_widget_set_sensitive (fill_data->angle2_entry,
                            activate_anglepitch2_entries);
  gtk_widget_set_sensitive (fill_data->pitch2_entry,
                            activate_anglepitch2_entries);

  return(0);
}

/*! \brief Apply the settings of the filltype dialog to the selection
 *  \par Function Description
 *  This function applies the settings of the filltype dialog to the
 *  selected objects
 */
static void
x_dialog_edit_fill_type_ok(GtkWidget *Dialog, fill_type_data *fill_data)
{
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;
  TOPLEVEL *toplevel = w_current->toplevel;

  GList *selection, *iter;
  OBJECT *object;
  const char *width_str, *angle1_str, *pitch1_str, *angle2_str, *pitch2_str;
  OBJECT_FILLING type;
  int width, angle1, pitch1, angle2, pitch2;
  OBJECT_FILLING otype;
  int owidth, oangle1, opitch1, oangle2, opitch2;

  /* get the selection */
  if (! o_select_selected(w_current))
    return;
  selection = geda_list_get_glist(Current_Selection);

  /* get the new values from the text entries of the dialog */
  width_str  = gtk_entry_get_text (GTK_ENTRY (
                                     fill_data->width_entry));
  angle1_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_data->angle1_entry));
  pitch1_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_data->pitch1_entry));
  angle2_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_data->angle2_entry));
  pitch2_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_data->pitch2_entry));
  type = GPOINTER_TO_INT(
    gtk_object_get_data (
      GTK_OBJECT (
        gtk_menu_get_active (
          GTK_MENU (gtk_option_menu_get_menu (
                      GTK_OPTION_MENU (
                        fill_data->fill_type))))), "filltype"));
  if (type == FILLING_VOID)
    type = -1;

  /* convert the options to integers (-1 means unchanged) */
  width  = g_ascii_strcasecmp (width_str,
                         _("*unchanged*")) ? atoi (width_str)  : -1;
  angle1 = g_ascii_strcasecmp (angle1_str,
                         _("*unchanged*")) ? atoi (angle1_str) : -1;
  pitch1 = g_ascii_strcasecmp (pitch1_str,
                         _("*unchanged*")) ? atoi (pitch1_str) : -1;
  angle2 = g_ascii_strcasecmp (angle2_str,
                         _("*unchanged*")) ? atoi (angle2_str) : -1;
  pitch2 = g_ascii_strcasecmp (pitch2_str,
                         _("*unchanged*")) ? atoi (pitch2_str) : -1;

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_fill_options(object, &otype, &owidth,
                             &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    otype = type == -1 ? otype : type;
    owidth = width == -1 ? owidth : width;
    opitch1 = pitch1 == -1 ? opitch1 : pitch1;
    oangle1 = angle1 == -1 ? oangle1 : angle1;
    opitch2 = pitch2 == -1 ? opitch2 : pitch2;
    oangle2 = angle2 == -1 ? oangle2 : angle2;

    /* set all not required options to -1 and
       set nice parameters if not provided by the user */
    switch (otype) {
    case (FILLING_HOLLOW):
    case (FILLING_FILL):
      owidth = opitch1 = oangle1 = opitch2 = oangle2 = -1;
      break;
    case (FILLING_HATCH):
      if (owidth < 1) owidth = 1;
      if (opitch1 < 1) opitch1 = 100;
      opitch2 = oangle2 = -1;
      break;
    case (FILLING_MESH):
      if (owidth < 1) owidth = 1;
      if (opitch1 < 1) opitch1 = 100;
      if (opitch2 < 1) opitch2 = 100;
      break;
    default:
      s_log_message ("Internal Error: <%s><x_dialog_edit_fill_type_ok>"
                     "unhandlered case for <%d>, line %d.\n",
                     __FILE__, otype, __LINE__);
    }

    o_set_fill_options (toplevel, object, otype, owidth,
                        opitch1, oangle1, opitch2, oangle2);
  }

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief response function for the filltype dialog
 *  \par Function Description
 *  This function handles the user response to the filltype dialog.
 *  It destroys the dialog after that.
 */
static void
x_dialog_edit_fill_type_response(GtkWidget *Dialog, int response,
                                 fill_type_data *fill_data)
{
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_grab_remove (Dialog);
    gtk_widget_destroy (Dialog);
    g_free (fill_data);
    w_current->hpwindow = NULL;
    break;
  case GTK_RESPONSE_ACCEPT:
    x_dialog_edit_fill_type_ok(Dialog, fill_data);
    break;
  default:
    printf("x_dialog_edit_line_type_response(): strange signal %d\n",response);
  }

  i_set_state (w_current, SELECT);

}
/*! \brief Handle selection change event for x_dialog_edit_fill_type
 *  \par Function Description
 *  Updates the fill_type dialog widgets when the selection changes.
 *  It uses the selection to set it's initial values.
 *  \param w_current pointer to GSCHEM_TOPLEVEL context
 *  \param object    pointer to a selected OBJECT.
 */
static void
x_dialog_fill_type_update_selection (GSCHEM_TOPLEVEL *w_current,
                                     OBJECT *object)
{
  GtkWidget *Dialog;
  GList *selection;
  OBJECT_FILLING type=FILLING_VOID;
  int width=0, pitch1=0, angle1=0, pitch2=0, angle2=0;
  fill_type_data *fill_data;

  /* Get ptr to the data structure */
  Dialog    = w_current->hpwindow;
  fill_data = g_object_get_data (G_OBJECT (Dialog), IDS_FILL_TYPE);

  if ( o_select_selected(w_current)) {
    selection = geda_list_get_glist(Current_Selection);
    if (! selection_get_fill_type(selection, &type, &width,
      &pitch1, &angle1, &pitch2, &angle2))
     return;
    x_dialog_edit_fill_type_set_values(fill_data, type, width,
                                       pitch1, angle1, pitch2, angle2);
    /* Set the widget activity according to the current filltype */
    x_dialog_edit_fill_type_change(fill_data->fill_type, fill_data);

    gtk_widget_grab_focus(fill_data->width_entry);
  }
}
GtkWidget *x_dialog_fill_type_create_dialog(GSCHEM_TOPLEVEL *w_current)
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
  DECLARE_TOOPTIPS

  fill_type_data *fill_data;
  Dialog = gschem_dialog_new_with_buttons(_("Edit Fill Type"),
                                          GTK_WINDOW(w_current->main_window),
         /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                          IDS_FILL_TYPE, w_current,
                                          GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                          GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                          NULL);
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  gtk_dialog_set_default_response(GTK_DIALOG(Dialog), GTK_RESPONSE_ACCEPT);

  gtk_container_border_width(GTK_CONTAINER(Dialog), DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG(Dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  /*  Don't know whether to use the headline or not (Werner) */
  /*  label = gtk_label_new(_("Fill Properties:"));
      gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
      gtk_box_pack_start(GTK_BOX(vbox),label, FALSE, FALSE, 0);  */

  table = gtk_table_new (6, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  label = gtk_label_new (_("Fill Type:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Line Width:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Angle 1:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Pitch 1:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Angle 2:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,4,5, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Pitch 2:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,5,6, GTK_FILL,0,0,0);

  optionmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                           create_menu_filltype (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), optionmenu,  1,2,0,1);
  gtk_widget_set_tooltip_text(optionmenu, _("Select fill pattern"));

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry, 1,2,1,2);
  gtk_widget_set_tooltip_text(width_entry,
                              _("Set the width of the filler lines"));

  angle1_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(angle1_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), angle1_entry, 1,2,2,3);
  gtk_widget_set_tooltip_text(angle1_entry,
                              _("Primary angle for filler lines"));

  pitch1_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(pitch1_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), pitch1_entry, 1,2,3,4);
  gtk_widget_set_tooltip_text(pitch1_entry,
                              _("Spacing for the primary filler lines"));

  angle2_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(angle2_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), angle2_entry, 1,2,4,5);
  gtk_widget_set_tooltip_text(angle2_entry,
                              _("Secondary angle for filler lines"));

  pitch2_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(pitch2_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), pitch2_entry, 1,2,5,6);
  gtk_widget_set_tooltip_text(pitch2_entry,
                              _("Spacing for the secondary filler lines"));

  fill_data = (fill_type_data*) g_malloc (sizeof (struct st_fill_type_data));

  /* populate the data structure */
  fill_data->fill_type    = optionmenu;
  fill_data->width_entry  = width_entry;
  fill_data->angle1_entry = angle1_entry;
  fill_data->pitch1_entry = pitch1_entry;
  fill_data->angle2_entry = angle2_entry;
  fill_data->pitch2_entry = pitch2_entry;

  /* fill in the fields of the dialog */
  x_dialog_edit_fill_type_set_values(fill_data, FILLING_VOID, 0, 0, 0, 0, 0);

  g_signal_connect (G_OBJECT (optionmenu), "changed",
                    G_CALLBACK (x_dialog_edit_fill_type_change),
                    fill_data);

  g_signal_connect (G_OBJECT (Dialog), "response",
                    G_CALLBACK (x_dialog_edit_fill_type_response),
                    fill_data);

  g_object_set (G_OBJECT (Dialog),
                DIALOG_DATA_SELECTION,
                x_dialog_fill_type_update_selection,
                NULL);

  g_object_set_data(G_OBJECT(Dialog), IDS_FILL_TYPE, fill_data);

  return Dialog;
}

/*! \brief Creates the fill type dialog
 *  \par Function Description
 *  This function creates the fill type dialog.
 */
void x_dialog_edit_fill_type(GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *Dialog;

  Dialog = w_current->hpwindow;
  if (!Dialog) {

    Dialog = x_dialog_fill_type_create_dialog(w_current);
    gtk_window_position(GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  GTK_WINDOW(w_current->main_window));

    w_current->hpwindow = Dialog;
    gtk_widget_show_all (Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }
  x_dialog_fill_type_update_selection (w_current, NULL);
}

/******************* End of Fill Type dialog box ************************/

/*!****** \section Line-Type-Dialog ( Line Type Editing-Dialogs ) *******/

/*! \brief Create a line type menu for the line type dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different linetypes.
 */
static GtkWidget *create_menu_linetype (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GSList *group;
  struct line_type {
    char *str;
    OBJECT_TYPE type;
  } types[] = { { N_("Solid"),   TYPE_SOLID },
                { N_("Dotted"),  TYPE_DOTTED },
                { N_("Dashed"),  TYPE_DASHED },
                { N_("Center"),  TYPE_CENTER },
                { N_("Phantom"), TYPE_PHANTOM },
                { N_("*unchanged*"), TYPE_ERASE } };
  int i;

  menu  = gtk_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof (types) / sizeof (struct line_type); i++) {
    GtkWidget *menuitem;

    menuitem = gtk_radio_menu_item_new_with_label (group, _(types[i].str));
    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
    gtk_menu_append (GTK_MENU (menu), menuitem);
    gtk_object_set_data (GTK_OBJECT(menuitem), "linetype",
                         GINT_TO_POINTER (types[i].type));
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*! \brief get the linetype data from selected objects
 *  \par Function Description
 *  Get linetype information over all selected objects.
 *  If a object property is different to the other objects, then
 *  return -2 in that variable.
 *  \param [in]   selection the selection list
 *  \param [out]  end       OBJECT_END type
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     line width
 *  \param [out]  length    length of each line
 *  \param [out]  space     space between points and lines
 *  \returns TRUE if linetype found, FALSE otherwise
 */
static bool
selection_get_line_type(GList *selection, OBJECT_END *end, OBJECT_TYPE *type,
                        int *width, int *length, int *space)
{
  GList *iter;
  OBJECT *object;
  bool found = FALSE;
  OBJECT_END oend;
  OBJECT_TYPE otype;
  int owidth=0, olength=0, ospace=0;

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_line_options(object, &oend, &otype,
                             &owidth, &olength, &ospace))
      continue;

    if (found == FALSE) {  /* first object with filltype */
      found = TRUE;
      *end = oend;
      *type = otype;
      *width = owidth;
      *length = olength;
      *space = ospace;
    } else {
      /* indicate different values with the value -2 */
      if (*end != oend) *end = -2;
      if (*type != otype) *type = -2;
      if (*width != owidth) *width = -2;
      if (*length != olength) *length = -2;
      if (*space != ospace) *space = -2;
    }
  }

  return found;
}

/*! \brief set the linetype in the linetype dialog
 *  \par Function Description
 *  Set all widgets in the linetype dialog. Variables marked with the
 *  invalid value -2 are set to *unchanged*.
 *  \param [in]   line_type_data dialog structure
 *  \param [in]   end       OBJECT_END type (currently not used)
 *  \param [in]   type      OBJECT_FILLING type
 *  \param [in]   width     fill width.
 *  \param [in]   length    length of each line
 *  \param [in]   space     space between points and lines
 */
static void
x_dialog_edit_line_type_set_values(line_type_data *line_data,
                                   OBJECT_END end, OBJECT_TYPE type,
                                   int width, int length, int space)
{
  char *text;
  GtkWidget *menu, *menuitem;

  if (type == -2)
    type = TYPE_ERASE;
  gtk_option_menu_set_history(GTK_OPTION_MENU(line_data->line_type), type);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(line_data->line_type));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);

  if (width == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", width);
  gtk_entry_set_text (GTK_ENTRY (line_data->width_entry), text);
  gtk_entry_select_region (GTK_ENTRY (line_data->width_entry),
                           0, strlen (text));
  g_free(text);

  if (length == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", length);
  gtk_entry_set_text (GTK_ENTRY (line_data->length_entry), text);
  gtk_entry_select_region (GTK_ENTRY (line_data->length_entry),
                           0, strlen (text));
  g_free(text);

  if (space == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", space);
  gtk_entry_set_text (GTK_ENTRY (line_data->space_entry), text);
  gtk_entry_select_region (GTK_ENTRY (line_data->space_entry),
                           0, strlen (text));
  g_free(text);
}

/*! \brief Callback function for the linetype menu item in the line type dialog
 *  \par Function Description
 *  This Function is called when the user changes the line type selection.
 *  It sets the dash space/length entries either active or inactive.
 */
static int
x_dialog_edit_line_type_change(GtkWidget *w,
                                        line_type_data *line_data)
{
  GtkWidget *menuitem;
  bool activate_length_entry, activate_space_entry;
  int type;

  menuitem = gtk_menu_get_active (
    GTK_MENU (gtk_option_menu_get_menu (
                GTK_OPTION_MENU (line_data->line_type))));

  type = GPOINTER_TO_INT(
    gtk_object_get_data (GTK_OBJECT (menuitem), "linetype"));
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

/*! \brief Worker function for the line type and width dialog
 *  \par Function Description
 *  The function takes the properties of the dialog and applies
 *  them to the selected objects.
 */
static void
x_dialog_edit_line_type_ok(GtkWidget *Dialog, line_type_data *line_data)
{
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;
  TOPLEVEL *toplevel = w_current->toplevel;

  GList *selection, *iter;
  OBJECT *object;
  const char *width_str, *length_str, *space_str;
  OBJECT_TYPE type;
  int width, length, space;
  OBJECT_TYPE otype;
  OBJECT_END oend;
  int owidth, olength, ospace;

  /* get the selection */
  if (! o_select_selected(w_current))
    return;
  selection = geda_list_get_glist(Current_Selection);

  /* get the new values from the text entries of the dialog */
  width_str   = gtk_entry_get_text (GTK_ENTRY (
                                      line_data->width_entry));
  length_str  = gtk_entry_get_text (GTK_ENTRY (
                                      line_data->length_entry));
  space_str   = gtk_entry_get_text (GTK_ENTRY (
                                      line_data->space_entry));
  type = GPOINTER_TO_INT(
    gtk_object_get_data (
      GTK_OBJECT (
        gtk_menu_get_active (
          GTK_MENU (gtk_option_menu_get_menu (
                      GTK_OPTION_MENU (
                        line_data->line_type))))), "linetype"));
  if (type == TYPE_ERASE)
    type = -1;

  /* convert the options to integers (-1 means unchanged) */
  width =  g_ascii_strcasecmp (width_str,
                         _("*unchanged*")) ? atoi (width_str)  : -1;
  length = g_ascii_strcasecmp (length_str,
                         _("*unchanged*")) ? atoi (length_str) : -1;
  space  = g_ascii_strcasecmp (space_str,
                         _("*unchanged*")) ? atoi (space_str)  : -1;

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_line_options(object, &oend, &otype,
                             &owidth, &olength, &ospace))
      continue;

    /* oend is not in the dialog, yet */
    otype   = type   == -1 ? otype : type;
    owidth  = width  == -1 ? owidth : width;
    olength = length == -1 ? olength : length;
    ospace  = space  == -1 ? ospace : space;

    /* set all not required options to -1 and
       set nice parameters if not provided by the user */
    switch (otype) {
    case (TYPE_SOLID):
      olength = ospace = -1;
      break;
    case (TYPE_DOTTED):
      olength = -1;
      if (ospace < 1) ospace = 100;
      break;
    case (TYPE_DASHED):
    case (TYPE_CENTER):
    case (TYPE_PHANTOM):
      if (ospace < 1) ospace = 100;
      if (olength < 1) olength = 100;
      break;
    default:
      s_log_message ("Internal Error: <%s><x_dialog_edit_line_type_ok>"
                     "unhandlered case for <%d>, line %d.\n",
                     __FILE__, otype, __LINE__);
    }

    o_set_line_options (toplevel,
                        object, oend, otype, owidth, olength, ospace);
  }

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief response function for the line type and width dialog
 *  \par Function Description
 *  This function takes the user input and applies it to selected
 *  objects.
 *  After that it kills the dialog.
 */
void
x_dialog_edit_line_type_response(GtkWidget *Dialog, int response,
                                 line_type_data *line_data)
{
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy (Dialog);
    g_free (line_data);
    w_current->ltwindow = NULL;
    break;
  case GTK_RESPONSE_ACCEPT:
    x_dialog_edit_line_type_ok(Dialog, line_data);
    break;
  default:
    printf("x_dialog_edit_line_type_response(): strange signal %d\n",response);
  }

  i_set_state (w_current, SELECT);

}

/*! \brief Handle selection change event for x_dialog_edit_fill_type
 *  \par Function Description
 *  Updates the fill_type dialog widgets when the selection changes.
 *  It uses the selection to set it's initial values.
 *
 *  \param w_current pointer to GSCHEM_TOPLEVEL context
 *  \param object    pointer to a selected OBJECT.
 */
static void
x_dialog_line_type_update_selection (GSCHEM_TOPLEVEL *w_current,
                                     OBJECT *object)
{
  GtkWidget *Dialog;
  GList *selection;
  OBJECT_END end=END_NONE;
  OBJECT_TYPE type=TYPE_SOLID;
  int width=1, length=-1, space=-1;
  line_type_data *line_data;

  /* Get ptr to the data structure */
  Dialog    = w_current->ltwindow;

  line_data = g_object_get_data (G_OBJECT (Dialog), IDS_LINE_TYPE);

  if ( o_select_selected(w_current)) {
    selection = geda_list_get_glist(Current_Selection);
    if (! selection_get_line_type(selection, &end, &type, &width, &length, &space))
     return;
    /* fill in the fields of the dialog */
    x_dialog_edit_line_type_set_values(line_data, end, type, width, length, space);

    /* calling it once will set the dash space/length activity */
    x_dialog_edit_line_type_change(line_data->line_type, line_data);

    gtk_widget_grab_focus(line_data->width_entry);
  }
}
GtkWidget *x_dialog_line_type_create_dialog(GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *Dialog;
  GtkWidget *vbox;
  GtkWidget *optionmenu   = NULL;
  GtkWidget *length_entry = NULL;
  GtkWidget *space_entry  = NULL;
  GtkWidget *width_entry  = NULL;
  GtkWidget *table;
  GtkWidget *label;
  DECLARE_TOOPTIPS

  line_type_data *line_data;

  Dialog = gschem_dialog_new_with_buttons(_("Edit Line Width & Type"),
                                          GTK_WINDOW(w_current->main_window),
         /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                          IDS_LINE_TYPE, w_current,
                                          GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                          GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (Dialog), GTK_RESPONSE_ACCEPT);

  gtk_container_border_width(GTK_CONTAINER(Dialog), DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG(Dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  /*  Don't know whether to set the headline or not (Werner) */
  /*  label = gtk_label_new(_("Line Properties:"));
      gtk_misc_set_alignment(GTK_MISC(label),0,0);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0); */

  table = gtk_table_new (4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  label = gtk_label_new (_("Type:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Width:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Dash Length:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Dash Space:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  optionmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                           create_menu_linetype (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), optionmenu, 1,2,0,1);

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(width_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry, 1,2,1,2);
  gtk_widget_set_tooltip_text(width_entry, _("Set width of the line"));

  length_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(length_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(length_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), length_entry, 1,2,2,3);
  gtk_widget_set_tooltip_text(length_entry, _("Set \"dash\" length of the line"));

  space_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(space_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(space_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), space_entry, 1,2,3,4);
  gtk_widget_set_tooltip_text(space_entry, _("Set spacing between dashes in the line"));

  line_data = (line_type_data*) g_malloc (sizeof (struct st_line_type_data));

  /* populate the data structure */
  line_data->width_entry  = width_entry;
  line_data->line_type    = optionmenu;
  line_data->length_entry = length_entry;
  line_data->space_entry  = space_entry;

  /* fill in the fields of the dialog */
  x_dialog_edit_line_type_set_values(line_data, END_NONE, TYPE_SOLID, 1, 1, 1);

  /* calling it once will set the dash space/length activity */
  x_dialog_edit_line_type_change(optionmenu, line_data);

  g_signal_connect(G_OBJECT (optionmenu), "changed",
                   G_CALLBACK (x_dialog_edit_line_type_change),
                   line_data);

  g_signal_connect (G_OBJECT (Dialog), "response",
                    G_CALLBACK (x_dialog_edit_line_type_response),
                    line_data);

  g_object_set (G_OBJECT (Dialog), DIALOG_DATA_SELECTION,
                x_dialog_line_type_update_selection,
                NULL);

  g_object_set_data(G_OBJECT(Dialog), IDS_LINE_TYPE, line_data);

  return Dialog;
}
/*! \brief Creates the line type and width dialog
 *  \par Function Description
 *  This function creates and sets up a dialog for manipulating
 *  properties of line objects.
 */
void x_dialog_edit_line_type (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *Dialog;
  Dialog = w_current->ltwindow;
  if (!Dialog) {

    Dialog = x_dialog_line_type_create_dialog(w_current);
    gtk_window_position(GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  GTK_WINDOW(w_current->main_window));

    w_current->ltwindow = Dialog;
    gtk_widget_show_all (Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }
  x_dialog_line_type_update_selection (w_current, NULL);
}

/***************** End of Line Type / Width dialog box ****************/

/*!******** \section Edit-Slots-Dialog ( Slots Editing-Dialogs ) ********/

/*! \brief response function for the slot edit dialog
 *  \par Function Description
 *  The function calles o_slot_end to apply the dialog entry to the slot
 *  the selected symbol, if they exist.
 */
void x_dialog_edit_slot_response(GtkWidget *ThisDialog, int response,
                                 GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  char *slot_string;
  int len;
  char *string = NULL;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(ThisDialog);
    w_current->sewindow = NULL;
    i_set_state(w_current, SELECT);
    break;
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(ThisDialog), IDS_SLOT_EDIT);
    string = (char*) gtk_entry_get_text(GTK_ENTRY(textentry));
    len = strlen(string);
    if (len != 0) {
      slot_string = g_strdup_printf ("slot=%s", string);
      o_slot_end (w_current, o_select_return_first_object (w_current),
                  slot_string);
      g_free (slot_string);
    }
    break;
  default:
    printf("x_dialog_edit_slot_response(): strange signal %d\n",response);
  }

}

/*! \brief Handle selection change event for the Slot Editor Dialog
 *  \par Function Description
 *  Updates the Slot Properties dialog widgets when the selection changes.
 *  The initial value is set when x_dialog_edit_slot is first called.
 *
 *  \param w_current pointer to GSCHEM_TOPLEVEL context
 *  \param object    pointer to a selected OBJECT.
 */
static void
x_dialog_slot_edit_update_selection (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  GtkWidget *ThisDialog;
  GtkWidget *textentry;
  char *slot_value = NULL;

  if (object != NULL) {

    if (object->type == OBJ_COMPLEX) {
      slot_value = o_attrib_search_object_attribs_by_name (object, "slot", 0);
    }
    else {
      if (object->type == OBJ_TEXT) {
        slot_value = object->text->string;
      }
    }

    /* Get ptr to the Dialog window */
    ThisDialog = w_current->sewindow;

    /* Get ptr to the text widget */
    textentry = g_object_get_data(G_OBJECT(ThisDialog), IDS_SLOT_EDIT);

    if (slot_value != NULL) {
      gtk_widget_set_sensitive (textentry, TRUE);
      gtk_entry_set_text(GTK_ENTRY(textentry), slot_value);
      gtk_editable_select_region (GTK_EDITABLE(textentry), 0, -1);
      /* And set focus to the widget */
      gtk_widget_grab_focus(textentry);
    }
    else {
      gtk_widget_set_sensitive (textentry, FALSE);
    }
  }

}
/*! \brief Create the slot entry dialog
 *  \par Function Description
 *  This function creates the slot edit dialog.
 */
void x_dialog_edit_slot (GSCHEM_TOPLEVEL *w_current, const char *string)
{
  GtkWidget *ThisDialog;
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  ThisDialog = w_current->sewindow;
  if (!ThisDialog) {
    ThisDialog = gschem_dialog_new_with_buttons(_("Edit slot number"),
                                   GTK_WINDOW(w_current->main_window),
          /* nonmodal Editing ThisDialog */    GSCHEM_MODELESS_DIALOG,
                                             IDS_SLOT_EDIT, w_current,
                                 GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                 GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                                                  NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(ThisDialog),
                        GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog),
                                     GTK_RESPONSE_ACCEPT);



    gtk_container_border_width(GTK_CONTAINER(ThisDialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Edit slot number:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    textentry = gtk_entry_new();
    gtk_box_pack_start( GTK_BOX(vbox),
                       textentry, FALSE, FALSE, 0);
    gtk_entry_set_max_length(GTK_ENTRY(textentry), 80);

    /* always set the current text and select the number of the slot */
    if (string != NULL) {
      gtk_entry_set_text(GTK_ENTRY(textentry), string);
      gtk_editable_select_region (GTK_EDITABLE(textentry), 0, -1);
    }

    gtk_entry_set_activates_default (GTK_ENTRY(textentry),TRUE);

    GSCHEM_HOOKUP_OBJECT(ThisDialog, textentry, IDS_SLOT_EDIT);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_edit_slot_response),
                      w_current);

    g_object_set (G_OBJECT (ThisDialog), DIALOG_DATA_SELECTION,
                 x_dialog_slot_edit_update_selection,
                 NULL);

    w_current->sewindow = ThisDialog;
    gtk_widget_show_all (ThisDialog);
  }

  else { /* dialog already created */
    x_dialog_slot_edit_update_selection (w_current, NULL);
    gtk_window_present (GTK_WINDOW(ThisDialog));
  }

}
/******************** End of Slot Edit dialog box ***********************/

/*!****** \section Find-Text-Dialog ( Find Text Editing-Dialogs ) *******/

/*! \brief response function for the find text dialog
 *  \par Function Description
 *  This function searches the schematic for the user input string.
 */
void x_dialog_find_text_response(GtkWidget *Dialog, int response,
                                 PAGE *remember_page)
{
  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG (Dialog)->w_current;
  TOPLEVEL *toplevel = w_current->toplevel;

  GtkWidget *textentry;
  GtkWidget *checkdescend;
  char *string;
  int done=0, close=0;
  int start_find;

  if (w_current->ftwindow == NULL) {
    w_current->ftwindow = Dialog;
    start_find = TRUE;
  }
  else
    start_find = FALSE;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(Dialog), IDS_FIND_TEXT);
    string = (char*) gtk_entry_get_text(GTK_ENTRY(textentry));
    checkdescend = g_object_get_data(G_OBJECT(Dialog), "checkdescend");

    strncpy(General_textstring, string, sizeof(General_textstring)-1);
    General_textstring[sizeof(General_textstring)-1] = '\0';

    if (remember_page != toplevel->page_current) {
      s_page_goto(toplevel, remember_page);
    }
    done =
      o_edit_find_text (w_current,
                        s_page_objects (remember_page),
                        string,
                        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
                                                    (checkdescend)),
                        !start_find);
    if (done) {
      o_invalidate_all (w_current);
      close = 1;
    }
    start_find = 0;
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    close = 1;
    break;
  default:
    printf("x_dialog_find_text_response(): strange signal %d\n", response);
  }
  if (close) {
    gtk_widget_destroy(Dialog);
    w_current->ftwindow = NULL;
  }
}

/*! \brief Create the text find dialog
 *  \par Function Description
 *  This function creates the text find dialog.
 */
void x_dialog_find_text(GSCHEM_TOPLEVEL *w_current)
{
  static GtkWidget *ThisDialog;
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *checkdescend;
  GtkWidget *textentry;
  OBJECT *object = NULL;
  PAGE *remember_page;

  remember_page = w_current->toplevel->page_current;
  if ((object = o_select_return_first_object(w_current)) != NULL) {
    if (object->type == OBJ_TEXT) {
      strncpy (General_textstring,
               o_text_get_string (w_current->toplevel, object),
               sizeof(General_textstring)-1);
      General_textstring[sizeof(General_textstring)-1] = '\0';
    }
  }

  if (GTK_IS_DIALOG(ThisDialog)) {
    /* dialog already created */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }
  else {
    ThisDialog = gschem_dialog_new_with_buttons(_("Find Text"),
                            GTK_WINDOW(w_current->main_window),
       /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                      IDS_FIND_TEXT, w_current,
                          GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                           GTK_STOCK_FIND, GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                     GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(ThisDialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Text to find:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    checkdescend = gtk_check_button_new_with_label(_("descend into hierarchy"));
    gtk_box_pack_start(GTK_BOX(vbox), checkdescend, TRUE, TRUE, 0);

    GSCHEM_HOOKUP_OBJECT(ThisDialog, textentry, IDS_FIND_TEXT);
    GSCHEM_HOOKUP_OBJECT(ThisDialog, checkdescend, "checkdescend");

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_find_text_response),
                      remember_page);

    gtk_widget_show_all(ThisDialog);
  }

  /* always select the text string in the entry */
  textentry = g_object_get_data (G_OBJECT (ThisDialog), IDS_FIND_TEXT);
  gtk_entry_set_text(GTK_ENTRY(textentry), General_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of find text dialog box *******/

/*! \section Edit-Hide-Text-Dialog in Editing-Dialogs*/

/*********** Start of hide text dialog box *******/

/*! \brief Response function for the hide text dialog
 *  \par Function Description
 *  This is the response function of the hide text dialog. It takes the user input
 *  and hides all text elements that starts with the searchtext.
 */
void x_dialog_hide_text_response(GtkWidget *Dialog, int response,
                                 GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  char *string;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(Dialog),IDS_HIDE_TEXT);
    string = (char*) gtk_entry_get_text(GTK_ENTRY(textentry));

    strncpy(General_textstring, string, sizeof(General_textstring)-1);
    General_textstring[sizeof(General_textstring)-1] = '\0';

    o_edit_hide_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(Dialog);
    w_current->htwindow = NULL;
    break;
  default:
    printf("x_dialog_show_text_response(): strange signal %d\n",response);
  }
}

/*! \brief Creates the hide text dialog
 *  \par Function Description
 *  This function creates the hide text dialog.
 */
void x_dialog_hide_text(GSCHEM_TOPLEVEL * w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  ThisDialog = w_current->htwindow;
  if (!ThisDialog) {
    ThisDialog = gschem_dialog_new_with_buttons(_("Hide Text"),
                            GTK_WINDOW(w_current->main_window),
      /* nonmodal Editing Dialog */     GSCHEM_MODELESS_DIALOG,
                                        IDS_HIDE_TEXT, w_current,
                            GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                            GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                                           NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(ThisDialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Hide text starting with:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GSCHEM_HOOKUP_OBJECT(ThisDialog, textentry, IDS_HIDE_TEXT);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_hide_text_response),
                      w_current);

    gtk_widget_show_all(ThisDialog);
    w_current->htwindow = ThisDialog;
  }

  else { /* dialog already created, just select it */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always select the text in the search entry */
  textentry = g_object_get_data (G_OBJECT (ThisDialog), IDS_HIDE_TEXT);
  gtk_entry_set_text(GTK_ENTRY(textentry), General_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of hide text dialog box *******/

/*! \section Edit-Show-Text-Dialog in Editing-Dialogs*/

/*********** Start of show text dialog box *******/

/*! \brief Response function for the show text dialog
 *  \par Function Description
 *  This function takes the users input and searches all strings starting with
 *  the given search text and hides those text objects.
 */
void x_dialog_show_text_response(GtkWidget *Dialog, int response,
                                 GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  char *string;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(Dialog),IDS_SHOW_TEXT);
    string = (char*) gtk_entry_get_text(GTK_ENTRY(textentry));

    strncpy(General_textstring, string, sizeof(General_textstring)-1);
    General_textstring[sizeof(General_textstring)-1] = '\0';
    o_edit_show_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(Dialog);
    w_current->stwindow = NULL;
    break;
  default:
    printf("x_dialog_show_text_response(): strange signal %d\n",response);
  }
}

/*! \brief Create the show text dialog.
 *  \par Function Description
 *  This function creates the show text dialog.
 */
void x_dialog_show_text(GSCHEM_TOPLEVEL * w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  ThisDialog = w_current->stwindow;
  if (!ThisDialog) {
    ThisDialog = gschem_dialog_new_with_buttons(_("Show Text"),
                            GTK_WINDOW(w_current->main_window),
       /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                      IDS_SHOW_TEXT, w_current,
                          GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                          GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                                                      NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(ThisDialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Show text starting with:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GSCHEM_HOOKUP_OBJECT(ThisDialog, textentry, IDS_SHOW_TEXT);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_show_text_response),
                      w_current);

    gtk_widget_show_all(ThisDialog);
    w_current->stwindow = ThisDialog;
  }

  else { /* dialog already created. Show it */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always select the text in the entry */
  textentry = g_object_get_data (G_OBJECT (ThisDialog), IDS_SHOW_TEXT);
  gtk_entry_set_text(GTK_ENTRY(textentry), General_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of show text dialog box *******/

/*!***** \section Text-Input-Dialog ( Text-Input Editing-Dialogs ) ******/

/*! \brief Apply function for the text entry dialog
 *  \par Function Description
 *  This function applies the text from the text entry dialog.
 */
void x_dialog_text_input_apply(GtkWidget *Dialog, GSCHEM_TOPLEVEL *w_current)
{
  char *string = NULL;
  char *tmp    = NULL;
  GtkWidget     *tientry;
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  tientry = gtk_object_get_data(GTK_OBJECT(Dialog), IDS_TEXT_INPUT);

  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tientry));

  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  string =  gtk_text_iter_get_text (&start, &end);

  if (string[0] != '\0' ) {

    switch(w_current->text_case) {
      case(LOWER_CASE):
        tmp = g_utf8_strdown (string, -1);
        break;

      case(UPPER_CASE):
        tmp = g_utf8_strup (string, -1);
        break;

      case(BOTH_CASES):
      default:
        /* do nothing */
        break;
    }

    /* Select the text, so you can continue immediatly writing the next text */
    select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
    gtk_widget_grab_focus(tientry);

    w_current->toplevel->page_current->CHANGED=1;

    o_text_prepare_place (w_current, tmp == NULL ? string : tmp);
    g_free (string);
    g_free (tmp);
  }
}

/*! \brief Response Function for the Text Entry dialog
 *  \par Function Description
 *  Callback function for the text entry dialog.
 */
void x_dialog_text_input_response(GtkWidget *Dialog, int response,
                                  GSCHEM_TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_ACCEPT:
    x_dialog_text_input_apply(Dialog, w_current);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    i_set_state(w_current, SELECT);
    gtk_widget_destroy(Dialog);
    w_current->tiwindow=NULL;
    break;
  default:
    printf("x_dialog_edit_text_response(): strange signal %d\n", response);
  }
}

/*! \brief create or present the text entry dialog
 *  \par Function Description
 *  This function creates or raises the modeless text entry dialog
 */
void x_dialog_text_input (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *label = NULL;
  GtkWidget *tientry = NULL;
  GtkWidget *vbox;
  GtkWidget *viewport1 = NULL;
  GtkWidget *scrolled_window = NULL;
  PangoTabArray *tab_array;
  int real_tab_width;

  ThisDialog = w_current->tiwindow;
  if (!ThisDialog) { /* dialog not created yet */

    ThisDialog = gschem_dialog_new_with_buttons(_("Text Entry..."),
                                GTK_WINDOW(w_current->main_window),
           /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                         IDS_TEXT_INPUT, w_current,
                              GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                              GTK_STOCK_APPLY, GTK_RESPONSE_ACCEPT,
                                                              NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW (ThisDialog), GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_text_input_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER (ThisDialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox),DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter text, click apply,\n"
                             "move cursor into window, click to place text.\n"
                             "Middle button to rotate while placing."));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    viewport1 = gtk_viewport_new (NULL, NULL);
    gtk_widget_show (viewport1);

    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
    gtk_box_pack_start( GTK_BOX(vbox), viewport1, TRUE, TRUE, 0);

    tientry = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(tientry), TRUE);
    select_all_text_in_textview(GTK_TEXT_VIEW(tientry));

    /* Set the tab width, using pango tab array */
    tab_array = pango_tab_array_new (1, TRUE);
    real_tab_width = text_view_calculate_real_tab_width(GTK_TEXT_VIEW(tientry),
                                                        tab_in_chars);
    if (real_tab_width >= 0) {
      pango_tab_array_set_tab (tab_array, 0, PANGO_TAB_LEFT, real_tab_width);
      /* printf("Real tab width: %i\n", real_tab_width);*/
      gtk_text_view_set_tabs (GTK_TEXT_VIEW (tientry),
                              tab_array);
    }
    else {
      g_warning ("x_dialog_text_input: Impossible to set tab width.\n");
    }
    pango_tab_array_free (tab_array);
    gtk_container_add(GTK_CONTAINER(scrolled_window), tientry);

    gtk_object_set_data(GTK_OBJECT(ThisDialog), IDS_TEXT_INPUT, tientry);

    gtk_widget_show_all (ThisDialog);

    w_current->tiwindow = ThisDialog;
  }
  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(ThisDialog));
  }

  /* always select the text in the entry */
  tientry = gtk_object_get_data(GTK_OBJECT(ThisDialog), IDS_TEXT_INPUT);
  select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
  gtk_widget_grab_focus(tientry);
}

/******************* End of Text Input dialog box ***********************/

/*!****** \section Translate-Dialog ( Translate Editing-Dialogs ) *******/

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  This function takes the user action and applies it.
 *  \todo improve error detection / use a spin button?
 */
void x_dialog_translate_response(GtkWidget *Dialog, int response,
                                 GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  char *string;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(Dialog),IDS_TRANSLATE);
    string = (char*) gtk_entry_get_text(GTK_ENTRY(textentry));
    if (strlen(string) != 0) {
      o_complex_translate_all(w_current, atoi(string));
    }
    break;
  default:
    printf("translate_edit_dialog_response(): strange signal %d\n",response);
  }

  i_set_state(w_current, SELECT);
  gtk_widget_destroy(Dialog);
  w_current->trwindow=NULL;
}


/*! \brief Create the translate dialog
 *  \par Function Description
 *  Create the dialog to translate symbols.
 */
void x_dialog_translate (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *label;
  GtkWidget *textentry;
  GtkWidget *vbox;

  ThisDialog = w_current->trwindow;
  if (!ThisDialog) {
    ThisDialog = gschem_dialog_new_with_buttons(_("Translate"),
                            GTK_WINDOW(w_current->main_window),
                                              GTK_DIALOG_MODAL,
                                      IDS_TRANSLATE, w_current,
                                              GTK_STOCK_CANCEL,
                                           GTK_RESPONSE_REJECT,
                                                  GTK_STOCK_OK,
                                           GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(ThisDialog),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Offset to translate?\n(0 for origin)"));
    gtk_misc_set_alignment(GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length (10);
    gtk_entry_set_text(GTK_ENTRY(textentry), "0");
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_box_pack_start(GTK_BOX(vbox),textentry, FALSE, FALSE, 0);

    GSCHEM_HOOKUP_OBJECT(ThisDialog, textentry, IDS_TRANSLATE);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK ( x_dialog_translate_response),
                      w_current);

    gtk_widget_show_all (ThisDialog);
    w_current->trwindow = ThisDialog;
  }

  else  { /* dialog already created */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }
}

/***************** End of Translate dialog box ***********************/
/*! @} endgroup Editing-Dialogs */

/*! \defgroup Systemic-Dialogs
 *  @{ \par This Group contains Functions for System Level Dialogs
*/

/*!********* \section Hotkeys-Dialog (Hotkeys Systemic-Dialogs) *********/

/*! \brief Response function for the hotkey dialog
 *  \par Function Description
 *  This function destroys the hotkey dialog and does some cleanup.
 */
void x_dialog_hotkeys_response(GtkWidget *Dialog, int response,
                               GSCHEM_TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("x_dialog_hotkeys_response(): strange signal %d\n", response);
  }
  /* clean up */
  gtk_widget_destroy(Dialog);
  w_current->hkwindow = NULL;
}

/*! \brief Creates the hotkeys dialog
 *  \par Function Description
 *  This function creates the hotkey dialog and puts the list of hotkeys
 *  into it.
 */
void x_dialog_hotkeys (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *vbox, *scrolled_win;
  GtkListStore *store;
  GtkWidget *treeview;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  ThisDialog = w_current->hkwindow;
  if (!w_current->hkwindow) {
    ThisDialog = gschem_dialog_new_with_buttons(_("Hotkeys"),
                          GTK_WINDOW(w_current->main_window),
    /* nonmodal Editing Dialog */     GSCHEM_MODELESS_DIALOG,
                                      IDS_HOTKEYS, w_current,
                        GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                                        NULL);

    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width (GTK_CONTAINER (ThisDialog),
                                DIALOG_BORDER_SPACING);

    gtk_widget_set_usize(ThisDialog, 300,300);

    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    /* the model */
    store = g_keys_to_list_store ();

    /* the tree view */
    treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
    gtk_container_add(GTK_CONTAINER(scrolled_win), treeview);

    /* the columns */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Function"),
                                                       renderer,
                                                       "text",
                                                       0,
                                                       NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Keystroke(s)"),
                                                       renderer,
                                                       "text",
                                                       1,
                                                       NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);


    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_hotkeys_response),
                      w_current);

    /* show all recursively */
    gtk_widget_show_all(ThisDialog);
    w_current->hkwindow = ThisDialog;
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }
}

/******************* End of help/keymapping dialog box ******************/

/*!*** \section Confirm-Exit-Dialog ( Confirm-Exit Systemic-Dialogs) ****/

/****************** Aka Confirm-Close, need's it's own file *************/

#define TYPE_CLOSE_CONFIRMATION_DIALOG            (close_confirmation_dialog_get_type ())
#define CLOSE_CONFIRMATION_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialog))
#define CLOSE_CONFIRMATION_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialogClass))
#define IS_CLOSE_CONFIRMATION_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define IS_CLOSE_CONFIRMATION_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define CLOSE_CONFIRMATION_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialogClass))

typedef struct _CloseConfirmationDialog      CloseConfirmationDialog;
typedef struct _CloseConfirmationDialogClass CloseConfirmationDialogClass;

struct _CloseConfirmationDialog
{
  GtkDialog parent;

  GtkListStore *store_unsaved_pages;
};

struct _CloseConfirmationDialogClass
{
  GtkDialogClass parent_class;
};

enum {
  PROP_UNSAVED_PAGE=1,
  PROP_UNSAVED_PAGES,
  PROP_SELECTED_PAGES
};

enum {
  COLUMN_SAVE,
  COLUMN_PAGE,
  NUM_COLUMNS
};

static void* close_confirmation_dialog_parent_class = NULL;
static void close_confirmation_dialog_class_init (CloseConfirmationDialogClass *klass);
static void close_confirmation_dialog_init (CloseConfirmationDialog *self);
static void close_confirmation_dialog_set_property (GObject      *object,
                                                    guint         property_id,
                                                    const GValue *value,
                                                    GParamSpec   *pspec);
static void close_confirmation_dialog_get_property (GObject      *object,
                                                    guint         property_id,
                                                    GValue       *value,
                                                    GParamSpec   *pspec);
static GObject* close_confirmation_dialog_constructor (GType type,
                                                       guint n_construct_properties,
                                                       GObjectConstructParam *construct_params);

GList *close_confirmation_dialog_get_selected_pages (CloseConfirmationDialog *dialog);

GType close_confirmation_dialog_get_type ()
{
  static GType close_confirmation_dialog_type = 0;

  if (!close_confirmation_dialog_type) {
    static const GTypeInfo close_confirmation_dialog_info = {
      sizeof(CloseConfirmationDialogClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) close_confirmation_dialog_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CloseConfirmationDialog),
      0,    /* n_preallocs */
      (GInstanceInitFunc) close_confirmation_dialog_init,
    };

    close_confirmation_dialog_type =
      g_type_register_static (GTK_TYPE_DIALOG,
                              "CloseConfirmationDialog",
                              &close_confirmation_dialog_info, 0);
  }

  return close_confirmation_dialog_type;
}

static void
close_confirmation_dialog_class_init (CloseConfirmationDialogClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  close_confirmation_dialog_parent_class = g_type_class_peek_parent (klass);

  gobject_class->constructor  = close_confirmation_dialog_constructor;
  gobject_class->set_property = close_confirmation_dialog_set_property;
  gobject_class->get_property = close_confirmation_dialog_get_property;

  g_object_class_install_property (
    gobject_class, PROP_UNSAVED_PAGE,
    g_param_spec_pointer ("unsaved-page",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE));
  g_object_class_install_property (
    gobject_class, PROP_UNSAVED_PAGES,
    g_param_spec_pointer ("unsaved-pages",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE));
  g_object_class_install_property (
    gobject_class, PROP_SELECTED_PAGES,
    g_param_spec_pointer ("selected-pages",
                          "",
                          "",
                          G_PARAM_READABLE));

}

static void
close_confirmation_dialog_init (CloseConfirmationDialog *self)
{
  /* create model for treeview and populate */
  self->store_unsaved_pages = gtk_list_store_new (NUM_COLUMNS,
                                                  G_TYPE_BOOLEAN,  /* save? */
                                                  G_TYPE_POINTER); /* page */

}

/*! \brief Returns the number of pages in the model.
 *  \par Function Description
 *  This function determines the number of pages with unsaved changes
 *  from the model.
 *
 *  \param [in] model The tree model.
 *  \returns The number of pages with unsaved changes.
 */
static int count_pages (GtkTreeModel *model)
{
  GtkTreeIter iter;
  int n_pages;

  gtk_tree_model_get_iter_first (model, &iter);
  for (n_pages = 1;
       gtk_tree_model_iter_next (model, &iter);
       n_pages++);

  return n_pages;
}

/*! \brief Returns the name to use for the given page in the model.
 *  \par Function Description
 *  This function determines the text to be used to identify a
 *  specific page from the model of pages with unsaved changes.
 *
 *  If <B>piter</B> is NULL, the name for the first page of the model
 *  is returned. Otherwise, it returns the name for the page defined
 *  by the pointed iterator.
 *
 *  The returned value must be freed by caller.
 *
 *  \param [in] model The tree model.
 *  \param [in] piter A pointer on a GtkTreeIter of model or NULL.
 *  \returns The name for the page.
 */
static char*get_page_name (GtkTreeModel *model, GtkTreeIter *piter)
{
  GtkTreeIter iter;
  PAGE *page;

  g_return_val_if_fail (GTK_IS_TREE_MODEL (model), NULL);

  if (piter == NULL) {
    gtk_tree_model_get_iter_first (model, &iter);
  } else {
    iter = *piter;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_PAGE, &page,
                      -1);

  return g_path_get_basename (page->page_filename);
}

/*! \brief Sets the contents of the name cell in the treeview of dialog.
 *  \par Function Description
 *  This functions sets the cell of the treeview with the short name
 *  of the page obtained with <B>get_page_name()</B>.
 *
 *  \param [in] tree_column A GtkTreeColumn.
 *  \param [in] cell        The GtkCellRenderer that is being rendered by
 *                        tree_column.
 *  \param [in] tree_model  The GtkTreeModel being rendered.
 *  \param [in] iter        A GtkTreeIter of the current row rendered.
 *  \param [in] data        .
 */
static void
close_confirmation_dialog_set_page_name (GtkTreeViewColumn *tree_column,
                                         GtkCellRenderer   *cell,
                                         GtkTreeModel      *tree_model,
                                         GtkTreeIter       *iter,
                                         void*           data)
{
  char *page_name;

  page_name = get_page_name (tree_model, iter);
  g_object_set (cell,
                "text", page_name,
                NULL);
  g_free (page_name);

}

/*! \brief Callback function for the toggled signal of check box in treeview.
 *  \par Function Description
 *  This functions changes the value of the save column in the model
 *  for the affected row when user toggles the check box in the
 *  treeview.
 *
 *  \param [in] cell_renderer The GtkCellRendererToggle.
 *  \param [in] path          The GtkTreePath to the concerned row in model.
 *  \param [in] user_data     The dialog as user data.
 */
static void
close_confirmation_callback_renderer_toggled (GtkCellRendererToggle *cell_renderer,
                                                     char                 *path,
                                                     void*               user_data)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (user_data);
  GtkTreeModel *model;
  GtkTreeIter iter;
  bool save;

  model = GTK_TREE_MODEL (dialog->store_unsaved_pages);

  /* Removed conditional but don't know why, can not find documentation supporting
   * change so re-instating the conditional to "see what happened" */
  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }
  //gtk_tree_model_get_iter_from_string (model, &iter, path);
  gtk_tree_model_get (model, &iter,
                      COLUMN_SAVE, &save,
                      -1);
  gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_SAVE, (save != TRUE),
                      -1);

}

/*! \brief Adds a treeview to confirmation dialog for selecting of pages.
 *  \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the dialog model of pages with unsaved changes.
 *
 *  The treeview displays the page names with check boxes.
 *
 *  \param [in] dialog The dialog.
 *  \returns A pointer on the GtkVBox to add to dialog.
 */
static GtkWidget*
close_confirmation_dialog_build_page_list (CloseConfirmationDialog *dialog)
{
  GtkWidget *vbox, *scrolled_window, *treeview, *label;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  const char *text;

  /* place the treeview and its caption into their own box */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     8,
                                   NULL));

  /* the list of pages with changes */
  /*  - scrolled window as container for the treeview first */
  scrolled_window = GTK_WIDGET (g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                              /* GtkScrolledWindow */
                             "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                             "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                    "shadow-type",       GTK_SHADOW_IN,
                                                                  NULL));
  /*  - then the treeview */
  /* create model for treeview and populate */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                 "enable-search",   FALSE,
                                 "headers-visible", FALSE,
                     "model", dialog->store_unsaved_pages,
                                                     NULL));

  renderer = gtk_cell_renderer_toggle_new ();

  g_signal_connect (renderer, "toggled",
                    G_CALLBACK (
                      close_confirmation_callback_renderer_toggled),
                      dialog);

  column   = gtk_tree_view_column_new_with_attributes ("Save?",
                                                       renderer,
                                                       "active", COLUMN_SAVE,
                                                       NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  renderer = gtk_cell_renderer_text_new ();
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN, /* GtkTreeViewColumn */
                  "title", _("Name"), NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           close_confirmation_dialog_set_page_name,
                                           NULL, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  gtk_container_add (GTK_CONTAINER (scrolled_window), treeview);

  gtk_box_pack_end (GTK_BOX (vbox), scrolled_window,
                    TRUE, TRUE, 0);

  /* the caption label above the list of pages */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",          0.0,
                                    "yalign",          0.0,
                                    /* GtkLabel */
                                    "wrap",            TRUE,
                                    "mnemonic-widget", treeview,
                                    NULL));
  text = _("S_elect the schematics you want to save:");
  gtk_label_set_text_with_mnemonic (GTK_LABEL (label), text);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), treeview);
  gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);

  return vbox;
}

static GObject*
close_confirmation_dialog_constructor (GType type,
                                       guint n_construct_properties,
                                       GObjectConstructParam *construct_params)
{
  GObject *object;
  CloseConfirmationDialog *dialog;
  GtkWidget *hbox, *image, *vbox, *label;
  GtkTreeIter iter;
  bool ret, single_page;
  char *tmp, *str;
  const char *cstr;

  /* chain up to constructor of parent class */
  object =
    G_OBJECT_CLASS (close_confirmation_dialog_parent_class)->constructor (
      type,
      n_construct_properties,
      construct_params);

  dialog = CLOSE_CONFIRMATION_DIALOG (object);

  g_object_set (dialog, /* GtkDialog */
                "has-separator",     FALSE,
                /* GtkWindow */
                "resizable",         FALSE,
                "skip-taskbar-hint", TRUE,
                /* GtkContainer */
                "border-width",      5,
                NULL);
  g_object_set (GTK_DIALOG (dialog)->vbox, /* GtkBox */
                "spacing", 14,
                NULL);
  g_object_set (GTK_DIALOG (dialog)->action_area, /* GtkBox */
                "spacing",      6,
                /* GtkContainer */
                "border-width", 5,
                NULL);

  /* check if there is one or more than one page with changes */
  ret = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (
                                         dialog->store_unsaved_pages),
                                       &iter);
  if(!ret) {
    s_log_message ("Internal Error: <%s>"
                   "<close_confirmation_dialog_constructor>"
                   "gtk_tree_model_get_iter_first returned NULL, line %d.\n",
                     __FILE__, __LINE__);
    return NULL;
  }
  single_page = !gtk_tree_model_iter_next (GTK_TREE_MODEL (
                                             dialog->store_unsaved_pages),
                                           &iter);

  /* here starts the layout of the dialog */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   /* GtkContainer */
                                   "border-width", 5,
                                   /* GtkBox */
                                   "homogeneous",  FALSE,
                                   "spacing",      12,
                                   NULL));

  /* warning image */
  image = g_object_new (GTK_TYPE_IMAGE,
                        /* GtkMisc */
                        "xalign",    0.5,
                        "yalign",    0.0,
                        /* GtkImage */
                        "stock",     GTK_STOCK_DIALOG_WARNING,
                        "icon-size", GTK_ICON_SIZE_DIALOG,
                        NULL);

  gtk_box_pack_start (GTK_BOX (hbox), image,  FALSE, FALSE, 0);

  /* vertical box on the right hand side of the dialog */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     12,
                                   NULL));

  /* primary label */
  if (single_page) {
    /* single page */
    char *page_name;

    page_name = get_page_name (GTK_TREE_MODEL (dialog->store_unsaved_pages), NULL);
    tmp = g_strdup_printf (
      _("Save the changes to schematic \"%s\" before closing?"), page_name);

    g_free (page_name);
  }
  else {
    /* multi page */
    tmp = g_strdup_printf (
      _("There are %d schematics with unsaved changes. "
        "Save changes before closing?"),
      count_pages (GTK_TREE_MODEL (dialog->store_unsaved_pages)));
  }
  str = g_strconcat ("<big><b>", tmp, "</b></big>", NULL);
  g_free (tmp);
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "use-markup", TRUE,
                                    "label",      str,
                                    NULL));
  g_free (str);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  if (!single_page) {
    /* more than one page with changes, display each page and offer */
    /* the opportunity to save them before exiting */
    gtk_box_pack_start (GTK_BOX (vbox),
                        close_confirmation_dialog_build_page_list (dialog),
                        FALSE, FALSE, 0);
  }

  /* secondary label */
  cstr = _("If you don't save, all your changes will be permanently lost.");
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "label",      cstr,
                                    NULL));

  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), vbox, FALSE, FALSE, 0);


  /* add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("Close without saving"),  GTK_RESPONSE_NO,
                          GTK_STOCK_CANCEL,           GTK_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,             GTK_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);

  /* all done, let's show the contents of the dialog */
  gtk_widget_show_all (hbox);

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox,
                      FALSE, FALSE, 0);

  return object;
}

static void
close_confirmation_dialog_set_property (GObject      *object,
                                        guint         property_id,
                                        const GValue *value,
                                        GParamSpec   *pspec)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);
  GtkTreeIter iter;
  void* data;
  GList *p_current;

  switch(property_id) {
    case PROP_UNSAVED_PAGE:
      data = g_value_get_pointer (value);
      if (data != NULL) {
        /* add single page to model */
        gtk_list_store_append (dialog->store_unsaved_pages,
                               &iter);
        gtk_list_store_set (dialog->store_unsaved_pages,
                            &iter,
                            COLUMN_SAVE, TRUE,
                            COLUMN_PAGE, data,
                            -1);
      }
      break;

    case PROP_UNSAVED_PAGES:
      data = g_value_get_pointer (value);
      /* add set of pages to model */
      for (p_current = (GList*)data;
           p_current != NULL;
           p_current = g_list_next (p_current)) {
        gtk_list_store_append (dialog->store_unsaved_pages,
                               &iter);
        gtk_list_store_set (dialog->store_unsaved_pages,
                            &iter,
                            COLUMN_SAVE, TRUE,
                            COLUMN_PAGE, p_current->data,
                            -1);
      }
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void
close_confirmation_dialog_get_property (GObject    *object,
                                        guint       property_id,
                                        GValue     *value,
                                        GParamSpec *pspec)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);

  switch(property_id) {
    case PROP_SELECTED_PAGES:
      g_value_set_pointer (
        value,
        close_confirmation_dialog_get_selected_pages (dialog));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

/*! \brief Helps building a list of selected page to save.
 *  \par Function Description
 *  This is the <B>GtkTreeModelForeachFunc</B> for function
 *  <B>close_confirmation_dialog_get_selected_pages()</B>.
 *
 *  It builds from the tree model a list of PAGEs for which a save
 *  action has been requested. Each selected page is appended to the
 *  GList pointed by <B>data</B>
 *
 *  \param [in] model The tree model.
 *  \param [in] path  .
 *  \param [in] iter  .
 *  \param [in] data  A pointer on a GList* to fill.
 *  \returns FALSE to continue walking the tree.
 */
static bool get_selected_pages (GtkTreeModel *model,
                                GtkTreePath  *path,
                                GtkTreeIter  *iter,
                                void*     data)
{
  PAGE *page;
  bool save;

  gtk_tree_model_get (model, iter,
                      COLUMN_SAVE, &save,
                      COLUMN_PAGE, &page,
                      -1);

  if (save) {
    if (page != NULL) {
      *(GList**)data = g_list_append (*(GList**)data, page);
    }
    else {
      g_warning("CloseConfirmationDialog found NULL value for page\n");
    }
  }

  return FALSE;
}

/*! \brief Returns a list of the selected pages with changes to save.
 *  \par Function Description
 *  This function returns the pages that the user has selected in the
 *  confirmation dialog.
 *
 *  The returned list must be freed.
 *
 *  \param [in] dialog The dialog.
 *  \returns A GList of selected PAGE* in dialog.
 */
GList*
close_confirmation_dialog_get_selected_pages (CloseConfirmationDialog *dialog)
{
  GList *selected = NULL;

  gtk_tree_model_foreach (GTK_TREE_MODEL (dialog->store_unsaved_pages),
                          (GtkTreeModelForeachFunc)get_selected_pages,
                          &selected);

  return selected;
}

/*! \brief Asks for confirmation before closing a changed page.
 *  \par Function Description
 *  This function asks the user to confirm its closing order for
 *  page <B>page</B> while it still has unsaved changes.
 *
 *  It displays a message dialog asking the user to cancel the
 *  closing, or to discard the changes or to save the changes to a
 *  file.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to close.
 *
 *  \returns TRUE if the page can be closed, FALSE otherwise.
 */
bool
x_dialog_close_changed_page (GSCHEM_TOPLEVEL *w_current, PAGE *page)
{
  GtkWidget *dialog;
  PAGE      *keep_page;
  bool       result;

  g_return_val_if_fail (page != NULL && page->CHANGED, TRUE);

  result = FALSE;

  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-page", page,
                                     NULL));

  /* set default response signal. This is usually triggered by the "Return" key */
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_YES);

  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {

      case GTK_RESPONSE_NO:
        /* close the page, discard changes */
        result = TRUE;
        break;

      case GTK_RESPONSE_YES:
        /* action selected: save */
        keep_page = w_current->toplevel->page_current;
        s_page_goto (w_current->toplevel, page);
        x_window_save_page (w_current,
                            w_current->toplevel->page_current,
                            w_current->toplevel->page_current->page_filename);

        if(!page->CHANGED) {
          if (keep_page != page)
            s_page_goto (w_current->toplevel, keep_page);
            result = TRUE;
        }
        break;

      case GTK_RESPONSE_CANCEL:

        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        /* nothing to do */
        break;
  }
  gtk_widget_destroy (dialog);

  return result;
}

/*! \brief Asks for confirmation before closing a window.
 *  \par Function Description
 *  This function asks the user to confirm closing the given window.
 *
 *  The user is given the possibility to save the pages that currently
 *  have unsaved changes, if any.
 *
 *  It returns TRUE if the user really accepts the close of the
 *  window. Otherwise the user has somehow cancelled and the window
 *  must not be closed.
 *
 *  \param [in] w_current The toplevel environment.
 *  \returns TRUE if the window can be closed, FALSE otherwise.
 */
bool
x_dialog_close_window (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *iter;
  GtkWidget *dialog;
  PAGE *p_current;
  PAGE *keep_page;
  GList *unsaved_pages, *p_unsaved;
  bool return_value = FALSE;

  keep_page = toplevel->page_current;

  unsaved_pages = NULL;

  /* Loop through all the pages */
  for ( iter = geda_list_get_glist( toplevel->pages ); iter != NULL; iter = g_list_next( iter ) ) {
    /* get ptr to a page */
    p_current = (PAGE*)iter->data;
    /* if flag set */
    if (p_current->CHANGED) {
      /* Add to list of un-saved pages */
      unsaved_pages = g_list_append (unsaved_pages, (void*)p_current);
    }
  }

  if (unsaved_pages == NULL) {
    /* no page with unsaved changes, close window */
    return TRUE;
  }

  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-pages", unsaved_pages,
                                     NULL));

  gtk_window_set_transient_for (GTK_WINDOW (dialog),
                                GTK_WINDOW (w_current->main_window));

  g_list_free (unsaved_pages);

  switch ( gtk_dialog_run (GTK_DIALOG (dialog)) ) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* discard changes, ok to close window */
        return_value = TRUE;
        break;

      case GTK_RESPONSE_YES:
        /* action selected: save */
        g_object_get (dialog, "selected-pages", &unsaved_pages, NULL);
        return_value = TRUE;
        for (p_unsaved = unsaved_pages; p_unsaved != NULL; p_unsaved = g_list_next (p_unsaved)) {

          p_current = (PAGE*)p_unsaved->data;

          s_page_goto (toplevel, p_current);

          x_window_save_page (w_current, p_current,
                              w_current->toplevel->page_current->page_filename);

          /* if user cancelled previous, do not close window */
          return_value &= !p_current->CHANGED;
        }
        g_list_free (unsaved_pages);
        break;

      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        return_value = FALSE;
        break;
  }
  gtk_widget_destroy (dialog);

  /* Switch back to the page we were on */
  g_return_val_if_fail (keep_page != NULL, return_value);
  s_page_goto (toplevel, keep_page);

  return return_value;
}

/****************** End of Close Confirmation dialog box ****************/

/*!**** \section Coordinates-Dialog ( Coordinates Systemic-Dialogs) *****/

/*! \brief Response function for the coord dialog
 *  \par Function Description
 *  This function destroys the coord dialog box and does some cleanup.
 */
void x_dialog_coord_dialog_response(GtkWidget *Dialog, int response,
                                    GSCHEM_TOPLEVEL *w_current)
{
  gtk_widget_destroy(Dialog);
  w_current->cowindow = NULL;
  w_current->coord_world = NULL;
  w_current->coord_screen = NULL;
}

/*! \brief Update the coordinates in the coord dialog box.
 *  \par Function Description
 *  This function takes the screen coordinates and prints the
 *  screen and the world coordinates in the coord dialog.
 */
void x_dialog_coord_display_update(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  GtkWidget *Dialog;
  GtkEntry  *screen_entry;
  GtkEntry  *world_entry;

  char *string;
  int world_x, world_y;

  Dialog = w_current->cowindow;
  screen_entry = gtk_object_get_data(GTK_OBJECT(Dialog), "screen");
  world_entry  = gtk_object_get_data(GTK_OBJECT(Dialog), "world");

  string = g_strdup_printf("(%d, %d)", x, y);
  gtk_entry_set_text(screen_entry, string );
  g_free(string);

  SCREENtoWORLD (w_current, x, y, &world_x, &world_y);
  world_x = snap_grid (w_current, world_x);
  world_y = snap_grid (w_current, world_y);

  string = g_strdup_printf("(%d, %d)", world_x, world_y);
  gtk_entry_set_text(world_entry, string );
  g_free(string);
}

static void co_on_entry_activate (GedaEntry *entry, GschemDialog *Dialog)
{
  GSCHEM_TOPLEVEL *w_current;
  const char *str;
  char buffer[36];
  char *x_str, *y_str;
  int   icomma, x, y;
  int   index;
  bool  valid;

  icomma = -1;
  valid  = FALSE;
  x_str  = NULL;
  y_str  = NULL;
  str    = NULL;
  str    = gtk_entry_get_text (GTK_ENTRY(entry));

  if (str) {
    strcpy(&buffer[0], str);
    for (index =0; index < 36; index++) {

      if (!buffer[index])
        break;

      if ( isdigit(buffer[index])) {
        if (!x_str) {
          x_str = &buffer[index];
        }
        else if (!y_str && icomma > 0) {
          y_str = &buffer[index];
        }
      }
      else if ( buffer[index] == ASCII_COMMA) {
        icomma = index;
      }
      else if ( buffer[index] == ASCII_LEFT_PARENTHESIS ||
        buffer[index] == ASCII_RIGHT_PARENTHESIS ) {
        buffer[index] = ASCII_SPACE;
      }
    }
    if ( x_str && y_str) {
      if ( icomma > 0)
        buffer[icomma] = '\0';
      x = atoi(x_str);
      y = atoi(y_str);
      valid = !FALSE;
    }

    if (valid) {
      w_current = Dialog->w_current;
      x_event_set_pointer_position (w_current, x, y);
      if (!w_current->inside_action) {
        o_place_motion (w_current, x, y);
      }
    }
  }
}

/*! \brief Create the coord dialog
 *  \par Function Description
 *  This function creates the coord dialog box.
 */
void x_dialog_coord_dialog (GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  GtkWidget *ThisDialog;
  GtkWidget *vbox;
  GtkWidget *frame;
  GtkWidget *screen_entry;
  GtkWidget *world_entry;
  GdkColor   bg_color;

  ThisDialog = w_current->cowindow;

  if (!ThisDialog) {

    ThisDialog = gschem_dialog_new_with_buttons(_("Coords"),
                         GTK_WINDOW(w_current->main_window),
                         GSCHEM_MODELESS_DIALOG,
                                 IDS_COORDINATES, w_current,
                       GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                                       NULL);

    gtk_window_position (GTK_WINDOW (ThisDialog), GTK_WIN_POS_NONE);

    gtk_container_border_width (GTK_CONTAINER(ThisDialog),
                                DIALOG_BORDER_SPACING);

    bg_color.red   = 0xEEEE;
    bg_color.green = 0xEBEB;
    bg_color.blue  = 0xE7E7;

    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    frame = gtk_frame_new (_("Screen"));
    gtk_container_add(GTK_CONTAINER (vbox), frame);
    g_object_set (frame, "visible", TRUE, NULL);
/*
    event_box = gtk_event_box_new();
    gtk_container_add (GTK_CONTAINER(frame), event_box);
    g_object_set (event_box, "visible", TRUE, NULL);
*/
    screen_entry = geda_visible_entry_new ( DISABLE, DISABLE);
    gtk_entry_set_has_frame (GTK_ENTRY(screen_entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(screen_entry), 0.5);
    geda_entry_widget_modify_color (screen_entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    gtk_container_add(GTK_CONTAINER (frame), screen_entry);

    frame = gtk_frame_new (_("World"));
    gtk_container_add(GTK_CONTAINER (vbox), frame);
    g_object_set (frame, "visible", TRUE, NULL);
/*
    event_box = gtk_event_box_new();
    gtk_container_add (GTK_CONTAINER(frame), event_box);
    g_object_set (event_box, "visible", TRUE, NULL);
*/
    world_entry = geda_visible_entry_new ( DISABLE, DISABLE);
    gtk_entry_set_has_frame (GTK_ENTRY(world_entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(world_entry), 0.5);
    geda_entry_widget_modify_color (world_entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color); 
    gtk_container_add(GTK_CONTAINER (frame), world_entry);
    geda_entry_set_valid_input((GedaEntry*)world_entry, ACCEPT_COORDINATE);

    GSCHEM_HOOKUP_OBJECT ( ThisDialog, screen_entry, "screen");
    GSCHEM_HOOKUP_OBJECT ( ThisDialog, world_entry,  "world");

    g_signal_connect (world_entry, "process-entry",
                      G_CALLBACK (co_on_entry_activate),
                      ThisDialog);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_coord_dialog_response),
                      w_current);

    gtk_widget_show(ThisDialog);


    w_current->cowindow = ThisDialog;
  }

  else { /* window already creatad  */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always update the coords when the dialog is requested */
  x_dialog_coord_display_update(w_current, x, y);
}

/***************** End of coord dialog box **************************/

/***************** Start of misc helper dialog boxes **************/
/*! \brief Raise All Dialogs
 *  \par Function Description
 *  This is a generic function called when the gschem main window is
 * made active after being non-active, such as minmized or behind
 * another application window. Since this could at time, this routine
 * request any open gschem dialog be raised/brought to the foreground.
 */
void x_dialog_raise_all(GSCHEM_TOPLEVEL *w_current)
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
  if(w_current->iwindow) { /* image writer */
    gdk_window_raise(w_current->iwindow->window);
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

/*!* \section Symbol-Changed-Dialog ( Symbol Changed Systemic-Dialogs) **/

/*! \todo Finish function documentation!!!
 *  \brief Annoyance Dialog
 *  \par Function Description
 *
 */
void x_dialog_symbol_changed(GSCHEM_TOPLEVEL* w_current)
{
  GtkWidget* dialog;
  char* refdes_string = NULL;
  char* tmp;

  if (w_current->toplevel->major_changed_refdes) {

    GList* current = w_current->toplevel->major_changed_refdes;
    while (current)
    {
      char *value = (char*) current->data;

      if (!refdes_string)
      {
        refdes_string = g_strdup (value);
      } else {
        tmp = g_strconcat (refdes_string, "\n", value, NULL);
        g_free(refdes_string);
        refdes_string = tmp;
      }

      current = g_list_next(current);
    }

    tmp = g_strconcat (refdes_string,
                       "\n\nBe sure to verify each of these symbols!",
                       NULL);
    g_free(refdes_string);
    refdes_string = tmp;

    dialog = gtk_message_dialog_new ((GtkWindow*) w_current->main_window,
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                        "Major symbol changes detected in refdes:\n\n%s\n",
                                     refdes_string);

    gtk_window_position(GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
    gtk_widget_show(dialog);

    g_signal_connect_swapped (dialog, "response",
                              G_CALLBACK (gtk_widget_destroy),
                              dialog);

    g_free(refdes_string);
  }
}

/****************** End of major symbol changed dialog box **************/

/*!******** \section Invalid-Dialog ( Invalid Systemic-Dialogs) *********/

/*! \brief Validate the input attribute
 *  \par Function Description
 *  This function validates the attribute and if it isn't valid
 *  pops up an error message box.
 *
 *  \param parent The parent window which spawned this dialog box.
 *  \param attribute The attribute to be validated.
 *  \returns TRUE if the attribute is valid, FALSE otherwise.
 */
int x_dialog_validate_attribute(GtkWindow* parent, char *attribute)
{
  GtkWidget* message_box;

  /* validate the new attribute */
  if (!o_attrib_string_get_name_value (attribute, NULL, NULL)) {
      message_box = gtk_message_dialog_new_with_markup (parent,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_CLOSE,
                                  _("<span weight=\"bold\" size=\"larger\">The input attribute \"%s\" is invalid\nPlease correct in order to continue</span>\n\nThe name and value must be non-empty.\nThe name cannot end with a space.\nThe value cannot start with a space."),
                                  attribute);
     gtk_window_set_title(GTK_WINDOW(message_box), _("Invalid Attribute"));
     gtk_dialog_run (GTK_DIALOG (message_box));
     gtk_widget_destroy (message_box);
     return FALSE;
  }
  return TRUE;
}

/************** End of misc support functions for dialog boxes **********/
/*! @} endgroup Systemic-Dialogs */

/*! \defgroup Gschem-General-Dialogs
 *  @{ \par This Group contains General Utility Dialogs
*/

/*!***** \section Message-Dialog ( Message Gschem-General-Dialogs) ******/

/*! \brief General Purpose Message Dialog
 *  \remarks See Utility Macros defined in globals.h
 */
void gschem_message_dialog (const char *msg, gEDA_MessageType context, char *title)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   context,
                                   GTK_BUTTONS_OK,
                                   "%s", msg);

  if(title) {
    gtk_window_set_title(GTK_WINDOW(dialog), _(title));
  }
  else
    gtk_window_set_title(GTK_WINDOW(dialog), _(IDS_MESSEAGE_TITLES[context]));


  gtk_dialog_run (GTK_DIALOG (dialog));

  gtk_widget_destroy (dialog);

}

/***************** End of General message dialog box ********************/

/*!**** \section Confirmation-Dialog (Confirmation General-Dialogs) *****/

/*! \brief General Purpose Confirmation Dialog
 *  \remarks TODO: derive this from gschem dialog class
 */
int gschem_confirm_dialog (const char *msg, gEDA_MessageType context)
{
  GtkWidget *dialog;
  int r;
  //gdk_threads_enter();
  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   context,
                                   GTK_BUTTONS_NONE,
                                   "%s", msg);

  /* add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          GTK_STOCK_NO,         GTK_RESPONSE_NO,
                          GTK_STOCK_CANCEL,     GTK_RESPONSE_CANCEL,
                          GTK_STOCK_YES,        GTK_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);

  gtk_window_set_title(GTK_WINDOW(dialog), _(IDS_MESSEAGE_TITLES[GEDA_MESSAGE_QUESTON]));

  r = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  //gdk_threads_leave();
  return r;
}

/****************** End of General confirm dialog box ********************/

/*!** \section File-Select-File-Dialog ( File Select General-Dialogs) ***/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *   Caller must g_free returned character string.
 */
char *gschem_filesel_dialog (const char *msg, const char *templ, int flags)
{
  GtkWidget *dialog;
  char *result = NULL, *folder, *seed;
  char *title;
  static char *path = NULL;
  static char *shortcuts = NULL;

  /* Default to load if not specified.  Maybe this should cause an error. */
  if (! (flags & (FSB_LOAD | FSB_SAVE))) {
    flags = flags | FSB_LOAD;
  }

  if (flags & FSB_LOAD) {
    title = g_strdup_printf("%s: Open", msg);
    dialog = gtk_file_chooser_dialog_new (_(title),
                                          NULL,
                                          GTK_FILE_CHOOSER_ACTION_OPEN,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GTK_RESPONSE_OK,
                                          NULL);
    /* Since this is a load dialog box, the file must exist! */
    flags = flags | FSB_MUST_EXIST;

  } else {
    title = g_strdup_printf("%s: Save", msg);
    dialog = gtk_file_chooser_dialog_new (_(title),
                                          NULL,
                                          GTK_FILE_CHOOSER_ACTION_SAVE,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GTK_RESPONSE_OK,
                                          NULL);
  }

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_OK,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  /* Pick the current default folder to look for files in */
  if (path && *path) {
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), path);
  }


  /* Pick the current template (*.rc) or default file name */
  if (templ && *templ) {
    if (flags & FSB_SAVE)  {
      gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), templ);
    } else {
      gtk_file_chooser_select_filename (GTK_FILE_CHOOSER (dialog), templ);
    }
  }


  if (shortcuts && *shortcuts) {
    printf ("shortcuts = \"%s\"\n", shortcuts);
    folder = g_strdup (shortcuts);
    seed = folder;
    while ((folder = strtok (seed, ":")) != NULL) {
      gtk_file_chooser_add_shortcut_folder (GTK_FILE_CHOOSER (dialog),
                                            folder, NULL);
      seed = NULL;
    }

    g_free (folder);
  }

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_OK) {
    result = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    folder = gtk_file_chooser_get_current_folder (GTK_FILE_CHOOSER (dialog));
    /*! \bug FIXME
    if (folder && path) {
      dup_string (path, folder);
      g_free (folder);
    }
    */
  }
  gtk_widget_destroy (dialog);

  g_free (title);

  return result;

}

/***************** End of General file select dialog box ****************/
/*! @} endgroup Gschem-General-Dialogs */
