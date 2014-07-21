/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/*!
 * \file x_image.c
 *
 * \brief A dialog box for creating output images.
 *
 */

#include <config.h>

#include <stdio.h>
#include <unistd.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <errno.h>
#include <glib.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "geda_dialog_controls.h"
#include <geda_widgets.h>

#include "x_dialog.h"

#define ThisDialog dialog
#define Switch_Responder switch_responder
//#define DEBUG_IMAGING 1

/** \defgroup Write-Image-Dialog Write-Image Dialog
 *  @{
 *  \ingroup (Standard-Dialogs)
 *
 *  \par This Group contains routines for the Write Image dialog.
 */

#define X_IMAGE_DEFAULT_SIZE_INDEX 2

#define X_IMAGE_SIZE_MENU_NAME "image_size_menu"
#define X_IMAGE_TYPE_MENU_NAME "image_type_menu"

#define X_IMAGE_DEFAULT_TYPE "PNG"

/* Enumerate Control IDs */
typedef enum {
  Extents, EnableColor, InvertImage,
} ControlID;

static WidgetStringData DialogStrings[] = {
  { "ExtentsSwitch",      "     Extents",  "Generate image from schematic Extents or the currently displayed view"},
  { "EnableColorSwitch",  "   Use Color",  "Enable or disable color imaging"},
  { "InvertImageSwitch",  "Invert Image",  "Enable to generate an inverted images"},
  { NULL, NULL, NULL},
};

static char *x_image_sizes[] = {"320x240", "640x480", "800x600", "1200x768",
                                "1280x960", "1600x1200", "3200x2400", NULL};

const char *ImageTypeStrings[] = { "ico", "bmp", "tiff", "jpeg", "png", "eps", "pdf" };

static int last_extents;
static int last_image_size;
static int last_image_type;

static GList *widget_list;

GtkWidget *ExtentsSwitch     = NULL;
GtkWidget *EnableColorSwitch = NULL;
GtkWidget *InvertImageSwitch = NULL;

/*! \brief Create the options of the image size combobox
 *  \par Function Description
 *  This function adds the options of the image size to the given combobox.
 *
 *  \note
 *  This function is only used in this file, there are other create_menus...
 */
static GtkWidget* create_size_menu (void)
{
  int   default_size_index;
  int   i;

  GtkWidget   *combo = geda_combo_box_text_new();

  for (i=0; x_image_sizes[i] != NULL; i++) {
    geda_combo_box_text_widget_append (combo, x_image_sizes[i]);
  }

  if (last_image_size == -1) {
    default_size_index = X_IMAGE_DEFAULT_SIZE_INDEX;
  }
  else {
    default_size_index = last_image_size;
  }

  geda_combo_box_text_widget_set_active(combo, default_size_index);
  g_object_set (G_OBJECT(combo), "visible", TRUE, NULL);
  gtk_widget_set_tooltip_text ( combo, _("Click to select size of the image to be created"));

  return combo;
}

/*! \brief Create the options of the image type combobox
 *
 *  \par Function Description
 *  This function adds the options of the image type to the given
 *  combobox.
 *
 *  \param default_type [in] the combobox to add the options to
 *
 *  \return nothing
 *
 *  \note This function is only used in this file, there are other
 *        create_menus...
 */
static GtkWidget* create_type_menu(IMAGE_TYPES default_type)
{
  GSList *formats;
  GSList *ptr;
  char   *buf;
  int i=0, default_index=-1;

  GtkWidget   *combo = geda_combo_box_text_new();

  /* If we were told use last, and last is NOT set */
  if (default_type == last_image) {
    default_type = last_image_type;       /* default to png */
  }

  formats = gdk_pixbuf_get_formats ();
  ptr     = formats;
  if(ptr) {
    while (ptr) {

      if (gdk_pixbuf_format_is_writable (ptr->data)) {

        /* Get the format description and add it to the menu */
        buf = geda_strdup (gdk_pixbuf_format_get_description(ptr->data));
        geda_combo_box_text_widget_append (combo, buf);

        /* Compare the name with default and store the index */
        buf = geda_strdup (gdk_pixbuf_format_get_name(ptr->data));

#if DEBUG || DEBUG_IMAGING
        fprintf(stderr, "default_type=[%d], buf=[%s]\n",default_type, buf);
#endif

        if (strcasecmp(buf, ImageTypeStrings[default_type]) == 0) {
          default_index = i;
        }

        i++;  /* this is the count of items added to the combo box */
        /* not the total number of pixbuf formats */
        GEDA_FREE(buf);
      }
      ptr = ptr->next;
    }
    g_slist_free (formats);
  }

  geda_combo_box_text_widget_append(combo, "Encapsulated Postscript");
  geda_combo_box_text_widget_append(combo, "Portable Document Format");

  if (default_index < 0) default_index = 4;

  /* Set the default menu */
  geda_combo_box_text_widget_set_active(combo, default_index);
  g_object_set (G_OBJECT(combo), "visible", TRUE, NULL);
  gtk_widget_set_tooltip_text ( combo, _("Click to select the type of image to be created"));
  return combo;
}

/*! \brief File Image File Extension given Description
 *  \par Function Description
 *   Given a gdk-pixbuf image type description, it returns the type,
 *   aka, the file extension to use for the given image description.
 *   Returns the gdk-pixbuf image type, or extension, which has the
 *   given gdk-pixbuf description.
 *
 *  \param descr The gdk-pixbuf image type description.
 *
 *  \return The gdk-pixbuf type, or extension, of the image.
 *
 *  \note.1 The returned value must be freed with GEDA_FREE
 *  \note.2 This function is only used in this file.
 */
static char *x_image_get_type_from_description(char *descr) {

  GSList *formats;
  char   *ptr_descr;
  char   *ret_val;

  ret_val = NULL;

  if (descr != NULL) {

    if (strcmp(descr, _("Encapsulated Postscript")) == 0) {
      ret_val = geda_strdup("eps");
    }
    else if (strcmp(descr, "Portable Document Format") == 0) {
      ret_val = geda_strdup("pdf");
    }
    else {
      formats = gdk_pixbuf_get_formats ();
      while (formats) {
        ptr_descr = gdk_pixbuf_format_get_description (formats->data);
        if (ptr_descr && (strcasecmp(ptr_descr, descr) == 0)) {
          ret_val = geda_strdup(gdk_pixbuf_format_get_name(formats->data));
          break;
        }
        formats = formats->next;
      }
      g_slist_free (formats);
    }
  }
  return ret_val;

}

/*! \brief Update the filename of a file dialog, when the image type has changed.
 *  \par Function Description
 *  Given a combobox inside a file chooser dialog, this function updates
 *  the filename displayed by the dialog, removing the current extension,
 *  and adding the extension of the image type selected.
 *
 *  \param[in] combo      A combobox inside a file chooser dialog, with
 *                        gdk-pixbuf image type descriptions.
 *  \param[in] w_current  the GschemToplevel structure.
 *
 *  \return nothing.
 *
 */
static void
x_image_update_dialog_filename(GtkComboBox     *combo,
                               GschemToplevel *w_current)
{
  GedaToplevel *toplevel       = w_current->toplevel;
  char* image_type_descr   = NULL;
  char *image_type         = NULL;
  char *old_image_filename = NULL;
  char *file_basename      = NULL;
  char *file_name          = NULL ;
  char *new_image_filename = NULL;

  GtkWidget *file_chooser;

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_update_dialog_filename: begin\n");
#endif

  image_type_descr =
  geda_combo_box_text_get_active_text (GEDA_COMBO_BOX_TEXT(combo));
  image_type = x_image_get_type_from_description(image_type_descr);

  GEDA_FREE(image_type_descr);

  /* Get the parent dialog */
  file_chooser = gtk_widget_get_ancestor(GTK_WIDGET(combo),
                                         GTK_TYPE_FILE_CHOOSER);

  /* Get the previous file name. If none, revert to the page filename */
  old_image_filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(file_chooser));
  if (!old_image_filename) {
    old_image_filename = toplevel->page_current->filename;
  }

  /* Get the file name, without extension */
  if (old_image_filename) {
    file_basename = g_path_get_basename(old_image_filename);

    if (g_strrstr(file_basename, ".") != NULL) {
      file_name = g_strndup(file_basename,
          g_strrstr(file_basename, ".") - file_basename);
    }
  }

  /* Add the extension */
  if (file_name) {
    new_image_filename = g_strdup_printf("%s.%s", file_name, image_type);
  }
  else {
    new_image_filename = g_strdup_printf("%s.%s", file_basename, image_type);
  }
  GEDA_FREE(image_type);

  /* Set the new filename */
  if (file_chooser) {
    gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(file_chooser),
        new_image_filename);
  }
  else {
    u_log_message("x_image_update_dialog_filename: No parent file chooser found!.\n");
  }

  GEDA_FREE(file_name);
  GEDA_FREE(file_basename);
  GEDA_FREE(new_image_filename);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_update_dialog_filename: exit\n");
#endif
}

/*! \brief Write eps image file.
 *  \par Function Description
 *  This function writes the encapsulated postscript file, using the
 *  postscript print code from libgeda. Orientation is portrait and
 *  type is extents without margins.
 *
 *  \param w_current [in] the GschemToplevel structure.
 *  \param filename  [in] the image filename.
 *
 *  \return nothing
 */
static
void x_image_write_eps(GschemToplevel *w_current, const char* filename)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int result;
  int w, h, orientation, type;

  w = toplevel->paper_width;
  h = toplevel->paper_height;

  orientation = toplevel->print_orientation;
  type        = toplevel->print_output_type;

  toplevel->paper_width = 0;
  toplevel->paper_height = 0;
  toplevel->print_orientation = PORTRAIT;
  toplevel->print_output_type = EXTENTS_NOMARGINS;
  result = f_print_file (toplevel, toplevel->page_current, filename);
  if (result) {
    u_log_message(_("x_image_write_eps: Unable to write eps file %s.\n"), filename);
  }

  toplevel->paper_width  = w;
  toplevel->paper_height = h;
  toplevel->print_orientation = orientation;
  toplevel->print_output_type = type;
}

/*! \brief Initialize Image Module.
 *  \par Function Description
 *  This function is used to set module level globals that are
 *  use to retain the user choices on a per session basis.
 *  The settings here are not considered important enough to
 *  retain between sessions. We set the values to our default
 *  choices.
 */
void x_image_init (void)
{
  last_extents    = Image_All;
  last_image_size = -1;
  last_image_type = png_image;

  widget_list     = NULL;
}

/*! \brief Write the Image file, with the desired options.
 *  \par Function Description
 *  This function writes the image file, with the options set in the
 *  dialog by the user.
 *
 *  \param [in] w_current       A GschemToplevel object.
 *  \param [in] filename        the image filename.
 *  \param [in] desired_width   the image width chosen by the user.
 *  \param [in] desired_height  the image height chosen by the user.
 *  \param [in] filetype        image filetype.
 *  \param [in] extent          If true then all, else just display .
 *  \param [in] use_print_map   If true then use print color map.
 *  \param [in] invert_color_bw If true invert the image.
 *  \return nothing
 *
 */
void x_image_lowlevel(GschemToplevel *w_current, const char* filename,
    int desired_width, int desired_height, char *filetype, ImageExtent extent,
    bool use_print_map, bool invert_color_bw )
{
  GedaToplevel *toplevel = w_current->toplevel;

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_lowlevel: begin\n");
#endif

  float prop;
  int   width, height;
  int   save_height, save_width;
  int   save_page_left, save_page_right, save_page_top, save_page_bottom;
  int   page_width, page_height, page_center_left, page_center_top;

  GdkPixbuf *pixbuf;
  GError    *err = NULL;

  w_current->image_width  = width  = desired_width;
  w_current->image_height = height = desired_height;

  save_width  = w_current->screen_width;
  save_height = w_current->screen_height;

  w_current->screen_width  = width;
  w_current->screen_height = height;

  save_page_left   = toplevel->page_current->left;
  save_page_right  = toplevel->page_current->right;
  save_page_top    = toplevel->page_current->top;
  save_page_bottom = toplevel->page_current->bottom;

  page_width  = save_page_right  - save_page_left;
  page_height = save_page_bottom - save_page_top;

  page_center_left = save_page_left + (page_width / 2);
  page_center_top  = save_page_top  + (page_height / 2);

  /* Preserve proportions */
  prop = (float)width / height;
  if (((float)page_width / page_height) > prop) {
    page_height = (page_width / prop);
  }
  else {
    page_width = (page_height * prop);
  }

  /* need to do this every time you change width / height */
  x_window_setup_page(w_current, toplevel->page_current,
                      page_center_left - (page_width / 2),
                      page_center_left + (page_width / 2),
                      page_center_top - (page_height / 2),
                      page_center_top + (page_height / 2));

  if (strcmp(filetype, "eps") == 0) { /*WK - catch EPS export case*/
    x_image_write_eps(w_current, filename);
  }
  else {
    if (strcmp(filetype, "pdf") == 0)
      x_print_export_pdf (w_current, filename);
    else {
      pixbuf = x_image_get_pixbuf(w_current, extent, use_print_map, invert_color_bw);
      if (pixbuf != NULL) {
        if (!gdk_pixbuf_save(pixbuf, filename, filetype, &err, NULL)) {
          /* Log the error */
          u_log_message(_("x_image_lowlevel: Unable to write %s file %s. %s\n"), filetype, filename, err->message);
          char *errmsg = g_strdup_printf (_("An error occured while saving image with type %s to filename:\n%s\n\n%s.\n"),
                                            filetype, filename, err->message);
          /* Warn the user */
          titled_pango_error_dialog ( _("<b>Error Writing Imaging.</b>"), errmsg, _("Write Image") );

          /* Free the message string and clear error */
          GEDA_FREE(errmsg);
          g_error_free(err);
          /* Unlink the output file */
          /* It's not safe to unlink the file if there was an error.
             For example: if the operation was not allowed due to permissions,
             the _previous existing_ file will be removed */
          /* unlink(filename); */
        }
        else {
          if (toplevel->image_color == TRUE) {
            u_log_message(_("Wrote color image to [%s] [%d x %d]\n"), filename, width, height);
          }
          else {
            u_log_message(_("Wrote black and white image to [%s] [%d x %d]\n"), filename, width, height);
          }
        }
        GEDA_FREE(filetype);
        if (pixbuf != NULL) {
          GEDA_UNREF(pixbuf);
        }
      }
      else {
        u_log_message(_("x_image_lowlevel: Unable to get pixbuf from gschem's window.\n"));
      }
    }
  }

  w_current->screen_width = save_width;
  w_current->screen_height = save_height;

  /* need to do this every time you change width / height */
  x_window_setup_page(w_current, toplevel->page_current, save_page_left,
                                                         save_page_right,
                                                         save_page_top,
                                                         save_page_bottom);
  o_invalidate_all (w_current);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_lowlevel: exit\n");
#endif
}

/** \defgroup X-Image-Switch-Callback Switch Callback Functions
 *  @{ \par
 *          Support functions for toggle buttons on the export
 *          image dialog.
 */

/*! \brief Toggle switch image & Set sensitivity of other Widgets
 *  \par Function Description: This function changes the images on
 *       EnableColorSwitch and set the sensitivity of all members
 *       of the attach glist, which is the Use Print Color (map)
 *       and Invert B&W only on Color widgets to the same state as
 *       this control.
 */
static void x_image_enable_color (GtkWidget *widget,
                                  GList     *widget_list)
{
  TOGGLE_SWITCH(widget);                  /* Swap-out the switch image */
  bool state;

  state = GET_SWITCH_STATE (widget);

  lambda (GtkWidget *button) {
    if ( GTK_IS_WIDGET(button)) {
      gtk_widget_set_sensitive (button, state); /* gray out button */
      gtk_widget_set_has_tooltip (button, state);
    }
    else {
      g_warning("<x_image_enable_color> Ignoring invalid object\n");
    }
    return FALSE;
  }
  foreach(widget_list)

  return;
}

/*! \brief Toggle switch image & Set invert_bw sensitivity
*   \par Function Description:
* This function changes the images on InvertImageSwitch and
* sets the sensitivity of the invert_bw Check button to the
* same state of this control.
*/
static void x_image_enable_color_bw_invert (GtkWidget *widget,
                                            GtkWidget *button)
{
  TOGGLE_SWITCH(widget); /* Swap-out the switch image */

  bool state;

  state = GET_SWITCH_STATE (widget);
  if (GET_SWITCH_STATE(EnableColorSwitch)) {
    gtk_widget_set_sensitive (button, state);
  }
  else {
    gtk_widget_set_sensitive (button, FALSE);
  }
  if (state && g_list_length (widget_list) < 2) {
    widget_list = g_list_append (widget_list, button);
  }
  else {
    widget_list = g_list_remove (widget_list, button);
  }
  gtk_widget_set_has_tooltip (button, state);
  return;
}
/*! \brief Toggle switch images on the Write Image Dialog
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 *       The functions handles callbacks for only two switches on
 *       this Dialog. This callback doesn't do anything other
 *       than toggle the image so it could be simplified to just
 *       TOGGLE_SWITCH(widget); but leaving stubbed for now for
 *       possible future innovations.
 */
static void switch_responder(GtkWidget *widget, ControlID *Control)
{

  bool state = GET_SWITCH_STATE (widget);
  GtkWidget* SwitchImage = get_geda_switch_image( state);
  gtk_button_set_image(GTK_BUTTON (widget), SwitchImage);

  int WhichOne = (int)(long*) Control;

  switch ( WhichOne ) {
  case Extents:
  case EnableColor: /* Has it's own callback, should not get here! */
  case InvertImage: /* Has it's own callback, should not get here! */
    break;
  default:
    u_log_message("toggle_switch(): UKNOWN SWITCH ID: %d\n", WhichOne);
  }
  return;
}

/** @} END Group X-Image-Switch-Callback Functions */

/*! \brief Display the image file selection dialog.
 *  \par Display the image file selection dialog, allowing the user to
 *  set several options, like image size and image type.
 *  When the user hits "ok", then it writes the image file.
 *
 *  \param[in] w_current    the GschemToplevel structure.
 *  \param[in] default_type the default (last) image type created.
 *
 *  \return nothing
 */
void x_image_setup (GschemToplevel *w_current, IMAGE_TYPES default_type)
{
  GtkWidget *ThisDialog;
  GtkWidget *vbox1;
  GtkWidget *vbox2;
  GtkWidget *vbox3;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *alignment;

  GtkWidget *size_combo;
  GtkWidget *type_combo;
  GtkWidget *switch_vbox;
  GtkWidget *use_print;
  GtkWidget *invert_bw;

  char *cwd;
  char *image_type_descr;
  char *filename;
  char *image_size;
  char *image_type;
  int   width              = w_current->image_width;
  int   height             = w_current->image_height;
  bool  image_color_save   = w_current->toplevel->image_color;
  bool  invert_images_save = w_current->toplevel->invert_images;
  bool  image_extents      = Image_Display;

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_setup: begin\n");
#endif
  /* de-select everything first */
  o_select_unselect_all( w_current );
#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_setup: back o_select_unselect_all\n");
#endif

  gschem_threads_enter();

  /* Create the dialog */
  ThisDialog = gtk_file_chooser_dialog_new (_("Write image..."),
                   GTK_WINDOW(w_current->main_window),
                   GTK_FILE_CHOOSER_ACTION_SAVE,
                   GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                   GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
                   NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
      GTK_RESPONSE_ACCEPT,
      GTK_RESPONSE_CANCEL,
      -1);

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = g_get_current_dir ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  GEDA_FREE (cwd);

  hbox = gtk_hbox_new(FALSE, 0);

  /* Image size selection */
  vbox1 = gtk_vbox_new(TRUE, 0);

  /* Label output image size */
  label = geda_visible_label_new (_("Width x Height"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_misc_set_padding   (GTK_MISC (label), 0, 0);
  gtk_box_pack_start     (GTK_BOX (vbox1), label, FALSE, FALSE, 0);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_setup: Creating image size button\n");
#endif

  /* Label image size selector combo */
  size_combo =  create_size_menu ();
  gtk_box_pack_start (GTK_BOX (vbox1), size_combo, TRUE, TRUE, 0);
  gtk_widget_show(vbox1);

  /* Image type selection */
  vbox2  = gtk_vbox_new(TRUE, 0);
  label  = geda_visible_label_new (_("Image type"));
  gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
  gtk_misc_set_padding (GTK_MISC (label), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox2), label, FALSE, FALSE, 0);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_setup: Creating image type button\n");
#endif

  type_combo = create_type_menu ( default_type);
  gtk_box_pack_start (GTK_BOX (vbox2), type_combo, TRUE, TRUE, 0);

  /* Connect the changed signal to the callback, so the filename
     gets updated every time the image type is changed */
  g_signal_connect (type_combo, "changed",
                    G_CALLBACK(x_image_update_dialog_filename),
                    w_current);

  gtk_widget_show (type_combo);
  gtk_widget_show(vbox2);

  switch_vbox = gtk_vbox_new(FALSE, 0);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_setup: Creating check buttons\n");
#endif

  /* This two check buttons are for color imaging only */
  use_print = gtk_check_button_new_with_label("Use print colors");
  invert_bw = gtk_check_button_new_with_label("B&W only");

  /* So add them to list to pass to the color switch callback */
  widget_list = g_list_append (widget_list, use_print);
  widget_list = g_list_append (widget_list, invert_bw);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_setup: Creating toggle switches\n");
#endif

  /* Create switches, aka check boxes with custom images */
  /* Add Switch widget for "extents or Display using Switch_Responder */
  GTK_SWITCH  (switch_vbox, Extents, 10, last_extents);

  /* Create Toggle Switch widget for Color or B/W with independent callback */
  GEDA_SWITCH ((GTK_WIDGET(ThisDialog)), switch_vbox, EnableColor, 0, image_color_save);

  /* Create Toggle Switch widget for Invert Image using Switch_Responder callback */
  GEDA_SWITCH ((GTK_WIDGET(ThisDialog)), switch_vbox, InvertImage, 0, invert_images_save);

  gtk_widget_show_all(switch_vbox); /* set every widget in container visible */

  /* setup callback for EnableColorSwitch passing ptr to use_print check button */
  GEDA_CALLBACK_SWITCH (EnableColor, x_image_enable_color, widget_list)

  /* setup callback for InvertImageSwitch passing ptr to use_print check button */
  GEDA_CALLBACK_SWITCH (InvertImage, x_image_enable_color_bw_invert, invert_bw)

  /* Add the extra widgets to the dialog*/
  gtk_box_pack_start(GTK_BOX(hbox), vbox1, FALSE, FALSE, 10);
  gtk_box_pack_start(GTK_BOX(hbox), vbox2, FALSE, FALSE, 10);
  gtk_box_pack_start(GTK_BOX(hbox), switch_vbox , FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0, 0.95, 0, 0); /* put near bottom of box */
  gtk_box_pack_start(GTK_BOX(hbox), alignment, TRUE, TRUE, 5);
  g_object_set (alignment, "visible", TRUE, NULL);

  vbox3 = gtk_vbox_new(FALSE, 6); /* move top button up to align with switch */
  gtk_container_add(GTK_CONTAINER(alignment), vbox3);

  /* Now add the check button widgets to vbox inside the alignment */
  gtk_box_pack_start(GTK_BOX(vbox3), use_print, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox3), invert_bw, FALSE, FALSE, 0);

  /* Set initial sensitivity of the check button widgets */
  gtk_widget_set_sensitive (use_print, image_color_save ? TRUE  : FALSE);
  gtk_widget_set_sensitive (invert_bw, image_color_save ? TRUE  : FALSE);

  gtk_widget_set_tooltip_text ( use_print, _("Enable to use the Print color map instead of the display color map"));
  gtk_widget_set_tooltip_text ( invert_bw, _("When enbled for reversed color images, only black and white will be reversed"));

  gtk_widget_show_all(vbox3); /* set every widget in container visible */

  gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(ThisDialog), hbox);

  /* Setup the GtkFileChooser options */
  g_object_set (ThisDialog, "select-multiple", FALSE,
#if ((GTK_MAJOR_VERSION > 2) || ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >=8)))
     /* only in GTK 2.8 */  "do-overwrite-confirmation", TRUE,
#endif
  NULL);

  /* Update the filename */
  x_image_update_dialog_filename(GTK_COMBO_BOX(type_combo), w_current);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "%s: configuring the dialog window\n", __func__);
#endif

  gtk_dialog_set_default_response((GtkDialog*) ThisDialog,
      GTK_RESPONSE_ACCEPT);

  gtk_window_position (GTK_WINDOW (ThisDialog),
      GTK_WIN_POS_MOUSE);

  gtk_container_set_border_width(GTK_CONTAINER(ThisDialog),
      DIALOG_BORDER_WIDTH);

  gtk_box_set_spacing(GTK_BOX(((GtkDialog*)ThisDialog)->vbox),
      DIALOG_V_SPACING);

  gtk_widget_show (ThisDialog);

  if (gtk_dialog_run((GtkDialog*)ThisDialog) == GTK_RESPONSE_ACCEPT) {

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "%s: Dialog GTK_RESPONSE_ACCEPT \n", __func__);
#endif

    /* Retrieve values from the dialog controls */
    image_size =
    geda_combo_box_text_get_active_text (GEDA_COMBO_BOX_TEXT(size_combo));
    last_image_size = gtk_combo_box_get_active(GTK_COMBO_BOX(size_combo));
    sscanf(image_size, "%ix%i", &width, &height);
    GEDA_FREE(image_size);

    image_type_descr =
    geda_combo_box_text_get_active_text (GEDA_COMBO_BOX_TEXT(type_combo));
    last_image_type = gtk_combo_box_get_active(GTK_COMBO_BOX(type_combo));
    image_type = x_image_get_type_from_description(image_type_descr);

    GEDA_FREE(image_type_descr);

    filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(ThisDialog));

    /* Only the Extents switch/button use a local variable */
                         image_extents = GET_SWITCH_STATE(ExtentsSwitch);
    w_current->toplevel->image_color   = GET_SWITCH_STATE(EnableColorSwitch);
    w_current->toplevel->invert_images = GET_SWITCH_STATE(InvertImageSwitch);

     /* Save the user's choices */
    last_extents = image_extents;  /* saved per session */

    /* these are restored between sessions */
    w_current->image_width  = width;
    w_current->image_height = height;

    int use_print_map;
    int invert_color_bw;

    if (w_current->toplevel->image_color) {
      use_print_map   = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(use_print));
      invert_color_bw = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(invert_bw));
    }
    else { /* it should not matter */
      use_print_map   = FALSE;
      invert_color_bw = FALSE;
    }

    /* Call low-level to do the work */
    x_image_lowlevel(w_current, filename, width, height, image_type, image_extents,
                     use_print_map, invert_color_bw);
  }

  gtk_widget_destroy (ThisDialog);
  gschem_threads_leave();
  g_list_free (widget_list);
  widget_list = NULL;

}

/*! \brief Convert Image to Grey Scale
 *  \par Function Description
 *  Replace color pixels with shades of grey and optionaly inverts the image
 *
 *  \param pixbuf Pointer to GdkPixbuf to be processed
 *  \param invert boolean, if true - do invert pixels
 */
static void x_image_convert_to_greyscale(GdkPixbuf *pixbuf, bool invert)
{
  unsigned char *pixels, *p, new_value;
  int width, height, rowstride, n_channels;
  int i, j;

  n_channels = gdk_pixbuf_get_n_channels (pixbuf);

  if (n_channels != 3)
  {
    return;
  }

  if (gdk_pixbuf_get_colorspace (pixbuf) != GDK_COLORSPACE_RGB)
  {
    return;
  }

  if (gdk_pixbuf_get_bits_per_sample (pixbuf) != 8)
  {
    return;
  }

  width  = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels    = gdk_pixbuf_get_pixels (pixbuf);

  for (j = 0; j < height; j++)
  {
    for (i = 0; i < width; i++)
    {
      p = pixels + j * rowstride + i * n_channels;

      /* new_value = 0.3 * p[0] + 0.59 * p[1] + 0.11 * p[2]; */
      new_value = p[0] * 0.21 + p[1] * 0.71 + p[2] * 0.07; /* weighted average */
      p[0] = invert ? 255 - new_value : new_value;
      p[1] = invert ? 255 - new_value : new_value;
      p[2] = invert ? 255 - new_value : new_value;
    }
  }
}
/*! \brief Invert Color Image
 *  \par Function Description
 *   This function inverts color image, sometime refered to as reversing
 *   the polarity of the image. Optionaly, only near Black and near White
 *   are reversed.
 *
 *  \param pixbuf  Pointer to GdkPixbuf to be processed
 *  \param bw_only boolean, if true - only invert the B&W pixels
 */
static void x_image_invert_color_buffer(GdkPixbuf *pixbuf, bool bw_only)
{
  unsigned char *pixels, *p;
  int width, height, rowstride, n_channels;
  int i, j;

  n_channels = gdk_pixbuf_get_n_channels (pixbuf);

  if (n_channels != 3)
  {
    return;
  }

  if (gdk_pixbuf_get_colorspace (pixbuf) != GDK_COLORSPACE_RGB)
  {
    return;
  }

  if (gdk_pixbuf_get_bits_per_sample (pixbuf) != 8)
  {
    return;
  }

  width  = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels    = gdk_pixbuf_get_pixels (pixbuf);

  for (j = 0; j < height; j++)
  {
    for (i = 0; i < width; i++)
    {
      p = pixels + j * rowstride + i * n_channels;

      if (bw_only) {
        if (p[0] + p[1] + p[2] < 4) {             /* if near black */
          p[0] = 255;                             /* make white    */
          p[1] = 255;
          p[2] = 255;
        }
        else if (p[0] + p[1] + p[2] > 0x251 ) {  /* if near white */
          p[0] = 0;                              /* make black    */
          p[1] = 0;
          p[2] = 0;
        }
      }
      /* This reverses polarity of the image, in this case color */
      else {                                     /* invert color */
        p[0] = 255 - p[0];
        p[1] = 255 - p[1];
        p[2] = 255 - p[2];
      }
    }
  }
}
/*! \brief Retreive Pixel Buffer for Imaging
 *  \par Function Description
 *  The entire top-level is copied, including
 */
GdkPixbuf *x_image_get_pixbuf (GschemToplevel *w_current, ImageExtent extent,
                               bool use_print_map, bool invert_color_bw)
{
  GschemToplevel *new_w_current;
         GedaToplevel *toplevel;

  GArray       *color_map;
  GdkPixbuf    *pixbuf;
  GdkRectangle  rect;
  EdaRenderer  *renderer;
  PangoLayout  *layout;
  PangoContext *context;

  int origin_x, origin_y, bottom, right;
  int size_x, size_y, s_right, s_left, s_top,s_bottom;

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_get_pixbuf: begin\n");
#endif

  new_w_current = malloc(sizeof(GschemToplevel));
  toplevel      = malloc(sizeof(GedaToplevel));

  if ( !new_w_current || !toplevel) {
    char *errmsg = strerror(errno);
    fprintf(stderr, "Error: <x_image_get_pixbuf> could not allocate memory resources: %s\n", errmsg);
    error_dialog("Could not allocate memory resources; %s, maybe you should try saving next",
                  errmsg);
    return NULL;
  }

  /* Got memory allocation so copy w_current struct and work with it */
  memcpy(new_w_current, w_current, sizeof(GschemToplevel));

  /* Do a copy of the toplevel struct and work with it */
  memcpy(toplevel, w_current->toplevel, sizeof(GedaToplevel));

  new_w_current->toplevel = toplevel;

  /* Do zoom extents to get entire schematic in the window if imaging All */
  if (extent == Image_All)
    i_zoom_world_extents (new_w_current,
                     toplevel->page_current->_object_list,
                     I_PAN_DONT_REDRAW);

  WORLDtoSCREEN (new_w_current, toplevel->page_current->right,
                                toplevel->page_current->left, &s_right, &s_left);
  WORLDtoSCREEN (new_w_current, toplevel->page_current->bottom,
                                toplevel->page_current->top,  &s_bottom, &s_top);

  size_x = s_left - s_right;
  size_y = s_bottom - s_top;

  size_x = new_w_current->image_width;
  size_y = new_w_current->image_height;

  new_w_current->window   = gdk_pixmap_new (w_current->window, size_x, size_y, -1);
  new_w_current->drawable = new_w_current->window;
  new_w_current->cr       = gdk_cairo_create (new_w_current->window);
  layout                  = pango_cairo_create_layout (new_w_current->cr);

  context = pango_layout_get_context (layout);

  if (use_print_map) {
    color_map = x_color_get_print_color_map();
  }
  else {
    color_map = x_color_get_display_color_map();
  }

  /* Note that since we copied w_current, w_current->renderer is pointing at a
   * valid renderer, we are replacing that pointer here with a new instances */
  renderer = g_object_new (EDA_TYPE_RENDERER,
                           "cairo-context", new_w_current->cr,
                           "pango-context", context,
                           "color-map",     color_map,
                           "render-flags", TRUE ? EDA_RENDERER_FLAG_HINTING : 0,
                           NULL);

  new_w_current->renderer = renderer;

  /* if B&W then all colors in map except BACKGROUND_COLOR were set to black
   * but marks and enpoints may not be using the stock map so ...*/
  if (toplevel->image_color == FALSE) {
    GdkColor black;
    gdk_color_parse ( "black", &black);
    eda_renderer_set_junction_color (renderer, &black);
    eda_renderer_set_net_endpoint_color (renderer, &black);
  }

  new_w_current->grid_mode = 0;
  new_w_current->renderer->text_origin_marker = FALSE;

  new_w_current->screen_width  = new_w_current->image_width;
  new_w_current->screen_height = new_w_current->image_height;

  origin_x = origin_y = 0;
  right    = size_x;
  bottom   = size_y;

  rect.x      = origin_x;
  rect.y      = origin_y;
  rect.width  = right - origin_x;
  rect.height = bottom - origin_y;

  o_redraw_rects (new_w_current, &rect, 1);

  /* Get the pixbuf */
  pixbuf = gdk_pixbuf_get_from_drawable (NULL,new_w_current->drawable, NULL,
                                         origin_x, origin_y, 0, 0,
                                         right-origin_x,
                                         bottom-origin_y);

  if (toplevel->image_color == FALSE) {
    x_image_convert_to_greyscale(pixbuf, toplevel->invert_images == TRUE);
  }
  else if (toplevel->invert_images == TRUE) {
    x_image_invert_color_buffer(pixbuf, invert_color_bw);
  }

  /* Cleanup resources */

  eda_renderer_destroy (new_w_current->renderer);
  g_array_free (color_map, TRUE);

  if (new_w_current->cr != NULL) cairo_destroy (new_w_current->cr);

  if (new_w_current->window != NULL) {
    GEDA_UNREF (new_w_current->window);
  }

  free (toplevel);
  free (new_w_current);

#if DEBUG || DEBUG_IMAGING
  fprintf(stderr, "x_image_get_pixbuf: exit\n");
#endif

  return(pixbuf);
}

/** @} endgroup Write-Image-Dialog */

#undef ThisDialog
#undef Switch_Responder