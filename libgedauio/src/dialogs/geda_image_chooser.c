/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: geda_image_chooser.c
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

#include "../../include/geda_image_chooser.h"
#include "../../include/geda_file_filter.h"
#include "../../include/geda_container.h"
#include "../../include/geda_marshal.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

#define ChooseClass GedaImageChooserClass

/**
 * \brief GedaImageChooser - An Image File Chooser Dialog
 * \par
 * A GedaImageChooser is a variant of GtkFileChooser.
 *
 * \defgroup GedaImageChooser Image Chooser Dialog
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

static GtkFileChooserDialogClass *geda_image_chooser_parent_class = NULL;

/* List of pointers to GedaImageChooser instances */
static GList *list_of_choosers = NULL;

static GtkEntry *chooser_entry;

static GedaFileFilterDataDef filter_data[] = {
    GEDA_FILTER_NONE,
    GEDA_FILTER_PNG,
    GEDA_FILTER_JPG,
    GEDA_FILTER_GIF,
    GEDA_FILTER_BMP,
    GEDA_FILTER_ICO,
    GEDA_FILTER_TIF,
    GEDA_FILTER_XPM,
    GEDA_FILTER_PNM,
    GEDA_FILTER_RAS,
    GEDA_FILTER_IMAGES,
    GEDA_NO_MORE_FILTERS
};

/*! \brief Creates filter for Geda Image Chooser.
 *  \par Function Description
 *  This function adds file filters to <B>filechooser</B>.
 *
 *  \param [in] filechooser The image chooser to apply filter to.
 */
static void
geda_image_chooser_setup_filters (GtkFileChooser *filechooser)
{
  GedaFileFilterDataDef *data;

  for (data = filter_data; data->name != NULL; data++) {

    GtkFileFilter *filter;
    int i;

    filter = gtk_file_filter_new();

    gtk_file_filter_set_name(filter, data->name);

    for (i = 0; data->pattern[i]; i++) {
      const char *ext = data->pattern[i];
      gtk_file_filter_add_pattern (filter, ext);
    }
    g_object_set_data( G_OBJECT(filter), "id", (void*)(long)(data->id));
    gtk_file_chooser_add_filter (filechooser, filter);
  }
}

static void
geda_image_chooser_restore_filter (GtkWidget *chooser)
{
  GError     *err   = NULL;
  EdaConfig  *cfg   = eda_config_get_user_context();
  const char *group = IMAGE_CHOOSER_CONFIG_GROUP;
  const char *key   = IMAGE_CHOOSER_CONFIG_FILTER;

  int filter_index;

  /* Attempt to restore the ImageChooser filter users preference */
  filter_index = eda_config_get_integer (cfg, group, key, &err);

  if (err != NULL) {
    g_clear_error (&err);
    filter_index = FILTER_IMAGES;
  }

  geda_image_chooser_set_filter(chooser, filter_index);
}

/*! \brief Get Current Filter Index of a Geda Image Chooser
 *  \par Function Description
 *  This function return the current filters index of a #GedaImageChooser
 *  dialog.
 *
 *  \param [in] widget The image chooser widget.
 *
 *  \returns filter index integer value
 */
int geda_image_chooser_get_filter (GtkWidget *widget)
{
  return (GEDA_IMAGE_CHOOSER(widget)->filter_index);
}

/*! \brief Set the Filter Index of a Geda Image Chooser
 *  \par Function Description
 *  This function sets the filters index of a #GedaImageChooser
 *  dialog.
 *
 *  \param [in] widget The image chooser widget.
 *  \param [in] index  The new index of the filter.
 *
 */
void geda_image_chooser_set_filter (GtkWidget *widget, int index)
{
  if (GEDA_IS_IMAGE_CHOOSER(widget)) {

    GedaImageChooser *chooser = (GedaImageChooser*)widget;

    if(chooser->handler) {
      g_signal_handler_block(chooser->filter_button, chooser->handler);
      gtk_combo_box_set_active(GTK_COMBO_BOX(chooser->filter_button), index);
      g_signal_handler_unblock(chooser->filter_button, chooser->handler);
    }
    else {
      gtk_combo_box_set_active(GTK_COMBO_BOX(chooser->filter_button), index);
    }
    chooser->filter_index = index;
  }
  else {
    BUG_MSG("chooser is not a GedaImageChooser");
  }
}

static void geda_image_chooser_filter_changed(GedaImageChooser *chooser)
{
    /* Do nothing here */
}

static void
chooser_update_filter_index(GtkWidget *button, GedaImageChooser *chooser)
{
  chooser->filter_index = gtk_combo_box_get_active (GTK_COMBO_BOX(button));
  g_signal_emit (chooser, chooser_signals[FILTER_CHANGED], 0);
}

/*! \brief Updates the visibility of the preview widget.
 *  \par Function Description
 *  This function updates the visibility based on the state of the
 *  checkbox toggle button object.
 *
 *  \param [in] checkbox  The GtkToggleButton widget
 *  \param [in] user_data Is the ImageChooser widget
 */
static void
chooser_preview_enabler (GtkToggleButton *checkbox, void *user_data)
{
  GtkWidget        *widget  = user_data;
  GedaImageChooser *chooser = user_data;

  chooser->preview_enabled = gtk_toggle_button_get_active (checkbox);
  g_object_set (widget, "preview-widget-active", chooser->preview_enabled, NULL);
}

static void
chooser_adjust_size (GtkAdjustment *adjustment, void *user_data)
{
  GedaImageChooser *chooser = user_data;
  GtkImage         *preview = GTK_IMAGE (chooser->preview);
  GError           *err     = NULL;
  char             *filename;

  filename = gtk_file_chooser_get_preview_filename (GTK_FILE_CHOOSER(chooser));

  if (filename != NULL && !g_file_test (filename, G_FILE_TEST_IS_DIR)) {

    GdkPixbuf *pixbuf;
    int        size;

    size   = chooser->preview_size = (int)gtk_adjustment_get_value(adjustment);
    pixbuf = gdk_pixbuf_new_from_file_at_size (filename, size, size, &err);

    if (err != NULL) {
      fprintf(stderr, "<%s> file error: %s\n", filename, strerror( errno ));
      g_clear_error (&err);
    }
    else { /* update preview */

      if (!chooser->zoom_mode) {

        gtk_image_set_from_pixbuf (preview, pixbuf);

        /* chooser->previous_size is initialized to -1 */
        if (chooser->previous_size < 0) {

          GtkAllocation *allocation;

          allocation = geda_get_widget_allocation(preview);

          chooser->previous_size = allocation->width;
        }

        if (size < chooser->previous_size) {
          if (!chooser->mouse_down) {
            gtk_widget_set_size_request ((GtkWidget*)preview, size, -1);
          }
          gtk_range_set_update_policy (GTK_RANGE (chooser->slider), GTK_UPDATE_DISCONTINUOUS);
        }
        else {
           gtk_widget_set_size_request ((GtkWidget*)preview, size, -1);
           gtk_range_set_update_policy (GTK_RANGE (chooser->slider), GTK_UPDATE_CONTINUOUS);
        }
        chooser->previous_size = size;
      }
      else { /* is zoom mode */

        if (chooser->zoom_mode > 0) {

          /* the first time the adjustment is change in zoom mode we
           * fix the preview pane size to the current allocation */

          int width  = ((GtkWidget*)preview)->allocation.width;
          int height = ((GtkWidget*)preview)->allocation.height;

          gtk_widget_set_size_request ((GtkWidget*)preview, width, height);
          chooser->zoom_mode = -1;
        }

        gtk_image_set_from_pixbuf (preview, pixbuf);
      }
    }
  }
}

/*! \brief Updates the preview widget.
 *  \par Function Description
 *  This function updates the preview: if the preview is active and a
 *  filename has been given, it opens the file and displays
 *  the contents. Otherwise the display will be a blank page.
 *
 *  \param [in] widget    The ImageChooser dialog widget
 *  \param [in] user_data ImageChooser preview, aka GtkImage, widget
 */
static void
chooser_update_preview (GtkFileChooser *widget, void *user_data)
{
  GedaImageChooser *chooser = GEDA_IMAGE_CHOOSER(widget);

  /* preview_enabled is used to by-pass updates instead of disconnecting
   * the signal handler because of flaws in gobject/gsignal.c one@2580,
   * which issues unnecessary noise claiming the handler is not connected.
   * Another solution would be to not use buggy gobject signals and just
   * call this function directly when the treeview selection changes.
   */
  if (chooser->preview_enabled) {

    char *filename;

    filename = gtk_file_chooser_get_preview_filename (widget);

    if (filename != NULL && !g_file_test (filename, G_FILE_TEST_IS_DIR)) {

      GdkPixbuf *pixbuf;
      GError    *err;
      int        size;

      err    = NULL;
      size   = chooser->preview_size;
      pixbuf = gdk_pixbuf_new_from_file_at_size (filename, size, size, &err);

      if (err != NULL) {
        /* silently clear the error, got no pic and don't care what happened */
        g_clear_error (&err);
      }
      else { /* update preview */

        gtk_image_set_from_pixbuf (GTK_IMAGE(user_data), pixbuf);
        gtk_widget_set_size_request (GTK_WIDGET(user_data), size, -1);
      }
    }
  }
}

static GtkWidget *popup_menu;
typedef enum  { ZoomMode,
                SizeMode,
                MidSize,
                MinSize,
                MaxSize,
                PreviewOff

}  IDS_PV_Popup_items; /* Enumerators to reference the string below: */

static char *popup_items[]={ "Zoom mode",
                             "Size mode",
                             "Default size",
                             "Max size",
                             "Min size",
                             "Preview Off",
};
static char *popup_tips[]={  "Set silder to zoom mode",
                             "Set silder to size mode",
                             "Set image size to the default size",
                             "Set image size to maximum",
                             "Set image size to minimum",
                             "Turn the preview pane off",
};

/*! \brief Callback Handler for Popup Mouse Context Menu
 *
 *  \par Function Description
 * This function calls the appropriate functions to process request
 * from the mouse menu. This function receives a pointer to enumerated
 * integer value for the menu item that was selected.
 *
 *  \param [in] widget    is menu item widget
 *  \param [in] selection pointer to enumerated menu selection
 */
static int popup_activated(GtkWidget *widget, IDS_PV_Popup_items *selection)
{
    GedaImageChooser *chooser = GEDA_OBJECT_GET_DATA(widget, "chooser");

    int WhichItem = (int)(long) selection;

    switch ( WhichItem ) {

      case ZoomMode:
        chooser->zoom_save = chooser->preview_size;
        chooser->zoom_mode = TRUE;
        gtk_range_set_update_policy (GTK_RANGE (chooser->slider), GTK_UPDATE_CONTINUOUS);
        break;

      case SizeMode:
        chooser->preview_size = chooser->zoom_save;
        chooser->zoom_mode    = FALSE;
        gtk_adjustment_set_value(chooser->adjustment, chooser->preview_size);
        gtk_window_set_resizable (GTK_WINDOW(chooser), TRUE);
        gtk_range_set_update_policy (GTK_RANGE (chooser->slider), GTK_UPDATE_DISCONTINUOUS);
        break;

      case MidSize:
        gtk_adjustment_set_value(chooser->adjustment, chooser->default_preview_size);
        break;

      case MinSize:
        gtk_adjustment_set_value(chooser->adjustment, chooser->min_preview_size);
        break;

      case MaxSize:
        gtk_adjustment_set_value(chooser->adjustment, chooser->max_preview_size);
        break;

      case PreviewOff:
        chooser->preview_enabled = FALSE;
        g_object_set (chooser, "preview-widget-active", FALSE, NULL);
        break;

      default:
        BUG_IMSG("unknown menu ID\n", WhichItem);
    } /* End Switch WhichItem */

    gtk_widget_destroy(popup_menu);
    popup_menu = NULL;
    return (TRUE);
}

/*! \brief Create and Setup Popup Mouse Menu for Preview
 *
 *  \par Function Description
 * This function is called when the user right clicks on a handlebox.
 * The function sets senitivty on menu choices based on the handlebox
 * position and the state of the containing toolbar.
 *
 *  \param [in] chooser The ImageChooser dialog widget
 */
static GtkWidget *build_menu(GedaImageChooser *chooser)
{
  GtkWidget   *menu;
  GtkTooltips *tooltips;

  int i;

  tooltips = gtk_tooltips_new ();
  menu     = geda_menu_new();

  for (i=0; i < (sizeof(popup_items)/sizeof(popup_items[0])) ; i++) {

    GtkWidget *item = geda_menu_item_new_with_label(_(popup_items[i]));

    gtk_tooltips_set_tip (tooltips, item, _(popup_tips[i]), NULL);
    g_object_set_data((GObject*)item, "chooser", chooser);

    g_signal_connect(item,"activate",
                    (void*) popup_activated,
                    (void*) (long) i);

    gtk_widget_set_sensitive(GTK_WIDGET(item), TRUE);
    gtk_widget_set_can_focus(GTK_WIDGET(item), TRUE);

    switch (i) {
      case ZoomMode:
        if (chooser->zoom_mode) { /* Disable is already active */
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case SizeMode:
        if (!chooser->zoom_mode) { /* Disable is already active */
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case MidSize:
        if (chooser->preview_size == chooser->default_preview_size) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case MinSize:
        if (chooser->preview_size == chooser->min_preview_size) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case MaxSize:
         if (chooser->preview_size == chooser->max_preview_size) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case PreviewOff:
        break;
      }
      g_object_set (item, "visible", TRUE, NULL);
      geda_menu_append (menu, item);
    }
    return (menu);
}

/*! \brief Mouse Button Call Back for Preview Event Area
 *
 *  \par Function Description
 * This function check mouse botton press and when the 3rd button
 * is released the build_menu function is called to create the mouse
 * menu.
 *
 *  \param [in] widget     The event box widget the user "right-clicked" on
 *  \param [in] event      Mouse event record
 *  \param [in] user_data  GedaImageChooser object.
 */
static int
On_mouse_button_press(GtkWidget *widget, GdkEventButton *event, void *user_data)
{
  GedaImageChooser *chooser = user_data;

  GdkModifierType mods;

  gdk_window_get_pointer (gtk_widget_get_window(widget), NULL, NULL, &mods);

  if (mods&GDK_BUTTON3_MASK) {

    if (popup_menu) {
      gtk_widget_destroy(GTK_WIDGET(popup_menu));
      popup_menu = NULL;
    }

    popup_menu = build_menu(chooser);
    /* Display the menu we just created */
    geda_menu_popup(GEDA_MENU(popup_menu), NULL, NULL, NULL, NULL,
                    event->button, event->time);
  }
  return (FALSE);
}
/*! \brief HandleBar Mouse Button Call Back
 *
 *  \par Function Description
 * This function check mouse botton press and when the 3rd button
 * is released the build_menu function is called to create the mouse
 * menu.
 *
 *  \param [in] widget     The event box widget the user "right-clicked" on
 *  \param [in] event      Mouse event record
 *  \param [in] user_data  GedaImageChooser object.
 */
static int
On_adjust_button_press(GtkWidget *widget, GdkEventButton *event, void *user_data)
{
  GedaImageChooser *chooser = user_data;

  if (event->button == 1) {
    chooser->mouse_down = TRUE;
  }
  return (FALSE);
}
/*! \brief HandleBar Mouse Button Call Back
 *
 *  \par Function Description
 * This function check mouse botton press and when the 3rd button
 * is released the build_menu function is called to create the mouse
 * menu.
 *
 *  \param [in] widget     The event box widget the user "right-clicked" on
 *  \param [in] event      Mouse event record
 *  \param [in] user_data  GedaImageChooser object.
 */
static int
On_adjust_button_release(GtkWidget *widget, GdkEventButton *event, void *user_data)
{
  GedaImageChooser *chooser = user_data;

  if (event->button == 1) {
    chooser->mouse_down = FALSE;
  }
  return (FALSE);
}
/*! \brief Adds a Preview to the Image Chooser.
 *  \par Function Description
 *  This function adds a preview section to a <B>GedaImageChooser</B>.
 *
 *  The <B>Preview</B> object is inserted in a frame and alignment
 *  widget for accurate positionning.
 *
 *  Other widgets can be added to this preview area for example to
 *  enable/disable the preview. Currently, the preview is always
 *  active.
 *
 *  Function <B>chooser_callback_update_preview()</B> is
 *  connected to the signal 'update-preview' of <B>GedaImageChooser</B>
 *  so that it redraws the preview area every time a new file is
 *  selected.
 *
 *  \param [in] chooser The Image chooser to add the preview to.
 *  \param [in] state   boolean preview-widget-active property setting
 *  \param [in] size    Widget pixel size to request, i.e. the current setting
 */
static GtkWidget*
chooser_add_preview (GtkWidget *chooser, bool state, int size)
{
  GtkWidget     *alignment, *frame, *preview;
  GtkAdjustment *adjustment;
  GtkWidget     *slider;
  GtkWidget     *hbox,  *vbox;
  GtkWidget     *ebox;

  /* Add our preview widget to the dialog */
  vbox = gtk_vbox_new(FALSE, 0);

  ebox = gtk_event_box_new();
  g_object_set (ebox, "visible", TRUE, NULL);

  frame = g_object_new (GTK_TYPE_FRAME,
                        "label", _("Preview"),
                        NULL);

  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            "right-padding", 5,
                            "left-padding", 5,
                            "xscale", 0.0,
                            "yscale", 0.0,
                            "xalign", 0.5,
                            "yalign", 0.5,
                            NULL);

  preview = g_object_new (GTK_TYPE_IMAGE, NULL);

  geda_container_add (alignment, preview);
  geda_container_add (frame, alignment);
  geda_container_add (ebox, frame);
  geda_container_add (vbox, ebox);
  gtk_widget_show_all (frame);


  hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (hbox, "visible", TRUE, NULL);

  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            "right-padding", 5,
                            "left-padding", 5,
                            "xscale", 1.0,
                            "yscale", 0.1,
                            "xalign", 0.5,
                            "yalign", 0.1,
                            NULL);

  g_object_set (alignment, "visible", TRUE, NULL);

  slider = gtk_hscale_new_with_range (100.0, 1000.0, 100.0);
  g_object_set (slider, "visible", TRUE, NULL);
  g_object_get(slider, "adjustment", &adjustment, NULL);
  gtk_adjustment_set_value(adjustment, (double)size);
  gtk_scale_set_value_pos (GTK_SCALE(slider), GTK_POS_BOTTOM);

  geda_container_add (alignment, slider);
  geda_container_add (hbox, alignment);
  gtk_box_pack_end (GTK_BOX (vbox), hbox, TRUE, FALSE, 0);

  (GEDA_IMAGE_CHOOSER(chooser))->slider = slider;
  (GEDA_IMAGE_CHOOSER(chooser))->adjustment = adjustment;

  g_object_set (chooser, "use-preview-label",     FALSE,
                         "preview-widget",        vbox,
                         "preview-widget-active", state,
                                                  NULL);

  gtk_widget_set_size_request(preview, size, -1);

  /* connect callback to update preview image */
  GEDA_SIGNAL_CONNECT (chooser, "update-preview",
                       G_CALLBACK (chooser_update_preview),
                       preview);

  /* connect callback to update preview image */
  GEDA_SIGNAL_CONNECT (adjustment, "value-changed",
                       G_CALLBACK (chooser_adjust_size),
                       chooser);

  gtk_widget_set_events (vbox,
                         GDK_BUTTON_PRESS_MASK |
                         GDK_BUTTON_RELEASE_MASK);

  GEDA_SIGNAL_CONNECT(ebox, "button_press_event",
                      On_mouse_button_press, chooser);

  GEDA_SIGNAL_CONNECT(slider, "button_press_event",
                      On_adjust_button_press, chooser);

  GEDA_SIGNAL_CONNECT(slider, "button_release_event",
                      On_adjust_button_release, chooser);

  return preview;
}

/* GtkFileChooserDialog does not expose the combo button used for filter
 * selection, we should make our own chooser dialog, but until then, we
 * search all widgets looking for a combobox, the one used for the filter
 * is the only combobox used in the dialog */
static void FixGtkCrap(GtkWidget *widget, void *self)
{
  if (GTK_IS_COMBO_BOX(widget)) {
    (GEDA_IMAGE_CHOOSER(self))->filter_button = widget;
  }
  else if (GTK_IS_CONTAINER(widget)) {
     geda_container_forall (widget, FixGtkCrap, self);
  }
}

static void look_for_entry(GtkWidget *widget, void *self)
{
  if (GTK_IS_ENTRY(widget)) {
    chooser_entry = (GtkEntry*)widget;
  }
  else if (GTK_IS_CONTAINER(widget)) {
     geda_container_forall (widget, look_for_entry, self);
  }
}

static void
geda_image_chooser_find_entry (GtkWidget *chooser)
{
  GList   *children, *iter;

  /* Get all objects inside the dialog */
  children = geda_container_get_children (chooser);

  for (iter = children; iter; iter = iter->next) {

    if (GTK_IS_CONTAINER(iter->data)) {

      geda_container_forall (iter->data, look_for_entry, chooser);

      if (chooser_entry != NULL) {
        break;
      }
    }
  }
  g_list_free (children);
}

static GObject*
geda_image_chooser_constructor (GType                  type,
                                unsigned int           n_properties,
                                GObjectConstructParam *properties)
{
  GObject          *obj;
  GList            *children, *iter;
  GedaImageChooser *chooser;
  GtkWidget        *vbox;

  /* Chain up to the parent constructor */
  obj = G_OBJECT_CLASS (geda_image_chooser_parent_class)->constructor (type, n_properties, properties);

  gtk_dialog_set_has_separator (GTK_DIALOG(obj), TRUE);

  chooser = GEDA_IMAGE_CHOOSER(obj);

  vbox = gtk_dialog_get_content_area(GTK_DIALOG(chooser));

  /* Get all object inside the contents area of the dialog */
  children = geda_container_get_children (vbox);

  /* For each container in the contents area to call look for combo box */
  for (iter = children; iter; iter = iter->next) {
    if (GTK_IS_CONTAINER(iter->data)) {
      geda_container_forall (iter->data, FixGtkCrap, obj);
      if (chooser->filter_button) {
        break;
      }
    }
  }

  g_list_free (children);

  chooser->default_preview_size = DEFAULT_CHOOSER_PREVIEW_SIZE;
  chooser->min_preview_size     = MIN_CHOOSER_PREVIEW_SIZE;
  chooser->max_preview_size     = MAX_CHOOSER_PREVIEW_SIZE;
  chooser->zoom_mode            = FALSE;

  GtkWidget  *widget;
  GtkWidget  *preview;
  GtkWidget  *hbox;
  GtkWidget  *chechbox;
  EdaConfig  *cfg;
  const char *group;
  const char *key;
  bool        enable;
  int         size;

  group    = IMAGE_CHOOSER_CONFIG_GROUP;
  cfg      = eda_config_get_user_context();
  widget   = GTK_WIDGET(obj);
  key      = IMAGE_CHOOSER_CONFIG_PREVIEW;
  enable   = eda_config_get_boolean (cfg, group, key, NULL);
  key      = IMAGE_CHOOSER_CONFIG_PVSIZE;
  size     = eda_config_get_integer (cfg, group, key, NULL);
  size     = size > 100 ? size : DEFAULT_CHOOSER_PREVIEW_SIZE;
  preview  = chooser_add_preview(widget, enable, size);
  hbox     = gtk_hbox_new(FALSE, 5);

  chechbox = gtk_check_button_new_with_label (_("Preview"));
  gtk_toggle_button_set_active ((GtkToggleButton*)chechbox, enable);
  gtk_widget_set_tooltip_text(chechbox, _("Active to enable the preview pane"));
  g_object_set (chechbox, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX(hbox), chechbox, FALSE, FALSE, 0);

  /* connect callback to update */
  g_signal_connect (chechbox, "toggled",
                    G_CALLBACK (chooser_preview_enabler),
                    widget);
 /* Add our extra widget to the dialog */
  geda_image_chooser_set_extra_widget (widget, hbox);

  chooser->extra = hbox;
  chooser->preview = preview;
  chooser->preview_size = size;
  chooser->preview_enabled = enable;
  chooser->preview_chechbox = chechbox;

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
static void geda_image_chooser_finalize (GObject *object)
{
  list_of_choosers = g_list_remove(list_of_choosers, object);

  if (!g_list_length(list_of_choosers)) {
    g_list_free(list_of_choosers);
    list_of_choosers = NULL;
  }

  chooser_entry = NULL;

  (G_OBJECT_CLASS (geda_image_chooser_parent_class))->finalize (object);
}

/*! \brief GObject property getter function for a GedaImageChooser Object
 *
 *  \par Function Description
 *  Getter function for GedaImageChooser's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_image_chooser_get_property (GObject *object, unsigned int  property_id,
                                GValue  *value,  GParamSpec   *pspec)
{
  GedaImageChooser *chooser = GEDA_IMAGE_CHOOSER(object);

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

/*! \brief GObject property setter for a GedaImageChooser Object
 *
 *  \par Function Description
 *  Setter function for GedaImageChooser's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_image_chooser_set_property (GObject *object, unsigned int  property_id,
                                 const    GValue *value,  GParamSpec   *pspec)
{
  GedaImageChooser *chooser = GEDA_IMAGE_CHOOSER(object);

  switch (property_id) {

    case PROP_FILTER_INDEX:
      geda_image_chooser_set_filter((GtkWidget*)chooser, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*! \brief GedaImageChooser "geometry_restore" class method handler
 *  \par Function Description
 *  Restore dialog's last position and size under the passed group name.
 *
 *  \param [in] chooser    The #GedaImageChooser Dialog to restore geometry.
 *  \param [in] group_name The group name in the key file to find the data under.
 */
static void
geda_image_chooser_geometry_restore (GedaImageChooser *chooser, char *group_name)
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

/*! \brief GedaImageChooser "geometry_save" class method handler
 *  \par Function Description
 *  Save the dialog's current position and size under the passed group name.
 *
 *  \param [in] chooser    The #GedaImageChooser Dialog to save the geometry.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void
geda_image_chooser_geometry_save (GedaImageChooser *chooser, char *group_name)
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
  char *group = IMAGE_CHOOSER_CONFIG_GROUP;

  /* Hack to fix BUG in GtkFileChooserDialog */
  gtk_window_set_resizable (GTK_WINDOW(widget), FALSE);

  /* Let Gtk show the window */
  GTK_WIDGET_CLASS (geda_image_chooser_parent_class)->show (widget);

  gtk_window_set_resizable (GTK_WINDOW(widget), TRUE);

  g_signal_emit (GEDA_IMAGE_CHOOSER (widget),
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
  GedaImageChooser *chooser = GEDA_IMAGE_CHOOSER(widget);
  EdaConfig        *cfg     = eda_config_get_user_context();
  char             *group   = IMAGE_CHOOSER_CONFIG_GROUP;
  const char       *key;
  int               index;

  g_signal_emit (GEDA_IMAGE_CHOOSER (widget),
                 chooser_signals[ GEOMETRY_SAVE ], 0, group);

  key = IMAGE_CHOOSER_CONFIG_PREVIEW;
  eda_config_set_boolean (cfg, group, key, chooser->preview_enabled);

  key = IMAGE_CHOOSER_CONFIG_PVSIZE;
  eda_config_set_integer (cfg, group, key, chooser->preview_size);

  /* Retrieve the active filter index from the dialog */
  index = geda_image_chooser_get_filter (widget);

  key = IMAGE_CHOOSER_CONFIG_FILTER;

  /* Preserve the ImageChooser filter users preference */
  eda_config_set_integer (cfg, group, key, index);

  /* Let Gtk unmap the window */
  GTK_WIDGET_CLASS (geda_image_chooser_parent_class)->unmap (widget);
}

/*! \brief Type class initializer for GedaImageChooser
 *  \par Function Description
 *  Type class initializer for GedaImageChooser. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 * \param [in] class A GedaImageChooserClass Object
 * \param [in] data  A GedaImageChooser data structure
 */
static void
geda_image_chooser_class_init (void *class, void *data)
{
  GParamSpec     *params;
  GedaType        type;

  ChooseClass    *chooser_class   = (ChooseClass*) class;
  GObjectClass   *gobject_class   = (GObjectClass*) class;
  GtkWidgetClass *widget_class    = (GtkWidgetClass*) class;

  gobject_class->get_property     = geda_image_chooser_get_property;
  gobject_class->set_property     = geda_image_chooser_set_property;
  gobject_class->constructor      = geda_image_chooser_constructor;
  gobject_class->finalize         = geda_image_chooser_finalize;

  chooser_class->filter_changed   = geda_image_chooser_filter_changed;
  chooser_class->geometry_save    = geda_image_chooser_geometry_save;
  chooser_class->geometry_restore = geda_image_chooser_geometry_restore;

  widget_class->show              = show_handler;
  widget_class->unmap             = unmap_handler;

  geda_image_chooser_parent_class = g_type_class_peek_parent (class);

  params = g_param_spec_int ("filter-index",
                           _("filter index"), /* nick name */
                           _("Set or retrieve index of the filter combo"), /* hint / blurb */
                              FILTER_PNG,       /* Min value */
                              FILTER_IMAGES,    /* Max value */
                              FILTER_IMAGES,    /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_FILTER_INDEX, params);

  type = geda_image_chooser_get_type();

  /**
   * GedaImageChooser::filter-changed:
   * Chooser: The chooser on which the signal is emitted
   *
   * The  GedaImageChooser::filter-changed signal is emitted when the user
   * changes the selection of the filter combo text box.
   */
  chooser_signals[FILTER_CHANGED]     = g_signal_new ("filter-changed", type,
                                                      G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                                                      G_STRUCT_OFFSET (GedaImageChooserClass,
                                                                       filter_changed),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      geda_marshal_VOID__VOID,
                                                      G_TYPE_NONE, 0);

  chooser_signals[ GEOMETRY_RESTORE ] = g_signal_new ("geometry-restore", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GedaImageChooserClass,
                                                                       geometry_restore ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      geda_marshal_VOID__STRING,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);

  chooser_signals[ GEOMETRY_SAVE ]    = g_signal_new ("geometry-save", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GedaImageChooserClass,
                                                                       geometry_save ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      geda_marshal_VOID__STRING,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);
}

/*! \brief Initialize new GedaImageChooser data structure instance.
 *
 *  \par Function Description
 *  This function is call after the GedaImageChooserClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaImageChooser data structure
 * \param [in] class     A GedaImageChooserClass Object
 */
static void
geda_image_chooser_instance_init (GTypeInstance *instance, void *class)
{
  GedaImageChooser *self = (GedaImageChooser*)instance;

  chooser_entry       = NULL;
  self->filter_button = NULL;
  self->previous_size = -1;

  /* Append instance to list of valid GedaImageChooser objects */
  list_of_choosers = g_list_append(list_of_choosers, instance);
}

/*! \brief Function to retrieve GedaImageChooser's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaImageChooser Type identifier. When
 *  first called, the function registers a #GedaImageChooser in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaImageChooser and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaImageChooser.
 */
GedaType geda_image_chooser_get_type (void)
{
  static volatile GedaType geda_image_chooser_type = 0;

  if (g_once_init_enter (&geda_image_chooser_type)) {

    static const GTypeInfo info = {
      sizeof(GedaImageChooserClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_image_chooser_class_init,   /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaImageChooser),
      0,                               /* n_preallocs         */
      geda_image_chooser_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaImageChooser");
    type   = g_type_register_static (GTK_TYPE_FILE_CHOOSER_DIALOG,
                                     string, &info, 0);

    g_once_init_leave (&geda_image_chooser_type, type);
  }

  return geda_image_chooser_type;
}

/*! \brief Instantiate a New Geda Image Chooser Dialog
 *  to provide a GedaImageChooser equivelant of the convenience function
 *  gtk_file_chooser_dialog_new(...)
 *
 *  \par Function Description
 *  Convenience function which creates a GedaImageChooser with buttons and options.
 *
 *  \param [in]  parent         GtkWindow Widget which will parent this dialog
 *  \param [in]  chooser_action The #ImageChooserAction to use when setting up the dialog
 *
 *  \return  The GedaImageChooser created.
 */
GtkWidget*
geda_image_chooser_new (void *parent, ImageChooserAction chooser_action)
{
  GtkWidget *widget;

  widget = g_object_new (geda_image_chooser_get_type(),
                         "action", chooser_action,
                         "select-multiple",
                         (chooser_action == IMAGE_CHOOSER_ACTION_OPEN),
                         NULL);

  if (widget) {

     GtkDialog        *dialog;
     GedaImageChooser *chooser;

     const char *second_button_text;
     const char *title = NULL;

    chooser = (GedaImageChooser*)widget;
    dialog  = (GtkDialog*)widget;

    switch (chooser_action) {
      case IMAGE_CHOOSER_ACTION_OPEN:
        second_button_text =  _("_Open");
        title = _("Open...");
        break;

      case IMAGE_CHOOSER_ACTION_SAVE:
        second_button_text = _("_Save");
        title = _("Save As...");
        break;

      case IMAGE_CHOOSER_ACTION_SELECT_FOLDER:
        second_button_text = _("_Select Folder...");
        title = _("Select Folder");
        break;

      default:
        second_button_text = _("OOPS");
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

    gtk_window_set_title ((GtkWindow*)dialog, title);

    if (parent != NULL) {
      gtk_window_set_transient_for((GtkWindow*)dialog, (GtkWindow*)parent);
    }

    geda_image_chooser_setup_filters (GTK_FILE_CHOOSER (dialog));
    geda_image_chooser_restore_filter (widget);

    if (chooser->filter_button) {
      chooser->handler = g_signal_connect_after(G_OBJECT(chooser->filter_button),
                                                "changed",
                                                G_CALLBACK (chooser_update_filter_index),
                                                chooser);
    }

    /* set default response signal, usually triggered by the "Return" key */
    gtk_dialog_set_default_response (dialog, GEDA_RESPONSE_ACCEPT);
  }

  return widget;
}

/*!
 * \brief Check if an object is a GedaImageChooser
 * \par Function Description
 *  Determines if \a chooser is valid by verifying \a chooser
 *  is included in the hash table of GedaImageChooser objects.
 *
 * \return TRUE if \a chooser is a valid GedaImageChooser
 */
bool is_a_geda_image_chooser (GedaImageChooser *chooser)
{
  if (chooser && list_of_choosers) {
    return g_list_find(list_of_choosers, chooser) ? TRUE : FALSE;
  }
  return FALSE;
}

static GtkWidget *
geda_image_chooser_dialog_new_valist (const char         *title,
                                      void               *parent,
                                      ImageChooserAction  action,
                                      const char         *first_button_text,
                                      va_list             varargs)
{
  GtkDialog  *result;
  const char *button_text = first_button_text;

  result = g_object_new (geda_image_chooser_get_type(),
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

/*! \brief Create a New GedaImageChooser specifying Buttons
 *  \par Function Description
 *   Creates a new #GedaImageChooser. This function is analogous to
 *   gtk_dialog_new_with_buttons().
 *
 * \param [in] title  Title of the dialog, or %NULL
 * \param [in] parent Transient parent of the dialog, or %NULL
 * \param [in] action Open or save mode for the dialog
 * \param [in] first_button_text (allow-none): stock ID or text to go in the first button, or %NULL
 * \param [in] ... response ID for the first button, then additional (button, id) pairs, ending with %NULL
 *
 * \return a new #GedaImageChooser
 *
 */
GtkWidget*
geda_image_chooser_dialog_new_full (const char        *title,
                                    void              *parent,
                                    ImageChooserAction action,
                                    const char        *first_button_text, ...)
{
  GtkWidget *result;
  va_list varargs;

  va_start (varargs, first_button_text);
  result = geda_image_chooser_dialog_new_valist (title, parent, action,
                                                first_button_text,
                                                varargs);
  va_end (varargs);

  return result;
}

/*! \brief Get Geda Image Chooser Entry Widget
 *  \par Function Description
 *  This function returns a pointer to the internal GtkEntry widget
 *
 *  \param [in] widget The image chooser widget.
 *
 *  \returns GtkEntry object
 */
GtkEntry *geda_image_chooser_get_entry (GtkWidget *widget)
{
  if (chooser_entry == NULL) {
    geda_image_chooser_find_entry (widget);
  }
  return chooser_entry;
}

/*!
 * \brief Retrieve the Image Chooser Entry Text
 * \par Function Description
 *  Returns a copy of the characters in the entry or NULL if
 *  the pointer does not reference a valid GedaImageChooser
 *  or the entry is empty. If a string is returned then the
 *  string must be freed.
 *
 *  \param [in] widget The despicable image chooser widget.
 *
 *  \returns GtkEntry object
 */
char*
geda_image_chooser_get_entry_text(GtkWidget *widget)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {

    GtkEntry *entry = geda_image_chooser_get_entry(widget);

    if (GTK_IS_ENTRY(entry)) {

      if (gtk_entry_get_text_length (entry)) {
        return geda_strdup (gtk_entry_get_text(entry));
      }
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
  return NULL;
}

/*!
 * \brief Get the Filename from a GedaImageChooser
 * \par Function Description
 *  Gets the filename for the currently selected file in the file selector.
 *  The filename is returned as an absolute path. If multiple files are
 *  selected, one of the filenames will be returned at random. If the image
 *  chooser is in folder mode, this function returns the selected folder.
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 *
 * \returns filename string, which should be freed, or NULL.
 */
char*
geda_image_chooser_get_filename(GtkWidget *widget)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    return gtk_file_chooser_get_filename((GtkFileChooser*)widget);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
  return NULL;
}

/*!
 * \brief Set the GedaImageChooser Filename
 * \par Function Description
 *  Sets filename as the current filename for the image chooser,
 *  changes to the file’s parent folder and selects the file in
 *  list; all other files will be unselected. If the chooser is
 *  in GTK_FILE_CHOOSER_ACTION_SAVE mode, the file’s base name
 *  will also appear in the dialog’s file name entry.
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 * \param [in] name    Pointer to the filename to set as current
 */
void
geda_image_chooser_set_filename (GtkWidget *widget, const char *name)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    gtk_file_chooser_set_filename((GtkFileChooser*)widget, name);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

/*!
 * \brief Get a list filenames from a GedaImageChooser
 * \par Function Description
 *  Lists all the selected files and subfolders in the current folder.
 *  The returned names are full absolute paths. If files in the current
 *  folder cannot be represented as local filenames they will be ignored
 *  The returned list should be released using g_slist_free, and each
 *  filenames with g_free().
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 *
 * \returns a GSList containing the filenames of all selected files and
 *          subfolders in the current folder.
 */
GSList*
geda_image_chooser_get_filenames(GtkWidget *widget)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    return gtk_file_chooser_get_filenames((GtkFileChooser*)widget);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
  return NULL;
}

/*!
 * \brief Retrieve the current folder from a GedaImageChooser
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
char*
geda_image_chooser_get_current_folder(GtkWidget *widget)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    return gtk_file_chooser_get_current_folder((GtkFileChooser*)widget);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
  return NULL;
}

/*!
 * \brief Set the current folder of a GedaImageChooser
 * \par Function Description
 *  Sets the current folder for chooser from a local filename. The
 *  user will be shown the full contents of the current folder, plus
 *  user interface elements for navigating to other folders.
 *
 * \param [in] widget  Pointer to a hideous GtkFileChooser widget
 * \param [in] folder  Pointer to the location to set as current
 */
void
geda_image_chooser_set_current_folder (GtkWidget *widget, const char *folder)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    gtk_file_chooser_set_current_folder((GtkFileChooser*)widget, folder);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

/*!
 * \brief Set the current name of a GedaImageChooser
 * \par Function Description
 * Sets the current name in the file selector, as if entered by the user.
 * Note that the name passed in here is a UTF-8 string rather than a filename.
 * This function is meant for such uses as a suggested name in a "Save As..."
 * dialog. You can pass "Untitled.jpg" or a similarly suitable suggestion for
 * the name.
 *
 * If you want to preselect a particular existing file, you should use
 * geda_file_chooser_set_filename instead.
 *
 * \param [in] widget Pointer to a hideous GtkFileChooser widget
 * \param [in] name   Pointer to the name to set as current
 */
void
geda_image_chooser_set_current_name (GtkWidget *widget, const char *name)
{
  if (GTK_IS_FILE_CHOOSER(widget)) {
    gtk_file_chooser_set_current_name((GtkFileChooser*)widget, name);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

/*!
 * \brief Append Extra widget to an Image Chooser Dialog
 *  Appends an extra \a child widget to an image chooser dialog.
 *
 * \sa geda_image_chooser_set_extra_widget geda_image_chooser_prepend_extra
 */
void
geda_image_chooser_append_extra (GtkWidget *dialog, GtkWidget *child)
{
  if (GEDA_IMAGE_CHOOSER(dialog)) {
    GedaImageChooser *chooser = (GedaImageChooser*)dialog;
    geda_container_add (chooser->extra, child);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

/*!
 * \brief Prepend Extra widget to an Image Chooser Dialog
 * \par Function Description
 * Prepends extra a \a child widget to an image chooser.
 *
 * \sa geda_image_chooser_set_extra_widget
 */
void
geda_image_chooser_prepend_extra (GtkWidget *dialog, GtkWidget *child)
{
  if (GEDA_IMAGE_CHOOSER(dialog)) {

    GedaImageChooser *chooser;
    GtkContainer     *container;
    GList            *children;
    GList            *iter;

    chooser   = (GedaImageChooser*)dialog;
    container = (GtkContainer*)chooser->extra;
    children  = gtk_container_get_children (container);

    for (iter = children; iter; iter = iter->next) {

      GtkWidget *widget = iter->data;

      g_object_ref(widget);

      gtk_container_remove (container, widget);
    }

    gtk_container_add (container, child);

    for (iter = children; iter; iter = iter->next) {

      GtkWidget *widget = iter->data;

      gtk_container_add (container, widget);

      g_object_unref(widget);
    }

    g_list_free(children);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

/*!
 * \brief Retrieve the extra widget of an Image Chooser Dialog
 * \par Function Description
 *  Normally the extra widget of a GedaImageChooser is the hbox
 *  containing the enable preview check button.
 */
GtkWidget*
geda_image_chooser_get_extra_widget(GtkWidget *chooser)
{
  if (GEDA_IMAGE_CHOOSER(chooser)) {
    return ((GedaImageChooser*)chooser)->extra;
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
  return NULL;
}

/*!
 * \brief Set Extra widgets to an Image Chooser Dialog
 * \par Function Description
 * Sets extra widget of an image chooser to provide options not present
 * in the default design.  For example, to add a toggle button to give
 * users the option to open a image in black-and-white mode.
 *
 * example:
 * \code
 *   GtkWidget *toggle;
 *
 *   ...
 *
 *   toggle = gtk_check_button_new_with_label ("Open image B&W");
 *   gtk_widget_show (toggle);
 *   gtk_file_chooser_set_extra_widget (image_chooser, toggle);
 * \endcode
 *
 * A container such as a GtkBox can be used to set more than one extra
 * widget in the image chooser, set the container as the whole extra
 * widget.
 *
 * \note The default extra widget of an image chooser is the hbox
 *       containing the enable preview check button.
 */
void
geda_image_chooser_set_extra_widget (GtkWidget *hideous, GtkWidget *extra)
{
  if (GTK_IS_FILE_CHOOSER(hideous)) {
    gtk_file_chooser_set_extra_widget((GtkFileChooser*)hideous, extra);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

/*!
 * \brief Programmatically Activate or Deactivate the Image Chooser Preview
 * \par Function Description
 *  Set the preview checkbon on the chooser dialog to the given \a state
 *  and this will trigger callback chooser_preview_enabler, which sets
 *  the "preview-widget-active" property.
 */
void geda_image_chooser_set_preview_active (GtkWidget *widget, bool state)
{
  if (GEDA_IS_IMAGE_CHOOSER(widget)) {
    GedaImageChooser *chooser = (GedaImageChooser*)widget;
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(chooser->preview_chechbox), state);
  }
  else {
    BUG_MSG ("Operative is not a GedaImageChooser");
  }
}

#undef ChooseClass
/** @} end group GedaImageChooser */
