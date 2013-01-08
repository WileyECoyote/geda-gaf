/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <unistd.h>
#include <sys/stat.h>
#include <gtk/gtk.h>

#include <geda_toolbars.h>
#include <gschem.h>           /* include Gscehm specific headers  */

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define TOOLBAR_GEOMETRY_STORE "gschem-toolbar-geometry"

static GSList *can_paste;
static GSList *can_undo;
static GSList *can_redo;
static GSList *have_pages;
static GSList *complex_object;
static GSList *some_object;    /* List of widgets on toolbars to set if some object is selected */
static GSList *text_object;

typedef enum { tb_Add,  tb_Attribute, tb_Edit,
               tb_Page, tb_Standard,  tb_Zoom

} ID_GSCHEM_Toolbar;

typedef enum  { new, open, save, save_as, print, export_pdf, cut, copy, paste,
                undo, redo, configure, selector, deselector, add_component,
                add_net, add_bus, add_attribute, add_text, add_line, add_box,
                add_circle, add_arc, add_pin, insert_pic, prev_page, next_page,
                new_page, page_manager, down_schematic, down_symbol,
                hierarchy_up,  comp_doc, view_redraw, zoom_pan, zoom_box,
                zoom_extents, zoom_in, zoom_out, zoom_all, edit_copy,
                multi_copy, move, rotate, mirror, edit_line, edit_color,
                edit_fill, lock, unlock, attach, detach, show_value, show_name,
                show_both, visibilty, find_text, hide_text, show_specific,
                auto_number
} IDS_GSCHEM_Toolbar;

const char* IDS_Toolbar_Names[] = {
  "add-bar", "Attribute", "Edit", "Page", "Standard", "Zoom", /* ToolBar Name Strings*/
  NULL
};

static ToolbarStringData ToolbarStrings[] = {
   /* Standard Toolbar*/
  { "new_button",       "New",          "Create a new file", GSCHEM_MAP(NEW)},
  { "open_button", 	"Open", 	"Open file",         GSCHEM_MAP(OPEN)},
  { "save_button", 	"Save",		"Save file",         GSCHEM_MAP(SAVE)},
  { "save_as_button", 	"Save As", 	"Save the file to different name or location", "Private"},

  { "print_button",      "Print",       "Open the Print Dialog",  "Private"}, /* Most do a ZAP print from bar */
  { "export_pdf_button", "Export PDF",  "Export to PDF document", "Private"},

  { "cut_button", 	"Cut", 		"Cut selection to the clipboard",     "Private"},
  { "copy_button",  	"Copy", 	"Copy selection to the clipboard",    "Private"},
  { "paste_button",	"Paste",	"Paste selection from the clipboard", "Private"},

  { "undo_button",      "Undo",         "Undo the last operation",       GSCHEM_MAP(UNDO)},
  { "redo_button",      "Redo",         "Redo the last undo",            GSCHEM_MAP(REDO)},
  { "configure",        "Config",       "Set configuration preferences", GEDA_MAP(TOOLS)},

  { "select_button",    "Select",     "Select mode",  GEDA_MAP(SELECT)},
  { "deselect_button",  "Deselect",   "Unselect everthing", GEDA_MAP(SELECT)},

  { "add_component_button", "Component", "Add Component...\nSelect library and component from list, move the mouse into main window, click to place\nRight mouse button to cancel", "Private"},
  { "add_net_button",       "Nets",      "Add Nets mode\nRight mouse button to cancel", "gschem_net.xpm"},
  { "add_bus_button",       "Bus",       "Add Buses mode\nRight mouse button to cancel", "gschem_bus.xpm"},
  { "add_attribute_button", "Attrib",    "Add Attribute...", "Private"},
  { "add_text_button",      "Text",      "Add Text...", "Private"},

  /* Add Toolbar */
  { "add_line_button",    "Line",     "Add line", "Private"},
  { "add_box_button",     "Box",      "Add Box", "Private"},
  { "add_circle_button",  "circle",   "Add Circle", "Private"},
  { "add_arc_button",     "Arc",      "Add Arc", "Private"},
  { "add_pin_button",     "Pin",      "Add Pin", "Private"},
  { "add_picture_button", "Picture",  "Insert an image", "Private"},

  /* Page Toolbar */
  { "prev_page_button",    "Prev",    "Show the previous page", "Private"},
  { "next-page_button",    "next",    "Show the next page", "Private"},
  { "new_page_button",     "New",     "Create a new page", "Private"},
  { "page_manager_button", "Manage",  "Open the Page Manager", "Private"},
  { "down_schematic_button", "Down",  "Lower schematic hierarchy", "Private"},
  { "down_symbol_button",    "Down",  "Lower symbol hierarchy", "Private"},
  { "hierarchy_up_button",   "Up",    "Elevate hierarchy", "Private"},
  { "comp_doc_button",       "Spec",  "View component documentation", "Private"},

  /* Zoom Toolbar */
  { "view_redraw_button",  "Redraw",  "Redraw current display", GEDA_MAP(VIEW_REDRAW)},
  { "zoom_pan_button",     "Pan",     "Zoom Pan",        GEDA_MAP(ZOOM_PAN)},
  { "zoom_box_button",     "Window",  "Zoom Window",     GEDA_MAP(ZOOM_BOX)},
  { "zoom_extents_button", "Extents", "Zoom to extents", GEDA_MAP(ZOOM_EXTENTS)},
  { "zoom_in_button",      "In",      "Zoom In",         GEDA_MAP(ZOOM_IN)},
  { "zoom_out_button",     "Out",     "Zoom Out",        GEDA_MAP(ZOOM_OUT)},
  { "zoom_all_button",     "All",     "Zoom to Limits",  GEDA_MAP(ZOOM_LIMITS)},

  /* Edit Toolbar */
  { "edit_copy_button",    "Copy",    "Copy Objects", "Private"},
  { "multi_copy_button",   "Multi",   "Make Multible Copies", "Private"},
  { "edit_move_button",    "Move",    "Move Objects",   "Private"},
  { "edit_rotate_button",  "Rotate",  "Rotate Objects", "Private"},
  { "edit_mirror_button",  "Mirror",  "Mirror objects", "Private"},
  { "edit_line_button",    "Line",    "Edit line type", "Private"},
  { "edit_color_button",   "Color",   "Change Colors",  "Private"},
  { "edit_fill_button",    "Fill",    "Edit Fill type", GEDA_MAP(MESH)},
  { "edit_lock_button",    "Lock",    "Lock Objects",   "Private"},
  { "edit_unlock_button",  "Unlock",  "Unlock Objects", "Private"},

  /* Attribute Toolbar */
  { "attach_button",        "promote",  "Attach the selected attribute", "Private"},
  { "detach_button",        "demote",   "Dettach the selected attribute", "Private"},
  { "show_value_button",    "value",    "Set selected value visible only", GEDA_MAP(VALUE)},
  { "show_both_button",     "name",     "Set selected name visible only"},
  { "name_value_button",    "both",     "Set selected name and value visible", "Private"},
  { "visibilty_button",     "visible",  "Toggle Visibilty", GEDA_MAP(EYE_GLASSES)},
  { "find_text_button",     "find",     "Find attribute", GEDA_MAP(FIND_ATTRIBUTE)},
  { "hide_text_button",     "hide",     "Hide selected attribute", "Private"},
  { "show_specific_button", "value",    "Show a specific attribute value", "Private"},
  { "auto_number_button",   "name",     "Open Auto Number dialog", "Private"},
  { NULL, NULL, NULL},
};

/*! \brief Creates a new Bitmap Image
 *
 * If for some reason our bitmap can not be found this function falls
 * back to the stock GTK icon bitmaps provided in the distribution.
 * This is the opposite of the predecessor function which used the stock
 * icon as the primary and our bitmap as the backup! This function is
 * used in combination with the TOOLBAR_xxx_BUTTON macros that use the
 * LOCAL_ALT option. When LOCAL_ALT options is used the primary icon is
 * the forth string in the ToolbarStrings, the secondary is will be GTK
 * _STK_ and the next macro parameter, for example:
 *
 * ex.:TOOLBAR_GEDA_BUTTON( Standard, new, LOCAL_ALT, NEW, callback, data);
 *                                     ^       ^       ^
 *                  enumerated index __|       |       |
 *                                             |       |
 *                         use this function __|       |
 *                                                     |
 *             if file not found then use GTK_STOCK_ __|
 *
 *
 * \param item Name of the stock icon ("new", "open", etc.)
 *
 */
static GtkWidget *get_stock_alt_pixmap(ToolbarItem* item )
{
  GtkWidget *wpixmap = NULL;
  GdkPixmap *pixmap;
  GdkBitmap *mask;

  GSCHEM_TOPLEVEL *w_current = g_current_window();
  GdkWindow *window=w_current->main_window->window;
  GdkColor  *background=&w_current->main_window->style->bg[GTK_STATE_NORMAL];

  char *filename=g_strconcat(w_current->toplevel->bitmap_directory,
                             G_DIR_SEPARATOR_S,
                             TB_ICON_NAME(item->ButtonId), NULL);

  /* 1ST Try custom icon */
  if(access(filename, R_OK) == 0) {
    pixmap = gdk_pixmap_create_from_xpm (window, &mask, background, filename);
    if (pixmap != NULL) {
      wpixmap = gtk_image_new_from_pixmap (pixmap, mask);
    }
  }
  if (wpixmap == NULL) { /* Try Falling back to Stock icon */
    wpixmap = gtk_image_new_from_stock(item->stock_id,
                                       GTK_ICON_SIZE_SMALL_TOOLBAR);
  }
  if (wpixmap == NULL) {
     s_log_message("get_stock_alt_pixmap: image file not found: \"%s\".\n", filename);
     wpixmap = gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE ,
                                        GTK_ICON_SIZE_SMALL_TOOLBAR);
  }

  g_free(filename);

  return wpixmap;
}

/*! \brief Save Toolbar State and Geometry
 *
 *  \par Function Description
 *  This function saves the state of the Toolbar widgets so we
 *  can restore them to same states the next time we run.
 */
void x_toolbars_save_state(GSCHEM_TOPLEVEL *w_current) {

  char *data, *filename;
  GKeyFile    *key_file = NULL;

  void SaveBarVisibility(GtkWidget * handlebox) {
    int   bar_id;
    int   visible;
    const char *group_name;

    bar_id = GET_TOOLBAR_ID(handlebox);
    group_name = IDS_Toolbar_Names[bar_id];
    visible = gtk_widget_get_visible(handlebox);
    g_key_file_set_integer (key_file, group_name, "visible", visible);
    //g_key_file_set_integer (key_file, group_name, "x", x);
    //g_key_file_set_integer (key_file, group_name, "y", y);
  }

  void SaveAllBars() {
    SaveBarVisibility(w_current->add_handlebox);
    SaveBarVisibility(w_current->attribute_handlebox);
    SaveBarVisibility(w_current->edit_handlebox);
    SaveBarVisibility(w_current->page_handlebox);
    SaveBarVisibility(w_current->standard_handlebox);
    SaveBarVisibility(w_current->zoom_handlebox);
  }

  bool setup_new_keyfile (char *filename) {

    bool results = TRUE;

    key_file = g_key_file_new();

    if (access(filename, W_OK) != 0) {
      v_log_message("Creating new Toolbar configuration\n");
      mkdir (s_path_user_config (), S_IRWXU | S_IRWXG);
      g_file_set_contents (filename, "", -1, NULL);
    }
    if (!g_file_test (filename, G_FILE_TEST_EXISTS))
      results = FALSE;
    return results;
  }

  v_log_message("Saving Toolbar configuration\n");
  filename = g_build_filename(s_path_user_config (), TOOLBAR_GEOMETRY_STORE, NULL);

  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    setup_new_keyfile (filename);
  else
    key_file = g_key_file_new();

  if(key_file) {
    SaveAllBars();
    data = g_key_file_to_data(key_file, NULL, NULL);
    g_file_set_contents(filename, data, -1, NULL);
    g_free(data);
  }
  else
    fprintf(stderr, "Warning, could not save Toolbar configuration to %s\n", filename);

  if(key_file) g_key_file_free(key_file);
  g_free(filename);
}

/*! \brief Restore Toolbar State and Geometry
 *
 *  \par Function Description
 *  This function restores the state of the Toolbar widgets.
 */
void x_toolbars_restore_state(GSCHEM_TOPLEVEL *w_current) {

  char       *filename;
  const char *group_name;
  GError     *err = NULL;
  GKeyFile   *key_file = NULL;

  void RestoreBarVisibility(GtkWidget * handlebox) {
    int bar_id;
    int visible;

    bar_id = GET_TOOLBAR_ID(handlebox);
    group_name = IDS_Toolbar_Names[bar_id];

    if(key_file) {
      visible = g_key_file_get_integer (key_file, group_name, "visible", &err);
      if(!err) {
        gtk_widget_set_visible(handlebox, visible);
        x_menu_set_toolbar_toggle(w_current, bar_id, visible);
      }
      else {
        gtk_widget_set_visible(handlebox, TRUE);
        g_clear_error (&err);
      }
    }
    else
      s_log_message("Error, Toolbar configuration key file, %s\n", group_name);

  }

  void RestoreAllBars() {
    if(key_file) {
      v_log_message("Retrieving toolbar geometry\n");
      RestoreBarVisibility(w_current->add_handlebox);
      RestoreBarVisibility(w_current->attribute_handlebox);
      RestoreBarVisibility(w_current->edit_handlebox);
      RestoreBarVisibility(w_current->page_handlebox);
      RestoreBarVisibility(w_current->standard_handlebox);
      RestoreBarVisibility(w_current->zoom_handlebox);
    }
  }

  filename = g_build_filename(s_path_user_config (), TOOLBAR_GEOMETRY_STORE, NULL);

  if(g_file_test (filename, G_FILE_TEST_EXISTS)) {
    if (access(filename, R_OK) == 0) {
      key_file = g_key_file_new();
      if(g_key_file_load_from_file(key_file, filename, G_KEY_FILE_NONE, &err))
          RestoreAllBars();
      else {
        s_log_message("Warning, Error Restoring Toolbar configuration, %s\n", err->message);
        g_clear_error (&err);
      }
    }
    else {
      s_log_message("Warning, Toolbar configuration file access error:, %s\n", err->message);
    }
  }
  else
    v_log_message("Toolbar configuration geometry not found!\n");

  if(key_file) g_key_file_free(key_file);
  g_free(filename);
}

/*! \brief Finialize Toolbar Initialization
 *
 *  \par Function Description
 * This function sets the visibility of the Close buttons on all the
 * handleboxes. The Main window did a Show All and that revealed all
 * the buttons on the handleboxes that should be hidden if they are
 * docked. Rather than turn each widget individually when creating the
 * main window, it's easier to "fix" this by having this routine emit
 * a signal to each handlebox.
 *
 *  \param [in] w_current pointer to top-level data structure
 */
void x_toolbars_finialize (GSCHEM_TOPLEVEL *w_current) {

  x_toolbars_restore_state(w_current);

  g_signal_emit_by_name(GTK_WIDGET(w_current->add_handlebox),       "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->attribute_handlebox), "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->edit_handlebox),      "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->page_handlebox),      "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->standard_handlebox),  "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->zoom_handlebox),      "child-attached");

}

/*! @brief Toolbar Close Button Handler */
void
On_Close_Handlebar(GtkWidget *CloseButton, GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *container;
  int HandleBoxId;

  container = gtk_widget_get_parent(CloseButton);
  container = gtk_widget_get_parent(container);
  container = gtk_widget_get_parent(container);

  if (GTK_IS_HANDLE_BOX(container)) {
    gtk_widget_hide(container);
    HandleBoxId =  GET_TOOLBAR_ID(container);
    x_menu_set_toolbar_toggle(w_current, HandleBoxId, FALSE);
  }
  else
    fprintf(stderr, "Error [On_Close_Handlebar]: container is not a handlebox\n");

}

/*! @brief Toolbar On Dock Handler to Hide the Close Button */
void
On_Dock_ToolBar(GtkHandleBox *handlebox, GtkWidget *widget, GtkWidget *CloseButton) {
  gtk_handle_box_set_shadow_type ( handlebox, GTK_SHADOW_ETCHED_OUT );
  gtk_widget_hide (CloseButton);
}

/*! @brief Toolbar On Float Handler to Show the Close Button */
void
On_Float_ToolBar(GtkHandleBox *handlebox, GtkWidget *widget, GtkWidget *CloseButton) {
  gtk_handle_box_set_shadow_type ( handlebox, GTK_SHADOW_ETCHED_IN );
  gtk_widget_show (CloseButton);
}

/*! \brief Add Close Button Toolbars - the little red X's
 *
 *  \par Function Description
 * This function creates a single close buttons on the each toolbars
 * along with the nessacary alignment containers. The function sets up
 * callback to the function defined above.
 *
 *  \param [in] parent_container is main vbox widget (main_box)
 */
static void
x_toolbars_add_closer(GSCHEM_TOPLEVEL *w_current, GtkWidget *HandlerBar, GtkWidget *ToolBar) {

  GtkWidget *CloseButton;
  GtkWidget *fixed;
  GtkWidget *alignment;
  GtkStyle  *style;
  GtkWidget *x_image;

  /* Create a Fixed Widget to hold the Close Buttton and add to the Toolbar */
  fixed = gtk_fixed_new();
  gtk_container_add(GTK_CONTAINER(ToolBar), fixed);

  /* Create the Close Button */
  CloseButton = gtk_button_new();

  /* Set Properties and Styles for the Close Button */
  gtk_widget_set_size_request(CloseButton, 16, 16);
  gtk_button_set_relief(GTK_BUTTON (CloseButton), GTK_RELIEF_NONE);
  gtk_button_set_focus_on_click(GTK_BUTTON (CloseButton), FALSE);
  style = gtk_widget_get_style(CloseButton);
  style->bg[GTK_STATE_PRELIGHT] = style->bg[GTK_STATE_NORMAL];
  gtk_widget_set_style(CloseButton, style);

  /* Put the Close Buttton inside the Fixed container and show it */
  gtk_container_add(GTK_CONTAINER(fixed), CloseButton);      /* Put Button Widget Inside the fixed container */
  gtk_widget_show (CloseButton);

  /* Create a New Alignment widget to hold the button Image */
  alignment = gtk_alignment_new (0, 0, 0, 0);
  gtk_widget_show (alignment);
  gtk_container_add (GTK_CONTAINER (CloseButton), alignment); /* Put Alignment Widget Inside the Button */

  /* Create a Pixmap widget containing the image and add the widget to the container */
  x_image = create_pixmap (CLOSE_TOOLBAR_BITMAP);
  gtk_container_add (GTK_CONTAINER (alignment), x_image);     /*Put image inside the Alignment container */
  gtk_widget_show (x_image);

  /* Setup the signal handlers */
  g_signal_connect (CloseButton,"pressed",
                    G_CALLBACK (On_Close_Handlebar),
                    w_current); /* not really needed since menu has embed ptr */

  g_signal_connect (HandlerBar,"child-attached",
                    G_CALLBACK (On_Dock_ToolBar),
                    CloseButton);

  g_signal_connect (HandlerBar,"child-detached",
                    G_CALLBACK (On_Float_ToolBar),
                    CloseButton);

  gtk_object_set_data(GTK_OBJECT(HandlerBar), "CloseButton",  CloseButton);

  return;
}

/*! \section Initialize-Toolbars
 *
 *  \par
 *  Note that this section relies heavily on MACROS defined in
 *  geda_toolbars.h in order to reduce coding errors and to help
 *  clarify the "subject" of the algorythms, rather then obsuring
 *  the intent in >10k lines of gtk_xxx's.
 *
 */

/*! \brief Initialize Toolbars at the Top of the Main Window
 *
 *  \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create all of the Toolbars at the
 * Top of the Main Window. This function initializes the global
 * variables in the compilation unit.
 *
 *  \param [in] parent_container is main vbox widget (main_box)
 */
void x_toolbars_init_top(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container) {

  /* ------------------------ Top Toolbars ---------------------- */
  GtkWidget *Add_Toolbar;
  GtkWidget *Page_Toolbar;
  GtkWidget *Standard_Toolbar;
  GtkWidget *Zoom_Toolbar;

  /* --------------- Initialize Module Level Globals ------------ */
  can_paste      = NULL;
  can_undo       = NULL;
  can_redo       = NULL;
  have_pages     = NULL;
  complex_object = NULL;
  some_object    = NULL;
  text_object    = NULL;

  /* ---------------------- Create Top ToolBox ------------------ */
  GtkWidget *toolbox_T1 = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (parent_container), toolbox_T1, FALSE, FALSE, 0);
  gtk_widget_show (toolbox_T1);

  /* --------- Create and Populate the Standard Toolbar -------- */
  /* Standard Toolbar*/
  w_current->standard_handlebox = gtk_handle_box_new();
  gtk_box_pack_start(GTK_BOX (toolbox_T1), w_current->standard_handlebox, FALSE, FALSE, 0);
  gtk_widget_show (w_current->standard_handlebox);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */

  Standard_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR (Standard_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style          (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_ICONS); /* get config GTK_TOOLBAR_BOTH*/
  gtk_container_set_border_width (GTK_CONTAINER (Standard_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->standard_handlebox), Standard_Toolbar);

  /* Add New, Open, Save and Save As Buttons to the Standard Toolbar */
  TOOLBAR_GEDA_BUTTON( Standard, new,     LOCAL_ALT, NEW,     i_callback_toolbar_file_new,     w_current);
  TOOLBAR_GEDA_BUTTON( Standard, open,    LOCAL_ALT, OPEN,    i_callback_toolbar_file_open,    w_current);
  TOOLBAR_GEDA_BUTTON( Standard, save,    LOCAL_ALT, SAVE,    i_callback_toolbar_file_save,    w_current);
  TOOLBAR_GEDA_BUTTON( Standard, save_as, LOCAL_STK, SAVE_AS, i_callback_toolbar_file_save_as, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  /* Add Print and Export PDF Buttons to the Standard Toolbar */
  TOOLBAR_GEDA_BUTTON( Standard, print,      LOCAL_STK, PRINT,   i_callback_toolbar_file_print, w_current);
  TOOLBAR_GEDA_BUTTON( Standard, export_pdf, LOCAL_PIX, GAF_PDF_BITMAP,    i_callback_toolbar_write_pdf,  w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_GEDA_BUTTON( Standard, cut,   LOCAL_STK, CUT,   i_callback_toolbar_clipboard_cut, w_current)
  TOOLBAR_GEDA_BUTTON( Standard, copy,  LOCAL_STK, COPY,  i_callback_toolbar_clipboard_copy, w_current)
  TOOLBAR_GEDA_BUTTON( Standard, paste, LOCAL_STK, PASTE, i_callback_toolbar_clipboard_paste, w_current)

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_GEDA_BUTTON( Standard, undo,      LOCAL_STK, UNDO,        i_callback_toolbar_edit_undo,  w_current);
  TOOLBAR_GEDA_BUTTON( Standard, redo,      LOCAL_STK, REDO,        i_callback_toolbar_edit_redo,  w_current);
  TOOLBAR_GEDA_BUTTON( Standard, configure, LOCAL_ALT, PREFERENCES, i_callback_toolbar_configure_settings,  w_current);

  gtk_widget_show (Standard_Toolbar);
  SET_TOOLBAR_ID  (w_current->standard_handlebox, tb_Standard);
  x_toolbars_add_closer(w_current, w_current->standard_handlebox, Standard_Toolbar );

  can_paste   = g_slist_append ( can_paste,   TB_BUTTON (paste));
  some_object = g_slist_append ( some_object, TB_BUTTON (cut  ));
  some_object = g_slist_append ( some_object, TB_BUTTON (copy ));
  can_undo    = g_slist_append ( can_undo,    TB_BUTTON (undo ));
  can_redo    = g_slist_append ( can_undo,    TB_BUTTON (redo ));

  /* ----------- Create and Populate the Page Toolbar ----------- */

  w_current->page_handlebox = gtk_handle_box_new ();

  gtk_box_pack_start(GTK_BOX (toolbox_T1), w_current->page_handlebox, FALSE, FALSE, 0);
  gtk_widget_show (w_current->page_handlebox);

  Page_Toolbar = gtk_toolbar_new ();
  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Page_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style          (GTK_TOOLBAR   (Page_Toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (Page_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->page_handlebox), Page_Toolbar);

  TOOLBAR_GEDA_BUTTON( Page, prev_page,    LOCAL_STK, GO_BACK,    i_callback_toolbar_page_prev, w_current);
  TOOLBAR_GEDA_BUTTON( Page, next_page,    LOCAL_STK, GO_FORWARD, i_callback_toolbar_page_next, w_current);
  TOOLBAR_GEDA_BUTTON( Page, new_page,     LOCAL_STK, NEW,        i_callback_toolbar_page_new,  w_current);
  TOOLBAR_GEDA_BUTTON( Page, page_manager, LOCAL_PIX, GEDA_SHEETS_BITMAP,  i_callback_toolbar_page_manager, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Page_Toolbar));

  TOOLBAR_GEDA_BUTTON( Page, down_schematic, LOCAL_PIX, GEDA_DEMOTE_BITMAP,   i_callback_toolbar_down_schematic, w_current);
  TOOLBAR_GEDA_BUTTON( Page, down_symbol,    LOCAL_PIX, GEDA_DEMOTE_BITMAP,   i_callback_toolbar_down_symbol, w_current);
  TOOLBAR_GEDA_BUTTON( Page, hierarchy_up,   LOCAL_PIX, GEDA_PROMOTE_BITMAP,  i_callback_toolbar_hierarchy_up,  w_current);
  TOOLBAR_GEDA_BUTTON( Page, comp_doc,       LOCAL_PIX, GAF_SEE_NOTES_BITMAP, i_callback_toolbar_cdocumentation, w_current);

  have_pages     = g_slist_append ( have_pages, TB_BUTTON( prev_page ));
  have_pages     = g_slist_append ( have_pages, TB_BUTTON( next_page ));

  complex_object = g_slist_append ( complex_object, TB_BUTTON( down_schematic));
  complex_object = g_slist_append ( complex_object, TB_BUTTON( down_symbol   ));
  complex_object = g_slist_append ( complex_object, TB_BUTTON( hierarchy_up  ));
  complex_object = g_slist_append ( complex_object, TB_BUTTON( comp_doc      ));

  gtk_widget_show (Page_Toolbar);
  SET_TOOLBAR_ID       (w_current->page_handlebox, tb_Page);
  x_toolbars_add_closer(w_current, w_current->page_handlebox, Page_Toolbar );

  // fprintf(stderr, "what happen to [%s]\n",GAF_SEE_NOTES_BITMAP );
  /* Start Second Toolbar Row */
  GtkWidget *toolbox_T2 = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (parent_container), toolbox_T2, FALSE, FALSE, 0);
  gtk_widget_show    (toolbox_T2);

  /* --------- Create and Populate the Add Toolbar -------- */
  w_current->add_handlebox = gtk_handle_box_new ();
  gtk_box_pack_start (GTK_BOX (toolbox_T2), w_current->add_handlebox, FALSE, FALSE, 0);
  gtk_widget_show    (w_current->page_handlebox);

  Add_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Add_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style          (GTK_TOOLBAR   (Add_Toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (Add_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->add_handlebox), Add_Toolbar);

  TOOLBAR_GEDA_BUTTON( Add, add_line,   LOCAL_PIX, GEDA_LINE_BITMAP,    i_callback_toolbar_add_line,  w_current);
  TOOLBAR_GEDA_BUTTON( Add, add_box,    LOCAL_PIX, GEDA_BOX_BITMAP,     i_callback_toolbar_add_box, w_current);
  TOOLBAR_GEDA_BUTTON( Add, add_circle, LOCAL_PIX, GEDA_CIRCLES_BITMAP, i_callback_toolbar_add_circle, w_current);
  TOOLBAR_GEDA_BUTTON( Add, add_arc,    LOCAL_PIX, GEDA_ARC_BITMAP,     i_callback_toolbar_add_arc,  w_current);
  TOOLBAR_GEDA_BUTTON( Add, add_pin,    LOCAL_PIX, GEDA_PIN_BITMAP,       i_callback_toolbar_add_pin, w_current);
  TOOLBAR_GEDA_BUTTON( Add, insert_pic, LOCAL_PIX, GEDA_FILM_ROLL_BITMAP, i_callback_toolbar_add_picture, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Add_Toolbar));

  TOOLBAR_GEDA_BUTTON( Add, add_component, LOCAL_PIX, GSCHEM_TRANSISTOR_BITMAP,   i_callback_toolbar_add_component,  w_current);

  /* Toolbar radio button group */
  w_current->toolbar_net =
      gtk_toolbar_append_element(GTK_TOOLBAR(Add_Toolbar),
                                 GTK_TOOLBAR_CHILD_RADIOBUTTON,
                                 NULL,
                                 _(TB_LABEL (add_net)),
                                 _(TB_TOOLTIP (add_net)),
                                 "toolbar/nets",
                                 create_pixmap (TB_ICON_NAME(add_net)),
                                 (GtkSignalFunc) i_callback_toolbar_add_net,
                                 w_current);
  w_current->toolbar_bus =
      gtk_toolbar_append_element(GTK_TOOLBAR(Add_Toolbar),
                                 GTK_TOOLBAR_CHILD_RADIOBUTTON,
                                 w_current->toolbar_net,
                                 _(TB_LABEL (add_bus)),
                                 _(TB_TOOLTIP (add_bus)),
                                 "toolbar/bus",
                                 create_pixmap (TB_ICON_NAME(add_bus)),
                                 (GtkSignalFunc) i_callback_toolbar_add_bus,
                                 w_current);
  /* not part of any radio button group */
  TOOLBAR_GEDA_BUTTON( Add, add_attribute, LOCAL_PIX, GAF_MAP(ADD_ATTRIBUTE), i_callback_toolbar_add_attribute,  w_current);
  TOOLBAR_GEDA_BUTTON( Add, add_text,      LOCAL_PIX, GSCHEM_TEXT_BITMAP,     i_callback_toolbar_add_text,  w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Add_Toolbar));
  w_current->toolbar_select = gtk_toolbar_append_element(GTK_TOOLBAR(Add_Toolbar),
                                 GTK_TOOLBAR_CHILD_RADIOBUTTON,
                                 w_current->toolbar_bus,
                                 _(TB_LABEL (selector)),
                                 _(TB_TOOLTIP (selector)),
                                 "toolbar/select",
                                 create_pixmap (TB_ICON_NAME(selector)),
                                 (GtkSignalFunc) i_callback_toolbar_edit_select,
                                 w_current);

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w_current->toolbar_select), TRUE);

  TOOLBAR_GEDA_BUTTON( Add, deselector,  LOCAL_PIX, GEDA_DESELECT_BITMAP, i_callback_toolbar_deselect,  w_current);

  gtk_widget_show (Add_Toolbar);
  SET_TOOLBAR_ID  (w_current->add_handlebox, tb_Add);
  x_toolbars_add_closer(w_current, w_current->add_handlebox, Add_Toolbar );

  /* --------- Create and Populate the Zoom Toolbar -------- */
  w_current->zoom_handlebox = gtk_handle_box_new ();
  gtk_box_pack_start(GTK_BOX (toolbox_T2), w_current->zoom_handlebox, FALSE, FALSE, 0);
  gtk_widget_show   (w_current->zoom_handlebox);

  Zoom_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Zoom_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style          (GTK_TOOLBAR   (Zoom_Toolbar), GTK_TOOLBAR_ICONS);
  gtk_container_set_border_width (GTK_CONTAINER (Zoom_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->zoom_handlebox), Zoom_Toolbar);

  TOOLBAR_GEDA_BUTTON( Zoom, view_redraw,  LOCAL_ALT, ZOOM_FIT, i_callback_toolbar_view_redraw,  w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, zoom_pan,     LOCAL_ALT, ZOOM_FIT, i_callback_toolbar_zoom_pan,  w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, zoom_box,     LOCAL_ALT, ZOOM_FIT, i_callback_toolbar_zoom_box, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, zoom_extents, LOCAL_ALT, ZOOM_FIT, i_callback_toolbar_zoom_extents, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, zoom_in,      LOCAL_ALT, ZOOM_IN,  i_callback_toolbar_zoom_in, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, zoom_out,     LOCAL_ALT, ZOOM_OUT, i_callback_toolbar_zoom_out, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, zoom_all,     LOCAL_ALT, ZOOM_100, i_callback_toolbar_zoom_limits, w_current);

  gtk_widget_show (Zoom_Toolbar);
  SET_TOOLBAR_ID (w_current->zoom_handlebox, tb_Zoom);
  x_toolbars_add_closer(w_current, w_current->zoom_handlebox, Zoom_Toolbar );
}

/*! \brief Initialize Toolbar at the Left of the Main Window
 *
 *  \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create the Toolbar at the Left
 * of the Main Window.
 *
 *  \param [in] parent_container is the center_hbox widget
 */
void x_toolbars_init_left(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container) {
  GtkWidget *Edit_Toolbar;

  GtkWidget *toolbox_L1 = gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (parent_container), toolbox_L1, FALSE, FALSE, 0);
  gtk_widget_show    (toolbox_L1);

  /* --------- Create and Populate the Edit Toolbar -------- */
  w_current->edit_handlebox = gtk_handle_box_new ();
  gtk_box_pack_start(GTK_BOX (toolbox_L1), w_current->edit_handlebox, FALSE, FALSE, 0);
  gtk_widget_show   (w_current->edit_handlebox);

  Edit_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Edit_Toolbar), GTK_ORIENTATION_VERTICAL);
  gtk_toolbar_set_style          (GTK_TOOLBAR   (Edit_Toolbar), GTK_TOOLBAR_ICONS);

  gtk_container_set_border_width (GTK_CONTAINER (Edit_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->edit_handlebox), Edit_Toolbar);

  TOOLBAR_GEDA_BUTTON( Edit, edit_copy,  LOCAL_PIX, GEDA_COPY_BITMAP,    i_callback_toolbar_edit_copy,   w_current);
  TOOLBAR_GEDA_BUTTON( Edit, multi_copy, LOCAL_PIX, GEDA_MULTI_BITMAP,   i_callback_toolbar_edit_mcopy,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, move,       LOCAL_PIX, GEDA_MOVE_BITMAP,    i_callback_toolbar_edit_move,   w_current);
  TOOLBAR_GEDA_BUTTON( Edit, mirror,     LOCAL_PIX, GEDA_MIRROR_BITMAP,  i_callback_toolbar_edit_mirror, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, rotate,     LOCAL_PIX, GEDA_ROTATE_BITMAP,  i_callback_toolbar_edit_rotate, w_current);

  TOOLBAR_GEDA_BUTTON( Edit, edit_line,  LOCAL_PIX, GEDA_LINE_TYPE_BITMAP,     i_callback_toolbar_edit_linetype,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, edit_color, LOCAL_PIX, GEDA_DISPLAY_COLOR_BITMAP, i_callback_toolbar_edit_color,     w_current);
  TOOLBAR_GEDA_BUTTON( Edit, edit_fill,  LOCAL_STR, NULL,                      i_callback_toolbar_edit_filltype,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, lock,       LOCAL_PIX, GEDA_LOCK_BITMAP,   i_callback_toolbar_edit_lock,   w_current);
  TOOLBAR_GEDA_BUTTON( Edit, unlock,     LOCAL_PIX, GEDA_UNLOCK_BITMAP, i_callback_toolbar_edit_unlock, w_current);

  some_object = g_slist_append (some_object, TB_BUTTON ( edit_copy  ));
  some_object = g_slist_append (some_object, TB_BUTTON ( multi_copy ));
  some_object = g_slist_append (some_object, TB_BUTTON ( move   ));
  some_object = g_slist_append (some_object, TB_BUTTON ( mirror ));
  some_object = g_slist_append (some_object, TB_BUTTON ( rotate ));

  some_object = g_slist_append (some_object, TB_BUTTON ( edit_line  ));
  some_object = g_slist_append (some_object, TB_BUTTON ( edit_color ));
  some_object = g_slist_append (some_object, TB_BUTTON ( edit_fill  ));
  some_object = g_slist_append (some_object, TB_BUTTON ( lock   ));
  some_object = g_slist_append (some_object, TB_BUTTON ( unlock ));

  gtk_widget_show (Edit_Toolbar);
  gtk_widget_show (Edit_Toolbar);
  SET_TOOLBAR_ID  (w_current->edit_handlebox, tb_Edit);
  x_toolbars_add_closer(w_current, w_current->edit_handlebox, Edit_Toolbar );
}
/*! \brief Initialize Toolbar at the Bottom of the Main Window
 *
 *  \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create the Toolbar at the Bottom
 * of the Main Window.
 *
 *  \param [in] parent_container is the main_box widget (same as the top bar)
 */
void x_toolbars_init_bottom(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container) {

  GtkWidget *Attribute_Toolbar;

  /* Start Second Toolbar Row */
  GtkWidget *toolbox_B1 = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (parent_container), toolbox_B1, FALSE, FALSE, 0);
  gtk_widget_show (toolbox_B1);

  /* --------- Create and Populate the Attribute Toolbar -------- */

  w_current->attribute_handlebox = gtk_handle_box_new ();
  //   handlebox.set_snap_edge(gtk.POS_LEFT)
  gtk_box_pack_start(GTK_BOX (toolbox_B1), w_current->attribute_handlebox, FALSE, FALSE, 0);
  gtk_widget_show (w_current->attribute_handlebox);

  Attribute_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR (Attribute_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style          (GTK_TOOLBAR (Attribute_Toolbar), GTK_TOOLBAR_ICONS);

  gtk_container_set_border_width (GTK_CONTAINER (Attribute_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->attribute_handlebox), Attribute_Toolbar);

  /* Add Attribute Button to Toolbar */
  TOOLBAR_GEDA_BUTTON(Attribute, attach,     LOCAL_PIX, GAF_PROMOTE_BITMAP,  i_callback_toolbar_attach_attributes,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, detach,     LOCAL_PIX, GAF_DEMOTE_BITMAP,   i_callback_toolbar_detach_attributes,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, show_value, LOCAL_PIX, GEDA_VALUE_BITMAP,    i_callback_toolbar_attributes_show_value,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, show_name,  LOCAL_PIX, GEDA_NAME_TAG_BITMAP,   i_callback_toolbar_attributes_show_name,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, show_both,  LOCAL_PIX, GEDA_NAME_VALUE_BITMAP,  i_callback_toolbar_attributes_show_both,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, visibilty,  LOCAL_PIX, GEDA_EYE_GLASSES_BITMAP, i_callback_toolbar_attributes_visibility,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, find_text,  LOCAL_ALT, FIND,                    i_callback_toolbar_find_attribute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, hide_text,  LOCAL_PIX, GEDA_GHOST_INVISIBLE_BITMAP,   i_callback_toolbar_hide_text,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, show_specific, LOCAL_PIX, GEDA_LOCATE_REFERENCE_BITMAP, i_callback_toolbar_show_text,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, auto_number,   LOCAL_PIX, GEDA_NUMBER_BITMAP,  i_callback_toolbar_autonumber,  w_current);

  text_object = g_slist_append ( text_object, TB_BUTTON ( attach     ));
  text_object = g_slist_append ( text_object, TB_BUTTON ( detach     ));
  text_object = g_slist_append ( text_object, TB_BUTTON ( show_value ));
  text_object = g_slist_append ( text_object, TB_BUTTON ( show_name  ));
  text_object = g_slist_append ( text_object, TB_BUTTON ( show_both  ));
  text_object = g_slist_append ( text_object, TB_BUTTON ( visibilty  ));

  gtk_widget_show (Attribute_Toolbar);
  SET_TOOLBAR_ID (w_current->attribute_handlebox, tb_Attribute);
  x_toolbars_add_closer(w_current, w_current->attribute_handlebox, Attribute_Toolbar );
}

/*! \brief Set Sensitivity of Toolbar Buttons
 *  \par Function Description
 *   This function is called by x_toolbars_set_sensitivities with a gslist
 *   of toolbasr button widgets to be set to the specified sensitivity
 *
 *  \param [in] ListToolBarItems SINGLE linked list of widgets
 *  \param [in] sensitivee boolean TRUE = sensitive, FALSE = gray-out
 */
static void x_toolbar_set_sensitivity(GSList *ListToolBarItems, int sensitive)
{
    lambda (GtkWidget *item)
    {
      gtk_widget_set_sensitive(item, sensitive);
      return FALSE;
    }
    mapcar(ListToolBarItems);
}

/*! \brief Set Sensitivity of Toolbar Button Groups
 *
 *  \par Function Description
 *  This functions sets the sensitivities of toolbar button as a visual
 *  aid to the user. When the sensitivity is set to FALSE widgets are
 *  grayed-out to indicate they are not applicable to the current context.
 *  Note that this is purely to aid the user, cosmetically the GUI "looks"
 *  better with color and nothing bad would happen even if a button is
 *  pressed when not applicable, gschem just ignores the signals.
 *
 *  \param [in] mode is an enumerated group identifier (see in globals.h)
 *  \param [in] state boolean TRUE = sensitive, FALSE = gray-out
 */
void x_toolbars_set_sensitivities(ID_SENITIVITY_MODE mode, bool state) {
  switch (mode) {
    case CAN_PASTE:      x_toolbar_set_sensitivity(can_paste, state);
      break;
    case CAN_UNDO:       x_toolbar_set_sensitivity(can_undo, state);
      break;
    case CAN_REDO:       x_toolbar_set_sensitivity(can_redo, state);
      break;
    case HAVE_PAGES:     x_toolbar_set_sensitivity(have_pages, state);
      break;
    case COMPLEX_OJECTS: x_toolbar_set_sensitivity(complex_object, state);
      break; /* is optional */
    case TEXT_OJECTS:    x_toolbar_set_sensitivity(text_object, state);
      break;
    case SOME_OJECTS:    x_toolbar_set_sensitivity(some_object, state);
  }
}
