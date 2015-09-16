/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gattrib.h>

#include <gtk/gtk.h>

#include <geda_toolbars.h>
#include <geda_debug.h>

static ToolbarStringData ToolbarStrings[] = {
   /* Standard Toolbar*/
  { "open_button", 	"Open", 	"Open file", "Private"},
  { "save_button", 	"Save",		"Save file", "Private"},
  { "save_as_button", 	"Save As", 	"Save the file to different name or location", "Private"},
  { "cut_button", 	"Cut", 		"Cut selection to the clipboard", "Private"},
  { "copy_button",  	"Copy", 	"Copy selection to the clipboard", "Private"},
  { "paste_button",	"Paste",	"Paste selection from the clipboard", "Private"},
  { "find_button", 	"Find", 	"Search for attribute value", "Private"},
  { "replace_button", 	"Replace", 	"Search and Replace attribute value", "Private"},
  { "attribute_button", "Attrib", 	"Locate attribute", "Private"},
  { "designator_button","Ref Des", 	"Locate Reference Designator", "Private"},

   /* Attribute Toolbar */
  { "invisible_button",  "invisible",  "Set selected invisible"},
  { "visible_button",    "visible",    "Set selected visible"},
  { "add_button",        "add",        "Add a new attribute"},
  { "promote_button",    "promote",    "Attach the selected attribute"},
  { "demote_button",     "demote",     "Dettach the selected attribute"},
  { "name_only_button",  "name",       "Set selected name visible only"},
  { "value_only_button", "value",      "Set selected value visible only"},
  { "name_value_button", "both",       "Set selected name and value visible"},
  { NULL, NULL, NULL},
};

/*! \brief Redirect Cut, Copy, Paste from Toolbar to Handler function */
static void callBack_clipboard (GtkWidget *button_widget, IDS_Toolbar *Control)
{
  int button = (int)(long)Control;
  x_window_clipboard_handler(button);
  return;
}
/*! \brief Redirect Open, Save & Save As from Toolbar to Handler functions
 *  \par Function Description
 * This is a Call-back function for the file related buttons on the standard
 * toolbar.
 *
 *  \param [in] widget is button widget
 *  \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_toolbar0 (GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long)Control;

  switch ( button ) {
    case open:
      x_menu_file_open();
      break;
    case save:
      x_menu_file_save();
      break;
    case save_as:
      x_menu_file_save_as();
      break;
    default:
     u_log_message("toolbar0(): Button ID %d\n", button);
  }

  return;
}
/*! \brief Callback Handler for  Search relates Toolbars buttons
 *
 *  \par Function Description
 * This function calls the approiate functions to process request
 * from search related buttons on the standard toolbar.
 *
 *  \param [in] widget is button widget
 *  \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_Searchbar (GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long) Control;

  switch ( button ) {
    case find:
      x_find_attribute_value();
      break;
    case replace:
      x_find_replace_attrib_value();
      break;
    case attribute:
      x_find_attribute();
      break;
    case designator:
      x_find_refdes();
      break;
    default:
     u_log_message("toolbar0(): Button ID %d\n", button);
  }

  return;
}
/*! \brief Callback Handler for Visibility relates Toolbars buttons
 *
 *  \par Function Description
 * This function calls the approiate functions to process request
 * from attribute toolbar buttons.
 *
 *  \param [in] widget is button widget
 *  \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_AttributeBar0(GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long)Control;

  switch ( button ) {
    case invisible:
      s_properties_set_invisible();
      break;
    case visible:
      s_properties_set_visible();
      break;
    case add:
      s_toplevel_add_new_attrib(-1);
      break;
    case promote:
      s_properties_promote_attribute();
      break;
    case demote:
      s_properties_demote_attribute();
      break;
    case name_only:
      s_properties_set_name_only();
      break;
    case value_only:
      s_properties_set_value_only();
      break;
    case name_value:
      s_properties_set_name_and_value();
      break;
    default:
     u_log_message("toolbar0(): Button ID %d\n", button);
  }

  return;
}

/*! \brief Initialize Toolbars
 *
 *  \par Function Description
 * This function creates the handlebox, toolbars and toolbar buttons.
 *
 *  \param [in] parent_container is main vbox widget
 */
void x_toolbars_init(GtkWidget *parent_container) {

  GtkWidget *tmp_toolbar_icon;

  /* --------- Create and Populate the Standard Toolbar -------- */

 /* Standard Toolbar*/
  Standard_handlebox = gtk_handle_box_new ();
  gtk_box_pack_start(GTK_BOX (parent_container), Standard_handlebox, FALSE, FALSE, 0);
  gtk_widget_show (Standard_handlebox);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */

  Standard_Toolbar = gtk_toolbar_new ();

  SET_TOOLBAR_ORIENTATION (Standard_Toolbar, HORIZONTAL);

  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (Standard_Toolbar), 0);
  gtk_container_add (GTK_CONTAINER (Standard_handlebox), Standard_Toolbar);

  /* Add Open Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, open, PIX, GSCHEM_OPEN_BITMAP, callBack_toolbar0)

  /* Add Save Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, save, PIX, GSCHEM_SAVE_BITMAP, callBack_toolbar0)

  /* Add Save As Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, save_as, STK, SAVE_AS, callBack_toolbar0)

  gtk_toolbar_append_space(GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_STD_BUTTON(Standard, cut,   STK, CUT,   callBack_clipboard)
  TOOLBAR_STD_BUTTON(Standard, copy,  STK, COPY,  callBack_clipboard)
  TOOLBAR_STD_BUTTON(Standard, paste, STK, PASTE, callBack_clipboard)

  gtk_toolbar_append_space(GTK_TOOLBAR(Standard_Toolbar));

  /* Add Find Button to Search_ToolbarToolbar */
  TOOLBAR_STD_BUTTON(Standard, find,  PIX, GEDA_BITMAP_FIND_TINY, callBack_Searchbar)

  /* Add Replace Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, replace, PIX, GEDA_BITMAP_FIND_REPLACE_TINY, callBack_Searchbar)

  /* Add Locate Attribute Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, attribute, PIX, GEDA_BITMAP_FIND_ATTRIBUTE_TINY, callBack_Searchbar)

  /* Add Locate Reference Designator to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, designator, PIX, GEDA_LOCATE_REFERENCE_BITMAP, callBack_Searchbar)

  gtk_widget_show (Standard_Toolbar);

  /* --------- Create and Populate the Attribute Toolbar -------- */
 /* Attribute Toolbar */
  Attribute_handlebox = gtk_handle_box_new ();
  gtk_box_pack_start(GTK_BOX (parent_container), Attribute_handlebox, FALSE, FALSE, 0);
  gtk_widget_show (Attribute_handlebox);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */

  Attribute_Toolbar = gtk_toolbar_new ();

  SET_TOOLBAR_ORIENTATION (Attribute_Toolbar, HORIZONTAL);

  gtk_toolbar_set_style (GTK_TOOLBAR (Attribute_Toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (Attribute_Toolbar), 0);
  gtk_container_add (GTK_CONTAINER (Attribute_handlebox), Attribute_Toolbar);

    /* Add Open Button to Toolbar */
  TOOLBAR_STD_BUTTON(Attribute, invisible, PIX, GEDA_GHOST_INVISIBLE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, visible,   PIX, GEDA_EYE_GLASSES_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, add,       PIX, GEDA_REDCROSS_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, promote,   PIX, GAF_PROMOTE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, demote,    PIX, GAF_DEMOTE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, name_only, PIX, GEDA_NAME_TAG_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, value_only, PIX, GEDA_VALUE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, name_value, PIX, GEDA_NAME_VALUE_BITMAP, callBack_AttributeBar0);

  gtk_widget_show (Attribute_Toolbar);

  open_button=open_button;
  save_button=save_button;
  save_as_button=save_as_button;
  cut_button=cut_button;
  copy_button=copy_button;
  paste_button=paste_button;

  find_button=find_button;
  replace_button=replace_button;

  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, attribute_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, designator_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, invisible_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, visible_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, add_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, promote_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, demote_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, name_only_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, value_only_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, name_value_button);

}

/*! \brief Set Senitivity of Toolbar Buttons.
 *  \par Function Description
 *       This function is called by on_notebook_switch_page when ever a TAB
 *       is selected, passing a gslist of toolbasr button widgets to be set
 *       to the specified sensitivity
 */
void x_toolbar_set_sensitivities(GSList *ListToolBarItems, int sensitive)
{
    lambda (GtkWidget *menu_item)
    {
      gtk_widget_set_sensitive(menu_item, sensitive);
      return FALSE;
    }
    mapcar(ListToolBarItems);
}