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

#include <geda_container.h>
#include <geda_toolbar.h>
#include <geda/geda_toolbars.h>
#include <geda_gtk_compat.h>
#include <geda_debug.h>

static ToolbarStringData ToolbarStrings[] = {
   /* Standard Toolbar*/
  { "open_button",       N_("Open"),     N_("Open file"), "Private"},
  { "save_button",       N_("Save"),     N_("Save file"), "Private"},
  { "save_as_button",    N_("Save As"),  N_("Save the file to different name or location"), "Private"},
  { "cut_button",        N_("Cut"),      N_("Cut selection to the clipboard"), "Private"},
  { "copy_button",       N_("Copy"),     N_("Copy selection to the clipboard"), "Private"},
  { "paste_button",      N_("Paste"),    N_("Paste selection from the clipboard"), "Private"},
  { "find_button",       N_("Find"),     N_("Search for attribute value"), "Private"},
  { "replace_button",    N_("Replace"),  N_("Search and Replace attribute value"), "Private"},
  { "attribute_button",  N_("Attrib"),   N_("Locate attribute"), "Private"},
  { "designator_button", N_("Ref Des"),  N_("Locate Reference Designator"), "Private"},

   /* Attribute Toolbar */
  { "invisible_button",  N_("invisible"), N_("Set selected invisible")},
  { "visible_button",    N_("visible"),   N_("Set selected visible")},
  { "add_button",        N_("add"),       N_("Add a new attribute")},
  { "promote_button",    N_("promote"),   N_("Attach the selected attribute")},
  { "demote_button",     N_("demote"),    N_("Detach the selected attribute")},
  { "name_only_button",  N_("name"),      N_("Set selected name visible only")},
  { "value_only_button", N_("value"),     N_("Set selected value visible only")},
  { "name_value_button", N_("both"),      N_("Set selected name and value visible")},
  { NULL, NULL, NULL},
};

/*!
 * \brief Callback for Cut, Copy, Paste Buttons on the Standard Toolbar
 * \par Function Description
 *  This is a callback function for cut, copy, and paste buttons on the
 *  standard toolbar. The function passes the call to a common handler;
 *  x_window_clipboard_handler, which also handles the same task menus.
 *  The toolbar and menus use a common enumeration, IDS_Toolbar, as the
 *  the identifier in order to synchronize the task.
 *
 * \param [in] widget  is the button widget
 * \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_clipboard (GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long)Control;

  x_window_clipboard_handler(button);

  return;
}

/*!
 * \brief Callback for Open, Save & Save As Buttons on the Standard Toolbar
 * \par Function Description
 *  This is a callback function for the file related buttons on the
 *  standard toolbar. The function redirects Open, Save & Save As
 *  from Toolbar to menu handler functions.
 *
 * \param [in] widget is button widget
 * \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_toolbar0 (GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long)Control;

  switch ( button ) {
    case tb_open:
      x_menu_file_open();
      break;

    case tb_save:
      x_menu_file_save();
      break;

    case tb_save_as:
      x_menu_file_save_as();
      break;

    default:
      fprintf (stderr, "%s: unknown button Id %d\n", __func__, button);
  }

  return;
}

/*!
 * \brief Callback Handler for  Search relates Toolbars buttons
 * \par Function Description
 *  This function calls the approiate functions to process request
 *  from search related buttons on the standard toolbar.
 *
 * \param [in] widget is button widget
 * \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_Searchbar (GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long) Control;

  switch ( button ) {
    case tb_find:
      x_find_attribute_value();
      break;

    case tb_replace:
      x_find_replace_attrib_value();
      break;

    case tb_attribute:
      x_find_attribute();
      break;

    case tb_designator:
      x_find_refdes();
      break;

    default:
      fprintf (stderr, "%s: unknown button Id %d\n", __func__, button);
  }

  return;
}

/*!
 * \brief Callback Handler for Visibility relates Toolbars buttons
 * \par Function Description
 *  This function calls the approiate functions to process request
 *  from attribute toolbar buttons.
 *
 * \param [in] widget is button widget
 * \param [in] Control pointer to enumerated integer ID of the button
 */
static void callBack_AttributeBar0(GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long)Control;

  switch ( button ) {
    case tb_invisible:
      s_properties_set_invisible();
      break;

    case tb_visible:
      s_properties_set_visible();
      break;

    case tb_add:
      s_toplevel_add_new_attrib(-1);
      break;

    case tb_promote:
      s_properties_promote_attribute();
      break;

    case tb_demote:
      s_properties_demote_attribute();
      break;

    case tb_name_only:
      s_properties_set_name_only();
      break;

    case tb_value_only:
      s_properties_set_value_only();
      break;

    case tb_name_value:
      s_properties_set_name_and_value();
      break;

    default:
      fprintf (stderr, "%s: unknown button Id %d\n", __func__, button);
  }

  return;
}

/*!
 * \brief Initialize Toolbars
 * \par Function Description
 *  This function creates the handlebox, toolbars and toolbar buttons.
 *
 * \param [in] parent_container is main vbox widget
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
  Standard_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_BOTH);
  geda_set_container_border_width (Standard_Toolbar, 0);
  geda_container_add (Standard_handlebox, Standard_Toolbar);

  /* Add Open Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_open, PIX, GSCHEM_OPEN_BITMAP, callBack_toolbar0)

  /* Add Save Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_save, PIX, GSCHEM_SAVE_BITMAP, callBack_toolbar0)

  /* Add Save As Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_save_as, STK, SAVE_AS, callBack_toolbar0)

  gtk_toolbar_append_space(GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_STD_BUTTON(Standard, tb_cut,   STK, CUT,   callBack_clipboard)
  TOOLBAR_STD_BUTTON(Standard, tb_copy,  STK, COPY,  callBack_clipboard)
  TOOLBAR_STD_BUTTON(Standard, tb_paste, STK, PASTE, callBack_clipboard)

  gtk_toolbar_append_space(GTK_TOOLBAR(Standard_Toolbar));

  /* Add Find Button to Search_ToolbarToolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_find,  PIX, GEDA_BITMAP_FIND_TINY, callBack_Searchbar)

  /* Add Replace Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_replace, PIX, GEDA_BITMAP_FIND_REPLACE_TINY, callBack_Searchbar)

  /* Add Locate Attribute Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_attribute, PIX, GEDA_BITMAP_FIND_ATTRIBUTE_TINY, callBack_Searchbar)

  /* Add Locate Reference Designator to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, tb_designator, PIX, GEDA_LOCATE_REFERENCE_BITMAP, callBack_Searchbar)

  gtk_widget_show (Standard_Toolbar);

  /* --------- Create and Populate the Attribute Toolbar -------- */

  /* Attribute Toolbar */
  Attribute_handlebox = gtk_handle_box_new ();
  gtk_box_pack_start(GTK_BOX (parent_container), Attribute_handlebox, FALSE, FALSE, 0);
  gtk_widget_show (Attribute_handlebox);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */
  Attribute_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  gtk_toolbar_set_style (GTK_TOOLBAR (Attribute_Toolbar), GTK_TOOLBAR_BOTH);
  geda_set_container_border_width (Attribute_Toolbar, 0);
  geda_container_add (Attribute_handlebox, Attribute_Toolbar);

    /* Add Open Button to Toolbar */
  TOOLBAR_STD_BUTTON(Attribute, tb_invisible,  PIX, GEDA_GHOST_INVISIBLE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_visible,    PIX, GEDA_EYE_GLASSES_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_add,        PIX, GEDA_REDCROSS_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_promote,    PIX, GAF_PROMOTE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_demote,     PIX, GAF_DEMOTE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_name_only,  PIX, GEDA_NAME_TAG_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_value_only, PIX, GEDA_VALUE_BITMAP, callBack_AttributeBar0);
  TOOLBAR_STD_BUTTON(Attribute, tb_name_value, PIX, GEDA_NAME_VALUE_BITMAP, callBack_AttributeBar0);

  gtk_widget_show (Attribute_Toolbar);

  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_attribute_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_designator_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_invisible_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_visible_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_add_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_promote_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_demote_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_name_only_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_value_only_button);
  ComponentToolbarButtons = g_slist_append(ComponentToolbarButtons, tb_name_value_button);

  geda_atexit((geda_atexit_func)g_slist_free, ComponentToolbarButtons);
}

/*!
 * \brief Set Senitivity of Toolbar Buttons.
 * \par Function Description
 *  This function is called by on_notebook_switch_page whenever a TAB
 *  is selected, passing a gslist of toolbar button widgets to be set
 *  to the specified sensitivity
 */
void x_toolbar_set_sensitivities(GSList *ListToolBarItems, int sensitive)
{
  lambda (GtkWidget *menu_item) {
    gtk_widget_set_sensitive(menu_item, sensitive);
    return FALSE;
  }
  mapcar(ListToolBarItems);
}

/*!
 * \brief Replease Gattrib Toolbar Resources
 * \par Function Description
 *  Destroys the Attribute and the Standard handleboxes.
 */
void x_toolbar_release_all(void)
{
  gtk_widget_destroy (Attribute_handlebox);
  gtk_widget_destroy (Standard_handlebox);
}
