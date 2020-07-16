/* -*- gschem_toplevel.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2018 Wiley Edward Hill
 * Copyright (C) 2013-2018 gEDA Contributors (see ChangeLog for details)
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
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: November, 4, 2013
 */
/*!
 * \file gschem_toplevel.c
 *
 * \brief Top-Level data structure for the Gschem Application
 */

#include <glib-object.h>

#include <gschem.h>

#include <geda_debug.h>

/** \defgroup Gschem-Top-Level Gschem Top Level
 * @{
 * \brief #GschemToplevel Class Implmentation
 * \par
 *  This module implements a top-level class in gschem.
 */

static GObjectClass *gschem_toplevel_parent_class = NULL;

/*!
 * \brief Type instance initializer for GschemToplevel
 * \par Function Description
 *  Type instance initializer for GschemToplevel.
 *
 * \param [in]  instance  The GschemToplevel being initialized.
 * \param [in]  class     The class of the type the instance is created for.
 */
static void gschem_toplevel_instance_init( GTypeInstance *instance, void *class)
{
  GschemToplevel *w_current     = (GschemToplevel *)instance;

  w_current->toplevel           = NULL;

  /* ----------------- main window widgets ----------------- */
  w_current->main_window        = NULL;
  w_current->drawing_area       = NULL;
  w_current->ui_index           = -1;

  w_current->h_scrollbar        = NULL;
  w_current->v_scrollbar        = NULL;

  w_current->macro_widget       = NULL;
  w_current->status_bar         = NULL;

  w_current->keyaccel_string    = NULL;
  w_current->keyaccel_ssid      = FALSE;

  /*  -------------------  Dialog boxes  ------------------- */
  w_current->sswindow           = NULL;
  w_current->tswindow           = NULL;
  w_current->aawindow           = NULL;
  w_current->clwindow           = NULL;
  w_current->hpwindow           = NULL;
  w_current->ltwindow           = NULL;
  w_current->prwindow           = NULL;
  w_current->ptwindow           = NULL;
  w_current->sewindow           = NULL;
  w_current->tewindow           = NULL;
  w_current->ftwindow           = NULL;
  w_current->htwindow           = NULL;
  w_current->stwindow           = NULL;
  w_current->trwindow           = NULL;

  w_current->hkwindow           = NULL;
  w_current->cowindow           = NULL;
  w_current->world_entry        = NULL;
  w_current->screen_entry       = NULL;
  w_current->tiwindow           = NULL;

  w_current->aewindow           = NULL;
  w_current->cpwindow           = NULL;
  w_current->cswindow           = NULL;
  w_current->mawindow           = NULL;
  w_current->pswindow           = NULL;
  w_current->sowindow           = NULL;

  /* -------------------- Picture placement ---------------- */
  w_current->current_pixbuf     = NULL;
  w_current->pixbuf_filename    = NULL;
  w_current->pixbuf_wh_ratio    = 0;

  /* ------------------- Drawing surfaces ------------------ */
  w_current->window             = NULL;
  w_current->cr                 = NULL;

  w_current->world_left         = default_world_left;
  w_current->world_right        = default_world_right;
  w_current->world_top          = default_world_top;
  w_current->world_bottom       = default_world_bottom;

  w_current->screen_width       = 0;
  w_current->screen_height      = 0;

  /* -------------------- Drawing state -------------------- */
  w_current->cairo_renderer     = g_object_new (EDA_TYPE_RENDERER, NULL);
  w_current->first_wx           = -1;
  w_current->first_wy           = -1;
  w_current->second_wx          = -1;
  w_current->second_wy          = -1;
  w_current->third_wx           = -1;
  w_current->third_wy           = -1;
  w_current->distance           =  0;
  w_current->magnetic_wx        = -1;
  w_current->magnetic_wy        = -1;
  w_current->rubber_visible     =  0;
  w_current->net_direction      =  0;
  w_current->which_grip         = -1;
  w_current->which_object       = NULL;
  w_current->temp_path          = NULL;

  w_current->override_color     = -1;
  w_current->override_net_color = -1;
  w_current->override_bus_color = -1;
  w_current->override_pin_color = -1;

  /* ----------------- Rubberbanding nets ------------------ */
  w_current->stretch_list = NULL;

  /* ---------------- Gschem internal state ---------------- */

  /* Buffer Related */
  w_current->buffer_number             = 0;
  w_current->clipboard_buffer          = NULL;

  /* Drag&Drop */
  w_current->drag_event                = NULL;
  w_current->dnd_state                 = 0;
  w_current->dnd_save_state            = 0;
  w_current->drag_action               = GDK_ACTION_COPY;

  /* Key States */
  w_current->CONTROLKEY                = 0;
  w_current->SHIFTKEY                  = 0;
  w_current->ALTKEY                    = 0;

  /* retained offset distance */
  w_current->offset                    = -0;

  /* Misc status flags and limits */
  w_current->drawbounding_action_mode  = FREE;
  w_current->doing_pan                 = 0;
  w_current->event_state               = SELECT;
  w_current->force_save_as             = FALSE;
  w_current->inside_redraw             = 0;
  w_current->last_drawb_mode           = LAST_DRAWB_MODE_NONE;
  w_current->min_zoom                  = 0;
  w_current->max_zoom                  = 8;

  /* Mode Control */
  w_current->inside_action             =  0;
  w_current->action_event              = gschem_event_new();

  /* Pointer Device */
  w_current->pointer_sx                = 0;
  w_current->pointer_sy                = 0;

  w_current->primary_selection         = NULL;

  /* Sessions */
  w_current->session_name              = NULL; /* Do not free */
  w_current->auto_sessions             = TRUE;

  /* ------------------ rc/user parameters ----------------- */

  /* Display Sub-System */
    CairoRenderer->draw_grips = TRUE;

  /* Grid Related - Display=>Grid */
    w_current->grid_mode               = GRID_MESH;
    w_current->dots_grid_dot_size      = DEFAULT_GRID_DOT_SIZE;
    w_current->dots_grid_threshold     = DEFAULT_GRID_DOT_THRESHOLD;
    w_current->dots_grid_mode          = DOTS_GRID_VARIABLE_MODE;

    w_current->dots_grid_minor_alpha   = DEFAULT_GRID_MINOR_ALPHA;
    w_current->dots_grid_major_alpha   = DEFAULT_GRID_MAJOR_ALPHA;

    w_current->mesh_grid_threshold     = DEFAULT_GRID_MESH_THRESHOLD;
    w_current->mesh_line_width_factor  = DEFAULT_MESH_LINE_WIDTH_FACTOR;

    w_current->mesh_grid_minor_alpha   = DEFAULT_GRID_MINOR_ALPHA;
    w_current->mesh_grid_major_alpha   = DEFAULT_GRID_MAJOR_ALPHA;

  w_current->scrollbars                = TRUE;
  w_current->scrollbar_update          = 0;
  w_current->scrollpan_steps           = DEFAULT_SCROLLPAN_STEPS;

  /* Zoom Related - Display=>Zoom */
    w_current->warp_cursor             = 0;
    w_current->zoom_gain               = DEFAULT_ZOOM_GAIN;
    w_current->zoom_with_pan           = 0;

  /* Imaging Related */
  w_current->image_width               = 0;
  w_current->image_height              = 0;

  /*    Log Related    */
  logging                              = 0;
  log_destiny                          = 0;
  console_window                       = 0;
  console_window_type                  = 0;

 /* Miscellaneous - in  alphabetical order */
  w_current->action_feedback_mode      = OUTLINE;
  w_current->add_attribute_offset      = DEFAULT_ATTRIBUTE_OFFSET;
  w_current->attribute_placement_grid  = DEFAULT_ATTRIB_PLACE_GRID;
  w_current->auto_pan                  = TRUE;
  w_current->auto_pan_step             = DEFAULT_AUTO_PAN_STEP;
  w_current->chooser_filter            = 0;
  w_current->component_select_attrlist = NULL;
  w_current->continue_component_place  = 0;
  w_current->embed_components          = 0;
  w_current->enforce_hierarchy         = 0;
  w_current->force_boundingbox         = FALSE;
  w_current->file_preview              = 0;
  w_current->handleboxes               = 0;
  w_current->include_complex           = 0;
  w_current->keyboardpan_gain          = DEFAULT_KEYBOARD_GAIN;
  w_current->magnetic_net_mode         = 0;
  w_current->netconn_rubberband        = 0;
  w_current->object_clipping           = 0;
  w_current->raise_dialog_boxes        = DEFAULT_RAISE_TIMER;
  w_current->save_ui_settings          = 1;
  w_current->select_slack_pixels       = DEFAULT_SLACK_PIXELS;
  w_current->sort_component_library    = 0;
  w_current->toolbars                  = 1;
  w_current->snap                      = SNAP_GRID;
  w_current->snap_size                 = 100;

  /* Nets and Routing */
  w_current->net_direction_mode        = TRUE;
  w_current->net_endpoint_mode         = FILLED_BOX;
  w_current->net_midpoint_mode         = FILLED_BOX;
  w_current->net_selection_mode        = NET_SELECT_NET;
  w_current->net_selection_state       = 0; /* internal, not RC */

  /* Ripper Related - Nets and Routing=>Ripper */
    w_current->bus_ripper_rotation     = 0;
    w_current->bus_ripper_size         = DEFAULT_RIPPER_SIZE;
    w_current->bus_ripper_type         = 0;
    w_current->bus_ripper_symname      = NULL;

  /* Print Related */
  w_current->print_command             = NULL;

  /* Text Related */
  w_current->text_alignment            = 0;
  w_current->text_case                 = BOTH_CASES;
  w_current->text_display_zoomfactor   = DEFAULT_TEXT_ZOOM;
  w_current->text_feedback             = ONLY_WHEN_READABLE;

  CairoRenderer->text_origin_marker    = TRUE;
  CairoRenderer->text_marker_size      = DEFAULT_TEXT_MARKER_SIZE;
  CairoRenderer->text_marker_threshold = DEFAULT_TEXT_MARKER_THLD;
  w_current->text_size                 = DEFAULT_TEXT_SIZE;

  /* Undo Sub-System */
  w_current->undo_control              = 0;
  w_current->undo_levels               = 0;
  w_current->undo_type                 = 0;
  w_current->undo_panzoom              = FALSE;
  w_current->undo_preserve             = TRUE;

  w_current->page_history              = gschem_page_history_new();

  w_current->last_image_path           = NULL;

  w_current->smob                      = SCM_UNDEFINED;
}

/*!
 * \brief GObject finalise handler for GschemToplevel
 * \par Function Description
 *  release the memory allocated to the given GschemToplevel data
 *  structure and then chain up to the parent's finalize handler.
 *
 * \param [in] object  The GschemToplevel object being finalized.
 */
static void gschem_toplevel_finalize( GObject *object )
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL(object);

  if (w_current->primary_selection != NULL) {
    g_list_free (w_current->primary_selection);
    w_current->primary_selection = NULL;
  }

  if (w_current->component_select_attrlist != NULL) {
    if (w_current->component_select_attrlist !=
        default_component_select_attrlist) {
      geda_glist_free_full (w_current->component_select_attrlist, g_free);
    }
    w_current->component_select_attrlist = NULL;
  }

  if (w_current->toolbar_mode_grp != NULL) {
    geda_gslist_free_all (w_current->toolbar_mode_grp);
    w_current->toolbar_mode_grp = NULL;
  }

  if (w_current->bus_ripper_symname != NULL) {
    GEDA_FREE (w_current->bus_ripper_symname);
  }

  if (w_current->pixbuf_filename != NULL) {
    GEDA_FREE (w_current->pixbuf_filename);
  }

  if (w_current->print_command != NULL) {
    GEDA_FREE (w_current->print_command);
  }

  if (w_current->page_history != NULL) {
    gschem_page_history_free(w_current->page_history);
    w_current->page_history = NULL;
  }

  if (w_current->last_image_path != NULL) {
    GEDA_FREE (w_current->last_image_path);
  }

  if (w_current->action_event != NULL) {
    GEDA_UNREF(w_current->action_event);;
  }

  if (CairoRenderer != NULL) {
    GEDA_UNREF (CairoRenderer);
    CairoRenderer = NULL;
  }

  if (w_current->cr != NULL) {
    cairo_destroy (w_current->cr);
    w_current->cr = NULL;
  }

  G_OBJECT_CLASS( gschem_toplevel_parent_class )->finalize( object );
}

/*!
 * \brief Type class initializer for GschemToplevel
 * \par Function Description
 *  Type class initializer for GschemToplevel. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  g_class       The GschemToplevel being initialized
 * \param [in]  g_class_data  (unused)
 */
static void gschem_toplevel_class_init( void *g_class, void *g_class_data )
{
  GschemToplevelClass *klass = GSCHEM_TOPLEVEL_CLASS( g_class );
  GObjectClass *gobject_class = G_OBJECT_CLASS( klass );
  gschem_toplevel_parent_class = g_type_class_peek_parent( klass );

  gobject_class->finalize = gschem_toplevel_finalize;

}

/*!
 * \brief Function to retrieve GschemToplevel's Type identifier.
 * \par Function Description
 *  Function to retrieve GschemToplevel's Type identifier. On first call,
 *  this registers the GattribDialog in the GType system. Subsequently
 *  the function returns the saved value from its first execution.
 *
 * \return GedaType identifier associated with GschemToplevel.
 */
GedaType gschem_toplevel_get_type(void)
{
  static GedaType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemToplevelClass),
      NULL,                            /* base_init */
      NULL,                            /* base_finalize */
      gschem_toplevel_class_init,      /* class_init */
      NULL,                            /* class_finalize */
      NULL,                            /* class_data */
      sizeof(GschemToplevel),
      0,                               /* n_preallocs */
      gschem_toplevel_instance_init    /* instance_init */
    };
    type = g_type_register_static (G_TYPE_OBJECT, "GschemToplevel", &info, 0);
  }
  return type;
}

/*!
 * \brief Returns a pointer to a new GschemToplevel object.
 * \par Function Description
 *  Returns a pointer to a new GschemToplevel object.
 *
 * \return pointer to the new GschemToplevel object.
 */
GschemToplevel *gschem_toplevel_new( void ) {
  return g_object_new( GSCHEM_TYPE_TOPLEVEL, NULL );
}

void gschem_toplevel_free(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    geda_toplevel_struct_release (w_current->toplevel);
    GEDA_UNREF (w_current);
  }
  else {
    BUG_MSG("w_current no bueno");
  }
}

/*!
 * \brief Release the Primary Selection in toplevel
 * \par Function Description
 *  The primary selection is a list of selected object utilized
 *  by operations involving two selections set. This function
 *  is called at the end of such operations to release the list.
 *
 * \param [in] w_current This gschem toplevel
 *
 * \return The libgeda toplevel
 */
void gschem_toplevel_free_primary (GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    g_list_free (w_current->primary_selection);
    w_current->primary_selection = NULL;
  }
}

/*!
 * \brief Get the Current Page from toplevel
 * \par Function Description
 * \param [in] w_current This gschem toplevel
 *
 * \return The libgeda toplevel
 */
Page *gschem_toplevel_get_current_page (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);
  g_return_val_if_fail (w_current->toplevel != NULL, NULL);

  return geda_toplevel_get_current_page(w_current->toplevel);
}

/*!
 * \brief Get the libgeda toplevel for this gschem toplevel
 * \par Function Description
 *
 * \param [in] w_current This gschem toplevel
 *
 * \return The libgeda toplevel
 */
GedaToplevel*
gschem_toplevel_get_geda_toplevel (GschemToplevel *w_current)
{
  g_return_val_if_fail (GSCHEM_IS_TOPLEVEL(w_current), NULL);

  return w_current->toplevel;
}

/*!
 * \brief Get the last_image_path in the GschemToplevel
 * \par Function Description
 *  Returns the last_image_path in \a w_current, which could be NULL.
 *  Silently returns NULL if w_current is not a GschemToplevel
 *
 * \param [in] w_current This GschemToplevel
 *
 * \returns last_image_path if \a w_current is a GschemToplevel
 */
char*
gschem_toplevel_get_last_image_path (GschemToplevel *w_current)
{
  if (w_current && GSCHEM_IS_TOPLEVEL(w_current)) {
    return w_current->last_image_path;
  }
  return NULL;
}

/*!
 * \brief Move specified Page Down
 * \par Function Description
 *  Calls geda_toplevel_move_page_down to move \a page down in the
 *  list of pages after validating \a w_current.
 *
 * \param [in] w_current This GschemToplevel
 * \param [in] page      The page that is to be moved.
 */
bool
gschem_toplevel_move_page_down (GschemToplevel *w_current, Page *page)
{
  GedaToplevel *toplevel;

  toplevel = gschem_toplevel_get_geda_toplevel (w_current);

  if (toplevel) {

    if (geda_toplevel_move_page_down(toplevel, page)) {
      return TRUE;
    }
  }

  return FALSE;
}

/*!
 * \brief Move specified Page up
 * \par Function Description
 *  Calls geda_toplevel_move_page_up to move \a page up in the list
 *  of pages after validating \a w_current.
 *
 * \param [in] w_current This GschemToplevel
 * \param [in] page      The page that is to be moved.
 */
bool
gschem_toplevel_move_page_up (GschemToplevel *w_current, Page *page)
{
  GedaToplevel *toplevel;

  toplevel = gschem_toplevel_get_geda_toplevel (w_current);

  if (toplevel) {

    if (geda_toplevel_move_page_up(toplevel, page)) {
      return TRUE;
    }
  }

  return FALSE;
}

/*!
 * \brief Set Page to the Current Page
 * \par Function Description
 *  Sets \a page to be the current page and updates history.
 *
 * \param [in] w_current This GschemToplevel
 * \param [in] page      The page that is to be the current page.
 */
bool
gschem_toplevel_set_current_page (GschemToplevel *w_current, Page *page)
{
  GedaToplevel *toplevel;
  int result;

  g_return_val_if_fail (GEDA_IS_PAGE(page), FALSE);

  toplevel = gschem_toplevel_get_geda_toplevel (w_current);

  if (toplevel) {

    gschem_page_history_push_back(w_current->page_history, page);

    result = geda_toplevel_set_current_page(toplevel, page);
  }
  else {
    result = FALSE;
  }
  return result;
}

/*!
 * \brief Set the grip size in the GschemToplevel
 * \par Function Description
 *  Set the grip size in \a w_current.
 *
 * \param [in] w_current This GschemToplevel
 * \param [in] pixels    grip size in pixel units.
 */
void
gschem_toplevel_set_grips_size (GschemToplevel *w_current, int pixels)
{
  w_current->grip_size = pixels & ~1;
}

/*!
 * \brief Set the last_image_path in the GschemToplevel
 * \par Function Description
 *  Set the last_image_path in \a w_current, which can be NULL. If \a w_current
 *  is not NULL then last_image_path will be set to \a path if \a path is also
 *  no NULL. The previous last_image_path is released if the pointer is retained
 *  otherwise the previous last_image_path is retained. Does not generate an
 *  error if either argument is NULL. This allows callers to not check the
 *  inputs, only the returned path, which callers would already need to do.
 *
 * \param [in] w_current This GschemToplevel
 * \param [in] path      New string for last_image_path.
 */
void
gschem_toplevel_set_last_image_path (GschemToplevel *w_current, char *path)
{
  if (w_current && path) {
    if (GSCHEM_IS_TOPLEVEL(w_current)) {
      if (w_current->last_image_path) {
        GEDA_FREE(w_current->last_image_path);
      }
      w_current->last_image_path = path;
    }
  }
}

/** @} endgroup Gschem-Top-Level */
