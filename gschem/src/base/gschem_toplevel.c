/* -*- gschem_page.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
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
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */

#include <gschem.h>
#include <glib-object.h>
#include "gschem_toplevel.h"
#include <geda_debug.h>

static GObjectClass *gschem_toplevel_parent_class = NULL;

/*! \brief Type instance initialiser for GschemToplevel
 *
 *  \par Function Description
 *  Type instance initialiser for GschemToplevel.
 *
 *  \param [in]  instance       The GschemToplevel we are initialising.
 *  \param [in]  g_class        The class of the type the instance is created for.
 */
static void gschem_toplevel_instance_init( GTypeInstance *instance, void * g_class )
{
  GschemToplevel *w_current = (GschemToplevel *)instance;

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
  w_current->iwindow            = NULL;
  w_current->mawindow           = NULL;
  w_current->pswindow           = NULL;
  w_current->sowindow           = NULL;

  /* -------------------- Picture placement ---------------- */
  w_current->current_pixbuf     = NULL;
  w_current->pixbuf_filename    = NULL;
  w_current->pixbuf_wh_ratio    = 0;

  /* ---------------- graphics context stuff --------------- */
  w_current->gc                 = NULL;

  /* ------------------- Drawing surfaces ------------------ */
  w_current->window             = NULL;
  w_current->drawable           = NULL;
  w_current->cr                 = NULL;

  w_current->world_left         = 0;
  w_current->world_right        = default_world_right;
  w_current->world_top          = 0;
  w_current->world_bottom       = default_world_bottom;

  w_current->screen_width       = 0;
  w_current->screen_height      = 0;

  /* -------------------- Drawing state -------------------- */
  w_current->cairo_renderer     = EDA_RENDERER (g_object_new (EDA_TYPE_RENDERER, NULL));
  w_current->first_wx           = -1;
  w_current->first_wy           = -1;
  w_current->second_wx          = -1;
  w_current->second_wy          = -1;
  w_current->third_wx           = -1;
  w_current->third_wy           = -1;
  w_current->distance           =  0;
  w_current->magnetic_wx        = -1;
  w_current->magnetic_wy        = -1;
  w_current->inside_action      =  0;
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

  /* Misc status flags and limits */
  w_current->drawbounding_action_mode  = FREE;
  w_current->doing_pan                 = 0;
  w_current->event_state               = SELECT;
  w_current->force_save_as             = FALSE;
  w_current->inside_redraw             = 0;
  w_current->last_drawb_mode           = LAST_DRAWB_MODE_NONE;
  w_current->min_zoom                  = 0;
  w_current->max_zoom                  = 8;

  /* Pointer Device */
  w_current->pointer_sx                = 0;
  w_current->pointer_sy                = 0;

  /* Sessions */
  w_current->session_name              = NULL;
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
  w_current->background_color          = 0;

  /*    Log Related    */
  logging                              = 0;
  log_destiny                          = 0;
  console_window                       = 0;
  console_window_type                  = 0;

 /* Miscellaneous - in  alphabetical order */
  w_current->action_feedback_mode      = OUTLINE;
  w_current->add_attribute_offset      = DEFAULT_ATTRIBUTE_OFFSET;
  w_current->attribute_placement_grid  = DEFAULT_ATTRIB_PLACE_GRID;
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
  w_current->toolbars                  = 0;
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
  w_current->smob                      = SCM_UNDEFINED;
}

/*! \brief GObject finalise handler for GschemToplevel
 *
 *  \par Function Description
 *  release the memory allocated to the given GschemToplevel data
 *  structure and then chain up to the parent's finalize handler.
 *
 *  \param [in] object  The GschemToplevel object being finalized.
 */
static void gschem_toplevel_finalize( GObject *object )
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL(object);

  if (w_current->toolbar_mode_grp != NULL) {
    g_slist_foreach (w_current->toolbar_mode_grp,(GFunc) g_free, NULL);
    g_slist_free (w_current->toolbar_mode_grp);
    w_current->toolbar_mode_grp = NULL;
  }

  if (w_current->bus_ripper_symname != NULL) {
    GEDA_FREE (w_current->bus_ripper_symname);
    w_current->bus_ripper_symname = NULL;
  }

  if (w_current->pixbuf_filename != NULL) {
    GEDA_FREE (w_current->pixbuf_filename);
    w_current->pixbuf_filename = NULL;
  }

  if (w_current->print_command != NULL) {
    GEDA_FREE (w_current->print_command);
    w_current->print_command = NULL;
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

/*! \brief Type class initialiser for GschemToplevel
 *
 *  \par Function Description
 *  Type class initialiser for GschemToplevel. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class       The GschemToplevel we are initialising
 *  \param [in]  g_class_data  (unused)
 */
static void gschem_toplevel_class_init( void *g_class, void *g_class_data )
{
  GschemToplevelClass *klass = GSCHEM_TOPLEVEL_CLASS( g_class );
  GObjectClass *gobject_class = G_OBJECT_CLASS( klass );
  gschem_toplevel_parent_class = g_type_class_peek_parent( klass );

  gobject_class->finalize = gschem_toplevel_finalize;

}

/*! \brief Function to retrieve GschemToplevel's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GschemToplevel's Type identifier. On first call,
 *  this registers the GattribDialog in the GedaType system. Subsequently
 *  the function returns the saved value from its first execution.
 *
 *  \return GedaType identifier associated with GschemToplevel.
 */
GedaType gschem_toplevel_get_type(void)
{
  static GedaType type = 0;
  if (type == 0) {
    static const GTypeInfo info = {
      sizeof (GschemToplevelClass),
      NULL,                            /* base_init */
      NULL,                            /* base_finalize */
      gschem_toplevel_class_init,         /* class_init */
      NULL,                            /* class_finalize */
      NULL,                            /* class_data */
      sizeof (GschemToplevel),
      0,                               /* n_preallocs */
      gschem_toplevel_instance_init    /* instance_init */
    };
    type = g_type_register_static (G_TYPE_OBJECT, "GschemToplevel", &info, 0);
  }
  return type;
}

/*! \brief Returns a pointer to a new GschemToplevel object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GschemToplevel object.
 *
 *  \return pointer to the new GschemToplevel object.
 */
GschemToplevel *gschem_toplevel_new( void ) {
  return g_object_new( GSCHEM_TYPE_TOPLEVEL, NULL );
}

void gschem_toplevel_free(GschemToplevel *w_current)
{
  if ( GSCHEM_IS_TOPLEVEL(w_current)) {
    s_toplevel_release (w_current->toplevel);
    GEDA_UNREF (w_current);
  }
  else
    BUG_MSG("w_current no bueno")

}
