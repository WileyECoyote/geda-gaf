/* -*- C o_redraw.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file o_redraw.c
 * \brief Low-level module for redrawing objects or regions
 */

#include <gschem.h>
#include <geda_debug.h>

#define INVALIDATE_MARGIN 1

/*! \brief Function Clean State - if drawing action in-progress
 *  \par Function Description
 *  This function is necessary to make jumps between event_states.
 *  If inside a drawing action that created something on the screen,
 *  e.g. if we are drawing a box and then jump to line drawing without
 *  leaving the box drawing mode, there will remain some rubberbands on
 *  the screen. Usually a intermediate select state would clean (redraw)
 *  the screen.
 */
int o_redraw_cleanstates(GschemToplevel *w_current)
{
  /* returns FALSE if the function was'nt nessecary */
  if (w_current->inside_action == 0) {
    return FALSE;
  }

  switch (w_current->event_state) {
    /* all states with something on the dc */
    case ( COMPMODE ):

      /* De-select the lists in the component selector */
      x_compselect_deselect (w_current);

      /* Fall through */
    case ( DRAGMOVE ):
    case ( MOVEMODE ):
      o_move_cancel (w_current);
    case ( GRIPS ):
    case ( COPYMODE ):

    case ( NETMODE ):
    case ( PINMODE ):
    case ( LINEMODE ):
    case ( BOXMODE ):
    case ( CIRCLEMODE ):
    case ( TEXTMODE ):
    case ( ARCMODE ):
    case ( PATHMODE ):
    case ( PASTEMODE ):
    case ( PICTUREMODE ):
    case ( BUSMODE ):
    case ( MCOPYMODE ):

      /* it is possible to cancel in the middle of a place,
       * so lets be sure to clean up the place_list structure */

      /* If we're cancelling from a grip action, call the specific cancel
       * routine to reset the visibility of the object being modified */
      if (w_current->event_state == GRIPS) {
        o_grips_cancel (w_current);
      }

      /* Free the place list and its contents. If we were in a move
       * action, the list (refering to objects on the page) would
       * already have been cleared in o_move_cancel(), so this is OK. */
      geda_struct_place_free_place_list(w_current->toplevel);

      i_event_stop_action_handler(w_current);

      /* touch the select state */
      i_status_set_state(w_current, SELECT);

      /* from i_callback_cancel() */
      o_invalidate_all (w_current);
      return TRUE;

    /* all remaining states without dc changes */
    case ( NONE ):
    case ( SELECT ):
    case ( PAN ):
    case ( ENDMIRROR ):
    case ( ENDROTATE ):
    case ( SBOX ):
    case ( STARTDESELECT ):
    case ( STARTSELECT ):
    case ( ZOOMBOX ):
      return FALSE;
  }

  i_event_cancel_action_handler (w_current);

  return FALSE;
}

/*! \brief Redraw a rectangular region of the drawing area
 *  \par Function Description
 *  Main drawing routine for gschem. This procedure is not normally used
 *  to draw the temporary "rubber" object during event motions. Objects
 *  are passed to the Renderer in 3 passes; First for non selected objects,
 *  secondly, for cues and markers and finally selected object and this has
 *  the effect of having the selected object in the foreground while
 *  minimizing obstruction of cues and markers. Any "rubber" object that
 *  need to be draw are handled afterwards by calling the corresponding
 *  "draw_rubber" handler.
 *
 */
void o_redraw_rectangle (GschemToplevel *w_current, GdkRectangle *rectangle)
{
  GedaToplevel *toplevel = w_current->toplevel;

  bool draw_selected;
  bool is_only_text;

  int bloat;
  int cue_half_size;
  int grip_half_size;
  int render_flags;

  int x       = rectangle->x;
  int y       = rectangle->y;
  int width   = rectangle->width;
  int height  = rectangle->height;

  GList *obj_list;
  GList *iter;

  RECTANGLE      world_rect;
  EdaRenderer   *renderer;
  cairo_matrix_t render_mtx;

  GArray *render_color_map         = NULL;
  GArray *render_outline_color_map = NULL;

  g_return_if_fail (w_current->toplevel != NULL);
  g_return_if_fail (w_current->toplevel->page_current != NULL);

  grip_half_size = gschem_toplevel_get_grips_half_size (w_current);

  cue_half_size = SCREENabs (w_current, CUE_BOX_SIZE);
  bloat = MAX (grip_half_size, cue_half_size);

  SCREENtoWORLD (w_current, x - bloat, y + height + bloat, &world_rect.lower_x, &world_rect.lower_y);
  SCREENtoWORLD (w_current, x + width + bloat, y - bloat, &world_rect.upper_x, &world_rect.upper_y);

  obj_list = geda_struct_page_objects_in_regions (toplevel->page_current, &world_rect, 1);

  render_flags = EDA_RENDERER_FLAG_HINTING;

  is_only_text = TRUE;
  iter = g_list_first(obj_list);
  while(iter != NULL) {
    GedaObject *object = iter->data;
    if (object->type != OBJ_TEXT) {
      is_only_text = FALSE;
      break;
    }
    iter = iter->next;
  }

  if (w_current->fast_mousepan && w_current->doing_pan) {
    render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE
                 |   EDA_RENDERER_FLAG_PICTURE_OUTLINE);
  }
  else {

    int zoom = toplevel->page_current->to_world_x_constant;

    if ((zoom > w_current->text_display_zoomfactor) &&
       (w_current->text_feedback != ALWAYS_FEEDBACK))
    {
      render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE);
    }
  }

  if ((is_only_text) &&
     (w_current->text_feedback != ALWAYS_FEEDBACK) &&
     (w_current->inside_action))
  {
    render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE);
  }

  /* The display color map is used for "normal" rendering. */
  render_color_map = geda_color_get_display_map();

  /* Retrive a copy of the outl ine color map used for rendering rubber
   * banding nets and buses, and objects which are in the process of
   * being placed. */
  render_outline_color_map = geda_color_get_outline_map();

  /* Set up renderer */
  renderer = g_object_ref (CairoRenderer);

  g_object_set (renderer,
                "cairo-context", w_current->cr,
                "grip-size", gschem_toplevel_get_double_world_size(w_current, grip_half_size),
                "render-flags", render_flags,
                "color-map", render_color_map,
                NULL);

  cairo_save (w_current->cr);

  /* Transform the cairo context to world coordinates */
  cairo_matrix_init (&render_mtx,
                    (double) Current_Page->to_screen_x_constant, 0, 0,
                   -(double) Current_Page->to_screen_y_constant,
                   -(double) Current_Page->to_screen_x_constant * Current_Page->left,
                    (double) Current_Page->to_screen_y_constant * Current_Page->top +
                                                                  w_current->screen_height);

  cairo_set_matrix (w_current->cr, &render_mtx);

#if WITH_LIBGEDADRAW

  x_draw_set_surface (w_current);

#endif

  /* First pass -- render non-selected objects */
  for (iter = obj_list; iter != NULL; iter = iter->next) {

   GedaObject *o_current = iter->data;

    if (!(o_current->dont_redraw || o_current->selected)) {
        geda_object_style_set_line_width(w_current->toplevel, o_current);

#ifdef WITH_LIBGEDADRAW

        x_draw_object(w_current, o_current);

#else

        eda_renderer_draw (CairoRenderer, o_current);

#endif

    }
  }

  if (!is_only_text) {
    /* Second pass -- render cues */
    for (iter = obj_list; iter != NULL; iter = iter->next) {
     GedaObject *o_current = iter->data;
      if (!(o_current->dont_redraw || o_current->selected)) {
        eda_renderer_draw_cues (renderer, o_current);
      }
    }
  }

  /* Determine whether we should draw the selection at all */
  draw_selected = !(w_current->inside_action && (w_current->event_state == MOVEMODE ||
                                                 w_current->event_state == DRAGMOVE));

  /* Third pass -- render selected objects, cues & grips. This is
   * done in a separate pass to non-selected items to make sure that
   * the selection and grips are never obscured by other objects. */
  if (draw_selected) {

    g_object_set (renderer, "override-color", SELECT_COLOR, NULL);

    for (iter = Current_Selection->glist; iter; iter = iter->next) {

     GedaObject *o_current = iter->data;

      if (!o_current->dont_redraw) {

        geda_object_style_set_line_width(w_current->toplevel, o_current);

#ifdef WITH_LIBGEDADRAW

        x_draw_object(w_current, o_current);

#else

        eda_renderer_draw (CairoRenderer, o_current);

#endif

        eda_renderer_draw_cues (renderer, o_current);

        if (CairoRenderer->draw_grips ) {
          eda_renderer_draw_grips (renderer, o_current);
        }
      }
    }

    /* Disable/reset "override-color" mode */
    g_object_set (renderer, "override-color", -1, NULL);
  }

  if (w_current->event_state == NETMODE) {
      eda_renderer_set_color_map (renderer, render_outline_color_map);
      o_net_draw_rubber (w_current);
      eda_renderer_set_color_map (renderer, render_color_map);
  }
  else if (w_current->inside_action) {

    /* Redraw the rubberband objects if previously visible */
    if (w_current->rubber_visible) {

      if (Current_PlaceList != NULL) {

        switch (w_current->event_state) {

          case COPYMODE:
          case COMPMODE:
          case TEXTMODE:
          case MCOPYMODE:
          case PASTEMODE:
            eda_renderer_set_color_map (renderer, render_outline_color_map);
            o_place_draw_rubber (w_current, draw_selected);
            eda_renderer_set_color_map (renderer, render_color_map);

          case DRAGMOVE:
          case MOVEMODE:

            if (w_current->last_drawb_mode != LAST_DRAWB_MODE_NONE) {
              eda_renderer_set_color_map (renderer, render_outline_color_map);
              o_move_draw_rubber (w_current, draw_selected);
              eda_renderer_set_color_map (renderer, render_color_map);
            }
            break;

          default:
            break;
        }
      }
      else {

        switch (w_current->event_state) {

          case GRIPS:
            o_grips_draw_rubber (w_current);
            break;

          case SBOX:
            o_select_box_draw_rubber (w_current);
            break;

          case ZOOMBOX:
            i_zoom_world_box_draw_rubber (w_current);
            break;

          case PINMODE:
            o_pin_draw_rubber (w_current);
            break;

          case LINEMODE:
            o_line_draw_rubber (w_current);
            break;

          case BOXMODE:
            o_box_draw_rubber (w_current);
            break;

          case CIRCLEMODE:
            o_circle_draw_rubber (w_current);
            break;

          case ARCMODE:
            o_arc_draw_rubber (w_current);
            break;

          case PATHMODE:
            o_path_draw_rubber (w_current);
            break;

          case PICTUREMODE:
            o_picture_draw_rubber (w_current);
            break;

          case BUSMODE:
            eda_renderer_set_color_map (renderer, render_outline_color_map);
            o_bus_draw_rubber(w_current);
            eda_renderer_set_color_map (renderer, render_color_map);
            break;
        }
      }
    }
    else if (w_current->event_state == MOVEMODE ||
             w_current->event_state == DRAGMOVE)
    {

      if (w_current->last_drawb_mode != -1) {
        eda_renderer_set_color_map (renderer, render_outline_color_map);
        o_move_draw_rubber (w_current, draw_selected);
        eda_renderer_set_color_map (renderer, render_color_map);
      }
    }
  }

  cairo_restore (w_current->cr);

  g_list_free (obj_list);
  GEDA_UNREF (renderer);
  g_array_free (render_color_map, TRUE);
  g_array_free (render_outline_color_map, TRUE);

}

/*!
 * \brief Redraw a List of Objects
 * \par Function Description
 * Causes the region bounded by the bounds of the objects in the list
 * to be redrawn.
 */
void o_redraw_list (GschemToplevel *w_current, GList *list)
{
  int left, top, bottom, right;

  if (geda_object_get_bounds_list (list, &left,  &top, &right, &bottom)) {

    GdkRectangle rect;

    WORLDtoSCREEN (w_current, left,  top,    &rect.x, &rect.y);
    WORLDtoSCREEN (w_current, right, bottom, &rect.width, &rect.height);

    o_redraw_rectangle (w_current, &rect);
  }
}
