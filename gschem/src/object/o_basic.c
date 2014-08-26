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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <geda_debug.h>

#define INVALIDATE_MARGIN 1

/*! \todo Lots of Gross code... needs lots of cleanup - mainly
 * readability issues
 */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_redraw_rects (GschemToplevel *w_current, GdkRectangle *rectangles, int n_rectangles)
{
  GedaToplevel *toplevel = w_current->toplevel;

  bool draw_selected;
  bool is_only_text;

  int bloat;
  int cue_half_size;
  int grip_half_size;
  int i;
  int render_flags;
  int zoom;

  GList *obj_list;
  GList *iter;
  RECTANGLE   *world_rects;
  EdaRenderer *renderer;

  GArray *render_color_map         = NULL;
  GArray *render_outline_color_map = NULL;
  cairo_matrix_t render_mtx;

  for (i = 0; i < n_rectangles; i++) {
    x_repaint_background_region (w_current, rectangles[i].x, rectangles[i].y,
                                 rectangles[i].width, rectangles[i].height);
  }

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (toplevel->page_current != NULL);

  grip_half_size = w_current->grip_pixel_size / 2;

  /*grip_half_size = o_grips_half_size (w_current, NULL); */

  cue_half_size = SCREENabs (w_current, CUE_BOX_SIZE);
  bloat = MAX (grip_half_size, cue_half_size);

  world_rects = g_new (RECTANGLE, n_rectangles);

  for (i = 0; i < n_rectangles; i++) {
    int x, y, width, height;

    x = rectangles[i].x;
    y = rectangles[i].y;
    width = rectangles[i].width;
    height = rectangles[i].height;

    SCREENtoWORLD (w_current, x - bloat, y + height + bloat,
                   &world_rects[i].lower_x, &world_rects[i].lower_y);
    SCREENtoWORLD (w_current, x + width + bloat, y - bloat,
                   &world_rects[i].upper_x, &world_rects[i].upper_y);
  }

  obj_list = s_page_objects_in_regions (toplevel->page_current,
                                        world_rects, n_rectangles);

  GEDA_FREE (world_rects);

  /* Set up renderer based on configuration in w_current and list */
  render_flags = EDA_RENDERER_FLAG_HINTING;
 /* if (toplevel->page_current->show_hidden_text) {
    render_flags |= EDA_RENDERER_FLAG_TEXT_HIDDEN;
  }*/

  is_only_text = TRUE;
  iter = g_list_first(obj_list);
  while(iter != NULL) {
    Object *object = iter->data;
    if (object->type != OBJ_TEXT) {
       is_only_text = FALSE;
       break;
    }
    NEXT(iter);
  }

  if (w_current->fast_mousepan && w_current->doing_pan) {
    render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE
                 |   EDA_RENDERER_FLAG_PICTURE_OUTLINE);
  }
  else {
    zoom = toplevel->page_current->to_world_x_constant;
    if ((zoom > w_current->text_display_zoomfactor) &&
        (w_current->text_feedback != ALWAYS_FEEDBACK))
       render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE);
  }

  if ((is_only_text) &&
      ( w_current->text_feedback != ALWAYS_FEEDBACK) &&
      ( w_current->inside_action))
     render_flags |= (EDA_RENDERER_FLAG_TEXT_OUTLINE);

  /* The display color map is used for "normal" rendering. */
  render_color_map = x_color_get_display_color_map();

  /* Retrive a copy pf the outine color map used for rendering rubber
   * banding nets and buses, and objects which are in the process of
   * being placed. */
  render_outline_color_map = x_color_get_outline_color_map();

  /* Set up renderer */
  renderer = g_object_ref (w_current->renderer);

  g_object_set (G_OBJECT (renderer),
                "cairo-context", w_current->cr,
                "grip-size", ((double) grip_half_size * toplevel->page_current->to_world_x_constant),
                "render-flags", render_flags,
                "color-map", render_color_map,
                NULL);

  /* We need to transform the cairo context to world coordinates while
   * we're drawing using the renderer. */
  cairo_matrix_init (&render_mtx,
                     (double) toplevel->page_current->to_screen_x_constant,
                     0,
                     0,
                     - (double) toplevel->page_current->to_screen_y_constant,
                     (- (double) toplevel->page_current->left * toplevel->page_current->to_screen_x_constant),
                     ((double) toplevel->page_current->to_screen_y_constant * toplevel->page_current->top + w_current->screen_height)
                    );

  cairo_save (w_current->cr);
  cairo_set_matrix (w_current->cr, &render_mtx);

  /* Determine whether we should draw the selection at all */
  draw_selected = !(w_current->inside_action &&
                  ((w_current->event_state == MOVE) ||
                   (w_current->event_state == ENDMOVE)));

  /* First pass -- render non-selected objects */
  for (iter = obj_list; iter != NULL; NEXT(iter)) {
    Object *o_current = iter->data;
    if (!(o_current->dont_redraw || o_current->selected)) {
      o_style_set_object(toplevel, o_current);
      eda_renderer_draw (renderer, o_current);
    }
  }

  if (!is_only_text) {
    /* Second pass -- render cues */
    for (iter = obj_list; iter != NULL; NEXT(iter)) {
      Object *o_current = iter->data;
      if (!(o_current->dont_redraw || o_current->selected)) {
        /* o_style_set_object(toplevel, o_current); cues don't have style yet*/
        eda_renderer_draw_cues (renderer, o_current);
      }
    }
  }

  /* Third pass -- render selected objects, cues & grips. This is
   * done in a separate pass to non-selected items to make sure that
   * the selection and grips are never obscured by other objects. */
  if (draw_selected) {

    g_object_set (G_OBJECT (renderer), "override-color", SELECT_COLOR, NULL);
    for (iter = geda_list_get_glist (toplevel->page_current->selection_list);
         iter != NULL; NEXT(iter)) {
      Object *o_current = iter->data;
      if (!o_current->dont_redraw) {
        o_style_set_object(toplevel, o_current);
        eda_renderer_draw (renderer, o_current);
        eda_renderer_draw_cues (renderer, o_current);
        if (w_current->renderer->draw_grips ) {
          /* get the dynamic size of the grip */
          //grip_half_size = o_grips_half_size (w_current, o_current);
          //g_object_set (G_OBJECT (renderer), "grip-size",((double) grip_half_size * toplevel->page_current->to_world_x_constant), NULL);
          eda_renderer_draw_grips (renderer, o_current);
        }
      }
    }
    g_object_set (G_OBJECT (renderer), "override-color", -1, NULL);
  }

  if (w_current->inside_action) {
    /* Redraw the rubberband objects (if they were previously visible) */

    switch (w_current->event_state) {
      case MOVE:
      case ENDMOVE:
        if (w_current->last_drawb_mode != -1) {
          /* FIXME shouldn't need to save/restore matrix/colormap here */
          cairo_save (w_current->cr);
          cairo_set_matrix (w_current->cr, &render_mtx);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_move_draw_rubber (w_current, draw_selected);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (w_current->cr);
        }
        break;

      case ENDCOPY:
      case ENDMCOPY:
      case ENDCOMP:
      case ENDTEXT:
      case ENDPASTE:
        if (w_current->rubber_visible) {
          /* FIXME shouldn't need to save/restore matrix/colormap here */
          cairo_save (w_current->cr);
          cairo_set_matrix (w_current->cr, &render_mtx);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_place_draw_rubber (w_current, draw_selected);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (w_current->cr);
        }
        break;

      case STARTDRAWNET:
      case DRAWNET:
      case NETCONT:
        if (w_current->rubber_visible) {
          /* FIXME shouldn't need to save/restore matrix/colormap here */
          cairo_save (w_current->cr);
          cairo_set_matrix (w_current->cr, &render_mtx);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_net_draw_rubber (w_current);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (w_current->cr);
        }
        break;

      case STARTDRAWBUS:
      case DRAWBUS:
      case BUSCONT:
        if (w_current->rubber_visible) {
          /* FIXME shouldn't need to save/restore matrix/colormap here */
          cairo_save (w_current->cr);
          cairo_set_matrix (w_current->cr, &render_mtx);
          eda_renderer_set_color_map (renderer, render_outline_color_map);

          o_bus_draw_rubber(w_current);

          eda_renderer_set_color_map (renderer, render_color_map);
          cairo_restore (w_current->cr);
        }
        break;

      case GRIPS:
        if (w_current->rubber_visible)
          o_grips_draw_rubber (w_current);
        break;

      case SBOX:
        if (w_current->rubber_visible)
          o_select_box_draw_rubber (w_current);
        break;

      case ZOOMBOXEND:
        if (w_current->rubber_visible)
          i_zoom_world_box_draw_rubber (w_current);
        break;

      case ENDLINE:
        if (w_current->rubber_visible)
          o_line_draw_rubber (w_current);
        break;
      case PATHCONT:
      case ENDPATH:
        if (w_current->rubber_visible)
          o_path_draw_rubber (w_current);
        break;

      case ENDBOX:
        if (w_current->rubber_visible)
          o_box_draw_rubber (w_current);
        break;

      case ENDPICTURE:
        if (w_current->rubber_visible)
          o_picture_draw_rubber (w_current);
        break;

      case ENDCIRCLE:
        if (w_current->rubber_visible)
          o_circle_draw_rubber (w_current);
        break;

      case ENDARC:
        if (w_current->rubber_visible)
          o_arc_draw_rubber (w_current);
        break;

      case ENDPIN:
        if (w_current->rubber_visible)
          o_pin_draw_rubber (w_current);
        break;
    }
  }

  g_list_free (obj_list);
  GEDA_UNREF (renderer);
  g_array_free (render_color_map, TRUE);
  g_array_free (render_outline_color_map, TRUE);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_invalidate_rubber (GschemToplevel *w_current)
{
  /* return FALSE if it did not erase anything */

  if (!w_current->inside_action)
    return(FALSE);

  switch(w_current->event_state) {

    case ( STARTDRAWBUS ):
    case ( DRAWBUS ):
    case ( BUSCONT ):
      o_bus_invalidate_rubber (w_current);
      break;

    case ( STARTDRAWNET ):
    case ( DRAWNET ):
    case ( NETCONT ):
      o_net_invalidate_rubber (w_current);
      break;

    case ( DRAWPIN ):
    case ( ENDPIN ):
      o_pin_invalidate_rubber (w_current);
      break;

    case ( DRAWLINE ):
    case ( ENDLINE ):
      o_line_invalidate_rubber (w_current);
      break;

    case ( DRAWPATH ):
    case ( PATHCONT ):
    case ( ENDPATH ):
      o_path_invalidate_rubber (w_current);
      break;

    case ( DRAWBOX ):
    case ( ENDBOX ):
      o_box_invalidate_rubber (w_current);
      break;

    case ( DRAWPICTURE ):
    case ( ENDPICTURE ):
      o_picture_invalidate_rubber (w_current);
      break;

    case ( DRAWCIRCLE ):
    case ( ENDCIRCLE ):
      o_circle_invalidate_rubber (w_current);
      break;

    case ( DRAWARC ):
    case ( ENDARC ):
      o_arc_invalidate_rubber (w_current);
      break;

    default:
      return(FALSE);
      break;
  }

  return(TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief Function Clean State - if drawing action in-progress
 *  \par Function Description
 *  This function is neccesary to make jumps between event_states.
 *  If we are inside an drawing action that created something on the dc,
 *  e.g. if we are drawing a box and then jump to line drawing without
 *  leaving the box drawing mode, there will remain some rubberbands on the
 *  screen.
 *  Usually a intermediate select state would clean (redraw) the screen.
 */
int o_redraw_cleanstates(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  /* returns FALSE if the function was'nt nessecary */
  if (w_current->inside_action == 0) {
    return FALSE;
  }

  switch (w_current->event_state) {
    /* all states with something on the dc */
    case ( ENDCOMP ):

      /* De-select the lists in the component selector */
      x_compselect_deselect (w_current);

      /* Fall through */
    case ( COPY ):
    case ( MCOPY ):
    case ( DRAWBUS ):
    case ( DRAWNET ):
    case ( ENDARC ):
    case ( ENDBOX ):
    case ( ENDCIRCLE ):
    case ( ENDCOPY ):
    case ( ENDMCOPY ):
    case ( ENDLINE ):
    case ( PATHCONT ):
    case ( ENDPATH ):
    case ( ENDMOVE ):
    case ( ENDPASTE ):
    case ( ENDPIN ):
    case ( ENDTEXT ):
    case ( GRIPS ):
    case ( MOVE ):
    case ( NETCONT ):
    case ( ZOOMBOXEND ):
      /* it is possible to cancel in the middle of a place,
       * so lets be sure to clean up the place_list structure */

      /* If we're cancelling from a move action, re-wind the
       * page contents back to their state before we started. */
      if ((w_current->event_state == MOVE) ||
          (w_current->event_state == ENDMOVE)) {
        o_move_cancel (w_current);
      }

      /* If we're cancelling from a grip action, call the specific cancel
       * routine to reset the visibility of the object being modified */
      if (w_current->event_state == GRIPS)
        o_grips_cancel (w_current);

      /* Free the place list and its contents. If we were in a move
       * action, the list (refering to objects on the page) would
       * already have been cleared in o_move_cancel(), so this is OK. */
      s_object_release_objects(toplevel->page_current->place_list);
      toplevel->page_current->place_list = NULL;

      w_current->inside_action = 0;

      /* touch the select state */
      i_status_set_state(w_current, SELECT);

      /* from i_callback_cancel() */
      o_invalidate_all (w_current);
      return TRUE;

    /* all remaining states without dc changes */
    case ( NONE ):
    case ( SELECT ):
    case ( DRAWLINE ):
    case ( DRAWBOX ):
    case ( DRAWCIRCLE ):
    case ( ZOOM ):
    case ( PAN ):
    case ( BUSCONT ):
    case ( DRAWARC ):
    case ( DRAWPICTURE ):
    case ( DRAWPIN ):
    case ( ENDMIRROR ):
    case ( ENDPICTURE ):
    case ( ENDROTATEP ):
    case ( ENDROUTENET ):
    case ( MOUSEPAN ):
    case ( SBOX ):
    case ( STARTCOPY ):
    case ( STARTMCOPY ):
    case ( STARTDRAWBUS ):
    case ( STARTDRAWNET ):
    case ( STARTMOVE ):
    case ( STARTPAN ):
    case ( STARTPASTE ):
    case ( STARTROUTENET ):
    case ( STARTDESELECT ):
    case ( STARTSELECT ):
    case ( ZOOMBOXSTART ):
      return FALSE;
  }
  return FALSE;
}

/*! \brief Invalidates a rectangular region of the on screen drawing area
 *  \par Function Description
 *
 *  Given a pair of (x,y) coordinates in SCREEN units, invalidate the
 *  rectangular on-screen drawing area which has those two coordinate
 *  pairs as opposite corners of its region. This will cause that region
 *  to be blitted from the back-buffer once the mainloop reaches idle.
 *
 *  A margin, INVALIDATE_MARGIN is added to the invalidated region as
 *  a hacky workaround for rounding errors which may occur in the
 *  WORLD -> SCREEN coordinate transform. This margin may also be used
 *  to expand the invalidated region if anti-aliased drawing is ever
 *  used.
 *
 *  A further, larger margin is added to account for invalidating the
 *  size occupied by an object's grips.
 *
 *  If the GschemToplevel in question is not rendering to a GDK_WINDOW,
 *  (e.g. image export), this function call is a no-op. A test is used:
 *  GDK_IS_WINDOW(), which should be safe since in either case,
 *  w_current->window is a GObject. This is really a _HACK_,
 *  and should be fixed with a re-worked drawing model.
 *
 *  \param [in] w_current  The GschemToplevel who's drawing area is being
 *                         invalidated.
 *  \param [in] x1         X coord for corner 1 (SCREEN units)
 *  \param [in] y1         Y coord for corner 1 (SCREEN units)
 *  \param [in] x2         X coord for corner 2 (SCREEN units)
 *  \param [in] y2         Y coord for corner 2 (SCREEN units)
 */
void o_invalidate_rect (GschemToplevel *w_current,
                        int x1, int y1, int x2, int y2)
{
  GdkRectangle rect;
  int grip_half_size;
  int cue_half_size;
  int bloat;

  /* Ensure we only invalidate GdkWindows - probably wasting time here */
  if (GDK_IS_WINDOW( w_current->window )) {

    grip_half_size = w_current->grip_pixel_size / 2;
    //grip_half_size = o_grips_half_size (w_current, NULL);

    cue_half_size  = SCREENabs (w_current, CUE_BOX_SIZE);

    bloat = MAX (grip_half_size, cue_half_size) + INVALIDATE_MARGIN;

    rect.x = MIN(x1, x2) - bloat;
    rect.y = MIN(y1, y2) - bloat;
    rect.width =  1 + abs( x1 - x2 ) + 2 * bloat;
    rect.height = 1 + abs( y1 - y2 ) + 2 * bloat;

    gdk_window_invalidate_rect( w_current->window, &rect, FALSE );
  }
  else
    BUG_MSG("w_current->window no bueno");
}

/*! \brief Invalidate the whole on-screen area
 *
 *  \par Function Description
 *  This function calls gdk_window_invalidate_rect() with a rect
 *  of NULL, causing the entire drawing area to be invalidated.
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void o_invalidate_all (GschemToplevel *w_current)
{
  if (w_current && GDK_IS_WINDOW(w_current->window)) {
    gdk_window_invalidate_rect (w_current->window, NULL, FALSE);
  }
  else
    BUG_MSG("w_current->window no bueno")
}

/*! \brief Invalidate on-screen area for an object
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect(), after validating the bounds of
 *  the passed Object, converted to screen coordinates.
 *
 *  \sa o_invalidate_force
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The Object invalidated on screen.
 */
void o_invalidate (GschemToplevel *w_current, Object *object)
{
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;

  if (world_get_single_object_bounds(object, &left,  &top, &right, &bottom)) {
    WORLDtoSCREEN (w_current, left, top, &s_left, &s_top);
    WORLDtoSCREEN (w_current, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left, s_top, s_right, s_bottom);
  }
  else {
    if (o_get_is_visible(object)) {
      fprintf(stderr, "o_invalidate could not get bounds <%s>\n", object->name);
    }
  }
}

/*! \brief Force invalidate on-screen area for an object
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect() with the bounds of the
 *  passed Object, converted to screen coordinates.
 *
 *  \sa o_invalidate
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] object     The Object invalidated on screen.
 */
void
o_invalidate_force(GschemToplevel *w_current, Object *object)
{
  int s_left, s_top, s_bottom, s_right;
  WORLDtoSCREEN (w_current, object->left,  object->top,    &s_left, &s_top);
  WORLDtoSCREEN (w_current, object->right, object->bottom, &s_right, &s_bottom);
  o_invalidate_rect (w_current, s_left, s_top, s_right, s_bottom);
}
/*! \brief Invalidate on-screen area for a GList of objects
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect() with the bounds of the
 *  passed GList, converted to screen coordinates.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] list       The glist objects invalidated on screen.
 */
void o_invalidate_glist (GschemToplevel *w_current, GList *list)
{
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;
  if (world_get_object_glist_bounds (list, &left,  &top, &right, &bottom)) {
    WORLDtoSCREEN (w_current, left,  top,    &s_left, &s_top);
    WORLDtoSCREEN (w_current, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left, s_top, s_right, s_bottom);
  }
}

/*! \brief Returns the color an object should be drawn in
 *  \par Function Description
 *  This function looks up and returns either the SELECT_COLOR, or the
 *  Object's natural colour, as appropriate. If toplevel->override_color
 *  is set, that takes precedence.
 *
 *  The parent field of the Object structure is used to recurse down
 *  and check whether the Object being drawn is a prim_obj belonging
 *  to some selected Object. If so, SELECT_COLOR is used.
 *
 *  As a convenience, the appropriate color index is looked up using
 *  x_color_lookup(), so that code is not duplicated in each drawing
 *  function.
 *
 *  \param [in] w_current   The GschemToplevel object.
 *  \param [in] object      The Object whos color to return.
 *
 *  \return Index of color to use for this object.
 *
 *  \remarks The function is not currently use in gschem
 */
int o_drawing_color (GschemToplevel *w_current, Object *object)
{
  int color_idx;
  Object *temp;

  color_idx = object->color;

  if (object->selected)
    color_idx = SELECT_COLOR;

  /* Check if the object, or its parent(s) are selected */
  for (temp = object; temp != NULL; temp = temp->parent_object) {
    if (temp->selected) {
      color_idx = SELECT_COLOR;
      break;
    }
  }

  if (w_current->override_color != -1)
    color_idx = w_current->override_color;

  return color_idx;
}

/*! \brief Update a component.
 *
 * \par Function Description
 * Updates \a o_current to the latest version of the symbol available
 * in the symbol library, while preserving any attributes set in the
 * current schematic. On success, returns the new Object which
 * replaces \a o_current on the page; \a o_current is deleted. On
 * failure, returns NULL, and \a o_current is left unchanged.
 *
 * \param [in]     w_current The GschemToplevel object.
 * \param [in,out] o_current The Object to be updated.
 *
 * \return the new Object that replaces \a o_current.
 *
 * TODO: This function retains attribute positions. If an attribute
 * position was what changed between symbols versions then using
 * this "update" function will have no effect.
 */
Object *
o_update_component (GschemToplevel *w_current, Object *o_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object   *o_new;
  Object   *attr_old;
  Page  *page;
  GList *new_attribs;
  GList *old_attribs;
  GList *iter;
  const CLibSymbol *clib;

  g_return_val_if_fail (GEDA_IS_COMPLEX(o_current), NULL);
  g_return_val_if_fail (o_current->complex->filename != NULL, NULL);

  page = o_get_page (o_current);

  /* Force symbol data to be reloaded from source */
  clib = s_clib_get_symbol_by_name (o_current->complex->filename);
  s_clib_symbol_invalidate_data (clib);

  if (clib == NULL) {
    u_log_message (_("Could not find symbol [%s] in library. Update failed.\n"),
                   o_current->complex->filename);
    return NULL;
  }
  else
    q_log_message (_("Updating symbol [%s]\n"), o_current->complex->filename);

  /* Unselect the old object. */
  o_selection_remove (page->selection_list, o_current);

  /* Create new object and set embedded */
  o_new = o_complex_new (toplevel,
                         o_current->complex->x,
                         o_current->complex->y,
                         o_current->complex->angle,
                         o_current->complex->mirror,
                         clib, o_current->complex->filename,
                         1);
  if (o_complex_is_embedded (o_current)) {
    o_embed (toplevel, o_new);
  }

  new_attribs = o_complex_promote_attribs (toplevel, o_new);

  /* Cull any attributes from new COMPLEX that are already attached to
   * old COMPLEX. Note that the new_attribs list is kept consistent by
   * setting GList data pointers to NULL if their Objects are
   * culled. At the end, the new_attribs list is updated by removing
   * all list items with NULL data. This is slightly magic, but
   * works. */
  for (iter = new_attribs; iter != NULL; NEXT(iter)) {
    Object *attr_new = iter->data;
    char *name;
    char *old_value;
    char *new_value;

    if (attr_new->type != OBJ_TEXT) {
      u_log_message("Internal Error: <o_update_component> "
                    "detected attr_new->type != OBJ_TEXT\n");
    }
    else {

      o_attrib_get_name_value (attr_new, &name, &new_value);

      old_value = o_attrib_search_attached_attribs_by_name (o_current, name, 0);

      if (old_value != NULL) {
        if ( strcmp(name, "symversion") == 0 ) {
          attr_old = o_attrib_find_attrib_by_name (o_current->attribs, name, 0);
          o_attrib_set_value (attr_old, name,  new_value);
        }
        o_attrib_remove (&o_new->attribs, attr_new);
        s_object_release (attr_new);
        iter->data = NULL;
      }

      GEDA_FREE (name);
      GEDA_FREE (old_value);
      GEDA_FREE (new_value);
    }
  }
  new_attribs = g_list_remove_all (new_attribs, NULL);

  /* Detach attributes from old Object and attach to new Object */
  old_attribs = g_list_copy (o_current->attribs);
  o_attrib_detach_all (o_current);
  o_attrib_attach_list (old_attribs, o_new, 1);
  g_list_free (old_attribs);

  /* Add new attributes to page */
  s_page_append_list (page, new_attribs);

  /* Update pinnumbers for current slot */
  s_slot_update_object (o_new);

  /* Replace old Object with new Object */
  s_page_replace_object (page, o_current, o_new);
  s_object_release (o_current);

  /* Select new Object */
  o_selection_add (page->selection_list, o_new);

  /* mark the page as modified */
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate (w_current, UNDO_ALL);

  return o_new;
}
