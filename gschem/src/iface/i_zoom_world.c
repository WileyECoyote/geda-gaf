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
#include <math.h>
#include <geda_debug.h>

/* Kazu - discuss with Ales
 * 1) rint
 * 2) SWAP & SORT
 */

/* Kazu on July 8, 1999 - added these macros to simplify the code */
/* keep these macros local to this file! KISS! */
/*! \brief */
#define GET_PAGE_WIDTH(w)                   \
    ((w)->page_current->right  - (w)->page_current->left)
/*! \brief */
#define GET_PAGE_HEIGHT(w)                  \
    ((w)->page_current->bottom - (w)->page_current->top )
/*! \brief */
#define GET_PAGE_ASPECT_RATIO(w)        \
    ((float) fabs(GET_PAGE_WIDTH (w)) / \
     (float) fabs(GET_PAGE_HEIGHT(w)))


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * \param w_current     GschemToplevel object
 * \param dir           #EID_ZOOM_DIRECTIVE to indicate direction and or magnitude
 * \param selected_from #EID_ACTION_ORIGIN flag indicating where action originated
 * \param pan_flags     #EID_PAN_DIRECTIVES
 */

void i_zoom_world(GschemToplevel *w_current, EID_ZOOM_DIRECTIVE dir,
                                             EID_ACTION_ORIGIN  selected_from,
                                             EID_PAN_DIRECTIVES pan_flags)
{
  GedaToplevel *toplevel = w_current->toplevel;
  double world_pan_center_x, world_pan_center_y, relative_zoom_factor = - 1;
  int start_x, start_y;
  double top, bottom, right, left;

  /* NB: w_current->zoom_gain is a percentage increase */
  switch(dir) {
  case(ZOOM_IN_DIRECTIVE):
    relative_zoom_factor = (100.0 + w_current->zoom_gain) / 100.0;
    break;

  case(ZOOM_OUT_DIRECTIVE):
    relative_zoom_factor = 100.0 / (100.0 + w_current->zoom_gain);
    break;

  case(ZOOM_FULL_DIRECTIVE):
    /* indicate the zoom full with a negative zoomfactor */
    relative_zoom_factor = -1;
    break;
  }

  /* calc center: either "mouse_to_world" or center=center or a
     virtual center if warp_cursor is disabled */
  if (w_current->zoom_with_pan == TRUE &&
     ((selected_from == ID_ORIGIN_KEYBOARD) ||
       (selected_from == ID_ORIGIN_MOUSE)))
  {
    if (!x_event_get_pointer_position(w_current, FALSE, &start_x, &start_y)) {
      return;
    }
    if ( w_current->warp_cursor ) {
      world_pan_center_x = start_x;
      world_pan_center_y = start_y;
    }
    else {
      left = ((toplevel->page_current->left - start_x)
              * (1/relative_zoom_factor) + start_x);
      right = ((toplevel->page_current->right - start_x)
               * (1/relative_zoom_factor) + start_x);
      top = ((toplevel->page_current->top - start_y)
             * (1/relative_zoom_factor) + start_y);
      bottom = ((toplevel->page_current->bottom - start_y)
                * (1/relative_zoom_factor) + start_y);
      world_pan_center_x = (right + left) / 2;
      world_pan_center_y = (top + bottom) / 2;
    }
  }
  else {
    world_pan_center_x = (double) (toplevel->page_current->left +
                                   toplevel->page_current->right ) / 2;
    world_pan_center_y = (double) (toplevel->page_current->top +
                                   toplevel->page_current->bottom ) / 2;
  }

#if DEBUG
  printf("relative zoomfactor: %E\n", relative_zoom_factor);
  printf("new center: x: %E, y: %E \n",
         world_pan_center_x, world_pan_center_y);
#endif

  /* calculate new window and draw it */
  i_pan_world_general(w_current, world_pan_center_x, world_pan_center_y,
                      relative_zoom_factor, pan_flags);

  /* Before warping the cursor, filter out any consecutive scroll events
   * from the event queue.  If the program receives more than one scroll
   * event before it can process the first one, then the globals mouse_x
   * and mouse_y won't contain the proper mouse position,
   * because the handler for the mouse moved event needs to
   * run first to set these values.
   */
  GdkEvent *topEvent = gdk_event_get();
  while( topEvent != NULL ) {
    if( topEvent->type != GDK_SCROLL ) {
      gdk_event_put( topEvent );
      gdk_event_free( topEvent );
      break;
    }
    gdk_event_free( topEvent );
    topEvent = gdk_event_get();
  }

  /* warp the cursor to the right position */
  if (w_current->warp_cursor) {
     WORLDtoSCREEN (w_current, world_pan_center_x, world_pan_center_y,
                    &start_x, &start_y);
     x_basic_warp_cursor (w_current->drawing_area, start_x, start_y);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_zoom_world_extents (GschemToplevel *w_current, const GList *list, int pan_flags)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int lleft, lright, ltop, lbottom;
  double zx, zy, relative_zoom_factor;
  double world_pan_center_x,world_pan_center_y;

  if (list == NULL) {
    return;
  }

  if (!world_get_object_glist_bounds (list,&lleft, &ltop,
                                      &lright, &lbottom)) {
    return;
  }

#if DEBUG
  printf("in %s:  left: %d, right: %d, top: %d, bottom: %d\n",
         __func_, lleft, lright, ltop, lbottom);
#endif

  /* Calc the necessary zoomfactor to show everything
   * Start with the windows width and height (minus a small padding in pixels),
   * then scale back to world coordinates with the to_screen_y_constant as the
   * initial page data may not have the correct aspect ratio. */
  zx = (double)(w_current->screen_width - 2 * ZOOM_EXTENTS_PADDING_PX) / (lright-lleft);
  zy = (double)(w_current->screen_height - 2 * ZOOM_EXTENTS_PADDING_PX) / (lbottom-ltop);
  /* choose the smaller one */
  relative_zoom_factor = (zx < zy ? zx : zy) /
  toplevel->page_current->to_screen_y_constant;

  /*get the center of the objects*/
  world_pan_center_x = (double) (lright + lleft) / 2.0;
  world_pan_center_y = (double) (lbottom + ltop) / 2.0;

  /* and create the new window*/
  i_pan_world_general(w_current, world_pan_center_x, world_pan_center_y,
                relative_zoom_factor, pan_flags );

  /*! \bug FIXME? trigger a x_event_motion() call without moving the cursor
   *  this will redraw rubberband lines after zooming
   *  removed!, it has side effects in the preview of the part dialog
   *  need to find another way to trigger x_event_motion() (Werner)
   */
  /* x_basic_warp_cursor(w_current->drawing_area, mouse_x, mouse_y); */

}

/*! \brief Zoom World to Magnification Level
 *  \par Function Description
 *  There really is no "Magnification" in gschem, this function causes scale
 *  factors to_screen_y_constant and to_screen_x_constant to be set to the
 *  reciprocal of #zoom_new, is not exact but pretty close. The view of the
 *  world will be pan to the given XY if #specified_from ID is the keyboard
 *  or mouse.
 *
 */
void i_zoom_world_specify (GschemToplevel *w_current, double zoom_new, int x, int y,
                           EID_ACTION_ORIGIN  specified_from)
{
  GedaToplevel *toplevel = w_current->toplevel;
  double top, bottom, right, left;
  double zoom_old, relative_zoom_factor;
  double world_pan_center_x, world_pan_center_y;

  /* calc center: either "cursor_to_world" or center=center or center  */
  if ((specified_from == ID_ORIGIN_KEYBOARD) ||
      (specified_from == ID_ORIGIN_MOUSE))
  {
    world_pan_center_x = x;
    world_pan_center_y = y;
  }
  else {

    top    = toplevel->page_current->top;
    bottom = toplevel->page_current->bottom;
    right  = toplevel->page_current->right;
    left   = toplevel->page_current->left;

    world_pan_center_x = (double) (left + right ) / 2;
    world_pan_center_y = (double) (top + bottom ) / 2;
  }

  zoom_old = toplevel->page_current->to_screen_y_constant;

  relative_zoom_factor = 1 / ( zoom_new * zoom_old);

  i_pan_world_general(w_current, world_pan_center_x, world_pan_center_y,
                      relative_zoom_factor, 0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_zoom_world_box(GschemToplevel *w_current, int pan_flags)
{
  GedaToplevel *toplevel = w_current->toplevel;
  double zx, zy, relative_zoom_factor;
  double world_pan_center_x, world_pan_center_y;

  /*test if there is really a box*/
  if (w_current->first_wx == w_current->second_wx ||
      w_current->first_wy == w_current->second_wy) {
    u_log_message(_("Zoom too small!  Cannot zoom further.\n"));
    return;
  }

  /*calc new zoomfactors and choose the smaller one*/
  zx = (double) abs(toplevel->page_current->left - toplevel->page_current->right) /
    abs(w_current->first_wx - w_current->second_wx);
  zy = (double) abs(toplevel->page_current->top - toplevel->page_current->bottom) /
    abs(w_current->first_wy - w_current->second_wy);

  relative_zoom_factor = (zx < zy ? zx : zy);

  /* calculate the center of the zoom box */
  world_pan_center_x = (w_current->first_wx + w_current->second_wx) / 2.0;
  world_pan_center_y = (w_current->first_wy + w_current->second_wy) / 2.0;

  /* and create the new window*/
  i_pan_world_general(w_current, world_pan_center_x, world_pan_center_y,
                relative_zoom_factor, pan_flags);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_zoom_world_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_zoom_world_box_end(GschemToplevel *w_current, int x, int y)
{
  if( w_current->inside_action != 0 ) {

    i_zoom_world_box_invalidate_rubber (w_current);
    w_current->rubber_visible = 0;

    i_zoom_world_box(w_current, 0);

    if (w_current->undo_panzoom == TRUE) {
      o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_zoom_world_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if( w_current->inside_action != 0 ) {

    if (w_current->rubber_visible)
      i_zoom_world_box_invalidate_rubber (w_current);

    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    i_zoom_world_box_invalidate_rubber (w_current);
    w_current->rubber_visible = 1;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void i_zoom_world_box_invalidate_rubber (GschemToplevel *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rectangle (w_current, x1, y1, x2, y1);
  o_invalidate_rectangle (w_current, x1, y1, x1, y2);
  o_invalidate_rectangle (w_current, x2, y1, x2, y2);
  o_invalidate_rectangle (w_current, x1, y2, x2, y2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_zoom_world_box_draw_rubber (GschemToplevel *w_current)
{
  double wwidth     = 0;
  cairo_t *cr       = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags         = eda_renderer_get_cairo_flags (CairoRenderer);

  eda_cairo_box (cr, flags, wwidth, w_current->first_wx, w_current->first_wy,
                 w_current->second_wx, w_current->second_wy);
  eda_cairo_set_source_color (cr, ZOOM_BOX_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void correct_aspect(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  double new_aspect;

  new_aspect = GET_PAGE_ASPECT_RATIO(toplevel);

  /* Make sure aspect ratio is correct */
  if (fabs(new_aspect - toplevel->page_current->coord_aspectratio)) {
    /* sign was > */
    if (new_aspect > toplevel->page_current->coord_aspectratio) {
#if DEBUG
      printf("%s: new larger then coord\n", __func_);
      printf("%s: implies that height is too large\n", __func_,);
#endif
      /* calculate neccesary padding on Y */
      toplevel->page_current->bottom =
        toplevel->page_current->top +
        GET_PAGE_WIDTH(toplevel) /
        toplevel->page_current->coord_aspectratio;

    } else {
#if DEBUG
      printf("%s: new smaller then coord\n",__func_);
      printf("%s: implies that width is too small\n",__func_);
#endif
      /* calculate necessary padding on X */
      toplevel->page_current->right =
        toplevel->page_current->left +
        GET_PAGE_HEIGHT(toplevel) *
        toplevel->page_current->coord_aspectratio;
    }
#if DEBUG
    printf("%s: invalid aspectratio corrected\n",__func_);
#endif
  }

  new_aspect = GET_PAGE_ASPECT_RATIO(toplevel);

#if DEBUG
  printf("%s: final %f\n",__func_, new_aspect);
#endif
}
