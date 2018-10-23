/* -*- C x_stroke.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file x_stroke.c
 * \brief Interface Module for Stroke Support
 */

#include "../../include/gschem.h"

#ifdef HAVE_LIBSTROKE
#include <stroke.h>

#include <geda_debug.h>

/*
 * <B>stroke_points</B> is an array of points for the stroke
 * footprints. The points of the stroke are displayed over the display
 * area of the main window. They have to be erased when the stroke is
 * translated and the sequence evaluated.
 *
 * Its size will never exceed <B>STROKE_MAX_POINTS</B> (the limit in
 * number of points of a stroke provided by libstroke).
 */
typedef struct {
  int x, y;
} StrokePoint;

static GArray *stroke_points = NULL;


/*!
 * \brief Initializes the stroke interface.
 * \par Function Description
 *  This is the initialization function for the stroke interface. It
 *  initializes the libstroke library and prepare an array of points
 *  for the mouse footprints.
 *
 *  This function has to be called only once at application
 *  initialization before any use of the stroke interface.
 */
void
x_stroke_init (void)
{
  g_return_if_fail (stroke_points == NULL);

  stroke_init ();

  stroke_points = g_array_new (FALSE, FALSE, sizeof(StrokePoint));
}

/*!
 * \brief Frees memory of the stroke interface.
 * \par Function Description
 *  This function frees the memory used for the mouse footprint
 *  points. It terminates the use of the stroke interface.
 */
void
x_stroke_free (void)
{
  g_return_if_fail (stroke_points != NULL);

  g_array_free (stroke_points, TRUE);
  stroke_points = NULL;
}

/*!
 * \brief Records a new point for the stroke.
 * \par Function Description
 *  This function adds the point (<B>x</B>,<B>y</B>) as a new point in
 *  the stroke.
 *
 *  The footprint is updated and the new point is drawn on the drawing area.
 *
 *  \param [in] w_current The GschemToplevel object.
 *  \param [in] x        The X coord of the new point.
 *  \param [in] Y        The X coord of the new point.
 */
void
x_stroke_record (GschemToplevel *w_current, int x, int y)
{
  if (stroke_points != NULL) {

    stroke_record (x, y);

    if (stroke_points->len < STROKE_MAX_POINTS) {

      GdkGC      *gc;
      StrokePoint point = { x, y };

      g_array_append_val (stroke_points, point);

      gc = gschem_page_view_get_gc(w_current);

      gdk_gc_set_foreground (gc, geda_color_x11_color_from_index (STROKE_COLOR));
      gdk_draw_point (w_current->window, gc, x, y);
    }
  }
}

/*!
 * \brief Evaluates the stroke.
 * \par Function Description
 *  This function transforms the stroke input so far in an action.
 *
 *  It makes use of the guile procedure <B>eval-stroke</B> to evaluate
 *  the stroke sequence into a possible action. The mouse footprint is
 *  erased in this function.
 *
 *  It returns 1 if the stroke has been successfully evaluated as an
 *  action. It returns 0 if libstroke failed to transform the stroke
 *  or there is no action attached to the stroke.
 *
 * \param [in] w_current The GschemToplevel object.
 * \returns 1 on success, 0 otherwise, or -1 if error.
 *
 * \note WEH: Revised eval-stroke so as to return the action rather
 *  than the stroke, we already knew what the stoke was because we
 *  passed it to eval-stroke. This new version evaluates the action
 *  rather than the stroke.
 */
int
x_stroke_translate_and_execute (GschemToplevel *w_current)
{
  char sequence[STROKE_MAX_SEQUENCE];
  StrokePoint *point;
  int min_x, min_y, max_x, max_y;
  int i;

  if (stroke_points == NULL)
    return -1;

  if (stroke_points->len == 0)
    return 0;

  point = &g_array_index (stroke_points, StrokePoint, 0);
  min_x = max_x = point->x;
  min_y = max_y = point->y;

  for (i = 1; i < stroke_points->len; i++) {
    point = &g_array_index (stroke_points, StrokePoint, i);
    min_x = min (min_x, point->x);
    min_y = min (min_y, point->y);
    max_x = max (max_x, point->x);
    max_y = max (max_y, point->y);
  }

   o_invalidate_rectangle (w_current, min_x, min_y, max_x + 1, max_y + 1);

  /* resets length of array */
  stroke_points->len = 0;
  SCM scm_str;
  int result = 0;

  /* try evaluating stroke */
  if (stroke_trans ((char*)&sequence)) {

    char *action;
    char *expr;

    expr    = geda_sprintf("(eval-stroke \"%s\")", sequence);
    scm_str = g_evaluate_c_string_protected (expr);
    action  = scm_to_utf8_string (scm_str);

    GEDA_FREE(expr);

    if (strlen(action) > 0) {

      /* "internal" actions are handled by i_command_process */
      if (i_command_is_valid(action)) {
        i_command_process(w_current, action, 0, NULL, ID_ORIGIN_STROKE);
        result = 1;
      }
      else {
        /* for this to work the user must have defined a custom stroke
           and the associated function */
        SCM ret;
        expr = geda_sprintf("(%s)", action);
        scm_dynwind_begin (0);
        scm_dynwind_unwind_handler (g_free, expr, SCM_F_WIND_EXPLICITLY);
        ret = g_evaluate_c_string_protected (expr);
        result= (SCM_NFALSEP (ret));
        scm_dynwind_end ();
      }
    }
    GEDA_FREE(action);
  }
  return result;
}

#endif /* HAVE_LIBSTROKE */
