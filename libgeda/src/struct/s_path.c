/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Code originally from librsvg 2.22.2 (LGPL) Copyright (C) 2000 Eazel, Inc.
 *
 *   Author: Raph Levien <raph@artofcode.com>
 *     rsvg-path.c:       Parse SVG path element data into bezier path.
 *     rsvg-bpath-util.c: Data structure and convenience functions for
 *                        creating bezier paths.
 *
 *  Adapted for gEDA by Peter Clifton <peter@clifton-electronics.co.uk>
 *
 *  THIS FILE IS LGPL LICENSED, gEDA AS A WHOLE IS GPL LICENSED
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program; if not, write to the
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 */

#include "../../../config.h"

#include <math.h>

#include <glib.h>

#include <libgeda_priv.h>

#define NUM_BEZIER_SEGMENTS 100

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GedaPath *s_path_new (void)
{
  GedaPath *path;

  path = (GedaPath*)geda_path_new ();
  path->num_sections = 0;
  path->num_sections_max = 16;
  path->sections = g_new (PATH_SECTION, path->num_sections_max);

  return path;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GedaPath *s_path_new_from (PATH_SECTION *sections)
{
  GedaPath *path;
  int i;

  g_return_val_if_fail (sections != NULL, NULL);

  for (i = 0; sections[i].code != PATH_END; i++);
  if (i <= 0)
    return s_path_new ();

  path = (GedaPath*)geda_path_new ();

  path->num_sections = i;
  path->num_sections_max = i;
  path->sections = g_new (PATH_SECTION, i);

  memcpy (path->sections, sections, i * sizeof(PATH_SECTION));
  return path;
}

/*!
 * \brief Release a GedaPath structure
 * \par Function Description
 *  Unreferences the GedaPath object. The path should not be referenced
 *  after calling the function.
 */
void s_path_free(GedaPath *path)
{
  g_return_if_fail (GEDA_IS_PATH(path));
  GEDA_UNREF (path);
}

void s_path_moveto (GedaPath *path, double x, double y)
{
  PATH_SECTION *sections;
  int num_sections;

  g_return_if_fail (path != NULL);

  /* if the last command was a moveto then change that last moveto instead of
     creating a new one */
  sections = path->sections;
  num_sections = path->num_sections;

  if (num_sections > 0) {
    if (sections[num_sections - 1].code == PATH_MOVETO_OPEN) {
      sections[num_sections - 1].x3 = x;
      sections[num_sections - 1].y3 = y;
      return;
    }
  }

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max) {
    path->sections = g_realloc (path->sections, (path->num_sections_max <<= 1) * sizeof(PATH_SECTION));
  }

  sections = path->sections;
  sections[num_sections].code = PATH_MOVETO_OPEN;
  sections[num_sections].x3 = x;
  sections[num_sections].y3 = y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_path_lineto (GedaPath *path, double x, double y)
{
  PATH_SECTION *sections;
  int num_sections;

  g_return_if_fail (path != NULL);

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max) {
    path->sections = g_realloc (path->sections, (path->num_sections_max <<= 1) * sizeof(PATH_SECTION));
  }

  sections = path->sections;
  sections[num_sections].code = PATH_LINETO;
  sections[num_sections].x3 = x;
  sections[num_sections].y3 = y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_path_curveto (GedaPath *path, double x1, double y1,
                     double x2, double y2, double x3, double y3)
{
  PATH_SECTION *sections;
  int num_sections;

  g_return_if_fail (path != NULL);

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max)
    path->sections = g_realloc (path->sections, (path->num_sections_max <<= 1) * sizeof(PATH_SECTION));
  sections = path->sections;
  sections[num_sections].code = PATH_CURVETO;
  sections[num_sections].x1 = x1;
  sections[num_sections].y1 = y1;
  sections[num_sections].x2 = x2;
  sections[num_sections].y2 = y2;
  sections[num_sections].x3 = x3;
  sections[num_sections].y3 = y3;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_path_art_finish (GedaPath *path)
{
  int num_sections;

  g_return_if_fail (path != NULL);

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max) {
    path->sections = g_realloc (path->sections,
                                (path->num_sections_max <<= 1) * sizeof(PATH_SECTION));
  }

  path->sections[num_sections].code = PATH_END;
}

/*!
 * \brief Modify a Path Object while copying coordinates
 * \par Function Description
 *  This function is used for drawing a temporary path with a rubber
 *  limp with end point new_x, new_x.
 *
 * \param [in] path      A GedaPage object
 * \param [in] dx        The RECTANGLE regions to check
 * \param [in] dy        The number of regions
 * \param [in] new_x     The number of regions
 * \param [in] new_y     The number of regions
 * \param [in] whichone  The number of regions
 *
 * \return The modified path
 */
GedaPath *geda_struct_path_copy_modify (GedaPath *path, int dx, int dy,
                                                        int new_x, int new_y,
                                                        int whichone)
{
  GedaPath *new_path;
  char     *path_string;

  int i;
  int color;
  int grip_no = 0;

  g_return_val_if_fail(GEDA_IS_PATH(path), NULL);

  color                      = geda_object_get_color((GedaObject*)path);
  path_string                = geda_struct_path_string_from_path (path);
  new_path                   = (GedaPath*)geda_path_object_new (color, path_string);
  new_path->sections         = GEDA_MEM_ALLOC (path->num_sections * sizeof(PATH_SECTION));
  new_path->num_sections     = path->num_sections;
  new_path->num_sections_max = path->num_sections;

  for (i = 0; i <  path->num_sections; i++) {

    int x1, y1, x2, y2, x3, y3;

    PATH_SECTION *section     = &path->sections[i];
    PATH_SECTION *new_section = &new_path->sections[i];

    x1 = section->x1 + dx; y1 = section->y1 + dy;
    x2 = section->x2 + dx; y2 = section->y2 + dy;
    x3 = section->x3 + dx; y3 = section->y3 + dy;

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        if (whichone == grip_no++) {
          x1 = new_x; y1 = new_y;
        }
        if (whichone == grip_no++) {
          x2 = new_x; y2 = new_y;
        }
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        if (whichone == grip_no++) {
          x3 = new_x; y3 = new_y;
        }
      /* Fall through */
      case PATH_END:
        break;
    }

    new_section->code = section->code;
    new_section->x1 = x1;  new_section->y1 = y1;
    new_section->x2 = x2;  new_section->y2 = y2;
    new_section->x3 = x3;  new_section->y3 = y3;
  }
  return new_path;
}

/* This module parses an SVG style path element into a Path.

  At present, there is no support for <marker> or any other contextual
  information from the SVG file. The API will need to change rather
  significantly to support these.

  Reference: SVG working draft 3 March 2000, section 8.
*/

typedef struct _RSVGParsePathCtx RSVGParsePathCtx;

struct _RSVGParsePathCtx {
  GedaPath  *path;
  double cpx, cpy;    /* current point */
  double rpx, rpy;    /* reflection point (for 's' and 't' commands) */
  double mpx, mpy;    /* Last moved to point (for path closures) */
  char   cmd;         /* current command (lowercase) */
  int    param;       /* parameter number */
  bool   rel;         /* true if relative coords */
  double params[7];   /* parameters that have been parsed */
};

static void s_path_arc_segment (RSVGParsePathCtx *ctx,
                                double xc, double yc, double th0, double th1,
                                double rx, double ry, double x_axis_rotation)
{
  double sin_th, cos_th;
  double a00, a01, a10, a11;
  double x1, y1, x2, y2, x3, y3;
  double t;
  double th_half;

  sin_th = sin (x_axis_rotation * (M_PI / 180.0));
  cos_th = cos (x_axis_rotation * (M_PI / 180.0));

  /* inverse transform compared with s_path_arc */
  a00 = cos_th * rx;
  a01 = -sin_th * ry;
  a10 = sin_th * rx;
  a11 = cos_th * ry;

  th_half = 0.5 * (th1 - th0);
  t = (8.0 / 3.0) * sin (th_half * 0.5) * sin (th_half * 0.5) / sin (th_half);
  x1 = xc + cos (th0) - t * sin (th0);
  y1 = yc + sin (th0) + t * cos (th0);
  x3 = xc + cos (th1);
  y3 = yc + sin (th1);
  x2 = x3 + t * sin (th1);
  y2 = y3 - t * cos (th1);
  s_path_curveto (ctx->path,
              a00 * x1 + a01 * y1, a10 * x1 + a11 * y1,
              a00 * x2 + a01 * y2, a10 * x2 + a11 * y2,
              a00 * x3 + a01 * y3, a10 * x3 + a11 * y3);
}

/*
 * s_path_arc: Add an arc to the path context.
 * @ctx: Path context.
 * @rx: Radius in x direction (before rotation).
 * @ry: Radius in y direction (before rotation).
 * @x_axis_rotation: Rotation angle for axes.
 * @large_arc_flag: 0 for arc length <= 180, 1 for arc >= 180.
 * @sweep: 0 for "negative angle", 1 for "positive angle".
 * @x: New x coordinate.
 * @y: New y coordinate.
 */
static void s_path_arc (RSVGParsePathCtx *ctx,
                        double rx, double ry, double x_axis_rotation,
                        int large_arc_flag, int sweep_flag, double x, double y)
{
  double sin_th, cos_th;
  double a00, a01, a10, a11;
  double x0, y0, x1, y1, xc, yc;
  double d, sfactor, sfactor_sq;
  double th0, th1, th_arc;
  int i, n_segs;

  /* Check that neither radius is zero, since its is not either
     geometrically or mathematically meaningful and will
     cause divide by zero and subsequent NaNs.  We should
     really do some ranged check ie -0.001 < x < 000.1 rather
     can just a straight check again zero.
   */
  if ((rx == 0.0) || (ry == 0.0))
    return;

  sin_th = sin (x_axis_rotation * (M_PI / 180.0));
  cos_th = cos (x_axis_rotation * (M_PI / 180.0));
  a00 = cos_th / rx;
  a01 = sin_th / rx;
  a10 = -sin_th / ry;
  a11 = cos_th / ry;
  x0 = a00 * ctx->cpx + a01 * ctx->cpy;
  y0 = a10 * ctx->cpx + a11 * ctx->cpy;
  x1 = a00 * x + a01 * y;
  y1 = a10 * x + a11 * y;
  /* (x0, y0) is current point in transformed coordinate space.
     (x1, y1) is new point in transformed coordinate space.

     The arc fits a unit-radius circle in this space.
   */
  d = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0);

  sfactor_sq = 1.0 / d - 0.25;

  if (sfactor_sq < 0)
    sfactor_sq = 0;

  sfactor = sqrt (sfactor_sq);

  if (sweep_flag == large_arc_flag)
    sfactor = -sfactor;

  xc = 0.5 * (x0 + x1) - sfactor * (y1 - y0);
  yc = 0.5 * (y0 + y1) + sfactor * (x1 - x0);
  /* (xc, yc) is center of the circle. */

  th0 = atan2 (y0 - yc, x0 - xc);
  th1 = atan2 (y1 - yc, x1 - xc);

  th_arc = th1 - th0;

  if (th_arc < 0 && sweep_flag)
    th_arc += 2 * M_PI;
  else if (th_arc > 0 && !sweep_flag)
    th_arc -= 2 * M_PI;

  n_segs = ceil (fabs (th_arc / (M_PI * 0.5 + 0.001)));

  for (i = 0; i < n_segs; i++)
    s_path_arc_segment (ctx, xc, yc,
                 th0 + i * th_arc / n_segs,
                 th0 + (i + 1) * th_arc / n_segs, rx, ry, x_axis_rotation);

  ctx->cpx = x;
  ctx->cpy = y;
}

/* supply defaults for missing parameters, assuming relative coordinates
   are to be interpreted as x,y */
static void geda_struct_path_parse_default_xy (RSVGParsePathCtx *ctx, int n_params)
{
  int i;

  if (ctx->rel) {
    for (i = ctx->param; i < n_params; i++) {
      if (i > 2)
        ctx->params[i] = ctx->params[i - 2];
      else if (i == 1)
        ctx->params[i] = ctx->cpy;
      else if (i == 0)
        /* we shouldn't get here (usually ctx->param > 0 as
           precondition) */
        ctx->params[i] = ctx->cpx;
    }
  }
  else {
    for (i = ctx->param; i < n_params; i++)
      ctx->params[i] = 0.0;
  }
}

static void geda_struct_path_parse_do_cmd (RSVGParsePathCtx *ctx, bool final)
{
  double x1, y1, x2, y2, x3, y3;

  switch (ctx->cmd) {
  case 'm':
    /* moveto */
    if (ctx->param == 2 || final) {
      geda_struct_path_parse_default_xy (ctx, 2);
      s_path_moveto (ctx->path, ctx->params[0], ctx->params[1]);
      ctx->mpx = ctx->cpx = ctx->rpx = ctx->params[0];
      ctx->mpy = ctx->cpy = ctx->rpy = ctx->params[1];
      ctx->param = 0;
      ctx->cmd = 'l'; /* implicit linetos after a moveto */
    }
    break;
  case 'l':
    /* lineto */
    if (ctx->param == 2 || final) {
      geda_struct_path_parse_default_xy (ctx, 2);
      s_path_lineto (ctx->path, ctx->params[0], ctx->params[1]);
      ctx->cpx = ctx->rpx = ctx->params[0];
      ctx->cpy = ctx->rpy = ctx->params[1];
      ctx->param = 0;
    }
    break;
  case 'c':
    /* curveto */
    if (ctx->param == 6 || final) {
      geda_struct_path_parse_default_xy (ctx, 6);
      x1 = ctx->params[0];
      y1 = ctx->params[1];
      x2 = ctx->params[2];
      y2 = ctx->params[3];
      x3 = ctx->params[4];
      y3 = ctx->params[5];
      s_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = x2;
      ctx->rpy = y2;
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    }
    break;
  case 's':
    /* smooth curveto */
    if (ctx->param == 4 || final) {
      geda_struct_path_parse_default_xy (ctx, 4);
      x1 = 2 * ctx->cpx - ctx->rpx;
      y1 = 2 * ctx->cpy - ctx->rpy;
      x2 = ctx->params[0];
      y2 = ctx->params[1];
      x3 = ctx->params[2];
      y3 = ctx->params[3];
      s_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = x2;
      ctx->rpy = y2;
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    }
    break;
  case 'h':
    /* horizontal lineto */
    if (ctx->param == 1) {
      s_path_lineto (ctx->path, ctx->params[0], ctx->cpy);
      ctx->cpx = ctx->rpx = ctx->params[0];
      ctx->param = 0;
    }
    break;
  case 'v':
    /* vertical lineto */
    if (ctx->param == 1) {
      s_path_lineto (ctx->path, ctx->cpx, ctx->params[0]);
      ctx->cpy = ctx->rpy = ctx->params[0];
      ctx->param = 0;
    }
    break;
  case 'q':
    /* quadratic bezier curveto */

    /* non-normative reference:
       http://www.icce.rug.nl/erikjan/bluefuzz/beziers/beziers/beziers.html
     */
    if (ctx->param == 4 || final) {
      geda_struct_path_parse_default_xy (ctx, 4);
      /* raise quadratic bezier to cubic */
      x1 = (ctx->cpx + 2 * ctx->params[0]) * (1.0 / 3.0);
      y1 = (ctx->cpy + 2 * ctx->params[1]) * (1.0 / 3.0);
      x3 = ctx->params[2];
      y3 = ctx->params[3];
      x2 = (x3 + 2 * ctx->params[0]) * (1.0 / 3.0);
      y2 = (y3 + 2 * ctx->params[1]) * (1.0 / 3.0);
      s_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = ctx->params[0];
      ctx->rpy = ctx->params[1];
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    }
    break;
  case 't':
    /* Truetype quadratic bezier curveto */
    if (ctx->param == 2 || final) {
      double xc, yc;    /* quadratic control point */

      xc = 2 * ctx->cpx - ctx->rpx;
      yc = 2 * ctx->cpy - ctx->rpy;
      /* generate a quadratic bezier with control point = xc, yc */
      x1 = (ctx->cpx + 2 * xc) * (1.0 / 3.0);
      y1 = (ctx->cpy + 2 * yc) * (1.0 / 3.0);
      x3 = ctx->params[0];
      y3 = ctx->params[1];
      x2 = (x3 + 2 * xc) * (1.0 / 3.0);
      y2 = (y3 + 2 * yc) * (1.0 / 3.0);
      s_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = xc;
      ctx->rpy = yc;
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    } else if (final) {
      if (ctx->param > 2) {
        geda_struct_path_parse_default_xy (ctx, 4);
        /* raise quadratic bezier to cubic */
        x1 = (ctx->cpx + 2 * ctx->params[0]) * (1.0 / 3.0);
        y1 = (ctx->cpy + 2 * ctx->params[1]) * (1.0 / 3.0);
        x3 = ctx->params[2];
        y3 = ctx->params[3];
        x2 = (x3 + 2 * ctx->params[0]) * (1.0 / 3.0);
        y2 = (y3 + 2 * ctx->params[1]) * (1.0 / 3.0);
        s_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
        ctx->rpx = ctx->params[0];
        ctx->rpy = ctx->params[1];
        ctx->cpx = x3;
        ctx->cpy = y3;
      } else {
        geda_struct_path_parse_default_xy (ctx, 2);
        s_path_lineto (ctx->path, ctx->params[0], ctx->params[1]);
        ctx->cpx = ctx->rpx = ctx->params[0];
        ctx->cpy = ctx->rpy = ctx->params[1];
      }
      ctx->param = 0;
    }
    break;
  case 'a':
    if (ctx->param == 7 || final) {
      s_path_arc (ctx,
               ctx->params[0], ctx->params[1], ctx->params[2],
               ctx->params[3], ctx->params[4], ctx->params[5], ctx->params[6]);
      ctx->param = 0;
    }
    break;
  default:
    ctx->param = 0;
  }
}

static void geda_struct_path_parse_data (RSVGParsePathCtx *ctx, const char *data)
{
  bool in_num        = FALSE;
  bool in_frac       = FALSE;
  bool in_exp        = FALSE;
  bool exp_wait_sign = FALSE;

  int sign     = 0;
  int exp      = 0;
  int exp_sign = 0;

  double frac  = 0.0;
  double val   = 0.0;
  int i;

  for (i = 0;; i++) {

    char c = data[i];

    if (c >= '0' && c <= '9') {
      /* digit */
      if (in_num) {
        if (in_exp) {
          exp = (exp * 10) + c - '0';
          exp_wait_sign = FALSE;
        }
        else if (in_frac) {
          val += (frac *= 0.1) * (c - '0');
        }
        else {
          val = (val * 10) + c - '0';
        }
      }
      else {
        in_num        = TRUE;
        in_frac       = FALSE;
        in_exp        = FALSE;
        exp           = 0;
        exp_sign      = 1;
        exp_wait_sign = FALSE;
        val           = c - '0';
        sign          = 1;
      }
    }
    else if (c == '.') {
      if (!in_num) {
        in_num = TRUE;
        val    = 0;
      }
      in_frac = TRUE;
      frac    = 1;
    }
    else if ((c == 'E' || c == 'e') && in_num) {
      in_exp        = TRUE;
      exp_wait_sign = TRUE;
      exp           = 0;
      exp_sign      = 1;
    }
    else if ((c == '+' || c == '-') && in_exp) {
      exp_sign = c == '+' ? 1 : -1;
    }
    else if (in_num) {

      /* end of number */

      val *= sign * pow (10, exp_sign * exp);

      if (ctx->rel) {
        /* Handle relative coordinates. The following switch statement attempts
         * to determine _what_ the coords are relative to. This is under
         * specified in the 12 Apr working draft. */
        switch (ctx->cmd) {
          case 'l':
          case 'm':
          case 'c':
          case 's':
          case 'q':
          case 't':

#ifndef RSVGV_RELATIVE
            /* rule: even-numbered params are x-relative, odd-numbered
             *            are y-relative */
            if ((ctx->param & 1) == 0)
              val += ctx->cpx;
            else if ((ctx->param & 1) == 1)
              val += ctx->cpy;
            break;

#else
            /* rule: even-numbered params are x-relative, odd-numbered
             *            are y-relative */
            if (ctx->param == 0 || (ctx->param % 2 == 0))
              val += ctx->cpx;
            else
              val += ctx->cpy;
            break;
#endif

          case 'a':
            /* rule: sixth and seventh are x and y, rest are not
             *            relative */
            if (ctx->param == 5)
              val += ctx->cpx;
            else if (ctx->param == 6)
              val += ctx->cpy;
            break;

          case 'h':
            /* rule: x-relative */
            val += ctx->cpx;
            break;

          case 'v':
            /* rule: y-relative */
            val += ctx->cpy;
            break;
        }
      }
      ctx->params[ctx->param++] = val;
      geda_struct_path_parse_do_cmd (ctx, FALSE);

      in_num = FALSE;
    }

    if (c == '\0') {
      break;
    }
    else if ((c == '+' || c == '-') && !exp_wait_sign) {
      sign          = c == '+' ? 1 : -1;
      val           = 0;
      in_num        = TRUE;
      in_frac       = FALSE;
      in_exp        = FALSE;
      exp           = 0;
      exp_sign      = 1;
      exp_wait_sign = FALSE;
    }
    else if (c == 'z' || c == 'Z') {

      if (ctx->param)
        geda_struct_path_parse_do_cmd (ctx, TRUE);

      /* s_path_closepath (ctx->path); */
      /* s_path_lineto (ctx->path, ctx->mpx, ctx->mpy); */
      s_path_art_finish (ctx->path);

      ctx->cpx = ctx->rpx = ctx->path->sections[ctx->path->num_sections - 1].x3;
      ctx->cpy = ctx->rpy = ctx->path->sections[ctx->path->num_sections - 1].y3;
    }
    else if (c >= 'A' && c <= 'Z' && c != 'E') {

      if (ctx->param)
        geda_struct_path_parse_do_cmd (ctx, TRUE);

      ctx->cmd = c + 'a' - 'A';
      ctx->rel = FALSE;
    }
    else if (c >= 'a' && c <= 'z' && c != 'e') {

      if (ctx->param)
        geda_struct_path_parse_do_cmd (ctx, TRUE);

      ctx->cmd = c;
      ctx->rel = TRUE;
    }
    /* else c _should_ be whitespace or , */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GedaPath *geda_struct_path_parse (const char *path_str)
{
  RSVGParsePathCtx ctx;

  ctx.path  = s_path_new ();
  ctx.cpx   = 0.0;
  ctx.cpy   = 0.0;
  ctx.mpx   = 0.0;
  ctx.mpy   = 0.0;
  ctx.cmd   = 0;
  ctx.param = 0;

  geda_struct_path_parse_data (&ctx, path_str);

  if (ctx.param)
    geda_struct_path_parse_do_cmd (&ctx, TRUE);

  return ctx.path;
}

/*!
 * \brief Converts a Path to a string describing the Path
 * \par Function Description
 *  Iterates over sections of an existing GedaPath and assembles
 *  a string for each vertex.
 */
char *geda_struct_path_string_from_path (const GedaPath *path)
{
  GString *path_string;
  int i;

  path_string = g_string_new ("");

  for (i = 0; i < path->num_sections; i++) {

    PATH_SECTION *section = &path->sections[i];

    switch (section->code) {
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
        g_string_append_printf (path_string, "M %i,%i\n",
                                section->x3, section->y3);
        break;

      case PATH_CURVETO:
        g_string_append_printf (path_string, "C %i,%i %i,%i %i,%i\n",
                                section->x1, section->y1,
                                section->x2, section->y2,
                                section->x3, section->y3);
        break;

      case PATH_LINETO:
        g_string_append_printf (path_string, "L %i,%i\n",
                                section->x3, section->y3);
        break;

      case PATH_END:
        g_string_append_printf (path_string, "z\n");
        break;
    }
  }

  return geda_utility_string_remove_last_nl(g_string_free (path_string, FALSE));
}

/*!
 * \brief Converts a path to a polygon
 * \par Function Description
 *  Parameters \a path and \a points must not be NULL.
 *
 * \param [in]  path   The path to convert to a polygon
 * \param [out] points An array of the polygon's vertices
 *
 * \returns TRUE if the path is closed, FALSE if it is open.
 */
int geda_struct_path_to_polygon (const GedaPath *path, GArray *points)
{
  int closed = FALSE;
  int i;
  GedaPoint point = { 0, 0 };

  if (points->len > 0) {
    g_array_remove_range (points, 0, points->len - 1);
  }

  for (i = 0; i < path->num_sections; i++) {
    BEZIER        bezier;
    PATH_SECTION *section = &path->sections[i];

    switch (section->code) {
      case PATH_CURVETO:
        bezier.x[0] = point.x;
        bezier.y[0] = point.y;
        bezier.x[1] = section->x1;
        bezier.y[1] = section->y1;
        bezier.x[2] = section->x2;
        bezier.y[2] = section->y2;
        point.x     = bezier.x[3] = section->x3;
        point.y     = bezier.y[3] = section->y3;
        geda_math_polygon_append_bezier (points, &bezier, NUM_BEZIER_SEGMENTS);
        break;

      case PATH_MOVETO_OPEN:
        /* Unsupported, just fall through and draw a line */
        /* Fall through */

      case PATH_MOVETO:
      case PATH_LINETO:
        point.x = section->x3;
        point.y = section->y3;
        geda_math_polygon_append_point (points, point.x, point.y);
        break;

      case PATH_END:
        closed = TRUE;
        break;
    }
  }

  return closed;
}

/*!
 * \brief Calculates the distance from Point to Path
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the given path segment.
 *
 *  \param [in] path    The path.
 *  \param [in] x       The x coordinate of the given point.
 *  \param [in] y       The y coordinate of the given point.
 *  \param [in] solid   TRUE if the path should be treated as solid, FALSE if
 *                      the path should be treated as hollow.
 *
 *  \return The shortest distance from the path to the point. With a solid
 *          shape, this function returns a distance of zero for interior
 *          points. With an invalid parameter, this function returns
 *          G_MAXDOUBLE.
 */
double geda_struct_path_shortest_distance (GedaPath *path, int x, int y, int solid)
{
  double shortest_distance = G_MAXDOUBLE;
  int closed;
  GArray *points;

  points = g_array_new (FALSE, FALSE, sizeof(GedaPoint));
  closed = geda_struct_path_to_polygon (path, points);

  if (!solid) {

    shortest_distance = geda_math_polygon_shortest_distance (points, x, y, closed);

  }
  else if (geda_math_polygon_interior_point (points, x, y)) {

    shortest_distance = 0;

  }
  else {

    shortest_distance = geda_math_polygon_shortest_distance (points, x, y, TRUE);

  }

  g_array_free (points, TRUE);

  return shortest_distance;
}
