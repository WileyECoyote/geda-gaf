/* -*- C edacairo.h indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * File: edacairo.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedacairo - Rendering gEDA schematics with Cairo
 *
 * Copyright (C) 2010-2013 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef __EDA_CAIRO_H__
#define __EDA_CAIRO_H__

typedef enum {
  EDA_CAIRO_ENABLE_HINTS = 1,
} EdaCairoFlags;

#ifdef __cplusplus
extern "C" {        /* c++ do not mangle function names */
#endif

void eda_cairo_arc (cairo_t *cr, int flags, double width, double x, double y,
                    double radius, double start_angle, double arc_sweep);

void eda_cairo_center_arc (cairo_t *cr, int flags, double center_width,
                           double line_width, double x, double y,
                           double radius, double start_angle, double arc_sweep);

void eda_cairo_box (cairo_t *cr, int flags, double line_width,
                    double x1, double y1, double x2, double y2);

void eda_cairo_center_box (cairo_t *cr, int flags, double center_width,
                           double line_width, double x, double y,
                           double half_width, double half_height);

void eda_cairo_line (cairo_t *cr, int flags, int line_end, double w_line_width,
                     double w_x1, double w_y1, double w_x2, double w_y2);

void eda_cairo_path (cairo_t *cr, int flags, double line_width, int nsections,
                     PATH_SECTION *sections);

void eda_cairo_set_source_color (cairo_t *cr, int color, GArray *map);

void eda_cairo_stroke (cairo_t *cr, int flags, int line_type, int line_end,
                       double wwidth, double wlength, double wspace);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__EDA_CAIRO_H__ */
