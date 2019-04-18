/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_math.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 Wiley Edward Hill
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: Sept, 18, 2016
 */
/*! \file geda_math.h "libgeda/geda_math.h"
 *  \brief Libgeda macros for math modules.
 */

#ifndef __GEDA_MATH_MAC__
#define __GEDA_MATH_MAC__

/* m_math.c */
#define geda_degrees_to_radians         geda_math_degrees_to_radians
#define geda_distance                   geda_math_distance
#define geda_papersize_to_world         geda_math_papersize_to_world
#define geda_random_number              geda_math_random_number
#define geda_radians_to_degrees         geda_math_radians_to_degrees
#define geda_rotate_point               geda_math_rotate_point
#define geda_rotate_point_90            geda_math_rotate_point_90

/* m_angle.c */
#define geda_angle_is_normal            geda_math_angle_is_normal
#define geda_angle_is_ortho             geda_math_angle_is_ortho
#define geda_angle_make_ortho           geda_math_angle_make_ortho
#define geda_angle_normalize            geda_math_angle_normalize

/* m_arc.c */
#define geda_arc_length                 geda_math_arc_length
#define geda_arc_includes_point         geda_math_arc_includes_point

/* m_box.c */
#define geda_box_area                   geda_math_box_area
#define geda_box_shortest_distance      geda_math_box_shortest_distance

/* m_bounds.c */
#define geda_bounds_init                geda_math_bounds_init
#define geda_bounds_of_points           geda_math_bounds_of_points

/* m_circle.c */
#define geda_circle_circumference       geda_math_circle_circumference
#define geda_circle_includes_point      geda_math_circle_includes_point
#define geda_circle_shortest_distance   geda_math_circle_shortest_distance

/* m_line.c */
#define geda_line_get_intersection      geda_math_line_get_intersection
#define geda_line_includes_point        geda_math_line_includes_point
#define geda_line_intersection          geda_math_line_intersection
#define geda_line_length                geda_math_line_length
#define geda_line_shortest_distance     geda_math_line_shortest_distance

/* m_polygon.c */
#define geda_polygon_interior_point     geda_math_polygon_interior_point
#define geda_polygon_shortest_distance  geda_math_polygon_shortest_distance
#define geda_polygon_append_bezier      geda_math_polygon_append_bezier
#define geda_polygon_append_point       geda_math_polygon_append_point

#endif /* __GEDA_MATH_MAC__ */
