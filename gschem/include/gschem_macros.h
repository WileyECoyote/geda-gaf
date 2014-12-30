/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: gschem_macros.h
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Ales Hvezda
 * Copyright (C) 2014 Wiley Edward Hill
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
 *
 * This program is distributed in the hope that it will be useful,
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
 *  Date Contributed: December, 29, 2014
 */
/*! \file gschem_macros.h
 *  \brief Pre-Processor Marcos for gschem
 *  The file contains macros used by gschem.
 *
 *  \note Presently, this header is only included by o_box.c,
 *        o_grips.c and o_picture.c.
 */

/* Macro for inserting and (grip) editing Box Objects. */
#define GET_BOX_WIDTH(w)  abs((w)->second_wx - (w)->first_wx)
#define GET_BOX_HEIGHT(w) abs((w)->second_wy - (w)->first_wy)
#define GET_BOX_LEFT(w)   ((w)->first_wx > (w)->second_wx) ? (w)->second_wx : (w)->first_wx
#define GET_BOX_TOP(w)    ((w)->first_wy > (w)->second_wy) ? (w)->first_wy : (w)->second_wy

/* Macro for inserting and (grip) editing Picture Objects. */
#define GET_PICTURE_WIDTH GET_BOX_WIDTH
#define GET_PICTURE_LEFT  GET_BOX_LEFT
/*
#define GET_PICTURE_HEIGHT(w) (w)->CONTROLKEY && (w)->pixbuf_wh_ratio != 0 ? \
                              (abs((w)->second_wx - (w)->first_wx))/(w)->pixbuf_wh_ratio : \
                              GET_BOX_HEIGHT(w)

#define GET_CONFINED_TOP(w) (w)->first_wy > (w)->second_wy ? (w)->first_wy  :  \
                            ((w)->first_wy+abs((w)->second_wx - (w)->first_wx))/(w)->pixbuf_wh_ratio

#define GET_PICTURE_TOP(w) (w)->CONTROLKEY && (w)->pixbuf_wh_ratio != 0 ? \
                            GET_CONFINED_TOP(w) : GET_BOX_TOP(w)
*/
#define GET_PICTURE_HEIGHT(w) (w)->pixbuf_wh_ratio != 0 ? \
                              abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio : \
                              GET_BOX_HEIGHT(w)

#define GET_PICTURE_TOP(w) (w)->first_wy > (w)->second_wy ? (w)->first_wy  :  \
                           (w)->first_wy+abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio











