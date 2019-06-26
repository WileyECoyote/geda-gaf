/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_lumins.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2015 Wiley Edward Hill
 * Copyright (C) 2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License.
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
 *  Date Contributed: September, 15, 2015
 */

/* CCIR601 version of Luminance weights, in fixed point */

#define ITURBT601 1

#ifdef CCIR601
#define Yred0   19595      /* 0.29900 x 65535 */
#define Ygre0   38469      /* 0.58700 x 65535 */
#define Yblu0    7471      /* 0.11400 x 65535 */
#endif

#ifdef ITURBT601
#define Yred0   13933      /* 0.21260 x 65535 */
#define Ygre0   46871      /* 0.71520 x 65535 */
#define Yblu0    4732      /* 0.07220 x 65535 */
#endif

/* Shift 8 */
#define Yred1  (Yred0 / 255)
#define Ygre1  Ygre0 / 255
#define Yblu1  Yblu0 / 255

/* Shift 16. Squares of above parameters. */
#define Yred2  (Yred1 << 2)
#define Ygre2  (Ygre1 << 2)
#define Yblu2  (Yblu1 << 2)
