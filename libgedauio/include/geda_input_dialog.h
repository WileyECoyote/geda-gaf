/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_input_dialog.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill
 *
 * Date: July 12, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
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
 */

#ifndef __GEDA_INPUT_DIALOG_H__
#define __GEDA_INPUT_DIALOG_H__

int   geda_dialog_get_integer     (const char *title, const char *prompt, int offer);
float geda_dialog_get_real        (const char *title, const char *prompt, float offer);
char *geda_dialog_get_string      (const char *title, const char *prompt, const char *string);

#endif /* __GEDA_INPUT_DIALOG_H__ */
