/* gEDA - GPL Electronic Design Automation
 *
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * Date: July 12, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the GTK Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 */

#ifndef __GEDA_INPUT_DIALOG_H__
#define __GEDA_INPUT_DIALOG_H__

char *geda_dialog_get_string(const char *title, const char *prompt);
float geda_dialog_get_real(const char *title, const char *prompt);

#endif /* __GEDA_INPUT_DIALOG_H__ */
