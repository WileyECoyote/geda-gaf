/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_status_bar.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 gEDA Contributors (see ChangeLog for details)
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
 *
 * Date: March 07, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*!
 * \file x_status_bar.h
 *
 * \brief header for the GschemStatusBar widget interface module
 */

#ifndef __X_STATUS_BAR_H__
#define __X_STATUS_BAR_H__

/*  The padding used around labels in the "status bar" */
#define STATUS_XPAD 10
#define STATUS_YPAD 1

#define STATUS_XALIGN 0.5
#define STATUS_YALIGN 0.5

#define StatusBar ((GschemStatusBar*)w_current->status_bar)


#endif /* __X_STATUS_BAR_H__ */
