/* -*- gschem_types.h -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: June, 21, 2015
 *
 */
/*! \file gschem_types.h
 *
 *  \brief List all Builtin User Interface Commands/Actions
 *  \par Description
 *   Maybe a temporary scheme used to synchronize the action
 *
 */
#ifndef _GSCHEM_TYPES_H
#define _GSCHEM_TYPES_H

/*! \brief Type of callback function for event handler */
typedef void (*ActionInit)(GschemToplevel*, int, int);
typedef void (*ActionPaster)(GschemToplevel*);
typedef void (*ActionAdder)(GschemToplevel*, int, int);

#endif