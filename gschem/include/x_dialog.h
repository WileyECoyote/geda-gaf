/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2006-2010 Dan McMahill
 *
 * Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * Date: December 26, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#ifndef __X_DIALOG_H__
#define __X_DIALOG_H__


/* Define spacings for dialogs. Defines are in a sperate header,
 * some dialog use only the define, for example x_compselect
 */
#include "gschem_xdefines.h"

/* The header defines the GschemDialog class, Should all gschem dialogs
 * be derived from this class? */
#include "gschem_dialog.h"

#include "x_compselect.h"
#include "x_dialog.h"
#include "x_console.h"
#include "x_multiattrib.h"
#include "x_pagesel.h"
#include "x_print.h"
#include "x_states.h"

#endif /* __X_DIALOG_H__ */
