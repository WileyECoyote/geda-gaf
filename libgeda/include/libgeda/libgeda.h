/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: libgeda.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors (see ChangeLog for details)
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
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef LIBGEDA_H
#define LIBGEDA_H

#include <glib.h>
#include <glib-object.h>

#ifndef WITHOUT_GUILE
# include <libguile.h>
#else
#define SCM void*
#endif

#include <geda/geda.h>

#include "defines.h"
#include "g_types.h"
#include "o_types.h"

#include "s_struct.h"
#include "g_struct.h"

#include "f_types.h"

#include "geda_config.h"
#include "geda_errors.h"

#include "papersizes.h"
#include "geda_keyfile.h"
#include "geda_list.h"
#include "geda_notify.h"
#include "geda_object.h"
#include "geda_page.h"

#include "geda_line.h"
#include "geda_arc.h"
#include "geda_box.h"
#include "geda_bus.h"
#include "geda_circle.h"
#include "geda_complex.h"
#include "geda_net.h"
#include "geda_path.h"
#include "geda_picture.h"
#include "geda_pin.h"
#include "geda_text.h"

#include "geda_toplevel.h"

#include "geda_attrib.h"
#include "geda_file.h"
#include "geda_hierarchy.h"
#include "geda_math.h"
#include "geda_object_list.h"
#include "geda_utility.h"
#include "prototype.h"

#endif /* LIBGEDA_H */
