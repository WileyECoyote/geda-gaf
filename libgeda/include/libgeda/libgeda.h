/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: libgeda.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2015 Ales Hvezda
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

#include <geda.h>

#include <glib.h>
#include <glib-object.h>

#ifndef WITHOUT_GUILE
# include <libguile.h>
#else
#define SCM void*
#endif

#ifndef WITHOUT_GDK_PIX_BUFFER
# include <gdk-pixbuf/gdk-pixbuf.h>
#endif

#include <libgeda/defines.h>
#include <libgeda/g_types.h>
#include <libgeda/o_types.h>

#include <libgeda/s_struct.h>
#include <libgeda/g_struct.h>

#include <libgeda/f_types.h>

#include <libgeda/geda_config.h>
#include <libgeda/geda_errors.h>

#include <libgeda/geda_colors.h>
#include <libgeda/papersizes.h>
#include <libgeda/geda_list.h>
#include <libgeda/geda_notify.h>
#include <libgeda/geda_object.h>
#include <libgeda/geda_page.h>

#include <libgeda/geda_line.h>
#include <libgeda/geda_arc.h>
#include <libgeda/geda_box.h>
#include <libgeda/geda_bus.h>
#include <libgeda/geda_circle.h>
#include <libgeda/geda_complex.h>
#include <libgeda/geda_net.h>
#include <libgeda/geda_path.h>
#include <libgeda/geda_picture.h>
#include <libgeda/geda_pin.h>
#include <libgeda/geda_text.h>

#include <libgeda/geda_toplevel.h>

#include <libgeda/prototype.h>
#endif
