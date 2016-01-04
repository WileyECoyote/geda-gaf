/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_preview.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file gschem_preview.h
 *
 * \brief header for the Preview Widget interface module
 */
/*! \class Preview gschem_preview.h "gschem_preview.h"
 *  \brief Preview Widget Interface
 */

#ifndef __X_PREVIEW_H__
#define __X_PREVIEW_H__

#define TYPE_PREVIEW         (preview_get_type())
#define PREVIEW(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_PREVIEW, Preview))
#define PREVIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_PREVIEW, PreviewClass))
#define IS_PREVIEW(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_PREVIEW))

typedef struct _PreviewClass PreviewClass;
typedef struct _Preview      Preview;

struct _PreviewClass {
  GtkDrawingAreaClass parent_class;
};

struct _Preview {
  GtkDrawingArea parent_instance;

  GschemToplevel *preview_window;
  char *filename;
  char *buffer;

  bool active;

};

GedaType preview_get_type (void) GEDA_CONST;

#endif /* __X_PREVIEW_H__ */
