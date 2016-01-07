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

#ifndef __GSCHEM_PREVIEW_H__
#define __GSCHEM_PREVIEW_H__

#define GSCHEM_TYPE_PREVIEW         (gschem_preview_get_type())
#define GSCHEM_PREVIEW(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_PREVIEW, GschemPreview))
#define GSCHEM_PREVIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_PREVIEW, GschemPreviewClass))
#define IS_PREVIEW(obj)             (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_PREVIEW))

typedef struct _GschemPreviewClass GschemPreviewClass;
typedef struct _GschemPreview      GschemPreview;

struct _GschemPreviewClass {
  GtkDrawingAreaClass parent_class;
};

struct _GschemPreview {
  GtkDrawingArea parent_instance;

  GschemToplevel *preview_window;
  char *filename;
  char *buffer;

  bool active;

};

GedaType   gschem_preview_get_type (void) GEDA_CONST;

GtkWidget *gschem_preview_new (void) GEDA_WARN_UNUSED_RESULT;

#endif /* __GSCHEM_PREVIEW_H__ */
