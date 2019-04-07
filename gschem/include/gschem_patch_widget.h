/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*!
 * \file gschem_patch_widget.h
 *
 * \brief A widget for finding text
 */

#define GSCHEM_TYPE_PATCH_WIDGET           (gschem_patch_widget_get_type())
#define GSCHEM_PATCH_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_PATCH_WIDGET, GschemPatchWidget))
#define GSCHEM_PATCH_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_PATCH_WIDGET, GschemPatchWidgetClass))
#define GSCHEM_IS_PATCH_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_PATCH_WIDGET))
#define GSCHEM_PATCH_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_PATCH_WIDGET, GschemPatchWidgetClass))

typedef struct _GschemPatchWidgetClass GschemPatchWidgetClass;
typedef struct _GschemPatchWidget GschemPatchWidget;

enum
{
  FIND_TYPE_PATTERN,
  FIND_TYPE_REGEX,
  FIND_TYPE_SUBSTRING,
  FIND_TYPE_PATCH
};

struct _GschemPatchWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _GschemPatchWidget
{
  GtkInfoBar parent;

  GtkTreeModel *find_type_model;

  GtkWidget *label;
  GtkWidget *descend_button;
  GtkWidget *entry;
  GtkWidget *find_button;
};


int
gschem_patch_widget_get_descend (GschemPatchWidget *widget);

GtkWidget*
gschem_patch_widget_get_entry (GschemPatchWidget *widget);

const char*
gschem_patch_widget_get_patch_string (GschemPatchWidget *widget);

GedaType
gschem_patch_widget_get_type (void);

void
gschem_patch_widget_set_descend (GschemPatchWidget *widget, int descend);

void
gschem_patch_widget_set_patch_string (GschemPatchWidget *widget, const char *str);

void
gschem_patch_widget_set_find_type (GschemPatchWidget *widget, int type);
