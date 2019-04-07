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
 * \file gschem_search_widget.h
 *
 * \brief A widget for finding text
 */

#define GSCHEM_TYPE_SEARCH_WIDGET           (gschem_search_widget_get_type())
#define GSCHEM_SEARCH_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_SEARCH_WIDGET, GschemSearchWidget))
#define GSCHEM_SEARCH_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_SEARCH_WIDGET, GschemSearchWidgetClass))
#define GSCHEM_IS_SEARCH_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_SEARCH_WIDGET))
#define GSCHEM_SEARCH_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_SEARCH_WIDGET, GschemSearchWidgetClass))

typedef struct _GschemSearchWidgetClass GschemSearchWidgetClass;
typedef struct _GschemSearchWidget GschemSearchWidget;

struct _GschemSearchWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _GschemSearchWidget
{
  GtkInfoBar parent;

  GtkTreeModel *find_type_model;

  GtkWidget *combo;
  GtkWidget *descend_button;
  GtkWidget *entry;
  GtkWidget *find_button;
};



int
gschem_search_widget_get_descend (GschemSearchWidget *widget);

GtkWidget*
gschem_search_widget_get_entry (GschemSearchWidget *widget);

const char*
gschem_search_widget_get_find_text_string (GschemSearchWidget *widget);

int
gschem_search_widget_get_find_type (GschemSearchWidget *widget);

GType
gschem_search_widget_get_type ();

void
gschem_search_widget_set_descend (GschemSearchWidget *widget, int descend);

void
gschem_search_widget_set_find_text_string (GschemSearchWidget *widget, const char *str);

void
gschem_search_widget_set_find_type (GschemSearchWidget *widget, int type);
