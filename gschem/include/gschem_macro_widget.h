/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_macro_widget.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 * Contributing Author: Edward Hennessy
 * Date Contributed: November 30th, 2013
 */
/*!
 * \file gschem_macro_widget.h
 *
 * \brief A widget for entering macros
 */

#define GSCHEM_TYPE_MACRO_WIDGET           (gschem_macro_widget_get_type())
#define GSCHEM_MACRO_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_MACRO_WIDGET, GschemMacroWidget))
#define GSCHEM_MACRO_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_MACRO_WIDGET, GschemMacroWidgetClass))
#define GSCHEM_IS_MACRO_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_MACRO_WIDGET))
#define GSCHEM_MACRO_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_MACRO_WIDGET, GschemMacroWidgetClass))

typedef struct _GschemMacroWidgetClass GschemMacroWidgetClass;
typedef struct _GschemMacroWidget GschemMacroWidget;

struct _GschemMacroWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _GschemMacroWidget
{
  GtkInfoBar parent;

  GtkWidget *entry;
  GtkWidget *cancel_button;
  GtkWidget *evaluate_button;
  GtkWidget *label;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType      gschem_macro_widget_get_type            (void);

GtkWidget    *gschem_macro_widget_new                 (void);

GtkWidget    *gschem_macro_widget_get_entry           (GtkWidget *widget);

const char   *gschem_macro_widget_get_label_text      (GtkWidget *widget);

const char   *gschem_macro_widget_get_macro_string    (GtkWidget *widget);

void          gschem_macro_widget_set_label_text      (GtkWidget *widget, const char *text);

void          gschem_macro_widget_set_macro_string    (GtkWidget *widget, const char *str);

#ifdef __cplusplus
}
#endif /* __cplusplus */