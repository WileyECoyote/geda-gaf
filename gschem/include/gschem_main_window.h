/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_main_window.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
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
 * Date Contributed: October 13th, 2013
 */
/*!
 * \file gschem_main_window.h
 *
 * \brief
 */

/*! \class GschemMainWindow gschem_main_window.h "gschem_main_window.h"
 *  \brief Main Window Object
 */

#define GSCHEM_TYPE_MAIN_WINDOW           (gschem_main_window_get_type())
#define GSCHEM_MAIN_WINDOW(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_MAIN_WINDOW, GschemMainWindow))
#define GSCHEM_MAIN_WINDOW_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_MAIN_WINDOW, GschemMainWindowClass))
#define GSCHEM_IS_MAIN_WINDOW(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_MAIN_WINDOW))
#define GSCHEM_MAIN_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_MAIN_WINDOW, GschemMainWindowClass))

typedef struct _GschemMainWindowClass GschemMainWindowClass;
typedef struct _GschemMainWindow GschemMainWindow;

struct _GschemMainWindowClass
{
  GtkWindowClass parent_class;
};

struct _GschemMainWindow
{
  GtkWindow parent;
};

GedaType           gschem_main_window_get_type   (void);
GschemMainWindow  *gschem_main_window_new        (void);

GdkWindow         *gschem_main_window_get_window (GtkWidget *main_window);

#if GTK_MAJOR_VERSION < 3

GtkStyle          *gschem_main_window_get_style  (GtkWidget *main_window);

#endif
