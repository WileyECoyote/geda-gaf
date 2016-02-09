/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
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
 * \file gschem_main_window.c
 *
 * \brief The Main Window Widget
 */

#include <gschem.h>
#include <geda_keysyms.h>
#include <geda_debug.h>

/** \defgroup Gschem-Main-Window Gschem Main Window
 * @{
 * \brief #GschemMainWindow Class Implmentation
 * \par
 *  This module implements the main window in gschem.
 */

/* Function Prototypes */

static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec);

static void
gschem_main_window_class_init (GschemMainWindowClass *klass);

static void
gschem_main_window_instance_init (GschemMainWindow *window);

static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec);


/*! \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec)
{
  //GschemMainWindow *window = GSCHEM_MAIN_WINDOW (object);

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \brief Initialize GschemMainWindow class
 *
 *  \param [in] klass The class for the GschemMainWindow
 */
static void
gschem_main_window_class_init (GschemMainWindowClass *klass)
{
  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;
}


/*! \brief Get/register GschemSelection type.
 */
GedaType gschem_main_window_get_type (void)
{
  static GedaType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemMainWindowClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_main_window_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemMainWindow),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_main_window_instance_init,
    };

    type = g_type_register_static (GTK_TYPE_WINDOW, "GschemMainWindow", &info, 0);
  }

  return type;
}

/*! \brief Initialize GschemSelection instance -NOP
 *
 *  \param [in,out] window
 */
static void
gschem_main_window_instance_init (GschemMainWindow *window)
{

}

/*! \brief Create a new instanceof the GschemMainWindow
 *
 *  \return A new instanceof the GschemMainWindow
 */
GschemMainWindow*
gschem_main_window_new ()
{
  return GSCHEM_MAIN_WINDOW (g_object_new (GSCHEM_TYPE_MAIN_WINDOW,
                                           "type", GTK_WINDOW_TOPLEVEL,
                                           NULL));
}

/*! \brief Set a property
 *
 *  \param [in,out] object
 *  \param [in]     param_id
 *  \param [in]     value
 *  \param [in]     pspec
 */
static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec)
{
  //GschemMainWindow *window = GSCHEM_MAIN_WINDOW (object);

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

GdkWindow *gschem_main_window_get_window (GtkWidget *main_window)
{
  if (GSCHEM_IS_MAIN_WINDOW(main_window)) {
    return geda_get_widget_window (main_window);
  }

  return NULL;
}

#if GTK_MAJOR_VERSION < 3

GtkStyle *gschem_main_window_get_style (GtkWidget *main_window)
{
  if (GSCHEM_IS_MAIN_WINDOW(main_window)) {
    return main_window->style;
  }

  return NULL;
}

#endif

/** @} endgroup Gschem-Main-Window */
