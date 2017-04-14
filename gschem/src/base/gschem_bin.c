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
 * \file gschem_bin.c
 *
 * \brief a container with one child
 *
 * The gschem subclass implements virtual methods needed for subclasses to
 * operate properly. These method implementations would also be common to many
 * derived classes.
 */

#include <config.h>
#include <gschem.h>

/*! \private
 *  \brief forward size allocation to child
 */
static void
gschem_bin_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GtkWidget *child = gtk_bin_get_child (GTK_BIN (widget));

  gtk_widget_size_allocate (child , allocation);
}

/*! \private
 *  \brief forward size request to child
 */
static void
gschem_bin_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkWidget *child = gtk_bin_get_child (GTK_BIN (widget));

  gtk_widget_size_request (child, requisition);
}

/*!
 * \brief Type class initializer for GschemBinClass
 * \par Function Description
 *  Type class initializer for GschemBinClass. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  g_class       The GschemBinClass being initialized
 * \param [in]  g_class_data  (unused)
 */
static void gschem_bin_class_init (void *g_class, void *g_class_data)
{
  GtkWidgetClass *widget_klass = GTK_WIDGET_CLASS (g_class);

  widget_klass->size_allocate = gschem_bin_size_allocate;
  widget_klass->size_request  = gschem_bin_size_request;
}

/*! \brief register/get class
 */
GType
gschem_bin_get_type ()
{
  static GedaType type = 0;

  if (type == 0) {

    static const GTypeInfo info = {
      sizeof(GschemBinClass),
      NULL,                                      /* base_init */
      NULL,                                      /* base_finalize */
      gschem_bin_class_init,                     /* (GClassInitFunc) */
      NULL,                                      /* class_finalize */
      NULL,                                      /* class_data */
      sizeof(GschemBin),
      0,                                         /* n_preallocs */
      NULL,                                      /* (GInstanceInitFunc) */
    };

    type = g_type_register_static (GTK_TYPE_BIN, "GschemBin", &info, 0);
  }

  return type;
}


/*! \brief create a new status log widget
 *
 *  \return a new status log widget
 */
GschemBin*
gschem_bin_new ()
{
  return GSCHEM_BIN (g_object_new (GSCHEM_TYPE_BIN, NULL));
}
