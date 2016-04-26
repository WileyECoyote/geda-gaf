/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_accel_label.h
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Code based on GTK 2.14.5 gtk/gtkaccellabel.c (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GedaAccelLabel: GtkLabel with accelerator monitoring facilities.
 * Copyright (C) 1998 Tim Janik
 *
 * Modified by the GTK+ Team and others 1997-2001.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 *  Adapted for gEDA by Peter Clifton <peter@clifton-electronics.co.uk>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include <gtk/gtkaccellabel.h>

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_accel_label.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaAccelLabel - A Button Widget for Menus
 * \par
 * A GedaAccelLabel is a variant of GtkAccelLabel, but allows menu items
 * with multi-key assignments.
 *
 * \defgroup GedaAccelLabel Accelerator Label
 * @{
 */

enum {
  PROP_0,
  PROP_ACCEL_CLOSURE,
  PROP_ACCEL_WIDGET,
  PROP_ACCEL_STRING,
};

static GObjectClass *geda_accel_label_parent_class = NULL;

bool
geda_accel_label_refetch (GedaAccelLabel *accel_label)
{
  bool enable_accels;

  g_return_val_if_fail (GEDA_IS_ACCEL_LABEL (accel_label), FALSE);

  g_object_get (gtk_widget_get_settings (GTK_WIDGET (accel_label)),
                "gtk-enable-accels", &enable_accels,
                NULL);

  if (!enable_accels || accel_label->accel_string == NULL) {
    if (accel_label->accel_string != NULL)
      g_free (accel_label->accel_string);

    accel_label->accel_string = g_strdup ("");
  }

  gtk_widget_queue_resize (GTK_WIDGET (accel_label));

  return FALSE;
}


static const char *
geda_accel_label_get_string (GedaAccelLabel *accel_label)
{
  if (!accel_label->accel_string)
    geda_accel_label_refetch (accel_label);

  return accel_label->accel_string;
}


static void
geda_accel_label_set_property (GObject      *object,
                               unsigned int  prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
  GedaAccelLabel  *accel_label;

  accel_label = GEDA_ACCEL_LABEL (object);

  switch (prop_id) {
    /* Dummy properties from GtkAccelLabel */
    case PROP_ACCEL_CLOSURE:
    case PROP_ACCEL_WIDGET:
      break;

    case PROP_ACCEL_STRING:
      geda_accel_label_set_accel_string (accel_label, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_accel_label_get_property (GObject      *object,
                               unsigned int  prop_id,
                               GValue       *value,
                               GParamSpec   *pspec)
{
  GedaAccelLabel  *accel_label;

  accel_label = GEDA_ACCEL_LABEL (object);

  switch (prop_id) {
    /* Dummy property from GtkAccelLabel */
    case PROP_ACCEL_CLOSURE:
      g_value_set_boxed (value, NULL);
      break;

      /* Dummy property from GtkAccelLabel */
    case PROP_ACCEL_WIDGET:
      g_value_set_object (value, NULL);
      break;

    case PROP_ACCEL_STRING:
      g_value_set_string (value, accel_label->accel_string);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
geda_accel_label_finalize (GObject *object)
{
  GedaAccelLabel *accel_label = GEDA_ACCEL_LABEL (object);

  GEDA_FREE (accel_label->accel_string);

  G_OBJECT_CLASS (geda_accel_label_parent_class)->finalize (object);
}

unsigned int
geda_accel_label_get_accel_width (GedaAccelLabel *accel_label)
{
  g_return_val_if_fail (GEDA_IS_ACCEL_LABEL (accel_label), 0);

  return (accel_label->accel_string_width +
         (accel_label->accel_string_width ? accel_label->accel_padding : 0));
}

static void
geda_accel_label_size_request (GtkWidget      *widget,
                               GtkRequisition *requisition)
{
  GedaAccelLabel *accel_label     = GEDA_ACCEL_LABEL (widget);
  GtkAccelLabel  *gtk_accel_label = GTK_ACCEL_LABEL (widget);
  PangoLayout    *layout;
  const char     *accel_string;
  int width;

  GTK_WIDGET_CLASS (geda_accel_label_parent_class)->size_request (widget, requisition);

  accel_string = geda_accel_label_get_string (accel_label);
  layout       = gtk_widget_create_pango_layout (widget, accel_string);

  pango_layout_get_pixel_size (layout, &width, NULL);
  g_object_unref (layout);

  accel_label->accel_string_width     = width;
  gtk_accel_label->accel_string_width = width; /* HACK: This field is private to GtkAccelLabel */
}

static int
get_first_baseline (PangoLayout *layout)
{
  PangoLayoutIter *iter;
  int result;

  iter   = pango_layout_get_iter (layout);
  result = pango_layout_iter_get_baseline (iter);
  pango_layout_iter_free (iter);

  return PANGO_PIXELS (result);
}

#if GTK_MAJOR_VERSION < 3

static bool
geda_accel_label_expose_event (GtkWidget *widget, GdkEventExpose *event)
{
  GedaAccelLabel  *accel_label;

  accel_label = GEDA_ACCEL_LABEL (widget);

  if (gtk_widget_is_drawable (widget)) {

    unsigned int   ac_width;
    GtkAllocation *allocation;

    ac_width   = geda_accel_label_get_accel_width (accel_label);
    allocation = geda_get_widget_allocation (widget);

    if (allocation->width >= widget->requisition.width + ac_width) {

      GtkTextDirection  direction;
      PangoLayout      *label_layout;
      PangoLayout      *accel_layout;
      GtkLabel         *label;
      GtkMisc          *misc;

      int x;
      int y;

      misc         = GTK_MISC (accel_label);
      label        = GTK_LABEL (widget);
      label_layout = gtk_label_get_layout (GTK_LABEL (accel_label));
      direction    = gtk_widget_get_direction (widget);

      if (direction == GTK_TEXT_DIR_RTL) {
        allocation->x += ac_width;
      }

      allocation->width -= ac_width;

      if (gtk_label_get_ellipsize (label)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width - ac_width * PANGO_SCALE);
      }

      if (GTK_WIDGET_CLASS (geda_accel_label_parent_class)->expose_event) {
        GTK_WIDGET_CLASS (geda_accel_label_parent_class)->expose_event (widget, event);
      }

      if (direction == GTK_TEXT_DIR_RTL) {
        allocation->x -= ac_width;
      }

      allocation->width += ac_width;

      if (gtk_label_get_ellipsize (label)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width + ac_width * PANGO_SCALE);
      }

      if (direction == GTK_TEXT_DIR_RTL) {
        x = allocation->x + misc->xpad;
      }
      else {
        x = allocation->x + allocation->width - misc->xpad - ac_width;
      }

      gtk_label_get_layout_offsets (GTK_LABEL (accel_label), NULL, &y);

      accel_layout = gtk_widget_create_pango_layout (widget, geda_accel_label_get_string (accel_label));

      y += get_first_baseline (label_layout) - get_first_baseline (accel_layout);

      GtkStateType state = gtk_widget_get_state (widget);

      gtk_paint_layout (widget->style, widget->window, state, FALSE,
                        &event->area, widget, "accellabel", x, y,
                        accel_layout);

      g_object_unref (accel_layout);
    }
    else {
      if (GTK_WIDGET_CLASS (geda_accel_label_parent_class)->expose_event)
        GTK_WIDGET_CLASS (geda_accel_label_parent_class)->expose_event (widget, event);
    }
  }

  return FALSE;
}

#else /* !GTK_MAJOR_VERSION < 3 */

static int geda_accel_label_draw (GtkWidget *widget, cairo_t *cr)
{
  GtkAccelLabel  *accel_label;
  unsigned int    ac_width;
  GtkAllocation  *allocation;
  GtkRequisition  requisition;

  accel_label = GTK_ACCEL_LABEL (widget);

  ac_width    = geda_accel_label_get_accel_width (accel_label);

  allocation  = geda_get_widget_allocation (widget);

  gtk_widget_get_preferred_size (widget, NULL, &requisition);

  if (allocation->width >= requisition.width + ac_width) {

      GtkMisc         *misc;
      GtkTextDirection direction;
      GtkStyleContext *context;
      PangoLayout     *label_layout;
      PangoLayout     *accel_layout;
      GtkLabel        *label = GTK_LABEL (widget);

      int x;
      int y;
      int xpad;

      misc         = GTK_MISC (accel_label);

      context      = gtk_widget_get_style_context (widget);
      direction    = gtk_widget_get_direction (widget);
      label_layout = gtk_label_get_layout (GTK_LABEL (accel_label));

      cairo_save (cr);

      /* XXX: Mad hack: We modify the label's width so it renders
       * properly in its draw function that we chain to. */
      if (direction == GTK_TEXT_DIR_RTL) {
        cairo_translate (cr, ac_width, 0);
      }

      if (gtk_label_get_ellipsize (label)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width - ac_width * PANGO_SCALE);
      }

      allocation->width -= ac_width;

      gtk_widget_set_allocation (widget, &allocation);

      if (GTK_WIDGET_CLASS (gtk_accel_label_parent_class)->draw) {
        GTK_WIDGET_CLASS (gtk_accel_label_parent_class)->draw (widget, cr);
      }

      allocation->width += ac_width;
      gtk_widget_set_allocation (widget, &allocation);

      if (gtk_label_get_ellipsize (label)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width + ac_width * PANGO_SCALE);
      }

      cairo_restore (cr);

      gtk_misc_get_padding (misc, &xpad, NULL);

      if (direction == GTK_TEXT_DIR_RTL) {
        x = xpad;
      }
      else {
        x = gtk_widget_get_allocated_width (widget) - xpad - ac_width;
      }

      gtk_label_get_layout_offsets (GTK_LABEL (accel_label), NULL, &y);

      const char *gtk_accel_label_get_string (accel_label);

      accel_layout = gtk_widget_create_pango_layout (widget, string);

      //y += get_first_baseline (label_layout) - get_first_baseline (accel_layout) - allocation->y;
      y += get_first_baseline (label_layout) - get_first_baseline (accel_layout);

      gtk_style_context_save (context);
      gtk_style_context_add_class (context, GTK_STYLE_CLASS_ACCELERATOR);

      gtk_render_layout (context, cr, x, y, accel_layout);
      gtk_style_context_restore (context);

      g_object_unref (accel_layout);
  }
  else if (GTK_WIDGET_CLASS (gtk_accel_label_parent_class)->draw) {
      GTK_WIDGET_CLASS (gtk_accel_label_parent_class)->draw (widget, cr);
  }

  return FALSE;
}
#endif

/* Underscores in key names are better displayed as spaces
 * E.g., Page_Up should be "Page Up"
 */
static void
substitute_underscores (char *str)
{
  char *p;

  for (p = str; *p; p++)
    if (*p == '_')
      *p = ' ';
}

/*! \brief Set Accelerator for a GedaAccelLabel
 *  \par Function Description
 *  This function Sets the accelerator string for this accelerator label.
 *
 * \param accel_label  A #GedaAccelLabel object
 * \param accel_string Pointer accelerator string.
 */
void
geda_accel_label_set_accel_string (GedaAccelLabel *accel_label,
                                   const char     *accel_string)
{
  g_return_if_fail (GEDA_IS_ACCEL_LABEL (accel_label));

  if (accel_label->accel_string) {
    g_free (accel_label->accel_string);
  }

  if (accel_string) {
    accel_label->accel_string = geda_utility_string_strdup (accel_string);
    substitute_underscores (accel_label->accel_string);
  }
  else {
    accel_label->accel_string = NULL;
  }

  g_object_notify (G_OBJECT (accel_label), "accel-string");
}

/*! \brief GedaAccelLabel Type Class Initializer
 *
 *  \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 *  \param [in]  g_class     GedaAccelLabel class we are initializing
 *  \param [in]  class_data  GedaAccelLabel structure associated with the class
 */
static void
geda_accel_label_class_init(void *g_class, void *class_data)
{
  GedaAccelLabelClass *class   = (GedaAccelLabelClass*)g_class;

  GObjectClass   *object_class = G_OBJECT_CLASS (class);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

  object_class->finalize       = geda_accel_label_finalize;
  object_class->set_property   = geda_accel_label_set_property;
  object_class->get_property   = geda_accel_label_get_property;

  widget_class->size_request   = geda_accel_label_size_request;

#if GTK_MAJOR_VERSION < 3
  widget_class->expose_event   = geda_accel_label_expose_event;
#else
  widget_class->draw           = geda_accel_label_draw;
#endif

  geda_accel_label_parent_class = g_type_class_peek_parent (class);

  g_object_class_install_property (object_class,
                                   PROP_ACCEL_CLOSURE,
                                   g_param_spec_boxed ("accel-closure",
                                                     _("Accelerator Closure"),
                                                     _("The closure to be monitored for accelerator changes"),
                                                       G_TYPE_CLOSURE,
                                                       G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_ACCEL_WIDGET,
                                   g_param_spec_object ("accel-widget",
                                                      _("Accelerator Widget"),
                                                      _("The widget to be monitored for accelerator changes"),
                                                        GTK_TYPE_WIDGET,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_ACCEL_STRING,
                                   g_param_spec_string ("accel-string",
                                                      _("Accelerator String"),
                                                      _("The accelerator string to be displayed"),
                                                        NULL,
                                                        G_PARAM_READWRITE));
}

/*! \brief Type instance initializer for GedaAccelLabel
 *
 *  \par Function Description
 *  Type instance initializer for GedaAccelLabel, initializes a new empty
 *  GedaAccelLabel object.
 *
 *  \param [in] instance The GedaAccelLabel structure being initialized,
 *  \param [in] g_class  The GedaAccelLabel class we are initializing.
 */
static void
geda_accel_label_instance_init (GTypeInstance *instance, void *g_class)
{
  GedaAccelLabel *accel_label = (GedaAccelLabel*)instance;
  accel_label->instance_type  = geda_accel_label_get_type();

  accel_label->accel_padding = 3;
  accel_label->accel_string  = NULL;
}

/*! \brief Function to retrieve GedaAccelLabel's Type identifier.
 *  \par Function Description
 *  Function to retrieve a #GedaAccelLabel Type identifier. When
 *  first called, the function registers a #GedaAccelLabel in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaAccelLabel and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaAccelLabel.
 */
GedaType geda_accel_label_get_type (void)
{
  static GedaType geda_accel_label_type = 0;

  if (g_once_init_enter (&geda_accel_label_type)) {

    static const GTypeInfo info = {
      sizeof(GedaAccelLabelClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_accel_label_class_init,     /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaAccelLabel),
      0,                               /* n_preallocs         */
      geda_accel_label_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaAccelLabel");
    type   = g_type_register_static (GTK_TYPE_ACCEL_LABEL, string, &info, 0);

    g_once_init_leave (&geda_accel_label_type, type);
  }

  return geda_accel_label_type;
}

/*!
 * \brief Check if an object is a GedaAccelLabel
 * \par Function Description
 *  Ensures accel_label is a valid G_Object and compares signature
 *  to geda accel label type.
 * \return TRUE if \a accel_label is a valid GedaAccelLabel
 */
bool
is_a_geda_accel_label (GedaAccelLabel *accel_label)
{
  if (G_IS_OBJECT(accel_label)) {
    return (geda_accel_label_get_type() == accel_label->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Create a New GedaAccelLabel.
 * \par Function Description
 * Creates a brand spanking new #GedaAccelLabel.
 *
 * \param [in] string The label string, Must be not NULL.
 *
 * \returns a new #GedaAccelLabel.
 */
GtkWidget*
geda_accel_label_new (const char *string)
{
  GedaAccelLabel *accel_label;

  g_return_val_if_fail (string != NULL, NULL);

  accel_label = g_object_new (GEDA_TYPE_ACCEL_LABEL, NULL);

  gtk_label_set_text (GTK_LABEL (accel_label), string);

  return GTK_WIDGET (accel_label);
}

/** @} endgroup GedaAccelLabel */
