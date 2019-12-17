/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_accel_label.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * Code originally based on GTK 2.14.5 gtk/gtkaccellabel.c (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GedaAccelLabel: GtkLabel with accelerator monitoring facilities.
 * Copyright (C) 1998 Tim Janik
 *
 * Adapted for gEDA by Peter Clifton <peter@clifton-electronics.co.uk>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
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
#include "../../../config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_accel_label.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaAccelLabel - A Label Widget for Menus
 * \par
 * The #GedaAccelLabel widget is a subclass of a GedaLabel that also displays
 * an accelerator key to the right of the label text, e.g. 'Ctl+S'.
 * A GedaAccelLabel is typically used in menus to show the keyboard short-cuts
 * for commands.
 * \par
 * The accelerator key to display is not set explicitly.
 * Instead, the #GedaAccelLabel displays the accelerators which have been added
 * to a particular widget. This widget is set by calling
 * geda_accel_label_set_accel_widget().
 * \par
 * For example, a #GedaMenuItem widget may have an accelerator added to emit the
 * "activate" signal when the 'Ctl+S' key combination is pressed.
 * A #GedaAccelLabel is created and added to the #GedaMenuItem, and
 * geda_accel_label_set_accel_widget() is called with the #GedaMenuItem as the
 * second argument. The #GedaAccelLabel will now display 'Ctl+S' after its label.
 * \par
 * Note that creating a #GedaMenuItem with geda_menu_item_new_with_label() (or
 * one of the similar functions for #GedaCheckMenuItem and #GedaRadioMenuItem)
 * automatically adds a #GedaAccelLabel to the #GedaMenuItem and calls
 * geda_accel_label_set_accel_widget() to setup the label.
 * \par
 * A #GedaAccelLabel will only display accelerators which have %GTK_ACCEL_VISIBLE
 * set (see GtkAccelFlags).
 * A #GedaAccelLabel can display multiple accelerators and even signal names,
 * though it is almost always used to display just one accelerator key.
 * \par
 * Example: Creating a simple menu item with an accelerator key.
 * \par
 * <example>
 * \code{.c}
 *   GtkWidget *save_item;
 *   GtkAccelGroup *accel_group;
 *
 *   // Create a GtkAccelGroup and add it to the window.
 *   accel_group = gtk_accel_group_new();
 *   gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
 *
 *   // Create the menu item using the convenience function.
 *   save_item = geda_menu_item_new_with_label ("Save");
 *   gtk_widget_show (save_item);
 *   geda_container_add (menu, save_item);
 *
 *   // Now add the accelerator to the GedaMenuItem. Note that since
 *   // geda_menu_item_new_with_label() was called to create the GedaMenuItem
 *   // the GedaAccelLabel is automatically set up to display the GedaMenuItem
 *   // accelerators. We just need to make sure we use GTK_ACCEL_VISIBLE here.
 *
 *   gtk_widget_add_accelerator (save_item, "activate", accel_group,
 *                               GDK_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
 * \endcode
 * </example>
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

static void *geda_accel_label_parent_class = NULL;

static GHashTable *accel_label_hash = NULL;

/*!
 * \brief Reset Accelerator String if not enabled
 * \par Function Description
 *  Ensures the string representing the accelerator keys is an empty
 *  string if enable-accels is not set. If enable-accels are enabled
 *  then this function does virtually nothing.
 *
 * \param accel_label  A #GedaAccelLabel object
 *
 * \retval FALSE
 */
bool geda_accel_label_refetch (GedaAccelLabel *accel_label)
{
  bool enable_accels;

  g_return_val_if_fail (GEDA_IS_ACCEL_LABEL (accel_label), FALSE);

  g_object_get (gtk_widget_get_settings ((GtkWidget*)accel_label),
                "gtk-enable-accels", &enable_accels,
                NULL);

  if (!enable_accels || accel_label->accel_string == NULL) {

    if (accel_label->accel_string != NULL) {
      g_free (accel_label->accel_string);
    }

    accel_label->accel_string = geda_strdup ("");
  }

  gtk_widget_queue_resize ((GtkWidget*)accel_label);

  return FALSE;
}

static void geda_accel_label_reset (GedaAccelLabel *accel_label)
{
  if (accel_label->accel_string) {

    g_free (accel_label->accel_string);
    accel_label->accel_string = NULL;
  }

  gtk_widget_queue_resize ((GtkWidget*)accel_label);
}

static void check_accel_changed (GtkAccelGroup   *accel_group,
                                 unsigned int     keyval,
                                 GdkModifierType  modifier,
                                 GClosure        *accel_closure,
                                 GedaAccelLabel  *accel_label)
{
  if (accel_closure == accel_label->accel_closure) {
    geda_accel_label_reset (accel_label);
  }
}

static void refetch_widget_accel_closure (GedaAccelLabel *accel_label)
{
  GClosure *closure = NULL;
  GList    *clist;
  GList    *list;

  g_return_if_fail (GEDA_IS_ACCEL_LABEL (accel_label));
  g_return_if_fail (GTK_IS_WIDGET (accel_label->accel_widget));

  clist = gtk_widget_list_accel_closures (accel_label->accel_widget);

  for (list = clist; list; list = list->next) {

    /* we just take the first closure used */
    closure = list->data;
    break;
  }

  g_list_free (clist);
  geda_accel_label_set_accel_closure (accel_label, closure);
}

static const char *geda_accel_label_get_string (GedaAccelLabel *accel_label)
{
  if (!accel_label->accel_string) {
    geda_accel_label_refetch (accel_label);
  }

  return accel_label->accel_string;
}

static void geda_accel_label_set_property (GObject      *object,
                                           unsigned int  prop_id,
                                           const GValue *value,
                                           GParamSpec   *pspec)
{
  GedaAccelLabel  *accel_label;

  accel_label = (GedaAccelLabel*)object;

  switch (prop_id) {
    case PROP_ACCEL_CLOSURE:
      geda_accel_label_set_accel_closure (accel_label, g_value_get_boxed (value));
      break;
    case PROP_ACCEL_WIDGET:
      geda_accel_label_set_accel_widget(accel_label, g_value_get_boxed (value));
      break;

    case PROP_ACCEL_STRING:
      geda_accel_label_set_accel_string (accel_label, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_accel_label_get_property (GObject      *object,
                                           unsigned int  prop_id,
                                           GValue       *value,
                                           GParamSpec   *pspec)
{
  GedaAccelLabel  *accel_label;

  accel_label = GEDA_ACCEL_LABEL (object);

  switch (prop_id) {
    case PROP_ACCEL_CLOSURE:
      g_value_set_boxed (value, NULL);
      break;

    case PROP_ACCEL_WIDGET:
      g_value_set_object (value, geda_accel_label_get_accel_widget(accel_label));
      break;

    case PROP_ACCEL_STRING:
      g_value_set_string (value, accel_label->accel_string);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void geda_accel_label_dispose (GObject *object)
{
  GedaAccelLabel *accel_label = GEDA_ACCEL_LABEL (object);

  geda_accel_label_set_accel_closure(accel_label, NULL);

  geda_accel_label_set_accel_widget(accel_label, NULL);

  ((GObjectClass*)geda_accel_label_parent_class)->dispose (object);
}

static void geda_accel_label_finalize (GObject *object)
{
  GedaAccelLabel *accel_label = GEDA_ACCEL_LABEL (object);

  if (g_hash_table_remove (accel_label_hash, object)) {
    if (!g_hash_table_size (accel_label_hash)) {
      g_hash_table_destroy (accel_label_hash);
      accel_label_hash = NULL;
    }
  }

  GEDA_FREE (accel_label->accel_string);

  ((GObjectClass*)geda_accel_label_parent_class)->finalize (object);
}

static void geda_accel_label_set_accel_string_width (GedaAccelLabel *accel_label)
{
  const char *accel_string;

  accel_string = geda_accel_label_get_string (accel_label);

  if (accel_string) {

    PangoLayout *layout;
    int width;

    layout = gtk_widget_create_pango_layout ((GtkWidget*)accel_label, accel_string);

    pango_layout_get_pixel_size (layout, &width, NULL);

    g_object_unref (layout);

    accel_label->accel_string_width = width;
  }
  else {
    accel_label->accel_string_width = 0;
  }

#ifdef DEBUG
  fprintf(stderr, "%s: <%p> with str<%s> has width %w\n", __func__,
          accel_label, accel_string, accel_label->accel_string_width);
#endif
}

unsigned int geda_accel_label_get_accel_width (GedaAccelLabel *accel_label)
{
  g_return_val_if_fail (GEDA_IS_ACCEL_LABEL (accel_label), 0);

  geda_accel_label_set_accel_string_width (accel_label);

  return (accel_label->accel_string_width +
         (accel_label->accel_string_width ? accel_label->accel_padding : 0));
}

static void geda_accel_label_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GedaAccelLabel *accel_label = GEDA_ACCEL_LABEL (widget);

  ((GtkWidgetClass*)geda_accel_label_parent_class)->size_request (widget, requisition);

  geda_accel_label_set_accel_string_width (accel_label);
}

static int get_first_baseline (PangoLayout *layout)
{
  PangoLayoutIter *iter;
  int result;

  iter   = pango_layout_get_iter (layout);
  result = pango_layout_iter_get_baseline (iter);
  pango_layout_iter_free (iter);

  return PANGO_PIXELS (result);
}

#if GTK_MAJOR_VERSION < 3

static bool geda_accel_label_expose_event (GtkWidget *widget, GdkEventExpose *event)
{
  GedaAccelLabel *accel_label;

  accel_label = (GedaAccelLabel*)widget;

  if (gtk_widget_is_drawable (widget)) {

    unsigned int   ac_width;
    GtkAllocation *allocation;

    ac_width   = geda_accel_label_get_accel_width (accel_label);
    allocation = geda_get_widget_allocation (widget);

    if (allocation->width >= widget->requisition.width + ac_width) {

      GtkTextDirection  direction;
      PangoLayout      *label_layout;
      PangoLayout      *accel_layout;
      GedaLabel        *label;
      GtkMisc          *misc;

      int x;
      int y;

      misc         = (GtkMisc*)accel_label;
      label        = (GedaLabel*)widget;
      label_layout = geda_label_get_layout ((GedaLabel*)accel_label);
      direction    = gtk_widget_get_direction (widget);

      if (direction == GTK_TEXT_DIR_RTL) {
        allocation->x += ac_width;
      }

      allocation->width -= ac_width;

      if (geda_label_get_ellipsize (label)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width - ac_width * PANGO_SCALE);
      }

      if (((GtkWidgetClass*)geda_accel_label_parent_class)->expose_event) {
        ((GtkWidgetClass*)geda_accel_label_parent_class)->expose_event (widget, event);
      }

      if (direction == GTK_TEXT_DIR_RTL) {
        allocation->x -= ac_width;
      }

      allocation->width += ac_width;

      if (geda_label_get_ellipsize (label)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width + ac_width * PANGO_SCALE);
      }

      if (direction == GTK_TEXT_DIR_RTL) {
        x = allocation->x + misc->xpad;
      }
      else {
        x = allocation->x + allocation->width - misc->xpad - ac_width;
      }

      geda_label_get_layout_offsets ((GedaLabel*)accel_label, NULL, &y);

      accel_layout = gtk_widget_create_pango_layout (widget, geda_accel_label_get_string (accel_label));

      y += get_first_baseline (label_layout) - get_first_baseline (accel_layout);

      GtkStateType state = gtk_widget_get_state (widget);

      gtk_paint_layout (widget->style, widget->window, state, FALSE,
                        &event->area, widget, "accellabel", x, y,
                        accel_layout);

      g_object_unref (accel_layout);
    }
    else {
      if (((GtkWidgetClass*)geda_accel_label_parent_class)->expose_event)
        ((GtkWidgetClass*)geda_accel_label_parent_class)->expose_event (widget, event);
    }
  }

  return FALSE;
}

#else /* !GTK_MAJOR_VERSION < 3 */

static int geda_accel_label_draw (GtkWidget *widget, cairo_t *cr)
{
  GedaAccelLabel *accel_label;
  unsigned int    ac_width;
  GtkAllocation  *allocation;
  GtkRequisition  requisition;

  accel_label = (GedaAccelLabel*)widget;

  ac_width    = geda_accel_label_get_accel_width (accel_label);

  allocation  = geda_get_widget_allocation (widget);

  gtk_widget_get_preferred_size (widget, NULL, &requisition);

  if (allocation->width >= requisition.width + ac_width) {

      GtkTextDirection direction;
      GtkStyleContext *context;
      PangoLayout     *label_layout;
      PangoLayout     *accel_layout;

      int x;
      int y;
      int xpad;

      context      = gtk_widget_get_style_context (widget);
      direction    = gtk_widget_get_direction (widget);
      label_layout = geda_label_get_layout ((GedaLabel*)accel_label);

      cairo_save (cr);

      /* XXX: Mad hack: We modify the label's width so it renders
       * properly in its draw function that we chain to. */
      if (direction == GTK_TEXT_DIR_RTL) {
        cairo_translate (cr, ac_width, 0);
      }

      if (geda_label_get_ellipsize ((GedaLabel*)widget)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width - ac_width * PANGO_SCALE);
      }

      allocation->width -= ac_width;

      gtk_widget_set_allocation (widget, &allocation);

      if (((GtkWidgetClass*)geda_accel_label_parent_class)->draw) {
        ((GtkWidgetClass*)geda_accel_label_parent_class)->draw (widget, cr);
      }

      allocation->width += ac_width;
      gtk_widget_set_allocation (widget, &allocation);

      if (geda_label_get_ellipsize ((GedaLabel*)widget)) {
        int width = pango_layout_get_width (label_layout);
        pango_layout_set_width (label_layout, width + ac_width * PANGO_SCALE);
      }

      cairo_restore (cr);

      gtk_misc_get_padding ((GtkMisc*)accel_label, &xpad, NULL);

      if (direction == GTK_TEXT_DIR_RTL) {
        x = xpad;
      }
      else {
        x = gtk_widget_get_allocated_width (widget) - xpad - ac_width;
      }

      geda_label_get_layout_offsets ((GedaLabel*)accel_label, NULL, &y);

      const char *string = geda_accel_label_get_string (accel_label);

      accel_layout = gtk_widget_create_pango_layout (widget, string);

      y += get_first_baseline (label_layout) - get_first_baseline (accel_layout);

      gtk_style_context_save (context);
      gtk_style_context_add_class (context, GTK_STYLE_CLASS_ACCELERATOR);

      gtk_render_layout (context, cr, x, y, accel_layout);
      gtk_style_context_restore (context);

      g_object_unref (accel_layout);
  }
  else if (((GtkWidgetClass*)geda_accel_label_parent_class)->draw) {
    ((GtkWidgetClass*)geda_accel_label_parent_class)->draw (widget, cr);
  }

  return FALSE;
}
#endif

/* Underscores in key names are better displayed as spaces
 * E.g., Page_Up should be "Page Up"
 */
static void substitute_underscores (char *str)
{
  char *p;

  for (p = str; *p; p++)
    if (*p == '_')
      *p = ' ';
}

/*!
 * \brief Get the Accelerator String from a GedaAccelLabel
 * \par Function Description
 *  This function returns the accelerator string for the
 *  accelerator label.
 *
 * \param accel_label  A #GedaAccelLabel object
 *
 * \returns pointer accelerator string.
 */
const char *geda_accel_label_get_accel_string (GedaAccelLabel *accel_label)
{
  g_return_val_if_fail (GEDA_IS_ACCEL_LABEL (accel_label), NULL);

  return accel_label->accel_string;
}

/*!
 * \brief Set Accelerator String for a GedaAccelLabel
 * \par Function Description
 *  This function Sets the accelerator string for this accelerator label.
 *
 * \param accel_label  A #GedaAccelLabel object
 * \param accel_string Pointer accelerator string.
 */
void geda_accel_label_set_accel_string (GedaAccelLabel *accel_label,
                                        const char     *accel_string)
{
  g_return_if_fail (GEDA_IS_ACCEL_LABEL (accel_label));

  if (accel_label->accel_string) {
    g_free (accel_label->accel_string);
  }

  if (accel_string) {
    accel_label->accel_string = geda_strdup (accel_string);
    substitute_underscores (accel_label->accel_string);
  }
  else {
    accel_label->accel_string = NULL;
  }

  GEDA_OBJECT_NOTIFY (accel_label, "accel-string");
}

/*!
 * \brief GedaAccelLabel Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       GedaAccelLabel class we are initializing
 * \param [in]  class_data  GedaAccelLabel structure associated with the class
 */
static void geda_accel_label_class_init(void *class, void *class_data)
{
  GObjectClass   *object_class = (GObjectClass*) class;
  GtkWidgetClass *widget_class = (GtkWidgetClass*) class;

  object_class->dispose        = geda_accel_label_dispose;
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

/*!
 * \brief Type instance initializer for GedaAccelLabel
 * \par Function Description
 *  Type instance initializer for GedaAccelLabel, initializes a new empty
 *  GedaAccelLabel object.
 *
 * \param [in] instance The GedaAccelLabel structure being initialized,
 * \param [in] g_class  The GedaAccelLabel class we are initializing.
 */
static void geda_accel_label_instance_init (GTypeInstance *instance, void *g_class)
{
  GedaAccelLabel *accel_label = (GedaAccelLabel*)instance;

  accel_label->accel_padding = 3;
  accel_label->accel_string  = NULL;

  if (!accel_label_hash) {
    accel_label_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (accel_label_hash, instance, instance);
}

/*!
 * \brief Function to retrieve GedaAccelLabel's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaAccelLabel Type identifier. When
 *  first called, the function registers a #GedaAccelLabel in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaAccelLabel and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaAccelLabel.
 */
GedaType geda_accel_label_get_type (void)
{
  static volatile GedaType geda_accel_label_type = 0;

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
    type   = g_type_register_static (GEDA_TYPE_LABEL, string, &info, 0);

    g_once_init_leave (&geda_accel_label_type, type);
  }

  return geda_accel_label_type;
}

/*!
 * \brief Check if an object is a GedaAccelLabel
 * \par Function Description
 *  Determines if \a accel_label is valid by verifying \a accel_label
 *  is included in the hash table of GedaAccelLabel objects.
 *
 * \return TRUE if \a accel_label is a valid GedaAccelLabel
 */
bool is_a_geda_accel_label (GedaAccelLabel *accel_label)
{
  if ((accel_label != NULL) && (accel_label_hash != NULL)) {
    return g_hash_table_lookup(accel_label_hash, accel_label) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Create a New GedaAccelLabel.
 * \par Function Description
 *  Creates a brand spanking new #GedaAccelLabel.
 *
 * \param [in] string The label string, Must be not NULL.
 *
 * \returns a new #GedaAccelLabel.
 */
GtkWidget *geda_accel_label_new (const char *string)
{
  GedaLabel *accel_label; /* GedaAccelLabel */

  g_return_val_if_fail (string != NULL, NULL);

  accel_label = g_object_new (GEDA_TYPE_ACCEL_LABEL, NULL);

  geda_label_set_text (accel_label, string);

  return GTK_WIDGET (accel_label);
}

/*!
 * \brief Set the GedaAccelLabel Closure.
 * \par Function Description
 *  Sets the closure to be monitored by this accelerator label. The closure
 *  must be connected to an accelerator group; see gtk_accel_group_connect().
 *
 * \param [in] accel_label   a GedaAccelLabel
 * \param [in] accel_closure the closure to monitor for accelerator changes.
 */
void geda_accel_label_set_accel_closure (GedaAccelLabel *accel_label,
                                         GClosure       *accel_closure)
{
  g_return_if_fail (GEDA_IS_ACCEL_LABEL (accel_label));

  if (accel_closure) {
    g_return_if_fail (gtk_accel_group_from_accel_closure (accel_closure) != NULL);
  }

  if (accel_closure != accel_label->accel_closure) {

    if (accel_label->accel_closure) {

      g_signal_handlers_disconnect_by_func (accel_label->accel_group,
                                            check_accel_changed,
                                            accel_label);
      accel_label->accel_group = NULL;
      g_closure_unref (accel_label->accel_closure);
    }

    accel_label->accel_closure = accel_closure;

    if (accel_closure) {

      g_closure_ref (accel_closure);
      accel_label->accel_group = gtk_accel_group_from_accel_closure (accel_closure);
      g_signal_connect_object (accel_label->accel_group, "accel-changed",
                               G_CALLBACK (check_accel_changed),
                               accel_label, 0);
    }

    geda_accel_label_reset (accel_label);

    GEDA_OBJECT_NOTIFY (accel_label, "accel-closure");
  }
}

/*!
 * \brief Retrieve the GedaAccelLabel widget
 * \par Function Description
 *  Returns the widget monitored by the accelerator label.
 *
 * \param [in] accel_label  a GedaAccelLabel
 *
 * \returns the monitored widget
 */
GtkWidget *geda_accel_label_get_accel_widget (GedaAccelLabel *accel_label)
{
  g_return_val_if_fail (GEDA_IS_ACCEL_LABEL (accel_label), NULL);

  return accel_label->accel_widget;
}

/*!
 * \brief Set the GedaAccelLabel widget
 * \par Function Description
 *  Sets the widget to be monitored by this accelerator label.
 *
 * \param [in] accel_label  a GedaAccelLabel
 * \param [in] accel_widget the widget to be monitored.
 */
void geda_accel_label_set_accel_widget (GedaAccelLabel *accel_label,
                                        GtkWidget      *accel_widget)
{
  g_return_if_fail (GEDA_IS_ACCEL_LABEL (accel_label));

  if (accel_widget) {
    g_return_if_fail (GTK_IS_WIDGET (accel_widget));
  }

  if (accel_widget != accel_label->accel_widget) {

    if (accel_label->accel_widget) {

      geda_accel_label_set_accel_closure (accel_label, NULL);
      g_signal_handlers_disconnect_by_func (accel_label->accel_widget,
                                            refetch_widget_accel_closure,
                                            accel_label);
      g_object_unref (accel_label->accel_widget);
    }

    /* Which could be NULL*/
    accel_label->accel_widget = accel_widget;

    if (accel_widget) {

      g_object_ref (accel_widget);

      g_signal_connect_object (accel_widget, "accel-closures-changed",
                               G_CALLBACK (refetch_widget_accel_closure),
                               accel_label, G_CONNECT_SWAPPED);
      refetch_widget_accel_closure (accel_label);
    }

    GEDA_OBJECT_NOTIFY (accel_label, "accel-widget");
  }
}

/** @} endgroup GedaAccelLabel */
