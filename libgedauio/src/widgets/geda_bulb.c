/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_bulb.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2017 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this Library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: September 22, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_bulb.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_marshal.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

#define INDICATOR_SIZE     26
#define INDICATOR_SPACING  2

#include "../../include/geda_bulb.xpm"

/** \defgroup GedaBulb GedaBulb Objects
 * @{
 * \brief A button resembling a light bulb used for multiple choices options
 * \par
 * A single bulb button performs the same basic function as a GtkCheckButton,
 * as its position in the object hierarchy reflects. Only when multiple bulb
 * buttons are grouped together that they perform a different role in the user
 * interface. When bulb buttons are grouped and one is selected, all other bulbs
 * in the same group are deselected. #GedaBulb are used to offer user a choice
 * from multiple options.
 * \par
 * Bulb widgets are created using one of the functions in the GedaBulb-Creators
 * group, passing %NULL as the argument if this is the first bulb in a group.
 * In subsequent calls, the bulb can be passed as an argument.
 * \par
 * To retrieve the group a #GedaBulb is assigned to, use geda_bulb_get_group().
 * To remove a #GedaBulb from one group and make it part of a new one,
 * use geda_bulb_set_group().
 * \par
 * Group list need not be freed, as each #GedaBulb will remove itself and its
 * list item when it is destroyed.
 * \par
 * Example: How to create a group of two bulb buttons.
 * \par
 * \code{.c}
 * void create_bulbs (void) {
 *
 *    GtkWidget *window, *bulb1, *bulb2, *box, *entry;
 *    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
 *    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
 *    gtk_box_set_homogeneous (GTK_BOX (box), TRUE);
 *
 *    // Create a bulb button with a GtkEntry widget
 *    bulb1 = geda_bulb_new (NULL);
 *    entry = gtk_entry_new ();
 *    geda_container_add (bulb1, entry);
 *
 *
 *    // Create a bulb button with a label
 *    bulb2 = geda_bulb_new_with_label_from_widget (GEDA_BULB (bulb1),
 *                                                          "I'm a second bulb.");
 *
 *    // Pack them into a box, then show all the widgets
 *    gtk_box_pack_start (GTK_BOX (box), bulb1, TRUE, TRUE, 2);
 *    gtk_box_pack_start (GTK_BOX (box), bulb2, TRUE, TRUE, 2);
 *    geda_container_add (window, box);
 *    gtk_widget_show_all (window);
 *    return;
 * }
 * \endcode
 * \par
 * When an unselected button in the group is clicked both the clicked button
 * and the active group member receives the GtkToggleButton::toggled signal,
 * but only the bulb that was clicked receives the "clicked" signal. This is
 * different from Gtk, where both buttons receive both signals.
 *
 */

enum {
  PROP_0,
  PROP_GROUP,
  PROP_SHOW_BUTT
};

static void *geda_bulb_parent_class = NULL;

static GHashTable *bulb_hash_table = NULL;

static unsigned int group_changed_signal = 0;

static GdkPixbuf *off_pixbuf = NULL;
static GdkPixbuf *on_pixbuf  = NULL;
static int        pix_buff_ref_count;

static void geda_bulb_set_property (GObject      *object,
                                    unsigned int  prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
  GtkWidget *widget;

  widget = (GtkWidget*)object;

  switch (prop_id) {

    GSList   *slist;
    GedaBulb *button;

    case PROP_GROUP:
      button = g_value_get_object (value);
      if (button) {
        slist = geda_bulb_get_group ((GtkWidget*)button);
      }
      else {
        slist = NULL;
      }
      geda_bulb_set_group (widget, slist);
      break;

    case PROP_SHOW_BUTT:
      geda_bulb_set_show_button (widget, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_bulb_get_property (GObject       *object,
                                    unsigned int   prop_id,
                                    GValue        *value,
                                    GParamSpec    *pspec)
{
  if (prop_id == PROP_SHOW_BUTT) {

    g_value_set_boolean (value, geda_bulb_get_show_button ((GtkWidget*)object));

  }
  else {
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

/*! \internal gobject_class->finalize */
static void geda_bulb_finalize (GObject *object)
{
  GtkWidget *old_group_singleton;
  GedaBulb  *bulb                = (GedaBulb*)object;
  GSList    *tmp_list;
  bool       was_in_group;

  if (g_hash_table_remove (bulb_hash_table, object)) {
    if (!g_hash_table_size (bulb_hash_table)) {
      g_hash_table_destroy (bulb_hash_table);
      bulb_hash_table = NULL;
    }
  }

  was_in_group = bulb->group && bulb->group->next;
  bulb->group  = g_slist_remove (bulb->group, bulb);

  if (bulb->group && !bulb->group->next) {
    old_group_singleton = bulb->group->data;
  }
  else {
    old_group_singleton = NULL;
  }

  tmp_list = bulb->group;

  while (tmp_list) {

    GedaBulb  *tmp_button;

    tmp_button = tmp_list->data;
    tmp_list   = tmp_list->next;

    tmp_button->group = bulb->group;
  }

  /* this button is no longer in the group */
  bulb->group = NULL;

  if (old_group_singleton) {
    g_signal_emit (old_group_singleton, group_changed_signal, 0);
  }
  else {
    if (was_in_group) {
      g_signal_emit (bulb, group_changed_signal, 0);
    }
  }

  g_object_unref (off_pixbuf);
  g_object_unref (on_pixbuf);

  pix_buff_ref_count--;

  /* A lot a good it does to have a gobject so we can keep track of object
   * usage. Gobject does not provide a means to get the current reference
   * count so effectively is worthless, we will have to keep track ourself.
   */
  if (pix_buff_ref_count == 0)  {
    off_pixbuf = NULL;
    on_pixbuf  = NULL;
  }

  ((GObjectClass*)geda_bulb_parent_class)->finalize (object);
}

/*! \internal used by left_right_compare and  up_down_compare */
static void get_coordinates (GtkWidget *widget, GtkWidget *reference, int *x, int *y)
{
  GtkAllocation *allocation;

  allocation = geda_get_widget_allocation (widget);

  *x = allocation->x + (allocation->width >> 1);
  *y = allocation->y + (allocation->height >> 1);

  gtk_widget_translate_coordinates (widget, reference, *x, *y, x, y);
}

/*! \internal helper function for geda_bulb_focus */
static int left_right_compare (const void * a, const void * b, void *data)
{
  int x1, y1, x2, y2;

  get_coordinates ((GtkWidget *)a, data, &x1, &y1);
  get_coordinates ((GtkWidget *)b, data, &x2, &y2);

  if (y1 == y2) {
    return (x1 < x2) ? -1 : ((x1 == x2) ? 0 : 1);
  }
  else {
    return (y1 < y2) ? -1 : 1;
  }
}

/*! \internal helper function for geda_bulb_focus */
static int up_down_compare (const void *a, const void * b, void *data)
{
  int x1, y1, x2, y2;

  get_coordinates ((GtkWidget*)a, data, &x1, &y1);
  get_coordinates ((GtkWidget*)b, data, &x2, &y2);

  if (x1 == x2) {
    return (y1 < y2) ? -1 : ((y1 == y2) ? 0 : 1);
  }
  else {
    return (x1 < x2) ? -1 : 1;
  }
}

static bool geda_bulb_focus (GtkWidget *widget, GtkDirectionType direction)
{
  GedaBulb *bulb = (GedaBulb*)widget;
  bool      result;

  /* Bulbs with draw_indicator unset focus "normally", since
   * they look like buttons to the user.
   */
  if (!gtk_toggle_button_get_mode ((GtkToggleButton*)widget)) {
    result = ((GtkWidgetClass*)geda_bulb_parent_class)->focus (widget, direction);
  }
  else {

    if (gtk_widget_is_focus (widget)) {

      GSList   *focus_list, *tmp_list;
      GtkWidget *toplevel  = gtk_widget_get_toplevel (widget);
      GtkWidget *new_focus = NULL;

      switch (direction) {

        case GTK_DIR_LEFT:
        case GTK_DIR_RIGHT:
          focus_list = g_slist_copy (bulb->group);
          focus_list = g_slist_sort_with_data (focus_list, left_right_compare, toplevel);
          break;

        case GTK_DIR_UP:
        case GTK_DIR_DOWN:
          focus_list = g_slist_copy (bulb->group);
          focus_list = g_slist_sort_with_data (focus_list, up_down_compare, toplevel);
          break;

        case GTK_DIR_TAB_FORWARD:
        case GTK_DIR_TAB_BACKWARD:
          /* fall through */
          default:
            return FALSE;
      }

      if (direction == GTK_DIR_LEFT || direction == GTK_DIR_UP) {
        focus_list = g_slist_reverse (focus_list);
      }

      tmp_list = g_slist_find (focus_list, widget);

      if (tmp_list) {

        tmp_list = tmp_list->next;

        while (tmp_list) {

          GtkWidget *child = tmp_list->data;

          if (gtk_widget_get_mapped (child) && gtk_widget_is_sensitive (child))
          {
            new_focus = child;
            break;
          }

          tmp_list = tmp_list->next;
        }
      }

      if (!new_focus) {

        tmp_list = focus_list;

        while (tmp_list) {

          GtkWidget *child = tmp_list->data;

          if (gtk_widget_get_mapped (child) && gtk_widget_is_sensitive (child))
          {
            new_focus = child;
            break;
          }

          tmp_list = tmp_list->next;
        }
      }

      g_slist_free (focus_list);

      if (new_focus) {

        gtk_widget_grab_focus (new_focus);

        gtk_toggle_button_set_active ((GtkToggleButton*)new_focus, TRUE);
      }

      result = TRUE;
    }
    else {

      GedaBulb *selected_button;
      GSList   *tmp_slist;

      /* Accept the focus if:
       *
       *  - we don't have the focus and
       *  - we are the currently active button in the group and
       *  - there is no currently active bulb button.
       */

      selected_button = NULL;
      tmp_slist       = bulb->group;

      while (tmp_slist) {

        if (gtk_toggle_button_get_active ((GtkToggleButton*)tmp_slist->data))
        {
          selected_button = tmp_slist->data;
        }
        tmp_slist = tmp_slist->next;
      }

      if (selected_button && selected_button != bulb) {
        result = FALSE;
      }
      else {
        gtk_widget_grab_focus (widget);
        result = TRUE;
      }
    }
  }
  return result;
}

static void button_set_depressed (GtkButton *button, bool depressed)
{
  GtkWidget *widget = (GtkWidget*)button;

  depressed = depressed != FALSE;

  if (depressed != button->depressed) {
    button->depressed = depressed;
    gtk_widget_queue_resize (widget);
  }
}

static void
geda_bulb_clicked (GtkButton *button)
{
  GedaBulb        *bulb          = (GedaBulb*)button;
  GtkToggleButton *toggle_button = (GtkToggleButton*)button;
  GtkToggleButton *tmp_button;
  GtkStateType     new_state;
  GSList          *tmp_list;
  int toggled;
  bool depressed;

  toggled = FALSE;

  g_object_ref (button);

  if (toggle_button->active) {

    tmp_button = NULL;
    tmp_list = bulb->group;

    while (tmp_list) {

      tmp_button = tmp_list->data;
      tmp_list = tmp_list->next;

      if (tmp_button->active && tmp_button != toggle_button) {
        break;
      }

      tmp_button = NULL;
    }

    if (!tmp_button) {
      new_state = (button->in_button ? GTK_STATE_PRELIGHT : GTK_STATE_ACTIVE);
    }
    else {
      toggled = TRUE;
      toggle_button->active = !toggle_button->active;
      new_state = (button->in_button ? GTK_STATE_PRELIGHT : GTK_STATE_NORMAL);
    }
  }
  else {

    toggled = TRUE;
    toggle_button->active = !toggle_button->active;

    tmp_list = bulb->group;

    while (tmp_list) {

      tmp_button = tmp_list->data;
      tmp_list = tmp_list->next;

      if (tmp_button->active && (tmp_button != toggle_button)) {
        /* WEH: Gtk does this ...
         * gtk_button_clicked (GTK_BUTTON (tmp_button));
         * which generates both a "clicked" and a "toggled" event and is
         * technically wrong, and also annoying, the other widget was not
         * clicked", instead we do this ... */
        tmp_button->active = FALSE;
        gtk_toggle_button_toggled ((GtkToggleButton*)tmp_button);
        gtk_widget_queue_draw((GtkWidget*)tmp_button);
        /* which only generates a "toggled" event for the other widget */
        break;
      }

    }

    new_state = (button->in_button ? GTK_STATE_PRELIGHT : GTK_STATE_ACTIVE);
  }

  if (toggle_button->inconsistent) {
    depressed = FALSE;
  }
  else if (button->in_button && button->button_down) {
    depressed = !toggle_button->active;
  }
  else {
    depressed = toggle_button->active;
  }

  if (gtk_widget_get_state ((GtkWidget*)button) != new_state) {
    gtk_widget_set_state ((GtkWidget*)button, new_state);
  }

  if (toggled) {

    gtk_toggle_button_toggled (toggle_button);

    GEDA_OBJECT_NOTIFY (toggle_button, "active");
  }

  button_set_depressed (button, depressed);

  gtk_widget_queue_draw ((GtkWidget*)button);

  g_object_unref (button);
}

static void
button_get_props (GtkCheckButton *check_button, int *indicator_size,
                                                int *indicator_spacing)
{
  GtkWidget *widget = (GtkWidget*)check_button;

  if (indicator_size)
    gtk_widget_style_get (widget, "indicator-size", indicator_size, NULL);

  if (indicator_spacing)
    gtk_widget_style_get (widget, "indicator-spacing", indicator_spacing, NULL);
}

static void
geda_bulb_draw_indicator (GtkCheckButton *check_button, GdkRectangle *area)
{
  GtkWidget *widget = (GtkWidget*)check_button;

  if (gtk_widget_is_drawable (widget)) {

    GtkAllocation   *allocation;
    GtkWidget       *child;
    GedaBulb        *bulb;
    GtkButton       *button;
    GtkToggleButton *toggle_button;
    GtkStateType     state_type;

    int  x, y;
    int  border_width;
    int  indicator_size;
    int  indicator_spacing;
    int  focus_width;
    int  focus_pad;

    bool interior_focus;
    cairo_t *cr;

    /* ----------------- Setup Auxiliary Pointers ----------------- */

    allocation    = geda_get_widget_allocation(widget);
    bulb          = (GedaBulb*)check_button;
    button        = (GtkButton*)check_button;
    toggle_button = (GtkToggleButton*)check_button;

    /* --------------- Retrieve Dimensional Factors --------------- */

    button_get_props (check_button, &indicator_size, &indicator_spacing);

    gtk_widget_style_get (widget,
                          "interior-focus",   &interior_focus,
                          "focus-line-width", &focus_width,
                          "focus-padding",    &focus_pad,
                          NULL);

    border_width = ((GtkContainer*)widget)->border_width;

    /* --------------- Calculate Indicator Position --------------- */

    /* Calculate lateral position based on the widget orientation */
    if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL) {

      /* get the amount to pull back from the right edge */
      int pullback = indicator_spacing + bulb->width + ( 2 * border_width);

      /* X RTL position = right edge - pullback = left + width - pullback */
      x = allocation->x + allocation->width - pullback;

    }
    else { /* Push away from the left side */

      x = allocation->x + indicator_spacing + ( 2 * border_width);
    }

    /* Make trival adjustment for child focus */
    child = geda_get_child_widget (check_button);

    if (!interior_focus || !(child && gtk_widget_get_visible (child))) {
      x += focus_width + focus_pad;
    }

    /* Calculate vertical position widget height and indicator size */
    y = allocation->y + (allocation->height - indicator_size) / 2;

    /* -------------------- Setup Widget State -------------------- */

    if (button->activate_timeout || (button->button_down && button->in_button))
      state_type = GTK_STATE_ACTIVE;
    else if (button->in_button)
      state_type = GTK_STATE_PRELIGHT;
    else if (!gtk_widget_is_sensitive (widget))
      state_type = GTK_STATE_INSENSITIVE;
    else
      state_type = GTK_STATE_NORMAL;

    if (bulb->show_butt) {

      if (gtk_widget_get_state (widget) == GTK_STATE_PRELIGHT) {

        GdkRectangle restrict_area;
        GdkRectangle new_area;

        restrict_area.x      = allocation->x + border_width;
        restrict_area.y      = allocation->y + border_width;

        restrict_area.width  = allocation->width - (2 * border_width);
        restrict_area.height = allocation->height - (2 * border_width);

        /* ----------------- Draw Backing Button Area ----------------- */
        if (gdk_rectangle_intersect (area, &restrict_area, &new_area)) {

          gtk_paint_flat_box (widget->style, widget->window, GTK_STATE_PRELIGHT,
                              GTK_SHADOW_ETCHED_OUT,
                              area, widget, "checkbutton",
                              new_area.x, new_area.y,
                              new_area.width, new_area.height);
        }
      }
    }
    /* ----------------- Draw the Indicator Bulb ------------------ */

    cr = gdk_cairo_create( widget->window );

    if (toggle_button->active) {
      gdk_cairo_set_source_pixbuf (cr, on_pixbuf, x, y);
    }
    else {
      gdk_cairo_set_source_pixbuf (cr, off_pixbuf, x, y);
    }

    if (state_type == GTK_STATE_INSENSITIVE) {
      cairo_paint_with_alpha (cr, 0.33);
    }
    else {
      cairo_paint (cr);
    }

    cairo_destroy (cr);
  }
}

/*!
 * \brief GedaBulb Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       GedaBulb class we are initializing
 * \param [in]  class_data  GedaBulb structure associated with the class
 */
static void geda_bulb_class_init(void *class, void *class_data)
{
  GedaBulbClass       *bulb_class;
  GObjectClass        *object_class;
  GtkButtonClass      *button_class;
  GtkCheckButtonClass *check_button_class;
  GtkWidgetClass      *widget_class;
  GParamSpec          *params;

  bulb_class         = (GedaBulbClass*)class;
  button_class       = (GtkButtonClass*)class;
  check_button_class = (GtkCheckButtonClass*)class;
  widget_class       = (GtkWidgetClass*)class;
  object_class       = (GObjectClass*)class;

  object_class->set_property = geda_bulb_set_property;
  object_class->get_property = geda_bulb_get_property;

  object_class->finalize     = geda_bulb_finalize;

  widget_class->focus        = geda_bulb_focus;

  button_class->clicked      = geda_bulb_clicked;

  check_button_class->draw_indicator = geda_bulb_draw_indicator;

  geda_bulb_parent_class = g_type_class_peek_parent (class);

  /*! property "group": GedaBulb::group
   *  \brief Sets a new group for a bulb button.
   *  \par
   *   The group property is actually a GSList containing pointers to all bulbs
   *   that are group members.
   */
  params = g_param_spec_object ("group",
                              _("Group"),
                              _("The bulb button whose group this widget belongs to."),
                                GEDA_TYPE_BULB,
                                G_PARAM_WRITABLE);

  g_object_class_install_property (object_class, PROP_GROUP, params);

  /*! property "show-button": GedaBulb::show-button
   * \brief Show Bulb Button during mouse over events
   * \par
   *  If this property is set to %TRUE, the button widget will be
   *  displayed during mouse over-events.
   */
  params = g_param_spec_boolean ("show-button",
                               _("Show Button"),
                               _("Whether to draw the button during mouse over events"),
                                  FALSE,
                                 (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_SHOW_BUTT, params);

  params = g_param_spec_int ("indicator-size",
                           _("Indicator Size"),
                           _("Size of check or radio indicator"),
                             0,
                             G_MAXINT,
                             INDICATOR_SIZE,
                             G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  params = g_param_spec_int ("indicator-spacing",
                           _("Indicator Spacing"),
                           _("Spacing around check or radio indicator"),
                             0,
                             G_MAXINT,
                             INDICATOR_SPACING,
                             G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  bulb_class->group_changed = NULL;

  /*! \brief Bulb group-changed
   * GedaBulb::group-changed:
   * bulb: the object whose group status was changed
   *
   * Emitted when the group of bulb buttons that a bulb button belongs
   * to changes. This is emitted when a bulb button switches from
   * being alone to being part of a group of 2 or more buttons, or
   * vice-versa, and when a button is moved from one group of 2 or
   * more buttons to a different one, but not when the composition
   * of the group that a button belongs to changes.
   *
   */
  group_changed_signal = g_signal_new ("group-changed",
                                       GEDA_TYPE_BULB,
                                       G_SIGNAL_RUN_FIRST,
                                       G_STRUCT_OFFSET (GedaBulbClass, group_changed),
                                       NULL, NULL,
                                       geda_marshal_VOID__VOID,
                                       G_TYPE_NONE, 0);
}

/*!
 * \brief Initialize new GedaBulb data structure instance.
 * \par Function Description
 *  This function is call after the GedaBulbClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaBulb data structure
 * \param [in] class     A GedaBulbClass Object
 */
static void geda_bulb_instance_init (GTypeInstance *instance, void *class)
{
  GedaBulb  *bulb   = (GedaBulb*)instance;
  GtkButton *button = (GtkButton*)bulb;
  GtkWidget *widget = (GtkWidget*)bulb;

  if (off_pixbuf == NULL) {
    off_pixbuf = gdk_pixbuf_new_from_xpm_data (geda_bulb_off_xpm);
  }
  else {
    g_object_ref (off_pixbuf);
  }

  if (on_pixbuf == NULL) {
    on_pixbuf = gdk_pixbuf_new_from_xpm_data (geda_bulb_on_xpm);
  }
  else {
    g_object_ref (on_pixbuf);
  }
  pix_buff_ref_count++;

  bulb->height = gdk_pixbuf_get_height (on_pixbuf);
  bulb->width  = gdk_pixbuf_get_width  (on_pixbuf);

  if (!bulb_hash_table) {
    bulb_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (bulb_hash_table, instance, instance);

  gtk_widget_set_receives_default (widget, FALSE);

  gtk_button_set_focus_on_click(button, FALSE);

  gtk_toggle_button_set_active ((GtkToggleButton*)bulb, TRUE);

  button->depress_on_activate = FALSE;

  ((GtkContainer*)bulb)->border_width = 1;

  bulb->group = g_slist_prepend (NULL, bulb);

  button_set_depressed (button, TRUE);
  gtk_widget_set_state (widget, GTK_STATE_ACTIVE);
}

/*! \brief Function to retrieve GedaBulb's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaBulb Type identifier. When
 *  first called, the function registers a #GedaBulb in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaBulb and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaBulb.
 */
GedaType geda_bulb_get_type (void)
{
  static volatile GedaType geda_bulb_type = 0;

  if (g_once_init_enter (&geda_bulb_type)) {

    static const GTypeInfo info = {
      sizeof(GedaBulbClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_bulb_class_init,           /* (GClassInitFunc)   */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaBulb),
      0,                               /* n_preallocs         */
      geda_bulb_instance_init         /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaBulb");
    type   = g_type_register_static (GTK_TYPE_CHECK_BUTTON, string, &info, 0);

    g_once_init_leave (&geda_bulb_type, type);
  }

  return geda_bulb_type;
}

/*!
 * \brief Check if an object is a GedaBulb
 * \par Function Description
 *  Determines if \a bulb is valid by verifying \a bulb is included
 *  in the hash table of GedaBulb objects.
 *
 * \return TRUE if \a bulb is a valid GedaBulb
 */
bool is_a_geda_bulb (GedaBulb *bulb)
{
  if ((bulb != NULL) && (bulb_hash_table != NULL)) {
    return g_hash_table_lookup(bulb_hash_table, bulb) ? TRUE : FALSE;
  }
  return FALSE;
}

/* -------------------------------------------------------------- */

/** \defgroup GedaBulb-Creators GedaBulb Creator Methods
 *  @{
 */

/*!
 * \brief Create a New GedaBulb
 * \par Function Description
 *  Creates a new #GedaBulb object, the bulb is add to \a group if the
 *  group exist. If the group does not exist a new bulb group is created.
 *
 * \param [in] group  The group to use for the new #GedaBulb
 *
 * \return a new #GedaBulb
 */
GtkWidget *geda_bulb_new (GSList *group)
{
  GtkWidget *bulb;

  bulb = g_object_new (GEDA_TYPE_BULB, NULL);

  if (group) {
    geda_bulb_set_group (bulb, group);
  }

  return bulb;
}

/*!
 * \brief Create a New Visible GedaBulb
 * \par Function Description
 *  Creates a new #GedaBulb object, the bulb is add to \a group if the
 *  group exist. If the group does not exist a new bulb group is created.
 *
 * \param [in] group  The group to use for the new #GedaBulb
 *
 * \return a new #GedaBulb with the visible property set TRUE
 */
GtkWidget *geda_bulb_new_visible (GSList *group)
{
  GtkWidget *bulb;

  bulb = g_object_new (GEDA_TYPE_BULB, NULL);

  if (group) {
    geda_bulb_set_group (bulb, group);
  }

  g_object_set (bulb, "visible", TRUE, NULL);

  return bulb;
}

/*!
 * \brief Create a New GedaBulb with a Label
 * \par Function Description
 *  Creates a new #GedaBulb object with a text label, the bulb is added to
 *  \a group if the group exist. If the group does not exist a new bulb group
 *  is created.
 *
 * \param [in] group  The group to use for the new #GedaBulb
 * \param [in] label  The string to use use for the label
 *
 * \return a new #GedaBulb
 */
GtkWidget *geda_bulb_new_with_label (GSList     *group,
                                     const char *label)
{
  GtkWidget *bulb;

  if (label) {
    bulb = g_object_new (GEDA_TYPE_BULB, "label", label, NULL);
  }
  else {
    bulb = g_object_new (GEDA_TYPE_BULB, NULL);
  }

  if (group) {
    geda_bulb_set_group (bulb, group);
  }

  return bulb;
}

/*!
 * \brief Create a New Visible GedaBulb with a Label
 * \par Function Description
 *  Creates a new #GedaBulb object with a text label, the bulb is add to
 *  \a group if the group exist. If the group does not exist a new bulb group
 *  is created.
 *
 * \param [in] group  The group to use for the new #GedaBulb
 * \param [in] label  The string to use use for the label
 *
 * \return a new labeled #GedaBulb with the visible property set TRUE
 */
GtkWidget *geda_bulb_new_visible_with_label (GSList     *group,
                                             const char *label)
{
  GtkWidget *bulb;

  bulb = geda_bulb_new_with_label (group, label);

  g_object_set (bulb, "visible", TRUE, NULL);

  return bulb;
}

/*!
 * \brief Create a New GedaBulb with a Mnemonic Label
 * \par Function Description
 *  Creates a new #GedaBulb object with a text label, the bulb is add to
 *  \a group if the group exist. If the group does not exist a new bulb group
 *  is created. \a label can, and should, include an underscores in indicate
 *  the mnemonic for the widget.
 *
 * \param [in] group  The group to use for the new #GedaBulb
 * \param [in] label  The string to use use for the label
 *
 * \return a new labeled #GedaBulb
 */
GtkWidget *geda_bulb_new_with_mnemonic (GSList     *group,
                                        const char *label)
{
  GtkWidget *bulb;

  bulb = g_object_new (GEDA_TYPE_BULB,
                       "label", label,
                       "use-underline", TRUE,
                       NULL);

  if (group)
    geda_bulb_set_group (bulb, group);

  return bulb;
}

/*!
 * \brief Create a New Visible GedaBulb with a Mnemonic Label
 * \par Function Description
 *  Creates a new #GedaBulb object with a text label, the bulb is add to
 *  \a group if the group exist. If the group does not exist a new bulb group
 *  is created. \a label can, and should, include an underscores in indicate
 *  the mnemonic for the widget.
 *
 * \param [in] group  The group to use for the new #GedaBulb
 * \param [in] label  The string to use use for the label
 *
 * \return a new labeled #GedaBulb with the visible property set TRUE
 */
GtkWidget *geda_bulb_new_visible_with_mnemonic (GSList     *group,
                                                const char *label)
{
  GtkWidget *bulb;

  bulb = g_object_new (GEDA_TYPE_BULB,
                       "label", label,
                       "use-underline", TRUE,
                       NULL);

  if (group) {
    geda_bulb_set_group (bulb, group);
  }

  g_object_set (bulb, "visible", TRUE, NULL);

  return bulb;
}

/*!
 * \brief Create a New GedaBulb from group Member
 * \par Function Description
 *  Creates a new #GedaBulb object with a text label, the bulb is added to
 *  same group as \a group_member.
 *
 * \param [in] group_member A #GedaBulb whose group the new bulb should join
 * \param [in] visible      Desired visiblity of the new bulb object
 *
 * \return a new #GedaBulb with the visible property set to \a visible
 */
GtkWidget *geda_bulb_new_from_widget (GtkWidget *group_member, bool visible)
{
  GSList    *group_list = NULL;
  GtkWidget *bulb;

  group_list = geda_bulb_get_group (group_member);

  bulb = geda_bulb_new (group_list);

  g_object_set (bulb, "visible", visible, NULL);

  return bulb;
}

/*!
 * \brief Create a New GedaBulb with Label from group Member
 * \par Function Description
 *  Creates a new #GedaBulb object with a text label, the bulb is added to
 *  same group as \a group_member.
 *
 * \param [in] group_member A #GedaBulb whose group the new bulb should join
 * \param [in] label        The string to use use for the label
 * \param [in] visible      Desired visiblity of the new bulb object
 *
 * \return a new labeled #GedaBulb with the visible property set TRUE
 */
GtkWidget *geda_bulb_new_with_label_from_widget (GtkWidget  *group_member,
                                                 const char *label,
                                                 bool        visible)
{
  GSList    *group_list = NULL;
  GtkWidget *bulb;

  group_list = geda_bulb_get_group (group_member);

  bulb = geda_bulb_new_with_label (group_list, label);

  g_object_set (bulb, "visible", visible, NULL);

  return bulb;
}

/*!
 * \brief Create a New GedaBulb with a Mnemonic Label from group Member
 * \par Function Description
 *  Creates a new #GedaBulb object with a mnemonic label, the bulb is added
 *  to same group as \a group_member. The \a label can, and should, include
 *  an underscores in indicate the mnemonic for the widget.
 *
 * \param [in] group_member The group to use for the new #GedaBulb
 * \param [in] label        String with mnemonic to use use for the label
 * \param [in] visible      Desired visiblity of the new bulb object
 *
 * \return a new labeled #GedaBulb with property \a visible
 */
GtkWidget *geda_bulb_new_with_mnemonic_from_widget (GtkWidget  *group_member,
                                                    const char *label,
                                                    bool        visible)
{
  GSList    *group_list = NULL;
  GtkWidget *bulb;

  group_list = geda_bulb_get_group (group_member);

  bulb = geda_bulb_new_with_mnemonic (group_list, label);

  g_object_set (bulb, "visible", visible, NULL);

  return bulb;
}
/** @} endgroup GedaBulb-Creators */


/** \defgroup GedaBulb-Groups Groups Creator Methods
 *  @{
 */

/*!
 * \brief Get the group list for a given GedaBulb member
 * \par Function Description
 *  Returns a single-linked list containing all the bulb in the same group
 *  as \a bulb. The returned list is owned by the bulb button and must not
 *  be modified or freed.
 *
 * \param [in] bulb  The group whose group is to be retrieved
 *
 * \return single-linked list of group members containing \a bulb
 */
GSList *geda_bulb_get_group (GtkWidget *bulb)
{
  g_return_val_if_fail (GEDA_IS_BULB (bulb), NULL);

  return ((GedaBulb*)bulb)->group;
}

/*!
 * \brief Set the Group a GedaBulb is to be Assocatiated
 * \par Function Description
 *  Sets the group of the #GedaBulb object.
 *
 * \note Setting the group does not change the interface layout in any way,
 *       if groups are changed the layout may need to be rearranged to reflect
 *       the changes.
 *
 * \param [in] widget The #GedaBulb is group is to be set
 * \param [in] group  The bulb group \a bulb is to join
 */
void geda_bulb_set_group (GtkWidget *widget, GSList *group)
{

  GtkWidget *old_group_singleton = NULL;
  GtkWidget *new_group_singleton = NULL;
  GedaBulb  *bulb;

  g_return_if_fail (GEDA_IS_BULB (widget));

  if (g_slist_find (group, widget)) {
    return;
  }

  bulb = (GedaBulb*)widget;

  if (bulb->group) {

    GSList *slist;

    bulb->group = g_slist_remove (bulb->group, bulb);

    if (bulb->group && !bulb->group->next) {
      old_group_singleton = g_object_ref (bulb->group->data);
    }

    for (slist = bulb->group; slist; slist = slist->next) {

      GedaBulb *tmp_button;

      tmp_button = slist->data;

      tmp_button->group = bulb->group;
    }
  }

  if (group && !group->next) {
    new_group_singleton = g_object_ref (group->data);
  }

  bulb->group = g_slist_prepend (group, bulb);

  if (group) {

    GSList *slist;

    for (slist = group; slist; slist = slist->next) {

      GedaBulb *tmp_button;

      tmp_button = slist->data;

      tmp_button->group = bulb->group;
    }
  }

  g_object_ref (bulb);

  GEDA_OBJECT_NOTIFY (bulb, "group");

  g_signal_emit (bulb, group_changed_signal, 0);

  if (old_group_singleton) {
    g_signal_emit (old_group_singleton, group_changed_signal, 0);
    g_object_unref (old_group_singleton);
  }

  if (new_group_singleton) {
    g_signal_emit (new_group_singleton, group_changed_signal, 0);
    g_object_unref (new_group_singleton);
  }

  gtk_toggle_button_set_active ((GtkToggleButton*)bulb, group == NULL);

  g_object_unref (bulb);
}

/*!
 * \brief Add #GedaBulb to a Group given a group member
 * \par Function Description
 *  Joins a #GedaBulb object to the group of another #GedaBulb object.
 *  If \a group_source is NULL, \a bulb will removed from any group that is
 *  currently a member. This function is equivilent to using geda_bulb_get_
 *  group and geda_bulb_set_group() methods
 *
 * \param [in] bulb         The #GedaBulb is group is to be set
 * \param [in] group_source The bulb whose group \a bulb is to join
 *
 *  example:
 * \code{.c}
 *   GedaBulb *bulb;
 *   GedaBulb *last_button;
 *
 *   while ( ...more buttons to add... ) {
 *
 *        bulb = geda_bulb_new (...);
 *
 *        geda_bulb_join_group (bulb, last_button);
 *        last_button = bulb;
 *   }
 * \endcode
 */
void geda_bulb_join_group (GtkWidget *bulb, GtkWidget *group_source)
{
  g_return_if_fail (GEDA_IS_BULB (bulb));
  g_return_if_fail (group_source == NULL || GEDA_IS_BULB (group_source));

  if (group_source) {

    GSList *group;

    group = geda_bulb_get_group (group_source);

    if (!group) {

      /* if we are not already part of a group we need to setup
       * a new group and then get the newly created group */
      geda_bulb_set_group (group_source, NULL);
      group = geda_bulb_get_group (group_source);
    }

    geda_bulb_set_group (bulb, group);
  }
  else {

    geda_bulb_set_group (bulb, NULL);
  }
}

/*!
 * \brief Get the Index of the Active GedaBulb in a group
 * \par Function Description
 *  Returns the numerical index of the active bulb in a group,
 *  in other words, the bulb that is turned-on.
 *
 *  \note Group list are Zero based single-linked list, so the
 *        first bulb is 0 and the last is group length minus 1.
 *
 * \param [in] group_list The bulb group to be queried
 *
 * \returns index of active member
 */
int geda_bulb_group_get_active_index (GSList *group_list) {

  int length;
  int index;
  int active = -1;

  g_return_val_if_fail (group_list != NULL, -1);

  length = g_slist_length (group_list);

  for (index = 0; index < length; index++) {

    GtkToggleButton *button;

    button = (GtkToggleButton*)g_slist_nth_data (group_list, index);

    if (button == NULL) {
      return -1;
    }

    if (gtk_toggle_button_get_active (button) == TRUE) {
      active = index;
      break;
    }
  }

  /* new buttons are *prepended* to the list, so buttons added first
   * in the last positions in the list and using glist reverse
   * confuses gtk */
  return ((length - 1) - active);
}

/*!
 * \brief Set the Active GedaBulb in a group by Index
 * \par Function Description
 *  Set the bulb at index active, that is turns-on the bulb, other
 *  members will be turn-off automatically.
 *
 * \note Group list are Zero based single-linked list, so the
 *       first bulb is 0 and the last is group length minus 1.
 *
 * \param [in] group_list  The bulb group to be set
 * \param [in] which_bulb  Index of bulb in group to set active
 */
void geda_bulb_group_set_active_index (GSList *group_list, int which_bulb)
{
  GtkToggleButton *button;
  int      length;
  int      index;
  unsigned pos;

  pos    = (unsigned int)(long)which_bulb;
  length = g_slist_length (group_list);

  /* new buttons are *prepended* to the list, so buttons added as
   * first have last position in the list and using glist reverse
   * confuses gtk, so do this instead ... */
  index = (length - 1) - pos;

  if (index < 0 || index >= length) return;

  button = (GtkToggleButton*)g_slist_nth_data (group_list, index);

  if (button != NULL) {

    if (gtk_toggle_button_get_active (button) == FALSE) {

      gtk_toggle_button_set_active (button, TRUE);
    }
  }

  return;
}

/*!
 * \brief Quietly Set the Active GedaBulb in a group by Index
 * \par Function Description
 *  Set the bulb at index active, that is, turns-on the bulb, and
 *  turns-off the other members. This function is generally only
 *  used to set the initial state of a group after creation as no
 *  signals will be generated. This means callback handlers in the
 *  application will not get notified of the state change.
 *
 * \note Group list are Zero based single-linked list, so the
 *       first bulb is 0 and the last is group length minus 1.
 *
 * \param [in] group_list  The bulb group to be set
 * \param [in] which_bulb  Index of bulb in group to set active
 */
void geda_bulb_group_quietly_set_active (GSList *group_list, int which_bulb)
{
  int length;
  int target;
  int index;
  int pos;

  pos    = (unsigned int)(long)which_bulb;
  length = g_slist_length (group_list);

  /* new buttons are *prepended* to the list, so buttons added as
   * first have last position in the list and using glist reverse
   * confuses gtk, so do this instead ... */
  target = (length - 1) - pos;

  for (index = 0; index < length; ++index) {

    GtkToggleButton *button;

    button = (GtkToggleButton*)g_slist_nth_data (group_list, index);

    if (button != NULL) {
      button->active = index == target;
      gtk_widget_queue_draw((GtkWidget*)button);
    }
  }

  return;
}
/** @} endgroup GedaBulb-Groups */

/*!
 * \brief GedaBulb Get Show Button during Mouse-Over events
 * \par Function Description
 *  Returns whether the pre-light button should be display during
 *  mouse over-events
 *
 * \param widget Pointer to a #GedaBulb object.
 *
 * \returns whether to show the button.
 */
bool geda_bulb_get_show_button (GtkWidget *widget)
{
  GedaBulb  *bulb;

  g_return_val_if_fail (GEDA_IS_BULB (widget), FALSE);

  bulb = (GedaBulb*)widget;

  return bulb->show_butt;
}

/*!
 * \brief GedaBulb Set Show Button during Mouse-Over events
 * \par Function Description
 *  If \a show_button is %TRUE, the pre-light button will be drawn
 *  during mouse-over event.
 *
 * \param [in] widget        Pointer to a #GedaBulb object.
 * \param [in] show_button   Desired setting.
 */
void geda_bulb_set_show_button (GtkWidget *widget, bool show_button)
{
  GedaBulb  *bulb;

  g_return_if_fail (GEDA_IS_BULB (widget));

  bulb = (GedaBulb*)widget;

  bulb->show_butt = (show_button != FALSE);
}

/*!
 * \brief Set sensitivity of a Group of GedaBulbs
 * \par Function Description
 *  The is a convenience function to set the sensitivity of all the
 *  widgets in the group to the given state.
 *
 * \param [in] group       Pointer to a #GedaBulb group.
 * \param [in] sensitive   Desired state.
 */
void geda_bulb_set_group_sensitive (GSList *group, bool sensitive)
{
  GSList *iter;

  for (iter = group; iter; iter = iter->next) {
    if (GEDA_IS_BULB(iter->data)) {
      gtk_widget_set_sensitive((GtkWidget*)iter->data, sensitive);
    }
  }
}

/** @} end group GedaBulb */
