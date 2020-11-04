/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
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
 */
/*!
 * \file gschem_page_view.c
 *
 * \brief A widget for viewing a schematic page
 */

#include <gschem.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtkmarshal.h>
#include <math.h>

#include <geda_debug.h>

/** \defgroup Gschem-Page-View Gschem Page View
 * @{
 * \brief #GschemPageView Class Implmentation
 * \par
 *  This module implements the Page View class in gschem.
 */

#define INVALIDATE_MARGIN 1

enum
{
  PROP_0,
  PROP_HADJUSTMENT,
  PROP_PAGE,
  PROP_PAGE_GEOMETRY,
  PROP_VADJUSTMENT
};

/* Function Prototypes */

static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec);

static void
gschem_page_view_class_init (void *g_class, void *g_class_data);

static void
gschem_page_view_instance_init (GTypeInstance *instance, void *g_class);

static void
gschem_page_view_update_hadjustment (GschemPageView *view);

static void
gschem_page_view_update_vadjustment (GschemPageView *view);

static void
hadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view);

static void
remove_page_weak_reference (Page *page, void *geometry, GschemPageView *view);

static void
page_deleted (Page *page, GschemPageView *view);

static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec);

//static void
//set_scroll_adjustments (GschemPageView *view, GtkAdjustment *hadjustment, GtkAdjustment *vadjustment);

static void
vadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view);

static GObjectClass *gschem_page_view_parent_class = NULL;


/*
 *  In later versions of GTK+, the GtkScrolledWindow uses an interface, instead
 *  of signals, to set the scrollbar adjustments. When Gschem uses on of these
 *  more recent version of GTK+, this function will no longer be needed.
 *  TODO: Use libgedauio
 */

/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  g_return_if_fail (view != NULL);

  gschem_page_view_set_hadjustment (view, NULL);
  gschem_page_view_set_vadjustment (view, NULL);

  g_hash_table_foreach (view->geometry_table, (GHFunc)remove_page_weak_reference, view);
  g_hash_table_remove_all (view->geometry_table);

  /* According to the GObject Manual the dispose function might be
   * called several times. We don't want to call
   * gschem_page_view_set_page twice here  */
  if (view->page != NULL) {
    gschem_page_view_set_page (view, NULL);
  }

  /* lastly, chain up to the parent dispose */

  g_return_if_fail (gschem_page_view_parent_class != NULL);
  gschem_page_view_parent_class->dispose (object);
}

/*!
 * \brief Event handler for window realized
 * \par Function Description
 */
static void
event_realize(GtkWidget *widget, void *unused)
{
  GschemPageView *view   = GSCHEM_PAGE_VIEW(widget);
  GdkWindow      *window = gtk_widget_get_window (widget);

  g_return_if_fail (view != NULL);
  g_return_if_fail (window != NULL);

  gtk_widget_get_allocation (widget, &(view->previous_allocation));
}

/*!
 * \brief Finalize object
 * \par Function Description
 */
static void
finalize (GObject *object)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  g_return_if_fail (view != NULL);
  g_return_if_fail (view->geometry_table != NULL);

  g_hash_table_destroy (view->geometry_table);

  /* lastly, chain up to the parent finalize */

  g_return_if_fail (gschem_page_view_parent_class != NULL);
  gschem_page_view_parent_class->finalize (object);
}

/*!
 * \brief Get a property
 * \par Function Description
 * \param [in]     object
 * \param [in]     param_id
 * \param [in,out] value
 * \param [in]     pspec
 */
static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  switch (param_id) {
    case PROP_HADJUSTMENT:
      g_value_set_object (value, gschem_page_view_get_hadjustment (view));
      break;

    case PROP_PAGE:
      g_value_set_pointer (value, gschem_page_view_get_page (view));
      break;

    case PROP_PAGE_GEOMETRY:
      g_value_set_boxed (value, gschem_page_view_get_page_geometry (view));
      break;

    case PROP_VADJUSTMENT:
      g_value_set_object (value, gschem_page_view_get_vadjustment (view));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*!
 * \brief Initialize GschemPageView class
 * \par Function Description
 * \param [in]  g_class       GschemPageViewClass being initialized
 * \param [in]  g_class_data  (unused)
 */
static void
gschem_page_view_class_init (void *g_class, void *g_class_data)
{
  GObjectClass   *gobject_class = G_OBJECT_CLASS (g_class);
  GtkWidgetClass *widget_class  = (GtkWidgetClass*)g_class;
  GParamSpec     *pspec;

  gschem_page_view_parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (g_class));

  gobject_class->dispose  = dispose;
  gobject_class->finalize = finalize;

  gobject_class->get_property = get_property;
  gobject_class->set_property = set_property;

  pspec = g_param_spec_object ("hadjustment",
                             _("Horizontal adjustment"),
                             _("Horizontal adjustment"),
                               GTK_TYPE_ADJUSTMENT,
                               G_PARAM_READWRITE | G_PARAM_CONSTRUCT);

  g_object_class_install_property (gobject_class, PROP_HADJUSTMENT, pspec);

  pspec = g_param_spec_pointer ("page",
                              _("Page"),
                              _("Page"),
                                G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);

  g_object_class_install_property (gobject_class, PROP_PAGE, pspec);

  pspec = g_param_spec_boxed ("page-geometry",
                            _("Page Geometry"),
                            _("Page Geometry"),
                              GSCHEM_TYPE_PAGE_GEOMETRY,
                              G_PARAM_READABLE | G_PARAM_STATIC_STRINGS);

  g_object_class_install_property (gobject_class, PROP_PAGE_GEOMETRY, pspec);

  pspec = g_param_spec_object ("vadjustment",
                             _("Vertical adjustment"),
                             _("Vertical adjustment"),
                               GTK_TYPE_ADJUSTMENT,
                               G_PARAM_READWRITE | G_PARAM_CONSTRUCT);

  g_object_class_install_property (gobject_class, PROP_VADJUSTMENT, pspec);

  GedaType type = gschem_page_view_get_type();

  widget_class->set_scroll_adjustments_signal =
      g_signal_new ("set-scroll-adjustments", type,
                    G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                    0,
                    NULL,
                    NULL,
                    geda_marshal_VOID__OBJECT_OBJECT,
                    G_TYPE_NONE,
                    2,
                    GTK_TYPE_ADJUSTMENT,
                    GTK_TYPE_ADJUSTMENT);

  g_signal_new ("update-grid-info", type,
                G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                0,
                NULL,
                NULL,
                geda_marshal_VOID__VOID,
                G_TYPE_NONE,
                0);
}

/*!
 * \brief Get the horizontal adjustment for this view
 * \par Function Description
 *
 * \param [in] view The view
 * \return The horizontal adjustment for this view
 */
GtkAdjustment *gschem_page_view_get_hadjustment (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->hadjustment;
}

/*!
 * \brief Get page for this view
 * \par Function Description
 * \param [in] view The view
 * \return The page for the view
 */
Page *gschem_page_view_get_page (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->page;
}

/*!
 * \brief Get page geometry for this view
 * \par Function Description
 * \param [in] view The view
 * \return The page geometry for the view
 */
GschemPageGeometry *gschem_page_view_get_page_geometry (GschemPageView *view)
{
  typedef void (*NotifyFunction) (void*,void*);

  GschemPageGeometry *geometry = NULL;
  Page *page = NULL;

  int screen_width;
  int screen_height;

  g_return_val_if_fail (view != NULL, NULL);

  page = gschem_page_view_get_page (view);
  if (page == NULL) {
    return NULL;
  }

  geometry = g_hash_table_lookup (view->geometry_table, page);

#if (HAVE_GDK_WINDOW_GET_WIDTH)

    screen_width  = gdk_window_get_width (GTK_WIDGET (view)->window);
    screen_height = gdk_window_get_height (GTK_WIDGET (view)->window);

#else

    gdk_drawable_get_size(GTK_WIDGET (view)->window, &screen_width, &screen_height);

#endif

  if (geometry == NULL) {

    int viewport_left   = view->page->left;
    int viewport_top    = view->page->top;
    int viewport_right  = view->page->right;
    int viewport_bottom = view->page->bottom;

    int world_left      = view->page->left;
    int world_top       = view->page->top;
    int world_right     = view->page->right;
    int world_bottom    = view->page->bottom;

    geometry = gschem_page_geometry_new_with_values (screen_width,
                                                     screen_height,
                                                     viewport_left,
                                                     viewport_top,
                                                     viewport_right,
                                                     viewport_bottom,
                                                     world_left,
                                                     world_top,
                                                     world_right,
                                                     world_bottom);

    g_hash_table_insert (view->geometry_table, page, geometry);
    geda_page_weak_ref (page, (NotifyFunction) page_deleted, view);

    gschem_page_geometry_zoom_extents (geometry,
                                       geda_struct_page_get_objects (page), I_PAN_IGNORE_BORDERS);
  }
  else {
    gschem_page_geometry_set_values (geometry,
                                     screen_width,
                                     screen_height,
                                     gschem_page_geometry_get_viewport_left (geometry),
                                     gschem_page_geometry_get_viewport_top (geometry),
                                     gschem_page_geometry_get_viewport_right (geometry),
                                     gschem_page_geometry_get_viewport_bottom (geometry));
  }

  return geometry;
}

/*!
 * \brief Get/register GschemPageView type.
 * \par Function Description
 */
GType gschem_page_view_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemPageViewClass),
      NULL,                                      /* base_init */
      NULL,                                      /* base_finalize */
      gschem_page_view_class_init,               /* GClassInitFunc */
      NULL,                                      /* class_finalize */
      NULL,                                      /* class_data */
      sizeof(GschemPageView),
      0,                                         /* n_preallocs */
      gschem_page_view_instance_init,            /* GInstanceInitFunc */
    };

    type = g_type_register_static (GTK_TYPE_DRAWING_AREA, "GschemPageView", &info, 0);
  }

  return type;
}

/*!
 * \brief Check if an object is a GschemPageView
 * \par Function Description
 *  Ensures view is a valid G_Object and compares signature
 *  to gschem page view type.
 * \return TRUE if \a view is a valid GschemPageView
 */
bool is_a_gschem_page_view (GschemPageView *view)
{
  if (G_IS_OBJECT(view)) {
    return (gschem_page_view_get_type() == view->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Get the vertical adjustment for this view
 * \par Function Description
 * \param [in] view The view
 * \return The vertical adjustment for this view
 */
GtkAdjustment *gschem_page_view_get_vadjustment (GschemPageView *view)
{
  g_return_val_if_fail (view != NULL, NULL);

  return view->vadjustment;
}

/*!
 * \brief Schedule redraw for the entire window
 * \par Function Description
 * \param [in,out] view The Gschem page view to redraw
 */
void gschem_page_view_invalidate_all (GschemPageView *view)
{
  GdkWindow *window;

  g_return_if_fail (view != NULL);

  window = gtk_widget_get_window (GTK_WIDGET (view));

  if (window == NULL) {
    return;
  }

  gdk_window_invalidate_rect (window, NULL, FALSE);
}


/*!
 * \brief Schedule redraw of the given object
 * \par Function Description
 * \param [in,out] w_current Pointer to GschemToplevel data structure
 * \param [in]     object    Object to redraw
 */
void
gschem_page_view_invalidate_object (GschemToplevel *w_current, GedaObject *object)
{
  GschemPageView *view;

  view = (GschemPageView*)w_current->drawing_area;

  g_return_if_fail (object != NULL);
  g_return_if_fail (view != NULL);

  if (!gtk_widget_get_realized(GTK_WIDGET(view))) {
    return;
  }

  if (view->page != NULL) {
    bool      success;
    int world_bottom;
    int world_right;
    int world_left;
    int world_top;

    success = geda_object_get_bounds (object,
                            &world_left,
                            &world_top,
                            &world_right,
                            &world_bottom);

    if (success) {
      gschem_page_view_invalidate_world_rect (w_current,
                                              world_left,
                                              world_top,
                                              world_right,
                                              world_bottom);
    }
  }
}

/*!
 * \brief Schedule redraw of the given rectange
 * \par Function Description
 * \param [in,out] w_current The Gschem page view to redraw
 * \param [in]     left
 * \param [in]     top
 * \param [in]     right
 * \param [in]     bottom
 */
void
gschem_page_view_invalidate_screen_rect (GschemToplevel *w_current, int left, int top, int right, int bottom)
{
  GschemPageView *view;
  GdkRectangle    rect;
  GdkWindow      *window;

  int bloat;
  int cue_half_size;
  int grip_half_size;

  view = (GschemPageView*)w_current->drawing_area;

  g_return_if_fail (view != NULL);

  window = gtk_widget_get_window (GTK_WIDGET (view));

  if (window == NULL) {
    return;
  }

  grip_half_size = gschem_toplevel_get_grips_half_size(w_current);
  cue_half_size = gschem_page_view_SCREENabs (view, CUE_BOX_SIZE);
  bloat = MAX (grip_half_size, cue_half_size) + INVALIDATE_MARGIN;

  rect.x = MIN(left, right) - bloat;
  rect.y = MIN(top, bottom) - bloat;
  rect.width = 1 + abs( left - right ) + 2 * bloat;
  rect.height = 1 + abs( top - bottom ) + 2 * bloat;

  gdk_window_invalidate_rect (window, &rect, FALSE);
}

/*!
 * \brief Schedule redraw of the given rectange
 * \par Function Description
 *
 * \param [in,out] w_current The Gschem page view to redraw
 * \param [in]     left
 * \param [in]     top
 * \param [in]     right
 * \param [in]     bottom
 */
void
gschem_page_view_invalidate_world_rect (GschemToplevel *w_current, int left, int top, int right, int bottom)
{
  GschemPageView *view;
  int screen_bottom;
  int screen_right;
  int screen_left;
  int screen_top;

  view = (GschemPageView*)w_current->drawing_area;

  g_return_if_fail (view != NULL);

  gschem_page_view_WORLDtoSCREEN (view, left, top, &screen_left, &screen_top);
  gschem_page_view_WORLDtoSCREEN (view, right, bottom, &screen_right, &screen_bottom);

  gschem_page_view_invalidate_screen_rect (w_current,
                                           screen_left,
                                           screen_top,
                                           screen_right,
                                           screen_bottom);
}

/*!
 * \brief Initialize GschemPageView instance
 * \par Function Description
 * \param [in,out] instance The GschemPageView being initialized.
 * \param [in]     g_class  Class of the type the instance is created for.
 */
static void
gschem_page_view_instance_init (GTypeInstance *instance, void *g_class)
{
  GschemPageView *view = (GschemPageView*)instance;
  view->instance_type  = gschem_page_view_get_type();

  view->hadjustment = NULL;
  view->vadjustment = NULL;

  view->geometry_table = g_hash_table_new_full (g_direct_hash,
                                                g_direct_equal,
                                                NULL,
                                                (GDestroyNotify) gschem_page_geometry_free);

  view->page = NULL;
  view->configured = FALSE;

  view->doing_pan = FALSE;
  view->pan_x = 0;
  view->pan_y = 0;
  view->throttle = 0;
/*
  g_signal_connect (view,
                    "set-scroll-adjustments",
                    G_CALLBACK (set_scroll_adjustments),
                    NULL);
*/
  g_signal_connect(view,
                   "realize",
                   G_CALLBACK (event_realize),
                   NULL);
}

/*!
 * \brief Create a new instance of GschemPageView
 * \par Function Description
 * \return A new instanceof the GschemPageView
*/
GschemPageView *gschem_page_view_new ()
{
  return g_object_new (GSCHEM_TYPE_PAGE_VIEW, NULL);
}

/*!
 * \brief Create a new instance of GschemPageView
 * \par Function Description
 * \return A new instance of the GschemPageView
 */
GschemPageView *gschem_page_view_new_with_page (Page *page)
{
  return g_object_new (GSCHEM_TYPE_PAGE_VIEW, "page", page, NULL);
}

/*!
 * \brief Pan the view on the given world coordinate using given zoom factor
 * \par Function Description
 * \param [in,out] view      This GschemPageView
 * \param [in]     w_x       The world x coordinate of the new center
 * \param [in]     w_y       The world y coordinate of the new center
 * \param [in]     rel_zoom  The Relative Zoom Factor
 */
void
gschem_page_view_pan_general (GschemPageView *view, int w_x, int w_y, double rel_zoom)
{
  GschemPageGeometry *geometry = NULL;

  g_return_if_fail (view != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  /* make mouse to the new world-center;
     attention: there are information looses because of type cast in mil_x */

  gschem_page_geometry_pan_general (geometry, w_x, w_y, rel_zoom, 0);

  g_signal_emit_by_name (view, "update-grid-info");
  gschem_page_view_update_scroll_adjustments (view);
  gschem_page_view_invalidate_all (view);
}


/*!
 * \brief Center the view on the given world coordinate
 * \par Function Description
 * \param [in,out] view This GschemPageView
 * \param [in]     w_x  The world x coordinate of the new center
 * \param [in]     w_y  The world y coordinate of the new center
 */
void gschem_page_view_pan (GschemPageView *view, int w_x, int w_y)
{
  gschem_page_view_pan_general (view, w_x, w_y, 1);
  /* Trigger a motion event to update the objects being drawn */
  /* This works e.g. if the view is centered at the mouse pointer position */
  // x_event_faked_motion (view, NULL);
}


/*!
 * \brief Pan the view by the given screen coordinate displacement
 * \par Function Description
 * \param [in,out] view      This GschemPageView
 * \param [in]     diff_x    The screen x coordinate displacement
 * \param [in]     diff_y    The screen y coordinate displacement
 */
void gschem_page_view_pan_mouse (GschemPageView *view, int diff_x, int diff_y)
{
  GschemPageGeometry *geometry = NULL;
  double world_cx, world_cy;
  double page_cx, page_cy;

  g_return_if_fail (view != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

#if DEBUG
  printf("gschem_page_view_pan_mouse(): diff_x=%d, diff_y=%d\n", diff_x, diff_y);
#endif

  page_cx = (gschem_page_geometry_get_viewport_left (geometry) + gschem_page_geometry_get_viewport_right (geometry)) / 2.0;
  page_cy = (gschem_page_geometry_get_viewport_top (geometry) + gschem_page_geometry_get_viewport_bottom (geometry)) / 2.0;

  world_cx = page_cx - gschem_page_view_WORLDabs (view, diff_x);
  world_cy = page_cy + gschem_page_view_WORLDabs (view, diff_y);

#if DEBUG
  printf("  world_cx=%f, world_cy=%f\n", world_cx, world_cy);
#endif

  gschem_page_view_pan_general (view, world_cx, world_cy, 1);

  /* Trigger a motion event to update the objects being drawn */
  /* Don't emit such an event if diffs are zero to avoid recursion */
  /*
  if (diff_x == 0 && diff_y == 0) {
    x_event_faked_motion (view, NULL);
  }
  */
}

/*!
 * \brief Start mouse panning in the view
 * \par Function Description
 *  This function saves current coordinates of the mouse pointer
 *  to pan_x and pan_y  and toggles the view into pan mode.
 *
 * \param [in,out] view  This GschemPageView
 * \param [in]     x     The screen x coordinate
 * \param [in]     y     The screen y coordinate
 */
void gschem_page_view_pan_start (GschemPageView *view, int x, int y)
{
  view->doing_pan = TRUE;
  view->pan_x = x;
  view->pan_y = y;
  view->throttle = 0;
}

/*!
 * \brief Continue mouse panning in the view
 * \par Function Description
 *  In the view pan mode, this function calculates displacement of
 *  the mouse pointer relative to its previous position and repans
 *  the view taking into account the given mouse pan gain setting.
 *  Then it replaces pan_x and pan_y with the new coordinates.
 *
 * \param [in,out] view            This GschemPageView
 * \param [in]     mousepan_gain   Mouse pan gain
 * \param [in]     x               The new screen x coordinate
 * \param [in]     y               The new screen y coordinate
 */
void
gschem_page_view_pan_motion (GschemPageView *view, int mousepan_gain, int x, int y)
{
  if (view->doing_pan) {

    int pdiff_x, pdiff_y;

    pdiff_x = x - view->pan_x;
    pdiff_y = y - view->pan_y;

    if (!(view->throttle % 5)) {
      gschem_page_view_pan_mouse(view,
                                 pdiff_x * mousepan_gain,
                                 pdiff_y * mousepan_gain);

      view->pan_x = x;
      view->pan_y = y;
    }
    view->throttle++;
  }
}

/*!
 * \brief End mouse panning in the view
 * \par Function Description
 *  This function resets the view pan mode and invalidates the
 *  view after panning.
 *
 * \param [in,out] view      This GschemPageView
 *
 * \returns TRUE if panning was completed, FALSE if there was no panning
 */
bool gschem_page_view_pan_end (GschemPageView *view)
{
  if (view->doing_pan) {

    gschem_page_view_invalidate_all (view);

    view->doing_pan = FALSE;

    return TRUE;
  }
  else {
    return FALSE;
  }
}

/*!
 * \brief Transform SCREEN coordinates to WORLD coordinates
 * \par Function Description
 *  This function takes in SCREEN x/y coordinates and
 *  transforms them to WORLD x/y coordinates.
 *
 * \param [in]  view       The GschemPageView object.
 * \param [in]  mx         The x coordinate in SCREEN units.
 * \param [in]  my         The y coordinate in SCREEN units.
 * \param [out] x          The x coordinate in WORLD units.
 * \param [out] y          The y coordinate in WORLD units.
 * \note Question: why are we returning in x and y
 *                 if this is SCREEN to WORLD shouldn't WORLD
 *                 coordinates be returned in mx and my?
 */
void
gschem_page_view_SCREENtoWORLD (GschemPageView *view, int mx, int my, int *x, int *y)
{
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_if_fail (geometry != NULL);

  *x = gschem_page_geometry_mil_x (geometry, mx);
  *y = gschem_page_geometry_mil_y (geometry, my);
}

/*!
 * \brief Set the horizontal scroll adjustment for this view
 * \par Function Description
 * \param [in,out] view The view
 * \param [in]     hadjustment The horizontal scroll adjustment
 */
void
gschem_page_view_set_hadjustment (GschemPageView *view, GtkAdjustment *hadjustment)
{
  g_return_if_fail (view != NULL);

  if (view->hadjustment != NULL) {
    g_signal_handlers_disconnect_by_func (G_OBJECT (view->hadjustment),
                                          G_CALLBACK (hadjustment_value_changed),
                                          view);

    g_object_unref (view->hadjustment);
  }

  view->hadjustment = hadjustment;

  if (view->hadjustment != NULL) {

    g_object_ref (view->hadjustment);

    g_signal_connect (view->hadjustment,
                      "value-changed",
                      G_CALLBACK (hadjustment_value_changed),
                      view);
  }

  GEDA_OBJECT_NOTIFY (view, "hadjustment");
}

/*!
 * \brief Set the page for this view
 * \par Function Description
 *  The toplevel property must be set and the page must belong to that
 *  toplevel. Currently, the codebase does not allow the page to be
 *  NULL.
 *
 * \param [in,out] view The view
 * \param [in]     page The page
 */
void gschem_page_view_set_page (GschemPageView *view, Page *page)
{
  g_return_if_fail (view != NULL);

  view->page = page;

  if (page != NULL) {
    g_return_if_fail (page->toplevel != NULL);
    geda_struct_page_goto (page);
  }

  GEDA_OBJECT_NOTIFY (view, "page");
  GEDA_OBJECT_NOTIFY (view, "page-geometry");
  g_signal_emit_by_name (view, "update-grid-info");
}

/*!
 * \brief Set the vertical scroll adjustment for this view
 * \par Function Description
 * \param [in,out] view        The view
 * \param [in]     vadjustment The vertical scroll adjustment
 */
void
gschem_page_view_set_vadjustment (GschemPageView *view, GtkAdjustment *vadjustment)
{
  g_return_if_fail (view != NULL);

  if (G_IS_OBJECT(view->vadjustment)) {

    g_signal_handlers_disconnect_by_func (view->vadjustment,
                                          G_CALLBACK (vadjustment_value_changed),
                                          view);
    g_object_unref (view->vadjustment);
  }

  view->vadjustment = vadjustment;

  if (G_IS_OBJECT(view->vadjustment)) {

    g_signal_connect (g_object_ref(view->vadjustment),
                      "value-changed",
                      G_CALLBACK (vadjustment_value_changed),
                      view);
  }

  GEDA_OBJECT_NOTIFY (view, "vadjustment");
}

/*!
 * \brief Signal handler for a horizontal scroll adjustment change
 * \par Function Description
 */
static void
hadjustment_value_changed (GtkAdjustment *hadjustment, GschemPageView *view)
{
  if (hadjustment != NULL) {

    g_return_if_fail (view != NULL);

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

    if (geometry != NULL) {

      if (view->hadjustment != NULL) {

        int current_left;
        int new_left;

        g_return_if_fail (view->hadjustment == hadjustment);

        current_left = gschem_page_geometry_get_viewport_left (geometry),
        new_left = (int) hadjustment->value;

        geometry->viewport_left = new_left;
        geometry->viewport_right = geometry->viewport_right - (current_left - new_left);

        gschem_page_view_invalidate_all (view);
      }
    }
  }
  else {
    BUG_MSG("hadjustment == NULL");
  }
}

/*!
 * \brief Set a gobject property
 * \par Function Description
 */
static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec)
{
  GschemPageView *view = GSCHEM_PAGE_VIEW (object);

  switch (param_id) {
    case PROP_HADJUSTMENT:
      gschem_page_view_set_hadjustment (view, g_value_get_object (value));
      break;

    case PROP_PAGE:
      gschem_page_view_set_page (view, g_value_get_pointer (value));
      break;

    case PROP_VADJUSTMENT:
      gschem_page_view_set_vadjustment (view, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*!
 * \brief Get absolute SCREEN value
 * \par Function Description
 *  Converts WORLD value \a val to absolute SCREEN value.
 *
 * \param [in] view This GschemPageView
 * \param [in] val The value to convert
 *
 * \return converted value in SCREEN pixels
*/
int gschem_page_view_SCREENabs(GschemPageView *view, int val)
{
  double f0,f1;
  double i;
  int j;
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_val_if_fail (view != NULL, 0);

  if (geometry == NULL) return 0;

  f0 = gschem_page_geometry_get_viewport_left  (geometry);
  f1 = gschem_page_geometry_get_viewport_right (geometry);
  i  = (double)(geometry->screen_width) * (double)(val) / (f1 - f0);

#ifdef HAVE_LRINT
  j = lrint(i);
#else
  j = i + 0.5;
#endif

  return(j);
}

/*!
 * \brief Update the horizontal scroll adjustment
 * \par Function Description
 */
static void gschem_page_view_update_hadjustment (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  if (view->hadjustment != NULL) {

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
    g_return_if_fail (geometry != NULL);

    gtk_adjustment_set_page_increment (view->hadjustment,
                                       fabs (geometry->viewport_right - geometry->viewport_left) - 100.0);

    gtk_adjustment_set_page_size (view->hadjustment,
                                  fabs (geometry->viewport_right - geometry->viewport_left));

    gtk_adjustment_set_value (view->hadjustment,
                               geometry->viewport_left);

#if DEBUG
    printf("H %f %f\n", view->hadjustment->lower, view->hadjustment->upper);
    printf("Hp %f\n", view->hadjustment->page_size);
#endif

    gtk_adjustment_changed(view->hadjustment);
    gtk_adjustment_value_changed (view->hadjustment);
  }
}

/*!
 * \brief Update the scroll adjustments
 * \par Function Description
 */
void gschem_page_view_update_scroll_adjustments (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  gschem_page_view_update_hadjustment (view);
  gschem_page_view_update_vadjustment (view);
}

/*!
 * \brief Update the vertical scroll adjustment
 * \par Function Description
 */
static void gschem_page_view_update_vadjustment (GschemPageView *view)
{
  g_return_if_fail (view != NULL);

  if (view->vadjustment != NULL) {

    GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
    g_return_if_fail (geometry != NULL);

    gtk_adjustment_set_page_increment(view->vadjustment,
                                      fabs (geometry->viewport_bottom - geometry->viewport_top) - 100.0);

    gtk_adjustment_set_page_size (view->vadjustment,
                                  fabs (geometry->viewport_bottom - geometry->viewport_top));

    gtk_adjustment_set_value(view->vadjustment,
                             geometry->world_bottom - geometry->viewport_bottom);

#if DEBUG
    printf("V %f %f\n", view->vadjustment->lower, view->vadjustment->upper);
    printf("Vp %f\n", view->vadjustment->page_size);
#endif

    gtk_adjustment_changed(view->vadjustment);
    gtk_adjustment_value_changed (view->vadjustment);
  }
}

/*!
 * \brief Get absolute WORLD coordinate.
 * \par Function Description
 *  Get absolute WORLD coordinate.
 *
 * \param [in,out] view The view
 * \param [in]     val  The coordinate to convert.
 *
 *
 * \return The converted WORLD coordinate.
 */
int gschem_page_view_WORLDabs(GschemPageView *view, int val)
{
  GtkAllocation *allocation;
  double fw0,fw1,fw,fval;
  double i;
  int j;

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  allocation = geda_get_widget_allocation (view);

  fw1 = geometry->viewport_right;
  fw0 = geometry->viewport_left;
  fw  = allocation->width;
  fval = val;
  i = fval * (fw1 - fw0) / fw;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}

/*!
 * \brief
 * \par Function Description
 */
static void
remove_page_weak_reference (Page *page, void *geometry, GschemPageView *view)
{
  typedef void (*NotifyFunction) (void*,void*);

  g_return_if_fail (page != NULL);
  g_return_if_fail (view != NULL);

  geda_page_weak_unref (page, (NotifyFunction) page_deleted, view);
}

/*!
 * \brief
 * \par Function Description
 */
static void page_deleted (Page *page, GschemPageView *view)
{
  g_return_if_fail (page != NULL);
  g_return_if_fail (view != NULL);
  g_return_if_fail (view->geometry_table != NULL);

  g_hash_table_remove (view->geometry_table, page);
}

/*!
 * \brief Signal handler for setting the scroll adjustments
 * \par Function Description
 *  Sent from the GtkScrolledWindow to set the adjustments for the
 *  corresponding scroll bars.

static void
set_scroll_adjustments (GschemPageView *view, GtkAdjustment *hadjustment, GtkAdjustment *vadjustment)
{
  gschem_page_view_set_hadjustment (view, hadjustment);
  gschem_page_view_set_vadjustment (view, vadjustment);
}
 */

/*! \brief Signal handler for a vertical scroll adjustment change
 */
static void
vadjustment_value_changed (GtkAdjustment *vadjustment, GschemPageView *view)
{
  if (vadjustment != NULL) {

    g_return_if_fail (view != NULL);

    if (view->vadjustment != NULL) {

      g_return_if_fail (view->vadjustment == vadjustment);

      GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

      if (geometry != NULL) {

        int current_bottom;
        int new_bottom;

        current_bottom = geometry->viewport_bottom;
        new_bottom = geometry->world_bottom - (int) vadjustment->value;

        geometry->viewport_bottom = new_bottom;
        geometry->viewport_top = geometry->viewport_top - (current_bottom - new_bottom);

        gschem_page_view_invalidate_all (view);
      }
    }
  }
  else {
    BUG_MSG("vadjustment == NULL");
  }
}

/*!
 * \brief Transform WORLD coordinates to SCREEN coordinates
 * \par Function Description
 * \sa gschem_page_geometry_pix_x gschem_page_geometry_pix_y
 */
void
gschem_page_view_WORLDtoSCREEN (GschemPageView *view, int x, int y, int *px, int *py)
{
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_if_fail (geometry != NULL);

  *px = gschem_page_geometry_pix_x (geometry, x);
  *py = gschem_page_geometry_pix_y (geometry, y);
}

/*!
 * \brief Zoom the view to the extents of a set of objects
 * \par Function Description
 *  By providing a NULL for the objects parameter, this function will zoom to
 *  the extents of all objects in the drawing.
 *
 *  \param [in,out] view    This GschemPageView
 *  \param [in]     objects The list of objects to compute extents, or NULL
 */
void
gschem_page_view_zoom_extents (GschemPageView *view, const GList *objects)
{
  GschemPageGeometry *geometry = NULL;
  const GList *temp = objects;

  g_return_if_fail (view != NULL);

  geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  if (temp == NULL) {
    temp = geda_struct_page_get_objects (gschem_page_view_get_page (view));
  }

  gschem_page_geometry_zoom_extents (geometry, temp, I_PAN_IGNORE_BORDERS);

  /* Trigger a motion event to update the objects being drawn */
  //x_event_faked_motion (view, NULL);

  g_signal_emit_by_name (view, "update-grid-info");
  gschem_page_view_update_scroll_adjustments (view);
  gschem_page_view_invalidate_all (view);
}

/*!
 * \brief Zoom in on a single text object
 * \par Function Description
 * \param [in] view      This GschemPageView
 * \param [in] object    The text object
 */
void gschem_page_view_zoom_text (GschemPageView *view, GedaObject *object)
{
  int x[2];
  int y[2];

  g_return_if_fail (view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);

  g_return_if_fail (geometry != NULL);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->page != NULL);
  g_return_if_fail (object->page->toplevel != NULL);
  g_return_if_fail (object->text != NULL);

  if (geda_object_get_bounds (object, &x[0], &y[0], &x[1], &y[1])) {

    int viewport_center_x, viewport_center_y, viewport_width, viewport_height;
    double k;

    /* Here we are trying to make the text screen height to be about */
    /* 50 pixels high, perhaps a future enhancement will be to make */
    /* this number configurable */
    viewport_center_x = (x[1] + x[0]) / 2;
    viewport_center_y = (y[1] + y[0]) / 2;
    k = ((y[1] - y[0]) / 50);
    viewport_height = geometry->screen_height * k;
    viewport_width  = geometry->screen_width  * k;

    gschem_page_geometry_set_values (geometry,
                                     geometry->screen_width,
                                     geometry->screen_height,
                                     viewport_center_x - viewport_width / 2,
                                     viewport_center_y - viewport_height / 2,
                                     viewport_center_x + viewport_width / 2,
                                     viewport_center_y + viewport_height / 2);

    gschem_page_view_invalidate_all (view);
  }
}

/** @} endgroup Gschem-Page-View */
