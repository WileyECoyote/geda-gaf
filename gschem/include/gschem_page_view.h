/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_page_view.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file gschem_page_view.h
 *
 * \brief A widget for viewing a schematic page
 */

/*! \class GschemPageView gschem_page_view.h "gschem_page_view.h"
 *  \brief Page View Object
 */

#define GSCHEM_TYPE_PAGE_VIEW           (gschem_page_view_get_type())
#define GSCHEM_PAGE_VIEW(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_PAGE_VIEW, GschemPageView))
#define GSCHEM_PAGE_VIEW_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_PAGE_VIEW, GschemPageViewClass))
#define GSCHEM_IS_PAGE_VIEW(obj)        (is_a_gschem_page_view((GschemPageView*)obj))
#define GSCHEM_PAGE_VIEW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_PAGE_VIEW, GschemPageViewClass))

typedef struct _GschemPageViewClass GschemPageViewClass;
typedef struct _GschemPageView GschemPageView;

struct _GschemPageViewClass
{
  GtkWindowClass parent_class;
};

struct _GschemPageView
{
  GtkWindow parent;
  GedaType  instance_type;

  GdkGC *gc;

  GtkAllocation previous_allocation;

  GtkAdjustment *hadjustment;
  GtkAdjustment *vadjustment;

  GHashTable *geometry_table;

  bool configured;

  bool doing_pan;  /* mouse pan status flag */
  int pan_x;
  int pan_y;
  int throttle;

  Page *page;
};

#ifdef __cplusplus
extern "C" {
#endif

GdkGC*
gschem_page_view_get_gc (GschemToplevel *w_current);

GtkAdjustment*
gschem_page_view_get_hadjustment (GschemPageView *view);

Page*
gschem_page_view_get_page (GschemPageView *view);

GschemPageGeometry*
gschem_page_view_get_page_geometry (GschemPageView *view);

GedaType
gschem_page_view_get_type (void);

bool
is_a_gschem_page_view (GschemPageView *view);

GtkAdjustment*
gschem_page_view_get_vadjustment (GschemPageView *view);

void
gschem_page_view_invalidate_all (GschemPageView *view);

void
gschem_page_view_invalidate_object (GschemToplevel *w_current, GedaObject *object);

void
gschem_page_view_invalidate_screen_rect (GschemToplevel *w_current, int left, int top, int right, int bottom);

void
gschem_page_view_invalidate_world_rect (GschemToplevel *w_current, int left, int top, int right, int bottom);

GschemPageView*
gschem_page_view_new ();

GschemPageView*
gschem_page_view_new_with_page (Page *page);

void
gschem_page_view_pan_general(GschemPageView *page_view, int x, int y, double relativ_zoom_factor);

void
gschem_page_view_pan(GschemPageView *page_view, int x, int y);

void
gschem_page_view_pan_mouse(GschemPageView *page_view, int diff_x, int diff_y);

void
gschem_page_view_pan_start(GschemPageView *page_view, int x, int y);

void
gschem_page_view_pan_motion (GschemPageView *view, int mousepan_gain, int x, int y);

bool
gschem_page_view_pan_end(GschemPageView *page_view);

int
gschem_page_view_SCREENabs(GschemPageView *view, int val);

void
gschem_page_view_SCREENtoWORLD (GschemPageView *view, int mx, int my, int *x, int *y);

void
gschem_page_view_set_hadjustment (GschemPageView *view, GtkAdjustment *hadjustment);

void
gschem_page_view_set_page (GschemPageView *view, Page *page);

void
gschem_page_view_set_vadjustment (GschemPageView *view, GtkAdjustment *vadjustment);

void
gschem_page_view_update_scroll_adjustments (GschemPageView *view);

int
gschem_page_view_WORLDabs (GschemPageView *view, int val);

void
gschem_page_view_WORLDtoSCREEN (GschemPageView *view, int x, int y, int *px, int *py);

void
gschem_page_view_zoom_extents (GschemPageView *view, const GList *list);

void
gschem_page_view_zoom_text (GschemPageView *view, GedaObject *object);

#ifdef __cplusplus
}
#endif /* __cplusplus */
