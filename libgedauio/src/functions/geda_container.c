#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <glib.h>

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>
#include "../../include/geda_gtk_compat.h"

typedef struct _CompareInfo CompareInfo;

struct _CompareInfo
{
  GtkContainer *container;
  int x;
  int y;
  bool reverse;
};

/* Get coordinates of @widget's allocation with respect to
 * allocation of @container.
 */
static bool
get_allocation_coords (GtkContainer  *container,
                       GtkWidget     *widget,
                       GdkRectangle  *allocation)
{
  *allocation = widget->allocation;

  return gtk_widget_translate_coordinates (widget, GTK_WIDGET (container),
                       0, 0, &allocation->x, &allocation->y);
}

/*!
 * \internal Look for a child in children that is intermediate
 *  between the focus widget and container. This widget, if it
 *  exists, acts as the starting widget for focus navigation.
 */
static GtkWidget*
find_old_focus (GtkContainer *container, GList *children)
{
  GList *tmp_list = children;

  while (tmp_list) {

    GtkWidget *child = tmp_list->data;
    GtkWidget *widget = child;

    while (widget && widget != (GtkWidget*)container) {

      GtkWidget *parent = geda_get_widget_parent(widget);

      if (parent && ((GtkContainer *)parent)->focus_child != widget)
        goto next;

      widget = parent;
    }

    return child;

    next:
    tmp_list = tmp_list->next;
  }

  return NULL;
}

static int
left_right_compare (const void *a, const void *b, void *data)
{
  GdkRectangle allocation1;
  GdkRectangle allocation2;
  CompareInfo *compare = data;
  int x1, x2;

  get_allocation_coords (compare->container, (GtkWidget*)a, &allocation1);
  get_allocation_coords (compare->container, (GtkWidget*)b, &allocation2);

  x1 = allocation1.x + allocation1.width / 2;
  x2 = allocation2.x + allocation2.width / 2;

  if (x1 == x2) {

    int y1 = abs (allocation1.y + allocation1.height / 2 - compare->y);
    int y2 = abs (allocation2.y + allocation2.height / 2 - compare->y);

    if (compare->reverse)
      return (y1 < y2) ? 1 : ((y1 == y2) ? 0 : -1);
    else
      return (y1 < y2) ? -1 : ((y1 == y2) ? 0 : 1);
  }
  else {
    return (x1 < x2) ? -1 : 1;
  }
}

static bool
old_focus_coords (GtkContainer *container, GdkRectangle *old_focus_rect)
{
  GtkWidget *widget   = GTK_WIDGET (container);
  GtkWidget *toplevel = gtk_widget_get_toplevel (widget);

  if (GTK_IS_WINDOW (toplevel) && GTK_WINDOW (toplevel)->focus_widget) {

      GtkWidget *old_focus = GTK_WINDOW (toplevel)->focus_widget;

      return get_allocation_coords (container, old_focus, old_focus_rect);
  }
  else {
    return FALSE;
  }
}

static GList*
geda_container_focus_sort_left_right (GtkContainer     *container,
                                      GList            *children,
                                      GtkDirectionType  direction,
                                      GtkWidget        *old_focus)
{
  CompareInfo compare;
  GList *tmp_list;
  GdkRectangle old_allocation;

  compare.container = container;
  compare.reverse = (direction == GTK_DIR_LEFT);

  if (!old_focus) {
    old_focus = find_old_focus (container, children);
  }

  if (old_focus && get_allocation_coords (container, old_focus, &old_allocation))
  {
    int compare_y1;
    int compare_y2;
    int compare_x;

    /* Delete widgets from list that don't match minimum criteria */

    compare_y1 = old_allocation.y;
    compare_y2 = old_allocation.y + old_allocation.height;

    if (direction == GTK_DIR_LEFT) {
      compare_x = old_allocation.x;
    }
    else {
      compare_x = old_allocation.x + old_allocation.width;
    }

    tmp_list = children;

    while (tmp_list) {

      GtkWidget *child = tmp_list->data;
      GList     *next  = tmp_list->next;

      GdkRectangle child_allocation;

      if (child != old_focus) {

        if (get_allocation_coords (container, child, &child_allocation)) {

          int child_y1, child_y2;

          child_y1 = child_allocation.y;
          child_y2 = child_allocation.y + child_allocation.height;

          if ((child_y2 <= compare_y1 || child_y1 >= compare_y2) /* No vertical overlap */ ||
            (direction == GTK_DIR_RIGHT && child_allocation.x + child_allocation.width < compare_x) || /* Not to left */
            (direction == GTK_DIR_LEFT && child_allocation.x > compare_x)) /* Not to right */
          {
            children = g_list_delete_link (children, tmp_list);
          }
        }
        else {
          children = g_list_delete_link (children, tmp_list);
        }
      }

      tmp_list = next;
    }

    compare.y = (compare_y1 + compare_y2) / 2;
    compare.x = old_allocation.x + old_allocation.width / 2;
  }
  else {

    GtkWidget     *widget;
    GtkAllocation *allocation;
    GdkRectangle   old_focus_rect;

    /* No old focus widget, need to figure out starting x,y some other way */
    widget     = (GtkWidget*)container;
    allocation = geda_get_widget_allocation (widget);

    if (old_focus_coords (container, &old_focus_rect)) {

      compare.y = old_focus_rect.y + old_focus_rect.height / 2;
    }
    else {

      int half_height;

      half_height = allocation->height >> 1; /* Divide by 2 */

      if (!gtk_widget_get_has_window (widget)) {
        compare.y = allocation->y + half_height;
      }
      else {
        compare.y = half_height;
      }
    }

    if (!gtk_widget_get_has_window (widget)) {
      compare.x = (direction == GTK_DIR_RIGHT) ? allocation->x : allocation->x + allocation->width;
    }
    else {
      compare.x = (direction == GTK_DIR_RIGHT) ? 0 : allocation->width;
    }
  }

  children = g_list_sort_with_data (children, left_right_compare, &compare);

  if (compare.reverse) {
    children = g_list_reverse (children);
  }

  return children;
}

static int
up_down_compare (const void *a, const void *b, void *data)
{
  GdkRectangle allocation1;
  GdkRectangle allocation2;
  CompareInfo *compare = data;
  int y1, y2;

  get_allocation_coords (compare->container, (GtkWidget *)a, &allocation1);
  get_allocation_coords (compare->container, (GtkWidget *)b, &allocation2);

  y1 = allocation1.y + allocation1.height / 2;
  y2 = allocation2.y + allocation2.height / 2;

  if (y1 == y2) {

    int x1 = abs (allocation1.x + allocation1.width / 2 - compare->x);
    int x2 = abs (allocation2.x + allocation2.width / 2 - compare->x);

    if (compare->reverse) {
      return (x1 < x2) ? 1 : ((x1 == x2) ? 0 : -1);
    }
    else {
      return (x1 < x2) ? -1 : ((x1 == x2) ? 0 : 1);
    }
  }
  else {
    return (y1 < y2) ? -1 : 1;
  }
}

static GList*
geda_container_focus_sort_up_down (GtkContainer     *container,
                                   GList            *children,
                                   GtkDirectionType  direction,
                                   GtkWidget        *old_focus)
{
  CompareInfo  compare;
  GList       *tmp_list;
  GdkRectangle old_allocation;

  compare.container = container;
  compare.reverse = (direction == GTK_DIR_UP);

  if (!old_focus) {
    old_focus = find_old_focus (container, children);
  }

  if (old_focus && get_allocation_coords (container, old_focus, &old_allocation))
  {
    int compare_x1;
    int compare_x2;
    int compare_y;

    /* Delete widgets from list that don't match minimum criteria */

    compare_x1 = old_allocation.x;
    compare_x2 = old_allocation.x + old_allocation.width;

    if (direction == GTK_DIR_UP) {
      compare_y = old_allocation.y;
    }
    else {
      compare_y = old_allocation.y + old_allocation.height;
    }

    tmp_list = children;
    while (tmp_list) {

      GtkWidget *child = tmp_list->data;
      GList     *next  = tmp_list->next;

      GdkRectangle child_allocation;

      if (child != old_focus) {

        if (get_allocation_coords (container, child, &child_allocation)) {

          int child_x1, child_x2;

          child_x1 = child_allocation.x;
          child_x2 = child_allocation.x + child_allocation.width;

          if ((child_x2 <= compare_x1 || child_x1 >= compare_x2) /* No horizontal overlap */ ||
            (direction == GTK_DIR_DOWN && child_allocation.y + child_allocation.height < compare_y) || /* Not below */
            (direction == GTK_DIR_UP && child_allocation.y > compare_y)) /* Not above */
          {
            children = g_list_delete_link (children, tmp_list);
          }
        }
        else
          children = g_list_delete_link (children, tmp_list);
      }

      tmp_list = next;
    }

    compare.x = (compare_x1 + compare_x2) / 2;
    compare.y = old_allocation.y + old_allocation.height / 2;
  }
  else {

    GtkWidget     *widget;
    GtkAllocation *allocation;
    GdkRectangle   old_focus_rect;

    /* No old focus widget, need to figure out starting x,y some other way */
    widget     = (GtkWidget*)container;
    allocation = geda_get_widget_allocation (widget);

    if (old_focus_coords (container, &old_focus_rect)) {

      compare.x = old_focus_rect.x + old_focus_rect.width / 2;
    }
    else {

      int half_width;

      half_width = allocation->width >> 1; /* Divide by 2 */

      if (!gtk_widget_get_has_window (widget)) {
        compare.x = allocation->x + half_width;
      }
      else {
        compare.x = half_width;
      }
    }

    if (!gtk_widget_get_has_window (widget)) {
      compare.y = (direction == GTK_DIR_DOWN) ? allocation->y : allocation->y + allocation->height;
    }
    else {
      compare.y = (direction == GTK_DIR_DOWN) ? 0 : + allocation->height;
    }
  }

  children = g_list_sort_with_data (children, up_down_compare, &compare);

  if (compare.reverse) {
    children = g_list_reverse (children);
  }

  return children;
}

static int
tab_compare (const void*a, const void*b, void*data)
{
  const GtkWidget *child1 = a;
  const GtkWidget *child2 = b;
  GtkTextDirection text_direction = GPOINTER_TO_INT (data);

  int y1 = child1->allocation.y + child1->allocation.height / 2;
  int y2 = child2->allocation.y + child2->allocation.height / 2;

  if (y1 == y2) {

    int x1 = child1->allocation.x + child1->allocation.width / 2;
    int x2 = child2->allocation.x + child2->allocation.width / 2;

    if (text_direction == GTK_TEXT_DIR_RTL) {
      return (x1 < x2) ? 1 : ((x1 == x2) ? 0 : -1);
    }
    else {
      return (x1 < x2) ? -1 : ((x1 == x2) ? 0 : 1);
    }
  }

  return (y1 < y2) ? -1 : 1;
}

static GList*
geda_container_focus_sort_tab (GtkContainer     *container,
                               GList            *children,
                               GtkDirectionType  direction,
                               GtkWidget        *old_focus)
{
  GtkTextDirection text_direction;

  text_direction = gtk_widget_get_direction (GTK_WIDGET (container));

  children = g_list_sort_with_data (children, tab_compare, INT_TO_POINTER (text_direction));

  /* if going backwards then reverse the order of the children. */
  if (direction == GTK_DIR_TAB_BACKWARD) {
    children = g_list_reverse (children);
  }

  return children;
}

/*!
 * \brief Sort a GtkContainer
 * \par Function Description
 * Sorts \a children in the correct order for focusing with
 * direction type \a direction. \a children do not have to be
 * direct children
 *
 * \param[in] container  Pointer to a GtkContainer
 * \param[in] children   List of descendents of \a container
 * \param[in] direction: focus direction
 * \param[in] old_focus: widget to use for the starting position, or %NULL
 *                       to determine this automatically.
 *
 * \return A copy of \a children, sorted in correct focusing order, with
 *         children that are not suitable for focusing in this direction
 *         removed.
 *
 * \note old_focus is not used for GTK_DIR_TAB_*, which is the only \a direction
 *       currently used, so perhaps this argument should be removed
 */
GList*
geda_container_focus_sort (GtkContainer     *container,
                           GList            *children,
                           GtkDirectionType  direction,
                           GtkWidget        *old_focus)
{
  GList *visible_children = NULL;
  GList *iter;

  iter = g_list_last(children);

  while (iter) {
    if (gtk_widget_get_realized (iter->data)) {
      visible_children = g_list_prepend (visible_children, iter->data);
    }
    iter = iter->prev;
  }

  switch (direction) {
    case GTK_DIR_TAB_FORWARD:
    case GTK_DIR_TAB_BACKWARD:
      return geda_container_focus_sort_tab (container, visible_children, direction, old_focus);

    case GTK_DIR_UP:
    case GTK_DIR_DOWN:
      return geda_container_focus_sort_up_down (container, visible_children, direction, old_focus);

    case GTK_DIR_LEFT:
    case GTK_DIR_RIGHT:
      return geda_container_focus_sort_left_right (container, visible_children, direction, old_focus);
  }

  return NULL;
}