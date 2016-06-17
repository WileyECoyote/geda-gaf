/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
/*! \class GedaMenu geda_menu.h "libgedauio/geda_menu.h"
 *  \brief Class for GedaMenu Objects.
 *
 *  GedaMenu is a derivative of the GedaMenuShell class.
 */

#ifndef __GEDA_MENU_H__
#define __GEDA_MENU_H__

#include "geda_menu_enum.h"
#include "geda_menu_shell.h"

#define GEDA_TYPE_MENU            (geda_menu_get_type ())
#define GEDA_MENU(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU, GedaMenu))
#define GEDA_MENU_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_MENU, GedaMenuClass))
#define GEDA_IS_MENU(obj)         (is_a_geda_menu((GedaMenu*)obj))
#define GEDA_IS_MENU_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_MENU))
#define GEDA_MENU_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_MENU, GedaMenuClass))


typedef struct _GedaMenu        GedaMenu;
typedef struct _GedaMenuClass   GedaMenuClass;
typedef struct _GedaMenuPriv    GedaMenuPriv;

/*!
 * \typedef GedaMenuPositionFunc
 * A user function supplied when calling geda_menu_popup() which
 * controls the positioning of the menu when it is displayed. The
 * function sets the \a x and \a y parameters to the coordinates where
 * the menu is to be drawn. To make the menu appear on a different
 * monitor than the mouse pointer, geda_menu_set_monitor() must be
 * called.
 *
 * \param[in]  menu     GedaMenu object
 * \param[out] x        address of the int representing the horizontal
 *                      position where the menu shall be drawn.
 * \param[out] y        address of the #int representing the vertical position
 *                      where the menu shall be drawn. This is an output parameter.
 * \param[out] push_in  This parameter controls how menus placed outside the monitor
 *                      are handled. If this is set to %TRUE and part of the menu is
 *                      outside the monitor then the menu's window is pushed into the
 *                      visible area, effectively modifying the popup position. Note
 *                      that moving and possibly resizing the menu around will alter
 *                      the scroll position to keep the menu items "in place", i.e.
 *                      at the same monitor position they would have been without
 *                      resizing. In practice, this behavior is only useful for
 *                      combobox popups or option menus and cannot be used to simply
 *                      confine a menu to monitor boundaries. In that case, changing
 *                      the scroll offset is not desirable.
 * \param[in] user_data Pointer supplied in the geda_menu_popup() data parameter.
 */
typedef void (*MenuPositionFunc) (GedaMenu *menu, int  *x,
                                                  int  *y,
                                                  bool *push_in,
                                                  void *user_data);

/*!
 * \typedef MenuDetachFunc
 * A user function supplied when calling geda_menu_attach_to_widget() which
 * will be called when the menu is later detached from the widget.
 *
 * \param[in]attach_widget Widget that the menu is being detached from.
 * \param[in]menu          Pointer to #GedaMenu being detached.
 */
typedef void (*MenuDetachFunc) (GtkWidget *attach_widget, GedaMenu *menu);

struct _GedaMenu
{
  GedaMenuShell menu_shell;
  GedaType      instance_type;

  GtkAccelGroup *accel_group;
  char          *accel_path;

  MenuPositionFunc  position_func;
  void             *position_func_data;

  GtkWidget *parent_menu_item;
  GtkWidget *old_active_menu_item;

  unsigned int toggle_size;
  /* Do _not_ touch these widgets directly. We hide the reference
   * count from the toplevel to the menu, so it must be restored
   * before operating on these widgets
   */
  GtkWidget *toplevel;

  GtkWidget *tearoff_window;
  GtkWidget *tearoff_hbox;
  GtkWidget *tearoff_scrollbar;
  GtkAdjustment *tearoff_adjustment;

  GdkWindow *view_window;
  GdkWindow *bin_window;

  int scroll_offset;
  int saved_scroll_offset;
  int scroll_step;

  unsigned int timeout_id;

  /* When a submenu of this menu is popped up, motion in this
   * region is ignored
   */
  GdkRegion *navigation_region; /* unused */
  unsigned int navigation_timeout;

  unsigned int needs_destruction_ref_count : 1;
  unsigned int torn_off : 1;

  /* The tearoff is active when it is torn off and the not-torn-off
   * menu is not popped up.
   */
  unsigned int tearoff_active : 1;

  unsigned int scroll_fast : 1;

  unsigned int upper_arrow_visible  : 1;
  unsigned int lower_arrow_visible  : 1;
  unsigned int upper_arrow_prelight : 1;
  unsigned int lower_arrow_prelight : 1;

  /*< private >*/
  GedaMenuPriv *priv;
};

struct _GedaMenuClass
{
  GedaMenuShellClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_menu_get_type              (void) GEDA_CONST;
bool       is_a_geda_menu                  (GedaMenu         *menu);

GtkWidget *geda_menu_new                   (void);

GtkWidget *geda_menu_new_from_model        (GMenuModel *model);

/* A reference count is kept for a widget when it is attached to
 * a particular widget. This is typically a menu item; it may also
 * be a widget with a popup menu - for instance, the Notebook widget.
 */

void       geda_menu_attach_to_widget            (GedaMenu         *menu,
                                                  GtkWidget        *attach_widget,
                                                  MenuDetachFunc    detacher);

void        geda_menu_attach                     (GedaMenu       *menu,
                                                  GtkWidget      *child,
                                                  unsigned int    left_attach,
                                                  unsigned int    right_attach,
                                                  unsigned int    top_attach,
                                                  unsigned int    bottom_attach);

void       geda_menu_detach                      (GedaMenu         *menu);

/* Display the menu onscreen */

void       geda_menu_popup                       (GedaMenu         *menu,
                                                  GtkWidget        *parent_menu_shell,
                                                  GtkWidget        *parent_menu_item,
                                                  MenuPositionFunc  func,
                                                  void             *data,
                                                  unsigned int      button,
                                                  uint32            activate_time);

void       geda_menu_popup_for_device            (GedaMenu         *menu,
                                                  GdkDevice        *device,
                                                  GtkWidget        *parent_menu_shell,
                                                  GtkWidget        *parent_menu_item,
                                                  MenuPositionFunc  func,
                                                  void             *data,
                                                  GDestroyNotify    destroy,
                                                  unsigned int      button,
                                                  uint32            activate_time);

void       geda_menu_popdown                     (GedaMenu         *menu);

/* Position the menu according to its position function. Called
 * from gtkmenuitem.c when a menu-item changes its allocation
 */

void       geda_menu_reposition                  (GedaMenu         *menu);

/* Keep track of the last menu item selected. (For the purposes
 * of the option menu
 */

GtkWidget *geda_menu_get_active                  (GedaMenu         *menu);

void       geda_menu_set_active                  (GedaMenu         *menu,
                                                  unsigned int      index);

/* set/get the accelerator group that holds global accelerators (should
 * be added to the corresponding toplevel with gtk_window_add_accel_group().
 */

void        geda_menu_set_accel_group            (GedaMenu         *menu,
                                                  GtkAccelGroup    *accel_group);

GtkAccelGroup
           *geda_menu_get_accel_group            (GedaMenu         *menu);

void        geda_menu_set_accel_path             (GedaMenu         *menu,
                                                  const char       *accel_path);

const char *geda_menu_get_accel_path             (GedaMenu         *menu);



/* This should be dumped in favor of data set when the menu is popped
 * up - that is currently in the ItemFactory code, but should be
 * in the Menu code.
 */
GtkWidget  *geda_menu_get_attach_widget          (GedaMenu         *menu);

void        geda_menu_set_tearoff_state          (GedaMenu         *menu,
                                                  bool              torn_off);
bool        geda_menu_get_tearoff_state          (GedaMenu         *menu);

/* This sets the window manager title for the window that
 * appears when a menu is torn off
 */

void        geda_menu_set_title                  (GedaMenu         *menu,
                                                  const char       *title);
const char *geda_menu_get_title                  (GedaMenu         *menu);



void        geda_menu_set_screen                 (GedaMenu         *menu,
                                                  GdkScreen        *screen);




void        geda_menu_set_monitor                (GedaMenu        *menu,
                                                  int              monitor_num);
int         geda_menu_get_monitor                (GedaMenu        *menu);

GList      *geda_menu_get_for_attach_widget      (GtkWidget       *widget);

void        geda_menu_set_reserve_toggle_size    (GedaMenu        *menu,
                                                  bool             size);
bool        geda_menu_get_reserve_toggle_size    (GedaMenu        *menu);

GtkWidget  *geda_menu_get_parent                 (GedaMenu         *menu);

void        geda_menu_reorder_child              (GedaMenu         *menu,
                                                  GtkWidget        *child,
                                                  int               position);
GtkWidget  *geda_menu_get_toplevel               (GedaMenu         *menu);

#define geda_menu_append(menu,child)     geda_menu_shell_append  ((GedaMenuShell *)(menu),(child))
#define geda_menu_prepend(menu,child)    geda_menu_shell_prepend ((GedaMenuShell *)(menu),(child))
#define geda_menu_insert(menu,child,pos) geda_menu_shell_insert ((GedaMenuShell *)(menu),(child),(pos))

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_H__ */
