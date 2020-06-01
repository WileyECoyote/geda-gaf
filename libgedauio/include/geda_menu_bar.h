/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_bar.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 * Date: June 16, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
/*! \class GedaMenuBar geda_menu_bar.h "libgedauio/geda_menu_bar.h"
 *  \brief Class for GedaMenuBar Objects.
 *
 *  GedaMenuBar is a derivative of the GedaMenuShell class.
 */
#ifndef __GEDA_MENU_BAR_H__
#define __GEDA_MENU_BAR_H__

#include "geda_menu_shell.h"
#include "geda_menu_enum.h"

#define GEDA_TYPE_MENU_BAR            (geda_menu_bar_get_type ())
#define GEDA_MENU_BAR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU_BAR, GedaMenuBar))
#define GEDA_MENU_BAR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_MENU_BAR, GedaMenuBarClass))
#define GEDA_IS_MENU_BAR(obj)         (is_a_geda_menu_bar((GedaMenuBar*)(obj)))
#define GEDA_IS_MENU_BAR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_MENU_BAR))
#define GEDA_MENU_BAR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_MENU_BAR, GedaMenuBarClass))

typedef struct _GedaMenuBar         GedaMenuBar;
typedef struct _GedaMenuBarPrivate  GedaMenuBarPrivate;
typedef struct _GedaMenuBarClass    GedaMenuBarClass;

struct _GedaMenuBar
{
  GedaMenuShell menu_shell;

  /*< private >*/
  GedaMenuBarPrivate *priv;
};

struct _GedaMenuBarClass
{
  GedaMenuShellClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType         geda_menu_bar_get_type                 (void) GEDA_CONST;
bool             is_a_geda_menu_bar                     (GedaMenuBar      *menu_bar);

GtkWidget       *geda_menu_bar_new                      (void);

void             geda_menu_bar_cycle_focus              (GedaMenuBar       *menubar,
                                                         GtkDirectionType   dir);

PackDirection    geda_menu_bar_get_pack_direction       (GedaMenuBar       *menubar);

void             geda_menu_bar_set_pack_direction       (GedaMenuBar       *menubar,
                                                         PackDirection      pack_dir);

PackDirection    geda_menu_bar_get_child_pack_direction (GedaMenuBar       *menubar);

void             geda_menu_bar_set_child_pack_direction (GedaMenuBar       *menubar,
                                                         PackDirection      child_pack_dir);
GList           *geda_menu_bar_get_viewable_menu_bars   (GtkWindow         *window);

void             geda_menu_bar_hide_mnemonics           (GedaMenuBar       *menubar);
void             geda_menu_bar_show_mnemonics           (GedaMenuBar       *menubar);

#define geda_menu_bar_append(menu,child)     geda_menu_shell_append  ((GedaMenuShell*)(menu),(child))
#define geda_menu_bar_prepend(menu,child)    geda_menu_shell_prepend ((GedaMenuShell*)(menu),(child))
#define geda_menu_bar_insert(menu,child,pos) geda_menu_shell_insert ((GedaMenuShell*)(menu),(child),(pos))

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_BAR_H__ */
