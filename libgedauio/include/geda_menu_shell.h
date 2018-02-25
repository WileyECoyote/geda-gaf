/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_shell.h
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
/*! \class GedaMenuShell geda_menu_shell.h "libgedauio/geda_menu_shell.h"
 *  \brief Class for GedaMenuShell Objects.
 *
 *  GedaMenuShell is a derivative of the GtkContainer class.
 */

#ifndef __GEDA_MENU_SHELL_H__
#define __GEDA_MENU_SHELL_H__

#include "geda_menu_enum.h"

#define GEDA_TYPE_MENU_SHELL            (geda_menu_shell_get_type ())
#define GEDA_MENU_SHELL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU_SHELL, GedaMenuShell))
#define GEDA_MENU_SHELL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_MENU_SHELL, GedaMenuShellClass))
#define GEDA_IS_MENU_SHELL(obj)         (is_a_geda_menu_shell((GedaMenuShell*)(obj)))
#define GEDA_IS_MENU_SHELL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_MENU_SHELL))
#define GEDA_MENU_SHELL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_MENU_SHELL, GedaMenuShellClass))

typedef struct _GedaMenuShell       GedaMenuShell;
typedef struct _GedaMenuShellClass  GedaMenuShellClass;
typedef struct _GedaMenuShellPriv   GedaMenuShellPriv;

struct _GedaMenuShell
{
  GtkContainer container;

  GList       *children;
  GtkWidget   *active_menu_item;
  GtkWidget   *parent_menu_shell;

  unsigned int button;
  uint32       activate_time;

  unsigned int active        : 1;
  unsigned int have_grab     : 1;
  unsigned int have_xgrab    : 1;
  unsigned int ignore_leave  : 1; /* unused */
  unsigned int menu_flag     : 1;    /* unused */
  unsigned int ignore_enter  : 1;
  unsigned int keyboard_mode : 1;

  /*< private >*/
  GedaMenuShellPriv *priv;
};

struct _GedaMenuShellClass
{
  GtkContainerClass parent_class;

  unsigned int submenu_placement : 1;

  void     (*activate_current) (GedaMenuShell *menu_shell,
                                bool           force_hide);
  void     (*cancel)           (GedaMenuShell *menu_shell);
  void     (*deactivate)       (GedaMenuShell *menu_shell);
  int      (*get_popup_delay)  (GedaMenuShell *menu_shell);
  void     (*insert)           (GedaMenuShell *menu_shell,
                                GtkWidget     *child,
                                int            position);
  void     (*move_current)     (GedaMenuShell *menu_shell,
                                MenuDirection  direction);
  bool     (*move_selected)    (GedaMenuShell *menu_shell,
                                int            distance);

  void     (*select_item)      (GedaMenuShell *menu_shell,
                                GtkWidget     *menu_item);
  void     (*selection_done)   (GedaMenuShell *menu_shell);


};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_menu_shell_get_type          (void)          GEDA_CONST;
bool       is_a_geda_menu_shell              (GedaMenuShell *menu_shell);

void       geda_menu_shell_activate          (GedaMenuShell *menu_shell);
void       geda_menu_shell_activate_item     (GedaMenuShell *menu_shell,
                                              GtkWidget     *menu_item,
                                              bool           force_deactivate);

void       geda_menu_shell_add_mnemonic      (GedaMenuShell *menu_shell,
                                              unsigned int   keyval,
                                              GtkWidget     *target);

void       geda_menu_shell_append            (GedaMenuShell *menu_shell,
                                              GtkWidget     *child);

void       geda_menu_shell_cancel            (GedaMenuShell *menu_shell);

void       geda_menu_shell_deactivate        (GedaMenuShell *menu_shell);

void       geda_menu_shell_deselect          (GedaMenuShell *menu_shell);
const
GList     *geda_menu_shell_get_children      (GedaMenuShell *menu_shell);

bool       geda_menu_shell_get_keyboard_mode (GedaMenuShell *menu_shell);

GtkWidget *geda_menu_shell_get_parent_shell  (GedaMenuShell *menu_shell);

int        geda_menu_shell_get_popup_delay   (GedaMenuShell *menu_shell);

GtkWidget *geda_menu_shell_get_selected_item (GedaMenuShell *menu_shell);

bool       geda_menu_shell_get_take_focus    (GedaMenuShell *menu_shell);

void       geda_menu_shell_insert            (GedaMenuShell *menu_shell,
                                              GtkWidget     *child,
                                              int            position);

void       geda_menu_shell_prepend           (GedaMenuShell *menu_shell,
                                              GtkWidget     *child);

void       geda_menu_shell_remove_mnemonic   (GedaMenuShell *menu_shell,
                                              unsigned int   keyval,
                                              GtkWidget     *target);

void       geda_menu_shell_set_grab_device   (GedaMenuShell *menu_shell,
                                              GdkDevice     *device);

void       geda_menu_shell_set_keyboard_mode (GedaMenuShell *menu_shell,
                                              bool           keyboard_mode);

void       geda_menu_shell_set_take_focus    (GedaMenuShell *menu_shell,
                                              bool           take_focus);

GtkWidget *geda_menu_shell_select_first      (GedaMenuShell *menu_shell,
                                              bool           search_sensitive);
void       geda_menu_shell_select_item       (GedaMenuShell *menu_shell,
                                              GtkWidget     *menu_item);
GtkWidget *geda_menu_shell_select_last       (GedaMenuShell *menu_shell,
                                              bool           search_sensitive);

void       geda_menu_shell_update_mnemonics  (GedaMenuShell *menu_shell);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_SHELL_H__ */
