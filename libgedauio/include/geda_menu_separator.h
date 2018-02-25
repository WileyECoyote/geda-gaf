/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_separator.h
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
/*! \class GedaMenuSeparator geda_menu_separator.h "libgedauio/geda_menu_separator.h"
 *  \brief Class for GedaMenuSeparator Objects.
 *
 *  GedaMenuSeparator is a derivative of the GedaMenuItem class.
 */

#ifndef __GEDA_MENU_SEPARATOR_H__
#define __GEDA_MENU_SEPARATOR_H__

#ifdef __cplusplus
extern "C" {
#endif

#define GEDA_TYPE_MENU_SEPARATOR            (geda_menu_separator_get_type ())
#define GEDA_MENU_SEPARATOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU_SEPARATOR, GedaMenuSeparator))
#define GEDA_MENU_SEPARATOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_MENU_SEPARATOR, GedaMenuSeparatorClass))
#define GEDA_IS_MENU_SEPERATOR(obj)         (is_a_geda_menu_separator((GedaMenuSeparator*)(obj)))
#define GEDA_IS_MENU_SEPERATOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_MENU_SEPARATOR))
#define GEDA_MENU_SEPARATOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_MENU_SEPARATOR, GedaMenuSeparatorClass))

typedef struct _GedaMenuSeparator       GedaMenuSeparator;
typedef struct _GedaMenuSeparatorClass  GedaMenuSeparatorClass;

struct _GedaMenuSeparator
{
  GedaMenuItem menu_item;
};

struct _GedaMenuSeparatorClass
{
  GedaMenuItemClass parent_class;
};


GedaType   geda_menu_separator_get_type     (void) GEDA_CONST;
bool       is_a_geda_menu_separator         (GedaMenuSeparator *menu_separator);

GtkWidget *geda_menu_separator_new          (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_SEPARATOR_H__ */
