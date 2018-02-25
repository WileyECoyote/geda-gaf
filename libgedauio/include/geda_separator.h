/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_separator.h
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
 * Date: Apirl 7th, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_SEPARATOR_H__
#define __GEDA_SEPARATOR_H__

#define GEDA_TYPE_SEPARATOR            (geda_separator_get_type ())
#define GEDA_SEPARATOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_SEPARATOR, GedaSeparator))
#define GEDA_SEPARATOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_SEPARATOR, GedaSeparatorClass))
#define GEDA_IS_SEPARATOR(obj)         (is_a_geda_separator((GedaSeparator*)(obj)))
#define GEDA_IS_SEPARATOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_SEPARATOR))
#define GEDA_SEPARATOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_SEPARATOR, GedaSeparatorClass))

typedef struct _GedaSeparator       GedaSeparator;
typedef struct _GedaSeparatorClass  GedaSeparatorClass;

struct _GedaSeparator
{
  GtkWidget widget;
  int       orientation;
};

struct _GedaSeparatorClass
{
  GtkWidgetClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_separator_get_type (void) GEDA_CONST;
bool       is_a_geda_separator     (GedaSeparator *separator);

GtkWidget *geda_separator_new      (int orientation);
GtkWidget *geda_hseparator_new     (void);
GtkWidget *geda_vseparator_new     (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_SEPARATOR_H__ */
