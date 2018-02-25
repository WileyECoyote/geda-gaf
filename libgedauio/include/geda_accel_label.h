/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_accel_label.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 1998-2018 gEDA Contributors (see ChangeLog for details)
 *
 * Code originally based on GTK 2.14.5 gtk/gtkaccellabel.h (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GedaAccelLabel: GtkLabel with accelerator monitoring facilities.
 * Copyright (C) 1998 Tim Janik
 *
 * Adapted for gEDA by Peter Clifton <peter@clifton-electronics.co.uk>
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
 */

/* 03/13/2014 WEH Renamed Gschem->Geda through-out
 * 06/19/2016 WEH Derive from GedaLabel
 */

#ifndef __GEDA_ACCEL_LABEL_H__
#define __GEDA_ACCEL_LABEL_H__

#include "geda_label.h"

#define GEDA_TYPE_ACCEL_LABEL            (geda_accel_label_get_type ())
#define GEDA_ACCEL_LABEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_ACCEL_LABEL, GedaAccelLabel))
#define GEDA_ACCEL_LABEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_ACCEL_LABEL, GedaAccelLabelClass))
#define GEDA_IS_ACCEL_LABEL(obj)         (is_a_geda_accel_label((GedaAccelLabel*)(obj)))
#define GEDA_IS_ACCEL_LABEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_ACCEL_LABEL))
#define GEDA_ACCEL_LABEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_ACCEL_LABEL, GedaAccelLabelClass))

typedef struct _GedaAccelLabel       GedaAccelLabel;
typedef struct _GedaAccelLabelClass  GedaAccelLabelClass;

struct _GedaAccelLabel
{
  GedaLabel      label;

  GClosure      *accel_closure;
  GtkAccelGroup *accel_group;        /* set by set_accel_closure() */
  GtkWidget     *accel_widget;

  unsigned int   accel_padding;
  char          *accel_string;
  uint16         accel_string_width;
};

struct _GedaAccelLabelClass
{
  GedaLabelClass parent_class;
};


#ifdef __cplusplus
extern "C" {
#endif

GedaType      geda_accel_label_get_type          (void) GEDA_CONST;
bool          is_a_geda_accel_label              (GedaAccelLabel *accel_label);

GtkWidget    *geda_accel_label_new               (const char     *string);

GtkWidget    *geda_accel_label_get_accel_widget  (GedaAccelLabel *accel_label);
void          geda_accel_label_set_accel_widget  (GedaAccelLabel *accel_label,
                                                  GtkWidget      *accel_widget);

unsigned int  geda_accel_label_get_accel_width   (GedaAccelLabel *accel_label);

const char   *geda_accel_label_get_accel_string  (GedaAccelLabel *accel_label);
void          geda_accel_label_set_accel_string  (GedaAccelLabel *accel_label,
                                                  const char     *accel_string);

bool          geda_accel_label_refetch           (GedaAccelLabel *accel_label);


void          geda_accel_label_set_accel_closure (GedaAccelLabel *accel_label,
                                                  GClosure       *accel_closure);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_ACCEL_LABEL_H__ */
