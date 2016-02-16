/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_accel_label.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Code based on GTK 2.14.5 gtk/gtkaccellabel.h (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GedaAccelLabel: GtkLabel with accelerator monitoring facilities.
 * Copyright (C) 1998 Tim Janik
 *
 * Modified by the GTK+ Team and others 1997-2001.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 * Adapted for gEDA by Peter Clifton <peter@clifton-electronics.co.uk>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */

/* 03/13/2014 WEH Renamed Gschem->Geda through-out */

#ifndef __GEDA_ACCEL_LABEL_H__
#define __GEDA_ACCEL_LABEL_H__

#define GEDA_TYPE_ACCEL_LABEL            (geda_accel_label_get_type ())
#define GEDA_ACCEL_LABEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_ACCEL_LABEL, GedaAccelLabel))
#define GEDA_ACCEL_LABEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_ACCEL_LABEL, GedaAccelLabelClass))
#define GEDA_IS_ACCEL_LABEL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_ACCEL_LABEL))
#define GEDA_IS_ACCEL_LABEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_ACCEL_LABEL))
#define GEDA_ACCEL_LABEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_ACCEL_LABEL, GedaAccelLabelClass))

typedef struct _GedaAccelLabel       GedaAccelLabel;
typedef struct _GedaAccelLabelClass  GedaAccelLabelClass;

struct _GedaAccelLabel
{
  GtkAccelLabel label;

  unsigned int   accel_padding;
  char          *accel_string;
  guint16        accel_string_width;
};

struct _GedaAccelLabelClass
{
  GtkAccelLabelClass  parent_class;
};


#ifdef __cplusplus
extern "C" {
#endif

GedaType      geda_accel_label_get_type          (void) GEDA_CONST;
GtkWidget*    geda_accel_label_new               (const char     *string);
unsigned int  geda_accel_label_get_accel_width   (GedaAccelLabel *accel_label);
void          geda_accel_label_set_accel_string  (GedaAccelLabel *accel_label,
                                                  const char     *accel_string);
bool          geda_accel_label_refetch           (GedaAccelLabel *accel_label);

/* private */
char *_geda_accel_label_class_get_accelerator_label (GedaAccelLabelClass *class,
                                                     unsigned int         accelerator_key,
                                                     GdkModifierType      accelerator_mods);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_ACCEL_LABEL_H__ */
