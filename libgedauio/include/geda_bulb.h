/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_bulb.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2018 Wiley Edward Hill
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
 * Date: September 22, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_BULB_H__
#define __GEDA_BULB_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtkcheckbutton.h>

#else

#include <gtk/gtk.h>

#endif

/*! \class GedaBulb geda_bulb.h "geda_bulb.h"
 *  \brief A toggle type button widget
 *  \par
 *  A GedaBulb a button widget resembling a light bulb used for multiple
 *  choices options.
 */

#define GEDA_TYPE_BULB            (geda_bulb_get_type ())
#define GEDA_BULB(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_BULB, GedaBulb))
#define GEDA_BULB_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_BULB, GedaBulbClass))
#define GEDA_IS_BULB(obj)         (is_a_geda_bulb((GedaBulb*)(obj)))
#define GEDA_IS_BULB_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_BULB))
#define GEDA_BULB_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_BULB, GedaBulbClass))

typedef struct _GedaBulb       GedaBulb;
typedef struct _GedaBulbClass  GedaBulbClass;

struct _GedaBulb
{
  GtkCheckButton check_button;

  GSList        *group;

  int show_butt;

  /* Private */
  int height;
  int width;
};

struct _GedaBulbClass
{
  GtkCheckButtonClass parent_class;

  /* Signals */
  void (*group_changed) (GedaBulb *bulb);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_bulb_get_type                         (void) GEDA_CONST;
bool       is_a_geda_bulb                             (GedaBulb      *bulb);

GtkWidget *geda_bulb_new                              (GSList        *group);
GtkWidget *geda_bulb_new_visible                      (GSList        *group);

GtkWidget *geda_bulb_new_with_label                   (GSList        *group,
                                                       const char    *label);
GtkWidget *geda_bulb_new_visible_with_label           (GSList        *group,
                                                       const char    *label);
GtkWidget *geda_bulb_new_with_mnemonic                (GSList        *group,
                                                       const char    *label);
GtkWidget *geda_bulb_new_visible_with_mnemonic        (GSList        *group,
                                                       const char    *label);
GtkWidget *geda_bulb_new_from_widget                  (GtkWidget     *group_member,
                                                       bool           visible);
GtkWidget *geda_bulb_new_with_label_from_widget       (GtkWidget     *group_member,
                                                       const char    *label,
                                                       bool           visible);
GtkWidget *geda_bulb_new_with_mnemonic_from_widget    (GtkWidget     *group_member,
                                                       const char    *label,
                                                       bool           visible);
GSList    *geda_bulb_get_group                        (GtkWidget     *bulb);
void       geda_bulb_set_group                        (GtkWidget     *bulb,
                                                       GSList        *group);
void       geda_bulb_join_group                       (GtkWidget     *bulb,
                                                       GtkWidget     *group_source);
int        geda_bulb_group_get_active_index           (GSList        *group);
void       geda_bulb_group_set_active_index           (GSList        *group,
                                                       int            which_bulb);
void       geda_bulb_group_quietly_set_active         (GSList        *group,
                                                       int            which_bulb);
bool       geda_bulb_get_show_button                  (GtkWidget     *bulb);
void       geda_bulb_set_show_button                  (GtkWidget     *bulb,
                                                       bool           show_button);

void       geda_bulb_set_group_sensitive              (GSList        *group,
                                                       bool           sensitive);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_BULB_H__ */
