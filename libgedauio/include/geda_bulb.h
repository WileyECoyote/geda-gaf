/* gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * Date: September 22, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_BULB_H__
#define __GEDA_BULB_H__

G_BEGIN_DECLS

#include <gtk/gtkcheckbutton.h>

/*! \class GedaBulb geda_bulb.h "geda_bulb.h"
 *  \brief A toggle type button widget
 *  \par
 *  A GedaBulb a button widget resembling a light bulb used for multiple
 *  choices options.
 */

#define GEDA_TYPE_BULB            (geda_bulb_get_type ())
#define GEDA_BULB(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_BULB, GedaBulb))
#define GEDA_BULB_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_BULB, GedaBulbClass))
#define GEDA_IS_BULB(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_BULB))
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


unsigned
int        geda_bulb_get_type                         (void) G_GNUC_CONST;

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

G_END_DECLS

#endif /* __GEDA_BULB_H__ */
