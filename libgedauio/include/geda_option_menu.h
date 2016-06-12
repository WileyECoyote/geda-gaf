/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 */

#ifndef __GEDA_OPTION_MENU_H__
#define __GEDA_OPTION_MENU_H__

#define GEDA_TYPE_OPTION_MENU            (geda_option_menu_get_type ())
#define GEDA_OPTION_MENU(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_OPTION_MENU, GedaOptionMenu))
#define GEDA_OPTION_MENU_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_OPTION_MENU, GedaOptionMenuClass))
#define GEDA_IS_OPTION_MENU(obj)         (is_a_geda_option_menu((GedaOptionMenu*)obj))
#define GEDA_IS_OPTION_MENU_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_OPTION_MENU))
#define GEDA_OPTION_MENU_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_OPTION_MENU, GedaOptionMenuClass))

typedef struct _GedaOptionMenu       GedaOptionMenu;
typedef struct _GedaOptionMenuClass  GedaOptionMenuClass;

struct _GedaOptionMenu
{
  GtkButton button;
  GedaType  instance_type;

  GtkWidget *menu;
  GtkWidget *menu_item;

  uint16 width;
  uint16 height;
};

struct _GedaOptionMenuClass
{
  GtkButtonClass parent_class;

  void (*changed) (GedaOptionMenu *option_menu);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_option_menu_get_type    (void) GEDA_CONST;
bool       is_a_geda_option_menu        (GedaOptionMenu *option_menu);

GtkWidget *geda_option_menu_new         (void);
GtkWidget *geda_option_menu_get_menu    (GedaOptionMenu *option_menu);
void       geda_option_menu_set_menu    (GedaOptionMenu *option_menu,
                                         GtkWidget      *menu);
void       geda_option_menu_remove_menu (GedaOptionMenu *option_menu);
int        geda_option_menu_get_history (GedaOptionMenu *option_menu);
void       geda_option_menu_set_history (GedaOptionMenu *option_menu,
                                         unsigned int    index);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_OPTION_MENU_H__ */
