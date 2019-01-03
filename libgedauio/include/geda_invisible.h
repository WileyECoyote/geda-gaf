/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 */

#ifndef __GEDA_INVISIBLE_H__
#define __GEDA_INVISIBLE_H__

#define GEDA_TYPE_INVISIBLE            (geda_invisible_get_type ())
#define GEDA_INVISIBLE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_INVISIBLE, GedaInvisible))
#define GEDA_INVISIBLE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_INVISIBLE, GedaInvisibleClass))
#define GEDA_IS_INVISIBLE(obj)         (is_a_geda_invisible((GedaInvisible*)(obj)))
#define GEDA_IS_INVISIBLE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_INVISIBLE))
#define GEDA_INVISIBLE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_INVISIBLE, GedaInvisibleClass))

typedef struct _GedaInvisible              GedaInvisible;
typedef struct _GedaInvisibleData          GedaInvisibleData;
typedef struct _GedaInvisibleClass         GedaInvisibleClass;

struct _GedaInvisible
{
  GtkWindow window;

  /*< private >*/
  GedaInvisibleData *priv;
};

struct _GedaInvisibleClass
{
  GtkWindowClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_invisible_get_type       (void) GEDA_CONST;
bool       is_a_geda_invisible           (GedaInvisible *label);

GtkWidget *geda_invisible_new            (void);

GtkWidget *geda_invisible_new_for_screen (GdkScreen     *screen);

void	   geda_invisible_set_screen     (GedaInvisible *invisible,
                                          GdkScreen     *screen);

GdkScreen *geda_invisible_get_screen     (GedaInvisible *invisible);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_INVISIBLE_H__ */
