/* C header                                           -*- geda_object.h -*-
 * file: geda_object.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
 *
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */
#ifndef __GEDA_OBJECT_H__
#define __GEDA_OBJECT_H__

#define GEDA_TYPE_OBJECT            (geda_gobject_get_type())
#define GEDA_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_OBJECT, Object))
#define GEDA_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_OBJECT, ObjectClass))
#define GEDA_IS_OBJECT(obj)         (is_a_geda_object(obj))
#define GEDA_IS_OBJECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_OBJECT))
#define GEDA_OBJECT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_OBJECT, ObjectClass))

G_BEGIN_DECLS

typedef struct _GedaObjectClass ObjectClass;

struct _GedaObjectClass {
  GObjectClass parent;
  void (*finalize) (GObject *object);
  BoundsFunc bounds;
};

/* The type definition for this structure is in g_types.h*/
struct _GedaObject {

  GObject parent;

  unsigned int head_marker;            /* structure type signature */

  char    type;                        /* Basic information */
  int     sid;                         /* sequence id ?? */
  char   *name;

  Page   *page;                        /* Parent page */

  int     top;                         /* Bounding box information */
  int     left;                        /* in world coords */
  int     right;
  int     bottom;
  void   *w_bounds_valid_for;          /* Hack = GedaToplevel * */

  Arc     *arc;
  Box     *box;
  Bus     *bus;
  Circle  *circle;
  Complex *complex;
  Line    *line;
  Net     *net;
  Path    *path;
  Picture *picture;
  Pin     *pin;
  Text    *text;

  FILL_OPTIONS *fill_options;
  LINE_OPTIONS *line_options;

  GList  *tiles;                  /* tiles in which this object appears */

  GList  *conn_list;              /* List of connections */
                                  /* to and from this object */

  /* Pointer to parent object is used for floating sibling */
  Object *parent_object;          /* Parent object pointer */

  int     color;                  /* Which color */
  int     dont_redraw;            /* Flag to skip redrawing */
  int     selectable;             /* object selectable flag */
  int     selected;               /* object selected flag */

  int     locked_color;           /* Locked color, object's real color */

  GList  *attribs;                /* attribute stuff */
  int     show_name_value;
  int     visibility;

  Object *attached_to;            /* when object is an attribute */
  Object *copied_to;              /* used when copying attributes */

  /* Attribute notification handling */
  int attrib_notify_freeze_count;
  int attrib_notify_pending;

  /* Connection notification handling */
  int conn_notify_freeze_count;
  int conn_notify_pending;

  GList   *weak_refs;             /* Weak references */

  unsigned int tail_marker;       /* structure type signature */
};

unsigned int  geda_gobject_get_type          (void) G_GNUC_CONST;
bool          is_a_geda_object               (Object *object);

Object       *geda_object_new                (int type, char const *name );
void          geda_object_weakref_notify     (Object *object);
void          geda_object_weak_ref           (Object *object, WeakNotifyFunc notify_func, void *user_data);
void          geda_object_weak_unref         (Object *object, WeakNotifyFunc notify_func, void *user_data);
void          geda_object_add_weak_ptr       (Object *object, void *weak_pointer_loc);
void          geda_object_remove_weak_ptr    (Object *object, void *weak_pointer_loc);

int           geda_object_bounds             (Object *object);
Page         *geda_object_get_page           (Object *object);

#endif /* __GEDA_OBJECT_H__ */
