/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_object.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */
/*! \class GedaObject geda_object.h "libgeda/geda_object.h"
 *  \brief GedaType for Geda Objects.
 *
 *  GedaObject is a derivative of the GObject class. GedaObject
 *  is the base class from which all other Geda objects are derived.
 */
#ifndef __GEDA_OBJECT_H__
#define __GEDA_OBJECT_H__

#if defined(__LP64__) || defined(_LP64)
# define GedaObjectType unsigned long
#else
# define GedaObjectType unsigned int
#endif

#define ConstObject const GedaObject

#define GEDA_TYPE_OBJECT            (geda_object_get_type())
#define GEDA_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_OBJECT, GedaObject))
#define GEDA_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_OBJECT, GedaObjectClass))
#define GEDA_IS_OBJECT(obj)         (is_a_geda_object(obj))
#define GEDA_IS_OBJECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_OBJECT))
#define GEDA_OBJECT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_OBJECT, GedaObjectClass))

typedef struct _GedaObjectClass GedaObjectClass;

struct _GedaObjectClass {

  GObjectClass parent;

  void (*finalize) (GObject *object);

  ObjectBoundsFunc bounds;
};

/* The type definition for this structure is in g_types.h*/
struct _GedaObject {

  GObject parent;

  char    type;                        /* Basic information */
  int     sid;                         /* sequence id ?? */
  char   *name;

  Page   *page;                        /* Parent page */

  int     top;                         /* Bounding box information */
  int     left;                        /* in world coords */
  int     right;
  int     bottom;
  void   *w_bounds_valid_for;          /* Hack = GedaToplevel * */

  GedaArc     *arc;
  GedaBox     *box;
  GedaBus     *bus;
  GedaCircle  *circle;
  GedaComplex *complex;
  GedaLine    *line;
  GedaNet     *net;
  GedaPath    *path;
  GedaPicture *picture;
  GedaPin     *pin;
  GedaText    *text;

  FILL_OPTIONS *fill_options;
  LINE_OPTIONS *line_options;

  GList  *tiles;                  /* tiles in which this object appears */

  GList  *conn_list;              /* List of connections */
                                  /* to and from this object */

  /* Pointer to parent object is used for floating sibling */
  GedaObject *parent_object;      /* Parent object pointer */

  int     color;                  /* Which color */
  int     dont_redraw;            /* Flag to skip redrawing */
  int     selectable;             /* object selectable flag */
  int     selected;               /* object selected flag */

  int     locked_color;           /* Locked color, object's real color */

  GList  *attribs;                /* attribute stuff */
  int     show_name_value;
  int     visibility;

  GedaObject *attached_to;            /* when object is an attribute */
  GedaObject *copied_to;              /* used when copying attributes */

  /* Attribute notification handling */
  int attrib_notify_freeze_count;
  int attrib_notify_pending;

  /* Connection notification handling */
  int conn_notify_freeze_count;
  int conn_notify_pending;

  GList   *weak_refs;             /* Weak references */
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_object_get_type          (void) GEDA_CONST;
bool           is_a_geda_object              (const void *object);

GedaObject   *geda_object_new                (int type, char const *name);
GedaObject   *geda_object_ref                (GedaObject *object);
void          geda_object_unref              (GedaObject *object);
void          geda_object_weakref_notify     (GedaObject *object);
void          geda_object_weak_ref           (GedaObject *object, WeakNotifyFunc notify_func, void *user_data);
void          geda_object_weak_unref         (GedaObject *object, WeakNotifyFunc notify_func, void *user_data);
void          geda_object_add_weak_ptr       (GedaObject *object, void *weak_pointer_loc);
void          geda_object_remove_weak_ptr    (GedaObject *object, void *weak_pointer_loc);

int           geda_object_bounds             (ConstObject *object);
const GList  *geda_object_get_attached       (ConstObject *object);
GedaObject   *geda_object_get_attached_to    (ConstObject *object);
int           geda_object_get_color          (ConstObject *object);
const GList  *geda_object_get_conn_list      (ConstObject *object);
int           geda_object_locked_color       (ConstObject *object);
Page         *geda_object_get_page           (ConstObject *object);

void          geda_object_set_color          (GedaObject *object, int color);
void          geda_object_set_selectable     (GedaObject *object, int state);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_OBJECT_H__ */
