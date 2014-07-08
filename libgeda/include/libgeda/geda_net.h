/* C header                                           -*- geda_net.h -*-
 * file: geda_net.h
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
 *  Date Contributed: November, 18, 2013
 */
#ifndef __GEDA_NET_H__
#define __GEDA_NET_H__

#define GEDA_TYPE_NET            (geda_net_get_type())
#define GEDA_NET(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_NET, Net))
#define GEDA_NET_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_NET, NetClass))
#define GEDA_IS_NET(obj)         (is_a_geda_net_object((Net*)obj))
#define GEDA_IS_NET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_NET))
#define GEDA_NET_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_NET, NetClass))

G_BEGIN_DECLS

typedef struct _GedaNetClass NetClass;

struct _GedaNetClass {
  LineClass parent_class;
};

struct _GedaNet {
  Line parent_instance;

  unsigned int head_marker;       /* structure type signature */

  int  *line_width;

  /* Current these are only used by gnetlist, but the Python API
   * also provides client side access some members */
  int   nid;

  int   net_name_has_priority;
  char *net_name;
  char *pin_label;

  char *connected_to;

  /* Tracking total number of entities connected by this net */
  int  net_num_connected;         /* for nets only */
  bool valid_num_connected;       /* for nets only */

  Net  *prev;
  Net  *next;

  unsigned int tail_marker;       /* structure type signature */
};

GType   geda_net_get_type        (void);
bool    is_a_geda_net_object     (Net *object);

Object *geda_net_new             (void);


G_END_DECLS
#endif /* __GEDA_NET_H__ */
