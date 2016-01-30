/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_net.h
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
 *  Date Contributed: November, 18, 2013
 */
/*! \class Net geda_net.h "libgeda/geda_net.h"
 *  \brief GedaType for GedaNet Objects.
 *
 *  GedaNet is a derivative of the GedaLine class specialized
 *  for representation and manipulation of connectivity data.
 */
#ifndef __GEDA_NET_H__
#define __GEDA_NET_H__

#define GEDA_TYPE_NET            (geda_net_get_type())
#define GEDA_NET(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_NET, Net))
#define GEDA_NET_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_NET, NetClass))
#define GEDA_IS_NET(obj)         (is_a_geda_net_object((Net*)obj))
#define GEDA_IS_NET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_NET))
#define GEDA_NET_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_NET, NetClass))

typedef struct _GedaNetClass NetClass;

struct _GedaNetClass {
  LineClass parent_class;
};

struct _GedaNet {

  Line parent_instance;

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
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_net_get_type      (void) GEDA_CONST;
bool           is_a_geda_net_object   (Net *object);

GedaObject    *geda_net_new           (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_NET_H__ */
