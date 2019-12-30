/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_bus.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Ales Hvezda
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
/*! \class Bus geda_bus.h "libgeda/geda_bus.h"
 *  \brief GedaType for GedaBus Objects.
 *
 *  GedaBus is a derivative of the GedaLine class specialized
 *  for representation and manipulation of Arc object data.
 */
#ifndef __GEDA_BUS_H__
#define __GEDA_BUS_H__

#define GEDA_TYPE_BUS            (geda_bus_get_type())
#define GEDA_BUS(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_BUS, GedaBus))
#define GEDA_BUS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_BUS, GedaBusClass))
#define GEDA_IS_BUS(obj)         (is_a_geda_bus((GedaBus*)(obj)))
#define GEDA_IS_BUS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_BUS))
#define GEDA_BUS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_BUS, GedaBusClass))

typedef struct _GedaGedaBusClass GedaBusClass;

struct _GedaGedaBusClass {
  GedaLineClass parent_class;
};

struct _GedaBus {

  GedaLine parent_instance;

  int  *line_width;

  /* controls which direction bus rippers goes, is either 0 for un-inited, */
  /* 1 for right, -1 for left (horizontal bus)  1 for up, -1 for down (vertial bus) */
  int   ripper_direction;

  char *bus_name;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_bus_get_type             (void) GEDA_CONST;
bool           is_a_geda_bus                 (const GedaBus *bus);

GedaObject    *geda_bus_new                  (void);

bool           geda_bus_get_position         (const GedaBus *bus, int *x, int *y);
int            geda_bus_get_ripper_direction (const GedaBus *bus) WARN_UNUSED;
int            geda_bus_get_x0               (const GedaBus *bus) WARN_UNUSED;
int            geda_bus_get_x1               (const GedaBus *bus) WARN_UNUSED;
int            geda_bus_get_y0               (const GedaBus *bus) WARN_UNUSED;
int            geda_bus_get_y1               (const GedaBus *bus) WARN_UNUSED;

void           geda_bus_set_ripper_direction (GedaBus *bus, int direction);
void           geda_bus_set_x0               (GedaBus *bus, int x);
void           geda_bus_set_x1               (GedaBus *bus, int x);
void           geda_bus_set_y0               (GedaBus *bus, int y);
void           geda_bus_set_y1               (GedaBus *bus, int y);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_BUS_H__ */
