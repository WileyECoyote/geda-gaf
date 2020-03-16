/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_pin.h
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
/*! \class Pin geda_pin.h "libgeda/geda_pin.h"
 *  \brief GedaType for GedaPin Objects.
 *
 *  GedaPin is a derivative of the GedaLine class specialized
 *  for representation and manipulation of Pin object data.
 */
#ifndef __GEDA_PIN_H__
#define __GEDA_PIN_H__

#if defined(__LP64__) || defined(_LP64)
# define GedaPinType unsigned long
#else
# define GedaPinType unsigned int
#endif

#define GEDA_TYPE_PIN            (geda_pin_get_type())
#define GEDA_PIN(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_PIN, GedaPin))
#define GEDA_PIN_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_PIN, GedaPinClass))
#define GEDA_IS_PIN(obj)         (is_a_geda_pin((GedaPin*)(obj)))
#define GEDA_IS_PIN_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_PIN))
#define GEDA_PIN_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_PIN, GedaPinClass))

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _GedaPinClass GedaPinClass;

struct _GedaPinClass {
  GedaLineClass parent_class;
};

struct _GedaPin {

  GedaLine parent_instance;

  char       *number;
  int         sequence;
  int         whichend;      /* either 0 or 1 */

  PIN_ELECT   elect_type;
  PIN_MECH    mech_type;
  PIN_NODE    node_type;     /* either NET or BUS */;

  char       *label;
  char       *electrical;
  char       *mechanical;

  int        *line_width;
};

GedaPinType  geda_pin_get_type           (void) GEDA_CONST;
bool         is_a_geda_pin               (const GedaPin *pin);

GedaObject *geda_pin_new                 (void);
const char *geda_pin_get_electrical      (GedaPin *pin);
const char *geda_pin_get_label           (GedaPin *pin);
const char *geda_pin_get_mechanical      (GedaPin *pin);

PIN_ELECT   geda_pin_lookup_etype        (const char *e_str);
const char *geda_pin_lookup_estring      (PIN_ELECT   e_type);
PIN_MECH    geda_pin_lookup_mtype        (const char *m_str);
const char *geda_pin_lookup_mstring      (PIN_MECH    m_type);

bool        geda_pin_set_electrical      (GedaPin *pin, const char *electrical);
bool        geda_pin_set_label           (GedaPin *pin, const char *label);
bool        geda_pin_set_mechanical      (GedaPin *pin, const char *mechanical);
bool        geda_pin_set_number          (GedaPin *pin, const char *number);
bool        geda_pin_set_sequence        (GedaPin *pin, const char *sequence);
bool        geda_pin_set_whichend        (GedaPin *pin, int   whichend);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_PIN_H__ */
