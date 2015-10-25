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

#define GEDA_TYPE_PIN            (geda_pin_get_type())
#define GEDA_PIN(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_PIN, Pin))
#define GEDA_PIN_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_PIN, PinClass))
#define GEDA_IS_PIN(obj)         (is_a_geda_pin_object((Pin*)obj))
#define GEDA_IS_PIN_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_PIN))
#define GEDA_PIN_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_PIN, PinClass))

BEGIN_DECLS

typedef struct _GedaPinClass PinClass;

struct _GedaPinClass {
  LineClass parent_class;
};

struct _GedaPin {

  Line parent_instance;

  unsigned int head_marker;       /* structure type signature */

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

  unsigned int tail_marker;  /* structure type signature */
};

GedaType     geda_pin_get_type           (void) GEDA_CONST;
bool         is_a_geda_pin_object        (Pin *object);

Object     *geda_pin_new                 (void);
const char *geda_pin_get_electrical      (Pin *pin);
bool        geda_pin_set_electrical      (Pin *pin, const char *electrical);
const char *geda_pin_get_label           (Pin *pin);
bool        geda_pin_set_label           (Pin *pin, const char *label);
const char *geda_pin_get_mechanical      (Pin *pin);
bool        geda_pin_set_mechanical      (Pin *pin, const char *mechanical);
bool        geda_pin_set_number          (Pin *pin, const char *number);
bool        geda_pin_set_sequence        (Pin *pin, const char *sequence);
bool        geda_pin_set_whichend        (Pin *pin, int   whichend);

PIN_ELECT   geda_pin_lookup_etype     (const char *e_str);
const char *geda_pin_lookup_estring   (PIN_ELECT   e_type);
PIN_MECH    geda_pin_lookup_mtype     (const char *m_str);
const char *geda_pin_lookup_mstring   (PIN_MECH    m_type);

END_DECLS
#endif /* __GEDA_PIN_H__ */
