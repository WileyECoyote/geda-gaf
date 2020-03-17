/* -*- geda_pin.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 *
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
/*! \file geda_pin.c
 *  \brief Geda Pin Object Class Module
 */

/** \defgroup geda-pin-object Geda Pin Object
 *  @{
 * \brief Implmentation of #GedaPin Class
 * \par
 *  This module implements GedaPin Objects in libgeda. GedaPin Objects are
 *  symbols for electrical objects used to represent a potential connection
 *  node in higher level assemblies. GedaPin object node points can represent
 *  a single node (PIN_NET_NODE) or many nodes (PIN_BUS_NODE). The GedaPin
 *  class is derived from the GedaLine class.
 *
 * \class GedaPin geda_pin.h "include/libgeda/geda_pin.h"
 * \implements geda-line
 * \implements geda-object
 */

#include <config.h>
#include <ctype.h>

#include <libgeda_priv.h>

static GObjectClass *geda_pin_parent_class = NULL;

enum {
  PROP_0,
  PROP_ELECTRICAL,
  PROP_LABEL,
  PROP_MECHANICAL,
  PROP_NUMBER,
  PROP_SEQUENCE,
  PROP_TYPE,         /* Obsolete, used to sort to new */
  PROP_WHICHEND
};

static char *e_strings[] = { "in",  "out", "io",  "oc", "oe", "pas", "tp",
                             "tri", "clk", "pwr", NULL };

static char *m_strings[] = { "lead", "body",  "pad", "bump",
                             "ball", "wedge", "ribbon", NULL };

static void geda_pin_set_property (GObject *object,     unsigned int  prop_id,
                                   const GValue *value, GParamSpec   *pspec)
{
  GedaPin *pin = GEDA_PIN (object);

  switch (prop_id)
  {
    case PROP_ELECTRICAL:
      geda_pin_set_electrical (pin, g_value_get_string (value));
      break;
    case PROP_LABEL:
      geda_pin_set_label (pin, g_value_get_string (value));
      break;
    case PROP_MECHANICAL:
      geda_pin_set_mechanical (pin, g_value_get_string (value));
      break;
    case PROP_NUMBER:
      geda_pin_set_number (pin, g_value_get_string (value));
      break;
    case PROP_SEQUENCE:
      geda_pin_set_sequence (pin, g_value_get_string (value));
      break;
    case PROP_TYPE:
      geda_pin_set_electrical (pin, g_value_get_string (value));
      break;
    case PROP_WHICHEND:
      pin->whichend = g_value_get_int (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_pin_get_property (GObject *object, unsigned int  prop_id,
                                   GValue  *value,  GParamSpec   *pspec)
{
  GedaPin *pin = GEDA_PIN (object);
  char s_val[4];

  switch (prop_id) {
    case PROP_ELECTRICAL:
      g_value_set_string (value, pin->electrical);
      break;

    case PROP_LABEL:
      g_value_set_string (value, pin->label);
      break;

    case PROP_MECHANICAL:
      g_value_set_string (value, pin->mechanical);
      break;

    case PROP_NUMBER:
      g_value_set_string (value, pin->number);
      break;

    case PROP_SEQUENCE:
      g_value_set_string (value, geda_utility_string_int2str( pin->sequence, s_val, 10 ));
      break;

    case PROP_TYPE:
      g_value_set_string (value, pin->electrical);
      break;

    case PROP_WHICHEND:
      g_value_set_int (value, pin->whichend);
    default:

      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void geda_pin_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_pin_parent_class)->dispose(object);
}

/*!
 * \brief Geda Pin GedaObject Finalization Function
 * \par Function Description
 *  This function removes or releases all internal references and
 *  releases the memory allocated to the given Pin data structure,
 *  invalidates the Pin's markers, then chain up to the parent's
 *  finalize handler after.
 */
static void geda_pin_finalize(GObject *object)
{
  GedaPin    *pin = GEDA_PIN(object);
  GedaObject *obj = GEDA_OBJECT(object);

  if (pin->electrical) {
    GEDA_FREE(pin->electrical);
    pin->electrical = NULL;
  }
  if (pin->mechanical) {
    GEDA_FREE(pin->mechanical);
    pin->mechanical = NULL;
  }

  GEDA_FREE(pin->label);
  GEDA_FREE(pin->number);

  /* The object is no longer a GedaPin */
  obj->pin = NULL;

  /* Finialize the parent GedaLine Class */
  GEDA_LINE_CLASS(geda_pin_parent_class)->finalize(object);
}

/*!
 * \brief GedaType class initializer for GedaPin
 * \par Function Description
 *  GedaType class initializer for GedaPin. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  klass      The GedaPin class we are initializing
 * \param [in]  class_data The Pin structure associated with the class
 */
static void geda_pin_class_init(void *klass, void *class_data)
{
  GParamSpec   *params;

  GedaPinClass *class          = (GedaPinClass*)klass;
  GObjectClass *gobject_class  = (GObjectClass*)klass;

  geda_pin_parent_class        = g_type_class_peek_parent( class );

  gobject_class->set_property  = geda_pin_set_property;
  gobject_class->get_property  = geda_pin_get_property;

  gobject_class->dispose       = geda_pin_dispose;
  gobject_class->finalize      = geda_pin_finalize;

  params = g_param_spec_string ("electrical", _("Electrical"),
                              _("Electrical Properties of the pin"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ELECTRICAL, params);

  params = g_param_spec_string ("label", _("Label"),
                              _("A Label for the pin"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_LABEL, params);

  params = g_param_spec_string ("number", _("Number"),
                              _("The pin number"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_NUMBER, params);

  params = g_param_spec_string ("mechanical", _("Mechanical"),
                              _("Mechanical Properties of the pin"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MECHANICAL, params);

  params = g_param_spec_string ("sequence", _("Sequence"),
                              _("The pin sequence"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_SEQUENCE, params);

  params = g_param_spec_string ("pin-type", _("Type"),
                              _("Either Mechanical or Electrical Properties of the pin"),
                                "",
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_TYPE, params);

  params = g_param_spec_int ("whichend", _("Which End"),
                           _("Which end of the pin gets connected"),
                             -1,
                              1,
                             -1,
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_WHICHEND, params);

}

/*!
 * \brief GedaType instance initializer for GedaPin
 * \par Function Description
 *  GedaType instance initializer for GedaPin, initializes a new empty
 *  Pin object by setting pointers to NULL and numbers to zero.
 *
 * \param [in] instance The GedaPin structure being initialized,
 * \param [in] g_class  The GedaPin class we are initializing.
 */
static void geda_pin_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaPin    *pin    = (GedaPin*)instance;
  GedaLine   *line   = &pin->parent_instance;
  GedaObject *object = &line->parent_instance;

  pin->number        = NULL;
  pin->whichend      = 0;    /* either 0 or 1 */

  pin->elect_type    = PIN_ELECT_VOID; /* electrical type code */
  pin->mech_type     = PIN_MECH_VOID;  /* mechanical type code */
  pin->node_type     = PIN_NET_NODE;   /* either NET or BUS */

  pin->label         = NULL; /* Pointer to label string */
  pin->electrical    = NULL; /* Pointer to electrical string */
  pin->mechanical    = NULL; /* Pointer to mechanical string */

  object->pin        = pin;

  pin->line_width    = &line->line_options.line_width;

  line->line_options.line_width = default_thick_pin_width;
}

/*! \brief Function to retrieve Pin's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Pin Type identifier. When first called,
 *  the function registers a #Pin in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Pin and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with Pin.
 */
GedaObjectType geda_pin_get_type (void)
{
  static volatile GedaObjectType geda_pin_type = 0;

  if (g_once_init_enter (&geda_pin_type)) {

    static const GTypeInfo info = {
      sizeof(GedaPinClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_pin_class_init,    /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaPin),
      0,                      /* n_preallocs         */
      geda_pin_instance_init  /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaPin");
    type   = g_type_register_static (GEDA_TYPE_LINE, string, &info, 0);

    g_once_init_leave (&geda_pin_type, type);
  }

  return geda_pin_type;
}

/*! \brief Determine if object is a Geda Pin GedaObject.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Pin object.
 *
 *  \return boolean.
 */
bool is_a_geda_pin (const GedaPin *pin)
{
  return GEDA_IS_OBJECT(pin) && (((GedaObject*)pin)->type == OBJ_PIN);
}

/*! \brief Returns a pointer to a new Pin object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Pin object.
 *
 *  \return pointer to the new Pin object.
 */
GedaObject *geda_pin_new (void)
{
  GedaObject *pin = g_object_new( GEDA_TYPE_PIN,
                             "type", OBJ_PIN,
                             "name", "pin",
                              NULL );
  return GEDA_OBJECT(pin);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_get_electrical(GedaPin *pin)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), NULL);
  return pin->electrical;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_get_label(GedaPin *pin)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), NULL);
  return pin->label;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_get_mechanical(GedaPin *pin)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), NULL);
  return pin->mechanical;
}

const char *geda_pin_get_number(GedaPin *pin)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), NULL);

  return pin->number;
}

int geda_pin_get_sequence(GedaPin *pin)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), -1);

  return pin->sequence;
}

/*!
 * \brief Look up Pin Electrical Type from String Code
 * \par Function Description
 *  Compares e_str to each entry in e_strings and returns the index,
 *  if found. The index is the PIN_ELECT associated with e_str or
 *  PIN_ELECT_VOID if e_str was not found in e_strings.
 *
 * \note e_strings must always be in the same order as the PIN_ELECT
 *       enumeration found in "geda_enum.h"
 *
 * \param [in]  e_str Pointer to pin-type string to lookup.
 *
 * \retval PIN_ELECT
 *
 * \remarks Libgeda does not treat pin-type PIN_ELECT_VOID as an
 *          error.
 */
PIN_ELECT geda_pin_lookup_etype(const char *e_str) {

  PIN_ELECT index;

  for (index = PIN_ELECT_IN; e_strings[index] != NULL; index++) {
    if (strcmp(e_str, e_strings[index]) == 0)
      break;
  }

  if (e_strings[index] == NULL ) index = PIN_ELECT_VOID;

  return index;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_lookup_estring(PIN_ELECT e_type) {

  char *str   = NULL;
  int   index = (int)e_type;

  if (index > -1 && index <= PIN_ELECT_VOID) {
    str = e_strings[index];
  }

  return str;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PIN_MECH geda_pin_lookup_mtype(const char *m_str) {

  PIN_MECH index;
  for (index = PIN_MECH_LEAD; m_strings[index] != NULL; index++) {
    if (strcmp(m_str, m_strings[index]) == 0)
      break;
  }
  if(m_strings[index] == NULL ) index = PIN_MECH_VOID;
  return index;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_lookup_mstring(PIN_MECH m_type) {

  char *str   = NULL;
  int   index = (int)m_type;

  if (index > -1 && index < PIN_MECH_VOID + 1) {
    str = m_strings[index];
  }

  return str;
}

/*! \brief Set Pin Electrical String
 *  \par Function Description
 *  Sets the electrical description property for \a pin to the value
 *  \a electrical, this function looks for the \a electical in the
 *  e_strings structure above and updates the elect_type, formally
 *  pin-type, property to the index of the found strings as returned
 *  by geda_pin_lookup_etype.
 *
 * \param [in,out] pin        A valid Pin object.
 * \param [in]     electrical String, member of e_strings.
 */
bool geda_pin_set_electrical(GedaPin *pin, const char *electrical)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), FALSE);

  bool      changed = FALSE;
  PIN_ELECT current_type;

  if (electrical != NULL) {

    if (pin->electrical) {
      if (strcmp(pin->electrical, electrical) != 0) {
        changed = TRUE;
        GEDA_FREE(pin->electrical);
      }
    }
    else {
      changed = TRUE; /* current value is NULL */
    }

    if (changed) {
      pin->electrical = geda_utility_string_strdup(electrical);

      /* Check if there is a code associated with the description */
      current_type = geda_pin_lookup_etype(electrical);

      /* Update the code if discrepant */
      if (current_type != pin->elect_type)
        pin->elect_type = current_type;
    }
  }
  return changed;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool geda_pin_set_label(GedaPin *pin, const char *label)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), FALSE);

  bool changed;

  if (label != NULL) {
    if (pin->label) {
      if (strcmp(pin->label, label) != 0) {
        GEDA_FREE(pin->label);
        changed = TRUE;
      }
      else {
        changed = FALSE;
      }
    }
    else {
      changed = TRUE;
    }

    if (changed) {
      pin->label = geda_utility_string_strdup(label);
    }
  }
  else {
   changed = FALSE;
  }

  return changed;
}

/*! \brief Set Pin Mechanical String
 *  \par Function Description
 *  Sets the mechanical description property for \a pin to the value
 *  \a mechanical, this function looks for the \a mechanical in the
 *  e_strings structure above and updates the elect_type, formally
 *  pin-type, property to the index of the found strings as returned
 *  by geda_pin_lookup_mtype.
 *
 * \param [in,out] pin        A valid Pin object.
 * \param [in]     mechanical String, member of m_strings.
 */
bool geda_pin_set_mechanical(GedaPin *pin, const char *mechanical)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), FALSE);

  bool    changed = FALSE;
  PIN_MECH current_type;

  if ( mechanical != NULL ) {

    if (pin->mechanical) {
      if (strcmp(pin->mechanical, mechanical) != 0) {
        changed = TRUE;
        GEDA_FREE(pin->mechanical);
      }
    }
    else {
      changed = TRUE;
    }

    if (changed) {
      pin->mechanical = geda_utility_string_strdup(mechanical);

      /* Check if there is a code associated with the description */
      current_type = geda_pin_lookup_mtype(mechanical);

      /* Update the code if discrepant */
      if (current_type != pin->mech_type)
        pin->mech_type = current_type;
    }
  }

  return changed;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool geda_pin_set_number(GedaPin *pin, const char *number)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), FALSE);

  bool changed = FALSE;

  if (number != NULL) {

    if (pin->number) {
      if (strcmp(pin->number, number) != 0) {
        changed = TRUE;
        GEDA_FREE(pin->number);
      }
    }
    else {
      changed = TRUE;
    }
    if (changed) {
      pin->number = geda_utility_string_strdup(number);
    }
  }

  return changed;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool geda_pin_set_sequence(GedaPin *pin, const char *sequence)
{
  const char *ptr     = sequence;
        bool  changed = FALSE;
        bool  valid   = TRUE;

  g_return_val_if_fail(GEDA_IS_PIN(pin), FALSE);

  while (*ptr) {
    if ( !isdigit(*ptr) ) {
      valid = FALSE;
      break;
    }
    ptr++;
  }

  if (valid) {

    int ivalue = atoi(sequence);

    if ( ivalue != pin->sequence ) {
      pin->sequence = ivalue;
      changed = TRUE;
    }
  }
  return changed;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool geda_pin_set_whichend(GedaPin *pin, int whichend)
{
  g_return_val_if_fail(GEDA_IS_PIN(pin), FALSE);
  pin->whichend = whichend;
  return TRUE;
}

/** @} endgroup geda-pin-object */
