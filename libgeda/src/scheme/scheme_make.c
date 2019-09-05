/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2014 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/*!
 * \file scheme_make.c
 * \brief Scheme API object creation procedures.
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

#ifndef  SCM_ARG8
 #define SCM_ARG8 8
 #define SCM_ARG9 9
#endif

/* ----------------------- Scheme GedaObject API ---------------------- */

/*! \brief Create a new line.
 * \par Function Description
 * Creates a new line object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-line procedure in the (geda
 * core object) module.
 *
 * \return a newly-created line object.
 */
EDA_SCM_DEFINE (object_make_line, "%make-line", 0, 0, 0,
               (), "Create a new line object.")
{
  GedaObject *obj = geda_line_object_new (DEFAULT_LINE_COLOR_INDEX, 0, 0, 0, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Create a new net.
 * \par Function Description
 * Creates a new net object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-net procedure in the (geda
 * core object) module.
 *
 * \return a newly-created net object.
 */
EDA_SCM_DEFINE (object_make_net, "%make-net", 0, 0, 0, (),
               "Create a new net object.")
{
  GedaObject *obj;
  SCM result;

  obj = geda_net_object_new (NET_COLOR, 0, 0, 0, 0);

  result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*! \brief Create a new bus.
 * \par Function Description
 * Creates a new bus object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-bus procedure in the (geda
 * core object) module.
 *
 * \todo Do we need a way to get/set bus ripper direction?
 *
 * \return a newly-created bus object.
 */
EDA_SCM_DEFINE (object_make_bus, "%make-bus", 0, 0, 0, (),
               "Create a new bus object.")
{
  GedaObject *obj;
  SCM result;

  obj = geda_bus_object_new (BUS_COLOR, 0, 0, 0, 0, 0);

  result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*! \brief Create a new pin.
 * \par Function description
 * Creates a new pin object, with all parameters set to default
 * values.  type_s is a Scheme symbol indicating whether the pin
 * should be a "net" pin or a "bus" pin.
 *
 * \note Scheme API: Implements the %make-pin procedure in the (geda
 * core object) module.
 *
 * \return a newly-created pin object.
 */
EDA_SCM_DEFINE (object_make_pin, "%make-pin", 1, 0, 0, (SCM type_s),
               "Create a new pin object.")
{
  SCM_ASSERT (scm_is_symbol (type_s), type_s, SCM_ARG1, scheme_object_make_pin);

  int type;

  SCM   s_type   = scm_symbol_to_string (type_s);
  char *pin_type = scm_to_utf8_string (s_type);

  if (strcmp(pin_type,"net") == 0) {
    type = PIN_NET_NODE;
  }
  else if (strcmp(pin_type,"bus") == 0) {
    type = PIN_BUS_NODE;
  }
  else {
    scm_misc_error (scheme_object_make_pin,
                  _("Invalid pin type ~A, must be 'net or 'bus"),
                    scm_list_1 (type_s));
  }
  free(pin_type);

  GedaObject *obj = geda_pin_object_new (PIN_COLOR, 0, 0, 0, 0, type, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);

  return result;
}

/*! \brief Create a new box.
 * \par Function Description
 * Creates a new box object, with all its parameters set to default
 * values.
 *
 * \note Scheme API: Implements the %make-box procedure in the (geda
 * core object) module.
 *
 * \return a newly-created box object.
 */
EDA_SCM_DEFINE (object_make_box, "%make-box", 0, 0, 0,
            (), "Create a new box object.")
{
  GedaObject *obj = geda_box_object_new (DEFAULT_BOX_COLOR_INDEX, 0, 0, 0, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*!
 * \brief Create a new circle.
 * \par Function Description
 *  Creates a new circle object, with all its parameters set to default
 *  values.
 *
 * \note Scheme API: Implements the %make-circle procedure in the
 *       (geda core object) module.
 *
 * \return a newly-created circle object.
 */
EDA_SCM_DEFINE (object_make_circle, "%make-circle", 0, 0, 0,
               (), "Create a new circle object.")
{
  GedaObject *obj = geda_circle_object_new (DEFAULT_CIRCLE_COLOR_INDEX, 0, 0, 1);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*!
 * \brief Create a new arc.
 * \par Function Description
 *  Creates a new arc object, with all its parameters set to default
 *  values.
 *
 * \note Scheme API: Implements the %make-arc procedure in the
 *       (geda core object) module.
 *
 * \return a newly-created arc object.
 */
EDA_SCM_DEFINE (object_make_arc, "%make-arc", 0, 0, 0,
               (), "Create a new arc object.")
{
  GedaObject *obj = geda_arc_object_new (DEFAULT_ARC_COLOR_INDEX, 0, 0, 1, 0, 0);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*!
 * \brief Create a new text item.
 * \par Function Description
 *  Creates a new text object, with all its parameters set to default
 *  values.
 *
 * \note Scheme API: Implements the %make-text procedure in the
 *       (geda core object) module.
 *
 * \return a newly-created text object.
 */
EDA_SCM_DEFINE (object_make_text, "%make-text", 0, 0, 0, (),
               "Create a new text object.")
{
  GedaObject *obj = geda_text_object_new (DEFAULT_TEXT_COLOR_INDEX, 0, 0, LOWER_LEFT, 0,
                            10, VISIBLE, SHOW_NAME_VALUE, "");

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*!
 * \brief Make a new, empty path object.
 * \par Function Description
 *  Creates a new, empty path object with default color, stroke and
 *  fill options.
 *
 * \note Scheme API: Implements the %make-path procedure in the (geda
 *       core object) module.
 *
 * \return a newly-created path object.
 */
EDA_SCM_DEFINE (object_make_path, "%make-path", 0, 0, 0,
           (), "Create a new path object")
{
  GedaObject *obj = geda_path_object_new (DEFAULT_PATH_COLOR_INDEX, "");

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);
  return result;
}

/*!
 * \brief Create a new, empty picture object.
 * \par Function Description
 *  Creates a new picture object with no filename, no image data and
 *  all other parameters set to default values.  It is initially set to
 *  be embedded.
 *
 * \note Scheme API: Implements the %make-picture procedure in the
 *       (geda core object) module.
 *
 * \return a newly-created picture object.
 */
EDA_SCM_DEFINE (object_make_picture, "%make-picture", 0, 0, 0, (),
               "Create a new picture object")
{

  GedaObject *obj;
  SCM     result;

  obj   = geda_picture_object_new (NULL, 0, NULL, 0, 0, 1, 1, 0, FALSE, TRUE);

  result= edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, 1);
  return result;
}

/*!
 * \brief Initialize the basic gEDA object Creator Routines.
 * \par Function Description
 *  Registers some Scheme procedures for creating new GedaObjects.
 *  Unlike other edascm initialization procedures, this function is
 *  not called from edascm_init. The routines in this module are part
 *  of the "geda core object" interface and so this function is called
 *  from init_module_geda_core_object as an extension to that module.
 */
void
edascm_init_make_object ()
{
  /* Register the functions */
  #include "scheme_make.x"

  /* Add them to the module's public definitions. */
  scm_c_export (scheme_object_make_arc,
                scheme_object_make_bus,
                scheme_object_make_box,
                scheme_object_make_circle,
                scheme_object_make_line,
                scheme_object_make_net,
                scheme_object_make_path,
                scheme_object_make_picture,
                scheme_object_make_pin,
                scheme_object_make_text,
                NULL);
}
