/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * \file scheme_deprecated.c
 * \brief Deprecated Scheme API functions
 */
#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

/*! \brief Get the width of line used to draw an object
 * \par Function Description
 * Returns the line width used to draw an object. Deprecated because
 * it does not respect type restrictions, unlike the %object-stroke
 * function in (geda core object).
 *
 * param obj_s the object to get line width for.
 *
 * \return the line width.
 */
EDA_SCM_DEFINE (get_line_width, "%get-line-width", 1, 0, 0,
               (SCM obj_s), "Get the width of line used to draw an object")
{
  SCM_ASSERT ((edascm_is_object_type (obj_s, OBJ_LINE)   ||
               edascm_is_object_type (obj_s, OBJ_BOX)    ||
               edascm_is_object_type (obj_s, OBJ_BUS)    ||
               edascm_is_object_type (obj_s, OBJ_CIRCLE) ||
               edascm_is_object_type (obj_s, OBJ_ARC)    ||
               edascm_is_object_type (obj_s, OBJ_NET)    ||
               edascm_is_object_type (obj_s, OBJ_PIN)    ||
               edascm_is_object_type (obj_s, OBJ_PATH)),
               obj_s, SCM_ARG1, scheme_get_line_width);

  GedaObject *object = edascm_to_object (obj_s);

  return scm_from_int(object->line_options->line_width);
}

/*!
 * \brief Create the (geda core deprecated) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core deprecated) module. The module
 * can be accessed using (use-modules (geda core deprecated)).
 */
static void
init_module_geda_core_deprecated (void *nothing)
{
  /* Register the functions */
  #include "scheme_deprecated.x"

  /* Add them to the module's public definitions. */
  scm_c_export (scheme_get_line_width, NULL);
}

/*!
 * \brief Initialize the basic gEDA page manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #Page smobs.
 * Should only be called by edascm_init().
 */
void
edascm_init_deprecated (void)
{
  /* Define the (geda core page) module */
  scm_c_define_module ("geda core deprecated",
                       init_module_geda_core_deprecated,
                       NULL);
}
