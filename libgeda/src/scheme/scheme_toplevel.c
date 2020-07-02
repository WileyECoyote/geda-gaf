/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2015 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_toplevel.c
 * \brief Scheme API procedures for working with the GedaToplevel.
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

static SCM scheme_toplevel_fluid = SCM_UNDEFINED;

/*!
 * \brief Set the #GedaToplevel fluid in the current dynamic context.
 * \ingroup guile_c_iface
 * \par Function Description
 * This function must be used inside a pair of calls to
 * scm_dynwind_begin() and scm_dynwind_end(). During the dynwind
 * context, the #GedaToplevel fluid is set to \a toplevel.
 *
 * \note This is a part of the public C interface to the Scheme API.
 */
void edascm_dynwind_toplevel (GedaToplevel *toplevel)
{
  g_return_if_fail(GEDA_IS_TOPLEVEL(toplevel));
  SCM s_toplevel = edascm_from_toplevel (toplevel);
  scm_dynwind_fluid (scheme_toplevel_fluid, s_toplevel);
}

/*!
 * \brief Get the value of the #GedaToplevel fluid.
 * \par Function Description
 * Return the value of the #GedaToplevel fluid in the current dynamic
 * context.
 */
EDA_SCM_DEFINE (current_toplevel, "%current-toplevel", 0, 0, 0, (),
               "Get the GedaToplevel for the current dynamic context.")
{
  return scm_fluid_ref (scheme_toplevel_fluid);
}

/*!
 * \brief Get the value of the #GedaToplevel fluid.
 * \ingroup guile_c_iface
 * \par Function Description
 * Return the value of the #GedaToplevel fluid in the current dynamic
 * context.
 *
 * \note This is a part of the public C interface to the Scheme API,
 *       this essentially makes the toplevel a global varible as was
 *       done in the old version of geda-gaf.
 */
GedaToplevel *edascm_c_current_toplevel (void)
{
  SCM s_toplevel = current_toplevel ();

  EDASCM_ASSERT_SMOB_VALID(s_toplevel);

  return (GedaToplevel*) SCM_SMOB_DATA (s_toplevel);
}

/*!
 * \brief Set the current #GedaToplevel temporarily.
 * \par Function Description
 * Set the #GedaToplevel fluid to \a toplevel and call \a thunk.
 */
EDA_SCM_DEFINE (with_toplevel, "%with-toplevel", 2, 0, 0,
            (SCM toplevel, SCM thunk),
            "Call `thunk', setting the GedaToplevel fluid to `toplevel'.")
{
  return scm_with_fluid (scheme_toplevel_fluid, toplevel, thunk);
}

/*!
 * \brief Set the current #GedaToplevel temporarily.
 * \ingroup guile_c_iface
 * \par Function Description
 * Set the #GedaToplevel fluid to \a toplevel and call \a func with \a
 * user_data.
 */
SCM
edascm_c_with_toplevel (GedaToplevel *toplevel, SCM (*func)(void *),
                        void *user_data)
{
  SCM s_toplevel = edascm_from_toplevel (toplevel);

  return scm_c_with_fluid (scheme_toplevel_fluid, s_toplevel, func, user_data);
}

/*!
 * \brief Create the (geda core toplevel) Scheme module
 * \par Function Description
 * Defines procedures in the (geda core toplevel) module. The module
 * can be accessed using (use-modules (geda core toplevel)).
 */
static void init_module_geda_core_toplevel (void *nothing)
{
  /* Register the functions */
  #include "scheme_toplevel.x"

  /* Add them to the module's public definitions. */
  scm_c_export (scheme_with_toplevel, scheme_current_toplevel, NULL);
}

/*!
 * \brief Initialize the GedaToplevel manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #GedaToplevel smobs
 * and creates the #GedaToplevel fluid. Should only be called by
 * edascm_init().
 */
void edascm_init_toplevel (void)
{
  scheme_toplevel_fluid = scm_permanent_object (scm_make_fluid ());

  /* Define the (geda core toplevel) module */
  scm_c_define_module ("geda core toplevel",
                       init_module_geda_core_toplevel,
                       NULL);
}
