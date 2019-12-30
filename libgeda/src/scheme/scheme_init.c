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
 * \file scheme_init.c
 * \brief Scheme API initialization
 */
#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

/*! Non-zero if the Scheme API has been initialized. */
static volatile GedaType init_called = 0;

/*!
 * \brief Scheme API initialization worker function.
 * \par Function Description
 *  Called by edascm_init() with current thread in Guile mode.
 */
static void *edascm_init_impl (void *data)
{
#if ENABLE_NLS

  scm_setlocale(scm_variable_ref(scm_c_lookup("LC_ALL")), scm_from_locale_string(""));

#endif

  edascm_init_smob ();
  edascm_init_toplevel ();
  edascm_init_object ();
  edascm_init_complex ();
  edascm_init_page ();
  edascm_init_attrib ();
  edascm_init_os ();
  edascm_init_config ();
  edascm_init_closure ();
  edascm_init_log ();
  edascm_init_version ();
  edascm_init_deprecated ();

  return NULL;
}

/*!
 * \brief Initialize the Scheme API.
 * \ingroup guile_c_iface
 *
 * \par Function Description
 *  Registers all modules, procedures and variables exported by the
 *  libgeda Scheme API.
 */
void edascm_init ()
{
  volatile GedaType *initialized = &init_called;

  if (g_once_init_enter (initialized)) {
    scm_with_guile (edascm_init_impl, NULL);
    g_once_init_leave (initialized, 1);
  }
}
