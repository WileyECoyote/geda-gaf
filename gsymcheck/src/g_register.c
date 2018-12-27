/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_register.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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
 * MA 02111-1301 USA
 */

#include <config.h>

#include <libgeda/libgeda.h>

#include "../include/struct.h"
#include "../include/prototype.h"

/*! \brief Register function with Scheme.
 *  \par Function Description
 *  Creates <B>subr</B> objects to make <B>g_rc_*</B> gsymcheck API functions
 *  visible to Scheme.
 */
void g_register_funcs(void)
{
  /* general functions */
  scm_c_define_gsubr ("quit", 0, 0, 0, g_quit);
  scm_c_define_gsubr ("exit", 0, 0, 0, g_quit);

  /* gsymcheckrc functions */
  scm_c_define_gsubr ("gsymcheck-version", 1, 0, 0, g_rc_gsymcheck_version);
  scm_c_define_gsubr ("known-devices",     1, 0, 0, g_rc_known_devices);
  scm_c_define_gsubr ("valid-attributes",  1, 0, 0, g_rc_valid_attributes);
}

/*! \brief SCM API terminated program
 *  \par Function Description
 *  Scheme
 */
SCM g_quit(void)
{
  gsymcheck_quit();
  exit(0);
}

