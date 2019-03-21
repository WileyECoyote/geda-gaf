/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: libgedaguile.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 *
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#include <libgeda/edascmvaluetypes.h>
#include <libgeda/edascmhookproxy.h>

/*!
 * \file libgedaguile.h
 *  Scheme API public declarations and definitions.
 *
 * \warning Do not include from libgeda.h: This file should only be
 *          included by source files that need to use the Scheme API.
 */

/* scheme_init.c */

/* Initialize the Scheme API. (implmentation scheme_init.c) */
void edascm_init ();

/* scheme_toplevel.c */

/* Get the value of the #GedaToplevel fluid. */
GedaToplevel *edascm_c_current_toplevel ();

/* Set the #GedaToplevel fluid in the current dynamic context. */
void edascm_dynwind_toplevel (GedaToplevel *toplevel); /* edascm_c_dynwind_toplevel? */

/* Set the current #GedaToplevel temporarily. */
SCM edascm_c_with_toplevel (GedaToplevel *toplevel, SCM (*func)(void *),
                            void *user_data);

/* scheme_smob.c */

/* Create a Guile value from a page structure. */
SCM edascm_from_page (Page *page);

/* Create a Guile value from an object structure. */
SCM edascm_from_object (GedaObject *object);

/* Create a Guile value from a configuration context structure. */
SCM edascm_from_config (EdaConfig *cfg);

/* Retrieve a page structure from a Guile value. */
Page *edascm_to_page (SCM smob);

/* Retrieve an object structure from a Guile value. */
GedaObject *edascm_to_object (SCM smob);

/* Retrieve an configuration context structure from a Guile value. */
EdaConfig *edascm_to_config (SCM smob);

/* Test if smob is a gEDA page. */
int edascm_is_page (SCM smob);

/* Test if smob is a gEDA object. */
int edascm_is_object (SCM smob);

/* Test if smob is a gEDA configuration context. */
int edascm_is_config (SCM smob);

/* Set whether a gEDA object may be garbage collected. */
void edascm_c_set_gc (SCM smob, int gc);

/* scheme_closure.c */

/* Create a Scheme closure around a C function. */
SCM edascm_c_make_closure (SCM (*func)(SCM, void *), void *user_data);
