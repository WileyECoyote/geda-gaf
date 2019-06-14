/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_libgedauio.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2017 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
 * License.
 *
  * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: January, 12, 2017
 * Contributing Author: Wiley Edward Hill
 *
 */

#include "../../config.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#include "../include/gettext.h"

/*!
 * \brief Perform runtime initialization of libgeda library.
 * \par Function Description
 *  This function calls "satellite" initialization functions in
 *  various modules to initialize data structures for runtime.
 *  This function should normally be called before any other
 *  functions in libgedauio are called.
 */
void libgedauio_setup_locale(void)
{

#ifdef ENABLE_NLS

  /* Initialize gettext */
  bindtextdomain (LIBGEDAUIO_GETTEXT_DOMAIN, LOCALEDIR);
  bind_textdomain_codeset(LIBGEDAUIO_GETTEXT_DOMAIN, "UTF-8");

#endif

}
