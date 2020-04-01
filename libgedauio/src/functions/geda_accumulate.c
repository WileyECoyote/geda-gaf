/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_accumulate.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: August 17, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>
#include <glib.h>
#include <glib-object.h>

bool
geda_boolean_handled_accumulator (GSignalInvocationHint *ihint,
                                  GValue                *return_accu,
                                  const GValue          *handler_return,
                                  void                  *dummy)
{
  bool continue_emission;
  bool signal_handled;

  signal_handled = g_value_get_boolean (handler_return);
  g_value_set_boolean (return_accu, signal_handled);
  continue_emission = !signal_handled;

  return continue_emission;
}

bool
geda_single_string_accumulator (GSignalInvocationHint *ihint,
                                GValue                *return_accu,
                                const GValue          *handler_return,
                                void                  *dummy)
{
  bool continue_emission;
  const char *str;

  str = g_value_get_string (handler_return);
  g_value_set_string (return_accu, str);
  continue_emission = str == NULL;

  return continue_emission;
}
