/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_threads.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: November 24, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif


#include <geda/geda.h>
#include <glib.h>

void
geda_main_context_invoke_full (GMainContext *context, int priority, GSourceFunc function,
                               void         *data, GDestroyNotify  notify)
{

  if (context == NULL) {
    context = g_main_context_default();
  }

  if (g_main_context_is_owner (context)) {

    while (function (data));

    if (notify != NULL) {
      notify (data);
    }
  }
  else {

    GMainContext *thread_default;

    thread_default = g_main_context_get_thread_default ();

    if (!thread_default) {
      thread_default = g_main_context_default ();
    }

    if (thread_default == context && g_main_context_acquire (context)) {

      while (function (data));

      g_main_context_release (context);

      if (notify != NULL) {
	notify (data);
      }
    }
    else {

      GSource *source;

      source = g_idle_source_new ();
      g_source_set_priority (source, priority);
      g_source_set_callback (source, function, data, notify);
      g_source_attach (source, context);
      g_source_unref (source);
    }
  }
}

void
geda_main_context_invoke (GMainContext *context, GSourceFunc function, void *data)
{
  geda_main_context_invoke_full (context, G_PRIORITY_DEFAULT, function, data, NULL);
}
