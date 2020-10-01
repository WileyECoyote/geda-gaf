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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * Invokes a function in such a way that context is owned during
 * the invocation of function. This function is the same as g_main_context_invoke()
 * except that it lets you specify the priority in case function
 * ends up being scheduled as an idle and also lets you give a
 * GDestroyNotify for data.
 *
 * Note: \a notify should not assume that it has been called from
 * any particular thread or with any particular context acquired.
 */
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

/*! \todo Finish function documentation!!!
 * \brief
 * \par Function Description
 * Invokes a function in such a way that context is owned during the
 * invocation of function. If context is NULL then the global default
 * main context — as returned by g_main_context_default() — is used.
 * If context is owned by the current thread, the function is called
 * directly. Otherwise, if context is the thread-default main context
 * of the current thread and g_main_context_acquire() succeeds, then
 * function is called and g_main_context_release() is called afterwards.
 *
 * In any other case, an idle source is created to call function and that
 * source is attached to context (presumably to be run in another thread).
 * The idle source is attached with G_PRIORITY_DEFAULT priority. If you
 * want a different priority, use g_main_context_invoke_full().
 *
 * \note that, as with normal idle functions, function should probably
 *       return FALSE. If it returns TRUE, it will be continuously run
 *       in a loop (and may prevent this call from returning).
 */
void
geda_main_context_invoke (GMainContext *context, GSourceFunc function, void *data)
{
  geda_main_context_invoke_full (context, G_PRIORITY_DEFAULT, function, data, NULL);
}
