/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_threads.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * 02110-1301 USA
 *
 * Date: July, 20, 2014
 * Contributing Author: Wiley Edward Hill
 *
 */
/*! \file i_threads.c
 *  \remarks
 *
 *  1.) glib Idles, Timeouts and IO callbacks are executed outside of the
 *      main GTK+ lock, but not signal handlers. To call GTK+ routines that
 *      involve X11 manipulation of the display, inside of such a callback,
 *      wrap the call with gschem_threads_enter and gschem_threads_leave.
 *
 *  2.) Since gschem_threads_init calls gdk_threads_set_lock_functions, all
 *      internal Gdk calls are redirected to our gschem_threads_enter and
 *      gschem_threads_leave handlers.
 */

#include <gschem.h>
#include <geda_debug.h>

/** \defgroup Gschem-Thread-System Thread Module
 *  @{ \par This Group contains core Routines for Thread.
*/

/*! Non-zero if the gschem threads has been initialised. */
static unsigned int init_called = 0;

/* Anonymous Static GMutex, seems more portable then GSTATIC_MUTEXT*/
static union
{
  void* p;
  unsigned int i[2];
} gschem_threads_mutex;

static void (*gschem_threads_lock)(void) = NULL;
static void (*gschem_threads_unlock)(void) = NULL;

/*! \brief Enter Thread
 *  \par Function Description
 *  This function marks the beginning of a critical section in which
 *  GDK and GTK+ functions can be called safely and without causing
 *  race conditions. Only one thread at a time can be in such a
 *  critial section.
 */
void gschem_threads_enter (void)
{
  if (gschem_threads_lock)
    (*gschem_threads_lock) ();
}

/*! \brief Leave Thread
 *  \par Function Description
 *  Leaves a critical region begun with gschem_threads_enter().
 */
void gschem_threads_leave (void)
{
  if (gschem_threads_unlock) {
    gdk_flush ();
    (*gschem_threads_unlock) ();
  }
}

static void
gschem_threads_impl_lock (void)
{
  g_mutex_lock((GMutex*)&gschem_threads_mutex);
}

static void
gschem_threads_impl_unlock (void)
{
  g_mutex_unlock((GMutex*)&gschem_threads_mutex);
}

/*! \brief Initialize Thread Support
 *  \par Function Description
 *   Initializes Gschem so multiple threads can call Gdk library functions
 *   in conjunction with gschem_threads_enter() and gschem_threads_leave().
 *
 *  \note This function call must be made before the Gtk main loop begins.
 */
bool gschem_threads_init (void)
{
  bool result;

  if (g_thread_supported ()) {

    result = TRUE;

    if (g_once_init_enter (&init_called)) {
      gdk_threads_set_lock_functions (gschem_threads_impl_lock,
                                      gschem_threads_impl_unlock);
      if (!gschem_threads_lock) {
        gschem_threads_lock = gschem_threads_impl_lock;
      }

      if (!gschem_threads_unlock) {
        gschem_threads_unlock = gschem_threads_impl_unlock;
      }

      gdk_threads_init();

      g_once_init_leave (&init_called, 1);
    }
  }
  else {
    result = FALSE;
  }
  return result;
}

/** @} endgroup Gschem-Thread-System */
