/* -*- C x_misc.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2011-2015 gEDA Contributors (see ChangeLog for details)
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
 */
/*!
 * \file x_misc.c
 * \brief Module for miscellaneous functions
 */

#include <gschem.h>

#ifdef OS_WIN32
#  ifndef STRICT
#    define STRICT
#    include <windows.h>
#    undef STRICT
#  endif
#endif

#include <geda_debug.h>

#if defined (OS_WIN32)

/*! \brief Launch application to show URI on Windows.
 * \par Function Description
 * On native Windows, the ShellExecute Windows API function provides a
 * reliable way to open a URI in a default application.
 *
 * This function is called by x_show_uri().
 *
 * \param uri    URI to launch viewer for.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
static bool show_uri__win32 (const char *uri, GError **error)
{
  /* On Windows, we need to use ShellExecute because allegedly GIO
   * doesn't cope very well with Windows. :-( */

  int status;
  char *msg = NULL;

  status =
    (int) ShellExecute (NULL, /* window handle */
                        "open",
                        uri,
                        NULL, /* No parameters (not launching application) */
                        NULL, /* Inherit working directory */
                        SW_SHOWNORMAL); /* Default application display mode */
  if (status > 32) {
    return TRUE;
  }

  if (status == 0) {
    msg = geda_utility_string_strdup (_("The operating system is out of memory or resources."));
  }
  else {
    LPVOID buf;
    FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER |
                    FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS),
                   NULL,
                   status,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                   (LPTSTR) &buf,
                   0,
                   NULL);
    msg = geda_utility_string_strdup ((char *) buf);
    LocalFree (buf);
  }
  /* \bug We should specify a domain and error code. */
  g_set_error (error, 0, 0, "%s", msg);
  GEDA_FREE (msg);
  return FALSE;
}
#endif /* OS_WIN32 */

/*! \brief Launch default application for a URI.
 * \par Function Description
 * Launches the default application associated with \a uri on the host
 * platform.
 *
 * Depending on the way gEDA was configured, this may occur by one of
 * the following methods:
 *
 * Calling gtk_show_uri() to use the GIO library (default on Linux)
 * Calling the ShellExecute() Windows API call (default on Windows)
 * Running an appropriate external tool.
 *
 * \param uri  URI to launch viewer for.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool
x_show_uri (const char *uri)
{
  GError *error;

  if (uri == NULL) {
    return FALSE;
  }

  error = NULL;

#if defined (OS_WIN32) && !defined (OS_CYGWIN)

  if (!show_uri__win32 (uri, &error)) {
    if (verbose_mode) {
      const char *log_msg = _("Failed to open");
      u_log_message("%s <%s>, %s\n", log_msg, uri, error->message);
    }
    g_error_free (error);
    error = NULL;
  }
  else {
    return TRUE;
  }

#else

#if HAVE_GTK_SHOW_URI

  GdkScreen *screen = gdk_screen_get_default ();

  if (!gtk_show_uri (screen, uri, GDK_CURRENT_TIME, &error)) {
    if (verbose_mode) {
      const char *log_msg = _("Failed to open");
      u_log_message("Gtk %s <%s>, %s\n", log_msg, uri, error->message);
    }
    g_error_free (error);
    error = NULL;
  }
  else {
    return TRUE;
  }

#endif /* HAVE_GTK_SHOW_URI */

  if (!g_app_info_launch_default_for_uri(uri, NULL, &error)) {
    if (verbose_mode) {
      const char *log_msg = _("Failed to open");
      u_log_message("glib %s <%s>, %s\n", log_msg, uri, error->message);
    }
    g_error_free (error);
    error = NULL;
  }
  else {
    return TRUE;
  }

#endif /* else (OS_WIN32) ! defined */

#if defined (SHOW_URI_GIO)

  if (verbose_mode) {
    const char *log_msg = _("falling back to");
    u_log_message("%s: %s %s\n", __func__, log_msg, SHOW_URI_COMMAND);
  }

  bool spawn_status;
  char *argv[3];

  argv[0] = SHOW_URI_COMMAND;
  argv[1] = (char*) uri;
  argv[2] = NULL; /* Null-terminated */

  /* Windows use another function so we don't need to worry about *pid */
  spawn_status = g_spawn_async (NULL, /* Inherit working directory */
                               &argv,
                                NULL, /* Inherit environment */
                                G_SPAWN_SEARCH_PATH, /* Flags */
                                NULL, /* No child setup function */
                                NULL, /* No child setup function data */
                                NULL, /* No child pid */
                                &error);

  if (!spawn_status) return FALSE;

#endif /* SHOW_URI_GIO */

  return TRUE;
}
