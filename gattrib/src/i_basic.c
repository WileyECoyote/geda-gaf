/* gEDA - GPL Electronic Design Automation
 * gattrib - gEDA gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 1998-2015 Ales V. Hvezda
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file
 *
 * \todo Unused function in here, i_update_status(). File is a candidate
 *       for removal?
 */

/*------------------------------------------------------------------
 * Gattrib specific includes.  Note that include order is important.
 *------------------------------------------------------------------*/
#include <gattrib.h>

/* --- This is necessary for i_basic.c --- */
//#include "../include/x_states.h"

#include <geda_debug.h>

/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
#if 0 /* not used, but leaving it here in case we need it later */
static void i_update_status(GedaToplevel *toplevel, const char *string)
{
  if (!toplevel->status_label) {
    return;
  }

  if (string) {
    /* NOTE: consider optimizing this if same label */
    gtk_label_set(GTK_LABEL(toplevel->status_label), (char *) string);
  }
}
#endif

/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
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
 * This function is called by i_basic_show_uri().
 *
 * \param uri    URI to launch viewer for.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
static bool i_basic_show_uri__win32 (const char *uri, GError **error)
{
  /* On Windows, we need to use ShellExecute because allegedly GIO
   * doesn't cope very well with Windows. :-( */

  int status;
  char *msg = NULL;

  if (uri == NULL) {
    BUG_MSG ("uri == NULL");
    return FALSE;
  }

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
    msg = u_string_strdup (_("The operating system is out of memory or resources."));
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
    msg = u_string_strdup ((char *) buf);
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
 * \param uri  URI to launch viewer for.
 *
 * \return TRUE on success, FALSE on failure.
 */
static bool i_basic_show_uri (const char *uri)
{
  GError *error = NULL;

#if defined (OS_WIN32) && !defined (OS_CYGWIN)

  return i_basic_show_uri__win32 (uri, error);

#elif defined (SHOW_URI_GIO)

  int result;

  if(uri == NULL) {
    return FALSE;
  }

  result = g_app_info_launch_default_for_uri(uri, NULL, &error);
  if (!result) {
    if (verbose_mode) {
      u_log_message("glib Failed to open <%s>, %s\n", uri, error->message);
    }
    g_error_free (error);
    error = NULL;
  }
  else
    return result;

  if (verbose_mode) {
    u_log_message("x_show_uri: falling back to %s\n", SHOW_URI_COMMAND);
  }

#endif

  bool spawn_status;
  char *argv[3];

  argv[0] = SHOW_URI_COMMAND;
  argv[1] = (char *) uri;
  argv[2] = NULL; /* Null-terminated */

  /* Windows use another function so we don't need to worry about *pid */
  spawn_status = g_spawn_async (NULL, /* Inherit working directory */
                               argv,
                               NULL, /* Inherit environment */
                               G_SPAWN_SEARCH_PATH, /* Flags */
                               NULL, /* No child setup function */
                               NULL, /* No child setup function data */
                               NULL, /* No child pid */
                               &error);

  if (!spawn_status) return FALSE;

  return TRUE;
}

/*! \brief Launch default application for a URI.
 * \par Function Description
 * Launches the default application associated with \a html_file
 * on the host platform.
 *
 * \param html_file  HTML file to launch in viewer.
 *
 * \return TRUE on success, FALSE on failure.
 */
void i_show_wiki_help(const char *html_file)
{
  char *pathname = NULL;

  pathname = g_build_filename (f_path_sys_doc (), "wiki", html_file, NULL);

  if (pathname) {

    if (!i_basic_show_uri (pathname)) {
      u_log_message(_("Check: path \"%s\"\n"), pathname);
    }

    GEDA_FREE(pathname);
  }
}
