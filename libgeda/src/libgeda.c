/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
#define GLIB_DISABLE_DEPRECATION_WARNINGS
#include <config.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STRARG_H
#include <stdarg.h>
#endif

#include <libgeda_priv.h>
#include <libgedaguile.h>

/*! \brief libgeda Parse command-line options.
 * \par Function Description
 * Parse command line options
 *
 * \param argc Number of command-line arguments.
 * \param argv Array of command-line arguments.
 */
static void parse_args (int argc, char **argv)
{
  int i;

  int quiet_mode   = FALSE;
  int verbose_mode = FALSE;

  for (i = 1; i < argc; ++i) {

    char *opt = argv[i];

    if (*opt == '-') {

      ++opt;

      if (*opt == '-') {
        ++opt;
      }

      if (!strcmp (opt, "quiet") || !strcmp (opt, "q")) {
        quiet_mode=TRUE;
        continue;
      }
      else if (!strcmp (opt, "verbose") || !strcmp (opt, "v")) {
        verbose_mode++;
        continue;
      }
    }
  }

  geda_utility_log_set_quiet_mode(quiet_mode);
  geda_utility_log_set_verbose_mode(verbose_mode);
}

/*! \brief Perform Guile runtime initialization of libgeda library.
 *  \par Function Description
 *  This function is called internally by libgeda_init using the
 *  scm_with_guile function to initialize <b>Guile</b> runtime
 *  routines. This function does not require any arguments, nor
 *  does the function return a meaningful value. The argument and
 *  return pointer is a requirement of the scm_with_guile
 *  function.
 *
 *  \sa libgeda_init
 *
 */
static void *libgeda_guile_init(void *lame)
{
  g_register_rc_handlers();
  g_register_libgeda_dirs();

  edascm_init ();
  return lame;
}

/*! \brief Perform runtime initialization of libgeda library.
 *  \par Function Description
 *  This function calls "satellite" initialization functions in
 *  various modules to initialize data structures for runtime.
 *  This function should normally be called before any other
 *  libgeda functions are called. The call scm_with_guile is
 *  used to ensure we are in guile mode, regardless of whether
 *  the client is in guile mode.
 *
 */
void libgeda_init(int argc, char **argv)
{
  int lame;
  const char *env_path;

#ifdef ENABLE_NLS
  /* Initialise gettext */
  bindtextdomain (LIBGEDA_GETTEXT_DOMAIN, LOCALEDIR);
  bind_textdomain_codeset(LIBGEDA_GETTEXT_DOMAIN, "UTF-8");
#endif

  parse_args(argc, argv);

  /* Check environment for location to write logs */
  env_path = getenv ("GEDALOGS");

  if (env_path != NULL) {
    char *path = geda_sprintf ("%s", env_path);
    if (f_path_create (path, 0777 /*octal*/ ) == NO_ERROR) {
      default_log_directory = path;
    }
    else {
      fprintf (stderr, "Environment GEDALOGS invalid [%s], %s\n",
               path, strerror (errno));
      GEDA_FREE(path);
    }
  }

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  geda_set_default_logger(NULL, NULL);

  f_path_sys_data ();
  f_path_sys_config ();

  s_clib_init();
  s_slib_init();
  i_menu_init();
  geda_struct_attrib_init();
  s_conn_init();

  /* Initialize scheme even if client has not booted Guile */
  scm_with_guile(libgeda_guile_init, &lame);
}

void libgeda_release(void)
{
  f_path_free();
  s_clib_free();
  s_slib_free();
  geda_struct_attrib_free();
  s_papersizes_free();
  i_vars_libgeda_freenames();
}
