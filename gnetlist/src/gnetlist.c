/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: gnetlist.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */

#include "../../config.h"
#include "../../version.h"

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <dirent.h>

#include <geda/geda_stat.h>
#include <gnetlist.h>
#include <libgeda/libgedaguile.h>

#include <locale.h>
#include <gettext.h>
#include <geda_debug.h>

/*!
 * \brief Quite gnetlist
 * \par Function Description
 *  This function is called before exiting gnetlist and serves
 *  to release various resources allocated by the program.
 */
void gnetlist_quit(void)
{
  GedaList *string_list;

  string_list = geda_list_new();

  s_netlist_destroy_or_report(graphical_netlist_head, string_list);

  s_netlist_destroy_or_report(netlist_head, string_list);

  geda_list_free_full(string_list);

  geda_list_unref (string_list);

  s_rename_destroy_all();

  libgeda_release();

  i_vars_free_strings();

  /* Free GSList backend_params */
  g_slist_free (backend_params);
}

void gnetlist_show_error(const char *msg1, const char *msg2, const char *msg3)
{
  const char *_ERROR = _("ERROR");
  fprintf (stderr, "%s: %s [%s]: %s\n", _ERROR, msg1, msg2, msg3);
}

void gnetlist_show_strerror(const char *msg1, const char *msg2)
{
  gnetlist_show_error (msg1, msg2, strerror (errno));
}


/*!
 * \brief Print a list of available backends.
 * \par Function Description
 *  Prints a list of available gnetlist backends by searching for files
 *  in each of the directories in the current Guile %load-path.  A file
 *  is considered to be a gnetlist backend if its basename begins with
 *  "gnet-" and ends with ".scm".
 *
 * \param pr_current  Current #GedaToplevel structure.
 */
void gnetlist_backends (GedaToplevel *pr_current)
{
  SCM s_load_path;
  GList *backend_names = NULL, *iter = NULL;

  /* Look up the current Guile %load-path */
  s_load_path = scm_variable_ref (scm_c_lookup ("%load-path"));

  while (s_load_path != SCM_EOL) {

    SCM    s_dir_name = scm_car (s_load_path);
    char  *dir_name;
    DIR   *dptr;

    /* Get directory name from Scheme */
    g_assert (scm_is_true (scm_list_p (s_load_path))); /* Sanity check */
    g_assert (scm_is_string (scm_car (s_load_path))); /* Sanity check */

    dir_name = scm_to_utf8_string (s_dir_name);

    /* Open directory */
    dptr = opendir (dir_name);
    if (dptr == NULL) {
      gnetlist_show_strerror(_("Can not open directory"), dir_name);
      continue;
    }
    free (dir_name);

    while (1) {

      struct dirent *dentry;
      char  *name;

      dentry = readdir (dptr);

      if (dentry == NULL) break;

      /* Check that filename has the right format to be a gnetlist
       * backend */
      if (!(g_str_has_prefix (dentry->d_name, "gnet-") &&
            g_str_has_suffix (dentry->d_name, ".scm")))
        continue;

      /* Copy filename and remove prefix & suffix.  Add to list of
       * backend names. */
      name = geda_utility_string_strdup (dentry->d_name + 5);
      name[strlen(name)-4] = '\0';
      backend_names = g_list_prepend (backend_names, name);
    }

    /* Close directory */
    closedir (dptr);

    s_load_path = scm_cdr (s_load_path);
  }

  /* Sort the list of backends */
  backend_names = g_list_sort (backend_names, (GCompareFunc) strcmp);

  printf ("%s:\n\n", _("List of available backends"));

  for (iter = backend_names; iter != NULL; iter = g_list_next (iter)) {
    printf ("%s\n", (char *) iter->data);
  }
  printf ("\n");

  scm_remember_upto_here_1 (s_load_path);
}

/*!
 * \brief Main Scheme(GUILE) program function.
 * \par Function Description
 *  This function is the main program called from scm_boot_guile.
 *  The function initializes languages, libgeda, all libraries and
 *  registers function used by back-ends and then attempts to load
 *  and execute the back-end specified on the command line.
 */
static void main_prog(void *closure, int argc, char *argv[])
{
  int   i;
  int   argv_index;
  char *cwd;
  char *output_filename;
  bool  defaulted_filename;
  GedaToplevel *pr_current;

  geda_utility_program_mem_set_vtable();

#if ENABLE_NLS

  setlocale (LC_ALL, "");
  setlocale (LC_NUMERIC, "C"); /* This must be the same for all locales */
  bindtextdomain ("geda-gnetlist", LOCALEDIR);
  textdomain ("geda-gnetlist");
  bind_textdomain_codeset("geda-gnetlist", "UTF-8");

#endif

  output_filename = NULL;

  argv_index = parse_commandline(argc, argv, &output_filename);

  scm_set_program_arguments (argc, argv, NULL);

  libgeda_init(argc, argv);

#if defined(__MINGW32__) && defined(DEBUG)
    printf("This is the MINGW32 port.\n\n");
#endif

  /* register guile (scheme) functions */
  g_register_funcs();

  scm_dynwind_begin (0);

  pr_current = geda_toplevel_new ();

  edascm_dynwind_toplevel (pr_current);

  geda_toplevel_set_file_open_flags(pr_current, F_OPEN_RC);

  /* Evaluate Scheme expressions that need to be run before rc files
   * are loaded. */
  scm_eval (pre_rc_list, scm_current_module ());

  g_rc_parse (argv[0], "gnetlistrc", rc_filename ? rc_filename : "gnetlistrc");

  if (list_backends) {
    gnetlist_backends(pr_current);
    exit (0);
  }

  /* create log file right away */
  /* WEH: even if logging is not enabled */
  geda_utility_log_init ("gnetlist");

  geda_log("%s %s%s.%s\n", "gEDA/gnetlist version",
                            PREPEND_VERSION_STRING,
                            PACKAGE_DOTTED_VERSION,
                            PACKAGE_DATE_VERSION);
  geda_log
  (_("gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n"));
  geda_log
  (_("This is free software, and you are welcome to redistribute it under certain\n"));
  geda_log
  (_("conditions; please see the COPYING file for more details.\n\n"));

  /* Immediately setup configuration and user params */
  i_vars_init_gnetlist_defaults ();

  if (!output_filename) {
    /* was not specified so set default output filename */
    output_filename = geda_utility_string_strdup("output.net");
    defaulted_filename = TRUE;
  }
  else {
    defaulted_filename = FALSE;
  }

  /* Evaluate the first set of Scheme expressions before we load any
   * schematic files */
  scm_eval (pre_backend_list, scm_current_module ());

  cwd = g_get_current_dir();

  i_vars_set (pr_current);

  i = argv_index;

  while (argv[i] != NULL) {

    GError *err = NULL;
    char   *filename;

    if (geda_file_get_is_path_absolute(argv[i])) {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = geda_utility_string_strdup (argv[i]);
    }
    else {
      filename = g_build_filename (cwd, argv[i], NULL);
    }

    if (!quiet_mode) {
      geda_log ("%s: \"%s\"\n",  _("Loading schematic"), filename);
    }

    geda_struct_page_goto (geda_struct_page_new (pr_current, filename));

    if (!geda_open_file (pr_current, pr_current->page_current, filename, &err)) {
      gnetlist_show_error(_("load failed"), filename, err->message);
      g_error_free (err);
      GEDA_FREE (filename);
      GEDA_FREE(output_filename);
      exit(2);
    }

    i++;
    GEDA_FREE (filename);
  }

  /* Change back to the directory where we started. This is done since
   * gnetlist is a command line utility and will deposit its output in
   * the current directory. Having the output go to a different directory
   * will confuse the user (confused me, at first). */
  if (chdir (cwd)) {
    gnetlist_show_strerror(_("System could not change to directory"), cwd);
    GEDA_FREE(cwd);
    GEDA_FREE(output_filename);
    exit(1);
  }

#if DEBUG
  geda_struct_page_print_all(pr_current);
#endif

  /* Load basic gnetlist functions */
  scm_primitive_load_path (scm_from_utf8_string ("gnetlist.scm"));

  if (guile_proc) {

    SCM   s_backend_path;
    char *str;

    /* Search for backend scm file in load path */
    str = geda_sprintf("gnet-%s.scm", guile_proc);
    s_backend_path = scm_sys_search_load_path (scm_from_locale_string (str));
    GEDA_FREE (str);

    /* If it could not be found, fail. */
    if (scm_is_false (s_backend_path)) {

      const char *msg1 = _("ERROR: Could not find backend");
      const char *msg2 = _("for a full list of available backends");

      fprintf (stderr,"%s \"%s\" %s\n", msg1, guile_proc, _("in load path"));
      fprintf (stderr, "\n%s '%s --list-backends' %s.\n", _("Run"), argv[0], msg2);

      GEDA_FREE(cwd);
      GEDA_FREE(output_filename);
      exit (1);
    }

    /* Load backend code. */
    scm_primitive_load (s_backend_path);

    /* Evaluate second set of Scheme expressions. */
    scm_eval (post_backend_list, scm_current_module ());

    /* This is a kludge to make sure spice mode gets set. Is Hacked by SDB
     * to allow spice netlisters of arbitrary name as long as they begin with
     * "spice".  For example, this spice netlister is valid: "spice-sdb".
     */
    if (!strncmp(guile_proc, "spice", 5)) {
      netlist_mode = SPICE;
    }
  }

  s_rename_init();

  if (g_list_length(geda_toplevel_get_pages (pr_current)) > 0) {

    s_traverse_process (pr_current);

    /* Change back to the directory where we started AGAIN. This is done
     * because the call to geda_struct_page_goto in s_hierarchy_traverse
     * could have changed the current working directory. */
    if (chdir (cwd)) {
      /* Error occurred with chdir */
      gnetlist_show_strerror(_("System could not change to directory"), cwd);
      GEDA_FREE(cwd);
      GEDA_FREE(output_filename);
      exit(1);
    }
  }

  if (!defaulted_filename) {

    char *path;

    errno = 0;
    path  = geda_get_dirname(output_filename);

    /* Check if a path was included in the output file name */
    if (strlen(path) > 1) {

      if (!geda_file_get_is_path_absolute(path)) {

        char   *name_norm;
        GError *err = NULL;

        name_norm = geda_normalize_filename (path, &err);

        GEDA_FREE(name_norm); /* Did not need, checking for error */

        if (err) {

          if (g_error_matches (err, EDA_ERROR, ENOENT)) {

            g_clear_error (&err);

            /* Attempt to create the directories */
            if (geda_create_path (path, S_IRWXU | S_IRWXG)) {

              const char *_Path = _("Path");
              const char *msg   = _("is not accessible");

              fprintf(stderr, "%s \"%s\" %s: %s\n",
                              _Path, path, msg, strerror(errno));
              GEDA_FREE(path);
              GEDA_FREE(cwd);
              GEDA_FREE(output_filename);
              exit(2);
            }
          }
          else {
            gnetlist_show_strerror(_("file"), output_filename);
            GEDA_FREE(path);
            GEDA_FREE(cwd);
            GEDA_FREE(output_filename);
            exit(2);
          }
        }
      }
      else {  /* path is absolute, so check it */

        if (!g_file_test(path, G_FILE_TEST_IS_DIR)) {

          /* Does not exist so attempt to create the path */
          if (geda_create_path (path, S_IRWXU | S_IRWXG)) {

            const char *_Path = _("Path");
            const char *msg   = _("is not accessible");

            fprintf(stderr, "%s \"%s\" %s: %s\n",
                            _Path, path, msg, strerror(errno));
            GEDA_FREE(path);
            GEDA_FREE(cwd);
            GEDA_FREE(output_filename);
            exit(2);
          }
        }
      }
    }

    GEDA_FREE(path);
  }
  GEDA_FREE(cwd);

  errno = 0;

  if (access(output_filename, W_OK) == -1) {

    if (errno != ENOENT) {
      gnetlist_show_strerror(_("Could not create"), output_filename);
      GEDA_FREE(output_filename);
      exit(2);
    }
  }

  /* Run post-traverse code. */
  scm_primitive_load_path (scm_from_utf8_string ("gnetlist-post.scm"));

  if (guile_proc) {

    char *eval;

    /* Check size here hack */
    eval = geda_sprintf ("(%s \"%s\")", guile_proc, output_filename);

    /* Execute the back-end passing the name of the output file */
    scm_c_eval_string (eval);

    GEDA_FREE (eval);
  }
  else if (interactive_mode) {
    scm_c_eval_string ("(set-repl-prompt! \"gnetlist> \")");
    scm_shell (0, NULL);
  }
  else {
    printf(_("Either specify a backend to execute or interactive mode!\n"));
  }

  scm_dynwind_end();

  geda_struct_page_delete_list(pr_current);

  geda_toplevel_struct_release(pr_current);

  gnetlist_quit();

  GEDA_FREE(output_filename);
}

/*!
 * \brief Main executable entrance point.
 * \par Function Description
 *  This is the main function for gnetlist. The function sets up the Scheme
 *  (GUILE) environment and passes control to via scm_boot_guile to the
 *  #main_prog function.
 */
int main(int argc, char *argv[])
{
#if ENABLE_NLS

  setlocale(LC_ALL, "");
  setlocale(LC_NUMERIC, "C");
  bindtextdomain("geda-gnetlist", LOCALEDIR);
  textdomain("geda-gnetlist");
  bind_textdomain_codeset("geda-gnetlist", "UTF-8");

#endif

  scm_boot_guile (argc, argv, main_prog, 0);

  return 0;
}
