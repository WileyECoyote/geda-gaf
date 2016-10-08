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

#include <config.h>
#include <version.h>

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

/*! \brief Quite gnetlist
 *  \par Function Description
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

  g_slist_free (input_files);

}

/*! \brief Print a list of available backends.
 *  \par Function Description
 * Prints a list of available gnetlist backends by searching for files
 * in each of the directories in the current Guile %load-path.  A file
 * is considered to be a gnetlist backend if its basename begins with
 * "gnet-" and ends with ".scm".
 *
 * \param pr_current  Current #GedaToplevel structure.
 */
void
gnetlist_backends (GedaToplevel *pr_current)
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
    fprintf (stderr, _("ERROR: Can not open directory [%s:] %s\n"),
             dir_name, strerror (errno));
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

  printf (_("List of available backends: \n\n"));

  for (iter = backend_names; iter != NULL; iter = g_list_next (iter)) {
    printf ("%s\n", (char *) iter->data);
  }
  printf ("\n");

  scm_remember_upto_here_1 (s_load_path);
}

/*! \brief Main Scheme(GUILE) program function.
 *  \par Function Description
 *  This function is the main program called from scm_boot_guile.
 *  The function initializes languages, libgeda, all libraries and
 *  registers function used by back-ends and then attempts to load
 *  and execute the back-end specified on the command line.
 */
void main_prog(void *closure, int argc, char *argv[])
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

  /* this is a kludge to make sure that spice mode gets set. Is Hacked by SDB
   * to allow spice netlisters of arbitrary name as long as they begin with
   * "spice".  For example, this spice netlister is valid: "spice-sdb".
   */
  if (guile_proc) {
    if (strncmp(guile_proc, "spice", 5) == 0) {
      netlist_mode = SPICE;
    }
  }

  libgeda_init(argc, argv);

#if defined(__MINGW32__) && defined(DEBUG)
    printf( "This is the MINGW32 port.\n\n");
#endif

  /* register guile (scheme) functions */
  g_register_funcs();

  scm_dynwind_begin (0);

  pr_current = geda_toplevel_new ();

  edascm_dynwind_toplevel (pr_current);

  /* Evaluate Scheme expressions that need to be run before rc files
   * are loaded. */
  scm_eval (pre_rc_list, scm_current_module ());

  g_rc_parse (argv[0], "gnetlistrc", rc_filename);

  /* create log file right away */
  /* WEH: even if logging is not enabled */
  geda_utility_log_init ("gnetlist");

  u_log_message("gEDA/gnetlist version %s%s.%s\n", PREPEND_VERSION_STRING,
                PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION);
  u_log_message
  (_("gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n"));
  u_log_message
  (_("This is free software, and you are welcome to redistribute it under certain\n"));
  u_log_message
  (_("conditions; please see the COPYING file for more details.\n\n"));

  /* immediately setup configuration and user params */
  i_vars_init_gnetlist_defaults ();
  i_vars_set (pr_current);

  s_rename_init();

  if(list_backends) {
    gnetlist_backends(pr_current);
    exit (0);
  }

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

  i = argv_index;

  while (argv[i] != NULL) {

    GError *err = NULL;
    char   *filename;

    if (f_get_is_path_absolute(argv[i])) {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = geda_utility_string_strdup (argv[i]);
    }
    else {
      filename = g_build_filename (cwd, argv[i], NULL);
    }

    if (!quiet_mode) {
      u_log_message (_("Loading schematic <%s>\n"), filename);
      fprintf (stderr, _("Loading schematic [%s]\n"), filename);
    }

    geda_struct_page_goto (geda_struct_page_new (pr_current, filename));

    if (!f_open (pr_current, pr_current->page_current, filename, &err)) {
      fprintf (stderr, _("load failed [%s]: %s\n"), filename, err->message);
      g_error_free (err);
      GEDA_FREE (filename);
      GEDA_FREE(output_filename);
      exit(2);
    }

    /* collect input filenames into Global GSList for backend use */
    input_files = g_slist_append(input_files, argv[i]);

    i++;
    GEDA_FREE (filename);
  }

  /* Change back to the directory where we started.  This is done since
   * gnetlist is a command line utility and will deposit its output in
   * the current directory. Having the output go to a different directory
   * will confuse the user (confused me, at first). */
  if (chdir (cwd)) {
    fprintf (stderr, _("ERROR: File System, could change to directory [%s:] %s\n"),
             cwd, strerror (errno));
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

    /* If it couldn't be found, fail. */
    if (scm_is_false (s_backend_path)) {
      fprintf (stderr, _("ERROR: Could not find backend `%s' in load path.\n"),
               guile_proc);
      fprintf (stderr,
             _("\nRun `%s --list-backends' for a full list of available backends.\n"),
               argv[0]);
      GEDA_FREE(cwd);
      GEDA_FREE(output_filename);
      exit (1);
    }

    /* Load backend code. */
    scm_primitive_load (s_backend_path);

    /* Evaluate second set of Scheme expressions. */
    scm_eval (post_backend_list, scm_current_module ());
  }

  if (g_slist_length(input_files) > 0) {

    s_traverse_process (pr_current);

    /* Change back to the directory where we started AGAIN. This is done
     * because the call to geda_struct_page_goto in s_hierarchy_traverse could have
     * changed the current working directory. */
    if (chdir (cwd)) {
      /* Error occured with chdir */
      fprintf (stderr, _("ERROR: File System, could change to directory [%s:] %s\n"),
      cwd, strerror (errno));
      GEDA_FREE(cwd);
      GEDA_FREE(output_filename);
      exit(1);
    }
  }

  if (!defaulted_filename) {

    char *path;

    errno = 0;
    path  = f_path_get_dirname(output_filename);

    /* Check if a path was included in the output file name */
    if (strlen(path) > 1) {

      if (!f_get_is_path_absolute(path)) {

        char   *name_norm;
        GError *err = NULL;

        name_norm = geda_file_sys_normalize_name (path, &err);

        GEDA_FREE(name_norm); /* Did not need, checking for error */

        if (err) {

          if (g_error_matches (err, G_FILE_ERROR, G_FILE_ERROR_NOENT)) {

            g_clear_error (&err);

            /* attempt to create the directories */
            if (f_path_create (path, S_IRWXU | S_IRWXG)) {
              fprintf(stderr, "Path \"%s\": is not accessible: %s\n", path,
                      strerror(errno));
              GEDA_FREE(path);
              GEDA_FREE(cwd);
              GEDA_FREE(output_filename);
              exit(2);
            }
          }
          else {
            fprintf (stderr, _("ERROR: file [%s:] %s\n"), output_filename,
                     strerror (errno));
            GEDA_FREE(path);
            GEDA_FREE(cwd);
            GEDA_FREE(output_filename);
            exit(2);
          }
        }
      }
      else {

        /* path is absolute, so check it */
        if (!g_file_test(path, G_FILE_TEST_IS_DIR)) {

          /* Does not exist so attempt to create the path */
          if (f_path_create (path, S_IRWXU | S_IRWXG)) {
            fprintf(stderr, "Path \"%s\": is not accessible: %s\n", path,
                    strerror(errno));
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
      fprintf(stderr,"ERROR: Could not create <%s>: %s\n", output_filename,
              strerror (errno));
      GEDA_FREE(output_filename);
      exit(2);

    }
  }

  /* Run post-traverse code. */
  scm_primitive_load_path (scm_from_utf8_string ("gnetlist-post.scm"));

  if (guile_proc) {

    char *eval;

    /* check size here hack */
    eval = geda_sprintf ("(%s \"%s\")", guile_proc, output_filename);

    /* Execute the back-end passing the name of the output file */
    scm_c_eval_string (eval);

    GEDA_FREE (eval);
    /* gh_eval_str_with_stack_saving_handler (input_str); */
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

/*! \brief Main executable entrance point.
 *  \par Function Description
 *  This is the main function for gnetlist. The function sets up the Scheme
 *  (GUILE) environment and passes control to via scm_boot_guile to the
 *  #main_prog function.
 */
int main(int argc, char *argv[])
{
    scm_boot_guile (argc, argv, main_prog, 0);
    return 0;
}
