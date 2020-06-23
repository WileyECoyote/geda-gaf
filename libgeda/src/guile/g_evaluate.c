/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
#include "../../../config.h"
#include <stdio.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <libgeda_priv.h>

static void process_error_stack (SCM s_stack, SCM s_key, SCM s_args, GError **err);

/*! \brief Process a Scheme error into the log and/or a GError
 * \par Function Description
 * Process a captured Guile exception with the given \a s_key and \a
 * s_args, and optionally the stack trace \a s_stack. The stack trace
 * and source location are logged, and if a GError return location \a
 * err is provided, it is populated with an informative error message.
 */
static void
process_error_stack (SCM s_stack, SCM s_key, SCM s_args, GError **err) {

  char *long_message;
  char *short_message;

  SCM s_port, s_subr, s_message, s_message_args, s_rest, s_location;

  /* Split s_args up */
  s_rest = s_args;
  s_subr = scm_car (s_rest);         s_rest = scm_cdr (s_rest);
  s_message = scm_car (s_rest);      s_rest = scm_cdr (s_rest);
  s_message_args = scm_car (s_rest); s_rest = scm_cdr (s_rest);

  /* Capture short error message */
  s_port = scm_open_output_string ();
  scm_display_error_message (s_message, s_message_args, s_port);
  short_message = scm_to_utf8_string (scm_get_output_string (s_port));
  scm_close_output_port (s_port);

  /* Capture long error message (including possible backtrace) */
  s_port = scm_open_output_string ();

  if (scm_is_true (scm_stack_p (s_stack))) {
    scm_puts (_("\nBacktrace:\n"), s_port);
    scm_display_backtrace (s_stack, s_port, SCM_BOOL_F, SCM_BOOL_F);
  }

#ifdef HAVE_SCM_DISPLAY_ERROR_FRAME

  s_location =
    scm_is_true (s_stack) ? scm_stack_ref (s_stack, SCM_INUM0) : SCM_BOOL_F;

#elif defined HAVE_SCM_DISPLAY_ERROR_STACK

  s_location = s_stack;

#else

  s_location = SCM_BOOL_F;

#endif

  scm_display_error (s_location, s_port, s_subr,
                     s_message, s_message_args, s_rest);

  long_message = scm_to_utf8_string (scm_get_output_string (s_port));

  scm_close_output_port (s_port);

  /* Send long message to log */
  u_log_message ("%s", long_message);

  /* Populate any GError */
  g_set_error (err, EDA_ERROR, EDA_ERROR_SCHEME, "%s", short_message);
}

/* Pre-unwind handler called in the context in which the exception was
 * thrown. */
static SCM protected_pre_unwind_handler (void *data, SCM key, SCM args)
{
  /* Capture the stack trace */
  *((SCM *) data) = scm_make_stack (SCM_BOOL_T, SCM_EOL);

  return SCM_BOOL_T;
}

/* Post-unwind handler called in the context of the catch expression.
 * This actually does the work of parsing the stack and generating log
 * messages. */
static SCM protected_post_unwind_handler (void *data, SCM key, SCM args)
{
  /* The stack was captured pre-unwind */
  SCM s_stack = *(SCM *) data;

  process_error_stack (s_stack, key, args, NULL);

  return SCM_BOOL_F;
}

/* Actually carries out evaluation for protected eval */
static SCM protected_body_eval (void *data)
{
  SCM args = *((SCM *)data);
  return scm_eval (scm_car (args), scm_cadr (args));
}

/*! \brief Evaluate a Scheme expression safely.
 *  \par Function Description
 *
 *  Often a libgeda program (or libgeda itself) will need to call out
 *  to Scheme code, for example to load a Scheme configuration file.
 *  If an error or exception caused by such code goes uncaught, it
 *  locks up the Scheme interpreter, stopping any further Scheme code
 *  from being run until the program is restarted.
 *
 *  This function is equivalent to scm_eval (), with the important
 *  difference that any errors or exceptions caused by the evaluated
 *  expression \a exp are caught and reported via the libgeda logging
 *  mechanism.  If an error occurs during evaluation, this function
 *  returns SCM_BOOL_F. If \a module_or_state is undefined, uses the
 *  current interaction environment.
 *
 *  \param exp             Expression to evaluate
 *  \param module_or_state Environment in which to evaluate \a exp
 *
 *  \returns Evaluation results or SCM_BOOL_F if exception caught.
 */
SCM g_evaluate_scm_protected (SCM exp, SCM module_or_state)
{
  SCM stack = SCM_BOOL_T;
  SCM body_data;
  SCM result;

  if (scm_is_eq (module_or_state, SCM_UNDEFINED)) {
    body_data = scm_list_2 (exp, scm_interaction_environment ());
  }
  else {
    body_data = scm_list_2 (exp, module_or_state);
  }

  result = scm_c_catch (SCM_BOOL_T,
                        protected_body_eval,           /* catch body */
                        &body_data,                    /* body data */
                        protected_post_unwind_handler, /* post handler */
                        &stack,                        /* post data */
                        protected_pre_unwind_handler,  /* pre handler */
                        &stack                         /* pre data */
                        );

  scm_remember_upto_here_2 (body_data, stack);
  return result;
}

/* -------------------------- STRINGS ---------------------------*/

/* Actually carries out evaluation for protected eval-string */
static SCM protected_body_eval_string (void *data)
{
  SCM str = *((SCM *)data);
  return scm_eval_string (str);
}

/*! \brief Evaluate a string as a Scheme expression safely
 *  \par Function Description
 *
 *  Evaluates a string similarly to scm_eval_string(), but catching
 *  any errors or exceptions and reporting them via the libgeda
 *  logging mechanism.
 *
 *  See also g_evaluate_scm_protected() and g_evaluate_c_string_protected().
 *
 *  \param str  String to evaluate.
 *
 *  \returns Evaluation results or SCM_BOOL_F if exception caught.
 */
SCM g_evaluate_scm_string_protected (SCM str)
{
  SCM stack = SCM_BOOL_T;
  SCM result;

  result = scm_c_catch (SCM_BOOL_T,
                        protected_body_eval_string,    /* catch body */
                        &str,                          /* body data */
                        protected_post_unwind_handler, /* post handler */
                        &stack,                        /* post data */
                        protected_pre_unwind_handler,  /* pre handler */
                        &stack                         /* pre data */
                        );

  scm_remember_upto_here_1 (stack);

  return result;
}

/* --------------------------- ACTION ---------------------------*/

/* Actually carries out evaluation for action eval */
static SCM action_body_eval (void *data)
{
  SCM args = *((SCM *)data);
  return scm_eval (scm_car (args), scm_cadr (args));
}

SCM  g_evaluate_scm_action (SCM action)
{
  SCM stack = SCM_BOOL_T;
  SCM body_data;
  SCM result;

  body_data = scm_list_2 (action, scm_interaction_environment ());

  result = scm_c_catch (SCM_BOOL_T,
                        action_body_eval,              /* catch body */
                        &body_data,                    /* body data */
                        protected_post_unwind_handler, /* post handler */
                        &stack,                        /* post data */
                        protected_pre_unwind_handler,  /* pre handler */
                        &stack                         /* pre data */
                        );

  scm_remember_upto_here_2 (body_data, stack);
  return result;
}

/* ---------------------------- FILE ----------------------------*/

/* Data to be passed to g_evaluate_scheme_file()'s worker functions. */
struct g_evaluate_scheme_file_data_t
{
  SCM stack;
  SCM filename;
  GError *err;
};

/* Body function for g_evaluate_scheme_file(). Simply loads the specified
 * file. */
static SCM g_evaluate_scheme_file__body (struct g_evaluate_scheme_file_data_t *data)
{
  return scm_primitive_load (data->filename);
}

/* Post-unwind handler for g_evaluate_scheme_file(). Processes the stack captured
 * in the pre-unwind handler. */
static SCM
g_evaluate_scheme_file__post_handler (struct g_evaluate_scheme_file_data_t *data, SCM key, SCM args)
{
  process_error_stack (data->stack, key, args, &data->err);
  return SCM_BOOL_F;
}

/* Pre-unwind handler for g_evaluate_scheme_file().  Captures the Guile stack for
 * processing in the post-unwind handler. */
static SCM
g_evaluate_scheme_file__pre_handler (struct g_evaluate_scheme_file_data_t *data, SCM key, SCM args)
{
  data->stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  return SCM_BOOL_F;
}

/*! \brief Evaluate a string as a Scheme expression safely
 *  \par Function Description
 *
 *  Evaluates string like scm_c_eval_string().  Simple wrapper for
 *  g_evaluate_scm_string_protected().
 *
 *  \param str  String to evaluate.
 *
 *  \returns Evaluation results or SCM_BOOL_F if exception caught.
 */
SCM g_evaluate_c_string_protected (const char *str) {

  SCM s_str;

  g_return_val_if_fail ((str != NULL), SCM_BOOL_F);

  s_str = scm_from_utf8_string (str);

  return g_evaluate_scm_string_protected (s_str);
}

/*!
 * \brief Load a Scheme file, catching and logging errors.
 * \par Function Description
 * Loads \a filename, catching any uncaught errors and logging them.
 *
 * \param filename  The file name of the Scheme file to load.
 * \param err       Return location for errors, or NULL.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool g_evaluate_scheme_file (const char *filename, GError **err)
{
  struct g_evaluate_scheme_file_data_t data;
  bool   result;

  if (filename == NULL) {
    result = FALSE;
  }
  else {

    char *file_directory;

#if defined (OS_WIN32_NATIVE)

    char *file_name;

    file_name = geda_strdup (filename);
    geda_utility_string_strstr_rep (file_name, "/", DIR_SEPARATOR_S);
    data.filename = scm_from_utf8_string (file_name);
    GEDA_FREE(file_name);

#else

    data.filename = scm_from_utf8_string (filename);

#endif

    data.stack = SCM_BOOL_F;
    data.err   = NULL;

    /* Before we load the file, first cd into file's directory. */
    file_directory = geda_file_path_get_dirname (filename);

    if (file_directory == NULL) {
      result = FALSE;
    }
    else {

      const char *err_dir = _("libgeda could not %s directory to %s:%s");

      char *saved_cwd;
      bool  dir_okay;

      dir_okay  = TRUE;
      result    = FALSE;
      saved_cwd = getcwd(0,0);

      if (strcmp(saved_cwd, file_directory)) {

        if (chdir (file_directory)) { /* Error occurred with chdir */

          const char *msg_change  = _("change");

          fprintf(stderr, err_dir, msg_change, file_directory, strerror (errno));

          dir_okay = FALSE;
        }
      }
      else {
        GEDA_FREE(saved_cwd);
      }

      if (dir_okay) {

        scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);

        scm_c_catch (SCM_BOOL_T,
                    (scm_t_catch_body)    g_evaluate_scheme_file__body, &data,
                    (scm_t_catch_handler) g_evaluate_scheme_file__post_handler, &data,
                    (scm_t_catch_handler) g_evaluate_scheme_file__pre_handler, &data);

        scm_dynwind_end ();

        /* If no error occurred, indicate success. */
        if (data.err == NULL) {
          result = TRUE;
        }
        else { /* otherwise propagate the error */
          if (err) {
            g_propagate_error (err, data.err);
          }
          else {

            const char *err_msg = _("Error processing");

            fprintf(stderr, "%s %s: %s\n", err_msg, filename, data.err->message);
          }
        }

        if (saved_cwd) {

          if (chdir (saved_cwd)) {

            const char *msg_restore = _("restore");

            fprintf(stderr, err_dir, msg_restore, saved_cwd, strerror (errno));
          }
          geda_free(saved_cwd);
        }
      }
      geda_free(file_directory);
    }
  }
  return result;
}
