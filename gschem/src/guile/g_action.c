/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*! \file g_action.c
 * \brief Functions relating to working with gschem actions.
 */

#include <gschem.h>
#include <geda_debug.h>

SCM g_process_action(SCM action)
{
  GschemToplevel *w_current = g_current_window ();

  char *action_str;
  action_str = scm_to_utf8_string(action);

  i_command_process(w_current, action_str, 0, NULL, ID_ORIGIN_KEYBOARD);
  free(action_str);
  return SCM_BOOL_T;
}

SCM g_process_anonymous_action()
{
  SCM   stack;
  SCM   s_subr;
  SCM   s_name;
  SCM   s_frame;
  char *action;
  char *real_action;

  stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);

  s_frame = scm_stack_ref(stack, scm_from_int(0));

  s_subr= scm_frame_procedure (s_frame);

  s_name = scm_procedure_name(s_subr);

  action = scm_to_utf8_string(scm_symbol_to_string(s_name));

  GschemToplevel *w_current = g_current_window ();

  if (*action == '%') {
    real_action = action + 1;
  }
  else {
    real_action = action;
  }

  i_command_process(w_current, real_action, 0, NULL, ID_ORIGIN_SCM);
  free(action);
  return SCM_BOOL_T;
}

/*!
 * \brief Evaluate a gschem action by name.
 * \par Function Description
 *  Evaluates the action named \a action_name, which should be a UTF-8
 *  string naming a symbol in the user module.  If evaluating the
 *  action fails, prints a message to the log and returns FALSE;
 *  otherwise, returns TRUE.
 *
 * \param w_current    Current gschem toplevel structure.
 * \param action_name  Name of action to evaluate.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool
g_action_eval_by_name (GschemToplevel *w_current, const char *action_name)
{
  SCM s_eval_action_proc;
  SCM s_expr;
  SCM s_result;
  bool result;

  if (w_current == NULL) {
    BUG_MSG ("w_current = NULL");
    return FALSE;
  }
  if (action_name == NULL) {
    BUG_MSG ("action = NULL");
    return FALSE;
  }

  scm_dynwind_begin (0);
  g_dynwind_window (w_current);

  /* Get the eval-action procedure */
  s_eval_action_proc =

#if HAVE_SCM_C_PUBLIC_VARIABLE

    scm_variable_ref (scm_c_public_variable ("gschem action", "eval-action!"));

#else

    scm_variable_ref (scm_c_module_lookup (scm_c_resolve_module ("gschem action"),
                                                                 "eval-action!"));
#endif

  /* Build expression to evaluate */
  /* FIXME use SCM_SYMBOL for quote */
  s_expr = scm_list_2 (s_eval_action_proc,
                       scm_list_2 (scm_from_utf8_symbol ("quote"),
                                   scm_from_utf8_symbol (action_name)));

  /* Evaluate and get return value */
  s_result = g_evaluate_scm_action (s_expr);
  result = scm_is_true (s_result);

  scm_dynwind_end ();
  return result;
}

/*!
 * \brief Get the action position.
 * \par Function Description
 *  Retrieves the current action position and stores it in \a x and \a y,
 *  optionally snapping it to the grid if \a snap is true. This should be
 *  interpreted as the position that the user was pointing with the mouse
 *  pointer when the current action was invoked. If there is no valid world
 *  position for the current action, returns FALSE without modifying the
 *  output variables.
 *
 *  This should be used by actions implemented in C to figure out where
 *  on the schematic the user wants them to apply the action.
 *
 *  See also the (gschem action) Scheme module.
 *
 * \param snap    If true then snap to grid.
 * \param x       Location to store x coordinate.
 * \param y       Location to store y coordinate.
 *
 * \return TRUE if current action position is set, FALSE otherwise.
 */
bool
g_action_get_position (bool snap, int *x, int *y)
{
  SCM s_action_position_proc;
  SCM s_point;
  GschemToplevel *w_current = g_current_window ();

  if (w_current == NULL) {
    BUG_MSG ("w_current = NULL");
    return FALSE;
  }

  /* Get the action-position procedure */

  s_action_position_proc =

#if HAVE_SCM_C_PUBLIC_VARIABLE

    scm_variable_ref (scm_c_public_variable ("gschem action",
                                             "action-position"));
#else

    scm_variable_ref (scm_c_module_lookup (scm_c_resolve_module ("gschem action"),
                                           "action-position"));
#endif

  /* Retrieve the action position */
  s_point = scm_call_0 (s_action_position_proc);

  if (scm_is_false (s_point)) return FALSE;

  if (x) {
    *x = scm_to_int (scm_car (s_point));
    if (snap) {
      *x = snap_grid (w_current, *x);
    }
  }
  if (y) {
    *y = scm_to_int (scm_cdr (s_point));
    if (snap) {
      *y = snap_grid (w_current, *y);
    }
  }

  return TRUE;
}
