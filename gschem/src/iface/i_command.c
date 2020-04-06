/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_command.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: February, 02, 2013
 *
 */
//#define PERFORMANCE 1

#include <gschem.h>

#include <i_command.h>
#include <x_menus.h>

#include <geda_dialogs.h>
#include <geda/geda_help.h>

#ifdef PERFORMANCE
#include <gschem_diagnostics.h>
#endif

#include <geda_debug.h>

#define LAST_ACTION cmd_do_show_about

#define USE_POSIX
#define MAX_THREADS              12
#define MAX_THREADS_UNUSED        3
#define MAX_THREADS_IDLE_TIME  1000   /* microseconds */
#define WAIT_THREADS_IDLE_TIME    5
#define TASK_WAIT_INTERVAL      100   /* microseconds */
#define MAX_WAIT_FOR_TASK        10   /* TASK_WAIT_INTERVAL */

/* These are just to reduce lines lengths */
#define CairoRenderer w_current->cairo_renderer
#define Toplevel      w_current->toplevel

#define COMMAND(symbol, repeat, aflag,  func) [ cmd_##func ] = \
{ ACTION(symbol), repeat, 0, aflag, i_cmd_##func, 0, {0, 0}, 0, 0, 0},

#ifdef PERFORMANCE

static GedaMutex (i_lock_thread_diagnostics);

static bool performance_diagnostics = FALSE;
static bool thread_diagnostics      = FALSE;

static void set_thread_diagnostics(int value)
{
  g_mutex_lock((GMutex*)&i_lock_thread_diagnostics);
    thread_diagnostics = value;
  g_mutex_unlock((GMutex*)&i_lock_thread_diagnostics);
}

static int get_thread_diagnostics()
{
  int ret_val;
  g_mutex_lock((GMutex*)&i_lock_thread_diagnostics);
    ret_val = thread_diagnostics;
  g_mutex_unlock((GMutex*)&i_lock_thread_diagnostics);
  return ret_val;
}

#endif /*PERFORMANCE */

typedef struct {
  union {
  void (*F1)(void*);
  void (*F2)(void*, void*);
  } func;
  void* arg1;
  void* arg2;
} gschem_task;

static struct {
   char *name;                               /* name = command = action str */
   char *repeat;                             /* if repeatable, then - str for status */
   unsigned char status;                     /* set inside handler, used for blocking */
   unsigned char aflag;                      /* see note #1 in i_command.h */
   void (*func) (GschemToplevel *w_current); /* ptr to action handler function */
   int   who;                                /* enumerated caller */

   struct {
   int     x;
   int     y;
   } point;

   char *icon_id;                            /* icon assigned to action str */
   int   narg;
   unsigned char *sarg;
   GschemToplevel *w_current;
} command_struc[COMMAND_COUNT] = {
 [ cmd_unknown ] = { "unknown", 0, 0, 0, 0, 0, {0, 0}, 0, 0, 0},

 #include <i_command.h>
};

#define CMD(symbol)cmd_##symbol
#define CMD_FUNC(symbol)command_struc[cmd_##symbol].func
#define CMD_OPTIONS(symbol)command_struc[cmd_##symbol].sarg
#define CMD_NAME(symbol)command_struc[cmd_##symbol].name
#define CMD_WHO(symbol)command_struc[cmd_##symbol].who
#define CMD_POINT &(command_struc[cmd_##symbol].point.x)
#define CMD_X(symbol)command_struc[cmd_##symbol].point.x
#define CMD_Y(symbol)command_struc[cmd_##symbol].point.y
#define CMD_INTEGER(symbol)command_struc[cmd_##symbol].narg
#define CMD_STATUS(symbol)command_struc[cmd_##symbol].status

#define GET_G(var)command_struc[cmd_##var].narg=var;
#define GET_R(var)command_struc[cmd_##var].narg=CairoRenderer->var;
#define GET_T(var)command_struc[cmd_##var].narg=Toplevel->var;
#define GET_W(var)command_struc[cmd_##var].narg=w_current->var;

#define SHOW_VARIABLE(name, type) GET_##type(name) \
  geda_log("%s <%s> %s <%d>\n", _("current value of"), #name, _("is"), CMD_INTEGER(name));

/* Anonymous Static Mutex */
static GedaMutex (i_lock_last_command);
static GedaMutex (i_lock_action_status);

static GThreadPool  *CommandPool     = NULL;
static int           is_engaged      = -1;
static int           last_command    =  0;

/*!
 * \brief Retrieve command_struc[i].status value
 * \par Function Description
 *  All action commands accepting arguments utilize macros such
 *  as BEGIN_COMMAND, which call BlockThread() to set the status
 *  flag. Subsequent threads will be blocked from executing the
 *  action command until the status flag is cleared by calling
 *  set_action_status, which in incorporated in the EXIT_COMMAND
 *  macro. This function retrieves the current status utilizing
 *  mutex blocking.
 *
 * \see BlockThread
 */
static int get_action_status(int index)
{
  int ret_val;
  g_mutex_lock((GMutex*)&i_lock_action_status);
    ret_val = command_struc[index].status;
  g_mutex_unlock((GMutex*)&i_lock_action_status);
  return ret_val;
}

/*!
 * \brief Set command_struc[i].status value
 * \par Function Description
 *  This function sets the status flag in the command_struc
 *  at the given \a index to the given \a state.
 *
 * \see BEGIN_COMMAND and EXIT_COMMAND
 *
 * \sa get_action_status
 */
static void set_action_status(int index, int state)
{
  g_mutex_lock((GMutex*)&i_lock_action_status);
    command_struc[index].status = state;
  g_mutex_unlock((GMutex*)&i_lock_action_status);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static int get_last_command()
{
  int ret_val;
  g_mutex_lock((GMutex*)&i_lock_last_command);
    ret_val = last_command;
  g_mutex_unlock((GMutex*)&i_lock_last_command);
  return ret_val;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static void set_last_command(int value)
{
  g_mutex_lock((GMutex*)&i_lock_last_command);
    last_command = value;
  g_mutex_unlock((GMutex*)&i_lock_last_command);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static bool i_command_dispatch(gschem_task *task)
{
  scm_dynwind_begin (0);

  g_dynwind_window (task->arg1); /* w_current */

  if (task->arg2) { task->func.F2(task->arg1,task->arg2); }
  else { task->func.F1(task->arg1); }

  scm_dynwind_end ();
  GEDA_FREE(task);

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static
void i_command_router(char *command, GschemToplevel *w_current)
{
  inline void route(int i) {

    /* see note #1 in i_command.h */
    if ((command_struc[i].aflag & ActionTaskMask) == USE_MAIN_LOOP) {

      gschem_task *task;

      task = g_malloc(sizeof(gschem_task));
      task->func.F1 = (void*)command_struc[i].func;
      task->arg1 = command_struc[i].w_current;
      task->arg2 = NULL;
      geda_main_context_invoke (NULL, (void*) i_command_dispatch, task);
    }
    else /* USE_WORKER_THREAD */ {
      gschem_threads_enter();
      scm_init_guile();
      command_struc[i].func(command_struc[i].w_current);
      gschem_threads_leave();
    }
  }

#if PERFORMANCE

  if (get_thread_diagnostics() == TRUE ) {
    unsigned int thread_id;
    thread_id = POINTER_TO_UINT (command);
    fprintf(stderr, "[Router] ---> entered thread:%2.2u\n", thread_id);
    g_usleep (WAIT_THREADS_IDLE_TIME * 1000);
    fprintf(stderr, "[Router] <--- exiting thread:%2.2u\n", thread_id);
  }
  else {

#endif

    int accelerator = get_last_command();

    /* This should almost always happen */
    if (geda_strequal(command_struc[accelerator].name, command)) {
      route(accelerator);
    }
    else {

      int i;

      /* TODO: Add debug and find out if this is ever executed */
      for (i = 1; i < COMMAND_COUNT; i++) {
        if (geda_strequal(command_struc[i].name, command)) {
          route(i);
          break;
        }
      }
    }
#if PERFORMANCE
  }
#endif
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief Enable multitasking mode
 *  \par Function Description
 */
void i_command_engage(GschemToplevel *w_current)
{
  GError *err = NULL;

  CommandPool = g_thread_pool_new ((void*)i_command_router, w_current,
                                    MAX_THREADS, FALSE, &err);

  if (err != NULL) {
    fprintf (stderr, "Error: failed to create thread pool: %s\n", err->message);
    g_error_free (err);
    is_engaged = FALSE;   /* Fallback to single thread "safe" mode if no pool */
  }
  else {
    is_engaged = TRUE;
    /* According to helgrind, order if important here! */
    g_thread_pool_set_max_unused_threads (MAX_THREADS_UNUSED);
    g_thread_pool_set_max_idle_time (MAX_THREADS_IDLE_TIME);
  }
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief Disable multitasking mode
 *  \par Function Description
 */
void i_command_disengage(bool immediate, bool wait_return)
{
  if (CommandPool) {
    g_thread_pool_free(CommandPool, immediate, wait_return);
  }
  is_engaged = FALSE;

  return;
}

/* Command Interface Helpers */

/*!
 * \brief Get List of actions
 * \par Function Description
 *  Indexes through the main command table and adds each action
 *  member to the \a list.
 *
 * \param [out] list Pointer of GList to be populated
 */
void i_command_get_action_list(GList** list)
{
  int i;

  for (i = 1; i <= LAST_ACTION; i++)
    *list = g_list_prepend(*list, command_struc[i].name);
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
const char *i_command_get_action_icon (const char *command)
{
  const char *icon_id = NULL;
  static int  icache  = 1;

  /* Hack for buffer */
  if (strstr(command, "buffer-copy") != 0) {
    icon_id = command_struc[cmd_do_copy_clip].icon_id;
  }
  else if (strstr(command, "buffer-cut") != 0) {
    icon_id = command_struc[cmd_do_cut_clip].icon_id;
  }
  else if (strstr(command, "buffer-paste") != 0) {
    icon_id = command_struc[cmd_do_paste_clip].icon_id;
  }
  else {

    int  index;

    for (index = icache; index <= LAST_ACTION; index++) {

      if (geda_strequal(command_struc[index].name, command)) {
        if (command_struc[index].icon_id) {
          icon_id = command_struc[index].icon_id;
          icache = index;
          break;
        }
      }
    }

    if (!icon_id) {
      icache = index;
      for (index = 1; index < icache; index++) {
        if (geda_strequal(command_struc[index].name, command)) {
          if (command_struc[index].icon_id) {
            icon_id = command_struc[index].icon_id;
            icache = index;
            break;
          }
        }
      }
    }
  }
  return icon_id;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void i_command_get_command_list(GList** list)
{
  int i;

  for (i = 1; i < COMMAND_COUNT; i++)
    *list = g_list_prepend(*list, (char*)command_struc[i].name);
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
bool i_command_is_valid(const char *command)
{
  int i;
  bool result = FALSE;

  for (i = 1; i < COMMAND_COUNT; i++) {
    if (geda_strequal(command_struc[i].name, command)) {
      result = TRUE;
      break;
    }
  }

  return result;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
bool i_command_map_icon  (const char *command, const char *icon)
{
  int i;
  bool result = FALSE;

  for (i = 1; i <= LAST_ACTION; i++) {
    if (geda_strequal(command_struc[i].name, command)) {
      if (command_struc[i].icon_id) {
        GEDA_FREE(command_struc[i].icon_id);
      }
      if (icon) {
        command_struc[i].icon_id = geda_strdup(icon);
      }
      else {
        command_struc[i].icon_id = NULL;
      }
      result = TRUE;
      break;
    }
  }

  if (!result){
    /* Hack for buffer */
    if (strstr(command, "buffer-copy") != 0) {
      result = TRUE;
    }
    else if (strstr(command, "buffer-cut") != 0) {
      result = TRUE;
    }
    else if (strstr(command, "buffer-paste") != 0) {
      result = TRUE;
    }
  }

  return result;
}

/*! \todo Finish function documentation!!!
 *  \brief Main Command Processor
 *  \par Function Description
 *
 */
void i_command_process(GschemToplevel *w_current, const char *command,
                       int narg, char *arg, EID_ACTION_ORIGIN who)
{
  int i;

  for (i = 1; i < COMMAND_COUNT; i++) {

    if (geda_strequal(command_struc[i].name, command)) {

#if DEBUG
      geda_log("Processing Action: %s, at index %d.", command_struc[i].name, i);
#else
      geda_log_v("%s: %s.\n", _("Processing Action"), command_struc[i].name);
#endif

      if (command_struc[i].repeat != NULL) {
        set_last_command(i); /* save last index for recall by repeat-last */
        x_status_bar_update_middle_mouse(w_current, command_struc[i].repeat);
      }

      /* Check the repeat last command action */
      if (i == CMD(do_repeat_last)) {
        if ( !get_last_command())
          break;
        i = get_last_command(); /* Change action to the previous value */
      }

      /* Check and set pointer coordinates if task bit 3 is set */
      if ( command_struc[i].aflag & XY_ActionMask) {

        int wx, wy, check_magnet;

        if (who != ID_ORIGIN_MOUSE) {

          /*  If not mouse, then likely is keyboard, this is intended to
           *  handle all except the mouse */
          if (i_window_get_pointer_position (w_current, TRUE, &wx, &wy)) {
            command_struc[i].point.x = wx;
            command_struc[i].point.y = wy;
            check_magnet = TRUE;
          }
          else {
            /* Most likely toolbar */
            command_struc[i].point.x = 0;
            command_struc[i].point.y = 0;
            w_current->first_wx      = -1;
            w_current->first_wy      = -1;
            check_magnet = FALSE;
          }
        }
        else { /* Must have been ID_ORIGIN_MOUSE*/
          /* low levels should do this */
          SCREENtoWORLD (w_current, w_current->pointer_sx, w_current->pointer_sy, &wx, &wy);
          command_struc[i].point.x = wx;
          command_struc[i].point.y = wy;
          check_magnet = TRUE;
          i_pan_warp_cursor (w_current->drawing_area,
                             w_current->pointer_sx,
                             w_current->pointer_sy);
        }
        /* could check action but all current actions that also
         * use "hot" seem magnetic-able candidates */
        if (check_magnet && w_current->magnetic_net_mode) {
          command_struc[i].point.x = snap_grid (w_current, wx);
          command_struc[i].point.y = snap_grid (w_current, wy);
        }
      }

      /* Fill in parameter arguments for this task */
      command_struc[i].narg      = narg;
      command_struc[i].who       = who;
      command_struc[i].sarg      = (unsigned char *) geda_strdup(arg);
      command_struc[i].w_current = w_current;

#ifdef PERFORMANCE
      /* Is temporary block used for performance and memory evaluations */
      if (performance_diagnostics) {
        struct rusage usage;
        if (getrusage (RUSAGE_SELF, &usage) == 0) {
          printRusage("", &usage);
        }
        else
          fprintf(stderr, "getrusage returned error: %s\n", strerror(errno));
      }
#endif

      /* Either push task to cache of actions, or do in-line */
      if (is_engaged && !(command_struc[i].aflag & USE_INLINE_MODE)) {
        g_thread_pool_push (CommandPool, command_struc[i].name, NULL);
      }
      else {
        command_struc[i].func(w_current);
      }
      break;
    }
  }
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void i_command_shutdown(void)
{
  int i;

  i_command_disengage(FALSE, FALSE);

  for (i = 1; i < COMMAND_COUNT; i++) {
    if (command_struc[i].icon_id) {
      GEDA_FREE(command_struc[i].icon_id);
    }
  }

  /* Exit with threads unlocked */
  gschem_threads_leave();
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static inline void msg_need_select_1st(GschemToplevel *w_current)
{
  char *message = MSG_SELECT_Object_1ST;
  i_status_set_state_msg(w_current, SELECT, message);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static inline void action_err(char *var)
{
  geda_log_v("%s %s %s!\n", _("Cannot"), var, _("while inside an action!\n"));
}

#define NO_ACTION(symbol) if (w_current->inside_action) \
                          return action_err(command_struc[cmd_##symbol].repeat)

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static inline void null_err(char *var)
{
  geda_log("internal error, i_command: variable <%s> can not be NULL\n", var);
}

#define NOT_NULL(symbol) if (!symbol) return null_err(#symbol)

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static inline void BlockThread (int index)
{
  int deadman = 0;
  while (get_action_status(index)) {
    g_usleep (TASK_WAIT_INTERVAL); /* sleep for Some time for action to complete. */
    deadman++;
    if (deadman == MAX_WAIT_FOR_TASK) {
      fprintf (stderr, "Error: Command <%s> is not relinquishing status flag\n",
               command_struc[index].name);
      return;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static inline char *tokenizer( int index, int *argc, char **argv[])
{
  char *arg;
  if (command_struc[index].sarg != NULL) {
    arg  = geda_strdup((char *)command_struc[index].sarg);
    GEDA_FREE(command_struc[index].sarg);
    arg  = geda_strstr_rep(arg, "  ", " ");
   *argv = g_strsplit (g_strstrip(arg), " ", 0);
   *argc = g_strv_length(*argv);
  }
  else {
    arg = NULL;
  }

  return arg;
}

/* ------------------------ Handler Macros ---------------------- */
#define COMMAND(func) void i_cmd_##func(GschemToplevel *w_current)
#define BEGIN_COMMAND(efunc) BlockThread(cmd_##efunc); \
                             set_action_status(cmd_##efunc, 1); /* Block this action */ \
                             int  narg NOWARN_UNUSED = CMD_INTEGER(efunc); \
                             char **argv = NULL; \
                             int    argc = 0; \
                             char  *arg  NOWARN_UNUSED = tokenizer(cmd_##efunc, &argc, &argv);

#define BEGIN_W_COMMAND(efunc) NOT_NULL(w_current); \
                               BEGIN_COMMAND(efunc);

#define BEGIN_NO_ACTION(efunc) NOT_NULL(w_current); \
                               NO_ACTION(efunc); \
                               BEGIN_COMMAND(efunc);

#define EXIT_COMMAND(efunc) if (arg) { GEDA_FREE(arg); \
                                       g_strfreev ( argv);} \
                            set_action_status(cmd_##efunc, 0)

#define BEGIN_NO_ARGUMENT(efunc) GEDA_FREE(CMD_OPTIONS(efunc))
#define HOT_ACTION(symbol) ((CMD_WHO(symbol)==ID_ORIGIN_KEYBOARD) || ((CMD_WHO(symbol)==ID_ORIGIN_MOUSE) && (CMD_Y(symbol) != 0)))

#ifdef PERFORMANCE

#  include "i_diagnostics.c"

#else
int gschem_diagnostics_dialog (GschemToplevel *w_current)
{
  return 0;
}
#endif

/* -------------------- Begin Handler Functions ------------------- */

COMMAND (do_debug)
{
  BEGIN_COMMAND(do_debug);

#ifdef PERFORMANCE

  Page *p_current;
  float cpu_time[10];
  float total = 0;
  float average, per_obj;
  int   cycle, count;
  int   old_page_state;
  char *results;

  const char *msg;
  const char *normal   = "exiting performance diagnostic, resumming normal mode\n";
  const char *complete = "Test complete, resumming normal mode\n";
  //const char *linefeed = "\n";

  gtk_window_resize (w_current->main_window, 1092, 924);

  /* Get ptr to the current page */
  p_current  = gschem_toplevel_get_current_page(w_current);

  i_zoom_world_extents (w_current, geda_struct_page_get_objects(p_current), I_PAN_REDRAW);
  /*
   * geda_attrib_append_changed_hook (p_current,
   *                                        (AttribsChangedFunc) o_diagnostics_notify_attribute,
   *                                         w_current);
   */
  int test = gschem_diagnostics_dialog(w_current);

  old_page_state = geda_page_get_changed(p_current);

  switch (test) {

    case CLOSE_PERFORMANCE:
      msg = normal;
      break;

    case REDRAW_DUMP_GRIND:
      i_diagnostics_grind_dump_redraw(w_current);
      msg = "Redraw dump complete\n";
      break;

    case RUN_REDRAW_TESTS:
      printf ("Running Redraw tests, 10 cycles x %d redraws per cycle\n", NUMBER_REDRAW_TEST);
      for (cycle = 0; cycle < 10; cycle++) {
        cpu_time[cycle] = i_diagnostics_test_draw_time(w_current, NUMBER_REDRAW_TEST);
        total = total + cpu_time[cycle];
      }
      average = total / 10;
      count   = g_list_length((GList*)geda_struct_page_get_objects(p_current));
      per_obj = ((average / count) * 1000) / NUMBER_REDRAW_TEST;
      results = geda_sprintf("Average per 10 redraws= %.4f seconds, or %.5f ms per object", average, per_obj);
      printf ("file=%s, has %d objects: %s\n", p_current->filename, count, results);
      msg = complete;
      g_free(results);
      break;

    case RUN_THREAD_TESTS:
      g_thread_pool_stop_unused_threads();
      fprintf (stderr, "***** Running Thread tests *****\n");
      set_thread_diagnostics(TRUE);
      test_thread_pool();
      set_thread_diagnostics(FALSE);
      msg = "All test have expired, resumming normal mode\n";
      break;

    case RUN_UNDO_TESTS:
      count   = g_list_length(geda_struct_page_get_objects(p_current));
      if (count > NUMBER_UNDO_TEST - 1) {
        if (w_current->undo_levels < NUMBER_UNDO_TEST) {
          printf ("Warning undo levels setting=%d, number of tests=%d\n", NUMBER_UNDO_TEST, w_current->undo_levels);
        }
        printf ("file=%s, has %d objects before testing\n", p_current->filename, count);
        printf ("undo system type: %s, ", (w_current->undo_type == UNDO_DISK) ? "DISK" : "MEMORY");
        printf ("undo capacity (levels): %d, undo pan-zoom setting: %d\n", w_current->undo_levels,
                w_current->undo_panzoom);
        printf ("Running Undo tests, 10 cycles x %d Undo per cycle\n", NUMBER_UNDO_TEST);
        for (cycle = 0; cycle < 10; cycle++) {
          test_undo_randomly_delete (w_current, NUMBER_UNDO_TEST);
          cpu_time[cycle] = test_undo_time(w_current, NUMBER_UNDO_TEST);
          total = total + cpu_time[cycle];
        }
        average = total / 10;
        count   = g_list_length(geda_struct_page_get_objects(p_current));
        per_obj = ((average / count) * 1000) / NUMBER_UNDO_TEST;
        printf ("file=%s, has %d objects after testing\n", p_current->filename, count);
        results = geda_sprintf("Average per 10 undo's= %.4f seconds, or %.5f ms per Object", average, per_obj);
        printf ("%s\n", results);
        msg = complete;
        g_free(results);
      }
      else {
        printf ("Can not run tests, must have at least %d objects, count=%d\n", NUMBER_UNDO_TEST, count);
      }
      break;

    default:
      break;
  }
  printf("%s", msg);

  p_current->CHANGED = old_page_state;

#else
  geda_log("Performance diagnostic is not enable, must recompile\n");
#endif

  EXIT_COMMAND(do_debug);
}

/*!\warning: { Do not point to another CMD_OPTIONS unless you know what your doing }*/

/** \defgroup i_command_Command_Handlers Action Handler Functions
 *  @{
 * \brief Processors functions for actions
 * \par
 *  Functions in the group process all of the actions generated from
 *  various sources, such as menus, toolbars, and the keyboard. The
 *  action handlers also process command input from the console. This
 *  module does <b>NOT</b> process actions orginating from Drag-N-Drop.
 *  Each of the function below "block" themself on entry and "unblock"
 *  on exit. Pending threads will wait up to TASK_WAIT_INTERVAL before
 *  reporting the dead task and returning without executing the action.
 *  This is really for error trapping and debugging, under normal
 *  circumstances this does not happen.
 */

/** \defgroup i_command_Action_Functions Action Handler Function */

/** @brief i_cmd_do_repeat_last in i_command_Command_Handlers
 *  @remark Yes, repeat is a command, not just an action
 */
COMMAND (do_repeat_last)
{
  BEGIN_NO_ARGUMENT(do_repeat_last);
}

/** @brief i_cmd_do_file in i_command_Command_Handlers */
COMMAND (do_file)
{
  BEGIN_COMMAND(do_file);
  /*close discard export import new open print revert save save as, save all */

  int i;
  fprintf(stderr, "there were %d arguments: ", argc);
  for (i = 0; i < argc; i++)
   fprintf(stderr, "%d = \"%s\", ", i, argv[i]);
  fprintf(stderr,"\n");

  EXIT_COMMAND(do_file);
}

/* ------------------ File ---------------- */

/** \defgroup i_command_File_Actions Actions under the File Menu
 * @{
 */

/** @brief i_cmd_do_new in i_command_File_Actions */
COMMAND (do_file_new)
{
  BEGIN_W_COMMAND(do_file_new);
  Page *page;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  g_hook_run_page (w_current, NEW_PAGE_HOOK, page);

  geda_log_q ("%s \"%s\"\n", _("New page created"), geda_page_get_filename(page));

  i_zoom_world_specify (w_current, 13.0, 0, 0, ID_ORIGIN_CCODE);

  EXIT_COMMAND(do_file_new);

}

/*!
 * \brief File New Window Action
 * \brief i_cmd_do_file_new_window in i_command_Option_Actions
 * \par Function Description
 *  Action handler function for the File New Window action. This
 *  function creates a new toplevel window.
 */
COMMAND (do_file_new_window)
{
  BEGIN_NO_ARGUMENT(do_file_new_window);

  GschemToplevel  *new_window;
  GedaToplevel    *toplevel;
  Page            *page;

  new_window           = gschem_toplevel_new ();
  new_window->toplevel = geda_toplevel_new ();
  toplevel             = new_window->toplevel;

  x_window_setup (new_window);

  geda_toplevel_set_bkloader_query_func (toplevel,
                                         x_fileselect_load_backup,
                                         new_window);

  geda_toplevel_struct_set_rbounds_func (toplevel,
                                         o_text_get_rendered_bounds,
                                         new_window);

  page = x_window_open_page (new_window, NULL);
  x_window_set_current_page (new_window, page);

  geda_log_q ("%s \"%s\"\n", _("New Window created"), geda_page_get_filename(page));

}

/**   \defgroup open-files-command Open File Action
 *  @{\par This group contains functions to open documents
 *    \ingroup i_command_File_Actions
 */
/* This does not do anything productive, is a delay. The destroy
 * notifier; open_command_idle_notify, does all the work. This is
 * a low priority main-loop task, instigated after higher priority
 * main-loop task were delegated to opening files. If the loader is
 * not done by the time this function is called, we give the task
 * more time and after a second interval we destroy our source.
 */
static bool
open_command_idle_callback (void *data)
{
  IdleTaskData *packet    = data;
  bool          status    = SOURCE_CONTINUE;
  char         *last_file = g_slist_last(packet->data)->data;

  if (geda_struct_page_search(packet->w_current->toplevel, last_file)) {
    status = SOURCE_REMOVE;
  }
  else if (packet->retry == 1) {
    status = SOURCE_REMOVE;
  }
  else {
    packet->retry++;
  }

  return status;
}

/* open_command_idle_notify is a callback handler notifying
 * us that the main loop source open_command_idle_callback
 * has been destroyed, which is of no particular interest.
 * The idle threads were to release the memory associated
 * with x_fileselect_list */
static void
open_command_idle_notify (void *data)
{
  IdleTaskData *packet = data;
  GSList       *files  = packet->data;
  char    *last_file   = g_slist_last(files)->data;
  Page    *page;

  page = geda_struct_page_search(packet->w_current->toplevel, last_file);
  if (GEDA_IS_PAGE(page)) {
    x_window_set_current_page (packet->w_current, page);
    g_hook_run_page (packet->w_current, OPEN_PAGE_HOOK, page);
  }

  GSource *source;

  source = g_main_context_find_source_by_id (NULL, packet->source_id);

  if (source) {
    /* TODO: Does this happen everytime? */
    g_source_destroy (source);
  }

  /* free all the filenames in the list, and the list */
  geda_gslist_free_all (files);

  /* free the IdleTaskData structure */
  GEDA_FREE(data);
}

/** @brief i_cmd_do_open in i_command_File_Actions
 *  \par Function Description
 *  Calls x_fileselect_list to display a standard system file
 *  open dialog, if not canceled, the dialog returns a single
 *  linked list of file names. A task to be ran in the main
 *  context is created for each file. A low priority idle task
 *  is assigned to clean up details, mainly to release memory
 *  allocated by x_fileselect_list.
 *
 *  This is a worker thread that spawns main-loop threads, and
 *  this dramatically increases multi-document load performance
 *  compared to the old sequential loading.
 */
COMMAND (do_open) {
  BEGIN_W_COMMAND(do_open);

  GSList *files;

  if (NULL != (files = x_fileselect_list (w_current))) {

    IdleTaskData *packet;
    int count = 0;

    geda_toplevel_set_file_open_flags(w_current->toplevel,
                                      F_OPEN_RC | F_OPEN_CHECK_BACKUP);

    lambda (void *filename) {

      gschem_task  *task;

      task          = g_malloc(sizeof(gschem_task));
      task->func.F2 = (void*)x_window_open_page;
      task->arg1    = command_struc[cmd_do_open].w_current;
      task->arg2    = filename;
      geda_main_context_invoke (NULL, (void*)i_command_dispatch, task);
      count++;
      return FALSE;
    }
    mapcar(files);

    packet            = g_malloc(sizeof(IdleTaskData));
    packet->w_current = command_struc[cmd_do_open].w_current;
    packet->data      = files;
    packet->retry     = FALSE;
    packet->source_id = g_idle_add_full (G_PRIORITY_LOW + (10 * count),
                                         open_command_idle_callback,
                                         packet,
                                         open_command_idle_notify);
  }
  EXIT_COMMAND(do_open);
}

/** @} endgroup open-files-command */

/** @brief i_cmd_do_save in i_command_File_Actions */
/*!
 * \brief Save File As command action handler function
 * \par Function Description
 *  Save the current file to disk.
 * \note should be a flag that says whether filename is
 *       derived from untitled_name or specified by a user.
 *       Some twisted people might name their files like
 *       untitled_name. :-)
 */
COMMAND (do_save) {
  BEGIN_NO_ARGUMENT(do_save);
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

  Page *p_current;

  p_current = gschem_toplevel_get_current_page(w_current);

  if (p_current->filename == NULL) {
    w_current->force_save_as = TRUE;
  }
  else if (strstr(p_current->filename, w_current->toplevel->untitled_name)) {
    w_current->force_save_as = TRUE;
  }

  if (w_current->force_save_as) {
      x_fileselect_save (w_current);
  }
  else {
      x_window_save_page (w_current,
                          p_current,
                          p_current->filename);
  }
}

/*!
 * \brief Save File As action
 * \par Function Description
 *  This is a callback function for the File Save As API
 *  The function calls i_command to process the action.
 */
/** @brief i_cmd_do_save_as in i_command_File_Actions */
COMMAND (do_save_as) {

  BEGIN_W_COMMAND(do_save_as);
  NOT_NULL(w_current->toplevel);

  Page *p_current;
  char *old_name;

  /* Get ptr to the current page */
  p_current = gschem_toplevel_get_current_page(w_current);

 /* Make a copy of the page file name */
  old_name = geda_strdup(p_current->filename);

  x_fileselect_save (w_current);

  i_status_update_title (w_current);

  /* If the user actually changed the file name then delete the
   * the old backup file or else the file will be stranded! */
  if (strcmp(p_current->filename, old_name) != 0) {
    geda_remove_backup_file(old_name);
  }

  GEDA_FREE(old_name);
  EXIT_COMMAND(do_save_as);
}

/** @brief i_cmd_do_save_all in i_command_File_Actions */
COMMAND (do_save_all) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_NO_ARGUMENT(do_save_all);

  if (geda_struct_page_save_all(w_current->toplevel)) {
     i_status_set_state_msg(w_current, SELECT, _("Failed to Save All"));
  }
  else {
     i_status_set_state_msg(w_current, SELECT, _("Saved All"));
  }
  i_status_update_title (w_current);
  x_pagesel_update (w_current);
  i_status_update_sensitivities(w_current);
}

/** @brief i_cmd_do_save_mods in i_command_File_Actions */
COMMAND (do_save_mods) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_NO_ARGUMENT(do_save_mods);

  if (geda_struct_page_save_all_changed(w_current->toplevel)) {
     i_status_set_state_msg(w_current, SELECT, _("Error encountered, Save Failed"));
  }
  else {
     i_status_set_state_msg(w_current, SELECT, _("Saved All"));
  }
  i_status_update_title (w_current);
  x_pagesel_update (w_current);
  i_status_update_sensitivities(w_current);
}

/** @brief i_cmd_do_print in i_command_File_Actions */
COMMAND (do_print) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);
  NOT_NULL(w_current->toplevel->page_current->filename);

  BEGIN_COMMAND(do_print);

  if (output_filename) {
    x_print_setup(w_current, output_filename);
  }
  else {

    const char *extension;
          char *base;
          char *filename;
          char *ps_filename;
          Page *p_current;

    p_current = gschem_toplevel_get_current_page(w_current);
    filename  = p_current->filename;

    extension = geda_file_get_filename_ext(filename);

    /* get the base file name */
    if (strncmp(extension, SCHEMATIC_FILE_DOT_SUFFIX, 4)) {

      /* the filename ends with ".sch", remove it */
      base = geda_strndup(filename, strlen(filename) - 4);
    }
    else if (strncmp(extension, SYMBOL_FILE_DOT_SUFFIX, 4)) {

      /* the filename ends with ".sch", remove it */
      base = geda_strndup(filename, strlen(filename) - 4);
    }
    else {

      /* the filename does not end with .sch */
      base = geda_strdup (filename);
    }

    /* add ".ps" to the base filename */
    ps_filename = geda_strconcat (base, ".ps", NULL);

    GEDA_FREE(base);

    x_print_setup(w_current, ps_filename);

    GEDA_FREE(ps_filename);
  }

  EXIT_COMMAND(do_print);
}

/** @brief i_cmd_do_write_image in i_command_File_Actions */
COMMAND (do_write_image) {
  BEGIN_W_COMMAND(do_write_image);
    i_status_action_stop(w_current);
    o_redraw_cleanstates (w_current);
    i_status_set_state_msg(w_current, SELECT, _("Write Image"));
    i_status_set_state(w_current, SELECT);
    i_status_update_sensitivities(w_current);
    x_image_setup(w_current, last_image);
  EXIT_COMMAND(do_write_image);
}

/** @brief i_cmd_do_write_pdf in i_command_File_Actions */
/*!
 * \brief Write PDF command
 * \par Function Description
 *  This is handles the file-write-pdf action
 */
COMMAND (do_write_pdf) {
  BEGIN_W_COMMAND(do_write_pdf);

  Page *page;
  char *fname;
  char *filename;

  i_status_action_stop(w_current);
  o_redraw_cleanstates (w_current);
  i_status_set_state_msg(w_current, SELECT, _("Write PDF"));
  i_status_set_state(w_current, SELECT);
  i_status_update_sensitivities(w_current);

  page  = gschem_toplevel_get_current_page(w_current);
  fname = (char*)geda_page_get_filename(page);

  if (fname) {

    /* Do not free this fname */

    char *name = geda_get_file_name(fname);

    if (name) {
      fname = geda_strconcat(name, ".pdf", NULL);
      GEDA_FREE(name);
    }
    else {
      fname = NULL;
    }
  }

  if (!fname) {
    fname = geda_strdup("*.pdf");
  }

  filename = x_dialog_select_file(w_current, "Select destination...", fname, FSB_SAVE);

  GEDA_FREE(fname);

  if (filename != NULL) { /* if user did not cancel */
    x_print_export_pdf_page(w_current, filename);
    GEDA_FREE(filename);
  }
  EXIT_COMMAND(do_write_pdf);
}

/** @brief i_cmd_do_run_script in i_command_File_Actions */
COMMAND (do_run_script) {
  BEGIN_W_COMMAND(do_run_script);
  char *filename;
  gschem_threads_enter();
  filename = x_dialog_select_file(w_current, "Execute Script...", NULL, FSB_LOAD);
  if (filename != NULL) { /* if user did not cancel */
    g_evaluate_scheme_file(filename, NULL);
    GEDA_FREE(filename);
  }
  gschem_threads_leave();
  EXIT_COMMAND(do_run_script);
}


/** @brief i_cmd_do_close in i_command_File_Actions */
COMMAND (do_close) {
  BEGIN_W_COMMAND(do_close);

  i_window_close_page(w_current);

  EXIT_COMMAND(do_close);
}

/** @brief i_cmd_do_close_all in i_command_File_Actions */
COMMAND (do_close_all) {
  BEGIN_W_COMMAND(do_close_all);
  GList *iter;
  GList *pages;
  Page  *p_current;
  bool   can_close  = TRUE;
  bool   close_all;

  if (w_current->inside_action &&
     (w_current->event_state == MOVEMODE ||
      w_current->event_state == DRAGMOVE)) {
    o_move_cancel (w_current);
  }

  x_window_close_edit_dialogs(w_current);

  pages = g_list_copy(geda_list_get_glist(w_current->toplevel->pages));

  /* Loop through all the pages looking for unsaved pages */
  for (iter = pages; iter != NULL; NEXT(iter)) {

    /* get ptr to a page */
    p_current = (Page*)iter->data;

    /* if flag set */
    if (geda_page_get_changed(p_current)  > 0) {
      can_close = FALSE;
      break;                 /* if at least one page */
    }
  }

  if (!can_close) {         /* Ask to save unsaved pages */
    close_all = x_confirm_close_window (w_current);
    if (!close_all) {       /* user canceled the close */
      geda_log_v(_("Canceled close all\n"));
    }
  }
  else {
    close_all = TRUE;       /* There were no unsaved pages */
  }

  if (close_all) {          /* Still want to close all? */

    geda_log_q(_("Closing all documents\n"));

    /* Loop through all the pages */
    for ( iter = pages; iter != NULL; NEXT(iter)) {

      /* get ptr to a page */
      p_current = (Page*)iter->data;
      if (p_current->filename) {
        x_window_close_page (w_current, p_current);
      }
    }
  }

  i_status_set_state(w_current, SELECT);

  g_list_free (pages);

  EXIT_COMMAND(do_close_all);
}

/** @brief i_cmd_do_quit in i_command_File_Actions */
COMMAND (do_quit) {
  BEGIN_NO_ARGUMENT(do_quit);
  geda_log_v("gschem: %s\n", _("starting shut-down"));
  x_window_close_all(w_current);
}

/** @brief i_cmd_do_export_symbol in i_command_File_Actions */
COMMAND (do_export_symbol) {
  BEGIN_W_COMMAND(do_export_symbol);

  GedaObject *o_current;

  o_current = o_select_return_first_object(w_current);

  if (o_current && (o_current->type == OBJ_COMPLEX)) {
    if (!geda_object_get_is_embedded(o_current)) {
      const char *question = _("Symbol is not embedded, export anyway?");
      int response = x_dialog_confirmation(question, GTK_MESSAGE_INFO, FALSE);
      if (response == GEDA_RESPONSE_YES) {
        o_complex_export(w_current, o_current);
      }
    }
    else {
      o_complex_export(w_current, o_current);
    }
  }

  EXIT_COMMAND(do_export_symbol);
}

/** @brief i_cmd_do_export_symbol in i_command_File_Actions */
COMMAND (do_export_picture) {
  BEGIN_W_COMMAND(do_export_picture);

 GedaObject *o_current;

  o_current = o_select_return_first_object(w_current);

  if (o_current && (o_current->type == OBJ_PICTURE)) {
    if (!geda_object_get_is_embedded(o_current)) {
      const char *question = _("Picture is not embedded, export anyway?");
      int response = x_dialog_confirmation(question, GTK_MESSAGE_INFO, FALSE);
      if (response == GEDA_RESPONSE_YES) {
        o_picture_export(w_current, o_current);
      }
    }
    else {
      o_picture_export(w_current, o_current);
    }
  }

  EXIT_COMMAND(do_export_picture);
}

/** @} endgroup i_command_File_Actions */

/* ------------------ Edit ---------------- */

COMMAND (do_edit)
{
  BEGIN_COMMAND(do_edit);
  //geda_log("do edit command handler");
  char *msg = "Not an object";

  geda_set_object_visibility ((GedaObject*)msg, 1);
  EXIT_COMMAND(do_edit);
}

/** \defgroup i_command_Edit_Actions Actions under the Edit Menu
 * @{*/

COMMAND (do_undo)
{
  BEGIN_W_COMMAND(do_undo);

  if (w_current->inside_action) {
    i_callback_cancel (w_current, 0, NULL);
  }
  else {
    /* TODO: loop here with arg */
    o_undo_callback(w_current, UNDO_ACTION);
  }
  EXIT_COMMAND(do_undo);
}

COMMAND (do_redo)
{
  BEGIN_W_COMMAND(do_redo);
  o_undo_callback(w_current, REDO_ACTION);
  EXIT_COMMAND(do_redo);
}

COMMAND (do_cut_clip)
{
  BEGIN_NO_ACTION(do_cut_clip);

  if (o_select_is_selection (w_current)){

    if ((narg < 0) || (arg == NULL)) {
      /* if no arguments then use buffer 0 */
      narg = 0;
    }

    o_buffer_cut (w_current, narg);
    if ( narg == 0)
      x_clipboard_set (w_current, object_buffer[narg]);
    else
      i_status_update_sensitivities(w_current);
  }

  EXIT_COMMAND(do_cut_clip);
}

COMMAND (do_copy_clip)
{
  BEGIN_W_COMMAND(do_copy_clip);

  if (o_select_is_selection (w_current)) {

    if (narg < 0 || arg == NULL) {
      /* if no arguments then use buffer 0 */
      narg = 0;
    }

    /* Copy to one of our buffer */
    o_buffer_copy (w_current, narg);

    /* if buffer number = 0, then copy to system buffer */
    if ( narg == 0) {
      x_clipboard_set (w_current, object_buffer[0]);
    }
    i_status_update_sensitivities(w_current);
  }
  EXIT_COMMAND(do_copy_clip);
}

/*!
 * \brief Action Paste Clipboard Contents
 * \par Function Description
 *  This function initiates the pasting of the clipboard contents
 *  into the drawing.
 */
COMMAND (do_paste_clip)
{
  BEGIN_W_COMMAND(do_paste_clip);

  if ((narg < 0) || (arg == NULL)) { /* if no arguments then use buffer 0 */

    narg = 0;

    geda_struct_object_release_objects (object_buffer[0]);

    object_buffer[0] = x_clipboard_get (w_current);
  }

  if (object_buffer[narg] != NULL) {

    w_current->buffer_number = narg;

    i_event_end_action_handler(w_current);

    if (HOT_ACTION (do_paste_clip)) {

      int w_x = CMD_X(do_paste_clip);
      int w_y = CMD_Y(do_paste_clip);

      if (!o_buffer_paste_start (w_current, w_x, w_y)) {
        i_status_set_state (w_current, SELECT);
      }
      else {
        o_place_motion (w_current, w_x, w_y);
      }
    }
    else {
      o_redraw_cleanstates (w_current);
      i_status_action_stop(w_current);
      i_status_set_state (w_current, PASTEMODE);
    }
  }
  else {

    /* Clean up if tried to paste "nothing" inside an action */
    if (w_current->inside_action) {
      o_redraw_cleanstates (w_current);
      i_status_action_stop(w_current);
    }

    i_status_set_state_msg (w_current, SELECT, _("Empty buffer"));
  }
  EXIT_COMMAND(do_paste_clip);
}

COMMAND (do_delete)
{
  BEGIN_W_COMMAND(do_delete);

  if (o_select_return_first_object(w_current)) {

    o_redraw_cleanstates(w_current);
    o_delete_selected(w_current);
    /* After deletion go into select mode */
    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
    i_status_update_sensitivities(w_current);

  }
  EXIT_COMMAND(do_delete);
}

/** @brief i_cmd_do_copy in i_command_Edit_Actions */
/*!
 * \brief Copy selected objects on page.
 * \par Function Description
 *  Initiate Copy mode for selected objects
 */
COMMAND (do_copy)
{
  BEGIN_W_COMMAND(do_copy);

  if (o_select_is_selection(w_current)) {

    o_redraw_cleanstates(w_current);

    if HOT_ACTION (do_copy) {
      o_copy_start (w_current, CMD_X(do_copy), CMD_Y(do_copy));
    }
    else {
      w_current->event_state   = COPYMODE;
      i_status_action_stop(w_current);
      x_toolbars_update (w_current);
      i_status_show_msg(w_current, "Copy from point");
    }
  }
  else {
    msg_need_select_1st(w_current);
  }

  EXIT_COMMAND(do_copy);
}

/** @brief i_cmd_do_mcopy in i_command_Edit_Actions */
/*!
 * \brief Make multi copies of selected objects on page.
 * \par Function Description
 *  Initiates Multi-Copy mode for selected objects
 */
COMMAND (do_mcopy)
{
  BEGIN_W_COMMAND(do_mcopy);

  if (o_select_is_selection(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_mcopy) {
      o_copy_multiple_start (w_current, CMD_X(do_mcopy), CMD_Y(do_mcopy));
    }
    else {
      w_current->event_state   = MCOPYMODE;
      i_status_action_stop(w_current);
      x_toolbars_update (w_current);
      i_status_show_msg(w_current, "Copy from point");
    }
  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_mcopy);
}

/*!
 * \brief Mirror selected objects on page
 * \brief i_cmd_do_mirror in i_command_Edit_Actions
 * \par Function Description
 *  Initiate Mirror mode for selected object.
 */
COMMAND (do_mirror)
{
  BEGIN_W_COMMAND(do_mirror);

  if (w_current->inside_action) {
    o_place_mirror (w_current);
  }
  else {

    int state;

    o_redraw_cleanstates(w_current);

    if HOT_ACTION (do_mirror) {

      GList *object_list;
      Page  *p_current;

      p_current   = geda_toplevel_get_current_page(w_current->toplevel);
      object_list = geda_page_get_selection(p_current);;

      if (object_list) {
        o_edit_mirror_world(w_current, CMD_X(do_mirror), CMD_Y(do_mirror), object_list);
      }

      state = SELECT;
    }
    else {
      state = ENDMIRROR;
    }

    i_status_set_state(w_current, state);
  }

  EXIT_COMMAND(do_mirror);
}

/** @brief i_cmd_do_move in i_command_Edit_Actions */
/*!
 * \brief Move selected objects on page.
 * \par Function Description
 *  Initiate Move mode for selected objects
 */
COMMAND (do_move)
{
  BEGIN_W_COMMAND(do_move);

  if (o_select_is_selection(w_current)) {
    o_redraw_cleanstates(w_current);
    if (HOT_ACTION (do_move)) {
      o_move_start (w_current, CMD_X(do_move), CMD_Y(do_move));
    }
    else {
      w_current->event_state = MOVEMODE;
      i_status_action_stop(w_current);
      x_toolbars_update (w_current);
      i_status_show_msg(w_current, "Move from point");
    }
  }
  else {
    msg_need_select_1st(w_current);
  }

  EXIT_COMMAND(do_move);
}

COMMAND (do_array)
{
  BEGIN_W_COMMAND(do_array);

  w_current->first_wx = CMD_X(do_move);
  w_current->first_wy = CMD_Y(do_move);

  x_dialog_array_edit(w_current);

  EXIT_COMMAND(do_array);
}

/*!
 * \brief Break Editing Mode
 * \brief i_cmd_do_break in i_command_Edit_Actions
 * \par Function Description
 *  Initiate Break mode, possibly handling if there
 *  object are already selected.
 */
COMMAND (do_break)
{
  BEGIN_W_COMMAND(do_break);

  GList *object_list;

  o_redraw_cleanstates(w_current);
  i_status_action_stop(w_current);

  object_list = geda_list_get_glist (Current_Selection);

  if (HOT_ACTION (do_break)) {

    if (object_list) {
      o_break_hot(w_current, object_list, CMD_X(do_break), CMD_Y(do_break));
    }
    if (w_current->event_state == ENDBREAK) {
      i_status_set_state(w_current, ENDBREAK);
    }
  }
  else {

    int status = o_break_interrogate (w_current, object_list);

    i_status_set_state(w_current, status);

  }
  EXIT_COMMAND(do_break);
}

/*!
 * \brief Project Editing Mode
 * \brief i_cmd_do_extend in i_command_Edit_Actions
 * \par Function Description
 *  Initiate Projection mode, possibly handling if there
 *  object are already selected.
 */
COMMAND (do_extend)
{
  BEGIN_W_COMMAND(do_extend);

  GList *object_list;

  o_redraw_cleanstates(w_current);
  i_status_action_stop(w_current);

  object_list = geda_list_get_glist (Current_Selection);

  if HOT_ACTION (do_extend) {
    if (object_list) {
      o_extend_hot(w_current, object_list, CMD_X(do_extend), CMD_Y(do_extend));
    }
  }
  else {

    int status = o_extend_interrogate (w_current, object_list);

    i_status_set_state(w_current, status);

  }
  EXIT_COMMAND(do_extend);
}

/*!
 * \brief Offset selected objects
 * \brief i_cmd_do_offset in i_command_Edit_Actions
 * \par Function Description
 *  Create a copy of selected objects at offset to pointer position.
 */
COMMAND (do_offset)
{
  BEGIN_NO_ACTION(do_offset);

  if (o_select_is_selection(w_current)) {

    int state = SELECT;

    o_redraw_cleanstates(w_current);

    if HOT_ACTION (do_offset) {

      GList *object_list = geda_list_get_glist (Current_Selection);

      if (object_list) {

        int x = CMD_X(do_offset);
        int y = CMD_Y(do_offset);

        o_edit_offset_hot(w_current, x, y, object_list);
      }
    }
    else {

      const char *title  = _("Offset Mode");
      const char *prompt = _("Specify offset distance:");

      int tmp_int = w_current->offset;

      w_current->offset = geda_dialog_get_integer(title, prompt, tmp_int);

      if (w_current->offset != -0) { /* If user did not cancel */
        if (w_current->offset) {     /* If user did not enter zero */
          state = ENDOFFSET;
        }
        else {
          geda_log("Ignoring zero offset\n");
          w_current->offset = -0;
        }
      }
    }
    i_status_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_offset);
}

/*!
 * \brief Action Rotate  in i_command_Edit_Actions
 * \brief i_cmd_do_rotate_left
 * \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 */
COMMAND (do_rotate_left)
{
  BEGIN_W_COMMAND(do_rotate_left);

  if (w_current->inside_action) {

    o_place_rotate (w_current, 90);

  }
  else {

    int state;

    o_redraw_cleanstates(w_current);

    if HOT_ACTION (do_rotate_left) {

      GList *object_list;

      object_list = geda_list_get_glist (Current_Selection);

      if (object_list) {
        /* Allow o_edit_rotate_world to redraw the objects */
        o_edit_rotate_world(w_current, CMD_X(do_rotate_left),
                                       CMD_Y(do_rotate_left), 90, object_list);
      }

      state = SELECT;
    }
    else {
      state = ENDROTATE;

    }

    i_status_set_state(w_current, state);
  }

  EXIT_COMMAND(do_rotate_left);
}

/*!
 * \brief Action Rotate  in i_command_Edit_Actions
 * \brief i_cmd_do_rotate_right
 * \par Function Description
 *  This function rotate all objects in the selection list by -90 degrees.
 */
COMMAND (do_rotate_right)
{
  BEGIN_W_COMMAND(do_rotate_right);

  if (w_current->inside_action) {

    o_place_rotate (w_current, 270);

  }
  else {

    int state;

    o_redraw_cleanstates(w_current);

    if HOT_ACTION (do_rotate_right) {

      GList *object_list;

      object_list = geda_list_get_glist (Current_Selection);

      if (object_list) {
        /* Allow o_edit_rotate_world to redraw the objects */
        o_edit_rotate_world(w_current, CMD_X(do_rotate_right),
                                       CMD_Y(do_rotate_right), 270, object_list);
      }

      state = SELECT;
    }
    else {
      state = ENDROTATE;

    }

    i_status_set_state(w_current, state);
  }

  EXIT_COMMAND(do_rotate_right);
}

/*!
 * \brief Snap Selecion Editing Action
 * \brief i_cmd_do_snap in i_command_Edit_Actions
 * \par Function Description
 *  Responses to edit-snap actions, simply by passing the list of
 *  currently selected objects to the function o_edit_snap.
 */
COMMAND (do_snap)
{
  BEGIN_W_COMMAND(do_snap);

  if (o_select_is_selection(w_current)) {

    GList *object_list;

    object_list = geda_list_get_glist (Current_Selection);

    o_redraw_cleanstates(w_current);

    o_edit_snap (w_current, object_list);
  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_snap);
}

/*!
 * \brief Edit Attributes for selected object
 * \brief i_cmd_edit_butes in i_command_Edit_Actions
 * \par Function Description
 *  Calls o_edit_objects to initiate the Edit Attributes dialog for
 *  selected object
 */
COMMAND (do_edit_butes)
{
  BEGIN_W_COMMAND(do_edit_butes);

  o_edit_objects (w_current, geda_list_get_glist (Current_Selection),
                  CMD_WHO(do_edit_butes));

  EXIT_COMMAND(do_edit_butes);
}

COMMAND (do_edit_ponent)
{
  BEGIN_W_COMMAND(do_edit_butes);
  GedaObject *o_current;

  o_current = o_select_return_first_object(w_current);

  x_dialog_edit_properties(w_current, o_current);

  EXIT_COMMAND(do_edit_butes);
}

/*! \brief Edit Text in i_command_Edit_Actions */
COMMAND (do_edit_text)
{
  BEGIN_W_COMMAND(do_edit_text);

  GList *s_current;

  s_current = geda_list_get_glist( Current_Selection );

  if (s_current) {

    GList  *iter;

    for (iter = s_current; iter; iter = iter->next) {

      GedaObject *o_current = iter->data;

      if (o_current->type == OBJ_TEXT) {
        o_text_edit(w_current, o_current);
        break;
      }
    }
  }

  EXIT_COMMAND(do_edit_text);
}

/*! \brief Edit Slot in i_command_Edit_Actions */
COMMAND (do_edit_slot)
{
  BEGIN_W_COMMAND(do_edit_slot);
  GedaObject *o_current;

  o_current = o_select_return_first_object(w_current);

  if (o_current && (o_current->type == OBJ_COMPLEX)) {
    o_slot_start(w_current, o_current);
  }
  EXIT_COMMAND(do_edit_slot);
}

/*! \brief Edit Color in i_command_Edit_Actions */
COMMAND (do_edit_color)
{
  BEGIN_W_COMMAND(do_edit_color);
  x_dialog_edit_color (w_current);
  EXIT_COMMAND(do_edit_color);
}

/*! \brief Edit Arc in i_command_Edit_Actions */
COMMAND (do_edit_arc)
{
  BEGIN_W_COMMAND(do_edit_arc);
  GedaObject *o_current;
  o_current = o_select_return_first_object(w_current);
  if ( o_current && o_current->type == OBJ_ARC) {
    x_dialog_edit_arc_angle(w_current, o_current);
  }
  EXIT_COMMAND(do_edit_arc);
}

/*! \brief Edit Pin-Type in i_command_Edit_Actions */
COMMAND (do_pintype)
{
  BEGIN_W_COMMAND(do_pintype);
  x_dialog_edit_pin_type (w_current);
  EXIT_COMMAND(do_pintype);
}

/*! \brief Edit Line-Type in i_command_Edit_Actions */
COMMAND (do_linetype)
{
  BEGIN_W_COMMAND(do_linetype);
  x_dialog_edit_line_type(w_current);
  EXIT_COMMAND(do_linetype);
}

/*! \brief Edit Fill-Type in i_command_Edit_Actions */
COMMAND (do_filltype)
{
  BEGIN_W_COMMAND(do_filltype);
  x_dialog_edit_fill_type(w_current);
  EXIT_COMMAND(do_filltype);
}

/*! \brief Lock in i_command_Edit_Actions
 *  \par Function Description
 *  This function calls o_edit_lock_selection to locks all
 *  objects in selection list.
 */
COMMAND (do_lock)
{
  BEGIN_W_COMMAND(do_lock);

  if (o_select_return_first_object(w_current)) {
    o_edit_lock_selection(w_current);
  }
  EXIT_COMMAND(do_lock);
}

/*!
 * \brief Unlock objects in selection list
 * \brief i_cmd_do_unlock in i_command_Edit_Actions
 * \par Function Description
 *  This function calls o_edit_unlock_selection to unlocks all
 *  objects in the selection list.
 */
COMMAND (do_unlock)
{
  BEGIN_W_COMMAND(do_unlock);
  if (o_select_return_first_object(w_current)) {
    o_edit_unlock_selection(w_current);
  }
  EXIT_COMMAND(do_unlock);
}

/** @} endgroup i_command_Edit_Actions */

/* ------------------ Select ---------------- */

/** \defgroup i_command_Select_Actions Actions under the Select Menu
 * @{*/
COMMAND (do_select)
{
  BEGIN_W_COMMAND(do_select);

  /* If inside action get rid of any rubber and put everything
   * back where it belongs, for example stretched nets */
  if (w_current->inside_action) {
    if (!o_invalidate_rubber (w_current)) {
      i_callback_cancel(w_current, 0, NULL);
    }
  }

  o_redraw_cleanstates(w_current);
  i_status_set_state(w_current, SELECT);

  if (w_current->action_event->state) {
    i_event_cancel_action_handler(w_current);
  }
  else {
    i_status_action_stop(w_current);
  }

  EXIT_COMMAND(do_select);
}

COMMAND (do_select_all)
{
  BEGIN_W_COMMAND(do_select_all);
  o_redraw_cleanstates (w_current);
  o_select_visible_unlocked (w_current);

  i_status_update_sensitivities (w_current);
  EXIT_COMMAND(do_select_all);
}

/** @brief i_cmd_do_select_invert in i_command_Select_Actions
 *! \brief Inverts the Selection Set for all unlocked objects on page.
 *  \par Function Description
 * Sets all objects on page as deselected if the objected is selected and
 * selected if the object was not select.
 */
COMMAND (do_select_invert)
{
  BEGIN_W_COMMAND(do_select_invert);

  GedaToplevel *toplevel  = w_current->toplevel;
  SELECTION    *selection = toplevel->page_current->selection_list;
  GList        *list      = g_list_copy (geda_list_get_glist(selection));

  o_select_visible_unlocked (w_current);

  while(list != NULL) {
    geda_object_selection_remove (selection, (GedaObject*)list->data);
    NEXT(list);
  }

  g_list_free (list);

  EXIT_COMMAND(do_select_invert);
}

/** @brief i_cmd_do_select_last in i_command_Select_Actions
 *! \brief Re Select the last object that was selected
 *  \par Function Description
 *  Sets the last object selected as selected.
 */
COMMAND (do_select_last)
{
  BEGIN_W_COMMAND(do_select_last);

  if (w_current->which_object) {

    GedaObject *object = w_current->which_object;

    if (GEDA_IS_OBJECT(w_current->which_object)) {

      if (!object->selected) {
        o_select_object(w_current, object, SINGLE, 0);
      }
    }
    else {
      w_current->which_object = NULL;
    }
  }

  EXIT_COMMAND(do_select_last);
}

/** @brief i_cmd_do_deselect in i_command_Select_Actions
 *! \brief Deselect all objects on page.
 *  \par Function Description
 * Sets all objects on page as deselected.
 */
COMMAND (do_deselect)
{
  BEGIN_W_COMMAND(do_deselect);

  o_redraw_cleanstates (w_current);

  if (o_select_is_selection (w_current)){
    i_status_set_state (w_current, DESELECT);
    if (w_current->action_event->state) {
      i_event_cancel_action_handler(w_current);
    }
    else {
      i_status_action_stop(w_current);
    }
  }
  else {
    /* Switch to SELECT mode because nothing to deselect */
    msg_need_select_1st(w_current);
  }

  i_status_update_sensitivities (w_current);
  EXIT_COMMAND(do_deselect);
}

/** @brief i_cmd_do_deselect in i_command_Select_Actions
 *! \brief Deselect all objects on page.
 *  \par Function Description
 * Sets all objects on page as deselected.
 */
COMMAND (do_deselect_all)
{
  BEGIN_W_COMMAND(do_deselect_all);
  o_redraw_cleanstates (w_current);
  o_select_unselect_all (w_current);

  i_status_update_sensitivities (w_current);
  EXIT_COMMAND(do_deselect_all);
}

/** @} endgroup i_command_Select_Actions */

/* ------------------ View ---------------- */

COMMAND (do_view)
{
  BEGIN_COMMAND(do_view);
  geda_log("do_view command handler");
  EXIT_COMMAND(do_view);
}

/** \defgroup i_command_View_Actions Actions under the View Menu
 * @{*/

COMMAND (do_redraw)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_redraw);
  o_invalidate_all (w_current);
}

COMMAND (do_pan)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_pan);
  o_redraw_cleanstates(w_current);
  i_status_action_stop(w_current);
  i_status_set_state(w_current, PAN);
}

/*!
 * \brief Zoom Box Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the Zoom Box action.
 */
COMMAND (do_zoom_box)
{
  BEGIN_W_COMMAND(do_zoom_box);

  o_redraw_cleanstates(w_current);

  i_status_set_state(w_current, ZOOMBOX);

  if HOT_ACTION (do_zoom_box) {
    i_zoom_world_box_start (w_current, CMD_X(do_zoom_box), CMD_Y(do_zoom_box));
  }
  else {
    i_status_action_stop(w_current);
  }

  EXIT_COMMAND(do_zoom_box);
}

/*!
 * \brief Zoom Selected Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the Zoom Selected action.
 */
COMMAND (do_zoom_selected)
{
  BEGIN_W_COMMAND(do_zoom_selected);

  if (o_select_is_selection(w_current)) {

    const GList *selection;

    selection = geda_struct_page_get_selection(Current_Page)->glist;

    i_zoom_world_extents (w_current, selection, 0);
    i_zoom_world(w_current, ZOOM_OUT_DIRECTIVE, CMD_WHO(do_zoom_selected), 0);

    if (w_current->undo_panzoom) {
      o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
    }
  }

  EXIT_COMMAND(do_zoom_selected);
}

/*!
 * \brief Zoom Extents Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the view-zoom-extents action.
 */
COMMAND (do_zoom_extents)
{
  BEGIN_W_COMMAND(do_zoom_extents);

  /* scroll bar stuff */
  i_zoom_world_extents (w_current, NULL, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }

  EXIT_COMMAND(do_zoom_extents);
}

/*!
 * \brief Zoom In Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the view-zoom-in action.
 */
COMMAND (do_zoom_in)
{
  BEGIN_W_COMMAND(do_zoom_in);

  i_zoom_world(w_current, ZOOM_IN_DIRECTIVE, CMD_WHO(do_zoom_in), 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }

  EXIT_COMMAND(do_zoom_in);
}

/*!
 * \brief Zoom Out Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the view-zoom-out action.
 */
COMMAND (do_zoom_out)
{
  BEGIN_W_COMMAND(do_zoom_out);

  i_zoom_world(w_current, ZOOM_OUT_DIRECTIVE, CMD_WHO(do_zoom_out), 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }

  EXIT_COMMAND(do_zoom_out);
}

/*!
 * \brief Zoom All Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the view-zoom-all action.
 */
COMMAND (do_zoom_all)
{
  BEGIN_W_COMMAND(do_zoom_all);

  i_zoom_world(w_current, ZOOM_FULL_DIRECTIVE, DONTCARE, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }

  EXIT_COMMAND(do_zoom_all);
}

/*!
 * \brief Zoom to Magnification Function in i_command_View_Actions
 * \par Function Description
 *  This is the callback handler for the view-zoom-to-mag action.
 *
 * \note Magnification in this context is the reciprocal of the scale
 *       factor, i.e. to_screen_y_constant and to_screen_x_constant.
 */
COMMAND (do_zoom_to_mag)
{
  BEGIN_W_COMMAND(do_zoom_to_mag);

  int x, y;
  double mag;

  x = CMD_X(do_zoom_to_mag);
  y = CMD_Y(do_zoom_to_mag);

  mag = geda_dialog_get_real(_("Zoom Magnification"), _("Specify new zoom:"), 5.0);

  if (mag != -0) { /* If user did not cancel */

    i_zoom_world_specify(w_current, mag, x, y, CMD_WHO(do_zoom_to_mag));

    if (w_current->undo_panzoom) {
      o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
    }
  }
  EXIT_COMMAND(do_zoom_to_mag);
}

/*!
 * \brief View Documentation Action Function in i_command_View_Actions
 * \par Function Description
 *  This is a callback function for the view-documentation action.
 */
COMMAND (do_documentation)
{
  BEGIN_W_COMMAND(do_documentation);

  GedaObject *object = o_select_return_first_object(w_current);

  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {

      char *attrib_doc;

      /* look for "documentation" */
      attrib_doc = geda_attrib_search_object_by_name (object, "documentation", 0);

      if (attrib_doc) {

        bool result;

#if 1
        result = x_show_uri (attrib_doc);

        if (!result) {

          geda_log("%s \"%s\"\n", _("Check path:"), attrib_doc);

        }

#else
        GError *error;

        /* Use this instead until debian-gnome work out thier iceweasel issue */
        result = g_app_info_launch_default_for_uri(attrib_doc, NULL, &error);

        if (!result) {
          geda_log("%s: %s\n", _("error"), error->message);
          g_error_free (error);
        }

#endif

        GEDA_FREE(attrib_doc);
      }
    }
  }
  else {
    geda_log_q(_("No component selected\n"));
  }

  EXIT_COMMAND(do_documentation);
}

COMMAND (do_show_hidden)
{
  BEGIN_W_COMMAND(do_show_hidden);

  i_window_show_attributes(w_current, FALSE);

  EXIT_COMMAND(do_show_hidden);
}

COMMAND (do_show_inherited)
{
  BEGIN_W_COMMAND(do_show_inherited);

  i_window_show_attributes(w_current, TRUE);

  EXIT_COMMAND(do_show_inherited);
}

COMMAND (do_show_nets)
{
  BEGIN_W_COMMAND(do_show_nets);

  GList *object_list = NULL;

  if (o_select_is_selection (w_current)) {

    SELECTION *selection;

    selection   = Current_Selection;
    object_list =  geda_list_get_glist (selection);

  }
  else {

    Page *p_current;

    p_current   = gschem_toplevel_get_current_page(w_current);
    object_list =  geda_struct_page_get_objects (p_current);
  }

  o_edit_show_netnames (w_current, object_list);

  EXIT_COMMAND(do_show_nets);
}

/*!
 * \brief Load the Dark color map scheme in i_command_View_Actions
 * \par Function Description
 *  This function loads the Dark color map scheme
 *  based on user input from the keyboard or menu.
 */
COMMAND (do_dark_colors)
{
  BEGIN_W_COMMAND(do_dark_colors);

  /* Change the scheme here */
  geda_color_load_display_scheme(DARK_DISPLAY_MAP); /* call for load */

  o_invalidate_all (w_current);

  EXIT_COMMAND(do_dark_colors);
}

/*!
 * \brief Load the Light color map scheme in i_command_View_Actions
 * \par Function Description
 *  This function loads the Light color map scheme
 *  based on user input from the keyboard or menu.
 */
COMMAND (do_light_colors)
{
  BEGIN_W_COMMAND(do_light_colors);

  /* Change the scheme here */
  geda_color_load_display_scheme(LIGHT_DISPLAY_MAP); /* call for load */

  o_invalidate_all (w_current);

  EXIT_COMMAND(do_light_colors);
}
/*!
 * \brief Load the BW color map scheme in i_command_View_Actions
 * \par Function Description
 *  This function loads the BW color map scheme
 *  based on user input from the keyboard or menu.
 */
COMMAND (do_bw_colors)
{
  BEGIN_W_COMMAND(do_bw_colors);

  /* Change the scheme here */
  geda_color_load_display_scheme(BW_DISPLAY_MAP); /* call for load */

  o_invalidate_all (w_current);

  EXIT_COMMAND(do_bw_colors);
}

/** @} endgroup i_command_View_Actions */

/* ------------------ Page ---------------- */

COMMAND (do_page)
{
  BEGIN_COMMAND(do_page);
  geda_log("do_page command handler");
  EXIT_COMMAND(do_page);
}

/** \defgroup i_command_Page_Actions Actions under the Page Menu
 * @{*/

/** @brief i_cmd_do_draw_after in i_command_Command_Functions */
COMMAND (do_draw_after)
{
  BEGIN_NO_ACTION(do_draw_after);

  if (o_select_is_selection(w_current)) {

    GList *object_list;

    o_redraw_cleanstates(w_current);

    gschem_toplevel_free_primary(w_current);

    object_list = geda_list_get_glist (Current_Selection);

    w_current->primary_selection = g_list_copy (object_list);
    i_status_show_msg(w_current, "Which objects");
    o_select_connect_selector(w_current, o_page_draw_after);

  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_draw_after);
}

/** @brief i_cmd_do_draw_before in i_command_Command_Functions */
COMMAND (do_draw_before)
{
  BEGIN_NO_ACTION(do_draw_before);

  if (o_select_is_selection(w_current)) {

    GList *object_list;

    o_redraw_cleanstates (w_current);

    gschem_toplevel_free_primary (w_current);

    object_list = geda_list_get_glist (Current_Selection);

    w_current->primary_selection = g_list_copy (object_list);

    i_status_show_msg(w_current, "Which objects");

    o_select_connect_selector (w_current, o_page_draw_before);

  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_draw_before);
}

/** @brief i_cmd_do_draw_first in i_command_Command_Functions */
COMMAND (do_draw_first)
{
  BEGIN_NO_ACTION(do_draw_first);

  if (o_select_is_selection(w_current)) {

    GList *object_list = geda_list_get_glist (Current_Selection);

    o_redraw_cleanstates(w_current);

    o_page_draw_first(w_current, object_list);

  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_draw_first);
}

/** @brief i_cmd_do_draw_last in i_command_Command_Functions */
COMMAND (do_draw_last)
{
  BEGIN_NO_ACTION(do_draw_last);

  if (o_select_is_selection (w_current)) {

    GList *object_list = geda_list_get_glist (Current_Selection);

    o_redraw_cleanstates (w_current);

    o_page_draw_last (w_current, object_list);

  }
  else {
    msg_need_select_1st (w_current);
  }
  EXIT_COMMAND(do_draw_last);
}

/** @brief i_cmd_do_page_manager in i_command_Command_Functions */
COMMAND (do_page_manager)
{
  BEGIN_W_COMMAND(do_page_manager);
  x_pagesel_open (w_current);
  EXIT_COMMAND(do_page_manager);
}

/** @brief i_cmd_do_page_first in i_command_Command_Functions */
COMMAND (do_page_first)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_first);

  GedaToplevel *toplevel = w_current->toplevel;

  Page  *p_current;
  Page  *p_first;
  GList *iter;

  p_current = geda_toplevel_get_current_page (toplevel);

  if (w_current->enforce_hierarchy) {

    iter = g_list_find (geda_list_get_glist(toplevel->pages), p_current);
    iter = g_list_previous(iter);

    if (iter != NULL) {

      Page  *p_prev = p_first = iter->data;

      while (p_prev) {
        p_prev = geda_hierarchy_find_prev_page (toplevel->pages, p_prev);
        if (p_prev) {
          p_first = p_prev;
        }
      }
    }
    else {

      iter = geda_toplevel_get_pages (toplevel);
      p_first = iter->data;

    }
  }
  else {
    iter = geda_toplevel_get_pages (toplevel);
    p_first = iter->data;
  }

  if (p_first != NULL && p_first != p_current) {
    x_window_set_current_page (w_current, p_first);
  }
}

/** @brief i_cmd_do_page_prev in i_command_Command_Functions */
COMMAND (do_page_prev) /* Aka Back */
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

  BEGIN_NO_ARGUMENT(do_page_prev);

  GedaToplevel *toplevel = w_current->toplevel;

  Page *p_new;
  Page *p_old;

  p_old = geda_toplevel_get_current_page (toplevel);

  p_new = gschem_page_history_get_back(w_current->page_history);

  if (p_new != NULL && p_new != p_old) {

   x_window_set_current_page (w_current, p_new);

  }
}

/** @brief i_cmd_do_page_next in i_command_Command_Functions */
COMMAND (do_page_next)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

  BEGIN_NO_ARGUMENT(do_page_next);

  GedaToplevel *toplevel = w_current->toplevel;

  Page *p_old;
  Page *p_new;

  p_old = geda_toplevel_get_current_page (toplevel);

  p_new = gschem_page_history_get_forward (w_current->page_history);

  if (p_new != NULL && p_new != p_old) {
    x_window_set_current_page (w_current, p_new);
  }
}

/** @brief i_cmd_do_page_up in i_command_Command_Functions */
COMMAND (do_page_up)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

  BEGIN_NO_ARGUMENT(do_page_up);

  GedaToplevel *toplevel = w_current->toplevel;

  Page *page_current;

  page_current = geda_toplevel_get_current_page (toplevel);

  if (page_current != NULL) {

    Page *p_new;

    if (w_current->enforce_hierarchy) {

      p_new = geda_hierarchy_find_prev_page (toplevel->pages, page_current);

    }
    else {

      p_new = geda_toplevel_get_page_up (toplevel);
    }

    if (p_new != NULL && p_new != page_current) {
      x_window_set_current_page (w_current, p_new);
    }
  }
}

/** @brief i_cmd_do_page_down in i_command_Command_Functions */
COMMAND (do_page_down)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

  BEGIN_NO_ARGUMENT(do_page_down);

  GedaToplevel *toplevel = w_current->toplevel;

  Page *current_page;

  current_page = geda_toplevel_get_current_page (toplevel);

  if (current_page != NULL) {

    Page *p_new;

    if (w_current->enforce_hierarchy) {

      p_new = geda_hierarchy_find_next_page(toplevel->pages, current_page);
    }
    else {

      p_new = geda_toplevel_get_page_down(toplevel);
    }

    if (p_new != NULL && p_new != current_page) {
      x_window_set_current_page (w_current, p_new);
    }
  }
}

/** @brief i_cmd_do_page_last in i_command_Command_Functions */
COMMAND (do_page_last)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_last);

  GedaToplevel *toplevel = w_current->toplevel;

  Page  *current_page;
  Page  *p_last;
  GList *iter;

  current_page = geda_toplevel_get_current_page(toplevel);

  if (w_current->enforce_hierarchy) {

    iter   = g_list_find(geda_toplevel_get_pages(toplevel), current_page);
    NEXT(iter);

    if (iter != NULL) {
      Page  *p_next = p_last = iter->data;
      while (p_next) {
        p_next = geda_hierarchy_find_next_page(toplevel->pages, p_next);
        if (p_next) {
          p_last = p_next;
        }
      }
    }
    else {
      iter = g_list_last(geda_toplevel_get_pages(toplevel));
      p_last = iter->data;
    }
  }
  else {
    iter = g_list_last(geda_toplevel_get_pages(toplevel));
    p_last = iter->data;
  }

  if (p_last != NULL && p_last != current_page) {
    x_window_set_current_page (w_current, p_last);
  }
}

/** @brief i_cmd_do_page_new in i_command_Command_Functions */
/* This is similar to file new accept we add new page hook*/
COMMAND (do_page_new)
{
  BEGIN_W_COMMAND(do_page_new);

  EdaConfig *cfg   = eda_config_get_user_context();
  char      *group = IVAR_CONFIG_GROUP;
  char      *tblock;

  Page      *page;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);

  x_window_set_current_page (w_current, page);

  g_hook_run_page (w_current, NEW_PAGE_HOOK, page);

 /* Add the titleblock here */
  tblock = eda_config_get_string (cfg, group, "default-titleblock", NULL);

  o_edit_add_titleblock(w_current, page, tblock);

  GEDA_FREE(tblock);

  geda_page_set_changed (page, FALSE);

  i_zoom_world_extents (w_current, NULL, I_PAN_DONT_REDRAW);

  geda_log_q ("%s \"%s\"\n", _("New page created"), geda_page_get_filename(page));

  EXIT_COMMAND(do_page_new);
}

/** @brief i_cmd_do_page_print in i_command_Command_Functions */
COMMAND (do_page_print)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_COMMAND(do_page_print);
  geda_struct_page_print_all(w_current->toplevel);
  EXIT_COMMAND(do_page_print);
}

/** @brief i_cmd_do_page_revert in i_command_Command_Functions */
COMMAND (do_page_revert)
{
  BEGIN_W_COMMAND(do_page_revert);

  i_window_revert_page(w_current);

  EXIT_COMMAND(do_page_revert);
}

/** @brief i_cmd_do_page_revert_all in i_command_Command_Functions */
COMMAND (do_page_revert_all)
{
  BEGIN_W_COMMAND(do_page_revert_all);

  GedaToplevel *toplevel;
  GList        *iter;
  GList        *pages;
  bool          revert;
  bool          unsaved = FALSE;

  void revert_command_idle_notify (void *data) {

    IdleTaskData *packet = data;
    GSList       *files  = packet->data;
    char         *last_file;
    Page         *page;

    last_file = g_slist_nth_data (files, CMD_INTEGER(do_page_revert_all));

    page = geda_struct_page_search(packet->w_current->toplevel, last_file);

    if (GEDA_IS_PAGE(page)) {
      x_window_set_current_page (packet->w_current, page);
      g_hook_run_page (packet->w_current, OPEN_PAGE_HOOK, page);
    }

    GSource *source;
    source = g_main_context_find_source_by_id (NULL, packet->source_id);
    if (source) {
      g_source_destroy (source);
    }

    /* free the list of filenames */
    g_slist_foreach (files, (GFunc)g_free, NULL);

    /* free the list that held pointers to filenames */
    g_slist_free (files);

    /* free the IdleTaskData structure */
    GEDA_FREE(data);
  }

  toplevel = gschem_toplevel_get_geda_toplevel(w_current);

  /* Check if any page are modified */
  pages = geda_toplevel_get_pages(toplevel);

  for (iter = pages; iter; iter = iter->next) {

    Page *page = iter->data;

    if (geda_page_get_changed(page)) {
      unsaved = TRUE;
      break;       /* Atl east one page changed, no need to continue looking */
    }
  }

  /* Get confirmation if any pages are modified */
  if (unsaved) {

    const char *question = _("Confirm revert ALL documents");
    int response;

    response = x_dialog_confirmation(question, GEDA_MESSAGE_WARNING, FALSE);

    if (response == GEDA_RESPONSE_YES) {
      revert = TRUE;
    }
    else {
      revert = FALSE;
    }
  }
  else {
    revert = TRUE;
  }

  if (revert) {

    GSList *files;
    Page   *p_current;
    char   *c_filename;
    int     index;

    /* Cancel in-progress move actions */
    if (w_current->inside_action &&
      (w_current->event_state == MOVEMODE ||
      w_current->event_state == DRAGMOVE)) {
      o_move_cancel (w_current);
    }

    /* Close dialogs that maybe tracking the selection */
    x_window_close_edit_dialogs(w_current);

    /* Retain the filename of the current page */
    p_current  = gschem_toplevel_get_current_page (w_current);
    c_filename = p_current->filename;
    files      = NULL;
    index      = 0;

    /* Loop through all the pages and get file names */
    for (iter = pages; iter; iter = iter->next) {

      /* Get ptr to page in list */
      Page *page     = iter->data;
      char *filename = geda_page_get_filename_dup(page);

      /* Copy the filename to the list */
      files = g_slist_append(files, filename);

      if (!strncmp(filename, c_filename, strlen(c_filename))) {
        CMD_INTEGER(do_page_revert_all) = index;
      }

      geda_page_freeze_notify(page); /* don't bother with thawing */

      /* remove the page from toplevel list of page and free */
      geda_struct_page_delete (toplevel, page, FALSE);
      index++;
    }

    /* Reload the list of files */
    if (files) {

      IdleTaskData *packet;
      int count = 0;

      geda_toplevel_set_file_open_flags(toplevel,
                                        F_OPEN_RC | F_OPEN_CHECK_BACKUP);
      lambda (void *filename) {

        gschem_task  *task;

        task          = g_malloc(sizeof(gschem_task));
        task->func.F2 = (void*)x_window_open_page;
        task->arg1    = command_struc[cmd_do_page_revert_all].w_current;
        task->arg2    = filename;
        geda_main_context_invoke (NULL, (void*)i_command_dispatch, task);
        count++;
        return FALSE;
      }
      mapcar(files);

      packet            = g_malloc(sizeof(IdleTaskData));
      packet->w_current = command_struc[cmd_do_page_revert_all].w_current;
      packet->data      = files;
      packet->retry     = FALSE;
      packet->source_id = g_idle_add_full (G_PRIORITY_LOW + (10 * count),
                                           open_command_idle_callback,
                                           packet,
                                           revert_command_idle_notify);
    }
  }

  /* Note files strings are freed by revert_command_idle_notify */

  EXIT_COMMAND(do_page_revert_all);
}

/** @brief i_cmd_do_page_close in i_command_Command_Functions */
COMMAND (do_page_close)
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_page_close);

  i_window_close_page(w_current);

  EXIT_COMMAND(do_page_close);
}

/** @brief i_cmd_do_page_discard in i_command_Command_Functions */
COMMAND (do_page_discard)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_discard);

  Page *p_current;

  p_current = gschem_toplevel_get_current_page (w_current);

  x_window_close_page (w_current, p_current);
}

/* ------------------ Hierarchy ---------------- */

/** \defgroup i_command_Hierarchy_Actions Hierarchy Actions under the Page Menu
 * @{
 * TODO: Need hierarchy command
 */

COMMAND (do_down_schematic)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_down_schematic);

  GedaObject *object           = NULL;
  Page       *child            = NULL;
  Page       *parent           = NULL;
  Page       *save_first_page  = NULL;

  bool    loaded_flag      = FALSE;
  bool    looking_inside   = FALSE;

  char   *attrib           = NULL;

  int     count            = 0;
  int     page_control     = 0;

  object = o_select_return_first_object(w_current);

  /* only allow going into symbols */
  if (object == NULL || object->type != OBJ_COMPLEX)
    return;

  parent = gschem_toplevel_get_current_page (w_current);
  attrib = geda_attrib_search_attached_by_name (object, "source", count);

  /* if above is null, then look inside symbol */
  if (attrib == NULL) {
    attrib = geda_attrib_search_inherited_by_name (object, "source", count);
    looking_inside = TRUE;
#if DEBUG
    printf("going to look inside now\n");
#endif
  }

  while (attrib) {

    /* look for source=filename,filename, ... */
    int   pcount = 0;
    char *current_filename = geda_strsplit(attrib, ',', pcount);

    /* loop over all filenames */
    while (current_filename != NULL) {

      GError *err = NULL;

      geda_log("%s: \"%s\"\n", _("Searching for source"), current_filename);

      child = geda_struct_hierarchy_down_single(w_current->toplevel,
                                                current_filename,
                                                parent,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD,
                                                &err);

      /* geda_struct_hierarchy_down_single() will not zoom the loaded page */
      if (child != NULL) {

        x_window_setup_page(w_current, child, w_current->world_left,
                                              w_current->world_right,
                                              w_current->world_top,
                                              w_current->world_bottom);
        geda_struct_page_goto (child);
        i_zoom_world_extents(w_current,
                             geda_struct_page_get_objects (child),
                             I_PAN_DONT_REDRAW);
        o_undo_savestate(w_current, UNDO_ALL);
        geda_struct_page_goto (parent);
        geda_object_notify_change_add (child,
                            (ChangeNotifyFunc) o_invalidate_object,
                            (ChangeNotifyFunc) o_invalidate_object, w_current);
      }

      /* save the first page */
      if ( !loaded_flag && (child != NULL)) {
        save_first_page = child;
      }

      /* Do error reporting */
      if (child == NULL) {

        const char *msg1 = _("Failed to descend into");
        const char *msg2 = (err != NULL) ? err->message : "Unknown error.";

        geda_log("%s '%s': %s\n", msg1, current_filename, msg2);

        char *secondary = geda_sprintf ("%s '%s': <i>%s</i>",
                                            msg1, current_filename, msg2);

        titled_pango_error_dialog("<b>Failed to descend hierarchy</b>",
                                  secondary, _("Hierarchy Error"));
        GEDA_FREE (secondary);
        g_error_free (err);

      }
      else {
        /* this only signifies that we tried */
        loaded_flag = TRUE;
        page_control = child->page_control;
      }

      GEDA_FREE(current_filename);
      pcount++;
      current_filename = geda_strsplit(attrib, ',', pcount);
    }

    GEDA_FREE(attrib);
    GEDA_FREE(current_filename);

    count++;

    /* continue looking outside first */
    if (!looking_inside) {
      attrib = geda_attrib_search_attached_by_name (object, "source", count);
    }

    /* okay we were looking outside and didn't find anything,
     * so now we need to look inside the symbol */
    if (!looking_inside && attrib == NULL && !loaded_flag) {

      looking_inside = TRUE;

#if DEBUG
      printf("switching to go to look inside\n");
#endif

    }

    if (looking_inside) {

#if DEBUG
      printf("looking inside\n");
#endif

      attrib = geda_attrib_search_inherited_by_name(object, "source", count);
    }
  }

  if (loaded_flag && (save_first_page != NULL)) {
    x_window_set_current_page (w_current, save_first_page);
  }
}

/*! \bug may cause problems with non-directory symbols */
COMMAND (do_down_symbol)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_down_symbol);

  GedaObject *object;

  object = o_select_return_first_object(w_current);

  if (object != NULL) {

    if (object->type == OBJ_COMPLEX) { /* only allow going into symbols */

      const CLibSymbol *sym;
      Page *child;
      Page *p_current;
      char *filename;

      filename = geda_complex_get_filename(object->complex);

      geda_log("%s: \"%s\"\n", _("Searching for symbol"), filename);

      sym = geda_struct_clib_get_symbol_by_name (filename);

      if (sym == NULL)
        return;

      filename = geda_struct_clib_symbol_get_filename(sym);

      if (filename == NULL) {
        geda_log(_("Symbol is not a real file. Symbol cannot be loaded.\n"));
        return;
      }
      geda_free(filename);

      p_current = gschem_toplevel_get_current_page(w_current);

      child = geda_hierarchy_down_symbol(w_current->toplevel, sym, p_current);

      x_window_setup_page(w_current, child, w_current->world_left,
                                            w_current->world_right,
                                            w_current->world_top,
                                            w_current->world_bottom);

      /* geda_hierarchy_down_symbol() will not zoom the loaded page */
      i_zoom_world_extents(w_current,
                           geda_struct_page_get_objects (child),
                           I_PAN_DONT_REDRAW);

      geda_object_notify_change_add (child,
                          (ChangeNotifyFunc) o_invalidate_object,
                          (ChangeNotifyFunc) o_invalidate_object, w_current);

      o_undo_savestate(w_current, UNDO_ALL);

      x_window_set_current_page(w_current, child);
    }
  }
}

COMMAND (do_hierarchy_up)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->pages);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_hierarchy_up);

  Page     *child;
  Page     *up_page;
  PageList *page_list;

  child     = gschem_toplevel_get_current_page (w_current);
  page_list = geda_toplevel_get_page_list(w_current->toplevel);
  up_page   = geda_hierarchy_find_up_page (page_list, child);

  if (up_page == NULL) {
    geda_log(_("Cannot find any schematics above the current one!\n"));
  }
  else {

    int answer = TRUE;

    if (geda_page_get_changed(child) > 0) {
      answer = x_confirm_close_changed_page (w_current, child);
    }

    if (answer == TRUE) {
      x_window_close_page (w_current, child);
      x_window_set_current_page(w_current, up_page);
    }
  }
}

/** @} endgroup i_command_Hierarchy_Actions */

/** @} endgroup i_command_Page_Actions */

/* ------------------ Add ---------------- */

COMMAND (do_add)
{
  BEGIN_COMMAND(do_add);
  geda_log("do_add command handler");
  EXIT_COMMAND(do_add);
}

/** \defgroup i_command_Add_Actions Actions under the Add Menu
 * @{
 */

/*!
 * \brief Action Add Component in i_command_Add_Actions
 * \par Function Description
 *  This is a callback function for the #ADD_COMPONENT action.
 */
COMMAND (do_add_component)
{
  BEGIN_W_COMMAND(do_add_component);

  o_redraw_cleanstates (w_current);

  if (w_current->action_event->state) {
    i_event_end_action_handler(w_current);
  }

  i_status_set_state(w_current, COMPMODE);

  x_compselect_open (w_current);

  EXIT_COMMAND(do_add_component);
}

/*!
 * \brief Action Add Net in i_command_Add_Actions
 * \par Function Description
 *  This is a callback function for the #ADD_NET action.
 */
COMMAND (do_add_net)
{
  BEGIN_W_COMMAND(do_add_net);

  o_redraw_cleanstates(w_current);
  o_net_reset(w_current);

  i_status_set_state(w_current, NETMODE);

  if HOT_ACTION (do_add_net) {
    o_net_start (w_current, CMD_X(do_add_net), CMD_Y(do_add_net));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_net);
}

/*!
 * \brief Action Add Bus in i_command_Add_Actions
 * \par Function Description
 *  This is a callback function for the #ADD_BUS action.
 */
COMMAND (do_add_bus)
{
  BEGIN_W_COMMAND(do_add_bus);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, BUSMODE);

  if HOT_ACTION (do_add_bus) {
    o_bus_start (w_current, CMD_X(do_add_bus), CMD_Y(do_add_bus));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_bus);
}

/*!
 * \brief Action Add Attribute in i_command_Add_Actions
 * \par Function Description
 *  This is the action handler function for #ADD_ATTRIB action.
 *  The function calls x_attrib_add_dialog to launch the Single
 *  Attribute Editor with the SAE_ADD_MODE flag.
 */
COMMAND (do_add_attribute)
{
  BEGIN_W_COMMAND(do_add_attribute);

  GedaObject *o_current = o_select_return_first_object(w_current);

  if (!geda_object_get_attached_to(o_current)) {

    if HOT_ACTION (do_add_attribute) {
      w_current->first_wx = CMD_X(do_add_attribute);
      w_current->first_wy = CMD_Y(do_add_attribute);
    }
    else {
      i_event_end_action_handler(w_current);
    }

    x_attrib_add_dialog(w_current, o_current);
  }
  else {
    geda_log(_("Object is attached to something\n"));
  }

  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_add_attribute);
}

/*!
 * \brief Add Line Mode  in i_command_Add_Actions
 * \par Function Description
 *  This is the action handler function for ADD_LINE.
 */
/** @brief i_cmd_do_add_line in i_command_Command_Functions */
COMMAND (do_add_line)
{
  BEGIN_W_COMMAND(do_add_line);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, LINEMODE);

  if HOT_ACTION (do_add_line) {
    o_line_start (w_current, CMD_X(do_add_line), CMD_Y(do_add_line));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_line);
}

/*!
 * \brief Add Text Mode in i_command_Add_Actions
 * \par Function Description
 *  This is the action handler function for #ADD_TEXT.
 */
COMMAND (do_add_text)
{
  BEGIN_W_COMMAND(do_add_text);

  x_toolbars_turn_off_all_radios(w_current);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_event_cancel_action_handler(w_current);

  x_dialog_text_input(w_current);
  EXIT_COMMAND(do_add_text);
}

/*!
 * \brief Action Add Pin Mode in i_command_Add_Actions
 * \par Function Description
 *  This is the command function for the #ADD_PIN hotkey action.
 */
COMMAND (do_add_pin)
{
  BEGIN_W_COMMAND(do_add_pin);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, PINMODE);

  if HOT_ACTION (do_add_pin) {
    o_pin_start (w_current, CMD_X(do_add_pin), CMD_Y(do_add_pin));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_pin);
}

/*!
 * \brief Action Add Box Mode initiated by Keyboard Hotkey
 * \brief i_cmd_do_add_bix in i_command_Add_Actions
 * \par Function Description
 *  This is the command function for the #ADD_BOX action.
 */
COMMAND (do_add_box)
{
  BEGIN_W_COMMAND(do_add_box);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, BOXMODE);

  if HOT_ACTION (do_add_box) {
    o_box_start (w_current, CMD_X(do_add_box), CMD_Y(do_add_box));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_box);
}

/*!
 * \brief Action Add Circle Mode in i_command_Add_Actions
 * \par Function Description
 *  This is the command function for the #ADD_CIRCLE action.
 */
COMMAND (do_add_circle)
{
  BEGIN_W_COMMAND(do_add_circle);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, CIRCLEMODE);

  if HOT_ACTION (do_add_circle) {
    o_circle_start (w_current, CMD_X(do_add_circle), CMD_Y(do_add_circle));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_circle);
}

/*!
 * \brief Action Add Arc Mode in i_command_Add_Actions
 * \par Function Description
 *  This is the command function for the #ADD_ARC action. An ARC
 *  is slightly different than other adder, an Arc requires users
 *  to draw the radius and then a dialog obtain further input, the
 *  dialog is also the Edit Arc and will react to all currently
 *  selected Arc objects, therefore this routine deselects all
 *  if needed.
 */
COMMAND (do_add_arc)
{
  BEGIN_W_COMMAND(do_add_arc);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, ARCMODE);

  if HOT_ACTION (do_add_arc) {
    o_arc_start (w_current, CMD_X(do_add_arc), CMD_Y(do_add_arc));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_arc);
}

/*!
 * \brief Action Add Path Mode in i_command_Add_Actions
 * \par Function Description
 *  This is the command function for the #ADD_PATH action.
 */
COMMAND (do_add_path)
{
  BEGIN_W_COMMAND(do_add_path);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_status_set_state(w_current, PATHMODE);

  if HOT_ACTION (do_add_path) {
    o_path_start (w_current, CMD_X(do_add_path), CMD_Y(do_add_path));
  }
  else {
    i_event_end_action_handler(w_current);
  }

  EXIT_COMMAND(do_add_path);
}

/*!
 * \brief Action Add Path Mode in i_command_Add_Actions
 * \par Function Description
 *  This is the command function for the #ADD_PICTURE action.
 */
COMMAND (do_add_picture)
{
  BEGIN_W_COMMAND(do_add_picture);

  char *filename = NULL;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_event_end_action_handler(w_current);

  filename = x_fileselect_select_image(w_current, NULL);

  if (filename != NULL) { /* if user did not cancel */

    if (o_picture_set_pixbuf(w_current, filename)) {
      i_status_set_state(w_current, PICTUREMODE);
    }
    else {
      i_status_set_state(w_current, SELECT);
    }
    GEDA_FREE(filename);
  }
  else {
    i_status_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_add_picture);
}

/** @} endgroup i_command_Add_Actions */

/* ------------------- Sessions ---------------- */
/** \defgroup i_command_Sessions_Actions Actions Under the Sessions Menu
 * @{
 */

/*!
 * \brief Create a New Session in i_command_Sessions_Actions
 * \par Function Description
 *  This is the action handler function for #SESSION_NEW.
 */
COMMAND (do_session_new)
{
  BEGIN_W_COMMAND(do_session_new);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  x_sessions_new_dialog (w_current);

  i_status_action_stop(w_current);
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_new);

}

/*!
 * \brief Open an Existing Session in i_command_Sessions_Actions
 * \par Function Description
 *  This is the action handler function for #SESSION_OPEN.
 */
COMMAND (do_session_open)
{
  BEGIN_W_COMMAND(do_session_open);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  x_sessions_open_dialog (w_current);

  i_status_action_stop(w_current);
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_open);
}

/*!
 * \brief Save Session in i_command_Sessions_Actions
 * \par Function Description
 *  This is the action handler function for #SESSION_SAVE.
 *
 * \note If the there is not current session then this option
 *       becomes a session-save-as
 */
COMMAND (do_session_save)
{
  BEGIN_W_COMMAND(do_session_save);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  if (w_current->session_name == NULL) {
    x_sessions_new_dialog (w_current);
  }
  else {
    i_sessions_save_session (w_current, NULL);
  }

  i_status_action_stop(w_current);
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_save);
}

/*!
 * \brief Save Session As in i_command_Sessions_Actions
 * \par Function Description
 *  This is the action handler function for #SESSION_SAVE_AS.
 */
COMMAND (do_session_save_as)
{
  BEGIN_W_COMMAND(do_session_save_as);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if (w_current->session_name == NULL) {
    x_sessions_new_dialog (w_current);
  }
  else {
    x_sessions_save_as_dialog (w_current);
  }

  i_status_action_stop(w_current);
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_save_as);
}

/*!
 * \brief Open Session Manager in i_command_Sessions_Actions
 * \par Function Description
 *  This is the action handler function for #SESSION_MANAGE.
 */
COMMAND (do_session_manage)
{
  BEGIN_W_COMMAND(do_session_manage);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  x_sessions_manage_dialog (w_current);

  i_status_action_stop(w_current);
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_manage);
}

/** @} endgroup i_command_Sessions_Actions */

/* ------------------ Attributes ---------------- */

/** \defgroup i_command_Attribute_Actions Actions Under the Attributes Menu
 * @{
 */

/*!
 * \brief Attach Selected Attributes
 * \par Function Description
 *  This is the action handler function to attach selected attributes
 *  to an object.
 */
/** @brief i_cmd_do_attach in i_command_Attribute_Actions */
COMMAND (do_attach)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

 /* Do Not modify attributes while inside an action */
  BEGIN_NO_ACTION(do_attach);

  w_current->which_object = NULL;

  if (o_select_is_selection(w_current)) {

    GedaObject *first_object;
    GList  *selected;
    int     count;

    count    = o_select_get_count(w_current);
    selected = geda_toplevel_struct_get_selection(w_current->toplevel);

    if (count == 1) {
      geda_log("Feature not implemented\n");
    }
    else if (count == 2) {

      GedaObject *second_object;
      GList  *next;
      bool    first_is_an_attribute;
      bool    second_is_an_attribute;

      first_object  = (GedaObject*)selected->data;
      next          = selected->next;
      second_object = (GedaObject*)next->data;

      first_is_an_attribute  = geda_object_get_is_valid_attribute(first_object);
      second_is_an_attribute = geda_object_get_is_valid_attribute(second_object);

      /* Ensure 1 and only 1 is a valid attribute */
      if (1 == first_is_an_attribute + second_is_an_attribute) {

        if (first_is_an_attribute) {
          if (!geda_object_get_is_attached(first_object)) {
            w_current->which_object = second_object;
          }
        }
        else {
          if (!geda_object_get_is_attached(second_object)) {
            w_current->which_object = first_object;
          }
        }

        if (w_current->which_object) {
          o_attrib_attach_list_2_object(w_current, selected);
        }
        else {
          geda_log(_("Attribute is already attached\n"));
        }
      }
    }
    else {

      GList *iter = selected;

      do {

        /* Find first object that is NOT an attribute */
        if (!geda_object_get_is_valid_attribute(iter->data)) {
          w_current->which_object = iter->data;
          break;
        }

      } while ((NEXT(iter)) != NULL);

      if (w_current->which_object) {
        o_attrib_attach_list_2_object(w_current, selected);
      }
    }
  }

  EXIT_COMMAND(do_attach);
}

/*!
 * \brief Detach Selected Attributes
 * \par Function Description
 *  This is the action handler function to detach selected attributes
 *  from their parent.
 */
/** @brief i_cmd_do_detach in i_command_Attribute_Actions */
COMMAND (do_detach)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);

 /* Do Not modify attributes while inside an action */
  BEGIN_NO_ACTION(do_detach);

  GList *s_current;
  GList *detached_attribs = NULL;

  s_current = geda_toplevel_struct_get_selection(w_current->toplevel);

  while (s_current != NULL) {

    GedaObject *o_current = (GedaObject*)s_current->data;

    if (o_current) {

      if (o_current->attribs) {

        detached_attribs = g_list_concat (g_list_copy (o_current->attribs),
                                          detached_attribs);

        geda_attrib_object_detach_all (o_current);
      }
      else {

        if (o_current->attached_to) {

          geda_attrib_object_detach (o_current);

          detached_attribs = g_list_concat (g_list_copy (o_current->attribs),
                                            detached_attribs);
        }
      }
    }
    NEXT(s_current);
  }

  if (detached_attribs != NULL) {

    o_undo_savestate(w_current, UNDO_ALL);

    g_hook_run_object_list(w_current, DETACH_ATTRIBS_HOOK, detached_attribs);
    g_list_free (detached_attribs);
  }

  EXIT_COMMAND(do_detach);
}

/*!
 * \brief Set Attributes to default X-Y Positions
 * \par Function Description
 *  This is the action handler function to reset Attributes
 *  positions. The function operates on either selected text
 *  attributes or complex objects.
 */
/** @brief do_home_attributes in i_command_Attribute_Actions */
COMMAND (do_home_attributes)
{
 /* Do Not modify attributes while inside an action */
  BEGIN_NO_ACTION(do_home_attributes);

  if (o_select_is_selection (w_current)) {

    bool       modified  = FALSE;
    SELECTION *selection = Current_Selection;

    GList *s_current = geda_list_get_glist (selection);

    while (s_current) {

      GedaObject *object = (GedaObject*)s_current->data;

      if (object->type == OBJ_COMPLEX) {
        if (o_complex_reset_attrib_positions (w_current, object)) {
          modified = TRUE;
        }
      }
      else if ((object->type == OBJ_TEXT && geda_object_get_is_attached(object))) {
        if (o_attrib_reset_position(w_current, object->attached_to, object)) {
          modified = TRUE;
        }
      }

      NEXT(s_current);
    }

    if (modified) {

      /* Get ptr to the current page */
      Page *p_current  = gschem_toplevel_get_current_page(w_current);

      geda_page_set_changed (p_current, TRUE);

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }

  EXIT_COMMAND(do_home_attributes);
}

/*!
 * \brief Set selected Attributes to Show value
 * \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show only the value of the attributes.
 */
/** @brief i_cmd_do_show_value in i_command_Attribute_Actions */
COMMAND (do_show_value)
{
  /* Do Not modify attributes while inside an action */
  BEGIN_NO_ACTION(do_show_value);

  if (o_select_is_selection (w_current)) {

    SELECTION *selection = Current_Selection;

    GList *s_current = geda_list_get_glist (selection);

    while (s_current) {

      GedaObject *object = (GedaObject*)s_current->data;

      if (object->type == OBJ_TEXT) {
        o_attrib_toggle_show_name_value (w_current, object, SHOW_VALUE);
      }
      NEXT(s_current);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }

  EXIT_COMMAND(do_show_value);
}

/*!
 * \brief Set selected Attributes to Show Name
 * \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show only the name of the attributes.
 */
/** @brief i_cmd_do_show_name in i_command_Attribute_Actions */
COMMAND (do_show_name)
{
 /* Do Not modify attributes while inside an action */
  BEGIN_NO_ACTION(do_show_name);

  if (o_select_is_selection (w_current)) {

    SELECTION *selection = Current_Selection;

    GList *s_current = geda_list_get_glist (selection);

    while (s_current) {

      GedaObject *object = (GedaObject*)s_current->data;

      if (object->type == OBJ_TEXT) {
        o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME);
      }
      NEXT(s_current);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }

  EXIT_COMMAND(do_show_name);
}

/*!
 * \brief Set selected Attributes to Show Both
 * \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show both the name and the value of selected attributes.
 */
/** @brief i_cmd_do_show_both in i_command_Attribute_Actions */
COMMAND (do_show_both)
{
  /* Do Not modify attributes while inside an action */
  BEGIN_NO_ACTION(do_show_both);

  if (o_select_is_selection (w_current)) {

    SELECTION *selection = Current_Selection;

    GList *s_current = geda_list_get_glist (selection);

    while (s_current) {

      GedaObject *object = (GedaObject*)s_current->data;

      if (object->type == OBJ_TEXT) {
        o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME_VALUE);
      }
      NEXT(s_current);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }

  EXIT_COMMAND(do_show_both);
}

/*! \brief Toggle Visibility of ALL Attribute Text */
COMMAND (do_toggle_visibility)
{
  /* Do Not toggle visibility while inside an action */
  BEGIN_NO_ACTION(do_toggle_visibility);

  if (o_select_is_selection (w_current)) {

    SELECTION *selection = Current_Selection;

    GList *s_current = geda_list_get_glist (selection);

    while (s_current) {

      GedaObject *object = (GedaObject*)s_current->data;

      if (object->type == OBJ_TEXT) {
        o_attrib_toggle_visibility (w_current, object);
      }
      NEXT(s_current);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }

  EXIT_COMMAND(do_toggle_visibility);
}

/*! \brief Launch the Find Attribute Text Dialog */
COMMAND (do_find_text)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_find_text);

  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    x_dialog_find_text(w_current);
  }
}

/*! \brief Launch the Hide Attribute Text Dialog */
COMMAND (do_hide_text)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_hide_text);
  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
     x_dialog_hide_text(w_current);
  }
}

/*! \brief Launch the Show Attribute Text Dialog */
COMMAND (do_show_text)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_show_text);
  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    x_dialog_show_text(w_current);
  }
}

/*! \brief Launch the Multi-Attributes Dialog */
COMMAND (do_attributes)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_attributes);

  if (!w_current->inside_action)  {  /* Don't execute this inside an action */
    x_multiattrib_open(w_current);
  }
}

/** @} endgroup i_command_Attribute_Actions */

/* ------------------- Tools ----------------- */

/*! \brief Action Responder to Launch the Auto Number Dialog */
COMMAND (do_autonumber)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_autonumber);

  if (!w_current->inside_action)  {  /* Don't execute this inside an action */
    autonumber_text_dialog(w_current);
  }
}


/*!
 * \brief Action Responder to Launch the Log Console Dialog
 * \brief i_cmd_do_show_console in i_command_Option_Actions
 *
 * \par Function Description
 *  This is a callback function to launch the Console Dialog.
 */
COMMAND (do_show_console)
{
  BEGIN_COMMAND(do_show_console);
  x_console_open (w_current);
  EXIT_COMMAND(do_show_console);
}

/*!
 * \brief Action Responder to Launch the Coordinates Dialog
 * \brief i_cmd_do_show_coordinates in i_command_Option_Actions
 * \par Function Description
 *  This is a callback function to launch the Coordinates Dialog.
 */
COMMAND (do_show_coordinates)
{
  BEGIN_COMMAND(do_show_coordinates);
  x_dialog_coord_dialog (w_current);
  EXIT_COMMAND(do_show_coordinates);
}

/*!
 * \brief Toggle Macro Entry Area
 * \brief i_cmd_do_macro in i_command_Edit_Actions
 * \par Function Description
 *  This function set the macro widget to visable and set focus to
 *  to the entry object member.
 */
COMMAND (do_macro)
{
  BEGIN_W_COMMAND(do_macro);

  GtkWidget *widget = w_current->macro_widget;

  if (gtk_widget_get_visible (widget)) {
    gtk_widget_hide(widget);
  }
  else {
    gtk_widget_show (widget);
    gtk_widget_grab_focus (gschem_macro_widget_get_entry(widget));
  }
  EXIT_COMMAND(do_macro);
}

/*! \brief Launch the Guile Path Dialog */
COMMAND (do_guile_path)
{
  BEGIN_NO_ARGUMENT(do_guile_path);
  x_guile_dialog(w_current);
}

/*! \brief Edit Translate in i_command_Edit_Actions */
COMMAND (do_translate)
{
  BEGIN_W_COMMAND(do_translate);

  if (w_current->snap == SNAP_OFF) {
    geda_log(_("WARNING: Do not translate with snap off!\n"));
    geda_log(_("WARNING: Turning snap on and continuing with translate.\n"));
    w_current->snap = SNAP_GRID;
    i_status_show_state(w_current, NULL); /* update status on screen */
  }

  if (w_current->snap_size != 100) {
    geda_log(_("WARNING: Snap grid size is not equal to 100!\n"));
    geda_log(_("WARNING: If you are translating a symbol "
                    "to the origin, the snap grid size should be "
                    "set to 100\n"));
  }

  x_dialog_translate (w_current);
  EXIT_COMMAND(do_translate);
}

COMMAND (do_embed)
{
  BEGIN_W_COMMAND(do_embed);

  /* anything selected ? */
  if (o_select_is_selection(w_current)) {
    /* yes, embed each selected component */
    GList *s_current = geda_list_get_glist(Current_Selection);

    while (s_current != NULL) {

      GedaObject *o_current = (GedaObject*)s_current->data;

      if (o_current != NULL) {
        if ( (o_current->type == OBJ_COMPLEX) ||
             (o_current->type == OBJ_PICTURE)) {
          geda_object_embed (o_current);
        }
      }
      NEXT(s_current);
    }
    o_undo_savestate(w_current, UNDO_ALL);
  }
  else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_embed);
}

/** @brief i_cmd_unembed in i_command_Edit_Actions */
COMMAND (do_unembed)
{
  BEGIN_W_COMMAND(do_unembed);

  /* anything selected ? */
  if (o_select_is_selection(w_current)) {

    /* yes, unembed each selected component */
    GList *s_current = geda_list_get_glist(Current_Selection);
    GList *not_found = NULL;

    while (s_current != NULL) {

      GedaObject *o_current = (GedaObject*)s_current->data;

      if (o_current != NULL) {
        if ((o_current->type == OBJ_COMPLEX) ||
            (o_current->type == OBJ_PICTURE)) {

          int result;

           result = geda_object_unembed (o_current);

          if (result == 2) {
            not_found = g_list_append(not_found, o_current);
          }
        }
      }
      NEXT(s_current);
    }
    o_undo_savestate(w_current, UNDO_ALL);
  }
  else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_unembed);
}

/** @brief i_cmd_update in i_command_Edit_Actions */
COMMAND (do_update)
{
  BEGIN_W_COMMAND(do_update);

  if (o_select_is_selection(w_current)) {

    Page  *p_current;
    GList *selection;
    GList *selected_components = NULL;
    GList *iter;

    /* Updating components modifies the selection. Therefore, create a
     * new list of only the Complex objects from the current selection,
     * then iterate over that new list to perform the update. */
    p_current = gschem_toplevel_get_current_page(w_current);
    selection = geda_page_get_selection_list (p_current);

    while (selection) {

      GedaObject *o_current = (GedaObject*)selection->data;

      if (o_current != NULL && o_current->type == OBJ_COMPLEX) {
        selected_components = g_list_prepend (selected_components, o_current);
      }

      NEXT (selection);
    }

    for (iter = selected_components; iter != NULL; NEXT(iter)) {

      GedaObject *o_current = (GedaObject*)iter->data;

      iter->data = o_edit_update_component (w_current, o_current);
    }

    g_list_free (selected_components);

  }
  else {
    /* nothing selected, go back to select state */
    geda_log("Nothing selected\n");
    o_redraw_cleanstates(w_current);
    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
  }

  EXIT_COMMAND(do_update);
}

/* ------------------ Options ---------------- */

/** \defgroup i_command_Option_Actions Actions Under the Options Menu
 * @{
 */

/*! \brief Set the Grid Display to Dots Mode */
COMMAND (do_grid_dots)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_dots);
  w_current->grid_mode = GRID_DOTS;
  x_grid_configure_variables (w_current);
  i_status_update_grid_info (w_current);
  x_menu_set_grid_radio (w_current);
  o_invalidate_all (w_current);
}
/*! \brief Set the Grid Display to Mesh Mode */
COMMAND (do_grid_mesh)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_mesh);
  w_current->grid_mode = GRID_MESH;
  x_grid_configure_variables (w_current);
  i_status_update_grid_info (w_current);
  x_menu_set_grid_radio (w_current);
  o_invalidate_all (w_current);
}
/*! \brief Turn the Grid Display Off in i_command_Option_Actions */
COMMAND (do_grid_off)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_off);
  w_current->grid_mode = GRID_NONE;
  i_status_update_grid_info (w_current);
  x_menu_set_grid_radio (w_current);
  o_invalidate_all (w_current);
}

/*! \brief Cycle the Grid Mode in i_command_Option_Actions */
COMMAND (do_cycle_grid)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_cycle_grid);

  switch (w_current->grid_mode) {
    case GRID_NONE: w_current->grid_mode = GRID_DOTS; break;
    case GRID_DOTS: w_current->grid_mode = GRID_MESH; break;
    case GRID_MESH: w_current->grid_mode = GRID_NONE; break;
  }

  switch (w_current->grid_mode) {
    case GRID_NONE: geda_log_q (_("Grid OFF\n"));            break;
    case GRID_DOTS: geda_log_q (_("Dot grid activated\n"));  break;
    case GRID_MESH: geda_log_q (_("Mesh grid activated\n")); break;
  }

  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);

}

/*! \brief Increase the Snap Scale in i_command_Option_Actions */
COMMAND (do_snap_up)
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_snap_up);
  w_current->snap_size *= 2;

  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_snap_up);
}

/*! \brief Decrease the Snap Scale in i_command_Option_Actions */
COMMAND (do_snap_down)
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_snap_down);
  if (w_current->snap_size % 2 == 0)
    w_current->snap_size /= 2;

  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_snap_down);
}

/*! \brief Launch the Snap Settings Dialog */
COMMAND (do_show_snap)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_show_snap);
  snap_size_dialog(w_current);
}

COMMAND (do_snap_off)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_snap_off);

  if (w_current->snap != SNAP_OFF)
     w_current->old_snap = w_current->snap;
  w_current->snap = SNAP_OFF;
  i_status_show_state(w_current, NULL);  /* update status on screen */
  i_status_update_grid_info (w_current); /* update on screen grid status */

}
/*! \brief Turn-On Snap Mode in i_command_Option_Actions */
COMMAND (do_snap_on)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_snap_on);

  if (w_current->old_snap != SNAP_OFF)
    w_current->snap =  w_current->old_snap;
  else
    w_current->snap = SNAP_GRID;

  i_status_show_state(w_current, NULL);  /* update status on screen */
  i_status_update_grid_info (w_current); /* update on screen grid status */

}

/*! \brief Cycle the Snap Mode in i_command_Option_Actions */
COMMAND (do_cycle_snap)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_cycle_snap);
  /* toggle to the next snap state */
  w_current->snap = (w_current->snap+1) % SNAP_STATE_COUNT;

  switch (w_current->snap) {
  case SNAP_OFF:
    geda_log_q(_("Snap OFF (CAUTION!)\n"));
    x_menu_set_togglable(w_current, SNAP_TOGGLE, FALSE);
    break;

  case SNAP_GRID:
    x_menu_set_togglable(w_current, SNAP_TOGGLE, TRUE);
    geda_log_q(_("Snap ON\n"));
    break;

  case SNAP_RESNAP:
    x_menu_set_togglable(w_current, SNAP_TOGGLE, TRUE);
    geda_log_q(_("Snap back to the grid (CAUTION!)\n"));
    break;

  default:
    g_critical("options_snap: toplevel->snap out of range: %d\n", w_current->snap);
  }
  i_status_show_state(w_current, NULL);  /* update status on screen */
  i_status_update_grid_info (w_current); /* update on screen grid status */
}

/*! \brief Toggle Rubberband Mode in i_command_Option_Actions
 *  \par Function Description
 *  This is a callback function for the Toggle Rubberband action API.
 *  \note
 *
 *  Rubber band is cool !
 *  Chris Ellec - January 2001:Added on/off option from the pull down menu
 *  Wiley E. Hill- December 2012:Changed to Menu Toggle Button
 */
COMMAND (do_toggle_rubberband)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_rubberband);
  if (w_current->netconn_rubberband) {
    w_current->netconn_rubberband = 0;
    geda_log_q(_("Rubber band mode is OFF\n"));
  }
  else {
    w_current->netconn_rubberband = 1;
    geda_log_q(_("Rubber band mode is ON\n"));
  }
  x_menu_set_togglable(w_current, RUBBER_TOGGLE, w_current->netconn_rubberband);
}

/*! \brief Toggle Magnetic Nets Mode in i_command_Option_Actions */
COMMAND (do_toggle_magneticnet)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_magneticnet);
  if ((w_current->magnetic_net_mode = !w_current->magnetic_net_mode)) {
    geda_log_q(_("Magnetic net mode is ON\n"));
  }
  else {
    geda_log_q(_("Magnetic net mode is OFF\n"));
  }
  x_menu_set_togglable(w_current, MAGNETIC_TOGGLE, w_current->magnetic_net_mode);
  i_status_show_state(w_current, NULL);
}

/*! \brief Toggle Drag-Can-Move Mode in i_command_Option_Actions
 *  \par Function Description
 *  This is a callback function for the Toggle draw-can-move action API.
 */
COMMAND (do_toggle_dragcanmove)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_dragcanmove);
  const char *OnOff;
  if (w_current->drag_can_move) {
    w_current->drag_can_move = 0;
    OnOff =_("Off");
  }
  else {
    w_current->drag_can_move = 1;
    OnOff =_("On");
  }
  geda_log_q("%s %s\n", _("Drag-Can-Move is now"), OnOff);
  x_menu_set_togglable(w_current, DRAG_CAN_MOVE, w_current->drag_can_move);
}

/*! \brief Toggle Action Feedback Mode in i_command_Option_Actions */
COMMAND (do_toggle_feedback)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_feedback);

  if (w_current->action_feedback_mode == BOUNDINGBOX) {
    w_current->action_feedback_mode = OUTLINE;
    geda_log_q(_("Action feedback mode set to OUTLINE\n"));
  }
  else {
    w_current->action_feedback_mode = BOUNDINGBOX;
    geda_log_q(_("Action feedback mode set to BOUNDINGBOX\n"));
  }

  if (w_current->inside_action && Current_PlaceList != NULL) {
    o_place_invalidate_rubber (w_current, FALSE);
  }

  x_menu_set_togglable(w_current, OUTLINE_TOGGLE, w_current->action_feedback_mode);
}

/*! \brief Toggle Auto-Pan Mode in i_command_Option_Actions
 *  \par Function Description
 *  This is a callback function for the Toggle options-auto-pan action API.
 */
COMMAND (do_toggle_auto_pan)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_auto_pan);
  const char *OnOff;
  if (w_current->auto_pan) {
    w_current->auto_pan = 0;
    OnOff =_("Off");
  }
  else {
    w_current->auto_pan = 1;
    OnOff =_("On");
  }
  geda_log_q("%s %s\n", _("Auto-Pan is now"), OnOff);
  x_menu_set_togglable(w_current, AUTO_PAN_TOGGLE, w_current->auto_pan);
}

/*! \brief Launch the Show Text Dialog */
COMMAND (do_show_text_size)
{
  NOT_NULL(w_current);

  BEGIN_COMMAND(do_show_text_size);
  text_size_dialog(w_current);
  EXIT_COMMAND(do_show_text_size);
}

/*! \brief Preferences Dialog Action Responder API Function
 *  @brief i_cmd_do_show_settings in i_command_Option_Actions
 *
 *  \par Function Description
 *  This is a callback function to Launch the Preferences Dialog.
 *
 *  Author: Wiley E. Hill
 *  Date:   Aug 5th, 2012
 */
COMMAND (do_show_settings)
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_show_settings);
  x_configure_settings(w_current); /* Load and display Dialog */
  EXIT_COMMAND(do_show_settings);
}

/** @} endgroup i_command_Option_Actions */

/* ------------------ Help ---------------- */

/** \defgroup i_command_Help_Actions Actions Under the Help Menu
 * @{
 */

/*! \brief Spawn the Help Guide in Browser */
COMMAND (do_show_manual)
{
  BEGIN_COMMAND(do_show_manual);

  char *pathname = NULL;

  pathname = g_build_filename (geda_sys_doc_path (), "wiki",
                               HELP_GSCHEM_GUIDE_HTML, NULL);
  if (pathname) {

    if (!x_show_uri (pathname)) {

      geda_log("%s \"%s\"\n", _("Check path:"), pathname);

    }

    GEDA_FREE(pathname);
  }

  EXIT_COMMAND(do_show_manual);
}

/*! \brief Launch the Help Hotkeys Dialog */
COMMAND (do_show_hotkeys)
{
  BEGIN_COMMAND(do_show_hotkeys);
  x_dialog_hotkeys(w_current);
  EXIT_COMMAND(do_show_hotkeys);
}

/*! \brief Spawn the Help FAQ in Browser */
COMMAND (do_show_faq)
{
  BEGIN_COMMAND(do_show_faq);

  char *pathname = NULL;
  pathname = g_build_filename (geda_sys_doc_path (), "wiki",
                               HELP_GSCHEM_FAQ_HTML, NULL);
  if (pathname) {

    if (!x_show_uri (pathname)) {

      geda_log("%s \"%s\"\n", _("Check path:"), pathname);

    }

    GEDA_FREE(pathname);
  }
  EXIT_COMMAND(do_show_faq);
}

/*! \brief Spawn the Help Geda in Browser */
COMMAND (do_show_geda)
{
  BEGIN_COMMAND(do_show_geda);

  char *pathname = NULL;
  pathname = g_build_filename (geda_sys_doc_path (), "wiki",
                               HELP_GEDA_DOC_HTML, NULL);
  if (pathname) {

    if (!x_show_uri (pathname)) {

      geda_log("%s \"%s\"\n", _("Check path:"), pathname);

    }
    GEDA_FREE(pathname);
  }
  EXIT_COMMAND(do_show_geda);
}

/*! \brief Spawn the Help Wiki in Browser */
COMMAND (do_show_wiki)
{
  BEGIN_COMMAND(do_show_wiki);

  char *pathname = NULL;
  pathname = g_build_filename (geda_sys_doc_path (), "wiki", HELP_GEDA_WIKI_HTML, NULL);

  if (pathname) {

    if (!x_show_uri (pathname)) {

      geda_log("%s \"%s\"\n", _("Check path:"), pathname);

    }
    GEDA_FREE(pathname);
  }
  EXIT_COMMAND(do_show_wiki);
}

/*! \brief Launch the Help About Dialog */
COMMAND (do_show_about)
{
  BEGIN_NO_ARGUMENT(do_show_about);
  about_dialog(w_current);
}

/** @} endgroup i_command_Help_Actions */

/** @} END Group i_command_Action_Functions */

/** \defgroup i_command_Variable_Handlers Handlers for Preference Variables
 *  @{
 *  TODO: Currently i_command_Variable_Handlers only display the
 *        value of varible, should also be able to set the variables
 *        (maybe optional command sequence "Set Varible")
 */

/** @brief i_cmd_draw_grips in i_command_Variable_Handlers */
COMMAND (draw_grips) {
  SHOW_VARIABLE(draw_grips, R);
}

/** @brief i_cmd_grid_mode in i_command_Variable_Handlers */
COMMAND (grid_mode) {
  SHOW_VARIABLE(grid_mode, W);
}

/** @brief i_cmd_dots_grid_dot_size in i_command_Variable_Handlers */
COMMAND (dots_grid_dot_size) {
  SHOW_VARIABLE(grid_mode, W);
}

/** @brief i_cmd_dots_grid_threshold in i_command_Variable_Handlers */
COMMAND (dots_grid_threshold) {

  SHOW_VARIABLE(dots_grid_threshold, W);
}

/** @brief i_cmd_dots_grid_mode in i_command_Variable_Handlers */
COMMAND (dots_grid_mode) {

  SHOW_VARIABLE(dots_grid_mode, W);
}

/** @brief i_cmd_mesh_grid_threshold in i_command_Variable_Handlers */
COMMAND (mesh_grid_threshold) {

  SHOW_VARIABLE(mesh_grid_threshold, W);
}

/** @brief i_cmd_object_clipping in i_command_Variable_Handlers */
COMMAND (object_clipping) {

  SHOW_VARIABLE(object_clipping, W);
}

/** @brief i_cmd_scrollbars in i_command_Variable_Handlers */
COMMAND (scrollbars) {

  SHOW_VARIABLE(scrollbars, W);
}

/** @brief i_cmd_scrollbar_update in i_command_Variable_Handlers */
COMMAND (scrollbar_update) {

  SHOW_VARIABLE(scrollbar_update, W);
}

/** @brief i_cmd_scrollbars_visible in i_command_Variable_Handlers */
COMMAND (scrollbars_visible) {

  SHOW_VARIABLE(scrollbars_visible, W);
}

/** @brief i_cmd_scrollpan_steps in i_command_Variable_Handlers */
COMMAND (scrollpan_steps) {

  SHOW_VARIABLE(scrollpan_steps, W);
}

/** @brief i_cmd_warp_cursor in i_command_Variable_Handlers */
COMMAND (warp_cursor) {

  SHOW_VARIABLE(warp_cursor, W);
}

/** @brief i_cmd_world_size in i_command_Variable_Handlers */
COMMAND (world_size) {

  int width  = w_current->world_right;
  int height = w_current->world_bottom;
  geda_log(_("(read only) width=%d, height=%d\n"), width, height);
}

/** @brief i_cmd_zoom_gain in i_command_Variable_Handlers */
COMMAND (zoom_gain) {

  SHOW_VARIABLE(zoom_gain, W);
}

/** @brief i_cmd_zoom_with_pan in i_command_Variable_Handlers */
COMMAND (zoom_with_pan) {

  SHOW_VARIABLE(zoom_gain, W);
}

/** @brief i_cmd_logging in i_command_Variable_Handlers */
COMMAND (logging) {
  SHOW_VARIABLE(logging, G);
}

/** @brief i_cmd_log_destiny in i_command_Variable_Handlers */
COMMAND (log_destiny) {
  SHOW_VARIABLE(log_destiny, G);
}

/** @brief i_cmd_console_window in i_command_Variable_Handlers */
COMMAND (console_window) {
  SHOW_VARIABLE(console_window, G);
}

/** @brief i_cmd_console_window_type in i_command_Variable_Handlers */
COMMAND (console_window_type) {
  SHOW_VARIABLE(console_window_type, G);
}

/** @brief i_cmd_action_feedback_mode in i_command_Variable_Handlers */
COMMAND (action_feedback_mode) {

  SHOW_VARIABLE(action_feedback_mode, W)
}

/** @brief i_cmd_add_attribute_offset in i_command_Variable_Handlers */
COMMAND (add_attribute_offset) {

  SHOW_VARIABLE(add_attribute_offset, W);
}

/** @brief i_cmd_auto_load_last in i_command_Variable_Handlers */
COMMAND (auto_load_last) {
  SHOW_VARIABLE(auto_load_last, G);
}

/** @brief i_cmd_auto_pan in i_command_Variable_Handlers */
COMMAND (auto_pan) {
  SHOW_VARIABLE(auto_pan, W);
}

/** @brief i_cmd_auto_pan_step in i_command_Variable_Handlers */
COMMAND (auto_pan_step) {
  SHOW_VARIABLE(auto_pan_step, W);
}

/** @brief i_cmd_auto_save_interval in i_command_Variable_Handlers */
COMMAND (auto_save_interval) {

  SHOW_VARIABLE(auto_save_interval, T);
}

/** @brief i_cmd_attribute_placement_grid in i_command_Variable_Handlers */
COMMAND (attribute_placement_grid) {

  SHOW_VARIABLE(attribute_placement_grid, W);
}

/** @brief i_cmd_continue_component_place in i_command_Variable_Handlers */
COMMAND (continue_component_place) {

  SHOW_VARIABLE(continue_component_place, W);
}

/** @brief i_cmd_embed_components in i_command_Variable_Handlers */
COMMAND (embed_components) {

  SHOW_VARIABLE(embed_components, W);
}

/** @brief i_cmd_enforce_hierarchy in i_command_Variable_Handlers */
COMMAND (enforce_hierarchy) {

  SHOW_VARIABLE(enforce_hierarchy, W);
}

/** @brief i_cmd_hierarchy_up_close in i_command_Variable_Handlers */
COMMAND (hierarchy_up_close) {

  SHOW_VARIABLE(hierarchy_up_close, W);
}

/** @brief i_cmdfile_preview in i_command_Variable_Handlers */
COMMAND (file_preview) {

  SHOW_VARIABLE(file_preview, W);
}

/** @brief i_cmd_force_boundingbox in i_command_Variable_Handlers */
COMMAND (force_boundingbox) {

  SHOW_VARIABLE(force_boundingbox, W);
}

/** @brief i_cmd_keyboardpan_gain in i_command_Variable_Handlers */
COMMAND (keyboardpan_gain) {

  SHOW_VARIABLE(keyboardpan_gain, W);
}

/** @brief i_cmd_magnetic_net_mode in i_command_Variable_Handlers */
COMMAND (magnetic_net_mode) {

  SHOW_VARIABLE(magnetic_net_mode, W);
}

/** @brief i_cmd_netconn_rubberband in i_command_Variable_Handlers */
COMMAND (netconn_rubberband) {

  SHOW_VARIABLE(netconn_rubberband, W);
}

/** @brief i_cmd_raise_dialog_boxes in i_command_Variable_Handlers */
COMMAND (raise_dialog_boxes) {

  SHOW_VARIABLE(raise_dialog_boxes, W);
}

/** @brief i_cmd_select_slack_pixels in i_command_Variable_Handlers */
COMMAND (select_slack_pixels) {

  SHOW_VARIABLE(select_slack_pixels, W);
}

/** @brief i_cmd_snap_size in i_command_Variable_Handlers */
COMMAND (snap_size) {

  SHOW_VARIABLE(snap_size, W);
}

/** @brief i_cmd_sort_component_library in i_command_Variable_Handlers */
COMMAND (sort_component_library) {

  SHOW_VARIABLE(sort_component_library, W);
}

/** @brief i_cmd_untitled_name in i_command_Variable_Handlers */
COMMAND (untitled_name) {

  geda_log("<%s>", w_current->toplevel->untitled_name);
}

/** @brief i_cmd_net_consolidate in i_command_Variable_Handlers */
COMMAND (net_consolidate) {

  SHOW_VARIABLE(net_consolidate, T);
}

/** @brief i_cmd_net_endpoint_mode in i_command_Variable_Handlers */
COMMAND (net_endpoint_mode) {

  SHOW_VARIABLE(net_endpoint_mode, W);
}

/** @brief i_cmd_net_midpoint_mode in i_command_Variable_Handlers */
COMMAND (net_midpoint_mode) {

  SHOW_VARIABLE(net_midpoint_mode, W);
}

/** @brief i_cmd_net_direction_mode in i_command_Variable_Handlers */
COMMAND (net_direction_mode) {

  SHOW_VARIABLE(net_direction_mode,W);
}

/** @brief i_cmd_net_selection_mode in i_command_Variable_Handlers */
COMMAND (net_selection_mode) {

  SHOW_VARIABLE(net_selection_mode, W)
}

/** @brief i_cmd_bus_style in i_command_Variable_Handlers */
COMMAND (bus_style) {

  SHOW_VARIABLE(bus_style, T)
}
/** @brief i_cmd_net_style in i_command_Variable_Handlers */
COMMAND (net_style) {

  SHOW_VARIABLE(net_style, T)
}
/** @brief i_cmd_pin_style in i_command_Variable_Handlers */
COMMAND (pin_style) {

  SHOW_VARIABLE(pin_style, T)
}
/** @brief i_cmd_line_style in i_command_Variable_Handlers */
COMMAND (line_style) {

  SHOW_VARIABLE(line_style, T)
}
/** @brief i_cmd_thick_bus_width in i_command_Variable_Handlers */
COMMAND (thick_bus_width) {

  SHOW_VARIABLE(thick_bus_width, T)
}
/** @brief i_cmd_thick_line_width in i_command_Variable_Handlers */
COMMAND (thick_line_width) {

  SHOW_VARIABLE(thick_line_width, T)
}
/** @brief i_cmd_thick_line_width in i_command_Variable_Handlers */
COMMAND (thick_net_width) {

  SHOW_VARIABLE(thick_net_width, T)
}
/** @brief i_cmd_thick_pin_width in i_command_Variable_Handlers */
COMMAND (thick_pin_width) {

  SHOW_VARIABLE(thick_pin_width, T)
}
/** @brief i_cmd_thick_line_width in i_command_Variable_Handlers */
COMMAND (thin_bus_width) {

  SHOW_VARIABLE(thin_bus_width, T)
}
/** @brief i_cmd_thin_line_width in i_command_Variable_Handlers */
COMMAND (thin_line_width) {

  SHOW_VARIABLE(thin_line_width, T)
}
/** @brief i_cmd_thin_net_width in i_command_Variable_Handlers */
COMMAND (thin_net_width) {

  SHOW_VARIABLE(thin_net_width, T)
}
/** @brief i_cmd_thin_pin_width in i_command_Variable_Handlers */
COMMAND (thin_pin_width) {

  SHOW_VARIABLE(thin_pin_width, T)
}

/** @brief i_cmd_bus_ripper_rotation in i_command_Variable_Handlers */
COMMAND (bus_ripper_rotation) {

  SHOW_VARIABLE(bus_ripper_rotation, W)
}

/** @brief i_cmd_bus_ripper_size in i_command_Variable_Handlers */
COMMAND (bus_ripper_size) {

  SHOW_VARIABLE(bus_ripper_size, W)
}
/** @brief i_cmd_bus_ripper_type in i_command_Variable_Handlers */
COMMAND (bus_ripper_type) {

  SHOW_VARIABLE(bus_ripper_type, W)
}
/** @brief i_cmd_bus_ripper_symname in i_command_Variable_Handlers */
COMMAND(bus_ripper_symname) {

  geda_log("<%s>", w_current->bus_ripper_symname);
}

/** @brief i_cmd_fast_mousepan in i_command_Variable_Handlers */
COMMAND (fast_mousepan) {

  SHOW_VARIABLE(fast_mousepan, W)
}

/** @brief i_cmd_drag_can_move in i_command_Variable_Handlers */
COMMAND (drag_can_move) {

  SHOW_VARIABLE(drag_can_move, W)
}

/** @brief i_cmd_middle_button in i_command_Variable_Handlers */
COMMAND (middle_button) {

  SHOW_VARIABLE(middle_button, W)
}

/** @brief i_cmd_third_button in i_command_Variable_Handlers */
COMMAND (third_button) {

  SHOW_VARIABLE(third_button, W)
}

/** @brief i_cmd_mousepan_gain in i_command_Variable_Handlers */
COMMAND (mousepan_gain) {

  SHOW_VARIABLE(mousepan_gain, W)
}

/** @brief i_cmd_scroll_wheel in i_command_Variable_Handlers */
COMMAND (scroll_wheel) {

  SHOW_VARIABLE(scroll_wheel, W)
}

/** @brief i_cmd_image_color in i_command_Variable_Handlers */
COMMAND (image_color) {

  SHOW_VARIABLE(image_color, T)
}
/** @brief i_cmd_invert_images in i_command_Variable_Handlers */
COMMAND (invert_images) {

  SHOW_VARIABLE(invert_images, T)
}
/** @brief i_cmd_text_case in i_command_Variable_Handlers */
COMMAND (text_case) {

  SHOW_VARIABLE(text_case, W)
}

/** @brief i_cmd_text_display_zoomfactor in i_command_Variable_Handlers */
COMMAND (text_display_zoomfactor) {

  SHOW_VARIABLE(text_display_zoomfactor, W)
}

/** @brief i_cmd_text_feedback in i_command_Variable_Handlers */
COMMAND (text_feedback) {

  SHOW_VARIABLE(text_feedback, W)
}

/** @brief i_cmd_text_origin_marker in i_command_Variable_Handlers */
COMMAND (text_origin_marker) {

  SHOW_VARIABLE(text_origin_marker, R)
}
/** @brief i_cmd_text_marker_size in i_command_Variable_Handlers */
COMMAND (text_marker_size) {

  SHOW_VARIABLE(text_marker_size, R)
}
/** @brief i_cmd_text_marker_threshold in i_command_Variable_Handlers */
COMMAND (text_marker_threshold) {
  const char *msg = _("current value of");
  geda_log("%s <%s> %s <%.1f>\n", msg, "text-marker-threshold", _("is"),
            w_current->cairo_renderer->text_marker_threshold);
}
/** @brief i_cmd_text_size in i_command_Variable_Handlers */
COMMAND (text_size) {

  SHOW_VARIABLE(text_size, W)
}

/** @brief i_cmd_undo_control in i_command_Variable_Handlers */
COMMAND (undo_control) {

  SHOW_VARIABLE(undo_control, W)
}

/** @brief i_cmd_undo_levels in i_command_Variable_Handlers */
COMMAND (undo_levels) {

  SHOW_VARIABLE(undo_levels, W)
}

/** @brief i_cmd_undo_panzoom in i_command_Variable_Handlers */
COMMAND (undo_panzoom) {

  SHOW_VARIABLE(undo_panzoom, W)
}

/** @brief i_cmd_undo_preserve in i_command_Variable_Handlers */
COMMAND (undo_preserve) {

  SHOW_VARIABLE(undo_preserve, W)
}

/** @brief i_cmd_undo_type in i_command_Variable_Handlers */
COMMAND (undo_type) {

  SHOW_VARIABLE(undo_type, W)
}

/** @} END Group i_command_Variable_Handlers */
/** @} END Group i_command Command Functions */

#undef CairoRenderer
#undef Toplevel
#undef CMD_FUNC
#undef CMD_OPTIONS
#undef CMD_NAME
#undef CMD_INTEGER
#undef COMMAND
#undef WCURRENT
#undef NO_ACTION
#undef BEGIN_NO_ACTION
