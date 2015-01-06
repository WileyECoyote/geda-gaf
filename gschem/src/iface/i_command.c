/* -*- i_command.c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
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

#include <gschem.h>

#include "i_command.h"
#include "x_menus.h"

#include <geda_dialogs.h>
#include <geda_debug.h>

#define PERFORMANCE
#ifdef PERFORMANCE

# include <sys/time.h>
# include <sys/resource.h>
# include "rusage/tlpi_hdr.h"
#endif

#define NUMBER_REDRAW_TEST      100
#define NUMBER_UNDO_TEST         15

#define USE_POSIX
#define MAX_THREADS              12
#define MAX_THREADS_UNUSED        2
#define MAX_THREADS_IDLE_TIME  1000   /* microseconds */
#define WAIT_THREADS_IDLE_TIME    5
#define TASK_WAIT_INTERVAL      100   /* microseconds */
#define MAX_WAIT_FOR_TASK        10   /* TASK_WAIT_INTERVAL */

/* These are just to reduce lines lengths */
#define CairoRenderer w_current->cairo_renderer
#define Toplevel      w_current->toplevel

#define COMMAND(symbol, repeat, aflag,  func) [ cmd_##func ] = \
{ ACTION(symbol), repeat, 0, aflag, i_cmd_##func, 0, {0, 0}, 0, 0, 0},

/*! \brief Enumerated Performance Diagnostics Directives
 *  \par Description
 *  Performance Diagnostics directives are include in the build if
 *  the PERFORMANCE options is defined. These enumerator are for
 *  dialog responses to the Diagnostics dialog, which is accessible
 *  by typing "debug" in the command console.
 */
typedef enum { CLOSE_PERFORMANCE, /* = 0, means not compiled in */
               TOGGLE_RUSAGE_DATA,
               RUN_REDRAW_TESTS,
               RUN_UNDO_TESTS,
} EID_PERFORMANCE;

#ifdef PERFORMANCE

static bool performance_diagnostics = FALSE;

void printRusage(const char *leader, const struct rusage *ru)
{
    const char *ldr;

    ldr = (leader == NULL) ? "" : leader;

    printf("%sCPU time (secs):         user=%.3f; system=%.3f\n", ldr,
            ru->ru_utime.tv_sec + ru->ru_utime.tv_usec / 1000000.0,
            ru->ru_stime.tv_sec + ru->ru_stime.tv_usec / 1000000.0);
    printf("%sMax resident set size:   %ld\n", ldr, ru->ru_maxrss);
    printf("%sIntegral shared memory:  %ld\n", ldr, ru->ru_ixrss);
    printf("%sIntegral unshared data:  %ld\n", ldr, ru->ru_idrss);
    printf("%sIntegral unshared stack: %ld\n", ldr, ru->ru_isrss);
    printf("%sPage reclaims:           %ld\n", ldr, ru->ru_minflt);
    printf("%sPage faults:             %ld\n", ldr, ru->ru_majflt);
    printf("%sSwaps:                   %ld\n", ldr, ru->ru_nswap);
    printf("%sBlock I/Os:              input=%ld; output=%ld\n",
            ldr, ru->ru_inblock, ru->ru_oublock);
    printf("%sSignals received:        %ld\n", ldr, ru->ru_nsignals);
    printf("%sIPC messages:            sent=%ld; received=%ld\n",
            ldr, ru->ru_msgsnd, ru->ru_msgrcv);
    printf("%sContext switches:        voluntary=%ld; "
            "involuntary=%ld\n", ldr, ru->ru_nvcsw, ru->ru_nivcsw);
    printf("------------- End Report-------------\n\n");
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
   unsigned char status;                     /* set inside hander, used for blocking */
   unsigned char aflag;                      /* see note #1 in i_command.h */
   void (*func) (GschemToplevel *w_current); /* ptr to action handler function */
   int   who;                                /* enumerated caller */

   struct {
   int     x;
   int     y;
   } point;

   int   narg;
   unsigned char *sarg;
   GschemToplevel *w_current;
} command_struc[COMMAND_COUNT] = {
 [ cmd_unknown ] = { "unknown", 0, 0, 0, 0, 0, {0, 0}, 0, 0},

 #include "i_command.h"
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
  u_log_message("current value of <%s> is <%d>\n", #name, CMD_INTEGER(name));

/* Anonymous Static GMutex */
static union
{
  void* p;
  unsigned int i[2];
} i_lock_last_command;

static GThreadPool  *CommandPool     = NULL;
static int           is_engaged      = -1;
static int           last_command    =  0;

static void set_last_command(int value)
{
  g_mutex_lock((GMutex*)&i_lock_last_command);
    last_command = value;
  g_mutex_unlock((GMutex*)&i_lock_last_command);
}
static int get_last_command()
{
  int ret_val;
  g_mutex_lock((GMutex*)&i_lock_last_command);
    ret_val = last_command;
  g_mutex_unlock((GMutex*)&i_lock_last_command);
  return ret_val;
}

static bool i_command_dispatch(gschem_task *task)
{
  scm_dynwind_begin (0);
  g_dynwind_window (task->arg1); /* w_current */
  if (task->arg2) task->func.F2(task->arg1,task->arg2);
  else task->func.F1(task->arg1);
  scm_dynwind_end ();
  GEDA_FREE(task);
  return FALSE;
}

static
void i_command_router(char* command, GschemToplevel *w_current)
{
  int accelerator;
  int i;
  gschem_task *task;

  inline void route(int i) {
    /* see note #1 in i_command.h */
    if ((command_struc[i].aflag & ActionTaskMask) == USE_MAIN_LOOP) {
      task = g_new( gschem_task, 1);
      task->func.F1 = (void*)command_struc[i].func;
      task->arg1 = command_struc[i].w_current;
      task->arg2 = NULL;
      geda_main_context_invoke (NULL, (void*) i_command_dispatch, task);
    }
    else /* USE_WORKER_THREAD */ {
      gschem_threads_enter();
      command_struc[i].func(command_struc[i].w_current);
      gschem_threads_leave();
    }
  }

  accelerator = get_last_command();

  /* This should almost always happen */
  if (u_string_strequal(command_struc[accelerator].name, command)) {
    route(accelerator);
  }
  else {
    for (i = 1; i < COMMAND_COUNT; i++) {
      if (u_string_strequal(command_struc[i].name, command)) {
        route(i);
        break;
      }
    }
  }

  return;
}

/*! Enable multitasking mode */
void i_command_engage(GschemToplevel *w_current)
{
  GError *err = NULL;

  CommandPool = g_thread_pool_new ((void*)i_command_router, w_current,
                                    MAX_THREADS, FALSE, &err);

  if(err != NULL) {
    fprintf (stderr, "Error: failed to create thread pool: %s\n", err->message);
    g_error_free (err);
    is_engaged = FALSE;   /* Fallback to single thread "safe" mode if no pool */
  }
  else {
    is_engaged = TRUE;
    g_thread_pool_set_max_unused_threads (MAX_THREADS_UNUSED);
    g_thread_pool_set_max_idle_time (MAX_THREADS_IDLE_TIME);
  }
  return;
}

/*! Disable multitasking mode */
void i_command_disengage(bool immediate, bool wait_return)
{
  if(CommandPool) {
    g_thread_pool_free(CommandPool, immediate, wait_return);
  }
  is_engaged = FALSE;

  return;
}

/* Command Interface Helpers */
void i_command_get_command_list(GList** list)
{
  int i;

  for (i = 1; i < COMMAND_COUNT; i++)
    *list = g_list_prepend(*list, (char*)command_struc[i].name);
  return;
}

void i_command_get_action_list(GList** list)
{
  int i;

  for (i = 1; i < cmd_do_show_about; i++)
    *list = g_list_prepend(*list, (char*)command_struc[i].name);
  return;
}

bool i_command_is_valid(const char *command)
{
  int i;
  bool result = FALSE;
  for (i = 1; i < COMMAND_COUNT; i++) {
    if (u_string_strequal(command_struc[i].name, command)) {
      result = TRUE;
      break;
    }
  }
  return result;
}

/* Main Command Processor */
void i_command_process(GschemToplevel *w_current, const char* command,
                       int narg, char *arg, EID_ACTION_ORIGIN who)
{
  int i;

  for (i = 1; i < COMMAND_COUNT; i++) {
    if (u_string_strequal(command_struc[i].name, command)) {

      v_log_message("Processing Action Command <%s>, at index %d\n", command_struc[i].name, i);

      if (command_struc[i].repeat != NULL) {
        set_last_command(i); /* save last index for recall by repeat-last */
        x_status_bar_middle_mouse(w_current, command_struc[i].repeat);
      }

      /* Check the repeat last command action */
      if ( i == CMD(do_repeat_last)) {
        if ( !get_last_command())
          break;
        i = get_last_command(); /* Change action to the previous value */
      }

      /* Check and set pointer coordinates if task bit 3 is set */
      if ( command_struc[i].aflag & XY_ActionMask) {

        int wx, wy, check_magnet = FALSE;

        if ( who != ID_ORIGIN_MOUSE) {

          /*  If not mouse, then likely is keyboard, this is intended to
           *  handle all except the mouse */
          if (x_event_get_pointer_position (w_current, TRUE, &wx, &wy)) {
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
          }
        }
        else { /* Must have been ID_ORIGIN_MOUSE*/
          /* low levels should do this */
          SCREENtoWORLD (w_current, w_current->pointer_sx, w_current->pointer_sy, &wx, &wy);
          command_struc[i].point.x = wx;
          command_struc[i].point.y = wy;
          check_magnet = TRUE;
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
      command_struc[i].sarg      = (unsigned char *) u_string_strdup(arg);
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
      if(is_engaged && !(command_struc[i].aflag & USE_INLINE_MODE))
        g_thread_pool_push (CommandPool,command_struc[i].name, NULL);
      else
        command_struc[i].func(w_current);
      break;
    }
  }
  return;
}

static inline void msg_need_select_1st(GschemToplevel *w_current)
{
  char *message = MSG_SELECT_Object_1ST;
  i_status_set_state_msg(w_current, SELECT, message);
}

static inline void null_err(char *var)
{
  u_log_message("internal error, i_command: variable <%s> can not be NULL\n", var);
}

#define NOT_NULL(symbol) if(!symbol) return null_err(#symbol)

static inline void BlockThread (int index)
{
  int deadman = 0;
  while (command_struc[index].status == 1) {
    g_usleep (TASK_WAIT_INTERVAL); /* sleep for Some time for action to complete. */
    deadman++;
    if (deadman == MAX_WAIT_FOR_TASK) {
      fprintf (stderr, "Error: Command <%s> is not relinquishing status flag\n", command_struc[index].name);
      return;
    }
  }
}
static inline char *tokenizer( int index, int *argc, char **argv[])
{
  char *arg;
  if (command_struc[index].sarg != NULL) {
    arg  = u_string_strdup((char *)command_struc[index].sarg);
    GEDA_FREE(command_struc[index].sarg);
    arg  = u_string_strstr_rep(arg, "  ", " ");
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
                             CMD_STATUS(efunc) = 1; /* Block this thread */ \
                             int  narg NOWARN_UNUSED = CMD_INTEGER(efunc); \
                             char **argv = NULL; \
                             int    argc = 0; \
                             char  *arg  NOWARN_UNUSED = tokenizer(cmd_##efunc, &argc, &argv);

#define BEGIN_W_COMMAND(efunc) NOT_NULL(w_current); \
                               BEGIN_COMMAND(efunc);

#define EXIT_COMMAND(efunc) if(arg) { GEDA_FREE(arg); \
                                      g_strfreev ( argv);} \
                            CMD_STATUS(efunc) = 0;

#define BEGIN_NO_ARGUMENT(efunc) GEDA_FREE(CMD_OPTIONS(efunc))
#define HOT_ACTION(symbol) (((CMD_WHO(symbol)==ID_ORIGIN_KEYBOARD) || (CMD_WHO(symbol)==ID_ORIGIN_MOUSE)) && (CMD_Y(symbol) != 0))

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

  float cpu_time[10];
  float total = 0;
  float average, per_obj;
  int   cycle, count;
  int   old_page_state;
  char *results;

  const char *msg;
  const char *normal   = "exiting performance diagnostic, resumming normal mode\n";
  const char *complete = "Test complete, resumming normal mode\n";
  const char *linefeed = "\n";

  gtk_window_resize (GTK_WINDOW(w_current->main_window), 1092, 924);

/*
  o_attrib_append_attribs_changed_hook (Current_Page,
                                       (AttribsChangedFunc) o_diagnostics_notify_attribute,
                                        w_current);
*/
  int test = gschem_diagnostics_dialog(w_current);

  old_page_state = Current_Page->CHANGED;

  switch (test) {

    case CLOSE_PERFORMANCE:
      msg = normal;
      break;

    case TOGGLE_RUSAGE_DATA:
      performance_diagnostics = performance_diagnostics ? FALSE : TRUE;
      printf ("toggled performance_diagnostics: state=<%d>", performance_diagnostics);
      msg = linefeed;
      break;

    case RUN_REDRAW_TESTS:

      printf ("Running Redraw tests, 10 cycles x %d redraws per cycle\n", NUMBER_REDRAW_TEST);
      for (cycle = 0; cycle < 10; cycle++) {
        cpu_time[cycle] = test_draw_time(w_current, NUMBER_REDRAW_TEST);
        total = total + cpu_time[cycle];
      }
      average = total / 10;
      count   = g_list_length((GList*)s_page_get_objects(Current_Page));
      per_obj = ((average / count) * 1000) / NUMBER_REDRAW_TEST;
      results = u_string_sprintf("Average per 10 redraws= %.4f seconds, or %.5f ms per object", average, per_obj);
      printf ("file=%s, has %d objects: %s\n", Current_Page->filename, count, results);
      msg = complete;
      g_free(results);
      break;

    case RUN_UNDO_TESTS:
      count   = g_list_length(s_page_get_objects(Current_Page));
      if (count > NUMBER_UNDO_TEST - 1) {
        if (w_current->undo_levels < NUMBER_UNDO_TEST) {
          printf ("Warning undo levels setting=%d, number of tests=%d\n", NUMBER_UNDO_TEST, w_current->undo_levels);
        }
        printf ("file=%s, has %d objects before testing\n", Current_Page->filename, count);
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
        count   = g_list_length(s_page_get_objects(Current_Page));
        per_obj = ((average / count) * 1000) / NUMBER_UNDO_TEST;
        printf ("file=%s, has %d objects after testing\n", Current_Page->filename, count);
        results = u_string_sprintf("Average per 10 undo's= %.4f seconds, or %.5f ms per Object", average, per_obj);
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

  Current_Page->CHANGED = old_page_state;
#else
  printf("Performance_diagnostic is not enable, must recompile\n");
#endif

  EXIT_COMMAND(do_debug);
}

/*!\warning: { Do not point to another CMD_OPTIONS unless you know what your doing }*/

/** \defgroup i_command_Command_Handlers Action Handler Functions
 *  @{ \par
 *      Functions in the group process all of the actions generated from
 *      various sources, such as menus, toolbars, and the keyboard. The
 *      action handlers also process command input from the console. This
 *      module does <b>NOT</b> process actions orginating from Drag-N-Drop.
 *      Each of the function below "block" themself on entry and "unblock"
 *      on exit. Pending threads will wait up to TASK_WAIT_INTERVAL before
 *      reporting the dead task and returning without executing the action.
 *      This is really for error trapping and debugging, under normal
 *      circumstances this does not happen.
 */

/** \defgroup i_command_Action_Functions Action Handler Function
 *  @{ \remark Yes, repeat is a command, not just an action
 */

/** @brief i_cmd_do_repeat_last in i_command_Command_Handlers */
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
  q_log_message (_("New page created [%s]\n"), page->filename);

  EXIT_COMMAND(do_file_new);

}

/*! \brief File New Window Action
 *
 *  @brief i_cmd_do_file_new_window in i_command_Option_Actions
 *
 *  \par Function Description
 *  This is an Action handler function for the File New Window.
 *  action. This function creates a new toplevel window.
 *
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

  f_set_backup_loader_query_func  (toplevel,
                                   x_fileselect_load_backup,
                                   new_window);

  s_toplevel_set_rendered_bounds_func (toplevel,
                                       o_text_get_rendered_bounds,
                                       new_window);

  page = x_window_open_page (new_window, NULL);
  x_window_set_current_page (new_window, page);

  q_log_message (_("New Window created [%s]\n"), page->filename);

}

/**   \defgroup open-files-command Open File Action
 *  @{\par This group contains functions to open documents
 *    \ingroup (i_command_File_Actions)
 */
/* This does not do anything productive, is a delay, the destroy
 * notifier; open_command_idle_notify, does all the work.
 * This is a low priority main-loop task, instigated after higher
 * priority main-loop task were delegated to opening files */
static bool
open_command_idle_callback (void *data)
{
  IdleTaskData *packet    = data;
  bool          status    = SOURCE_CONTINUE;
  char         *last_file = g_slist_last(packet->data)->data;

  if (s_page_search(packet->w_current->toplevel, last_file)) {
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
 * These idle threads were to release the memory associated
 * with x_fileselect_list */
static void
open_command_idle_notify (void *data)
{
  IdleTaskData *packet = data;
  GSList       *files  = packet->data;
  char    *last_file   = g_slist_last(files)->data;
  Page    *page;

  page = s_page_search(packet->w_current->toplevel, last_file);
  if (GEDA_IS_PAGE(page)) {
    x_window_set_current_page (packet->w_current, page);
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

/** @brief i_cmd_do_open in i_command_File_Actions
 *  \par Function Description
 *  Calls x_fileselect_list to display a standard system file
 *  open dialog, if not canceled, the dialog returns a single
 *  linked list of file names. A task to be ran in the main
 *  context is created for each file. A low priority idle task
 *  is assigned to clean up details, mainly to release memory
 *  allocated by x_fileselect_list.
 *
 *  This is worker thread that spawns main-loop threads, the
 *  main reason for this is that, eventually libgeda will be
 *  called and guile will used to process gafrc files and these
 *  guile routines need to be ran in the main context.
 *  As a bonus, our multi-document load performance increased
 *  dramatically, compared to the old sequential loading.
 */
COMMAND (do_open) {
  BEGIN_W_COMMAND(do_open);

  GSList       *files;
  gschem_task  *task;
  IdleTaskData *packet;
  int           count = 0;

  if ( NULL != (files = x_fileselect_list (w_current))) {

    w_current->toplevel->open_flags = F_OPEN_RC | F_OPEN_CHECK_BACKUP;

    lambda (void *filename) {
      task          = g_new(gschem_task, 1);
      task->func.F2 = (void*)x_window_open_page;
      task->arg1    = command_struc[cmd_do_open].w_current;
      task->arg2    = filename;
      geda_main_context_invoke (NULL, (void*) i_command_dispatch, task);
      count++;
      return FALSE;
    }
    mapcar(files);

    packet            = g_new(IdleTaskData, 1);
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
/*! \brief Save File As command action handler function
 *  \par Function Description
 *  Save the current file to disk.
 *  \note should be a flag that says whether filename is
 *        derived from untitled_name or specified by a user.
 *        Some twisted people might name their files like
 *        untitled_name. :-)
 */
COMMAND (do_save) {
  BEGIN_NO_ARGUMENT(do_save);
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  if(Current_Page->filename == NULL)
    w_current->force_save_as = TRUE;

  if (strstr(Current_Page->filename,
      w_current->toplevel->untitled_name))
        w_current->force_save_as = TRUE;

  if (w_current->force_save_as) {
      x_fileselect_save (w_current);
  }
  else {
      x_window_save_page (w_current,
                          Current_Page,
                          Current_Page->filename);
  }
}

/*! \brief Save File As action
 *  \par Function Description
 *  This is a callback function for the File Save As API
 *  The function calls i_command to process the action.
 */
/** @brief i_cmd_do_save_as in i_command_File_Actions */
COMMAND (do_save_as) {
  BEGIN_W_COMMAND(do_save_as);
  x_fileselect_save (w_current);
  i_status_update_title (w_current);
  EXIT_COMMAND(do_save_as);
}

/** @brief i_cmd_do_save_all in i_command_File_Actions */
COMMAND (do_save_all) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_NO_ARGUMENT(do_save_all);

  if (s_page_save_all(w_current->toplevel)) {
     i_status_set_state_msg(w_current, SELECT, _("Failed to Save All"));
  } else {
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
  char *base=NULL, *filename;
  char *ps_filename=NULL;

  /* shortcut */
  filename = Current_Page->filename;

  /* get the base file name */
  if (g_str_has_suffix(filename, ".sch")) {
    /* the filename ends with ".sch", remove it */
    base = g_strndup(filename, strlen(filename) - strlen(".sch"));
  } else {
    /* the filename does not end with .sch */
    base = u_string_strdup (filename);
  }

  /* add ".ps" tp the base filename */
  ps_filename = u_string_concat (base, ".ps", NULL);
  GEDA_FREE(base);

  if (output_filename) {
    x_print_setup(w_current, output_filename);
  } else {
    x_print_setup(w_current, ps_filename);
  }

  GEDA_FREE(ps_filename);
  EXIT_COMMAND(do_print);
}

/** @brief i_cmd_do_write_image in i_command_File_Actions */
COMMAND (do_write_image) {
  BEGIN_W_COMMAND(do_write_image);
    w_current->inside_action = 0;
    o_redraw_cleanstates (w_current);
    i_status_set_state_msg(w_current, SELECT, _("Write Image"));
    i_status_set_state(w_current, SELECT);
    i_status_update_sensitivities(w_current);
    x_image_setup(w_current, last_image);
  EXIT_COMMAND(do_write_image);
}

/** @brief i_cmd_do_write_pdf in i_command_File_Actions */
/*! \brief Write PDF command
 *  \par Function Description
 *  This is handles the write-pdf action
 */
COMMAND (do_write_pdf) {
  BEGIN_W_COMMAND(do_write_pdf);
    w_current->inside_action = 0;
    o_redraw_cleanstates (w_current);
    i_status_set_state_msg(w_current, SELECT, _("Write PDF"));
    i_status_set_state(w_current, SELECT);
    i_status_update_sensitivities(w_current);
    x_image_setup(w_current, pdf_image);
  EXIT_COMMAND(do_write_pdf);
}

/** @brief i_cmd_do_run_script in i_command_File_Actions */
COMMAND (do_run_script) {
  BEGIN_W_COMMAND(do_run_script);
  char* filename = NULL;
  gschem_threads_enter();
  filename = x_dialog_select_file("Execute Script...", filename, FSB_LOAD);
  if(filename != NULL) { /* if user did not cancel */
    g_read_scheme_file(filename, NULL);
    GEDA_FREE(filename);
  }
  gschem_threads_leave();
  EXIT_COMMAND(do_run_script);
}


/** @brief i_cmd_do_close in i_command_File_Actions */
COMMAND (do_close) {
  BEGIN_W_COMMAND(do_close);
  bool can_close;
  can_close = TRUE;

  if (Current_Page->CHANGED) {
    can_close = x_dialog_close_changed_page (w_current, Current_Page);
  }

  if (can_close) {
    q_log_message(_("Closing Window\n"));
    x_window_close_page (w_current, Current_Page);
  }
  i_status_set_state(w_current, SELECT);
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
    (w_current->event_state == MOVE || w_current->event_state == ENDMOVE))
  {
    o_move_cancel (w_current);
  }

  x_window_close_edit_dialogs(w_current);

  pages = g_list_copy(geda_list_get_glist(w_current->toplevel->pages));

  /* Loop through all the pages looking for unsaved pages */
  for ( iter = pages; iter != NULL; NEXT(iter))
  {
    /* get ptr to a page */
    p_current = (Page*)iter->data;

    /* if flag set */
    if (p_current->CHANGED) {
      can_close = FALSE;
      break;                 /* if at least one page */
    }
  }

  if (!can_close) {         /* Ask to save unsaved pages */

    close_all = x_dialog_close_window (w_current);
    if (!close_all) {       /* user cancelled the close */
      v_log_message("Close all canceled");
    }
  }
  else {
    close_all = TRUE;       /* There were no unsaved pages */
  }

  if (close_all) {          /* Still want to close all? */

    q_log_message(_("Closing all documents\n"));

    /* Loop through all the pages */
    for ( iter = pages; iter != NULL; NEXT(iter))
    {
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
  BEGIN_NO_ARGUMENT(do_file_new_window);
  v_log_message(_("gschem: starting shut-down\n"));
  x_window_close_all(w_current);
}

/** @brief i_cmd_do_export_symbol in i_command_File_Actions */
COMMAND (do_export_symbol) {
  BEGIN_W_COMMAND(do_export_symbol);

  Object *o_current;

  o_current = o_select_return_first_object(w_current);

  if (o_current && (o_current->type == OBJ_COMPLEX)) {
    if (!o_get_is_embedded(o_current)) {
      const char *question = _("Symbol is not embeded, export anyway?");
      int response = x_dialog_confirmation(question, GTK_MESSAGE_INFO, FALSE);
      if (response == GTK_RESPONSE_YES) {
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

 Object *o_current;

  o_current = o_select_return_first_object(w_current);

  if (o_current && (o_current->type == OBJ_PICTURE)) {
    if (!o_get_is_embedded(o_current)) {
      const char *question = _("Picture is not embeded, export anyway?");
      int response = x_dialog_confirmation(question, GTK_MESSAGE_INFO, FALSE);
      if (response == GTK_RESPONSE_YES) {
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
  //u_log_message("do edit command handler");
  char *msg = "Not an object";

  o_set_visibility ((Object *)msg, 1);
  EXIT_COMMAND(do_edit);
}

/** \defgroup i_command_Edit_Actions Actions under the Edit Menu
 * @{*/

COMMAND (do_undo)
{
  BEGIN_W_COMMAND(do_undo);
  /* If we're cancelling from a move action, re-wind the
   * page contents back to the state before we started.
   *
   * It "might" be nice to sub-undo rotates / zoom changes
   * made whilst moving components, but when the undo code
   * hits s_page_delete(), the place list objects are free'd.
   * Since they are also contained in the schematic page, a
   * crash occurs when the page objects are free'd.
   * */
  if (w_current->inside_action &&
      (w_current->event_state == MOVE ||
       w_current->event_state == ENDMOVE)) {
    i_callback_cancel (w_current, 0, NULL);
  }
  else {
    /* can loop here with arg */
    o_undo_callback(w_current, UNDO_ACTION);
  }
  EXIT_COMMAND(do_undo);
}
COMMAND (do_redo)
{
  NOT_NULL(w_current);
  BEGIN_W_COMMAND(do_redo);
  o_undo_callback(w_current, REDO_ACTION);
  EXIT_COMMAND(do_redo);
}

COMMAND (do_cut_clip)
{
  BEGIN_W_COMMAND(do_cut_clip);

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
    /* if buffer number =0, the copy to system buffer */
    if ( narg == 0) {
      x_clipboard_set (w_current, object_buffer[0]);
    }
    i_status_update_sensitivities(w_current);
  }
  EXIT_COMMAND(do_copy_clip);
}

/*! \brief Action Paste Clipboard Contents
 *  \par Function Description
 *  This function initiates the pasting of the contents of a clip-
 *  board into the drawing.
 */
COMMAND (do_paste_clip)
{
  BEGIN_W_COMMAND(do_paste_clip);

  GList *object_list = NULL;
  int state;

  if ((narg < 0) || (arg == NULL)) {
    /* if no arguments then use buffer 0 */
    narg = 0;
    object_list = x_clipboard_get (w_current);
    s_object_release_objects (object_buffer[narg]);
    object_buffer[0] = object_list;
  }

  if ( object_buffer[narg] != NULL) {
    if HOT_ACTION (do_paste_clip) {

      o_buffer_paste_start (w_current, CMD_X(do_paste_clip),
                                       CMD_Y(do_paste_clip), narg);
      w_current->inside_action = 1;
      state = ENDPASTE;
    }
    else {
      o_redraw_cleanstates (w_current);
      w_current->buffer_number = narg;
      w_current->inside_action = 0;
      state = STARTPASTE;
    }
    i_status_set_state (w_current, state);
  }
  else {
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
    w_current->inside_action = 0;
    i_status_set_state(w_current, SELECT);
    i_status_update_sensitivities(w_current);

  }
  EXIT_COMMAND(do_delete);
}

/** @brief i_cmd_do_copy in i_command_Edit_Actions */
/*! \brief Copy selected objects on page.
 *  \par Function Description
 *   Initiate Copy mode for selected objects
 */
COMMAND (do_copy)
{
  BEGIN_W_COMMAND(do_copy);
  int state;

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_copy) {
      i_status_set_state (w_current, COPY);;
      o_copy_start (w_current,  CMD_X(do_copy),  CMD_Y(do_copy));
      state = ENDCOPY;
      w_current->inside_action = 1;
    }
    else {
      state = STARTCOPY;
      w_current->inside_action = 0;
    }
    i_status_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }

  EXIT_COMMAND(do_copy);
}

/** @brief i_cmd_do_mcopy in i_command_Edit_Actions */
/*! \brief Make multi copies of selected objects on page.
 *  \par Function Description
 *   Initiates Multi-Copy mode for selected objects
 */
COMMAND (do_mcopy)
{
  BEGIN_W_COMMAND(do_mcopy);
  int state;

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_mcopy) {
      i_status_set_state(w_current, MCOPY);
      o_copy_start (w_current, CMD_X(do_mcopy), CMD_Y(do_mcopy));
      state = ENDMCOPY;
      w_current->inside_action = 1;
    }
    else {
      state = STARTMCOPY;
      w_current->inside_action = 0;
    }
    i_status_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_mcopy);
}

/** @brief i_cmd_do_move in i_command_Edit_Actions */
/*! \brief Move selected objects on page.
 *  \par Function Description
 *   Initiate Move mode for selected objects
 */
COMMAND (do_move)
{
  BEGIN_W_COMMAND(do_move);
  int state;

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_move) {
      i_status_set_state(w_current, MCOPY);
      o_move_start (w_current, CMD_X(do_move), CMD_Y(do_move));
      state = ENDMOVE;
      w_current->inside_action = 1;
    }
    else {
      state = STARTMOVE;
      w_current->inside_action = 0;
    }
    i_status_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }

  EXIT_COMMAND(do_move);
}

/*! \brief Action Rotate  in i_command_Edit_Actions
 *
 *  @brief i_cmd_do_rotate
 *
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 *
 */
COMMAND (do_rotate)
{
  BEGIN_W_COMMAND(do_rotate);

  /* If inside an appropriate action, send a button 2 released,
   * so rotating will be handled by x_event.c */
  if (w_current->inside_action &&
     (w_current->event_state == ENDCOMP  ||
      w_current->event_state == ENDTEXT  ||
      w_current->event_state == ENDMOVE  ||
      w_current->event_state == ENDCOPY  ||
      w_current->event_state == ENDMCOPY ||
      w_current->event_state == ENDPASTE ))
  {

    GdkEvent* event;

    event = gdk_event_new(GDK_BUTTON_RELEASE);
    ((GdkEventButton*) event)->button = 2;
    x_event_button_released (NULL, (GdkEventButton *) event, w_current);
    gdk_event_free(event);

  }
  else {

    int state;

    o_redraw_cleanstates(w_current);

    if HOT_ACTION (do_rotate) {

      GList *object_list;

      object_list = geda_list_get_glist (Current_Selection);

      if (object_list) {
        /* Allow o_edit_rotate_world to redraw the objects */
        o_edit_rotate_world(w_current, CMD_X(do_rotate),
                                       CMD_Y(do_rotate), 90, object_list);
      }

      state = SELECT;
    }
    else {
      state = ENDROTATEP;

    }
    w_current->inside_action = 0;
    i_status_set_state(w_current, state);
  }

  EXIT_COMMAND(do_rotate);
}

/*! \brief Mirror selected objects on page
 *
 *  @brief i_cmd_do_mirror in i_command_Edit_Actions
 *
 *  \par Function Description
 *  Initiate Mirror mode for selected object.
 *
 */
COMMAND (do_mirror)
{
  BEGIN_W_COMMAND(do_mirror);
  int state;

  o_redraw_cleanstates(w_current);
  if HOT_ACTION (do_mirror) {

    GList *object_list;

    object_list = geda_list_get_glist (w_current->toplevel->
                                       page_current->selection_list);

    if (object_list) {
      o_edit_mirror_world(w_current, CMD_X(do_mirror), CMD_Y(do_mirror), object_list);
    }

    state = SELECT;
  }
  else {
    state = ENDMIRROR;
  }

  w_current->inside_action = 0;
  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_mirror);
}

/*! \brief Edit Attributes for selected object
 *
 *  @brief i_cmd_edit_butes in i_command_Edit_Actions
 *
 *  \par Function Description
 *   Calls o_edit_objects to initiate the Edit Attributes dialog for
 *   selected object
 *
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
  Object *o_current;

  o_current = o_select_return_first_object(w_current);

  //if (o_current && (o_current->type == OBJ_COMPLEX)) {
    x_dialog_edit_properties(w_current, o_current);
  //}

  EXIT_COMMAND(do_edit_butes);
}

/*! \brief Edit Text in i_command_Edit_Actions */
COMMAND (do_edit_text)
{
  BEGIN_W_COMMAND(do_edit_text);
  Object *o_current;

  o_current = o_select_return_first_object(w_current);
  if (o_current && (o_current->type == OBJ_TEXT)) {
      o_text_edit(w_current, o_current);
  }
  EXIT_COMMAND(do_edit_text);
}

/*! \brief Edit Slot in i_command_Edit_Actions */
COMMAND (do_edit_slot)
{
  BEGIN_W_COMMAND(do_edit_slot);
  Object *o_current;

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
  Object *o_current;
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
 *  This function calls o_edit_lock to locks all objects in selection list.
 *
 */
COMMAND (do_lock)
{
  BEGIN_W_COMMAND(do_lock);

  if (o_select_return_first_object(w_current)) {
    o_edit_lock(w_current);
  }
  EXIT_COMMAND(do_lock);
}

/*! \brief Unlock objects in selection list
 *
 *  @brief i_cmd_do_unlock in i_command_Edit_Actions
 *
 *  \par Function Description
 *  This function calls o_edit_unlock to unlocks all objects in selection.
 *
 */
COMMAND (do_unlock)
{
  BEGIN_W_COMMAND(do_unlock);
  if (o_select_return_first_object(w_current)) {
    o_edit_unlock(w_current);
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
  if (!o_invalidate_rubber (w_current)) {
    i_callback_cancel(w_current, 0, NULL);
  }

  o_redraw_cleanstates(w_current);
  i_status_set_state(w_current, SELECT);
  w_current->inside_action = 0;

  EXIT_COMMAND(do_select);
}

COMMAND (do_select_all)
{
  BEGIN_W_COMMAND(do_select_all);
  o_redraw_cleanstates (w_current);
  o_select_visible_unlocked (w_current);

  i_status_set_state (w_current, SELECT);
  w_current->inside_action = 0;
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
  GedaToplevel *toplevel = w_current->toplevel;
  SELECTION *selection = toplevel->page_current->selection_list;

  GList *list = g_list_copy (geda_list_get_glist(selection));
  o_select_visible_unlocked (w_current);
  while(list != NULL) {
    o_selection_remove (selection, (Object*) list->data);
    NEXT(list);
  }
  g_list_free (list);
  o_redraw_cleanstates(w_current);
  i_status_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  EXIT_COMMAND(do_select_invert);
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

  if (o_select_is_selection (w_current))
    i_status_set_state (w_current, DESELECT);
  else /* Automaticaly switch to SELECT mode cause nothing to deselect */
    msg_need_select_1st(w_current);
    //i_status_set_state (w_current, SELECT);

  w_current->inside_action = 0;
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

  i_status_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  i_status_update_sensitivities (w_current);
  EXIT_COMMAND(do_deselect_all);
}

/** @} endgroup i_command_Select_Actions */

/* ------------------ View ---------------- */

COMMAND (do_view)
{
  BEGIN_COMMAND(do_view);
  u_log_message("do_view command handler");
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
  w_current->inside_action = 0;
  i_status_set_state(w_current, STARTPAN);
}

/*! \brief Zoom Box Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the Zoom Box action.
 */
COMMAND (do_zoom_box)
{
  BEGIN_W_COMMAND(do_zoom_box);
  int state;

  o_select_unselect_all (w_current);
  o_redraw_cleanstates(w_current);

  if HOT_ACTION (do_zoom_box) {
    i_zoom_world_box_start (w_current, CMD_X(do_zoom_box), CMD_Y(do_zoom_box));
    w_current->inside_action = 1;
    state = ZOOMBOXEND;
  }
  else {
    w_current->inside_action = 0;
    state = ZOOMBOXSTART;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_zoom_box);
}
/*! \brief Zoom Extents Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the Zoom Extents action.
 */
COMMAND (do_zoom_selected)
{
  BEGIN_W_COMMAND(do_zoom_selected);
  /* scroll bar stuff */
  const GList *selection;
  //    SELECTION *
  selection = Current_Selection->glist;
  i_zoom_world_extents (w_current, selection, 0);
  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_selected);
}

/*! \brief Zoom Extents Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the view-zoom-extents action.
 */
COMMAND (do_zoom_extents)
{
  BEGIN_W_COMMAND(do_zoom_extents);
  /* scroll bar stuff */
  i_zoom_world_extents (w_current, s_page_get_objects (Current_Page), 0);
  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_extents);
}

/*! \brief Zoom In Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the view-zoom-in action.
 */
COMMAND (do_zoom_in)
{
  BEGIN_W_COMMAND(do_zoom_in);

  i_zoom_world(w_current, ZOOM_IN_DIRECTIVE, CMD_WHO(do_zoom_out), 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);

  EXIT_COMMAND(do_zoom_in);
}

/*! \brief Zoom Out Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the view-zoom-out action.
 */
COMMAND (do_zoom_out)
{
  BEGIN_W_COMMAND(do_zoom_out);

  i_zoom_world(w_current, ZOOM_OUT_DIRECTIVE, CMD_WHO(do_zoom_out), 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);

  EXIT_COMMAND(do_zoom_out);
}

/*! \brief Zoom All Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the view-zoom-all action.
 */
COMMAND (do_zoom_all)
{
  BEGIN_W_COMMAND(do_zoom_all);

  i_zoom_world(w_current, ZOOM_FULL_DIRECTIVE, DONTCARE, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_all);
}

/*! \brief Zoom to Magnification Function in i_command_View_Actions
 *  \par Function Description
 *  This is the callback handler for the view-zoom-to-mag action.
 *
 *  \note Magnification in this context is the reciprocal of the scale
 *        factor, i.e. to_screen_y_constant and to_screen_x_constant.
 */
COMMAND (do_zoom_to_mag)
{
  BEGIN_W_COMMAND(do_zoom_to_mag);

  int x, y;
  double mag;

  x = CMD_X(do_zoom_to_mag);
  y = CMD_Y(do_zoom_to_mag);

  mag = geda_dialog_get_real(_("Zoom Mag"),
                             _("Specify new zoom:"));

  i_zoom_world_specify(w_current, mag, x, y, CMD_WHO(do_zoom_to_mag));

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_to_mag);
}

/*! \brief View Documentation Action Function in i_command_View_Actions
 *  \par Function Description
 *  This is a callback function for the view-documentation action.
 */
COMMAND (do_documentation)
{
  BEGIN_W_COMMAND(do_documentation);

  char *attrib_doc = NULL;
  Object *object = NULL;
  GError *error = NULL;
  bool result;

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {

      /* look for "documentation" */
      attrib_doc = o_attrib_search_object_attribs_by_name (object, "documentation", 0);
      if (attrib_doc) {
        //g_type_init();
        //result = x_show_uri (w_current, attrib_doc, &error);
        /* Use this instead until debian-gnome work out thier iceweasel issue */
        result = g_app_info_launch_default_for_uri(attrib_doc, NULL, &error);
        if (!result) {
          u_log_message("error: %s", error->message);
          g_error_free (error);
        }
        GEDA_FREE(attrib_doc);
      }
    }
  }
  else {
    q_log_message(_("No component selected"));
  }

  EXIT_COMMAND(do_documentation);
}
COMMAND (do_show_hidden)
{
  BEGIN_W_COMMAND(do_show_hidden);
  GList *object_list = NULL;

  /* Don't execute this inside an action - retest this */
  if (!w_current->inside_action) {
    if (o_select_is_selection (w_current)) {
      SELECTION *selection = Current_Selection;
      object_list =  geda_list_get_glist (selection);
    }
    else {
      object_list =  s_page_get_objects (Current_Page);
    }
    o_undo_savestate (w_current, UNDO_ALL);
    o_edit_show_hidden (w_current, object_list, FALSE);
  }
  EXIT_COMMAND(do_show_hidden);
}

COMMAND (do_show_inherited)
{
  BEGIN_W_COMMAND(do_show_inherited);
  GList *object_list = NULL;

  /* Don't execute this inside an action - retest this */
  if (!w_current->inside_action) {
    if (o_select_is_selection (w_current)) {
      SELECTION *selection = Current_Selection;
      object_list =  geda_list_get_glist (selection);
    }
    else {
      object_list =  s_page_get_objects (Current_Page);
    }

    /* Should we really do this? */
    o_undo_savestate (w_current, UNDO_ALL);
    o_edit_show_hidden (w_current, object_list, TRUE);
  }
  EXIT_COMMAND(do_show_inherited);
}

COMMAND (do_show_nets)
{
  BEGIN_COMMAND(do_show_nets);
  GList *object_list = NULL;

  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    if (o_select_is_selection (w_current)) {
      SELECTION *selection = Current_Selection;
      object_list =  geda_list_get_glist (selection);
    }
    else {
      object_list =  s_page_get_objects (Current_Page);
    }
    o_edit_show_netnames (w_current, object_list);
  }
  EXIT_COMMAND(do_show_nets);
}

/*! \brief Load the Dark color map scheme in i_command_View_Actions
 *  \par Function Description
 *       This function loads the Dark color map scheme
 *       based on user input from the keyboard or menu.
 */
COMMAND (do_dark_colors)
{
  BEGIN_W_COMMAND(do_dark_colors);
  /* Change the scheme here */
  x_color_load_scheme(DARK_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_dark_colors);
}
/*! \brief Load the Light color map scheme in i_command_View_Actions
 *  \par Function Description
 *       This function loads the Light color map scheme
 *       based on user input from the keyboard or menu.
 */
COMMAND (do_light_colors)
{
  BEGIN_W_COMMAND(do_light_colors);
  /* Change the scheme here */
  x_color_load_scheme(LIGHT_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_light_colors);
}
/*! \brief Load the BW color map scheme in i_command_View_Actions
 *  \par Function Description
 *       This function loads the BW color map scheme
 *       based on user input from the keyboard or menu.
 */
COMMAND (do_bw_colors)
{
  BEGIN_W_COMMAND(do_bw_colors);
  /* Change the scheme here */
  x_color_load_scheme(BW_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_bw_colors);
}

/** @} endgroup i_command_View_Actions */

/* ------------------ Page ---------------- */

COMMAND (do_page)
{
  BEGIN_COMMAND(do_page);
  u_log_message("do_page command handler");
  EXIT_COMMAND(do_page);
}

/** \defgroup i_command_Page_Actions Actions under the Page Menu
 * @{*/

/** @brief i_cmd_do_page_manager in i_command_Command_Functions */
COMMAND (do_page_manager)
{
  BEGIN_W_COMMAND(do_page_manager);
  x_pagesel_open (w_current);
  EXIT_COMMAND(do_page_manager);
}

/** @brief i_cmd_do_page_prev in i_command_Command_Functions */
COMMAND (do_page_prev)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_prev);

  GedaToplevel *toplevel = w_current->toplevel;

  Page *p_new;
  GList *iter;

  iter = g_list_find( geda_list_get_glist(toplevel->pages), Current_Page);
  iter = g_list_previous( iter);

  if ( iter != NULL) {

    p_new = (Page *)iter->data;

    if (w_current->enforce_hierarchy) {
      p_new = s_hierarchy_find_prev_page(toplevel->pages, Current_Page);
    }
    else {
      p_new = (Page *)iter->data;
    }

    if (p_new != NULL || p_new != Current_Page) {
      x_window_set_current_page (w_current, p_new);
    }
  }
}

/** @brief i_cmd_do_page_next in i_command_Command_Functions */
COMMAND (do_page_next)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_next);

  GedaToplevel *toplevel = w_current->toplevel;

  Page *p_new;
  GList *iter;

  iter = g_list_find( geda_list_get_glist(toplevel->pages), Current_Page);
  NEXT(iter);

  if (iter != NULL) {

    if (w_current->enforce_hierarchy) {
      p_new = s_hierarchy_find_next_page(toplevel->pages, Current_Page);
    }
    else {
      p_new = (Page *)iter->data;
    }

    if (p_new != NULL || p_new != Current_Page) {
      x_window_set_current_page (w_current, p_new);
    }
  }
}

/** @brief i_cmd_do_page_new in i_command_Command_Functions */
/* This is simular to file new accept we add new page hook*/
COMMAND (do_page_new)
{
  BEGIN_W_COMMAND(do_page_new);

  EdaConfig *cfg   = eda_config_get_user_context();
  char      *group = IVAR_CONFIG_GROUP;
  char      *tblock;
  char      *sym_file;
  char      *ext;

  Object    *object;
  Page      *page;

  const CLibSymbol *clib;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);

  x_window_set_current_page (w_current, page);

  g_run_hook_page (w_current, "%new-page-hook", page);

 /* would be far easier, faster, and safer to set page->CHANGED=FALSE
  * here then for scheme to have done this in the hook, could just add
  * the titleblock here too, like so */
  ext = tblock = eda_config_get_string (cfg, group, "default-titleblock", NULL);

  while (*ext) ext++;
  ext = ext - 4;

  if (strcmp(ext, SYMBOL_FILE_DOT_SUFFIX)) {
    sym_file = u_string_concat(tblock, SYMBOL_FILE_DOT_SUFFIX, NULL);
  }
  else {
    sym_file = g_strdup(tblock);
  }

  clib = s_clib_get_symbol_by_name (sym_file);

  if (clib != NULL) {
    object = o_complex_new (w_current->toplevel, 0, 0, 0, FALSE, clib, sym_file, FALSE);
    s_page_append_object (page, object);
  }

  GEDA_FREE(sym_file);
  GEDA_FREE(tblock);

  page->CHANGED = 0;
  i_zoom_world_extents (w_current, s_page_get_objects (page), I_PAN_DONT_REDRAW);

  q_log_message (_("New page created [%s]\n"), page->filename);

  EXIT_COMMAND(do_page_new);
}

/** @brief i_cmd_do_page_print in i_command_Command_Functions */
COMMAND (do_page_print) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_COMMAND(do_page_print);
  s_page_print_all(w_current->toplevel);
  EXIT_COMMAND(do_page_print);
}

/** @brief i_cmd_do_revert in i_command_Command_Functions */
COMMAND (do_page_revert) {

  BEGIN_W_COMMAND(do_page_revert);

  Page *page;
  char *filename;
  int page_control;
  int up;
  int answer;

  answer = x_dialog_confirmation (_("Really revert page?"), GTK_MESSAGE_QUESTION, TRUE);

  if (answer == GTK_RESPONSE_YES) {

    /* save this for later */
    filename = u_string_strdup (Current_Page->filename);
    page_control = Current_Page->page_control;
    up = Current_Page->up;

    /* delete the page, then re-open the file as a new page */
    s_page_delete (w_current->toplevel, Current_Page);

    page = x_window_open_page (w_current, filename);

    /* make sure we maintain the hierarchy info */
    page->page_control = page_control;
    page->up = up;

    x_window_set_current_page (w_current, page);
    GEDA_FREE (filename);
  }

  EXIT_COMMAND(do_page_revert);
}

/** @brief i_cmd_do_page_close in i_command_Command_Functions */
COMMAND (do_page_close)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_COMMAND(do_page_close);

  bool  can_close;

  can_close = TRUE;

  if (Current_Page->CHANGED) {
    can_close = x_dialog_close_changed_page (w_current, Current_Page);
  }

  if (can_close) {
    q_log_message(_("Closing Page\n"));
    x_window_close_page (w_current, Current_Page);
  }
  EXIT_COMMAND(do_page_close);
}

/** @brief i_cmd_do_page_discard in i_command_Command_Functions */
COMMAND (do_page_discard)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_discard);

  x_window_close_page (w_current, Current_Page);

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

  Object *object           = NULL;
  Page   *child            = NULL;
  Page   *parent           = NULL;
  Page   *save_first_page  = NULL;

  bool    loaded_flag      = FALSE;
  bool    looking_inside   = FALSE;

  char   *attrib           = NULL;
  char   *current_filename = NULL;

  int     count            = 0;
  int     page_control     = 0;
  int     pcount           = 0;

  object = o_select_return_first_object(w_current);

  /* only allow going into symbols */
  if (object == NULL || object->type != OBJ_COMPLEX)
    return;

  parent = Current_Page;
  attrib = o_attrib_search_attached_attribs_by_name (object, "source", count);

  /* if above is null, then look inside symbol */
  if (attrib == NULL) {
    attrib =
      o_attrib_search_inherited_attribs_by_name (object, "source", count);
    looking_inside = TRUE;
#if DEBUG
    printf("going to look inside now\n");
#endif
  }

  while (attrib) {

    /* look for source=filename,filename, ... */
    pcount = 0;
    current_filename = u_string_split(attrib, ',', pcount);

    /* loop over all filenames */
    while(current_filename != NULL) {

      GError *err = NULL;

      u_log_message(_("Searching for source [%s]\n"), current_filename);

      child = s_hierarchy_down_schematic_single(w_current->toplevel,
                                                current_filename,
                                                parent,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD,
                                                &err);

      /* s_hierarchy_down_schematic_single() will not zoom the loaded page */
      if (child != NULL) {
        x_window_setup_page(w_current, child, w_current->world_left,
                                              w_current->world_right,
                                              w_current->world_top,
                                              w_current->world_bottom);
        s_page_goto (w_current->toplevel, child);
        i_zoom_world_extents(w_current,
                             s_page_get_objects (child),
                             I_PAN_DONT_REDRAW);
        o_undo_savestate(w_current, UNDO_ALL);
        s_page_goto (w_current->toplevel, parent);
        o_notify_change_add (child,
                            (ChangeNotifyFunc) o_invalidate_object,
                            (ChangeNotifyFunc) o_invalidate_object, w_current);
      }

      /* save the first page */
      if ( !loaded_flag && (child != NULL)) {
        save_first_page = child;
      }

      /* now do some error reporting */
      if (child == NULL) {

        const char *msg = (err != NULL) ? err->message : "Unknown error.";

        u_log_message(_("Failed to descend into '%s': %s\n"),
                         current_filename, msg);

        char *secondary = u_string_sprintf (
                         _("Failed to descend hierarchy into '%s': %s"),
                            current_filename, msg);

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
      current_filename = u_string_split(attrib, ',', pcount);
    }

    GEDA_FREE(attrib);
    GEDA_FREE(current_filename);

    count++;

    /* continue looking outside first */
    if (!looking_inside) {
      attrib =
        o_attrib_search_attached_attribs_by_name (object, "source", count);
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
      attrib = o_attrib_search_inherited_attribs_by_name(object, "source", count);
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

  Object *object;

  object = o_select_return_first_object(w_current);

  if (object != NULL) {

    if (object->type == OBJ_COMPLEX) { /* only allow going into symbols */

      const char *filename = object->complex->filename;
      const CLibSymbol *sym;
      Page *child;

      u_log_message(_("Searching for symbol [%s]\n"), filename);

      sym = s_clib_get_symbol_by_name (filename);

      if (sym == NULL)
        return;
      if (s_clib_symbol_get_filename(sym) == NULL) {
        u_log_message(_("Symbol is not a real file."
                        " Symbol cannot be loaded.\n"));
        return;
      }

      child = s_hierarchy_down_symbol(w_current->toplevel, sym, Current_Page);

      x_window_setup_page(w_current, child, w_current->world_left,
                                            w_current->world_right,
                                            w_current->world_top,
                                            w_current->world_bottom);

      /* s_hierarchy_down_symbol() will not zoom the loaded page */
      i_zoom_world_extents(w_current,
                           s_page_get_objects (child),
                           I_PAN_DONT_REDRAW);

      o_notify_change_add (child,
                          (ChangeNotifyFunc) o_invalidate_object,
                          (ChangeNotifyFunc) o_invalidate_object, w_current);

      //o_undo_savestate(w_current, UNDO_ALL);
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

  Page *up_page;

  up_page = s_hierarchy_find_up_page (w_current->toplevel->pages, Current_Page);

  if (up_page == NULL) {
    u_log_message(_("Cannot find any schematics above the current one!\n"));
  }
  else {

    int   answer = TRUE;
    Page *child  = Current_Page;

    if (child->CHANGED) {
      answer = x_dialog_close_changed_page (w_current, child);
    }
    if (answer == TRUE) {
      x_window_set_current_page(w_current, up_page);
      x_window_close_page (w_current, child);
    }
  }
}

/** @} endgroup i_command_Hierarchy_Actions */

/** @} endgroup i_command_Page_Actions */

/* ------------------ Add ---------------- */

COMMAND (do_add)
{
  BEGIN_COMMAND(do_add_component);
  u_log_message("do_add command handler");
  EXIT_COMMAND(do_add_component);
}

/** \defgroup i_command_Add_Actions Actions under the Add Menu
 * @{
 */

/*! \brief Action Add Component in i_command_Add_Actions
 *  \par Function Description
 *  This is a callback function for the #ADD_COMPONENT action.
 */
COMMAND (do_add_component)
{
  BEGIN_W_COMMAND(do_add_component);

  o_redraw_cleanstates (w_current);
  x_compselect_open (w_current);

  i_status_set_state(w_current, SELECT);
  EXIT_COMMAND(do_add_component);
}

/*! \brief Action Add Net in i_command_Add_Actions
 *  \par Function Description
 *  This is a callback function for the #ADD_NET action.
 */
COMMAND (do_add_net)
{
  BEGIN_W_COMMAND(do_add_net);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);
  o_net_reset(w_current);

  if HOT_ACTION (do_add_net) {
    /* need to click */
    i_status_set_state(w_current, STARTDRAWNET);
    o_net_start (w_current, CMD_X(do_add_net), CMD_Y(do_add_net));
    w_current->inside_action = 1;
    state = DRAWNET;
  }
  else {
    state = STARTDRAWNET;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);

  EXIT_COMMAND(do_add_net);
}

/*! \brief Action Add Bus in i_command_Add_Actions
 *  \par Function Description
 *  This is a callback function for the #ADD_BUS action.
 */
COMMAND (do_add_bus)
{
  BEGIN_W_COMMAND(do_add_bus);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_bus) {

    /* need to click */
    i_status_set_state(w_current, STARTDRAWBUS);
    o_bus_start (w_current, CMD_X(do_add_bus), CMD_Y(do_add_bus));
    w_current->inside_action = 1;
    state = DRAWBUS;
  }
  else {
    state = STARTDRAWBUS;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);

  EXIT_COMMAND(do_add_bus);
}

/*! \brief Action Add Attribute in i_command_Add_Actions
 *  \par Function Description
 *  This is the action handler function for #ADD_ATTRIB action.
 *  The function calls x_attrib_add_dialog to launch the Single
 *  Attribute Editor with the SAE_ADD_MODE flag.
 *
 */
COMMAND (do_add_attribute)
{
  BEGIN_W_COMMAND( do_add_attribute);

  Object *o_current = o_select_return_first_object(w_current);
  if HOT_ACTION (do_add_attribute) {
    w_current->first_wx = CMD_X(do_add_attribute);
    w_current->first_wy = CMD_Y(do_add_attribute);
  }
  x_attrib_add_dialog(w_current, o_current);

  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND( do_add_attribute);
}

/*! \brief Add Text Mode in i_command_Add_Actions
 *  \par Function Description
 *  This is the action handler function for #ADD_TEXT.
 */
COMMAND (do_add_text)
{
  BEGIN_W_COMMAND(do_add_text);

  x_toolbars_turn_off_all_radios(w_current);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);

  x_dialog_text_input(w_current);
  EXIT_COMMAND(do_add_text);
}

/*! \brief Add Line Mode  in i_command_Add_Actions
 *  \par Function Description
 *  This is the action handler function for ADD_LINE.
 */
/** @brief i_cmd_do_add_line in i_command_Command_Functions */
COMMAND (do_add_line)
{
  BEGIN_W_COMMAND(do_add_line);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_line) {
    o_line_start (w_current, CMD_X(do_add_line), CMD_Y(do_add_line));
    w_current->inside_action = 1;
    state = ENDLINE;
  }
  else {
    state = DRAWLINE;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_add_line);
}

/*! \brief Action Add Pin Mode in i_command_Add_Actions
 *  \par Function Description
 *  This is the command function for the #ADD_PIN hotkey action.
 */
COMMAND (do_add_pin)
{
  BEGIN_W_COMMAND(do_add_pin);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_pin) {
    o_pin_start (w_current, CMD_X(do_add_pin), CMD_Y(do_add_pin));
    w_current->inside_action = 1;
    state = ENDPIN;
  }
  else {
    state = DRAWPIN;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_add_pin);
}

/*! \brief Action Add Box Mode initiated by Keyboard Hotkey
 *
 *  @brief i_cmd_do_add_bix in i_command_Add_Actions
 *
 *  \par Function Description
 *  This is the command function for the #ADD_BOX action.
 *
 */
COMMAND (do_add_box)
{
  BEGIN_W_COMMAND(do_add_box);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_box) {
    o_box_start (w_current, CMD_X(do_add_box), CMD_Y(do_add_box));
    w_current->inside_action = 1;
    state = ENDBOX;
  }
  else {
    state = DRAWBOX;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_add_box);
}

/*! \brief Action Add Circle Mode in i_command_Add_Actions
 *  \par Function Description
 *  This is the command function for the #ADD_CIRCLE action.
 */
COMMAND (do_add_circle)
{
  BEGIN_W_COMMAND(do_add_circle);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_circle) {
    o_circle_start (w_current, CMD_X(do_add_circle), CMD_Y(do_add_circle));
    w_current->inside_action = 1;
    state = ENDCIRCLE;
  }
  else {
    state = DRAWCIRCLE;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_add_circle);
}

/*! \brief Action Add Arc Mode in i_command_Add_Actions
 *  \par Function Description
 *  This is the command function for the #ADD_ARC action. An ARC
 *  is slightly different than other adder, an Arc requires users
 *  to draw the radius and then a dialog obtain further input, the
 *  dialog is also the Edit Arc and will react to all currently
 *  selected Arc objects, therefore this routine deselects all
 *  if needed.
 *
 */
COMMAND (do_add_arc)
{
  BEGIN_W_COMMAND(do_add_arc);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_arc) {
    o_arc_start (w_current, CMD_X(do_add_arc), CMD_Y(do_add_arc));
    w_current->inside_action = 1;
    state = ENDARC;
  }
  else {
    state = DRAWARC;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_add_arc);
}

/*! \brief Action Add Path Mode in i_command_Add_Actions
 *  \par Function Description
 *  This is the command function for the #ADD_PATH action.
 */
COMMAND (do_add_path)
{
  BEGIN_W_COMMAND(do_add_path);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_path) {
    o_path_start (w_current, CMD_X(do_add_path), CMD_Y(do_add_path));
    w_current->inside_action = 1;
    state = ENDPATH;
  }
  else {
    state = DRAWPATH;
    w_current->inside_action = 0;
  }

  i_status_set_state(w_current, state);
  EXIT_COMMAND(do_add_path);
}

/*! \brief Action Add Path Mode in i_command_Add_Actions
 *  \par Function Description
 *  This is the command function for the #ADD_PICTURE action.
 */
COMMAND (do_add_picture)
{
  BEGIN_W_COMMAND(do_add_picture);

  char *filename = NULL;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  w_current->inside_action = 0;

  filename = x_fileselect_select_image(w_current, NULL);

  if (filename != NULL) { /* if user did not cancel */

    if (o_picture_set_pixbuf(w_current, filename)) {
      w_current->inside_action = 1;
      i_status_set_state(w_current, DRAWPICTURE);
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

/*! \brief Create a New Session in i_command_Sessions_Actions
 *  \par Function Description
 *  This is the action handler function for #SESSION_NEW.
 */
COMMAND (do_session_new)
{
  BEGIN_W_COMMAND(do_session_new);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  x_sessions_new_dialog (w_current);

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_new);

}

/*! \brief Open an Existing Session in i_command_Sessions_Actions
 *  \par Function Description
 *  This is the action handler function for #SESSION_OPEN.
 */
COMMAND (do_session_open)
{
  BEGIN_W_COMMAND(do_session_open);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  x_sessions_open_dialog (w_current);

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_open);
}

/*! \brief Save Session in i_command_Sessions_Actions
 *  \par Function Description
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

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_save);
}

/*! \brief Save Session As in i_command_Sessions_Actions
 *  \par Function Description
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

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_save_as);
}

/*! \brief Open Session Manager in i_command_Sessions_Actions
 *  \par Function Description
 *  This is the action handler function for #SESSION_MANAGE.
 */
COMMAND (do_session_manage)
{
  BEGIN_W_COMMAND(do_session_manage);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  x_sessions_manage_dialog (w_current);

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);

  EXIT_COMMAND(do_session_manage);
}

/** @} endgroup i_command_Sessions_Actions */

/* ------------------ Attributes ---------------- */

/** \defgroup i_command_Attribute_Actions Actions Under the Attributes Menu
 * @{
 */

/*! \brief Attach Selected Attributes
 *  \par Function Description
 *  This is the action handler function to attach selected attributes
 *  to an object.
 *
 */
/** @brief i_cmd_do_attach in i_command_Attribute_Actions */
COMMAND (do_attach)
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_COMMAND(do_attach);

  Object *first_object;
  GList  *s_current;
  GList  *attached_objects = NULL;

  /* Do Not attach while inside an action */
  if (!w_current->inside_action) {

  /* skip over head */
    s_current = geda_list_get_glist(Current_Selection);

    if (s_current) {

      first_object = (Object *) s_current->data;

      if (first_object) {

        s_current = s_current->next; /* skipping over first object */

        while (s_current != NULL) {
          Object *object = s_current->data;
          if (object != NULL && object->attached_to == NULL) {
            o_attrib_attach (object, first_object, TRUE);
            attached_objects = g_list_prepend (attached_objects, object);
            Current_Page->CHANGED=1;
          }
          s_current = s_current->next;
        }

        if (attached_objects != NULL) {
          g_run_hook_object_list (w_current, "%attach-attribs-hook",
                                  attached_objects);
          g_list_free (attached_objects);
        }

        o_undo_savestate(w_current, UNDO_ALL);
      }
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_attach);
}

/*! \brief Detach Selected Attributes
 *  \par Function Description
 *  This is the action handler function to detach selected attributes
 *  from their parent.
 *
 */
/** @brief i_cmd_do_detach in i_command_Attribute_Actions */
COMMAND (do_detach)
{
  BEGIN_W_COMMAND(do_detach);

  GList *s_current;
  Object *o_current;
  GList *detached_attribs = NULL;

  /* Do Not detach while inside an action */
  if (!w_current->inside_action) {

    s_current = geda_list_get_glist(Current_Selection);
    while (s_current != NULL) {
      o_current = (Object *) s_current->data;
      if (o_current) {
        if (o_current->attribs) {
          detached_attribs = g_list_concat (g_list_copy (o_current->attribs),
                                            detached_attribs);
          o_attrib_detach_all (o_current);
        }
        else {
          if (o_current->attached_to) {
            o_attrib_detach (o_current);
            detached_attribs = g_list_concat (g_list_copy (o_current->attribs),
                                              detached_attribs);
          }
        }
      }
      NEXT(s_current);
    }

    if (detached_attribs != NULL) {
      g_run_hook_object_list (w_current, "%detach-attribs-hook",
                              detached_attribs);
      g_list_free (detached_attribs);
    }

    o_undo_savestate(w_current, UNDO_ALL);
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_detach);
}

/*! \brief Set Attributes to default X-Y Positions
 *  \par Function Description
 *  This is the action handler function to reset Attributes
 *  positions. The function operates on either selected text
 *  attributes or complex objects.
 *
 */
/** @brief do_home_attributes in i_command_Attribute_Actions */
COMMAND (do_home_attributes)
{
  BEGIN_W_COMMAND(do_home_attributes);

  /* Do Not do this while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {

      bool       modified  = FALSE;
      Page      *page      = Current_Page;
      SELECTION *selection = Current_Selection;
      GList     *s_current;

      for (s_current = geda_list_get_glist (selection); s_current != NULL; NEXT(s_current)) {

        Object *object = (Object*)s_current->data;

        if (object->type == OBJ_COMPLEX) {
          if (o_complex_reset_attrib_positions (w_current, object)) {
            modified = TRUE;
          }
        }
        else if ((object->type == OBJ_TEXT && o_get_is_attached(object))) {
          if (o_attrib_reset_position(w_current, object->attached_to, object)) {
            modified = TRUE;
          }
        }
      }

      if (modified) {

        o_undo_savestate (w_current, UNDO_ALL);
        if (GEDA_IS_PAGE(page)) {
          page->CHANGED = 1;
        }
        else {
          BUG_MSG("Object has no page association")
        }
      }
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_home_attributes);
}

/*! \brief Set selected Attributes to Show value
 *  \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show only the value of the attributes.
 *
 */
/** @brief i_cmd_do_show_value in i_command_Attribute_Actions */
COMMAND (do_show_value)
{
  BEGIN_W_COMMAND(do_show_value);

  /* Do Not show value while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = Current_Selection;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        Object *object = (Object*)s_current->data;
        if (object->type == OBJ_TEXT) {
          o_attrib_toggle_show_name_value (w_current, object, SHOW_VALUE);
	}
      }

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_show_value);
}

/*! \brief Set selected Attributes to Show Name
 *  \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show only the name of the attributes.
 *
 */
/** @brief i_cmd_do_show_name in i_command_Attribute_Actions */
COMMAND (do_show_name)
{
  BEGIN_W_COMMAND(do_show_name);

  /* Do Not show name while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = Current_Selection;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        Object *object = (Object*)s_current->data;
        if (object->type == OBJ_TEXT) {
            o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME);
	}
      }

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_show_name);
}

/*! \brief Set selected Attributes to Show Both
 *  \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show both the name and the value of selected attributes.
 *
 */
/** @brief i_cmd_do_show_both in i_command_Attribute_Actions */
COMMAND (do_show_both)
{
  BEGIN_W_COMMAND(do_show_both);

  /* Do Not show both while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = Current_Selection;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        Object *object = (Object*)s_current->data;
        if (object->type == OBJ_TEXT) {
          o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME_VALUE);
	}
      }

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_show_both);
}

/*! @brief Toggle Visibility of ALL Attribute Text */
COMMAND (do_toggle_visibility)
{
  BEGIN_W_COMMAND(do_toggle_visibility);

  /* Do Not toggle visibility while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {

      SELECTION *selection = Current_Selection;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        Object *object = (Object*)s_current->data;
        if (object->type == OBJ_TEXT) {
          o_attrib_toggle_visibility (w_current, object);
	}
      }

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_toggle_visibility);
}

/*! @brief Launch the Find Attribute Text Dialog */
COMMAND (do_find_text)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_find_text);

  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    x_dialog_find_text(w_current);
  }
}

/*! @brief Launch the Hide Attribute Text Dialog */
COMMAND (do_hide_text)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_hide_text);
  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
     x_dialog_hide_text(w_current);
  }
}

/*! @brief Launch the Show Attribute Text Dialog */
COMMAND (do_show_text)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_show_text);
  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    x_dialog_show_text(w_current);
  }
}

/*! @brief Launch the Multi-Attributes Dialog */
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

/*! @brief Launch the Auto Number Dialog */
COMMAND (do_autonumber)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_autonumber);
  if (!w_current->inside_action)  {  /* Don't execute this inside an action */
    autonumber_text_dialog(w_current);
  }
}


/*! \brief Launch the Log Console Dialog Action Responder
 *
 *  @brief i_cmd_do_show_console in i_command_Option_Actions
 *
 *  \par Function Description
 *  This is a callback function to launch the Console Dialog.
 *
 */
COMMAND (do_show_console)
{
  BEGIN_COMMAND(do_show_console);
  x_console_open (w_current);
  EXIT_COMMAND(do_show_console);
}

/*! \brief Launch the Coordinates Dialog Action Responder
 *
 *  @brief i_cmd_do_show_coordinates in i_command_Option_Actions
 *
 *  \par Function Description
 *  This is a callback function to launch the Coordinates Dialog.
 *
 *  TODO: Slated to be relocated in 2.09
 */
COMMAND (do_show_coordinates)
{
  BEGIN_COMMAND(do_show_coordinates);
  x_dialog_coord_dialog (w_current, 0, 0);
  EXIT_COMMAND(do_show_coordinates);
}

/*! \brief Toggle Macro Entry Area
 *
 *  @brief i_cmd_do_macro in i_command_Edit_Actions
 *
 *  \par Function Description
 *  This function set the macro widget to visable and set focus to
 *  to the entry object member.
 *
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

/*! \brief Edit Translate in i_command_Edit_Actions */
COMMAND (do_translate)
{
  BEGIN_W_COMMAND(do_translate);

  if (w_current->snap == SNAP_OFF) {
    u_log_message(_("WARNING: Do not translate with snap off!\n"));
    u_log_message(_("WARNING: Turning snap on and continuing "
                    "with translate.\n"));
    w_current->snap = SNAP_GRID;
    i_status_show_state(w_current, NULL); /* update status on screen */
  }

  if (w_current->snap_size != 100) {
    u_log_message(_("WARNING: Snap grid size is "
                    "not equal to 100!\n"));
    u_log_message(_("WARNING: If you are translating a symbol "
                    "to the origin, the snap grid size should be "
                    "set to 100\n"));
  }

  x_dialog_translate (w_current);
  EXIT_COMMAND(do_translate);
}

COMMAND (do_embed)
{
  BEGIN_W_COMMAND(do_embed);
  Object *o_current;

  /* anything selected ? */
  if (o_select_is_selection(w_current)) {
    /* yes, embed each selected component */
    GList *s_current = geda_list_get_glist(Current_Selection);

    while (s_current != NULL) {
      o_current = (Object *) s_current->data;
      if(o_current != NULL) {
        if ( (o_current->type == OBJ_COMPLEX) ||
             (o_current->type == OBJ_PICTURE)) {
          o_embed (w_current->toplevel, o_current);
        }
      }
      NEXT(s_current);
    }
    o_undo_savestate(w_current, UNDO_ALL);
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_status_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_embed);
}

/** @brief i_cmd_unembed in i_command_Edit_Actions */
COMMAND (do_unembed)
{
  BEGIN_W_COMMAND(do_unembed);
  Object *o_current;

  /* anything selected ? */
  if (o_select_is_selection(w_current)) {
    /* yes, unembed each selected component */
    GList *s_current =
      geda_list_get_glist(Current_Selection);

    while (s_current != NULL) {
      o_current = (Object *) s_current->data;
      if (o_current != NULL) {
        if ( (o_current->type == OBJ_COMPLEX) ||
             (o_current->type == OBJ_PICTURE)) {
          o_unembed (w_current->toplevel, o_current);
        }
      }
      NEXT(s_current);
    }
    o_undo_savestate(w_current, UNDO_ALL);
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_status_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_unembed);
}

/** @brief i_cmd_update in i_command_Edit_Actions */
COMMAND (do_update)
{
  BEGIN_W_COMMAND(do_update);

  GedaToplevel *toplevel = w_current->toplevel;
  GList *selection;
  GList *selected_components = NULL;
  GList *iter;

  if (o_select_is_selection(w_current)) {

    /* Updating components modifies the selection. Therefore, create a
     * new list of only the Objects we want to update from the current
     * selection, then iterate over that new list to perform the
     * update. */
    selection = geda_list_get_glist (toplevel->page_current->selection_list);
    for (iter = selection; iter != NULL; NEXT(iter)) {
      Object *o_current = (Object *) iter->data;
      if (o_current != NULL && o_current->type == OBJ_COMPLEX) {
        selected_components = g_list_prepend (selected_components, o_current);
      }
    }

    for (iter = selected_components; iter != NULL; NEXT(iter)) {
      Object *o_current = (Object *) iter->data;
      iter->data = o_edit_update_component (w_current, o_current);
    }

    g_list_free (selected_components);

  } else {
    /* nothing selected, go back to select state */
    u_log_message("Nothing selected\n");
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_status_set_state(w_current, SELECT);
  }

  EXIT_COMMAND(do_update);
}

/* ------------------ Options ---------------- */

/** \defgroup i_command_Option_Actions Actions Under the Options Menu
 * @{
 */

/*! @brief Set the Grid Display to Dots Mode */
COMMAND (do_grid_dots)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_dots);
  w_current->grid_mode = GRID_DOTS;
  x_grid_configure_variables (w_current);
  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);
}
/*! @brief Set the Grid Display to Mesh Mode */
COMMAND (do_grid_mesh)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_mesh);
  w_current->grid_mode = GRID_MESH;
  x_grid_configure_variables (w_current);
  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);
}
/*! @brief Turn the Grid Display Off in i_command_Option_Actions */
COMMAND (do_grid_off)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_off);
  w_current->grid_mode = GRID_NONE;
  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);
}

/*! @brief Cycle the Grid Mode in i_command_Option_Actions */
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
    case GRID_NONE: q_log_message (_("Grid OFF\n"));           break;
    case GRID_DOTS: q_log_message (_("Dot grid selected\n"));  break;
    case GRID_MESH: q_log_message (_("Mesh grid selected\n")); break;
  }

  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);

}

/*! @brief Increase the Snap Scale in i_command_Option_Actions */
COMMAND (do_snap_up)
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_snap_up);
  w_current->snap_size *= 2;

  i_status_update_grid_info (w_current);
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_snap_up);
}

/*! @brief Decrease the Snap Scale in i_command_Option_Actions */
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

/*! @brief Launch the Snap Settings Dialog */
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
/*! @brief Turn-On Snap Mode in i_command_Option_Actions */
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

/*! @brief Cycle the Snap Mode in i_command_Option_Actions */
COMMAND (do_cycle_snap)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_cycle_snap);
  /* toggle to the next snap state */
  w_current->snap = (w_current->snap+1) % SNAP_STATE_COUNT;

  switch (w_current->snap) {
  case SNAP_OFF:
    q_log_message(_("Snap OFF (CAUTION!)\n"));
    x_menu_set_togglable(w_current, SNAP_TOGGLE, FALSE);
    break;
  case SNAP_GRID:
    x_menu_set_togglable(w_current, SNAP_TOGGLE, TRUE);
    q_log_message(_("Snap ON\n"));
    break;
  case SNAP_RESNAP:
    x_menu_set_togglable(w_current, SNAP_TOGGLE, TRUE);
    q_log_message(_("Snap back to the grid (CAUTION!)\n"));
    break;
  default:
    g_critical("options_snap: toplevel->snap out of range: %d\n", w_current->snap);
  }
  i_status_show_state(w_current, NULL);  /* update status on screen */
  i_status_update_grid_info (w_current); /* update on screen grid status */
}

/*! @brief Toggle Action Feedback Mode in i_command_Option_Actions */
COMMAND (do_toggle_feedback)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_feedback);
  if (w_current->action_feedback_mode == BOUNDINGBOX) {
    w_current->action_feedback_mode = OUTLINE;
    q_log_message(_("Action feedback mode set to OUTLINE\n"));
  } else {
    w_current->action_feedback_mode = BOUNDINGBOX;
    q_log_message(_("Action feedback mode set to BOUNDINGBOX\n"));
  }

  if (w_current->inside_action && Current_Page->place_list != NULL) {
    w_current->debug = I_COMMAND_3485;
    o_place_invalidate_rubber (w_current, FALSE);
  }

  x_menu_set_togglable(w_current, OUTLINE_TOGGLE, w_current->action_feedback_mode);
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
    q_log_message(_("Rubber band OFF \n"));
  } else {
    w_current->netconn_rubberband = 1;
    q_log_message(_("Rubber band ON\n"));
  }
  x_menu_set_togglable(w_current, RUBBER_TOGGLE, w_current->netconn_rubberband);
}

/*! @brief Toggle Magnetic Nets Mode in i_command_Option_Actions */
COMMAND (do_toggle_magneticnet)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_magneticnet);
  if ((w_current->magnetic_net_mode = !w_current->magnetic_net_mode)) {
    q_log_message(_("magnetic net mode: ON\n"));
  }
  else {
    q_log_message(_("magnetic net mode: OFF\n"));
  }
  x_menu_set_togglable(w_current, MAGNETIC_TOGGLE, w_current->magnetic_net_mode);
  i_status_show_state(w_current, NULL);
}

/*! \brief Toggle Drag-Can-Move Mode in i_command_Option_Actions
 *  \par Function Description
 *  This is a callback function for the Toggle draw-can-move action API.
 *
 */
COMMAND (do_toggle_drawcanmove)
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_drawcanmove);
  const char *OnOff;
  if (w_current->drag_can_move) {
    w_current->drag_can_move = 0;
    OnOff =_("Off");
  } else {
    w_current->drag_can_move = 1;
    OnOff =_("On");
  }
  q_log_message(_("Drag-Can-Move is now %s\n"), OnOff);
  x_menu_set_togglable(w_current, DRAG_CAN_MOVE, w_current->drag_can_move);
}


/*! @brief Launch the Show Text Dialog */
COMMAND (do_show_text_size)
{
  NOT_NULL(w_current);

  BEGIN_COMMAND(do_show_text_size);
  text_size_dialog(w_current);
  EXIT_COMMAND(do_show_text_size);
}

/*! \brief Preferences Dialog Action Responder API Function
 *
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

/*! @brief Spawn the Help Guide in Browser */
COMMAND (do_show_manual)
{
  BEGIN_COMMAND(do_show_manual);
  bool result;
  char *pathname = NULL;

  pathname = g_build_filename (f_path_sys_doc (), "wiki", HELP_MANUAL_FILE, NULL);

  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      u_log_message("Check: pathname=%s\n", pathname);
    }
    GEDA_FREE(pathname);
  }

  EXIT_COMMAND(do_show_manual);
}
/*! @brief Launch the Help Hotkeys Dialog */
COMMAND (do_show_hotkeys)
{
  BEGIN_COMMAND(do_show_hotkeys);
  x_dialog_hotkeys(w_current);
  EXIT_COMMAND(do_show_hotkeys);
}

/*! @brief Spawn the Help FAQ in Browser */
COMMAND (do_show_faq)
{
  BEGIN_COMMAND(do_show_faq);
  bool result;
  char *pathname = NULL;
  pathname = g_build_filename (f_path_sys_doc (), "wiki", HELP_FAQ_FILE, NULL);
  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      u_log_message("Check: pathname=%s\n", pathname);
    }
    GEDA_FREE(pathname);
  }
  EXIT_COMMAND(do_show_faq);
}

/*! @brief Spawn the Help Geda in Browser */
COMMAND (do_show_geda)
{
  BEGIN_COMMAND(do_show_geda);
  bool result;
  char *pathname = NULL;
  pathname = g_build_filename (f_path_sys_doc (), "wiki", HELP_GEDA_FILE, NULL);
  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      u_log_message("Check: pathname=%s\n", pathname);
    }
    GEDA_FREE(pathname);
  }
  EXIT_COMMAND(do_show_geda);
}

/*! @brief Spawn the Help Wiki in Browser */
COMMAND (do_show_wiki)
{
  BEGIN_COMMAND(do_show_wiki);
  bool result;
  char *pathname = NULL;
  pathname = g_build_filename (f_path_sys_doc (), "wiki", HELP_WIKI_FILE, NULL);
  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      u_log_message("Check: pathname=%s\n", pathname);
    }
    GEDA_FREE(pathname);
  }
  EXIT_COMMAND(do_show_wiki);
}

/*! @brief Launch the Help About Dialog */
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
  u_log_message("(read only width=%d, height=%d\n)", width, height);
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

  u_log_message("<%s>", w_current->toplevel->untitled_name);
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

  u_log_message("<%s>", w_current->bus_ripper_symname);
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
