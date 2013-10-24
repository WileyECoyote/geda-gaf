/* -*- i_command.c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 Wiley Edward Hill
 *
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: February, 02, 2013
 * 
 */

#define PERFORMANCE
#ifdef PERFORMANCE
# include <sys/time.h>
# include <sys/resource.h>
# include "rusage/tlpi_hdr.h"
#endif

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#define USE_POSIX
#define MAX_THREADS              12
#define MAX_THREADS_UNUSED        2
#define MAX_THREADS_IDLE_TIME  1000   /* microseconds */
#define WAIT_THREADS_IDLE_TIME    5
#define TASK_WAIT_INTERVAL      100   /* microseconds */
#define MAX_WAIT_FOR_TASK        10   /* microseconds TASK_WAIT_INTERVAL */


#include "gschem.h"
#include "x_menu.h"
#include "i_command.h"

#define Renderer w_current->renderer
#define Toplevel w_current->toplevel

#define COMMAND(symbol, repeat, aflag,  func) [ cmd_##func ] = \
{ ACTION(symbol), repeat, 0, aflag, i_cmd_##func, 0, {0, 0}, 0, 0, 0},

#define DEBUG 1

#ifdef PERFORMANCE

static bool performance_diagnostics = FALSE;
static bool thread_diagnostics      = FALSE;

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
  void (*func)(void*);
  void* arg1;
  void* arg2;
} gschem_task;

static struct {
   char *name;
   char *repeat;
   unsigned char status;
   unsigned char aflag;
   void (*func) (GschemToplevel *w_current);
   int   who;

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
#define GET_R(var)command_struc[cmd_##var].narg=Renderer->var;
#define GET_T(var)command_struc[cmd_##var].narg=Toplevel->var;
#define GET_W(var)command_struc[cmd_##var].narg=w_current->var;

#define SHOW_VARIABLE(name, type) GET_##type(name) \
  s_log_message("current value of <%s> is <%d>\n", #name, CMD_INTEGER(name));

static GMutex i_lock_last_command;

static GThreadPool  *CommandPool     = NULL;
static int           is_engaged      = -1;
static int           last_command    =  0;

static void set_last_command(int value)
{
  g_mutex_lock(&i_lock_last_command);
    last_command = value;
  g_mutex_unlock(&i_lock_last_command);
}
static int get_last_command()
{
  int ret_val;
  g_mutex_lock(&i_lock_last_command);
    ret_val = last_command;
  g_mutex_unlock(&i_lock_last_command);
  return ret_val;
}

static bool i_command_dispatch(gschem_task *task)
{
  scm_dynwind_begin (0);
  g_dynwind_window (task->arg1); /* w_current */
  task->func(task->arg1);
  scm_dynwind_end ();
  g_free(task);
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
      task->func = (void*)command_struc[i].func;
      task->arg1 = command_struc[i].w_current;
      g_main_context_invoke (NULL, (void*) i_command_dispatch, task);
    }
    else /* USE_WORKER_THREAD */ {
      gdk_threads_enter();
      command_struc[i].func(command_struc[i].w_current);
      gdk_flush ();
      gdk_threads_leave();
    }
  }

  if ( thread_diagnostics == TRUE ) {
    unsigned int thread_id;
    thread_id = GPOINTER_TO_UINT (command);
    fprintf(stderr, "[idle] ---> entered thread:%2.2d\n", thread_id);
    g_usleep (WAIT_THREADS_IDLE_TIME * 1000);
    fprintf(stderr, "[idle] <--- exiting thread:%2.2d\n", thread_id);
  }
  else {

    accelerator = get_last_command();

    /* This should almost always happen */
    if (strequal(command_struc[accelerator].name, command)) {
      route(accelerator);
    }
    else {
      for (i = 1; i < COMMAND_COUNT; i++) {
        if (strequal(command_struc[i].name, command)) {
          route(i);
          break;
        }
      }
    }
  }
  return;
}


static bool
test_thread_idle_timeout (gpointer data)
{
  //unsigned int interval;
  int i;
  int nt;
  int up;

  for (i = 0; i < 2; i++) {

    g_thread_pool_push (CommandPool, GUINT_TO_POINTER (100 + i), NULL);
    nt = g_thread_pool_get_num_threads (CommandPool);
    up = g_thread_pool_unprocessed (CommandPool);

    fprintf (stderr, "[idle] ===> pushed new thread with id:%d, number of threads:%d, unprocessed:%d\n",
             100 + i, nt, up);
  }

  return !thread_diagnostics;
}

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
    if (strequal(command_struc[i].name, command)) {
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
    if (strequal(command_struc[i].name, command)) {

      if(verbose_mode) s_log_message("Processing Action Command <%s>, at index %d\n", command_struc[i].name, i);

      if (command_struc[i].repeat != NULL) {
        set_last_command(i); /* save last index for recall by repeat-last */
        i_update_middle_button(w_current, command_struc[i].repeat);
      }

      /* Check the repeat last command action */
      if ( i == CMD(do_repeat_last)) {
        if ( !get_last_command())
          break;
        i = get_last_command();
      }

      /* Check and set pointer coordinates if task bit 3 is set */
      if ( command_struc[i].aflag & XY_ActionMask ) {
        int wx, wy;
        if (x_event_get_pointer_position (w_current, TRUE, &wx, &wy)) {
          command_struc[i].point.x = wx;
          command_struc[i].point.y = wy;
        }
        else {
          command_struc[i].point.x = 0;
          command_struc[i].point.y = 0;
          w_current->first_wx      = -1; /* is the right thing to do? */
          w_current->first_wy      = -1;
        }
      }

      /* Fill in parameter arguments for this task */
      command_struc[i].narg      = narg;
      command_struc[i].who       = who;
      command_struc[i].sarg      = (unsigned char *) g_strdup(arg);
      command_struc[i].w_current = w_current;

      if (performance_diagnostics) {
        struct rusage usage;
        if (getrusage (RUSAGE_SELF, &usage) == 0) {
          printRusage("", &usage);
        }
        else
          fprintf(stderr, "getrusage returned error: %s\n", strerror( errno ));
      }

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
  char *message = MSG_SELECT_OBJECT_1ST;
  i_set_state_msg(w_current, SELECT, message);
}

static inline void null_err(char *var)
{
  s_log_message("internal error, i_command: variable <%s> can not be NULL\n", var);
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
  if (command_struc[index].sarg != NULL ) {
    arg = g_strdup((char *)command_struc[index].sarg);
    g_free(command_struc[index].sarg);
    arg = strstr_rep(arg, "  ", " ");
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

#define EXIT_COMMAND(efunc) if(arg) { g_free(arg); \
                                      g_strfreev ( argv);} \
                            CMD_STATUS(efunc) = 0;

#define BEGIN_NO_ARGUMENT(efunc) g_free(CMD_OPTIONS(efunc))
#define HOT_ACTION(symbol) (((CMD_WHO(symbol)==ID_ORIGIN_KEYBOARD) || (CMD_WHO(symbol)==ID_ORIGIN_MOUSE)) && (CMD_Y(symbol) != 0))

static void test_thread_idle_time ()
{
  unsigned int limit = 50;
  unsigned int interval = MAX_THREADS_IDLE_TIME;
  int i;
  int nt;
  int up;

  g_thread_pool_set_max_idle_time (interval);

  g_assert (g_thread_pool_get_max_idle_time () == interval);

  for (i = 0; i < limit; i++) {

    g_thread_pool_push (CommandPool, GUINT_TO_POINTER (i + 1), NULL);

    //g_usleep (1000);

    nt = g_thread_pool_get_num_threads (CommandPool);
    up = g_thread_pool_unprocessed (CommandPool);

    fprintf (stderr, "[idle] ===> pushed new thread with id:%d, number of threads:%d, unprocessed:%d\n",  i, nt, up);
  }

   g_timeout_add ((interval - 1000),
   test_thread_idle_timeout,
   GUINT_TO_POINTER (interval));

}
int gschem_diagnostics_dialog (GschemToplevel *w_current)
{
  GtkWidget *dialog;
  int r;
  gdk_threads_enter();
  dialog =  gtk_dialog_new_with_buttons ("GSCHEM Internal Diagnostics",
                                        (GtkWindow*) w_current->main_window,
                                         GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                         "Toggle Thread Monitor", GTK_RESPONSE_OK,
                                         "Toggle rusage data", GTK_RESPONSE_APPLY,
                                         "Run Thread Tests", GTK_RESPONSE_YES,
                                         GTK_STOCK_CLOSE,  GTK_BUTTONS_CLOSE,
                                         NULL);

  r = gtk_dialog_run (GTK_DIALOG (dialog));

  gtk_widget_destroy (dialog);
  gdk_threads_leave();
  return r;
}
/* -------------------- Begin Handler Functions ------------------- */
COMMAND (do_debug)
{
  BEGIN_COMMAND(do_debug);

  //msgbox("The string is \"%s\"\n and the integer is %d", arg, narg);
  int test = gschem_diagnostics_dialog(w_current);

  if (test == GTK_RESPONSE_OK) {
    msgbox("zoom-gain=%d \n",  w_current->zoom_gain);
  }
  if (test == GTK_RESPONSE_APPLY) {
    performance_diagnostics = performance_diagnostics ? FALSE : TRUE;
  }
  if (test == GTK_RESPONSE_YES) {
    int test_number;
    int nt;
    int up;

    thread_diagnostics = TRUE;
    test_number = 1;
    fprintf (stderr, "***** RUNNING TEST %2.2d *****\n", test_number);
    test_thread_idle_time();
    nt = g_thread_pool_get_num_threads (CommandPool);
    up = g_thread_pool_unprocessed (CommandPool);
    fprintf (stderr, "Test complete, waiting for %d threads to terminate, and %d unstarted processed\n", nt, up);

    while (up){
      g_usleep (WAIT_THREADS_IDLE_TIME*1000);
      up = g_thread_pool_unprocessed (CommandPool);
      fprintf (stderr, "Test complete, still waiting for %d threads to terminate\n", up);
    };
    thread_diagnostics = FALSE;
    fprintf (stderr, "All test have expired, resumming normal mode\n");
  }
  /*
   * if(is_engaged) {
   *  int nt = g_thread_pool_get_num_threads (CommandPool);
   *  int up = g_thread_pool_unprocessed (CommandPool);
   *  int it = g_thread_pool_get_max_idle_time()/1000;
   *  msgbox("The number of threads is %d, of which %d are pending, max idle is %d seconds\n", nt, up, it);
   }
   */

  EXIT_COMMAND(do_debug);
}
/*!\WARNING: { Do not point to another CMD_OPTIONS unless you know what your doing }*/

/** @fn i_cmd_do_repeat_last in i_command_Command_Handlers */
COMMAND (do_repeat_last)
{
  BEGIN_NO_ARGUMENT(do_repeat_last);
}
/* \defgroup i_command_Command_Handlers
 *  @{
 */
/** @fn i_cmd_do_file in i_command_Command_Handlers */
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

/*! \defgroup i_command_Command_Functions
 *  @{
 */

/*! \defgroup File-Actions Functions */

/*! \brief File New Window action in i_command_Command_Functions
 *  \par Function Description
 *  This is an Action handler function for the File New Window API.
 *  This function creates a new toplevel window.
 */
COMMAND (do_file_new_window)
{
  BEGIN_NO_ARGUMENT(do_file_new_window);

  GschemToplevel  *new_window;
  TOPLEVEL        *toplevel;
  PAGE            *page;

  new_window           = gschem_toplevel_new ();
  new_window->toplevel = s_toplevel_new ();
  toplevel             = new_window->toplevel;

  x_window_setup (new_window);

  //new_window->toplevel->load_newer_backup_func = x_fileselect_load_backup;
  //w_current->toplevel->load_newer_backup_data  = new_window;
  f_set_backup_loader_query_func  (toplevel,
                                   x_fileselect_load_backup,
                                   new_window);

  o_text_set_rendered_bounds_func (toplevel,
                                   o_text_get_rendered_bounds,
                                   new_window);

  /* Damage notifications should invalidate the object on screen */
  o_add_change_notify (new_window->toplevel,
                      (ChangeNotifyFunc) o_invalidate,
                      (ChangeNotifyFunc) o_invalidate, new_window);

  page = x_window_open_page (new_window, NULL);
  x_window_set_current_page (new_window, page);

  q_log_message (_("New Window created [%s]\n"), page->page_filename);

}
/** @fn i_cmd_do_new in i_command_Command_Functions */
COMMAND (do_file_new)
{
  BEGIN_W_COMMAND(do_file_new);
  PAGE *page;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  q_log_message (_("New page created [%s]\n"), page->page_filename);

  EXIT_COMMAND(do_file_new);

}

/** @fn i_cmd_do_open in i_command_Command_Functions */
COMMAND ( do_open ) {
  BEGIN_W_COMMAND(do_open);
    x_fileselect_open (w_current);
  EXIT_COMMAND(do_open);
}
/** @fn i_cmd_do_close in i_command_Command_Functions */
COMMAND ( do_close ) {
  BEGIN_W_COMMAND(do_close);
  bool can_close;
  can_close = TRUE;

  if (Current_Page->CHANGED ) {
    can_close = x_dialog_close_changed_page (w_current, Current_Page);
  }

  if (can_close) {
    q_log_message(_("Closing Window\n"));
    x_window_close_page (w_current, Current_Page);
  }

  EXIT_COMMAND(do_close);
}

/** @fn i_cmd_do_quit in i_command_Command_Functions */
COMMAND ( do_quit ) {
  BEGIN_NO_ARGUMENT(do_file_new_window);
  v_log_message(_("gschem: starting shut-down\n"));
  x_window_close_all(w_current);
}

/** @fn i_cmd_do_save in i_command_Command_Functions */
/*! \brief Save File As command action handler function
 *  \par Function Description
 *  Save the current file to disk.
 *  \note should be a flag that says whether
 *   page_filename is derived from untitled_name or specified by
 *   a user. Some twisted people might name their files like
 *   untitled_name. :-)
 */
COMMAND ( do_save ) {
  BEGIN_NO_ARGUMENT(do_save);
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  if(Current_Page->page_filename == NULL)
    w_current->force_save_as = TRUE;

  if (strstr(Current_Page->page_filename,
      w_current->toplevel->untitled_name))
        w_current->force_save_as = TRUE;

  if (w_current->force_save_as) {
      x_fileselect_save (w_current);
  }
  else {
      x_window_save_page (w_current,
                          Current_Page,
                          Current_Page->page_filename);
  }
}

/*! \brief Save File As action
 *  \par Function Description
 *  This is a callback function for the File Save As API
 *  The function calls i_command to process the action.
 */
/** @fn i_cmd_do_save_as in i_command_Command_Functions */
COMMAND ( do_save_as ) {
  BEGIN_W_COMMAND(do_save_as);
  x_fileselect_save (w_current);
  EXIT_COMMAND(do_save_as);
}
/** @fn i_cmd_do_save_all in i_command_Command_Functions */
COMMAND ( do_save_all ) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_NO_ARGUMENT(do_save_all);

  if (s_page_save_all(w_current->toplevel)) {
     i_set_state_msg(w_current, SELECT, _("Failed to Save All"));
  } else {
     i_set_state_msg(w_current, SELECT, _("Saved All"));
  }

  x_pagesel_update (w_current);
  i_update_sensitivities(w_current);
}
/** @fn i_cmd_do_print in i_command_Command_Functions */
COMMAND ( do_print ) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);
  NOT_NULL(w_current->toplevel->page_current->page_filename);
  BEGIN_COMMAND(do_print);
  char *base=NULL, *filename;
  char *ps_filename=NULL;

  /* shortcut */
  filename = Current_Page->page_filename;

  /* get the base file name */
  if (g_str_has_suffix(filename, ".sch")) {
    /* the filename ends with ".sch", remove it */
    base = g_strndup(filename, strlen(filename) - strlen(".sch"));
  } else {
    /* the filename does not end with .sch */
    base = g_strdup (filename);
  }

  /* add ".ps" tp the base filename */
  ps_filename = g_strconcat (base, ".ps", NULL);
  g_free(base);

  if (output_filename) {
    x_print_setup(w_current, output_filename);
  } else {
    x_print_setup(w_current, ps_filename);
  }

  g_free(ps_filename);
  EXIT_COMMAND(do_print);
}
/** @fn i_cmd_do_write_image in i_command_Command_Functions */
COMMAND ( do_write_image ) {
  BEGIN_W_COMMAND(do_write_image);
    w_current->inside_action = 0;
    o_redraw_cleanstates (w_current);
    i_set_state_msg(w_current, SELECT, _("Write Image"));
    i_set_state(w_current, SELECT);
    i_update_sensitivities(w_current);
    x_image_setup(w_current, last_image);
  EXIT_COMMAND(do_write_image);
}

/** @fn i_cmd_do_write_pdf in i_command_Command_Functions */
/*! \brief Write PDF command
 *  \par Function Description
 *  This is handles the write-pdf action
 */
COMMAND ( do_write_pdf ) {
  BEGIN_W_COMMAND(do_write_pdf);
    w_current->inside_action = 0;
    o_redraw_cleanstates (w_current);
    i_set_state_msg(w_current, SELECT, _("Write PDF"));
    i_set_state(w_current, SELECT);
    i_update_sensitivities(w_current);
  x_image_setup(w_current, pdf_image);
  EXIT_COMMAND(do_write_pdf);
}

/** @fn i_cmd_do_run_script in i_command_Command_Functions */
COMMAND ( do_run_script ) {
  BEGIN_W_COMMAND(do_run_script);
  char* filename = NULL;
  gdk_threads_enter();
  filename = gschem_filesel_dialog("Execute Script...", filename, FSB_LOAD );
  if(filename != NULL) { /* if user did not cancel */
    g_read_file(w_current->toplevel, filename, NULL);
    g_free(filename);
  }
  gdk_threads_leave();
  EXIT_COMMAND(do_run_script);
}

/*! \defgroup Edit-Actions Functions */

COMMAND ( do_edit )
{
  BEGIN_COMMAND(do_edit);
  s_log_message("do edit command handler");
  EXIT_COMMAND(do_edit);
}
COMMAND ( do_undo )
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
  } else {
    /* can loop here with arg */
    o_undo_callback(w_current, UNDO_ACTION);
  }
  EXIT_COMMAND(do_undo);
}
COMMAND ( do_redo )
{
  NOT_NULL(w_current);
  BEGIN_W_COMMAND(do_redo);
  o_undo_callback(w_current, REDO_ACTION);
  EXIT_COMMAND(do_redo);
}

COMMAND ( do_cut_clip )
{
  BEGIN_W_COMMAND(do_cut_clip);

  if (o_select_is_selection (w_current)){

    if ((narg < 0 ) || (arg == NULL )) {
      /* if no arguments then use buffer 0 */
      narg = 0;
    }

    o_buffer_cut (w_current, narg);
    if ( narg == 0)
      x_clipboard_set (w_current, object_buffer[narg]);
    else
      i_update_sensitivities(w_current);
  }
  EXIT_COMMAND(do_cut_clip);
}

COMMAND ( do_copy_clip )
{
  BEGIN_W_COMMAND(do_copy_clip);

  if (o_select_is_selection (w_current)) {

    if (narg < 0 || arg == NULL ) {
      /* if no arguments then use buffer 0 */
      narg = 0;
    }

    /* Copy to one of our buffer */
    o_buffer_copy (w_current, narg);
    /* if buffer number =0, the copy to system buffer */
    if ( narg == 0) {
      x_clipboard_set (w_current, object_buffer[0]);
    }
    i_update_sensitivities(w_current);
  }
  EXIT_COMMAND(do_copy_clip);
}

/*! \brief Action Paste Clipboard Contents
 *  \par Function Description
 *  This function initiates the pasting of the contents of a clip-
 *  board into the drawing.
 */
COMMAND ( do_paste_clip )
{
  BEGIN_W_COMMAND(do_paste_clip);

  TOPLEVEL *toplevel = w_current->toplevel;
  GList *object_list = NULL;
  int state;

  if ((narg < 0 ) || (arg == NULL )) {
    /* if no arguments then use buffer 0 */
    narg = 0;
    object_list = x_clipboard_get (w_current);
    s_delete_object_glist (toplevel, object_buffer[narg]);
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
    i_set_state (w_current, state);
  }
  else {
    i_set_state_msg (w_current, SELECT, _("Empty buffer"));
  }
  EXIT_COMMAND(do_paste_clip);
}

COMMAND ( do_delete )
{
  BEGIN_W_COMMAND(do_delete);

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    o_delete_selected(w_current);
   /* After deletion go into select mode */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    i_update_sensitivities(w_current);
    Current_Page->CHANGED = TRUE;
  }
  EXIT_COMMAND(do_delete);
}
COMMAND ( do_select )
{
  BEGIN_W_COMMAND(do_select);
  if (!o_invalidate_rubber (w_current)) {
    i_callback_cancel(w_current, 0, NULL);
  }

  o_redraw_cleanstates(w_current);
  i_set_state(w_current, SELECT);
  w_current->inside_action = 0;

  EXIT_COMMAND(do_select);
}
COMMAND ( do_select_all )
{
  BEGIN_W_COMMAND(do_select_all);
  o_redraw_cleanstates (w_current);
  o_select_visible_unlocked (w_current);

  i_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  i_update_sensitivities (w_current);
  EXIT_COMMAND(do_select_all);
}
/** @fn i_cmd_do_select_invert in i_command_Command_Functions
 *! \brief Inverts the Selection Set for all unlocked objects on page.
 *  \par Function Description
 * Sets all objects on page as deselected if the objected is selected and
 * selected if the object was not select.
 */
COMMAND ( do_select_invert )
{
  BEGIN_W_COMMAND(do_select_invert);
  TOPLEVEL *toplevel = w_current->toplevel;
  SELECTION *selection = toplevel->page_current->selection_list;

  GList *list = g_list_copy(geda_list_get_glist( selection ));
  o_select_visible_unlocked (w_current);
  while(list != NULL) {
    o_selection_remove (toplevel, selection, (OBJECT*) list->data);
    NEXT(list);
  }
  g_list_free (list);
  o_redraw_cleanstates(w_current);
  i_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  EXIT_COMMAND(do_select_invert);
}
/** @fn i_cmd_do_deselect in i_command_Command_Functions
 *! \brief Deselect all objects on page.
 *  \par Function Description
 * Sets all objects on page as deselected.
 */
COMMAND ( do_deselect )
{
  BEGIN_W_COMMAND(do_deselect);

  o_redraw_cleanstates (w_current);

  if (o_select_is_selection (w_current))
    i_set_state (w_current, DESELECT);
  else /* Automaticaly switch to SELECT mode cause nothing to deselect */
    msg_need_select_1st(w_current);
    //i_set_state (w_current, SELECT);

  w_current->inside_action = 0;
  i_update_sensitivities (w_current);
  EXIT_COMMAND(do_deselect);
}

/** @fn i_cmd_do_deselect in i_command_Command_Functions
 *! \brief Deselect all objects on page.
 *  \par Function Description
 * Sets all objects on page as deselected.
 */
COMMAND ( do_deselect_all )
{
  BEGIN_W_COMMAND(do_deselect_all);
  o_redraw_cleanstates (w_current);
  o_select_unselect_all (w_current);

  i_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  i_update_sensitivities (w_current);
  EXIT_COMMAND(do_deselect_all);
}

/** @fn i_cmd_do_copy in i_command_Command_Functions */
/*! \brief Copy selected objects on page.
 *  \par Function Description
 *   Initiate Copy mode for selected objects
 */
COMMAND ( do_copy )
{
  BEGIN_W_COMMAND(do_copy);
  int state;

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_copy) {
      i_set_state(w_current, COPY);;
      o_copy_start(w_current,  CMD_X(do_copy),  CMD_Y(do_copy));
      state = ENDCOPY;
      w_current->inside_action = 1;
    }
    else {
      state = STARTCOPY;
      w_current->inside_action = 0;
    }
    i_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }

  EXIT_COMMAND(do_copy);
}
/** @fn i_cmd_do_mcopy in i_command_Command_Functions */
/*! \brief Make multi copies of selected objects on page.
 *  \par Function Description
 *   Initiates Multi-Copy mode for selected objects
 */
COMMAND ( do_mcopy )
{
  BEGIN_W_COMMAND(do_mcopy);
  int state;

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_mcopy) {
      i_set_state(w_current, MCOPY);
      o_copy_start(w_current,  CMD_X(do_mcopy),  CMD_Y(do_mcopy));
      state = ENDMCOPY;
      w_current->inside_action = 1;
    }
    else {
      state = STARTMCOPY;
      w_current->inside_action = 0;
    }
    i_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }
  EXIT_COMMAND(do_mcopy);
}

/** @fn i_cmd_do_move in i_command_Command_Functions */
/*! \brief Move selected objects on page.
 *  \par Function Description
 *   Initiate Move mode for selected objects
 */
COMMAND ( do_move )
{
  BEGIN_W_COMMAND(do_move);
  int state;

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if HOT_ACTION (do_move) {
      i_set_state(w_current, MCOPY);
      o_move_start(w_current,  CMD_X(do_move),  CMD_Y(do_move));
      state = ENDMOVE;
      w_current->inside_action = 1;
    }
    else {
      state = STARTMOVE;
      w_current->inside_action = 0;
    }
    i_set_state(w_current, state);
  }
  else {
    msg_need_select_1st(w_current);
  }

  EXIT_COMMAND(do_move);
}
/** @fn i_cmd_do_rotate in i_command_Command_Functions */
/*! \brief Action Rotate
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 */
COMMAND ( do_rotate )
{
  BEGIN_W_COMMAND(do_rotate);

  /* If inside an appropriate action, send a button 2 released,
   * so rotating will be handled by x_event.c */
  if (w_current->inside_action &&
     (w_current->event_state == ENDCOMP ||
      w_current->event_state == ENDTEXT ||
      w_current->event_state == ENDMOVE ||
      w_current->event_state == ENDCOPY ||
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

      object_list = geda_list_get_glist( Current_Selection );

      if (object_list) {
        /* Allow o_rotate_world_update to redraw the objects */
        o_rotate_world_update(w_current, CMD_X(do_rotate),
                                         CMD_Y(do_rotate), 90, object_list);
      }

      state = SELECT;
    }
    else {
      state = ENDROTATEP;

    }
    w_current->inside_action = 0;
    i_set_state(w_current, state);
  }

  EXIT_COMMAND(do_rotate);
}

/*! \brief Mirror selected objects on page.
 *  \par Function Description
 *   Initiate Mirror mode for selected object
 */
COMMAND ( do_mirror )
{
  BEGIN_W_COMMAND(do_mirror);
  int state;

  o_redraw_cleanstates(w_current);
  if HOT_ACTION (do_mirror) {

    GList *object_list;

    object_list = geda_list_get_glist( w_current->toplevel->
                                       page_current->selection_list );

    if (object_list) {
      o_mirror_world_update(w_current, CMD_X(do_mirror), CMD_Y(do_mirror), object_list);
    }

    state = SELECT;
  }
  else {
    state = ENDMIRROR;
  }

  w_current->inside_action = 0;
  i_set_state(w_current, state);
  EXIT_COMMAND(do_mirror);
}
/*! \brief Edit Attributes for selected object.
 *  \par Function Description
 *   Calls o_edit to initiate the Edit Attributes dialog for
 *   selected object
 */
COMMAND ( do_edit_butes )
{
  BEGIN_W_COMMAND(do_edit_butes);

  o_edit(w_current, geda_list_get_glist( Current_Selection ),
         CMD_WHO(do_edit_butes));

  EXIT_COMMAND(do_edit_butes);
}
COMMAND ( do_edit_text )
{
  BEGIN_W_COMMAND(do_edit_text);
  OBJECT *object;

  object = o_select_return_first_object(w_current);
  if (object) {
    if (object->type == OBJ_TEXT) {
      o_text_edit(w_current, object);
    }
  }
  EXIT_COMMAND(do_edit_text);
}
COMMAND ( do_edit_slot )
{
  BEGIN_W_COMMAND(do_edit_slot);
  OBJECT *object;

  object = o_select_return_first_object(w_current);

  if (object) {
    o_slot_start(w_current, object);
  }
  EXIT_COMMAND(do_edit_slot);
}
/*! \brief Edit Color*/
COMMAND ( do_edit_arc )
{
  BEGIN_W_COMMAND(do_edit_arc);
  OBJECT *object;
  object = o_select_return_first_object(w_current);
  if ( object && object->type == OBJ_ARC ) {
    x_dialog_edit_arc_angle(w_current, NULL);
  }
  EXIT_COMMAND(do_edit_arc);
}
/*! \brief Edit Color*/
COMMAND ( do_edit_color )
{
  BEGIN_W_COMMAND(do_edit_color);
  x_dialog_edit_color (w_current);
  EXIT_COMMAND(do_edit_color);
}
COMMAND ( do_pintype )
{
  BEGIN_W_COMMAND(do_pintype);
  x_dialog_edit_pin_type (w_current);
  EXIT_COMMAND(do_pintype);
}
COMMAND ( do_linetype )
{
  BEGIN_W_COMMAND(do_linetype);
  x_dialog_edit_line_type(w_current);
  EXIT_COMMAND(do_linetype);
}
COMMAND ( do_filltype )
{
  BEGIN_W_COMMAND(do_filltype);
  x_dialog_edit_fill_type(w_current);
  EXIT_COMMAND(do_filltype);
}
COMMAND ( do_translate )
{
  BEGIN_W_COMMAND(do_translate);

  if (w_current->snap == SNAP_OFF) {
    s_log_message(_("WARNING: Do not translate with snap off!\n"));
    s_log_message(_("WARNING: Turning snap on and continuing "
                    "with translate.\n"));
    w_current->snap = SNAP_GRID;
    i_show_state(w_current, NULL); /* update status on screen */
  }

  if (w_current->snap_size != 100) {
    s_log_message(_("WARNING: Snap grid size is "
                    "not equal to 100!\n"));
    s_log_message(_("WARNING: If you are translating a symbol "
                    "to the origin, the snap grid size should be "
                    "set to 100\n"));
  }

  x_dialog_translate (w_current);
  EXIT_COMMAND(do_translate);
}
/*! \brief Lock
 *  \par Function Description
 *  This function calls o_lock to locks all objects in selection list.
 *
 */
COMMAND ( do_lock )
{
  BEGIN_W_COMMAND(do_lock);

  if (o_select_return_first_object(w_current)) {
    o_lock(w_current);
  }
  EXIT_COMMAND(do_lock);
}
/*! \brief Unlock objects in selection list
 *  \par Function Description
 *  This function calls o_unlock to unlocks all objects in selection list.
 */
COMMAND ( do_unlock )
{
  BEGIN_W_COMMAND(do_unlock);
  if (o_select_return_first_object(w_current)) {
    o_unlock(w_current);
  }
  EXIT_COMMAND(do_unlock);
}

COMMAND ( do_macro )
{
  BEGIN_W_COMMAND(do_macro);
  gtk_widget_show(w_current->macro_box);
  gtk_widget_grab_focus(w_current->macro_entry);
  EXIT_COMMAND(do_macro);
}

COMMAND ( do_embed )
{
  BEGIN_W_COMMAND(do_embed);
  OBJECT *o_current;

  /* anything selected ? */
  if (o_select_is_selection(w_current)) {
    /* yes, embed each selected component */
    GList *s_current = geda_list_get_glist( Current_Selection );

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
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
    i_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_embed);
}
/** @fn i_cmd_unembed in i_command_Command_Functions */
COMMAND ( do_unembed )
{
  BEGIN_W_COMMAND(do_unembed);
  OBJECT *o_current;

  /* anything selected ? */
  if (o_select_is_selection(w_current)) {
    /* yes, unembed each selected component */
    GList *s_current =
      geda_list_get_glist( Current_Selection );

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      if (o_current != NULL) {
        if ( (o_current->type == OBJ_COMPLEX) ||
             (o_current->type == OBJ_PICTURE) ) {
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
    i_set_state(w_current, SELECT);
  }
  EXIT_COMMAND(do_unembed);
}
/** @fn i_cmd_update in i_command_Command_Functions */
COMMAND (do_update)
{
  BEGIN_W_COMMAND(do_update);

  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection;
  GList *selected_components = NULL;
  GList *iter;

  if (o_select_is_selection(w_current)) {

    /* Updating components modifies the selection. Therefore, create a
     * new list of only the OBJECTs we want to update from the current
     * selection, then iterate over that new list to perform the
     * update. */
    selection = geda_list_get_glist (toplevel->page_current->selection_list);
    for (iter = selection; iter != NULL; NEXT(iter)) {
      OBJECT *o_current = (OBJECT *) iter->data;
      if (o_current != NULL && o_current->type == OBJ_COMPLEX) {
        selected_components = g_list_prepend (selected_components, o_current);
      }
    }
    for (iter = selected_components; iter != NULL; NEXT(iter)) {
      OBJECT *o_current = (OBJECT *) iter->data;
      iter->data = o_update_component (w_current, o_current);
    }
    g_list_free (selected_components);

  } else {
    /* nothing selected, go back to select state */
    s_log_message("Nothing selected\n");
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }

  EXIT_COMMAND(do_update);
}

/*! \defgroup View-Actions Functions */

COMMAND ( do_view )
{
  BEGIN_COMMAND(do_view);
  s_log_message("do_view command handler");
  EXIT_COMMAND(do_view);
}
COMMAND ( do_redraw )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_redraw);
  o_invalidate_all (w_current);
}

COMMAND ( do_pan )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_pan);
  o_redraw_cleanstates(w_current);
  w_current->inside_action = 0;
  i_set_state(w_current, STARTPAN);
}

/*! \brief Zoom Box Action Function
 *  \par Function Description
 *  This is a callback function for the Zoom Box action.
 */
COMMAND ( do_zoom_box )
{
  BEGIN_W_COMMAND(do_zoom_box);
  int state;

  o_select_unselect_all (w_current);
  o_redraw_cleanstates(w_current);

  if HOT_ACTION (do_zoom_box) {
    a_zoom_box_start( w_current, CMD_X(do_zoom_box), CMD_Y(do_zoom_box) );
    w_current->inside_action = 1;
    state = ZOOMBOXEND;
  }
  else {
    w_current->inside_action = 0;
    state = ZOOMBOXSTART;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_zoom_box);
}
/*! \brief Zoom Extents Action Function
 *  \par Function Description
 *  This is a callback function for the Zoom Extents action.
 */
COMMAND ( do_zoom_selected )
{
  BEGIN_W_COMMAND(do_zoom_selected);
  /* scroll bar stuff */
  const GList *selection;
  //    SELECTION *
  selection = Current_Selection->glist;
  a_zoom_extents (w_current, selection, 0);
  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_selected);
}
/*! \brief Zoom Extents Action Function
 *  \par Function Description
 *  This is a callback function for the Zoom Extents action.
 */
COMMAND ( do_zoom_extents )
{
  BEGIN_W_COMMAND(do_zoom_extents);
  /* scroll bar stuff */
  a_zoom_extents (w_current,
                  s_page_objects (Current_Page), 0);
  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_extents);
}
COMMAND ( do_zoom_in )
{
  BEGIN_W_COMMAND(do_zoom_in);
  a_zoom(w_current, ZOOM_IN_DIRECTIVE, CMD_WHO(do_zoom_out), 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_in);
}
COMMAND ( do_zoom_out )
{
  BEGIN_W_COMMAND(do_zoom_out);
  a_zoom(w_current, ZOOM_OUT_DIRECTIVE, CMD_WHO(do_zoom_out), 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_out);
}
COMMAND ( do_zoom_all)
{
  BEGIN_W_COMMAND(do_zoom_all);
  /* scroll bar stuff */
  a_zoom(w_current, ZOOM_FULL_DIRECTIVE, DONTCARE, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  EXIT_COMMAND(do_zoom_all);
}
COMMAND ( do_documentation)
{
  BEGIN_W_COMMAND(do_documentation);

  char *attrib_doc = NULL;
  OBJECT *object = NULL;
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
          s_log_message("error: %s", error->message);
          g_error_free (error);
        }
        g_free(attrib_doc);
      }
    }
  }
  else {
    q_log_message(_("No component selected"));
  }

  EXIT_COMMAND(do_documentation);
}
COMMAND ( do_show_hidden )
{
  BEGIN_W_COMMAND(do_show_hidden);
  /* Don't execute this inside an action - retest this */
  if (w_current->inside_action)
    return;

  o_edit_show_hidden (w_current,
                      s_page_objects (Current_Page));

  EXIT_COMMAND(do_show_hidden);
}
COMMAND ( do_show_nets )
{
  BEGIN_COMMAND(do_show_nets);
  s_log_message("Need show net names");
  EXIT_COMMAND(do_show_nets);
}
/*! \brief Load the Dark color map scheme
 *  \par Function Description
 *       This function loads the Dark color map scheme
 *       based on user input from the keyboard or menu.
 */
COMMAND ( do_dark_colors )
{
  BEGIN_W_COMMAND(do_dark_colors);
  /* Change the scheme here */
  x_load_color_scheme(DARK_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_dark_colors);
}
/*! \brief Load the Light color map scheme
 *  \par Function Description
 *       This function loads the Light color map scheme
 *       based on user input from the keyboard or menu.
 */
COMMAND ( do_light_colors )
{
  BEGIN_W_COMMAND(do_light_colors);
  /* Change the scheme here */
  x_load_color_scheme(LIGHT_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_light_colors);
}
/*! \brief Load the BW color map scheme
 *  \par Function Description
 *       This function loads the BW color map scheme
 *       based on user input from the keyboard or menu.
 */
COMMAND ( do_bw_colors )
{
  BEGIN_W_COMMAND(do_bw_colors);
  /* Change the scheme here */
  x_load_color_scheme(BW_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_bw_colors);
}
/* ------------------ Page ---------------- */
COMMAND ( do_page )
{
  BEGIN_COMMAND(do_page);
  s_log_message("do_page command handler");
  EXIT_COMMAND(do_page);
}
COMMAND ( do_page_manager )
{
  BEGIN_W_COMMAND(do_page_manager);
  x_pagesel_open (w_current);
  EXIT_COMMAND(do_page_manager);
}
COMMAND ( do_page_prev )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_prev);

  TOPLEVEL *toplevel = w_current->toplevel;

  PAGE *p_new;
  GList *iter;

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), Current_Page);
  iter = g_list_previous( iter );

  if ( iter != NULL  ) {

    p_new = (PAGE *)iter->data;

    if (w_current->enforce_hierarchy) {
      p_new = s_hierarchy_find_prev_page(toplevel->pages, Current_Page);
    }
    else {
      p_new = (PAGE *)iter->data;
    }

    if (p_new != NULL || p_new != Current_Page) {
      x_window_set_current_page (w_current, p_new);
    }
  }
}
COMMAND ( do_page_next )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_next);

  TOPLEVEL *toplevel = w_current->toplevel;

  PAGE *p_new;
  GList *iter;

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), Current_Page);
  NEXT(iter);

  if (iter != NULL) {

    if (w_current->enforce_hierarchy) {
      p_new = s_hierarchy_find_next_page(toplevel->pages, Current_Page);
    }
    else {
      p_new = (PAGE *)iter->data;
    }

    if (p_new != NULL || p_new != Current_Page) {
      x_window_set_current_page (w_current, p_new);
    }
  }
}

/* This is simular to file new accept we add new page hook*/
COMMAND ( do_page_new )
{
  BEGIN_W_COMMAND(do_page_new);

  EdaConfig *cfg = eda_config_get_user_context ();
  PAGE *page;
  char *tblock;
  char *sym_file;
  const CLibSymbol *clib;
  OBJECT *object;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);

  x_window_set_current_page (w_current, page);
  g_run_hook_page (w_current, "%new-page-hook", page);
 /* would be far easier, faster, and safer to set page->CHANGED=FALSE
  * here then for scheme to have done this in the hook, could just add
  * the titleblock here too */
  tblock = eda_config_get_string (cfg, "gschem", "default-titleblock", NULL);
  sym_file = g_strconcat(tblock, SYMBOL_FILE_DOT_SUFFIX, NULL);
  clib = s_clib_get_symbol_by_name (sym_file);
  if (clib != NULL) {
    object = o_complex_new (w_current->toplevel,
                            OBJ_COMPLEX, DEFAULT_COLOR_INDEX, 0, 0, 0,
                            FALSE, clib, sym_file, FALSE);

    s_page_append_object (w_current->toplevel, page, object);
  }

  g_free(sym_file);
  g_free(tblock);

  page->CHANGED = 1;
  a_zoom_extents (w_current, s_page_objects (page), A_PAN_DONT_REDRAW);

  q_log_message (_("New page created [%s]\n"), page->page_filename);

  EXIT_COMMAND(do_page_new);
}

COMMAND ( do_page_print ) {
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  BEGIN_COMMAND(do_page_print);
  s_page_print_all(w_current->toplevel);
  EXIT_COMMAND(do_page_print);
}
/** @fn i_cmd_do_revert in i_command_Command_Functions */
COMMAND ( do_page_revert ) {

  BEGIN_W_COMMAND(do_page_revert);

  PAGE *page;
  char *filename;
  int page_control;
  int up;
  int answer;

  answer = gschem_confirm_dialog (_("Really revert page?"), GTK_MESSAGE_QUESTION);

  if (answer == GTK_RESPONSE_YES ) {

    /* save this for later */
    filename = g_strdup (Current_Page->page_filename);
    page_control = Current_Page->page_control;
    up = Current_Page->up;

    /* delete the page, then re-open the file as a new page */
    s_page_delete (w_current->toplevel, Current_Page);

    page = x_window_open_page (w_current, filename);

    /* make sure we maintain the hierarchy info */
    page->page_control = page_control;
    page->up = up;

    x_window_set_current_page (w_current, page);
    g_free (filename);
  }

  EXIT_COMMAND(do_page_revert);
}
COMMAND ( do_page_close )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_COMMAND(do_page_close);

  bool  can_close;

  can_close = TRUE;

  if (Current_Page->CHANGED ) {
    can_close = x_dialog_close_changed_page (w_current, Current_Page);
  }

  if (can_close) {
    q_log_message(_("Closing Page\n"));
    x_window_close_page (w_current, Current_Page);
  }
  EXIT_COMMAND(do_page_close);
}
COMMAND ( do_page_discard )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_page_discard);

  x_window_close_page (w_current, Current_Page);

}
COMMAND ( do_add )
{
  BEGIN_COMMAND(do_add_component);
  s_log_message("do_add command handler");
  EXIT_COMMAND(do_add_component);
}

COMMAND ( do_add_component )
{
  BEGIN_W_COMMAND(do_add_component);

  o_redraw_cleanstates (w_current);
  x_compselect_open (w_current);

  i_set_state(w_current, SELECT);
  EXIT_COMMAND(do_add_component);
}

/*! \brief Action Add Net
 *  \par Function Description
 *  This is a callback function for the Add Net action.
 */
COMMAND ( do_add_net )
{
  BEGIN_W_COMMAND(do_add_net);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);
  o_net_reset(w_current);

  if HOT_ACTION (do_add_net) {
    /* need to click */
    i_set_state(w_current, STARTDRAWNET);
    o_net_start( w_current, CMD_X(do_add_net), CMD_Y(do_add_net) );
    w_current->inside_action = 1;
    state = DRAWNET;
  }
  else {
    state = STARTDRAWNET;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  /* somewhere you need to nearest point locking... */
  EXIT_COMMAND(do_add_net);
}
/*! \brief Action Add Bus
 *  \par Function Description
 *  This is a callback function for the Add Bus action.
 */
COMMAND ( do_add_bus )
{
  BEGIN_W_COMMAND(do_add_bus);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_bus) {

    /* need to click */
    i_set_state(w_current, STARTDRAWBUS);

    o_bus_start( w_current, CMD_X(do_add_bus), CMD_Y(do_add_bus) );
    w_current->inside_action = 1;
    state = DRAWBUS;
  }
  else {
    state = STARTDRAWBUS;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);

  /* somewhere we need nearest point locking... */
  w_current->inside_action = 0;
  EXIT_COMMAND(do_add_bus);
}

/*! \brief Action Add Attribute
 *  \par Function Description
 *  This is the action handler function for Add Attribute.
 *  \note This function calls the attrib_edit_dialog passing
 *  the integer who, which is a flag to indicate whether the
 *  Small Attribute Editor is creating a new attribute or
 *  editing an existing attribute. To create a new attribute
 *  the flag must be set to SAE_CREATE_NEW (non zero really).
 */
COMMAND ( do_add_attribute )
{//CMD_WHO(do_add_attribute))
  BEGIN_W_COMMAND( do_add_attribute);

  OBJECT *o_current = o_select_return_first_object(w_current);
 // POINT  *point;

 // point = CMD_POINT(do_add_attribute)
 //point.x

  if HOT_ACTION (do_add_attribute) {
    x_attrib_add_dialog(w_current, o_current );
  }
  else
    x_attrib_add_dialog(w_current, o_current);

  i_set_state(w_current, SELECT);

  EXIT_COMMAND( do_add_attribute);
}

/*! \brief Add Text Mode
 *  \par Function Description
 *  This is the action handler function for Add Text.
 */
COMMAND ( do_add_text )
{
  BEGIN_W_COMMAND(do_add_text);

  x_toolbars_turn_off_all_radios(w_current);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  w_current->inside_action = 0;
  i_set_state(w_current, SELECT);

  x_dialog_text_input(w_current);
  EXIT_COMMAND(do_add_text);
}

/*! \brief Add Line Mode
 *  \par Function Description
 *  This is the action handler function for Add Line.
 */
/** @fn i_cmd_do_add_line in i_command_Command_Functions */
COMMAND (do_add_line)
{
  BEGIN_W_COMMAND(do_add_line);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_line) {
    o_line_start( w_current, CMD_X(do_add_line), CMD_Y(do_add_line) );
    w_current->inside_action = 1;
    state = ENDLINE;
  }
  else {
    state = DRAWLINE;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_add_line);
}

/*! \brief Action Add Path
 *  \par Function Description
 *  This is the command function for the Add Path action.
 */
COMMAND ( do_add_path )
{
  BEGIN_W_COMMAND(do_add_path);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_path) {
    o_path_start( w_current, CMD_X(do_add_path), CMD_Y(do_add_path) );
    w_current->inside_action = 1;
    state = ENDPATH;
  }
  else {
    state = DRAWPATH;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_add_path);
}

/*! \brief Action Add Box initiated by Keyboard Hotkey
 *  \par Function Description
 *  This is the command function for the Add Box action.
 */
COMMAND ( do_add_box )
{
  BEGIN_W_COMMAND(do_add_box);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_box) {
    o_box_start( w_current, CMD_X(do_add_box), CMD_Y(do_add_box) );
    w_current->inside_action = 1;
    state = ENDBOX;
  }
  else {
    state = DRAWBOX;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_add_box);
}
/*! \brief Action Add Circle
 *  \par Function Description
 *  This is the command function for the Add Circle action.
 */
COMMAND ( do_add_circle )
{
  BEGIN_W_COMMAND(do_add_circle);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_circle) {
    o_circle_start( w_current, CMD_X(do_add_circle), CMD_Y(do_add_circle));
    w_current->inside_action = 1;
    state = ENDCIRCLE;
  }
  else {
    state = DRAWCIRCLE;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_add_circle);
}

/*! \brief Action Add Arc
 *  \par Function Description
 *  This is the command function for the Add Arc action.
 */
COMMAND ( do_add_arc )
{
  BEGIN_W_COMMAND(do_add_arc);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_arc) {
    o_arc_start( w_current, CMD_X(do_add_arc), CMD_Y(do_add_arc));
    w_current->inside_action = 1;
    state = ENDARC;
  }
  else {
    state = DRAWARC;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_add_arc);
}

/*! \brief Action Add Pin
 *  \par Function Description
 *  This is the command function for the Add Pin hotkey action.
 */
COMMAND ( do_add_pin )
{
  BEGIN_W_COMMAND(do_add_pin);

  int state;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  if HOT_ACTION (do_add_pin) {
    o_pin_start( w_current, CMD_X(do_add_pin), CMD_Y(do_add_pin));
    w_current->inside_action = 1;
    state = ENDPIN;
  }
  else {
    state = DRAWPIN;
    w_current->inside_action = 0;
  }

  i_set_state(w_current, state);
  EXIT_COMMAND(do_add_pin);
}
COMMAND ( do_add_picture )
{
  BEGIN_W_COMMAND(do_add_picture);

  char* filename = NULL;
  GdkPixbuf *pixbuf;
  GError *error = NULL;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  w_current->inside_action = 0;
  i_set_state(w_current, SELECT);

  if (w_current->pixbuf_filename)
    filename = w_current->pixbuf_filename;

  filename = gschem_filesel_dialog("Select image file", filename, FSB_LOAD);
  if(filename != NULL) { /* if user did not cancel */
    pixbuf = gdk_pixbuf_new_from_file (filename, &error);
    if (pixbuf) {

      w_current->inside_action = 1;
      o_picture_set_pixbuf(w_current, pixbuf, filename);
      Toplevel->page_current->CHANGED=1;
      i_set_state(w_current, DRAWPICTURE);
    }
    else
      gschem_message_dialog(_("Failed to load picture: %s"), GEDA_MESSAGE_ERROR, NULL);
    g_free(filename);
  }
  EXIT_COMMAND(do_add_picture);
}

COMMAND ( do_down_schematic )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_down_schematic);

  OBJECT *object           = NULL;
  PAGE   *child            = NULL;
  PAGE   *save_first_page  = NULL;

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
    current_filename = u_basic_breakup_string(attrib, ',', pcount);

    /* loop over all filenames */
    while(current_filename != NULL) {
      GError *err = NULL;
      s_log_message(_("Searching for source [%s]\n"), current_filename);
      child = s_hierarchy_down_schematic_single(w_current->toplevel,
                                                current_filename,
                                                Current_Page,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD,
                                                &err);

      /* s_hierarchy_down_schematic_single() will not zoom the loaded page */
      if (child != NULL) {
        s_page_goto (w_current->toplevel, child);
        a_zoom_extents(w_current, s_page_objects (Current_Page),
                       A_PAN_DONT_REDRAW);
        o_undo_savestate(w_current, UNDO_ALL);
        s_page_goto (w_current->toplevel, Current_Page);
      }

      /* save the first page */
      if ( !loaded_flag && (child != NULL)) {
        save_first_page = child;
      }

      /* now do some error reporting */
      if (child == NULL) {

        const char *msg = (err != NULL) ? err->message : "Unknown error.";

        s_log_message(_("Failed to descend into '%s': %s\n"),
                         current_filename, msg);

        char *secondary = g_strdup_printf (
                         _("Failed to descend hierarchy into '%s': %s"),
                            current_filename, msg);

        titled_pango_error_dialog("<b>Failed to descend hierarchy</b>",
                                  secondary, _("Hierarchy Error"));
        g_free (secondary);
        g_error_free (err);

      }
      else {
        /* this only signifies that we tried */
        loaded_flag = TRUE;
        page_control = child->page_control;
      }

      g_free(current_filename);
      pcount++;
      current_filename = u_basic_breakup_string(attrib, ',', pcount);
    }

    g_free(attrib);
    g_free(current_filename);

    count++;

    /* continue looking outside first */
    if (!looking_inside) {
      attrib =
        o_attrib_search_attached_attribs_by_name (object, "source", count);
    }

    /* okay we were looking outside and didn't find anything,
     * so now we need to look inside the symbol */
    if (!looking_inside && attrib == NULL && !loaded_flag ) {
      looking_inside = TRUE;
#if DEBUG
      printf("switching to go to look inside\n");
#endif
    }

    if (looking_inside) {
#if DEBUG
      printf("looking inside\n");
#endif
      attrib =
        o_attrib_search_inherited_attribs_by_name (object, "source", count);
    }
  }

  if (loaded_flag && (save_first_page != NULL)) {
    x_window_set_current_page (w_current, save_first_page);
  }
}

/*! \bug may cause problems with non-directory symbols */
COMMAND ( do_down_symbol )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_down_symbol);

  OBJECT *object;
  const CLibSymbol *sym;

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {
      s_log_message(_("Searching for symbol [%s]\n"),
                    object->complex_basename);
      sym = s_clib_get_symbol_by_name (object->complex_basename);
      if (sym == NULL)
        return;
      if (s_clib_symbol_get_filename(sym) == NULL) {
        s_log_message(_("Symbol is not a real file."
                        " Symbol cannot be loaded.\n"));
        return;
      }
      s_hierarchy_down_symbol(w_current->toplevel, sym, Current_Page);

      /* s_hierarchy_down_symbol() will not zoom the loaded page */
      a_zoom_extents(w_current,
                     s_page_objects (Current_Page),
                     A_PAN_DONT_REDRAW);
      o_undo_savestate(w_current, UNDO_ALL);
      x_window_set_current_page(w_current, Current_Page);
    }
  }
}

COMMAND ( do_hierarchy_up )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->pages);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_NO_ARGUMENT(do_hierarchy_up);

  PAGE *up_page;

  up_page = s_hierarchy_find_up_page (w_current->toplevel->pages, Current_Page);

  if (up_page == NULL) {
    s_log_message(_("Cannot find any schematics above the current one!\n"));
  }
  else {
    int answer = TRUE;
    if (Current_Page->CHANGED){
      answer = x_dialog_close_changed_page (w_current, Current_Page);
    }
    if(answer == TRUE) {
      x_window_set_current_page(w_current, up_page);
      x_window_close_page (w_current, Current_Page);
    }
  }
}
/*
COMMAND ( do_hierarchy_documentation )
{
  BEGIN_COMMAND(do_hierarchy_documentation);
  s_log_message("in hierarchy_documentation command handler");
  EXIT_COMMAND(do_hierarchy_documentation);
}
*/
/*! \brief Attach Selected Attributes
 *  \par Function Description
 *  This is the action handler function to attach selected attributes
 *  to an object.
 *
 */
/** @fn i_cmd_do_attach in i_command_Command_Functions */
COMMAND ( do_attach )
{
  NOT_NULL(w_current);
  NOT_NULL(w_current->toplevel);
  NOT_NULL(w_current->toplevel->page_current);

  BEGIN_COMMAND(do_attach);

  OBJECT *first_object;
  GList  *s_current;
  GList  *attached_objects = NULL;

  /* Do Not attach while inside an action */
  if (!w_current->inside_action) {

  /* skip over head */
    s_current = geda_list_get_glist( Current_Selection );
    if (s_current) {

      first_object = (OBJECT *) s_current->data;
      if (first_object) {
        NEXT(s_current); /* skipping over first object */
        while (s_current != NULL) {
          OBJECT *object = s_current->data;
          if (object != NULL) {
            o_attrib_attach (w_current->toplevel, object, first_object, TRUE);
            attached_objects = g_list_prepend (attached_objects, object);
            Current_Page->CHANGED=1;
          }
          NEXT(s_current);
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
/** @fn i_cmd_do_detach in i_command_Command_Functions */
COMMAND ( do_detach )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_detach);
  GList *s_current;
  OBJECT *o_current;
  GList *detached_attribs = NULL;

  /* Do Not detach while inside an action */
  if (!w_current->inside_action) {

    s_current = geda_list_get_glist( Current_Selection );
    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      if (o_current) {
        if (o_current->attribs) {
          detached_attribs = g_list_concat (g_list_copy (o_current->attribs),
                                            detached_attribs);
          o_attrib_detach_all (w_current->toplevel, o_current);
          Current_Page->CHANGED=1;
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

/*! \brief Set selected Attributes to Show value
 *  \par Function Description
 *  This is the action handler function to set selected Attributes bits
 *  to show only the value of the attributes.
 *
 */
/** @fn i_cmd_do_show_value in i_command_Command_Functions */
COMMAND ( do_show_value )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_show_value);
  TOPLEVEL *toplevel = w_current->toplevel;

  /* Do Not show value while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = toplevel->page_current->selection_list;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        OBJECT *object = (OBJECT*)s_current->data;
        if (object->type == OBJ_TEXT)
          o_attrib_toggle_show_name_value (w_current, object, SHOW_VALUE);
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
/** @fn i_cmd_do_show_name in i_command_Command_Functions */
COMMAND ( do_show_name )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_show_name);
  TOPLEVEL *toplevel = w_current->toplevel;

  /* Do Not show name while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = toplevel->page_current->selection_list;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        OBJECT *object = (OBJECT*)s_current->data;
        if (object->type == OBJ_TEXT)
            o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME);
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
/** @fn i_cmd_do_show_both in i_command_Command_Functions */
COMMAND ( do_show_both )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_show_both);
  TOPLEVEL *toplevel = w_current->toplevel;

  /* Do Not show both while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = toplevel->page_current->selection_list;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        OBJECT *object = (OBJECT*)s_current->data;
        if (object->type == OBJ_TEXT)
          o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME_VALUE);
      }

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_show_both);
}

/*! @brief Toggle Visibility of ALL Attribute Text */
COMMAND ( do_toggle_visibility )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_toggle_visibility);
  TOPLEVEL *toplevel = w_current->toplevel;

  /* Do Not toggle visibility while inside an action */
  if (!w_current->inside_action) {

    if (o_select_is_selection (w_current)) {
      SELECTION *selection = toplevel->page_current->selection_list;
      GList *s_current;

      for (s_current = geda_list_get_glist (selection);
           s_current != NULL;
           NEXT(s_current)) {
        OBJECT *object = (OBJECT*)s_current->data;
        if (object->type == OBJ_TEXT)
          o_attrib_toggle_visibility (w_current, object);
      }

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else
    v_log_message(_("Cannot edit attribute properties inside action!\n"));

  EXIT_COMMAND(do_toggle_visibility);
}

/*! @brief Launch the Find Attribute Text Dialog */
COMMAND ( do_find_text )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_find_text);

  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    x_dialog_find_text(w_current);
  }
}

/*! @brief Launch the Hide Attribute Text Dialog */
COMMAND ( do_hide_text )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_hide_text);
  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
     x_dialog_hide_text(w_current);
  }
}

/*! @brief Launch the Show Attribute Text Dialog */
COMMAND ( do_show_text )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_show_text);
  /* Don't execute this inside an action */
  if (!w_current->inside_action) {
    x_dialog_show_text(w_current);
  }
}
/*! @brief Launch the Multi-Attributes Dialog */
COMMAND ( do_attributes )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_attributes);
  if (!w_current->inside_action)  {  /* Don't execute this inside an action */
    x_multiattrib_open(w_current);
  }
}
/*! @brief Launch the Auto Number Dialog */
COMMAND ( do_autonumber )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_autonumber);
  if (!w_current->inside_action)  {  /* Don't execute this inside an action */
    autonumber_text_dialog(w_current);
  }
}
/*! @brief Set the Grid Display to Dots Mode */
COMMAND ( do_grid_dots )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_dots);
  w_current->grid_mode = GRID_DOTS;
  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}
/*! @brief Set the Grid Display to Mesh Mode */
COMMAND ( do_grid_mesh )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_mesh);
  w_current->grid_mode = GRID_MESH;
  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}
/*! @brief Turn the Grid Display Off */
COMMAND ( do_grid_off )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_grid_off);
  w_current->grid_mode = GRID_NONE;
  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}

/*! @brief Cycle the Grid Mode */
COMMAND ( do_cycle_grid )
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

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);

}

/*! @brief Increase the Snap Scale */
COMMAND ( do_snap_up )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_snap_up);
  w_current->snap_size *= 2;

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_snap_up);
}

/*! @brief Decrease the Snap Scale */
COMMAND ( do_snap_down )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_snap_down);
  if (w_current->snap_size % 2 == 0)
    w_current->snap_size /= 2;

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
  EXIT_COMMAND(do_snap_down);
}

/*! @brief Launch the Snap Settings Dialog */
COMMAND ( do_show_snap )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_show_snap);
  snap_size_dialog(w_current);
}

COMMAND ( do_snap_off )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_snap_off);

  if (w_current->snap != SNAP_OFF)
     w_current->old_snap = w_current->snap;
  w_current->snap = SNAP_OFF;
  i_show_state(w_current, NULL);  /* update status on screen */
  i_update_grid_info (w_current); /* update on screen grid status */

}
COMMAND ( do_snap_on )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_snap_on);

  if (w_current->old_snap != SNAP_OFF)
    w_current->snap =  w_current->old_snap;
  else
    w_current->snap = SNAP_GRID;

  i_show_state(w_current, NULL);  /* update status on screen */
  i_update_grid_info (w_current); /* update on screen grid status */

}

/*! @brief Cycle the Snap Mode */
COMMAND ( do_cycle_snap )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_cycle_snap);
  /* toggle to the next snap state */
  w_current->snap = (w_current->snap+1) % SNAP_STATE_COUNT;

  switch (w_current->snap) {
  case SNAP_OFF:
    q_log_message(_("Snap OFF (CAUTION!)\n"));
    x_menu_set_toggle(w_current, SNAP_TOGGLE, FALSE);
    break;
  case SNAP_GRID:
    x_menu_set_toggle(w_current, SNAP_TOGGLE, TRUE);
    q_log_message(_("Snap ON\n"));
    break;
  case SNAP_RESNAP:
    x_menu_set_toggle(w_current, SNAP_TOGGLE, TRUE);
    q_log_message(_("Snap back to the grid (CAUTION!)\n"));
    break;
  default:
    g_critical("options_snap: toplevel->snap out of range: %d\n", w_current->snap);
  }
  i_show_state(w_current, NULL);  /* update status on screen */
  i_update_grid_info (w_current); /* update on screen grid status */
}

/*! @brief Toggle Action Feedback Mode */
COMMAND ( do_toggle_feedback )
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
  if (w_current->inside_action &&
      Current_Page->place_list != NULL)
    o_place_invalidate_rubber (w_current, FALSE);

  x_menu_set_toggle(w_current, OUTLINE_TOGGLE, w_current->action_feedback_mode);
}

/*! \brief Toggle Rubberband Mode
 *  \par Function Description
 *  This is a callback function for the Toggle Rubberband action API.
 *  \note
 *
 *  Rubber band is cool !
 *  Chris Ellec - January 2001:Added on/off option from the pull down menu
 *  Wiley E. Hill- December 2012:Changed to Menu Toggle Button
 */
COMMAND ( do_toggle_rubberband )
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
  x_menu_set_toggle(w_current, RUBBER_TOGGLE, w_current->netconn_rubberband);
}

/*! @brief Toggle Magnetic Nets Mode */
COMMAND ( do_toggle_magneticnet )
{
  NOT_NULL(w_current);
  BEGIN_NO_ARGUMENT(do_toggle_magneticnet);
  if ((w_current->magnetic_net_mode = !w_current->magnetic_net_mode)) {
    q_log_message(_("magnetic net mode: ON\n"));
  }
  else {
    q_log_message(_("magnetic net mode: OFF\n"));
  }
  x_menu_set_toggle(w_current, MAGNETIC_TOGGLE, w_current->magnetic_net_mode);
  i_show_state(w_current, NULL);
}

/*! @brief Launch the Log Console Dialog */
COMMAND ( do_show_console )
{
  BEGIN_COMMAND(do_show_console);
  x_console_open (w_current);
  EXIT_COMMAND(do_show_console);
}

/*! @brief Launch the Coordinates Dialog */
COMMAND ( do_show_coordinates )
{
  BEGIN_COMMAND(do_show_coordinates);
  x_dialog_coord_dialog (w_current, 0, 0);
  EXIT_COMMAND(do_show_coordinates);
}

/*! @brief Launch the Show Text Dialog */
COMMAND ( do_show_text_size )
{
  NOT_NULL(w_current);

  BEGIN_COMMAND(do_show_text_size);
  text_size_dialog(w_current);
  EXIT_COMMAND(do_show_text_size);
}

/*! \brief Preferences Dialog Action Responder API Function
 *  \par Function Description
 *  This is a callback function to Launch the Preferences Dialog.
 *
 *  Author: Wiley E. Hill
 *  Date:   Aug 5th, 2012
 */
COMMAND ( do_show_settings )
{
  NOT_NULL(w_current);
  BEGIN_COMMAND(do_show_settings);
  x_configure_settings(w_current); /* Load and display Dialog */
  EXIT_COMMAND(do_show_settings);
}

/*! @brief Spawn the Help Guide in Browser */
COMMAND ( do_show_manual )
{
  BEGIN_COMMAND(do_show_manual);
  bool result;
  char *pathname = NULL;

  pathname = g_build_filename (s_path_sys_doc (), "wiki", HELP_MANUAL_FILE, NULL);

  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      s_log_message("Check: pathname=%s\n", pathname);
    }
    g_free(pathname);
  }

  EXIT_COMMAND(do_show_manual);
}
/*! @brief Launch the Help Hotkeys Dialog */
COMMAND ( do_show_hotkeys )
{
  BEGIN_COMMAND(do_show_hotkeys);
  x_dialog_hotkeys(w_current);
  EXIT_COMMAND(do_show_hotkeys);
}
/*! @brief Spawn the Help FAQ in Browser */
COMMAND ( do_show_faq )
{
  BEGIN_COMMAND(do_show_faq);
  bool result;
  char *pathname = NULL;
  pathname = g_build_filename (s_path_sys_doc (), "wiki", HELP_FAQ_FILE, NULL);
  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      s_log_message("Check: pathname=%s\n", pathname);
    }
    g_free(pathname);
  }
  EXIT_COMMAND(do_show_faq);
}
/*! @brief Spawn the Help Geda in Browser */
COMMAND ( do_show_geda )
{
  BEGIN_COMMAND(do_show_geda);
  bool result;
  char *pathname = NULL;
  pathname = g_build_filename (s_path_sys_doc (), "wiki", HELP_GEDA_FILE, NULL);
  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      s_log_message("Check: pathname=%s\n", pathname);
    }
    g_free(pathname);
  }
  EXIT_COMMAND(do_show_geda);
}
/*! @brief Spawn the Help Wiki in Browser */
COMMAND ( do_show_wiki )
{
  BEGIN_COMMAND(do_show_wiki);
  bool result;
  char *pathname = NULL;
  pathname = g_build_filename (s_path_sys_doc (), "wiki", HELP_WIKI_FILE, NULL);
  if (pathname) {
    result = x_show_uri (pathname);
    if (!result) {
      s_log_message("Check: pathname=%s\n", pathname);
    }
    g_free(pathname);
  }
  EXIT_COMMAND(do_show_wiki);
}
/*! @brief Launch the Help About Dialog */
COMMAND ( do_show_about )
{
  BEGIN_NO_ARGUMENT(do_show_about);
  about_dialog(w_current);
}

/** @} END Group i_command Command Functions */

/* \defgroup i_command_Variable Functions
 *  @{
 */

/** @fn i_cmd_draw_grips in i_command_Variable_Handlers */
COMMAND (draw_grips) {
  SHOW_VARIABLE(draw_grips, R);
}

/** @fn i_cmd_logging in i_command_Variable_Handlers */
COMMAND (logging) {
  SHOW_VARIABLE(logging, G);
}

/** @fn i_cmd_grid_mode in i_command_Variable_Handlers */
COMMAND (grid_mode) {
  SHOW_VARIABLE(grid_mode, W);
}

/** @fn i_cmd_dots_grid_dot_size in i_command_Variable_Handlers */
COMMAND (dots_grid_dot_size) {
  SHOW_VARIABLE(grid_mode, W);
}

/** @fn i_cmd_dots_grid_fixed_threshold in i_command_Variable_Handlers */
COMMAND (dots_grid_fixed_threshold) {

  SHOW_VARIABLE(dots_grid_fixed_threshold, W);
}

/** @fn i_cmd_dots_grid_mode in i_command_Variable_Handlers */
COMMAND (dots_grid_mode) {

  SHOW_VARIABLE(dots_grid_mode, W);
}

/** @fn i_cmd_mesh_grid_threshold in i_command_Variable_Handlers */
COMMAND (mesh_grid_threshold) {

  SHOW_VARIABLE(mesh_grid_threshold, W);
}

/** @fn i_cmd_object_clipping in i_command_Variable_Handlers */
COMMAND (object_clipping) {

  SHOW_VARIABLE(object_clipping, T);
}

/** @fn i_cmd_scrollbars in i_command_Variable_Handlers */
COMMAND (scrollbars) {

  SHOW_VARIABLE(scrollbars, W);
}

/** @fn i_cmd_scrollbar_update in i_command_Variable_Handlers */
COMMAND (scrollbar_update) {

  SHOW_VARIABLE(scrollbar_update, W);
}

/** @fn i_cmd_scrollpan_steps in i_command_Variable_Handlers */
COMMAND (scrollpan_steps) {

  SHOW_VARIABLE(scrollpan_steps, W);
}

/** @fn i_cmd_warp_cursor in i_command_Variable_Handlers */
COMMAND (warp_cursor) {

  SHOW_VARIABLE(warp_cursor, W);
}

/** @fn i_cmd_world_size in i_command_Variable_Handlers */
COMMAND (world_size) {

  int width  = w_current->toplevel->init_right;
  int height = w_current->toplevel->init_bottom;
  s_log_message("(read only width=%d, height=%d\n)", width, height);
}

/** @fn i_cmd_zoom_gain in i_command_Variable_Handlers */
COMMAND (zoom_gain) {

  SHOW_VARIABLE(zoom_gain, W);
}

/** @fn i_cmd_zoom_with_pan in i_command_Variable_Handlers */
COMMAND (zoom_with_pan) {

  SHOW_VARIABLE(zoom_gain, W);
}

/** @fn i_cmd_log_destiny in i_command_Variable_Handlers */
COMMAND (log_destiny) {
  SHOW_VARIABLE(log_destiny, G);
}

/** @fn i_cmd_console_window in i_command_Variable_Handlers */
COMMAND (console_window) {
  SHOW_VARIABLE(console_window, G);
}

/** @fn i_cmd_console_window_type in i_command_Variable_Handlers */
COMMAND (console_window_type) {
  SHOW_VARIABLE(console_window_type, G);
}

/** @fn i_cmd_action_feedback_mode in i_command_Variable_Handlers */
COMMAND (action_feedback_mode) {

  SHOW_VARIABLE(action_feedback_mode, W)
}

/** @fn i_cmd_add_attribute_offset in i_command_Variable_Handlers */
COMMAND (add_attribute_offset) {

  SHOW_VARIABLE(add_attribute_offset, W);
}

/** @fn i_cmd_auto_load_last in i_command_Variable_Handlers */
COMMAND (auto_load_last) {
  SHOW_VARIABLE(auto_load_last, G);
}

/** @fn i_cmd_auto_save_interval in i_command_Variable_Handlers */
COMMAND (auto_save_interval) {

  SHOW_VARIABLE(auto_save_interval, T);
}

/** @fn i_cmd_attribute_placement_grid in i_command_Variable_Handlers */
COMMAND (attribute_placement_grid) {

  SHOW_VARIABLE(attribute_placement_grid, W);
}

/** @fn i_cmd_continue_component_place in i_command_Variable_Handlers */
COMMAND (continue_component_place) {

  SHOW_VARIABLE(continue_component_place, W);
}

/** @fn i_cmd_embed_components in i_command_Variable_Handlers */
COMMAND (embed_components) {

  SHOW_VARIABLE(embed_components, W);
}

/** @fn i_cmd_enforce_hierarchy in i_command_Variable_Handlers */
COMMAND (enforce_hierarchy) {

  SHOW_VARIABLE(enforce_hierarchy, W);
}

/** @fn i_cmdfile_preview in i_command_Variable_Handlers */
COMMAND (file_preview) {

  SHOW_VARIABLE(file_preview, W);
}

/** @fn i_cmd_force_boundingbox in i_command_Variable_Handlers */
COMMAND (force_boundingbox) {

  SHOW_VARIABLE(force_boundingbox, T);
}

/** @fn i_cmd_keyboardpan_gain in i_command_Variable_Handlers */
COMMAND (keyboardpan_gain) {

  SHOW_VARIABLE(keyboardpan_gain, W);
}

/** @fn i_cmd_magnetic_net_mode in i_command_Variable_Handlers */
COMMAND (magnetic_net_mode) {

  SHOW_VARIABLE(magnetic_net_mode, W);
}

/** @fn i_cmd_netconn_rubberband in i_command_Variable_Handlers */
COMMAND (netconn_rubberband) {

  SHOW_VARIABLE(netconn_rubberband, W);
}

/** @fn i_cmd_raise_dialog_boxes in i_command_Variable_Handlers */
COMMAND (raise_dialog_boxes) {

  SHOW_VARIABLE(raise_dialog_boxes, W);
}

/** @fn i_cmd_select_slack_pixels in i_command_Variable_Handlers */
COMMAND (select_slack_pixels) {

  SHOW_VARIABLE(select_slack_pixels, W);
}

/** @fn i_cmd_snap_size in i_command_Variable_Handlers */
COMMAND (snap_size) {

  SHOW_VARIABLE(snap_size, W);
}

/** @fn i_cmd_sort_component_library in i_command_Variable_Handlers */
COMMAND (sort_component_library) {

  SHOW_VARIABLE(sort_component_library, W);
}

/** @fn i_cmd_untitled_name in i_command_Variable_Handlers */
COMMAND(untitled_name) {

  s_log_message("<%s>", w_current->toplevel->untitled_name);
}

/** @fn i_cmd_net_consolidate in i_command_Variable_Handlers */
COMMAND (net_consolidate) {

  SHOW_VARIABLE(net_consolidate, T);
}

/** @fn i_cmd_net_endpoint_mode in i_command_Variable_Handlers */
COMMAND (net_endpoint_mode) {

  SHOW_VARIABLE(net_endpoint_mode, W);
}

/** @fn i_cmd_net_midpoint_mode in i_command_Variable_Handlers */
COMMAND (net_midpoint_mode) {

  SHOW_VARIABLE(net_midpoint_mode, W);
}

/** @fn i_cmd_net_direction_mode in i_command_Variable_Handlers */
COMMAND (net_direction_mode) {

  SHOW_VARIABLE(net_direction_mode,W);
}

/** @fn i_cmd_net_selection_mode in i_command_Variable_Handlers */
COMMAND (net_selection_mode) {

  SHOW_VARIABLE(net_selection_mode, W)
}

/** @fn i_cmd_bus_style in i_command_Variable_Handlers */
COMMAND ( bus_style ) {

  SHOW_VARIABLE(bus_style, T)
}
/** @fn i_cmd_net_style in i_command_Variable_Handlers */
COMMAND ( net_style ) {

  SHOW_VARIABLE(net_style, T)
}
/** @fn i_cmd_pin_style in i_command_Variable_Handlers */
COMMAND ( pin_style ) {

  SHOW_VARIABLE(pin_style, T)
}
/** @fn i_cmd_line_style in i_command_Variable_Handlers */
COMMAND ( line_style ) {

  SHOW_VARIABLE(line_style, T)
}
/** @fn i_cmd_thick_bus_width in i_command_Variable_Handlers */
COMMAND ( thick_bus_width ) {

  SHOW_VARIABLE(thick_bus_width, T)
}
/** @fn i_cmd_thick_line_width in i_command_Variable_Handlers */
COMMAND ( thick_line_width ) {

  SHOW_VARIABLE(thick_line_width, T)
}
/** @fn i_cmd_thick_line_width in i_command_Variable_Handlers */
COMMAND ( thick_net_width ) {

  SHOW_VARIABLE(thick_net_width, T)
}
/** @fn i_cmd_thick_pin_width in i_command_Variable_Handlers */
COMMAND ( thick_pin_width ) {

  SHOW_VARIABLE(thick_pin_width, T)
}
/** @fn i_cmd_thick_line_width in i_command_Variable_Handlers */
COMMAND ( thin_bus_width ) {

  SHOW_VARIABLE(thin_bus_width, T)
}
/** @fn i_cmd_thin_line_width in i_command_Variable_Handlers */
COMMAND ( thin_line_width ) {

  SHOW_VARIABLE(thin_line_width, T)
}
/** @fn i_cmd_thin_net_width in i_command_Variable_Handlers */
COMMAND ( thin_net_width ) {

  SHOW_VARIABLE(thin_net_width, T)
}
/** @fn i_cmd_thin_pin_width in i_command_Variable_Handlers */
COMMAND ( thin_pin_width ) {

  SHOW_VARIABLE(thin_pin_width, T)
}

/** @fn i_cmd_bus_ripper_rotation in i_command_Variable_Handlers */
COMMAND ( bus_ripper_rotation ) {

  SHOW_VARIABLE(bus_ripper_rotation, W)
}

/** @fn i_cmd_bus_ripper_size in i_command_Variable_Handlers */
COMMAND ( bus_ripper_size ) {

  SHOW_VARIABLE(bus_ripper_size, W)
}
/** @fn i_cmd_bus_ripper_type in i_command_Variable_Handlers */
COMMAND ( bus_ripper_type ) {

  SHOW_VARIABLE(bus_ripper_type, W)
}
/** @fn i_cmd_bus_ripper_symname in i_command_Variable_Handlers */
COMMAND(bus_ripper_symname) {

  s_log_message("<%s>", w_current->bus_ripper_symname);
}

/** @fn i_cmd_fast_mousepan in i_command_Variable_Handlers */
COMMAND ( fast_mousepan ) {

  SHOW_VARIABLE(fast_mousepan, W)
}

/** @fn i_cmd_drag_can_move in i_command_Variable_Handlers */
COMMAND ( drag_can_move ) {

  SHOW_VARIABLE(drag_can_move, W)
}

/** @fn i_cmd_middle_button in i_command_Variable_Handlers */
COMMAND ( middle_button ) {

  SHOW_VARIABLE(middle_button, W)
}

/** @fn i_cmd_third_button in i_command_Variable_Handlers */
COMMAND ( third_button ) {

  SHOW_VARIABLE(third_button, W)
}

/** @fn i_cmd_mousepan_gain in i_command_Variable_Handlers */
COMMAND ( mousepan_gain ) {

  SHOW_VARIABLE(mousepan_gain, W)
}

/** @fn i_cmd_scroll_wheel in i_command_Variable_Handlers */
COMMAND ( scroll_wheel ) {

  SHOW_VARIABLE(scroll_wheel, W)
}

/** @fn i_cmd_image_color in i_command_Variable_Handlers */
COMMAND (image_color) {

  SHOW_VARIABLE(image_color, T)
}
/** @fn i_cmd_invert_images in i_command_Variable_Handlers */
COMMAND (invert_images) {

  SHOW_VARIABLE(invert_images, T)
}
/** @fn i_cmd_text_case in i_command_Variable_Handlers */
COMMAND ( text_case ) {

  SHOW_VARIABLE(text_case, W)
}

/** @fn i_cmd_text_display_zoomfactor in i_command_Variable_Handlers */
COMMAND ( text_display_zoomfactor ) {

  SHOW_VARIABLE(text_display_zoomfactor, W)
}

/** @fn i_cmd_text_feedback in i_command_Variable_Handlers */
COMMAND ( text_feedback ) {

  SHOW_VARIABLE(text_feedback, W)
}

/** @fn i_cmd_text_origin_marker in i_command_Variable_Handlers */
COMMAND ( text_origin_marker ) {

  SHOW_VARIABLE(text_origin_marker, R)
}
/** @fn i_cmd_text_marker_size in i_command_Variable_Handlers */
COMMAND ( text_marker_size ) {

  SHOW_VARIABLE(text_marker_size, R)
}
/** @fn i_cmd_text_size in i_command_Variable_Handlers */
COMMAND ( text_size ) {

  SHOW_VARIABLE(text_size, W)
}

/** @fn i_cmd_undo_control in i_command_Variable_Handlers */
COMMAND ( undo_control ) {

  SHOW_VARIABLE(undo_control, W)
}

/** @fn i_cmd_undo_levels in i_command_Variable_Handlers */
COMMAND ( undo_levels ) {

  SHOW_VARIABLE(undo_levels, W)
}

/** @fn i_cmd_undo_panzoom in i_command_Variable_Handlers */
COMMAND ( undo_panzoom ) {

  SHOW_VARIABLE(undo_panzoom, W)
}

/** @fn i_cmd_undo_type in i_command_Variable_Handlers */
COMMAND ( undo_type ) {

  SHOW_VARIABLE(undo_type, W)
}

/** @} END Group i_command_Variable_Handlers */
#undef Renderer
#undef Toplevel
#undef CMD_FUNC
#undef CMD_OPTIONS
#undef CMD_NAME
#undef CMD_INTEGER
#undef COMMAND
#undef WCURRENT