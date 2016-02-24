/* -*- C x_console.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

/*!
 * \file x_console.c
 * \brief A dialog box for command input entry and displays log data
 */

/* Who |   When   |  What (Why)
 * -----------------------------------------------------------------
 * WEH | 09/01/12 |  Apply patch by Gareth Edwards in function x_log_open
 *                   ( to suppress  GLib-GObject-WARNING message.)
 * -----------------------------------------------------------------
 * WEH | 01/01/13 |  Converted old Log Window to a Console Dialog (as
 *                   this seems more usefull and still functions as
 *                   a log window )
 * -----------------------------------------------------------------
 * WEH | 09/28/13 |  Revised q_log_message & v_log_message to accept
 *                   variable number of arguments, (so superfluous info
 *                   is not forced to log window). Revised
 *                   x_console_init_commands (to have dynamic messages).
 * -----------------------------------------------------------------
 * WEH | 06/28/15 |  Retrieve Log & Console Systems variables in function
 *     |          |  x_console_init_commands.
*/

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <errno.h>
#include <ctype.h>

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"
#include <geda/geda_stat.h>
#include <geda_entry.h>
#include <geda_debug.h>

#define MAX_COMMAND_LENGTH 128

static void console_class_init (ConsoleClass *class);
static void console_init       (Console      *console);

static GList     *command_list;
static GList     *command_buffer;
static GtkWidget *console_entry;
static GtkWidget *console_dialog = NULL;

static ConsoleInputMode console_input_mode;

static GObjectClass *console_parent_class = NULL;

static void x_console_callback_response (GtkDialog *dialog, int arg1, void * user_data);
static void log_message (Console *console, const char *message, const char *style);

/** \defgroup Logging-Utilities Logging Utilities
 *    @{
 */
void q_log_message(const char *format, ...)
{
  if(!quiet_mode) {

    char *buffer;

    buffer = malloc(MESSAGE_BUFFER_SIZE);
    va_list args;
    va_start (args, format);
      vsnprintf (buffer, MESSAGE_BUFFER_SIZE, format, args);
    va_end (args);
    u_log_message("%s", buffer);
    if (buffer) free(buffer);
  }
}

void v_log_message(const char *format, ...)
{
  if(verbose_mode) {

    char *buffer;

    buffer = malloc(MESSAGE_BUFFER_SIZE);
    va_list args;
    va_start (args, format);
      vsnprintf (buffer, MESSAGE_BUFFER_SIZE, format, args);
    va_end (args);
    u_log_message("gschem: %s", buffer);
    if (buffer) free(buffer);
  }
}
/** @} endgroup Logging-Utilities */

/** \defgroup Console-Dialog-Implementation Console Dialog Implementation
 *    @{
 *  \ingroup (Console-Dialog Systemic-Dialogs)
 */

/*!
 *  \brief Destroy Command Buffer
 *
 *  \par Function Description
 *  We elected to create the GList *command_buffer in this module, rather
 * than in main, so we don't have to externally reference, but this means
 * we also have to free it (or not really since the only time this function
 * is used is when the main line exits and all memory is freed by the OS.
 *
 *  If the Console dialog instance does exist, present it to the user.
 */
void x_console_destroy_command_buffer(void * user_data) {
  if (command_buffer) {
      v_log_message(_("destroying history\n"));
      g_list_foreach(command_buffer, (GFunc)g_free, NULL);
      g_list_free (command_buffer);
      command_buffer = NULL; /* This is not optional */
  }
}

void x_console_init_commands(GschemToplevel *w_current, int mode) {

  inline void i_setv_rc(volatile int *var, int rc) { if (rc != RC_NIL) *var = rc; };

  v_log_message(_("Initializing Log & Console Systems configuration settings\n"));

  EdaConfig    *cfg   = eda_config_get_user_context ();
  const char   *group = LOG_CONFIG_GROUP;

 /*! \internal Retrieve Log & Console Systems variables
   * \par
   *   Retrieve the log settings. The log settings are retrieved early during
   *   startup, before the main variables.
   *
   * \note 1. RC Initialization files must have been processed prior to calling
   *          this function.
   *       2. Log & Console variables are saved in x_settings_save_settings().
   */
  i_var_restore_group_integer (cfg, group, "logging",     (int*)&logging, TRUE);
  i_var_restore_group_integer (cfg, group, "log-destiny", (int*)&log_destiny, CONSOLE_WINDOW);

  group = IDS_CONSOLE;

  i_var_restore_group_integer (cfg, group, "console-window",     (int*)&console_window, MAP_ON_STARTUP);
  i_var_restore_group_integer (cfg, group, "console-window-type", (int*)&console_window_type, DECORATED);

  i_setv_rc (&logging,   default_logging);
  i_setv_rc (&log_destiny,   default_log_destiny);
  i_setv_rc (&console_window,   default_console_window);
  i_setv_rc (&console_window_type,   default_console_window_type);

  command_buffer = NULL;

#ifdef HAVE_GTHREAD

  char* describe_level[] = {  "safe mode",
                              "multitasking mode",
                              "unknown"
                           };

  unsigned int nlevel = G_N_ELEMENTS (describe_level);

  if (mode > 1) {
    mode = mode - 1;
    nlevel = mode > nlevel ? nlevel - 1 : mode;
    i_command_engage(w_current);
    v_log_message(_("Command interface: engaged using (%s)\n"), describe_level[mode]);
  }
  else {
    v_log_message(_("Command interface: engaged using (%s)\n"), describe_level[0]);
    i_command_disengage(FALSE, FALSE);
  }
#endif

}

/*! ====================== Console Dialog-Handlers ==================== */

/** \defgroup Console-Dialog-Handlers Console Dialog Support Functions
 *  @{
 */

/*!
 *  \brief Open the Console window
 *
 *  \par Function Description
 *  If the Console dialog instance doesn't exist, create it, and read
 *  the current log file contents (if they exist) and insert them
 *  into the console dialog.
 *
 *  If the Console dialog instance does exist, present it to the user.
 */
/* 12/08/10 Gareth Edwards <gareth@edwardsfamily.org.uk>  added
 * / GtkWindow / "type", GTK_WINDOW_TOPLEVEL, to suppress errors:
 * "gschem:62319): GLib-GObject-WARNING **: g_object_set_valist:
 * construct property "type" for object `Log' can't be set after
 * construction"
 * 01/01/13 Wiley E. Hill.
 * Added "gschem-toplevel" construct property so we could inherit
 * pointer to top-level variables.
 * 08/31/13 Replaced g_assert with conditional to shown dialog only
 * if is our console type instead of crashing entire program.
 * 09/26/13 Added parent property to dialog and change static
 * setting string to IDS_defined in sdefines.h
 * 09/29/13 Removed parent property, it makes the dialog annoying
 * because it stays above the main window.
 */
void x_console_open (GschemToplevel *w_current)
{

  if (console_dialog == NULL) {
    char *contents;
    console_dialog = GTK_WIDGET (g_object_new (TYPE_CONSOLE,
                                           "type", GTK_WINDOW_TOPLEVEL,
                                           "settings-name", IDS_CONSOLE,
                                           "gschem-toplevel", w_current,
                                           NULL));

    g_signal_connect (console_dialog,
                      "response",
                      G_CALLBACK (x_console_callback_response),
                      NULL);

    /* make it read the content of the current log file */
    /* and add its contents to the dialog */
    contents = geda_utility_log_read ();

    /* geda_utility_log_read can return NULL if the log file cannot be written to */
    if (contents == NULL) {
      return;
    }

    log_message (CONSOLE (console_dialog), contents, "old");
    GEDA_FREE (contents);

    geda_utility_log_set_update_func(x_log_message);

    if ( auto_place_mode ) {
      gtk_widget_set_uposition ( console_dialog, 10, 10);
    }
    gtk_widget_show (console_dialog);
  }
  else {
    if (IS_CONSOLE (console_dialog))
      gtk_window_present ((GtkWindow*)console_dialog);
    else
      BUG_MSG("Object is not a console\n");
  }

}

/*!
 *  \brief Close the Log window
 *  \par Function Description
 *  If the log window exists, destroy it.
 */
void x_console_close ()
{
  if (console_dialog) {
    if (IS_CONSOLE (console_dialog)) {
      gtk_widget_destroy (console_dialog);
      geda_utility_log_set_update_func(NULL);
      console_dialog = NULL;
    }
    else
      BUG_MSG("object is not a console\n");
  }
}

/*!
 *  \brief Console Window Callback Function
 *
 *  \par Function Description
 *  Callback function for the Console window. Only used to close the window.
 */
static void x_console_callback_response (GtkDialog *dialog, int arg1,
                                         void      *user_data)
{
  switch (arg1) {
    case GEDA_RESPONSE_DELETE_EVENT:
    case GEDA_RESPONSE_CLOSE:
    x_console_close ();
    break;
    default:
      BUG_MSG("unhandled case\n");
  }
}

/*! ===================== Console Logging-Handlers ==================== */

/** \defgroup Logging-Handlers Console Dialog Logging Functions
 *  @{
 */

/*!
 *  \brief Add a message to the Console Log window
 *
 *  \par Function Description
 *  \param [in] console The console instance
 *  \param [in] message The message to be logged
 *  \param [in] style   The style to use in the text rendering
 */
static void log_message (Console *console, const char *message, const char *style)
{
  GtkTextBuffer *buffer;
  GtkTextIter iter;
  GtkTextMark *mark;

  g_return_if_fail (IS_CONSOLE (console));

  buffer = gtk_text_view_get_buffer (console->textview);
  gtk_text_buffer_get_end_iter (buffer, &iter);
  /* Apply the "plain" tag before the level-specific tag in order to
   * reset the formatting */

  if (g_utf8_validate (message, -1, NULL)) {
    gtk_text_buffer_insert_with_tags_by_name (buffer, &iter, message, -1,
                                              "plain", style, NULL);
  }
  else {
    /* If UTF-8 wasn't valid (due to a system locale encoded filename or
     * other string being included by mistake), log a warning, and print
     * the original message to stderr, where it may be partly intelligible */
    gtk_text_buffer_insert_with_tags_by_name (buffer, &iter,
      _("** Invalid UTF-8 in log message. See stderr or gschem.log.\n"),
                                              -1, "plain", style, NULL);
    if(verbose_mode) {
      fprintf (stderr, "gschem: %s", message);
    }
    else {
      fprintf (stderr, "%s", message);
    }
  }

  mark = gtk_text_buffer_create_mark(buffer, NULL, &iter, FALSE);
  gtk_text_view_scroll_to_mark (console->textview, mark, 0, TRUE, 0, 1);
  gtk_text_buffer_delete_mark (buffer, mark);
}

/*!
 *  \brief Add a message to the Console Window
 *  \par Function Description
 *  This is notify function for the logging systems, the actual logging
 *  is performed by libgeda. This function was registered as a callback
 *  to echo messages to the Console window. This function sets the style
 *  based on the \a log_level flag and calls log_message() to do display
 *  the actual \a message.
 *
 *  \param [in] log_domain
 *  \param [in] log_level The severity of the message
 *  \param [in] message   The message to be displayed
 */
void x_log_message (const char *log_domain, GLogLevelFlags log_level, const char *message)
{
  char *style;

  g_return_if_fail (console_dialog != NULL);

  if (log_level & (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_ERROR)) {
    style = "critical";
  }
  else if (log_level & G_LOG_LEVEL_WARNING) {
    style = "warning";
  }
  else {
    style = "message";
  }

  log_message (CONSOLE(console_dialog), message, style);
}

/** @} end group Logging-Handlers */
/** @} end group Console-Dialog-Handlers */
/** @} endgroup Console-Dialog-Implementation */

/*! ============ Console-Class Initializers & Constructors ============ */

/** \defgroup Console-Dialog-Class Console Dialog Class
 *  @{
 *  \ingroup (Console-Dialog)
 *  \par
 *  Definition of the Console dialog Class. The Console Dialog Class is
 *  derived from #GschemDialogClass.
 */

/*!
 *  \brief Get the Console class type
 *
 *  \par Function Description
 *
 * On first call, registers the Console class with the GedaType dynamic
 * type system. On subsequent calls, returns the saved value from first
 * execution.
 *
 * \returns GedaType identifier for the Console class
 */
GedaType console_get_type ()
{
  static GedaType console_type = 0;

  if (!console_type) {
    static const GTypeInfo console_info = {
      sizeof(ConsoleClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) console_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Console),
      0,    /* n_preallocs */
      (GInstanceInitFunc) console_init,
    };

    console_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                           "Console",
                                           &console_info, 0);
  }

  return console_type;
}

void x_console_eval_command (GedaEntry *entry, int arg1, void * user_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (console_dialog)->w_current;

  char *ptr;

  char command_line[MAX_COMMAND_LENGTH];

  char *get_str_token(char* cl) {
    char *e_ptr, *s_ptr;
    s_ptr =  e_ptr = cl;
    while ( *e_ptr != ASCII_NUL) ++e_ptr;
    while (  s_ptr != e_ptr) if (*s_ptr == ASCII_SPACE) break; else ++s_ptr;
    if (s_ptr == e_ptr) return geda_utility_string_strdup(cl);
    return g_strndup(cl, s_ptr - cl );
  }

  ptr = strcpy (command_line, GetEntryText(entry));

  while (*ptr == ASCII_SPACE) ++ptr;
  if (ptr != command_line)
    ptr = strcpy(command_line, ptr);

  if (*ptr == ASCII_OP) {
    SCM interpreter = scm_list_2(scm_from_utf8_symbol("invoke-macro"),
                                 scm_from_utf8_string(command_line));
    scm_dynwind_begin (0);
    g_dynwind_window (w_current);
    g_scm_eval_protected(interpreter, SCM_UNDEFINED);
    scm_dynwind_end ();
  }
  else {

    char *command;
    char *command_echo;

    command = get_str_token(command_line);
    command_echo = geda_utility_string_concat(command_line, "\n", NULL);
    x_log_message ("console", G_LOG_LEVEL_INFO, command_echo);
    GEDA_FREE (command_echo);

    if (i_command_is_valid(command)) {
      i_command_process(w_current, command, geda_utility_string_word_count(command_line),
                        command_line, ID_ORIGIN_CONSOLE);
    }
    else {
      if (strlen(command)) {
        u_log_message("Unknown command: \"%s\"\n", command );
      }
    }
    GEDA_FREE (command);
  }
  SetEntryText( entry, "");
}

static void x_console_on_activate (GedaEntry *entry, int arg1, void * user_data)
{

  if(console_input_mode == CONSOLE_COMMAND_MODE)
    x_console_eval_command (entry, arg1, user_data);
}

static const char *x_console_get_input_data(void) {
  const char *string;

  console_input_mode = CONSOLE_INPUT_MODE;

  string = geda_utility_string_strdup(GetEntryText(console_entry));

  gtk_widget_grab_focus(console_entry);

  /* reset the console to command mode*/
  SetEntryText ( console_entry, "" );
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);

  console_input_mode = CONSOLE_COMMAND_MODE;
  return string;
}

const char *x_console_get_alphanumeric(void) {
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALPHANUMERIC);
  return x_console_get_input_data();
}
const char *x_console_get_numeric(void) {
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_NUMERIC);
  return x_console_get_input_data();
}
int x_console_get_number(void) {
  char *string;
  int   result;
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_NUMBER);
  string = (char*) x_console_get_input_data();
  result = atoi(string);
  GEDA_FREE(string);
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);
  return result;
}
int x_console_get_integer(void) {
  char *string;
  int   result;
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_INTEGER);
  string = (char*) x_console_get_input_data();
  result = atoi(string);
  GEDA_FREE(string);
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);
  return result;
}
float x_console_get_real(void) {
  char *string;
  float result;
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_REAL);
  string = (char*) x_console_get_input_data();
  result = atof(string);
  GEDA_FREE(string);
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);
  return result;
}
const char *x_console_get_string() {
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);
  return x_console_get_input_data();
}

/*!
 *  \brief Console class initialization function
 *
 *  \par Function Description
 *
 * Class initialization function for the Console class. Currently
 * does nothing.
 */

static void console_class_init (ConsoleClass *klass)
{
  console_parent_class = g_type_class_peek_parent (klass);
}

/*!
 *  \brief Console class instance initialization function
 *
 *  \par Function Description
 *
 * Instance initialization for the Console class. This function sets up
 * the Console Dialog. A scrollable text view is created for log output
 * and highlighting parameters are configured. Below the scroll window,
 * a Custom Entry box is setup to receive "typed" command user input.
 *  and a Close button.
 *
 * \param console the instance of the class to initialize
 */
static void console_init (Console *console) /* *Self */
{
  GtkWidget*      scrolled_win;
  GtkWidget*      text_view;
  GtkTextBuffer*  text_buffer;

  GtkEntryBuffer* command_entry_buffer;
  GtkWidget*      console_box;

  GtkTextMark*    mark;

  bool            decorate   = (console_window_type == DECORATED);

  /* dialog initialization */
  g_object_set (G_OBJECT (console),
                /* GtkContainer */
                "border-width",    0,
                "title",           _("gEDA Console"),
                "default-width",   600,
                "default-height",  200,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                /* GtkDialog */
                "has-separator",   decorate,
                 NULL);

  /* create a scrolled window for the textview */
  scrolled_win = GTK_WIDGET (g_object_new  (GTK_TYPE_SCROLLED_WINDOW,
                                            /* GtkContainer */
                                            "border-width",      1,
                                            /* GtkScrolledWindow */
                                            "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                            "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                            "shadow-type",       GTK_SHADOW_ETCHED_IN,
                                            NULL));

  /* create the text buffer */
  text_buffer = GTK_TEXT_BUFFER (g_object_new (GTK_TYPE_TEXT_BUFFER, NULL));

  /* Add some tags for highlighting log messages to the buffer */
  gtk_text_buffer_create_tag (text_buffer,    "plain",
                             "foreground",    "black",
                             "foreground-set", TRUE,
                             "weight",         PANGO_WEIGHT_NORMAL,
                             "weight-set",     TRUE,
                              NULL);

  /* The default "message" style is plain */
  gtk_text_buffer_create_tag (text_buffer,    "message", NULL);

  /* "old" messages are in dark grey */
  gtk_text_buffer_create_tag (text_buffer,    "old",
                             "foreground",    "#404040",
                             "foreground-set", TRUE,
                              NULL);

  /* "warning" messages are printed in red */
  gtk_text_buffer_create_tag (text_buffer,    "warning",
                             "foreground",    "red",
                             "foreground-set", TRUE,
                              NULL);

  /* "critical" messages are bold red */
  gtk_text_buffer_create_tag (text_buffer,    "critical",
                             "foreground",    "red",
                             "foreground-set", TRUE,
                             "weight",         PANGO_WEIGHT_BOLD,
                             "weight-set",     TRUE,
                              NULL);

  /* create the text view and attach the buffer to it */
  text_view = GTK_WIDGET (g_object_new (GTK_TYPE_TEXT_VIEW, /* GtkTextView */
                                       "cursor-visible", FALSE,
                                       "editable",       FALSE,
                                       "left_margin",    7,
                                        NULL));

  gtk_text_view_set_buffer (GTK_TEXT_VIEW (text_view), text_buffer);

  /* add the text view to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), text_view);

  /* set textview of console */
  console->textview = GTK_TEXT_VIEW (text_view);

  console_box   = gtk_vbox_new (FALSE, 0);

/*! \note: Note: command_buffer is a GLIST of text the user typed
 * in, aka command history. command_entry_buffer is a gtk text entry
 * buffer embedded in the GTK Entry control/widget. Our custom Entry
 * does not directly interact with the command_entry. A pointer to
 * the command_buffer list is passed as an argument to the Our Entry
 * control so that we retain command history between instances...
*/
  /* if 1st time using global buffer add to the geda_atexit */
  if (!command_buffer)
    geda_atexit(x_console_destroy_command_buffer, NULL);

/*! \note:command list is an extended version of the action list, and
 * includes all the RC variables, the glist list is passed to our
 * custom entry widget for the command completion feature
*/
  i_command_get_command_list(&command_list);

  /* Instantiate one our Custom Entry Widgets */
  /* Glib-2.40 generates console noise from gtk-lib */
  console_entry = geda_entry_new(&command_buffer, &command_list);

  geda_entry_completion_set_case((GedaEntry*) console_entry, FALSE);

  if(console_window_type == DECORATED)
    gtk_entry_set_has_frame((GtkEntry*)console_entry, TRUE);
  else
    gtk_entry_set_has_frame((GtkEntry*)console_entry, FALSE);

  /* create the command entry buffer */
  command_entry_buffer = gtk_entry_buffer_new(NULL, -1);
  gtk_entry_set_buffer((GtkEntry*) console_entry, command_entry_buffer);
  gtk_entry_buffer_set_max_length (command_entry_buffer, MAX_COMMAND_LENGTH);

  console_input_mode = CONSOLE_COMMAND_MODE;

  g_signal_connect (console_entry,
                   "process-entry",
                    G_CALLBACK (x_console_on_activate), /*x_console_eval_command*/
                    NULL);

  /* Now glue everthing together */
  GtkWidget *align1 = gtk_alignment_new (0, 1, 1, 1);
  GtkWidget *align2 = gtk_alignment_new (0, 1, 1, 0);

  gtk_container_add           (GTK_CONTAINER (align1), scrolled_win);
  gtk_container_add           (GTK_CONTAINER (align2), console_entry);

  gtk_alignment_set_padding   (GTK_ALIGNMENT (align1) ,0, 0, 0, 0);
  gtk_alignment_set_padding   (GTK_ALIGNMENT (align2) ,0, 0, 7, 7);

  gtk_box_pack_start (GTK_BOX (console_box), align1, TRUE,  TRUE, 0);
  gtk_box_pack_start (GTK_BOX (console_box), align2, FALSE,  FALSE, 0);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (console)->vbox), console_box,
                               TRUE, TRUE, 0);

  gtk_widget_show_all         (console_box);
  /* now add the close button to the action area */
  /* Glib-2.40 generates console noise from gtk-lib */
  gtk_dialog_add_button       (GTK_DIALOG (console),
                               GTK_STOCK_CLOSE, GEDA_RESPONSE_CLOSE);

  /* scroll to the end of the buffer */
  mark = gtk_text_buffer_get_insert (text_buffer);
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (text_view), mark, 0.0, TRUE, 0.0, 1.0);

  gtk_widget_grab_focus(console_entry); /* Not the Close the Button */
}

/** @} end group Console-Dialog-Class */
