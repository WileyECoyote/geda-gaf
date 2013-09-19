/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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

/*
 * \file x_log.c
 * \brief GType class and functions to support the gschem log window.
 */

/* Who |   When   |  What (Why)
 * -----------------------------------------------------------------
 * WEH | 09/01/12 |  Apply patch by Gareth Edwards in function x_log_open
 *                   ( to suppress  GLib-GObject-WARNING message.)
 * -----------------------------------------------------------------
 * WEH | 01/01/13 |  Converted old Log Window to a Console Dialog (as
 *                   this seems more usefull and still functions as
 *                   a log window )
*/

#include <config.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
#include <ctype.h>

#include <ascii.h>
#include "gschem.h"
#include "x_dialog.h"

#define max_command_length 128

#include "widgets/geda_entry.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static void console_class_init (ConsoleClass *class);
static void console_init       (Console      *console);

static GList     *command_list;
static GList     *command_buffer;
static GtkWidget *console_entry;
static GtkWidget *console_dialog = NULL;

static ConsoleInputMode console_input_mode;

static GObjectClass *console_parent_class = NULL;

static void x_console_callback_response (GtkDialog *dialog, int arg1, gpointer user_data);
static void log_message (Console *console, const char *message, const char *style);

void q_log_message(const char *message){ if(!quiet_mode)  s_log_message(message); }
void v_log_message(const char *message){ if(verbose_mode) s_log_message(message); }

/*!
 *  \brief Destroy Command Buffer
 *
 *  \par Function Description
 *  We elected to create the GList *command_buffer in this module, rather
 * than in main, so we don't have to externally reference, but this means
 * we also have to free it (or not realy since the only time this function
 * is used is when the main line exits and all memory is freed by the OS.
 *
 *  If the Console dialog instance does exist, present it to the user.
 */
void x_console_destroy_command_buffer(gpointer user_data) {
  if (command_buffer) {
      fprintf(stderr, "destroying history\n");
      g_list_foreach(command_buffer, (GFunc)g_free, NULL);
      g_list_free (command_buffer);
      command_buffer=NULL; /* This is not optional */
  }
}
void x_console_init_commands(GSCHEM_TOPLEVEL *w_current, int mode) {
  command_buffer=NULL;

#ifdef HAVE_GTHREAD
  if (mode > 1) {
    i_command_engage(w_current);
    v_log_message("Command interface is running in multitasking mode");
  }
  else
#endif
    i_command_disengage(FALSE, FALSE);
}
/*! ====================== @section Dialog-Handlers ==================== */

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
 */
void x_console_open (GSCHEM_TOPLEVEL *w_current)
{
  if (console_dialog == NULL) {
    char *contents;
    console_dialog = GTK_WIDGET (g_object_new (TYPE_CONSOLE,
                                           /* GtkWindow */
                                           "type", GTK_WINDOW_TOPLEVEL,
                                           /* GschemDialog */
                                           "settings-name", "console",
                                           "gschem-toplevel", w_current,
                                           NULL));

    gtk_window_set_destroy_with_parent (GTK_WINDOW(console_dialog), TRUE);

    g_signal_connect (console_dialog,
                      "response",
                      G_CALLBACK (x_console_callback_response),
                      NULL);

    /* make it read the content of the current log file */
    /* and add its contents to the dialog */
    contents = s_log_read ();

    /* s_log_read can return NULL if the log file cannot be written to */
    if (contents == NULL)
    {
      return;
    }

    log_message (CONSOLE (console_dialog), contents, "old");
    g_free (contents);

    x_log_update_func = x_log_message;

    if( auto_place_mode )
      gtk_widget_set_uposition ( console_dialog, 10, 10);
    gtk_widget_show (console_dialog);
  }
  else {
    if (IS_CONSOLE (console_dialog))
      gtk_window_present ((GtkWindow*)console_dialog);
    else
      s_log_message("Internal Error: <x_console_open> object is not a console\n");
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
      x_log_update_func = NULL;
      console_dialog = NULL;
    }
    else
      s_log_message("Internal Error: <x_console_close> object is not a console\n");
  }
}

/*!
 *  \brief Console Window Callback Function
 *
 *  \par Function Description
 *  Callback function for the Console window. Only used to close the window.
 */
static void x_console_callback_response (GtkDialog *dialog, int arg1,
                                         gpointer user_data)
{
  switch (arg1) {
    case GTK_RESPONSE_DELETE_EVENT:
    case CONSOLE_RESPONSE_CLOSE:
    x_console_close ();
    break;
    default:
      g_critical("Internal Error: <x_console_callback_response> unhandled case\n");
  }
}

/*! ====================== @section Logging-Handlers ==================== */

/*!
 *  \brief Add a message to the Console Window
 *  \par Function Description
 *  Add a message to the Console window.
 *  Calls log_message() to do the actual logging.
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
  } else if (log_level & G_LOG_LEVEL_WARNING) {
    style = "warning";
  } else {
    style = "message";
  }

  log_message (CONSOLE(console_dialog), message, style);
}

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
  } else {
    /* If UTF-8 wasn't valid (due to a system locale encoded filename or
     * other string being included by mistake), log a warning, and print
     * the original message to stderr, where it may be partly intelligible */
    gtk_text_buffer_insert_with_tags_by_name (buffer, &iter,
      _("** Invalid UTF-8 in log message. See stderr or gschem.log.\n"),
                                              -1, "plain", style, NULL);
    fprintf (stderr, "%s", message);
  }

  mark = gtk_text_buffer_create_mark(buffer, NULL, &iter, FALSE);
  gtk_text_view_scroll_to_mark (console->textview, mark, 0, TRUE, 0, 1);
  gtk_text_buffer_delete_mark (buffer, mark);
}

/*! ========= @section Console-Class Initializers & Constructors ======== */
/*!
 *  \brief Get the Console class type
 *
 *  \par Function Description
 *
 * On first call, registers the Console class with the GType dynamic type system.
 * On subsequent calls, returns the saved value from first execution.
 * \returns the type identifier for the Console class
 */
GType console_get_type ()
{
  static GType console_type = 0;

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

void x_console_eval_command (GedaEntry *entry, int arg1, gpointer user_data)
{
  char *command;
  char *command_echo;
  char *ptr;

  GSCHEM_TOPLEVEL *w_current = GSCHEM_DIALOG (console_dialog)->w_current;

  char  command_line[max_command_length];

  char *get_str_token(char* cl) {
    char *e_ptr, *s_ptr;
    s_ptr =  e_ptr = cl;
    while ( *e_ptr != ASCII_NUL) ++e_ptr;
    while (  s_ptr != e_ptr) if (*s_ptr == ASCII_SPACE) break; else ++s_ptr;
    if (s_ptr == e_ptr) return g_strdup(cl);
    return g_strndup(cl, s_ptr - cl );
  }

  ptr = strcpy (command_line, gtk_entry_get_text (GTK_ENTRY(entry)));
  while ( *ptr == ASCII_SPACE) ++ptr;
  if (ptr != command_line)
    ptr = strcpy(command_line, ptr);
  if ( *ptr == ASCII_OP)
    s_log_message("is scm \"%s\"\n", command_line );
  else {
    command = get_str_token(command_line);
    command_echo = g_strconcat(command_line, "\n", NULL);
    x_log_message ("console", G_LOG_LEVEL_INFO, command_echo);
    g_free (command_echo);

    if (i_command_is_valid(command)) {
      i_command_process(w_current, command, word_count(command_line),
                        command_line, ID_ORIGIN_CONSOLE);
    }
    else
      s_log_message("Unknown command: \"%s\"\n", command );

    g_free (command);
  }
  gtk_entry_set_text (GTK_ENTRY(entry), "");
}

static void x_console_on_activate (GedaEntry *entry, int arg1, gpointer user_data)
{
  if(console_input_mode == CONSOLE_COMMAND_MODE)
    x_console_eval_command (entry, arg1, user_data);
}

static const char *x_console_get_input_data(void) {
  const char *string;

  console_input_mode = CONSOLE_INPUT_MODE;

  string = g_strdup(gtk_entry_get_text (GTK_ENTRY(console_entry)));

  gtk_widget_grab_focus(console_entry);

  /* reset the console to command mode*/
  gtk_entry_set_text (GTK_ENTRY(console_entry), "");
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
  g_free(string);
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);
  return result;
}
int x_console_get_integer(void) {
  char *string;
  int   result;
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_INTEGER);
  string = (char*) x_console_get_input_data();
  result = atoi(string);
  g_free(string);
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_ALL_ASCII);
  return result;
}
float x_console_get_real(void) {
  char *string;
  float result;
  geda_entry_set_valid_input((GedaEntry*)console_entry, ACCEPT_REAL);
  string = (char*) x_console_get_input_data();
  result = atof(string);
  g_free(string);
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
 * \param log the instance of the class to initialize
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
  scrolled_win = GTK_WIDGET (
  g_object_new  (GTK_TYPE_SCROLLED_WINDOW,
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

/*! \remarks: Note: command_buffer is a GLIST of text the user typed
 * in, aka command history. command_entry_buffer is a gtk text entry
 * buffer embedded in the GTK Entry control/widget. Our custom Entry
 * does not directly interact with the command_entry. A pointer to
 * the command_buffer list is passed as an argument to the Our Entry
 * control so that we retain command history between instances...
*/
  /* if 1st time using global buffer add to the geda_atexit */
  if (!command_buffer)
    geda_atexit(x_console_destroy_command_buffer, NULL);

  i_command_get_command_list(&command_list);

  /* Instantiate one our Custom Entry Widgets */
  console_entry = geda_entry_new(&command_buffer, &command_list);

  geda_entry_completion_set_case((GedaEntry*) console_entry, FALSE);

  if(console_window_type == DECORATED)
    gtk_entry_set_has_frame((GtkEntry*)console_entry, TRUE);
  else
    gtk_entry_set_has_frame((GtkEntry*)console_entry, FALSE);

  /* create the command entry buffer */
  command_entry_buffer = gtk_entry_buffer_new(NULL, -1);
  gtk_entry_set_buffer((GtkEntry*) console_entry, command_entry_buffer);
  gtk_entry_buffer_set_max_length (command_entry_buffer, max_command_length);

  console_input_mode = CONSOLE_COMMAND_MODE;

  g_signal_connect (console_entry,
                   "activate",
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
  gtk_dialog_add_button       (GTK_DIALOG (console),
                               GTK_STOCK_CLOSE, CONSOLE_RESPONSE_CLOSE);

  /* scroll to the end of the buffer */
  mark = gtk_text_buffer_get_insert (text_buffer);
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (text_view), mark, 0.0, TRUE, 0.0, 1.0);

  gtk_widget_grab_focus(console_entry); /* Not the Close the Button */
}
