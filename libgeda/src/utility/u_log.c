/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <syslog.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

extern int libgeda_quiet_mode;
extern int libgeda_verbose_mode;

/** \defgroup Libgeda-Logging-Utilities Libgeda Logging Utilities
 *    @{
 */

/*! Default setting for log update callback function. */
static void (*x_log_update_func)() = NULL;

static int is_logging = FALSE; /* Variable to controls whether logging is enable or not */

#define CATCH_LOG_LEVELS (G_LOG_LEVEL_MASK ^ \
                          (G_LOG_LEVEL_DEBUG | G_LOG_LEVEL_INFO))
#define PRINT_LOG_LEVELS (CATCH_LOG_LEVELS ^ \
                          (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_MESSAGE))

#define LOG_OPEN_ATTEMPTS 5
#define LOG_READ_BUFFER_SIZE  200
#define LOG_WRITE_BUFFER_SIZE 1024

static void u_log_handler (const char *log_domain, GLogLevelFlags log_level,
                           const char *message, void *user_data);

static int logfile_fd = -1;
static int log_time   =  1;

static unsigned int log_handler_id;

/*! \brief Get whether log entries are prefixed with the Time Of Day
 *  \par Function Description
 *  Getter function for the static module integer log_time.
 *
 *  \returns the current log-time setting.
 *  \todo Add Scheme API
 */
int geda_utility_log_get_log_time(void)
{
  return log_time;
}

/*! \brief Set whether log entries are prefixed with the Time Of Day
 *  \par Function Description
 *  Setter function for the static module integer log_time.
 *
 *  \param [in] mode If 0 entries will not be prefixed with the TOD
 */
void geda_utility_log_set_log_time(int mode)
{
  log_time = mode;
}

/*! \brief Write a message to the current log file.
 *  \par Function Description
 *  Writes <B>message</B> to the current log file whose file descriptor
 *  is <B>logfile_fd</B>.
 *
 *  It also sends <B>message</B> to the optional function <B>x_log_update</B>
 *  for further use.
 *
 *  \param [in] log_domain  (unused).
 *  \param [in] log_level   (unused).
 *  \param [in] message     Character string containing message to
 *                          write to log.
 *  \param [in] user_data   (unused).
 *
 */
static void u_log_handler (const char    *log_domain,
                           GLogLevelFlags log_level,
                           const char    *message,
                           void          *user_data)
{
  int status;

  g_return_if_fail (logfile_fd != -1);

  if (log_time) {

    char   buffer[LOG_WRITE_BUFFER_SIZE];
    struct tm *nowtm;
    time_t nowt;

    time (&nowt);
    nowtm = localtime (&nowt);

    if (strftime(buffer,LOG_WRITE_BUFFER_SIZE,"[%H:%M:%S]", nowtm) == 0)
    {
      status = write (logfile_fd, message, strlen (message));
    }
    else
    {
      char *log_entry;
      int   len;

      log_entry = strcat(buffer, " ");
      len       = LOG_WRITE_BUFFER_SIZE - strlen (log_entry) - 1;
      log_entry = strncat(buffer, message, len);
      status    = write (logfile_fd, log_entry, strlen (log_entry));
    }
  }
  else {

    status = write (logfile_fd, message, strlen (message));
  }

  if (status == -1) {
    perror("Could not write message to log file\n");
  }

  if ((status == -1) || (log_level & PRINT_LOG_LEVELS)) {

    /* If messages are serious or writing to file failed, call the
     * default handler to write to the console. */
    g_log_default_handler (log_domain, log_level, message, NULL);
  }

  if (x_log_update_func) {
    (*x_log_update_func) (log_domain, log_level, message);
  }
}

/*! \brief Initialize libgeda logging feature.
 *  \par Function Description
 *  This function opens the file <B>filename</B> to log to and registers
 *  the handler to redirect log message to this file.
 *
 *  \param [in] prefix  Character string with file name prefix to log to.
 */
void geda_utility_log_init (const char *prefix)
{
  /* FIXME we assume that the prefix is in the filesystem encoding. */
  GSList *files           = NULL;
  char   *dir_path        = NULL;
  char   *full_prefix     = NULL;
  size_t  full_prefix_len = 0;
  struct  tm *nowtm;
  time_t  nowt;

  /* Somebody called for initialization, therefore */
  is_logging = TRUE;

  if (logfile_fd != -1) {
    BUG_MSG ("Log already initialized.");
    return;
  }

  time (&nowt);
  nowtm = gmtime (&nowt);

  /* create "real" prefix -- this has the form "<prefix>-<date>-" */
  full_prefix = geda_utility_string_sprintf ("%s-%04i%02i%02i-", prefix,
                                  nowtm->tm_year + 1900, nowtm->tm_mon + 1,
                                  nowtm->tm_mday);

  full_prefix_len = strlen (full_prefix);

  /* Find/create the directory where we are going to put the logs. Then
   * iterate over files in the directory finding the "biggest" existing
   * filename with a matching prefix & date. */

  if (default_log_directory) {
    dir_path = default_log_directory;
  }
  else {

    const char *user_dir;

    user_dir = f_path_user_config();
    dir_path = g_build_filename(user_dir, "logs", NULL);
  }

  /* Try to create the directory. */
  if (f_path_create (dir_path, 0777 /*octal*/ ) != 0) {
    /* It is okay to use the logging functions from here, because
     * there is already a default handler. */
    fprintf(stderr, _("Could not create log directory %s: %s\n"), dir_path,
            strerror (errno));
  }
  else {

    GSList *iter;
    int     index;
    int     last_exist_logn;
    int     logcount;

    last_exist_logn = 0;

    files = f_get_dir_list_files(dir_path, "log");

    for (iter = files; iter != NULL; iter = iter->next) {

      const char *file = iter->data;
      int n;

      if (strncmp (full_prefix, file, full_prefix_len)) continue;
      if (sscanf (file + full_prefix_len, "%i", &n) != 1) continue;
      if (n > last_exist_logn) last_exist_logn = n;
    }

    logcount = g_slist_length (files);
    logcount = logcount;

    geda_utility_gslist_free_full (files, g_free);
    files = NULL;

    /* Now try and create a new file. When we fail, increment the number. */
    index = 0;
    while (logfile_fd == -1 && (LOG_OPEN_ATTEMPTS > index++)) {

      char *filename;

      filename = geda_utility_string_sprintf ("%s%s%s%i.log", dir_path,
                                   DIR_SEPARATOR_S, full_prefix,
      ++last_exist_logn);
      logfile_fd = open (filename, O_RDWR|O_CREAT|O_EXCL, 0600);

      GEDA_FREE (filename);

      if (logfile_fd == -1 && (errno != EEXIST)) {
        break;
      }
    }

    if (logfile_fd != -1) {

      /* install the log handler */
      log_handler_id = g_log_set_handler (NULL,
                                          CATCH_LOG_LEVELS,
                                          u_log_handler,
                                          NULL);
    }
    else {

      /* It is okay to use the logging functions from here,
       * because there is already a default handler.
       */
      if (errno == EEXIST) {
        fprintf(stderr, "Could not create unique log filename in %s\n",
                dir_path);
      }
      else {
        fprintf(stderr, "Could not create log file in %s: %s\n", dir_path,
                strerror (errno));
      }
    }
  }

  if (!default_log_directory) {
    GEDA_FREE (dir_path);;
  }

  GEDA_FREE (full_prefix);
}

/*! \brief Terminates the logging of messages.
 *  \par Function Description
 *  This function de-registers the handler for redirection to the log
 *  file and then close the log file. Subsequent messages are lost after
 *  the close.
 */
void geda_utility_log_close (void)
{
  is_logging = FALSE;

  if (logfile_fd == -1) {
    return;
  }

  /* remove the handler */
  g_log_remove_handler (NULL, log_handler_id);

  /* close the file */
  if (logfile_fd != -1) {
    close (logfile_fd);
    logfile_fd = -1;
  }

}

/*! \brief  Set Log callback function.
 *  \par Function Description
 *  Call to set the a handler function to be called for each log
 *  event.
 *
 *  \param [in] func Pointer to callback function
 */
void geda_utility_log_set_update_func (LogUpdateFunc func)
{
    x_log_update_func = func;
}

/*! \brief  Reads the current log file and returns its contents.
 *  \par Function Description
 *  This function reads the current log file and returns its contents.
 *
 *  \return Character string with current log's contents.
 */
char *geda_utility_log_read (void)
{
  bool  tmp;
  int   len;
  int   size;
  char *contents;
  char  buf[LOG_READ_BUFFER_SIZE];

  if (logfile_fd == -1) {
    return NULL;
  }

  tmp        = is_logging;
  is_logging = FALSE;
  contents   = NULL;
  size       = 0;

  /* rewind the file */
  lseek(logfile_fd, 0, SEEK_SET);

  /* read its contents and build a string */
  while ((len = read (logfile_fd, &buf, LOG_READ_BUFFER_SIZE)) != 0) {
    size = size + len;
    if (!contents) {
      contents = (char*)malloc(size + 1);
      strncpy(contents, &buf[0], len);
    }
    else {
      contents = (char*)realloc(contents, size + 1);
      if (!contents)
        break;
      strncat(contents, &buf[0], len);
    }
  }

  is_logging = tmp;

  return contents;
}

/*! \brief Write Message to Log if Not Quiet Mode
 *  \par Function Description
 *  This is a utlitity function to write a formatted message to
 *  the log handler if quiet mode is not set.
 */
void geda_utility_log_quite(const char *format, ...)
{
  if (!libgeda_quiet_mode) {

    va_list args;
    char   *buffer;
    int     size;

    va_start (args, format);
    size = geda_utility_string_strsize(format, args) + 1;
    va_end (args);

    buffer = malloc(size);

    va_start (args, format);
    vsnprintf (buffer, size, format, args);
    va_end (args);

    u_log_message("%s", buffer);

    if (buffer) free(buffer);
  }
}

/*! \brief Write Message to Log if Verbose Mode
 *  \par Function Description
 *  This is a utlitity function to write a formatted message to
 *  the log handler if verbose mode was set.
 */
void geda_utility_log_verbose(const char *format, ...)
{
  if (libgeda_verbose_mode) {

    va_list args;
    char   *buffer;
    int     size;

    va_start (args, format);
    size = geda_utility_string_strsize(format, args) + 1;
    va_end (args);

    buffer = malloc(size);

    va_start (args, format);
    vsnprintf (buffer, size, format, args);
    va_end (args);

    u_log_message("%s", buffer);

    if (buffer) free(buffer);
  }
}

void
geda_utility_log_system(const char *format, ...)
{
  va_list args;
  char   *buffer;
  int     size;
  int     options;

  va_start (args, format);
  size = geda_utility_string_strsize(format, args) + 1;
  va_end (args);

  buffer = malloc(size);

  va_start (args, format);
  vsnprintf (buffer, size, format, args);
  va_end (args);

  options = LOG_CONS | LOG_PID | LOG_NDELAY;

  openlog ("geda", options, LOG_INFO);

  syslog (LOG_INFO, "%s", buffer);

  closelog ();

  if (buffer) free(buffer);

}

/** @} endgroup Libgeda-Logging-Utilities */
