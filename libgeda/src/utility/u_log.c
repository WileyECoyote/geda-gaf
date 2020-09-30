/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License.
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
#include "../../../config.h"
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif

#include <libgeda_priv.h>

#include <geda_debug.h>

/* These are initialized by parse_args() */
static int quiet_mode;
static int verbose_mode;

/** \defgroup Libgeda-Logging-Utilities Libgeda Logging Utilities
 *    @{
 */

/*! Default setting for log update callback function. */
static void (*log_update_func)() = NULL;

static int is_logging = FALSE; /* Variable to controls whether logging is enable or not */

#define CATCH_LOG_LEVELS (G_LOG_LEVEL_MASK ^ \
                          (G_LOG_LEVEL_DEBUG | G_LOG_LEVEL_INFO))
#define PRINT_LOG_LEVELS (CATCH_LOG_LEVELS ^ \
                          (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_MESSAGE))

#define LOG_OPEN_ATTEMPTS 50
#define LOG_READ_BUFFER_SIZE  200
#define LOG_WRITE_BUFFER_SIZE 1024

static void u_log_handler (const char *log_domain, GLogLevelFlags log_level,
                           const char *message, void *user_data);

static int logfile_fd = -1;
static int log_time   =  1;

static unsigned int log_handler_id;

/*!
 * \brief Write a message to the current log file.
 * \par Function Description
 *  Writes <B>message</B> to the current log file whose file descriptor
 *  is <B>logfile_fd</B>.
 *
 *  It also sends <B>message</B> to the optional function <B>x_log_update</B>
 *  for further use.
 *
 * \param [in] log_domain  (unused).
 * \param [in] log_level   (unused).
 * \param [in] message     Character string containing message to
 *                          write to log.
 * \param [in] user_data   (unused).
 */
static void u_log_handler (const char    *log_domain,
                           GLogLevelFlags log_level,
                           const char    *message,
                           void          *user_data)
{
  int status;

  if (logfile_fd == -1) {
    printf ("%s\n", message);
    return;
  }

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
    perror(_("Could not write message to log file\n"));
  }

  if ((status == -1) || (log_level & PRINT_LOG_LEVELS)) {

    /* If messages are serious or writing to file failed, call the
     * default handler to write to the console. */
    g_log_default_handler (log_domain, log_level, message, NULL);
  }

  if (log_update_func) {
    (*log_update_func) (log_domain, log_level, message);
  }
}

/*!
 * \brief Terminates the logging of messages.
 * \par Function Description
 *  This function de-registers the handler for redirection to the log
 *  file and then closes the log file. Subsequent messages are lost
 *  after the closure.
 */
void geda_utility_log_close (void)
{
  is_logging = FALSE;

  if (logfile_fd == -1) {
    return;
  }

  /* remove the handler */
  if (log_handler_id) {
    g_log_remove_handler (NULL, log_handler_id);
  }

  /* close the file */
  if (logfile_fd != -1) {
    close (logfile_fd);
    logfile_fd = -1;
  }
}

/*!
 * \brief Get whether log entries are prefixed with the Time Of Day
 * \par Function Description
 *  Getter function for the static module integer log_time.
 *
 * \returns the current log-time setting.
 * \todo Add Scheme API
 */
int geda_utility_log_get_log_time(void)
{
  return log_time;
}

/*!
 * \brief Get active Quiet mode setting
 * \par Function Description
 *  Getter function for the static module integer quiet_mode.
 *
 * \returns the current quiet_mode setting.
 * \todo Add Scheme API
 */
int geda_utility_log_get_quiet_mode(void)
{
  return quiet_mode;
}

/*!
 * \brief Get active Verbose mode setting
 * \par Function Description
 *  Getter function for the static module integer verbose_mode.
 *
 * \returns the current verbose_mode setting.
 * \todo Add Scheme API
 */
int geda_utility_log_get_verbose_mode(void)
{
  return verbose_mode;
}

/*!
 * \brief Initialize libgeda logging feature.
 * \par Function Description
 *  This function opens the file <B>filename</B> to log to and registers
 *  the handler to redirect log message to this file.
 *
 * \param [in] prefix  Character string with file name prefix to log to.
 */
void geda_utility_log_init (const char *prefix)
{
  GSList *files           = NULL;
  char   *dir_path        = NULL;
  char   *full_prefix     = NULL;
  size_t  full_prefix_len = 0;
  struct  tm *nowtm;
  time_t  nowt;

  /* Somebody called for initialization, therefore */
  is_logging     = TRUE;

  if (logfile_fd != -1) {
    return;
  }

  log_handler_id = 0;

  time (&nowt);
  nowtm = gmtime (&nowt);

  if (!prefix) {
    prefix = g_get_application_name ();
  }

  if (!prefix) {
    /* create "real" prefix -- this has the form "<date>-" */
    full_prefix = geda_sprintf ("geda-%04i%02i%02i-",
                                nowtm->tm_year + 1900, nowtm->tm_mon + 1,
                                nowtm->tm_mday);
  }
  else {
    /* create "real" prefix with the form "<prefix>-<date>-" */
    full_prefix = geda_sprintf ("%s-%04i%02i%02i-", prefix,
                                 nowtm->tm_year + 1900, nowtm->tm_mon + 1,
                                 nowtm->tm_mday);
  }

  full_prefix_len = strlen (full_prefix);

  /* Find/create the directory where we are going to put the logs. Then
   * iterate over files in the directory finding the "biggest" existing
   * filename with a matching prefix & date. It is okay to use the logging
   * functions from here, because there is already a default handler.
   */
  if (default_log_directory) {

    if (g_file_test(default_log_directory, G_FILE_TEST_IS_DIR)) {

      int status;

      status = geda_file_sys_ckmod(default_log_directory, 0600);

      if (status == NO_ERROR) {

        dir_path = default_log_directory;
      }
    }
    else if (geda_create_path (default_log_directory, 0764) != 0) {

      const char *msg = _("Could not create log directory");

      fprintf(stderr, "%s %s: %s\n", msg, default_log_directory, strerror (errno));
    }
    else {
      dir_path = default_log_directory;
    }
  }

  /* Check if need to fallback */
  if (!dir_path) {

    const char *user_dir;
    int status;

    user_dir = geda_user_cache_path();
    dir_path = g_build_filename(user_dir, "logs", NULL);

    if (g_file_test(dir_path, G_FILE_TEST_IS_DIR)) {

      status = geda_file_sys_ckmod(dir_path, 0600);

    }
    else if (geda_create_path (dir_path, 0764) != 0) {

      const char *msg = _("Could not create log directory");

      status = errno;

      fprintf(stderr, "%s %s: %s\n", msg, dir_path, strerror (errno));
    }
    else {

      status = NO_ERROR;
    }

    if (status != NO_ERROR) {
      GEDA_FREE (dir_path);
    }
  }

  if (dir_path) {

    GError *err;
    GSList *iter;
    int     index;
    int     last_exist_logn;
    int     logcount;

    err = NULL;
    last_exist_logn = 0;
    files = geda_get_dir_list(dir_path, "log", &err);

    if (err != NULL) {
        /* Should never occur since path was checked above,
         * but glib will squawk if was set and not cleared */
        g_error_free(err);
    }

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

      filename = geda_sprintf ("%s%s%s%i.log", dir_path, DIR_SEPARATOR_S,
                                               full_prefix,
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
        const char *msg = _("Could not create unique log filename in");
        fprintf(stderr, "%s %s\n", msg, dir_path);
      }
      else {
        const char *msg = _("Could not create log file in");
        fprintf(stderr, "%s  %s: %s\n", msg, dir_path, strerror (errno));
      }
    }
  }

  if (!default_log_directory) {
    GEDA_FREE (dir_path);;
  }

  GEDA_FREE (full_prefix);
}

/*!
 * \brief Write Message to Log if Not Quiet Mode
 * \par Function Description
 *  This is a utlitity function to write a formatted message to
 *  the log handler if quiet mode is not set.
 */
void geda_utility_log_quite(const char *format, ...)
{
  if (!quiet_mode) {

    if (format != NULL) {

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

      g_log (0, G_LOG_LEVEL_MESSAGE, "%s", buffer);

      if (buffer)
        free(buffer);
    }
    else {
      BUG_MSG("format can not be NULL");
    }
  }
}

/*!
 * \brief  Reads the current log file and returns its contents.
 * \par Function Description
 *  This function reads the current log file and returns its contents.
 *
 * \return Character string with current log's contents.
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
      char *buffer = (char*)realloc(contents, size + 1);
      if (!buffer)
        break;
      contents = buffer;
      strncat(contents, &buf[0], len);
    }
  }

  is_logging = tmp;

  return contents;
}

/*!
 * \brief Set the default log handler
 * \par Function Description
 *  Installs a default log handler which is used if no log handler has been
 *  set for the particular log domain and log level combination. By default,
 *  Libgeda uses u_log_handler() as the default log handler but this can be
 *  over-ridden by calling this function and specifying \a log_func as the
 *  default handler with an optional pointer to \a user_data. Either argument
 *  can be NULL, when both are NULL u_log_handler is set the default handler.
 *
 * \param [in] log_func The default log handler function to use or NULL
 * \param [in] user_data Pointer to date to be passed to the log handler
 *
 * \returns the previous log handler function.
 */
LogFunc geda_utility_log_set_default_handler (LogFunc log_func, void *user_data)
{
  GLogFunc old_func;

  if (log_func) {
    old_func = g_log_set_default_handler (log_func, user_data);
  }
  else {
    old_func = g_log_set_default_handler (u_log_handler, user_data);
  }
  return (LogFunc)old_func;
}

/*!
 * \brief Set whether log entries are prefixed with the Time Of Day
 * \par Function Description
 *  Setter function for the static module integer log_time.
 *
 * \param [in] mode If 0 entries will not be prefixed with the TOD
 */
void geda_utility_log_set_log_time(int mode)
{
  log_time = mode;
}

/*!
 * \brief Set Quite mode
 * \par Function Description
 *  Setter function for the static module integer quiet_mode.
 *
 * \param [in] mode new quite mode value
 */
void geda_utility_log_set_quiet_mode (int mode)
{
  quiet_mode = mode;
}

/*!
 * \brief  Set Log callback function.
 * \par Function Description
 *  Call to set the a handler function to be called for each log
 *  event.
 *
 * \param [in] func Pointer to callback function
 */
void geda_utility_log_set_update_func (LogUpdateFunc func)
{
    log_update_func = func;
}

/*!
 * \brief Set Verbose mode
 * \par Function Description
 *  Setter function for the static module integer verbose_mode.
 *
 * \param [in] mode new verbose mode value
 */
void geda_utility_log_set_verbose_mode (int mode)
{
  verbose_mode = mode;
}

/*!
 * \brief Write Message to the System Log
 * \par Function Description
 *  This is a utility function to write a formatted message to
 *  the system log.
 *
 * \param [in] format String containing format specifiers
 * \param [in] ...    NULL terminated list strings.
 */
void geda_utility_log_system(const char *format, ...)
{
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

#ifdef HAVE_SYSLOG_H

  openlog ("geda", LOG_CONS | LOG_PID | LOG_NDELAY, LOG_INFO);

  syslog (LOG_INFO, "%s", buffer);

  closelog ();

#else /* Likely Win32 platform */

  u_log_handler (NULL, G_LOG_LEVEL_INFO, buffer, NULL);

#endif

  if (buffer) free(buffer);

}

/*!
 * \brief Write Message to Log if Verbose Mode
 * \par Function Description
 *  This is a utility function to write a formatted message to
 *  the log handler if verbose mode was set.
 *
 * \param [in] format String containing format specifiers
 * \param [in] ...    NULL terminated list strings.
 */
void geda_utility_log_verbose(const char *format, ...)
{
  if (verbose_mode) {

    if (format != NULL) {

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

      g_log (0, G_LOG_LEVEL_MESSAGE, "%s", buffer);

      if (buffer)
        free(buffer);
    }
    else {
      BUG_MSG("format can not be NULL");
    }
  }
}

/** @} endgroup Libgeda-Logging-Utilities */
