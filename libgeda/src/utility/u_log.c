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

#include <geda_standard.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include "libgeda_priv.h"

#include <geda_debug.h>

/*! Default setting for log update callback function. */
void (*x_log_update_func)() = NULL;

int is_logging = FALSE; /* Variable to controls whether logging is enable or not */

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

static unsigned int log_handler_id;

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
  char    buffer[LOG_WRITE_BUFFER_SIZE];
  char   *log_entry;
  int     len;
  int     status;
  struct  tm *nowtm;
  time_t  nowt;

  g_return_if_fail (logfile_fd != -1);

  time (&nowt);
  nowtm = localtime (&nowt);

  if(strftime(buffer,LOG_WRITE_BUFFER_SIZE,"[%H:%M:%S]", nowtm) == 0) {
    perror("Could not format time string");
    status = write (logfile_fd, message, strlen (message));
  }
  else {

    log_entry = strcat(buffer, " ");
    len = LOG_WRITE_BUFFER_SIZE - strlen (log_entry) - 1;

    log_entry = strncat(buffer, message, len);

    status = write (logfile_fd, log_entry, strlen (log_entry));
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
 *  This function opens the file <B>filename</B> to log to and registers the
 *  handler to redirect log message to this file.
 *
 *  \param [in] prefix  Character string with file name prefix to log to.
 */
void u_log_init (const char *prefix)
{
  /* FIXME we assume that the prefix is in the filesystem encoding. */
  GSList    *files           = NULL;
  char      *dir_path        = NULL;
  char      *filename        = NULL;
  char      *full_prefix     = NULL;
  size_t     full_prefix_len = 0;
  struct     tm *nowtm;
  time_t     nowt;

  int last_exist_logn;
  int logcount;
  int i;

  /* Somebody called for initialization, therefore */
  is_logging = TRUE;

  if (logfile_fd != -1) {
    BUG_MSG ("u_log_init: Log already initialised.");
    return;
  }

  time (&nowt);
  nowtm = gmtime (&nowt);

  /* create "real" prefix -- this has the form "<prefix>-<date>-" */
  full_prefix = u_string_sprintf ("%s-%04i%02i%02i-", prefix,
                                  nowtm->tm_year + 1900, nowtm->tm_mon + 1,
                                  nowtm->tm_mday);
  full_prefix_len = strlen (full_prefix);

  /* Find/create the directory where we are going to put the logs.
   * FIXME should this be configured somehow? WEH:Yes it should!
   *
   * Then run through it finding the "biggest" existing filename with
   * a matching prefix & date. */
  dir_path = g_build_filename (f_path_user_config (), "logs", NULL);

  /* Try to create the directory. */
  if (f_path_create (dir_path, 0777 /*octal*/ ) != 0) {
    /* It is okay to use the logging functions from here, because
     * there is already a default handler. */
    fprintf(stderr, _("Could not create log directory %s: %s\n"), dir_path,
            strerror (errno));
  }
  else {

    GSList *iter;

    last_exist_logn = 0;

    files = f_get_dir_list_files(dir_path, "log");

    for ( iter = files; iter != NULL; iter = iter->next) {

      const char *file = iter->data;
      int n;

      if (strncmp (full_prefix, file, full_prefix_len)) continue;
      if (sscanf (file + full_prefix_len, "%i", &n) != 1) continue;
      if (n > last_exist_logn) last_exist_logn = n;
    }

    logcount = g_slist_length (files);
    logcount = logcount;                 /* stub */
    u_gslist_free_full (files, g_free);
    files = NULL;

    /* Now try and create a new file. When we fail, increment the number. */
    i = 0;
    while (logfile_fd == -1 && (LOG_OPEN_ATTEMPTS > i++)) {
      filename = u_string_sprintf ("%s%s%s%i.log", dir_path,
                                  DIR_SEPARATOR_S, full_prefix,
      ++last_exist_logn);
      logfile_fd = open (filename, O_RDWR|O_CREAT|O_EXCL, 0600);

      if (logfile_fd == -1 && (errno != EEXIST)) break;
    }

    if (logfile_fd != -1) {

      /* install the log handler */
      log_handler_id = g_log_set_handler (NULL,
                                          CATCH_LOG_LEVELS,
                                          u_log_handler,
                                          NULL);

    }
    else {
      /* It's okay to use the logging functions from here, because
       * there's already a default handler. */
      if (errno == EEXIST) {
        fprintf(stderr, "Could not create unique log filename in %s\n",
                dir_path);
      }
      else {
        fprintf(stderr, "Could not create log file in %s: %s\n", dir_path,
                strerror (errno));
      }
    }

    GEDA_FREE (filename);
  }
  GEDA_FREE (dir_path);
  GEDA_FREE (full_prefix);
}

/*! \brief Terminates the logging of messages.
 *  \par Function Description
 *  This function deregisters the handler for redirection to the log file
 *  and closes it.
 */
void u_log_close (void)
{
  is_logging = FALSE; /* subsequent messages are lost after the close */

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

/*! \brief  Reads the current log file and returns its contents.
 *  \par Function Description
 *  This function reads the current log file and returns its contents.
 *
 *  \return Character string with current log's contents.
 *
 */
char *u_log_read (void)
{
  bool tmp;

  char buf[LOG_READ_BUFFER_SIZE];
  GString *contents;
  int len;

  if (logfile_fd == -1) {
    return NULL;
  }

  tmp = is_logging;
  is_logging = FALSE;

  /* rewind the file */
  lseek(logfile_fd, 0, SEEK_SET);

  /* read its contents and build a string */
  contents = g_string_new ("");
  while ((len = read (logfile_fd, &buf, LOG_READ_BUFFER_SIZE)) != 0) {
    contents = g_string_append_len (contents, buf, len);
  }

  is_logging = tmp;

  return g_string_free (contents, FALSE);
}
