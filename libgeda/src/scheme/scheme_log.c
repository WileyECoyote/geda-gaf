/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2016  Peter Brett <peter@peter-b.co.uk>
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

/*!
 * \file scheme_log.c
 * \brief Scheme API logging support
 */

#include "../../../config.h"

#include "../../include/libgeda_priv.h"
#include "../../include/libgedaguile_priv.h"

SCM_SYMBOL(error_sym,    "error");
SCM_SYMBOL(critical_sym, "critical");
SCM_SYMBOL(warning_sym,  "warning");
SCM_SYMBOL(message_sym,  "message");
SCM_SYMBOL(info_sym,     "info");
SCM_SYMBOL(debug_sym,    "debug");


/*!
 * \brief Convert a Scheme symbol to log level flags.
 * \par Function Description
 *  Helper function to construct a GLogLevelFlags value from a Scheme
 *  symbol.
 */
static GLogLevelFlags decode_level (SCM level_s)
{
    if (scm_is_eq (level_s, error_sym))    return (G_LOG_LEVEL_ERROR | G_LOG_FLAG_FATAL);
    if (level_s == critical_sym) return G_LOG_LEVEL_CRITICAL;
    if (level_s == warning_sym)  return G_LOG_LEVEL_WARNING;
    if (level_s == message_sym)  return G_LOG_LEVEL_MESSAGE;
    if (level_s == info_sym)     return G_LOG_LEVEL_INFO;
    if (level_s == debug_sym)    return G_LOG_LEVEL_DEBUG;

    g_return_val_if_reached(G_LOG_LEVEL_MESSAGE);
}

/* ================================================================
 * Functions for use from Scheme
 * ================================================================ */

/*!
 * \brief Open Log file Scheme API provider
 * \par Function Description
 *  Initializes libgeda logging to a file. Silently ignore request to
 *  re-initialize. Log messages are not saved to a log file unless
 *  log-open has been called prior to writing the maseage to the log.
 *
 * \note Scheme API: Implements the \%log-open! procedure in the (geda
 *       core log) module.
 *
 * param prefix_s Optional log entry prefix.
 *
 * \return undefined.
 */
EDA_SCM_DEFINE (log_open_x, "%log-open", 0, 1, 0,
               (SCM prefix_s),
               "Initialize libgeda logging system")
{
  char *prefix;

  if (scm_is_string(prefix_s)) {
    prefix = scm_to_utf8_string(prefix_s);
  }
  else {
    prefix = NULL;
  }

  geda_utility_log_init(prefix);

  return SCM_UNSPECIFIED;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
EDA_SCM_DEFINE (log_close_x, "%log-close", 0, 0, 0,
               (),
               "Shuts down the libgeda logging system")
{
   geda_utility_log_close();
   return SCM_UNSPECIFIED;
}

/*!
 * \brief Log a message Scheme API provider
 * \par Function Description
 *  Add a message to the message log. The \a domain_s should normally
 *  be SCM_BOOL_F, and the \a message_s should almost always be
 *  translated for all log levels other than "debug". The \a level_s
 *  should be one of the symbols "error", "critical", "message", "info"
 *  "warning" or "debug".
 *
 * \note Scheme API: Implements the \%log! procedure in the (geda core
 *       log) module.
 *
 * param domain_s  The log domain, as a string, or SCM_BOOL_F.
 * param level_s   The log level, as a symbol.
 * param message_s The log message, as a string.
 *
 * \return undefined.
 */
EDA_SCM_DEFINE (log_x, "%log!", 3, 0, 0,
               (SCM domain_s, SCM level_s, SCM message_s),
               "Emit a log message.")
{
    SCM_ASSERT (scm_is_false(domain_s) || scm_is_string(domain_s), domain_s,
                SCM_ARG1, scheme_log_x);
    SCM_ASSERT (scm_is_symbol(level_s), level_s,
                SCM_ARG2, scheme_log_x);
    SCM_ASSERT (scm_is_string(message_s), message_s,
                SCM_ARG3, scheme_log_x);

    char *domain = NULL;
    char *message;

    scm_dynwind_begin(0);

    if (domain) {
        scm_to_utf8_string(domain_s);
        scm_dynwind_free(domain);
    }

    message = scm_to_utf8_string(message_s);

    scm_dynwind_free(message);

    GLogLevelFlags level = decode_level(level_s);

    g_log(domain, level, "%s", message);

    scm_dynwind_end();

    return SCM_UNSPECIFIED;
}

/*!
 * \brief Retrieve the Log.
 * \par Function Description
 *  Returns a scheme string of the log.
 *
 * \note Scheme API: Implements the %log-read procedure in the (geda
 *       core log) module.
 *
 * \return string array.
 */
EDA_SCM_DEFINE (log_read_x, "%log-read", 0, 0, 0, (),
               "Get log contents as string.")
{
  SCM value_s;

  scm_dynwind_begin (0);

  char *string = geda_utility_log_read ();

  if (string) {

    char *str;

    scm_dynwind_unwind_handler ((void (*)(void *)) geda_free, string,
                                SCM_F_WIND_EXPLICITLY);

    str = geda_utility_string_get_valid_utf8 (string);
    value_s = scm_from_utf8_string (str);

    geda_free(str);
  }
  else {
    value_s = SCM_BOOL_F;
  }

  scm_dynwind_end ();

  return value_s;
}

/* ================================================================
 * Initialization
 * ================================================================ */

/*!
 * \brief Create the (geda core log) Scheme module.
 * \par Function Description
 *  Defines procedures in the (geda core log) module.  The module can
 *  be accessed using (use-modules (geda core log)).
 */
static void
init_module_geda_core_log ()
{
    /* Register the functions and symbols */
    #include "scheme_log.x"

    /* Add them to the module's public definitions */
    scm_c_export (scheme_log_x,
                  scheme_log_close_x,
                  scheme_log_open_x,
                  scheme_log_read_x,
                  NULL);
}

/*!
 * \brief Initialize the basic gEDA logging procedures
 * \par Function Description
 *  Registers some core Scheme procedures for logging support. Should
 *  only be called by edascm_init().
 */
void edascm_init_log (void)
{
    /* Define the (geda core log) module */
    scm_c_define_module ("geda core log",
                         init_module_geda_core_log,
                         NULL);
}
