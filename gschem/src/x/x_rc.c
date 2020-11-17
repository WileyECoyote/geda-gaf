/* -*- C x_rc.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_rc.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: October 17th, 2014
 * Contributing Author: Wiley Edward Hill
 *
 */
/*!
 * \file x_rc.c
 * \brief Module for interfacing with Library initialization routines
 */

#include <gschem.h>

#include <geda_debug.h>

/*! \brief This Function is used to Recover from Unbound Variables.
 *  \par Function Description
 *  This function is not normally executed. If an unbound variable is
 *  encountered while processing an initialization file, this function
 *  is assigned to that variable. What the function does is not so much
 *  important as it's existent. When the same initialization file is
 *  reprocessed, the variable is no longer unbound. If the would be
 *  variable had been defined like (typo "enabled") or (type 1), then
 *  cadr is returned, otherwise <b>UNDEFINED</b> is returned.
 *
 */
static
SCM g_rc_unbound_handler (SCM rst)
{
  SCM  s_value;
  SCM  r_value;

  if (scm_list_p(rst)) {
    s_value = SCM_CAR(rst);
    r_value = s_value;
  }
  else
    r_value = SCM_UNDEFINED;

  return r_value;
}

/*! \brief Error handler function used by x_rc_parse_gschem.
 * \par Function Description
 *  The file is called from functions in Libgeda, after an error was
 *  encountered processing an RC initialization file. If a valid err
 *  pointer is passed the error is interrogated. Missing RC files are
 *  not reported. For other errors, a descriptive message is logged
 *  and a dialog is displayed to inform the user of the error. If the
 *  error did not include a message then the error is reported as
 *  unknown. If the error was due to an  "Unbound variable", then a
 *  scheme subroutine is assigned to the string name and the integer
 *  pointed to by the second argument is incremented to indicated
 *  that another attempt should be made to process the RC file.
 *
 * \param err         A point to a ponter to a Gerror structure.
 * \param retry_flag  Pointer to current retry count.
 */
static void
x_rc_parse_gschem_error (GError **err, void *retry_flag)
{
  const char *dialog_message  = _("<b>Error Processing Configuration.</b>");
  const char *dialog_title    = _("gschem RC File Error");
  const char *err_msg_unknown = _("An unknown error occurred while parsing configuration files.");
  const char *msg_log_more    = _("The log may contain more information.");
  char       *msg2;             /* Secondary message text */

  if (err == NULL) {
    BUG_MSG("err is NULL.");
    return;
  }

  /* Take no chances; if err was not set for some reason, it's a
   * problem. */
  if (*err == NULL) {

    /* Log message */
     geda_log ("%s: %s\n", _("ERROR"), err_msg_unknown);

    /* Dialog message */
    msg2 = geda_strconcat ( err_msg_unknown, "\n\n", msg_log_more, NULL);
  }
  else {

    const char *unbound_msg;      /* As received Unbound */
    const char *unbound_needle  = "Unbound variable:";
    int len;

    /* Config files are allowed to be missing or skipped; check for this. */
    if (g_error_matches (*err, EDA_ERROR, ENOENT) ||
        g_error_matches (*err, EDA_ERROR, EDA_ERROR_RC_TWICE)) {
      return;
    }

    /* Check if this was an "Unbound variable:" message */
    unbound_msg = geda_utility_string_istr ((*err)->message, unbound_needle);

    if (unbound_msg != NULL) {
      unbound_msg = unbound_msg + 18;
      len = strlen(unbound_msg);
    }
    else {
      len = 0;
    }

    if (len > 0) { /* True if geda_string_istr found "Unbound variable:" */

      char *unbound_sym;      /* Unbound Symbol name */

      int  *iptr  = (int*)retry_flag;
      int   retry = *iptr;

      /* Guild added a carriage return character to the end of the
       * message, so get rid of it! */
      unbound_sym = geda_strndup (unbound_msg, len - 1);

      /* Define a dummy routine using this name */
      scm_c_define_gsubr (unbound_sym, 0, 1, 1, g_rc_unbound_handler);
      *iptr = ++retry;
      GEDA_FREE(unbound_sym);
    }
    else  /* Log message */ {
       geda_log ("%s: %s\n", _("ERROR"), (*err)->message);
    }
    /* Dialog message */
    msg2 = geda_sprintf ("%s\n\n%s", (*err)->message, msg_log_more);
  }

  /* Inform the user */
  titled_pango_error_dialog ( dialog_message, msg2, dialog_title );

  GEDA_FREE (msg2);
}

#define ConfigParseFunc void (*parser)(const char*, ConfigParseErrorFunc, void *)

/*! \brief Load gschem configuration files and display error dialogs.
 * \par Function Description
 *  Calls functions in Libgeda to process RC initialization files using
 *  previous function, x_rc_parse_gschem_error as an error handler. The
 *  function passes a pointer to an integer as the last parameter, and
 *  if that integer is incremented then a recoverable error occurred,
 *  and the call to process the RC file is repeated until either the
 *  integer is not incremented, meaning an error did not occur, or the
 *  number of attempts equals MAX_RC_ATTEMPTS, defined in idefines.h.
 *  Currently the limit is set to make 3 attempts, which applies to
 *  each type, gafrc, gschemrc, and rcfile, separately, not in total.
 *
 * \param w_current  The current #GschemToplevel structure.
 * \param rcfile     Specific config file path, or NULL.
 */
void
x_rc_parse_gschem (GschemToplevel *w_current, const char *rcfile) {

  int index;
  int retry;

  struct {
    ConfigParseFunc;
    const char *arg;
  } rc_processors[] = {{ g_rc_parse_gafrc_handler,   NULL      },
                       { g_rc_parse_rcname_handler, "gschemrc" },
                       { g_rc_parse_rcfile_handler,  rcfile    }};

  for ( index = 0; index < G_N_ELEMENTS(rc_processors); index++)
  {
    int attempt;

    attempt = -1;
    retry   =  0;

    do {
      rc_processors[index].parser (rc_processors[index].arg,
                                   x_rc_parse_gschem_error, &retry);
      attempt++;
    } while (attempt < retry && attempt < MAX_RC_ATTEMPTS);
  }
}

#undef ConfigParseFunc
