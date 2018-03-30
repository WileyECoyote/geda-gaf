/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_input_dialog.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2017 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License.
 *
 * This Library is distributed in the hope that it will be useful,
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: July 12, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
/**
 * \brief GedaInputDialogs - Dialogs for User Input
 * \par
 *  This module contains routines for general purpose input dialogs.
 *
 * \defgroup GedaInputDialogs General Purpose Dialogs for User Input
 * @{
 * \par
 *  The static function geda_dialog_get_input creates and displays the
 *  actual dialog and returns the user input via strings. Publicly
 *  accessible functions are used to pass the required type and other
 *  data to geda_dialog_get_input. These accessible functions convert
 *  the returned string to the desired data type.
 *
 *  The publicly accessible functions are:
 *
 *      1. geda_dialog_get_integer
 *      2. geda_dialog_get_real
 *      3. geda_dialog_get_string
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <stdlib.h>

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include "../../include/geda_container.h"
#include "../../include/geda_label.h"
#include "../../include/geda_entry.h"

static char *geda_dialog_get_input(const char *title,
                                   const char *prompt,
                                   const char *str,
                                   GedaEntryAccept type)
{
    GtkDialog *dialog;
    GtkWidget *ok_butt;
    GtkWidget *entry;
    GtkWidget *content_area;
    char      *text = NULL;

    dialog  = (GtkDialog*)gtk_dialog_new();
    ok_butt = gtk_dialog_add_button(dialog, "OK", 1);
    gtk_dialog_add_button(dialog, "CANCEL", 0);

    if (title) {
      gtk_window_set_title(GTK_WINDOW(dialog), title);
    }

    content_area = gtk_dialog_get_content_area(dialog);

    if (prompt) {
      GtkWidget *label = geda_visible_label_new(prompt);
      geda_container_add(content_area, label);
    }

    entry = geda_entry_new_visible ();
    geda_entry_set_valid_input((GedaEntry*)entry, type);
    geda_container_add(content_area, entry);

    if (str) {
      geda_entry_widget_set_text(entry, str);
    }

    gtk_widget_show(GTK_WIDGET(dialog));

    gtk_window_set_focus(GTK_WINDOW(dialog), entry);
    gtk_widget_set_can_default(ok_butt,TRUE);
    gtk_window_set_default(GTK_WINDOW(dialog), ok_butt);
    geda_entry_set_activates_default((GedaEntry*)entry, TRUE);

    switch (gtk_dialog_run(dialog)) {

      case 0:
        text = NULL;
        break;

      case 1:
        text = geda_strdup(geda_entry_widget_get_text(entry));
        break;

      default:
        break;
    }

    gtk_widget_destroy((GtkWidget*)dialog);
    return text;
}

/*!
 * \brief Get an Integer using a Dialog Input Field
 * \par Function Description
 *  Displays a GedaInputDialog and prompts the user to input
 *  in integer values.
 *
 * \param [in] title  String to be displayed in the title bar or NULL
 * \param [in] prompt String to be displayed for input prompt or NULL
 * \param [in] offer  Default integer value or -0
 *
 * \returns the integer value or -0 if the user canceled the dialog.
 *
 * \image html geda_dialog_get_integer.png
 * \image latex geda_dialog_get_integer.png
 */
int geda_dialog_get_integer(const char *title, const char *prompt, int offer)
{
  char *string;
  char *text;
  float value;

  if (offer != -0) {
    string = geda_sprintf("%d", offer);
  }
  else {
    string = NULL;
  }

  text = geda_dialog_get_input(title, prompt, string, ACCEPT_INTEGER);

  if (text) { /* If user did not cancel */
    value = atoi(text);
  }
  else {
    value = -0;
  }

  GEDA_FREE(string);
  GEDA_FREE(text);

  return value;
}

/*!
 * \brief Get a Real number using a Dialog Input Field
 * \par Function Description
 *  Displays a GedaInputDialog and prompts the user to input
 *  in floating point value.
 *
 * \param [in] title  String to be displayed in the title bar or NULL
 * \param [in] prompt String to be displayed for input prompt or NULL
 * \param [in] offer  Default floating value or -0
 *
 * \returns the floating value or -0 if the user canceled the dialog.
 */
float geda_dialog_get_real(const char *title, const char *prompt, float offer)
{
  char *string;
  char *tail;
  char *text;
  float value;

  if (offer != -0) {
    string = tail = geda_sprintf("%f", offer);
    while (*tail) {
      if (*tail == ASCII_DIGIT_ZERO) {
        tail++;
        *tail = '\0';
      }
      else{
        ++tail;
      }
    }
  }
  else {
    string = NULL;
  }

  text = geda_dialog_get_input(title, prompt, string, ACCEPT_REAL);

  if (text) { /* If user did not cancel */
    value = atof(text);
  }
  else {
    value = -0;
  }

  GEDA_FREE(string);
  GEDA_FREE(text);

  return value;
}

/*!
 * \brief Get a Real number using a Dialog Input Field
 * \par Function Description
 *  Displays a GedaInputDialog and prompts the user to input
 *  in string value.
 *
 * \param [in] title  String to be displayed in the title bar or NULL
 * \param [in] prompt String to be displayed for input prompt or NULL
 * \param [in] string Default string or NULL
 *
 * \returns the string or NULL if the user canceled the dialog.
 *
 * \note The returned string should be release using g_free.
 */
char *geda_dialog_get_string(const char *title, const char *prompt, const char *string)
{
  return geda_dialog_get_input(title, prompt, string, ACCEPT_ALL_ASCII);
}

/** @} end group GedaInputDialogs */
