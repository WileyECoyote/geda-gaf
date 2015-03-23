/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_input_dialog.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
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
/*! \file geda_input_dialog.h This module contains generic dialog routines
 *  for geda projects.
 */

#include <stdlib.h>

#include <gtk/gtk.h>

//#include <geda.h>
#include <libgeda/libgeda.h>

#include "geda_label.h"
#include "geda_entry.h"

static char*
geda_dialog_get_input(const char *title, const char *prompt, const char *str, GedaEntryAccept type)
{
    GtkDialog *dialog;
    GtkWidget *ok_butt;
    GtkWidget *entry;
    GtkWidget *label;
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
      label = geda_visible_label_new(prompt);
      gtk_container_add(GTK_CONTAINER(content_area), label);
    }

    entry = geda_visible_entry_new (DISABLE, DISABLE);
    geda_entry_set_valid_input(GEDA_ENTRY(entry), type);
    gtk_container_add(GTK_CONTAINER(content_area), entry);

    if (str) {
      gtk_entry_set_text((GtkEntry*)entry, str);
    }

    gtk_widget_show(GTK_WIDGET(dialog));

    gtk_window_set_focus(GTK_WINDOW(dialog), entry);
    gtk_widget_set_can_default(ok_butt,TRUE);
    gtk_window_set_default(GTK_WINDOW(dialog), ok_butt);
    geda_entry_set_activates_default(GEDA_ENTRY(entry), TRUE);

    switch (gtk_dialog_run(dialog))
    {
    case 0:
        text = NULL;
        break;

    case 1:
        text = g_strdup(gtk_entry_get_text(GTK_ENTRY(entry)));
        break;

    default:
        break;
    }

    gtk_widget_destroy((GtkWidget*)dialog);
    return text;
}

int geda_dialog_get_integer(const char *title, const char *prompt, int offer)
{
  char *string;
  char *text;
  float value;

  if (offer != -0) {
    string = u_string_sprintf("%d", offer);
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

float geda_dialog_get_real(const char *title, const char *prompt, float offer)
{
  char *string;
  char *tail;
  char *text;
  float value;

  if (offer != -0) {
    string = tail = u_string_sprintf("%f", offer);
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

/* \note The returned string must be freed */
char *geda_dialog_get_string(const char *title, const char *prompt, const char *string)
{
  return geda_dialog_get_input(title, prompt, string, ACCEPT_ALL_ASCII);
}
