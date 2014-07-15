/* gEDA - GPL Electronic Design Automation
 *
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
 *
 * Date: July 12, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the GTK Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 */
/*! \file geda_input_dialog.c This module contains generic dialog routines
 *  for geda projects.
 */
#include <gtk/gtk.h>
#include <geda.h>
#include <geda_label.h>

/* \note The returned string must be freed */
char *geda_dialog_get_string(const char *title, const char *prompt)
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

    entry = gtk_entry_new();
    gtk_container_add(GTK_CONTAINER(content_area), entry);
    g_object_set (entry, "visible", TRUE, NULL);

    gtk_widget_show(GTK_WIDGET(dialog));

    gtk_window_set_focus(GTK_WINDOW(dialog), entry);
    gtk_widget_set_can_default(ok_butt,TRUE);
    gtk_window_set_default(GTK_WINDOW(dialog), ok_butt);
    gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);

    switch (gtk_dialog_run(dialog))
    {
    case 0:
        text = NULL;
        break;

    case 1:
        text = g_strdup(gtk_entry_get_text(GTK_ENTRY(entry)));
        break;

    default:
        fprintf(stderr, "yep'em, Gtk sure sucks!\n");
        break;
    }

    gtk_widget_destroy((GtkWidget*)dialog);
    return text;
}