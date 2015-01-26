/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2015 Ales Hvezda
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */


#ifndef __GSCHEM_DIALOG_H__
#define __GSCHEM_DIALOG_H__

/* Date: Aug 12, 2012
 * Who:  Wiley E. Hill
 * What  MACRO: NEW_GSCHEM_DIALOG
 * Why:  This Macro facilitate creating new Dialog Boxes.
*/
#define NEW_STD_GSCHEM_DIALOG(title, identifier, ptrWindowWidget) \
      gschem_dialog_new_empty(title, GTK_WINDOW(w_current->main_window), \
                                     GTK_DIALOG_MODAL, identifier, ptrWindowWidget)

/* Date: Aug 12, 2012
 * Who:  Wiley E. Hill
 * What  MACRO: NEW_GSCHEM_DIALOG
 * Why:  This Macro facilitate creating new Dialog Boxes.
*/
#define NEW_GSCHEM_DIALOG(title, flags, identifier, ptrWindowWidget) \
  gschem_dialog_new_empty(title, GTK_WINDOW(w_current->main_window), \
                                  flags, identifier, ptrWindowWidget);

/* String for storing Selection data in Mode-less Editing Dialogs */
#define DIALOG_SELECTION_TRACKER "selection-tracker"
#define DIALOG_SELECTION_DATA    "current-selection"

#define GSCHEM_TYPE_DIALOG           (gschem_dialog_get_type())
#define GSCHEM_DIALOG(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_DIALOG, GschemDialog))
#define GSCHEM_DIALOG_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_DIALOG, GschemDialogClass))
#define GSCHEM_IS_DIALOG(obj)        (is_a_gschem_dialog(obj))
#define GSCHEM_DIALOG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_DIALOG, GschemDialogClass))

typedef struct _GschemDialogClass GschemDialogClass;
typedef struct _GschemDialog      GschemDialog;

typedef enum {
  GSCHEM_MODELESS_DIALOG,
  GSCHEM_DIALOG_MODAL               = 1 << 0, /* call gtk_window_set_modal (win, TRUE) */
  GSCHEM_DIALOG_DESTROY_WITH_PARENT = 1 << 1, /* call gtk_window_set_destroy_with_parent () */
  GSCHEM_DIALOG_NO_SEPARATOR        = 1 << 2  /* no separator bar above buttons */
} GschemDialogFlags;

enum {
  PROP_SETTINGS_NAME = 1,
  PROP_GSCHEM_TOPLEVEL,
  PROP_PARENT_WINDOW,
  PROP_SELECTION_LIST,
  PROP_SELECTION_TRACKER,
  PROP_TITLE,
};

typedef enum
{
  GSCHEM_RESPONSE_NONE         = -1,
  GSCHEM_RESPONSE_REJECT       = -2,
  GSCHEM_RESPONSE_ACCEPT       = -3,
  GSCHEM_RESPONSE_DELETE_EVENT = -4,
  GSCHEM_RESPONSE_OK           = -5,
  GSCHEM_RESPONSE_CANCEL       = -6,
  GSCHEM_RESPONSE_CLOSE        = -7,
  GSCHEM_RESPONSE_YES          = -8,
  GSCHEM_RESPONSE_NO           = -9,
  GSCHEM_RESPONSE_APPLY        = -10,
  GSCHEM_RESPONSE_HELP         = -11
} GschemResponseType;

struct _GschemDialogClass {
  GtkDialogClass parent_class;

  void (*geometry_save)    (GschemDialog *dialog, EdaConfig *cfg, char *group);
  void (*geometry_restore) (GschemDialog *dialog, EdaConfig *cfg, char *group);

};

struct _GschemDialog {
  GtkDialog parent_instance;

  char            *settings_name;
  GtkWindow       *parent_window;
  GschemToplevel  *w_current;
  SELECTION       *selection;
  void (*tracker) (GschemToplevel *w_current, Object *object);

  unsigned int tail_marker;       /* structure type signature */
};


GedaType     gschem_dialog_get_type       (void);
bool         is_a_gschem_dialog           (void *dialog);

GtkWidget* gschem_dialog_new_empty        (const char           *title,
                                           GtkWindow      *parent,
                                           GtkDialogFlags  flags,
                                           const char     *settings_name,
                                           GschemToplevel *w_current);

GtkWidget* gschem_dialog_new_with_buttons (const char *title, GtkWindow *parent, GtkDialogFlags flags,
                                           const char *settings_name, GschemToplevel *w_current,
                                           const char *first_button_text, ...);


GtkWindow  *gschem_dialog_get_parent(GschemDialog *dialog);
void        gschem_dialog_set_parent(GschemDialog *dialog, GtkWindow *parent);

const char *gschem_dialog_get_title(GschemDialog *dialog);
void        gschem_dialog_set_title(GschemDialog *dialog, const char*title);

/* Prototypes for Dialogs */
GtkWidget* create_geda_switch(GtkWidget *Dialog, GtkWidget *parent,
           GtkWidget *widget, GtkWidget *SwitchImage, gboolean istate);
GtkWidget* get_geda_switch_image ( gboolean WhichState);


#endif /* __GSCHEM_DIALOG_H__ */
