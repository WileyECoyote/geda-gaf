/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill
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
 *
 * WEH: Minor Tweak from gschem_dialog.h
 */


#ifndef __GATTRIB_DIALOG_H__
#define __GATTRIB_DIALOG_H__

#define DIALOG_BORDER_WIDTH 5
#define DEFAULT_WIDGET_SPACING 2
#define DIALOG_V_SPACING 2
#define DEFAULT_BUTTON_WIDTH 122
#define DEFAULT_BUTTON_HEIGHT 31

/* For x_window.c */
#define GATTRIB_THEME_ICON_NAME     "geda-gattrib"

/* These are image filenames used for the widgets in dialog boxes */
#define SWITCH_ON_IMAGE  	   "geda_switch_64x48_on.png"
#define SWITCH_OFF_IMAGE 	   "geda_switch_64x48_off.png"

#define BULB_ON_IMAGE    	   "geda_bulb_24x26_on.png"
#define BULB_OFF_IMAGE   	   "geda_bulb_24x26_off.png"

#define NEW_STD_GATTRIB_DIALOG(title, identifier, ptrWindowWidget) \
        (GtkDialog*)gattrib_dialog_new_empty(title, main_window, \
                    GTK_DIALOG_MODAL, \
                    identifier)

/* Date: Aug 12, 2012
 * Who:  Wiley E. Hill
 * What  MACRO: NEW_GATTRIB_DIALOG
 * Why:  This Macro facilitate creating new Dialog Boxes.
*/
#define NEW_GATTRIB_DIALOG(title, flags, identifier, ptrWindowWidget) \
  gattrib_dialog_new_empty(title, main_window, \
                                  flags, identifier, ptrWindowWidget);

#define GATTRIB_TYPE_DIALOG           (gattrib_dialog_get_type())
#define GATTRIB_DIALOG(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GATTRIB_TYPE_DIALOG, GattribDialog))
#define GATTRIB_DIALOG_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GATTRIB_TYPE_DIALOG, GattribDialogClass))
#define GATTRIB_IS_DIALOG(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GATTRIB_TYPE_DIALOG))
#define GATTRIB_DIALOG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GATTRIB_TYPE_DIALOG, GattribDialogClass))

typedef struct _GattribDialogClass GattribDialogClass;
typedef struct _GattribDialog      GattribDialog;


struct _GattribDialogClass {
  GtkDialogClass parent_class;

  void (*geometry_save)    (GattribDialog *dialog,
                            GedaKeyFile   *key_file,
                            char          *group_name);
  void (*geometry_restore) (GattribDialog *dialog,
                            GedaKeyFile   *key_file,
                            char          *group_name);
};

struct _GattribDialog {
  GtkDialog parent_instance;

  char *settings_name;
  GedaToplevel *pr_current;
};

GedaType   gattrib_dialog_get_type  (void) GEDA_CONST;
GtkWidget *gattrib_dialog_new_empty (const char            *title,
                                           GtkWindow       *parent,
                                           GtkDialogFlags   flags,
                                           const char *settings_name);

GtkWidget *gattrib_dialog_new_with_buttons (const char *title, GtkWindow *parent, GtkDialogFlags flags,
                                            const char *settings_name, const char *first_button_text, ...);

/* Prototypes for Dialogs */
GtkWidget *create_geda_switch(GtkWidget *parent, GtkWidget *SwitchImage, bool istate);
GtkWidget *get_geda_switch_image (bool WhichState);

#endif /* __GATTRIB_DIALOG_H__ */
