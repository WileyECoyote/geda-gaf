/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 */

/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions used to display dialog boxes.
 *
 * Functions used to display dialog boxes.
 */

#include <gattrib.h>
#include "version.h"
#include "geda_dialog_controls.h"
#include "geda_widgets.h"
#include "gattrib_dialog.h"
#include <geda_debug.h>

/***************** Start of generic message dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void generic_msg_dialog (const char *msg)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   "%s", msg);

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

}

/***************** End of generic message dialog box *********************/
/***************** Start of generic confirm dialog box *******************/

/*! \brief Generic Confirmation Dialog
 *
 *  \par Function Description
 *       Display a basic dialog with okay/cancel buttons
 *
 *  \param msg    pointer to message to be displayed
 *  \param type   The context type of the message
 *
 *  \returns True if user select OKAY, False if user select CANCEL
 *
 */
bool x_dialog_generic_confirm_dialog (const char *msg, int type)
{
  GtkWidget *dialog;
  int r;

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   type,
                                   GTK_BUTTONS_OK_CANCEL,
                                   "%s", msg);

  r = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (r ==  GEDA_RESPONSE_OK)
    return 1;
  else
    return 0;
}
/*! \brief Add new attribute dialog.
 *
 * This asks for the name of the attrib column to insert
 *         and then inserts the column.
 */
char *x_dialog_new_attrib()
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *attrib_entry;
  char *entry_text;

  /* Create the dialog */
  dialog = gtk_dialog_new_with_buttons(_("Add new attribute"), NULL,
				       GTK_DIALOG_MODAL,
				       GTK_STOCK_OK, GEDA_RESPONSE_OK,
				       GTK_STOCK_CANCEL, GEDA_RESPONSE_CANCEL,
				       NULL);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GEDA_RESPONSE_OK);

  /*  Create a text label for the dialog window */
  label = geda_label_new (_("Enter new attribute name"));
  gtk_box_pack_start (GTK_BOX(GTK_DIALOG(dialog)->vbox), label,
		      FALSE, FALSE, 0);

  /*  Create the "attrib" text entry area */
  attrib_entry = gtk_entry_new();
  gtk_entry_set_max_length ((GtkEntry *)attrib_entry, 48);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), attrib_entry, TRUE, TRUE, 5);
  gtk_widget_set_size_request (dialog, 260, 140);

  gtk_widget_show_all(dialog);

  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GEDA_RESPONSE_OK:
      entry_text = u_string_strdup( GetEntryText(attrib_entry) );
      break;
    case GEDA_RESPONSE_CANCEL:
    default:
      entry_text = NULL;
      break;
  }

  gtk_widget_destroy(dialog);
  return entry_text;
}


/*! \brief Delete Attribute dialog
 *
 * This function throws up the "Delete foo, are you sure?" dialog
 *         box.  It offers two buttons: "yes" and "cancel".
 */
void x_dialog_delete_attrib()
{
  GtkWidget *dialog;
  GtkSheet *sheet;
  int mincol, maxcol;
  int cur_page;

  /* First verify that exactly one column is selected.  */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = GTK_SHEET(sheets[cur_page]);
  if (sheet == NULL) {
    return;
  }

  mincol = x_gtksheet_get_min_col(sheet);
  maxcol =  x_gtksheet_get_max_col(sheet);

  if ( (mincol != maxcol) || (mincol == -1) || (maxcol == -1) ) {
    /* Improper selection -- maybe throw up error box? */
    return;
  }

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_QUESTION,
                                  GTK_BUTTONS_YES_NO,
                                  _("Are you sure you want to delete this attribute?"));

  gtk_window_set_title(GTK_WINDOW(dialog), _("Delete attribute"));
  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GEDA_RESPONSE_YES:
      /* call the fcn to actually delete the attrib column.  */
      s_toplevel_delete_attrib_col(sheet);  /* this fcn figures out
                                        * which col to delete. */
      break;

    default:
      break;
  }

  gtk_widget_destroy(dialog);
}

/*! \brief Missing Symbol dialog
 *
 * This is the "missing symbol file found on object" dialog.
 *
 *  It offers the user the chance to close the project without
 *  saving because he read a schematic with a missing symbol file.
 */
void x_dialog_missing_sym()
{
  GtkWidget *dialog;
  const char *string = _("One or more components have been found with missing symbol files!\n\nThis probably happened because gattrib could not find your component libraries, perhaps because your gafrc or gattribrc files are misconfigured.\n\nChoose \"Quit\" to leave gattrib and fix the problem, or\n\"Forward\" to continue working with gattrib.\n");

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_WARNING,
                                  GTK_BUTTONS_NONE,
                                  "%s", string);

  gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                  GTK_STOCK_QUIT, GEDA_RESPONSE_REJECT,
                  GTK_STOCK_GO_FORWARD, GEDA_RESPONSE_ACCEPT,
                  NULL);

  gtk_window_set_title(GTK_WINDOW(dialog), _("Missing symbol file found for component!"));
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GEDA_RESPONSE_REJECT);

  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GEDA_RESPONSE_ACCEPT:
      /* Continue with the execution */
      break;

    default:
      /* Terminate */
      exit(0);
      break;
  }

  gtk_widget_destroy(dialog);
}
int x_dialog_file_not_saved()
{
  GtkWidget  *dialog;
  const char *tmp;
  char *str;
  int result;

  tmp = _("Save the changes before closing?");
  str = u_string_concat (N_("<big><b>"), tmp, N_("</b></big>"), NULL);

  tmp = _("If you don't save, all your changes will be permanently lost.");
  str = u_string_concat (str, "\n\n", tmp, NULL);

  dialog = gtk_message_dialog_new (GTK_WINDOW (main_window),
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_WARNING,
                                   GTK_BUTTONS_NONE, NULL);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), str);
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("Close without saving"), GEDA_RESPONSE_NO,
                          GTK_STOCK_CANCEL,          GEDA_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,            GEDA_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_YES,
                                          GEDA_RESPONSE_NO,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GEDA_RESPONSE_YES);

  result = (gtk_dialog_run (GTK_DIALOG (dialog)));
  gtk_widget_destroy (dialog);
  return result;
}
/*! \brief Unsaved data dialog
 *
 * This is the "Unsaved data -- are you sure you want to quit?" dialog
 *         box which is thrown up before the user quits.
 */
void x_dialog_unsaved_data()
{
  switch (x_dialog_file_not_saved())
  {
    case GEDA_RESPONSE_NO:
    {
      gattrib_quit(0);
      break;
    }
    case GEDA_RESPONSE_YES:
    {
      s_toplevel_gtksheet_to_toplevel(pr_current);  /* Dumps sheet data into GedaToplevel */
      s_page_save_all(pr_current);  /* saves all pages in design */
      sheet_head->CHANGED = FALSE;
      gattrib_quit(0);
      break;
    }
    case GEDA_RESPONSE_CANCEL:
    default:
    {
       break;
    }
  }
  return;
}

/*! \brief Unimplemented feature dialog
 *
 * This function informs the user that he has chosen an unimplemented
 *         feature.  It presents only an "OK" button to leave.
 */
void x_dialog_unimplemented_feature()
{
  GtkWidget *dialog;
  const char *string = _("Sorry -- you have chosen a feature which has not been\nimplemented yet.\n\nGattrib is an open-source program which\nI work on as a hobby. It is still a work in progress.\nIf you wish to contribute (perhaps by implementing this\nfeature), please do so! Please send patches to gattrib\nto Stuart Brorson: sdb@cloud9.net.\n\nOtherwise, just hang tight -- I'll implement this feature soon!\n");

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_INFO,
                                  GTK_BUTTONS_OK,
                                  "%s", string);

  gtk_window_set_title(GTK_WINDOW(dialog), _("Unimplemented feature!"));

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

/*! \brief Fatal error dialog
 *
 * This function displays a dialog with the error string and
 * terminates the program.
 *
 *  \param [in] string the error string
 *  \param [in] return_code the exit code
 *  \todo Is the GPOINTER_TO_INT() call needed in exit()?
 */
void x_dialog_fatal_error(char *string, int return_code)
{
  GtkWidget *dialog;

  fprintf(stderr, "%s\n", string);

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_OK,
                                  "%s", string);

  gtk_window_set_title(GTK_WINDOW(dialog), _("Fatal error"));

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);

  exit(GPOINTER_TO_INT(return_code));
}

/*! \brief About gattrib dialog
 *
 * This dosplays the about dialog.
 */
void x_dialog_about_dialog()
{
  GtkWidget *dialog;
  const char *string = _("gEDA : GPL Electronic Design Automation\n\nThis is gattrib -- gEDA's attribute editor\n\nGattrib version: %s%s.%s\n\nGattrib is written by: Stuart Brorson (sdb@cloud9.net)\nwith generous helpings of code from gschem, gnetlist, \nand gtkextra, as well as support from the gEDA community.");


  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   string, PREPEND_VERSION_STRING,
                                   PACKAGE_DOTTED_VERSION,
                                   PACKAGE_DATE_VERSION);

  gtk_window_set_title(GTK_WINDOW(dialog), _("About..."));

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

/*! \brief Export file dialog
 *
 * This asks for the filename for the CSV export file and then
 *         does the exporting.
 */
void x_dialog_export_file()
{
  char *cwd;
  char *filename;
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new(_("Export CSV"), NULL,
      GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GEDA_RESPONSE_CANCEL,
      GTK_STOCK_SAVE, GEDA_RESPONSE_ACCEPT,
      NULL);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GEDA_RESPONSE_ACCEPT);

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = getcwd(0,0);
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  free (cwd);

  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GEDA_RESPONSE_ACCEPT:
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      if(filename != NULL) {
        f_export_components(filename);
        GEDA_FREE(filename);
      }
      break;

    default:
      break;
  }

  gtk_widget_destroy(dialog);
}

/*********** Start of get text dialog box *******/

/*! \brief Create the text find dialog
 *  \par Function Description
 *  This function creates the get text dialog and returns a pointer
 *  to the string or NULL is the user canceled.
 */
char *x_dialog_get_search_text(char* prompt)
{
  GtkDialog *dialog    = NULL;
  GtkWidget *textentry = NULL;

  char      *text      = NULL;
  int r;

  if (dialog != NULL) {

    gtk_widget_hide((GtkWidget*)dialog);
    gtk_widget_destroy((GtkWidget*)dialog);
  }

  dialog = (GtkDialog*)gtk_dialog_new_with_buttons (_("Find Text"),
                                                      GTK_WINDOW(main_window),
                                                      GTK_DIALOG_MODAL,
                                                      GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                                      GTK_STOCK_FIND, GEDA_RESPONSE_ACCEPT,
                                                      NULL);
  if (dialog) {

    /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(dialog,
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);
    gtk_dialog_set_default_response(dialog, GEDA_RESPONSE_ACCEPT);

    g_object_set (dialog, "border-width", DIALOG_BORDER_WIDTH, NULL);

    GtkWidget *vbox = dialog->vbox;
    gtk_box_set_spacing((GtkBox*)vbox, DIALOG_V_SPACING);

    GtkWidget *label = geda_aligned_label_new(_(prompt), 0, 0);
    gtk_box_pack_start((GtkBox*)vbox, label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(32);

    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);
    gtk_widget_show_all(GTK_WIDGET(dialog));

    r = gtk_dialog_run ((GtkDialog*)dialog);

    if (r ==  GEDA_RESPONSE_ACCEPT)
      text = u_string_strdup( GetEntryText(textentry) );
    gtk_widget_destroy (GTK_WIDGET(dialog));
  }
  return text;

}

/*********** Start of Search and Replace dialog box *******/
#define ThisDialog SearchReplaceDialog
#define DialogTitle "Search and Replace"
#define DialogSettings "SearchReplace"
#define ControlID EnumeratedSearchControl

#define AlternateTitle "Geda-gaf Search"
#define AlternateSettings "FindDialog"

#define Combo_Responder search_replace_combo_responder
//#define Butt_Responder butt_responder
//#define Radio_Responder radio_responder
#define Switch_Responder search_replace_switch_responder

typedef enum {
/* 2 Combo Controls  */
  SearchText,
  ReplaceText,

/* 4 Switch Controls */
  IgnoreCase,
  WholeWord,
  SearchBackword,
  WrapAround,

} ControlID;

WidgetStringData DialogStrings[] = {
  /* 2 String for Edit Controls */
        { "SearchTextCombo",	        "  Search for:",      "Enter or select the text to find"},
        { "ReplaceTextCombo",	        "Replace with:",      "Enter or select the replacement text"},

  /* 4 Strings for Switch Controls */
        { "IgnoreCaseSwitch",	        "    Ignore Case",   "Set search case sensitivity"},
        { "WholeWordSwitch",	        "  Match Words",       "Limit Search hits to entire work"},
        { "SearchBackwordSwitch",	"Search Backword",   "Reverse search direction"},
        { "WrapAroundSwitch",	        "  Wrap Around",        "Continue search from the beginning"},
        { NULL, NULL, NULL}
};

/* The Buttons */
static GtkWidget *ReplaceAllButt;
static GtkWidget *ReplaceButt;
static GtkWidget *FindButt;

/* The Combo Boxes */
static GtkWidget *SearchTextCombo;
static GtkWidget *ReplaceTextCombo;
/* The Switches */
static GtkWidget *IgnoreCaseSwitch=NULL;
static GtkWidget *WholeWordSwitch=NULL;
static GtkWidget *SearchBackwordSwitch=NULL;
static GtkWidget *WrapAroundSwitch=NULL;

/*! \brief Action Response function for the Search dialogs
 *  \par Function Description
 *  This function processes the "response" signals from the action
 *  buttons in the Search dialogs.
 */
static void search_replace_dialog_response(GtkWidget    *ThisDialog,
                                           int           response,
                                           SearchRecord *Search)
{
  char* search_text;
  char* replacement_text;

  /*!@brief Add new text to Search History List */
  void add_search_history(char *new_text) {
    /*! \note: String added to search_history is freed at program exit */
    if (!g_list_stri_inlist(search_history, new_text)) {
      search_history = g_list_prepend(search_history, u_string_strdup(new_text));
    }
  }

  /*!@brief Retrieve values and settings from Search Dialog controls */
  void unload_dialog() {
    search_text      = u_string_strdup(gtk_combo_box_get_active_text (GTK_COMBO_BOX (SearchTextCombo)));
    Search->Case     = !GET_SWITCH_STATE (IgnoreCaseSwitch);
    Search->Whole    =  GET_SWITCH_STATE (WholeWordSwitch);
    Search->Backword =  GET_SWITCH_STATE (SearchBackwordSwitch);
    Search->Wrap     =  GET_SWITCH_STATE (WrapAroundSwitch);
    add_search_history(search_text);
  }
  switch (response) {
  case GEDA_RESPONSE_REJECT: /* Don't replace find next */
    unload_dialog();
    Search->Found = x_find_main_search(search_text, NULL);
    if(search_text) GEDA_FREE(search_text);
    break;
  case GEDA_RESPONSE_APPLY: /*"Replace All and close dialog"*/
    Search->ReplaceAll = TRUE;
  case GEDA_RESPONSE_ACCEPT: /* Replace*/
    unload_dialog();
    replacement_text = u_string_strdup(gtk_combo_box_get_active_text (GTK_COMBO_BOX (ReplaceTextCombo)));
    add_search_history(replacement_text);
    Search->Found = x_find_main_search(search_text, replacement_text);
    if(search_text) GEDA_FREE(search_text);
    if(replacement_text) GEDA_FREE(replacement_text);
    break;
  case GEDA_RESPONSE_DELETE_EVENT:
  case GEDA_RESPONSE_CANCEL:
    gtk_widget_destroy(ThisDialog);
    ThisDialog = NULL;
    break;
  default:
    BUG_IMSG ("unhandled case for signal <%d>", response);
  }
  Search->ReplaceAll = FALSE; /* This must be enabled by user each loop */
}

/* ------------------------ ComboBox Support Functions ----------------------*/

/*! \brief Search Dialog Combo Responder
 *  \par Function Description: This callback function is used to set the
 *       sensitivity of other controls based on combo-box input.
 */
static void search_replace_combo_responder(GtkWidget *widget, void * data)
{
  int WhichComboBox = GPOINTER_TO_UINT (data);

  char *text = gtk_combo_box_get_active_text (GTK_COMBO_BOX (widget));

  switch ( WhichComboBox ) {
  case SearchText:
    if ( strlen(text) >0)
    {
       gtk_widget_set_sensitive (FindButt, TRUE);
       gtk_widget_set_sensitive (IgnoreCaseSwitch, TRUE);
       gtk_widget_set_sensitive (WholeWordSwitch, TRUE);
       gtk_widget_set_sensitive (SearchBackwordSwitch, TRUE);
       gtk_widget_set_sensitive (WrapAroundSwitch, TRUE);
    }
    else
    {
       gtk_widget_set_sensitive (FindButt, FALSE);
       gtk_widget_set_sensitive (ReplaceButt, FALSE);
       gtk_widget_set_sensitive (ReplaceAllButt, FALSE);
       gtk_widget_set_sensitive (IgnoreCaseSwitch, FALSE);
       gtk_widget_set_sensitive (WholeWordSwitch, FALSE);
       gtk_widget_set_sensitive (SearchBackwordSwitch, FALSE);
       gtk_widget_set_sensitive (WrapAroundSwitch, FALSE);
    }
    /* No break! */
  case ReplaceText:
    /* We are not going to enable Replace buttons unless there is text in the
     * Search Combo, the pointer in text is to the text in the Replace Combo */
    if (( strlen(gtk_combo_box_get_active_text (GTK_COMBO_BOX (SearchTextCombo))) >0) &&
        ( strlen(gtk_combo_box_get_active_text (GTK_COMBO_BOX (ReplaceTextCombo))) >0))
    {
       gtk_widget_set_sensitive (ReplaceButt, TRUE);
       gtk_widget_set_sensitive (ReplaceAllButt, TRUE);
    }
    else
    {
       gtk_widget_set_sensitive (ReplaceButt, FALSE);
       gtk_widget_set_sensitive (ReplaceAllButt, FALSE);
    }
    break;
  default:
    u_log_message("combo_responder(): Warning, Unknown Combo Id: %d\n",WhichComboBox);
  }

 return;
}

/* ------------------------- Switch Support Functions -----------------------*/

/*! \brief Toggle Search Dialog Switch Images
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 *       The functions enables or disables other widgets based on
 *       the state of the switch.
 */
static void search_replace_switch_responder(GtkWidget *widget, int response,  ControlID *Control)
{
   gboolean state = GET_SWITCH_STATE (widget);
   GtkWidget* SwitchImage = get_geda_switch_image( state);
   gtk_button_set_image(GTK_BUTTON (widget), SwitchImage);

   /* We don't have a pointer to the Search structure but this does not
    * matter since we don't need to do anything here, stubbing code here */
   switch ( response ) {
   case IgnoreCase:
   case WholeWord:
   case SearchBackword:
   case WrapAround:
     break;
   default:
    u_log_message("toggle_switch(): UKNOWN SWITCH ID: %d\n", response);
   }

   return;
}

/* ---------------- Search Dialog Initialization Functions ------------------*/
/*! \brief Initialize Search Dialog Controls
 *  \par Function Description: This function sets the initial state of the
 *       Search Dialog. The toggle switches are set to the values in the
 *       Search Record structure. Controls for options and all but the
 *       Close button are disabled.
 */
static void x_dialog_init_search_replace(GtkWidget *ThisDialog, SearchRecord *Search)
{
  Search->ReplaceAll = FALSE; /* could have been on if struc re-used */

  SetSwitch( IgnoreCase,    !Search->Case);
  SetSwitch( WholeWord,      Search->Whole);
  SetSwitch( SearchBackword, Search->Backword);
  SetSwitch( WrapAround,     Search->Wrap);

  /* User can not change these until there is a search string */
  /* disable options */
  gtk_widget_set_sensitive (IgnoreCaseSwitch, FALSE);
  gtk_widget_set_sensitive (WholeWordSwitch, FALSE);
  gtk_widget_set_sensitive (SearchBackwordSwitch, FALSE);
  gtk_widget_set_sensitive (WrapAroundSwitch, FALSE);

  /* Disable buttons */
  gtk_widget_set_sensitive (FindButt, FALSE);
  gtk_widget_set_sensitive (ReplaceButt, FALSE);
  gtk_widget_set_sensitive (ReplaceAllButt, FALSE);

  if(g_list_length(search_history) > 0) {
    {
       lambda (const char* data)
       {
         LOAD_STD_COMBO(SearchText,data);
         return FALSE;
       }
       foreach (search_history);
    }
    {
       lambda (const char* data)
       {
         LOAD_STD_COMBO(ReplaceText,data);
         return FALSE;
       }
       foreach (search_history);
    }
  }
}
/*! \brief Create Search Dialog Controls
 *  \par Function Description: This function creates the Search Dialog
 *  and all controls. The Dialog can be either a Search/Find or a Search
 *  and Replace Dialog depending on the second parameter, find_only_mode.
 *  If find_only_mode is TRUE, then the ReplaceTextCombo, Replace and
 *  Replace All button are not made visible, effectively creating a Find
 *  dialog without options to replace. All of the functionality remains
 *  but is not utilize. This make since because the Search And Replace
 *  Dialog can also be used as a Find Dialog (without actually replacing
 *  anything).
 *
 */
static
GtkWidget* x_dialog_create_search_replace_dialog (GtkWindow *parent, int find_only_mode)
{
  GtkWidget *ThisDialog;
  GtkWidget *MainDialogVBox;

  GtkWidget *dialog_action_area;
  GtkWidget *CloseButt;

  GtkTooltips *tooltips;
  tooltips = gtk_tooltips_new ();

  if (find_only_mode)
    ThisDialog=NEW_STD_GATTRIB_DIALOG( AlternateTitle, AlternateSettings, parent);
  else
    ThisDialog=NEW_STD_GATTRIB_DIALOG( DialogTitle, DialogSettings, parent);

  gtk_window_set_title (GTK_WINDOW (ThisDialog), _(DialogTitle));
  gtk_window_set_modal (GTK_WINDOW (ThisDialog), FALSE);
  gtk_window_set_destroy_with_parent (GTK_WINDOW (ThisDialog), TRUE);
  gtk_window_set_type_hint (GTK_WINDOW (ThisDialog), GDK_WINDOW_TYPE_HINT_DIALOG);

  MainDialogVBox = GTK_DIALOG (ThisDialog)->vbox;
  gtk_widget_show (MainDialogVBox);

  HSECTION (MainDialogVBox, InputText);   /* Row 1 */
    GTK_NEW_COMBO (InputText_hbox, SearchText, 200, 41);

  HSECTION (MainDialogVBox, NewText);   /*  Row 2 */
    GTK_NEW_COMBO (NewText_hbox, ReplaceText, 200, 41);
  if (find_only_mode) {
    gtk_widget_hide(ReplaceTextLabel);
    gtk_widget_hide(ReplaceTextCombo);
  }
  HXYP_SEPERATOR (MainDialogVBox, Grp3, 10);

  HSECTION (MainDialogVBox, SearchOptions1);   /*  Row 3 */
    GTK_SWITCH(SearchOptions1_hbox, IgnoreCase, 0, TRUE);
    GTK_SWITCH(SearchOptions1_hbox, WholeWord, 0, TRUE);
  HSECTION (MainDialogVBox, SearchOptions2);   /*  Row 4 */
    GTK_SWITCH(SearchOptions2_hbox, SearchBackword, 0, FALSE);
    GTK_SWITCH(SearchOptions2_hbox, WrapAround, 0, TRUE);

  HXYP_SEPERATOR (MainDialogVBox, Grp4, 10);

  dialog_action_area = GTK_DIALOG (ThisDialog)->action_area;
  g_object_set ( dialog_action_area, "visible", TRUE, NULL);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area), GTK_BUTTONBOX_END);

  CloseButt = gtk_button_new_from_stock ("gtk-close");
  g_object_set ( CloseButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), CloseButt, GEDA_RESPONSE_CANCEL);
  gtk_widget_set_can_default(CloseButt, TRUE);
  gtk_widget_set_size_request (CloseButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, CloseButt, _("Close this dialog"), NULL);

  ReplaceAllButt = gtk_button_new_with_mnemonic (_("Replace All"));
  if (!find_only_mode)
    g_object_set ( ReplaceAllButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), ReplaceAllButt, GEDA_RESPONSE_APPLY);
  gtk_widget_set_can_default(ReplaceAllButt, TRUE);
  gtk_widget_set_size_request (ReplaceAllButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, ReplaceAllButt, _("Replace All and close dialog"), NULL);

  ReplaceButt = gtk_button_new_with_mnemonic (_("Replace"));
  if (!find_only_mode)
    g_object_set ( ReplaceButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), ReplaceButt, GEDA_RESPONSE_ACCEPT);
  gtk_widget_set_can_default(ReplaceButt, TRUE);
  gtk_widget_set_size_request (ReplaceButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, ReplaceButt, _("Replace selected text and continue"), NULL);

  FindButt = gtk_button_new_with_mnemonic (_("Find"));
  g_object_set ( FindButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), FindButt, GEDA_RESPONSE_REJECT);
  gtk_widget_set_can_default(FindButt, TRUE);
  gtk_widget_set_size_request (FindButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, FindButt, _("Find next"), NULL);

  /* Store pointers to widgets, for use by get_widget_data(). */
  GEDA_OBJECT_SET_DATA (ThisDialog, ThisDialog,         DialogTitle);
  GEDA_OBJECT_SET_DATA (ThisDialog, MainDialogVBox,     "MainDialogVBox");
  GEDA_OBJECT_SET_DATA (ThisDialog, dialog_action_area, "dialog_action_area");
  GEDA_HOOKUP_OBJECT (ThisDialog, CloseButt,       "CloseButt");
  GEDA_HOOKUP_OBJECT (ThisDialog, ReplaceButt,     "ReplaceButt");
  GEDA_HOOKUP_OBJECT (ThisDialog, ReplaceAllButt,  "ReplaceAllButt");
  GEDA_HOOKUP_OBJECT (ThisDialog, FindButt,        "FindButt");
  GEDA_OBJECT_SET_DATA (ThisDialog, tooltips, "tooltips");

  return ThisDialog;
}

/*! \brief Startup Search and Replace Dialog
 *  \par Function Description: This is the main function called by external
 *       to launch a new Search and Replace Dialog session.
 */
void x_dialog_search_replace(SearchRecord *Search) {

  GtkWidget *ThisDialog;

  ThisDialog = x_dialog_create_search_replace_dialog(GTK_WINDOW ( main_window),
                                                     Search->FindOnlyMode);

  gtk_window_position (GTK_WINDOW (ThisDialog), GTK_WIN_POS_MOUSE);

  x_dialog_init_search_replace(ThisDialog, Search);

  GEDA_SIGNAL_CONNECT(ThisDialog, "response",
                      search_replace_dialog_response, Search);

  gtk_container_border_width (GTK_CONTAINER(ThisDialog), DIALOG_BORDER_WIDTH);

  gtk_widget_show(ThisDialog);
}

/*********** End of find text dialog box *******/
