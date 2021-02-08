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
 * \par
 * Functions used to display dialog boxes.
 */

#include "../../version.h"

#include "../include/gattrib.h"
#include "../include/gattrib_dialog.h"
#include <geda/geda_dialog_controls.h>
#include <geda_widgets.h>
#include <geda_dialogs.h>

/***************** Start of generic message dialog box *******************/

/*!
 * \brief Create generic dialog and display message
 * \par Function Description
 *  Use generic Gtk message dialog to display \a msg
 *
 * \param msg    pointer to message to be displayed
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

  gtk_dialog_run ((GtkDialog*)dialog);
  gtk_widget_destroy (dialog);

}

/***************** End of generic message dialog box *********************/

/***************** Start of generic confirm dialog box *******************/

/*!
 * \brief Generic Confirmation Dialog
 * \par Function Description
 *  Display a basic dialog with okay/cancel buttons
 *
 * \param msg    pointer to message to be displayed
 * \param type   The context type of the message
 *
 * \returns True if user select OKAY, False if user select CANCEL
 */
bool x_dialog_generic_confirm_dialog (const char *msg, int type)
{
  GtkWidget *dialog;
  int response;

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   type,
                                   GTK_BUTTONS_OK_CANCEL,
                                   "%s", msg);

  response = gtk_dialog_run ((GtkDialog*)dialog);
  gtk_widget_destroy (dialog);

  return (response == GEDA_RESPONSE_OK);
}

/***************** End of generic message dialog box *********************/

/****************** Start of New Attribute dialog box ********************/

/*!
 * \brief Add new attribute dialog
 * \par Function Description
 * This function displays a dialog box to prompt for the name of the
 * attribute column to insert and then inserts the column.
 */
char *x_dialog_new_attrib(void)
{
  GtkDialog *dialog;
  GtkWidget *container;
  GtkWidget *widget;
  GtkWidget *label;
  GtkWidget *attrib_entry;
  char      *entry_text;

  /* Create the dialog */
  widget = gtk_dialog_new_with_buttons(_("Add new attribute"), NULL,
                                       GTK_DIALOG_MODAL,
                                       GTK_STOCK_CANCEL, GEDA_RESPONSE_CANCEL,
                                       GTK_STOCK_OK, GEDA_RESPONSE_OK,
                                       NULL);

  dialog = (GtkDialog*)widget;

  container = gtk_dialog_get_content_area(dialog);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(dialog,
                                          GEDA_RESPONSE_OK,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response(dialog, GEDA_RESPONSE_OK);

  /*  Create a text label for the dialog window */
  label = geda_label_new (_("Enter new attribute name"));
  geda_container_add(container, label);

  /*  Create the "attrib" text entry area */
  attrib_entry = geda_entry_new_with_max_length(48);
  geda_entry_widget_set_activates_default(attrib_entry, TRUE);

  geda_container_add(container, attrib_entry);
  gtk_widget_set_size_request (widget, 300, 140);
  gtk_widget_show(attrib_entry);

  gtk_widget_grab_focus(attrib_entry);

  switch (gtk_dialog_run(dialog)) {
    case GEDA_RESPONSE_OK:
      entry_text = geda_utility_string_strdup(GetEntryText(attrib_entry));
      break;

    case GEDA_RESPONSE_CANCEL:
    default:
      entry_text = NULL;
      break;
  }

  gtk_widget_destroy(widget);

  return entry_text;
}

/****************** End of New Attribute dialog box **********************/

/***************** Start of Column Visibility dialog box *****************/

enum {
  COLUMN_VISIBLE,
  COLUMN_NAME,
  COLUMN_DATA,
  NUM_COLUMNS
};

/*!
 * \internal
 * Callback function for the first column of activateble cells "toggled"
 * signal on the Column Visibility Dialog, aka the column of attributes.
 * Oddly, the active state is not passed to the callback, and so this
 * function obtains the state of the cell at the \a path and toggles
 * the cell and updates the ColumnVisible.visible field, whose address
 * is contained in the cooresponding COLUMN_DATA cell.
 */
static void
x_dialog_column_visibility_toggled (GtkCellRendererToggle *cell_renderer,
                                    char                  *path,
                                    void                  *store)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  int  *ptr;
  bool  visible;

  model = GTK_TREE_MODEL (store);

  /* Abort if tree path not found */
  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_VISIBLE, &visible,
                      COLUMN_DATA, &ptr,
                      -1);

  gtk_tree_store_set (GTK_TREE_STORE (store), &iter,
                      COLUMN_VISIBLE, (visible != TRUE),
                      -1);

  /* Store the value in the data structure */
  *ptr = (visible != TRUE);
}

/*!
 * \brief Show Column Visibility Dialog
 * \par Function Description
 *  This function creates and presents a dialog with a list of all
 *  column titles contained within the \a list of ColumnVisible
 *  records, which is all columns in the active sheet, along with
 *  an activatable widget set to the current visible state of the
 *  column.
 *  The address of the visible field in the ColumnVisible record is
 *  stored in the COLUMN_DATA column of the tree store.
 */
bool x_dialog_column_visibility (GList *list)
{
  GtkWidget         *dialog;
  GtkWidget         *scrolled_win;
  GtkTreeModel      *store;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;
  GtkTreeView       *tree_view;

  bool response;

  dialog = gattrib_dialog_new_with_buttons(_("Column Visibility"),
                                             NULL,
                                             1,   /* Modal */
                                             "column-visibility",
                                             GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                             GTK_STOCK_APPLY, GEDA_RESPONSE_APPLY,
                                             NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order (GTK_DIALOG(dialog),
                                           GEDA_RESPONSE_APPLY,
                                           GEDA_RESPONSE_REJECT,
                                           -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GEDA_RESPONSE_APPLY);

  /* create the model for the TreeView */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_BOOLEAN,  /* visibility */
                                             G_TYPE_STRING,   /* column name */
                                             G_TYPE_POINTER); /* pointer visibility */

  /* create a scrolled window for the TreeView */
  scrolled_win = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                               /* GtkContainer */
                               "border-width",      DIALOG_BORDER_WIDTH,
                               /* GtkScrolledWindow */
                               "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                               "vscrollbar-policy", GTK_POLICY_ALWAYS,
                               "shadow-type",       GTK_SHADOW_ETCHED_IN,
                               NULL);

  /* create the TreeView */
  tree_view = g_object_new (GTK_TYPE_TREE_VIEW,
                            /* GtkTreeView */
                            "model",      store,
                            "rules-hint", TRUE,
                            NULL);

  /* --------------------- first column: Visible  ---------------------- */

  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
                           /* GtkCellRendererToggle */
                           "activatable", TRUE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         /* GtkTreeViewColumn */
                         "title", _("Visible"),
                         /*"min-width", COLUMN_VISIBLE_MIN_WIDTH,*/
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  gtk_tree_view_column_add_attribute (column, renderer, "active", COLUMN_VISIBLE);
  gtk_tree_view_append_column (tree_view, column);

  g_signal_connect (renderer, "toggled",
                    G_CALLBACK (x_dialog_column_visibility_toggled),
                    store);

  /* --------------------- second column: title  ----------------------- */

  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           /* GtkCellRendererText */
                           "editable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         /* GtkTreeViewColumn */
                         "title",    _("Column"),
                        /*"min-width", COLUMN_NAME_MIN_WIDTH,*/
                         "sizing",    GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                         "resizable", TRUE,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_append_column (tree_view, column);

  /* ------------------------------------------------------------------- */

  /* add the tree view to the scrolled window */
  geda_container_add (scrolled_win, tree_view);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), scrolled_win, TRUE, TRUE, 0);

  GtkTreeIter *parent = NULL;

  while (list) {

    ColumnVisible *cv;

    cv  = list->data;

    GtkTreeIter iter;

    /* add the data to the store */
    gtk_tree_store_append (GTK_TREE_STORE (store), &iter, parent);

    gtk_tree_store_set (GTK_TREE_STORE (store), &iter,
                        COLUMN_VISIBLE, cv->visible,
                        COLUMN_NAME, cv->name,
                        COLUMN_DATA, &cv->visible,
                        -1);

    list = list->next;
  }

  gtk_widget_show_all(dialog);

  response = gtk_dialog_run((GtkDialog*)dialog);

  gtk_widget_destroy(dialog);

  return (response == GEDA_RESPONSE_APPLY);
}

/****************** End of Column Visibility dialog box ******************/

/*!
 * \brief Missing Symbol dialog
 * \par Function Description
 *  This is the "missing symbol file found on object" dialog, which
 *  offers users an opertunity to close the project without saving
 *  because the schematic was read with a missing symbol.
 */
void x_dialog_missing_sym(void)
{
  GtkDialog  *dialog;
  const char *string = _("One or more components have been found with missing symbol files!\n\nThis probably happened because gattrib could not find your component libraries, perhaps because your gafrc or gattribrc files are misconfigured.\n\nChoose \"Quit\" to leave gattrib and fix the problem, or\n\"Forward\" to continue working with gattrib.\n");

  /* Create the dialog */
  dialog = (GtkDialog*)gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                               GTK_MESSAGE_WARNING,
                                               GTK_BUTTONS_NONE,
                                               "%s", string);

  gtk_dialog_add_buttons(dialog,
                         GTK_STOCK_QUIT, GEDA_RESPONSE_REJECT,
                         GTK_STOCK_GO_FORWARD, GEDA_RESPONSE_ACCEPT,
                         NULL);

  gtk_window_set_title((GtkWindow*)dialog, _("Missing symbol file found for component!"));
  gtk_dialog_set_default_response(dialog, GEDA_RESPONSE_REJECT);

  switch (gtk_dialog_run(dialog)) {
    case GEDA_RESPONSE_ACCEPT:
      /* Continue with the execution */
      break;

    default:
      /* Terminate */
      exit(0);
      break;
  }

  gtk_widget_destroy((GtkWidget*)dialog);
}

/*!
 * \brief File Not Saved dialog
 * \par Function Description
 *  Displays a dialog notifing users that the content has changes and
 *  provides an opportunity to save the file.
 */
int x_dialog_file_not_saved(void)
{
  GtkDialog  *dialog;
  const char *tmp;
  char *str, *msg;
  int result;

  tmp = _("Save the changes before closing?");
  str = geda_strconcat ("<big><b>", tmp, "</b></big>", NULL);

  tmp = _("If you do not save, all your changes will be permanently lost.");
  msg = geda_strconcat (str, "\n\n", tmp, NULL);

  geda_free(str);

  dialog = (GtkDialog*)gtk_message_dialog_new ((GtkWindow*)main_window,
                                               GTK_DIALOG_MODAL |
                                               GTK_DIALOG_DESTROY_WITH_PARENT,
                                               GTK_MESSAGE_WARNING,
                                               GTK_BUTTONS_NONE, NULL);

  gtk_message_dialog_set_markup ((GtkMessageDialog*)dialog, msg);

  geda_free(msg);

  gtk_dialog_add_buttons (dialog,
                        _("Close without saving"), GEDA_RESPONSE_NO,
                          GTK_STOCK_CANCEL,        GEDA_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,          GEDA_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(dialog,
                                          GEDA_RESPONSE_YES,
                                          GEDA_RESPONSE_NO,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (dialog, GEDA_RESPONSE_YES);

  result = gtk_dialog_run (dialog);

  gtk_widget_destroy ((GtkWidget*)dialog);

  return result;
}

/*!
 * \brief Unsaved data dialog
 * \par Function Description
 *  This is the "Unsaved data -- are you sure you want to quit?" dialog
 *  box which is thrown up before the user quits.
 */
void x_dialog_unsaved_data(void)
{
  switch (x_dialog_file_not_saved()) {
    case GEDA_RESPONSE_NO:
      gattrib_quit(0);
      break;

    case GEDA_RESPONSE_YES:
      s_toplevel_gtksheet_to_toplevel(pr_current);  /* Dumps sheet data into GedaToplevel */
      geda_struct_page_save_all(pr_current);  /* saves all pages in design */
      sheet_head->CHANGED = FALSE;
      gattrib_quit(0);
      break;

    case GEDA_RESPONSE_CANCEL:
    default:
       break;
  }

  return;
}

/*!
 * \brief Unimplemented feature dialog
 * \par Function Description
 *  This function informs the user that he has chosen an unimplemented
 *  feature. The dialog presents only an "OK" button to leave.
 */
void x_dialog_unimplemented_feature(void)
{
  GtkWidget  *dialog;
  const char *string = _("Sorry -- you have chosen a feature which has not been\nimplemented yet.\n\nGattrib is an open-source program which\nI work on as a hobby. It is still a work in progress.\nIf you wish to contribute (perhaps by implementing this\nfeature), please do so! Please send patches to gattrib\nto Stuart Brorson: sdb@cloud9.net.\n\nOtherwise, just hang tight -- I'll implement this feature soon!\n");

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_INFO,
                                  GTK_BUTTONS_OK,
                                  "%s", string);

  gtk_window_set_title((GtkWindow*)dialog, _("Unimplemented feature!"));

  gtk_dialog_run((GtkDialog*)dialog);
  gtk_widget_destroy(dialog);
}

/*!
 * \brief Fatal error dialog
 * \par Function Description
 *  This function displays a dialog with the error string and terminates
 *  the program.
 *
 * \param [in] string the error string
 * \param [in] return_code the exit code
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

  gtk_window_set_title((GtkWindow*)dialog, _("Fatal error"));

  gtk_dialog_run((GtkDialog*)dialog);
  gtk_widget_destroy(dialog);

  exit (return_code);
}

/*!
 * \brief About gattrib dialog
 * \par Function Description
 *  This function displays the about dialog.
 */
void x_dialog_about_dialog(void)
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

  gtk_window_set_title((GtkWindow*)dialog, _("About..."));

  gtk_dialog_run((GtkDialog*)dialog);
  gtk_widget_destroy(dialog);
}

/*!
 * \brief Export file dialog
 * \par Function Description
 *  This asks for the filename for the CSV export file and then
 *  does the exporting.
 */
void x_dialog_export_file(void)
{
  char *cwd;
  char *filename;
  GtkWidget *dialog;

  dialog = geda_file_chooser_dialog_new_full(_("Export CSV"), NULL,
                                                FILE_CHOOSER_ACTION_OPEN,
                                                GTK_STOCK_CANCEL, GEDA_RESPONSE_CANCEL,
                                                GTK_STOCK_SAVE, GEDA_RESPONSE_ACCEPT,
                                                NULL);

  gtk_dialog_set_default_response((GtkDialog*)dialog, GEDA_RESPONSE_ACCEPT);

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = getcwd(0,0);
  geda_file_chooser_set_current_folder (dialog, cwd);
  free (cwd);

  switch (gtk_dialog_run((GtkDialog*)dialog)) {
    case GEDA_RESPONSE_ACCEPT:
      filename = geda_file_chooser_get_filename (dialog);
      if (filename != NULL) {
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

/*!
 * \brief Create the Locate Attribute dialog
 * \par Function Description
 *  This function creates the get text dialog and returns a pointer
 *  to the string or NULL is the user canceled.
 */
char *x_dialog_get_search_text(const char *prompt)
{
  GtkDialog *dialog    = NULL;
  char      *text      = NULL;
  char      *title;

  if (dialog != NULL) {
    gtk_widget_hide((GtkWidget*)dialog);
    gtk_widget_destroy((GtkWidget*)dialog);
  }

  title = geda_strconcat(_("Find"), " ", prompt, NULL);

  dialog = (GtkDialog*)gtk_dialog_new_with_buttons (title,
                                                    (GtkWindow*)main_window,
                                                    GTK_DIALOG_MODAL,
                                                    GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                                    GTK_STOCK_FIND, GEDA_RESPONSE_ACCEPT,
                                                    NULL);
  GEDA_FREE(title);

  if (dialog) {

    GtkWidget *label;
    GtkWidget *textentry;
    GtkBox    *vbox;
    char      *real_prompt;
    int        response;

    /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(dialog,
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);
    gtk_dialog_set_default_response(dialog, GEDA_RESPONSE_ACCEPT);

    geda_set_container_border_width (dialog, DIALOG_BORDER_WIDTH);

    vbox = (GtkBox*)dialog->vbox;

    g_object_set (vbox, "spacing", DIALOG_V_SPACING + 5, NULL);

    real_prompt = geda_strconcat(_("Enter"), " ", prompt, ":", NULL);
    label       = geda_aligned_visible_label_new(real_prompt, 0, 0);
    gtk_box_pack_start(vbox, label, TRUE, TRUE, 0);
    GEDA_FREE(real_prompt);

    textentry = geda_entry_new_with_max_length(32);

    gtk_widget_show (textentry);

    gtk_editable_select_region((GtkEditable*)textentry, 0, -1);
    gtk_box_pack_start(vbox, textentry, FALSE, FALSE, 0);

    geda_entry_widget_set_activates_default(textentry, TRUE);

    gtk_widget_grab_focus(textentry);

    response = gtk_dialog_run (dialog);

    if (response == GEDA_RESPONSE_ACCEPT) {
      text = geda_utility_string_strdup( GetEntryText(textentry) );
    }

    gtk_widget_destroy ((GtkWidget*)dialog);
  }

  return text;
}

/*********** Start of Search and Replace dialog box *******/
#define ThisDialog SearchReplaceDialog
#define DialogTitle "Search and Replace"
#define DialogSettings "SearchReplace"
#define ControlID EnumeratedSearchControl

#define AlternateTitle "Find Attribute Value"
#define AlternateSettings "FindDialog"

#define Combo_Responder search_replace_combo_responder
//#define Butt_Responder butt_responder
//#define Radio_Responder radio_responder
#define Switch_Responder search_replace_switch_responder

#undef ReplaceText  /* Defined in mingw\include\windows.h */

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
        { "SearchTextCombo",       N_("Search for:"),     N_("Enter or select the text to find")},
        { "ReplaceTextCombo",      N_("Replace with:"),   N_("Enter or select the replacement text")},

  /* 4 Strings for Switch Controls */
        { "IgnoreCaseSwitch",      N_("Ignore Case"),     N_("Set search case sensitivity")},
        { "WholeWordSwitch",       N_("Match Words"),     N_("Limit Search hits to entire work")},
        { "SearchBackwordSwitch",  N_("Search Backward"), N_("Reverse search direction")},
        { "WrapAroundSwitch",      N_("Wrap Around"),     N_("Continue search from the beginning")},
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

static void search_replace_dialog_destroy_widgets(GtkWidget *self)
{
  GtkWidget *tooltips;

  tooltips = GEDA_OBJECT_GET_DATA (self, "tooltips");
  g_object_unref(tooltips);
}

/*!
 * \brief Action Response function for the Search dialogs
 * \par Function Description
 *  This function processes the "response" signals from the action
 *  buttons in the Search dialogs.
 */
static void search_replace_dialog_response(GtkWidget    *ThisDialog,
                                           int           response,
                                           SearchRecord *Search)
{
  char *search_text;
  char *replacement_text;

  /*!@brief Add new text to Search History List */
  void add_search_history(char *new_text) {
    /*! \note: String added to search_history is freed at program exit */
    if (!geda_utility_glist_stri_inlist(search_history, new_text)) {
      search_history = g_list_prepend(search_history, geda_strdup(new_text));
    }
  }

  /*!@brief Retrieve values and settings from Search Dialog controls */
  void unload_dialog() {
    search_text      =  GetGedaComboActiveText(SearchText);
    Search->Case     = !GET_SWITCH_STATE (IgnoreCaseSwitch);
    Search->Whole    =  GET_SWITCH_STATE (WholeWordSwitch);
    Search->Backword =  GET_SWITCH_STATE (SearchBackwordSwitch);
    Search->Wrap     =  GET_SWITCH_STATE (WrapAroundSwitch);
    add_search_history(search_text);
  }

  switch (response) {
  case GEDA_RESPONSE_REJECT: /* FindButt, do not replace, just find next */
    unload_dialog();
    Search->Found = x_find_main_search(search_text, NULL);
    GEDA_FREE(search_text);
    break;

  case GEDA_RESPONSE_APPLY:  /* Replace All , dialog not closed */
    Search->ReplaceAll = TRUE;

  case GEDA_RESPONSE_ACCEPT: /* Replace */
    unload_dialog();
    replacement_text = geda_combo_widget_get_active_text (ReplaceTextCombo);

    /* The search text was added to history, also add the replacement text */
    add_search_history(replacement_text);
    Search->Found = x_find_main_search(search_text, replacement_text);
    GEDA_FREE(search_text);
    GEDA_FREE(replacement_text);
    break;

  case GEDA_RESPONSE_DELETE_EVENT: /* X widget in dialog bar */
  case GEDA_RESPONSE_CANCEL:       /* Close button */
    search_replace_dialog_destroy_widgets(ThisDialog);
    gtk_widget_destroy(ThisDialog);
    break;

  default:
    fprintf (stderr,"%s unhandled case for signal: %d\n", __func__, response);
  }

  Search->ReplaceAll = FALSE; /* This must be enabled by user each loop */
}

/* ------------------------ ComboBox Support Functions ----------------------*/

/*!
 * \brief Search Dialog Combo Responder
 * \par Function Description: This callback function is used to set the
 *  sensitivity of other controls based on combo-box input.
 */
static void search_replace_combo_responder(GtkWidget *widgetCombo, void *data)
{
  int WhichComboBox = (int)(long) (data);

  char *text = GetGedaComboActiveText (widget);

  switch ( WhichComboBox ) {
  case SearchText:
    if ( strlen(text) > 0) {
       gtk_widget_set_sensitive (FindButt, TRUE);
       gtk_widget_set_sensitive (IgnoreCaseSwitch, TRUE);
       gtk_widget_set_sensitive (WholeWordSwitch, TRUE);
       gtk_widget_set_sensitive (SearchBackwordSwitch, TRUE);
       gtk_widget_set_sensitive (WrapAroundSwitch, TRUE);
    }
    else {
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
    if ((geda_combo_box_text_widget_get_text_length (SearchTextCombo) > 0) &&
        (geda_combo_box_text_widget_get_text_length (ReplaceTextCombo) > 0))
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
    fprintf (stderr,"%s Warning: unknown Id: %d\n", __func__, WhichComboBox);
  }

  geda_free(text);

  return;
}

/* ------------------------- Switch Support Functions -----------------------*/

/*!
 * \brief Toggle Search Dialog Switch Images
 * \par Function Description
 *  This function changes images of controls created with create_geda_switch
 *  to the opposite state, i.e. if ON use OFF image and if OFF use ON image.
 *  The functions enables or disables other widgets based on the state of the
 *  switch.
 */
static void search_replace_switch_responder(GtkWidget *widget, int response, ControlID *Control)
{
   bool state = GET_SWITCH_STATE (widget);
   GtkWidget *SwitchImage = get_geda_switch_image(state);

   gtk_button_set_image(GTK_BUTTON (widget), SwitchImage);

   /* We don't have a pointer to the Search structure but this does not
    * matter since we don't need to do anything here, stubbing code here */
   switch (response) {
   case IgnoreCase:
   case WholeWord:
   case SearchBackword:
   case WrapAround:
     break;

   default:
    fprintf (stderr,"%s: Unknown Switch ID: %d\n", __func__,response);
   }

   return;
}

/* ---------------- Search Dialog Initialization Functions ------------------*/

/*!
 * \brief Initialize Search Dialog Controls
 * \par Function Description
 *  This function sets the initial state of the Search Dialog. The toggle
 *  switches are set to the values in the Search Record structure. Controls
 *  for options and all but the Close button are disabled.
 */
static void x_dialog_init_search_replace(GtkWidget    *ThisDialog,
                                         SearchRecord *Search,
                                         const char   *text)
{
  Search->ReplaceAll = FALSE; /* could have been on if struc re-used */

  SetSwitch (IgnoreCase,    !Search->Case);
  SetSwitch (WholeWord,      Search->Whole);
  SetSwitch (SearchBackword, Search->Backword);
  SetSwitch (WrapAround,     Search->Wrap);

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
       lambda (const char* data) {
         LOAD_GEDA_TEXT_COMBO(SearchText,data);
         return FALSE;
       }
       foreach (search_history);
    }
    {
       lambda (const char* data) {
         LOAD_GEDA_TEXT_COMBO(ReplaceText,data);
         return FALSE;
       }
       foreach (search_history);
    }
  }

  if (text) {
    geda_combo_box_text_widget_set_active_text(SearchTextCombo, text);
  }
}

/*!
 * \brief Create Search Dialog Controls
 * \par Function Description
 *  This function creates the Search Dialog and all controls. The Dialog
 *  can be either a Search/Find or a Search and Replace Dialog depending
 *  on the second parameter, find_only_mode. If find_only_mode is TRUE,
 *  then the ReplaceTextCombo, Replace and Replace All button are not made
 *  visible, effectively creating a Find dialog without options to replace.
 *  All of the functionality remains but is not utilize. This make since
 *  because the Search And Replace Dialog can also be used as a Find Dialog
 *  (without actually replacing anything).
 */
static
GtkWidget *x_dialog_create_search_replace_dialog (GtkWindow *parent,
                                                  int find_only_mode)
{
  GtkDialog *ThisDialog;
  GtkWidget *MainDialogVBox;

  GtkWidget *alignment;
  GtkWidget *dialog_action_area;
  GtkWidget *hbox;
  GtkWidget *CloseButt;

  GtkTooltips *tooltips;
  tooltips = gtk_tooltips_new ();

  if (find_only_mode) {
    ThisDialog=NEW_STD_GATTRIB_DIALOG(_(AlternateTitle), AlternateSettings, parent);
  }
  else {
    ThisDialog=NEW_STD_GATTRIB_DIALOG(_(DialogTitle), DialogSettings, parent);
  }

  gtk_window_set_modal ((GtkWindow*)ThisDialog, FALSE);
  gtk_window_set_destroy_with_parent ((GtkWindow*)ThisDialog, TRUE);
  gtk_window_set_type_hint ((GtkWindow*)ThisDialog, GDK_WINDOW_TYPE_HINT_DIALOG);

  MainDialogVBox = ThisDialog->vbox;
  gtk_widget_show (MainDialogVBox);

  /* Create an alignment widget to shift the Search text right */
  alignment = gtk_alignment_new (1, 0, 1, 1);
  gtk_box_pack_start (GTK_BOX (MainDialogVBox), alignment, FALSE, TRUE, 0);
  gtk_widget_show (alignment);

  g_object_set (alignment, "left-padding", 25, NULL);

  hbox = gtk_hbox_new(FALSE, 1);
  gtk_widget_show (hbox);
  geda_container_add (alignment, hbox);

  HSECTION (hbox, InputText);   /* Row 1 */
  GEDA_NEW_TEXT_ENTRY_COMBO (InputText_hbox, SearchText, 306, 24);

  HSECTION (MainDialogVBox, NewText);   /*  Row 2 */
    GEDA_NEW_TEXT_ENTRY_COMBO (NewText_hbox, ReplaceText, 306, 24);

  if (find_only_mode) {
    gtk_widget_hide(ReplaceTextLabel);
    gtk_widget_hide(ReplaceTextCombo);
    gtk_dialog_set_default_response(ThisDialog, GEDA_RESPONSE_ACCEPT);
  }
  else {
    gtk_dialog_set_default_response(ThisDialog, GEDA_RESPONSE_REJECT);
    geda_combo_box_text_set_activate_default((GedaComboBoxText*)SearchTextCombo, 1);
  }

  HXYP_SEPARATOR (MainDialogVBox, Grp3, 5);

  /* Create an alignment widget to shiftSearchOptions row 1 right */
  alignment = gtk_alignment_new (1, 0, 1, 1);
  gtk_box_pack_start (GTK_BOX (MainDialogVBox), alignment, FALSE, TRUE, 0);
  gtk_widget_show (alignment);

  g_object_set (alignment, "left-padding", 55, NULL);

  hbox = gtk_hbox_new(FALSE, 1);
  gtk_widget_show (hbox);
  geda_container_add (alignment, hbox);

  HSECTION (hbox, SearchOptions1);   /*  Row 3 */
    GTK_SWITCH(SearchOptions1_hbox, IgnoreCase, 0, TRUE);
    GTK_SWITCH(SearchOptions1_hbox, WholeWord, 0, TRUE);

  HSECTION (MainDialogVBox, SearchOptions2);   /*  Row 4 */
    GTK_SWITCH(SearchOptions2_hbox, SearchBackword, 0, FALSE);
    GTK_SWITCH(SearchOptions2_hbox, WrapAround, 0, TRUE);

  HXYP_SEPARATOR (MainDialogVBox, Grp4, 10);

  dialog_action_area = ThisDialog->action_area;

  gtk_widget_show(dialog_action_area);

  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area), GTK_BUTTONBOX_END);

  CloseButt = gtk_button_new_from_stock ("gtk-close");
  gtk_widget_show(CloseButt);
  gtk_dialog_add_action_widget (ThisDialog, CloseButt, GEDA_RESPONSE_CANCEL);
  gtk_widget_set_can_default(CloseButt, TRUE);
  gtk_widget_set_size_request (CloseButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, CloseButt, _("Close this dialog"), NULL);

  ReplaceAllButt = gtk_button_new_with_mnemonic (_("Replace All"));

  if (!find_only_mode) {
    gtk_widget_show(ReplaceAllButt);
  }

  gtk_dialog_add_action_widget (ThisDialog, ReplaceAllButt, GEDA_RESPONSE_APPLY);
  gtk_widget_set_can_default(ReplaceAllButt, TRUE);
  gtk_widget_set_size_request (ReplaceAllButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, ReplaceAllButt, _("Replace All and close dialog"), NULL);

  ReplaceButt = gtk_button_new_with_mnemonic (_("Replace"));

  if (!find_only_mode) {
    gtk_widget_show(ReplaceButt);
  }

  gtk_dialog_add_action_widget (ThisDialog, ReplaceButt, GEDA_RESPONSE_ACCEPT);
  gtk_widget_set_can_default(ReplaceButt, TRUE);
  gtk_widget_set_size_request (ReplaceButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, ReplaceButt, _("Replace selected text and continue"), NULL);

  FindButt = gtk_button_new_with_mnemonic (_("Find"));
  gtk_widget_show(FindButt);
  gtk_dialog_add_action_widget (ThisDialog, FindButt, GEDA_RESPONSE_REJECT);
  gtk_widget_set_can_default(FindButt, TRUE);
  gtk_widget_set_size_request (FindButt, DEFAULT_BUTTON_WIDTH, DEFAULT_BUTTON_HEIGHT);
  gtk_tooltips_set_tip (tooltips, FindButt, _("Find next"), NULL);

  /* Store pointers to widgets, for use by get_widget_data(). */
  GEDA_OBJECT_SET_DATA (ThisDialog, ThisDialog,         DialogTitle);
  GEDA_OBJECT_SET_DATA (ThisDialog, MainDialogVBox,     "MainDialogVBox");
  GEDA_OBJECT_SET_DATA (ThisDialog, dialog_action_area, "dialog_action_area");

  GEDA_HOOKUP_OBJECT   (ThisDialog, CloseButt,          "CloseButt");
  GEDA_HOOKUP_OBJECT   (ThisDialog, ReplaceButt,        "ReplaceButt");
  GEDA_HOOKUP_OBJECT   (ThisDialog, ReplaceAllButt,     "ReplaceAllButt");
  GEDA_HOOKUP_OBJECT   (ThisDialog, FindButt,           "FindButt");
  GEDA_OBJECT_SET_DATA (ThisDialog, tooltips,           "tooltips");

  return (GtkWidget*)ThisDialog;
}

/*!
 * \brief Startup Search and Replace Dialog
 * \par Function Description
 *  This is the main function called by either x_find_attribute_value
 *  or x_find_replace_attrib_value to launch a new Search and Replace
 *  Dialog session.
 *
 * \param Search Pointer to st_search search record structure
 * \param text   String to search for or NULL.
 */
void x_dialog_search_replace(SearchRecord *Search, const char *text)
{
  GtkWidget *ThisDialog;

  ThisDialog = x_dialog_create_search_replace_dialog((GtkWindow*)main_window,
                                                     Search->FindOnlyMode);

  gtk_window_set_position ((GtkWindow*)ThisDialog, GTK_WIN_POS_MOUSE);

  x_dialog_init_search_replace(ThisDialog, Search, text);

  GEDA_SIGNAL_CONNECT(ThisDialog, "response",
                      search_replace_dialog_response, Search);

  geda_set_container_border_width (ThisDialog, DIALOG_BORDER_WIDTH);

  gtk_widget_show(ThisDialog);
}

/*********** End of find text dialog box *******/
