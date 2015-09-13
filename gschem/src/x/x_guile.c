
#include "gschem.h"
#include "x_dialog.h"
#include <geda_dialog_controls.h>

#ifndef QUOTE_SYMBOL
#define QUOTE_SYMBOL(symbol) #symbol
#endif

/** \defgroup Guile-Dialog  Guile Dialog
 *  @{ \memberof Systemic-Dialogs
*/

#define COL_PATH 0

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  This function takes the user action and applies it.
 */
void x_dialog_guile_response(GtkWidget *ThisDialog, int response,
                             GschemToplevel *w_current)
{
  GtkWidget  *textentry;
  const char *string;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GEDA_RESPONSE_ACCEPT:
    textentry = GEDA_OBJECT_GET_DATA(ThisDialog, "textentry");
    string = GetEntryText( textentry );
    if (strlen(string) != 0) {
      u_log_message("WTF?\n");
    }
    break;
  default:
    BUG_IMSG ("unhandled case for signal <%d>", response);
  }
}


static GtkTreeModel*
create_and_fill_model (SCM s_load_path)
{
  GtkListStore  *store;
  GtkTreeIter    iter;

  store = gtk_list_store_new (1, G_TYPE_STRING);

  int i;
  int  scm_search_len = (int) scm_ilength (s_load_path);

  for (i = 0 ; i < scm_search_len; i++) {

    char *path;
    SCM elem = scm_list_ref(s_load_path, scm_from_int(i));

    path = scm_to_utf8_string(elem);

    /* Append a row and fill in the path */
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, COL_PATH, path, -1);

    free(path);
  }

  return GTK_TREE_MODEL (store);
}

/*! \brief Adds a treeview for displaying path string.
 *  \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the current guile search paths
 *
 *  \param [in] s_load_path SCM list of paths.
 *
 *  \returns A pointer on the GtkHBox to add to dialog.
 */
static GtkWidget*
x_guile_dialog_path_list (SCM s_load_path)
{
  GtkCellRenderer   *renderer;
  GtkTreeModel      *model;
  GtkWidget         *hbox, *scrolled_window, *treeview;


  /* place the treeview and its caption into a box */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   /* GtkBox */
                                   "homogeneous", TRUE,
                                   "spacing",     8,
                                   NULL));

  /* list guile search path */

  /* create a scrolled window container for a treeview */
  scrolled_window = GTK_WIDGET (g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                              /* GtkScrolledWindow */
                             "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                             "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                  "shadow-type",  GTK_SHADOW_ETCHED_IN,
                                                                NULL));

  treeview = gtk_tree_view_new ();

  renderer = GTK_CELL_RENDERER(g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                              "editable", FALSE, NULL));

  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (treeview),
                                               -1,
                                               "Path",
                                               renderer,
                                               "text", COL_PATH,
                                               NULL);

  model = create_and_fill_model(s_load_path);

  gtk_tree_view_set_model (GTK_TREE_VIEW (treeview), model);

  /* The tree view has acquired its own reference to the model,
   * so we can drop ours. That way the model will */

  g_object_unref (model);

  gtk_container_add (GTK_CONTAINER (scrolled_window), treeview);

  gtk_box_pack_start (GTK_BOX (hbox), scrolled_window, TRUE, TRUE, 0);

  return hbox;
}

/*! \brief Create the Guile dialog
 *  \par Function Description
 *  Create the dialog to translate symbols.
 */
void x_guile_dialog (GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  //GtkWidget  *label;
  GtkWidget  *hbox;
  GtkWidget  *vbox;

  const char *guile_str;
        char *guile_ver;
        char *title;

  guile_str = _("Guile (version:");

  guile_ver = scm_to_utf8_string(scm_version());

  title     = u_string_concat(guile_str, guile_ver, ")",NULL);

  GEDA_FREE(guile_ver);

  ThisDialog = gschem_dialog_new_with_buttons(title,
                                              GTK_WINDOW(w_current->main_window),
                                              GTK_DIALOG_MODAL,
                                              "x-guile", w_current,
                                              GTK_STOCK_CANCEL,
                                              GEDA_RESPONSE_REJECT,
                                              GTK_STOCK_OK,
                                              GEDA_RESPONSE_ACCEPT,
                                              NULL);
  free(title);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_REJECT,
                                          -1);

  gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                  GEDA_RESPONSE_ACCEPT);

  vbox = GTK_DIALOG(ThisDialog)->vbox;

  //gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, FALSE, 0);

  SCM s_load_path;
  SCM s_load_path_var;

  s_load_path_var = scm_c_lookup ("%load-path");
  s_load_path     = scm_variable_ref (s_load_path_var);

  hbox = x_guile_dialog_path_list(s_load_path);

  //gtk_container_add (GTK_CONTAINER (vbox), hbox);

  gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

  gtk_widget_show_all (ThisDialog);

  if (gtk_dialog_run (GTK_DIALOG (ThisDialog)) == GEDA_RESPONSE_ACCEPT) {
    printf("update the damn guile path if modified");
  };

  gtk_widget_destroy(ThisDialog);

}

/***************** End of Guile dialog box ***********************/

/** @} End Group Guile-Dialog */
