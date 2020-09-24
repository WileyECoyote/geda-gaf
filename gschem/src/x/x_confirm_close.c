/* -*- C x_confirm_close.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file x_confirm_close.c
 * \brief Confirm-Close-Dialog Class Definition and Implementatiom Module
 */

#include <gschem.h>

#include <geda_debug.h>

/** \defgroup Confirm-Close-Dialog Confirm Close Dialog Implementation
 *  @{
 *  \ingroup ConfirmCloseDialog Systemic-Dialogs
 *  This group contains routines for the Confirm Close dialog. The
 *  Component Select Dialog implements ConfirmCloseDialog Class and
 *  is used to alert users of unsaved documents when closing documents
 *  or exiting the program. Users are given the option to save the
 *  pages.
*/

#include <x_confirm_close.h>

static void     confirm_close_dialog_class_init   (void *g_class,
                                                   void *g_class_data);
static void     confirm_close_dialog_init         (GTypeInstance *instance,
                                                   void *g_class);
static void     confirm_close_dialog_set_property (GObject      *object,
                                                   unsigned int  property_id,
                                                   const GValue *value,
                                                   GParamSpec   *pspec);
static void     confirm_close_dialog_get_property (GObject      *object,
                                                   unsigned int  property_id,
                                                   GValue       *value,
                                                   GParamSpec   *pspec);
static GObject *confirm_close_dialog_constructor  (GedaType      type,
                                                   unsigned int n_construct_properties,
                                                   GObjectConstructParam *construct_params);
static void     confirm_close_dialog_finalize     (GObject *object);

GList          *confirm_close_dialog_get_selected_pages  (ConfirmCloseDialog *dialog);

static void    *confirm_close_dialog_parent_class = NULL;

/***************** Begin Confirmation dialog box Helpers ****************/

/*!
 * \brief Helps building a list of selected page to save.
 * \par Function Description
 *  This is the <B>GtkTreeModelForeachFunc</B> for function
 *  <B>confirm_close_dialog_get_selected_pages()</B>.
 *
 *  It builds from the tree model a list of Pages for which a save
 *  action has been requested. Each selected page is appended to the
 *  GList pointed by <B>data</B>
 *
 * \param [in] model The tree model.
 * \param [in] path  .
 * \param [in] iter  .
 * \param [in] data  A pointer on a GList* to fill.
 *
 * \returns FALSE to continue walking the tree.
 */
static bool get_selected_pages (GtkTreeModel *model,
                                GtkTreePath  *path,
                                GtkTreeIter  *iter,
                                void         *data)
{
  Page *page;
  bool  save;

  gtk_tree_model_get (model, iter, COLUMN_SAVE, &save, COLUMN_PAGE, &page, -1);

  if (save) {
    if (page != NULL) {
      *(GList**)data = g_list_append (*(GList**)data, page);
    }
    else {
      fprintf(stderr, "ConfirmCloseDialog: page value is NULL\n");
    }
  }

  return FALSE;
}

/*!
 * \brief Returns a list of the selected pages with changes to save.
 * \par Function Description
 *  This function returns the pages that the user has selected in the
 *  confirmation dialog.
 *
 *  The returned list must be freed.
 *
 * \param [in] dialog The dialog.
 *
 * \returns A GList of selected Page* in dialog.
 */
GList*
confirm_close_dialog_get_selected_pages (ConfirmCloseDialog *dialog)
{
  GList *selected = NULL;

  gtk_tree_model_foreach (GTK_TREE_MODEL (dialog->store_unsaved_pages),
                         (GtkTreeModelForeachFunc)get_selected_pages,
                          &selected);

  return selected;
}

/*!
 * \brief Returns the number of pages in the model.
 * \par Function Description
 *  This function determines the number of pages with unsaved changes
 *  from the model.
 *
 * \param [in] model The tree model.
 *
 * \returns The number of pages with unsaved changes.
 */
static int count_pages (GtkTreeModel *model)
{
  GtkTreeIter iter;
  int n_pages;

  gtk_tree_model_get_iter_first (model, &iter);

  for (n_pages = 1; gtk_tree_model_iter_next (model, &iter); n_pages++);

  return n_pages;
}

/*!
 * \brief Returns the name to use for the given page in the model.
 * \par Function Description
 *  This function determines the text to be used to identify a
 *  specific page from the model of pages with unsaved changes.
 *
 *  If <B>piter</B> is NULL, the name for the first page of the model
 *  is returned. Otherwise, it returns the name for the page defined
 *  by the pointed iterator.
 *
 *  The returned value must be freed by caller.
 *
 * \param [in] model The tree model.
 * \param [in] piter A pointer on a GtkTreeIter of model or NULL.
 *
 * \returns The name for the page.
 */
static char *get_page_name (GtkTreeModel *model, GtkTreeIter *piter)
{
  GtkTreeIter iter;
  Page *page;

  g_return_val_if_fail (GTK_IS_TREE_MODEL (model), NULL);

  if (piter == NULL) {
    gtk_tree_model_get_iter_first (model, &iter);
  }
  else {
    iter = *piter;
  }

  gtk_tree_model_get (model, &iter, COLUMN_PAGE, &page, -1);

  /* Do not unreference the Page */

  return geda_file_get_basename_dup (page->filename);;
}

/*!
 * \brief Sets the contents of the name cell in the treeview of dialog.
 * \par Function Description
 *  This functions sets the cell of the treeview with the short name
 *  of the page obtained with <B>get_page_name()</B>.
 *
 * \param [in] tree_column A GtkTreeColumn.
 * \param [in] cell        The GtkCellRenderer that is being rendered by
 *                         tree_column.
 * \param [in] tree_model  The GtkTreeModel being rendered.
 * \param [in] iter        A GtkTreeIter of the current row rendered.
 * \param [in] data        .
 */
static void
confirm_close_dialog_set_page_name (GtkTreeViewColumn *tree_column,
                                    GtkCellRenderer   *cell,
                                    GtkTreeModel      *tree_model,
                                    GtkTreeIter       *iter,
                                    void              *data)
{
  char *page_name;

  page_name = get_page_name (tree_model, iter);
  g_object_set (cell, "text", page_name, NULL);
  GEDA_FREE (page_name);
}

/*!
 * \brief Callback function for the toggled signal of check box in treeview.
 * \par Function Description
 *  This functions changes the value of the save column in the model
 *  for the affected row when user toggles the check box in the
 *  treeview.
 *
 * \param [in] cell_renderer The GtkCellRendererToggle.
 * \param [in] path          The GtkTreePath to the concerned row in model.
 * \param [in] user_data     The dialog as user data.
 */
static void
confirm_close_callback_renderer_toggled (GtkCellRendererToggle *cell_renderer,
                                         char                  *path,
                                         void                  *user_data)
{
  ConfirmCloseDialog *dialog = CLOSE_CONFIRMATION_DIALOG (user_data);
  GtkTreeModel *model;
  GtkTreeIter iter;
  bool save;

  model = GTK_TREE_MODEL (dialog->store_unsaved_pages);

  /* Removed conditional but don't know why, can not find documentation
   * supporting change so re-instating the conditional to "see what happens" */
  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_SAVE, &save,
                      -1);
  gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_SAVE, (save != TRUE),
                      -1);

}

/*!
 * \brief Adds a treeview to confirmation dialog for selecting of pages.
 * \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the dialog model of pages with unsaved changes. The treeview
 *  displays the page names with check boxes.
 *
 * \param [in] dialog The dialog.
 *
 * \returns A pointer on the GtkVBox to add to dialog.
 */
static GtkWidget*
confirm_close_dialog_build_page_list (ConfirmCloseDialog *dialog)
{
  GtkWidget         *vbox, *scrolled_window, *treeview, *label;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;
  const char        *text;

  /* place the treeview and its caption into their own box */
  vbox = g_object_new (GTK_TYPE_VBOX,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing",     8,
                       NULL);

  /* the list of pages with changes */
  /*  - scrolled window as container for the treeview first */
  scrolled_window = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                  /* GtkScrolledWindow */
                                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                  "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                  "shadow-type",       GTK_SHADOW_IN,
                                  NULL);
  /*  - then the treeview */
  /* create model for treeview and populate */
  treeview = g_object_new (GTK_TYPE_TREE_VIEW,
                           /* GtkTreeView */
                           "enable-search",   FALSE,
                           "headers-visible", FALSE,
                           "model", dialog->store_unsaved_pages,
                           NULL);

  renderer = gtk_cell_renderer_toggle_new ();

  g_signal_connect (renderer, "toggled",
                    G_CALLBACK (confirm_close_callback_renderer_toggled),
                    dialog);

  column   = gtk_tree_view_column_new_with_attributes ("Save?",
                                                       renderer,
                                                       "active", COLUMN_SAVE,
                                                       NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  renderer = gtk_cell_renderer_text_new ();
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN, "title", _("Name"), NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           confirm_close_dialog_set_page_name,
                                           NULL, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  geda_container_add (scrolled_window, treeview);

  gtk_box_pack_end (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);

  /* the caption label above the list of pages */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign",          0.0,
                        "yalign",          0.0,
                        /* GtkLabel */
                        "wrap",            TRUE,
                        "mnemonic-widget", treeview,
                        NULL);
  text = _("S_elect the schematics you want to save:");
  gtk_label_set_text_with_mnemonic (GTK_LABEL (label), text);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), treeview);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  return vbox;
}

/******************* End Confirmation dialog box Helpers ****************/

static void
confirm_close_dialog_unsaved_page(ConfirmCloseDialog *dialog, Page *page)
{
  if (page != NULL) {

    GtkTreeIter iter;

    /* add single page to model */
    gtk_list_store_append (dialog->store_unsaved_pages, &iter);

    gtk_list_store_set (dialog->store_unsaved_pages,
                        &iter,
                        COLUMN_SAVE, TRUE,
                        COLUMN_PAGE, page,
                        -1);
  }
}

static void
confirm_close_dialog_unsaved_pages(ConfirmCloseDialog *dialog, GList *list)
{
  GList *p_current;

  /* add set of pages to model */
  for (p_current = (GList*)list; p_current != NULL; NEXT(p_current)) {

    GtkTreeIter iter;

    gtk_list_store_append (dialog->store_unsaved_pages, &iter);

    gtk_list_store_set (dialog->store_unsaved_pages,
                        &iter,
                        COLUMN_SAVE, TRUE,
                        COLUMN_PAGE, p_current->data,
                        -1);
  }
}

static void
confirm_close_dialog_set_property (GObject      *object,
                                   unsigned int  property_id,
                                   const GValue *value,
                                   GParamSpec   *pspec)
{
  ConfirmCloseDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);

  switch(property_id) {
    case PROP_UNSAVED_PAGE:
      confirm_close_dialog_unsaved_page(dialog, g_value_get_object(value));
      break;

    case PROP_UNSAVED_PAGES:
      confirm_close_dialog_unsaved_pages(dialog, g_value_get_pointer(value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
confirm_close_dialog_get_property (GObject     *object,
                                   unsigned int property_id,
                                   GValue      *value,
                                   GParamSpec  *pspec)
{
  ConfirmCloseDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);

  switch(property_id) {
    case PROP_SELECTED_PAGES:
      g_value_set_pointer (value,
                           confirm_close_dialog_get_selected_pages (dialog));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static GObject*
confirm_close_dialog_constructor (GedaType type,
                                  unsigned int n_construct_properties,
                                  GObjectConstructParam *construct_params)
{
  GObject *object;
  ConfirmCloseDialog *dialog;
  GtkWidget *hbox, *image, *vbox, *label;
  GtkTreeIter iter;
  bool ret, single_page;
  char *tmp, *str;
  const char *cstr;

  /* chain up to constructor of parent class */
  object = G_OBJECT_CLASS (confirm_close_dialog_parent_class)->
           constructor (type, n_construct_properties, construct_params);

  dialog = CLOSE_CONFIRMATION_DIALOG (object);

  g_object_set (dialog, /* GtkDialog */
                "has-separator",     FALSE,
                /* GtkWindow */
                "resizable",         TRUE,
                "skip-taskbar-hint", TRUE,
                /* GtkContainer */
                "border-width",      5,
                NULL);

  g_object_set (GTK_DIALOG (dialog)->vbox, /* GtkBox */
                "spacing", 14,
                NULL);

  g_object_set (GTK_DIALOG (dialog)->action_area, /* GtkBox */
                "spacing",      6,
                /* GtkContainer */
                "border-width", 5,
                NULL);

  /* check if there is one or more than one page with changes */
  ret = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (
                                       dialog->store_unsaved_pages),
                                       &iter);
  if(!ret) {
    BUG_MSG ("gtk_tree_model_get_iter_first returned NULL");
    return NULL;
  }

  single_page = !gtk_tree_model_iter_next (GTK_TREE_MODEL (
                                           dialog->store_unsaved_pages),
                                           &iter);

  /* here starts the layout of the dialog */
  hbox = g_object_new (GTK_TYPE_HBOX,
                       /* GtkContainer */
                       "border-width", 5,
                       /* GtkBox */
                       "homogeneous",  FALSE,
                       "spacing",      12,
                       NULL);

  /* warning image */
  image = g_object_new (GTK_TYPE_IMAGE,
                        /* GtkMisc */
                        "xalign",    0.5,
                        "yalign",    0.0,
                        /* GtkImage */
                        "stock",     GTK_STOCK_DIALOG_WARNING,
                        "icon-size", GTK_ICON_SIZE_DIALOG,
                        NULL);

  gtk_box_pack_start (GTK_BOX (hbox), image,  FALSE, FALSE, 0);

  /* vertical box on the right hand side of the dialog */
  vbox = g_object_new (GTK_TYPE_VBOX,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing",     12,
                       NULL);

  /* primary label */
  if (single_page) {
    /* single page */
    char *page_name;

    page_name = get_page_name (GTK_TREE_MODEL (dialog->store_unsaved_pages), NULL);
    tmp = geda_sprintf (
      _("Save changes to the schematic \"%s\" before closing?"), page_name);

    GEDA_FREE (page_name);
  }
  else {
    /* multi page */
    tmp = geda_sprintf (
      _("There are %d schematics with unsaved changes. "
        "Save changes before closing?"),
      count_pages (GTK_TREE_MODEL (dialog->store_unsaved_pages)));
  }

  str = geda_strconcat ("<big><b>", tmp, "</b></big>", NULL);
  GEDA_FREE (tmp);

  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign",     0.0,
                        "yalign",     0.0,
                        "selectable", TRUE,
                        /* GtkLabel */
                        "wrap",       TRUE,
                        "use-markup", TRUE,
                        "label",      str,
                        NULL);
  GEDA_FREE (str);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  if (!single_page) {
    /* more than one page with changes, display each page and offer */
    /* the opportunity to save them before exiting */
    gtk_box_pack_start (GTK_BOX (vbox),
                        confirm_close_dialog_build_page_list (dialog),
                        TRUE, TRUE, 0);
  }

  /* secondary label */
  cstr = _("If you do not save, all your changes will be permanently lost.");
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign",     0.0,
                        "yalign",     0.0,
                        "selectable", TRUE,
                        /* GtkLabel */
                        "wrap",       TRUE,
                        "label",      cstr,
                        NULL);

  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);


  /* Add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("Close without saving"),  GEDA_RESPONSE_NO,
                          GTK_STOCK_CANCEL,           GEDA_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,             GEDA_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_YES,
                                          GEDA_RESPONSE_NO,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GEDA_RESPONSE_YES);

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, TRUE, TRUE, 0);

  /* all done, let's show the contents of the dialog */
  gtk_widget_show_all (hbox);

  return object;
}

static void confirm_close_dialog_finalize (GObject *object)
{
  ConfirmCloseDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);

  if (GTK_IS_LIST_STORE(dialog->store_unsaved_pages)) {
    gtk_list_store_clear (dialog->store_unsaved_pages);
    GEDA_UNREF(dialog->store_unsaved_pages);
  }

  G_OBJECT_CLASS (confirm_close_dialog_parent_class)->finalize (object);
}

/*!
 *  TODO: Update parameter spec strings!
 */
static void
confirm_close_dialog_class_init (void *g_class, void *g_class_data)
{
  GParamSpec *params;

  GObjectClass *gobject_class = G_OBJECT_CLASS (g_class);

  confirm_close_dialog_parent_class = g_type_class_peek_parent (g_class);

  gobject_class->constructor  = confirm_close_dialog_constructor;
  gobject_class->finalize     = confirm_close_dialog_finalize;
  gobject_class->set_property = confirm_close_dialog_set_property;
  gobject_class->get_property = confirm_close_dialog_get_property;

  params = g_param_spec_object  ("unsaved-page",
                               _("unsaved page"),
                                 "",
                                 geda_page_get_type(),
                                 G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE);

  g_object_class_install_property (gobject_class, PROP_UNSAVED_PAGE, params);

  params = g_param_spec_pointer ("unsaved-pages",
                               _("unsaved pages"),
                                 "",
                                 G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE);

  g_object_class_install_property (gobject_class, PROP_UNSAVED_PAGES, params);

  params = g_param_spec_pointer ("selected-pages",
                               _("selected pages"),
                                 "",
                                 G_PARAM_READABLE);

  g_object_class_install_property (gobject_class, PROP_SELECTED_PAGES, params);
}

static void
confirm_close_dialog_init (GTypeInstance *instance, void *g_class)
{
  ConfirmCloseDialog *self = (ConfirmCloseDialog*)instance;

  /* create empty model for treeview */
  self->store_unsaved_pages = gtk_list_store_new (NUM_COLUMNS,
                                                  G_TYPE_BOOLEAN,  /* save? */
                                                  G_TYPE_POINTER); /* page */
}

/*!
 * \brief Function to retrieve ConfirmCloseDialog's Type identifier.
 * \par Function Description
 *  Function to retrieve ConfirmCloseDialog's Type identifier. On the
 *  first call, this registers the ConfirmCloseDialog in the GedaType
 *  system. Subsequently the function returns the saved value from the
 *  first execution.
 *
 * \return GedaType identifier associated with ConfirmCloseDialog.
 */
GedaType confirm_close_dialog_get_type (void)
{
  static GedaType confirm_close_dialog_type = 0;

  if (!confirm_close_dialog_type) {

    static const GTypeInfo confirm_close_dialog_info = {
      sizeof(ConfirmCloseDialogClass),
      NULL,                            /* base_init */
      NULL,                            /* base_finalize */
      confirm_close_dialog_class_init, /* (GClassInitFunc) */
      NULL,                            /* class_finalize */
      NULL,                            /* class_data */
      sizeof(ConfirmCloseDialog),
      0,                               /* n_preallocs */
      confirm_close_dialog_init,       /* (GInstanceInitFunc) */
    };

    confirm_close_dialog_type =
      g_type_register_static (GTK_TYPE_DIALOG,
                              "ConfirmCloseDialog",
                              &confirm_close_dialog_info, 0);
  }

  return confirm_close_dialog_type;
}

/****************** End of Close Confirmation dialog box ****************/

/*!
 * \brief Asks for confirmation before closing a changed page.
 * \par Function Description
 *  This function asks the user to confirm its closing order for
 *  page <B>page</B> while it still has unsaved changes.
 *
 *  It displays a message dialog asking the user to cancel the
 *  closing, or to discard the changes or to save the changes to a
 *  file.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] page      The page to close.
 *
 * \returns TRUE if the page can be closed, FALSE otherwise.
 */
bool
x_confirm_close_changed_page (GschemToplevel *w_current, Page *page)
{
  GtkDialog *dialog;
  bool       result;

  g_return_val_if_fail (geda_page_get_changed(page) > 0, TRUE);

  result = FALSE;

  dialog = g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                         "unsaved-page", page, NULL);

  /* set default response signal, usually triggered by the "Return" key */
  gtk_dialog_set_default_response(dialog, GEDA_RESPONSE_YES);

  switch (gtk_dialog_run (dialog)) {

      case GEDA_RESPONSE_NO:      /* close the page, discard changes */

        result = TRUE;
        break;

      case GEDA_RESPONSE_YES:     /* action selected: save */
        x_window_save_page (w_current, page, page->filename);
        result = TRUE;
        break;

      case GEDA_RESPONSE_CANCEL:  /* action selected: cancel */


        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        /* nothing to do */
        break;
  }
  gtk_widget_destroy (GTK_WIDGET(dialog));

  return result;
}

/*!
 * \brief Asks for confirmation before closing a window.
 * \par Function Description
 *  This function asks the user to confirm closing the given window.
 *
 *  The user is given the possibility to save the pages that currently
 *  have unsaved changes, if any.
 *
 *  It returns TRUE if the user really accepts the close of the
 *  window. Otherwise the user has somehow canceled and the window
 *  must not be closed.
 *
 * \param [in] w_current The toplevel environment.
 * \returns TRUE if the window can be closed, FALSE otherwise.
 */
bool x_confirm_close_window (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList        *iter;
  GtkWidget    *dialog;
  Page         *p_current;
  Page         *keep_page;
  GList        *unsaved_pages, *p_unsaved;
  bool          return_value;

  return_value  = FALSE;
  unsaved_pages = NULL;

  /* Loop through all the pages */
  for (iter = geda_toplevel_get_pages(toplevel); iter != NULL; NEXT(iter))
  {
    /* get ptr to a page */
    p_current = (Page*)iter->data;

    if (geda_page_get_changed(p_current) > 0) {
      /* Add to list of un-saved pages */
      unsaved_pages = g_list_append (unsaved_pages, p_current);
    }
  }

  if (unsaved_pages == NULL) {
    /* no page with unsaved changes, close window */
    return TRUE;
  }

  dialog = g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                        "unsaved-pages", unsaved_pages,
                         NULL);

  g_list_free (unsaved_pages);

  switch ( gtk_dialog_run (GTK_DIALOG (dialog)) ) {
      case GEDA_RESPONSE_NO:
        /* action selected: close without saving */
        /* discard changes, ok to close window */
        return_value = TRUE;
        break;

      case GEDA_RESPONSE_YES:
        /* action selected: save */
        unsaved_pages = NULL;
        g_object_get (dialog, "selected-pages", &unsaved_pages, NULL);
        return_value = TRUE;
        keep_page = geda_toplevel_get_current_page(toplevel);
        for (p_unsaved = unsaved_pages; p_unsaved != NULL; NEXT(p_unsaved))
        {
          p_current = (Page*)p_unsaved->data;

          geda_struct_page_goto (p_current);

          x_window_save_page (w_current, p_current,
                              Current_Page->filename);

          /* if user canceled previous, do not close window */
          return_value &= !p_current->CHANGED;
        }
        /* Switch back to the original page */
        geda_struct_page_goto (keep_page);
        geda_glist_free_full (unsaved_pages, g_object_unref);
        break;

      case GEDA_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        return_value = FALSE;
        break;
  }

  gtk_widget_destroy (dialog);

  return return_value;
}

/** @} endgroup Confirm-Exit-Dialog */