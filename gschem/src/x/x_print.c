/* -*- C x_print.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_print.c
 * \brief Print Dialog.
 */

#include <math.h>

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#include <cairo-pdf.h>

#include <geda_debug.h>

enum
  {
  PROP_FILENAME = 1,
  PROP_COMMAND,
  PROP_PAPERSIZE,
  PROP_ORIENTATION,
  PROP_LAYOUT,
  PROP_USEFILE
  };

/* Private functions */

static void print_dialog_action_radio_toggled    (GtkWidget    *w,
                                                  PrintDialog  *dialog);

static void print_dialog_instance_init                    (PrintDialog  *dialog);
static void print_dialog_instance_init_paper_combobox     (PrintDialog  *d);
static void print_dialog_instance_init_layout_combobox    (PrintDialog  *d);
static void print_dialog_instance_init_orient_combobox    (PrintDialog  *d);
static void print_dialog_set_property            (GObject      *object,
                                                  unsigned int  property_id,
                                            const GValue       *value,
                                                  GParamSpec   *pspec);
static void print_dialog_set_property_comboboxes (PrintDialog  *dialog,
                                                  GedaComboBox *cbox,
                                            const GValue       *value);
static void print_dialog_get_property            (GObject      *object,
                                                  unsigned int  property_id,
                                                  GValue       *value,
                                                  GParamSpec   *pspec);
static void print_dialog_get_property_comboboxes (PrintDialog  *dialog,
                                                  GedaComboBox *cbox,
                                                  GValue       *value);

static void print_dialog_class_init (PrintDialogClass *class);


/*!
 *  \brief Callback function to show file chooser dialog
 *
 *  \par Shows file chooser dialog for user to select PostScript file
 *  to print to.
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static void print_dialog_action_choosefile (GtkWidget   *w,
                                            PrintDialog *dialog)
{
  GtkWidget  *filechooser;
        char *cwd;
  const char *filename;

  filechooser = gtk_file_chooser_dialog_new (_("Select PostScript Filename..."),
                                             GTK_WINDOW (dialog),
                                             GTK_FILE_CHOOSER_ACTION_SAVE,
                                             GTK_STOCK_CANCEL,
                                             GEDA_RESPONSE_CANCEL,
                                             GTK_STOCK_OK,
                                             GEDA_RESPONSE_ACCEPT, NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(filechooser),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  filename = GetEntryText( dialog->fnfield );
  gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (filechooser), filename);

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = getcwd(0,0);
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  free (cwd);

  gtk_dialog_set_default_response(GTK_DIALOG(filechooser),
                                  GEDA_RESPONSE_ACCEPT);

  if (gtk_dialog_run (GTK_DIALOG (filechooser)) == GEDA_RESPONSE_ACCEPT)
  {
    const char *newfilename;

    newfilename =
    gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (filechooser));
    SetEntryText( dialog->fnfield, newfilename);
  }

  gtk_widget_destroy (filechooser);

}

/*!
 * \brief Add Paper Size Combo Box to the Print Dialog
 * \par Private function
 *  Creates and populates a combobox for selecting the paper
 *  size to print to.
 */
static void print_dialog_instance_init_paper_combobox (PrintDialog *d)
{
  GedaComboBox *combobox;

  char *string;
  int   i;

  combobox = GEDA_COMBO_BOX (geda_combo_box_new_text ());
  geda_combo_box_set_active (combobox, -1);

  /* Populate combo box with available paper sizes */
  i = 0;
  string = (char*)geda_struct_papersizes_get (i);
  while (string != NULL) {

    geda_combo_box_insert_text (combobox, i, string);

    i++;
    string = (char*)geda_struct_papersizes_get (i);
  }

  d->papercbox = combobox;
}

/*!
 * \brief Add Layout Combo Box to the Print Dialog
 * \par Private function
 *  Creates and populates a combobox for selecting the layout of the
 *  output to produce.
 */
static void print_dialog_instance_init_layout_combobox (PrintDialog *d)
{
  GtkListStore    *model;
  GtkTreeIter      iter;
  GtkCellRenderer *renderer;

  GtkWidget       *combobox;

  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter, 0, _("Extents with margins"),
                      1, EXTENTS, -1);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter, 0, _("Extents no margins"),
                      1, EXTENTS_NOMARGINS, -1);

  gtk_list_store_append (model, &iter);

  gtk_list_store_set (model, &iter, 0, _("Current Window"),
                      1, WINDOW, -1);

  combobox = geda_combo_box_new_with_model (GTK_TREE_MODEL (model));

  renderer = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox), renderer, TRUE);

  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
                                 renderer, "text", 0);

  d->layoutcbox = GEDA_COMBO_BOX (combobox);
}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  paper orientation.
 *  \par Private function, should not be called by any code
 *  outside x_print.c
 */
static void
print_dialog_instance_init_orient_combobox (PrintDialog *d)
{
  GtkWidget       *combobox;
  GtkListStore    *model;
  GtkCellRenderer *renderer;
  GtkTreeIter      iter;

  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
                      0, _("Landscape"),
                      1, LANDSCAPE,
                     -1);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
                      0, _("Portrait"),
                      1, PORTRAIT,
                     -1);

  combobox = geda_combo_box_new_with_model (GTK_TREE_MODEL (model));

  renderer = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox), renderer, TRUE);

  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
                                 renderer, "text", 0);

  d->orientcbox = GEDA_COMBO_BOX (combobox);
}

/*!
 *  \brief Handle the user clicking on radio buttons to select print
 *  destination.
 *
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static void
print_dialog_action_radio_toggled (GtkWidget *w, PrintDialog *dialog)
{
  if (w == GTK_WIDGET (dialog->cmdradio))  {

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->cmdfield), GetToggleState (w));
  }
  else if (w == GTK_WIDGET (dialog->fileradio)) {

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->fnfield), GetToggleState (w));

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->saveasbutton), GetToggleState (w));
  }
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_set_property (GObject *object,
                           unsigned int property_id,
                           const GValue *value, GParamSpec *pspec)
{
  PrintDialog *dialog = PRINT_DIALOG (object);
  bool file_active = FALSE;

  switch (property_id)
  {
    case PROP_FILENAME:
      SetEntryText( dialog->fnfield, g_value_get_string (value));
      return;

    case PROP_COMMAND:
      SetEntryText( dialog->cmdfield,  g_value_get_string (value));
      return;

    case PROP_PAPERSIZE:
      geda_combo_box_set_active (dialog->papercbox,
                                g_value_get_int (value));
      return;

    case PROP_ORIENTATION:
      print_dialog_set_property_comboboxes (dialog,
                                            dialog->orientcbox,
                                            value);
      return;

    case PROP_LAYOUT:
      print_dialog_set_property_comboboxes (dialog,
                                            dialog->layoutcbox,
                                            value);
      return;

    case PROP_USEFILE:
      file_active = g_value_get_boolean (value);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->fileradio),
                                    file_active);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->cmdradio),
                                    !file_active);
      return;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void print_dialog_set_property_comboboxes (PrintDialog  *dialog,
                                                  GedaComboBox *cbox,
                                                  const GValue *value)
{
  GtkTreeIter iter;
  GtkTreeModel *model;

  model = geda_combo_box_get_model (cbox);
  gtk_tree_model_get_iter_first (model, &iter);

  do {
    GValue temp_value = {0, }; /* Make sure it's blank*/
    gtk_tree_model_get_value (model, &iter, 1, &temp_value);

    if (g_value_get_int (&temp_value) == g_value_get_int (value))
    {
      geda_combo_box_set_active_iter (cbox, &iter);
      return;
    }

  } while (gtk_tree_model_iter_next (model, &iter));

  geda_combo_box_set_active (cbox, 0);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void print_dialog_get_property (GObject     *object,
                                       unsigned int property_id,
                                       GValue      *value,
                                       GParamSpec  *pspec)
{
  PrintDialog *dialog = PRINT_DIALOG (object);
  bool file_active = FALSE;

  switch (property_id)
  {
    case PROP_FILENAME:
      g_value_set_string (value, GetEntryText( dialog->fnfield ));
      return;

    case PROP_COMMAND:
      g_value_set_string (value, GetEntryText( dialog->cmdfield ));
      return;

    case PROP_PAPERSIZE:
      g_value_set_int (value, geda_combo_box_get_active(dialog->papercbox));
      return;

    case PROP_ORIENTATION:
      print_dialog_get_property_comboboxes (dialog,
                                            dialog->orientcbox,
                                            value);
      return;

    case PROP_LAYOUT:
      print_dialog_get_property_comboboxes (dialog,
                                            dialog->layoutcbox,
                                            value);
      return;

    case PROP_USEFILE:
      file_active =
      gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (dialog->fileradio));
      g_value_set_boolean (value, file_active);
      return;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void print_dialog_get_property_comboboxes (PrintDialog  *dialog,
                                                  GedaComboBox *cbox,
                                                  GValue       *value)
{
  GValue        temp_value = {0, };
  GtkTreeModel *model;
  GtkTreeIter   iter;

  model = geda_combo_box_get_model (cbox);

  geda_combo_box_get_active_iter (cbox, &iter);
  gtk_tree_model_get_value (model, &iter, 1, &temp_value);
  g_value_copy (&temp_value, value);
  g_value_unset (&temp_value);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *  \bug Hardcoded 'magic' numbers in this function
 *  \todo Update parameter spec strings!
 */
static void
print_dialog_class_init (PrintDialogClass *class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  gobject_class->set_property = print_dialog_set_property;
  gobject_class->get_property = print_dialog_get_property;

  g_object_class_install_property (gobject_class, PROP_FILENAME,
                                   g_param_spec_string ("filename",
                                                        "Filename",
                                                        "The filename to be printed",
                                                        "",
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_COMMAND,
                                   g_param_spec_string ("command",
                                                        "Print command",
                                                        "The command-line print command",
                                                        "lpr",
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_PAPERSIZE,
                                   g_param_spec_int ("papersize",
                                                     "Paper size",
                                                     "papersize",
                                                     0, G_MAXINT, 0,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_ORIENTATION,
                                   g_param_spec_int ("orientation",
                                                     "Orientation",
                                                     "The page orientation",
                                                     0, G_MAXINT, 0,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_LAYOUT,
                                   g_param_spec_int ("layout",
                                                     "Layout",
                                                     "Print area with or without margins",
                                                     0, G_MAXINT, 0,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_USEFILE,
                                   g_param_spec_boolean ("usefile",
                                                         "usefile",
                                                         "usefile",
                                                         FALSE,
                                                         G_PARAM_READWRITE));
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void print_dialog_instance_init (PrintDialog *dialog)
{
  GtkWidget *box;
  GtkWidget *frame;
  GtkWidget *settingstable, *desttable;
  GtkWidget *label;
  GtkWidget *print_button;

  dialog->instance_type = print_dialog_get_type();

  /* Initialize properties */
  g_object_set (dialog,
                /* GtkWindow */
                "title", _("Print..."),
                "modal", TRUE, "destroy-with-parent", TRUE, NULL);

  /* Setup hbox for two main panes */
  box = gtk_vbox_new (FALSE, 2);
  geda_container_add (GTK_DIALOG (dialog)->vbox, box);

  /* Upper frame */
  frame = gtk_frame_new (_("Settings"));
  geda_set_container_border_width (frame, 3);
  geda_container_add (box, frame);

  /* Upper table with drop-down menus & labels
   * Left-hand column contains labels, right-hand contains comboboxes*/
  settingstable = gtk_table_new (2, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (settingstable), 5);
  gtk_table_set_row_spacings (GTK_TABLE (settingstable), 5);
  geda_set_container_border_width (settingstable, 5);
  geda_container_add (frame, settingstable);

  label = geda_aligned_label_new (_("Output paper size:"), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
                    label,
                    0, 1, 0, 1, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_instance_init_paper_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
                    GTK_WIDGET (dialog->papercbox),
                    1, 2, 0, 1, GTK_FILL, 0, 0, 0);

  label = geda_aligned_label_new (_("Layout:"), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
                    label,
                    0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_instance_init_layout_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
                    GTK_WIDGET (dialog->layoutcbox),
                    1, 2, 1, 2, GTK_FILL, 0, 0, 0);

  label = geda_aligned_label_new (_("Orientation:"), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
                    label,
                    0, 1, 2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_instance_init_orient_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
                    GTK_WIDGET (dialog->orientcbox),
                    1, 2, 2, 3, GTK_FILL, 0, 0, 0);

  /* Lower frame */
  frame = gtk_frame_new (_("Destination"));
  geda_set_container_border_width (frame, 3);
  geda_container_add (box, frame);

  /* Table with destination selectors */
  desttable = gtk_table_new (3, 2, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (desttable), 5);
  gtk_table_set_row_spacings (GTK_TABLE (desttable), 5);
  geda_set_container_border_width (desttable, 5);
  geda_container_add (frame, desttable);

  /* Widgets for printing to file */
  dialog->fileradio =
  GTK_RADIO_BUTTON (gtk_radio_button_new_with_label (NULL, _("File:")));
  gtk_table_attach (GTK_TABLE (desttable),
                    GTK_WIDGET (dialog->fileradio),
                    0, 1, 0, 1, GTK_FILL, GTK_EXPAND, 0, 0);
  g_signal_connect (dialog->fileradio,
                    "toggled",
                    G_CALLBACK (print_dialog_action_radio_toggled),
                    dialog);

  dialog->fnfield = GTK_ENTRY (gtk_entry_new ());
  gtk_table_attach (GTK_TABLE (desttable),
                    GTK_WIDGET (dialog->fnfield),
                    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, 0, 0, 0);

  dialog->saveasbutton = GTK_BUTTON(gtk_button_new());
  geda_container_add(dialog->saveasbutton,
                     gtk_image_new_from_stock(GTK_STOCK_OPEN,
                                             GTK_ICON_SIZE_SMALL_TOOLBAR));
  gtk_button_set_relief(GTK_BUTTON(dialog->saveasbutton), GTK_RELIEF_NONE);

  gtk_table_attach (GTK_TABLE (desttable),
                    GTK_WIDGET (dialog->saveasbutton), 2, 3, 0, 1,
                    GTK_FILL, 0, 0, 0);
  g_signal_connect (dialog->saveasbutton,
                    "clicked",
                    G_CALLBACK (print_dialog_action_choosefile), dialog);

  /* Widgets for printing to command */
  dialog->cmdradio =
  GTK_RADIO_BUTTON (gtk_radio_button_new_with_label_from_widget
  (dialog->fileradio, _("Command:")));
  gtk_table_attach (GTK_TABLE (desttable),
                    GTK_WIDGET (dialog->cmdradio),
                    0, 1, 1, 2,  GTK_FILL, GTK_EXPAND, 0, 0);
  g_signal_connect (dialog->cmdradio,
                    "toggled",
                    G_CALLBACK (print_dialog_action_radio_toggled),
                    dialog);

  dialog->cmdfield = GTK_ENTRY (gtk_entry_new ());
  gtk_table_attach (GTK_TABLE (desttable), GTK_WIDGET (dialog->cmdfield),
                    1, 3, 1, 2, GTK_EXPAND | GTK_FILL, 0, 0, 0);

  /* Add "Cancel" and "Print" buttons */
  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CANCEL, GEDA_RESPONSE_REJECT);
  print_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                                        GTK_STOCK_PRINT, GEDA_RESPONSE_ACCEPT);

  gtk_widget_set_tooltip_text(print_button, _("Print the current document"));
  gtk_widget_grab_focus(print_button);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_REJECT,
                                          -1);

  /* Set initial radiobutton selection */
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->cmdradio), TRUE);
}

/*!
 * \brief Function to retrieve PrintDialog's Type identifier.
 * \par Function Description
 *  Function to retrieve PrintDialog's Type identifier. On the first call,
 *  this registers the pagesel in the GedaTypesystem. Subsequently
 *  the functions returns the saved value from its first execution.
 *
 * \return the Type identifier associated with PrintDialog.
 */
GedaType print_dialog_get_type (void)
{
  static GedaType print_dialog_type = 0;

  if (!print_dialog_type) {

    static const GTypeInfo print_dialog_info = {
      sizeof(PrintDialogClass),
      NULL,                      /* base_init */
      NULL,                      /* base_finalize */
      (GClassInitFunc) print_dialog_class_init,
      NULL,                      /* class_finalize */
      NULL,                      /* class_data */
      sizeof(PrintDialog),
      0,                      /* n_preallocs */
      (GInstanceInitFunc) print_dialog_instance_init,
    };
    print_dialog_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                                "PrintDialog",
                                                &print_dialog_info, 0);
  }

  return print_dialog_type;
}

/*!
 * \brief Check if an object is a PrintDialog
 * \par Function Description
 *  Ensures dialog is a valid G_Object and compares signature
 *  to print dialog type.
 * \return TRUE if \a dialog is a valid PrintDialog
 */
bool
is_a_print_dialog (PrintDialog *dialog)
{
  if (G_IS_OBJECT(dialog)) {
    return (print_dialog_get_type() == dialog->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Display the Print Dialog.
 * \par Function Description
 *  Displays the Print dialog, allowing the user to set several
 *  options, like paper size and orientation. If the user hits
 *  "Print", the document is written to the destination.
 *
 * \param[in] w_current  GschemToplevel structure.
 * \param[in] filename   Suggested output filename.
 *
 * \todo There should be an option to use the GTK or system
 *       dialog in the configuration.
 */
void x_print_setup (GschemToplevel *w_current, char *filename)
{
  GedaToplevel *toplevel = w_current->toplevel;

  char *command = w_current->print_command;
  int   orient  = toplevel->print_orientation;
  int   type    = toplevel->print_output_extents;

  int   paperidx, x, y, result;

  bool  usefile = FALSE;

  GtkDialog *dialog;

  /* Work out current paper size by iterating through available paper
   * sizes.  Set the default paper size as the active selection */

  /* FIXME: ought to have a GedaToplevel property containing
   * default paper size name, this is somewhat hackish. No
   * better way of doing it with current implementation of
   * varying paper size though. */
  paperidx = 0;

  while (TRUE) {

    char *string = (char*)geda_struct_papersizes_get (paperidx);

    geda_struct_papersizes_get_size (string, &x, &y);

    if ((x == toplevel->paper_width) && (y == toplevel->paper_height)) {
      break;
    }

    if (string == NULL) {
      paperidx = 0;
      break;
    }

    paperidx++;
  }

  /* Create a print dialog, find out whether the user clicks Print or
   *    Cancel, and then print or return accordingly */
  dialog = g_object_new (TYPE_PRINT_DIALOG,
                         "command", command,
                         "filename", filename,
                         "papersize", paperidx,
                         "orientation", orient,
                         "type", type,
                         "usefile", usefile,
                         /* GschemDialog */
                         "settings-name", "print",
                         "gschem-toplevel", w_current,
                         NULL);
  gtk_widget_show_all (GTK_WIDGET (dialog));

  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GEDA_RESPONSE_ACCEPT);

  gtk_window_set_transient_for(GTK_WINDOW(dialog), w_current->main_window);

  result = gtk_dialog_run (dialog);

  if (result == GEDA_RESPONSE_ACCEPT) {

    Page     *page;
    GArray   *color_map;
    char     *destination;

    /* Extract values from dialog and set the paper size */
    g_object_get (dialog,
                  "command", &command,
                  "filename", &filename,
                  "papersize", &paperidx,
                  "orientation", &toplevel->print_orientation,
                  "type", &toplevel->print_output_extents,
                  "usefile", &usefile,
                  NULL);

    geda_struct_papersizes_get_size (geda_struct_papersizes_get (paperidx),
                           &toplevel->paper_width,
                           &toplevel->paper_height);

    /* de select everything first */
    o_select_unselect_all( w_current );

    page = geda_toplevel_get_current_page(toplevel);

    if (usefile && filename[0]) {

      color_map = geda_color_get_print_map();

      /* Print to file */
      destination = filename;

      result = geda_file_print_file (toplevel, page, color_map, filename);

      g_array_free (color_map, TRUE);
    }
    else if (command[0]) {

      color_map = geda_color_get_print_map();

      /* Print to command and save command for later use. */
      destination = command;

      result = geda_file_print_command (toplevel, page, color_map, command);

      GEDA_FREE (w_current->print_command);
      w_current->print_command = geda_utility_string_strdup (command);

      g_array_free (color_map, TRUE);
    }
    else {

      u_log_message (_("No print destination specified\n"));
      return;
    }

    /* Check whether it worked */
    if (result) {

      char *bold_msg;

      u_log_message ("%s \"%s\"\n", _("Cannot print current schematic to"), destination);


      bold_msg = geda_sprintf ("<b>%s.</b>", _("An error occurred while printing"));

      /* Inform user */
      titled_pango_error_dialog (bold_msg,
                               _("The log may contain more information."),
                               _("Print Error"));
      GEDA_FREE(bold_msg);
    }
    else {

      u_log_message ("%s \"%s\"\n", _("Printed current schematic to"), destination);
    }
  }

  /* We do not need the dialog any more */
  gtk_widget_destroy (GTK_WIDGET (dialog));

}

/* ------------------------ End Print Old Dialogs -------------------------- */
#define DEFAULT_PDF_SIZE 256

/*! \def DEFAULT_ADOBE_PDF_PPI
 *  \brief
 *  PDF scale used by Adobe is fixed and equal to 72 ppi. */
#define DEFAULT_ADOBE_PDF_PPI 72

/*! \def DEFAULT_GSCHEM_PPI
 *  \brief Gschem Points Per Inch
 * Scaling factor Set 1 gschem point = 1 mil, i.e. 1000 points = 1 inch.
 * See requirements: http://wiki.geda-project.org/geda:file_format_spec
 */
#define DEFAULT_GSCHEM_PPI 1000

#define CFG_GROUP_PRINTING "gschem.printing"
#define CFG_KEY_PRINTING_ORIENTATION "layout"
#define CFG_KEY_PRINTING_PAPER "paper"
#define CFG_KEY_PRINTING_MONOCHROME "monochrome"

/*!
 * \brief Create a default page setup for a schematic page.
 * \par Function Description
 *  Creates and returns a new GtkPageSetup for \a page, taking into
 *  account the requested \a paper_size_name.  If \a paper_size_name is
 *  NULL, the system default paper size is used. The \a orientation may
 *  be LANDSCAPE, PORTRAIT or AUTOLAYOUT.  If \a AUTOLAYOUT is chosen,
 *  the page orientation that best fits the page contents is chosen.
 *
 * \param toplevel A GedaToplevel structure.
 * \param page     The Page to generate a page setup for.
 *
 * \returns A newly-created page setup.
 */
static GtkPageSetup *x_print_default_page_setup (GedaToplevel *toplevel, Page *page)
{
  GtkPageSetup *setup = gtk_page_setup_new ();
  GtkPaperSize *papersize;
  EdaConfig    *cfg;
  char *paper, *orientation;

  /* Get configuration values */
  cfg =         eda_config_get_context_for_path (page->filename);
  paper =       eda_config_get_string (cfg, CFG_GROUP_PRINTING,
                                       CFG_KEY_PRINTING_PAPER, NULL);
  orientation = eda_config_get_string (cfg, CFG_GROUP_PRINTING,
                                       CFG_KEY_PRINTING_ORIENTATION, NULL);

  /* If the paper size is valid, set it up with default margins. */
  papersize = gtk_paper_size_new (paper);
  if (papersize != NULL) {
    gtk_page_setup_set_paper_size_and_default_margins (setup, papersize);
  }

  if (g_strcmp0 (orientation, "landscape") == 0) {
    gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_LANDSCAPE);
  }
  else if (g_strcmp0 (orientation, "portrait") == 0) {
    gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_PORTRAIT);
  }
  else if (orientation == NULL || g_strcmp0 (orientation, "auto") == 0) {

    int status, wx_min, wy_min, wx_max, wy_max;

    /* Automatically choose the orientation that fits best */
    status = geda_object_get_bounds_list (geda_struct_page_get_objects (page),
                                &wx_min, &wy_min, &wx_max, &wy_max);

    if (!status || (wx_max - wx_min) > (wy_max - wy_min)) {
      /* Default to landscape */
      gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_LANDSCAPE);
    }
    else {
      gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_PORTRAIT);
    }
  }

  GEDA_FREE (paper);
  GEDA_FREE (orientation);
  return setup;
}

/*!
 * \brief Draw a page.
 * \par Function Description
 *  Draws the \a page on the Cairo context \a cr, which should have
 *  dimensions \a cr_width and \a cr_height.  If the Pango context \a
 *  pc is provided, it is used for rendering of text.  The parameter \a
 *  is_color controls whether to enable color printing, and \a
 *  is_raster should be set if drawing to a raster surface such as an
 *  image.
 *
 * \param toplevel A GedaToplevel structure.
 * \param page     The Page to be rendered.
 * \param cr       The Cairo context to render to.
 * \param pc       A Pango context for text rendering, or NULL.
 * \param cr_width The width of the drawing area.
 * \param cr_height The height of the drawing area.
 * \param is_color TRUE if drawing should be in color; FALSE otherwise.
 * \param is_raster TRUE if drawing to a raster image surface; FALSE otherwise.
 */
static void x_print_draw_page (GedaToplevel *toplevel, Page *page,
                               cairo_t *cr, PangoContext *pc,
                               double cr_width, double cr_height,
                               bool is_color, bool is_raster)
{
  EdaRenderer *renderer;
  cairo_matrix_t mtx;
  GArray *color_map;
  int status, wx_min, wy_min, wx_max, wy_max;
  double w_width, w_height, scale;
  GList *iter;

  /* First, calculate a transformation matrix for the cairo
   * context. We want to center the extents of the page in the
   * available page area. */
  status = geda_object_get_bounds_list (geda_struct_page_get_objects (page),
                                          &wx_min, &wy_min, &wx_max, &wy_max);
  /* If there are no printable objects, draw nothing. */
  if (!status) return;

  w_width  = wx_max - wx_min;
  w_height = wy_max - wy_min;

  scale = fmin (cr_width / w_width, cr_height / w_height);

  cairo_matrix_init (&mtx,
                     scale, 0,
                     0, -scale,
                     - (wx_min + 0.5*w_width) * scale + 0.5*cr_width,
                       (wy_min + 0.5*w_height) * scale + 0.5*cr_height);

  /* Second, get the color map. If no color printing is desired,
   * transform the print color map into a black-and-white color map by
   * making the background color transparent and replacing all other
   * enabled colors with solid black. */

  color_map = geda_color_get_print_map();

  if (!is_color) {
    int i,len;
    len = color_map->len;
    for (i = 0; i < len; i++) {

      COLOR *c = &g_array_index (color_map, COLOR, i);

      if (!c->enabled) continue;

      /* Disable background color & fully-transparent colors */
      if (c->a == 0 || i == BACKGROUND_COLOR) {
        c->enabled = FALSE;
        continue;
      }

      /* Set any remaining colors solid black */
      c->r = 0;
      c->g = 0;
      c->b = 0;
      c->a = ~0;
    }
  }

  /* Thirdly, create and initialise a renderer */
  renderer = g_object_new (EDA_TYPE_RENDERER,
                           "pango-context", pc,
                           "cairo-context", cr,
                           "color-map", color_map,
                           "render-flags", is_raster ? EDA_RENDERER_FLAG_HINTING : 0,
                           NULL);

  /* if B&W then all colors in map except BACKGROUND_COLOR were set to black
   * but marks and enpoints may not be using the stock map so ...*/
  if (!is_color) {
    GdkColor black;
    gdk_color_parse ( "black", &black);
    eda_renderer_set_junction_color (renderer, &black);
    eda_renderer_set_net_endpoint_color (renderer, &black);
  }

  /* Finally, actually do drawing */
  cairo_save (cr);
  cairo_transform (cr, &mtx);

  /* Draw background */
  eda_cairo_set_source_color (cr, BACKGROUND_COLOR, color_map);
  cairo_paint (cr);

  /* Draw all objects and cues */
  for (iter = (GList *) geda_struct_page_get_objects (page);
       iter != NULL;
       iter = g_list_next (iter)) {
    eda_renderer_draw (renderer, (GedaObject*) iter->data);
  }

  for (iter = (GList *) geda_struct_page_get_objects (page);
       iter != NULL;
       iter = g_list_next (iter)) {
    eda_renderer_draw_cues (renderer, (GedaObject*) iter->data);
  }

  cairo_restore (cr);

  GEDA_UNREF (renderer);
  g_array_free (color_map, TRUE);
}

/*! Drawing callback for use with GtkPrintOperation. */
static void
draw_page__print_operation (GtkPrintOperation *print,
                            GtkPrintContext *context,
                            int page_nr,
                            void * user_data)
{
  GschemToplevel *w_current = (GschemToplevel*) user_data;
  Page           *page;
  cairo_t        *cr;
  PangoContext   *pc;
  EdaConfig      *cfg;

  double width, height;
  bool   is_color;

  /* Find the page data */
  g_return_if_fail (page_nr != 1);

  page = w_current->toplevel->page_current;

  g_return_if_fail (page != NULL);

  /* Get cairo & pango contexts */
  cr = gtk_print_context_get_cairo_context (context);
  pc = gtk_print_context_create_pango_context (context);

  width  = gtk_print_context_get_width (context);
  height = gtk_print_context_get_height (context);

  /* Find out if colour printing is enabled */
  cfg      = eda_config_get_context_for_path (page->filename);
  is_color = !eda_config_get_boolean (cfg, CFG_GROUP_PRINTING,
                                      CFG_KEY_PRINTING_MONOCHROME, NULL);

  x_print_draw_page (w_current->toplevel, page, cr, pc,
                     width, height, is_color, FALSE);

  /* Clean up */
  GEDA_UNREF (pc);
}

/*!
 * \brief Export a print-style PDF file of the current page.
 * \par Function Description
 *  Exports the current page as a PDF file to \a filename. The
 *  export is carried out using a normal paper size and margins,
 *  as if printing. This function is associated with the action
 *  file-write-pdf, which is similar to the gschem-pdf action
 *  except this function always writes the entire page.
 *
 * \param w_current A GschemToplevel structure.
 * \param filename  The filename for generated PDF.
 *
 * \returns TRUE if the operation was successful.
 */
bool
x_print_export_pdf_page (GschemToplevel *w_current, const char *filename)
{
  Page            *page;
  cairo_surface_t *surface;
  cairo_status_t   status;
  cairo_t         *cr;
  GtkPageSetup    *setup;
  EdaConfig       *cfg;

  double width, height;
  bool   is_color;
  bool   result;

  page = w_current->toplevel->page_current;

  setup  = x_print_default_page_setup (w_current->toplevel, page );
  width  = gtk_page_setup_get_paper_width (setup, GTK_UNIT_POINTS);
  height = gtk_page_setup_get_paper_height (setup, GTK_UNIT_POINTS);

  surface = cairo_pdf_surface_create (filename, width, height);
  cr      = cairo_create (surface);

  cairo_translate (cr, gtk_page_setup_get_left_margin (setup, GTK_UNIT_POINTS),
                   gtk_page_setup_get_top_margin (setup, GTK_UNIT_POINTS));

  width  = gtk_page_setup_get_page_width  (setup, GTK_UNIT_POINTS);
  height = gtk_page_setup_get_page_height (setup, GTK_UNIT_POINTS);

  /* Find out if colour printing is enabled */
  cfg = eda_config_get_context_for_path (page->filename);
  is_color = !eda_config_get_boolean (cfg, CFG_GROUP_PRINTING,
                                      CFG_KEY_PRINTING_MONOCHROME, NULL);

  x_print_draw_page (w_current->toplevel, page,
                     cr, NULL, width, height, is_color, FALSE);

  cairo_destroy (cr);
  cairo_surface_finish (surface);

  status = cairo_surface_status (surface);
  if (status != CAIRO_STATUS_SUCCESS) {
    const char *err_str = cairo_status_to_string (status);
    fprintf(stderr, "%s '%s': %s\n", _("Failed to write PDF to"), filename, err_str);
    result = FALSE;
  }
  else {
    result = TRUE;
  }

  GEDA_UNREF (setup);
  cairo_surface_destroy (surface);
  return result;
}

/*!
 * \brief Export a figure-style PDF file of the current page.
 * \par Function Description
 *  Exports the current page as a PDF file to \a filename. The export
 *  is carried out using a page size matching the size of the visible
 *  extents of the schematic page.
 *
 * \param w_current A GschemToplevel structure.
 * \param filename  The filename for generated PDF.
 *
 * \returns TRUE if the operation was successful.
 */
bool x_print_export_pdf (GschemToplevel *w_current, const char *filename)
{
  cairo_surface_t *surface;
  cairo_status_t   cr_status;
  cairo_t         *cr;

  int    wx_min, wy_min, wx_max, wy_max;
  int    result, status;
  double width, height;

  /* First, calculate a transformation matrix for the cairo
   * context. We want to center the extents of the page in the
   * available page area. */
  status = geda_object_get_bounds_list (
           geda_struct_page_get_objects (w_current->toplevel->page_current),
           &wx_min, &wy_min, &wx_max, &wy_max);

  if (status) {
    width = (wx_max - wx_min) * DEFAULT_ADOBE_PDF_PPI / DEFAULT_GSCHEM_PPI;
    height = (wy_max - wy_min) * DEFAULT_ADOBE_PDF_PPI / DEFAULT_GSCHEM_PPI;
  }
  else {
    /* Fallback size if there are no drawable objects */
    width = height = DEFAULT_PDF_SIZE;
  }

  surface = cairo_pdf_surface_create (filename, width, height);
  cr = cairo_create (surface);

  x_print_draw_page (w_current->toplevel, w_current->toplevel->page_current,
                     cr, NULL, width, height,
                     w_current->toplevel->image_color, FALSE);

  cairo_destroy (cr);
  cairo_surface_finish (surface);

  cr_status = cairo_surface_status (surface);
  if (cr_status != CAIRO_STATUS_SUCCESS) {
    const char *err_str = cairo_status_to_string (cr_status);
    fprintf(stderr, "%s '%s': %s\n", _("Failed to write PDF to"), filename, err_str);
    result = FALSE;
  }
  else {
    result = TRUE;
  }

  cairo_surface_destroy (surface);

  return result;
}

/*!
 * \brief Show a print dialog and print current page if requested.
 * \par Function Description
 *  Shows a standard print dialog, and allows the user to print the current page.
 *
 * \param w_current A GschemToplevel structure.
 */
void x_print (GschemToplevel *w_current)
{
  static GtkPrintSettings *settings = NULL;
  GtkPageSetup *setup;
  GtkPrintOperation *print;
  GtkPrintOperationResult res;
  GError *err = NULL;
  int num_pages = 1;

  /* Create the print operation and set it up */
  print = g_object_new (GTK_TYPE_PRINT_OPERATION,
                        "n-pages", num_pages,
                        "use-full-page", FALSE,
                        "unit", GTK_UNIT_POINTS,
                        NULL);

  if (settings != NULL) {
    gtk_print_operation_set_print_settings (print, settings);
  }

  setup = x_print_default_page_setup (w_current->toplevel,
                                      w_current->toplevel->page_current);
  gtk_print_operation_set_default_page_setup (print, setup);

  g_signal_connect (print, "draw_page", G_CALLBACK (draw_page__print_operation),
                    w_current);

  res = gtk_print_operation_run (print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                 w_current->main_window, &err);

  if (res == GTK_PRINT_OPERATION_RESULT_ERROR) {

    char *bold_msg;

    /* Log the error */
    u_log_message("%s:\n%s", _("Error printing file"), err->message);

    bold_msg = geda_sprintf ("<b>%s.</b>", _("An error occurred while printing"));

    /* If printing failed due to an error, inform the user */
    titled_pango_error_dialog (bold_msg,  err->message, _("Print Error"));

    GEDA_FREE(bold_msg);

    /* clear error */
    g_error_free(err);

  }
  else if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
    /* We're supposed to store the print settings, so do that */
    if (settings != NULL) {
      GEDA_UNREF (settings);
    }
    settings = g_object_ref (gtk_print_operation_get_print_settings (print));
  }

  /* Clean up */
  GEDA_UNREF (print);
}
