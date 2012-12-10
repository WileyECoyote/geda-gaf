/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <math.h>

#include "gschem.h"
#include <cairo-pdf.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

enum
  {
  PROP_FILENAME = 1,
  PROP_COMMAND,
  PROP_PAPERSIZE,
  PROP_ORIENTATION,
  PROP_TYPE,
  PROP_USEFILE
  };

/* Private functions */

static void print_dialog_action_radio_toggled (GtkWidget * w,
                                              PrintDialog * dialog);

static void print_dialog_init (PrintDialog * dialog);
static void print_dialog_init_paper_combobox (PrintDialog * d);
static void print_dialog_init_type_combobox (PrintDialog * d);
static void print_dialog_init_orient_combobox (PrintDialog * d);
static void print_dialog_set_property (GObject * object, guint property_id,
                                      const GValue * value,
                                      GParamSpec * pspec);
static void print_dialog_set_property_comboboxes (PrintDialog *dialog,
						  GtkComboBox *cbox,
						  const GValue * value);
static void print_dialog_get_property (GObject * object, guint property_id,
                                      GValue * value, GParamSpec * pspec);
static void print_dialog_get_property_comboboxes (PrintDialog * dialog,
						  GtkComboBox * cbox,
						  GValue * value);
static void print_dialog_class_init (PrintDialogClass * class);



/*!
 *  \brief Callback function to show file chooser dialog
 *
 *  \par Shows file chooser dialog for user to select PostScript file
 *  to print to.
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static void print_dialog_action_choosefile (GtkWidget * w,
                                            PrintDialog * dialog)
{
  GtkWidget *filechooser;
  const char *filename;
  const char *newfilename;
  filechooser = gtk_file_chooser_dialog_new (_("Select PostScript Filename..."),
					     GTK_WINDOW (dialog),
					     GTK_FILE_CHOOSER_ACTION_SAVE,
					     GTK_STOCK_CANCEL,
					     GTK_RESPONSE_CANCEL,
					     GTK_STOCK_OK,
					     GTK_RESPONSE_ACCEPT, NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(filechooser),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  filename = gtk_entry_get_text (GTK_ENTRY (dialog->fnfield));
  gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (filechooser), filename);

  gtk_dialog_set_default_response(GTK_DIALOG(filechooser),
				  GTK_RESPONSE_ACCEPT);

  if (gtk_dialog_run (GTK_DIALOG (filechooser)) == GTK_RESPONSE_ACCEPT)
    {
      newfilename =
	gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (filechooser));
      gtk_entry_set_text (GTK_ENTRY (dialog->fnfield), newfilename);
    }

  gtk_widget_destroy (filechooser);

}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  what paper size to print to.
 *  \par Private function, should not be
 *  called by any code outside x_print.c
 */
static void print_dialog_init_paper_combobox (PrintDialog * d)
{
  GtkComboBox *combobox;
  char *string;
  gint i;

  combobox = GTK_COMBO_BOX (gtk_combo_box_new_text ());
  gtk_combo_box_set_active (combobox, -1);

  /* Populate combo box with available paper sizes */
  i = 0;
  string = (char *) s_papersizes_get (i);
  while (string != NULL)
    {
      gtk_combo_box_insert_text (GTK_COMBO_BOX (combobox), i, string);
      
      i++;
      string = (char *) s_papersizes_get (i);
    }

  d->papercbox = combobox;
}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  the type of printout to produce.
 *  \par Private function, should not be called by any code
 *  outside x_print.c  
 */
static void print_dialog_init_type_combobox (PrintDialog * d)
{
  GtkListStore *model;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;

  GtkWidget *combobox;
  
  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter, 
		      0, _("Extents with margins"),
		      1, EXTENTS,
		      -1);
  
  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
		      0, _("Extents no margins"),
		      1, EXTENTS_NOMARGINS,
		      -1);
  
  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
		      0, _("Current Window"),
		      1, WINDOW,
		      -1);

  combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (model));
  
  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox),
			      renderer, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
				 renderer, "text", 0);

  d->typecbox = GTK_COMBO_BOX (combobox);
}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  paper orientation.
 *  \par Private function, should not be called by any code
 *  outside x_print.c  
 */
static void
print_dialog_init_orient_combobox (PrintDialog * d)
{
  GtkListStore *model;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;

  GtkWidget *combobox;
  
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

  combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (model));
  
  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox),
			      renderer, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
				 renderer, "text", 0);

  d->orientcbox = GTK_COMBO_BOX (combobox);
}

/*!
 *  \brief Handle the user clicking on radio buttons to select print
 *  destination.
 *
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static void
print_dialog_action_radio_toggled (GtkWidget * w, PrintDialog * dialog)
{
  if (w == GTK_WIDGET (dialog->cmdradio))
    {
      gtk_widget_set_sensitive (GTK_WIDGET (dialog->cmdfield),
                               gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (w)));
    }
  else if (w == GTK_WIDGET (dialog->fileradio))
    {
      gtk_widget_set_sensitive (GTK_WIDGET (dialog->fnfield),
                               gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (w)));
      gtk_widget_set_sensitive (GTK_WIDGET (dialog->saveasbutton),
                               gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (w)));
    }
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void print_dialog_init (PrintDialog * dialog)
{
  GtkWidget *box;
  GtkWidget *frame;
  GtkWidget *settingstable, *desttable;
  GtkWidget *label;
  GtkWidget *print_button;

  /* Initialize properties */
  g_object_set (G_OBJECT (dialog),
		/* GtkWindow */
		"title", _("Print..."),
		"modal", TRUE, "destroy-with-parent", TRUE, NULL);

  /* Setup hbox for two main panes */
  box = gtk_vbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), box);

  /* Upper frame */
  frame = gtk_frame_new (_("Settings"));
  gtk_container_set_border_width (GTK_CONTAINER (frame), 3);
  gtk_container_add (GTK_CONTAINER (box), frame);

  /* Upper table with drop-down menus & labels 
   * Left-hand column contains labels, right-hand contains comboboxes*/
  settingstable = gtk_table_new (2, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (settingstable), 5);
  gtk_table_set_row_spacings (GTK_TABLE (settingstable), 5);
  gtk_container_set_border_width (GTK_CONTAINER (settingstable), 5);
  gtk_container_add (GTK_CONTAINER (frame), settingstable);

  label = gtk_label_new (_("Output paper size:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
		    label,
		    0, 1, 0, 1, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_init_paper_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
		    GTK_WIDGET (dialog->papercbox),
		    1, 2, 0, 1, GTK_FILL, 0, 0, 0);

  label = gtk_label_new (_("Type:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
		    label,
		    0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_init_type_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
		    GTK_WIDGET (dialog->typecbox),
		    1, 2, 1, 2, GTK_FILL, 0, 0, 0);

  label = gtk_label_new (_("Orientation:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
		    label,
		    0, 1, 2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_init_orient_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
		    GTK_WIDGET (dialog->orientcbox),
		    1, 2, 2, 3, GTK_FILL, 0, 0, 0);

  /* Lower frame */
  frame = gtk_frame_new (_("Destination"));
  gtk_container_set_border_width (GTK_CONTAINER (frame), 3);
  gtk_container_add (GTK_CONTAINER (box), frame);

  /* Table with destination selectors */
  desttable = gtk_table_new (3, 2, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (desttable), 5);
  gtk_table_set_row_spacings (GTK_TABLE (desttable), 5);
  gtk_container_set_border_width (GTK_CONTAINER (desttable), 5);
  gtk_container_add (GTK_CONTAINER (frame), desttable);

  /* Widgets for printing to file */
  dialog->fileradio =
    GTK_RADIO_BUTTON (gtk_radio_button_new_with_label (NULL, _("File:")));
  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->fileradio),
		    0, 1, 0, 1, GTK_FILL, GTK_EXPAND, 0, 0);
  g_signal_connect (dialog->fileradio,
		    "toggled",
		    GTK_SIGNAL_FUNC (print_dialog_action_radio_toggled),
		    dialog);

  dialog->fnfield = GTK_ENTRY (gtk_entry_new ());
  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->fnfield),
		    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, 0, 0, 0);

  dialog->saveasbutton = GTK_BUTTON(gtk_button_new());
  gtk_container_add(GTK_CONTAINER(dialog->saveasbutton),
		    gtk_image_new_from_stock(GTK_STOCK_OPEN,
					     GTK_ICON_SIZE_SMALL_TOOLBAR));
  gtk_button_set_relief(GTK_BUTTON(dialog->saveasbutton), GTK_RELIEF_NONE);

  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->saveasbutton), 2, 3, 0, 1,
		    GTK_FILL, 0, 0, 0);
  g_signal_connect (dialog->saveasbutton,
		    "clicked",
		    GTK_SIGNAL_FUNC (print_dialog_action_choosefile), dialog);

  /* Widgets for printing to command */
  dialog->cmdradio =
    GTK_RADIO_BUTTON (gtk_radio_button_new_with_label_from_widget
		      (dialog->fileradio, _("Command:")));
  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->cmdradio),
		    0, 1, 1, 2,  GTK_FILL, GTK_EXPAND, 0, 0);
  g_signal_connect (dialog->cmdradio,
		    "toggled",
		    GTK_SIGNAL_FUNC (print_dialog_action_radio_toggled),
		    dialog);

  dialog->cmdfield = GTK_ENTRY (gtk_entry_new ());
  gtk_table_attach (GTK_TABLE (desttable), GTK_WIDGET (dialog->cmdfield),
		    1, 3, 1, 2, GTK_EXPAND | GTK_FILL, 0, 0, 0);

  /* Add "Cancel" and "Print" buttons */
   gtk_dialog_add_button (GTK_DIALOG (dialog),
                  GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT);
   print_button = gtk_dialog_add_button (GTK_DIALOG (dialog),
                  GTK_STOCK_PRINT, GTK_RESPONSE_ACCEPT);
   gtk_widget_grab_focus(print_button);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_REJECT,
					  -1);

  /* Set initial radiobutton selection */
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->cmdradio), TRUE);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_set_property (GObject * object,
                          guint property_id,
                          const GValue * value, GParamSpec * pspec)
{
  PrintDialog *dialog = PRINT_DIALOG (object);
  bool file_active = FALSE;

  switch (property_id)
    {
    case PROP_FILENAME:
      gtk_entry_set_text (dialog->fnfield,
                         (char *) g_value_get_string (value));
      return;

    case PROP_COMMAND:
      gtk_entry_set_text (dialog->cmdfield,
                         (char *) g_value_get_string (value));
      return;

    case PROP_PAPERSIZE:
      gtk_combo_box_set_active (dialog->papercbox,
                               g_value_get_int (value));
      return;

    case PROP_ORIENTATION:
      print_dialog_set_property_comboboxes (dialog, 
					    dialog->orientcbox,
					    value);
      return;

    case PROP_TYPE:
      print_dialog_set_property_comboboxes (dialog, 
					    dialog->typecbox,
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
static void print_dialog_set_property_comboboxes (PrintDialog * dialog,
                                                  GtkComboBox * cbox,
                                                  const GValue * value)
{
  GtkTreeIter iter;
  GtkTreeModel *model;

  model = gtk_combo_box_get_model (cbox);
  gtk_tree_model_get_iter_first (model, &iter);

  do {
    GValue temp_value = {0, }; /* Make sure it's blank*/
    gtk_tree_model_get_value (model, &iter, 1, &temp_value);

    if (g_value_get_int (&temp_value) == g_value_get_int (value))
      {
	gtk_combo_box_set_active_iter (cbox, &iter);
	return;
      }

  } while (gtk_tree_model_iter_next (model, &iter));

  gtk_combo_box_set_active (cbox, 0);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void print_dialog_get_property (GObject * object,
                                       guint property_id,
                                       GValue * value, GParamSpec * pspec)
{
  PrintDialog *dialog = PRINT_DIALOG (object);
  bool file_active = FALSE;

  switch (property_id)
    {
    case PROP_FILENAME:
      g_value_set_string (value,
			  gtk_entry_get_text (GTK_ENTRY (dialog->fnfield)));
      return;
 
    case PROP_COMMAND:
      g_value_set_string (value,
			  gtk_entry_get_text (GTK_ENTRY (dialog->cmdfield)));
      return;
 
    case PROP_PAPERSIZE:
      g_value_set_int (value, gtk_combo_box_get_active (dialog->papercbox));
      return;
 
    case PROP_ORIENTATION:
      print_dialog_get_property_comboboxes (dialog,
					    dialog->orientcbox,
					    value);
      return;
      
    case PROP_TYPE:
      print_dialog_get_property_comboboxes (dialog,
					    dialog->typecbox,
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
static void print_dialog_get_property_comboboxes (PrintDialog * dialog,
				                  GtkComboBox * cbox,
				                  GValue * value)
{
  GValue temp_value = {0, };
  GtkTreeModel *model;
  GtkTreeIter iter;

  model = gtk_combo_box_get_model (cbox);

  gtk_combo_box_get_active_iter (cbox, &iter);
  gtk_tree_model_get_value (model, &iter, 1, &temp_value);
  g_value_copy (&temp_value, value);
  g_value_unset (&temp_value);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *  \bug Hardcoded 'magic' numbers in this function
 *
 */
static void
print_dialog_class_init (PrintDialogClass * class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  gobject_class->set_property = print_dialog_set_property;
  gobject_class->get_property = print_dialog_get_property;

  g_object_class_install_property (gobject_class, PROP_FILENAME,
				   g_param_spec_string ("filename",
							"", "", "",
							 G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_COMMAND,
				   g_param_spec_string ("command",
							"", "", "lpr",
							 G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_PAPERSIZE,
				   g_param_spec_int ("papersize",
						     "", "", 0, G_MAXINT, 0,
						     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_ORIENTATION,
				   g_param_spec_int ("orientation",
						     "", "", 0, G_MAXINT, 0,
						     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_TYPE,
				   g_param_spec_int ("type",
						     "", "", 0, G_MAXINT, 0,
						     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_USEFILE,
				   g_param_spec_boolean ("usefile",
							 "", "", FALSE,
							 G_PARAM_READWRITE));
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
GType print_dialog_get_type ()
{
  static GType print_dialog_type = 0;
 
  if (!print_dialog_type)
    {
      static const GTypeInfo print_dialog_info = {
	sizeof (PrintDialogClass),
	NULL,			/* base_init */
	NULL,			/* base_finalize */
	(GClassInitFunc) print_dialog_class_init,
	NULL,			/* class_finalize */
	NULL,			/* class_data */
	sizeof (PrintDialog),
	0,			/* n_preallocs */
	(GInstanceInitFunc) print_dialog_init,
      };
      print_dialog_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
						  "PrintDialog",
						  &print_dialog_info, 0);
    }

  return print_dialog_type;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
void x_print_setup (GSCHEM_TOPLEVEL *w_current, char *filename)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  char * command = w_current->print_command;
  gint orient = toplevel->print_orientation;
  gint type = toplevel->print_output_type;
  gint paperidx, x, y, result;
  char *string, *destination;
  bool usefile = FALSE;
  GtkDialog *dialog; 
  GtkWidget *popup_message;

  /* Work out current paper size by iterating through available paper
   * sizes.  Set the default paper size as the active selection */

  /* FIXME: ought to have a TOPLEVEL property containing
   * default paper size name, this is somewhat hackish. No
   * better way of doing it with current implementation of
   * varying paper size though. */
  paperidx = 0;
  while (TRUE)
    {
      string = (char *) s_papersizes_get (paperidx);
      s_papersizes_get_size (string, &x, &y);

      if ((x == toplevel->paper_width)
	  && (y == toplevel->paper_height))
	{
	  break;
	}
      if (string == NULL)
	{
	  paperidx = 0;
	  break;
	}
      paperidx++;
    }
 
  /* Create a print dialog, find out whether the user clicks Print or
     Cancel, and then print or return accordingly */
  dialog = GTK_DIALOG (g_object_new (TYPE_PRINT_DIALOG,
                                     "command", command,
                                     "filename", filename,
                                     "papersize", paperidx,
                                     "orientation", orient,
                                     "type", type,
                                     "usefile", usefile,
                                     /* GschemDialog */
                                     "settings-name", "print",
                                     "gschem-toplevel", w_current,
                                     NULL));
  gtk_widget_show_all (GTK_WIDGET (dialog));

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
				  GTK_RESPONSE_ACCEPT);
  gtk_window_set_transient_for(GTK_WINDOW(dialog),
			       GTK_WINDOW(w_current->main_window));

  result = gtk_dialog_run (dialog);
  
  if (result == GTK_RESPONSE_ACCEPT)
    {
      /* Extract values from dialog and set the paper size */
      g_object_get (dialog,
		    "command", &command,
		    "filename", &filename,
		    "papersize", &paperidx,
		    "orientation", &toplevel->print_orientation,
		    "type", &toplevel->print_output_type,
		    "usefile", &usefile,
		    NULL);

      s_papersizes_get_size (s_papersizes_get (paperidx),
			     &toplevel->paper_width,
			     &toplevel->paper_height);
		
      /* de select everything first */
      o_select_unselect_all( w_current );

      if (usefile && filename[0])
	/* Print to file */
	{
	  destination = filename;
	  result = f_print_file (toplevel,
                                 toplevel->page_current,
                                 filename);
	}
      else if (command[0])
	/* Print to command and save command for later use. */
	{
	  destination = command;
	  result = f_print_command (toplevel,
                                    toplevel->page_current,
                                    command);
	  
	  g_free (w_current->print_command);
	  w_current->print_command = g_strdup (command);
	}
      else
	{
	  s_log_message (_("No print destination specified\n"));
	  return;
	}

      /* Check whether it worked */
      if (result)
	{
	  s_log_message (_("Cannot print current schematic to [%s]\n"), 
			 destination);

	  /* Pop up a message warning the user */
	  popup_message = 
	    gtk_message_dialog_new (GTK_WINDOW(dialog),
				    GTK_DIALOG_DESTROY_WITH_PARENT,
				    GTK_MESSAGE_ERROR,
				    GTK_BUTTONS_CLOSE,
				    _("Error printing to file '%s'\n"
				      "Check the log window for more information"),
				    destination);
	  gtk_dialog_run (GTK_DIALOG (popup_message));	  
	}
      else
	{
	  s_log_message (_("Printed current schematic to [%s]\n"), 
			 destination);
	}
    }
   
  /* We don't need the dialog any more */
  gtk_widget_destroy (GTK_WIDGET (dialog));

}
/* ------------------------ End Print Old Dialogs -------------------------- */

#define DEFAULT_PDF_SIZE 256
#define CFG_GROUP_PRINTING "gschem.printing"
#define CFG_KEY_PRINTING_ORIENTATION "layout"
#define CFG_KEY_PRINTING_PAPER "paper"
#define CFG_KEY_PRINTING_MONOCHROME "monochrome"

/*! \brief Create a default page setup for a schematic page.
 * \par Function Description
 * Creates and returns a new #GtkPageSetup for \a page, taking into
 * account the requested \a paper_size_name.  If \a paper_size_name is
 * NULL, the system default paper size is used. The \a orientation may
 * be LANDSCAPE, PORTRAIT or AUTOLAYOUT.  If \a AUTOLAYOUT is chosen,
 * the page orientation that best fits the page contents is chosen.
 *
 * \param toplevel A #TOPLEVEL structure.
 * \param page     The #PAGE to generate a page setup for.
 * \param paper_size_name   The name of the paper size to use.
 * \param orientation       The paper orientation to use.
 *
 * \returns A newly-created page setup.
 */
static GtkPageSetup *x_print_default_page_setup (TOPLEVEL *toplevel, PAGE *page)
{
  GtkPageSetup *setup = gtk_page_setup_new ();
  GtkPaperSize *papersize;
  int status, wx_min, wy_min, wx_max, wy_max;
  EdaConfig *cfg;
  char *paper, *orientation;

  /* Get configuration values */
  cfg =         eda_config_get_context_for_path (page->page_filename);
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
  } else if (g_strcmp0 (orientation, "portrait") == 0) {
    gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_PORTRAIT);
  } else if (orientation == NULL
             || g_strcmp0 (orientation, "auto") == 0) {
    /* Automatically choose the orientation that fits best */
    status = world_get_object_glist_bounds (toplevel, s_page_objects (page),
                                            &wx_min, &wy_min, &wx_max, &wy_max);
    if (!status || (wx_max - wx_min) > (wy_max - wy_min)) {
      /* Default to landscape */
      gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_LANDSCAPE);
    } else {
      gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_PORTRAIT);
    }
  }

  g_free (paper);
  g_free (orientation);
  return setup;
}

/*! \brief Draw a page.
 * \par Function Description
 * Draws the \a page on the Cairo context \a cr, which should have
 * dimensions \a cr_width and \a cr_height.  If the Pango context \a
 * pc is provided, it is used for rendering of text.  The parameter \a
 * is_color controls whether to enable color printing, and \a
 * is_raster should be set if drawing to a raster surface such as an
 * image.
 *
 * \param toplevel A #TOPLEVEL structure.
 * \param page     The #PAGE to be rendered.
 * \param cr       The Cairo context to render to.
 * \param pc       A Pango context for text rendering, or NULL.
 * \param cr_width The width of the drawing area.
 * \param cr_height The height of the drawing area.
 * \param is_color TRUE if drawing should be in color; FALSE otherwise.
 * \param is_raster TRUE if drawing to a raster image surface; FALSE otherwise.
 */
static void x_print_draw_page (TOPLEVEL *toplevel, PAGE *page,
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
  status = world_get_object_glist_bounds (toplevel, s_page_objects (page),
                                          &wx_min, &wy_min, &wx_max, &wy_max);
  /* If there are no printable objects, draw nothing. */
  if (!status) return;

  w_width = wx_max - wx_min;
  w_height = wy_max - wy_min;

  scale = fmin (cr_width / w_width, cr_height / w_height);
  cairo_matrix_init (&mtx,
                     scale, 0,
                     0, -scale,
                     - (wx_min + 0.5*w_width) * scale + 0.5*cr_width,
                     (wy_min + 0.5*w_height) * scale + 0.5*cr_height);

  /* Second, build the color map.  If no color printing is desired,
   * transform the print color map into a black-and-white color map by
   * making the background color transparent and replacing all other
   * enabled colors with solid black. */
  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, print_colors, MAX_COLORS);
  if (!is_color) {
    int i;
    for (i = 0; i < MAX_COLORS; i++) {
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
                           "cairo-context", cr,
                           "pango-context", pc,
                           "color-map", color_map,
                           "render-flags", is_raster ? EDA_RENDERER_FLAG_HINTING : 0,
                           NULL);

  /* Finally, actually do drawing */
  cairo_save (cr);
  cairo_transform (cr, &mtx);

  /* Draw background */
  eda_cairo_set_source_color (cr, BACKGROUND_COLOR, color_map);
  cairo_paint (cr);

  /* Draw all objects and cues */
  for (iter = (GList *) s_page_objects (page);
       iter != NULL;
       iter = g_list_next (iter)) {
    eda_renderer_draw (renderer, (OBJECT *) iter->data);
  }
  for (iter = (GList *) s_page_objects (page);
       iter != NULL;
       iter = g_list_next (iter)) {
    eda_renderer_draw_cues (renderer, (OBJECT *) iter->data);
  }

  cairo_restore (cr);

  g_object_unref (renderer);
  g_array_free (color_map, TRUE);
}

/*! Drawing callback for use with GtkPrintOperation. */
static void
draw_page__print_operation (GtkPrintOperation *print,
                            GtkPrintContext *context,
                            gint page_nr,
                            gpointer user_data)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) user_data;
  PAGE *page;
  cairo_t *cr;
  PangoContext *pc;
  double width, height;
  EdaConfig *cfg;
  bool is_color;

  /* Find the page data */
  g_return_if_fail (page_nr != 1);
  page = w_current->toplevel->page_current;
  g_return_if_fail (page != NULL);

  /* Get cairo & pango contexts */
  cr = gtk_print_context_get_cairo_context (context);
  pc = gtk_print_context_create_pango_context (context);

  width = gtk_print_context_get_width (context);
  height = gtk_print_context_get_height (context);

  /* Find out if colour printing is enabled */
  cfg = eda_config_get_context_for_path (page->page_filename);
  is_color = !eda_config_get_boolean (cfg, CFG_GROUP_PRINTING,
                                      CFG_KEY_PRINTING_MONOCHROME, NULL);

  x_print_draw_page (w_current->toplevel, page, cr, pc,
                     width, height, is_color, FALSE);

  /* Clean up */
  g_object_unref (pc);
}

/*! \brief Export a print-style PDF file of the current page.
 * \par Function Description
 * Exports the current page as a PDF file to \a filename. The
 * export is carried out using a normal paper size and margins, as if
 * printing.
 *
 * \param w_current A #GSCHEM_TOPLEVEL structure.
 * \param filename  The filename for generated PDF.
 *
 * \returns TRUE if the operation was successful.
 */
bool
x_print_export_pdf_page (GSCHEM_TOPLEVEL *w_current,
                         const char *filename)
{
  PAGE *page;
  cairo_surface_t *surface;
  cairo_status_t status;
  cairo_t *cr;
  GtkPageSetup *setup;
  double width, height;
  EdaConfig *cfg;
  bool is_color;

  page = w_current->toplevel->page_current;

  setup = x_print_default_page_setup (w_current->toplevel, page );
  width = gtk_page_setup_get_paper_width (setup, GTK_UNIT_POINTS);
  height = gtk_page_setup_get_paper_height (setup, GTK_UNIT_POINTS);

  surface = cairo_pdf_surface_create (filename, width, height);
  cr = cairo_create (surface);
  cairo_translate (cr, gtk_page_setup_get_left_margin (setup, GTK_UNIT_POINTS),
                   gtk_page_setup_get_top_margin (setup, GTK_UNIT_POINTS));

  width = gtk_page_setup_get_page_width (setup, GTK_UNIT_POINTS);
  height = gtk_page_setup_get_page_height (setup, GTK_UNIT_POINTS);

  /* Find out if colour printing is enabled */
  cfg = eda_config_get_context_for_path (page->page_filename);
  is_color = !eda_config_get_boolean (cfg, CFG_GROUP_PRINTING,
                                      CFG_KEY_PRINTING_MONOCHROME, NULL);

  x_print_draw_page (w_current->toplevel, page,
                     cr, NULL, width, height, is_color, FALSE);

  cairo_destroy (cr);
  cairo_surface_finish (surface);

  status = cairo_surface_status (surface);
  if (status != CAIRO_STATUS_SUCCESS) {
    g_warning (_("Failed to write PDF to '%s': %s\n"),
               filename,
               cairo_status_to_string (status));
    return FALSE;
  }

  g_object_unref (setup);
  cairo_surface_destroy (surface);
  return TRUE;
}

/*! \brief Export a figure-style PDF file of the current page.
 * \par Function Description
 * Exports the current page as a PDF file to \a filename.  The export
 * is carried out using a page size matching the size of the visible
 * extents of the schematic page.
 *
 * \param w_current A #GSCHEM_TOPLEVEL structure.
 * \param filename  The filename for generated PDF.
 *
 * \returns TRUE if the operation was successful.
 */
bool x_print_export_pdf (GSCHEM_TOPLEVEL *w_current,
                         const char *filename)
{
  cairo_surface_t *surface;
  cairo_status_t cr_status;
  cairo_t *cr;
  int status, wx_min, wy_min, wx_max, wy_max;
  double width, height;

  /* First, calculate a transformation matrix for the cairo
   * context. We want to center the extents of the page in the
   * available page area. */
  status = world_get_object_glist_bounds (w_current->toplevel,
                                          s_page_objects (w_current->toplevel->page_current),
                                          &wx_min, &wy_min, &wx_max, &wy_max);
  if (status) {
    width = wx_max - wx_min;
    height = wy_max - wy_min;
  } else {
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
    g_warning (_("Failed to write PDF to '%s': %s\n"),
               filename,
               cairo_status_to_string (cr_status));
    return FALSE;
  }

  cairo_surface_destroy (surface);
  return TRUE;
}

/*! \brief Show a print dialog and print current page if requested.
 * \par Function Description
 * Shows a standard print dialog, and allows the user to print the current page.
 *
 * \param w_current A #GSCHEM_TOPLEVEL structure.
 */

void x_print (GSCHEM_TOPLEVEL *w_current)
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
                                 GTK_WINDOW (w_current->main_window), &err);

  if (res == GTK_PRINT_OPERATION_RESULT_ERROR) {
    /* If printing failed due to an error, show an error dialog */
    GtkWidget *error_dialog =
      gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                              GTK_DIALOG_DESTROY_WITH_PARENT,
                              GTK_MESSAGE_ERROR,
                              GTK_BUTTONS_CLOSE,
                              _("Error printing file:\n%s"),
                              err->message);
    g_signal_connect (error_dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);
    gtk_widget_show (error_dialog);
    g_error_free (err);

  } else if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
    /* We're supposed to store the print settings, so do that */
    if (settings != NULL) {
      g_object_unref (settings);
    }
    settings = g_object_ref (gtk_print_operation_get_print_settings (print));
  }

  /* Clean up */
  g_object_unref (print);
}