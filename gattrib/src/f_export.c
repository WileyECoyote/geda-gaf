/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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

/*! \file
 *  \brief Import/export functions
 *
 * This file holds fcns used for import/export of attribute sheets.
 * At the moment, this is only component sheets.
 */

#include "../include/gattrib.h"

static inline void
print_quote_if_comma(FILE *fp, char *text)
{
  /* if there's a comma anywhere in the field, wrap the field in " */
  if (strstr(text, ",")) {
    fprintf(fp, "\"%s\"", text);
  }
  else {
    fprintf(fp, "%s", text);
  }
}

/* ===================  Public Functions  ====================== */

/*!
 * \brief Export components to CSV
 * \par Function Description
 * This function is invoked when the user selects file ->
 * export from the pull-down menu. The function writes out a CSV
 * file of the design for external processing.
 *
 * \param filename The name of the file to export to
 */
void f_export_components(const char *filename)
{
  int cur_page;
  int num_rows;
  int num_cols;
  int i, j;
  char *text;
  FILE *fp;

  /* -----  Check that we have a component ----- */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  if (cur_page != 0) {
    /* We only export the component table */
    /* XXXXX  Maybe throw up error message in window instead? */
    x_dialog_unimplemented_feature();
    return;

  }

  /* -----  First try to open file for writing ----- */

  fp = fopen(filename, "wb");

  if (fp == NULL) {
    geda_log ("%s: \"%s\"\n", _("Could not open"), filename);
    /* XXXXX Throw up error message in window */
    return;
  }

  /* -----  Now write out data  ----- */
  num_rows = sheet_head->comp_count;
  num_cols = sheet_head->comp_attrib_count;

  /*  First export top row -- attribute names  */
  /*  Print out "refdes" since that's always the first column  */
  fprintf(fp, "refdes, ");

  /*  Print out optional attrib names  */
  for (i = 0; i < num_cols - 1; i++) {
    text = s_string_list_get_data_at_index(sheet_head->master_comp_attrib_list_head, i);
    fprintf(fp, "%s, ", text);
  }

  /*  Print out last attrib name with no comma and with \n.  */
  text = s_string_list_get_data_at_index(sheet_head->master_comp_attrib_list_head, i);

  fprintf(fp, "%s\n", text);

  /*  Now export the contents of the sheet  */
  for (j = 0; j < num_rows; j++) {

    /*  First output the component refdes  */
    text = s_string_list_get_data_at_index(sheet_head->master_comp_list_head, j);

#ifdef DEBUG
    printf("In f_export_components, getting refes, j = %d.\n", j);
    printf("In f_export_components, output component refdes %s.\n", text);
#endif

    fprintf(fp, "%s, ",text);

    /*  Now export the attrib values for first n-1 cols */
    for (i = 0; i < num_cols - 1; i++) {

      char *str = sheet_head->component_table[i][j].attrib_value;

      if (str) { /* found a string */

        /* make a copy of the text, escaping any special chars, like " */
        text = (char*)g_strescape(str, "" );

        print_quote_if_comma(fp, text);
        fprintf(fp, ", ");

        GEDA_FREE(text);
      }
      else {                                                  /* no attrib string */

#ifdef DEBUG
        printf("In f_export_components, output blank attrib space\n");
#endif

        fprintf(fp, ", ");
      }
    }  /* end of for loop over cols  */

    /* Now export attrib value for last col (with no "," and with "\n" */
    if ((sheet_head->component_table)[i][j].attrib_value ) { /* found a string */

      /* make a copy of the text, escaping any special chars, like " */
      text = (char*)g_strescape((sheet_head->component_table)[i][j].attrib_value, "" );

#ifdef DEBUG
      printf("In f_export_components, output final attribute %s.\n", text);
#endif

      print_quote_if_comma(fp, text);
      fprintf(fp, "\n");

      GEDA_FREE(text);
    }
    else {                                                  /* no attrib string */

#ifdef DEBUG
      printf("In f_export_components, output blank at end of line.\n");
#endif

      fprintf(fp, "\n");
    }

#ifdef DEBUG
    printf("In f_export_components, Go to next row.\n");
#endif

  }  /* close of for loop over rows */

  fclose(fp);

  return;
}
