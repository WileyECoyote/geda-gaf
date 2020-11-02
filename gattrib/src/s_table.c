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

/*------------------------------------------------------------------*/
/*! \file
 *  \brief Functions to manipulate the TABLE structure
 *
 * This file holds functions involved in manipulating the TABLE structure,
 * which is subsidiary to PageDataSet.  TABLE is a 2 dimensional array
 * of structs; each struct corresponds to the data about an element
 * in a single cell of the spreadsheet.
 *
 * \todo TABLE should also store its dimensions in its own data structure
 *       to save carrying the dimensions around separately.
 */

#include "../include/gattrib.h"

/* ===================  Public Functions  ====================== */

/*------------------------------------------------------------------*/
/*!
 * \brief Create a new table
 * \par Function Description
 *  This is the table creator.  It returns a pointer to an initialized
 *  TABLE data struct. The table is a dynamically allocated 2D array of
 *  structs. The tables are allocated as an array of columns so the arrays
 *  are accessed as (sheet_data->comp_table)[col][row]).field. This is
 *  different then GTK sheets which uses row,col. The orginal version of
 *  this function was YX but was changed to X,Y so memory could be allocated
 *  and deallocated in terms of the number of attributes AND not the number
 *  of components in the design.
 *
 * \param rows Number of rows required in the new table
 * \param cols Number of columns required in the new table
 * \returns a pointer to an initialized TABLE struct.
 */
TABLE **s_table_new( int rows, int cols)
{
  TABLE **new_table;
  int x, y;

  /* Create a 2 dimensional array of structs */

  new_table = (TABLE**)GEDA_MEM_ALLOC(cols * sizeof(TABLE*));

  for (x = 0; x < cols; x++) {
    new_table[x] = (TABLE *) GEDA_MEM_ALLOC(rows * sizeof(TABLE));
    /* Should checks be here to verify that malloc was successful*/
  }

  /* Now pre-load the table with NULLs */
  for (x = 0; x < cols; x++) {
    for (y = 0; y < rows; y++) {
      (new_table[x][y]).attrib_value = NULL;
      (new_table[x][y]).row_name = NULL;
      (new_table[x][y]).col_name = NULL;
      (new_table[x][y]).row = y;
      (new_table[x][y]).col = x;
      (new_table[x][y]).visibility = VISIBLE;
      (new_table[x][y]).show_name_value = SHOW_VALUE;
    }
  }

  return (new_table);
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add Comlumn to the Component TABLE
 * \par Function Description
 *  This function adds a column to the component table by increasing the
 *  allocated memory and initialized the new column record. If the column
 *  is inserted rather than appended, existing record are relocated to
 *  make room for the insertion.
 *
 * \param table Table to resize
 * \param rows  Number of rows in the table
 * \param Xa    Where to add the new column
 * \param Xt    The current number of columns in the table.
 *
 * \returns a pointer to the resized table
 */
TABLE **s_table_add_column(TABLE **table, int rows, int Xa, int Xt)
{
  int y;
  TABLE **new_table;

  void init_new_record(int col) {
    for (y = 0; y < rows; y++) {
      (new_table[col][y]).attrib_value = NULL;
      (new_table[col][y]).row_name = NULL;
      (new_table[col][y]).col_name = NULL;
      (new_table[col][y]).row = y;
      (new_table[col][y]).col = col;
      (new_table[col][y]).is_inherited= FALSE;
      (new_table[col][y]).is_promoted = -2;
      (new_table[col][y]).visibility = VISIBLE;
      (new_table[col][y]).show_name_value = SHOW_VALUE;
    }
  }

  /* resize the 2 dimensional array of structs */
  new_table = (TABLE**)realloc(table, (Xt + 1) * sizeof(TABLE *));

  if (new_table == NULL) {
    return NULL;  /* die if failed to realloc new memory */
  }

  new_table[Xt] = (TABLE *) GEDA_MEM_ALLOC(rows * sizeof(TABLE));

  if (Xa == Xt) { /* if appending a column */
     init_new_record(Xt);
  }
  else {

    int x;

    /* Loop over the table, starting from the far right column, and
     * shift columns to the right of Xa one column to the right */
    for (x = Xt; x > Xa; x--) {
      for (y = 0; y < rows; y++) {
        new_table[x][y].row = new_table[x-1][y].row;
        new_table[x][y].col = new_table[x-1][y].col;
        new_table[x][y].row_name = new_table[x-1][y].row_name;
        new_table[x][y].col_name = new_table[x-1][y].col_name;
        new_table[x][y].attrib_value = new_table[x-1][y].attrib_value;
        new_table[x][y].visibility = new_table[x-1][y].visibility;
        new_table[x][y].show_name_value = new_table[x-1][y].show_name_value;
        new_table[x][y].is_inherited = new_table[x-1][y].is_inherited;
        new_table[x][y].is_promoted = new_table[x-1][y].is_promoted;
      }
    }

    /* Now blank out the column at Xa */
    init_new_record(Xa);
  }

  return new_table;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Destroy a table
 * \par Function Description
 *  This function destroys a table array. Use this after reading in a
 *  new page to get rid of the old table before building a new one.
 *
 * \param table Table to destroy
 * \param row_count Number of rows in table
 * \param col_count Number of columns in table
 */
void s_table_destroy(TABLE **table, int row_count, int col_count)
{
  int x;

  if (table == NULL)
    return;

  for (x = 0; x < col_count; x++) {

    int y;

    for (y = 0; y < row_count; y++) {
      GEDA_FREE ((table[x][y]).attrib_value);
      GEDA_FREE ((table[x][y]).row_name);
      GEDA_FREE ((table[x][y]).col_name);
    }

    GEDA_FREE (table[x]);
  }

  GEDA_FREE(table);

  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Get a string index number
 * \par Function Description
 *  This function returns the index number when given a STRING_LIST and a
 *  string to match.  It finds the index number by iterating through the
 *  master  list.
 *
 * \param local_list
 * \param local_string
 *
 * \returns the index of the string
 */
int s_table_get_index(STRING_LIST *local_list, char *local_string) {

  int count = 0;
  STRING_LIST *iter;

#ifdef DEBUG
  printf("%s: looking for %s in %p.\n", __func__, local_string, local_list);
#endif

  iter = local_list;

  while (iter != NULL) {

    if (strcmp(iter->data, local_string) == 0) {
      return count;
    }

    count++;

    iter = iter->next;
  }

  return(-1);  /* return code when string is not in local_list  */
}

/*------------------------------------------------------------------*/
/*!
 * \brief Create attribute pair
 * \par Function Description
 *  This function takes a table, a row list, and a row name, and
 *  returns a list holding name=value pairs for all attribs that
 *  pertainent to that particular row. If the row holds no attribs,
 *  it just returns NULL.
 *
 * \param row_name Name of the row to search for
 * \param table Table to be searched
 * \param row_list list of rows
 * \param num_attribs
 *
 * \returns STRING_LIST of name=value pairs
 */
STRING_LIST *s_table_create_attrib_pair(char        *row_name,
                                        TABLE      **table,
                                        STRING_LIST *row_list,
                                        int          num_attribs)
{
  STRING_LIST *attrib_pair_list;

  int row, col;
  int count = 0;

  attrib_pair_list = s_string_list_new();

  row = s_table_get_index(row_list, row_name);
  /* Sanity check */
  if (row == -1) {
    return attrib_pair_list;
  }

  for (col = 0; col < num_attribs; col++) {

    int is_inherited;
    int is_promoted;

    is_inherited = (table[col][row]).is_inherited;
    is_promoted  = (table[col][row]).is_promoted;

    if (is_inherited && ! is_promoted) continue;

    /* pull attrib from table.  If non-null, add it to attrib_pair_list  */
    if ((table[col][row]).attrib_value != NULL) {

      char *attrib_name, *attrib_value, *name_value_pair;

      attrib_name = (table[col][row]).col_name;
      attrib_value = (table[col][row]).attrib_value;
      name_value_pair = geda_strconcat(attrib_name, "=", attrib_value, NULL);

      s_string_list_add_item(attrib_pair_list, &count, name_value_pair);
      GEDA_FREE(name_value_pair);
    }
  }

  return attrib_pair_list;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add components to the component table
 * \par Function Description
 *  This fcn iterates over a list of objects looking for components.
 *  When a component is found, all attributes of the commponent are
 *  added in the TABLE.
 *
 * \param obj_list pointer to GList containing objects
 */
void s_table_add_items_to_comp_table (const GList *obj_list) {

  char *temp_uref;
  char *attrib_text;
  char *attrib_name;
  char *attrib_value;

  int  row, col;
  int  old_visibility, old_show_name_value;
  int  counter;
  bool is_attached;

  GedaObject  *a_current;
  const GList *o_iter;
  const GList *a_iter;

  /* ----- Iterate through all objects found on page ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {

    GedaObject *o_current = o_iter->data;

    /* ----- Now process objects found on page ----- */
    if (o_current->type == OBJ_COMPLEX &&
        o_current->attribs != NULL) {

      /* ---- Don't process part if it lacks a refdes ----- */
      temp_uref = s_attrib_get_refdes(o_current);

      /* Don't add graphical objects or pin label designators*/
      if (temp_uref) {

        if ((strcmp (temp_uref, "none")) &&
            (strcmp (temp_uref, "pinlabel")))
        {
          GList       *all_attribs;
          STRING_LIST *AttachedAttributes;

          /* Having found a component, loop over All ATTACHED attribs for
           * this component, and stick them into cells in the table. */
          AttachedAttributes = s_string_list_new();
          a_iter             = geda_object_get_attached(o_current);
          counter            = 0;

          while (a_iter != NULL) {

            a_current = a_iter->data;

            if (a_current->type == OBJ_TEXT && a_current->text != NULL) { /* found an attribute */

              /* may need to check more thoroughly here. . . . */
              attrib_text         = geda_strdup(a_current->text->string);
              attrib_name         = geda_strsplit(attrib_text, '=', 0);
              attrib_value        = s_misc_remaining_string(attrib_text, '=', 1);
              old_visibility      = geda_object_get_is_visible (a_current) ? VISIBLE : INVISIBLE;
              old_show_name_value = a_current->show_name_value;

              /* Don't include "refdes" or "slot" because they form the row name. */
              /* Also don't include "net" per bug found by Steve W. 4.3.2007 -- SDB */
              if ((strcmp(attrib_name, "refdes") != 0) &&
                  (strcmp(attrib_name, "net") != 0) &&
                  (strcmp(attrib_name, "slot") != 0)) {

                /* Get row and col where to put this attrib */
                row = s_table_get_index(sheet_head->master_comp_list_head, temp_uref);
                col = s_table_get_index(sheet_head->master_comp_attrib_list_head, attrib_name);

                /* Sanity check */
                if (row == -1) {
                  /* did not find the item in the table */
                  const char *err_msg = _("Component Error looking for row ref");
                  fprintf (stderr, "%s [%s]\n", err_msg, temp_uref);
                }
                else if (col == -1) {
                  const char *err_msg = _("Component Error looking for column");
                  fprintf (stderr, "%s [%s]\n", err_msg, attrib_name);
                }
                else {
                    /* Is there a compelling reason to put this into a separate fcn? */
                    ((sheet_head->component_table)[col][row]).row             = row;
                    ((sheet_head->component_table)[col][row]).col             = col;
                    ((sheet_head->component_table)[col][row]).row_name        = geda_strdup(temp_uref);
                    ((sheet_head->component_table)[col][row]).col_name        = geda_strdup(attrib_name);
                    ((sheet_head->component_table)[col][row]).attrib_value    = geda_strdup(attrib_value);
                    ((sheet_head->component_table)[col][row]).visibility      = old_visibility;
                    ((sheet_head->component_table)[col][row]).show_name_value = old_show_name_value;
                    ((sheet_head->component_table)[col][row]).is_inherited    = FALSE;
                    ((sheet_head->component_table)[col][row]).is_promoted     = -1;
                    s_string_list_add_item(AttachedAttributes, &counter, attrib_name);
                    counter++;
                }
              }
              GEDA_FREE(attrib_name);
              GEDA_FREE(attrib_text);
              GEDA_FREE(attrib_value);
            }
            a_iter = g_list_next (a_iter);
          } /* while (a_iter != NULL) */

          /* Do it again but this time for ALL attributes associated with this component */
          all_attribs = geda_attrib_return_attribs (o_current);

          for (a_iter = all_attribs; a_iter; a_iter = a_iter->next) {

            a_current   = a_iter->data;
            is_attached = a_current->attached_to == o_current ? TRUE : FALSE;

            if (!is_attached) {

              if (a_current->type == OBJ_TEXT && a_current->text != NULL) { /* found an attribute */

                attrib_text  = geda_strdup(a_current->text->string);
                attrib_name  = geda_strsplit(attrib_text, '=', 0);
                attrib_value = s_misc_remaining_string(attrib_text, '=', 1);

                if (!s_string_list_in_list(AttachedAttributes, attrib_name)) {

                  old_visibility      = geda_object_get_is_visible (a_current) ? VISIBLE : INVISIBLE;
                  old_show_name_value = a_current->show_name_value;

                  /* Do not include "refdes" or "slot" because they form the row name. */
                  /* Also do not include "net" per bug found by Steve W. 4.3.2007 -- SDB */
                  if ((strcmp(attrib_name, "refdes") != 0) &&
                      (strcmp(attrib_name, "net") != 0) &&
                      (strcmp(attrib_name, "slot") != 0))
                  {
                    /* Get row and col where to put this attrib */
                    row = s_table_get_index(sheet_head->master_comp_list_head, temp_uref);
                    col = s_table_get_index(sheet_head->master_comp_attrib_list_head, attrib_name);

                    /* Sanity check */
                    if (row == -1) {
                      /* did not find the item in the table */
                      const char *err_msg = _("Component Error looking for row ref");
                      fprintf (stderr, "%s [%s]\n", err_msg, temp_uref);
                    }
                    else {
                      if (col == -1) {
                        const char *err_msg = _("Component Error looking for column");
                        fprintf (stderr, "%s [%s]\n", err_msg, attrib_name);
                      }
                      else {
                        /* Is there a compelling reason for me to put this into a separate fcn? */
                        ((sheet_head->component_table)[col][row]).row = row;
                        ((sheet_head->component_table)[col][row]).col = col;
                        ((sheet_head->component_table)[col][row]).row_name = geda_strdup(temp_uref);
                        ((sheet_head->component_table)[col][row]).col_name = geda_strdup(attrib_name);
                        ((sheet_head->component_table)[col][row]).attrib_value = geda_strdup(attrib_value);
                        ((sheet_head->component_table)[col][row]).visibility = old_visibility;
                        ((sheet_head->component_table)[col][row]).show_name_value = old_show_name_value;
                        ((sheet_head->component_table)[col][row]).is_inherited = TRUE;
                        ((sheet_head->component_table)[col][row]).is_promoted = FALSE;
                      }
                    }
                  }
                }
                GEDA_FREE(attrib_name);
                GEDA_FREE(attrib_text);
                GEDA_FREE(attrib_value);
              }
            }
          } /* for (a_iter != NULL) */
          g_list_free (all_attribs);
          s_string_list_free (AttachedAttributes);
        }
        GEDA_FREE(temp_uref);
      } /* if (temp_uref) */
    } /* if (o_current->type == OBJ_COMPLEX) */
  }

  verbose_done();
}

#if 0
/*------------------------------------------------------------------*/
/*!
 * \brief Add nets to net table
 * \par Function Description
 *  This function iterates over adds all items found on this page looking
 *  for nets and adds them individually to the net table.  Looping over
 *  objects occurs here.
 *
 * \param start_obj Pointer to first object
 *
 * \todo Why do the calling semantics of this function disagree with
 *       s_table_add_tems_to_pin_table()?  That function
 *       takes a GList, this one takes a pointer to Object.
 */
void s_table_add_items_to_net_table(Object *start_obj) {
  GedaObject *o_current;
  char *temp_netname;
  int row, col;
  char *attrib_text;
  char *attrib_name;
  char *attrib_value;
  ATTRIB *a_current;

  /* -----  Iterate through all objects found on page  ----- */
  o_current = start_obj;
  while (o_current != NULL) {

    /* -----  Now process objects found on page  ----- */
    if (o_current->type == OBJ_NET) {
#if DEBUG
      fflush(stderr);
      fflush(stdout);
      printf("In %s, Found net on page\n", __func__);
#endif
      verbose_print(" N");

      /* Having found a net, we stick it into the table. */
      a_current = o_current->attribs;
      while (a_current != NULL) {
        if (a_current->object->type == OBJ_TEXT
            && a_current->object->text != NULL) {  /* found an attribute */
          /* may need to check more thoroughly here. . . . */
          attrib_text = geda_strdup(a_current->object->text->string);
          attrib_name = geda_strsplit(attrib_text, '=', 0);
          attrib_value = s_misc_remaining_string(attrib_text, '=', 1);
          if (strcmp(attrib_name, "netname") != 0) {
            /* Don't include "netname" */

            /* Get row and col where to put this attrib */
            row = s_table_get_index(sheet_head->master_net_list_head, temp_netname);
            col = s_table_get_index(sheet_head->master_net_attrib_list_head, attrib_name);
#if DEBUG
            fflush(stderr);
            fflush(stdout);
            printf("In s_table_add_items_to_net_table, about to add row %d, col %d, attrib_value = %s\n",
                   row, col, attrib_value);
            printf(" . . . current address of attrib_value cell is [%p]\n", &((sheet_head->net_table)[col][row]).attrib_value);
#endif
            /* Is there a compelling reason for me to put this into a separate fcn? */
            ((sheet_head->net_table)[col][row]).row = row;
            ((sheet_head->net_table)[col][row]).col = col;
            ((sheet_head->net_table)[col][row]).row_name = geda_strdup(temp_netname);
            ((sheet_head->net_table)[col][row]).col_name = geda_strdup(attrib_name);
            ((sheet_head->net_table)[col][row]).attrib_value = geda_strdup(attrib_value);
          }
          GEDA_FREE(attrib_name);
          GEDA_FREE(attrib_text);
          GEDA_FREE(attrib_value);
        }
        a_current = a_current->next;

      }  /* while (a_current != NULL) */
      GEDA_FREE(temp_netname);

    }    /*--- if (o_current->type == OBJ_NET)   ---*/


    o_current = o_current->next;  /* iterate to next object on page */
  }  /* while o_current != NULL */

  verbose_done();

#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("%s: exit\n", __func__);
#endif

}
#endif


/*------------------------------------------------------------------*/
/*!
 * \brief Add pins to pin table.
 * \par Function Description
 *  This function iterates over adds all items found on this page
 *  looking for pins.  WHen it finds a pin, it gathers all
 *  pin attribs and sticks them into the pin table.
 *
 * \param obj_list List of objects on page
 */
void s_table_add_tems_to_pin_table (const GList *obj_list) {

  GedaObject  *pin_attrib;
  GList       *a_iter;
  GList       *o_lower_iter;
  const GList *o_iter;
  char        *temp_uref;
  char        *pinnumber;
  char        *row_label;
  char        *attrib_text;
  char        *attrib_name;
  char        *attrib_value;
  int          row, col;

  if (verbose_mode) {
    printf("%s...", _("Creating pin table"));
  }

#ifdef DEBUG
  printf("=========== Just entered %s!  ==============\n", __func__);
#endif

  /* -----  Iterate through all objects found on page  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter))
  {
    GedaObject *o_current = o_iter->data;

#ifdef DEBUG
    printf("\t%s: examining o_current->name = %s\n", __func__, o_current->name);
#endif

    /* -----  Now process objects found on page  ----- */
    if (o_current->type == OBJ_COMPLEX && o_current->attribs != NULL)
    {
      /* ---- Don't process part if it lacks a refdes ----- */
      temp_uref = s_attrib_get_refdes(o_current);
      if ((temp_uref) &&
         (strcmp (temp_uref, "none")) &&
         (strcmp (temp_uref, "pinlabel")))
      {

        /* -----  Now iterate through lower level objects looking for pins.  ----- */
        for (o_lower_iter = o_current->complex->prim_objs;
             o_lower_iter != NULL;
             o_lower_iter = g_list_next (o_lower_iter))
        {
          GedaObject *o_lower_current = o_lower_iter->data;

          if (o_lower_current->type == OBJ_PIN) {

            /* -----  Found a pin.  First get its pinnumber.  then get attrib head and loop on attribs.  ----- */
            pinnumber = geda_attrib_search_object_by_name (o_lower_current, "pinnumber", 0);
            row_label = geda_strconcat(temp_uref, ":", pinnumber, NULL);

#if DEBUG
            printf("\t%s, examining pin %s\n",__func__, row_label);
#endif

            a_iter = o_lower_current->attribs;

            while (a_iter != NULL) {

              pin_attrib = a_iter->data;

              if (pin_attrib->type == OBJ_TEXT &&
                  pin_attrib->text != NULL)
              {  /* found an attribute */
                attrib_text = geda_strdup(pin_attrib->text->string);
                attrib_name = geda_strsplit(attrib_text, '=', 0);
                attrib_value = s_misc_remaining_string(attrib_text, '=', 1);

                if ((strcmp(attrib_name, "pinnumber") != 0) &&
                    (attrib_value != 0))
                {
                  /* Don't include "pinnumber" because it is already in other
                   * master list. Also must ensure that value is non-null;
                   * certain symbols are not well formed. */

                  /* Get row and col where to put this attrib */
                  row = s_table_get_index(sheet_head->master_pin_list_head, row_label);
                  col = s_table_get_index(sheet_head->master_pin_attrib_list_head, attrib_name);

                  /* Sanity check */
                  if (row == -1) {

                    /* we didn't find the item in the table */
                    const char *err_msg = _("Pin Error looking for row ref");
                   fprintf (stderr, "%s [%s]\n", err_msg, row_label);
                  }
                  else {
                    if (col == -1) {
                      const char *err_msg = _("Pin Error looking for column");
                      fprintf (stderr, "%s [%s]\n", err_msg, attrib_name);
                    }
                    else {
                     /* Is there a compelling reason for me to put this into
                      * a separate fcn? */
                      ((sheet_head->pin_table)[col][row]).row = row;
                      ((sheet_head->pin_table)[col][row]).col = col;
                      ((sheet_head->pin_table)[col][row]).row_name = geda_strdup(row_label);
                      ((sheet_head->pin_table)[col][row]).col_name = geda_strdup(attrib_name);
                      ((sheet_head->pin_table)[col][row]).attrib_value = geda_strdup(attrib_value);
                    }
                  }
                }
                GEDA_FREE(attrib_name);
                GEDA_FREE(attrib_text);
                GEDA_FREE(attrib_value);
              }
              a_iter = g_list_next (a_iter);

            }  /* while (pin_attrib != NULL) */
            GEDA_FREE(pinnumber);
            GEDA_FREE(row_label);
          }
        }
      }

      GEDA_FREE(temp_uref);
    }
  }

  verbose_done();
}

/*!
 * \brief Remove an Attribute from a table
 * \par Function Description
 *  This function deletes an attribute record in the component and
 *  releases the associated memory.
 *
 * \param table Component Table containing record to remove
 * \param X     The index of the attribute column to be removed
 */
#define data_table sheet_head->component_table
#define free_if(field) if(((data_table)[X][Y]).field) GEDA_FREE(((data_table)[X][Y]).field);
#define col_count sheet_head->comp_attrib_count
bool s_table_remove_attribute(TABLE **table, int X)
{
  bool result = FALSE;
  int Y;

  void free_column(int X) {
    for (Y = 0; Y < sheet_head->comp_count; Y++) {
      free_if (row_name)
      free_if (col_name)
      free_if (attrib_value)
    }
  }

  if ( X > col_count ) return result;

  free_column(X);

  if ( X == col_count ) {     /* if the last record */
    GEDA_FREE (table[X - 1]);
  }
  else {

    int Xi;

    for (Xi = X; Xi < col_count - 1; Xi++) {
      for (Y = 0; Y < sheet_head->comp_count; Y++) {
        table[Xi][Y].row = table[Xi + 1][Y].row;
        table[Xi][Y].col = table[Xi + 1][Y].col;
        table[Xi][Y].row_name = table[Xi + 1][Y].row_name;
        table[Xi][Y].col_name = table[Xi + 1][Y].col_name;
        table[Xi][Y].attrib_value = table[Xi + 1][Y].attrib_value;
        table[Xi][Y].visibility = table[Xi + 1][Y].visibility;
        table[Xi][Y].show_name_value = table[Xi + 1][Y].show_name_value;
        table[Xi][Y].is_inherited = table[Xi + 1][Y].is_inherited;
        table[Xi][Y].is_promoted = table[Xi + 1][Y].is_promoted;
      }
    }
    GEDA_FREE (table[col_count - 1]);
  }

  return TRUE;
}

#undef data_table
#undef free_if
#undef col_count

/*------------------------------------------------------------------*/
/*!
 * \brief Pull spreadsheet data to TABLEs.
 * \par Function Description
 *  This function traverses the spreadsheet, extracts the attribs from
 *  the cells, and places them back into TABLE. This is the first step
 *  in saving a project.
 */
void s_table_gtksheet_to_all_tables() {

  int num_rows;
  int num_cols;
  STRING_LIST *master_row_list;
  STRING_LIST *master_col_list;
  TABLE **local_table;
  GtkSheet *local_gtk_sheet;

  /* First handle component sheet */
  num_rows = sheet_head->comp_count;
  num_cols = sheet_head->comp_attrib_count;
  local_gtk_sheet = sheets[Components];
  master_row_list = sheet_head->master_comp_list_head;
  master_col_list = sheet_head->master_comp_attrib_list_head;

  local_table = sheet_head->component_table;

  /* now fill out new table */
#ifdef DEBUG
  printf("%s: now about to fill out new component table.\n", __func__);
#endif

  s_table_gtksheet_to_table(local_gtk_sheet, master_row_list,
                            master_col_list, local_table,
                            num_rows, num_cols);

#ifdef UNIMPLEMENTED_FEATURES
  /* Next handle net sheet */
  num_rows = sheet_head->net_count;
  num_cols = sheet_head->net_attrib_count;
  local_gtk_sheet = sheets[Nets];
  master_row_list = sheet_head->master_net_list_head;
  master_col_list = sheet_head->master_net_attrib_list_head;
  local_table = sheet_head->net_table;

  s_table_gtksheet_to_table(local_gtk_sheet, master_row_list,
                            master_col_list, local_table,
                            num_rows, num_cols);
#endif

  /* Finally, handle component pin sheet */
  num_rows = sheet_head->pin_count;
  num_cols = sheet_head->pin_attrib_count;
  local_gtk_sheet = sheets[Pins];
  master_row_list = sheet_head->master_pin_list_head;
  master_col_list = sheet_head->master_pin_attrib_list_head;
  /*  local_table = s_table_new(num_rows, num_cols);  */
  local_table = sheet_head->pin_table;

  s_table_gtksheet_to_table(local_gtk_sheet, master_row_list,
                            master_col_list, local_table,
                            num_rows, num_cols);
  return;
}


/* ===================  Private Functions  ====================== */
/*------------------------------------------------------------------*/
/*!
 * \brief Extract attributes from gtksheet into TABLE
 * \par Function Description
 *  This function loops through the spreadsheet, extractins attribute
 *  data from the cells and updatea the attributes in the data TABLE.
 *
 * \param local_gtk_sheet GtkSheet to save
 * \param master_row_list STRING_LIST of rows
 * \param master_col_list STRING_LIST of columns
 * \param local_table TABLE structure to fill
 * \param num_rows Number of rows in table
 * \param num_cols Number of columns in table
 */
void s_table_gtksheet_to_table(GtkSheet    *local_gtk_sheet,
                               STRING_LIST *master_row_list,
                               STRING_LIST *master_col_list,
                               TABLE       **local_table,
                               int num_rows, int num_cols)
{
  STRING_LIST *row_list_item;
  STRING_LIST *col_list_item;
  int          row;

#ifdef DEBUG
  printf("********** Entering s_table_gtksheet_to_table ******************\n");
#endif

  row_list_item = master_row_list;

  for (row = 0; row < num_rows; row++) {

    const char *row_title;
    int   col;

    row_title     = (char*)row_list_item->data;
    col_list_item = master_col_list;

    for (col = 0; col < num_cols; col++) {

      const char *attrib_value;
      const char *col_title;

      col_title = (char*)col_list_item->data;

      /* get value of attrib in cell  */
      attrib_value = gtk_sheet_cell_get_text(GTK_SHEET(local_gtk_sheet), row, col);

#if 0
      if (strlen(attrib_value) == 0) {
        /* GEDA_FREE(attrib_value);  */   /* sometimes we have spurious, zero length strings creep */
        attrib_value = NULL;    /* into the GtkSheet                                     */
      }
#endif

#if DEBUG
      printf("%s: found attrib_value = %s in cell row=%d, col=%d\n",
             __func__, attrib_value, row, col);
#endif

    /* first handle attrib value in cell */
#if DEBUG
      printf("\tUpdating attrib_value %s\n", attrib_value);
#endif

      GEDA_FREE (local_table[col][row].attrib_value);

      if (attrib_value != NULL) {
        local_table[col][row].attrib_value = geda_strdup(attrib_value);
      }
      else {
        local_table[col][row].attrib_value = NULL;
      }

      /* next handle name of row (also held in TABLE cell) */
#ifdef DEBUG
      printf("\tUpdating row_name %s\n", row_title);
#endif

      GEDA_FREE (local_table[col][row].row_name);

      if (row_title != NULL) {
        local_table[col][row].row_name = geda_strdup(row_title);
      }
      else {
        local_table[col][row].row_name = NULL;
      }

      /* finally handle name of col */
#ifdef DEBUG
      printf("\tUpdating col_name %s\n", col_title);
#endif

      GEDA_FREE(local_table[col][row].col_name);

      if (col_title != NULL) {
        local_table[col][row].col_name = geda_strdup(col_title);
      }
      else {
        local_table[col][row].col_name = NULL;
      }

      /* get next col list item and then iterate. */
      col_list_item = col_list_item->next;
    }

    row_list_item = row_list_item->next;
  }

  return;
}

/*!
 * \brief Load data tables
 * \par Function Description
 *  This function calls s_table_new to create new tables loads and
 *  then s_table_add_items_to_comp_table to add values the tables.
 *
 * \param PageData ptr to sheet_head/PageDataSet structure
 */
void s_table_load_new_page(PageDataSet *PageData)
{
  GList *iter;

  PageData->component_table = s_table_new(PageData->comp_count, PageData->comp_attrib_count);
  PageData->net_table       = s_table_new(PageData->net_count, PageData->net_attrib_count);
  PageData->pin_table       = s_table_new(PageData->pin_count, PageData->pin_attrib_count);

  /* iterate over all pages in design */
  for (iter  = geda_toplevel_get_pages (pr_current);
       iter != NULL;
       iter  = g_list_next(iter)) {

    Page *p_local = (Page *)iter->data;

    /* only traverse pages which are toplevel */
    if (p_local->page_control == 0) {
      /* adds all components from page to comp_table */
      s_table_add_items_to_comp_table (geda_struct_page_get_objects (p_local));
#if 0
      /* Note that this must be changed.  We need to input the entire project
       * before doing anything with the nets because we need to first
       * determine where they are all connected!   */

      /* adds all nets from page to net_table */
      s_table_add_items_to_net_table(p_local->object_head);
#endif

      /* adds all pins from page to pin_table */
      s_table_add_tems_to_pin_table (geda_struct_page_get_objects (p_local));
    }
  } /* for loop over pages */

  /* Why not run gnetlist -g geda to get the net list ?*/
}
