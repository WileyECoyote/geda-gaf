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

/*--------------------------------------------------------------*/
/*!
 * \file
 *
 * \brief Functions involved in manipulating an entire
 * PageDataSet structure.
 *
 * This file holds functions involved in manipulating an entire
 * PageDataSet structure.  The PageDataSet structure is the intermediate
 * structure between GedaToplevel (gEDA's native format) and the graphical
 * gtksheet widget (from gtkextra), which is the spreadsheet widget
 * displaying the attribs.
 */

#include "../include/gattrib.h"

/*------------------------------------------------------------------*/
/*!
 * \brief Create a PageDataSet Page Data Struct.
 *  \par Function Description
 * Creates an initialized but empty data struct.
 * \returns a pointer to a data struct.
 */
PageDataSet *s_sheet_data_new()
{
  PageDataSet *new_data_set;

  new_data_set = (PageDataSet*)malloc(sizeof(PageDataSet));

  /* We will allocate and fill out the comp table later. */
  new_data_set->component_table = NULL;

  /* We will allocate and fill out the net table later. */
  new_data_set->net_table = NULL;

  /* We will allocate and fill out the pin table later. */
  new_data_set->pin_table = NULL;

  /* Now we create the first cell in each master list. */
  new_data_set->master_comp_list_head = (STRING_LIST *) s_string_list_new();
  new_data_set->master_comp_attrib_list_head = (STRING_LIST *) s_string_list_new();
  new_data_set->attached_attrib = (STRING_LIST *) s_string_list_new();

  new_data_set->comp_count = 0;
  new_data_set->comp_attrib_count = 0;
  new_data_set->attached_attrib_count = 0;

  new_data_set->master_net_list_head = (STRING_LIST *) s_string_list_new();
  new_data_set->master_net_attrib_list_head = (STRING_LIST *) s_string_list_new();
  new_data_set->net_count = 0;
  new_data_set->net_attrib_count = 0;

  new_data_set->master_pin_list_head = (STRING_LIST *) s_string_list_new();
  new_data_set->master_pin_attrib_list_head = (STRING_LIST *) s_string_list_new();
  new_data_set->pin_count = 0;
  new_data_set->pin_attrib_count = 0;

  new_data_set->CHANGED = FALSE;

  return (new_data_set);
}

/*------------------------------------------------------------------*/

/*!
 * \brief Frees the contents of a PageDataSet struct.
 * \par Function Description
 * Frees the string lists in a PageDataSet struct.
 */
void
s_sheet_data_free(PageDataSet *PageData)
{
  if (PageData != NULL) {

    s_string_list_free(PageData->master_comp_list_head);
    s_string_list_free(PageData->master_comp_attrib_list_head);
    s_string_list_free(PageData->attached_attrib);

    s_string_list_free(PageData->master_net_list_head);
    s_string_list_free(PageData->master_net_attrib_list_head);

    s_string_list_free(PageData->master_pin_list_head);
    s_string_list_free(PageData->master_pin_attrib_list_head);

    free(PageData);
  }
}

/* ------------ s_sheet_data interface to s_string_list --------- */

/*!
 * \brief Add Data to string list in sheet-head structures
 * \par Description
 *  The next 6 functions are  called from the
 *  s_sheet_data_add_yada-yada_list_items functions below but have "reduced"
 *  names and simplified parameters to improve readability of those functions.
 *  These functions are also used by the s_sheet_data_load_blank function
 *  below.
 *
 * \param PageData           pointer to sheet_head structure.
 * \param component_str_name pointer to string to be added to the associated
 *                           string list
 */
static void
s_sheet_data_add_comp(PageDataSet *PageData, const char *component_str_name)
{
  s_string_list_add_item(PageData->master_comp_list_head,
                         &(PageData->comp_count),
                         component_str_name);
}

static void
s_sheet_data_add_comp_attrib(PageDataSet *PageData,
                             const char *comp_attrib_str_name)
{
  s_string_list_add_item(PageData->master_comp_attrib_list_head,
                         &(PageData->comp_attrib_count),
                         comp_attrib_str_name);
}

static void
s_sheet_data_attached_attrib(PageDataSet *PageData,
                             const char  *comp_attrib_str_name)
{
  s_string_list_add_item(PageData->attached_attrib,
                         &(PageData->attached_attrib_count),
                         comp_attrib_str_name);
}

static void
s_sheet_data_add_net(PageDataSet *PageData, const char *net_str_name)
{
  s_string_list_add_item(PageData->master_net_list_head,
                         &(PageData->net_count), net_str_name);
}

static void s_sheet_data_add_net_attrib(PageDataSet *PageData,
                                        const char *net_attrib_str_name)
{
  s_string_list_add_item(PageData->master_net_attrib_list_head,
                         &(PageData->net_attrib_count),net_attrib_str_name);
}

static void
s_sheet_data_add_pin(PageDataSet *PageData, const char *pin_str_name)
{
  s_string_list_add_item (PageData->master_pin_list_head,
                          &(PageData->pin_count), pin_str_name);
}

static void
s_sheet_data_add_pin_attrib(PageDataSet *PageData, const char *pin_attrib_str)
{
  s_string_list_add_item(PageData->master_pin_attrib_list_head,
                         &(PageData->pin_attrib_count), pin_attrib_str);
}

/*------------------------------------------------------------------*/
/*!
 * \brief Fill a PageDataSet struct with Template Data and load tables.
 * \par Function Description
 * Creates and initializes a PageDataSet struct with dummy data.
 */
void s_sheet_data_load_blank(PageDataSet *PageData)
{
  if (PageData != NULL) {

    const char *comp_attrib[]= { "device",  "footprint", "value",
                                 "description", "symversion" };
    char tmp_str[5];
    char none[6];
    int  blank;

    for (blank = 0; blank < 5; blank++) {

      char *str = geda_utility_string_int2str(blank, tmp_str, 10);

      s_sheet_data_add_comp (PageData, str);
      s_sheet_data_add_comp_attrib(PageData, comp_attrib[blank]);
      s_sheet_data_attached_attrib(PageData, comp_attrib[blank]);

      s_sheet_data_add_net(PageData, strcat(strcpy(none, "none"), str));
      s_sheet_data_add_net_attrib(PageData, strcat(strcpy(none, "node"), str));

      s_sheet_data_add_pin(PageData, str);
    }

    s_sheet_data_add_pin_attrib(PageData, "pinseq");
    s_sheet_data_add_pin_attrib(PageData, "pintype");
    s_sheet_data_add_pin_attrib(PageData, "pinlabel");
  }

  /* s_table_load_new_page used to be called in x_fileselect but the old
   * algorithms were rearranged so that this call was moved to s_toplevel
   * _init_data_set, (for real data) but for a blank "workbook" we are by-
   * bassing and need to load the dummy data we just put in sheet_data.
   */
  s_table_load_new_page(PageData);

  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add components to master list
 * \par Function Description
 *  Add to the master list of components refdeses by iterating through
 *  the components and selectively recording discovered comp refdeses.
 *  This list is used for the row labels on the component sheet.
 *
 * \param obj_list pointer to the component list to be added.
 */
void s_sheet_data_add_master_comp_list_items (const GList *obj_list)
{
  char *temp_uref;
  const GList *iter;

#ifdef DEBUG
  printf("============== %s! ==============\n", __func__);
#endif

  if (verbose_mode) {
    printf(_("Creating master component list.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {

    GedaObject *o_current = iter->data;

#ifdef DEBUG
    printf("%s: examining o_current->name = %s\n", __func__, o_current->name);
#endif

    /*-----  only process if this is a component with attributes ----*/
    if (o_current->type == OBJ_COMPLEX && o_current->attribs != NULL) {

#if DEBUG
      printf("%s: found component on page", __func__);
      printf(". . . . filename = %s.\n", o_current->name);
#endif

      temp_uref = s_attrib_get_refdes(o_current);

#if DEBUG
      fprintf(stderr, "ref= %s\n", temp_uref);
#endif

      /* Store refdes and attach attrib list to the component */
      if (temp_uref) {

        /* Skip graphical objects and pin label designators */
        if ((strcmp(temp_uref, "none")) && (strcmp(temp_uref, "pinlabel"))) {

#if DEBUG
          printf("%s: adding refdes=%s to master list\n", __func__, temp_uref);
#endif

          s_sheet_data_add_comp(sheet_head, temp_uref);
        }
        GEDA_FREE(temp_uref);
      }
    }   /* if (o_current->type == OBJ_COMPLEX . . . . .) */
  }

  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add attributes to master list
 * \par Function Description
 *  Adds attribute names to the master list of comp attributes. The names
 *  are obtained by iterating through each component on the page, selectively
 *  recording discovered attributes.The data struct being searched  is:
 *  sheet_head->component_list_head->attrib->name;
 *
 * \param obj_list pointer to list of objects
 */
void s_sheet_data_add_master_comp_attrib_list_items (const GList *obj_list)
{
  GList       *a_iter;
  char        *attrib_text;
  char        *attrib_name;
  const GList *o_iter;

  bool is_attached;

  if (verbose_mode) {
    printf(_("Creating master component attribute list.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components (OBJ_COMPLEX) ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {

    GedaObject *o_current = o_iter->data;

#ifdef DEBUG
    printf("%s: examining o_current->name = %s\n", __func__, o_current->name);
#endif

    /*-----  only process if this is a component with attributes ----*/
    if (o_current->type == OBJ_COMPLEX) {

      GList *object_attribs = geda_attrib_return_attribs (o_current);

      for (a_iter = object_attribs; a_iter != NULL; a_iter = g_list_next (a_iter))
      {
        GedaObject *a_current = a_iter->data;

        if (a_current->type == OBJ_TEXT ) { /* WEH: Are there attributes that are not text? */

          /* found an attribute */
          attrib_text = geda_utility_string_strdup(a_current->text->string);
          attrib_name = geda_utility_string_split(attrib_text, '=', 0);

          /* Don't include "refdes" or "slot" because they form the row name */
          /* Also don't include "net" per bug found by Steve W. -- 4.3.2007, SDB */
          //WEH: use instr and gang strings?
          if ((strcmp(attrib_name, "graphical") != 0) &&
            (strcmp(attrib_name, "refdes") != 0) &&
            (strcmp(attrib_name, "net") != 0) &&
            (strcmp(attrib_name, "slot") != 0) )
          {

            is_attached = a_current->attached_to == o_current ? TRUE : FALSE;

            if (is_attached) {

#if DEBUG
              printf("adding an attached attrib to master attrib list, attrib = %s\n", attrib_text);
#endif

              s_sheet_data_attached_attrib(sheet_head, attrib_name);

            }

#if DEBUG
            printf("adding an attrib to master comp attrib list attrib = %s\n", attrib_text);
#endif

            s_sheet_data_add_comp_attrib(sheet_head, attrib_name);
          }
          GEDA_FREE(attrib_name);
          GEDA_FREE(attrib_text);
        }
      } /* Next attribute_iter*/
      g_list_free(object_attribs);

    } /* if (o_current->type == OBJ_COMPLEX) */
  }

  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add net names to master list.
 * \par Function Description
 *  Build the master list of net names by running through individual
 *  cells and recording the net refdeses.
 *  It's currently empty, waiting for implementation of net
 *  attributes.
 */
void s_sheet_data_add_master_net_list_items (const GList *obj_start) {
  s_sheet_data_add_net(sheet_head, "none");
  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add net attributes to master list.
 * \par Function Description
 *  Build the master list of net attribs.
 *  It's currently empty, waiting for implementation of net
 *  attributes.
 */
void s_sheet_data_add_master_net_attrib_list_items (const GList *obj_start) {
  s_sheet_data_add_net_attrib(sheet_head, "none");
  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add pin names to master list.
 * \par Function Description
 *  Build the master
 *  list of pin names.  It writes the
 *  label refdes:pinnumber into the global master pin list.
 *  Algorithm:
 *  -# Loop on o_current looking for OBJ_COMPLEX
 *  -# When we find a complex, save the refdes.
 *  -# Dive down to o_lower_current = o_current->complex->prim_objs
 *  -# Loop on o_lower_current looking for OBJ_PIN
 *  -# When we find a pin, find the pinnumber by calling
 *     geda_attrib_search_object_by_name(o_lower_current, "pinnumber", 0)
 *  -# Create the pin list label as "refdes=XXX", and stick it into
 *     the master pin list.
 *  Since this function operates on the global sheet_data->master_pin_list,
 *  it doesn't return a value.
 *
 * \param obj_list pointer to list of pin names to be added.
 */
void s_sheet_data_add_master_pin_list_items (const GList *obj_list) {

  char *temp_uref;
  char *temp_pinnumber;
  char *row_label;
  const GList *o_iter;
  GList *o_lower_iter;

#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("============== %s! ==============\n", __func__);
#endif

  if (verbose_mode) {
    printf(_("Creating master pin list.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {

    GedaObject *o_current = o_iter->data;

#ifdef DEBUG
    printf ("%s: examining o_current->name = %s\n", __func__, o_current->name);
#endif

    if (o_current->type == OBJ_COMPLEX) {

      temp_uref = s_attrib_get_refdes (o_current);

      if ((temp_uref) &&
          (strcmp (temp_uref, "none")) &&
          (strcmp (temp_uref, "pinlabel")) ){

        /* -----  Now iterate through lower level objects looking for pins.  ----- */
        for (o_lower_iter = o_current->complex->prim_objs;
             o_lower_iter != NULL;
             o_lower_iter = g_list_next (o_lower_iter))
        {

          GedaObject *o_lower_current = o_lower_iter->data;

#if DEBUG
          printf ("%s: examining object name %s\n", __func__, o_lower_current->name);
#endif

          if (o_lower_current->type == OBJ_PIN) {

            temp_pinnumber = geda_attrib_search_object_by_name (o_lower_current, "pinnumber", 0);

            if (temp_pinnumber != NULL) {

              row_label = geda_strconcat (temp_uref, ":", temp_pinnumber, NULL);

#if DEBUG
              printf ("%s: about to add to master pin list row_label = %s\n", __func__, row_label);
#endif

              s_sheet_data_add_pin(sheet_head, row_label);

              GEDA_FREE (row_label);
            }
            else {      /* didn't find pinnumber.  Report error to log. */

              const char *msg = _("found component pin without a pin number");

              fprintf (stderr, "%s: %s.\n", __func__, msg);

#ifdef DEBUG
              fprintf (stderr, ". . . . refdes = %s.\n", temp_uref);
#endif
            }
            GEDA_FREE (temp_pinnumber);
          }
        }

      }
      else {          /* didn't find refdes.  Report error to log. */

#ifdef DEBUG
        fprintf (stderr, "In s_sheet_data_add_master_pin_list_items, found component with no refdes.\n");
        fprintf (stderr, ". . . . filename = %s.\n", o_current->filename);
#endif

      }
      GEDA_FREE (temp_uref);

    }  /*  if (o_current->type == OBJ_COMPLEX)  */
  }

  return;
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add pin attributes to master list.
 * \par Function Description
 *  Build the master
 *  list of pin attributes.  It writes
 *  each attrib name into the master pin attrib list.
 *  Algorithm:
 *  -# Loop on o_current looking for OBJ_COMPLEX
 *  -# When we find a complex, save the refdes.
 *  -# Dive down to o_lower_current = o_current->complex->prim_objs
 *  -# Loop on o_lower_current looking for OBJ_PIN
 *  -# When we find a pin, get pin_attribs = o_lower_current->attribs
 *  -# Loop on attribs looking for non-NULL text.
 *  -# When we find a non-NULL text attrib, extract the attrib name
 *     and stick it in the master pin attrib list.
 *
 * \param obj_list pointer to list of pin attributes to be added.
 */
void s_sheet_data_add_master_pin_attrib_list_items (const GList *obj_list) {

  GedaObject  *pin_attrib;
  const GList *o_iter;
  GList       *a_iter;
  char        *temp_uref;
  char        *attrib_text;
  char        *attrib_name;
  char        *attrib_value;

#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("============== %s! ==============\n", __func__);
#endif

  if (verbose_mode) {
    printf("%s...", _("Creating master pin attribute list"));
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {

    GedaObject *o_current = o_iter->data;

#ifdef DEBUG
    printf("%s: examining o_current->name = %s\n", __func__, o_current->name);
#endif

    if (o_current->type == OBJ_COMPLEX) {

      temp_uref = s_attrib_get_refdes(o_current);

      if (temp_uref != NULL) {      /* make sure object complex has a refdes  */

        const GList *objects = geda_complex_object_get_prim_objs(o_current);
        const GList *o_lower_iter;

        /* -----  Now iterate through lower level objects looking for pins.  ----- */
        for (o_lower_iter = objects; o_lower_iter != NULL; o_lower_iter = o_lower_iter->next)
        {
          GedaObject *o_lower_current = o_lower_iter->data;

#if DEBUG
          printf("%s: examining component refdes =  %s\n", __func__, temp_uref);
#endif
          if (o_lower_current->type == OBJ_PIN) {

            /* -----  Found a pin.  Now get attrib head and loop on attribs.  ----- */
            a_iter = o_lower_current->attribs;

            while (a_iter != NULL) {

              pin_attrib = a_iter->data;

              if (pin_attrib->type == OBJ_TEXT && pin_attrib->text != NULL) {  /* found an attribute */

                attrib_text  = geda_utility_string_strdup(pin_attrib->text->string);
                attrib_name  = geda_utility_string_split(attrib_text, '=', 0);
                attrib_value = s_misc_remaining_string(attrib_text, '=', 1);

                if ((strcmp(attrib_name, "pinnumber") != 0) && (attrib_value != NULL)) {

                  /* Don't include "pinnumber" because it is already in other master list.
                   * Also guard against pathalogical symbols which have non-attrib text inside pins. */

#if DEBUG
                  printf("%s: found pin attrib =  %s", __func__, attrib_name);
                  printf("... adding attribute name to master_pin_attrib_list\n");
#endif

                  s_sheet_data_add_pin_attrib(sheet_head, attrib_name);

                }   /* if (strcmp(attrib_name, "pinnumber") != 0) */
                GEDA_FREE(attrib_value);
                GEDA_FREE(attrib_name);
                GEDA_FREE(attrib_text);
              }
              a_iter = g_list_next (a_iter);
            }                               /* wend (pin_attrib != NULL)  */
          }
        }

        GEDA_FREE(temp_uref);
      }                         /* if (temp_uref != NULL )  */
    }                           /* if (o_current->type == OBJ_COMPLEX) */
  }

  return;
}
