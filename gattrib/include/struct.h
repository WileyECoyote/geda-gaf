/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2010 Stuart D. Brorson.
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

/* ----------------------------------------------------------------- */
/*! \file
 *  \brief Definitions of structures used in gattrib
 *
 *  This file holds definitions of the structures used in gattrib.
 */
/* ----------------------------------------------------------------- */

#ifndef GATTRIB_DATA_STRUCT
#define GATTRIB_DATA_STRUCT

#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include <glib.h>
#include <glib-object.h>

#include <geda_keysyms.h>

/* -------  Includes needed to make the GTK stuff work  ------ */

#include "gtksheet.h"

/* ========  Data structures used in processing below here  ========== */


/* ----------------------------------------------------------------- *
 *  The sheet data hierarchy built by the prog should look like this:
 *  PageDataSet->(STRING_LIST *master_XXX_list)          // list of comps/nets/pins (row labels)
 *             ->(STRING_LIST *master_XXX_attrib_list)   // list of attached names  (column labels)
 *             ->(TABLE *XXX_table)                      // table of attrib values (table entries)
 * ----------------------------------------------------------------- */
typedef struct st_sheet_data PageDataSet;

typedef struct st_table TABLE;
typedef struct st_string_list STRING_LIST;
typedef struct st_pin_list PIN_LIST;

typedef struct st_search SearchRecord;
typedef struct st_column_visible ColumnVisible;

/* -------------------------------------------------------------------- */
/*! \struct st_sheet_data
 *  \brief Sheet data structure
 *  \par
 * st_sheet_data defines PageDataSet, and holds master lists holding
 * sorted lists of comp/netlist names. Also holds pointers to the
 * heads of the attribute-holding component and net structures.
 */
struct st_sheet_data {
  STRING_LIST *master_comp_list_head;         /*!< Sorted list of all component refdeses used in design */
  STRING_LIST *master_comp_attrib_list_head;  /*!< Sorted list of all component attribs used in design */
  STRING_LIST *attached_attrib;               /*!<  list of all attached attribs used in design */

  int comp_count;                             /*!< This cannnot change -- user must edit design using gschem */
  int comp_attrib_count;                      /*!< This can change in this prog if the user adds attribs */
  int attached_attrib_count;

  STRING_LIST *master_net_list_head;          /*!< Sorted list of all net names used in design */
  STRING_LIST *master_net_attrib_list_head;   /*!< Sorted list of all net attribs used in design */
  int net_count;                              /*!< This cannnot change -- user must edit design using gschem */
  int net_attrib_count;                       /*!< This can change in this prog if the user adds attribs */

  STRING_LIST *master_pin_list_head;          /*!< Sorted list of all refdes:pin items used in design.   */
  STRING_LIST *master_pin_attrib_list_head;   /*!< Sorted list of all pin attribs used in design */
  int pin_count;                              /*!< This cannnot change -- user must edit design using gschem */
  int pin_attrib_count;                       /*!< This can change in this prog if the user adds attribs */

  TABLE **component_table;                    /*!< points to 2d array of component attribs */
  TABLE **net_table;                          /*!< points to 2d array of net attribs */
  TABLE **pin_table;                          /*!< points to 2d array of pin attribs */

  int CHANGED;                                /*!< for "file not saved" warning upon exit */
};

/* -------------------------------------------------------------------- */
/*! \struct st_table
 *  \brief Table cell struct
 *  \par
 * st_table defined what is held in a spreadsheet cell for both comp
 * and net spreadsheets. Holds pointer to individual comp/net name,
 * and pointer to attrib list. Ideally, the name pointer points to
 * the refdes/netname string held in the GedaToplevel data structure,
 * so that when PageDataSet is manipulated, so is GedaToplevel.
 */
struct st_table {
  int   row;                    /*!< location on spreadsheet */
  int   col;                    /*!< location on spreadsheet */
  char *row_name;               /*!< comp, net, or refdes:pin name */
  char *col_name;               /*!< attrib name */
  char *attrib_value;           /*!< attrib value */
  int   is_inherited;
  int   is_promoted;
  int   visibility;
  int   show_name_value;
};


/* -------------------------------------------------------------------- */
/*! \struct st_pin_list
 *  \brief A list of strings.
 *  \par
 * STRING_LIST is a doubly-linked list of strings.  This struct is
 * used for several different jobs, including serving as base class
 * for master lists.
 */
struct st_string_list {
  char *data;        /*!< points to zero-terminated string */
  int pos;           /*!< position on spreadsheet */
  int length;        /*!< number of items in list */
  STRING_LIST *prev; /*!< pointer to previous item in linked list */
  STRING_LIST *next; /*!< pointer to next item in linked list */
};

/* -------------------------------------------------------------------- */
/*! \struct st_pin_list
 *  \brief A list of pins
 *  \par
 * PIN_LIST is a special struct used for keeping track of pins.  Since
 * the master_pin_list must keep track of both refdes and pin, we need a
 * special struct for pins.  Later processing will for a STRING_LIST
 * of refdes:pinnumber pairs for insertion in the spreadsheet.
 */
struct st_pin_list {
  char *refdes;        /*!< holds refdes string */
  int   pinnumber;
  char *pinlabel;      /*!< holds pin label string */
  int pos;             /*!< pos on spreadsheet */
  int length;          /*!< number of items in list */
  PIN_LIST *prev;
  PIN_LIST *next;
};


struct st_search {
  GtkSheet     *sheet;
  GtkSheetRange range;
  bool FindOnlyMode;
  bool Case;
  bool Whole;
  bool Backword;
  bool Wrap;
  bool Found;
  bool ReplaceAll;
  int  count;
  int  mode;
};

struct st_column_visible {
  const char *name;    /*!< Column name string */
  int visible;
};

#endif /* ifndef GATTRIB_DATA_STRUCT */
