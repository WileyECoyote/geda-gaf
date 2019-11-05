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

/*!
 * \file
 * \brief Global variable declarations
 *
 * \section sdb_note SDB note about philosophy behind globals
 *
 * I made the "GedaToplevel project" and all the GTK window stuff into
 * global variables.  I know that this is supposedly bad programming form.
 * However, here are some observations:
 * - I wanted to use gEDA's GedaToplevel structure as much as possible, at
 *    least to hold info about the design's netlist & components.
 *    The GedaToplevel strucuture is architected to hold info about gschem's
 *    window also.  However, gschem's windows are architected differently
 *    than mine in gattrib.  This is because my windowing system does
 *    completely different things, and also uses the GtkSheet widget, which
 *    is architected completely differently from GedaToplevel.
 * - Since I couldn't easily or naturally cram my windowing scheme into
 *    GedaToplevel (or so I think), I decided to use a separate set of windows
 *    from those defined under GedaToplevel for my application.
 * - The problem arises when using callbacks.  Callbacks from GTK allow
 *    only one argument to be passed.  Given the way I set up the menu bar,
 *    I didn't have easy acces to the information inside both the GtkSHeet
 *    objects *and* the GedaToplevel stuff while only having one callback
 *    argument.  This makes it hard to have access to e.g. a GtkSheet window
 *    and a list of files (in GedaToplevel) simultaneously.
 * - Therefore, I decided to make both the window stuff and GedaToplevel
 *    globals.
 * - Similarly, because I couldn't cram the PageDataSet struct into any
 *    hook in GedaToplevel, I just made it a global also.
 * - Finally, in my defense, in gschem and gnetlist, (GedaToplevel *w_current
 *    or pr_current) is passed  to almost every function.  Since it
 *    is just a pointer to a huge struct of stuff, manipulating
 *    the stuff in the struct has a global
 *    effect.  That is, manipulating w_current (or pr_current) has side
 *    effects, so it is basically a global anyway.  The real problem with
 *    globals occurs when you have a global variable caled "i" or "temp"
 *    which conflicts with a global in a module written by somebody else.
 *    Since pr_current is a very uncommon name, this should not be a
 *    problem here.  Therefore, I decided
 *    to make life easy for myself dealing with callbacks by making both
 *    the windows and GedaToplevel global variables.
 *
 * If there is a better way to solve this problem, I'd like to hear it.
 *
 */
/* ------------------------------------------------------------------ */

#ifndef __GATTRIB_GLOBALS__
#define __GATTRIB_GLOBALS__

/* i18n */
#include "gettext.h"

#define GATTRIB_GETTEXT_DOMAIN     "geda-gattrib"

/*------------------------------------------------------------------*/
/*!
 * The main data structure from gEDA is defined in libgeda/structs.h
 */
/*------------------------------------------------------------------*/
GedaToplevel *pr_current;

/*------------------------------------------------------------------*/
/*!
 *  a global data structure defined in structs.h
 */
/*------------------------------------------------------------------*/
PageDataSet *sheet_head;

/*! @brief Globals to control displaying of only the attached attributes */
bool show_attached;

/*! @brief Globals list for retaining Search and Replace History */
GList *search_history;

/*! @brief Global general purpose char array for notification messages */
char msg_buffer[256];

/*------------------------------------------------------------------
 * GTKsheet includes: stuff for dealing with windows.
 *------------------------------------------------------------------*/
#define MAX_WINDOW_TITLE 96
#define DEFAULT_FONT_WIDTH 12
#define DEFAULT_PRECISION 2
#define DEFAULT_SPACE 8
#define NUM_SHEETS 3              /*!< Components, Nets, and Pins */
#define COLUMN_WIDTH_LIMIT 180    /*!< Auto width adjustment upper limit*/

GtkWindow        *main_window;    /*!< Main window */
GtkWidget        *menu_bar;
GtkUIManager     *menu_manager;   /*!< Manager for menus */
GtkRecentManager *recent_manager; /*!< Manager for recently used files */

GtkWidget  *Standard_handlebox;
GtkWidget  *Standard_Toolbar;

GtkWidget  *Attribute_handlebox;
GtkWidget  *Attribute_Toolbar;

GtkWidget  *notebook;
GtkWidget  *popup;
GtkWidget  *edit_box;
GtkWidget  *entry;               /*!< Active Cell Edit Entry */
GtkWidget  *location;
GtkWidget **scrolled_windows;
GtkSheet  **sheets;              /*!< These are the spreadsheet widgets themselves */

GSList *ComponentMenuItems;
GSList *ComponentToolbarButtons;

extern int export_mode;          /*!< command line switch export csv*/
extern int verbose_mode;         /*!< command line switch verbose settings */
extern int quiet_mode;           /*!< command line switch quiet settings */

/* rc variables */
extern GList *hide_columns;
extern int    sort_components;
extern int    tearoff_menus;

typedef enum { Black, Red, Blue,
               Green, Orange, Purple,
               Gray, Pink, SkyBlue,
               LightGreen, Tan, Violet,
               Yellow, White
} ColorId;

typedef enum  { Components, Nets, Pins } SheetId;
typedef enum  { ToggleVisibility,
                AddAttribute,
                InsertAttribute,
                HideAttribute,
                RevealAttribute,
                DeleteAttribute,
                ClearAttributeData
}  IDS_Popup_items;

typedef enum  { tb_open, tb_save, tb_save_as, tb_cut, tb_copy, tb_paste,
                tb_find, tb_replace, tb_attribute, tb_designator,
                tb_invisible, tb_visible, tb_add, tb_del, tb_promote,
                tb_demote, tb_name_only, tb_value_only, tb_name_value
} IDS_Toolbar;

#endif
