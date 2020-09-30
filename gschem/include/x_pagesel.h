/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_pagesel.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file x_pagesel.h
 *
 * \brief header for the Page Select Dialog
 */
/*! \class Pagesel x_pagesel.h "x_pagesel.h"
 *  \brief Page Select Dialog
 *  \par
 *  The Page Select Dialog ...
 */

/*#define NAME_WIDTH_HIGH 535  Minimum column with when full filenames */
#define COLUMN_NAME_MIN_WIDTH     235 /* Minimum width for filename column */
#define COLUMN_CHANGED_MIN_WIDTH   35 /* Minimum width for Changed column */

#define PAGESEL_MIN_HEIGHT        335
#define PAGESEL_ROWS_THRESHOLD      4

#define TYPE_PAGESEL         (pagesel_get_type())
#define PAGESEL(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_PAGESEL, Pagesel))
#define PAGESEL_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_PAGESEL, PageselClass))
#define IS_PAGESEL(obj)      (is_a_pagesel((Pagesel*)obj))

typedef struct _PageselClass PageselClass;
typedef struct _Pagesel      Pagesel;


struct _PageselClass {
  GschemDialogClass parent_class;
};

struct _Pagesel {
  GschemDialog     parent_instance;
  GedaType         instance_type;
  GtkWidget       *popup;
  GtkCellRenderer *renderer;        /* For Name Column */
  GtkTreeView     *treeview;
  int              action_height;
  int              row_height;
};

GedaType pagesel_get_type (void) GEDA_CONST;
bool     is_a_pagesel     (Pagesel *pagesel);
void     pagesel_update   (Pagesel *pagesel);
