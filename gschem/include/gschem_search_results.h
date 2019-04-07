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
/*!
 * \file gschem_search_results.h
 *
 * \brief
 */

#define GSCHEM_TYPE_SEARCH_RESULTS           (gschem_search_results_get_type())
#define GSCHEM_SEARCH_RESULTS(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_SEARCH_RESULTS, GschemSearchResults))
#define GSCHEM_SEARCH_RESULTS_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_SEARCH_RESULTS, GschemSearchResultsClass))
#define GSCHEM_IS_SEARCH_RESULTS(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_SEARCH_RESULTS))
#define GSCHEM_SEARCH_RESULTS_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_SEARCH_RESULTS, GschemSearchResultsClass))


enum SearchType {
  SEARCH_TYPE_PATTERN,
  SEARCH_TYPE_REGEX,
  SEARCH_TYPE_SUBSTRING,
  SEARCH_TYPE_PATCH
} SearchType;


typedef struct _GschemSearchResultsClass GschemSearchResultsClass;
typedef struct _GschemSearchResults GschemSearchResults;

struct _GschemSearchResultsClass
{
  GschemBinClass parent_class;
};

struct _GschemSearchResults
{
  GschemBin parent;

  GtkListStore *store;
};


int                  gschem_search_results_find     (GschemSearchResults *state,
                                                     GList               *pages,
                                                     int                  type,
                                                     const char          *text,
                                                     bool                 descend);

GedaType             gschem_search_results_get_type (void);

GschemSearchResults *gschem_search_results_new      ();
