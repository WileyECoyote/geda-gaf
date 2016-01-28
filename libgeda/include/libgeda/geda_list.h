/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_list.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 2007-2015 Peter Clifton
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Peter Clifton <peter@clifton-electronics.co.uk>
 */

/*! \class GedaList geda_list.h "libgeda/geda_list.h"
 *  \brief GedaType for GedaList Objects.
 *
 *  GedaList is a List object wrapper for GLists. The GedaListclass
 *  provides advanced methods for manipulation of GLists, including
 *  Gobject signals.
 */

#ifndef __GEDA_LIST_H__
#define __GEDA_LIST_H__

#define GEDA_TYPE_LIST            (geda_list_get_type())
#define GEDA_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_LIST, GedaList))
#define GEDA_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_LIST, GedaListClass))
#define GEDA_IS_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GEDA_TYPE_LIST))
#define GEDA_IS_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_LIST))
#define GEDA_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_LIST, GedaListClass))

typedef struct _GedaList      GedaList;
typedef struct _GedaListClass GedaListClass;

struct _GedaList {
  GObject parent;
  GList  *glist;
};

struct _GedaListClass {
  GObjectClass parent;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType  geda_list_get_type (void) GEDA_CONST;

/* It would be nice to add const qualifiers to some of these, but GLib
 * is buggy in this respect, and doesn't have const where necessary. */
GedaList *geda_list_new                     ( void );
void      geda_list_add                     ( GedaList *list, void  *item );
void      geda_list_add_glist               ( GedaList *list, GList *items );
void      geda_list_add_glist_unique        ( GedaList *list, GList *items );
void      geda_list_add_unique              ( GedaList *list, void  *item );
bool      geda_list_add_unique_string       ( GedaList *list, char  *text );
GList    *geda_list_copy_glist              ( GedaList *list );
void     *geda_list_find                    ( GedaList *list, void *item );
int       geda_glist_is_homogeneous_objects ( GList    *list);
bool      geda_list_is_in_list              ( GedaList *list, void *item );
void      geda_list_remove                  ( GedaList *list, void *item );
void      geda_list_unref                   ( GedaList *list );

/*void geda_list_remove_glist( GedaList *list, GList *items ); */ /* Undemanded as yet */
void      geda_list_remove_all              ( GedaList *list );

/*const GList *geda_list_get_glist( GedaList *list ); */

#define geda_list_get_glist(list) ((list->glist) ? ((GList *)(g_list_first (list->glist))) : NULL)
#define geda_list_free_full(list) u_glist_free_full(list->glist, g_free); list->glist = NULL;
#define geda_list_length(list) g_list_length(list->glist)

#define Place_List    toplevel->page_current->place_list
#define Top_Selection toplevel->page_current->selection_list

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_LIST_H__ */

