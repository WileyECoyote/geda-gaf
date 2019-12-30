/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_page.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */
/*! \class Page geda_page.h "libgeda/geda_page.h"
 *  \brief GedaType for GedaPage Objects.
 *
 * GedaPage is a derivative of the GObject class.  When schematic or
 * symbol files are loaded, mosts of the objects described in the file
 * are associated with a GedaPage object, those that are not attached
 * to a GedaPage are called floating objects. Floating objects are
 * GedaText "attribute" object associated with Complexes.
 *
 */
#ifndef __GEDA_PAGE_H__
#define __GEDA_PAGE_H__

#define IS_ACTIVE_PAGE(page) GEDA_IS_PAGE(page) && page == page->toplevel->page_current

#if defined(__LP64__) || defined(_LP64)
# define GedaPageType unsigned long
#else
# define GedaPageType unsigned int
#endif

#define GEDA_TYPE_PAGE            (geda_page_get_type())
#define GEDA_PAGE(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_PAGE, Page))
#define GEDA_PAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_PAGE, PageClass))
#define GEDA_IS_PAGE(obj)         (is_a_geda_page((Page*)(obj)))
#define GEDA_IS_PAGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_PAGE))
#define GEDA_PAGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_PAGE, PageClass))

typedef struct _GedaPageClass PageClass;

struct _GedaPageClass {
  GObjectClass parent;
};

struct _GedaPage {

  GObject parent;

  GedaToplevel *toplevel;

  int pid;
  int seq;

  GList      *_object_list;            /* List of all Object on this page*/
  SELECTION  *selection_list;          /* List of selected objects */
  GList      *place_list;              /* gschem list of objects being placed */
  GedaObject *object_lastplace;        /* the last found item */

  GList      *major_changed_refdes;    /* A list of all refdes's that have */
                                       /* major symbol version changes */
  char       *filename;
  int         CHANGED;                 /* changed flag */

  int left, right, top, bottom;        /* World coord limits */
  int width, height;                   /* Global height, width of page */

  double coord_aspectratio;            /* Real worldcoords ratio (?) */

  float to_screen_x_constant;
  float to_screen_y_constant;

  float to_world_x_constant;
  float to_world_y_constant;

  TILE world_tiles[MAX_TILES_X][MAX_TILES_Y];

  /* Time tracking variables */
  time_t last_load_or_save_time;   /* Currently set but not used */

  /* Undo/Redo Stacks and pointers */
  /* needs to go into page mechanism actually */
  UNDO *undo_bottom;
  UNDO *undo_current;
  UNDO *undo_tos;                  /* Top Of Stack */

  /* used to control which pages are viewable when traversing hierarchy */
  int page_control;                /* WEH sound's hokey */

  /* For hierarchy */
  int hierarchy_up;                /* pid of the parent page */

  /* Flag to indicate if hidden text should be displayed */
  int show_hidden_text;

  /* backup variables */
  char saved_since_first_loaded;
  int  ops_since_last_backup;      /* page->CHANGED since last backup */
  char do_autosave_backup;         /* If true files should be backed up */

  /* Callback function for calculating text bounds */
  RenderedBoundsFunc rendered_text_bounds_func;
  void *rendered_text_bounds_data;

  /* Callback functions for object change notification */
  GedaNotifyList *change_notify_funcs;

  /* Callback functions for object attribute change notification */
  GList *attribs_changed_hooks;

  /* Callback functions for object connections change notification */
  GList *conns_changed_hooks;

  GList *weak_refs;               /* Weak references */
};

#ifdef __cplusplus
extern "C" {
#endif

GedaPageType geda_page_get_type          (void) GEDA_CONST;
bool         is_a_geda_page              (const Page *page);
void         geda_page_debug_print       (Page *page);

Page        *geda_page_new               (void);
Page        *geda_page_new_with_notify   (void);
void         geda_page_weakref_notify    (Page *page);
void         geda_page_weak_ref          (Page *page, WeakNotifyFunc notify_func, void *user_data);
void         geda_page_weak_unref        (Page *page, WeakNotifyFunc notify_func, void *user_data);
void         geda_page_add_weak_ptr      (Page *page, void *weak_pointer_loc);
void         geda_page_remove_weak_ptr   (Page *page, void *weak_pointer_loc);
void         geda_page_freeze_notify     (Page *page);
void         geda_page_thaw_notify       (Page *page);

Page        *geda_page_open              (const char *filename);
int          geda_page_rename            (Page *page, const char *filename);
int          geda_page_save              (Page *page);
void         geda_page_close             (Page *page);
int          geda_page_copy              (Page *page);

void         geda_page_add_object        (Page *page, GedaObject *object);
GedaObject  *geda_page_get_object        (Page *page, int sid) GEDA_WARN_UNUSED_RESULT;
void         geda_page_remove_object     (Page *page, GedaObject *object);

int          geda_page_get_changed       (Page *page) GEDA_WARN_UNUSED_RESULT;
void         geda_page_set_changed       (Page *page, int changed);

GList       *geda_page_get_changed_refdes(Page *page) WARN_UNUSED;

const char  *geda_page_get_filename      (Page *page) WARN_UNUSED;
char        *geda_page_get_filename_dup  (Page *page) WARN_UNUSED;
void         geda_page_set_filename      (Page *page, const char *filename);

int          geda_page_get_pid           (Page *page) GEDA_WARN_UNUSED_RESULT;
GList       *geda_page_get_place_list    (Page *page) GEDA_WARN_UNUSED_RESULT;
void         geda_page_set_place_list    (Page *page, GList *object_list);

GList       *geda_page_get_selection_list(Page *page) GEDA_WARN_UNUSED_RESULT;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#define geda_page_get_selection(pg) geda_list_get_glist (pg->selection_list)

#endif /* __GEDA_PAGE_H__ */
