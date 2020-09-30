/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_toplevel.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
/*! \class GedaToplevel geda_toplevel.h "libgeda/geda_toplevel.h"
 *  \brief GedaType for GedaToplevel Objects.
 *
 *  GedaToplevel is a derivative of the GObject class. A Toplevel
 *  object is normally created by each client application after
 *  initializing LibGeda. The GedaToplevel object hold frequently
 *  accessed configuration data and other information such as
 *  Page Objects and directory locations.
 */

#ifndef _LIBGEDA_TOPLEVEL_H
#define _LIBGEDA_TOPLEVEL_H

#if defined(__LP64__) || defined(_LP64)
# define GedaTopType unsigned long
#else
# define GedaTopType unsigned int
#endif

#define GEDA_TYPE_TOPLEVEL            (geda_toplevel_get_type())
#define GEDA_TOPLEVEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_TOPLEVEL, GedaToplevel))
#define GEDA_TOPLEVEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_TOPLEVEL, GedaToplevelClass))
#define GEDA_IS_TOPLEVEL(obj)         (is_a_geda_toplevel((GedaToplevel*)(obj)))
#define GEDA_IS_TOPLEVEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_TOPLEVEL))
#define GEDA_TOPLEVEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_TOPLEVEL, GedaToplevelClass))

typedef struct _GedaToplevelClass GedaToplevelClass;

struct _GedaToplevelClass {
  GObjectClass parent;
};

struct _GedaToplevel {

  /* This is the GObject parent and has nothing to do with LibGeda */
  GObject parent;

  int    open_flags;         /* Control flags for the geda_file_open function. Indexed by FOpenFlags */
  char  *untitled_name;      /* untitled sch basename */
  int    num_untitled;       /* keep track of untitled pages */

  char  *bitmap_directory;   /* path of the bitmaps */

  /* page system */
  Page     *page_current;
  PageList *pages;

  int width;
  int height;

  /* backup variables */
  int auto_save_interval;
  int auto_save_timeout;

  /* used by o_styles.c profiling widths */
  int pin_style;
  int net_style;
  int bus_style;
  int line_style;

  int thick_bus_width;
  int thick_line_width;
  int thick_net_width;
  int thick_pin_width;

  int thin_bus_width;
  int thin_line_width;
  int thin_net_width;
  int thin_pin_width;

  int default_line_end;
  int default_line_type;
  int default_line_width;
  int default_line_length;
  int default_line_space;

  int default_fill_type;
  int default_fill_width;
  int default_fill_pitch1;
  int default_fill_angle1;
  int default_fill_pitch2;
  int default_fill_angle2;

  /* either TRUE or FALSE (color or no color) */
  int image_color;

  /* either TRUE or FALSE (Blk on Wht or Wht on Blk) */
  int invert_images;

  /* either TRUE or FALSE (color or no color) */
  int print_color;

  /* color used color ouput for background */
  int print_color_background;

  /* either landscape or portrait */
  int print_orientation;

  /* either window or limits */
  int print_output_extents;

  /* BUTT, ROUND, SQUARE caps */
  int print_output_capstyle;

  /* landscape printing only */
  int paper_width, paper_height;

  /* setpagedevice orientation option enable (TRUE or FALSE) */
  int setpagedevice_orientation;

  /* setpagedevice pagesize option enable (TRUE or FALSE) */
  int setpagedevice_pagesize;

  /* The name of the prolog file to paste into the Postscript output */
  char *postscript_prolog;

  /* controls if the net consolidation code is used */
  int net_consolidate;

  /* controls if attribute promotion happens */
  int attribute_promotion;

  /* controls if invisible attribs are promoted */
  int promote_invisible;

  /* controls if invisible attribs are kept and not deleted */
  int keep_invisible;

  /* controls the generation of backup (~) files */
  int make_backup_files;

  /* controls whether apps should display the full filename */
  int show_full_path;

  /* List of attributes to always promote */
  GList *always_promote_attributes;

  /* used during creation of attributes */
  int attribute_offset;
  int attribute_font_size;

  /* controls if symbol versions are checked */
  int check_symbol_version;

  /* holds a list of group names displayed in the component select dialog */
  GList *component_groups;

  /* gnetlist specific */
  int   net_naming_priority;
  int   hierarchy_traversal;
  int   hierarchy_uref_mangle;
  int   hierarchy_netname_mangle;
  int   hierarchy_netattrib_mangle;
  char *hierarchy_uref_separator;
  char *hierarchy_netname_separator;
  char *hierarchy_netattrib_separator;
  int   hierarchy_netattrib_order;
  int   hierarchy_netname_order;
  int   hierarchy_uref_order;
  char *unnamed_netname;
  char *unnamed_busname;

  /* Callback function for calculating text bounds */
  RenderedBoundsFunc rendered_text_bounds_func;
  void *rendered_text_bounds_data;

  /* Callback function to load a backup file. */
  LoadBackupQueryFunc load_newer_backup_func;
  void *load_newer_backup_data;

  GList   *weak_refs;             /* Weak references */
};

#ifdef __cplusplus
extern "C" {
#endif

/* geda_toplevel.c */
GedaTopType   geda_toplevel_get_type                (void) GEDA_CONST;
GedaToplevel *geda_toplevel_new                     (void);

bool          is_a_geda_toplevel                    (GedaToplevel *toplevel);
void          geda_toplevel_weakref_notify          (GedaToplevel *toplevel);
void          geda_toplevel_weak_ref                (GedaToplevel *toplevel, WeakNotifyFunc notify_func, void *user_data);
void          geda_toplevel_weak_unref              (GedaToplevel *toplevel, WeakNotifyFunc notify_func, void *user_data);
void          geda_toplevel_add_weak_ptr            (GedaToplevel *toplevel, void *weak_pointer_loc);
void          geda_toplevel_remove_weak_ptr         (GedaToplevel *toplevel, void *weak_pointer_loc);

void          geda_toplevel_add_page                (GedaToplevel *toplevel, Page *page);
int           geda_toplevel_get_auto_save_interval  (GedaToplevel *toplevel);
Page         *geda_toplevel_get_current_page        (GedaToplevel *toplevel);
int           geda_toplevel_get_file_open_flags     (GedaToplevel *toplevel);
int           geda_toplevel_get_image_color         (GedaToplevel *toplevel);
int           geda_toplevel_get_invert_images       (GedaToplevel *toplevel);
int           geda_toplevel_get_make_backups        (GedaToplevel *toplevel);
int           geda_toplevel_get_net_consolidate     (GedaToplevel *toplevel);
Page         *geda_toplevel_get_page_by_id          (GedaToplevel *toplevel, int page_id);
int           geda_toplevel_get_page_count          (GedaToplevel *toplevel);
Page         *geda_toplevel_get_page_down           (GedaToplevel *toplevel);
Page         *geda_toplevel_get_page_up             (GedaToplevel *toplevel);
PageList     *geda_toplevel_get_page_list           (GedaToplevel *toplevel);
char         *geda_toplevel_get_untitled_name       (GedaToplevel *toplevel);

bool          geda_toplevel_is_current_page         (GedaToplevel *toplevel, Page *page);

bool          geda_toplevel_move_page_down          (GedaToplevel *toplevel, Page *page);
bool          geda_toplevel_move_page_up            (GedaToplevel *toplevel, Page *page);

void          geda_toplevel_remove_page             (GedaToplevel *toplevel, Page *page);

bool          geda_toplevel_set_auto_save_interval  (GedaToplevel *toplevel, int interval);
void          geda_toplevel_set_bkloader_query_func (GedaToplevel *toplevel, void *func, ...);
bool          geda_toplevel_set_current_page        (GedaToplevel *toplevel, Page *page);
int           geda_toplevel_set_file_open_flags     (GedaToplevel *toplevel, int open_flags);
bool          geda_toplevel_set_make_backups        (GedaToplevel *toplevel, int make_backups);
void          geda_toplevel_set_net_consolidate     (GedaToplevel *toplevel, int consolidate);
void          geda_toplevel_set_rendered_bounds_func(GedaToplevel *toplevel,
                                                     RenderedBoundsFunc func, void *user_data);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#define geda_toplevel_get_pages(top) geda_list_get_glist (top->pages)
#define geda_toplevel_get_page(top,p) g_list_find(geda_toplevel_get_pages(top), p)

#endif /* _LIBGEDA_TOPLEVEL_H */
