/* C header
 * File: s_toplevel.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
 * MA 02111-1301 USA
 */

#ifndef LIBGEDA_TOPLEVEL_H
#define LIBGEDA_TOPLEVEL_H

/* Prerequisites:
 * #include <glib.h>  for GList.
 */

struct st_toplevel {

  /* have to decided on component list stuff */
  /* if it should go in here or not */
  /* leave outside for now */

  GList *RC_list;                       /* List of RC files which have been read in. */

  char *untitled_name;                  /* untitled sch basename */
  char *bitmap_directory;               /* path of the bitmaps */

  int init_left, init_right;            /* Starting values for above */
  int init_top, init_bottom;

  int width, height;                    /* height, width of window */

  int override_color;                   /* used in doing selections */

  int last_ps_color;                    /* used in print code */

  /* page system */
  PAGE *page_current;
  GedaPageList *pages;

  /* show_hidden_text is used to control which text is hidden in gschem */
  int show_hidden_text;

  GList* major_changed_refdes;          /* A list of all refdes's that have */
                                        /* major symbol version changes */
  /* backup variables */
  int auto_save_interval;
  int auto_save_timeout;

  /* BLOCK SET IN GSCHEM, BUT USED IN LIBGEDA - NEEDS A RETHINK */
  int background_color;
  int override_net_color;
  int override_bus_color;
  int override_pin_color;
  /* END BLOCK - ALTHOUGH THERE ARE MORE CASES! */

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

  /* controls whether objects are clipped */
  int object_clipping;

  /* either landscape or portrait */
  int print_orientation;

  /* either TRUE or FALSE (color or no color) */
  int image_color;
  /* either TRUE or FALSE (Blk on Wht or Wht on Blk) */
  int invert_images;

  /* either TRUE or FALSE (color or no color) */
  int print_color;

  /* color used color ouput for background */
  int print_color_background;

  /* setpagedevice orientation option enable (TRUE or FALSE) */
  int setpagedevice_orientation;

  /* setpagedevice pagesize option enable (TRUE or FALSE) */
  int setpagedevice_pagesize;

  /* The name of the prolog file to paste into the Postscript output */
  char *postscript_prolog;

  /* controls if the net consolidation code is used */
  int net_consolidate;

  /*controls if attribute promotion happens */
  int attribute_promotion;

  /* controls if invisible attribs are promoted */
  int promote_invisible;

  /* controls if invisible attribs are kept and not deleted */
  int keep_invisible;

  /* controls the generation of backup (~) files */
  int make_backup_files;

  /* either window or limits */
  int print_output_type;

  /* BUTT, ROUND, SQUARE caps */
  int print_output_capstyle;

  /* landscape printing only */
  int paper_width, paper_height;

  /* controls if the whole bounding box is used in the auto whichend code */
  int force_boundingbox;

  /* List of attributes to always promote */
  GList *always_promote_attributes;

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

  /* Callback functions for object change notification */
  GList *change_notify_funcs;

  /* Callback functions for object attribute change notification */
  GList *attribs_changed_hooks;

  /* Callback functions for object connections change notification */
  GList *conns_changed_hooks;

  /* Callback function for deciding whether to load a backup file. */
  LoadBackupQueryFunc load_newer_backup_func;
  //GSCHEM_TOPLEVEL *load_newer_backup_data;

  GList *weak_refs; /* Weak references */
};


#endif
