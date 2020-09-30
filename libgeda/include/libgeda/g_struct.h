/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: g_struct.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2010-2015 gEDA Contributors (see ChangeLog for details)
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
 * MA 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef G_STRUCT_H
#define G_STRUCT_H

/* Prerequisites:
 * #include <glib.h>  for GList.
 * Conditional:
 * #include <gdk-pixbuf/gdk-pixbuf.h> for GdkPixbuf
 */
#ifndef GDK_PIXBUF_H
#define GdkPixbuf void
#endif

/* used by world_tiles to set the size of the array */
#define MAX_TILES_X             10
#define MAX_TILES_Y             10

/* -- Component library -- */
/* Component library search modes */
typedef enum { CLIB_EXACT=0, CLIB_GLOB } CLibSearchMode;

typedef struct st_CLibSource CLibSource;
typedef struct st_CLibSymbol CLibSymbol;
typedef struct st_CacheEntry CacheEntry;

/* -- gschem structures (gschem) -- */
typedef struct st_complex COMPLEX;
typedef struct st_conn CONN;         /* uses type GedaObject */
typedef struct st_picture PICTURE;
typedef struct st_tile TILE;         /* uses glist */

typedef struct st_undo UNDO;

/* -- netlist structures (gnetlist) -- */
typedef struct st_cpinlist CPINLIST; /* uses NET, CPINLIST */
typedef struct st_net NET;
typedef struct st_netlist NETLIST;

/* -- sch check structures (gschcheck) -- */
typedef struct st_chkerrs CHKERRS;
typedef struct st_schcheck SCHCHECK;

/* Wrappers around a alternate list mechanism */
typedef struct _GedaList SELECTION;
typedef struct _GedaList PageList;

/* -------------- Start Structure Definitions -------------*/

/* Type definitions for component sources */
/*! \brief Valid types of component source */
enum CLibSourceType {
  CLIB_NONE = 0,
  /*! Directory source */
  CLIB_DIR,
  /*! Command source */
  CLIB_CMD,
  /*! Guile function source */
  CLIB_SCM,
};

/*! \brief Stores data about a particular component source
 *
 * \remarks The strings for category and groups are optional
 * and do not necessarily represent functional characteristics,
 * these are used for organizing the associates symbols when
 * sorting and displaying libraries.
 *
 */
struct st_CLibSource {

  /*! Type of source */
  enum CLibSourceType type;

  /*! Name of source */
  char *name;

  /*! Available symbols (#CLibSymbol) */
  GList *symbols;

  /*! Optional Catagory */
  char *category;

  /*! Path to directory */
  char *directory;

  /*! Optional group */
  char *group;

  /*! Command & arguments for listing symbols */
  char *list_cmd;

  /*! Command & arguments for retrieving symbol data */
  char *get_cmd;

  /*! Scheme function for listing symbols */
  SCM list_fn;

  /*! Scheme function for retrieving symbol data */
  SCM get_fn;
};

/* Component library objects */
/*! \brief Stores data about a particular symbol */
struct st_CLibSymbol {
  /*! The source this symbol is available from */
  CLibSource *source;

  /*! The name of this symbol */
  char *name;

};

/*! \brief Symbol data cache entry */
struct st_CacheEntry {
  /*! Pointer to symbol */
  CLibSymbol *ptr;

  /*! Symbol data */
  char *data;

  /*! Last access */
  time_t accessed;
};

/* ---------------------- gschem structures --------------------- */
struct st_complex {
  int x, y;             /* world origin */

  int angle;                            /* orientation, only multiples
                                         * of 90 degrees allowed */
  /* in degrees */
  int mirror;

  GList *prim_objs;                     /* Primitive objects */
  /* objects which make up the */
  /* complex */
};

#define COMPLEX_INSERTION 1

/*! \brief Structure for connections between GedaObjects
 *
 * The st_conn structure contains a single connection
 * to another object.
 * The connection system in s_conn.c uses this struct
 */
struct st_conn {

  /*! \brief The "other" object connected to this one */
  GedaObject *other_object;

  /*! \brief type of connection. Always in reference to how the "other"
    object is connected to the current one */
  int type;

  /*! \brief x coord of the connection position */
  int x;

  /*! \brief y coord of the connection position */
  int y;

  /*! \brief which endpoint of the current object caused this connection */
  int whichone;

  /*! \brief which endpoint of the "other" object caused this connection */
  int other_whichone;
};

struct st_picture {

  GdkPixbuf   *pixbuf;

  char        *file_content;
  unsigned int file_length;

  double ratio;
  char  *filename;
  int    angle;
  bool   mirrored;
  bool   embedded;

  /* upper is considered the origin */
  int    upper_x, upper_y; /* world */
  int    lower_x, lower_y;

};

#define PICTURE_UPPER_LEFT 0
#define PICTURE_LOWER_RIGHT 1
#define PICTURE_UPPER_RIGHT 2
#define PICTURE_LOWER_LEFT 3

/*! \brief structure to split a page into tiles
 *
 *  This structure is used to track objects that are inside
 *  a smaller TILE of o a page.
 *  See s_tile.c for further informations.
 */
struct st_tile {

  GList *objects;

  int top, left, right, bottom;
};

struct st_undo {

  /* one of these is used, depending on if you are doing in-memory */
  /* or file based undo state saving */
  char  *filename;
  GList *object_list;

  /* either UNDO_ALL or UNDO_VIEWPORT_ONLY */
  int type;

  int modified;

  /* viewport information */
  int left, top, right, bottom;

  /* up and down the hierarchy */
  int hierarchy_up;

  /* used to control which pages are viewable when moving around */
  int page_control; /* WEH: Still sounds hokey */

  UNDO *prev;
  UNDO *next;
};

/* -------- structures below are for mostly for netlist --------- */

/* for every pin on a component */
struct st_cpinlist {

  int       plid;
  PIN_NODE  node_type;             /* PIN_NET_NODE or PIN_BUS_NODE */

  char     *pin_number;
  char     *net_name;              /* this is resolved at very end */
  char     *pin_label;

  NET      *nets;

  CPINLIST *prev;
  CPINLIST *next;
};

/* the net run connected to a pin */
struct st_net {

  int   nid;

  int   net_name_has_priority;
  char *net_name;
  char *pin_label;

  char *connected_to;

  /* Tracking total number of entities connected by this net */
  int  net_num_connected;         /* for nets only */
  bool valid_num_connected;       /* for nets only */

  NET  *prev;
  NET  *next;
};

/* for every component in the object database */
struct st_netlist {

  int         nlid;

  char       *component_uref;

  GedaObject *object_ptr;

  CPINLIST   *cpins;

  char       *hierarchy_tag;
  int         composite_component;

  NETLIST    *prev;
  NETLIST    *next;
};

/* By Jamil Khatib */
/* typedef struct st_chkerrs CHKERRS; */
struct st_chkerrs{

  GedaObject *err_obj;
  CHKERRS    *next;
};

/* Schem check struct */
struct st_schcheck {

  int         no_errors;        /* Number of Errors */
  int         no_warnings;      /* Number of Warnings */

  CHKERRS    *sheet_errs;

  CHKERRS    *float_nets;       /* Header of the list of floating nets */
  int         net_errs;         /* Number of floating nets */

  GedaObject *float_pins;       /* Header of List of floating pins*/
  int         pin_errs;         /* Number of floating pins */

  int         net_names;        /* Number of mismatched net names */
};

/* These do not have type defines in header of this file */

/*------------------------------------------------------------------
 *                             GUILE
 *------------------------------------------------------------------*/
/*! \struct gsubr_t
 *  \brief  Used in function tables for scm_c_define_gsubr
 */
struct gsubr_t {
  char *name;
  int   req;
  int   opt;
  int   rst;
  void *func;
};

struct st_attrib_smob {
  GedaToplevel *world;          /* We need this when updating schematic */
  GedaObject   *attribute;
};

struct st_object_smob {
  GedaToplevel *world;          /* We need this when updating schematic */
  GedaObject   *object;
};

struct st_page_smob {
  GedaToplevel *world;          /* We need this when updating schematic */
  Page         *page;
};

#ifndef GDK_PIXBUF_H
#undef GdkPixbuf
#endif
#endif
