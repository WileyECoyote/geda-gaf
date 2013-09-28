/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2013 Ales Hvezda
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

#ifndef G_STRUCT_H
#define G_STRUCT_H

/* Prerequisites:
 * #include <glib.h>  for GList.
 * #include <gdk-pixbuf/gdk-pixbuf.h> for GdkPixbuf
 */

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
typedef struct st_object OBJECT;     /* uses glist, simple struct */
typedef struct st_conn CONN;         /* uses type OBJECT */

typedef struct st_picture PICTURE;
typedef struct st_tile TILE;         /* uses glist */
typedef struct st_page PAGE;         /* uses glist, SELECTION, UNDO, GTimeVal*/

typedef struct st_toplevel TOPLEVEL;
typedef struct st_undo UNDO;

/* -- netlist structures (gnetlist) -- */
typedef struct st_cpinlist CPINLIST; /* uses NET, CPINLIST */
typedef struct st_net NET;
typedef struct st_netlist NETLIST;

/* -- sch check structures (gschcheck) -- */
typedef struct st_chkerrs CHKERRS;
typedef struct st_schcheck SCHCHECK;

/* Wrappers around a new list mechanism */
typedef struct _GedaList SELECTION;
typedef struct _GedaList GedaPageList;

/*! \brief Type of callback function for calculating text bounds */
typedef int(*RenderedBoundsFunc)(void *, OBJECT *, int *, int *, int *, int *);

/*! \brief Type of callback function for object damage notification */
typedef int(*ChangeNotifyFunc)(void *, OBJECT *);

/*! \brief Type of callback function for notification when a new TOPLEVEL is created */
typedef void(*NewToplevelFunc)(TOPLEVEL *, void *);

/*! \brief Type of callback function for notification when an object's attributes change */
typedef void(*AttribsChangedFunc)(void *, OBJECT *);

/*! \brief Type of callback function for notification when an object's connections change */
typedef void(*ConnsChangedFunc)(void *, OBJECT *);

/*! \brief Type of callback function for querying loading of backups */
typedef bool(*LoadBackupQueryFunc)( GString *);

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
 * \Remarks The strings for category and groups are optional
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

struct st_object {
  int type;                             /* Basic information */
  int sid;                              /* sequence id ?? */
  char *name;

  PAGE *page; /* Parent page */

  int w_top;                            /* Bounding box information */
  int w_left;                           /* in world coords */
  int w_right;
  int w_bottom;
  TOPLEVEL *w_bounds_valid_for;

  COMPLEX *complex;
  LINE *line;
  CIRCLE *circle;
  ARC *arc;
  BOX *box;
  TEXT *text;
  PICTURE *picture;
  PATH *path;

  GList *tiles;                         /* tiles */

  GList *conn_list;                     /* List of connections */
  /* to and from this object */

  /* every graphical primitive have more or less the same options. */
  /* depending on its nature a primitive is concerned with one or more */
  /* of these fields. If not, value must be ignored. */
  OBJECT_END line_end;
  OBJECT_TYPE line_type;
  int line_width;
  int line_space;
  int line_length;

  OBJECT_FILLING fill_type;
  int fill_width;
  int fill_angle1, fill_pitch1;
  int fill_angle2, fill_pitch2;

  bool complex_embedded;                /* is embedded component? */
  char *complex_basename;               /* Component Library Symbol name */
  OBJECT *parent;                       /* Parent object pointer */

  int color;                            /* Which color */
  int dont_redraw;                      /* Flag to skip redrawing */
  int selectable;                       /* object selectable flag */
  int selected;                         /* object selected flag */
  int locked_color;                     /* Locked color (used to save */
  /* the object's real color */
  /* when the object is locked) */

  /* controls which direction bus rippers go */
  /* it is either 0 for un-inited, */
  /* 1 for right, -1 for left (horizontal bus) */
  /* 1 for up, -1 for down (vertial bus) */
  int bus_ripper_direction;             /* only valid on buses */

  int font_text_size;                   /* used only with fonts defs */
  GList *font_prim_objs;                        /* used only with fonts defs */

  int whichend;         /* for pins only, either 0 or 1 */
  PIN_TYPE pin_type;    /* for pins only, either NET or BUS */

  /* Tracking total number of entities connected by this net */
  int net_num_connected;          /* for nets only */
  bool valid_num_connected;       /* for nets only */

  GList *attribs;       /* attribute stuff */
  int show_name_value;
  int visibility;
  OBJECT *attached_to;  /* when object is an attribute */
  OBJECT *copied_to;    /* used when copying attributes */

  GList *weak_refs; /* Weak references */

  /* Attribute notification handling */
  int attrib_notify_freeze_count;
  int attrib_notify_pending;

  /* Connection notification handling */
  int conn_notify_freeze_count;
  int conn_notify_pending;
};

/*! \brief Structure for connections between OBJECTs
 *
 * The st_conn structure contains a single connection
 * to another object.
 * The connection system in s_conn.c uses this struct
 */
struct st_conn {
  /*! \brief The "other" object connected to this one */
  OBJECT *other_object;
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
  GdkPixbuf *pixbuf;
  char *file_content;
  unsigned int file_length;

  double ratio;
  char *filename;
  int angle;
  char mirrored;
  char embedded;

  /* upper is considered the origin */
  int upper_x, upper_y; /* world */
  int lower_x, lower_y;

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

struct st_page {

  int pid;

  GList *_object_list;
  SELECTION *selection_list;           /* new selection mechanism */
  GList *place_list;
  OBJECT *object_lastplace;            /* the last found item */

  char *page_filename;
  int CHANGED;                         /* changed flag */
  /*int zoom_factor; no longer used*/
  int left, right, top, bottom;        /* World coord limits */
  double coord_aspectratio;            /* Real worldcoords ratio (?) */

  float to_screen_x_constant;
  float to_screen_y_constant;

  float to_world_x_constant;
  float to_world_y_constant;

  TILE world_tiles[MAX_TILES_X][MAX_TILES_Y];

  /* Undo/Redo Stacks and pointers */
  /* needs to go into page mechanism actually */
  UNDO *undo_bottom;
  UNDO *undo_current;
  UNDO *undo_tos;                      /* Top Of Stack */

  /* up and down the hierarchy */
  /* this holds the pid of the parent page */
  int up;
  /* int down; not needed */

  /* used to control which pages are viewable when moving around */
  int page_control;

  /* backup variables */
  GTimeVal last_load_or_save_time;
  char saved_since_first_loaded;
  int ops_since_last_backup;
  char do_autosave_backup;

  GList *weak_refs;                    /* Weak references */
};

struct st_undo {

  /* one of these is used, depending on if you are doing in-memory */
  /* or file based undo state saving */
  char *filename;
  GList *object_list;

  /* either UNDO_ALL or UNDO_VIEWPORT_ONLY */
  int type;

  /* viewport information */
  int left, top, right, bottom;

  /* up and down the hierarchy */
  int up;
  /* used to control which pages are viewable when moving around */
  int page_control;

  UNDO *prev;
  UNDO *next;
};

/* ------------- structures below are for gnetlist -------------- */

/* for every pin on a component */
struct st_cpinlist {
  int plid;
  int type;                             /* PIN_TYPE_NET or PIN_TYPE_BUS */

  char *pin_number;
  char *net_name;                       /* this is resolved at very end */
  char *pin_label;

  NET *nets;

  CPINLIST *prev;
  CPINLIST *next;
};

/* the net run connected to a pin */
struct st_net {

  int nid;

  int net_name_has_priority;
  char *net_name;
  char *pin_label;

  char *connected_to; /* new to replace above */

  NET *prev;
  NET *next;
};

/* for every component in the object database */
struct st_netlist {

  int nlid;

  char *component_uref;

  OBJECT *object_ptr;

  CPINLIST *cpins;

  char *hierarchy_tag;
  int composite_component;

  NETLIST *prev;
  NETLIST *next;
};

/* By Jamil Khatib */
/* typedef struct st_chkerrs CHKERRS; */
struct st_chkerrs{

  OBJECT * err_obj;
  CHKERRS * next;
};

/* Schem check struct */
struct st_schcheck {
  int no_errors;                /* No of Errors */
  int no_warnings;              /* No of Warinings */

  CHKERRS * sheet_errs;

  CHKERRS *float_nets;           /* Header of the list of floating nets */
  int net_errs;                 /* No of floating nets */

  OBJECT *float_pins;           /* Header of List of floating pins*/
  int pin_errs;                 /* No of floating pins */

  int net_names;                /* No of mismatched net names */
};

/* These do not have type defines in header of this file */
struct st_attrib_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  OBJECT   *attribute;
};

struct st_object_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  OBJECT   *object;
};

struct st_page_smob {
  TOPLEVEL *world;   /* We need this when updating schematic */
  PAGE   *page;
};

#endif
