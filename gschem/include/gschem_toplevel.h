/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_toplevel.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 Wiley Edward Hill
 *
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 * Date Contributed: November, 4, 2013
 *
 */

/*!
 * \file gschem_toplevel.h
 *
 * \brief Toplevel Data Structure Object
 */

/*! \class GschemToplevel gschem_toplevel.h "gschem_toplevel.h"
 *  \brief Gschem Toplevel Object
 */

#ifndef __GSCHEM_TOPLEVEL_H__
#define __GSCHEM_TOPLEVEL_H__

#define GSCHEM_TYPE_TOPLEVEL            (gschem_toplevel_get_type())
#define GSCHEM_TOPLEVEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GSCHEM_TYPE_TOPLEVEL, GschemToplevel))
#define GSCHEM_TOPLEVEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GSCHEM_TYPE_TOPLEVEL, GschemToplevelClass))
#define GSCHEM_IS_TOPLEVEL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GSCHEM_TYPE_TOPLEVEL))
#define GSCHEM_IS_TOPLEVEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GSCHEM_TYPE_TOPLEVEL))
#define GSCHEM_TOPLEVEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GSCHEM_TYPE_TOPLEVEL, GschemToplevelClass))

typedef struct _GschemToplevel      GschemToplevel;
typedef struct _GschemToplevelClass GschemToplevelClass;

struct _GschemToplevelClass {
  GObjectClass parent;
};

struct _GschemToplevel {

  GObject parent;

  GedaToplevel *toplevel;

  /* ----------------- main window widgets ---------------- */
  GtkWindow *main_window;
  GtkWidget *drawing_area;

  int        ui_index;

  GtkWidget *add_handlebox;
  GtkWidget *attribute_handlebox;
  GtkWidget *edit_handlebox;
  GtkWidget *grid_snap_handlebox;
  GtkWidget *page_handlebox;
  GtkWidget *select_handlebox;
  GtkWidget *standard_handlebox;
  GtkWidget *symbol_handlebox;
  GtkWidget *zoom_handlebox;

  GSList    *toolbar_mode_grp;   /* Single-linked list of MENU toolbar radios */

  GtkWidget *h_scrollbar;
  GtkWidget *v_scrollbar;

  GtkWidget *command_box;
  GtkWidget *command_entry;

  GtkWidget *patch_widget;
  GtkWidget *macro_widget;
  GtkWidget *search_widget;
  GtkWidget *status_bar;

  GtkWidget *bottom_notebook;

  char *keyaccel_string;       /* visual feedback when pressing
                                         keyboard accelerators */
  bool keyaccel_ssid;          /* event source ID used by above */

  /* -------------------- Dialog boxes -------------------- */
  /* x_dialog.c */
  GtkWidget *sswindow;          /* Snap Size,  settings: IDS_SNAP_SIZE  */
  GtkWidget *tswindow;          /* text size,  settings: IDS_TEXT_SIZE  */
  GtkWidget *aawindow;          /* Arc attrib, settings: IDS_ARC_ANGLE  */
  GtkWidget *cawindow;          /* Array edit, settings: IDS_ARRAY_EDIT */
  GtkWidget *clwindow;          /* Color edit, settings: IDS_COLOR_EDIT */
  GtkWidget *hpwindow;          /* Fill Type,  settings: IDS_FILL_TYPE  */
  GtkWidget *ltwindow;          /* Line Type,  settings: IDS_LINE_TYPE  */
  GtkWidget *prwindow;          /* Prop edit,  settings: IDS_PROP_EDIT  */
  GtkWidget *ptwindow;          /* Pin Type,   settings: IDS_PIN_EDIT   */
  GtkWidget *sewindow;          /* Slot edit   settings: IDS_SLOT_EDIT  */
  GtkWidget *tewindow;          /* text edit   settings: IDS_TEXT_EDIT  */
  GtkWidget *ftwindow;          /* find text   settings: IDS_FIND_TEXT  */
  GtkWidget *htwindow;          /* text hide   settings: IDS_HIDE_TEXT  */
  GtkWidget *stwindow;          /* show text   settings: IDS_SHOW_TEXT  */
  GtkWidget *tiwindow;          /* text input  settings: IDS_TEXT_INPUT */
  GtkWidget *trwindow;          /* translate   settings: IDS_TRANSLATE  */

  GtkWidget *hkwindow;         /* Help/Hotkeys settings: IDS_HOTKEYS    */
  GtkWidget *cowindow;         /* Coordinate   settings: IDS_COORDINATES*/
  GtkWidget *world_entry;      /* World coordinate Entry */
  GtkWidget *screen_entry;     /* Screen coordinate window */

  GtkWidget *aewindow;   /* attribute edit settings: IDS_SINGLE_ATTRIB   */
  GtkWidget *cpwindow;   /* Preferences    settings: IDS_CONFIG_SETTINGS */
  GtkWidget *cswindow;   /* Component      settings: IDS_COMP_SELECT     */
  GtkWidget *mawindow;   /* multi attribute IDS_MULTI_ATTRBIBUTE */
  GtkWidget *pswindow;   /* page select    settings: IDS_PAGE_MANAGER */
  GtkWidget *sowindow;   /* Script open */

  /* ------------------ Picture placement ----------------- */
  GdkPixbuf *current_pixbuf;            /* used by add picture dialog */
  float      pixbuf_wh_ratio;           /* width/height ratio of the pixbuf */
  char      *pixbuf_filename;

  /* ------------------  Drawing surfaces  ---------------- */
  GdkWindow   *window;                  /* drawing_area's X drawable */
  cairo_t     *cr;                      /* Cairo surface */

  int world_left;
  int world_right;
  int world_top;
  int world_bottom;

  int screen_width;                      /* Actual size of window (?) */
  int screen_height;

  double   grid_size_factor;
  edaColor grid_minor_color;
  edaColor grid_major_color;

  /* --------------------- Drawing state ------------------ */

  EdaRenderer *cairo_renderer;
  int render_adaptor;

  int first_wx;
  int first_wy;
  int second_wx;
  int second_wy;
  int third_wx;
  int third_wy;
  int magnetic_wx, magnetic_wy;         /* Position of the magnetic marker*/
  int distance;
  int rubber_visible;                   /* Are there any rubber lines on the screen? */
  int rubber_x1;                        /* Used to invalidate rubber drawn based on */
  int rubber_y1;                        /* key states */
  int rubber_x2;                        /* Used to invalidate rubber drawn based on */
  int rubber_y2;

  int net_direction;                    /* bit field to guess the best net direction */
  int grip_half_size;                   /* One half the grip size adjusted for zoom */
  int which_grip;                       /* Which grip is being manipulated.
                                           Its range of values depends on the
                                           type of object being manipulated. */
  GedaObject *which_object;             /* Object being manipulated */
  GedaPath   *temp_path;                /* Path Object being created */

  int override_color;
  int override_net_color;
  int override_bus_color;
  int override_pin_color;

  /* ----------------- Rubberbanding nets ----------------- */
  GList *stretch_list;

  /* ---------------- Gschem internal state --------------- */

  /* Buffer Related */
  int    buffer_number;                 /* current paste buffer in use */
  GList *clipboard_buffer;              /* buffer for system clipboard integration */

  /* Drag&Drop */
  GdkEvent *drag_event;                 /* copy of motion event for Drag&Drop */
  int drag_action;                      /* Controls the GdkDragAction for Drag&Drop */
  int dnd_state;                        /* */
  int dnd_save_state;                   /* what we were doing when pointer left window */

  /* Key States */
  int CONTROLKEY;                       /* control key pressed? */
  int SHIFTKEY;                         /* shift key pressed? */
  int ALTKEY;                           /* alt key pressed? */

  /* retained offset distance */
  int offset;                           /* used by offset mode, previous offset dist. */

  /* Misc status flags and limits */
  int doing_pan;                        /* mouse pan status flag */
  int drawbounding_action_mode;         /* outline vs bounding box */
  int event_state;                      /* Current event state */
  int force_save_as;                    /* Flag to force use file-saveas */
  int inside_redraw;                    /* complex vs list redrawing */
  int last_drawb_mode;                  /* last above mode */
  int min_zoom;                         /* minimum zoom factor */
  int max_zoom;                         /* maximum zoom factor */

  /* Mode Control */
  int inside_action;                    /* Are we doing an action? */
  GschemEvent *action_event;            /* Action Event record */

  /* Pointer Device */
  int pointer_sx;                       /* screen x coordinates */
  int pointer_sy;                       /* screen y coordinates */

  /* Selection Set Related */
  GList *primary_selection;             /* Use in multi-selection set operations */

  /* Sessions */
  char *session_name;                   /* Name of active session */
  bool  auto_sessions;

  /* ----------------- rc/user parameters ----------------- */

/* Display Related */

  int action_color;       /* The color to use for status text inside an action */

  int anti_aliasing;
  int grip_size;

  /* sets the mininum number of pixels necessary for the grid to be displayed */
  int grid_mode;          /* sets what type of grid to show, can be None, Dots, or Mesh */
  int dots_grid_dot_size; /* sets the grid dot size */
  int dots_grid_mode;     /* sets the mode of the dotted grid (either variable or fixed) */
  int dots_grid_threshold;
  int dots_grid_minor_alpha;
  int dots_grid_major_alpha;

  /* Minimum grid line pitch to display. Applies to major and minor lines. */
  int mesh_grid_threshold;
  int mesh_line_width_factor;
  int mesh_grid_minor_alpha;
  int mesh_grid_major_alpha;

  GdkColor dots_grid_minor_color;
  GdkColor dots_grid_major_color;
  GdkColor mesh_grid_minor_color;
  GdkColor mesh_grid_major_color;

  /* controls whether objects are clipped */
  int object_clipping;

  /* Zoom Related - Display=>Zoom */
  int warp_cursor;        /* warp the cursor when zooming */
  int zoom_gain;          /* Percentage increase in size for a zoom-in operation */
  int zoom_with_pan;

/* Imaging Related */
  int image_width, image_height;        /* h, w of image write */

 /* Miscellaneous - in  alphabetical order */
  int action_feedback_mode;   /* can be either OUTLINE or BOUNDINGBOX */

  /* sets the offset (in world coordinates) that are added to netname */
  /* attributes when they are attached to vertical or horizontal nets */
  int add_attribute_offset;

  int attribute_placement_grid;

  int auto_pan;           /* controls if auto-pan is enabled */
  int auto_pan_step;      /* Number of pixels to scroll per auto-pan event */

  int chooser_filter;     /* retains user's file chooser filter preference */

  /* holds a list of attribute names displayed in the component select dialog */
  GList *component_select_attrlist;

  /* controls if after doing a place the same component can be placed again */
  int continue_component_place;
  int embed_components;   /* controls if complex objects are embedded */
  int enforce_hierarchy;  /* controls how much freedom user has when traversing the hierarchy */
  int hierarchy_up_close; /* Control if documents are closed during ascension */
  /* controls if the whole bounding box is used in the auto whichend code */
  int force_boundingbox;
  int include_complex;    /* controls if complex objects are included */
  int keyboardpan_gain;   /* Controls the gain of the keyboard pan */
  int magnetic_net_mode;  /* enables/disables the magnetic net mode ON/OFF */

  /* sets whether nets rubberband as you move them (or connecting comps) */
  int netconn_rubberband;

  int select_slack_pixels;    /* Number of pixels around an object we can still select it with */
  SNAP_STATE snap;            /* Whether/how to snap to grid */
  int snap_size;              /* Snap grid parameter */
  int sort_component_library; /* sort the component library */
  SNAP_STATE old_snap;        /* Use to toggle the snap to the previous state */

  /* Nets and Routing */
  int net_endpoint_mode;    /* can be either NONE, FILLED_BOX, EMPTY_BOX, X */
  int net_midpoint_mode;    /* can be either NONE or FILLED_BOX or EMPTY_BOX */
  int net_direction_mode;   /* controls if the net direction mode is used */
  int net_selection_mode;   /* controls the behaviour when selecting a net */
  int net_selection_state;  /* current status of the net selecting mode */

  int bus_ripper_size;      /* sets size of the bus rippers */
  int bus_ripper_type;      /* sets type of the bus ripper (component or net) */
  int bus_ripper_rotation;  /* sets if the the bus ripper is symmetric or not */
  char *bus_ripper_symname; /* filename of the bus ripper component if set above */

/* Pointer Device, aka Mouse stuff */
  GdkCursor* cursor;        /* Mouse GEDA Cursor Code */
  int drawing_pointer;      /* Mouse GEDA Cursor Code */
  int fast_mousepan;        /* controls if text is completely drawn during mouse pan */
  int drag_can_move;        /* Controls if drag can move objects or not */
  int middle_button;        /* controls what the third mouse button does */
  int mousepan_gain;        /* Controls the gain of the mouse pan */
  int scrollpan_steps;      /* Number of scroll pan events required to traverse the viewed area */
  int scroll_wheel;         /* controls what the mouse scroll wheel does */
  int pointer_hscroll;      /* controls if the mouse can do horizonal scrolling */
  int third_button;         /* controls what the third mouse button does */
  int third_button_cancel;  /* controls if third mouse button cancels actions */

  /* Print Related */
  char *print_command;      /* The command to send postscript to when printing */

  /* System Related */
  int file_preview;         /* controls if the preview area is enabled or not */
  int handleboxes;          /* sets if the handleboxes are enabled or disabled */
  int raise_dialog_boxes;   /* controls when dialogs are brought to foreground */
  int save_ui_settings;     /* controls if EDA config are written when exiting */
  int toolbars;             /* sets if the toolbar(s) are enabled or disabled */
  int toolbars_mode;        /* either text, icons or both */
  int show_toolbar_tips;    /* determines if tips are enbled on toolbar items */

  /* Scrollbar Stuff */
  int scrollbars;           /* controls if scrollbars are enabled */
  int scrollbar_update;     /* controls if display is updated while scrolling */
  int scrollbars_visible;   /* controls if scrollbars are displayed */

  /* Text Related Stuff */
  int text_alignment;           /* current alignment of text */
  int text_case;
  int text_display_zoomfactor;  /* zoom factor at which text is displayed completely */
  int text_feedback;            /* is text is drawn or not in copy/move/place ops */
  int text_size;

  /* Undo System */
  int undo_levels;        /* number of undo levels stored on disk */
  int undo_control;       /* sets if undo is enabled or not */
  int undo_type;          /* type of undo (disk/memory), enumerated UNDO_TYPE */
  int undo_panzoom;       /* sets if pan / zoom info is saved in undo */
  int undo_preserve;      /* sets whether views should be restored when undoing */

  /* ------------- end of rc/user parameters -------------- */

  /* ---------------- History/Session Retention ----------------- */

  GschemPageHistory *page_history;

  char *last_image_path;  /* Use by file dialogs */

  SCM smob;               /* The Scheme representation of this window */

};

#ifdef __cplusplus
extern "C" {
#endif

GedaType        gschem_toplevel_get_type              (void) GEDA_CONST;
GschemToplevel *gschem_toplevel_new                   (void);

void            gschem_toplevel_free                  (GschemToplevel *w_current);
void            gschem_toplevel_free_primary          (GschemToplevel *w_current);
Page           *gschem_toplevel_get_current_page      (GschemToplevel *w_current);
GedaToplevel   *gschem_toplevel_get_geda_toplevel     (GschemToplevel *w_current);
char           *gschem_toplevel_get_last_image_path   (GschemToplevel *w_current);

bool            gschem_toplevel_move_page_down        (GschemToplevel *w_current,
                                                       Page           *page);
bool            gschem_toplevel_move_page_up          (GschemToplevel *w_current,
                                                       Page           *page);

bool            gschem_toplevel_set_current_page      (GschemToplevel *w_current,
                                                       Page           *page);
void            gschem_toplevel_set_grips_size        (GschemToplevel *w_current,
                                                       int             pixels);
void            gschem_toplevel_set_last_image_path   (GschemToplevel *w_current,
                                                       char           *path);


inline int      gschem_toplevel_get_grips_size        (GschemToplevel *w_current)
{
  return w_current->grip_size;
};

inline int      gschem_toplevel_get_grips_half_size   (GschemToplevel *w_current)
{
  if (Current_Page->to_world_x_constant  < 0.5) {
    return (w_current->grip_size >> 1) + 10;
  }

  if (Current_Page->to_world_x_constant  < 1.0) {
    return (w_current->grip_size >> 1) + 8;
  }

  if (Current_Page->to_world_x_constant  < 2.0) {
    return (w_current->grip_size >> 1) + 6;
  }

  if (Current_Page->to_world_x_constant  < 3.0) {
    return (w_current->grip_size >> 1) + 4;
  }

  if (Current_Page->to_world_x_constant  < 4.0) {
    return (w_current->grip_size >> 1) + 2;
  }

  if (Current_Page->to_world_x_constant  < 6.0) {
    return (w_current->grip_size >> 1) + 1;
  }

  return w_current->grip_size >> 1;
};

inline double   gschem_toplevel_get_double_world_size (GschemToplevel *w_current, int grip_size)
{
  return ((double) grip_size) * Current_Page->to_world_x_constant;
};

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* __GSCHEM_TOPLEVEL_H__ */
