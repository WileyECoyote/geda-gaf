/*! \brief different kind of snapping mechanisms used in TOPLEVEL */
typedef enum {SNAP_OFF, SNAP_GRID, SNAP_RESNAP, SNAP_STATE_COUNT} SNAP_STATE;

typedef struct st_gschem_toplevel GSCHEM_TOPLEVEL;
typedef struct st_stretch STRETCH;

struct st_gschem_toplevel {

  TOPLEVEL *toplevel;

  /* ----------------- main window widgets ---------------- */
  GtkWidget *main_window;
  GtkWidget *drawing_area;

  int        ui_index;

  GtkWidget *add_handlebox;
  GtkWidget *attribute_handlebox;
  GtkWidget *edit_handlebox;
  GtkWidget *page_handlebox;
  GtkWidget *standard_handlebox;
  GtkWidget *zoom_handlebox;

  GSList    *toolbar_mode_grp;   /* Single list of MENU toolbar radios */

  GtkWidget     *h_scrollbar;
  GtkWidget     *v_scrollbar;
  GtkAdjustment *h_adjustment;
  GtkAdjustment *v_adjustment;

  GtkWidget *command_box;
  GtkWidget *command_entry;

  GtkWidget *macro_box;
  GtkWidget *macro_entry;
  GtkWidget *left_label;
  GtkWidget *middle_label;
  GtkWidget *right_label;
  GtkWidget *grid_label;
  GtkWidget *status_label;

  char *keyaccel_string;               /* visual feedback when pressing
                                          keyboard accelerators */
  bool keyaccel_string_source_id;   /* event source ID used by above */

  /* -------------------- Dialog boxes -------------------- */
  /* x_dialog.c */
  GtkWidget *sswindow;          /* Snap Size,  settings: IDS_SNAP_SIZE  */
  GtkWidget *tswindow;          /* text size,  settings: IDS_TEXT_SIZE  */
  GtkWidget *aawindow;          /* Arc attrib, settings: IDS_ARC_ANGLE  */
  GtkWidget *clwindow;          /* Color edit, settings: IDS_COLOR_EDIT */
  GtkWidget *hpwindow;          /* Fill Type,  settings: IDS_FILL_TYPE  */
  GtkWidget *ltwindow;          /* Line Type,  settings: IDS_LINE_TYPE  */
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
  GtkWidget *coord_world;      /* World coordinate label */
  GtkWidget *coord_screen;     /* Screen coordinate window */

  GtkWidget *aewindow;   /* attribute edit */
  GtkWidget *cpwindow;   /* Preferences   settings: IDS_CONFIG_SETTINGS */
  GtkWidget *cswindow;   /* Component     settings: IDS_COMP_SELECT     */
  GtkWidget *iwindow;    /* image write dialog box */
  GtkWidget *mawindow;   /* multi attribute IDS_MULTI_ATTRBIBUTE */
  GtkWidget *pswindow;   /* page select */
  GtkWidget *sowindow;   /* Script open */

  /* ------------------ Picture placement ----------------- */
  GdkPixbuf *current_pixbuf;            /* used by add picture dialog */
  double     pixbuf_wh_ratio;           /* width/height ratio of the pixbuf */
  char      *pixbuf_filename;

  /* ------------------- graphics context ----------------- */
  GdkGC *gc;

  /* ------------------  Drawing surfaces  ---------------- */
  GdkWindow *window;                    /* drawing_area's X drawable */
  GdkPixmap *drawable;                  /* drawable to paint onto */
  cairo_t *cr;                          /* Cairo surface */
  PangoLayout *pl;                      /* Pango layout */

  int win_width, win_height;            /* Actual size of window (?) */

  /* --------------------- Drawing state ------------------ */
  EdaRenderer *renderer;
  int first_wx;
  int first_wy;
  int second_wx;
  int second_wy;
  int third_wx;
  int third_wy;
  int magnetic_wx, magnetic_wy;         /* Position of the magnetic marker*/
  int distance;
  int inside_action;                    /* Are we doing an action? */
  int rubber_visible;                   /* Are there any rubber lines on
					   the screen? */
  int net_direction;                    /* bit field to guess the best net direction */
  int which_grip;                       /* Which grip is being manipulated.
                                           Its range of values depends on the
                                           type of object being manipulated. */
  OBJECT *which_object;                 /* Object being manipulated */

  /* ----------------- Rubberbanding nets ----------------- */
  GList *stretch_list;

  /* ---------------- Gschem internal state --------------- */
  int num_untitled;                     /* keep track of untitled wins */
  int event_state;                      /* Current event state */
  int image_width, image_height;        /* h, w of image write */
  int min_zoom;                         /* minimum zoom factor */
  int max_zoom;                         /* maximum zoom factor */
  int text_alignment;                   /* current alignment of text */
  int inside_redraw;                    /* complex vs list redrawing */
  int drawbounding_action_mode;         /* outline vs bounding box */
  int last_drawb_mode;                  /* last above mode */
  int CONTROLKEY;                       /* control key pressed? */
  int SHIFTKEY;                         /* shift key pressed? */
  int ALTKEY;                           /* alt key pressed? */
  int doing_pan;                        /* mouse pan status flag */
  int buffer_number;                    /* current paste buffer in use */
  GList *clipboard_buffer;              /* buffer for system clipboard integration */

  /* ----------------- rc/user parameters ----------------- */

  /* Display Related */

  /* sets the mininum number of pixels necessary for the grid to be displayed */
  int grid_mode;          /* sets what type of grid to show, can be None, Dots, or Mesh */
  int dots_grid_fixed_threshold;
  int dots_grid_dot_size; /* sets the grid dot size */
  int dots_grid_mode;     /* sets the mode of the dotted grid (either variable or fixed) */

  /* Minimum grid line pitch to display. Applies to major and minor lines. */
  int mesh_grid_threshold;

  /* Zoom Related - Display=>Zoom */
  int warp_cursor;        /* warp the cursor when zooming */
  int zoom_gain;          /* Percentage increase in size for a zoom-in operation */
  int zoom_with_pan;

/* Miscellaneous - in  alphabetical order */
  int action_feedback_mode;   /* can be either OUTLINE or BOUNDINGBOX */

  /* sets the offset (in world coordinates) that are added to netname */
  /* attributes when they are attached to vertical or horizontal nets */
  int add_attribute_offset;

  int attribute_placement_grid;

  /* holds a list of attribute names displayed in the component select dialog */
  GList *component_select_attrlist;

  /* controls if after doing a place the same component can be placed again */
  int continue_component_place;
  int embed_components;   /* controls if complex objects are embedded */
  int enforce_hierarchy;  /* controls how much freedom user has when traversing the hierarchy */
  int include_complex;    /* controls if complex objects are included */
  int keyboardpan_gain;   /* Controls the gain of the keyboard pan */
  int magnetic_net_mode;  /* enables/disables the magnetic net mode ON/OFF */

  /* sets whether nets rubberband as you move them (or connecting comps) */
  int netconn_rubberband;

  int select_slack_pixels;  /* Number of pixels around an object we can still select it with */
  SNAP_STATE snap;          /* Whether/how to snap to grid */
  int snap_size;            /* Snap grid parameter */
  int sort_component_library; /* sort the component library */

  /* Nets and Routing */
  int net_endpoint_mode;    /* can be either NONE, FILLEDBOX, EMPTYBOX, X */
  int net_midpoint_mode;    /* can be either NONE or FILLED or EMPTY */
  int net_direction_mode;   /* controls if the net direction mode is used */
  int net_selection_mode;   /* controls the behaviour when selecting a net */
  int net_selection_state;  /* current status of the net selecting mode */

  int bus_ripper_size;      /* sets size of the bus rippers */
  int bus_ripper_type;      /* sets type of the bus ripper (component or net) */
  int bus_ripper_rotation;  /* sets if the the bus ripper is symmetric or not */
  char *bus_ripper_symname; /* filename of the bus ripper component if set above */

/* Pointer Device, aka Mouse stuff */
  int fast_mousepan;      /* controls if text is completely drawn during mouse pan */
  int drag_can_move;      /* Controls if drag can move objects or not */
  int middle_button;      /* controls what the third mouse button does */
  int mousepan_gain;      /* Controls the gain of the mouse pan */
  int scroll_wheel;       /* controls what the mouse scroll wheel does */
  int pointer_hscroll;    /* controls if the mouse can do horizonal scrolling */
  int third_button;       /* controls what the third mouse button does */

  /* Print Related */
  char *print_command;    /* The command to send postscript to when printing */

  /* System Related */
  int file_preview;         /* controls if the preview area is enabled or not */
  int handleboxes;          /* sets if the handleboxes are enabled or disabled */
  int raise_dialog_boxes;   /* controls if expose events raise dialog boxes */
  int save_ui_settings;     /* controls if EDA config are written when exiting */
  int show_menu_icons;      /* controls menu images are displayed or not */
  int toolbars;             /* sets if the toolbar(s) are enabled or disabled */
  int toolbars_mode;

  /* Scrollbar Stuff */
  int scrollbars;         /* controls if scrollbars are enabled */
  int scrollbar_update;   /* controls if display is updated while scrolling */
  int scrollbars_visible; /* controls if scrollbars are displayed */
  int scrollpan_steps;    /* Number of scroll pan events required to traverse the viewed area */

  /* Text Related Stuff */
  int text_case;
  int text_display_zoomfactor;  /* zoom factor at which text is displayed completely */
  int text_feedback;            /* is text is drawn or not in copy/move/place ops */
  int text_size;

  /* Undo System */
  int undo_levels;        /* number of undo levels stored on disk */
  int undo_control;       /* sets if undo is enabled or not */
  int undo_type;          /* type of undo (disk/memory) */
  int undo_panzoom;       /* sets if pan / zoom info is saved in undo */

  SCM smob;               /* The Scheme representation of this window */
};

struct st_stretch
{
  OBJECT *object;
  int whichone;
};

