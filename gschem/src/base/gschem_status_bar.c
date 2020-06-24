/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file gschem_status_bar.c
 *
 * \brief A widget for the "status bar" at the bottom of the window
 *
 */

#include <gschem.h>
#include <geda_widgets.h>

#include <math.h>

/** \defgroup Gschem-Status-Bar Gschem Status Bar
 * @{
 * \brief #GschemStatusBar Class Implmentation
 * \par
 *  This module implements the status bar in gschem.
 */

enum
{
  PROP_0,
  PROP_COORDINATES_MODE,
  PROP_GRID_MODE,
  PROP_GRID_SIZE,
  PROP_HEIGHT,
  PROP_LEFT_BUTTON_TEXT,
  PROP_MIDDLE_BUTTON_TEXT,
  PROP_RIGHT_BUTTON_TEXT,
  PROP_SNAP_MODE,
  PROP_SNAP_SIZE,
  PROP_STATUS_TEXT,
  PROP_STATUS_TEXT_COLOR,
};

enum {
  SET_MIDDLE_ACTION,
  SET_MIDDLE_MOUSEPAN,
  SET_MIDDLE_POPUP,
  SET_MIDDLE_REPEAT,
#ifdef HAVE_LIBSTROKE
  SET_MIDDLE_STROKE,
#endif
  SET_THIRD_POPUP,
  SET_THIRD_MOUSEPAN,
  UPDATE_GRID_LABEL,
  LAST_SIGNAL
};

struct _GschemStatusBarBuffers
{
  char status_left_text_buffer   [STATUS_LEFT_TEXT_BUFFER_SIZE];
  char status_middle_text_buffer [STATUS_MIDDLE_TEXT_BUFFER_SIZE];
  char status_right_text_buffer  [STATUS_RIGHT_LEFT_TEXT_BUFFER_SIZE];
  char status_grid_text_buffer   [STATUS_GRID_TEXT_BUFFER_SIZE];
  char status_coord_text_buffer  [STATUS_COORD_TEXT_BUFFER_SIZE];
  char status_label_text_buffer  [STATUS_STATUS_TEXT_BUFFER_SIZE];
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

static void
finalize (GObject *object);

static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec);

static void
gschem_status_bar_class_init (void *g_class, void *g_class_data);

static void
gschem_status_bar_instance_init (GTypeInstance *instance, void *g_class);

static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec);

static void
update_grid_label (GschemStatusBar *widget);

static GObjectClass *gschem_status_bar_parent_class = NULL;

typedef struct st_popup_menu_entry {
  const char *text;
  const int   signal;
} StatusPopupEntry;

static StatusPopupEntry coord_popup_items[] = {

  { N_( COORD_DISPLAY_OFF ),    COORD_FORMAT_OFF   },
  { N_( COORD_DISPLAY_VEC180 ), COORD_FORMAT_V180  },
  { N_( COORD_DISPLAY_VEC360 ), COORD_FORMAT_V360  },
  { N_( COORD_DISPLAY_XY ),     COORD_FORMAT_XY    },
  { N_( COORD_DISPLAY_COORD ),  COORD_FORMAT_COORD },
  { N_( COORD_DISPLAY_COMMA ),  COORD_FORMAT_COMMA },
  { N_( COORD_DISPLAY_X ),      COORD_FORMAT_X     },
  { N_( COORD_DISPLAY_Y ),      COORD_FORMAT_Y     },
  { N_( COORD_DISPLAY_XONLY ),  COORD_FORMAT_XONLY },
  { N_( COORD_DISPLAY_YONLY ),  COORD_FORMAT_YONLY },
  {NULL} /* sentinel */
};

static StatusPopupEntry middle_popup_items[] = {

  { N_( RC_STR_MID_ACTION ),   SET_MIDDLE_ACTION   },
#ifdef HAVE_LIBSTROKE
  { N_( RC_STR_MID_STROKE ),   SET_MIDDLE_STROKE   },
#endif
  { N_( RC_STR_MID_REPEAT ),   SET_MIDDLE_REPEAT   },
  { N_( RC_STR_MID_MOUSEPAN ), SET_MIDDLE_MOUSEPAN },
  { N_( RC_STR_MID_MOUSEPOP ), SET_MIDDLE_POPUP },
  {NULL} /* sentinel */
};

static StatusPopupEntry third_popup_items[] = {

  { N_( RC_STR_3RD_POPUP ),    SET_THIRD_POPUP    },
  { N_( RC_STR_3RD_PAN ),      SET_THIRD_MOUSEPAN },
  {NULL} /* sentinel */
};

static void coord_options_popup_clicked (GedaMenuItem *menuitem, void *user_data)
{
  GschemStatusBar *bar;
  GtkWidget       *widget;

  unsigned mode = (unsigned)(long)user_data;

  bar    = GEDA_OBJECT_GET_DATA(menuitem, "status-bar");
  widget = (GtkWidget*)bar;

  gschem_status_bar_set_coord_mode (widget, mode);
  gschem_status_bar_set_coordinates (widget, bar->x0, bar->y0, bar->x1, bar->y1);
}

static void status_options_popup_clicked (GedaMenuItem *menuitem, void *user_data)
{
  GtkWidget *widget;
  unsigned   signal = (unsigned)(long)user_data;

  widget = GEDA_OBJECT_GET_DATA(menuitem, "status-bar");

  g_signal_emit (widget, signals[signal], 0);

}

/* -------------- Popup Menu for Mouse Middle Button Options  -------------- */

/*! Creates the context menu for the third button options */
static GtkWidget *create_coord_display_options_popup(GschemStatusBar *status_bar)
{
  GtkWidget *menu;
  int i;

  /* create the context menu coordinates display */
  menu = geda_menu_new();

  for (i = 0; coord_popup_items[i].text != NULL; i++) {

    StatusPopupEntry entry = coord_popup_items[i];

    GtkWidget *popup_item = geda_menu_item_new_with_label (entry.text);

    g_signal_connect (popup_item, "activate",
                      G_CALLBACK(coord_options_popup_clicked),
                      UINT_TO_POINTER(entry.signal));

    GEDA_OBJECT_SET_DATA (popup_item, status_bar, "status-bar");

    geda_menu_append (menu, popup_item);
  }

  status_bar->coord_popup = menu;

  return menu;
}

/*!
 * \brief GschemStatusBar Show Coordinate Display Options Popup
 * \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the coordinates display widget when the right mouse button
 *  is released on the widget.
 */
static void coord_display_options_popup (GtkWidget       *event_box,
                                         GdkEventButton  *event,
                                         GschemStatusBar *status_bar)
{
  GtkWidget *menu;

  if (!status_bar->coord_popup) {
    menu = create_coord_display_options_popup (status_bar);
  }
  else {
    menu = status_bar->coord_popup;
  }

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
}

/*!
 * \brief coordinate Display Indicator Button Released callback
 * \par Function Description
 *  Called when a mouse button is release with the cursor over
 *  the coordinate display indicator. If the 3rd button was
 *  released, a small popup menu is displayed.
 *
 * \sa middle_button_options_popup
 */
static bool coord_display_released (GtkWidget      *label,
                                    GdkEventButton *event,
                                    void           *status_bar)
{
  bool ret_val;

  if (event->button == 3) {

    coord_display_options_popup(label, event, status_bar);

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

/* -------------- Popup Menu for Mouse Middle Button Options  -------------- */

/*! Creates the context menu for the middle button options */
static GtkWidget *create_middle_button_options_popup(GschemStatusBar *status_bar)
{
  GtkWidget *menu;
  int i;

  /* create the middle button context menu */
  menu = geda_menu_new();

  for (i = 0; middle_popup_items[i].text != NULL; i++) {

    StatusPopupEntry entry = middle_popup_items[i];

    GtkWidget *popup_item = geda_menu_item_new_with_label (entry.text);

    g_signal_connect (popup_item, "activate",
                      G_CALLBACK(status_options_popup_clicked),
                      (void*)(long)(entry.signal));

    GEDA_OBJECT_SET_DATA (popup_item, status_bar, "status-bar");

    geda_menu_append (menu, popup_item);
  }
  status_bar->middle_popup = menu;

  return menu;
}

/*!
 * \brief GschemStatusBar Show Middle Mouse Options Popup
 * \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the middle-button status widget when the right mouse button
 *  is released on the widget.
 */
static void middle_button_options_popup (GtkWidget       *event_box,
                                         GdkEventButton  *event,
                                         GschemStatusBar *status_bar)
{
  GtkWidget *menu;

  if (!status_bar->middle_popup) {
    menu = create_middle_button_options_popup (status_bar);
  }
  else {
    menu = status_bar->middle_popup;
  }

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
}

/*!
 * \brief Middle Button Status Indicator Button Released callback
 * \par Function Description
 *  Called when a mouse button is release with the cursor over
 *  the middle button status indicator. If the 3rd button was
 *  released, a small popup menu is displayed.
 *
 * \sa middle_button_options_popup
 */
static bool middle_button_released (GtkWidget      *label,
                                    GdkEventButton *event,
                                    void           *status_bar)
{
  bool ret_val;

  if (event->button == 3) {

    middle_button_options_popup(label, event, status_bar);

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

/* --------------- Popup Menu for Mouse Third Button Options  -------------- */

/*! Creates the context menu for the third button options */
static GtkWidget *create_third_button_options_popup(GschemStatusBar *status_bar)
{
  GtkWidget *menu;
  int i;

  /* create the context menu */
  menu = geda_menu_new();

  for (i = 0; third_popup_items[i].text != NULL; i++) {

    StatusPopupEntry entry = third_popup_items[i];

    GtkWidget *popup_item = geda_menu_item_new_with_label (entry.text);

    g_signal_connect(popup_item, "activate",
                     G_CALLBACK(status_options_popup_clicked),
                     (void*)(long)(entry.signal));

    GEDA_OBJECT_SET_DATA (popup_item, status_bar, "status-bar");

    geda_menu_append (menu, popup_item);
  }

  status_bar->third_popup = menu;

  return menu;
}

/*!
 * \brief GschemStatusBar Show Third Mouse Options Popup
 * \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the third-button status widget when the right mouse button
 *  is released on the widget.
 */
static void third_button_options_popup (GtkWidget       *event_box,
                                        GdkEventButton  *event,
                                        GschemStatusBar *status_bar)
{
  GtkWidget *menu;

  if (!status_bar->third_popup) {
    menu = create_third_button_options_popup (status_bar);
  }
  else {
    menu = status_bar->third_popup;
  }

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
}

/*!
 * \brief Third Button Status Indicator Button Released callback
 * \par Function Description
 *  Called when a mouse button is release with the cursor over
 *  the third button status indicator. If the 3rd button was
 *  released, a small popup menu is displayed.
 *
 * \sa middle_button_options_popup
 */
static bool third_button_released (GtkWidget       *label,
                                   GdkEventButton  *event,
                                   void            *status_bar)
{
  bool ret_val;

  if (event->button == 3) {

    third_button_options_popup(label, event, status_bar);

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}


/* ------------------------ End Popup Menu Callbacks  ---------------------- */

static void gschem_status_bar_reformat_coordinates (GschemStatusBar *gsb)
{
  gschem_status_bar_set_coordinates (GTK_WIDGET(gsb), gsb->x0, gsb->y0, gsb->x1, gsb->y1);
}

static void gschem_status_bar_style_set (GtkWidget *widget, GtkStyle *previous)
{
  int height;

  gtk_widget_style_get (GTK_WIDGET (widget), "height", &height, NULL);

  gschem_status_bar_set_height (widget, height);
}

static void destroy_popup(GtkWidget *widget)
{
  gtk_widget_destroy(widget);
  g_object_ref_sink(widget);
  g_object_unref(widget);
}

/*!
 * \brief Dispose Reference of a  GschemStatusBar object
 * \par Function Description
 */
static void dispose (GObject *object)
{
  GschemStatusBar *status_bar = GSCHEM_STATUS_BAR (object);

  if (status_bar->coord_popup) {
    destroy_popup(status_bar->coord_popup);
    status_bar->coord_popup = NULL;
  }

  if (status_bar->middle_popup) {
    destroy_popup(status_bar->middle_popup);
    status_bar->middle_popup = NULL;
  }

  if (status_bar->third_popup) {
    destroy_popup(status_bar->third_popup);
    status_bar->third_popup = NULL;
  }

  /* lastly, chain up to the parent finalize */
  g_return_if_fail (gschem_status_bar_parent_class != NULL);
  gschem_status_bar_parent_class->dispose (object);
}

/*!
 * \brief Finalize object
 * \par Function Description
 */
static void finalize (GObject *object)
{
  GschemStatusBar *status_bar = GSCHEM_STATUS_BAR (object);

  GEDA_FREE(status_bar->buffers);

  /* lastly, chain up to the parent finalize */
  g_return_if_fail (gschem_status_bar_parent_class != NULL);
  gschem_status_bar_parent_class->finalize (object);
}

/*!
 * \brief Get a property
 * \par Function Description
 * \param [in]     object
 * \param [in]     param_id
 * \param [in,out] value
 * \param [in]     pspec
 */
static void get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec)
{
  GtkWidget  *status_bar = (GtkWidget*)GSCHEM_STATUS_BAR (object);
  const char *string;

  switch (param_id) {
    case PROP_COORDINATES_MODE:
      g_value_set_int (value, gschem_status_bar_get_coord_mode(status_bar));
      break;

    case PROP_GRID_MODE:
      g_value_set_int (value, gschem_status_bar_get_grid_mode (status_bar));
      break;

    case PROP_GRID_SIZE:
      g_value_set_int (value, gschem_status_bar_get_grid_size (status_bar));
      break;

    case PROP_HEIGHT:
      g_value_set_int (value, gschem_status_bar_get_height (status_bar));
      break;

    case PROP_LEFT_BUTTON_TEXT:
      string = gschem_status_bar_get_left_button_text (status_bar);
      g_value_set_string (value, string);
      break;

    case PROP_MIDDLE_BUTTON_TEXT:
      string =  gschem_status_bar_get_middle_button_text (status_bar);
      g_value_set_string (value, string);
      break;

    case PROP_RIGHT_BUTTON_TEXT:
      string = gschem_status_bar_get_right_button_text (status_bar);
      g_value_set_string (value, string);
      break;

    case PROP_SNAP_MODE:
      g_value_set_int (value, gschem_status_bar_get_snap_mode (status_bar));
      break;

    case PROP_SNAP_SIZE:
      g_value_set_int (value, gschem_status_bar_get_snap_size (status_bar));
      break;

    case PROP_STATUS_TEXT:
      string = gschem_status_bar_get_status_text (status_bar);
      g_value_set_string (value, string);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*!
 * \brief Initialize GschemStatusBar class
 * \par Function Description
 * \param [in]  class       The GschemStatusBarClass to be initialized
 * \param [in]  class_data  (unused)
 */
static void gschem_status_bar_class_init (void *class, void *class_data)
{
  GschemStatusBarClass *bar_class = (GschemStatusBarClass*)class;
  GObjectClass    *gobject_class  = G_OBJECT_CLASS (class);
  GtkWidgetClass  *widget_class   = (GtkWidgetClass*)class;
  GParamSpec      *pspec;

  gschem_status_bar_parent_class  = G_OBJECT_CLASS (g_type_class_peek_parent (class));

  bar_class->reformat_coordinates = gschem_status_bar_reformat_coordinates;

  gobject_class->dispose          = dispose;
  gobject_class->finalize         = finalize;

  gobject_class->get_property     = get_property;
  gobject_class->set_property     = set_property;

  widget_class->style_set         = gschem_status_bar_style_set;

  /* Register properties */
  pspec = g_param_spec_int ("coord-mode",
                          _("Coordinates Mode"),
                          _("Sets the Format Mode used to display coordinates on the status bar"),
                              COORD_FORMAT_OFF,
                              COORD_FORMAT_YONLY,
                              COORD_FORMAT_XY,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_COORDINATES_MODE, pspec);

  pspec = g_param_spec_int ("grid-mode",
                          _("Grid Mode"),
                          _("Sets the Grid Mode to display on the status bar"),
                              GRID_NONE,
                              GRID_MESH,
                              GRID_NONE,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_GRID_MODE, pspec);

  pspec = g_param_spec_int ("grid-size",
                          _("Grid Size"),
                          _("Sets the Grid Size to display on the status bar"),
                             -51200,
                              51200,
                              0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_GRID_SIZE, pspec);

  pspec = g_param_spec_int ("height",
                          _("height"),
                          _("Sets the height of the status bar"),
                              0,
                              25,
                              2,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_HEIGHT, pspec);

  pspec = g_param_spec_string ("left-text",
                             _("Left Button Text"),
                             _("Set the string for the Left Button Text"),
                               "none", (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_LEFT_BUTTON_TEXT, pspec);

  pspec = g_param_spec_string ("middle-text",
                             _("Middle Button Text"),
                             _("Set the string for the Middle Button Text"),
                               "none", (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_MIDDLE_BUTTON_TEXT, pspec);

  pspec = g_param_spec_string ("right-text",
                             _("Right Button Text"),
                             _("Set the string for the Right Button Text"),
                               "none", (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_RIGHT_BUTTON_TEXT, pspec);

  pspec = g_param_spec_int ("snap-mode",
                          _("Snap Mode"),
                          _("Sets the Snap Mode to display on the status bar"),
                             SNAP_OFF,
                             SNAP_RESNAP,
                             SNAP_OFF,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_SNAP_MODE, pspec);

  pspec = g_param_spec_int ("snap-size",
                          _("Snap Size"),
                          _("Sets the Snap Size to display on the status bar"),
                            -MAX_SNAP_SIZE,
                             MAX_SNAP_SIZE,
                             MIN_SNAP_SIZE,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_SNAP_SIZE, pspec);

  pspec = g_param_spec_string ("status-text",
                             _("Status Text"),
                             _("Set the string for the text on the Status Bar"),
                               "none", (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_STATUS_TEXT, pspec);

  pspec = g_param_spec_int ("status-text-color",
                          _("Status State"),
                          _("Status State"),
                             0,
                             MAX_COLORS,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_STATUS_TEXT_COLOR, pspec);

  /*!
  * GschemStatusBar:height:
  * Sets the status_height of the status bar widget.
  */
  pspec = g_param_spec_int ("height",
                          _("Status Bar Height"),
                          _("Set or get the height of the status bar"),
                             0,
                             25,
                             2,
                            (G_PARAM_READABLE));

  gtk_widget_class_install_style_property (widget_class, pspec);

  signals[SET_MIDDLE_ACTION]   = g_signal_new ("set-middle-action",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_FIRST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 middle_action),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);

  signals[SET_MIDDLE_MOUSEPAN] = g_signal_new ("set-middle-pan",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_LAST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 middle_pan),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);

  signals[SET_MIDDLE_POPUP]   = g_signal_new ("set-middle-popup",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_LAST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 middle_pop),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);

  signals[SET_MIDDLE_REPEAT]   = g_signal_new ("set-middle-repeat",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_LAST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 middle_repeat),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);
#ifdef HAVE_LIBSTROKE

  signals[SET_MIDDLE_STROKE]   = g_signal_new ("set-middle-stroke",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_LAST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 middle_stroke),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);
#endif

  signals[SET_THIRD_POPUP]     = g_signal_new ("set-third-popup",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_FIRST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 third_popup),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);

  signals[SET_THIRD_MOUSEPAN]  = g_signal_new ("set-third-pan",
                                                gschem_status_bar_get_type(),
                                                G_SIGNAL_RUN_LAST,
                                                G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                 third_pan),
                                                NULL, NULL,
                                                geda_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);
}


/*!
 * \brief Get the Status Bar grid mode
 * \par Function Description
 *  This function returns the grid mode shown on the status bar.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The grid mode
 */
int gschem_status_bar_get_grid_mode (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return (GSCHEM_STATUS_BAR(widget))->grid_mode;
#else
  int ret_val = -1;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      ret_val = gsb->grid_mode;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}


/*!
 * \brief Get the Status Bar grid size
 * \par Function Description
 *  This function returns the grid size shown on the status bar.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The grid size
 */
int gschem_status_bar_get_grid_size (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return (GSCHEM_STATUS_BAR(widget))->grid_size;
#else
  int ret_val = -1;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      ret_val = gsb->grid_size;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}

/*!
 * \brief Get the Height of the Status Bar
 * \par Function Description
 *  This function returns the height of the status bar.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The height
 */
int gschem_status_bar_get_height (GtkWidget *widget)
{
  int ypad = -1;

#if defined (G_DISABLE_ASSERT)
  g_object_get (GTK_MISC (((GschemStatusBar*)widget)->left_label), "ypad", &ypad, NULL);
#else

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      g_object_get (GTK_MISC (((GschemStatusBar*)widget)->left_label), "ypad", &ypad, NULL);
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
  return ypad;
}

/*!
 * \brief Get the Status Bar left button text
 * \par Function Description
 *  This function returns the text of left button on the status bar.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The left button text
 */
const char *gschem_status_bar_get_left_button_text (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_widget_get_text (gsb->left_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->left_label)) {
        ret_val = geda_label_widget_get_text (gsb->left_label);
      }
      else {
        BUG_MSG("left_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}

/*!
 * \brief Get the Status Bar middle button text
 * \par Function Description
 *  This function returns the text of middle button on the status bar.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The middle button text
 */
const char *gschem_status_bar_get_middle_button_text (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_widget_get_text (gsb->middle_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->middle_label)) {
        ret_val = geda_label_widget_get_text(gsb->middle_label);
      }
      else {
        BUG_MSG("middle_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}


/*!
 * \brief Get the Status Bar right button text
 * \par Function Description
 *  This function returns the text of right button on the status bar.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The right button text
 */
const char *gschem_status_bar_get_right_button_text( GtkWidget *widget)
{

#if defined (G_DISABLE_ASSERT)
  return geda_label_widget_get_text (gsb->right_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->right_label)) {
        ret_val = geda_label_widget_get_text (gsb->right_label);
      }
      else {
        BUG_MSG("right_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}

/*!
 * \brief Get the Status Bar Coordinates Mode
 * \par Function Description
 *  This function returns the coordinates display mode used by
 *  the #GschemStatusBar. If \a widget is not GschemStatusBar,
 *  aka w_current->status_bar is NULL the function returns 0
 *  without squealing.
 *
 * \param [in] widget This GschemStatusBar
 *
 * \return The snap mode
 */
int gschem_status_bar_get_coord_mode (GtkWidget *widget)
{
  if (widget && GSCHEM_IS_STATUS_BAR(widget)) {
    return ((GschemStatusBar*)widget)->coord_mode;
  }
  return 0;
}

/*!
 * \brief Get the Status Bar  snap mode
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 *
 * \return The snap mode
 */
int gschem_status_bar_get_snap_mode (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return (GSCHEM_STATUS_BAR(widget))->snap_mode;
#else
  int ret_val = -1;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      ret_val = gsb->snap_mode;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}


/*!
 * \brief Get the Status Bar snap size
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 *
 * \return The snap size
 */
int gschem_status_bar_get_snap_size (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return (GSCHEM_STATUS_BAR(widget))->snap_size;
#else
  int ret_val = -1;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      ret_val = gsb->snap_size;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}

/*!
 * \brief Get the Status Bar status text
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 *
 * \return The status text
 */
const char *gschem_status_bar_get_status_text (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_widget_get_text (gsb->status_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->status_label)) {
        ret_val = geda_label_widget_get_text (gsb->status_label);
      }
      else {
        BUG_MSG("status_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
  return ret_val;
#endif
}


/*!
 * \brief Get/register GschemStatusBar type.
 * \par Function Description
 */
GedaType gschem_status_bar_get_type (void)
{
  static volatile GedaType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemStatusBarClass),
      NULL,                                      /* base_init */
      NULL,                                      /* base_finalize */
      gschem_status_bar_class_init,              /* GClassInitFunc */
      NULL,                                      /* class_finalize */
      NULL,                                      /* class_data */
      sizeof(GschemStatusBar),
      0,                                         /* n_preallocs */
      gschem_status_bar_instance_init,           /* GInstanceInitFunc */
    };

    type = g_type_register_static (GTK_TYPE_HBOX, "GschemStatusBar", &info, 0);
  }

  return type;
}

/* Macro for gschem_status_bar_setup_buffers. This macro is undefined immediately
 * after the function.
 */
#define TheTarget(target) (unsigned long int*)(dest + offsetof(GschemStatusBar, target))

/*!
 * \brief Gschem Status Bar Setup Buffers
 * \par Function Description
 *  This function dynamically allocates memory for a structure similar to
 *  private structures used by GtkWidgets, except that we don't use the
 *  g_type_class_add_private() function. Currently, the structure contains
 *  only string buffers but other data could be added. The buffers are used
 *  to create compound strings for status-bar text rather than using strdup
 *  or derivatives to constantly allocate and de-allocate trival amounts of
 *  ram. To facilitates usage, the status-bar object provides public pointers
 *  to these buffers. The pointers are defined as char * const, so the content
 *  can be changed but the pointers can not. Since the pointers point to
 *  dynamicaly allocated ram, we must initially write to the read only ram
 *  that the pointers are pointing to, this function does that.
 *
 * \param [in] widget This GschemStatusBar
 */
static GschemStatusBarBuffers *gschem_status_bar_setup_buffers (GschemStatusBar *widget)
{
  unsigned int nbytes = sizeof(GschemStatusBarBuffers);

  GschemStatusBarBuffers *buffers = GEDA_MEM_ALLOC(nbytes);

  if (buffers) {

    unsigned long int dest;

    /* zero-out the newly allocated memory */
    unsigned char *ptr = (unsigned char*)buffers;
    while (nbytes-- > 0)
      *ptr++ = 0;

    /* We can not change pointers directly because we defined as const,
     * so we address indirectly using structure offsets */
    dest                         = (long int)widget;
   *TheTarget(left_label_text)   = (long int)&(buffers->status_left_text_buffer[0]);
   *TheTarget(middle_label_text) = (long int)&(buffers->status_middle_text_buffer[0]);
   *TheTarget(right_label_text)  = (long int)&(buffers->status_right_text_buffer[0]);
   *TheTarget(grid_label_text)   = (long int)&(buffers->status_grid_text_buffer[0]);
   *TheTarget(coord_label_text)  = (long int)&(buffers->status_coord_text_buffer[0]);
   *TheTarget(status_label_text) = (long int)&(buffers->status_label_text_buffer[0]);

  }
  return buffers;
}
#undef TheTarget

/*!
 * \brief Initialize GschemStatusBar instance
 * \par Function Description
 * \param [in,out] instance The GschemStatusBar being initialized.
 * \param [in]     g_class  The class of the type the instance is created for.
 */
static void gschem_status_bar_instance_init (GTypeInstance *instance, void *g_class)
{
  GschemStatusBar *bar = (GschemStatusBar*)instance;
  EdaConfig  *cfg;

  GtkWidget  *coord_event;
  GtkWidget  *middle_event;
  GtkWidget  *third_event;
  GtkWidget  *separator;
  GtkBox     *status_box;

  const char *coord_label_tip;
  const char *left_label_tip;
  const char *middle_label_tip;
  const char *right_label_tip;
  const char *grid_label_tip;
  const char *status_label_tip;
  const char *status_bar_tip;

  coord_label_tip  = _("Coordinate display, right click for format options");
  left_label_tip   = _("Left pointer button assignment");
  middle_label_tip = _("Middle pointer button assignment, right click for options");
  right_label_tip  = _("Right pointer button assignment, right click for options");
  grid_label_tip   = _("Indicates the current snap, grid units or state");
  status_label_tip = _("Indicates the current command state");
  status_bar_tip   = _("Gschem status bar");

  g_return_if_fail (bar != NULL);

  const char *grp;
  const char *key;

  cfg = eda_config_get_user_context();
  grp = WIDGET_CONFIG_GROUP;
  key = "status-coord-mode";
  i_var_restore_group_integer(cfg, grp, key, &bar->coord_mode, COORD_FORMAT_XY);

  bar->coord_popup  = NULL;
  bar->middle_popup = NULL;
  bar->third_popup  = NULL;

  bar->buffers = gschem_status_bar_setup_buffers (bar);

  status_box = (GtkBox*)bar;

  gtk_widget_push_composite_child ();

  bar->left_label = geda_aligned_visible_label_new (NULL, STATUS_XALIGN, STATUS_YALIGN);
  gtk_box_pack_start (status_box, bar->left_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (bar->left_label, left_label_tip);

  separator = geda_vseparator_new ();
  gtk_widget_show (separator);
  gtk_box_pack_start (status_box, separator, FALSE, FALSE, 0);

  middle_event = gtk_event_box_new();
  gtk_widget_show (middle_event);
  gtk_box_pack_start (status_box, middle_event, FALSE, FALSE, 0);

  bar->middle_label = geda_aligned_visible_label_new (NULL, STATUS_XALIGN, STATUS_YALIGN);
  geda_container_add(middle_event, bar->middle_label);
  gtk_widget_set_tooltip_text (bar->middle_label, middle_label_tip);

  separator = geda_vseparator_new ();
  gtk_widget_show (separator);
  gtk_box_pack_start (status_box, separator, FALSE, FALSE, 0);

  third_event = gtk_event_box_new();
  gtk_widget_show (third_event);
  gtk_box_pack_start (status_box, third_event, FALSE, FALSE, 0);

  bar->right_label = geda_aligned_visible_label_new (NULL, STATUS_XALIGN, STATUS_YALIGN);
  geda_container_add(third_event, bar->right_label);
  gtk_widget_set_tooltip_text (bar->right_label, right_label_tip);

  separator = geda_vseparator_new ();
  gtk_widget_show (separator);
  gtk_box_pack_start (status_box, separator, FALSE, FALSE, 0);

  bar->grid_label = geda_aligned_visible_label_new (NULL, STATUS_XALIGN, STATUS_YALIGN);
  gtk_box_pack_start (status_box, bar->grid_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (bar->grid_label, grid_label_tip);

  separator = geda_vseparator_new ();
  gtk_widget_show (separator);
  gtk_box_pack_start (status_box, separator, FALSE, FALSE, 1);

  coord_event = gtk_event_box_new();
  gtk_widget_show (coord_event);
  gtk_box_pack_start (status_box, coord_event, FALSE, FALSE, 0);

  bar->coord_label = geda_aligned_visible_label_new (NULL, STATUS_XALIGN, STATUS_YALIGN);
  geda_label_widget_set_text(bar->coord_label, _(COORD_DISPLAY_OFF));
  geda_container_add(coord_event, bar->coord_label);
  gtk_widget_set_tooltip_text (bar->coord_label, coord_label_tip);

  separator = geda_vseparator_new ();
  gtk_widget_show (separator);
  gtk_box_pack_start (status_box, separator, FALSE, FALSE, 0);

  bar->status_label = geda_aligned_visible_label_new (NULL, STATUS_XALIGN, STATUS_YALIGN);
  gtk_box_pack_end (status_box, bar->status_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (bar->status_label, status_label_tip);

  {
    AtkObject *obj;
    obj = gtk_widget_get_accessible((GtkWidget*)bar);
    atk_object_set_name (obj, status_bar_tip);
    atk_object_set_description(obj, status_bar_tip);
  }

  gtk_widget_pop_composite_child ();

  g_signal_connect (bar, "notify::coord-mode",
                    G_CALLBACK (G_STRUCT_OFFSET (GschemStatusBarClass,
                                                 reformat_coordinates)),
                    NULL);

  signals[UPDATE_GRID_LABEL] =
    g_signal_new_class_handler ("update-grid-label",
                                 gschem_status_bar_get_type (),
                                 G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                                 G_CALLBACK (update_grid_label),
                                 NULL, NULL,
                                 geda_marshal_VOID__VOID,
                                 G_TYPE_NONE, 0);

  g_signal_connect (coord_event, "button-release-event",
                    G_CALLBACK (coord_display_released),
                    bar);

  g_signal_connect (middle_event, "button-release-event",
                    G_CALLBACK (middle_button_released),
                    bar);

  g_signal_connect (third_event, "button-release-event",
                    G_CALLBACK (third_button_released),
                    bar);
}

/*!
 * \brief Set the mode used to display coordinates on the status bar
 * \par Function Description
 *  This routine sets the coordinate display mode on the status bar,
 *  and, if the value is not a "solo" format, save the value to the
 *  key file. The "solo" formats are not saved because it is assumed
 *  users would not want to retain this state between sessions.
 *
 * \param [in] widget This GschemStatusBar
 * \param [in] mode   The coordinate mode
 */
void gschem_status_bar_set_coord_mode (GtkWidget *widget, int mode)
{
  GschemStatusBar *gsb;

  inline unsigned get_coord_mode(int new_mode) {
    gsb->coord_mode &= ~COORD_FORMAT_VECTOR; /* Clear off any old vector bits */
    return ((new_mode & COORD_FORMAT_V180) || (new_mode & COORD_FORMAT_V360))
                                            ? gsb->coord_mode |= new_mode : new_mode;
  }

  if (GSCHEM_IS_STATUS_BAR(widget)) {

    gsb = (GschemStatusBar*)widget;
    gsb->coord_mode = get_coord_mode(mode);

    if (mode < COORD_FORMAT_X) {

      EdaConfig  *cfg = eda_config_get_user_context ();
      const char *grp = WIDGET_CONFIG_GROUP;
      const char *key = "status-coord-mode";
      eda_config_set_integer (cfg, grp, key, gsb->coord_mode);
    }
  }
  else {
    BUG_MSG("widget is not a GschemStatusBar");
  }
}

/*!
 * \brief Set the grid mode displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] mode   The grid mode
 */
void gschem_status_bar_set_grid_mode (GtkWidget *widget, int mode)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->grid_mode = mode;
#else

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->grid_mode = mode;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the grid size displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] size   The grid size
 */
void gschem_status_bar_set_grid_size (GtkWidget *widget, int size)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->grid_size = size;
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->grid_size = size;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the Height of the Status Bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] height The new height
 */
void gschem_status_bar_set_height (GtkWidget *widget, int height)
{
  GschemStatusBar *status_bar;

#if defined (G_DISABLE_ASSERT)

  status_bar = GSCHEM_STATUS_BAR(widget);
  gtk_misc_set_padding ((GtkMisc*)status_bar->left_label, STATUS_XPAD, height);
  gtk_misc_set_padding ((GtkMisc*)status_bar->middle_label, STATUS_XPAD, height);
  gtk_misc_set_padding ((GtkMisc*)status_bar->right_label, STATUS_XPAD, height);
  gtk_misc_set_padding ((GtkMisc*)status_bar->grid_label, STATUS_XPAD, height);
  gtk_misc_set_padding ((GtkMisc*)status_bar->status_label, STATUS_XPAD, height);
  gtk_misc_set_padding ((GtkMisc*)status_bar->coord_label, STATUS_XPAD, height);

#else

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      status_bar = (GschemStatusBar*)widget;
      gtk_misc_set_padding ((GtkMisc*)status_bar->left_label, STATUS_XPAD, height);
      gtk_misc_set_padding ((GtkMisc*)status_bar->middle_label, STATUS_XPAD, height);
      gtk_misc_set_padding ((GtkMisc*)status_bar->right_label, STATUS_XPAD, height);
      gtk_misc_set_padding ((GtkMisc*)status_bar->grid_label, STATUS_XPAD, height);
      gtk_misc_set_padding ((GtkMisc*)status_bar->status_label, STATUS_XPAD, height);
      gtk_misc_set_padding ((GtkMisc*)status_bar->coord_label, STATUS_XPAD, height);
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the left button text displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] text   The text
 */
void gschem_status_bar_set_left_button_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_widget_set_text ((GSCHEM_STATUS_BAR(widget))->right_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->left_label)) {
        geda_label_widget_set_text (gsb->left_label, text);
        //g_object_notify (G_OBJECT (widget), "left-text");
      }
      else {
        BUG_MSG("left_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}


/*!
 * \brief Set the middle button text displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] text   The text
 */
void gschem_status_bar_set_middle_button_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_widget_set_text ((GSCHEM_STATUS_BAR(widget))->middle_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->middle_label)) {
        geda_label_widget_set_text (gsb->middle_label, text);
      }
      else {
        BUG_MSG("middle_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the right button text displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] text   The text
 */
void gschem_status_bar_set_right_button_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_widget_set_text ((GSCHEM_STATUS_BAR(widget))->right_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->right_label)) {
        geda_label_widget_set_text ( gsb->right_label, text);
      }
      else {
        BUG_MSG("right_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the snap mode displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] mode   The snap mode
 */
void gschem_status_bar_set_snap_mode (GtkWidget *widget, int mode)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->snap_mode = mode;
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->snap_mode = mode;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the snap size displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] size   The snap size
 */
void gschem_status_bar_set_snap_size (GtkWidget *widget, int size)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->snap_size = size;
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->snap_size = size;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set the Coordinate text displayed on the status bar
 * \par Function Description
 *  This routine sets the coordinate string displayed on the status bar.
 *  The x0 and y0 arguments are only used in vector mode, otherwise both
 *  are ignored. In vector mode, if the x0 argument is equal to negative
 *  zero, yes -0, the format specified prior to changing to vector mode
 *  is used. In our scheme, vector mode is bit 1 of mode, the other bits
 *  are used for the "other" formats. For the other formats we calculate
 *  the index to the format string by counting bit shifts after clearing
 *  bit 1, which may be set but gschem is not inside an action.
 *
 * \param [in] widget  This GschemStatusBar
 * \param [in] x0      First abscissa or -0 in not inside an action
 * \param [in] y0      First ordinate, used if in vectored mode and x0
 * \param [in] x1      Second abscissa, is X value of the cursor position
 * \param [in] y1      Second ordinate, is Y value of the cursor position
 *
 * \note All coordinates must be world (since this module has no w_current).
 *
 * \sa gschem_status_bar_get_coord_mode gschem_status_bar_set_coord_mode
 */
void gschem_status_bar_set_coordinates (GtkWidget *widget, int x0, int y0, int x1, int y1)
{
  GschemStatusBar *status_bar;
  char  *text;

  struct st_coordinate_formats {
    const char *text;
  } coordinate_formats[] = {
    {_(COORD_DISPLAY_OFF)},
    {"%d<%.1f"},              /* COORD_FORMAT_V180 */
    {"%d<<%.1f"},             /* COORD_FORMAT_V360 */
    {"X=%d, Y=%d"},           /* COORD_FORMAT_XY */
    {"(%d, %d)"},             /* COORD_FORMAT_COORD */
    {"%d, %d"},               /* COORD_FORMAT_COMMA */
    {"X=%d"},                 /* COORD_FORMAT_X */
    {"y=%d"},                 /* COORD_FORMAT_Y */
    {"X=%d"},                 /* COORD_FORMAT_XONLY */
    {"y=%d"},                 /* COORD_FORMAT_YONLY */
  };

  /* Save coodinates in GschemStatusBar, see gschem_status_bar.h */
  inline void save_coordinates(void) {
    status_bar->x0 = x0;
    status_bar->y0 = x0;
    status_bar->x1 = x1;
    status_bar->y1 = y1;
  }

  /* Returns string to display on the status bar */
  inline char *get_coordinates_text(unsigned mode) {

    char  *string;
    int    index;

    index = 0;

    /* Check if vector format and valid first abscissa */
    if ((mode & COORD_FORMAT_V180  ||
         mode & COORD_FORMAT_V360) && x0 != -0) {

      double radians;
      float  degrees;
      int    length;

      /* Get the magnitude*/
      length = geda_math_line_length(x0, y0, x1, y1);

      /* Get the angle */
      radians = atan2((y1 - y0), (x1 - x0));
      degrees = radians * 180.0 / M_PI;

      if (mode & COORD_FORMAT_V360) {

        if (degrees < 0) {
          degrees = degrees + 360;
        }

        index = COORD_FORMAT_V360;
      }
      else {

        index = COORD_FORMAT_V180;
      }

      string = geda_sprintf(coordinate_formats[index].text, length, degrees);
    }
    else {

      /* Clear the vector bits, for case not inside_action */
      mode &= ~COORD_FORMAT_VECTOR;

      /* Look for next bit, the shift count is index to the string */
      while(mode) {
        index++;
        mode = mode>>1;
      }

      string = geda_sprintf(coordinate_formats[index].text, x1, y1);
    }

    return string;
  }

#if defined (G_DISABLE_ASSERT)

  status_bar = GSCHEM_STATUS_BAR(widget);
  text       = get_coordinates_text(status_bar->coord_mode);
  geda_label_widget_set_text (status_bar->coord_label, text);
  save_coordinates();

#else

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
    text = NULL;
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      status_bar = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(status_bar->coord_label)) {
        text = get_coordinates_text(status_bar->coord_mode);
        geda_label_widget_set_text (status_bar->coord_label, text);
        save_coordinates();
      }
      else {
        BUG_MSG("coord_label is not a GedaLabel");
        text = NULL;
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
      text = NULL;
    }
  }

#endif

  GEDA_FREE(text);
}

/*!
 * \brief Set the status text displayed on the status bar
 * \par Function Description
 * \param [in] widget This GschemStatusBar
 * \param [in] text   The status text
 */
void gschem_status_bar_set_status_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_widget_set_text ((GSCHEM_STATUS_BAR(widget))->status_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->status_label)) {
        geda_label_widget_set_text (gsb->status_label, text);
      }
      else {
        BUG_MSG("status_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a ");
    }
  }
#endif
}

/*!
 * \brief Set the status text color
 * \par Function Description
 *  Changes the status text color to show if the current editing
 *  action is active or not.
 *
 * \param [in] widget This GschemStatusBar
 * \param [in] index  The state to visualize
 */
void gschem_status_bar_set_status_text_color (GtkWidget *widget, int index)
{
  GdkColor *color = geda_color_x11_color_from_index(index);

#if defined (G_DISABLE_ASSERT)
  GschemStatusBar *gsb = (GschemStatusBar*)widget;
  gtk_widget_modify_fg (GTK_WIDGET (gsb->status_label), GTK_STATE_NORMAL, color);

#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->status_label)) {
        gtk_widget_modify_fg (GTK_WIDGET (gsb->status_label), GTK_STATE_NORMAL, color);
      }
      else {
        BUG_MSG("status_label is not a GedaLabel");
      }
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*!
 * \brief Set a gobject property
 * \par Function Description
 */
static void set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec)
{
  GtkWidget *status_bar = (GtkWidget*)GSCHEM_STATUS_BAR (object);

  switch (param_id) {

    case PROP_COORDINATES_MODE:
      gschem_status_bar_set_coord_mode(status_bar, g_value_get_int (value));
      break;

    case PROP_GRID_MODE:
      gschem_status_bar_set_grid_mode (status_bar, g_value_get_int (value));
      break;

    case PROP_GRID_SIZE:
      gschem_status_bar_set_grid_size (status_bar, g_value_get_int (value));
      break;

    case PROP_HEIGHT:
      gschem_status_bar_set_height (status_bar, g_value_get_int (value));
      break;

    case PROP_LEFT_BUTTON_TEXT:
      gschem_status_bar_set_left_button_text (status_bar, g_value_get_string (value));
      break;

    case PROP_MIDDLE_BUTTON_TEXT:
      gschem_status_bar_set_middle_button_text (status_bar, g_value_get_string (value));
      break;

    case PROP_RIGHT_BUTTON_TEXT:
      gschem_status_bar_set_right_button_text (status_bar, g_value_get_string (value));
      break;

    case PROP_SNAP_MODE:
      gschem_status_bar_set_snap_mode (status_bar, g_value_get_int (value));
      break;

    case PROP_SNAP_SIZE:
      gschem_status_bar_set_snap_size (status_bar, g_value_get_int (value));
      break;

    case PROP_STATUS_TEXT:
      gschem_status_bar_set_status_text (status_bar, g_value_get_string (value));
      break;

    case PROP_STATUS_TEXT_COLOR:
      gschem_status_bar_set_status_text_color (status_bar, g_value_get_int (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*!
 * \brief Write the grid settings to the gschem "status bar"
 * \par Function Description
 *  This function creates the string for the Grid/Snap label on
 *  the status bar utilizing the classes internal scratch ram,
 *  status_bar->grid_label_text and local char array for integers.
 *  The assemulated string is uploaded to the grid label status_bar.
 *
 * \param [in] status_bar This GschemStatusBar
 */
static void update_grid_label (GschemStatusBar *status_bar)
{
  if (status_bar->grid_label != NULL) {

    char scratch[6]; /* tmp char used to convert large integers */

    char *ptr = strcpy (status_bar->grid_label_text,_("Grid"));

    ptr = strcat(ptr, "(");

    switch (status_bar->snap_mode) {
      case SNAP_OFF:
        ptr = strcat(ptr,_("OFF"));
        break;

      case SNAP_GRID:
        strcat (ptr, geda_string_int2str(status_bar->snap_size, &scratch[0], 10));
        break;

      case SNAP_RESNAP:
        strcat (ptr, geda_string_int2str(status_bar->snap_size, &scratch[0], 10));
        strcat (ptr, "R");
        break;

      default:
        ptr = strcat(ptr,_("Error"));
        fprintf(stderr, "%s: snap_mode out of range: %d\n", __func__,
                status_bar->snap_mode);
    }

    ptr = strcat(ptr, ", ");

    if (status_bar->grid_mode == GRID_NONE) {
        ptr = strcat (ptr,_("OFF"));
    }
    else {
      if (status_bar->grid_size <= 0) {
        ptr = strcat (ptr,_("NONE"));
      }
      else {
        strcat (ptr, geda_string_int2str(status_bar->grid_size,&scratch[0], 10));
      }
    }

    ptr = strcat(ptr, ")");

    geda_label_widget_set_text(status_bar->grid_label,
                               status_bar->grid_label_text);
  }
}

GtkWidget *gschem_status_bar_new (void)
{
  return g_object_new (GSCHEM_TYPE_STATUS_BAR, NULL);
}

/** @} endgroup Gschem-Status-Bar */
