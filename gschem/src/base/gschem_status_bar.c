/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file gschem_status_bar.c
 *
 * \brief A widget for the "status bar" at the bottom of the window
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_MATH_H
#include <math.h>
#endif

#include "gschem.h"
#include "geda_widgets.h"

enum
{
  PROP_0,
  PROP_GRID_MODE,
  PROP_GRID_SIZE,
  PROP_HEIGHT,
  PROP_LEFT_BUTTON_TEXT,
  PROP_MIDDLE_BUTTON_TEXT,
  PROP_RIGHT_BUTTON_TEXT,
  PROP_SNAP_MODE,
  PROP_SNAP_SIZE,
  PROP_STATUS_TEXT
};

enum {
  SET_MIDDLE_ACTION,
#ifdef HAVE_LIBSTROKE
  SET_MIDDLE_STROKE,
#endif
  SET_MIDDLE_REPEAT,
  SET_MIDDLE_MOUSEPAN,
  UPDATE_GRID_LABEL,
  LAST_SIGNAL
};

struct _GschemStatusBarBuffers
{
  char status_left_text_buffer   [STATUS_LEFT_TEXT_BUFFER_SIZE];
  char status_middle_text_buffer [STATUS_MIDDLE_TEXT_BUFFER_SIZE];
  char status_right_text_buffer  [STATUS_RIGHT_LEFT_TEXT_BUFFER_SIZE];
  char status_grid_text_buffer   [STATUS_GRID_TEXT_BUFFER_SIZE];
  char status_label_text_buffer  [STATUS_STATUS_TEXT_BUFFER_SIZE];
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec);

static void
gschem_status_bar_class_init (GschemStatusBarClass *klass);

static void
gschem_status_bar_init (GschemStatusBar *view);

static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec);

static void
update_grid_label (GschemStatusBar *widget);

static GObjectClass *gschem_status_bar_parent_class = NULL;

struct st_popup_menu_entry {
  const char *text;
  const int   signal;
};

typedef struct st_popup_menu_entry StatusPopupEntry;

static StatusPopupEntry middle_popup_items[] = {

  { N_( RC_STR_MID_ACTION ),   SET_MIDDLE_ACTION },
#ifdef HAVE_LIBSTROKE
  { N_( RC_STR_MID_STROKE ),   SET_MIDDLE_STROKE },
#endif
  { N_( RC_STR_MID_REPEAT ),   SET_MIDDLE_REPEAT},
  { N_( RC_STR_MID_MOUSEPAN ), SET_MIDDLE_MOUSEPAN},
  {NULL} /* sentinel */
};

static void status_options_popup_clicked (GtkMenuItem *menuitem, void *user_data)
{
  GtkWidget *widget;
  unsigned   signal = (unsigned)(long*) user_data;

  widget = g_object_get_data (G_OBJECT(menuitem), "status-bar");

  g_signal_emit (widget, signals[signal], 0);

}

/*! \brief GschemStatusBar Show Middle Mouse Options Popup
 *
 *  \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the middle-button status widget when the right mouse button
 *  is released on the widget.
 */
static void middle_button_options_popup (GtkWidget      *event_box,
                                         GdkEventButton *event,
                                         void           *user_data)
{
  GtkWidget *menu;
  GtkWidget *popup_item;
  int i;

  /* create the context menu */
  menu = gtk_menu_new();

  for (i = 0; middle_popup_items[i].text != NULL; i++) {

    StatusPopupEntry entry = middle_popup_items[i];

    popup_item = gtk_menu_item_new_with_label (entry.text);

    g_signal_connect (GTK_OBJECT(popup_item), "activate",
                     (GCallback)status_options_popup_clicked,
                      GUINT_TO_POINTER(entry.signal));

    g_object_set_data (G_OBJECT(popup_item), "status-bar", user_data);

    gtk_menu_shell_append (GTK_MENU_SHELL (menu), popup_item);
  }

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
}

/*! \brief Middle Button Status Indicator Button Released callback
 *  \par Function Description
 *  Called when a mouse button is release with the cursor over
 *  the middle button status indicator. If the 3rd button was
 *  released, a small popup menu is displayed.
 *
 *  \sa middle_button_options_popup
 */
static bool middle_button_released (GtkWidget      *label,
                                    GdkEventButton *event,
                                    void           *user_data)
{
  bool ret_val;

  if (event->button == 3) {

    middle_button_options_popup(label, event, user_data);

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  /* lastly, chain up to the parent dispose */
  g_return_if_fail (gschem_status_bar_parent_class != NULL);
  gschem_status_bar_parent_class->dispose (object);
}

/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  GschemStatusBar *status_bar = GSCHEM_STATUS_BAR (object);
  GEDA_FREE(status_bar->buffers);

  /* lastly, chain up to the parent finalize */
  g_return_if_fail (gschem_status_bar_parent_class != NULL);
  gschem_status_bar_parent_class->finalize (object);
}

/*! \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec)
{
  GtkWidget  *status_bar = (GtkWidget*)GSCHEM_STATUS_BAR (object);
  const char *string;

  switch (param_id) {
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

/*! \brief Initialize GschemStatusBar class
 *
 *  \param [in] klass The class for the GschemStatusBar
 */
static void
gschem_status_bar_class_init (GschemStatusBarClass *klass)
{
  gschem_status_bar_parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));

  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_MODE,
                                   g_param_spec_int ("grid-mode",
                                                     "Grid Mode",
                                                     "Grid Mode",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     GRID_NONE,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_SIZE,
                                   g_param_spec_int ("grid-size",
                                                     "Grid Size",
                                                     "Grid Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     G_PARAM_READWRITE));
  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_HEIGHT,
                                   g_param_spec_int ("height",
                                                     "height",
                                                     "height",
                                                     0,
                                                     25,
                                                     2,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LEFT_BUTTON_TEXT,
                                   g_param_spec_string ("left-text",
                                                        "Left Button Text",
                                                        "Left Button Text",
                                                        "none",
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_MIDDLE_BUTTON_TEXT,
                                   g_param_spec_string ("middle-text",
                                                        "Middle Button Text",
                                                        "Middle Button Text",
                                                        "none",
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_RIGHT_BUTTON_TEXT,
                                   g_param_spec_string ("right-text",
                                                        "Right Button Text",
                                                        "Right Button Text",
                                                        "none",
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_MODE,
                                   g_param_spec_int ("snap-mode",
                                                     "Snap Mode",
                                                     "Snap Mode",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_SIZE,
                                   g_param_spec_int ("snap-size",
                                                     "Snap Size",
                                                     "Snap Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_STATUS_TEXT,
                                   g_param_spec_string ("status-text",
                                                        "Status Text",
                                                        "Status Text",
                                                        "none",
                                                        G_PARAM_READWRITE));

  signals[SET_MIDDLE_ACTION] = g_signal_new ("set-middle-action",
                                              gschem_status_bar_get_type(),
                                              G_SIGNAL_RUN_FIRST,
                                              G_STRUCT_OFFSET (GschemStatusBarClass,
                                                               middle_action),
                                              NULL, NULL,
                                              g_cclosure_marshal_VOID__VOID,
                                              G_TYPE_NONE, 0);

  signals[SET_MIDDLE_MOUSEPAN] = g_signal_new ("set-middle-pan",
                                               gschem_status_bar_get_type(),
                                               G_SIGNAL_RUN_LAST,
                                               G_STRUCT_OFFSET (GschemStatusBarClass,
                                                                middle_pan),
                                               NULL, NULL,
                                               g_cclosure_marshal_VOID__VOID,
                                               G_TYPE_NONE, 0);

  signals[SET_MIDDLE_REPEAT] = g_signal_new ("set-middle-repeat",
                                             gschem_status_bar_get_type(),
                                             G_SIGNAL_RUN_LAST,
                                             G_STRUCT_OFFSET (GschemStatusBarClass,
                                                              middle_repeat),
                                             NULL, NULL,
                                             g_cclosure_marshal_VOID__VOID,
                                             G_TYPE_NONE, 0);
#ifdef HAVE_LIBSTROKE

  signals[SET_MIDDLE_STROKE] = g_signal_new ("set-middle-stroke",
                                             gschem_status_bar_get_type(),
                                             G_SIGNAL_RUN_LAST,
                                             G_STRUCT_OFFSET (GschemStatusBarClass,
                                                               middle_stroke),
                                             NULL, NULL,
                                             g_cclosure_marshal_VOID__VOID,
                                             G_TYPE_NONE, 0);
#endif
}


/*! \brief Get the grid mode
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The grid mode
 */
int
gschem_status_bar_get_grid_mode (GtkWidget *widget)
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


/*! \brief Get the grid size
 *
 *  \param [in] widget This GschemStatusBar
 *  \return The grid size
 */
int
gschem_status_bar_get_grid_size (GtkWidget *widget)
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

/*! \brief Get the Height of the Status Bar
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The height
 */
int
gschem_status_bar_get_height (GtkWidget *widget)
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

/*! \brief Get the left button text
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The left button text
 */
const char*
gschem_status_bar_get_left_button_text (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_get_text ((GedaLabel*)gsb->left_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
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

/*! \brief Get the middle button text
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The middle button text
 */
const char*
gschem_status_bar_get_middle_button_text (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_get_text ((GedaLabel*)gsb->middle_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
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


/*! \brief Get the right button text
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The right button text
 */
const char*
gschem_status_bar_get_right_button_text( GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_get_text ((GedaLabel*)gsb->right_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
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


/*! \brief Get the snap mode
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The snap mode
 */
int
gschem_status_bar_get_snap_mode (GtkWidget *widget)
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


/*! \brief Get the snap size
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The snap size
 */
int
gschem_status_bar_get_snap_size (GtkWidget *widget)
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


/*! \brief Get the status text
 *
 *  \param [in] widget This GschemStatusBar
 *
 *  \return The status text
 */
const char*
gschem_status_bar_get_status_text (GtkWidget *widget)
{
#if defined (G_DISABLE_ASSERT)
  return geda_label_get_text ((GedaLabel*)gsb->status_label);
#else
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
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


/*! \brief Get/register GschemStatusBar type.
 */
unsigned int
gschem_status_bar_get_type ()
{
  static unsigned int type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemStatusBarClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_status_bar_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemStatusBar),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_status_bar_init,
    };

    type = g_type_register_static (GTK_TYPE_HBOX, "GschemStatusBar", &info, 0);
  }

  return type;
}

/* Macro for gschem_status_bar_setup_buffers. This macro is undefined immediately
 * after the function.
 */
#define TheTarget(target) (unsigned long int*)(dest + offsetof(GschemStatusBar, target))

/*! \brief Gschem Status Bar Setup Buffers
 *
 *  \par Function Description
 *  This function  dynamically allocates memory for a structure similar to
 *  the private structures used by GtkWidgets, except that we don't use the
 *  g_type_class_add_private() function. Currently, the structure contains
 *  only string buffers but other data could be added. The buffers are used
 *  to create compound strings for status-bar text rather than using strdup
 *  or derivitve to constantly allocate and de-allocate trival amounts of
 *  ram. In or to facilitates usage, the status-bar object provides puplic
 *  pointers to these buffers. The pointers are defined as char * const, so
 *  the content can be changed but the pointers can not. Since the pointers
 *  point to dynamicaly allocated ram, we must initially write to the read
 *  only pointers. This function does that.
 *
 *  \param [in] widget This GschemStatusBar
 */
static GschemStatusBarBuffers*
gschem_status_bar_setup_buffers (GschemStatusBar *widget)
{
  unsigned int nbytes = sizeof(GschemStatusBarBuffers);
  unsigned long int dest;

  GschemStatusBarBuffers *buffers = GEDA_MEM_ALLOC(nbytes);

  if (buffers) {

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
   *TheTarget(status_label_text) = (long int)&(buffers->status_label_text_buffer[0]);

  }
  return buffers;
}
#undef TheTarget

/*! \brief Initialize GschemStatusBar instance
 *
 *  \param [in] widget This GschemStatusBar
 */
static void
gschem_status_bar_init (GschemStatusBar *widget)
{
  GtkWidget  *middle_event;
  GtkWidget  *separator;

  const char *left_label_tip;
  const char *middle_label_tip;
  const char *right_label_tip;
  const char *grid_label_tip;
  const char *status_label_tip;
  const char *status_bar_tip;

  left_label_tip   = _("Left pointer button assignment");
  middle_label_tip = _("Middle pointer button assignment");
  right_label_tip  = _("Right pointer button assignment");
  grid_label_tip   = _("Indicates the current grid and snap units or states");
  status_label_tip = _("Indicates the current command state");
  status_bar_tip   = _("Gschem status bar");

  g_return_if_fail (widget != NULL);

  widget->buffers = gschem_status_bar_setup_buffers (widget);

  gtk_widget_push_composite_child ();

  widget->left_label = geda_visible_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->left_label), STATUS_XPAD, STATUS_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->left_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (GTK_WIDGET(widget->left_label), left_label_tip);

  separator = gtk_vseparator_new ();
  g_object_set (separator, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  middle_event = gtk_event_box_new();
  g_object_set (middle_event, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (widget), middle_event, FALSE, FALSE, 0);

  widget->middle_label = geda_visible_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->middle_label), STATUS_XPAD, STATUS_YPAD);
  gtk_container_add(GTK_CONTAINER(middle_event), widget->middle_label);
  gtk_widget_set_tooltip_text (GTK_WIDGET(widget->middle_label), middle_label_tip);

  separator = gtk_vseparator_new ();
  g_object_set (separator, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->right_label = geda_visible_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->right_label), STATUS_XPAD, STATUS_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->right_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (GTK_WIDGET(widget->right_label), right_label_tip);

  separator = gtk_vseparator_new ();
  g_object_set (separator, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->grid_label = geda_visible_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->grid_label), STATUS_XPAD, STATUS_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->grid_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (GTK_WIDGET(widget->grid_label), grid_label_tip);

  separator = gtk_vseparator_new ();
  g_object_set (separator, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->status_label = geda_visible_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->status_label), STATUS_XPAD, STATUS_YPAD);
  gtk_box_pack_end (GTK_BOX (widget), widget->status_label, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text (GTK_WIDGET(widget->status_label), status_label_tip);

  {
    AtkObject *obj;
    obj = gtk_widget_get_accessible(GTK_WIDGET(widget));
    atk_object_set_name (obj, _("Gschme Status Bar"));
    atk_object_set_description(obj,_(status_bar_tip));
  }

  gtk_widget_pop_composite_child ();

/*
  g_signal_connect (G_OBJECT (widget),
                    "notify::grid-mode",
                    G_CALLBACK (update_grid_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::grid-size",
                    G_CALLBACK (update_grid_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::snap-mode",
                    G_CALLBACK (update_grid_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::snap-size",
                    G_CALLBACK (update_grid_label),
                    NULL);
                    widget->middle_label
*/

  signals[UPDATE_GRID_LABEL] =
    g_signal_new_class_handler ("update-grid-label",
                              gschem_status_bar_get_type (),
                              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                              G_CALLBACK (update_grid_label),
                              NULL, NULL,
                              gtk_marshal_VOID__VOID,
                              G_TYPE_NONE, 0);

  g_signal_connect (middle_event, "button-release-event",
                    G_CALLBACK (middle_button_released),
                    widget);
}


/*! \brief Set the grid mode
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] mode   The grid mode
 */
void
gschem_status_bar_set_grid_mode (GtkWidget *widget, int mode)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->grid_mode = mode;
#else

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->grid_mode = mode;
      //g_object_notify (G_OBJECT (widget), "grid-mode");
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}


/*! \brief Set the grid size
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] size   The grid size
 */
void
gschem_status_bar_set_grid_size (GtkWidget *widget, int size)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->grid_size = size;
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->grid_size = size;
      //g_object_notify (G_OBJECT (widget), "grid-size");
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*! \brief Set the Height of the Status Bar
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] height The new height
 */
void
gschem_status_bar_set_height (GtkWidget *widget, int height)
{
  GschemStatusBar *status_bar;
#if defined (G_DISABLE_ASSERT)
  status_bar = GSCHEM_STATUS_BAR(widget);
  gtk_misc_set_padding (GTK_MISC (status_bar->left_label), STATUS_XPAD, height);
  gtk_misc_set_padding (GTK_MISC (status_bar->middle_label), STATUS_XPAD, height);
  gtk_misc_set_padding (GTK_MISC (status_bar->right_label), STATUS_XPAD, height);
  gtk_misc_set_padding (GTK_MISC (status_bar->grid_label), STATUS_XPAD, height);
 gtk_misc_set_padding (GTK_MISC (status_bar->status_label), STATUS_XPAD, height);
#else
fprintf(stderr, "gschem_status_bar_set_height: value=%d\n", height);
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      status_bar = (GschemStatusBar*)widget;
      gtk_misc_set_padding (GTK_MISC (status_bar->left_label), STATUS_XPAD, height);
      gtk_misc_set_padding (GTK_MISC (status_bar->middle_label), STATUS_XPAD, height);
      gtk_misc_set_padding (GTK_MISC (status_bar->right_label), STATUS_XPAD, height);
      gtk_misc_set_padding (GTK_MISC (status_bar->grid_label), STATUS_XPAD, height);
      gtk_misc_set_padding (GTK_MISC (status_bar->status_label), STATUS_XPAD, height);
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*! \brief Set the left button text
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] text   The text
 */
void
gschem_status_bar_set_left_button_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_set_text ((GedaLabel *)(GSCHEM_STATUS_BAR(widget))->right_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
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


/*! \brief Set the middle button text
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] text   The text
 */
void
gschem_status_bar_set_middle_button_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_set_text ((GedaLabel *)(GSCHEM_STATUS_BAR(widget))->right_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->middle_label)) {
        geda_label_widget_set_text (gsb->middle_label, text);
        //g_object_notify (G_OBJECT (widget), "middle-text");
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


/*! \brief Set the right button text
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] text   The text
 */
void
gschem_status_bar_set_right_button_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_set_text ((GedaLabel*)(GSCHEM_STATUS_BAR(widget))->right_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->right_label)) {
        geda_label_widget_set_text ( gsb->right_label, text);
        //g_object_notify (G_OBJECT (widget), "right-text");
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


/*! \brief Set the snap mode
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] mode   The snap mode
 */
void
gschem_status_bar_set_snap_mode (GtkWidget *widget, int mode)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->snap_mode = mode;
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->snap_mode = mode;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}


/*! \brief Set the snap size
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] size   The snap size
 */
void
gschem_status_bar_set_snap_size (GtkWidget *widget, int size)
{
#if defined (G_DISABLE_ASSERT)
  (GSCHEM_STATUS_BAR(widget))->snap_size = size;
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      ((GschemStatusBar*)widget)->snap_size = size;
    }
    else {
      BUG_MSG("widget is not a GschemStatusBar");
    }
  }
#endif
}

/*! \brief Set the status text
 *
 *  \param [in] widget This GschemStatusBar
 *  \param [in] text   The status text
 */
void
gschem_status_bar_set_status_text (GtkWidget *widget, const char *text)
{
#if defined (G_DISABLE_ASSERT)
  geda_label_set_text ((GedaLabel *)(GSCHEM_STATUS_BAR(widget))->status_label, text);
#else
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_STATUS_BAR(widget)) {
      GschemStatusBar *gsb = (GschemStatusBar*)widget;
      if (GEDA_IS_LABEL(gsb->status_label)) {
        geda_label_widget_set_text (gsb->status_label, text);
        //g_object_notify (G_OBJECT (widget), "status-text");
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


/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec)
{
  GtkWidget *status_bar = (GtkWidget*)GSCHEM_STATUS_BAR (object);

  switch (param_id) {
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

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \brief Write the grid settings to the gschem "status bar."
 *
 *  \par Function Description
 *  This function creates the string for the Grid/Snap label on
 *  the status bar utilizing the classes internal scratch ram,
 *  widget->grid_label_text and local char array for integers.
 *  The assemulated string is uploaded to the grid label widget.
 *
 *  \param [in] widget This GschemStatusBar
 */
static void
update_grid_label (GschemStatusBar *widget)
{

  if (widget->grid_label != NULL) {

    char scratch[6]; /* tmp char used to convert large integers */

    char *ptr = strcpy (widget->grid_label_text,_("Grid("));

    switch (widget->snap_mode) {
      case SNAP_OFF:
        ptr = strcat(ptr,_("OFF"));
        break;

      case SNAP_GRID:
        strcat (ptr, u_string_int2str(widget->snap_size,&scratch[0], 10));
        break;

      case SNAP_RESNAP:
        strcat (ptr, u_string_int2str(widget->snap_size,&scratch[0], 10));
        strcat (ptr, "R");
        break;

      default:
        ptr = strcat(ptr,_("Err"));
        fprintf(stderr, "%s: snap_mode out of range: %d\n", __func__,
                widget->snap_mode);
    }

    ptr = strcat(ptr, ", ");

    if (widget->grid_mode == GRID_NONE) {
        ptr = strcat(ptr,_("OFF"));
    }
    else {
      if (widget->grid_size <= 0) {
        ptr = strcat(ptr,_("NONE"));
      }
      else {
        strcat (ptr, u_string_int2str(widget->grid_size,&scratch[0], 10));
      }
    }

    ptr = strcat(ptr, ")");

    geda_label_widget_set_text(widget->grid_label, widget->grid_label_text);
  }
}

GtkWidget *gschem_status_bar_new (void)
{
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_STATUS_BAR, NULL));
}
