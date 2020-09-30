/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_status_bar.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file gschem_status_bar.h
 *
 * \brief A widget for the "status bar" at the bottom of the window
 */
/*! \class GschemStatusBar gschem_status_bar.h "gschem_status_bar.h"
 *  \brief A Status Bar widget
 *  \par
 *  A GschemStatusBar the widget at the bottom of the main window if
 *  enabled. The status widget provide various feedback to the user,
 *  such as current grid & snap setting and mouse button assignments.
 */

#define GSCHEM_TYPE_STATUS_BAR           (gschem_status_bar_get_type())
#define GSCHEM_STATUS_BAR(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_STATUS_BAR, GschemStatusBar))
#define GSCHEM_STATUS_BAR_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_STATUS_BAR, GschemStatusBarClass))
#define GSCHEM_IS_STATUS_BAR(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_STATUS_BAR))
#define GSCHEM_STATUS_BAR_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_STATUS_BAR, GschemStatusBarClass))

#define STATUS_COORD_TEXT_BUFFER_SIZE      24  /* X=1234567, Y=1234567 || (1234567,1234567) */
#define STATUS_GRID_TEXT_BUFFER_SIZE       24
#define STATUS_LEFT_TEXT_BUFFER_SIZE       24
#define STATUS_MIDDLE_TEXT_BUFFER_SIZE     36
#define STATUS_RIGHT_LEFT_TEXT_BUFFER_SIZE 24
#define STATUS_STATUS_TEXT_BUFFER_SIZE     96

#define COORD_FORMAT_VECTOR 3

typedef enum
{
  COORD_FORMAT_OFF     =   0,
  COORD_FORMAT_V180    =   1,
  COORD_FORMAT_V360    =   2,
  COORD_FORMAT_XY      =   4,
  COORD_FORMAT_COORD   =   8,
  COORD_FORMAT_COMMA   =  16,
  COORD_FORMAT_X       =  32,
  COORD_FORMAT_Y       =  64,
  COORD_FORMAT_XONLY   =  64,
  COORD_FORMAT_YONLY   = 256
} IDE_COORD_FORMATS;

typedef struct _GschemStatusBar GschemStatusBar;
typedef struct _GschemStatusBarClass GschemStatusBarClass;
typedef struct _GschemStatusBarBuffers GschemStatusBarBuffers;

struct _GschemStatusBarClass
{
  GtkHBoxClass parent_class;

  void     (* reformat_coordinates) (GschemStatusBar *widget);

    /* signals */
  void     (* middle_action)        (GschemStatusBar *widget);
  void     (* middle_pan)           (GschemStatusBar *widget);
  void     (* middle_pop)           (GschemStatusBar *widget);
  void     (* middle_repeat)        (GschemStatusBar *widget);
#ifdef HAVE_LIBSTROKE
  void     (* middle_stroke)        (GschemStatusBar *widget);
#endif
  void     (* third_popup)          (GschemStatusBar *widget);
  void     (* third_pan)            (GschemStatusBar *widget);
};

struct _GschemStatusBar
{
  GtkHBox parent;

  GschemStatusBarBuffers *buffers;

  char *  const coord_label_text;
  char *  const left_label_text;
  char *  const middle_label_text;
  char *  const right_label_text;
  char *  const grid_label_text;
  char *  const status_label_text;

  GtkWidget *coord_label;
  GtkWidget *grid_label;
  GtkWidget *left_label;
  GtkWidget *middle_label;
  GtkWidget *right_label;
  GtkWidget *status_label;

  GtkWidget *coord_popup;              /*!< Coordinates popup menu */
  GtkWidget *middle_popup;             /*!< Middle Button popup menu */
  GtkWidget *third_popup;              /*!< Third Button popup menu */

  int        coord_mode;
  int        grid_mode;
  int        grid_size;
  int        snap_mode;
  int        snap_size;

  /* Unfortunately, we have to keep a copy of the last coordinates used.
   * The values are only used when the user changes coordinates formats
   * using the pop-up menu, in which case the coordinates display needs
   * to be updated but we would not get new coordinates until the mouse
   * is moved, we could temporarily turn off but it looks better if the
   * new format instantly appears with the previous values.
   **/
  int        x0;
  int        y0;
  int        x1;
  int        y1;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType      gschem_status_bar_get_type               (void);
GtkWidget    *gschem_status_bar_new                    (void);

int           gschem_status_bar_get_grid_mode          (GtkWidget *widget);

int           gschem_status_bar_get_grid_size          (GtkWidget *widget);

int           gschem_status_bar_get_height             (GtkWidget *widget);

const char   *gschem_status_bar_get_left_button_text   (GtkWidget *widget);

const char   *gschem_status_bar_get_middle_button_text (GtkWidget *widget);

const char   *gschem_status_bar_get_right_button_text  (GtkWidget *widget);

int           gschem_status_bar_get_coord_mode         (GtkWidget *widget);

int           gschem_status_bar_get_snap_mode          (GtkWidget *widget);

int           gschem_status_bar_get_snap_size          (GtkWidget *widget);

const char   *gschem_status_bar_get_status_text        (GtkWidget *widget);

void          gschem_status_bar_set_coord_mode         (GtkWidget *widget, int mode);

void          gschem_status_bar_set_grid_mode          (GtkWidget *widget, int mode);

void          gschem_status_bar_set_grid_size          (GtkWidget *widget, int size);

void          gschem_status_bar_set_coordinates        (GtkWidget *widget, int x0, int y0, int x1, int y1);

void          gschem_status_bar_set_height             (GtkWidget *widget, int height);

void          gschem_status_bar_set_left_button_text   (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_middle_button_text (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_right_button_text  (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_snap_mode          (GtkWidget *widget, int mode);

void          gschem_status_bar_set_snap_size          (GtkWidget *widget, int size);

void          gschem_status_bar_set_status_text        (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_status_text_color  (GtkWidget *widget, int color);

#ifdef __cplusplus
}
#endif /* __cplusplus */
