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
 * \file gschem_status_bar.h
 *
 * \brief A widget for the "status bar" at the bottom of the window
 */

#define GSCHEM_TYPE_STATUS_BAR           (gschem_status_bar_get_type())
#define GSCHEM_STATUS_BAR(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_STATUS_BAR, GschemStatusBar))
#define GSCHEM_STATUS_BAR_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_STATUS_BAR, GschemStatusBarClass))
#define GSCHEM_IS_STATUS_BAR(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_STATUS_BAR))
#define GSCHEM_STATUS_BAR_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_STATUS_BAR, GschemStatusBarClass))

#define STATUS_GRID_TEXT_BUFFER_SIZE       24
#define STATUS_LEFT_TEXT_BUFFER_SIZE       24
#define STATUS_MIDDLE_TEXT_BUFFER_SIZE     36
#define STATUS_RIGHT_LEFT_TEXT_BUFFER_SIZE 24
#define STATUS_STATUS_TEXT_BUFFER_SIZE     96

typedef struct _GschemStatusBar GschemStatusBar;
typedef struct _GschemStatusBarClass GschemStatusBarClass;
typedef struct _GschemStatusBarBuffers GschemStatusBarBuffers;

struct _GschemStatusBarClass
{
  GtkHBoxClass parent_class;
};

struct _GschemStatusBar
{
  GtkHBox parent;

  GschemStatusBarBuffers *buffers;

  char *  const left_label_text;
  char *  const middle_label_text;
  char *  const right_label_text;
  char *  const grid_label_text;
  char *  const status_label_text;

  GtkWidget *grid_label;
  GtkWidget *left_label;
  GtkWidget *middle_label;
  GtkWidget *right_label;
  GtkWidget *status_label;

  int        grid_mode;
  int        grid_size;
  int        snap_mode;
  int        snap_size;

};

unsigned int  gschem_status_bar_get_type               (void);
GtkWidget    *gschem_status_bar_new                    (void);

int           gschem_status_bar_get_grid_mode          (GtkWidget *widget);

int           gschem_status_bar_get_grid_size          (GtkWidget *widget);

int           gschem_status_bar_get_height             (GtkWidget *widget);

const char*   gschem_status_bar_get_left_button_text   (GtkWidget *widget);

const char*   gschem_status_bar_get_middle_button_text (GtkWidget *widget);

const char*   gschem_status_bar_get_right_button_text  (GtkWidget *widget);

int           gschem_status_bar_get_snap_mode          (GtkWidget *widget);

int           gschem_status_bar_get_snap_size          (GtkWidget *widget);

const char*   gschem_status_bar_get_status_text        (GtkWidget *widget);

void          gschem_status_bar_set_grid_mode          (GtkWidget *widget, int mode);

void          gschem_status_bar_set_grid_size          (GtkWidget *widget, int size);

void          gschem_status_bar_set_height             (GtkWidget *widget, int height);

void          gschem_status_bar_set_left_button_text   (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_middle_button_text (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_right_button_text  (GtkWidget *widget, const char *text);

void          gschem_status_bar_set_snap_mode          (GtkWidget *widget, int mode);

void          gschem_status_bar_set_snap_size          (GtkWidget *widget, int size);

void          gschem_status_bar_set_status_text        (GtkWidget *widget, const char *text);