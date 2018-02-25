/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_entry.h
 * libgedauio - gEDA's library for User Interface Objects
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2018 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 * Date: December 31, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
#ifndef _GEDA_ENTRY_H
#define _GEDA_ENTRY_H

/*!
 * Use NO_HISTORY as an argument to disable GedeaEntry history feature.
 */
#define NO_HISTORY (void*) -1

/*!
 * Use NO_COMPLETION as an argument to disable GedeaEntry completion feature.
 */
#define NO_COMPLETION (void*) -1

#ifndef MAX_ENTRY_HISTORY /* Make sure this gets defined */
  #define MAX_ENTRY_HISTORY 22
#endif

#ifndef max_command_length
  #define max_command_length 256
#endif

#define DISABLE (GList **)-1

#include <gtk/gtk.h>

#include "geda_completion.h"

typedef struct
{
  char  *name;
  void (*func)(void*);
  void  *arg;
  char  *help;
} commands;

#define GEDA_TYPE_ENTRY            (geda_entry_get_type ())
#define GEDA_ENTRY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_ENTRY, GedaEntry))
#define GEDA_ENTRY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_ENTRY, GedaEntryClass))
#define GEDA_IS_ENTRY(obj)         (is_a_geda_entry((GedaEntry*)(obj)))
#define GEDA_IS_ENTRY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_ENTRY))
#define GEDA_ENTRY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_ENTRY, GedaEntryClass))

typedef struct _GedaEntry      GedaEntry;
typedef struct _GedaEntryClass GedaEntryClass;
typedef struct _GedaEntryPriv  GedaEntryPriv;

typedef enum {
  ACCEPT_ALL_ASCII,
  ACCEPT_ALPHANUMERIC,
  ACCEPT_COORDINATE,
  ACCEPT_NUMERIC,
  ACCEPT_NUMBER,
  ACCEPT_INTEGER,
  ACCEPT_REAL,
} GedaEntryAccept;

struct _GedaEntry
{
    GtkEntry          parent;

    GedaEntryAccept   validation_mode;
    volatile int      enable_drag_n_drop   : 1;
    volatile int      activates_default    : 1;
    volatile int      auto_complete        : 1;
    volatile int      completion_enabled   : 1;
    volatile int      have_history         : 1;
    volatile int      history_index;
    volatile int      text_case;
    unsigned int      max_history;
    PangoLayout      *cached_layout;
    GedaEntryPriv    *priv;
};

struct _GedaEntryClass
{
  GtkEntryClass   parent_class;

  /* Action signals */
  void (* activate)           (GedaEntry        *entry);

  /* Source side drag signals */
  void (* drag_begin)         (GtkWidget         *widget,
                               GdkDragContext    *context);
  void (* drag_end)           (GtkWidget         *widget,
                               GdkDragContext    *context);
  void (* drag_data_get)      (GtkWidget         *widget,
                               GdkDragContext    *context,
                               GtkSelectionData  *selection_data,
                               unsigned int       info,
                               unsigned int       time_);
  void (* drag_data_delete)   (GtkWidget         *widget,
                               GdkDragContext    *context);

  /* Target side drag signals */
  void (* drag_leave)         (GtkWidget         *widget,
                               GdkDragContext    *context,
                               unsigned int       time_);
  bool (* drag_motion)        (GtkWidget         *widget,
                               GdkDragContext    *context,
                               int                x,
                               int                y,
                               unsigned int       time_);
  bool (* drag_drop)          (GtkWidget         *widget,
                               GdkDragContext    *context,
                               int                x,
                               int                y,
                               unsigned int       time_);
  void (* drag_data_received) (GtkWidget         *widget,
                               GdkDragContext    *context,
                               int                x,
                               int                y,
                               GtkSelectionData  *selection_data,
                               unsigned int       info,
                               unsigned int       time_);
};

#ifdef __cplusplus
extern "C" {
#endif

     GedaType  geda_entry_get_type               (void)                     GEDA_CONST;
         bool  is_a_geda_entry                   (GedaEntry      *entry);

    GtkWidget *geda_entry_new                    (void)                     WARN_UNUSED;
    GtkWidget *geda_entry_new_history_complete   (GList         **history,
                                                  GList         **complete) WARN_UNUSED;
    GtkWidget *geda_entry_new_visible            (void)                     WARN_UNUSED;
    GtkWidget *geda_entry_new_visible_buffer     (GtkEntryBuffer *buffer);
    GtkWidget *geda_entry_new_visible_completion (GList         **complete) WARN_UNUSED;
    GtkWidget *geda_entry_new_visible_history    (GList         **history)  WARN_UNUSED;

    GtkWidget *geda_entry_new_with_buffer        (GtkEntryBuffer *buffer)   WARN_UNUSED;
    GtkWidget *geda_entry_new_with_completion    (GList         **complete) WARN_UNUSED;
    GtkWidget *geda_entry_new_with_history       (GList         **history)  WARN_UNUSED;

    GtkWidget *geda_entry_new_with_max_length    (unsigned int    length);

         bool  geda_entry_get_activates_default  (GedaEntry      *entry);
         void  geda_entry_set_activates_default  (GedaEntry      *entry,
                                                  bool            setting);
PangoAttrList *geda_entry_get_attributes         (GedaEntry      *entry);
         void  geda_entry_set_attributes         (GedaEntry      *entry,
                                                  PangoAttrList  *attrs);
GedaCompletion *geda_entry_get_completion        (GedaEntry      *entry);
         void  geda_entry_set_completion         (GedaEntry      *entry,
                                                  GedaCompletion *completion);

         bool  geda_entry_completion_get_case    (GedaEntry      *entry);
         void  geda_entry_completion_set_case    (GedaEntry      *entry,
                                                  bool            sensitive);

         bool  geda_entry_get_input_case         (GedaEntry      *entry);
         void  geda_entry_set_input_case         (GedaEntry      *entry,
                                                  int             mode);

 unsigned int  geda_entry_get_length_history     (GedaEntry      *entry);
 unsigned int  geda_entry_get_max_history        (GedaEntry      *entry);
         void  geda_entry_set_max_history        (GedaEntry      *entry,
                                                  unsigned int    value);

         void  geda_entry_set_max_length         (GedaEntry      *entry,
                                                  unsigned int    max);
 unsigned int  geda_entry_get_max_length         (GedaEntry      *entry);

   const char *geda_entry_get_text               (GedaEntry      *entry);
         void  geda_entry_set_text               (GedaEntry      *entry,
                                                  const char     *new_text);
          int  geda_entry_get_text_length        (GedaEntry      *entry);

GedaEntryAccept geda_entry_get_valid_input       (GedaEntry      *entry);
         void  geda_entry_set_valid_input        (GedaEntry      *entry,
                                                  GedaEntryAccept mode);

         void  geda_entry_select_all             (GedaEntry      *entry);
         void  geda_entry_select_region          (GedaEntry      *entry,
                                                  int             start,
                                                  int             end);

         void  geda_entry_modify_color           (GedaEntry      *entry,
                                                  GtkRcFlags      component,
                                                  GtkStateType    state,
                                                  const GdkColor *color);
         void  geda_entry_modify_bg              (GedaEntry      *entry,
                                                  GtkStateType    state,
                                                  const GdkColor *color);
         void  geda_entry_modify_fg              (GedaEntry      *entry,
                                                  GtkStateType    state,
                                                  const GdkColor *color);

  bool geda_entry_widget_get_activates_default   (GtkWidget      *entry);
  void geda_entry_widget_set_activates_default   (GtkWidget      *entry,
                                                  bool            setting);

PangoAttrList *geda_entry_widget_get_attributes  (GtkWidget      *entry);
         void  geda_entry_widget_set_attributes  (GtkWidget      *entry,
                                                  PangoAttrList  *attrs);
GedaCompletion*
               geda_entry_widget_get_completion  (GtkWidget      *entry);
         void  geda_entry_widget_set_completion  (GtkWidget      *entry,
                                                  GedaCompletion *completion);

  bool geda_entry_widget_completion_get_case     (GtkWidget      *entry);
  void geda_entry_widget_completion_set_case     (GtkWidget      *entry,
                                                  bool            sensitive);

         bool  geda_entry_widget_get_input_case  (GtkWidget      *entry);
         void  geda_entry_widget_set_input_case  (GtkWidget      *entry,
                                                  int             mode);

 unsigned int  geda_entry_widget_get_max_history (GtkWidget      *entry);
         void  geda_entry_widget_set_max_history (GtkWidget      *entry,
                                                  unsigned int    value);

         void  geda_entry_widget_set_max_length  (GtkWidget      *entry,
                                                  unsigned int    max);
 unsigned int  geda_entry_widget_get_max_length  (GtkWidget      *entry);

const char    *geda_entry_widget_get_text        (GtkWidget      *entry);
         void  geda_entry_widget_set_text        (GtkWidget      *entry,
                                                  const char     *new_text);
GedaEntryAccept
               geda_entry_widget_get_valid_input (GtkWidget      *entry);
         void  geda_entry_widget_set_valid_input (GtkWidget      *entry,
                                                  GedaEntryAccept mode);
         void  geda_entry_widget_modify_bg       (GtkWidget      *entry,
                                                  GtkStateType    state,
                                                  const GdkColor *color);
         void  geda_entry_widget_modify_fg       (GtkWidget      *entry,
                                                  GtkStateType    state,
                                                  const GdkColor *color);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_ENTRY_H__ */
