/* -*- C x_dnd.c  indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_dnd.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2017 Wiley Edward Hill
 * Copyright (C) 2013-2017 gEDA Contributors (see ChangeLog for details)
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
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: September, 27, 2013
 */
/*!
 * \file x_dnd.c
 * \brief Drag and Drop Module
 * \par
 *  There are several different ways we could implement Drag & Drop.
 * Using the "basic" Gtk D&D works well but interferes with both implicit
 * drag moving and drawing selection boxes. The basic D&D could easily
 * replace our implicit drag moving but the drag threshold would have
 * be zero. And we would have to conjure up a pixel buffer for the drag
 * motion, less we be stuck with an icon, and that sounds slow. Our
 * implicit moving also keeps connected objected "connected" while
 * dragging, D&D would not.
 *
 * This implementation uses a different strategy, one that allows us to
 * utilize our current functionality. We manually initiate gtk_drag_begin
 * based on the leave_notify_event signal and our state variables. This
 * seems to solve most of our problems. The new coordinates dialog is
 * also a drop site, and not just for us but also for other instances.
 * We manage this by using a set of DND state flags, see x_coord.c.
 *
 * Note GTK is filtering X's signal and once D&D is initiate, many of
 * the old signal handlers are blocked until the button is released,
 * and this seems mostly correct. One functionality GTK/GDK lacks is a
 * practical means to abort D&D. The depreciated functions to do this
 * don't seem to abort anything, and maybe that is why they have been
 * depreciated, there are no replacements functions. The best tactic
 * seems to be to let GTK/GDK finish normally, and ignore the data.
 *
 */

#include "../../include/gschem.h"
#include "../../include/x_window.h"
#include "../../include/x_dnd.h"

#include <geda_debug.h>

/****************************** Drag-N-Drop **********************************/

/** \defgroup Drag-N-Drop Gschem Drag and Drop Module
 *  @{
 *  \brief This group contains functions and data to support Drag-N-Drop events
 *  \par
 *   This module contains functions and data to support Drag-N-Drop events
 *   Other modules supporting Drag&Drop may use portions of this module
 *
 * \sa Coordinates-Drag-N-Drop-Destination
 */

typedef const char* (* GschemDndSendFunc) (GschemToplevel   *w_current,
                                           GdkDragContext   *context,
                                           GtkSelectionData *selection);

typedef bool   (* GschemDndRecvFunc) (GschemToplevel  *w_current,
                                      int              x,
                                      int              y,
                                      const char      *buffer,
                                      int              where);

typedef const char *(* x_dnd_str_data_drag_func) (GschemToplevel *w_current, GedaObject *object);

static GtkTargetEntry dnd_send_list[] = {
    GSCHEM_TARGET_NONE,
    GSCHEM_TARGET_TEXT,
    GSCHEM_TARGET_STRING,
    GSCHEM_TARGET_TEXT_PLAIN,
    GSCHEM_TARGET_UTF8_STRING,
    GSCHEM_TARGET_OBJECTS,
};

static GtkTargetEntry dnd_target_list[] = {
    GSCHEM_TARGET_NONE,
    GSCHEM_TARGET_TEXT,
    GSCHEM_TARGET_STRING,
    GSCHEM_TARGET_TEXT_PLAIN,
    GSCHEM_TARGET_URI_LIST,
    GSCHEM_TARGET_UTF8_STRING,
    GSCHEM_TARGET_OBJECTS,
};

static unsigned int dnd_ntargets = G_N_ELEMENTS (dnd_target_list);

typedef struct _GschemDndDataDef GschemDndDataDef;

struct _GschemDndDataDef
{
  GtkTargetEntry     target_entry;

  const char        *icon_name;

  GschemDndSendFunc  send_data_func;
  GschemDndRecvFunc  receive_data_func;
};

static bool x_dnd_source_leave    (GtkWidget        *widget,
                                   GdkEventCrossing *event,
                                   GschemToplevel   *w_current);
static
const char *x_dnd_send_string     (GschemToplevel   *w_current,
                                   GdkDragContext   *context,
                                   GtkSelectionData *selection);
static
const char *x_dnd_send_objects    (GschemToplevel   *w_current,
                                   GdkDragContext   *context,
                                   GtkSelectionData *selection);
static
bool        x_dnd_receive_objects (GschemToplevel   *w_current, int x, int y,
                                   const char       *buffer,    int who);

static const GschemDndDataDef dnd_data_defs[] =
{
  {
    GSCHEM_TARGET_NONE,
    NULL,
    NULL,
    NULL
  },
  {
    GSCHEM_TARGET_TEXT,
    "geda-component",
    x_dnd_send_string,
    x_dnd_receive_string
  },
  {
    GSCHEM_TARGET_STRING,
    "geda-component",
    x_dnd_send_string,
    x_dnd_receive_string
  },
  {
    GSCHEM_TARGET_TEXT_PLAIN,
    "geda-component",
    x_dnd_send_string,
    x_dnd_receive_string
  },
  {
    GSCHEM_TARGET_URI_LIST,
    "geda-component",
    x_dnd_send_string,
    x_dnd_receive_string
  },
  {
    GSCHEM_TARGET_UTF8_STRING,
    "geda-component",
    x_dnd_send_string,
    x_dnd_receive_string
  },
  {
    GSCHEM_TARGET_OBJECTS,
    "geda-component",
    x_dnd_send_objects,
    x_dnd_receive_objects
  }
};

/* Begin Data type Specific Sub-Handlers */

static const char*
x_dnd_send_string_nil (GschemToplevel *w_current, GedaObject *object)
{
  return (DND_NIL);
}

/*!
 * \brief Drag & Drop Send String Object Attributes
 * \par Function Description
 *  Helper for x_dnd_send_string_object. Retrieves all of the attributes
 *  belonging to object and combine into one string, skip the symversion
 */
static char *x_dnd_get_object_attributes (GedaObject *object)
{
  const GList *attributes;
  char *string;

  attributes = geda_object_get_attached (object);

  string = geda_strdup("");

  while (attributes) {

    const char *text;

    text = geda_text_object_get_string(attributes->data);

    if (text) {

      if (strncmp(text, "symversion", 10)) {
        string = geda_strconcat (string, " ", text, NULL);
      }
    }

    NEXT(attributes);
  }
  return string;
}

/*!
 * \brief Drag & Drop Handle Send Object
 * \par Function Description
 *  Handles sending an Object as a string.
 */
static const char*
x_dnd_send_string_object (GschemToplevel *w_current, GedaObject *object)
{
  const char *string_1;
  const char *string_3;
        char *string_2;

  switch (object->type) {
    case OBJ_COMPLEX:
      string_1 = geda_complex_object_get_filename(object);
      string_2 = x_dnd_get_object_attributes(object);
      break;

    case OBJ_PICTURE:
      string_1 = DND_FILE_LEADER;
      string_2 = geda_strdup(geda_picture_object_get_filename(object));
      break;

    default:
      string_1 = _("Error:");
      string_2 = geda_strdup(_("unidentified complex object"));
  }

  string_3 = geda_strconcat (string_1, string_2, NULL);

  GEDA_FREE(string_2);

  return string_3;
}

/******************* Shape Catagory Data Helpers *******************/
static char dnd_string_data_name[10];

static const char *x_dnd_string_data_name(char *name)
{
  int  index;

  for (index = 0; index < sizeof(dnd_string_data_name); index++) {
    if (*name == '.')
      break;
    dnd_string_data_name[index] = *name++;
  }

  if (index < sizeof(dnd_string_data_name))
    dnd_string_data_name[index] = '\0'; /* replace period with NULL*/

  return &dnd_string_data_name[0];
}

static char *x_dnd_string_data_arc_properties(GedaArc *arc)
{
  return
  geda_sprintf("center=(%d,%d), radius=%d, angle=%d",
             arc->x, arc->y, arc->radius, arc->start_angle);
}

static char *x_dnd_string_data_circle_properties(GedaCircle *circle)
{
  return
  geda_sprintf("center=(%d,%d), radius=%d", circle->center_x,
                   circle->center_y, circle->radius);
}

static char *x_dnd_string_data_line_properties(GedaLine *line)
{
  return
  geda_sprintf("start (%d,%d), end (%d,%d)",
                  line->x[0], line->y[0], line->x[1], line->y[1]);
}

static char *x_dnd_string_data_path_properties(GedaPath *path)
{
  return
  geda_sprintf("sections=%d",path->num_sections);
}

static char *x_dnd_string_data_box_properties(GedaBox *box)
{
  return
  geda_sprintf("upper point (%d,%d), lower point (%d,%d)",
                  box->upper_x, box->upper_y, box->lower_x, box->lower_y);
}

static const char *x_dnd_send_string_shape (GschemToplevel *w_current, GedaObject *object)
{
  const char *string;
  const char *name;
        char *common;   /* common to all shapes */
        char *properties;

  name   = x_dnd_string_data_name(object->name);

  common = geda_sprintf("line width=%d, dash=%d",
                           object->line_options->line_width,
                           object->line_options->line_space);

  switch(object->type) {
      case OBJ_ARC:
        properties = x_dnd_string_data_arc_properties(object->arc);
        break;

      case OBJ_CIRCLE:
        properties = x_dnd_string_data_circle_properties(object->circle);
        break;

      case OBJ_LINE:
        properties = x_dnd_string_data_line_properties(object->line);
        break;

      case OBJ_PATH:
        properties = x_dnd_string_data_path_properties(object->path);
        break;

      case OBJ_BOX:
        properties = x_dnd_string_data_box_properties(object->box);
        break;

      default:
        properties = geda_sprintf("%s", " object data error");
  }

  string = geda_sprintf ("%s, sequence id=%d, %s, %s, color=%d",
                             name, object->sid, common, properties,
                             object->color);

  GEDA_FREE(common);
  GEDA_FREE(properties);

  return string;
}

static const char*
x_dnd_send_string_signal (GschemToplevel *w_current, GedaObject *object)
{
  const char *string;
  const char *name;
        char *netname;
        char *properties;

  name    = x_dnd_string_data_name(object->name);
  netname = geda_attrib_search_object_by_name(object, "netname", 0);

  if (netname == NULL)
      netname = geda_sprintf("%s","NONE,");

  properties = x_dnd_string_data_line_properties (object->line);

  string = geda_sprintf ("%s, sequence id=%d, netname=%s color=%d, %s",
                             name, object->sid, netname,
                             object->color, properties);

  GEDA_FREE(netname);
  GEDA_FREE(properties);

  return string;
}

static const char*
x_dnd_send_string_text (GschemToplevel *w_current, GedaObject *object)
{
  const char *string;
  const char *name;
  name   = x_dnd_string_data_name(object->name);
  string = geda_sprintf ("%s:%s, sequence id=%d, color=%d", name,
                             object->text->string, object->sid,
                             object->color);
  return string;
}

/******************* Begin Handlers by Data Catagory ******************/

/*! \brief Route Request for String Data to type Specific Sub-Handlers */
static const char*
x_dnd_send_string (GschemToplevel *w_current, GdkDragContext   *context,
                   GtkSelectionData *selection)
{
  GedaObject *object;
  x_dnd_str_data_drag_func dnd_str_data_func;

  dnd_str_data_func = x_dnd_send_string_nil;
  object = o_select_return_first_object(w_current);

  if (object) {
    switch(object->type) {

/* Has file association */
      case OBJ_COMPLEX:
      case OBJ_PICTURE:
        dnd_str_data_func = x_dnd_send_string_object;
        break;

/* Shapes Objects */
      case OBJ_ARC:
      case OBJ_CIRCLE:
      case OBJ_LINE:
      case OBJ_PATH:
      case OBJ_BOX:
        dnd_str_data_func = x_dnd_send_string_shape;
        break;

/* Signal connections */
      case OBJ_NET:
      case OBJ_BUS:
      case OBJ_PIN:
        dnd_str_data_func = x_dnd_send_string_signal;
        break;

/* Text and Attributes String*/
      case OBJ_TEXT:
        dnd_str_data_func = x_dnd_send_string_text;
    }
  }

  return (*dnd_str_data_func)(w_current, object);

}

static
const char *x_dnd_send_objects (GschemToplevel   *w_current,
                                GdkDragContext   *context,
                                GtkSelectionData *selection)
{
    return DND_NIL;
}

/*!
 * \brief When Received String filename with SYM suffix
 * \par Function Description
 *  Called by x_dnd_receive_string after determining the received
 *  string ends with ".sym".
 *
 * \remarks If the symbols are not in the library search path then symbols
 *          will be loaded as a new page and that maybe what the user wants.
 */
static bool
x_dnd_receive_string_sym (GschemToplevel *w_current, int x, int y,
                          const char *filename, int where)
{
  bool  result;
  Page *page;
  char *path;

  page = gschem_toplevel_get_current_page(w_current);
  path = geda_get_dirname(filename);

  if (page && geda_struct_clib_source_path_exist(path)) {

    const CLibSymbol *symbol;
    const char       *symbolfile;

    symbolfile = geda_file_get_basename (filename);
    symbol     = geda_struct_clib_get_symbol_by_name(symbolfile);

    result = FALSE;
    if (symbol) {

      if (where == DROPPED_ON_COORD) {

        GedaObject *object;

        w_current->second_wx = x;
        w_current->second_wy = y;

        object = geda_complex_object_new (w_current->toplevel, x, y, 0,
                                          FALSE, symbol, symbolfile, TRUE);

        geda_struct_page_append_object(page, object);
        o_place_end(w_current, 0, 0, PASTE_OBJECTS_HOOK);
        o_undo_savestate (w_current, UNDO_ALL);
      }
      else if (where == DROPPED_ON_PAGESEL && !x) {
        result = TRUE;
      }
      else {
        /* If the current page is not a sym, then insert symbol*/
        if (!geda_struct_page_is_symbol_file(page)) {
          gtk_window_present(w_current->main_window);
          o_redraw_cleanstates    (w_current);
          o_complex_start (w_current, symbol, COPYMODE);
          i_status_show_msg(w_current, "Place object");
        }
        else {
          result = TRUE;
        }
      }
    }
    else {
      /* TODO: Should embed the symbol */
      const char *log_msg1 = _("Could not locate symbol");
      const char *log_msg2 = _("in library, try refreshing");
      u_log_message("%s \"%s\", %s\n", log_msg1, symbolfile, log_msg2);
    }
  }
  else { /* symbol file is not our library source path so load new page */
    const char *log_msg = _("not in library, opening as page");
    v_log_message("%s \"%s\" %s\n", _("symbol"), filename, log_msg);
    result = TRUE;
  }
  GEDA_FREE(path);
  return result;
}

/*! \brief Process String Data Received from a Drag & Drop Source */
bool
x_dnd_receive_string(GschemToplevel *w_current, int x, int y, const char *string, int where)
{
  bool result;

  result = FALSE;

  if (string) {

    GList *files;
    Page  *page;

    bool  load_as_page;   /* Could be either .sch or .sym */

    const char *leader   = DND_FILE_LEADER;
    char       *filename;
    char       *buffer;

    int   tail;
    int   index, count;
    int   len;

    load_as_page   = FALSE;
    result         = TRUE;
    files          = NULL;
    len            = strlen(string);
    buffer         = geda_utility_string_strdup (string);

    /* Replace line feeds and carriage returns with nulls */
    for(tail = 0; tail < len; tail++) {
      if (buffer[tail] == '\n')
        buffer[tail] = '\0';
      if (buffer[tail] == '\r')
        buffer[tail] = '\0';
    }

    /* Index through full string and put filename in list */
    for (index = 0; index < len; index++) {

      int head, prefix;

      prefix = strlen(leader) + index;

      /* Skip passed URL crap, i.e. "file://" */
      for (head = 0; head < prefix; head++) {
        if (buffer[head + index] != leader[head]) {break;}
      }

      index = head + index;

      if (index < len) {
        if (buffer + index) {
          filename = geda_utility_string_strdup (buffer + index);  /* copy file spec */
          if (filename) {
            files = g_list_append(files, filename);
          }
        }
      }

      /* Advance to the next null */
      while (buffer[index]) index++;

      /* And the next null, for LF and CR pairs */
      if (len - index > 1) {
        if (buffer[index] == '\0') {
          index++;
        }
      }
    }

    count = g_list_length(files);

    if (count > 1) {
      for(index = 0; index < count; index++) {
        filename = g_list_nth_data(files, index);
        page = x_window_open_page(w_current, filename);
        GEDA_FREE(filename);
      }
    }
    else {

      filename = files->data;  /* just one file in list */

      /* now that we have stripped the bad stuff, glib will work for us again */
      if ( g_str_has_suffix(filename, SCHEMATIC_FILE_DOT_SUFFIX)) {
        load_as_page = TRUE;
      }
      else if ( g_str_has_suffix(filename, SYMBOL_FILE_DOT_SUFFIX)) {
        load_as_page = x_dnd_receive_string_sym(w_current, x, y, filename, where);
      }

      if (load_as_page) {

        page = x_window_open_page(w_current, filename);

        if (page) {
          x_window_set_current_page (w_current, page);
        }
        else {
          result = FALSE;
        }
      }
      GEDA_FREE(filename);
    }
    g_list_free(files);
  }
  else {
    fprintf(stderr, "<%s>: Buffer is NULL\n", __func__);
  }

  return result;
}

/*! \brief Process Object Data Received from Drag & Drop Source */
static bool
x_dnd_receive_objects(GschemToplevel  *w_current, int x, int y, const char *buffer, int who)
{
  bool result;

  if (buffer) {

    GError *err = NULL;

    /* Make sure the buffer is empty, if str maybe in deep dodo */
    if (object_buffer[DND_BUFFER] != NULL) {
      geda_struct_object_release_objects(object_buffer[DND_BUFFER]);
      object_buffer[DND_BUFFER] = NULL;
    }

    /* Copy the objects to the Drag&Drop buffer */
    object_buffer[DND_BUFFER] = geda_object_read_buffer (w_current->toplevel, NULL,
                                                         buffer, -1, "Drag & Drop", &err);
    if (err) {

      char *errmsg;

      errmsg = geda_sprintf ("%s: %s.", _("An error occurred while receiving Drag & Drop data"), err->message);

      titled_pango_error_dialog (_("<b>Invalid Data.</b>"), errmsg, _("Drag & Drop failed"));

      GEDA_FREE(errmsg);
      g_error_free(err);
      result = FALSE;
    }
    else {
      gtk_window_present(w_current->main_window);
      result = TRUE;
    }
  }
  else {
    result = FALSE;
  }

  return result;
}

/** \defgroup Drag-N-Drop-Destination Signal Handlers
 *  @{ \par This sub-group contains routines to handle signals receivable by
 *          the Drawing Area as the destination
 */

/*!
 * \brief When Drag Received from the Source
 * \par Function Description
 *  Called when the data has been received from the source. Check GtkSelectionData
 *  sent by the source, and handle the target. Finish by calling gtk_drag_finish,
 *  which will emit the "data-delete" signal if told to.
 */
static void
x_dnd_drag_receive(GtkWidget *widget, GdkDragContext   *context, int x, int y,
                                      GtkSelectionData *selection_data,
                                      unsigned int      target_type,
                                      unsigned int      time,
                                      GschemToplevel   *w_current)
{
  bool  dnd_success = FALSE;
  bool  delete_data = FALSE;

#if DEBUG || DEBUG_DND_EVENTS

  const char *name = gtk_widget_get_name (widget);
  printf ("<%s> %s:", __func__, name);

#endif

  /* Deal with what we are given from source */
  if ((selection_data != NULL) &&
    (gtk_selection_data_get_length(selection_data) >= 0))
  {

    const GschemDndDataDef  *datadef;
    const unsigned char     *buffer;
    const char              *string;

/*
    Seems context->suggested_action is useless, is always
    GDK_ACTION_MOVE regardless of source or buttons.

    switch (context->suggested_action) {
      case GDK_ACTION_DEFAULT:
      case GDK_ACTION_COPY:
      case GDK_ACTION_MOVE:
      case GDK_ACTION_LINK:
      case GDK_ACTION_PRIVATE:
      case GDK_ACTION_ASK:
        delete_data = TRUE;
      default:
        break;
    }
*/
    delete_data = TRUE;

    /* Get pointer to the data */
#if GTK_CHECK_VERSION(2,14,0)
    buffer = gtk_selection_data_get_data (selection_data);
#else
    buffer = selection_data->data;
#endif
    string = (const char*)buffer;

    /* Check that we got the format we can use */
    switch (target_type) {

      case DND_TARGET_TEXT:
      case DND_TARGET_STRING:
      case DND_TARGET_PLAIN_TEXT:
      case DND_TARGET_URI_LIST:
      case DND_TARGET_UTF8_STRING:
        datadef = &dnd_data_defs[DND_TARGET_UTF8_STRING];
        dnd_success = (datadef->receive_data_func)(w_current, x, y, string, DROPPED_ON_CANVAS);

#if DEBUG || DEBUG_DND_EVENTS
        printf (" string handler returned %d\n", dnd_success);
#endif
        break;

      case DND_TARGET_OBJECTS:
        datadef = &dnd_data_defs[DND_TARGET_OBJECTS];
        dnd_success = (datadef->receive_data_func)(w_current, x, y, string, DROPPED_ON_CANVAS);

#if DEBUG || DEBUG_DND_EVENTS
        printf (" object handler returned %d\n", dnd_success);
#endif
        if (dnd_success) {
          o_redraw_cleanstates (w_current);
          w_current->buffer_number = DND_BUFFER;
          dnd_success = o_buffer_paste_start (w_current, x, y);
        }
        break;

      default:
        printf ("<%s> nothing good.\n", __func__);
    }
  }

  gtk_drag_finish (context, dnd_success, delete_data, time);
}

#if DEBUG || DEBUG_DND_EVENTS

/*!
 * \brief Debug When Drag Leaves the Destination
 * \par Function Description
 *  Called when the drag leaves the destination.
 */
static void
x_dnd_drag_leave (GtkWidget *widget, GdkDragContext *context, unsigned int time, GschemToplevel *w_current)
{
  const char *name = gtk_widget_get_name (widget);
  printf ("\n<%s> %s: leaving\n", __func__, name);
}

#endif

/*!
 * \brief When Drag Motion over the Destination
 * \par Function Description
 *  Called when a drag is over the destination.
 *
 * \return FALSE if the operation should continue
 * \todo Set icon
 */
/* Emitted /
static bool x_dnd_drag_motion
(GtkWidget *widget, GdkDragContext *context, int x, int y, unsigned int t, GschemToplevel *w_current)
{
  // Fancy stuff here. This signal spams the console something horrible.
  //const char *name = gtk_widget_get_name (widget);
  return  FALSE;
}
*/
/*
static void
drag_motion (GtkWidget *widget, GdkDragContext *context, int x, int y, unsigned int time)
{
  GdkModifierType mask;
  gdk_window_get_pointer (gtk_widget_get_window (widget), NULL, NULL, &mask);
  if (mask & GDK_CONTROL_MASK)
    gdk_drag_status (context, GDK_ACTION_COPY, time);
  else
    gdk_drag_status (context, GDK_ACTION_MOVE, time);

}
*/

/*!
 * \brief When Drag gets Dropped on the Drawing Area
 * \par Function Description
 *  Called when the user releases (drops) the selection. We choose the
 *  target type we wish the source could send. We call gtk_drag_get_data
 *  which will emit "drag-data-get" on the source, passing along our wish.
 *
 * \return TRUE if the operation should continue, otherwise FALSE
 */
static bool
x_dnd_drag_drop (GtkWidget *widget, GdkDragContext *context, int x, int y, unsigned int time, GschemToplevel *w_current)
{
  bool   is_valid_drop_site;
  GList *targets;

#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  printf ("\n<%s> %s: entry\n", __func__, name);
#endif

  /* Check to see if (x,y) is a valid drop site within widget */
  is_valid_drop_site = TRUE;

  /* Get a list of target types to choose from */
  targets = context->targets;

  /* If the source offers a target */
  if (targets) {

    GtkTargetEntry *target_entry;
    GdkAtom         target_type;

    int index = dnd_ntargets - 1;

    //targets = gdk_drag_context_list_targets(context);

    /* Set what we really want! */
    target_type = GDK_POINTER_TO_ATOM (&dnd_target_list[DND_TARGET_OBJECTS]);

    /* For each of our targets, look backwards to see if we find a match */
    for (target_entry = &dnd_target_list[index]; index > -1 ; index--) {

      GList *iter;

      for (iter = targets; iter != NULL; iter = g_list_next (iter)) {
        target_type = GDK_POINTER_TO_ATOM(iter->data);
        if (!strcasecmp (target_entry->target, gdk_atom_name(target_type))) {
          index = -1;
          break;
        }
      }
    }

    /* Request the data from the source. */
    gtk_drag_get_data ( widget,          /* will receive 'drag-data-received' signal */
                        context,         /* represents the current state of the DnD */
                        target_type,     /* the target type we want */
                        time );          /* time stamp */

#if DEBUG || DEBUG_DND_EVENTS
    printf ("<%s> Site is Valid target\n", __func__);
#endif
  }

  /* No target offered by source => error */
  else {
    is_valid_drop_site = FALSE;

#if DEBUG || DEBUG_DND_EVENTS
    printf ("<%s> Target site is Invalid\n", __func__);
#endif

  }
  return  is_valid_drop_site;
}

/** @} end-subgroup Drag-N-Drop-Destination  */

/******************************************************************************/

/** \defgroup Drag-N-Drop-Source Drag N Drop Source (Originator) Functions
 *  @{ \par
 *  This sub-group contains routines to handle signals receivable by the
 *  Source.
*/

/*!
 * \brief When Destination Request Data from the Source
 * \par Function Description
 *  Called when the destination requests data from the source via
 *  gtk_drag_get_data. We attempt to provide data in the form requested in
 *  the target_type passed to us from the destination. If we cannot, then
 *  default to a "safe" type, i.e. a string or text, even if only to print
 *  an error. Then use gtk_selection_data_set to put the source data into
 *  the allocated selection_data object, which will then be passed to the
 *  destination. This will cause "drag-data-received" to be emitted on the
 *  destination. GdkSelectionData is based on X's selection mechanism which,
 *  via X properties, is only capable of storing data in blocks of 8, 16, or
 *  32 bit units.
 */
static void
x_dnd_drag_data_get (GtkWidget *widget, GdkDragContext   *context,
                                        GtkSelectionData *selection_data,
                                        unsigned int      target_type,
                                        unsigned int      time,
                                        GschemToplevel   *w_current)
{
  const GschemDndDataDef *datadef;

  const char *err_string_data = "gschem: unknown source data type requested";
  const char *string_data;

#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  printf ("<%s> %s: (%u)", __func__, name, target_type);
#endif

  if (!selection_data)
    return;

  switch (target_type)
  {
    case DND_TARGET_TEXT:
    case DND_TARGET_STRING:
    case DND_TARGET_PLAIN_TEXT:
    case DND_TARGET_UTF8_STRING: /* send as (UTF-8) text */
      datadef = &dnd_data_defs[DND_TARGET_UTF8_STRING];
      string_data = (datadef->send_data_func)(w_current, context, selection_data);
      gtk_selection_data_set_text (selection_data,
                                   string_data,
                                   strlen (string_data));

#if DEBUG || DEBUG_DND_EVENTS
      printf (" Sending string \"%s\".\n", string_data);
#endif
      g_free((void*)string_data);
      break;

    case DND_TARGET_OBJECTS:
      o_buffer_copy(w_current, DND_BUFFER);

#if DEBUG || DEBUG_DND_EVENTS
      printf (" Sending %d objects.\n", g_list_length(object_buffer[DND_BUFFER]));
#endif
      char *buf = geda_object_save_buffer (object_buffer[DND_BUFFER]);

      gtk_selection_data_set (selection_data,
                              (GdkAtom)target_type,
                              BITS_BYTE, /* 8-bit data (UTF-8) */
                              (unsigned char *) buf,
                              strlen(buf));

      GEDA_FREE (buf);
      break;

    default:
      /* Send an informative error message instead of failing. */
      gtk_selection_data_set_text (selection_data,
                                   err_string_data,
                                   strlen (err_string_data));
#if DEBUG || DEBUG_DND_EVENTS
      char *tt = gdk_atom_name((GdkAtom)target_type);
      printf ("%s: \"%s\" (%u)\n", err_string_data, tt, target_type);
      GEDA_FREE (tt);
#endif
      break;
  }
}

/* Emitted after "drag-data-received" is handled, and gtk_drag_finish is called
 * with the "delete" parameter set to TRUE (when DnD is GDK_ACTION_MOVE).
 */
static void
x_dnd_drag_delete (GtkWidget *widget, GdkDragContext *context, GschemToplevel *w_current)
{
#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  printf ("<%s> %s\n", __func__, name);
#endif
  return;
}

/* Emitted when DnD begins. This event is triggered when a drag from Gschem
 * begins to some other window, which could be another Gschem. This is often
 * used to present custom graphics.
 * \todo: implement custom graphics */
static void
x_dnd_drag_begin (GtkWidget *widget, GdkDragContext *context, GschemToplevel *w_current)
{
#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  fflush(stdout);
  printf ("<%s> %s: ia=%d, state=%d, dnd_save_state=%d\n", __func__, name, w_current->inside_action, w_current->event_state, w_current->dnd_save_state );
#endif

  w_current->dnd_state = SELECT;
}

/*!
 * \brief When Pointer Leaves the Drawing Area
 * \par Function Description
 *  Called when DnD ends to clean up leftover data. We have a couple of
 *  things to do here; first, free the event that was saved when the mouse
 *  button was pressed down to start the implicit Move action and secondly,
 *  cancel the Move action so the object gets put back where it was before
 *  the D&D operation.
 */
static void x_dnd_drag_end (GtkWidget *widget, GdkDragContext *context,
                                               GschemToplevel *w_current)
{

#if DEBUG || DEBUG_DND_EVENTS
  const char *name;
  name = gtk_widget_get_name (widget);
  printf ("\n<%s> %s: on entry: w_current->event_state =%d\n",  __func__, name, w_current->event_state);
#endif

  if (w_current->drag_event) {
    gdk_event_free(w_current->drag_event);
    w_current->drag_event = NULL;
  }

  switch (w_current->dnd_save_state) {
    case COPYMODE:
      o_copy_cancel (w_current);
      break;

    case DRAGMOVE:
    case MOVEMODE:
      if (w_current->event_state != ENDDND_MOVE_OBJ) {
        o_move_cancel (w_current);
      }
      break;

    case COMPMODE:
    default:
      break;
  }

  if (w_current->dnd_state) {
    i_status_action_start(w_current);
    w_current->event_state = w_current->dnd_state;
  }

  o_invalidate_all (w_current);

#if DEBUG || DEBUG_DND_EVENTS
  printf (" on exit: %s w_current->event_state =%d\n", name, w_current->event_state);
#endif
}
/** @} end-subgroup Drag-N-Drop-Source  */

/*!
 * \brief When Pointer Leaves the Drawing Area
 * \par Function Description
 *  This gets called when the pointer cursor leaves the Drawing Area window.
 *  Unfortunately, with GTK the "leave_notify_event" does not translate to
 *  "moved beyond the boundary of". If the cursor is moved over any object,
 *  such as a popup menu or the component dialog, this function gets called.
 *  We could easily check for a boundary condition but all of our dialogs
 *  are allowed to be either inside OR outside of the main window boundary
 *  so that won't work too well. Instead, if we are not inside an action or if
 *  we are and the event-state is not one we are interested in, i.e. DRAGMOVE,
 *  MOVEMODE, or ENDCOPY, then we do nothing. Else, we initiate a D&D. And this
 *  allows Gschem to function as it otherwise would, i.e. implicit Move actions
 *  or drawing selection boxes.
 *  [This fixes an annoyance with Gschem, which drops what ever is being
 *  moved, some where, if the pointer is not returned to the Drawing Area
 *  before releasing the mouse button. Usually is near the edge of window
 *  but drops can be completely out-side the current view. Other drawing
 *  programs, lacking D&D, also do this, an example is Inkscape]
 *
 * \param [in] widget     is Drawing Area    = DrawingArea
 * \param [in] event      GdkEventCrossing * = not what we want.
 * \param [in] w_current  pointer to GschemToplevel data structure.
 */
static bool
x_dnd_source_leave (GtkWidget *widget, GdkEventCrossing *event, GschemToplevel *w_current)
{
  /* Only interested in the pointer leaving the drawing area while
   * button 1 was down and we are doing something */
  if ((event->state & GDK_BUTTON1_MASK) && w_current->inside_action) {

    w_current->dnd_save_state = 0;

#if DEBUG || DEBUG_DND_EVENTS
    printf ("<%s> %s: ia=%d, state=%d\n", __func__, gtk_widget_get_name (widget),
            w_current->inside_action, w_current->event_state);
#endif

    switch (w_current->event_state) {
      case SBOX:
        return FALSE;

      case DRAGMOVE:
      case MOVEMODE:
      case COPYMODE:
        /*  case ENDCOMP: We're not quite there yet */
        w_current->dnd_save_state = w_current->event_state; /* save state */
        w_current->event_state = STARTDND;
        w_current->action_event->state = STARTDND;
        break;

      default:
        break;
    }

    if (w_current->event_state == STARTDND) {

      GdkDragContext *context;
      GtkTargetList  *target_list;

      target_list = gtk_target_list_new (dnd_send_list, dnd_ntargets - 1);

      /*! \note We pass the event we got when the button was pressed, which
       *  was saved for us by x_event_pressed in w_current, not the event
       *  Crossing that was passed to us */

      context = gtk_drag_begin(DrawingArea, /* or widget */
                               target_list,
                               w_current->drag_action,
                               GDK_BUTTON_PRIMARY,
                               w_current->drag_event);
      gtk_target_list_unref (target_list);
      gtk_drag_set_icon_stock(context, "geda-component", 0, 0);
    }
  }

#if DEBUG || DEBUG_DND_EVENTS
  printf ("<%s> on exit: w_current->event_state =%d\n", __func__, w_current->event_state);
#endif

  return FALSE;
}

/*!
 * \brief Setup Drag-n-Drop Events for the Drawing Area
 * \par Function Description
 *  enough said!
 */
void x_dnd_setup_event_handlers (GschemToplevel *w_current)
{
  struct event_reg_t {
    char *detailed_signal;
    GCallback c_handler;
  };

  struct event_reg_t drag_n_drop_events[] = {

    /* Possible destination signals */
    { "drag-data-received",   G_CALLBACK (x_dnd_drag_receive)    },
#if DEBUG || DEBUG_DND_EVENTS
    { "drag-leave",           G_CALLBACK (x_dnd_drag_leave)      },
  /*{ "drag-motion",          G_CALLBACK (x_dnd_drag_motion)     },*/
#endif
    { "drag-drop",            G_CALLBACK (x_dnd_drag_drop)       },

    /* Possible source signals */
    { "drag-data-get",        G_CALLBACK (x_dnd_drag_data_get)   },
    { "drag-data-delete",     G_CALLBACK (x_dnd_drag_delete)     },
    { "drag-begin",           G_CALLBACK (x_dnd_drag_begin)      },
    { "drag-end",             G_CALLBACK (x_dnd_drag_end)        },

    { "leave_notify_event",   G_CALLBACK (x_dnd_source_leave)    },

    { NULL,                   NULL                               }
  };

  struct event_reg_t *tmp;

  //gtk_widget_add_events(DrawingArea, GDK_LEAVE_NOTIFY_MASK );
                                   //GDK_ENTER_NOTIFY_MASK );

  /* Make the Drawing Area a DnD destination. */
  gtk_drag_dest_set (DrawingArea,  /* Our widget that will accept the drop */
                     GTK_DEST_DEFAULT_MOTION,  /* default actions for dest on DnD */
                     dnd_target_list,          /* lists of targets we support */
                     dnd_ntargets,             /* size of list */
                     GDK_ACTION_COPY);         /* what to do with data after dropped */

  for (tmp = drag_n_drop_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (DrawingArea,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }
}

/** @} endgroup Drag-N-Drop */
/******************************************************************************/
