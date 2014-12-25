/* -*- C x_dnd.c  indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_dnd.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
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

#include "gschem.h"
#include "x_window.h"
#include "x_dnd.h"

#include <geda_debug.h>

/****************************** Drag-N-Drop **********************************/

/** \defgroup Drag-N-Drop Gschem Drag and Drop Module
 *  @{ \par
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

typedef const char *(* x_dnd_str_data_drag_func) (GschemToplevel *w_current, Object *object);

static GtkTargetEntry dnd_target_list[] = {
    GSCHEM_TARGET_NONE,
    GSCHEM_TARGET_TEXT,
    GSCHEM_TARGET_STRING,
    GSCHEM_TARGET_TEXT_PLAIN,
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

static bool x_dnd_source_leave    (GtkWidget *widget,
                                   GdkEventCrossing *event,
                                   GschemToplevel *w_current);

const char *x_dnd_send_string     (GschemToplevel   *w_current,
                                   GdkDragContext   *context,
                                   GtkSelectionData *selection);

const char *x_dnd_send_objects    (GschemToplevel   *w_current,
                                   GdkDragContext   *context,
                                   GtkSelectionData *selection);

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
const char*
x_dnd_send_string_nil (GschemToplevel *w_current, Object *object)
{
  return (DND_NIL);
}
const char*
x_dnd_send_string_object (GschemToplevel *w_current, Object *object)
{
  const char *string_1;
  const char *string_2;

  switch (object->type) {
    case OBJ_COMPLEX:
      string_1 = object->complex->filename;
      string_2 = "";
      break;
    case OBJ_PICTURE:
      string_1 = DND_FILE_LEADER;
      string_2 = object->picture->filename;
      break;
    default:
      string_1 = "Error:";
      string_2 = "unidentified complex object";
  }
  return u_string_concat (string_1, string_2, NULL);;
}

/******************* Shape Catagory Data Helpers *******************/
static char dnd_string_data_name[10];

const char *x_dnd_string_data_name(char* name)
{

  int  index;
  for (index = 0; index < sizeof(dnd_string_data_name); index++) {
    if (*name == '.')
      break;
    dnd_string_data_name[index] = *name++;
  }
  dnd_string_data_name[index] = '\0'; /* replace period with NULL*/
  return &dnd_string_data_name[0];
}

char *x_dnd_string_data_arc_properties(Arc *arc)
{
  return
  g_strdup_printf("center=(%d,%d), radius=%d, angle=%d",
             arc->x, arc->y, arc->width, arc->start_angle);
}
char *x_dnd_string_data_circle_properties(Circle *circle)
{
  return
  g_strdup_printf("center=(%d,%d), radius=%d", circle->center_x,
                   circle->center_y, circle->radius);
}
char *x_dnd_string_data_line_properties(Line *line)
{
  return
  g_strdup_printf("start (%d,%d), end (%d,%d)",
                  line->x[0], line->y[0], line->x[1], line->y[1]);
}
char *x_dnd_string_data_path_properties(Path *path)
{
  return
  g_strdup_printf("sections=%d",path->num_sections);
}
char *x_dnd_string_data_box_properties(Box *box)
{
  return
  g_strdup_printf("upper point (%d,%d), lower point (%d,%d)",
                  box->upper_x, box->upper_y, box->lower_x, box->lower_y);
}

const char *x_dnd_send_string_shape (GschemToplevel *w_current, Object *object)
{
  const char *string;
  const char *name;
        char *common;   /* common to all shapes */
        char *properties;

  name   = x_dnd_string_data_name(object->name);
  common = g_strdup_printf("line width=%d, dash=%d",
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
        properties = g_strdup_printf("%s", " object data error");
  }

  string = g_strdup_printf ("%s, sequence id=%d, %s, %s, color=%d",
                             name, object->sid, common, properties,
                             object->color);

  GEDA_FREE(common);
  GEDA_FREE(properties);
  return string;
}

const char*
x_dnd_send_string_signal (GschemToplevel *w_current, Object *object)
{
  const char *string;
  const char *name;
        char *netname;

  name        = x_dnd_string_data_name(object->name);

  netname = o_attrib_search_object_attribs_by_name(object, "netname", 0);

  if (netname == NULL)
      netname = g_strdup_printf("%s","NONE,");

  string = g_strdup_printf ("%s, sequence id=%d, netname=%s color=%d",
                             name, object->sid, netname,
                             object->color);

  if (netname != NULL) GEDA_FREE(netname);
  return string;
}
const char*
x_dnd_send_string_text (GschemToplevel *w_current, Object *object)
{
  const char *string;
  const char *name;
  name   = x_dnd_string_data_name(object->name);
  string = g_strdup_printf ("%s:%s, sequence id=%d, color=%d", name,
                             object->text->string, object->sid,
                             object->color);
  return string;
}

/******************* Begin Handlers by Data Catagory ******************/

/*! \brief Route Request for String Data to type Specific Sub-Handlers */
const char*
x_dnd_send_string (GschemToplevel *w_current, GdkDragContext   *context,
                   GtkSelectionData *selection)
{
  Object *object;
  x_dnd_str_data_drag_func  dnd_str_data_func;

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

const char *x_dnd_send_objects (GschemToplevel   *w_current,
                                GdkDragContext   *context,
                                GtkSelectionData *selection)
{
    return DND_NIL;
}

/*! \brief When Received String filename with SYM suffix
 *
 *  \par Function Description
 *
 * Called by x_dnd_receive_string after determining the recieved
 * string ends with ".sym".
*/
bool x_dnd_receive_string_sym(GschemToplevel *w_current, int x, int y, const char *filename, int where)
{
  bool        result;
  char       *symbolfile;
  char       *path;

  const       CLibSymbol *symbol = NULL;
  Page       *page;
  Object     *object;

  page = Current_Page;
  path = g_path_get_dirname(filename);

  if (s_clib_source_path_exist(path)) {

    symbolfile = g_path_get_basename (filename);
    symbol     = s_clib_get_symbol_by_name(symbolfile);

    result = FALSE;
    if (symbol) {

      if(where == DROPPED_ON_COORD) {
        object = o_complex_new (w_current->toplevel, x, y, 0,
                                FALSE, symbol, symbolfile, TRUE);

        s_page_append_object(page, object);
        o_place_end(w_current, x, y, 0, 0, NULL);
      }
      else {
        /* If the current page is not a sym, then insert symbol*/
        if (!s_page_is_symbol_file(page)) {
          o_redraw_cleanstates    (w_current);
          o_complex_prepare_place (w_current, symbol);
        }
        else {
          result = TRUE;
        }
      }
    }
    else {
      u_log_message(_("Could not locate symbol [%s] in library, try refreshing\n"), symbolfile);
    }
    GEDA_FREE(symbolfile);
  }
  else { /* symbol file is not our library source path so load new page */
    v_log_message(_("symbol [%s] not in library opening as page\n"), filename);
    result = TRUE;
  }
  GEDA_FREE(path);
  return result;
}

/*! \brief Process String Data Received from a Drag & Drop Source */
bool
x_dnd_receive_string(GschemToplevel *w_current, int x, int y, const char *buffer, int where)
{
  Page *page;

  bool  load_as_page;   /* Could be either .sch or .sym */
  bool  result;

  const char *leader   = DND_FILE_LEADER;
  const char  bad_tail = '\x13';
  char       *filename;

  int   head;
  int   tail;
  int   len;

  result = FALSE;

  if (buffer) {

    load_as_page   = FALSE;
    result         = TRUE;

    /* g_str_has_prefix worked for a couple weeks, and then it stop working
     * has to do with gcc escaping //, even strncmp didn't work, crap. */
    for(head = 0; head < 8; head++) {
      if (buffer[head] != leader[head]) {break;}
    }

    /* TODO: If there is not a file:// prefix then we should still load but
     * don't have a means to test, someone is always adding the prefix */
    if (head == 7) {

      len = strlen(buffer);

      len = len - 7;                               /* substrate out the prefix */
      for(tail = head; tail < len; tail++) {
        if (buffer[tail] == '\0' || buffer[tail] == bad_tail ) break;
      }

      --tail;  /* backup from what ever we found */
      --tail;  /* + 1 more, is base zero      */

      filename = g_strndup (buffer + 7, tail );  /* copy just the file spec */

      /* now that we have stripped the bad stuff, glib will work for us again */
      if ( g_str_has_suffix(filename, SCHEMATIC_FILE_DOT_SUFFIX)) {
        load_as_page = TRUE;
      }
      else if ( g_str_has_suffix(filename, SYMBOL_FILE_DOT_SUFFIX)) {
        load_as_page = x_dnd_receive_string_sym(w_current, x, y, filename, where);
      }

      if ( load_as_page ) {
        page = x_window_open_page(w_current, filename);
        if (page)
          x_window_set_current_page (w_current, page);
        else
          result = FALSE;
      }
      GEDA_FREE(filename);
    }
  }
  else {
    fprintf(stderr, "<%s>: TODO insert string [%s]\n", __func__, buffer);
  }

  return result;
}

/*! \brief Process Object Data Received from Drag & Drop Source */
bool
x_dnd_receive_objects(GschemToplevel  *w_current, int x, int y, const char *buffer, int who)
{
  GError *err;
  bool    result;

  if (buffer) {

    /* Make sure the buffer is empty, if str maybe in deep dodo */
    if (object_buffer[DND_BUFFER] != NULL) {
      s_object_release_objects(object_buffer[DND_BUFFER]);
      object_buffer[DND_BUFFER] = NULL;
    }

    err = NULL;

    /* Copy the objects to the Drag&Drop buffer */
    object_buffer[DND_BUFFER] = o_read_buffer (w_current->toplevel,
                                               object_buffer[DND_BUFFER],
                                               buffer,
                                               -1, "Drag&Drop", &err);
    if (err) {
      char *errmsg = g_strdup_printf ( _("An error occurred while dropping data: %s."), err->message);
      titled_pango_error_dialog ( _("<b>Invalid Data.</b>"), errmsg, _("Drag&Drop failed") );
      GEDA_FREE(errmsg);
      g_error_free(err);
      result = FALSE;
    }
    else {
      gtk_window_present(GTK_WINDOW(w_current->main_window));
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


/*! \brief When Drag Received from the Source
 *
 *  \par Function Description
 *
 * Called when the data has been received from the source. It should check
 * the GtkSelectionData sent by the source, and do something with it. Finally
 * it needs to finish the operation by calling gtk_drag_finish, which will emit
 * the "data-delete" signal if told to.
*/
void
x_dnd_drag_receive
(GtkWidget *widget, GdkDragContext *context, int x, int y,
        GtkSelectionData *selection_data, guint target_type, guint time,
        GschemToplevel *w_current)
{
  const GschemDndDataDef  *datadef;
  const unsigned char     *buffer;
  const char              *string;

  bool  dnd_success = FALSE;
  bool  delete_data = FALSE;

#if DEBUG  || DEBUG_DND_EVENTS
  const char  *name = gtk_widget_get_name (widget);
  g_print ("<%s> %s:", __func__, name);
#endif

  /* Deal with what we are given from source */
  if ((selection_data != NULL) &&
    (gtk_selection_data_get_length(selection_data) >= 0))
  {
    //if (gdk_drag_context_get_suggested_action(context) == GDK_ACTION_ASK)
    if (context->suggested_action == GDK_ACTION_ASK) {
      /* Ask the user to move or copy, then set the context action. */
    }
    else if (context->suggested_action == GDK_ACTION_MOVE) {
      delete_data = TRUE;
    }

    /* Get pointer to the data */
#if GTK_CHECK_VERSION(2,14,0)
    buffer = gtk_selection_data_get_data (selection_data);
#else
    buffer = selection_data->data;
#endif

    string = (char*)buffer;

    /* Check that we got the format we can use */
    switch (target_type) {

      case DND_TARGET_TEXT:
      case DND_TARGET_STRING:
      case DND_TARGET_PLAIN_TEXT:
      case DND_TARGET_UTF8_STRING:
        datadef = &dnd_data_defs[DND_TARGET_UTF8_STRING];
        dnd_success = (datadef->receive_data_func)(w_current, x, y, string, DROPPED_ON_CANVAS);

#if DEBUG  || DEBUG_DND_EVENTS
        g_print (" string handler returned %d\n", dnd_success);
#endif
        break;

      case DND_TARGET_OBJECTS:
        datadef = &dnd_data_defs[DND_TARGET_OBJECTS];
        dnd_success = (datadef->receive_data_func)(w_current, x, y, string, DROPPED_ON_CANVAS);

#if DEBUG  || DEBUG_DND_EVENTS
          g_print (" object handler returned %d\n", dnd_success);
#endif
        if (dnd_success) {
          o_redraw_cleanstates (w_current);
          if (o_buffer_paste_start (w_current, x, y, DND_BUFFER)) {
            w_current->inside_action = 1;
            i_status_set_state (w_current, ENDPASTE);
            dnd_success = TRUE;
          }
          else {
            dnd_success = TRUE;
          }
        }
        break;

      default:
        g_print ("<%s> nothing good.\n", __func__);
    }
  }

  gtk_drag_finish (context, dnd_success, delete_data, time);
}

#if DEBUG  || DEBUG_DND_EVENTS

/*! \brief When Drag Leaves the Destination
 *
 *  \par Function Description
 *
 *  Called when the drag leaves the destination.
 */
void x_dnd_drag_leave
(GtkWidget *widget, GdkDragContext *context, guint time, GschemToplevel *w_current)
{
  const char *name = gtk_widget_get_name (widget);

}

#endif

/*! \brief When Drag Motion over the Destination
 *
 *  \par Function Description
 *
 *  Called when a drag is over the destination.
 *
 * \return FALSE if the operation should continue
 */
/* Emitted  */
bool x_dnd_drag_motion
(GtkWidget *widget, GdkDragContext *context, int x, int y, guint t, GschemToplevel *w_current)
{
  // Fancy stuff here. This signal spams the console something horrible.
  //const char *name = gtk_widget_get_name (widget);
  return  FALSE;
}
/*
static void
drag_motion (GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint time)
{
  GdkModifierType mask;
  gdk_window_get_pointer (gtk_widget_get_window (widget), NULL, NULL, &mask);
  if (mask & GDK_CONTROL_MASK)
    gdk_drag_status (context, GDK_ACTION_COPY, time);
  else
    gdk_drag_status (context, GDK_ACTION_MOVE, time);

}
*/


/*! \brief When Drag gets Dropped on the Drawing Area
 *
 *  \par Function Description
 *
 *  Called when the user releases (drops) the selection. We choose the
 *  target type we wish the source could send. We call gtk_drag_get_data
 *  which will emit "drag-data-get" on the source, passing along our wish.
 *
 *  \return TRUE if the operation should continue, otherwise FALSE
 */
bool x_dnd_drag_drop
(GtkWidget *widget, GdkDragContext *context, int x, int y, guint time, GschemToplevel *w_current)
{
  bool            is_valid_drop_site;
  GtkTargetEntry *target_entry;
  GdkAtom         target_type;
  GList          *targets;
  GList          *iter;
  int             index;

#if DEBUG  || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  g_print ("\n<%s> %s: entry\n", __func__, name);
#endif

  /* Check to see if (x,y) is a valid drop site within widget */
  is_valid_drop_site = TRUE;

  /* Get a list of target types to choose from */
  targets = context->targets;

  /* If the source offers a target */
  if (targets) {

    //targets = gdk_drag_context_list_targets(context);

    /* Set what we really want! */
    target_type = GDK_POINTER_TO_ATOM (&dnd_target_list[DND_TARGET_OBJECTS]);

    index  = dnd_ntargets - 1;
    /* For each of our targets, look backwards to see if we find a match */
    for (target_entry = &dnd_target_list[index]; index > -1 ; index--) {
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

#if DEBUG  || DEBUG_DND_EVENTS
    g_print ("<%s> Site is Valid target\n", __func__);
#endif
  }

  /* No target offered by source => error */
  else {
    is_valid_drop_site = FALSE;

#if DEBUG  || DEBUG_DND_EVENTS
    g_print ("<%s> Target site is Invalid\n", __func__);
#endif

  }
  return  is_valid_drop_site;
}

/** @} end-subgroup Drag-N-Drop-Destination  */

/******************************************************************************/
/* Signals receivable by source */

/** \defgroup Drag-N-Drop-Source Drag N Drop Source (Originator) Functions
 *  @{ \par This sub-group contains routines to handle signals receivable by the
 *          Source
*/

/*! \brief When Destination Request Data from the Source
 *
 *  \par Function Description
 *
 * Called when the destination requests data from the source via
 * gtk_drag_get_data. We attempt to provide data in the form requested in
 * the target_type passed to us from the destination. If we cannot, then
 * default to a "safe" type, i.e. a string or text, even if only to print
 * an error. Then use gtk_selection_data_set to put the source data into
 * the allocated selection_data object, which will then be passed to the
 * destination. This will cause "drag-data-received" to be emitted on the
 * destination. GdkSelectionData is based on X's selection mechanism which,
 * via X properties, is only capable of storing data in blocks of 8, 16, or
 * 32 bit units.
 */
void x_dnd_drag_data_get
(GtkWidget *widget, GdkDragContext *context, GtkSelectionData *selection_data,
                    unsigned int target_type, unsigned int time, GschemToplevel *w_current)
{
  const GschemDndDataDef *datadef;

  const char *err_string_data = "gschem: unknown source data type request.";
  const char *string_data;

#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  g_print ("<%s> %s:", __func__, name);
#endif

  g_assert (selection_data != NULL);

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
#if DEBUG  || DEBUG_DND_EVENTS
      g_print (" Sending string \"%s\".\n", string_data);
#endif
      g_free((void*)string_data);
      break;

    case DND_TARGET_OBJECTS:
      o_buffer_copy(w_current, DND_BUFFER);
#if DEBUG  || DEBUG_DND_EVENTS
      g_print (" Sending %d objects.\n", g_list_length(object_buffer[DND_BUFFER]));
#endif
      char *buf = o_save_buffer (object_buffer[DND_BUFFER]);
      gtk_selection_data_set (selection_data,
                              gtk_selection_data_get_target(selection_data),
                              BITS_BYTE, /* 8-bit data (UTF-8) */
                              (unsigned char *) buf,
                              strlen(buf));

      GEDA_FREE (buf);
      break;

    default:
      /* Default to some a safe target instead of fail. */
      gtk_selection_data_set_text (selection_data,
                                   err_string_data,
                                   strlen (err_string_data));
#if DEBUG  || DEBUG_DND_EVENTS
      g_print (" Sending string \"%s\".\n", err_string_data);
#endif
      break;
  }
}

/* Emitted after "drag-data-received" is handled, and gtk_drag_finish is called
 * with the "delete" parameter set to TRUE (when DnD is GDK_ACTION_MOVE). */
void x_dnd_drag_delete
(GtkWidget *widget, GdkDragContext *context, GschemToplevel *w_current)
{
#if DEBUG  || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  g_print ("<%s> %s\n", __func__, name);
#endif
  return;
}

/* Emitted when DnD begins. This is often used to present custom graphics. */
void
x_dnd_drag_begin (GtkWidget *widget, GdkDragContext *context, GschemToplevel *w_current)
{
#if DEBUG  || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  fflush(stdout);
  g_print ("<%s> %s: ia=%d, state=%d, dnd_save_state=%d\n", __func__, name, w_current->inside_action, w_current->event_state, w_current->dnd_save_state );
#endif

  w_current->dnd_state = SELECT;
}

/*! \brief When Pointer Leaves the Drawing Area
 *
 *  \par Function Description
 *
 *  Called when DnD ends to clean up leftover data. We have a couple of
 *  things to do here; first, free the event that was saved when the mouse
 *  button was pressed down to start the implicit Move action and secondly,
 *  cancel the Move action so the object get put back where it was before
 *  the D&D operation.
 */
void x_dnd_drag_end
(GtkWidget *widget, GdkDragContext *context, GschemToplevel *w_current)
{

#if DEBUG  || DEBUG_DND_EVENTS
  const char *name;
  name = gtk_widget_get_name (widget);
  g_print ("\n<%s> %s: on entry: w_current->event_state =%d\n",  __func__, name, w_current->event_state);
#endif

  if (w_current->drag_event) {
    gdk_event_free(w_current->drag_event);
    w_current->drag_event = NULL;
  }

  switch (w_current->dnd_save_state) {
    case ENDCOPY:
      o_copy_cancel (w_current);
    case ENDMOVE:
      if (w_current->event_state != ENDDND_MOVE_OBJ) {

        o_move_cancel (w_current);
      }
        break;
    case ENDCOMP:
    default:
      break;
  }

  if (w_current->dnd_state) {
    w_current->event_state = w_current->dnd_state;
    w_current->inside_action = 1;
  }

  o_invalidate_all (w_current);

#if DEBUG  || DEBUG_DND_EVENTS
  g_print (" on exit: %s w_current->event_state =%d\n", name, w_current->event_state);
#endif
}
/** @} end-subgroup Drag-N-Drop-Source  */

/*! \brief When Pointer Leaves the Drawing Area
 *
 *  \par Function Description
 *
 *  This gets called when the pointer cursor leaves the Drawing Area window.
 *  Unfortunately, with GTK the "leave_notify_event" does not translate to
 *  "moved beyond the boundary of". If the cursor is moved over any object,
 *  such as a popup menu or the component dialog, this function gets called.
 *  We could easily check for a boundary condition but all of our dialogs
 *  are allowed to be either inside OR outside of the main window boundary
 *  so that won't work too well. Instead, if we are not inside an action or if
 *  we are and the event-state is not one we are interested in, i.e. ENDMOVE
 *  or ENDCOPY, then we do nothing. Else, we initiate a D&D. And this allows
 *  Gschem to function as it otherwise would, i.e. implicit Move actions or
 *  drawing selection boxes.
 *  [This fixes an annoyance with Gschem, which drops what ever is being
 *  moved, some where, if the pointer is not returned to the Drawing Area
 *  before releasing the mouse button. Usually is near the edge of window
 *  but drops can be completely out-side the current view. Other drawing
 *  programs, lacking D&D, also do this, an example is Inkscape]
 *
 *  \param [in] widget     is Drawing Area    = DrawingArea
 *  \param [in] event      GdkEventCrossing * = not what we want.
 *  \param [in] w_current  pointer to GschemToplevel data structure.
 */
static bool
x_dnd_source_leave (GtkWidget *widget, GdkEventCrossing *event, GschemToplevel *w_current)
{
  GdkDragContext *context;

  /* Only interested in the pointer leaving the drawing area while
   * button 1 was down and we are doing something */
  if ((event->state & GDK_BUTTON1_MASK) && w_current->inside_action) {

    w_current->dnd_save_state = 0;

#if DEBUG  || DEBUG_DND_EVENTS
    g_print ("<%s> %s: ia=%d, state=%d\n", __func__, gtk_widget_get_name (widget), w_current->inside_action, w_current->event_state);
#endif

    switch (w_current->event_state) {
      case ENDMOVE:
      case ENDCOPY:
        /*  case ENDCOMP: We're not quite there yet */
        w_current->dnd_save_state = w_current->event_state; /* save state */
        w_current->event_state = STARTDND;
        break;
      default:

        break;
    }

    if (w_current->event_state == STARTDND) {

      GtkTargetList  *target_list;

      target_list    = gtk_target_list_new (dnd_target_list, dnd_ntargets);

      /*! \note We pass the event we got when the button was pressed, which
       *  was saved for us by x_event_pressed in w_current, not the event
       *  Crossing that was passed to us */

      context  = gtk_drag_begin(DrawingArea, /* or widget */
                                target_list,
                                w_current->drag_action,
                                GDK_BUTTON_PRIMARY,
                                w_current->drag_event);
      gtk_target_list_unref (target_list);
      gtk_drag_set_icon_stock(context, "geda-component", 0, 0);
    }
  }

#if DEBUG  || DEBUG_DND_EVENTS
  g_print ("<%s> on exit: w_current->event_state =%d\n", __func__, w_current->event_state);
#endif

  return FALSE;
}

/*! \brief Setup Drag-n-Drop Events for the Drawing Area
 *
 *  \par Function Description
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
#if DEBUG  || DEBUG_DND_EVENTS
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

  gtk_widget_add_events(DrawingArea, GDK_LEAVE_NOTIFY_MASK );
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
