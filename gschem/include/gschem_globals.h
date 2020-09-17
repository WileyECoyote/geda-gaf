/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/globals.h $"
 *
 * gEDA - GPL Electronic Design Automation
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file gschem_globals.h
 * \brief Gschem global variables and type definitions
 */

#ifndef H_GSCHEM_GLOBALS_H
#define H_GSCHEM_GLOBALS_H

/* Our Process ID */
extern int prog_pid;

/* window list */
extern GList *global_window_list;

/* command line options */

extern int  auto_place_mode;
extern int  auto_load_last;
extern int  iconify_main_window;
extern int  override_autoload;
extern int  run_mode;
extern int  quiet_mode;
extern int  verbose_mode;
extern char *comline_font;
extern char *comline_tblock;
extern char *output_filename;
extern char *rc_filename;
extern char *start_session;
extern char *tmp_directory;

/* Log Related */
extern volatile int logging;             /* controls if whether logging is enabled or not */
extern volatile int log_destiny;         /* controls where the log system writes data  */
extern volatile int console_window;      /* maybe should be widget pointer, if enabled */
extern volatile int console_window_type; /* controls if console dialog is decorated */

#define command_history 22

/* Global buffers - see file globals.c*/
#define MAX_BUFFERS      7
#define DND_BUFFER   MAX_BUFFERS - 1
extern GList *object_buffer[MAX_BUFFERS];

/* Hooks */
extern SCM complex_place_list_changed_hook;

/* Gettext translation */
#include "gettext.h"

typedef enum  { CAIRO_ADAPTOR, X11_ADAPTOR } RenderSystem;

typedef enum  { Image_Display, Image_All } ImageExtent;

/*! \brief Enumerated Action Origin - EID_ACTION_ORIGIN
 *  \par Description
 *  All modules passing actions to i_command_process use these to
 * indicate how the action was initiated. The command processor will
 * automatically capture positional information for actions originating
 * from ID_ORIGIN_KEYBOARD or ID_ORIGIN_MOUSE. Action handlers can use
 * the macro CMD_WHO to get this value. (see do_zoom_in or do_add_attribute
 * for examples of action handlers that utilize EID_ACTION_ORIGIN.)
 */
typedef enum { ID_ORIGIN_MENU       = -32, /* can't pass paramerter */
               ID_ORIGIN_TOOLBAR,  /* -31     can't pass paramerter */
               ID_ORIGIN_KEYBOARD, /* -30     can't pass paramerter */
 /* popup */   ID_ORIGIN_MOUSE,    /* -29     can't pass paramerter */
 /* clicked */ ID_ORIGIN_EVENT,    /* -28     can't pass paramerter */
               ID_ORIGIN_SCM,      /* -27     could pass paramerter */
               ID_ORIGIN_STROKE,   /* -26     could pass paramerter */
               ID_ORIGIN_CONSOLE,  /* -25     could pass paramerter */
               ID_ORIGIN_COMMAND,  /* -24     could pass paramerter */
               ID_ORIGIN_CCODE,    /* -23     could pass paramerter */
} EID_ACTION_ORIGIN;


/*! \brief Enumerated Selection Mode - EID_EVENT_HANDLERS
 *  \par Description
 *  Used as argument to i_event_block_xxx i_event_unblock_xxx to identify
 *  which handler to blocked or unblock.
 */
typedef enum  { EXPOSE_EVENT_HANDLER,
                BUTTON_PRESS_HANDLER,
                BUTTON_RELEASE_HANDLER,
                MOTION_NOTIFY_HANDLER,
                CONFIGURE_EVENT_HANDLER,
                KEY_PRESS_EVENT_HANDLER,
                KEY_RELEASE_EVENT_HANDLER,
                SCROLL_EVENT_HANDLER
} EID_EVENT_HANDLERS;
#define EventHandler EID_EVENT_HANDLERS

/*! \enum EID_HOOK_TYPE Hook Record Data Type */
typedef enum {
               LIST_HOOK,
               OBJECT_HOOK,
               PAGE_HOOK,
} EID_HOOK_TYPE;


/*! \enum  EID_SCM_HOOKS Enumerated SCM Hook Identifier
 *  \par Description
 *  Used to identify which SCM hook should be called.
 */
typedef enum {
              INVALID_HOOK,
              ACTION_PROPERTY_HOOK,
              ADD_OBJECT_HOOK,
              ATTACH_ATTRIBS_HOOK,
              BIND_KEYS_HOOK,
              CHANGE_PAGE_HOOK,
              COPY_OBJECTS_HOOK,
              CLOSE_PAGE_HOOK,
              DESELECT_OBJECTS_HOOK,
              DETACH_ATTRIBS_HOOK,
              MIRROR_OBJECTS_HOOK,
              MOVE_OBJECTS_HOOK,
              NEW_PAGE_HOOK,
              OPEN_PAGE_HOOK,
              PASTE_OBJECTS_HOOK,
              REMOVE_OBJECTS_HOOK,
              ROTATE_OBJECTS_HOOK,
              SELECT_OBJECTS_HOOK
} EID_SCM_HOOKS;
#define Hooker EID_SCM_HOOKS


/*! \brief Enumerated Pan Directive - EID_PAN_DIRECTIVES
 *  \par Description
 *  Directives used as arguments to zoom and pan routines that indicate
 *  whether to invalidate screen regions, resulting in screen redraws and
 *  or whether borders should be ignored.
 */
typedef enum { I_PAN_REDRAW,
               I_PAN_IGNORE_BORDERS,
               I_PAN_DONT_REDRAW
} EID_PAN_DIRECTIVES;


/*! \brief Enumerated Selection Mode - EID_SELECTION_MODE
 *  \par Description
 *  Used as argument to routines in o_find module to indicate
 *  how found object should be handled.
 */
typedef enum  { SELECTION_NOACTION,
                SELECTION_REPLACE,
                SELECTION_ADD
} EID_SELECTION_MODE;


/*! \brief Enumerated Sensitivity Mode - EID_SENITIVITY_MODE
 *  \par Description
 *  The ToolBar module maintains lists of GtkToolBarItems groups. These
 *  enumerators refer to groups associated with the active state and can
 *  be passed to x_toolbars_set_sensitivities to enable or disable the
 *  items.
 */
typedef enum { ANY_OBJECT, CAN_PASTE, CAN_UNDO, CAN_REDO, CAN_HATCH, CAN_ELINE,
               COMPLEX_OBJECT, HAVE_PAGES, HAVE_PIN, HAVE_TEXT
} EID_SENITIVITY_MODE;


/*! \enum EID_TEXT_SEARCH_DIRECTIVES Text Search Directives
 *  \brief Enumeration for o_edit.c::o_edit_find_text
 */
typedef enum {
  SEARCH_DESCEND  = 1,
  SEARCH_HIDDEN   = 2,
} EID_TEXT_SEARCH_DIRECTIVES;


/*! \enum  EID_ZOOM_DIRECTIVE
 *  \brief Enumerated Zoom Directive - ZOOM_DIRECTIVE
 *  \par Description
 *  Zoom Directives are used as an argument to i_zoom_world to indicate
 *  the polarity and magnitude for the relative zoom factor.
 */
typedef enum { ZOOM_OUT_DIRECTIVE,
               ZOOM_IN_DIRECTIVE,
               ZOOM_FULL_DIRECTIVE
} EID_ZOOM_DIRECTIVE;

/*! \enum EID_UNDO_TYPE undo-type
 *  \brief Undo Type mechanism the Undo system is to use
 */
typedef enum {UNDO_NONE, UNDO_DISK, UNDO_MEMORY} EID_UNDO_TYPE;

/* define some convenience macros (helps reduce lines lengths) */
#define i_status_action_start(w) i_status_update_action_state (w, TRUE);
#define i_status_action_stop(w) i_status_update_action_state (w, FALSE);

#define CairoRenderer w_current->cairo_renderer

#define Current_Page      w_current->toplevel->page_current
#define Current_Selection w_current->toplevel->page_current->selection_list
#define Current_PlaceList w_current->toplevel->page_current->place_list

/*EK* used by prototype.h */
#include "../include/x_states.h"
#endif
