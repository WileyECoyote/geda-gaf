/* -*- geda-h -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef H_GSCHEM_GLOBALS_H
#define H_GSCHEM_GLOBALS_H

/* window list */
extern GList *global_window_list;

/* colors */
extern GdkColor white;
extern GdkColor black;

extern char *rc_filename;
extern char *output_filename;

/* command line options */
extern int quiet_mode;
extern int verbose_mode;
extern int auto_place_mode;

extern int auto_load_last;

/* Log Related */
extern volatile int logging;           /* controls if whether logging is enabled or not */
extern volatile int log_destiny;       /* controls where the log system writes data */
extern volatile int console_window;
extern volatile int console_window_type;

#define command_history 22
#define MAX_BUFFERS 	6

/* Global buffers */
extern GList *object_buffer[MAX_BUFFERS];

/* Hooks */
extern SCM complex_place_list_changed_hook;

#include "gettext.h"
#ifdef ENABLE_NLS
# ifdef gettext_noop
#  define N_(String) gettext_noop (String)
# else
#  define N_(String) (String)
# endif
#else
# define N_(String) (String)
#endif

/*
 * __attribute__((unused)) is a gcc extension so define
 * a portable macro, ATTRIBUTE_UNUSED, to use instead
 */
#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif /* GCC_VERSION */

#if GCC_VERSION > 2007
#define ATTRIBUTE_UNUSED __attribute__((unused))
#else
#define ATTRIBUTE_UNUSED
#endif

typedef enum  { Image_Display, Image_All,} ImageExtent;

typedef enum { CAN_PASTE, CAN_UNDO, CAN_REDO, HAVE_PAGES, COMPLEX_OBJECTS,
               SOME_OBJECTS, TEXT_OBJECTS
} ID_SENITIVITY_MODE;

typedef enum { ID_ORIGIN_MENU = -32, /* can't pass paramerter */
               ID_ORIGIN_TOOLBAR,    /* can't pass paramerter */
               ID_ORIGIN_KEYBOARD,   /* can't pass paramerter */
               ID_ORIGIN_MOUSE,      /* can't pass paramerter */
               ID_ORIGIN_SCM,        /* could pass paramerter */
               ID_ORIGIN_STROKE,     /* could pass paramerter */
               ID_ORIGIN_CONSOLE,    /* could pass paramerter */
               ID_ORIGIN_CAMMAND,    /* could pass paramerter */
} ID_ACTION_ORIGIN;

/* This macro is used to reduce lines lengths */
#define Current_Selection w_current->toplevel->page_current->selection_list

/* Utility Macros for Message Dialogs */
#define message_dialog(text, type) \
        gschem_message_dialog(text, type, NULL);
#define titled_message_dialog(text, type, title) \
        gschem_message_dialog(text, type, title);
#define information_dialog(text) \
        message_dialog(text, GEDA_MESSAGE_INFO)
#define titled_information_dialog(text, title) \
        titled_message_dialog(text, GEDA_MESSAGE_INFO, title)
#define warning_dialog(text) \
        message_dialog(text, GEDA_MESSAGE_WARNING)
#define titled_warning_dialog(text, title) \
        titled_message_dialog(text, GEDA_MESSAGE_WARNING, title)
#define error_dialog(text) \
        gschem_message_dialog(text, GEDA_MESSAGE_ERROR, NULL)
#define titled_error_dialog(text, title) \
        titled_message_dialog(text, GEDA_MESSAGE_ERROR, title)

/*EK* used by prototype.h */
#include "../include/x_states.h"
#endif
