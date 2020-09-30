/* gEDA - GPL Electronic Design Automation
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*! \todo Add global variable documentation!!!
 *
 */

#include "../include/gschem.h"

/* Our Process ID as reported by getpid, set by o_undo_init */
int prog_pid = 0;

/* window list */
GList *global_window_list = NULL;

/* Color Related */
int cmap_flag = 0;

/* command line options */
int  auto_place_mode      = FALSE; /*!< automatically place window, -p enables */
int  auto_load_last       = FALSE; /*!< auto load last document, -n disables */
int  iconify_main_window  = FALSE; /*!< hide main window for scripting -z enables */
int  override_autoload    = FALSE;
int  quiet_mode           = FALSE;
int  run_mode             = 0;
int  verbose_mode         = FALSE;
char *start_session;
char *comline_font;
char *comline_tblock;
char *output_filename = NULL;
char *rc_filename     = NULL;
char *tmp_directory   = NULL;

/* Global Log Configuration - gotta move someday, these should not be
 * globals */
volatile int logging;
volatile int log_destiny;
volatile int console_window;
volatile int console_window_type;

/*! \brief General Purpose Global GList Buffers
 *  \par Description
 * Gschem currently uses 7 glist buffers, the first 5 are used
 * as auxillary clipboard buffers. These are the buffers in the
 * menus labeled 1 through 5, cooresponding to object_buffer[0]
 * thru object_buffer[4]. The system clipboard is not mixed with
 * these buffers. The system clipboard data is stored in toplevel
 * member clipboard_buffer.
 * Currently, object_buffer[5] and object_buffer[6] are not used,
 * object_buffer[7] is used by Drag & Drop.
 */
GList *object_buffer[MAX_BUFFERS];

/* Hooks */
SCM complex_place_list_changed_hook;
