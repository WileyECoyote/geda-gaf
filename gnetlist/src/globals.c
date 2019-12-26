/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: globals.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
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

#include "../../config.h"

#include <gnetlist.h>
#include <geda_debug.h>

/* rc_filename is changable via parse_commandline */
char *rc_filename = NULL;
int   logfile_fd  = -1;

volatile int log_destiny=CONSOLE_WINDOW;

/* Netlist specific variables */
NETLIST *netlist_head=NULL;

/* Special objects with graphical=1 attribute */
NETLIST *graphical_netlist_head=NULL;
char    *guile_proc=NULL;

/* Command line arguments */
int list_backends=FALSE;
int verbose_mode=FALSE;
int interactive_mode=FALSE;
int quiet_mode=FALSE;

/* What kind of netlist are we generating? see define.h for #defs */
int netlist_mode=gEDA;

/* Scheme expression to evaluate before loading of rc files */
SCM pre_rc_list = SCM_EOL;

/* Scheme expression to evaluate before loading of the backend */
SCM pre_backend_list = SCM_EOL;

/* Scheme expression to evaluate after loading of the backend but
 * before the execution of the backend procedure */
SCM post_backend_list = SCM_EOL;

/* List of input filenames */
GSList *input_files;

/* Parameters passed to the backend from the command line */
GSList *backend_params = NULL;
