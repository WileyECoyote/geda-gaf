/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * file: globals.h
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2015 Ales Hvezda
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
 */

extern char   *rc_filename;

extern          int logfile_fd;
extern volatile int logging;
extern volatile int log_destiny;

/* gnetlist specific stuff */
extern NETLIST *netlist_head;
extern NETLIST *graphical_netlist_head; /* Special objects with
                                           graphical=1 attribute */
extern char   *guile_proc;
extern int     list_backends;
extern int     verbose_mode;
extern int     interactive_mode;
extern int     quiet_mode;
extern int     netlist_mode;
extern SCM     pre_rc_list;         /* before rc loaded */
extern SCM     pre_backend_list;    /* before backend loaded */
extern SCM     post_backend_list;   /* after backend loaded, before execute */
extern GSList *backend_params;  /* Parameters passed to the backend from the command line */
