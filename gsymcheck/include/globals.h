/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: globals.h
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

extern char *rc_filename;

extern int logfile_fd;
extern volatile int logging;
extern volatile int log_destiny;

/* gnetlist specific stuff */
extern int verbose_mode;
extern int interactive_mode;
extern int quiet_mode;


