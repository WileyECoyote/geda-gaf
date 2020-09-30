/*!
 * \file globals.c
 *
 * \brief Global declarations in gschlas
 *
 * <hr>
 *
 * <h1><b>Copyright.</b></h1>\n
 * gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 *
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors (see ChangeLog for details)
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
 * along with this program; if  not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include "../../config.h"
#include <geda/geda.h>

/* command line arguments */
int verbose_mode=0;
int interactive_mode   = 0;
int quiet_mode         = 0;
int embed_mode         = 0;
int unembed_mode       = 0;

