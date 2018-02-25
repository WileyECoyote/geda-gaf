/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_threads.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2018 Wiley Edward Hill
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
 * Date: November 24, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
#ifndef __GEDA_THREADS_H__
#define __GEDA_THREADS_H__

#include <glib.h>

void          geda_main_context_invoke           (GMainContext   *context,
                                                  GSourceFunc     function,
                                                  void           *data);
void          geda_main_context_invoke_full      (GMainContext   *context,
                                                  int             priority,
                                                  GSourceFunc     function,
                                                  void           *data,
                                                  GDestroyNotify  notify);

#endif /* __GEDA_THREADS_H__ */