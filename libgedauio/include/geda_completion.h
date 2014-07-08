/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

#ifndef __GEDA_COMPLETION_H__
#define __GEDA_COMPLETION_H__

#include <glib.h>

G_BEGIN_DECLS

typedef struct _GedaCompletion     GedaCompletion;

typedef char* (*GedaCompletionFunc)      (gpointer);

/* GedaCompletion */

typedef int (*GedaStrCompareNFunc) (const char   *s1,
                                          const char   *s2,
                                          unsigned long n);

struct _GedaCompletion
{
  GList *items;
  GedaCompletionFunc func;
 
  char  *prefix;
  GList *cache;
  GedaStrCompareNFunc strncmp_func;
};

GedaCompletion *geda_completion_new        (GedaCompletionFunc func);

void         geda_completion_add_items     (GedaCompletion *cmp,
                                            GList          *items);

void         geda_completion_remove_items  (GedaCompletion *cmp,
                                            GList          *items);

void         geda_completion_clear_items   (GedaCompletion *cmp);

GList       *geda_completion_complete      (GedaCompletion *cmp,
                                            const char     *prefix,
                                            char          **new_prefix);

GList       *geda_completion_complete_utf8 (GedaCompletion *cmp,
                                            const char*     prefix,
                                            char**          new_prefix);

void         geda_completion_set_compare   (GedaCompletion *cmp,
                                            GedaStrCompareNFunc strncmp_func);

void         geda_completion_free          (GedaCompletion *cmp);

G_END_DECLS

#endif /* __GEDA_COMPLETION_H__ */
