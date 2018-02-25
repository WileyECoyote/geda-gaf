/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_completion.h
 *
 * GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
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
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_COMPLETION_H__
#define __GEDA_COMPLETION_H__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _GedaCompletion GedaCompletion;

typedef char* (*GedaCompletionFunc) (void *);

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

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_COMPLETION_H__ */
