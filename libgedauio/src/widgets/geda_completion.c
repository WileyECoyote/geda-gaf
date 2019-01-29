/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * \file: geda_completion.c
 *
 * GLIB - Library of useful routines for C programming
 *
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 */

/*
 * MT safe
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_completion.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/*!
 * \brief GedaCompletion - Automatic Entry Completion
 * \par
 * #GedaCompletion provides support for automatic completion of a string
 * using any group of target strings and is typically used for file name
 * completion as is common in many UNIX shells.
 * \par
 * A #GedaCompletion is created using geda_completion_new(). Target items
 * are added and removed with geda_completion_add_items(),
 * geda_completion_remove_items() and geda_completion_clear_items(). A
 * completion attempt is requested with geda_completion_complete() or
 * geda_completion_complete_utf8(). When no longer needed, the
 * #GedaCompletion is freed with geda_completion_free().
 * \par
 * Items in the completion can be simple strings (e.g. filenames), or
 * pointers to arbitrary data structures. If data structures are used
 * you must provide a #GedaCompletionFunc in geda_completion_new(), which
 * retrieves the item's string from the data structure. You can change
 * the way in which strings are compared by setting a different
 * #GedaStrCompareNFunc in geda_completion_set_compare().
 *
 * \sa #GedaEntry
 *
 * \defgroup GedaCompletion  Automatic String Completion
 * @{
 */

/**
 * GedaCompletion:
 * items: list of target items (strings or data structures).
 * func: function which is called to get the string associated with a
 *       target item. It is %NULL if the target items are strings.
 * prefix: the last prefix passed to geda_completion_complete() or
 *         geda_completion_complete_utf8().
 * cache: the list of items which begin with prefix.
 * strncmp_func: The function to use when comparing strings.  Use
 *               geda_completion_set_compare() to modify this function.
 *
 * The data structure used for automatic completion.
 */

/**
 * GedaCompletionFunc:
 * Param1: the completion item.
 *
 * Specifies the type of the function passed to geda_completion_new(). It
 * should return the string corresponding to the given target item.
 * This is used when you use data structures as #GedaCompletion items.
 *
 * Returns: the string corresponding to the item.
 */

/**
 * GedaStrCompareNFunc:
 * s1: string to compare with s2.
 * s2: string to compare with s1.
 * n: maximal number of bytes to compare.
 *
 * Specifies the type of the function passed to
 * geda_completion_set_compare(). This is used when you use strings as
 * #GedaCompletion items.
 *
 * Returns: an integer less than, equal to, or greater than zero if
 *          the first n bytes of s1 is found, respectively, to be
 *          less than, to match, or to be greater than the first n
 *          bytes of s2.
 */

static void completion_check_cache (GedaCompletion  *comp,
                                    char           **new_prefix);

/*!
 * \brief Create a New Geda Completion Object
 * \par Function Description
 * This function returns a new #GedaCompletion object. If completion
 * items are strings the \a func should be %NULL, otherwise \a func
 * should be the pointer to a function that will return the string
 * representing an item in the #GedaCompletion.
 *
 * \param [in] func pointer to return handler or NULL
 *
 * \returns the new #GedaCompletion.
 */
GedaCompletion*
geda_completion_new (GedaCompletionFunc func)
{
  GedaCompletion *comp;

  comp               = g_new (GedaCompletion, 1);
  comp->items        = NULL;
  comp->cache        = NULL;
  comp->prefix       = NULL;
  comp->func         = func;
  comp->strncmp_func = (GedaStrCompareNFunc)strncmp;

  return comp;
}

/*!
 * \brief Geda Completion Add Items
 * \par Function Description
 *  Adds items to the #GedaCompletion.
 *
 * \param [in] comp  A #GedaCompletion object.
 * \param [in] items List of items to add.
 */
void
geda_completion_add_items (GedaCompletion *comp, GList *items)
{
  GList *iter;

  g_return_if_fail (comp != NULL);

  /* optimize adding to cache? */
  if (comp->cache) {

      g_list_free (comp->cache);
      comp->cache = NULL;
  }

  if (comp->prefix) {

      g_free (comp->prefix);
      comp->prefix = NULL;
  }

  iter = items;

  while (iter) {

      comp->items = g_list_prepend (comp->items, iter->data);
      iter = iter->next;
  }
}

/*!
 * \brief Geda Completion Remove Items
 * \par Function Description
 * Removes items from a #GedaCompletion. The items are not freed, if the
 * memory was dynamically allocated, free items with geda_gslist_free_full
 * after calling this function.
 *
 * \param [in] comp   A #GedaCompletion object.
 * \param [in] items  List of items to remove.
 */
void
geda_completion_remove_items (GedaCompletion *comp, GList *items)
{
  GList *iter;

  g_return_if_fail (comp != NULL);

  iter = items;

  while (comp->items && iter) {
      comp->items = g_list_remove (comp->items, iter->data);
      iter = iter->next;
  }

  iter = items;

  while (comp->cache && iter) {

      comp->cache = g_list_remove(comp->cache, iter->data);
      iter = iter->next;
  }
}

/*!
 * \brief Geda Completion Clear Items
 * \par Function Description
 * Removes all items from the #GedaCompletion. The items are not freed,
 * so if the memory was dynamically allocated, it should be freed after
 * calling this function.
 *
 * \param [in] comp  A #GedaCompletion object.
 */
void
geda_completion_clear_items (GedaCompletion *comp)
{
  g_return_if_fail (comp != NULL);

  g_list_free (comp->items);
  comp->items = NULL;

  g_list_free (comp->cache);
  comp->cache = NULL;

  g_free (comp->prefix);
  comp->prefix = NULL;
}

static void
completion_check_cache (GedaCompletion *comp, char **new_prefix)
{
  register GList *list;
  register size_t len;
  register size_t i;
  register size_t plen;
  char    *postfix;
  char    *s;

  if (!new_prefix) {
    return;
  }

  if (!comp->cache) {
   *new_prefix = NULL;
    return;
  }

  len     = strlen(comp->prefix);
  list    = comp->cache;
  s       = comp->func ? comp->func (list->data) : (char*) list->data;
  postfix = s + len;
  plen    = strlen (postfix);
  list    = list->next;

  while (list && plen) {

    s  = comp->func ? comp->func (list->data) : (char*) list->data;
    s += len;

    for (i = 0; i < plen; ++i) {

      if (postfix[i] != s[i]) {
        break;
      }
    }
    plen = i;
    list = list->next;
  }

 *new_prefix = GEDA_MEM_ALLOC0(sizeof(char) * len + plen + 1);
  strncpy (*new_prefix, comp->prefix, len);
  strncpy (*new_prefix + len, postfix, plen);
}

/*!
 * \brief Geda Completion Complete UTF8
 * \par Function Description
 * Attempts to complete the string prefix using the #GedaCompletion target items.
 * In contrast to geda_completion_complete(), this function returns the largest
 * common prefix that is a valid UTF-8 string, omitting a possible common partial
 * character. The prefix string, typically used by the user, which is compared
 * with each of the items. If new_prefix: if non-%NULL, returns the longest prefix
 * which is common to all items that matched prefix, or %NULL if no items matched
 * prefix.
 *
 * \remark This string should be freed when no longer needed.
 *
 * \param [in] comp        A #GedaCompletion object.
 * \param [in] prefix      the prefix string.
 * \param [in] new_prefix  the new prefix string.
 *
 * \returns List of items whose strings begin with prefix. This should list
 *          should not be changed.
 *
 * \sa geda_completion_complete
 *
 * \note This function should be used instead of geda_completion_complete()
 *       if strings contains only UTF-8 characters.
 */
GList*
geda_completion_complete_utf8 (GedaCompletion  *comp,
                               const char      *prefix,
                               char           **new_prefix)
{
  GList *list;

  list = geda_completion_complete (comp, prefix, new_prefix);

  if (new_prefix && *new_prefix) {

    char *p, *q;

    p = *new_prefix + strlen (*new_prefix);
    q = g_utf8_find_prev_char (*new_prefix, p);

    switch (g_utf8_get_char_validated (q, p - q))
    {
      case (gunichar)-2:
      case (gunichar)-1:
        *q = 0;
        break;
      default: ;
    }

  }

  return list;
}

/*!
 * \brief Geda Completion Complete
 * \par Function Description
 * Attempts to complete the string prefix using the #GedaCompletion
 * target items. The prefix string, typically typed by the user, which
 * is compared with each of the items. If new_prefix non-%NULL, returns
 * the longest prefix which is common to all items that matched prefix,
 * or %NULL if no items matched prefix. This string should be freed
 * when no longer needed.
 *
 * \param [in] comp        A #GedaCompletion object.
 * \param [in] prefix      the prefix string.
 * \param [in] new_prefix  the new prefix string.
 *
 * \returns list of items whose strings begin with prefix. This should
 *          not be changed.
 *
 * \sa geda_completion_complete_utf8
 */
GList*
geda_completion_complete (GedaCompletion *comp, const char *prefix, char **new_prefix)
{
  bool   done = FALSE;
  size_t len;
  GList *list;

  g_return_val_if_fail (comp != NULL, NULL);
  g_return_val_if_fail (prefix != NULL, NULL);

  len = strlen (prefix);

  if (comp->prefix && comp->cache) {

    size_t plen = strlen (comp->prefix);

    if (plen <= len && ! comp->strncmp_func (prefix, comp->prefix, plen))
    {
      /* use the cache */
      list = comp->cache;

      while (list) {

        GList *next = list->next;
        char  *str2;

        str2 = comp->func ? comp->func (list->data) : (char*)list->data;

        if (comp->strncmp_func (prefix, str2, len)) {
          comp->cache = g_list_delete_link (comp->cache, list);
        }

        list = next;
      }
      done = TRUE;
    }
  }

  if (!done) {

    /* normal code */
    g_list_free (comp->cache);

    comp->cache = NULL;
    list = comp->items;

    while (*prefix && list) {

      char  *str2;

      str2 = comp->func ? comp->func (list->data) : (char*)list->data;

      if (!comp->strncmp_func (prefix, str2, len)) {
        comp->cache = g_list_prepend (comp->cache, list->data);
      }

      list = list->next;
    }
  }

  if (comp->prefix) {

    g_free (comp->prefix);
    comp->prefix = NULL;
  }

  if (comp->cache)
    comp->prefix = geda_strdup (prefix);

  completion_check_cache (comp, new_prefix);

  return *prefix ? comp->cache : comp->items;
}

/*!
 * \brief Free a Geda Completion Object
 * \par Function Description
 *  Frees all memory used by the #GedaCompletion. The items are not freed,
 *  dynamically allocated memory for the items should be freed after calling
 *  this function.
 *
 * \param [in] comp  A #GedaCompletion object.
 */
void
geda_completion_free (GedaCompletion* comp)
{
  g_return_if_fail (comp != NULL);

  geda_completion_clear_items (comp);
  g_free (comp);
}

/*!
 * \brief Geda Completion Object Set Compare Function
 * \par Function Description
 *  Sets the function to use for string comparisons. The default string
 *  comparison function is strncmp().
 *
 * \param [in] comp         A #GedaCompletion object.
 * \param [in] strncmp_func Pointer to a string comparator function.
 */
void
geda_completion_set_compare(GedaCompletion *comp,
                            GedaStrCompareNFunc strncmp_func)
{
  comp->strncmp_func = strncmp_func;
}

#ifdef TEST_COMPLETION

#include <stdio.h>
int
main (int argc, char *argv[])
{
  GedaCompletion *comp;
  FILE           *file;
  GList          *list;
  char           *longp = NULL;
  int i;

  char buf[1024];

  if (argc < 3) {
    fprintf(stderr, "Usage: %s filename prefix1 [prefix2 ...]\n", argv[0]);
    return 1;
  }

  file = fopen (argv[1], "r");

  if (!file) {

    fprintf(stderr, "Cannot open %s\n", argv[1]);
    return 1;

  }

  comp = geda_completion_new (NULL);
  list = g_list_alloc ();

  while (fgets (buf, 1024, file)) {

    list->data = geda_strdup (buf);
    geda_completion_add_items (comp, list);

  }
  fclose (file);

  for (i = 2; i < argc; ++i) {

    GList *result;

    printf ("COMPLETING: %s\n", argv[i]);
    result = geda_completion_complete (comp, argv[i], &longp);
    g_list_foreach (result, (GFunc) printf, NULL);
    printf ("LONG MATCH: %s\n", longp);
    g_free (longp);
    longp = NULL;

  }

  g_list_foreach (comp->items, (GFunc) g_free, NULL);
  geda_completion_free (comp);
  g_list_free (list);

  return 0;
}

#endif

/** @} end group GedaCompletion */
