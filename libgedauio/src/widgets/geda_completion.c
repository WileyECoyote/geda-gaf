/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_completion.h
 *
 * GLIB - Library of useful routines for C programming
 *
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
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

#include "config.h"

#include <geda.h>
#include <geda_completion.h>

#include <string.h>

#include "gettext.h"

#include <geda_debug.h>

/*!
 * \brief GedaCompletion - Automatic Entry Completion
 * \par
 * #GedaCompletion provides support for automatic completion of a string
 * using any group of target strings. It is typically used for file
 * name completion as is common in many UNIX shells.
 * \par
 * A #GedaCompletion is created using geda_completion_new(). Target items are
 * added and removed with geda_completion_add_items(),
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

static void completion_check_cache (GedaCompletion* cmp,
                                    char**          new_prefix);

/*! \brief Create a New Geda Completion Object
 *
 *  \par Function Description
 * This function returns a new #GedaCompletion object. If func
 * to be called to return the string representing an item in the
 * #GedaCompletion, or %NULL if strings are going to  be used as
 * the #GedaCompletion items.
 *
 * \param [in] func: A return handler
 *
 * Returns: the new #GedaCompletion.
 */
GedaCompletion*
geda_completion_new (GedaCompletionFunc func)
{
  GedaCompletion* gcomp;

  gcomp               = g_new (GedaCompletion, 1);
  gcomp->items        = NULL;
  gcomp->cache        = NULL;
  gcomp->prefix       = NULL;
  gcomp->func         = func;
  gcomp->strncmp_func = (GedaStrCompareNFunc) strncmp;

  return gcomp;
}

/*! \brief Geda Completion Add Items
 *  \par Function Description
 *
 * Adds items to the #GedaCompletion.
 *
 * \param [in] cmp:   A #GedaCompletion object.
 * \param [in] items: List of items to add.
 */
void
geda_completion_add_items (GedaCompletion* cmp, GList* items)
{
  GList* it;

  g_return_if_fail (cmp != NULL);

  /* optimize adding to cache? */
  if (cmp->cache)
    {
      g_list_free (cmp->cache);
      cmp->cache = NULL;
    }

  if (cmp->prefix)
    {
      g_free (cmp->prefix);
      cmp->prefix = NULL;
    }

  it = items;
  while (it)
    {
      cmp->items = g_list_prepend (cmp->items, it->data);
      it = it->next;
    }
}

/*! \brief Geda Completion Remove Items
 *  \par Function Description
 *
 * Removes items from a #GedaCompletion. The items are not freed, if the
 * memory was dynamically allocated, free items with u_glist_free_full()
 * after calling this function.
 *
 * \param [in] cmp:   A #GedaCompletion object.
 * \param [in] items: List of items to remove.
 *
 */
void
geda_completion_remove_items (GedaCompletion* cmp, GList* items)
{
  GList* it;

  g_return_if_fail (cmp != NULL);

  it = items;
  while (cmp->items && it)
    {
      cmp->items = g_list_remove (cmp->items, it->data);
      it = it->next;
    }

  it = items;
  while (cmp->cache && it)
    {
      cmp->cache = g_list_remove(cmp->cache, it->data);
      it = it->next;
    }
}

/*! \brief Geda Completion Clear Items
 *  \par Function Description
 *
 * Removes all items from the #GedaCompletion. The items are not freed,
 * so if the memory was dynamically allocated, it should be freed after
 * calling this function.
 *
 * \param [in] cmp A #GedaCompletion object.
 */
void
geda_completion_clear_items (GedaCompletion* cmp)
{
  g_return_if_fail (cmp != NULL);

  g_list_free (cmp->items);
  cmp->items = NULL;
  g_list_free (cmp->cache);
  cmp->cache = NULL;
  g_free (cmp->prefix);
  cmp->prefix = NULL;
}

static void
completion_check_cache (GedaCompletion* cmp, char** new_prefix)
{
  register GList* list;
  register gsize len;
  register gsize i;
  register gsize plen;
  char    *postfix;
  char    *s;

  if (!new_prefix)
    return;
  if (!cmp->cache)
    {
      *new_prefix = NULL;
      return;
    }

  len = strlen(cmp->prefix);
  list = cmp->cache;
  s = cmp->func ? cmp->func (list->data) : (char*) list->data;
  postfix = s + len;
  plen = strlen (postfix);
  list = list->next;

  while (list && plen)
    {
      s = cmp->func ? cmp->func (list->data) : (char*) list->data;
      s += len;
      for (i = 0; i < plen; ++i)
	{
	  if (postfix[i] != s[i])
	    break;
	}
      plen = i;
      list = list->next;
    }

  *new_prefix = g_new0 (char, len + plen + 1);
  strncpy (*new_prefix, cmp->prefix, len);
  strncpy (*new_prefix + len, postfix, plen);
}

/*! \brief Geda Completion Complete UTF8
 *  \par Function Description
 *
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
 * \param [in] cmp          A #GedaCompletion object.
 * \param [in] prefix       A the prefix string.
 * \param [in] new_prefix   A the new prefix string.
 *
 * Return value: List of items whose strings begin with prefix. This should
 *               not be changed.
 *
 * \sa geda_completion_complete
 *
 * \note This function should be used instead of geda_completion_complete()
 * if strings contains only UTF-8 characters.
 */
GList*
geda_completion_complete_utf8 (GedaCompletion  *cmp,
                               const char  *prefix,
                               char       **new_prefix)
{
  GList *list;
  char *p, *q;

  list = geda_completion_complete (cmp, prefix, new_prefix);

  if (new_prefix && *new_prefix)
    {
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

/*! \brief Geda Completion Complete
 *  \par Function Description
 *
 * Attempts to complete the string prefix using the #GedaCompletion
 * target items. The prefix string, typically typed by the user, which
 * is compared with each of the items. if new_prefix non-%NULL, returns
 * the longest prefix which is common to all items that matched prefix,
 * or %NULL if no items matched prefix. This string should be freed
 * when no longer needed.
 *
 * \param [in] cmp          A #GedaCompletion object.
 * \param [in] prefix       A the prefix string.
 * \param [in] new_prefix   A the new prefix string.
 *
 * Returns: the list of items whose strings begin with
 *          prefix. This should not be changed.
 *
 * \sa geda_completion_complete_utf8
 *
 */
GList*
geda_completion_complete (GedaCompletion *cmp, const char *prefix, char **new_prefix)
{
  bool   done = FALSE;
  gsize  plen, len;
  GList *list;

  g_return_val_if_fail (cmp != NULL, NULL);
  g_return_val_if_fail (prefix != NULL, NULL);

  len = strlen (prefix);

  if (cmp->prefix && cmp->cache) {

    plen = strlen (cmp->prefix);
    if (plen <= len && ! cmp->strncmp_func (prefix, cmp->prefix, plen)) {

      /* use the cache */
      list = cmp->cache;
      while (list) {

        GList *next = list->next;

        if (cmp->strncmp_func (prefix, cmp->func ? cmp->func (list->data) : (char*) list->data, len))
          cmp->cache = g_list_delete_link (cmp->cache, list);

        list = next;
      }
      done = TRUE;
    }
  }

  if (!done) {

    /* normal code */
    g_list_free (cmp->cache);
    cmp->cache = NULL;
    list = cmp->items;
    while (*prefix && list) {

      if (!cmp->strncmp_func (prefix,
        cmp->func ? cmp->func (list->data) : (char*) list->data,
                              len))
        cmp->cache = g_list_prepend (cmp->cache, list->data);
      list = list->next;
    }
  }

  if (cmp->prefix) {

    g_free (cmp->prefix);
    cmp->prefix = NULL;
  }

  if (cmp->cache)
    cmp->prefix = g_strdup (prefix);

  completion_check_cache (cmp, new_prefix);

  return *prefix ? cmp->cache : cmp->items;
}

/*! \brief Free a Geda Completion Object
 *  \par Function Description
 *
 * Frees all memory used by the #GedaCompletion. The items are not freed, so
 * if the memory was dynamically allocated, it should be freed after calling
 * this function.
 *
 * \param [in] cmp A #GedaCompletion object.
 */
void
geda_completion_free (GedaCompletion* cmp)
{
  g_return_if_fail (cmp != NULL);

  geda_completion_clear_items (cmp);
  g_free (cmp);
}

/*! \brief Geda Completion Object Set Compare Function
 *  \par Function Description
 *
 * Sets the function to use for string comparisons. The default string
 * comparison function is strncmp().
 *
 * \param [in] cmp          A #GedaCompletion object.
 * \param [in] strncmp_func Pointer to a string comparator function.
 *
 */
void
geda_completion_set_compare(GedaCompletion *cmp,
                            GedaStrCompareNFunc strncmp_func)
{
  cmp->strncmp_func = strncmp_func;
}

#ifdef TEST_COMPLETION
#include <stdio.h>
int
main (int   argc,
      char* argv[])
{
  FILE *file;
  char buf[1024];
  GList *list;
  GList *result;
  GList *tmp;
  GedaCompletion *cmp;
  int   i;
  char *longp = NULL;

  if (argc < 3)
  {
    g_warning ("Usage: %s filename prefix1 [prefix2 ...]\n", argv[0]);
    return 1;
  }

  file = fopen (argv[1], "r");
  if (!file)
  {
    g_warning ("Cannot open %s\n", argv[1]);
    return 1;
  }

  cmp = geda_completion_new (NULL);
  list = g_list_alloc ();
  while (fgets (buf, 1024, file))
  {
    list->data = g_strdup (buf);
    geda_completion_add_items (cmp, list);
  }
  fclose (file);

  for (i = 2; i < argc; ++i)
  {
    printf ("COMPLETING: %s\n", argv[i]);
    result = geda_completion_complete (cmp, argv[i], &longp);
    g_list_foreach (result, (GFunc) printf, NULL);
    printf ("LONG MATCH: %s\n", longp);
    g_free (longp);
    longp = NULL;
  }

  g_list_foreach (cmp->items, (GFunc) g_free, NULL);
  geda_completion_free (cmp);
  g_list_free (list);

  return 0;
}
#endif
/** @} end group GedaCompletion */