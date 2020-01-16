/* geda_mnemonic_hash.c: Sets of mnemonics with cycling
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 2002, Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 * with modifications, March 2016.
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif


#include <glib.h>
#include <geda/geda.h>
#include <gtk/gtk.h>

#include "../../include/geda_mnemonic_hash.h"
#include "../../include/geda_gtk_compat.h"

struct _GedaMnemonicHash
{
  GHashTable *hash;
};

/*!
 * \brief Create a New GedaMnemonicHash
 * \par Function Description
 * Returns a pointer to newly allocated  GedaMnemonicHash, which is nothing
 * more than a pointer to a GHashTable.
 */
GedaMnemonicHash *
geda_mnemonic_hash_new (void)
{
  GedaMnemonicHash *mnemonic_hash = g_malloc(sizeof(GedaMnemonicHash));

  mnemonic_hash->hash = g_hash_table_new (g_direct_hash, NULL);

  return mnemonic_hash;
}

/*!
 * \internal GedaMnemonicHash free foreach
 * \par Function Description
 *  Releases the string associated with the key.
 */
static void
mnemonic_hash_free_foreach (void *key, void *value, void *user)
{
  unsigned int keyval = (unsigned int)(long) (key);
  GSList *targets = value;

  char *name = gtk_accelerator_name (keyval, 0);

  g_free (name);

  g_slist_free (targets);
}

/*!
 * \brief Destory a GedaMnemonicHash
 * \par Function Description
 *  Releases all resources allocated to a GedaMnemonicHash.
 */
void
geda_mnemonic_hash_free (GedaMnemonicHash *mnemonic_hash)
{
  g_hash_table_foreach (mnemonic_hash->hash,
                        mnemonic_hash_free_foreach,
                        NULL);

  g_hash_table_destroy (mnemonic_hash->hash);
  g_free (mnemonic_hash);
}

/*!
 * \brief Add a Key to a GedaMnemonicHash
 * \par Function Description
 *  Appends GedaMnemonicHash with new keyval for target.
 */
void
geda_mnemonic_hash_add (GedaMnemonicHash *mnemonic_hash,
                        unsigned int      keyval,
                        GtkWidget        *target)
{
  void *key = (void*)(DWORD) (keyval);
  GSList *targets, *new_targets;

  g_return_if_fail (GTK_IS_WIDGET (target));

  targets = g_hash_table_lookup (mnemonic_hash->hash, key);

  g_return_if_fail (g_slist_find (targets, target) == NULL);

  new_targets = g_slist_append (targets, target);

  if (new_targets != targets) {
    g_hash_table_insert (mnemonic_hash->hash, key, new_targets);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_mnemonic_hash_remove (GedaMnemonicHash *mnemonic_hash,
                           unsigned int      keyval,
                           GtkWidget        *target)
{
  void *key = (void*)(DWORD) (keyval);
  GSList *targets, *new_targets;

  g_return_if_fail (GTK_IS_WIDGET (target));

  targets = g_hash_table_lookup (mnemonic_hash->hash, key);

  g_return_if_fail (targets && g_slist_find (targets, target) != NULL);

  new_targets = g_slist_remove (targets, target);

  if (new_targets != targets) {

    if (new_targets == NULL) {
      g_hash_table_remove (mnemonic_hash->hash, key);
    }
    else {
      g_hash_table_insert (mnemonic_hash->hash, key, new_targets);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool
geda_mnemonic_hash_activate (GedaMnemonicHash *mnemonic_hash,
                             unsigned int      keyval)
{
  GSList    *list,   *targets;
  GtkWidget *widget, *chosen_widget;
  bool overloaded;

  targets = g_hash_table_lookup (mnemonic_hash->hash,
                                 (void*)(DWORD) (keyval));
  if (!targets)
    return FALSE;

  overloaded    = FALSE;
  chosen_widget = NULL;

  for (list = targets; list; list = list->next) {

    widget = GTK_WIDGET (list->data);

    if (gtk_widget_is_sensitive (widget) &&
      gtk_widget_get_mapped (widget) &&
      widget->window &&
      gdk_window_is_viewable (widget->window))
    {
      if (chosen_widget) {

        overloaded = TRUE;
        break;
      }
      else
        chosen_widget = widget;
    }
  }

  if (chosen_widget) {

    /* For round robin we put the activated entry on
     * the end of the list after activation
     */
    targets = g_slist_remove (targets, chosen_widget);
    targets = g_slist_append (targets, chosen_widget);
    g_hash_table_insert (mnemonic_hash->hash,
                         (void*)(DWORD)keyval,
                         targets);

    return gtk_widget_mnemonic_activate (chosen_widget, overloaded);
  }
  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GSList *
geda_mnemonic_hash_lookup (GedaMnemonicHash *mnemonic_hash,
                           unsigned int      keyval)
{
  return g_hash_table_lookup (mnemonic_hash->hash, (void*)(DWORD)keyval);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
mnemonic_hash_foreach_func (void *key,
                            void *value,
                            void *data)
{
  struct {
    MnemonicHashFunc func;
    void *func_data;
  } *info = data;

  unsigned int keyval = (unsigned int)(long)key;
  GSList *targets = value;

  (*info->func) (keyval, targets, info->func_data);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_mnemonic_hash_foreach (GedaMnemonicHash *mnemonic_hash,
                            MnemonicHashFunc  func,
                            void             *func_data)
{
  struct {
    MnemonicHashFunc func;
    void *func_data;
  } info;

  info.func = func;
  info.func_data = func_data;

  g_hash_table_foreach(mnemonic_hash->hash, mnemonic_hash_foreach_func, &info);
}
