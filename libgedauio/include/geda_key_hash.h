/*
 * File: geda_key_hash.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016-2018 gEDA Contributors (see ChangeLog for details)
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
 * Date: March 31, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */

#ifndef __GEDA_KEY_HASH_H__
#define __GEDA_KEY_HASH_H__

#include <gdk/gdk.h>

typedef struct _GedaKeyHash GedaKeyHash;

#ifdef __cplusplus
extern "C" {
#endif

GedaKeyHash *geda_key_hash_new           (GdkKeymap       *keymap,
                                          GDestroyNotify   item_destroy_notify);
void         geda_key_hash_add_entry     (GedaKeyHash      *key_hash,
                                          unsigned int     keyval,
                                          GdkModifierType  modifiers,
                                          void            *value);
void         geda_key_hash_remove_entry  (GedaKeyHash      *key_hash,
                                          void            *value);
GSList      *geda_key_hash_lookup        (GedaKeyHash      *key_hash,
                                          uint16           hardware_keycode,
                                          GdkModifierType  state,
                                          GdkModifierType  mask,
                                          int              group);
GSList      *geda_key_hash_lookup_keyval (GedaKeyHash      *key_hash,
                                          unsigned int     keyval,
                                          GdkModifierType  modifiers);
void         geda_key_hash_free          (GedaKeyHash      *key_hash);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_KEY_HASH_H__ */
