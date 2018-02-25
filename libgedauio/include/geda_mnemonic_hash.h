/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_mnemonic_hash.h
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
 */

#ifndef __GEDA_MNEMONIC_HASH_H__
#define __GEDA_MNEMONIC_HASH_H__

typedef struct _GedaMnemonicHash GedaMnemonicHash;

typedef void (*MnemonicHashFunc) (unsigned int keyval, GSList *targets, void *data);

#ifdef __cplusplus
extern "C" {
#endif

GedaMnemonicHash *geda_mnemonic_hash_new      (void);
void              geda_mnemonic_hash_free     (GedaMnemonicHash *mnemonic_hash);
void              geda_mnemonic_hash_add      (GedaMnemonicHash *mnemonic_hash,
                                               unsigned int      keyval,
                                               GtkWidget        *target);
void              geda_mnemonic_hash_remove   (GedaMnemonicHash *mnemonic_hash,
                                               unsigned int      keyval,
                                               GtkWidget        *target);
bool              geda_mnemonic_hash_activate (GedaMnemonicHash *mnemonic_hash,
                                               unsigned int      keyval);
GSList           *geda_mnemonic_hash_lookup   (GedaMnemonicHash *mnemonic_hash,
                                               unsigned int      keyval);
void              geda_mnemonic_hash_foreach  (GedaMnemonicHash *mnemonic_hash,
                                               MnemonicHashFunc  func,
                                               void             *func_data);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MNEMONIC_HASH_H__ */
