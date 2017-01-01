/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/*
 * File: geda_keyfile.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 Wiley Edward Hill
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: March, 06, 2016
 */

#ifndef __GEDA_KEYFILE_H__
#define __GEDA_KEYFILE_H__

/*! Domain for Errors originating in libgeda. */
#define GEDA_KEYFILE_ERROR geda_keyfile_error ()

/*!
 * GedaKeyFileError:
 * GEDA_KEYFILE_ERROR_UNKNOWN_ENCODING: the text being parsed was in
 *                                      an unknown encoding
 * GEDA_KEYFILE_ERROR_PARSE: document was ill-formed
 * GEDA_KEYFILE_ERROR_NOT_FOUND: the file was not found
 * GEDA_KEYFILE_ERROR_KEY_NOT_FOUND: a requested key was not found
 * GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND: a requested group was not found
 * GEDA_KEYFILE_ERROR_INVALID_VALUE: a value could not be parsed
 *
 * Error codes returned by key file parsing.
 */
typedef enum
{
  GEDA_KEYFILE_ERROR_UNKNOWN_ENCODING,
  GEDA_KEYFILE_ERROR_PARSE,
  GEDA_KEYFILE_ERROR_NOT_FOUND,
  GEDA_KEYFILE_ERROR_KEY_NOT_FOUND,
  GEDA_KEYFILE_ERROR_GROUP_NOT_FOUND,
  GEDA_KEYFILE_ERROR_INVALID_VALUE
} GedaKeyFileError;

/*!
 * GEDA_KEYFILE_ERROR:
 *
 * Error domain for key file parsing. Errors in this domain will
 * be from the #GedaKeyFileError enumeration.
 *
 * See GError for information on error domains.
 */
GQuark geda_keyfile_error (void);

typedef struct _GedaKeyFile GedaKeyFile;

/*!
 * GedaKeyFileFlags:
 * GEDA_KEYFILE_NONE: No flags, default behaviour
 * GEDA_KEYFILE_KEEP_COMMENTS: Use this flag if you plan to write the
 *     (possibly modified) contents of the key file back to a file;
 *     otherwise all comments will be lost when the key file is
 *     written back.
 * GEDA_KEYFILE_KEEP_TRANSLATIONS: Use this flag if you plan to write the
 *     (possibly modified) contents of the key file back to a file;
 *     otherwise only the translations for the current language will be
 *     written back.
 *
 * Flags which influence the parsing.
 */
typedef enum
{
  GEDA_KEYFILE_NONE              = 0,
  GEDA_KEYFILE_KEEP_COMMENTS     = 1 << 0,
  GEDA_KEYFILE_KEEP_TRANSLATIONS = 1 << 1
} GedaKeyFileFlags;

#ifdef __cplusplus
extern "C" {
#endif

GedaKeyFile *geda_keyfile_new                 (void);
GedaKeyFile *geda_keyfile_ref                 (GedaKeyFile        *key_file);
void      geda_keyfile_unref                  (GedaKeyFile        *key_file);
void      geda_keyfile_free                   (GedaKeyFile        *key_file);
void      geda_keyfile_set_list_separator     (GedaKeyFile        *key_file,
                                               char                separator);
bool      geda_keyfile_load_from_file         (GedaKeyFile        *key_file,
                                               const char         *file,
                                               GedaKeyFileFlags    flags,
                                               GError            **error);
bool      geda_keyfile_load_from_data         (GedaKeyFile        *key_file,
                                               const char         *data,
                                               size_t              length,
                                               GedaKeyFileFlags    flags,
                                               GError            **error);
bool      geda_keyfile_load_from_dirs         (GedaKeyFile        *key_file,
                                               const char         *file,
                                               const char        **search_dirs,
                                               char              **full_path,
                                               GedaKeyFileFlags    flags,
                                               GError            **error);
bool      geda_keyfile_load_from_data_dirs    (GedaKeyFile        *key_file,
                                               const char         *file,
                                               char              **full_path,
                                               GedaKeyFileFlags    flags,
                                               GError            **error);
char     *geda_keyfile_to_data                (GedaKeyFile        *key_file,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;

char     *geda_keyfile_get_start_group        (GedaKeyFile        *key_file) G_GNUC_MALLOC;
char    **geda_keyfile_get_groups             (GedaKeyFile        *key_file,
                                               size_t             *length) G_GNUC_MALLOC;

char    **geda_keyfile_get_keys               (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;

bool      geda_keyfile_has_group              (GedaKeyFile        *key_file,
                                               const char         *group_name);

bool      geda_keyfile_has_key                (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);

char     *geda_keyfile_get_value              (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error) G_GNUC_MALLOC;

void      geda_keyfile_set_value              (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *value);

char     *geda_keyfile_get_string             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error) G_GNUC_MALLOC;
void      geda_keyfile_set_string             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *string);

char     *geda_keyfile_get_locale_string      (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *locale,
                                               GError            **error) G_GNUC_MALLOC;
void      geda_keyfile_set_locale_string      (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *locale,
                                               const char         *string);
bool      geda_keyfile_get_boolean            (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);
void      geda_keyfile_set_boolean            (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               bool                value);
int       geda_keyfile_get_integer            (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);
void      geda_keyfile_set_integer            (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               int                 value);
int64     geda_keyfile_get_int64              (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);
void      geda_keyfile_set_int64              (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               int64               value);
uint64    geda_keyfile_get_uint64             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);
void      geda_keyfile_set_uint64             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               uint64              value);
double    geda_keyfile_get_double             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);
void      geda_keyfile_set_double             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               double              value);
char    **geda_keyfile_get_string_list        (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;
void      geda_keyfile_set_string_list        (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char * const  list[],
                                               size_t              length);
char    **geda_keyfile_get_locale_string_list (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *locale,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;
void      geda_keyfile_set_locale_string_list (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *locale,
                                               const char * const  list[],
                                               size_t              length);

bool     *geda_keyfile_get_boolean_list       (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;

void      geda_keyfile_set_boolean_list       (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               bool                list[],
                                               size_t              length);

int      *geda_keyfile_get_integer_list       (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;

void      geda_keyfile_set_double_list        (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               double              list[],
                                               size_t              length);
double   *geda_keyfile_get_double_list        (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               size_t             *length,
                                               GError            **error) G_GNUC_MALLOC;

void      geda_keyfile_set_integer_list       (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               int                 list[],
                                               size_t              length);

bool      geda_keyfile_set_comment            (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               const char         *comment,
                                               GError            **error);

char     *geda_keyfile_get_comment            (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error) G_GNUC_MALLOC;

bool      geda_keyfile_remove_comment         (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);

bool      geda_keyfile_remove_key             (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               const char         *key,
                                               GError            **error);

bool      geda_keyfile_remove_group           (GedaKeyFile        *key_file,
                                               const char         *group_name,
                                               GError            **error);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_KEYFILE_H__ */
