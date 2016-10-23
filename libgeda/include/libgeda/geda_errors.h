/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_errors.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 2013-2015  gEDA Contributors (see ChangeLog for details)
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

/*! Domain for GErrors originating in libgeda. */
#define EDA_ERROR eda_error_quark ()

/*! Error numbers for errors originating in libgeda. */
typedef enum {
  EDA_ERROR_SCHEME,           /* A Scheme error occurred */
  EDA_ERROR_RC_TWICE,         /* Attempted to read a configuration file twice */
  EDA_ERROR_PARSE,            /* Schematic data could not be parsed. */
  EDA_ERROR_NOLIB,            /* A requested library resource was missing. */
  EDA_ERROR_LOOP,             /* The data model contains a circular dependency. */
  EDA_ERROR_UNKNOWN_ENCODING, /* Schematic data was not UTF-8-encoded. */
  EDA_ERROR_NUM_ERRORS,
  EDA_ERROR_NULL_POINTER,
  EDA_ERROR_INVALID_PAGE
} EdaError;

#ifdef __cplusplus
extern "C" {
#endif

GQuark eda_error_quark (void);

void   geda_error_object_argument(const char *file,
                                  const char *func,
                                  const void *object,
                                  const char  type);

#ifdef __cplusplus
}
#endif /* __cplusplus */
