/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: s_symstruct.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#include <config.h>

#include "../include/gsymcheck.h"

/*! \brief Create and initialize a new SYMCHECK structure
 *  \par Function Description
 *  This is called for every symbol that needs to be checked
 *
 *  \returns A new initialized SYMCHECK structure
 */
SYMCHECK *s_symstruct_init(void)
{
  SYMCHECK *s_symcheck;

  s_symcheck = (SYMCHECK*) GEDA_MEM_ALLOC(sizeof(SYMCHECK));

  i_vars_set_valid_attributes (memset(s_symcheck, 0, sizeof(SYMCHECK)));
  i_vars_set_known_devices (s_symcheck);

  s_symcheck->numslots = -1;

  return s_symcheck;
}

/*! \brief Print results stored in SYMCHECK structure
 *  \par Function Description
 *  This is called after routines in the s_check module have
 *  interogated the symbol data in order to print the results.
 *  This routine is call for each symbol that is checked.
 *
 *  \param [in] s_current Pointer to SYMCHECK containing the results
 */
void s_symstruct_print(SYMCHECK *s_current)
{
  GList *list;
  char  *msg;

  if (verbose_mode > 2) {

    list = s_current->info_messages;

    while (list != NULL) {

      msg = (char *) list->data;

      /* printf("found info: %s\n", msg); */
      if (msg) {
         geda_log("%s: %s", _("Information"), msg);
      }

      NEXT(list);
    }
  }

  if (verbose_mode > 1) {

    list = s_current->warning_messages;

    while (list != NULL) {

      msg = (char *) list->data;

      /* printf("found warning: %s\n", msg); */
      if (msg) {
         geda_log("%s: %s", _("Warning"), msg);
      }

      NEXT(list);
    }
  }

  if (verbose_mode > 0) {

    list = s_current->error_messages;

    while (list != NULL) {

      msg = (char *) list->data;

      /* printf("found error: %s\n", msg); */
      if (msg && verbose_mode) {
         geda_log("%s: %s", _("Error"), msg);
      }

      NEXT(list);
    }
  }
}

/*! \brief Release resources in SYMCHECK structure and reset to zero
 *  \par Function Description
 *  Called after each symbol is checked and the results reported to
 *  release memory allocated for string message and list within
 *  #SYMCHECK structure \a s_current and zero-out the memory.
 *
 *  \param [in] s_current Pointer to SYMCHECK structure to reset
 */
void s_symstruct_reset(SYMCHECK *s_current)
{
  if (s_current) {

    GList *list;

    GEDA_FREE(s_current->description_attribute);
    GEDA_FREE(s_current->device_attribute);

    /* release memory allocated for message strings */
    list = s_current->info_messages;

    while (list != NULL) {
      GEDA_FREE(list->data);
      NEXT(list);
    }

    list = s_current->warning_messages;

    while (list != NULL) {
      GEDA_FREE(list->data);
      NEXT(list);
    }

    list = s_current->error_messages;

    while (list != NULL) {
      GEDA_FREE(list->data);
      NEXT(list);
    }

    /* free the List */
    g_list_free(s_current->info_messages);
    g_list_free(s_current->warning_messages);
    g_list_free(s_current->error_messages);

    memset(s_current, 0, sizeof(SYMCHECK));

    i_vars_set_known_devices (s_current);
    i_vars_set_valid_attributes(s_current);

    s_current->numslots = -1;
  }
}

/*! \brief Release allocation for SYMCHECK structure
 *  \par Function Description
 *  Called to release memory allocated for the structure.
 *
 *  \param [in] s_current Pointer to SYMCHECK free
 */
void s_symstruct_free(SYMCHECK *s_current)
{
  if (s_current) {
    GEDA_FREE(s_current);
  }
}
