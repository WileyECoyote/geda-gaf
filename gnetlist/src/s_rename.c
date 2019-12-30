/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_rename.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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
 *
 */
 /*
  * 2005/05/02  Almost totally reimplemented to support dynamic allocation.
  *
  * Changes are Copyright (C) 2005 Carlos A. R. Azevedo
  *
  * 2016/06/02 WEH: Add s_rename_compare_strings, s_rename_have_rename_record
  *            and hash to eliminate creation of redundant rename records.
  */
/*! \file s_rename.c
 *  \brief Module to Manage Renaming of Net Nodes
 */

#include "../../config.h"
#include <gnetlist.h>
#include <gettext.h>
#include <geda_debug.h>

/** \defgroup gnetlist-rename Netlister Rename Module
 * @{
 * \brief Gnetlist Rename Net Name
 * \par
 *  This module implements routines to create and maintain records for the
 *  renaming of net nodes as artifacts are interrogated by other modules.
 *  After the entire schematic has been traversed, this module applies the
 *  information in the records to perform the actual renaming.
 */

typedef struct {
    void *next;
    char *src;
    char *dest;
} RENAME;

typedef struct {
    void   *next_set;
    RENAME *first_rename;
    RENAME *last_rename;
} SET;

static void s_rename_all_lowlevel(NETLIST *netlist_head, char *src, char *dest);

static SET *first_set = NULL;
static SET *last_set = NULL;

static GHashTable *rename_table;

/*!
 * \brief Initialize Rename module
 * \par Function Description
 *  Checks that first_set has not been set and creates a new
 *  empty hash table for use later by the rename module.
 */
void s_rename_init(void)
{
  if (first_set) {
    fprintf(stderr,_("ERROR: Overwriting a valid rename list.\n"));
  }

  rename_table = g_hash_table_new(g_direct_hash, g_str_equal);
}

/*!
 * \brief Destroy all Rename Records and Release Resources
 * \par Function Description
 *  Destroys all rename records in first_set and then the rename hash table.
 */
void s_rename_destroy_all(void)
{
  while (first_set) {

    RENAME *temp;
    void   *to_free;

    for (temp = first_set->first_rename; temp;) {
      GEDA_FREE(temp->src);
      GEDA_FREE(temp->dest);
      to_free = temp;
      temp = temp->next;
      GEDA_FREE(to_free);
    }

    to_free = first_set;
    first_set = first_set->next_set;
    GEDA_FREE(to_free);
  }
  last_set = NULL;

  g_hash_table_destroy(rename_table);
  rename_table = NULL;
}

/*!
 * \brief Pre-allocate a RENAME record
 * \par Function Description
 *  Called by s_hierarchy_post_process to create a new RENAME
 *  record in anticipation of s_hierarchy_setup_rename calling
 *  s_rename_add. This function is only called once and only
 *  for hierarchical schematics; this function is not called
 *  when the schematic is flat.
 */
void s_rename_next_set(void)
{
  SET *new_set;

  new_set = GEDA_MEM_ALLOC(sizeof(SET));
  memset(new_set,0,sizeof(SET));

  if (first_set) {
    last_set->next_set = new_set;
    last_set = new_set;
  }
  else {
    first_set = last_set = new_set;
  }
}

/*!
 * \brief Print a Rename records when Verbose Enabled
 * \par Function Description
 *  Called by s_rename_all to print remapped net names when
 *  verbose is enabled.
 */
void s_rename_print(void)
{
  RENAME *temp_rename;
  SET    *temp_set;

  int i = 1;

  for (temp_set = first_set; temp_set;
       temp_set = temp_set->next_set) {
    for (temp_rename = temp_set->first_rename; temp_rename;
         temp_rename = temp_rename->next) {
      printf("%d) Source: _%s_", i++, temp_rename->src);
      printf(" -> Dest: _%s_\n",      temp_rename->dest);
    }
  }
}

/*!
 * \brief Search Rename Records
 * \par Function Description
 * if the src is found, return true
 * if the dest is found, also return true, but warn user
 * If quiet_flag is true than don't print anything
 */
int s_rename_search(char *src, char *dest, int quiet_flag)
{
  RENAME *temp;

  /* If last_set is unset, then there nothing to search, the answer is FALSE */
  if (last_set) {

    for (temp = last_set->first_rename; temp; temp = temp->next) {

      if (strcmp(src, temp->src) == 0) {
        return (TRUE);
      }

      if (strcmp(dest, temp->src) == 0) {

        if (!quiet_flag) {

          fprintf(stderr,_("WARNING: Trying to rename something twice:\n\t"
                           "%s and %s\nare both a src and dest name\n"),
                            dest, temp->src);
          fprintf(stderr,_("This warning is okay if there multiple levels of hierarchy!\n"));
        }
        return (TRUE);
      }
    }
  }
  return (FALSE);
}

/*!
 * \brief Compares target strings to previous targets
 * \par Function Description
 *  Checks if the src and dest string targets are equivilent to the strings
 *  of the from the previous rename. This function is used to avoid adding
 *  duplicate renames.
 *
 * \returns TRUE if strings are the same as the last rename or
 *          FALSE if either of the strings are different.
 */
static int s_rename_compare_strings (const RENAME *rename,
                                     const char   *src,
                                     const char   *dest)
{
  return (!strcmp(rename->src, src) && !strcmp(rename->dest, dest));
}

/*!
 * \brief Compares target strings to previous targets
 * \par Function Description
 *  Checks if src and dest string targets are in the rename_table.
 *
 * \returns TRUE if rename record exist, FALSE if the record does not exist.
 */
static int s_rename_have_rename_record (const char *src, const char *dest)
{
  char *ptr = g_hash_table_lookup (rename_table, src);

  if (ptr) {
    return (!strcmp(dest, ptr));
  }
  return FALSE;
}

/*!
 * \brief Add Rename Record Low Level
 * \par Function Description
 *  Checks for the existence of \a src and \a dest pairs, first in the
 *  previous record and then in the hash table using helper functions
 *  above, if a record of the pair does not currently exist then a new
 *  record is allocated and the pair added to the record and the list
 *  pointers updated.
 */
static void s_rename_add_lowlevel (const char *src, const char *dest)
{
  RENAME *new_rename;

  g_return_if_fail(last_set != NULL);

  /* Skip adding if strings are the same as previous strings */
  if (!s_rename_compare_strings(last_set->last_rename, src, dest)) {

    /* Check if pair already exist in table */
    if (!s_rename_have_rename_record(src, dest)) {

      new_rename = GEDA_MEM_ALLOC(sizeof(RENAME));

      g_return_if_fail(new_rename != NULL);

      new_rename->next = NULL;
      new_rename->src  = geda_utility_string_strdup(src);
      new_rename->dest = geda_utility_string_strdup(dest);

      if (last_set->first_rename == NULL) {
        last_set->first_rename = last_set->last_rename = new_rename;
      }
      else {
        last_set->last_rename->next = new_rename;
        last_set->last_rename = new_rename;
      }
      g_hash_table_insert (rename_table, (char*)src, (char*)dest);
    }
  }
}

/*!
 * \brief Add a Pair to the Rename stack
 * \par Function Description
 *  Wrapper for s_rename_add_lowlevel.
 */
void s_rename_add(char *src, char *dest)
{
  RENAME *temp;
  RENAME *new_rename;
  SET    *new_set;

  if (src == NULL || dest == NULL) {
    return;
  }

  if (s_rename_search(src, dest, quiet_mode)) {

    /* If found follow the original behavior, limiting the operation
     * to the current end-of-list */
    RENAME *last = last_set->last_rename;

    for (temp = last_set->first_rename; ; temp = temp->next) {

      if ((strcmp(dest, temp->src) == 0) && (strcmp(src, temp->dest) != 0))
      {
        /* we found a -> b, while adding c -> a.
         * hence we would have c -> a -> b, so add c -> b.
         * avoid renaming if b is the same as c!
         */
#if DEBUG
        printf( "Found destination [%s] in source [%s] with destination as: [%s]\n"
                 "So you want rename [%s] to [%s]\n",
                   dest, temp->src, temp->dest, src, temp->dest);
#endif
        s_rename_add_lowlevel(src, temp->dest);

      }
      else if ((strcmp(src, temp->src) == 0) &&
               (strcmp(dest, temp->dest) != 0))
      {
        /* we found a -> b, while adding a -> c.
         * hence b <==> c, so add c -> b.
         * avoid renaming if b is the same as c!
         */
#if DEBUG
        printf("Found source [%s] with destination as: [%s]\n"
                 "Unify nets by renaming [%s] to [%s]\n",
                  src, temp->dest, dest, temp->dest);
#endif
        s_rename_add_lowlevel(dest, temp->dest);
      }

      if (temp == last) {
        break;
      }
    }
  }
  else {

    /* Check for a valid set */
    if (first_set == NULL) {

      new_set = GEDA_MEM_ALLOC(sizeof(SET));
      memset(new_set,0,sizeof(SET));
      first_set = last_set = new_set;
    }

    new_rename = GEDA_MEM_ALLOC(sizeof(RENAME));
    new_rename->next = NULL;
    new_rename->src = geda_utility_string_strdup(src);
    new_rename->dest = geda_utility_string_strdup(dest);

    if (last_set->first_rename == NULL) {

      last_set->first_rename = last_set->last_rename = new_rename;
    }
    else {

      last_set->last_rename->next = new_rename;
      last_set->last_rename = new_rename;
    }
  }
}

/*!
 * \brief Rename Netnames Low Level
 * \par Function Description
 *  Searches each pin list in the netlist for \a src and when found,
 *  replaces the string with \a dest.
 *
 *  Example 1 of renamed netname:
 *
 *     src "GPIO_28" renamed to dest "GPIO-28"
 *
 *  Example 2 with (hierarchy-netname-mangle "enabled")
 *
 *     src "Sheets_18/DIM_SCLKA" renamed to dest "DIM_SCLKA"
 *
 * \todo consider revising to a two pass approach, with the first pass
 *       only detecting and collecting the pins and then renaming after
 *       all pins have been identified. The reason for this is that each
 *       pin net_name must be allocated separately so that the strings
 *       are not freed everywhere on the first encounter in the single
 *       pass approach. Previously the strings were not freed in this
 *       routine prior to re-assignment and the last reference to the
 *       pointers were lost after the routine so the memory could not
 *       be freed later, WEH.
 */
static void s_rename_all_lowlevel(NETLIST *netlist_head, char *src, char *dest)
{
  NETLIST  *nl_current = NULL;
  CPINLIST *pl_current;

  nl_current = netlist_head;

  while (nl_current != NULL) {

    if (nl_current->cpins) {

      pl_current = nl_current->cpins;

      while (pl_current != NULL) {

        if (pl_current->net_name != NULL) {

          if (strcmp(pl_current->net_name, src) == 0) {

            GEDA_FREE(pl_current->net_name);
            pl_current->net_name = geda_utility_string_strdup(dest);

          }
        }
        pl_current = pl_current->next;
      }
    }
    nl_current = nl_current->next;
  }
}

/*!
 * \brief Rename Nets
 * \par Function Description
 *  Wrapper for s_rename_all_lowlevel, iterates over the collection
 *  of netname pairs, passing each pair to the low level function
 *  in order to search the pin lists and perform the actual renaming.
 *
 * \note Called before and before exiting of s_hierarchy_post_process.
 */
void s_rename_all(GedaToplevel *pr_current, NETLIST *netlist_head)
{
  RENAME *temp;

  if (verbose_mode) {
    s_rename_print();
  }

  if (last_set) {

    for (temp = last_set->first_rename; temp; temp = temp->next) {
      verbose_print("R");
      s_rename_all_lowlevel(netlist_head, temp->src, temp->dest);
    }
  }
}

/*! \brief Get Top-Level Attributes
 *  \par Function Description
 *  \ingroup gnetlist-SCM-API
 *  When gnetlist expands a hierarchical subcircuit, gnetlist first assigns
 *  every net within the subcircuit a unique name based on the refdes of the
 *  subcircuit instance and, if present, the netname within the subcircuit.
 *  If a net is attached to the higher level circuit, gnetlist then changes
 *  the name of the subcircuit net to the name of the higher level net to
 *  which it is attached. This function returns a list of lists of pairs of
 *  names. The first name in a pair is the initial unique netname within the
 *  subcircuit, the second is the higher level netname it has acquired. The
 *  "level" argument is unused.
 *
 *  Alias gnetlist:get-renamed-nets
 *
 * \param [in] scm_level string unused parameter.
 *
 * \return list of lists of pairs of names
 */
SCM g_get_renamed_nets(SCM scm_level)
{
  SCM pairlist  = SCM_EOL;
  SCM outerlist = SCM_EOL;

  SET    *temp_set;

  for (temp_set = first_set; temp_set; temp_set = temp_set->next_set)
  {

    RENAME *temp_rename;

    for (temp_rename = temp_set->first_rename; temp_rename; temp_rename = temp_rename->next)
    {
      pairlist = scm_list_n (scm_from_utf8_string (temp_rename->src),
                             scm_from_utf8_string (temp_rename->dest),
                             SCM_UNDEFINED);
      outerlist = scm_cons (pairlist, outerlist);
    }
  }

  return (outerlist);
}

/** @} endgroup gnetlist-rename */
