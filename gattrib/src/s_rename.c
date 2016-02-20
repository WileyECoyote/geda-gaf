/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*!
 * \file
 * \brief Functions to rename STRING_LIST contents
 *
 * Functions to rename STRING_LIST contents
 */

#include <gattrib.h>
#include <geda_debug.h>

typedef struct {
    char *src;
    char *dest;
} RENAME;

#define MAX_RENAME 64
#define MAX_SETS 10

/*! size is fixed...
 * \todo maybe make this dynamic */
static RENAME rename_pairs[MAX_SETS][MAX_RENAME];

static int rename_counter = 0;
static int cur_set = 0;


/*! \brief Initialize the renaming data space
 *
 * Initializee the renaming data space by setting all the pair
 * pointers to NULL.
 */
void s_rename_init(void)
{
    int i, j;

    for (i = 0; i < MAX_SETS; i++) {

      for (j = 0; j < MAX_RENAME; j++) {
        rename_pairs[i][j].src = NULL;
        rename_pairs[i][j].dest = NULL;
      }
    }
    rename_counter = 0;
    cur_set = 0;
}

/*! \brief Free all data referred to by the rename pairs
 *
 * Loops rename pairs and calls GEDA_FREE() on the non-NULL
 * entries, then sets the entry to NULL.
 */
void s_rename_destroy_all(void)
{
  int i, j;

  for (i = 0; i < MAX_SETS; i++) {

    for (j = 0; j < MAX_RENAME; j++) {

      if (rename_pairs[i][j].src) {
        g_free(rename_pairs[i][j].src);
        rename_pairs[i][j].src = NULL;
      }

      if (rename_pairs[i][j].dest) {
        g_free(rename_pairs[i][j].dest);
        rename_pairs[i][j].dest = NULL;
      }
    }
  }
  rename_counter = 0;
  cur_set = 0;
}

void s_rename_next_set(void)
{
  if (cur_set == MAX_SETS) {
    fprintf(stderr,
            _("Increase number of rename_pair sets in s_net.c\n"));
            return;
  }
  cur_set++;
  rename_counter = 0;
}

/*! \brief Print all rename sets
 *
 * Iterate through the array and print all the rename sets to stdout.
 */
void s_rename_print(void)
{
    int i, j;

    for (i = 0; i < MAX_SETS; i++) {
      for (j = 0; j < MAX_RENAME; j++) {
        if (rename_pairs[i][j].src) {
          printf(_("%d) Source: _%s_"), i, rename_pairs[i][j].src);
        }

        if (rename_pairs[i][j].dest) {
          printf(_(" -> Dest: _%s_\n"), rename_pairs[i][j].dest);
        }
      }
    }
}

/*! \brief Search the rename sets
 *
 * Search through the rename sets looking for src and dest.  If
 * quiet_flag is true than don't print anything.
 * \param src Source to search for
 * \param dest Destination to search for
 * \param quiet_flag Suppress printing if set to TRUE
 * \returns TRUE if the
 * src is found. If the dest is found, also return true, but warn
 * user
 */
int s_rename_search(char *src, char *dest, int quiet_flag)
{
    int i;
    for (i = 0; i < rename_counter; i++) {

	if (rename_pairs[cur_set][i].src && rename_pairs[cur_set][i].dest) {

	    if (strcmp(src, rename_pairs[cur_set][i].src) == 0) {
		return (TRUE);
	    }

	    if (strcmp(dest, rename_pairs[cur_set][i].src) == 0) {
		if (!quiet_flag) {
		    fprintf(stderr,
			    _("WARNING: Trying to rename something twice:\n\t%s and %s\nare both a src and dest name\n"
                             "This warning is okay if you have multiple levels of hierarchy!\n"),
                            dest, rename_pairs[cur_set][i].src);
		}
		return (TRUE);
	    }
	}

    }

    return (FALSE);
}

/*! \brief Add to the rename pairs
 *
 * Add a source and destination to the rename pairs.
 * \param src Source to add
 * \param dest Destination to add
 */

void s_rename_add(char *src, char *dest)
{
  int flag;

  if (src == NULL || dest == NULL) {
    return;
  }

  flag = s_rename_search(src, dest, FALSE);

  if (flag) {

    int i, orig_rename_counter;

    /* Rename_counter may be incremented within this loop, so it cannot
     * be used in the loop exit condition.  Just iterate over the number
     * of renames that were in the list at the start of the loop. */
    orig_rename_counter = rename_counter;

    for (i = 0; i < orig_rename_counter; i++) {
      if (rename_pairs[cur_set][i].src
        && rename_pairs[cur_set][i].dest)
      {
        if (strcmp(dest, rename_pairs[cur_set][i].src) == 0) {
#if DEBUG
          printf
          ("Found dest [%s] in src [%s] and that had a dest as: [%s]\nSo you want rename [%s] to [%s]\n",
           dest, rename_pairs[cur_set][i].src,
           rename_pairs[cur_set][i].dest,
           src, rename_pairs[cur_set][i].dest);
#endif

          rename_pairs[cur_set][rename_counter].src = geda_utility_string_strdup(src);
          rename_pairs[cur_set][rename_counter].dest =
          geda_utility_string_strdup(rename_pairs[cur_set][i].dest);
          rename_counter++;
        }
      }
    }
  }
  else {

    rename_pairs[cur_set][rename_counter].src = geda_utility_string_strdup(src);
    rename_pairs[cur_set][rename_counter].dest = geda_utility_string_strdup(dest);
    rename_counter++;
  }
  if (rename_counter == MAX_RENAME) {
    fprintf(stderr,
            _("Increase number of rename_pairs (MAX_RENAME) in s_rename.c\n"));
    return;
  }

}



void s_rename_all_lowlevel(NETLIST * netlist_head, char *src, char *dest)
{
  NETLIST *nl_current = NULL;
  CPINLIST *pl_current;

  nl_current = netlist_head;

  while (nl_current != NULL) {
    if (nl_current->cpins) {
      pl_current = nl_current->cpins;
      while (pl_current != NULL) {

        if (pl_current->net_name != NULL) {

          if (strcmp(pl_current->net_name, src) == 0) {

            /* this is a bad idea */
            /* because inside nets-> */
            /* there is another pointer */
            /*GEDA_FREE(pl_current->net_name); */

            pl_current->net_name = geda_utility_string_strdup(dest);
          }
        }

        pl_current = pl_current->next;
      }
    }
    nl_current = nl_current->next;
  }

}

void s_rename_all (GedaToplevel *toplevel, NETLIST * netlist_head)
{
    int i;

#if DEBUG
    s_rename_print();
#endif

    for (i = 0; i < rename_counter; i++) {

	verbose_print("R");

#if DEBUG
	printf("%d Renaming: %s -> %s\n", i, rename_pairs[cur_set][i].src,
	       rename_pairs[cur_set][i].dest);
#endif

	s_rename_all_lowlevel(netlist_head,
			      rename_pairs[cur_set][i].src,
			      rename_pairs[cur_set][i].dest);
    }
}

