/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

#include "../../../config.h"
#include <stdio.h>

#include <libgeda_priv.h>

/*! \brief */
struct st_menu {
  char *menu_name;
  SCM menu_items;
};

static int menu_index=0;

#define MAX_MENUS 16

/* and eventually make this unlimited, is a hack */
static struct st_menu menu[MAX_MENUS];

/*! \brief Add Menu Record Entry
 *  \par Function Description
 *  This function checks the static menu array and appends
 *  menu_items to existing records or adds a new record with
 *  for the menu_items if a not found.
 */
int geda_iface_menu_add_entry(char *menu_name, SCM menu_items)
{
  int index;

  if (menu_name == NULL) {
    index = -1;
  }
  else if (menu_index >= MAX_MENUS) {
    index = -1;
  }
  else {

    int found = FALSE;

    for (index = 0; index < menu_index; index++) {

      /* Check if this menu item already exist */
      if (strcmp(menu[index].menu_name, menu_name) == 0) {

        SCM new_list;
        SCM old_list;

        old_list = menu[index].menu_items;

        scm_gc_unprotect_object (old_list);

        new_list = scm_append_x(scm_list_2(old_list, menu_items));

        menu[index].menu_items = scm_gc_protect_object (new_list);

        found = TRUE;
        break;
      }
    }

    if (!found) { /* If item did not exist then add */

      menu[menu_index].menu_name = geda_utility_string_strdup (menu_name);

      menu[menu_index].menu_items = scm_gc_protect_object (menu_items);

      index = ++menu_index;
    }
  }

  return index;
}

/*! \brief Free All Menu Records
 *  \par Function Description
 *   The function index through the Menu array data
 *   structure and frees memory for the strings and call
 *   scm_gc_unprotect_object to unprotect the guile objects.
 */
void geda_iface_menu_free(void)
{
  int i;

  for (i = 0; i < menu_index; i++) {
    if (menu[i].menu_name) {
      GEDA_FREE(menu[i].menu_name);
      scm_gc_unprotect_object (menu[i].menu_items);
    }
  }

  menu_index=0;
}

/*!
 * \brief Initialize menu interface module
 * \par Function Description
 * Set all pointer to menu names in menu[] to NULL.
 */
void geda_iface_menu_init(void)
{
  int i;
  for (i = 0; i < MAX_MENUS; i++) {
    menu[i].menu_name = NULL;
  }
}

/*!
 * \brief Print All Menu Records
 * \par Function Description
 *  Utility function used for debugging menu routines.
 */
void geda_iface_menu_print(void)
{
  int i;

  for (i = 0; i < menu_index; i++) {
    printf("Name; %s\n", menu[i].menu_name);
    scm_display (menu[i].menu_items, scm_current_output_port ());
    printf("\n");
  }
}

/*!
 * \brief Retrieve Top Level Menu and Submenu Items given an Index
 * \par Function Description
 *  Sets menu_name to a pointer to the raw menu name for the
 *  toplevel menu container at the given \a index. The sub menu
 *  items associated with toplevel menu container is returned
 *  on the stack in Scheme form.
 */
SCM geda_iface_menu_return_entry(int index, char **menu_name)
{
  if (menu_name == NULL) {
    return SCM_BOOL_F;
  }

  if (index >= MAX_MENUS || index < 0) {
    *menu_name = NULL;
    return SCM_BOOL_F;
  }

  *menu_name = menu[index].menu_name;

  return(menu[index].menu_items);
}

/*!
 * \brief Retrieve the Number of Top Level Menus
 * \par Function Description
 * Returns the number of top-level menu containers.
 */
int geda_iface_menu_return_num(void)
{
  return(menu_index);
}

char *geda_iface_menu_return_icon(const char *action_name)
{
  char *icon = NULL;

  if (action_name) {

    unsigned int menu_count = geda_iface_menu_return_num();
    unsigned int i;

    /* Loop through all top-level menu container */
    for (i = 0 ; i < menu_count; i++) {

      unsigned int scm_items_len;
      unsigned int j;

      SCM scm_items;

      scm_items = menu[i].menu_items;

      /* Loop through all items subordinate to the top-level menu container */
      scm_items_len = (int) scm_ilength (scm_items);

      for (j = 0 ; j < scm_items_len; j++) {

        SCM scm_index     = scm_from_int (j);
        SCM scm_item      = scm_list_ref (scm_items, scm_index);
        SCM scm_item_func = SCM_CADR (scm_item);

        if (scm_is_true (scm_item_func)) {

          char *action;

          action = scm_to_utf8_string (scm_symbol_to_string (scm_item_func));

          if (strcmp(action, action_name) == 0) {

           SCM scm_item_stock;

           scm_item_stock = scm_is_pair  (SCM_CDDR (scm_item)) ? SCM_CADDR (scm_item) : SCM_BOOL_F;

            if (!scm_is_false (scm_item_stock)) {

              /* Extract icon string */
              icon = scm_to_utf8_string (scm_item_stock);
            }

            break;
          }

          free(action);
        }
      }

      if (icon) {
        break;
      }
    }
  }

  return icon;
}

char *geda_iface_menu_return_tooltip(const char *action_name)
{
  char *tooltip = NULL;

  if (action_name) {

    unsigned int menu_count = geda_iface_menu_return_num();
    unsigned int i;

    /* Loop through all top-level menu container */
    for (i = 0 ; i < menu_count; i++) {

      unsigned int scm_items_len;
      unsigned int j;

      SCM scm_items;

      scm_items = menu[i].menu_items;

      /* Loop through all items subordinate to the top-level menu container */
      scm_items_len = (int) scm_ilength (scm_items);

      for (j = 0 ; j < scm_items_len; j++) {

        SCM scm_index    = scm_from_int (j);

        SCM scm_item     = scm_list_ref (scm_items, scm_index);

        int scm_item_len = scm_ilength  (scm_item);

        /* Check for a 4th parameter = tooltip string */
        if (scm_item_len == 4) {

          SCM scm_item_func  = SCM_CADR (scm_item);

          if (scm_is_true (scm_item_func)) {

            char *action;

            action = scm_to_utf8_string (scm_symbol_to_string (scm_item_func));

            if (strcmp(action, action_name) == 0) {

              /* Extract tooltip string */
              SCM scm_item_tip = SCM_CAR (scm_cdddr (scm_item));

              /* Validate tip is really a string */
              if (scm_is_string(scm_item_tip)) {

                /* if valid, convert to c string */
                tooltip = scm_to_utf8_string (scm_item_tip);
                break;
              }
            }

            free(action);
          }
        }
      }

      if (tooltip) {
        break;
      }
    }
  }

  return tooltip;
}
