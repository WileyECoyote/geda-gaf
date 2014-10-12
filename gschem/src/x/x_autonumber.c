/* -*- C x_autonumber.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 * MA 02110-1301 USA
 */
/*!
 * \file x_autonumber.c
 * \brief A dialog box for Automatically Text Numbering
 */

#include <config.h>

#include <ctype.h>

#include "gschem.h"
#include <gdk/gdkkeysyms.h>

#include "x_dialog.h"
#include "geda_dialog_controls.h"
#include "geda_widgets.h"

#include <geda_debug.h>

#define ThisDialog autonumber_text
#define Switch_Responder switch_responder

/** \defgroup Auto-Number-Dialog Auto Number Dialog
 *  @{
 *  \ingroup (Editing-Dialogs)
 *
 *  \par This Group contains routines for the AutoNumber dialog.
 *  AutoNumber dialog is primarily used for editing reference
 *  designator attribute, particularly for automatically number
 *  designation but can also be used to auto-number any other
 *  types of text attributes.
 */

/*! \brief How many entries to keep in the "Search text" combo box. */
#define HISTORY_LENGTH		15

/* autonumber text structs and enums */
enum {
  AUTONUMBER_SORT_DIAGONAL,
  AUTONUMBER_SORT_YX,
  AUTONUMBER_SORT_YX_REV,
  AUTONUMBER_SORT_XY,
  AUTONUMBER_SORT_XY_REV,
  AUTONUMBER_SORT_FILE
};

enum {
  AUTONUMBER_IGNORE,
  AUTONUMBER_RENUMBER,
  AUTONUMBER_RESPECT
};

enum {
  SCOPE_HISTORY,
  SCOPE_QUESTION,
  SCOPE_WILD,
} ScopeFilter;

typedef enum {
  SCOPE_SELECTED,
  SCOPE_PAGE,
  SCOPE_HIERARCHY
} AutoNumberScope;

/*! \brief Enumerate Control IDs. */
typedef enum {
  /* Combo Entry */
  ScopeText,

  /* Combo Chooser */
  ScopeNumber,
  ScopeSkip,
  SortOrder,

  /* Spinner*/
  StartNumber,

  /* Switches */
  DoRemoveNumber,
  DoSlotting,
  ScopeOverwrite

} ControlID;


/*! \brief String Arrays for Dialog Contrls.
 *  { "Hook-Up-String", "Label", "Tooltip string"},
 */
static WidgetStringData DialogStrings[] = {
  { "scope_text",      "Search for:",                 "Set the search criteria"},
  { "scope_number",    "Autonumber text in:",         "Set the context of the search scope"},
  { "scope_skip",      "Skip numbers found in:",      "Set where NOT to look for numbers"},
  { "sort_order",      "Sort order:",                 "Select the orientation of the Scan method"},
  { "opt_startnum",    "Starting number:",            "Set the starting number for the current scan"},
  { "opt_removenum",   "    Remove numbers:",         "Remove existing reference numbers"},
  { "opt_slotting",    "Automatic slotting:",         "Automatic slotting"},
  { "scope_overwrite", "Overwrite existing numbers:", "Overwrite existing numbers"},
  { NULL, NULL, NULL},
};

static char *unset_text[] = {
  "refdes=*",
  "refdes=C?",
  "refdes=D?",
  "refdes=I?",
  "refdes=L?",
  "refdes=Q?",
  "refdes=R?",
  "refdes=T?",
  "refdes=U?",
  "refdes=X?",
  "netname=A?",
  "netname=D?",
  NULL
};

static char *wild_text[] = {
  "refdes=*",
  "refdes=C*",
  "refdes=D*",
  "refdes=I*",
  "refdes=L*",
  "refdes=Q*",
  "refdes=R*",
  "refdes=T*",
  "refdes=U*",
  "refdes=X*",
  "netname=*",
  NULL
};

typedef struct autonumber_text_t AUTONUMBER_TEXT;

/*! \brief Stored state of the autonumber text dialog */
struct autonumber_text_t {

  /*! \brief Search text history */
  GList *scope_history;

  /*! \brief What scope data set to restore */
  int last_criteria;

  /*! \brief Scope for autonumbering text */
  AutoNumberScope scope_number;

  /*! \brief Scope for searching existing numbers */
  AutoNumberScope scope_skip;

  /*! \brief Overwrite existing numbers in scope */
  bool scope_overwrite;

  /*! \brief Sort order */
  int order;

  /*! \brief Starting number for automatic numbering */
  int startnum;

  /*! \brief Remove numbers instead of automatic numbering */
  bool removenum;

  /*! \brief Automatic assignments of slots */
  bool slotting;

  /*! \brief Pointer to the dialog */
  GtkWidget *dialog;

  /*! \brief Pointer to the GschemToplevel struct */
  GschemToplevel *w_current;

  /* variables used while autonumbering */
  char * current_searchtext;
  int root_page;       /* flag whether its the root page or not */
  GList *used_numbers; /* list of used numbers */
  GList *free_slots;   /* list of FREE_SLOT objects */
  GList *used_slots;   /* list of USED_SLOT objects */

};

typedef struct autonumber_slot_t AUTONUMBER_SLOT;

struct autonumber_slot_t {
  char *symbolname;     /* or should we use the device name? (Werner) */
  int number;           /* usually this is the refdes number */
  int slotnr;           /* just the number of the free slot */
};

/* The Combo Boxes */
static GtkWidget *ScopeTextCombo;
static GtkWidget *SortOrderCombo;

/* The Combo Menus */
static GtkWidget *ScopeNumberMenu;
static GtkWidget *ScopeSkipMenu;

/* Spinner Widget Control */
static GtkWidget *StartNumberSpin;

/* Switches */
static GtkWidget *DoRemoveNumberSwitch=NULL;
static GtkWidget *DoSlottingSwitch    =NULL;
static GtkWidget *ScopeOverwriteSwitch=NULL;

/* **************************** BACK-END CODE ****************************** */

/** \defgroup Auto-Number-Sorters Auto-Number-Sorters
 *  @{
 *  \ingroup (Auto-Number-Dialog)
 */

/* ******************* compare functions for g_list_sort, ****************** */
/*! \brief GCompareFunc function to sort a list with g_list_sort().
 *  \par Function Description
 *  Compares the integer values of the gconstpointers a and b.
 *  \return -1 if a<b, 1 if a>b, 0 if a==b
 */
static int autonumber_sort_numbers(const void *a, const void *b) {
  if (GPOINTER_TO_INT(a) < GPOINTER_TO_INT(b))
    return -1;
  if (GPOINTER_TO_INT(a) > GPOINTER_TO_INT(b))
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This Funcion takes two <B>Object*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  The Function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_xy(const void *a, const void *b) {
  Object *aa, *bb;
  aa=(Object *) a;  bb=(Object *) b;
  if (aa->text->x < bb->text->x)
    return -1;
  if (aa->text->x > bb->text->x)
    return 1;
  /* x == x */
  if (aa->text->y > bb->text->y)
    return -1;
  if (aa->text->y < bb->text->y)
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This funcion takes two <B>Object*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_xy_rev(const void *a, const void *b) {
  Object *aa, *bb;
  aa=(Object *) a;  bb=(Object *) b;
  if (aa->text->x < bb->text->x)
    return 1;
  if (aa->text->x > bb->text->x)
    return -1;
  /* x == x */
  if (aa->text->y < bb->text->y)
    return 1;
  if (aa->text->y > bb->text->y)
    return -1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This funcion takes two <B>Object*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_yx(const void *a, const void *b) {
  Object *aa, *bb;
  aa=(Object *) a;  bb=(Object *) b;
  if (aa->text->y > bb->text->y)
    return -1;
  if (aa->text->y < bb->text->y)
    return 1;
  /* y == y */
  if (aa->text->x < bb->text->x)
    return -1;
  if (aa->text->x > bb->text->x)
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This Funcion takes two <B>Object*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_yx_rev(const void *a, const void *b) {
  Object *aa, *bb;
  aa=(Object *) a;  bb=(Object *) b;
  if (aa->text->y > bb->text->y)
    return 1;
  if (aa->text->y < bb->text->y)
    return -1;
  /* y == y */
  if (aa->text->x > bb->text->x)
    return 1;
  if (aa->text->x < bb->text->x)
    return -1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This Funcion takes two <B>Object*</B> arguments and compares the
 *  location of the two text objects. The sort criteria is the combined x- and the
 *  y-location. The function sorts from top left to bottom right.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_diagonal(const void *a, const void *b) {
  Object *aa, *bb;
  aa=(Object *) a;  bb=(Object *) b;
  if (aa->text->x - aa->text->y < bb->text->x - bb->text->y)
    return -1;
  if (aa->text->x - aa->text->y > bb->text->x - bb->text->y)
    return 1;
  return 0;
}

/** @} end group Auto-Number-Sorters */

/*! \brief GCompareFunc function to acces <B>AUTONUMBER_SLOT</B> object in a GList
 *  \par Function Description
 *  This Funcion takes two <B>AUTONUMBER_SLOT*</B> arguments and compares them.
 *  Sorting criteria is are the AUTONUMBER_SLOT members: first the symbolname, than
 *  the number and last the slotnr.
 *  If the number or the slotnr is set to zero it acts as a wildcard.
 *  The function is used as GCompareFunc by GList functions.
 */
static int freeslot_compare(const void *a, const void *b)
{
  AUTONUMBER_SLOT *aa, *bb;
  int res;
  aa = (AUTONUMBER_SLOT *) a;  bb = (AUTONUMBER_SLOT *) b;

  if ((res = strcmp(aa->symbolname, bb->symbolname)) != 0)
    return res;

  /* aa->symbolname == bb->symbolname */
  if (aa->number == 0 || bb->number == 0)
    return 0;
  if (aa->number > bb->number)
    return 1;
  if (aa->number < bb->number)
    return -1;

  /* aa->number == bb->number */
  if (aa->slotnr == 0 || bb->slotnr == 0)
    return 0;
  if (aa->slotnr > bb->slotnr)
    return 1;
  if (aa->slotnr < bb->slotnr)
    return -1;

  return 0;
}

/*! \brief Prints a <B>GList</B> of <B>AUTONUMBER_SLOT</B> elements
 *  \par Function Description
 *  This funcion prints the elements of a GList that contains <B>AUTONUMBER_SLOT</B>
 *  elements. This function is is only used for debugging purposes.
 */
void freeslot_print(GList *list) {
  GList *item;
  AUTONUMBER_SLOT *fs;

  printf("freeslot_print(): symname, number, slot\n");
  for (item = list; item != NULL; item = g_list_next(item)) {
    fs = item ->data;
    printf("  %s, %d, %d\n",fs->symbolname, fs->number, fs->slotnr);
  }
}

/*! \brief Function to Clear the Databases of used parts
 *  \par Function Descriptions
 *  Just remove the list of used numbers, used slots and free slots.
 *
 *  \param [in] autotext   Pointer to <B>AUTONUMBER_TEXT</B> data structure
 */
static void autonumber_clear_database (AUTONUMBER_TEXT *autotext)
{
  /* cleanup everything for the next searchtext */
  if (autotext->used_numbers != NULL) {
    g_list_free(autotext->used_numbers);
    autotext->used_numbers = NULL;
  }
  if (autotext->free_slots != NULL) {
    g_list_foreach(autotext->free_slots, (GFunc) g_free, NULL);
    g_list_free(autotext->free_slots);
    autotext->free_slots = NULL;
  }
  if (autotext->used_slots != NULL) {
    g_list_foreach(autotext->used_slots, (GFunc) g_free, NULL);
    g_list_free(autotext->used_slots);
    autotext->used_slots = NULL;
  }
}

/*! \brief Function to test, whether the Object matches the autotext criteria
 *  \par Function Description
 *  The criteria are those of the autonumber text dialog. The function decides
 *  whether the <B>Object</B> has to be renumberd, ignored or taken care of when
 *  renumbering all other objects.
 *  \return one of these integer values:
 *  <DL>
 *    <DT><B>AUTONUMBER_IGNORE</B></DT>
 *    <DT><B>AUTONUMBER_RESPECT</B>and the text number of the object in \a number </DT>
 *    <DT><B>AUTONUMBER_RENUMBER</B></DT>
 *  </DL>
 *
 *  \param [in] autotext   Pointer to <B>AUTONUMBER_TEXT</B> data structure
 *  \param [in] o_current  Is an attribute GedaTextObject to be interrogated
 *
 *  \param [out] number    integer value that is to be the designation*
 *
 *  \note *number is not modified unless the returned value is AUTONUMBER_RESPECT
 */
static int
autonumber_match(AUTONUMBER_TEXT *autotext, Object *o_current, int *number)
{
  int len, isnumbered=1;
  const char *str = NULL;

  len = strlen(autotext->current_searchtext);
  /* first find out whether we can ignore that object */
  if (o_current->type != OBJ_TEXT)  /* text object */
    return AUTONUMBER_IGNORE;

  str = o_text_get_string (o_current);

  if (!(strlen(str) - len > 0) || !g_str_has_prefix(str, autotext->current_searchtext))
    return AUTONUMBER_IGNORE;

  /* the string object matches with its leading characters to the searchtext */
  /* now look for the extension, either a number or the "?" */
  if (g_str_has_suffix (str,"?")) {
    isnumbered = 0;
    /* There must not be any characters between the "?" and the searchtext */
    if (strlen(str) != len+1)
      return AUTONUMBER_IGNORE;
  }
  else {

    if (!isdigit((str[len]) )) /* has at least one digit */
      return AUTONUMBER_IGNORE;
/*
    for (i = len + 1; str[i]; i++) // and only digits
      if (!isdigit((str[i]) )) {
        return AUTONUMBER_IGNORE;
      }
*/
  }

  /* we have six cases, 3 from focus multiplied by 2 selection cases */
  if ((autotext->root_page || autotext->scope_number == SCOPE_HIERARCHY) &&
      (o_current->selected || autotext->scope_number == SCOPE_HIERARCHY || autotext->scope_number == SCOPE_PAGE) &&
      (!isnumbered || (autotext->scope_overwrite)))
    return AUTONUMBER_RENUMBER;

  if (isnumbered &&
    !(autotext->scope_skip == SCOPE_SELECTED &&
    !(o_current->selected)  && autotext->root_page))
  {
    sscanf(&(str[len])," %d", number);
    return AUTONUMBER_RESPECT; /* numbered objects which we don't renumber */
  }

  return AUTONUMBER_IGNORE;  /* unnumbered objects outside the focus */
}

/*! \brief Creates a list of already numbered objects and slots
 *  \par Function Description
 *  This function collects the used numbers of a single schematic page.
 *  The used element numbers are stored in a GList container
 *  inside the <B>AUTONUMBER_TEXT</B> struct.
 *  The slotting container is a little bit different. The container stores
 *  free slots of multislotted symbols, that were only partially used.
 *  The criterias are derivated from the autonumber dialog entries.
 *
 *  \param [in] w_current Pointer to GschemToplevel data structure
 *  \param [in] autotext  Pointer to <B>AUTONUMBER_TEXT</B> data structure
 */
static void
autonumber_get_used(GschemToplevel *w_current, AUTONUMBER_TEXT *autotext)
{
  int number, numslots, slotnr, i;
  Object *o_current, *o_parent;
  AUTONUMBER_SLOT *slot;
  GList *slot_item;
  char *numslot_str, *slot_str;
  const GList *iter;

  for (iter = s_page_get_objects (Current_Page); iter != NULL; NEXT(iter)) {
    o_current = iter->data;
    if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RESPECT) {
      /* check slot and maybe add it to the lists */
      o_parent = o_current->attached_to;
      if (autotext->slotting && o_parent != NULL) {
        /* check for slotted symbol */
        numslot_str =
        o_attrib_search_object_attribs_by_name (o_parent, "numslots", 0);
        if (numslot_str != NULL) {
          sscanf(numslot_str," %d",&numslots);
          GEDA_FREE(numslot_str);

          if (numslots > 0) {

            slot_str = o_attrib_search_object_attribs_by_name (o_parent, "slot", 0);
            if (slot_str == NULL) {
              u_log_message(_("slotted object without slot attribute may cause "
              "problems when autonumbering slots\n"));
            }
            else {

              sscanf(slot_str, " %d", &slotnr);
              slot = g_new(AUTONUMBER_SLOT,1);
              slot->number = number;
              slot->slotnr = slotnr;
              slot->symbolname = o_parent->complex->filename;

              slot_item = g_list_find_custom(autotext->used_slots,
                                             slot,
                                             (GCompareFunc) freeslot_compare);
              if (slot_item != NULL) { /* duplicate slot in used_slots */
                u_log_message(_("duplicate slot may cause problems: "
                "[symbolname=%s, number=%d, slot=%d]\n"),
                slot->symbolname, slot->number, slot->slotnr);
                GEDA_FREE(slot);
              }
              else {

                autotext->used_slots = g_list_insert_sorted(autotext->used_slots,
                                                            slot,
                                                            (GCompareFunc) freeslot_compare);

                slot_item = g_list_find_custom(autotext->free_slots,
                                               slot,
                                               (GCompareFunc) freeslot_compare);
                if (slot_item == NULL) {
                  /* insert all slots to the list, except of the current one */
                  for (i=1; i <= numslots; i++) {
                    if (i != slotnr) {
                      slot = g_memdup(slot, sizeof(AUTONUMBER_SLOT));
                      slot->slotnr = i;
                      autotext->free_slots = g_list_insert_sorted(autotext->free_slots,
                                                                  slot,
                                                                  (GCompareFunc) freeslot_compare);
                    }
                  }
                }
                else {
                  GEDA_FREE(slot_item->data);
                  autotext->free_slots = g_list_delete_link(autotext->free_slots, slot_item);
                }
              }
            }
          }
        }
      }
      /* put number into the used list */
      autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
                                                    GINT_TO_POINTER(number),
                                                    (GCompareFunc) autonumber_sort_numbers);
    }
  }
}

/*! \brief Gets or generates free numbers for the autonumbering process.
 *  \par Function Description
 *  This function gets or generates new numbers for the <B>Object o_current</B>.
 *  It uses the element numbers <B>used_numbers</B> and the list of the free slots
 *  <B>free_slots</B> of the <B>AUTONUMBER_TEXT</B> struct. The new number is
 *  returned into the <B>number</B> parameter. <B>slot</B> is set if autoslotting
 *  is active, else it is set to zero.
 *
 *  \param [in]  autotext  Pointer to <B>AUTONUMBER_TEXT</B> state data structure
 *  \param [in]  o_current Is an attribute GedaTextObject
 *
 *  \param [out] number    integer value that is to be the designation
 *  \param [out] slot      integer value is the slot id the parent object should be
 *
 *  \returns designation and slot number
 */
static void
autonumber_get_new_numbers(AUTONUMBER_TEXT *autotext,
                           Object          *o_current,
                           int             *number,
                           int             *slot)
{
  AUTONUMBER_SLOT *freeslot;

  Object *o_parent = NULL;
  GList  *freeslot_item;
  GList  *item;
  char   *numslot_str;

  int new_number, numslots, i;

  new_number = autotext->startnum;

  /* Check for slots first */
  /* 1. are there any unused slots in the database? */
  o_parent = o_current->attached_to;

  if (autotext->slotting && o_parent != NULL) {

    freeslot = g_new(AUTONUMBER_SLOT,1);
    freeslot->symbolname = o_parent->complex->filename;
    freeslot->number = 0;
    freeslot->slotnr = 0;
    freeslot_item = g_list_find_custom(autotext->free_slots,
                                       freeslot,
                                       (GCompareFunc) freeslot_compare);
    GEDA_FREE(freeslot);
    /* Yes! -> remove from database, apply it */
    if (freeslot_item != NULL) {
      freeslot = freeslot_item->data;
      *number = freeslot->number;
      *slot = freeslot->slotnr;
      GEDA_FREE(freeslot);
      autotext->free_slots = g_list_delete_link(autotext->free_slots, freeslot_item);

      return;
    }
  }

  /* get a new number */
  item = autotext->used_numbers;
  while (1) {
    while (item != NULL && GPOINTER_TO_INT(item->data) < new_number)
      item = g_list_next(item);

    if (item == NULL || GPOINTER_TO_INT(item->data) > new_number)
      break;
    else  /* new_number == item->data */
      new_number++;
  }

  *number = new_number;
  *slot = 0;

  /* insert the new number to the used list */
  autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
                                                GINT_TO_POINTER(new_number),
                                                (GCompareFunc) autonumber_sort_numbers);

  /* 3. is o_current a slotted object ? */
  if ((autotext->slotting) && o_parent != NULL) {
    numslot_str =
    o_attrib_search_object_attribs_by_name (o_parent, "numslots", 0);
    if (numslot_str != NULL) {
      sscanf(numslot_str," %d",&numslots);
      GEDA_FREE(numslot_str);
      if (numslots > 0) {
        /* Yes! -> new number and slot=1; add the other slots to the database */
        *slot = 1;
        for (i=2; i <=numslots; i++) {
          freeslot = g_new(AUTONUMBER_SLOT,1);
          freeslot->symbolname = o_parent->complex->filename;
          freeslot->number = new_number;
          freeslot->slotnr = i;
          autotext->free_slots = g_list_insert_sorted(autotext->free_slots,
                                                      freeslot,
                                                      (GCompareFunc) freeslot_compare);
        }
      }
    }
  }
}

/*! \brief Removes the number from the element.
 *  \par Function Description
 *  This function updates the text content of the \a o_current object.
 *
 *  \param [in] autotext Pointer to the state structure
 *  \param [in] o_current Pointer to the object from which to remove the number
 *
 */
static void
autonumber_remove_number(AUTONUMBER_TEXT * autotext, Object *o_current)
{
  Object *o_parent, *o_slot;
  char *str;

  /* allocate memory for the search string*/
  str = malloc(strlen(autotext->current_searchtext) + 2); /* allocate space */

  /* make copy of the search string and append "?" */
  strcpy(str, autotext->current_searchtext);
  strcat(str, "?");

  /* replace old text */
  o_text_set_string (o_current, str);
  free (str);

  /* if slotting is active then remove the slot attribute */
  if (autotext->slotting) {
    /* get the slot attribute */
    o_parent = o_current->attached_to;
    if (o_parent != NULL) { /* Does child->parent_object->child make sense?*/
      /* \remark s_slot_search_slot updates o_slot variable */
      g_free (s_slot_search_slot (o_parent, &o_slot));
      /* Only attempt to remove non-inherited slot attributes */
      if (o_slot != NULL && !o_attrib_is_inherited (o_slot)) {
        /* delete the slot attribute */
        o_delete (autotext->w_current, o_slot);
      }
    }
  }
}

/*! \brief Changes the number <B>Object</B> element. Changes the slot attribute.
 *  \par Function Description
 *  This function updates the text content of the <B>\a o_current</B> object
 *  if the <B>slot</B> value is non zero. The slot attribute of the complex
 *  element that is also the parent object of the o_current element is also
 *  updates.
 *
 *  \param [in] autotext  Pointer to <B>AUTONUMBER_TEXT</B> state data structure
 *  \param [in] o_current Is an attribute GedaTextObject
 *  \param [in] number    integer value that is to be the designation
 *  \param [in] slot      integer value is the slot id of the parent object
 */
/* \note 11/04/12 WEH: Revised to eliminate gmalloc of trivial strings (twice).
 *       added conditional so that slot=0 is not passed to o_slot_end function
 *       even though slot=0 is valid, it just means the component has none.
 */
static void
autonumber_apply_new_text(AUTONUMBER_TEXT *autotext,
                          Object          *o_current,
                          int              number,
                          int              slot)
{
  char string[32]="slot=";  /* char buffer to hold set=refdes=xx*/
  char s_val[5];            /* char buffer for integer conversion to string */

  if ( slot > 0) {
    /* update the slot on the owner object */
    u_string_int2str( slot, &string[5], 10);
    o_slot_end (autotext->w_current, o_current->attached_to, string);
  }

  /* replace old text, looks like "set=refdes=U1"*/
  strcpy(string, autotext->current_searchtext);
  strcat(string, u_string_int2str( number, s_val, 10));
  o_text_set_string (o_current, string);
}

/*! \brief Handles all the options of the autonumber text dialog
 *  \par Function Description
 *  This function is the main routine for the autonumber code. The function
 *  retrieves options from the \a autotext structure. First it collects all
 *  pages of a hierarchical schematic, even if hierarchy info is not used.
 *  The function retrieves all matching text elements for the searchtext and
 *  then renumbers text elements based on options from the dialog.
 *
 *  \param [in] autotext  Pointer to <B>AUTONUMBER_TEXT</B> state data structure
 */
static void autonumber_text_autonumber(AUTONUMBER_TEXT *autotext)
{
  GschemToplevel *w_current;

  const GList *iter;
  GList *o_list          = NULL;
  GList *pages;
  GList *searchtext_list = NULL;
  GList *text_item;
  GList *obj_item;
  GList *page_item;

  Object *o_current;

  char *searchtext;
  char *scope_text;
  char *new_searchtext;

  int i, number;
  int slot, scope_len;

  w_current = autotext->w_current;
  autotext->current_searchtext = NULL;
  autotext->root_page = 1;

  autotext->used_numbers = NULL;
  autotext->free_slots = NULL;
  autotext->used_slots = NULL;

  /* Step 1: Retrieve the Scope Search text */
  scope_text = g_list_first(autotext->scope_history)->data;

  /* Step 2: Get all pages of the hierarchy */
  pages = s_hierarchy_traverse_pages (w_current->toplevel,
                                      Current_Page,
                                      HIERARCHY_NODUPS);

  /*  g_list_foreach(pages, (GFunc) s_hierarchy_print_page, NULL); */

  /* Step 3: if searchtext has an asterisk at the end we have to find
   *    all matching searchtextes.
   *
   *  Example:  "refdes=*" will match each text that starts with "refdes="
   *  and has a trailing "?" or a trailing number if the "all"-option is set.
   *  We get a list of possible prefixes: refdes=R, refdes=C.
   *
   *  If there is only one search pattern, it becomes a single item
   *  in the searchtext list */

  scope_len = strlen(scope_text);
  if (scope_len == 0) {
    u_log_message(_("No searchstring given in autonumber text.\n"));
    return; /* error */
  }
  else if (g_str_has_suffix(scope_text,"?") == TRUE) {
    /* single searchtext, strip of the "?" */
    searchtext = g_strndup(scope_text, scope_len - 1);
    searchtext_list=g_list_append (searchtext_list, searchtext);
  }
  else if (g_str_has_suffix(scope_text,"*") == TRUE) {
    /* strip of the "*" */
    searchtext = g_strndup(scope_text, scope_len - 1);
    /* collect all the possible searchtexts in all pages of the hierarchy */
    for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
      s_page_goto(w_current->toplevel, page_item->data);
      /* iterate over all objects an look for matching searchtext's */
      for (iter = s_page_get_objects(Current_Page); iter != NULL; NEXT(iter))
      {
        o_current = iter->data;
        if (o_current->type == OBJ_TEXT) {
          if (autotext->scope_number == SCOPE_HIERARCHY
            || autotext->scope_number == SCOPE_PAGE
            || ((autotext->scope_number == SCOPE_SELECTED) && (o_current->selected)))
          {

            const char *str = o_text_get_string (o_current);


            if (g_str_has_prefix (str, searchtext)) {

              int search_len;

              /* the beginning of the current text matches with the searchtext now */
              /* strip off the trailing [0-9?] chars and add it too the searchtext */

              search_len = strlen(searchtext);
              i = strlen (str)-1;

              /* Skip in trailing character ( the ones we realy should save */
              while ((i >= search_len + 1) && isalpha((str[i]))) i--;
              while ((i >= search_len) && (str[i] == '?' || isdigit( (int) (str[i]) ))) i--;

              new_searchtext = g_strndup (str, i+1);

              if (g_list_find_custom(searchtext_list, new_searchtext, (GCompareFunc) strcmp) == NULL )
              {
                searchtext_list = g_list_append(searchtext_list, new_searchtext);
              }
              else {
                GEDA_FREE(new_searchtext);
              }
            }
          }
        }
      }
      if (autotext->scope_number == SCOPE_SELECTED || autotext->scope_number == SCOPE_PAGE)
        break; /* search only in the first page */
    }
    GEDA_FREE(searchtext);
  }
  else {
    u_log_message(_("No '*' or '?' given at the end of the autonumber text.\n"));
    return;
  }

  /* Step 4: iterate over the search items in the list */
  for (text_item=searchtext_list; text_item !=NULL; NEXT(text_item)) {

    autotext->current_searchtext = text_item->data;

    /* printf("autonumber_text_autonumber: searchtext %s\n", autotext->current_searchtext); */
    /* decide whether to renumber page by page or get a global used-list */

    if (autotext->scope_skip == SCOPE_HIERARCHY) {  /* whole hierarchy database */
      /* renumbering all means that no db is required */
      if (!(autotext->scope_number == SCOPE_HIERARCHY && autotext->scope_overwrite))
      {
        for (page_item = pages; page_item != NULL; NEXT(page_item))
        {
          autotext->root_page = (pages->data == page_item->data);
          s_page_goto(w_current->toplevel, page_item->data);
          autonumber_get_used(w_current, autotext);
        }
      }
    }

    /* renumber the elements */
    for (page_item = pages; page_item != NULL; NEXT(page_item)) {

      s_page_goto(w_current->toplevel, page_item->data);

      autotext->root_page = (pages->data == page_item->data);

      /* build a page database if we're numbering pagebypage or selection only */
      if (autotext->scope_skip == SCOPE_PAGE || autotext->scope_skip == SCOPE_SELECTED) {
        autonumber_get_used(w_current, autotext);
      }

      /* RENUMBER CODE FOR ONE Page AND ONE SEARCHTEXT*/
      /* 1. get objects to renumber */
      for (iter = s_page_get_objects(Current_Page); iter != NULL; NEXT(iter))
      {
        o_current = iter->data;

        if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RENUMBER) {
          /* put number into the used list */
          o_list = g_list_append(o_list, o_current);
        }
      }

      /* 2. sort object list */
      switch (autotext->order) {
        case AUTONUMBER_SORT_YX:
          o_list=g_list_sort(o_list, autonumber_sort_yx);
          break;
        case AUTONUMBER_SORT_YX_REV:
          o_list=g_list_sort(o_list, autonumber_sort_yx_rev);
          break;
        case AUTONUMBER_SORT_XY:
          o_list=g_list_sort(o_list, autonumber_sort_xy);
          break;
        case AUTONUMBER_SORT_XY_REV:
          o_list=g_list_sort(o_list, autonumber_sort_xy_rev);
          break;
        case AUTONUMBER_SORT_DIAGONAL:
          o_list=g_list_sort(o_list, autonumber_sort_diagonal);
          break;
        default:
          ; /* unsorted file order */
      }

      /* 3. renumber/reslot the objects */
      for(obj_item=o_list; obj_item != NULL; NEXT(obj_item)) {
        o_current= obj_item->data;
        if(autotext->removenum) {
          autonumber_remove_number(autotext, o_current);
        }
        else {
          /* get valid numbers from the database */
          autonumber_get_new_numbers(autotext, o_current, &number, &slot);
          autonumber_apply_new_text(autotext, o_current, number, slot);
        }
      }
      g_list_free(o_list);
      o_list = NULL;

      /* destroy the page database */
      if (autotext->scope_skip == SCOPE_PAGE
        || autotext->scope_skip == SCOPE_SELECTED)
        autonumber_clear_database(autotext);

      if (autotext->scope_number == SCOPE_SELECTED
        || autotext->scope_number == SCOPE_PAGE)
        break; /* only renumber the parent page (the first page) */
    }
    autonumber_clear_database(autotext);   /* cleanup */
  }

  /* cleanup and redraw all*/
  g_list_foreach(searchtext_list, (GFunc) g_free, NULL);
  g_list_free(searchtext_list);
  s_page_goto(w_current->toplevel, pages->data); /* go back to the root page */
  o_invalidate_all (w_current);
  g_list_free(pages);
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Put the icons and the text into the SortOrder combobox
 *  \par Function Description
 *  Load all bitmaps for the combobox and store them together with the label
 *  in a GtkListStore.
 *
 *  \param w_current Pointer to GschemToplevel data structure
 */
static void autonumber_sortorder_create(GschemToplevel *w_current)
{
  GtkListStore    *store;
  GtkTreeIter      iter;
  GtkCellRenderer *renderer;
  GdkPixbuf       *pixbuf;
  GError          *error=NULL;
  char             *path;

  int i;

  char *filenames[] = { "gschem_diagonal.png",
                        "gschem_top2bottom.png",
                        "gschem_bottom2top.png",
                        "gschem_left2right.png",
                        "gschem_left2right.png",
                        "gschem_fileorder.png",
                        NULL};

  char *names[] = { N_( "Diagonal"),
                    N_("Top to bottom"),
                    N_("Bottom to top"),
                    N_("Left to right"),
                    N_("Right to left"),
                    N_("File order"),
                    NULL};

  store = gtk_list_store_new(2, G_TYPE_STRING, GDK_TYPE_PIXBUF);

  for (i=0; filenames[i] != NULL; i++) {
    path=g_build_filename(w_current->toplevel->bitmap_directory,
                          filenames[i], NULL);
    pixbuf = gdk_pixbuf_new_from_file(path, &error);
    GEDA_FREE(path);
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, _(names[i]), 1, pixbuf, -1);
  }

  gtk_combo_box_set_model(GTK_COMBO_BOX(SortOrderCombo), GTK_TREE_MODEL(store));
  renderer = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(SortOrderCombo), renderer, TRUE);

  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (SortOrderCombo),
                                  renderer, "text", 0, NULL);

  renderer = gtk_cell_renderer_pixbuf_new();

  g_object_set(G_OBJECT(renderer), "xpad", 5, "ypad", 5, NULL);

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(SortOrderCombo), renderer, FALSE);

  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (SortOrderCombo),
                                  renderer, "pixbuf", 1, NULL);
}

/* ***** STATE STRUCT HANDLING (interface between GUI and backend code) **** */

/*! \brief Adds a line to the search text history list
 *  \par Function Description
 *  This function prepends \a text to the list \a history and removes
 *  AND releases \a text from any other position in the \a history if
 *  present. The length of the list is checked and truncated if longer
 *  than #HISTORY_LENGTH.
 *  \par
 *  <DL>
 *    <DT>1) Removes \a text from the list</DT>
 *    <DT>2) Prepends \a text to the list list</DT>
 *    <DT>3) Truncates \a history if longer than #HISTORY_LENGTH</DT>
 *  </DL>
 *  \param [in] history GList to be prepended with \a text
 *  \param [in] text    The text string to be prepended to the list
 */
static GList *
autonumber_add_history(GList *history, char *text)
{
  /* Search for this text in history and delete it (so we don't have
   * duplicate entries) */

  GList *current;

  current = history;

  while(current !=NULL ) {

    if(!strcmp(text, current->data)) {

      history = g_list_remove_link(history, current);

      GEDA_FREE(current->data);
      g_list_free(current);
      break;
    }
    NEXT(current);
  }

  /* Add the new text at the beginning of the list */
  history = g_list_prepend(history, text);

  /* Truncate history */
  while(g_list_length(history) > HISTORY_LENGTH) {

    GList *last = g_list_last(history);

    history = g_list_remove_link(history, last);

    GEDA_FREE(last->data);
    g_list_free(last);
  }

  return history;
}

/*! \brief Set the Scope Filter Text to the History option
 *  \par Function Description
 *  Clears the ScopeTextCombo and reloads the combo with values
 *  from the scope_history list. The combo entry is set to the
 *  first value in the list. The value of last_criteria in the
 *  state \a data structure is set to SCOPE_HISTORY so that the
 *  same list will be restored if the dialog is reloaded.
 *
 *  \param [in] button Pointer to Widget that was Activated (bulb) or NULL
 *  \param [in] data   Is a pointer to data structure
 */
static void
set_scope_filter_text_history (GtkWidget *button, void *data)
{
  AUTONUMBER_TEXT  *autotext = data;
  GedaComboBoxText *combo    = GEDA_COMBO_BOX_TEXT(ScopeTextCombo);

  GList *iter;

  int count = 0;

  geda_combo_box_text_remove_all(combo);

  for (iter = autotext->scope_history; iter; NEXT(iter)) {

    geda_combo_box_text_widget_append(ScopeTextCombo, iter->data);

    count++;
  }

  if (count) {

    SetEntryText( combo->entry, g_list_first(autotext->scope_history)->data);

  }

  autotext->last_criteria = SCOPE_HISTORY;
}

/*! \brief Set the Scope Filter Text to the Question type
 *  \par Function Description
 *  Clears the ScopeTextCombo and reloads the combo with values
 *  in the structure #unset_text. The combo entry is set to the
 *  first value in the structure. The value of last_criteria in
 *  the state \a data structure is set to SCOPE_QUESTION so the
 *  same list will be restored if the dialog is reloaded.
 *
 *  \param [in] button Pointer to Widget that was Activated (bulb) or NULL
 *  \param [in] data   Is a pointer to data structure
 */
static void
set_scope_filter_text_question (GtkWidget *button, void *data)
{
  AUTONUMBER_TEXT  *autotext = data;
  GedaComboBoxText *combo    = GEDA_COMBO_BOX_TEXT(ScopeTextCombo);

  int index;

  geda_combo_box_text_remove_all(combo);

  for (index = 0; unset_text[index] != NULL; index++) {

    geda_combo_box_text_append(combo, unset_text[index]);

  }

  SetEntryText( combo->entry, unset_text[0]);

  autotext->last_criteria = SCOPE_QUESTION;
}

/*! \brief Set the Scope Filter Text to the Wild type
 *  \par Function Description
 *  Clears the ScopeTextCombo and reloads the combo with values
 *  in the structure #wild_text. The combo entry is set to the
 *  first value in the structure. The value of last_criteria in
 *  the state \a data structure is set to SCOPE_WILD so that the
 *  same list will be restored if the dialog is reloaded.
 *
 *  \param [in] button Pointer to Widget that was Activated (bulb) or NULL
 *  \param [in] data   Is a pointer to data structure
 */
static void
set_scope_filter_text_wild (GtkWidget *button, void *data)
{
  AUTONUMBER_TEXT  *autotext = data;
  GedaComboBoxText *combo    = GEDA_COMBO_BOX_TEXT(ScopeTextCombo);

  int index;

  geda_combo_box_text_remove_all(combo);

  for (index = 0; wild_text[index] != NULL; index++) {

    geda_combo_box_text_append(combo, wild_text[index]);

  }

  SetEntryText( combo->entry, wild_text[0]);

  autotext->last_criteria = SCOPE_WILD;
}

/*! \brief Allocate and initialize the state structure
 *  \par Function Description
 *  Creates and returns a new <B>AUTONUMBER_TEXT</B> structure.
 *  Values in the structure are initialized to default or set to
 *  trigger the defaults to be load later.
 *
 *  \param [in] w_current Pointer to the top level struct
 *
 *  \return Pointer to the allocated structure or NULL on error.
 */
static AUTONUMBER_TEXT *autonumber_init_state(GschemToplevel *w_current)
{
  AUTONUMBER_TEXT *autotext;

  /* Default contents of the combo box history */

  autotext = g_new(AUTONUMBER_TEXT, 1);

  if(autotext==NULL) return NULL;

  autotext->scope_history = NULL;

  autotext->last_criteria = SCOPE_QUESTION;

  if (s_hierarchy_find_up_page(w_current->toplevel->pages, Current_Page)) {
    autotext->scope_skip   = SCOPE_HIERARCHY;
    autotext->scope_number = SCOPE_HIERARCHY;
  }
  else {
    autotext->scope_skip = SCOPE_PAGE;
    if (o_select_is_selection(w_current)) {
      autotext->scope_number = SCOPE_SELECTED;
    }
    else {
      autotext->scope_number = SCOPE_PAGE;
    }
  }

  autotext->scope_overwrite = FALSE;
  autotext->order = AUTONUMBER_SORT_DIAGONAL;

  autotext->startnum=1;

  autotext->removenum = FALSE;
  autotext->slotting  = FALSE;

  autotext->dialog = NULL;

  return autotext;
}

/*! \brief Restore the settings for the autonumber text dialog
 *  \par Function Description
 *   Retrieves values from control in the dialog and save the
 *   values to the \a autotext <B>AUTONUMBER_TEXT</B> structure.
 *
 *  \param [in] autotext  Pointer to state data structure
 */
static void restore_dialog_values(AUTONUMBER_TEXT *autotext)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList    *scope_group;

  /* Scope */
  scope_group = g_object_get_data (G_OBJECT (autotext->dialog), "ScopeGroup");

  geda_bulb_group_set_active_index (scope_group, autotext->last_criteria);

  switch (autotext->last_criteria) {
    case SCOPE_HISTORY:
      set_scope_filter_text_history (NULL, autotext);
      break;
    case SCOPE_QUESTION:
      set_scope_filter_text_question (NULL, autotext);
      break;
    case SCOPE_WILD:
      set_scope_filter_text_wild (NULL, autotext);
    default:
      break;
  }

  /* Set the ScopeNumber ComboMenu with the value in autotext->scope_number */
  gtk_option_menu_set_history(GTK_OPTION_MENU(ScopeNumberMenu), autotext->scope_number);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(ScopeNumberMenu));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);

  /* Set the ScopeSkip ComboMenu with the value in autotext->scope_skip */
  gtk_option_menu_set_history(GTK_OPTION_MENU(ScopeSkipMenu), autotext->scope_skip);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(ScopeSkipMenu));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);

  SetSwitch (ScopeOverwrite, autotext->scope_overwrite);
  gtk_widget_set_sensitive(ScopeOverwriteSwitch, autotext->removenum);

  /* Options */
  SetSpin (StartNumber, autotext->startnum);

  SetCombo (SortOrder, autotext->order);

  SetSwitch (DoRemoveNumber, autotext->removenum);

  SetSwitch (DoSlotting, autotext->slotting);

}

/*! \brief Get the settings from the autonumber text dialog
 *  \par Function Description
 *   Retrieves values from control in the dialog and save the
 *   values to the \a autotext <B>AUTONUMBER_TEXT</B> structure.
 *
 *  \param [in] autotext  Pointer to data structure
 */
static void retrieve_values_from_dialog(AUTONUMBER_TEXT *autotext)
{
  char *text;

  /* Scope */

  /* Note we do not have to retrieve autotext->last_criteria here because
   * the callbacks did this whenever a change was made.
   */

  /* Search text history */
  text = geda_combo_box_text_get_active_text(GEDA_COMBO_BOX_TEXT(ScopeTextCombo));

  autotext->scope_history = autonumber_add_history(autotext->scope_history, text);

  /* Retrieve scope_number selection from ScopeNumberMenu Combo/Menu */
  autotext->scope_number = GPOINTER_TO_INT(
    gtk_object_get_data (
      GTK_OBJECT (
        gtk_menu_get_active (
          GTK_MENU (gtk_option_menu_get_menu (
            GTK_OPTION_MENU (ScopeNumberMenu))))), "scope_menu"));

    /* Retrieve scope_skip selection from ScopeSkipMenu Combo/Menu */
    autotext->scope_skip = GPOINTER_TO_INT(
      gtk_object_get_data (
        GTK_OBJECT (
          gtk_menu_get_active (
            GTK_MENU (gtk_option_menu_get_menu (
              GTK_OPTION_MENU (ScopeSkipMenu))))), "scope_menu"));

      autotext->scope_overwrite = GET_SWITCH_STATE (ScopeOverwriteSwitch);

      /* Sort order */
      autotext->order = gtk_combo_box_get_active(GTK_COMBO_BOX(SortOrderCombo));

      /* Options */
      autotext->startnum  = GET_SPIN_IVALUE (StartNumberSpin);
      autotext->removenum = GET_SWITCH_STATE (DoRemoveNumberSwitch);
      autotext->slotting  = GET_SWITCH_STATE (DoSlottingSwitch);

}

/* ***** CALLBACKS (functions that get called from GTK) ******* */

/*! \brief response callback for the Autonumber text dialog
 *  \par Function Description
 *  The function just closes the dialog if the close button is pressed or
 *  the user closes the dialog window. If the Apply button is pressed this
 *  function calls retrieve_values_from_dialog and then it doesn't make
 *  any sense.
 *
 *  \param [in] widget    Pointer to Widget that was Activated.
 *  \param [in] response  Integer response (basically control id)
 *  \param [in] autotext  Pointer to data structure
 */
static void autonumber_text_response(GtkWidget       *widget,
                                     int              response,
                                     AUTONUMBER_TEXT *autotext)
{
  switch (response) {
    case GSCHEM_RESPONSE_ACCEPT:
      retrieve_values_from_dialog(autotext);
      if (autotext->removenum == TRUE && autotext->scope_overwrite == FALSE) {
        /* temporarly set the overwrite flag */
        autotext->scope_overwrite = TRUE;
        autonumber_text_autonumber(autotext);
        autotext->scope_overwrite = FALSE;
      }
      else {
        autonumber_text_autonumber(autotext);
      }
      break;

    case GSCHEM_RESPONSE_CLOSE:
      retrieve_values_from_dialog(autotext);
    case GSCHEM_RESPONSE_DELETE_EVENT:
      gtk_widget_destroy(autotext->dialog);
      autotext->dialog = NULL;
      break;
    default:
      BUG_IMSG ("unhandled case for signal <%d>", response);
  }
}

/*! \brief Function to toggle switch images
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 *       The functions handles callback for all switches on This
 *       Dialog.
 *
 *  \param [in] widget    Pointer to the GedaSwitch object.
 *  \param [in] Control   Pointer to integer Switch identifier
 */
static void
switch_responder(GtkWidget *widget, ControlID *Control)
{
  bool state = GET_SWITCH_STATE (widget);
  GtkWidget* SwitchImage = get_geda_switch_image( state);
  gtk_button_set_image(GTK_BUTTON (widget), SwitchImage);

  int WhichOne = (int)(long*) Control;

  switch ( WhichOne ) {
    case DoRemoveNumber:
      gtk_widget_set_sensitive(ScopeOverwriteSwitch, state);
      break;
    case DoSlotting:
    case ScopeOverwrite:
      break;
    default:
      u_log_message("toggle_switch(): UKNOWN SWITCH ID: %d\n", WhichOne);
  }

  return;
}

/* ***** DIALOG SET-UP ***************************************************** */

/*! \brief Creates the Filter Option Bulbs on the Autonumber text dialog.
 *  Constructor extension for the AutoNumber Dialog.
 *
 *  \param [in] Dialog    Pointer to the AutoNumber Dialog.
 *  \param [in] autotext  Pointer to AUTONUMBER_TEXT data structure
 *  \param [in] container Pointer to container for the bulb widgets
 */
static inline void
autonumber_create_filter_options (GtkWidget *Dialog,
                                  AUTONUMBER_TEXT *autotext,
                                  GtkWidget *container)
{
  GtkWidget *widget;

  widget = geda_aligned_visible_label_new(_("<b>Filter:</b>"), 0, 0);
  geda_label_widget_set_use_markup(widget, TRUE);
  gtk_box_pack_start (GTK_BOX (container), widget,  FALSE, FALSE, 5);
  gtk_misc_set_alignment (GTK_MISC (widget), 0, 0.5);

  widget = geda_bulb_new_visible_with_mnemonic(NULL, "History");
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  gtk_box_pack_start (GTK_BOX (container), widget,  FALSE, FALSE, 5);

  g_signal_connect(widget, "clicked", G_CALLBACK(set_scope_filter_text_history), autotext);

  g_object_set (widget, "show-button", TRUE, NULL);

  widget = geda_bulb_new_with_mnemonic_from_widget(widget, "Unset", 1);
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  gtk_box_pack_start (GTK_BOX (container), widget,  FALSE, FALSE, 5);
  g_signal_connect(widget, "clicked", G_CALLBACK(set_scope_filter_text_question), autotext);

  widget = geda_bulb_new_with_mnemonic_from_widget(widget, "Wild", 1);
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  gtk_box_pack_start (GTK_BOX (container), widget,  FALSE, FALSE, 5);
  g_signal_connect(widget, "clicked", G_CALLBACK(set_scope_filter_text_wild), autotext);

  g_object_set_data(G_OBJECT(Dialog), "ScopeGroup", geda_bulb_get_group(widget));

}

/*! \brief Create Scope menus for the AutoNumber dialog
 *  \par Function Description
 *  This function creates a GtkMenu with different scope options.
 */
static inline GtkWidget*
autonumber_create_scope_menu (GschemToplevel *w_current)
{
  GtkWidget *menu;
  GSList *group;

  struct scope_options {
    char *str;
    AutoNumberScope scope;
  } types[] = {
    { N_("Selected objects"),     SCOPE_SELECTED},
    { N_("Current page"),         SCOPE_PAGE},
    { N_("Whole hierarchy"),      SCOPE_HIERARCHY }
  };

  int i;

  menu  = gtk_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof (types) / sizeof (struct scope_options); i++) {
    GtkWidget *menuitem;

    menuitem = gtk_radio_menu_item_new_with_label (group, _(types[i].str));
    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
    gtk_menu_append (GTK_MENU (menu), menuitem);
    gtk_object_set_data (GTK_OBJECT(menuitem), "scope_menu", GINT_TO_POINTER (types[i].scope));
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*! \brief Creates the autonumber text dialog.
 *   Constructor for the AutoNumber Dialog, the Dialog returned but is not
 *   shown.
 *
 *  \param [in] w_current Pointer to the top level struct.
 *  \param [in] autotext  Pointer to AUTONUMBER_TEXT data structure
 *
 *  \returns Pointer to the dialog window.
 */
static GtkWidget*
autonumber_create_dialog(GschemToplevel *w_current, AUTONUMBER_TEXT *autotext)
{
  GtkWidget *ThisDialog;
  GtkWidget *main_vbox;
  GtkWidget *alignment;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *upper_table;
  GtkWidget *lower_table;
  GtkWidget *upper_vbox;
  GtkWidget *lower_vbox;

  ThisDialog = gschem_dialog_new_with_buttons(_("Autonumber text"),
                                              GTK_WINDOW(w_current->main_window),
            /* modal-less */                  GSCHEM_MODELESS_DIALOG,
                                              IDS_AUTONUMBER,
                                              w_current,
                                              GTK_STOCK_CLOSE, GSCHEM_RESPONSE_CLOSE,
                                              GTK_STOCK_APPLY, GSCHEM_RESPONSE_ACCEPT,
                                              NULL );

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                          GSCHEM_RESPONSE_ACCEPT,
                                          GSCHEM_RESPONSE_CLOSE,
                                          -1);

  /* gtk_window_position (GTK_WINDOW (ThisDialog), GTK_WIN_POS_MOUSE);*/

  main_vbox = GTK_DIALOG(ThisDialog)->vbox;

  /* scope section */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME, "label", "", NULL));
  gtk_box_pack_start(GTK_BOX(main_vbox), frame, FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  label = geda_aligned_visible_label_new(_("<b>Scope</b>"), 0, 0);
  geda_label_widget_set_use_markup(label, TRUE);
  gtk_frame_set_label_widget (GTK_FRAME(frame), label);

  alignment = gtk_alignment_new (0, 0, 1, 1);
  g_object_set (alignment, "visible", TRUE, NULL);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0,
                             DIALOG_INDENTATION, DIALOG_INDENTATION);

  upper_vbox = gtk_vbox_new (FALSE, 0);
  g_object_set (upper_vbox, "visible", TRUE, NULL);
  gtk_container_add (GTK_CONTAINER (alignment), upper_vbox);

  upper_table = gtk_table_new (3, 2, FALSE);
  g_object_set (upper_table, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (upper_vbox), upper_table, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (upper_table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (upper_table), DIALOG_H_SPACING);

  label = geda_aligned_visible_label_new (_LABEL(ScopeText), 0, 0.5);
  gtk_table_attach (GTK_TABLE (upper_table), label, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  ScopeTextCombo = geda_combo_box_text_new_with_entry();
  geda_combo_box_text_set_activate_default(GEDA_COMBO_BOX_TEXT(ScopeTextCombo), TRUE);
  gtk_widget_show (ScopeTextCombo);
  gtk_table_attach (GTK_TABLE (upper_table), ScopeTextCombo, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  label = geda_aligned_visible_label_new (_LABEL(ScopeNumber), 0, 0.5);
  gtk_table_attach (GTK_TABLE (upper_table), label, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  ScopeNumberMenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(ScopeNumberMenu),
                           autonumber_create_scope_menu (w_current));
  gtk_table_attach_defaults(GTK_TABLE(upper_table), ScopeNumberMenu, 1, 2, 1, 2);
  gtk_widget_show (ScopeNumberMenu);

  label = geda_aligned_visible_label_new (_LABEL(ScopeSkip), 0, 0.5);
  gtk_table_attach (GTK_TABLE (upper_table), label, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  ScopeSkipMenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(ScopeSkipMenu),
                           autonumber_create_scope_menu (w_current));
  gtk_widget_show (ScopeSkipMenu);
  gtk_table_attach (GTK_TABLE (upper_table), ScopeSkipMenu, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   "border-width", 5,
                                   "homogeneous",  FALSE,
                                   "spacing",      10,
                                    NULL));

  gtk_container_add (GTK_CONTAINER (upper_vbox), hbox);
  g_object_set (hbox, "visible", TRUE, NULL);

  autonumber_create_filter_options (ThisDialog, autotext, hbox);

  GTK_SWITCH(upper_vbox, ScopeOverwrite, 6, FALSE)

  /* Options section */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME, "label", "", NULL));
  gtk_box_pack_start(GTK_BOX(main_vbox), frame, FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  label = geda_aligned_visible_label_new(_("<b>Options</b>"), 0, 0);
  geda_label_widget_set_use_markup(label, TRUE);
  gtk_frame_set_label_widget (GTK_FRAME(frame), label);

  alignment = gtk_alignment_new (0, 0, 1, 1);
  g_object_set (alignment, "visible", TRUE, NULL);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0,
                             DIALOG_INDENTATION, DIALOG_INDENTATION);

  lower_vbox = gtk_vbox_new (FALSE, 3);
  g_object_set (lower_vbox, "visible", TRUE, NULL);
  gtk_container_add (GTK_CONTAINER (alignment), lower_vbox);

  lower_table = gtk_table_new (2, 2, FALSE);
  gtk_widget_show (lower_table);
  gtk_box_pack_start (GTK_BOX (lower_vbox), lower_table, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (lower_table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (lower_table), DIALOG_H_SPACING);

  label = geda_aligned_visible_label_new (_LABEL(StartNumber), 0, 0.5);
  gtk_table_attach (GTK_TABLE (lower_table), label, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  GEDA_NUMERIC_SPIN(StartNumber, 1, 0, 99999);
  gtk_table_attach (GTK_TABLE (lower_table), StartNumberSpin, 1, 2, 0, 1,
                    (GtkAttachOptions) ( GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  label = geda_aligned_visible_label_new (_LABEL(SortOrder), 0, 0.5);
  gtk_table_attach (GTK_TABLE (lower_table), label, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  SortOrderCombo = gtk_combo_box_new();
  gtk_widget_show (SortOrderCombo);
  gtk_table_attach (GTK_TABLE (lower_table), SortOrderCombo, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  GTK_SWITCH(lower_vbox, DoRemoveNumber, 0, FALSE)
  GTK_SWITCH(lower_vbox, DoSlotting, 0, FALSE)

  /* Store pointers to all widgets, for use by get_widget_data(). */
  HOOKUP_GEDA_OBJECT (ScopeText,   Combo);
  HOOKUP_GEDA_OBJECT (ScopeNumber, Menu);
  HOOKUP_GEDA_OBJECT (ScopeSkip,   Menu);
  HOOKUP_GEDA_OBJECT (SortOrder,   Combo);

  return autonumber_text;
}

/*! \brief Create or restore the autonumber text dialog
 *
 *  If the function is called the first time the dialog is created.
 *  If the dialog is only in background it is moved to the foreground.
 *
 *  \param [in] w_current Pointer to the top level struct
 */
void autonumber_text_dialog(GschemToplevel *w_current)
{
  static AUTONUMBER_TEXT *autotext = NULL;

  if(autotext == NULL) {
    /* first call of this function, to allocate and init our structure */
    autotext=autonumber_init_state(w_current);
  }

  /* set the GschemToplevel always. Can it be changed between the calls??? */
  autotext->w_current = w_current;

  if(autotext->dialog == NULL) {

    /* Dialog is not currently displayed - create it */
    autotext->dialog = autonumber_create_dialog(w_current, autotext);

    autonumber_sortorder_create(w_current);

    gtk_dialog_set_default_response (GTK_DIALOG (autotext->dialog),
                                     GSCHEM_RESPONSE_ACCEPT);

    g_signal_connect (G_OBJECT (autotext->dialog), "response",
                      G_CALLBACK (autonumber_text_response),
                      autotext);

    restore_dialog_values(autotext);

    gtk_widget_show_all(autotext->dialog);
  }

  /* if the dialog is in the background or minimized: show it */
  gtk_window_present(GTK_WINDOW(autotext->dialog));
}
/** @} end group Auto-Number-Dialog */
#undef ThisDialog
#undef Switch_Responder