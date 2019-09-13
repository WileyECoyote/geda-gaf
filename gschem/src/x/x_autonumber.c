/* -*- C x_autonumber.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_autonumber.c
 * \brief A dialog box for Automatomating Text Numbering
 */

#include <ctype.h>

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#include <geda_keysyms.h>
#include <geda_widgets.h>

#include <geda_debug.h>

#define ThisDialog autonumber_text
#define Switch_Responder switch_responder

/** \defgroup Auto-Number-Dialog Auto Number Dialog
 *  @{
 *  \ingroup Standard-Dialogs Editing-Dialogs
 *  \image html auto_number_dialog.png
 *  \image latex auto_number_dialog.png
 *  \par
 *  The Auto Number Dialog is a modeless dialog derived from
 *  #GschemDialogClass and is primarily used to edit reference
 *  designators, particularly for automatically numbering but
 *  can also be used to auto-number other types of attributes.
 *  @} endgroup Auto-Number-Dialog
 *
 *  \defgroup Auto-Number-Dialog-Module Auto Number Dialog Module
 *  @{
 *  \ingroup Auto-Number-Dialog
 *  \par This group contains routines for the Auto Number dialog.
 */

/*! \def AUTONUM_HISTORY_LENGTH
 *  \brief How many entries to keep in the "Search text" combo box. */
#define AUTONUM_HISTORY_LENGTH  15

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

/*! \typedef IDE_AN_ControlID Enumerated Control IDs
 *  \memberof GschemDialog
 */
typedef enum IDE_AN_ControlID {
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
  { "scope_text",      N_("Search for:"),                 N_("Set the search criteria")},
  { "scope_number",    N_("Autonumber text in:"),         N_("Set the context of the search scope")},
  { "scope_skip",      N_("Skip numbers found in:"),      N_("Set where NOT to look for numbers")},
  { "sort_order",      N_("Sort order:"),                 N_("Select the orientation of the Scan method")},
  { "opt_startnum",    N_("Starting number:"),            N_("Set the starting number for the current scan")},
  { "opt_removenum",   N_("    Remove numbers:"),         N_("Remove existing reference numbers")},
  { "opt_slotting",    N_("Automatic slotting:"),         N_("Automatic slotting")},
  { "scope_overwrite", N_("Overwrite existing numbers:"), N_("Overwrite existing numbers")},
  { NULL, NULL, NULL},
};

static char *unset_text[] = {
  "refdes=*",
  "refdes=C?",
  "refdes=D?",
  "refdes=I?",
  "refdes=J?",
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
  "refdes=J*",
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

  /*! \brief Overwrite text in scope matching exact */
  char *scope_exact;

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

  /*! \brief Variables used while autonumbering */
  char  *current_searchtext;
  GList *used_numbers;         /*!< list of used numbers */
  GList *free_slots;           /*!< list of FREE_SLOT objects */
  GList *used_slots;           /*!< list of USED_SLOT objects */
  int    root_page;            /*!< flag whether its the root page or not */
};

typedef struct autonumber_slot_t AUTONUMBER_SLOT;

struct autonumber_slot_t {
  char *symbolname;     /*!< or should we use the device name? (Werner) */
  int number;           /*!< usually this is the refdes number */
  int slotnr;           /*!< just the number of the free slot */
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
 *  \ingroup Auto-Number-Dialog
 */

/* ******************* compare functions for g_list_sort, ****************** */
/*!
 * \brief GCompareFunc function to sort a list with g_list_sort().
 * \par Function Description
 *  Compares the integer values of the const pointers a and b.
 * \return -1 if a<b, 1 if a>b, 0 if a==b
 */
static int autonumber_sort_numbers(const void *a, const void *b) {
  if (POINTER_TO_INT(a) < POINTER_TO_INT(b))
    return -1;
  if (POINTER_TO_INT(a) > POINTER_TO_INT(b))
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location
 *  \par Function Description
 *  This Funcion takes two <B>GedaObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  The Function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_xy(const void *a, const void *b) {

  GedaObject *aa, *bb;

  aa = (GedaObject*) a;
  bb = (GedaObject*) b;

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

/*!
 * \brief GCompareFunc function to sort text objects by there location
 * \par Function Description
 *  This funcion takes two <B>GedaObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_xy_rev(const void *a, const void *b) {
  GedaObject *aa, *bb;
  aa=(GedaObject*) a;  bb=(GedaObject*) b;
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
 *  This funcion takes two <B>GedaObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_yx(const void *a, const void *b) {
  GedaObject *aa, *bb;
  aa=(GedaObject*) a;  bb=(GedaObject*) b;
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

/*!
 * \brief GCompareFunc function to sort text objects by there location
 * \par Function Description
 *  This Funcion takes two <B>GedaObject*</B> arguments and compares the
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_yx_rev(const void *a, const void *b) {
  GedaObject *aa, *bb;
  aa=(GedaObject*) a;  bb=(GedaObject*) b;
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

/*!
 * \brief GCompareFunc function to sort text objects by there location
 * \par Function Description
 *  This Funcion takes two <B>GedaObject*</B> arguments and compares the
 *  location of the two text objects. The sort criteria is the combined
 *  x- and the y-locations. The function sorts from top left to bottom
 *  right. The function is used as GCompareFunc by g_list_sort().
 */
static int autonumber_sort_diagonal(const void *a, const void *b) {
  GedaObject *aa, *bb;
  aa=(GedaObject*) a;  bb=(GedaObject*) b;
  if (aa->text->x - aa->text->y < bb->text->x - bb->text->y)
    return -1;
  if (aa->text->x - aa->text->y > bb->text->x - bb->text->y)
    return 1;
  return 0;
}

/** @} end group Auto-Number-Sorters */

/*!
 * \brief GCompareFunc function to acces <B>AUTONUMBER_SLOT</B> object in a GList
 * \par Function Description
 *  This Funcion takes two <B>AUTONUMBER_SLOT*</B> arguments and compares them.
 *  Sorting criteria is are the AUTONUMBER_SLOT members: first the symbolname,
 *  than the number and last the slotnr. If the number or the slotnr is set to
 *  zero it acts as a wildcard. The function is used as GCompareFunc by GList
 *  functions.
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

/*!
 * \brief Prints a <B>GList</B> of <B>AUTONUMBER_SLOT</B> elements
 * \par Function Description
 *  This funcion prints the elements of a GList that contains <B>AUTONUMBER_SLOT</B>
 *  elements. This function is is only used for debugging purposes.
 *
 * \param [in] list   Pointer to list to print
 */
void freeslot_print(GList *list) {

  GList *item;

  printf("freeslot_print(): symname, number, slot\n");

  for (item = list; item != NULL; item = g_list_next(item)) {

    AUTONUMBER_SLOT *fs = item ->data;
    printf("  %s, %d, %d\n",fs->symbolname, fs->number, fs->slotnr);
  }
}

/*!
 * \brief Function to Clear the Databases of used parts
 * \par Function Descriptions
 *  Just remove the list of used numbers, used slots and free slots.
 *
 * \param [in] autotext   Pointer to <B>AUTONUMBER_TEXT</B> data structure
 */
static void autonumber_clear_database (AUTONUMBER_TEXT *autotext)
{
  /* cleanup everything for the next searchtext */
  if (autotext->used_numbers != NULL) {
    g_list_free(autotext->used_numbers);
    autotext->used_numbers = NULL;
  }

  if (autotext->free_slots != NULL) {
    geda_glist_free_full(autotext->free_slots, g_free);
    autotext->free_slots = NULL;
  }

  if (autotext->used_slots != NULL) {
    geda_glist_free_full(autotext->used_slots, g_free);
    autotext->used_slots = NULL;
  }
}

/*!
 * \brief Function to test, whether the Object matches the autotext criteria
 * \par Function Description
 * The criteria are those of the autonumber text dialog. The function decides
 * whether the <B>Object</B> has to be renumberd, ignored or taken care of when
 * renumbering all other objects.
 * \return one of these integer values:
 *  <DL>
 *    <DT><B>AUTONUMBER_IGNORE</B></DT>
 *    <DT><B>AUTONUMBER_RESPECT</B>and the text number of the object in \a number </DT>
 *    <DT><B>AUTONUMBER_RENUMBER</B></DT>
 *  </DL>
 *
 * \param [in] autotext   Pointer to <B>AUTONUMBER_TEXT</B> data structure
 * \param [in] o_current  Is an attribute GedaTextObject to be interrogated
 *
 * \param [out] number    integer value that is to be the designation*
 *
 * \note *number is not modified unless the returned value is AUTONUMBER_RESPECT
 */
static int autonumber_match(AUTONUMBER_TEXT *autotext, GedaObject *o_current, int *number)
{
  const char *str;
  const char *searchtext;
  int  len;
  bool isnumbered = TRUE;

  /* Check if object is a Text object */
  if (o_current->type != OBJ_TEXT)
    return AUTONUMBER_IGNORE;          /* ignore if not a text object */

  /* autotext->current_searchtext looks like "refdes=R" */
  searchtext = autotext->current_searchtext;
  len        = strlen (searchtext);
  str        = geda_text_object_get_string (o_current);

  /* Check if the search text length is greater than the string length */
  if (len > strlen(str))
    return AUTONUMBER_IGNORE;         /* ignore if looking for longer string */

  /* Check if the string contains the search text */
  if (!g_str_has_prefix(str, searchtext))
    return AUTONUMBER_IGNORE;         /* ignore if search text not in string */

  /* the string object matches with its leading characters to the searchtext */
  /* now look for the extension, either a number or the "?" */
  if (g_str_has_suffix (str,"?")) {

    isnumbered = 0;

    /* There must not be any characters between the "?" and the searchtext */
    if (strlen(str) != len + 1) {
      return AUTONUMBER_IGNORE;
    }
  }
  else {

    /* Check if the SEARCH has digits */
    if (!isdigit(searchtext[len - 1]))  {
      if (!isdigit(str[len])) { /* has at least one digit */
        return AUTONUMBER_IGNORE;
      }
    }
    else {

      /* str has matching prefix but searchtext does not have a digit,
       * respect if strings are not an exact match */
      if (strcmp(str, searchtext)) {
        /* The two strings do not exactly match */
        return AUTONUMBER_RESPECT;
      }
    }
  }

  /* we have six cases, 3 from focus multiplied by 2 selection cases */
  if ((autotext->root_page || autotext->scope_number == SCOPE_HIERARCHY) &&
      (o_current->selected || autotext->scope_number == SCOPE_HIERARCHY ||
       autotext->scope_number == SCOPE_PAGE) &&
      (!isnumbered || (autotext->scope_overwrite)))
  {

    if (!autotext->scope_exact) {
      return AUTONUMBER_RENUMBER;
    }

    if (!strcmp(str, autotext->scope_exact)) {
      return AUTONUMBER_RENUMBER;
    }
  }

  if (isnumbered &&
    !(autotext->scope_skip == SCOPE_SELECTED &&
    !(o_current->selected) && autotext->root_page))
  {
    sscanf(&(str[len])," %d", number);
    return AUTONUMBER_RESPECT; /* numbered objects which we don't renumber */
  }

  return AUTONUMBER_IGNORE;    /* unnumbered objects outside the focus */
}

/*!
 * \brief Creates a list of already numbered objects and slots
 * \par Function Description
 *  This function collects the used numbers of a single schematic page.
 *  The used element numbers are stored in a GList container inside the
 *  <B>AUTONUMBER_TEXT</B> struct. The slotting container is a little bit
 *  different. The container stores free slots of multislotted symbols,
 *  that were only partially used. The criterias are derivated from the
 *  autonumber dialog entries.
 *
 * \param [in] w_current Pointer to GschemToplevel data structure
 * \param [in] autotext  Pointer to <B>AUTONUMBER_TEXT</B> data structure
 */
static void autonumber_get_used(GschemToplevel *w_current, AUTONUMBER_TEXT *autotext)
{
  AUTONUMBER_SLOT *slot;
  const GList     *iter;
  GList           *slot_item;
  Page            *page;
  char            *numslot_str, *slot_str;
  int              number, numslots, slotnr, i;

  page = gschem_toplevel_get_current_page(w_current);

  for (iter = geda_struct_page_get_objects (page); iter != NULL; NEXT(iter)) {

    GedaObject *o_current = iter->data;

    if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RESPECT) {

      /* check slot and maybe add it to the lists */
      GedaObject *o_parent = geda_object_get_attached_to (o_current);

      if (autotext->slotting && o_parent != NULL) {

        /* check for slotted symbol */
        numslot_str = geda_attrib_search_object_by_name (o_parent, "numslots", 0);

        if (numslot_str != NULL) {

          sscanf(numslot_str," %d",&numslots);
          GEDA_FREE(numslot_str);

          if (numslots > 0) {

            slot_str = geda_attrib_search_object_by_name (o_parent, "slot", 0);

            if (slot_str == NULL) {
              u_log_message(_("slotted object without slot attribute may cause "
                              "problems when autonumbering slots\n"));
            }
            else {

              sscanf(slot_str, " %d", &slotnr);

              slot = geda_malloc(sizeof(AUTONUMBER_SLOT));
              slot->number = number;
              slot->slotnr = slotnr;
              slot->symbolname = geda_complex_get_filename (o_parent->complex);

              slot_item = g_list_find_custom(autotext->used_slots,
                                             slot,
                                             (GCompareFunc) freeslot_compare);
              if (slot_item != NULL) { /* duplicate slot in used_slots */
                u_log_message(_("duplicate slot may cause problems: "
                                "[symbol name=%s, number=%d, slot=%d]\n"),
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
                                                    INT_TO_POINTER(number),
                                                    (GCompareFunc) autonumber_sort_numbers);
    }
  }
}

/*!
 * \brief Gets or generates free numbers for the autonumbering process.
 * \par Function Description
 *  This function gets or generates new numbers for the <B>Object o_current</B>.
 *  It uses the element numbers <B>used_numbers</B> and the list of the free slots
 *  <B>free_slots</B> of the <B>AUTONUMBER_TEXT</B> struct. The new number is
 *  returned into the <B>number</B> parameter. <B>slot</B> is set if autoslotting
 *  is active, else it is set to zero.
 *
 * \param [in]  autotext  Pointer to <B>AUTONUMBER_TEXT</B> state data structure
 * \param [in]  o_current Is an attribute GedaTextObject
 *
 * \param [out] number    integer value that is to be the designation
 * \param [out] slot      integer value is the slot id the parent object should be
 *
 * \returns designation and slot number
 */
static void autonumber_get_new_numbers(AUTONUMBER_TEXT *autotext,
                                       GedaObject      *o_current,
                                       int             *number,
                                       int             *slot)
{
  AUTONUMBER_SLOT *freeslot;

  GedaObject *o_parent;
  GList  *freeslot_item;
  GList  *item;
  int     new_number;

  new_number = autotext->startnum;

  /* Check for slots first */
  /* 1. are there any unused slots in the database? */
  o_parent = geda_object_get_attached_to (o_current);

  if (autotext->slotting && o_parent != NULL) {

    freeslot = geda_malloc(sizeof(AUTONUMBER_SLOT));
    freeslot->symbolname = geda_complex_get_filename(o_parent->complex);
    freeslot->number = 0;
    freeslot->slotnr = 0;
    freeslot_item = g_list_find_custom(autotext->free_slots,
                                       freeslot,
                                       (GCompareFunc) freeslot_compare);
    GEDA_FREE(freeslot);

    /* Yes! -> remove from database, apply it */
    if (freeslot_item != NULL) {

      freeslot = freeslot_item->data;
      *number  = freeslot->number;
      *slot    = freeslot->slotnr;

      GEDA_FREE(freeslot);
      autotext->free_slots = g_list_delete_link(autotext->free_slots, freeslot_item);

      return;
    }
  }

  /* get a new number */
  item = autotext->used_numbers;

  while (1) {

    while (item != NULL && POINTER_TO_INT(item->data) < new_number)
      item = g_list_next(item);

    if (item == NULL || POINTER_TO_INT(item->data) > new_number)
      break;
    else  /* new_number == item->data */
      new_number++;
  }

  *number = new_number;
  *slot = 0;

  /* insert the new number to the used list */
  autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
                                                INT_TO_POINTER(new_number),
                                                (GCompareFunc) autonumber_sort_numbers);

  /* 3. is o_current a slotted object ? */
  if ((autotext->slotting) && o_parent != NULL) {

    char *numslot_str;
    int   numslots;

    numslot_str = geda_attrib_search_object_by_name (o_parent, "numslots", 0);

    if (numslot_str != NULL) {

      sscanf(numslot_str," %d",&numslots);
      GEDA_FREE(numslot_str);

      if (numslots > 0) {

        int i;

        /* Yes! -> new number and slot=1; add the other slots to the database */
        *slot = 1;

        for (i = 2; i <=numslots; i++) {
          freeslot = geda_malloc(sizeof(AUTONUMBER_SLOT));
          freeslot->symbolname = geda_complex_get_filename(o_parent->complex);
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

/*!
 * \brief Removes the number from the element.
 * \par Function Description
 *  This function updates the text content of the \a o_current object.
 *
 * \param [in] autotext  Pointer to the state structure
 * \param [in] o_current Pointer to the object from which to remove the number
 */
static void autonumber_remove_number(AUTONUMBER_TEXT *autotext, GedaObject *o_current)
{
  char       *str;

  /* allocate memory for the search string*/
  str = malloc(strlen(autotext->current_searchtext) + 2); /* allocate space */

  /* make copy of the search string and append "?" */
  strcpy(str, autotext->current_searchtext);

  strcat(str, "?");

  /* replace old text */
  geda_text_object_set_string (o_current, str);
  free (str);

  /* if slotting is active then remove the slot attribute */
  if (autotext->slotting) {

    /* get the slot attribute */
    GedaObject *o_parent = o_current->attached_to;

    /* Does child->parent_object->child make sense? */
    if (o_parent != NULL) {

      GedaObject *o_slot;

      /* geda_struct_slot_search_slot updates o_slot variable */
      g_free (geda_struct_slot_search_slot (o_parent, &o_slot));

      /* Only attempt to remove non-inherited slot attributes */
      if (o_slot != NULL && !geda_attrib_is_inherited (o_slot)) {

        /* delete the slot attribute */
        o_delete (autotext->w_current, o_slot);
      }
    }
  }
}

/*!
 * \brief Changes the number <B>Object</B> element. Changes the slot attribute.
 * \par Function Description
 *  This function updates the text content of the <B>\a o_current</B> object
 *  if the <B>slot</B> value is non zero. The slot attribute of the complex
 *  element that is also the parent object of the o_current element is also
 *  updates.
 *
 * \param [in] autotext  Pointer to <B>AUTONUMBER_TEXT</B> state data structure
 * \param [in] o_current Is an attribute GedaTextObject
 * \param [in] number    integer value that is to be the designation
 * \param [in] slot      integer value is the slot id of the parent object
 */
/* \note 11/04/12 WEH: Revised to eliminate gmalloc of trivial strings (twice).
 *       added conditional so that slot=0 is not passed to o_slot_end function
 *       even though slot=0 is valid, it just means the component has none.
 */
static void autonumber_apply_new_text(AUTONUMBER_TEXT *autotext,
                                      GedaObject      *o_current,
                                      int              number,
                                      int              slot)
{
  char string[32]="slot=";  /* char buffer to hold set=refdes=xx*/
  char s_val[5];            /* char buffer for integer conversion to string */

  if (slot > 0) {
    /* update the slot on the owner object */
    geda_utility_string_int2str( slot, &string[5], 10);
    o_slot_end (autotext->w_current, o_current->attached_to, string);
  }

  /* replace old text, looks like "set=refdes=U1"*/
  strcpy(string, autotext->current_searchtext);
  strcat(string, geda_utility_string_int2str( number, s_val, 10));
  geda_text_object_set_string (o_current, string);
}

/*!
 * \brief Handles all the options of the autonumber text dialog
 * \par Function Description
 *  This function is the main routine for the autonumber code. The function
 *  retrieves options from the \a autotext structure. First it collects all
 *  pages of a hierarchical schematic, even if hierarchy info is not used.
 *  The function retrieves all matching text elements for the searchtext and
 *  then renumbers text elements based on options from the dialog.
 *
 * \param [in] autotext  Pointer to <B>AUTONUMBER_TEXT</B> state data structure
 */
static void autonumber_text_autonumber(AUTONUMBER_TEXT *autotext)
{
  GschemToplevel *w_current;
  GedaObject     *o_current;

  const GList *iter;
  GList *o_list          = NULL;
  GList *pages;
  GList *searchtext_list = NULL;
  GList *text_item;
  GList *obj_item;
  GList *page_item;

  char *searchtext;
  char *scope_text;
  char *new_searchtext;

  int i, number;
  int slot, scope_len;

  w_current                    = autotext->w_current;
  autotext->current_searchtext = NULL;
  autotext->root_page          = 1;
  autotext->scope_exact        = NULL;

  autotext->used_numbers       = NULL;
  autotext->free_slots         = NULL;
  autotext->used_slots         = NULL;

  /* Step 1: Retrieve the Scope Search text */
  scope_text = g_list_first(autotext->scope_history)->data;

  /* Step 2: Get all pages of the hierarchy */
  pages = geda_hierarchy_traverse_pages (w_current->toplevel,
                                         Current_Page,
                                         HIERARCHY_NODUPS);

  /*  g_list_foreach(pages, (GFunc) geda_hierarchy_print_page, NULL); */

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
    u_log_message(_("No search string given in autonumber text.\n"));
    return; /* error */
  }
  else if (g_str_has_suffix(scope_text,"?") == TRUE) {
    /* single searchtext, strip of the "?" */
    searchtext = geda_strndup(scope_text, scope_len - 1);
    searchtext_list=g_list_append (searchtext_list, searchtext);
  }
  else if (g_str_has_suffix(scope_text,"*") == TRUE) {

    /* strip of the "*" */
    searchtext = geda_strndup(scope_text, scope_len - 1);

    /* collect all the possible searchtexts in all pages of the hierarchy */
    for (page_item = pages; page_item != NULL; NEXT(page_item)) {

      Page *p_current;

      geda_struct_page_goto(page_item->data);

      p_current = geda_toplevel_get_current_page(w_current->toplevel);

      /* iterate over all objects an look for matching searchtext's */
      for (iter = geda_struct_page_get_objects(p_current); iter != NULL; NEXT(iter))
      {
        o_current = iter->data;
        if (o_current->type == OBJ_TEXT) {
          if (autotext->scope_number == SCOPE_HIERARCHY
            || autotext->scope_number == SCOPE_PAGE
            || ((autotext->scope_number == SCOPE_SELECTED) && (o_current->selected)))
          {

            const char *str = geda_text_object_get_string (o_current);

            if (g_str_has_prefix (str, searchtext)) {

              int search_len;

              /* the beginning of the current text matches with the searchtext now */
              /* strip off the trailing [0-9?] chars and add it too the searchtext */

              search_len = strlen(searchtext);
              i = strlen (str)-1;

              /* Skip in trailing character ( the ones we realy should save */
              while ((i >= search_len + 1) && isalpha((str[i]))) i--;
              while ((i >= search_len) && (str[i] == '?' || isdigit( (int) (str[i]) ))) i--;

              new_searchtext = geda_strndup (str, i + 1);

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
      if (autotext->scope_number == SCOPE_SELECTED ||
          autotext->scope_number == SCOPE_PAGE)
        break; /* search only in the first page */
    }
    GEDA_FREE(searchtext);
  }
  else {

    /* Searching for specified text, which may or may not contain digits */

    int pos = scope_len - 1;

    /* Look for trailing digits */
    while (pos && isdigit(scope_text[pos])) {
      pos--;
    }

    /* Check if found digits */
    if (pos && !(pos == scope_len - 1)) {
      pos++;           /* Increment to first digit */
      searchtext      = geda_strndup(scope_text, pos);
      searchtext_list = g_list_append (searchtext_list, searchtext);
      autotext->scope_exact = scope_text;
    }
    else {

      /* Numbering plain text, just make a copy of the string */
      searchtext      = geda_strdup(scope_text);
      searchtext_list = g_list_append (searchtext_list, searchtext);
    }
  }

  /* Step 4: iterate over the search items in the list */
  for (text_item = searchtext_list; text_item != NULL; NEXT(text_item)) {

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
          geda_struct_page_goto(page_item->data);
          autonumber_get_used(w_current, autotext);
        }
      }
    }

    /* renumber the elements */
    for (page_item = pages; page_item != NULL; NEXT(page_item)) {

      Page *p_current;

      geda_struct_page_goto(page_item->data);

      autotext->root_page = (pages->data == page_item->data);

      /* build a page database if we're numbering page by page or selection only */
      if (autotext->scope_skip == SCOPE_PAGE ||
          autotext->scope_skip == SCOPE_SELECTED)
      {
        autonumber_get_used(w_current, autotext);
      }

      p_current = geda_toplevel_get_current_page(w_current->toplevel);

      /* RENUMBER CODE FOR ONE Page AND ONE SEARCHTEXT*/
      /* 1. get objects to renumber */
      for (iter = geda_struct_page_get_objects(p_current); iter != NULL; NEXT(iter))
      {
        o_current = iter->data;

        if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RENUMBER)
        {
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
      for (obj_item = o_list; obj_item != NULL; NEXT(obj_item)) {

        o_current= obj_item->data;

        if (autotext->removenum) {
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
  geda_glist_free_full(searchtext_list, g_free);
  geda_struct_page_goto(pages->data); /* go back to the root page */
  o_invalidate_all (w_current);
  g_list_free(pages);
  o_undo_savestate(w_current, UNDO_ALL);
}

/*!
 * \brief Put the icons and the text into the SortOrder combobox
 * \par Function Description
 *  Load all bitmaps for the combobox and store them together with the label
 *  in a GtkListStore.
 *
 * \param w_current Pointer to GschemToplevel data structure
 */
static void autonumber_sortorder_create(GschemToplevel *w_current)
{
  GtkListStore    *store;
  GtkCellRenderer *renderer;
  int i;

  /* Note the next two arrays must be in the same order */
  char *filenames[] = { "gschem_diagonal.png",
                        "gschem_top2bottom.png",
                        "gschem_bottom2top.png",
                        "gschem_left2right.png",
                        "gschem_left2right.png",
                        "gschem_fileorder.png",
                        NULL};

  char *names[] = { N_("Diagonal"),
                    N_("Top to bottom"),
                    N_("Bottom to top"),
                    N_("Left to right"),
                    N_("Right to left"),
                    N_("File order"),
                    NULL};

  store = gtk_list_store_new(2, G_TYPE_STRING, GDK_TYPE_PIXBUF);

  for (i = 0; filenames[i] != NULL; i++) {

    GError    *error;
    GdkPixbuf *pixbuf;
    char      *path;

    GtkTreeIter iter;

    error  = NULL;
    path   = geda_file_get_bitmap_filespec(filenames[i]);
    pixbuf = gdk_pixbuf_new_from_file(path, &error);
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 0, _(names[i]), 1, pixbuf, -1);
    GEDA_FREE(path);
  }

  geda_combo_widget_set_model(SortOrderCombo, (GtkTreeModel*)store);

  renderer = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start ((GtkCellLayout*)SortOrderCombo, renderer, TRUE);

  gtk_cell_layout_set_attributes ((GtkCellLayout*)SortOrderCombo,
                                  renderer, "text", 0, NULL);

  renderer = gtk_cell_renderer_pixbuf_new();

  g_object_set(renderer, "xpad", 5, "ypad", 5, NULL);

  gtk_cell_layout_pack_start ((GtkCellLayout*)SortOrderCombo, renderer, FALSE);

  gtk_cell_layout_set_attributes ((GtkCellLayout*)SortOrderCombo,
                                  renderer, "pixbuf", 1, NULL);
}

/* ***** STATE STRUCT HANDLING (interface between GUI and backend code) **** */

/*!
 * \brief Adds a line to the search text history list
 * \par Function Description
 *  This function prepends \a text to the list \a history and removes
 *  AND releases \a text from any other position in the \a history if
 *  present. The length of the list is checked and truncated if longer
 *  than #AUTONUM_HISTORY_LENGTH.
 * \par
 *  <DL>
 *    <DT>1) Removes \a text from the list</DT>
 *    <DT>2) Prepends \a text to the list list</DT>
 *    <DT>3) Truncates \a history if longer than #AUTONUM_HISTORY_LENGTH</DT>
 *  </DL>
 * \param [in] history GList to be prepended with \a text
 * \param [in] text    The text string to be prepended to the list
 */
static GList *autonumber_add_history(GList *history, char *text)
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
  while(g_list_length(history) > AUTONUM_HISTORY_LENGTH) {

    GList *last = g_list_last(history);

    history = g_list_remove_link(history, last);

    GEDA_FREE(last->data);
    g_list_free(last);
  }

  return history;
}

/*!
 * \brief Set the Scope Filter Text to the History option
 * \par Function Description
 *  Clears the ScopeTextCombo and reloads the combo with values
 *  from the scope_history list. The combo entry is set to the
 *  first value in the list. The value of last_criteria in the
 *  state \a data structure is set to SCOPE_HISTORY so that the
 *  same list will be restored if the dialog is reloaded.
 *
 * \param [in] button Pointer to Widget that was Activated (bulb) or NULL
 * \param [in] data   Is a pointer to data structure
 */
static void set_scope_filter_text_history (GtkWidget *button, void *data)
{
  AUTONUMBER_TEXT  *autotext = data;
  GedaComboBoxText *combo    = (GedaComboBoxText*)ScopeTextCombo;

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

/*!
 * \brief Set the Scope Filter Text to the Question type
 * \par Function Description
 *  Clears the ScopeTextCombo and reloads the combo with values
 *  in the structure #unset_text. The combo entry is set to the
 *  first value in the structure. The value of last_criteria in
 *  the state \a data structure is set to SCOPE_QUESTION so the
 *  same list will be restored if the dialog is reloaded.
 *
 * \param [in] button Pointer to Widget that was Activated (bulb) or NULL
 * \param [in] data   Is a pointer to data structure
 */
static void set_scope_filter_text_question (GtkWidget *button, void *data)
{
  AUTONUMBER_TEXT  *autotext = data;
  GedaComboBoxText *combo    = (GedaComboBoxText*)ScopeTextCombo;

  int index;

  geda_combo_box_text_remove_all(combo);

  for (index = 0; unset_text[index] != NULL; index++) {

    geda_combo_box_text_append(combo, unset_text[index]);

  }

  SetEntryText( combo->entry, unset_text[0]);

  autotext->last_criteria = SCOPE_QUESTION;
}

/*!
 * \brief Set the Scope Filter Text to the Wild type
 * \par Function Description
 *  Clears the ScopeTextCombo and reloads the combo with values
 *  in the structure #wild_text. The combo entry is set to the
 *  first value in the structure. The value of last_criteria in
 *  the state \a data structure is set to SCOPE_WILD so that the
 *  same list will be restored if the dialog is reloaded.
 *
 * \param [in] button Pointer to Widget that was Activated (bulb) or NULL
 * \param [in] data   Is a pointer to data structure
 */
static void set_scope_filter_text_wild (GtkWidget *button, void *data)
{
  AUTONUMBER_TEXT  *autotext = data;
  GedaComboBoxText *combo    = (GedaComboBoxText*)ScopeTextCombo;

  int index;

  geda_combo_box_text_remove_all(combo);

  for (index = 0; wild_text[index] != NULL; index++) {

    geda_combo_box_text_append(combo, wild_text[index]);

  }

  SetEntryText( combo->entry, wild_text[0]);

  autotext->last_criteria = SCOPE_WILD;
}

/*!
 * \brief Allocate and initialize the state structure
 * \par Function Description
 *  Creates and returns a new <B>AUTONUMBER_TEXT</B> structure.
 *  Values in the structure are initialized to default or set to
 *  trigger the defaults to be load later.
 *
 * \param [in] w_current Pointer to the top level struct
 *
 * \return Pointer to the allocated structure or NULL on error.
 */
static AUTONUMBER_TEXT *autonumber_init_state(GschemToplevel *w_current)
{
  AUTONUMBER_TEXT *autotext;

  /* Default contents of the combo box history */

  autotext = geda_malloc(sizeof(AUTONUMBER_TEXT));

  if(autotext==NULL) return NULL;

  autotext->scope_history = NULL;

  autotext->last_criteria = SCOPE_QUESTION;

  if (Current_Page->hierarchy_up < 0) {

    autotext->scope_skip = SCOPE_PAGE;

    if (o_select_is_selection(w_current)) {
      autotext->scope_number = SCOPE_SELECTED;
    }
    else {
      autotext->scope_number = SCOPE_PAGE;
    }
  }
  else {
    autotext->scope_skip   = SCOPE_HIERARCHY;
    autotext->scope_number = SCOPE_HIERARCHY;
  }

  autotext->scope_overwrite = FALSE;
  autotext->order           = AUTONUMBER_SORT_DIAGONAL;
  autotext->startnum        = 1;
  autotext->removenum       = FALSE;
  autotext->slotting        = FALSE;
  autotext->dialog          = NULL;

  return autotext;
}

/*!
 * \brief Restore the settings for the autonumber text dialog
 * \par Function Description
 *  Retrieves values from control in the dialog and save the
 *  values to the \a autotext <B>AUTONUMBER_TEXT</B> structure.
 *
 * \param [in] autotext  Pointer to state data structure
 */
static void restore_dialog_values(AUTONUMBER_TEXT *autotext)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList    *scope_group;

  /* Scope */
  scope_group = GEDA_OBJECT_GET_DATA(autotext->dialog, "ScopeGroup");

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
  geda_option_menu_set_history((GedaOptionMenu*)ScopeNumberMenu, autotext->scope_number);
  menu = geda_option_menu_get_menu((GedaOptionMenu*)ScopeNumberMenu);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active((GedaCheckMenuItem*)menuitem, TRUE);

  /* Set the ScopeSkip ComboMenu with the value in autotext->scope_skip */
  geda_option_menu_set_history((GedaOptionMenu*)ScopeSkipMenu, autotext->scope_skip);
  menu = geda_option_menu_get_menu((GedaOptionMenu*)ScopeSkipMenu);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active((GedaCheckMenuItem*)menuitem, TRUE);

  SetSwitch (ScopeOverwrite, autotext->scope_overwrite);

  /* Options */
  SetSpin (StartNumber, autotext->startnum);

  SetGedaCombo (SortOrder, autotext->order);

  SetSwitch (DoRemoveNumber, autotext->removenum);

  SetSwitch (DoSlotting, autotext->slotting);

}

/*!
 * \brief Get the settings from the autonumber text dialog
 * \par Function Description
 *  Retrieves values from control in the dialog and save the
 *  values to the \a autotext <B>AUTONUMBER_TEXT</B> structure.
 *
 * \param [in] autotext  Pointer to data structure
 */
static void retrieve_values_from_dialog(AUTONUMBER_TEXT *autotext)
{
  char *text;

  /* Scope */

  /* Note we do not have to retrieve autotext->last_criteria here because
   * the callbacks did this whenever a change was made.
   */

  /* Search text history */
  text = GetGedaComboActiveText(ScopeText);

  autotext->scope_history = autonumber_add_history(autotext->scope_history, text);

  /* Retrieve scope_number selection from ScopeNumberMenu Combo/Menu */
  autotext->scope_number = POINTER_TO_INT(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_menu_get_menu (
            (GedaOptionMenu*)ScopeNumberMenu)), "scope_menu"));

  /* Retrieve scope_skip selection from ScopeSkipMenu Combo/Menu */
  autotext->scope_skip = POINTER_TO_INT(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_menu_get_menu (
            (GedaOptionMenu*)ScopeSkipMenu)), "scope_menu"));

  autotext->scope_overwrite = GET_SWITCH_STATE (ScopeOverwriteSwitch);

  /* Sort order */
  autotext->order = geda_combo_widget_get_active(SortOrderCombo);

  /* Options */
  autotext->startnum  = GET_SPIN_IVALUE (StartNumberSpin);
  autotext->removenum = GET_SWITCH_STATE (DoRemoveNumberSwitch);
  autotext->slotting  = GET_SWITCH_STATE (DoSlottingSwitch);
}

/*!
 * \brief Select the Scope Text Value in the Autonumber text dialog
 * \internal Helper Function Description
 *  This function selects the "value" portion of the scope text in
 *  the scope text combo entry. The entire text is selected in the
 *  entry be default, including the name portion and equal sign,
 *  normally "refdes=", this function moves the left cursor past
 *  the equal sign.
 *
 * \param [in] autotext  Pointer to data structure
 */
static void select_scope_text_value(AUTONUMBER_TEXT *autotext)
{
  char *text;
  int   pos;

  /* Get the search text from the widget */
  text = GetGedaComboActiveText(ScopeText);

  pos  = geda_utility_string_stristr (text, "=");

  if (pos) {

    GtkEditable *entry;

    pos++;  /* skip over the equal sign */

    entry = (GtkEditable*)geda_combo_widget_get_entry(ScopeTextCombo);

    gtk_editable_select_region (entry, pos, strlen(text));
  }

  g_free(text);
}

/* ***** CALLBACKS (functions that get called from GTK) ******* */

/*!
 * \brief response callback for the Autonumber text dialog
 * \par Function Description
 *  The function just closes the dialog if the close button is pressed or
 *  the user closes the dialog window. If the Apply button is pressed this
 *  function calls retrieve_values_from_dialog and then it does not make
 *  any sense.
 *
 * \param [in] widget    Pointer to Widget that was Activated.
 * \param [in] response  Integer response (basically control id)
 * \param [in] autotext  Pointer to data structure
 */
static void autonumber_text_response(GtkWidget       *widget,
                                     int              response,
                                     AUTONUMBER_TEXT *autotext)
{
  switch (response) {
    case GEDA_RESPONSE_ACCEPT:
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

    case GEDA_RESPONSE_CLOSE:
      retrieve_values_from_dialog(autotext);
    case GEDA_RESPONSE_DELETE_EVENT:
      gtk_widget_destroy(autotext->dialog);
      autotext->dialog = NULL;
      break;

    default:
      BUG_IMSG ("unhandled case", response);
  }
}

/*!
 * \brief Function to toggle switch images
 * \par Function Description
 *  This function changes the images of controls created with
 *  create_geda_switch to the opposite state, i.e. if ON use
 *  OFF image and if OFF use ON image. The functions handles
 *  callbacks for all switches on this Dialog.
 *
 *  \param [in] widget    Pointer to the GedaSwitch object.
 *  \param [in] Control   Pointer to integer Switch identifier
 */
static void switch_responder(GtkWidget *widget, ControlID *Control)
{
  GtkWidget *SwitchImage;

  bool state = GET_SWITCH_STATE (widget);

  SwitchImage = get_geda_switch_image (state);

  gtk_button_set_image((GtkButton*)widget, SwitchImage);

  int WhichOne = (int)(long)Control;

  switch ( WhichOne ) {
    case DoRemoveNumber:
      //gtk_widget_set_sensitive(ScopeOverwriteSwitch, state);
      break;

    case DoSlotting:
    case ScopeOverwrite:
      break;

    default:
      u_log_message("toggle_switch(): UKNOWN SWITCH ID: %d\n", WhichOne);
  }

  return;
}

/**************************** DIALOG SET-UP *****************************/

/*!
 * \brief Creates the Filter Option Bulbs on the Autonumber text dialog.
 * \par Function Description
 *  Constructor extension for the AutoNumber Dialog.
 *
 * \param [in] Dialog    Pointer to the AutoNumber Dialog.
 * \param [in] autotext  Pointer to AUTONUMBER_TEXT data structure
 * \param [in] container Pointer to container for the bulb widgets
 */
static inline
void autonumber_create_filter_options (GtkWidget       *Dialog,
                                       AUTONUMBER_TEXT *autotext,
                                       GtkWidget       *container)
{
  GtkWidget *widget;

  widget = geda_aligned_visible_bold_label_new(_("Filter:"), 0, 0);
  geda_label_widget_set_use_markup(widget, TRUE);
  gtk_box_pack_start ((GtkBox*)container, widget,  FALSE, FALSE, 5);
  gtk_misc_set_alignment ((GtkMisc*)widget, 0, 0.5);

  widget = geda_bulb_new_visible_with_mnemonic(NULL, _("_History"));
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  gtk_box_pack_start ((GtkBox*)container, widget,  FALSE, FALSE, 5);

  g_signal_connect(widget, "clicked", G_CALLBACK(set_scope_filter_text_history), autotext);

  g_object_set (widget, "show-button", TRUE, NULL);

  widget = geda_bulb_new_with_mnemonic_from_widget(widget, _("_Unset"), 1);
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  gtk_box_pack_start ((GtkBox*)container, widget,  FALSE, FALSE, 5);
  g_signal_connect(widget, "clicked", G_CALLBACK(set_scope_filter_text_question), autotext);

  widget = geda_bulb_new_with_mnemonic_from_widget(widget, _("_Wild"), 1);
  gtk_widget_set_direction(widget, GTK_TEXT_DIR_RTL);
  gtk_box_pack_start ((GtkBox*)container, widget,  FALSE, FALSE, 5);
  g_signal_connect(widget, "clicked", G_CALLBACK(set_scope_filter_text_wild), autotext);

  GEDA_OBJECT_SET_DATA(Dialog, geda_bulb_get_group(widget), "ScopeGroup");
}

/*!
 * \brief Create Scope menus for the AutoNumber dialog
 * \par Function Description
 *  This function creates a GedaMenu with different scope options.
 */
static inline
GtkWidget *autonumber_create_scope_menu (GschemToplevel *w_current)
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

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct scope_options); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));
    group    = geda_radio_menu_item_group (menuitem);
    geda_menu_append (menu, menuitem);
    GEDA_OBJECT_SET_DATA(menuitem, INT_TO_POINTER (types[i].scope), "scope_menu");
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*!
 * \brief Creates the autonumber text dialog
 * \par Function Description
 *   Constructor for the AutoNumber Dialog, the Dialog returned but is not
 *   shown.
 *
 * \param [in] w_current Pointer to the top level struct.
 * \param [in] autotext  Pointer to AUTONUMBER_TEXT data structure
 *
 * \returns Pointer to the dialog window.
 */
static
GtkWidget *autonumber_create_dialog(GschemToplevel  *w_current,
                                    AUTONUMBER_TEXT *autotext)
{
  GtkWidget *ThisDialog;
  GtkWidget *main_vbox;
  GtkWidget *alignment;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *label;
  GtkWidget *upper_vbox;
  GtkWidget *lower_vbox;
  GtkTable  *upper_table;
  GtkTable  *lower_table;

  ThisDialog = gschem_dialog_new_with_buttons(_("Autonumber text"),
                                              w_current->main_window,
            /* modal-less */                  GSCHEM_MODELESS_DIALOG,
                                              IDS_AUTONUMBER,
                                              w_current,
                                              GTK_STOCK_CLOSE, GEDA_RESPONSE_CLOSE,
                                              GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                              NULL );

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order((GtkDialog*)ThisDialog,
                                           GEDA_RESPONSE_ACCEPT,
                                           GEDA_RESPONSE_CLOSE,
                                           -1);

  gtk_window_set_position ((GtkWindow*)ThisDialog, GTK_WIN_POS_NONE);

  main_vbox = ((GtkDialog*)ThisDialog)->vbox;

  /* scope section */
  frame = g_object_new (GTK_TYPE_FRAME, "label", "", NULL);
  gtk_box_pack_start(GTK_BOX(main_vbox), frame, FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  label = geda_aligned_visible_bold_label_new (_("Scope"), 0, 0);
  geda_label_widget_set_use_markup(label, TRUE);
  gtk_frame_set_label_widget ((GtkFrame*)frame, label);

  alignment = gtk_alignment_new (0, 0, 1, 1);
  gtk_widget_show (alignment);

  geda_container_add (frame, alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0,
                             DIALOG_INDENTATION, DIALOG_INDENTATION);

  upper_vbox = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (upper_vbox);
  geda_container_add (alignment, upper_vbox);

  upper_table = (GtkTable*)gtk_table_new (3, 2, FALSE);
  gtk_widget_show ((GtkWidget*)upper_table);
  gtk_box_pack_start (GTK_BOX (upper_vbox), (GtkWidget*)upper_table, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (upper_table, DIALOG_V_SPACING);
  gtk_table_set_col_spacings (upper_table, DIALOG_H_SPACING);

  label = geda_aligned_visible_label_new (_LABEL(ScopeText), 0, 0.5);
  gtk_table_attach (upper_table, label, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  ScopeTextCombo = geda_combo_box_text_new_with_entry();
  geda_combo_box_text_widget_set_activate_default(ScopeTextCombo, TRUE);
  gtk_widget_show (ScopeTextCombo);
  gtk_table_attach (upper_table, ScopeTextCombo, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  label = geda_aligned_visible_label_new (_LABEL(ScopeNumber), 0, 0.5);
  gtk_table_attach (upper_table, label, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  ScopeNumberMenu = geda_option_menu_new ();
  geda_option_menu_set_menu((GedaOptionMenu*)ScopeNumberMenu,
                            autonumber_create_scope_menu (w_current));
  gtk_table_attach_defaults(GTK_TABLE(upper_table), ScopeNumberMenu, 1, 2, 1, 2);
  gtk_widget_show (ScopeNumberMenu);

  label = geda_aligned_visible_label_new (_LABEL(ScopeSkip), 0, 0.5);
  gtk_table_attach (upper_table, label, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  ScopeSkipMenu = geda_option_menu_new ();
  geda_option_menu_set_menu((GedaOptionMenu*)ScopeSkipMenu,
                           autonumber_create_scope_menu (w_current));
  gtk_widget_show (ScopeSkipMenu);
  gtk_table_attach (upper_table, ScopeSkipMenu, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  hbox = g_object_new (GTK_TYPE_HBOX,
                       "border-width", 5,
                       "homogeneous",  FALSE,
                       "spacing",      10,
                       NULL);

  geda_container_add (upper_vbox, hbox);
  gtk_widget_show (hbox);

  autonumber_create_filter_options (ThisDialog, autotext, hbox);

  GTK_SWITCH(upper_vbox, ScopeOverwrite, 6, FALSE)

  /* Options section */
  frame = g_object_new (GTK_TYPE_FRAME, "label", "", NULL);
  gtk_box_pack_start(GTK_BOX(main_vbox), frame, FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  label = geda_aligned_visible_bold_label_new (_("Options"), 0, 0);
  gtk_frame_set_label_widget (GTK_FRAME(frame), label);

  alignment = gtk_alignment_new (0, 0, 1, 1);
  gtk_widget_show (alignment);
  geda_container_add (frame, alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0,
                             DIALOG_INDENTATION, DIALOG_INDENTATION);

  lower_vbox = gtk_vbox_new (FALSE, 3);
  gtk_widget_show (lower_vbox);
  geda_container_add (alignment, lower_vbox);

  lower_table = (GtkTable*)gtk_table_new (2, 2, FALSE);
  gtk_widget_show ((GtkWidget*)lower_table);
  gtk_box_pack_start (GTK_BOX (lower_vbox), (GtkWidget*)lower_table, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (lower_table, DIALOG_V_SPACING);
  gtk_table_set_col_spacings (lower_table, DIALOG_H_SPACING);

  label = geda_aligned_visible_label_new (_LABEL(StartNumber), 0, 0.5);
  gtk_table_attach (lower_table, label, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  GEDA_NUMERIC_SPIN(StartNumber, 1, 0, 99999);
  gtk_table_attach (lower_table, StartNumberSpin, 1, 2, 0, 1,
                    (GtkAttachOptions) ( GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  label = geda_aligned_visible_label_new (_LABEL(SortOrder), 0, 0.5);
  gtk_table_attach (lower_table, label, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  SortOrderCombo = geda_combo_box_new();
  gtk_widget_show (SortOrderCombo);
  gtk_table_attach (lower_table, SortOrderCombo, 1, 2, 1, 2,
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

/*!
 * \brief Create or restore the autonumber text dialog
 * \par Function Description
 *  If the function is called the first time the dialog is created.
 *  If the dialog is in the background the dialog is raised to the
 *  foreground.
 *
 *  \param [in] w_current Pointer to the top level struct
 */
void autonumber_text_dialog(GschemToplevel *w_current)
{
  static AUTONUMBER_TEXT *autotext = NULL;

  if (autotext == NULL) {
    /* first call of this function, to allocate and init our structure */
    autotext = autonumber_init_state(w_current);
  }

  /* set the GschemToplevel always. Can it be changed between the calls??? */
  autotext->w_current = w_current;

  if (autotext->dialog == NULL) {

    /* Dialog is not currently displayed - create it */
    autotext->dialog = autonumber_create_dialog(w_current, autotext);

    autonumber_sortorder_create(w_current);

    gtk_dialog_set_default_response (GTK_DIALOG (autotext->dialog),
                                     GEDA_RESPONSE_ACCEPT);

    g_signal_connect (autotext->dialog, "response",
                      G_CALLBACK (autonumber_text_response),
                      autotext);

    restore_dialog_values(autotext);

    gtk_widget_show_all(autotext->dialog);
  }

  select_scope_text_value(autotext);

  /* if the dialog is in the background or minimized: show it */
  gtk_window_present(GTK_WINDOW(autotext->dialog));
}

/** @} endgroup Auto-Number-Dialog-Module */

#undef ThisDialog
#undef Switch_Responder
