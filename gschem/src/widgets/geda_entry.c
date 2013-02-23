/* gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * Date: December 31, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#include "config.h"
#include <geda.h>
#include <ascii.h>

#include <string.h>
#include <ctype.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "widgets/geda_entry.h"
#include "gettext.h"

/**
 * SECTION:gedaentry
 * @Short_description: A single line text entry field
 * @Title: GedaEntry
 * @See_also: #GedaEntryCompletion
 *
 * The #GedaEntry widget is a single line text entry
 * widget. A fairly large set of key bindings are supported
 * by default. If the entered text is longer than the allocation
 * of the widget, the widget will scroll so that the cursor
 * position is visible.
 *
 */

enum {
  AUTO_SUB_MENU,
  AUTO_COMPLETE_ON,
  AUTO_COMPLETE_OFF,
};

#define GTK_ENTRY_COMPLETION_KEY "gtk-entry-completion-key"

static void     geda_entry_class_init        (GedaEntryClass *klass);
static void     geda_entry_init              (GedaEntry      *entry);
static void     geda_entry_finalize          (GObject        *object);
static bool     geda_entry_key_press         (GedaEntry      *widget,
                                              GdkEventKey    *event,
                                              gpointer        data);
static void     geda_entry_grab_focus        (GtkWidget      *widget);
static void     geda_entry_activate          (GedaEntry      *entry,
                                              gpointer        data);
static void     geda_entry_history_up        (GedaEntry      *entry);
static void     geda_entry_history_down      (GedaEntry      *entry);
static bool     geda_entry_tab_complete      (GtkEntry        *entry);
static void     geda_entry_populate_popup    (GedaEntry       *entry,
                                              GtkMenu         *menu,
                                              gpointer         data);

static void     popup_menu_callback          (GtkMenuItem    *item,
                                              gpointer        data);


static void     geda_entry_set_property      (GObject        *object,
                                              unsigned int    property_id,
                                              const GValue   *value,
                                              GParamSpec     *pspec);
static void     geda_entry_get_property      (GObject        *object,
                                              unsigned int    property_id,
                                              GValue         *value,
                                              GParamSpec   *pspec);

static int strncmpi(char *str1, char *str2, int n);

static GList  *history_list;
static GList **history_list_arg;
static bool    is_history;
static GList  *complete_list;
static GList **old_complete_list;

static bool    have_auto_complete;
static bool    set_auto_complete;
static bool    do_auto_complete;

static GtkEntryClass *parent_class = NULL;
G_DEFINE_TYPE (GedaEntry, geda_entry, GTK_TYPE_ENTRY);
gpointer *entry_parent_class;

struct _GedaEntryPriv
{
  GCompletion      *command_completion;

};

const char* IDS_CONSOLE_POPUP[] = {
  "Auto Complete", "On", "Off", /* Popup Menu Strings*/
  NULL
};

void geda_entry_set_max_history (GedaEntry *entry, int value)
{
  entry->max_history = value;
}
int  geda_entry_get_max_history (GedaEntry *entry)
{
  return entry->max_history;
}
/*! \brief Set sensitivity of internal completion algorithms */
void
geda_entry_completion_set_case (GedaEntry *entry, bool sensitive)
{
  if(have_auto_complete) {
  if (sensitive)
    g_completion_set_compare( entry->priv->command_completion,
                              strncmp);
  else
    g_completion_set_compare( entry->priv->command_completion,
                              (GCompletionStrncmpFunc) strncmpi);
  }
}
void geda_entry_set_input_case  (GedaEntry *entry, int mode) {
  GEDA_ENTRY(entry)->text_case = mode;
}
void geda_entry_set_valid_input (GedaEntry *entry, GedaEntryAccept mode) {
  GEDA_ENTRY(entry)->validation_mode = mode;
}
/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GedaEntry's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_entry_set_property (GObject *object, unsigned int  property_id,
                  const  GValue  *value,  GParamSpec   *pspec)
{
  GedaEntry *entry = GEDA_ENTRY(object);

  switch (property_id)
    {
    case PROP_AUTO_COMPLETION:
      entry->auto_complete = have_auto_complete ? g_value_get_boolean (value) : FALSE;
      break;
    case PROP_CASE_SENSITIVE:
      geda_entry_set_max_history(entry, g_value_get_boolean (value));
      break;
    case PROP_INPUT_CASE:
      geda_entry_set_input_case(entry, g_value_get_int (value));
      break;
    case PROP_MAX_HISTORY:
      entry->max_history = g_value_get_int (value);
      break;
    case PROP_VALIDATE:
      geda_entry_set_valid_input(entry, g_value_get_int (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GedaEntry's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_entry_get_property (GObject *object, unsigned int  property_id,
                         GValue  *value,  GParamSpec   *pspec)
{
  if(property_id == PROP_MAX_HISTORY)
    g_value_set_int (value, GEDA_ENTRY(object)->max_history);
}
static void geda_entry_validate_input (GtkEntry    *entry,
                                       const char  *text,
                                       int          length,
                                       int         *position,
                                       gpointer     data)
{
  GtkEditable *editable   = GTK_EDITABLE (entry);
  GedaEntry   *geda_entry = GEDA_ENTRY   (entry);

  int i, count=0;
  char *result = g_new (char, length);
  bool valid = FALSE;

  for (i=0; i < length; i++) {
    switch (geda_entry->validation_mode) {
      case ACCEPT_ALL_ASCII:
         valid = TRUE;
         break;
      case ACCEPT_ALPHANUMERIC:
         if (isalnum(text[i]) || ((text[i] > ASCII_APO) && (text[i] < ASCII_QUESTION_MARK)))
           valid = TRUE;
         break;
      case ACCEPT_NUMERIC:
         if ((text[i] > ASCII_APO) && (text[i] < ASCII_QUESTION_MARK)) /* includes colon and semicolon */
           valid = TRUE;
         break;
      case ACCEPT_NUMBER:
         if (isdigit(text[i]))
           valid = TRUE;
         break;
      case ACCEPT_INTEGER:
         if (isdigit(text[i]) || (text[i] == ASCII_MINUS))
           valid = TRUE;
         break;
      case ACCEPT_REAL:
         if (isdigit(text[i]) || (text[i] == ASCII_MINUS) || (text[i] == ASCII_PERIOD))
           valid = TRUE;
         break;
      default:
         valid = TRUE;
    }
    if (!valid)
      continue;

    if (geda_entry->text_case == LOWER_CASE)
      result[count++] = isupper(text[i]) ? tolower(text[i]) : text[i];
    else
      if (geda_entry->text_case == UPPER_CASE)
        result[count++] = islower(text[i]) ? toupper(text[i]) : text[i];
      else
        result[count++] = text[i];
  }

  if (count > 0) {
    g_signal_handlers_block_by_func (G_OBJECT (editable),
                                     G_CALLBACK (geda_entry_validate_input),
                                     data);
    gtk_editable_insert_text (editable, result, count, position);
    g_signal_handlers_unblock_by_func (G_OBJECT (editable),
                                       G_CALLBACK (geda_entry_validate_input),
                                       data);
  }
  g_signal_stop_emission_by_name (G_OBJECT (editable), "insert_text");

  g_free (result);
}
static void geda_entry_class_init (GedaEntryClass *klass)
{
  GObjectClass   *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);
  entry_parent_class = g_type_class_peek_parent (g_type_class_peek (GTK_TYPE_ENTRY));

  gobject_class->finalize = geda_entry_finalize;

  gobject_class->set_property = geda_entry_set_property;
  gobject_class->get_property = geda_entry_get_property;

  g_object_class_install_property (gobject_class,
                                   PROP_AUTO_COMPLETION,
                                   g_param_spec_boolean ("auto-completion",
                                                       _("Auto-Completion"),
                                                       _("Enable Auto-completion, if installed"),
                                                         TRUE, /* default_value */
                                                         G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
                                   PROP_CASE_SENSITIVE,
                                   g_param_spec_boolean ("case-sensitive",
                                                       _("FALSE Auto-Completion in NOT case sensitive"),
                                                       _("if Auto-completion is enabled, set case compare function"),
                                                         FALSE, /* default_value */
                                                         G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_INPUT_CASE,
                                   g_param_spec_int ("input-case",
                                                   _("Set case of input"), /* nick name */
                                                   _("0 = lower, 1 lower, 2 don't change the case"), /* hint / blurb */
                                                     0, /* Min value */
                                                     2, /* Max value */
                                                     2, /* default_value */
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_MAX_HISTORY,
                                   g_param_spec_int ("max-history",
                                                   _("Set maxium history"), /* nick name */
                                                   _("0 = lower, 1 lower, 2 don't change the case"), /* hint / blurb */
                                                     0,        /* Min value */
                                                     G_MAXINT, /* Max value */
                                                     MAX_ENTRY_HISTORY,      /* default_value */
                                                     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_VALIDATE,
                                   g_param_spec_int ("accept-input-type",
                                                   _("Set valid input type"), /* nick name */
                                                   _("0 = All, 1 Alphnumeric, 3 Numeric, 4 Number, 5 Integer, 6 Real"), /* hint / blurb */
                                                     ACCEPT_ALL_ASCII, /* Min value */
                                                     ACCEPT_REAL,      /* Max value */
                                                     ACCEPT_ALL_ASCII, /* default_value */
                                                     G_PARAM_READWRITE));

  widget_class->grab_focus = geda_entry_grab_focus;

}

static void
geda_entry_init (GedaEntry *entry)
{
  entry->priv = g_new0 (GedaEntryPriv, 1);

  g_signal_connect_after (G_OBJECT (entry), "key_press_event", G_CALLBACK (geda_entry_key_press), NULL);
  if(is_history) {
    g_signal_connect     (G_OBJECT (entry), "activate",        G_CALLBACK (geda_entry_activate), NULL);
    if(history_list_arg) {
      history_list = *history_list_arg;
      entry->history_index = g_list_length (history_list);
      entry->max_history = MAX_ENTRY_HISTORY;
    }
    else {
      entry->history_index = 0;
    }
  }
  g_signal_connect       (G_OBJECT (entry), "populate-popup",  G_CALLBACK (geda_entry_populate_popup), NULL);

  g_signal_connect       (G_OBJECT (entry), "insert_text",     G_CALLBACK (geda_entry_validate_input), NULL);

  /* Initialize & populate a GCompletion for commands */
  if(old_complete_list) {
    complete_list = *old_complete_list;
    entry->priv->command_completion = g_completion_new (NULL);
    g_completion_add_items (entry->priv->command_completion, complete_list);
    entry->auto_complete = TRUE;
  }
  else
    entry->auto_complete = FALSE;

  /* set initial flag state for popup menu*/
  set_auto_complete       = FALSE;
  entry->validation_mode  = ACCEPT_ALL_ASCII;
  entry->text_case        = BOTH_CASES;
}
static void
geda_entry_finalize (GObject *object)
{
	GedaEntry *entry;

	entry = GEDA_ENTRY (object);

	if (entry->priv->command_completion) {
	  g_completion_free (entry->priv->command_completion);
	}
	if (entry->priv) {
	  g_free (entry->priv);
	}

       *history_list_arg  = history_list;
        entry = entry;
	G_OBJECT_CLASS (parent_class)->finalize (object);
}
static bool
geda_entry_key_press (GedaEntry *entry, GdkEventKey *event, gpointer data)
{
  unsigned int state = event->state & gtk_accelerator_get_default_mod_mask ();
  bool handled = FALSE;

  if (( set_auto_complete ) && ( have_auto_complete )) {/* If somebody wants & we have */
    entry->auto_complete = do_auto_complete;
    set_auto_complete = FALSE; /* We did it so reset flag */
    fprintf(stderr, "setting auto complete to [%d}\n",do_auto_complete);
  }
  switch (event->keyval) {
    case GDK_Down:
      if ((state == 0) && (is_history)) {
        geda_entry_history_down (entry);
        handled = TRUE;
      }
      break;
    case GDK_Up:
      if ((state == 0) && (is_history)) {
        geda_entry_history_up (entry);
        handled = TRUE;
      }
      break;
    case GDK_Tab:
      if ( (state  == 0) && (entry->auto_complete) ) {
        handled = geda_entry_tab_complete (GTK_ENTRY (entry));
      }
      break;
    default:
      break;
  }

  return handled;
}
static void
geda_entry_grab_focus (GtkWidget *widget)
{
 /* GtkEntry's grab_focus selects the contents and therefore
  * claims PRIMARY. So we bypass it; see bug #345356 and bug #347067.
  */
  GTK_WIDGET_CLASS (entry_parent_class)->grab_focus (widget);
}

static void
geda_entry_activate (GedaEntry *entry, gpointer data)
{
  int list_length;
  GList *iter;
  GList *prev;

  const char *entry_text;
  entry_text = gtk_entry_get_text (GTK_ENTRY (entry));

  /* if user hit enter with no value then ignore entry */
  if ( (entry_text) && (strlen (entry_text) == 0) ) {
    return;
  }

  if (history_list) {                                         /* if not a new buffer */
    list_length = g_list_length (history_list);   /* hit enter so end is now current */
    iter = g_list_last(history_list);                          /* get the last entry */
    if (g_ascii_strcasecmp (iter->data, entry_text) != 0) {   /* same as last entry? */
      ++list_length;                                /* if not then increment counter */
      if (list_length > entry->max_history) {                  /* at history limit?  */
        iter = g_list_first(history_list);                          /* get the start */
        g_free(iter->data);                                 /* first the oldest data */
        prev = iter;                       /* but save address that held the pointer */
        for (iter = g_list_next(iter); iter != NULL; iter=g_list_next(iter)) {
          prev->data = iter->data;                           /* rotate pointers down */
          prev       = iter;
        }
        iter       = g_list_last(history_list);          /* get the last entry again */
        iter->data = g_strdup (entry_text);                     /* save the new text */
        list_length = g_list_length (history_list); /* is really ++list_length | max */
      }
      else { /* the buffer is not full so just add to the end */
        history_list = g_list_append(history_list, g_strdup(entry_text));
      }
    }
  }
  else { /* we were created with a NULL list, this means glist is a new buffer list */
    history_list = g_list_append(history_list, g_strdup(entry_text));
    list_length = 1;
  }
  entry->history_index = list_length;
}

static void
geda_entry_history_up (GedaEntry *entry)
{
  char *new_line;

  if (entry->history_index > 0) {
    --entry->history_index;
      new_line = g_list_nth_data(history_list, entry->history_index);
      gtk_entry_set_text (GTK_ENTRY (entry), new_line);
      gtk_editable_set_position (GTK_EDITABLE (GTK_ENTRY (entry)), -1);
  }
}

static void
geda_entry_history_down (GedaEntry *entry)
{
  char *new_line;
  int list_length;

  GtkEntry *gtk_entry = GTK_ENTRY (entry);

  if (entry->history_index < (entry->max_history - 1)) {
    list_length = g_list_length (history_list);
    if (entry->history_index < list_length) {
      if (g_list_nth_data(history_list, entry->history_index + 1)) {
        ++entry->history_index;
          new_line = g_list_nth_data(history_list, entry->history_index);
          gtk_entry_set_text (gtk_entry, new_line);
          gtk_editable_set_position (GTK_EDITABLE (gtk_entry), -1);
      }
      else { /* There is no more data so set blank line */
        gtk_entry_set_text (gtk_entry, "");
      }
    }
    else {
      gtk_entry_set_text (gtk_entry, "");
    }
  }
  else { /* user hit "down" at the end of the buffer make blank line */
    gtk_entry_set_text (gtk_entry, "");
  }
}
/*  */
  /*! \brief Compare n characters ignoring case.
 *
 *  \par Function Description
 *  Another garden varity string compare using toupper
 *  on both inputs. This is somthimes found in standard.
 *  libraries but not always.
 *
 *  \param [in] char* str1 is the string to be search
 *  \param [in] char* str1 is the string to search for
 *  \param [in] int   n is the number of char to compare
 *
 *  \retval 0 if the strings are equivalent, -1 if str2 if
 *  first mis-match is because str2 is greater, or 1 if the
 *  first mis-match is because str1 is greater.
 */
static int strncmpi(char *str1, char *str2, int n)
{
  int i = 0;
  while (( toupper(*str1) == toupper(*str2)) && i < n)
  {
    str1++;
    str2++;
    i++;
  }
  if ( i == n)
    return 0;
  else
    if ((*str1 == *str2 ) && (!str1))
      return 0;
    else
      if ((*str1) && (!str2))
        return -1;
      else
        if ((*str2) && (!str1))
          return 1;
        else
          return ((*str1 > *str2 ) ? -1 : 1);
}
static bool
geda_entry_tab_complete (GtkEntry *entry)
{
  char  *buffer;
  char  *s_ptr;
  char  *match;

  GList     *options;
  GedaEntry *geda_entry = GEDA_ENTRY (entry);

  bool exit ( bool answer ) { g_free ( buffer ); return answer; }

  buffer = g_malloc0(max_command_length);

  if (!buffer)
    return FALSE;

  s_ptr = strcpy(buffer, gtk_entry_get_text (entry));   /* get the text */

  while ( *s_ptr != ASCII_NUL) ++s_ptr;     /* advance to end of string */

  if (s_ptr == buffer) /* if string empty */
    return exit (TRUE);

  if ( *(--s_ptr) == ASCII_SPACE)     /* If previous char is space then */
    return exit (TRUE);                 /* there is nothing to complete */

  while ( (s_ptr != buffer) && *s_ptr != ASCII_SPACE) s_ptr--; /* go backwards */

  if (s_ptr != buffer) ++s_ptr;       /* if compounding then skip space */

  options = g_completion_complete (geda_entry->priv->command_completion, s_ptr, &match);

  if (g_list_length (options) == 0)                    /* if no matches */
    return exit (TRUE);

  if (g_list_length (options) == 1) {                       /* one match */
    strcpy (s_ptr, options->data);

  }
  else
    strcpy (s_ptr, match);

  gtk_entry_set_text (entry, buffer);
  gtk_editable_set_position (GTK_EDITABLE (entry), strlen (buffer));

  g_free (match);
  /* Don't free buffer! */;
  return exit (TRUE);

}

/*! \section Popup-Menu */
static void
geda_entry_populate_popup (GedaEntry *entry, GtkMenu *menu, gpointer data)
{
        GtkWidget *item;
        GtkWidget *submenu;

        item = gtk_menu_item_new_with_mnemonic (_("Auto Complete"));
        gtk_widget_show (item);

        submenu = gtk_menu_new ();
        gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
        gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

        item = gtk_image_menu_item_new_with_label (_("On"));
        g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_menu_callback), GINT_TO_POINTER (1));
        gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);

        item = gtk_image_menu_item_new_with_label (_("Off"));
        g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_menu_callback), GINT_TO_POINTER (2));
        gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);

        gtk_widget_show_all (submenu);
}

static void
popup_menu_callback (GtkMenuItem *item, gpointer data)
{
  int menu_option = GPOINTER_TO_INT (data);
  switch(menu_option) {
      case AUTO_COMPLETE_ON:
        fprintf(stderr, "setting auto complete on\n");
        set_auto_complete = TRUE;
        do_auto_complete  = TRUE;
        break;
      case AUTO_COMPLETE_OFF:
        fprintf(stderr, "disabling auto complete\n");
        set_auto_complete = TRUE;
        do_auto_complete  = FALSE;
        break;
      default:
        break;
  }
}
/* ------------------------------------------------------------- */
GtkWidget *geda_entry_new (GList** history, GList** complete)
{
  if ((int)history == -1)
    is_history = FALSE;
  else {
    history_list_arg = history;
    is_history = TRUE;
  }
  if((int)complete == -1)
    have_auto_complete = FALSE;
  else {
    if (complete) {
      old_complete_list = complete;
      have_auto_complete = TRUE;
    }
    else {
      have_auto_complete = FALSE;
    }
  }
  return GTK_WIDGET (g_object_new (geda_entry_get_type (), NULL));
}
/*!
 * gtk_entry_new_with_buffer:
 * @buffer: The buffer to use for the new #GtkEntry.
 *
 * Creates a new entry with the specified text buffer.
 * Return value: a new #GtkEntry
 *
 */
GtkWidget*
geda_entry_new_with_buffer (GtkEntryBuffer *buffer)
{
  g_return_val_if_fail (GTK_IS_ENTRY_BUFFER (buffer), NULL);
  return g_object_new  (GEDA_TYPE_ENTRY, "buffer", buffer, NULL);
}

