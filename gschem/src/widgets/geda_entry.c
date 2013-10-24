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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
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

#define MAX_ICONS 2

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

enum {
  PROCESS_ENTRY,
  LAST_SIGNAL
};
enum {
  PROP_ACTIVATES_DEFAULT = 1,
  PROP_ATTRIBUTES,
  PROP_AUTO_COMPLETION,
  PROP_CASE_SENSITIVE,
  PROP_INPUT_CASE,
  PROP_MAX_HISTORY,
  PROP_VALIDATE,
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

static void    geda_entry_class_init         (GedaEntryClass   *class);
static void    geda_entry_init               (GedaEntry        *entry);
static void    geda_entry_finalize           (GObject          *object);
static void    geda_entry_real_activate      (GedaEntry        *entry);
static bool    geda_entry_key_press          (GedaEntry        *widget,
                                              GdkEventKey      *event,
                                              gpointer          data);
static void    geda_entry_grab_focus         (GtkWidget        *widget);
static void    geda_entry_realize            (GtkWidget        *widget);
static void    geda_entry_activate           (GedaEntry        *entry,
                                              gpointer          data);
static void    geda_entry_history_up         (GedaEntry        *entry);
static void    geda_entry_history_down       (GedaEntry        *entry);
static bool    geda_entry_tab_complete       (GtkEntry         *entry);

static void    geda_entry_populate_popup     (GedaEntry        *entry,
                                              GtkMenu          *menu,
                                              gpointer          data);
static void    popup_menu_callback           (GtkMenuItem      *item,
                                              gpointer          data);

static void    geda_entry_set_property       (GObject          *object,
                                              unsigned int      property_id,
                                              const GValue     *value,
                                              GParamSpec       *pspec);
static void    geda_entry_get_property       (GObject          *object,
                                              unsigned int      property_id,
                                              GValue           *value,
                                              GParamSpec       *pspec);

static int strncmpi(char *str1, char *str2, int n);

static GList  *history_list;
static GList **history_list_arg;
static bool    have_history;
static GList  *complete_list;
static GList **old_complete_list;

static bool    have_auto_complete;
static bool    set_auto_complete;
static bool    do_auto_complete;

static GtkEntryClass *parent_class = NULL;
G_DEFINE_TYPE (GedaEntry, geda_entry, GTK_TYPE_ENTRY);
gpointer *entry_parent_class;

typedef struct
{
  GdkWindow     *window;
  char          *tooltip;
  unsigned int   insensitive    : 1;
  unsigned int   nonactivatable : 1;
  unsigned int   prelight       : 1;
  unsigned int   in_drag        : 1;
  unsigned int   pressed        : 1;

  GtkImageType   storage_type;
  GdkPixbuf     *pixbuf;
  char          *stock_id;
  char          *icon_name;
  GIcon         *gicon;

  GtkTargetList *target_list;
  GdkDragAction  actions;
} EntryIconInfo;

struct _GedaEntryPriv
{
  GedaCompletion *command_completion;
  bool            case_sensitive;

  PangoAttrList *attrs;
  PangoFontMap  *font_map;

  EntryIconInfo *icons[MAX_ICONS];
};

const char* IDS_CONSOLE_POPUP[] = {
  "Auto Complete", "On", "Off", /* Popup Menu Strings*/
  NULL
};
/**
 * gtk_entry_get_activates_default:
 * @entry: a #GtkEntry
 *
 * Retrieves the value set by gtk_entry_set_activates_default().
 *
 * Return value: %TRUE if the entry will activate the default widget
 **/
bool
geda_entry_get_activates_default (GedaEntry *entry)
{
  g_return_val_if_fail (IS_GEDA_ENTRY (entry), FALSE);

  return entry->activates_default;
}

/*! \brief Set Entry attribute List
 *  \par Function Description
 * @setting: %TRUE to activate window's default widget on Enter keypress
 *
 * If @setting is %TRUE, pressing Enter in the @entry will activate the default
 * widget for the window containing the entry. This usually means that
 * the dialog box containing the entry will be closed, since the default
 * widget is usually one of the dialog buttons.
 *
 * (For experts: if @setting is %TRUE, the entry calls
 * gtk_window_activate_default() on the window containing the entry, in
 * the default handler for the #GtkEntry::activate signal.)
 **/
void
geda_entry_set_activates_default (GedaEntry *entry, bool setting)
{
  g_return_if_fail (IS_GEDA_ENTRY (entry));

  setting = setting != FALSE;

  if (setting != entry->activates_default)
    {
      entry->activates_default = setting;
      g_object_notify (G_OBJECT (entry), "activates-default");
    }
}

/*! \brief Set Entry attribute List
 *  \par Function Description
 * This function applies the PangoAttrList to entry text font
 * description.
 */
void geda_entry_set_attributes ( GedaEntry *entry, PangoAttrList *attrs)
{
  PangoAttrList *old_attrs;
  PangoLayout* layout;

  g_return_if_fail (IS_GEDA_ENTRY (entry));

  old_attrs = entry->priv->attrs;

  if (attrs)
    pango_attr_list_ref (attrs);

  if (old_attrs)
    pango_attr_list_unref (old_attrs);

  entry->priv->attrs = attrs;

  layout = gtk_entry_get_layout ( (GtkEntry*) entry );
  if (layout) {
    pango_layout_set_attributes (layout, entry->priv->attrs);
    g_object_notify (G_OBJECT (entry), "attributes");
  }

  gtk_widget_queue_resize (GTK_WIDGET (entry));
}

/*! \brief Get Entry attribute List
 *  \par Function Description
 * Gets the attribute list that was set on the entry using
 * geda_entry_set_attributes(), if any.
 *
 * \retval: list the PangoAttrList attribute list, or %NULL
 *          if none was set.
 */
PangoAttrList  *geda_entry_get_attributes (GedaEntry *entry)
{
  g_return_val_if_fail (IS_GEDA_ENTRY (entry), NULL);

  return entry->priv->attrs;
}

void geda_entry_set_max_history (GedaEntry *entry, int value)
{
  entry->max_history = value;
}
int  geda_entry_get_max_history (GedaEntry *entry)
{
  return entry->max_history;
}

/*! \brief Get sensitivity of internal completion algorithms */
bool geda_entry_completion_get_case (GedaEntry *entry) {

  g_return_val_if_fail (IS_GEDA_ENTRY (entry), FALSE);

  return entry->priv->case_sensitive;
}

/*! \brief Set sensitivity of internal completion algorithms */
void
geda_entry_completion_set_case (GedaEntry *entry, bool sensitive)
{
  g_return_if_fail (IS_GEDA_ENTRY (entry));

  if(have_auto_complete) {

    sensitive = sensitive != FALSE;
    entry->priv->case_sensitive = sensitive;
    if (sensitive)
      geda_completion_set_compare( entry->priv->command_completion,
                                   (GedaStrCompareNFunc) strncmp);
    else
      geda_completion_set_compare( entry->priv->command_completion,
                                 (GedaStrCompareNFunc) strncmpi);
  }
}
bool geda_entry_get_input_case (GedaEntry *entry)
{
  g_return_val_if_fail (IS_GEDA_ENTRY (entry), FALSE);
  return entry->text_case;
}
void geda_entry_set_input_case  (GedaEntry *entry, int mode)
{
  GEDA_ENTRY(entry)->text_case = mode;
}

GedaEntryAccept
geda_entry_get_valid_input (GedaEntry *entry)
{
  g_return_val_if_fail (IS_GEDA_ENTRY (entry), ACCEPT_ALL_ASCII);
  return entry->validation_mode;
}

void geda_entry_set_valid_input (GedaEntry *entry, GedaEntryAccept mode)
{
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
    case PROP_ACTIVATES_DEFAULT:
      geda_entry_set_activates_default (entry, g_value_get_boolean (value));
      break;
    case PROP_ATTRIBUTES:
      geda_entry_set_attributes (entry, g_value_get_boxed (value));
      break;
    case PROP_AUTO_COMPLETION:
      entry->auto_complete = have_auto_complete ? g_value_get_boolean (value) : FALSE;
      break;
    case PROP_CASE_SENSITIVE:
      geda_entry_completion_set_case(entry, g_value_get_boolean (value));
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
  GedaEntry *entry = GEDA_ENTRY(object);

  switch (property_id)
    {
    case PROP_ACTIVATES_DEFAULT:
      g_value_set_boolean (value, entry->activates_default);
      break;
    case PROP_ATTRIBUTES:
      g_value_set_boxed (value, entry->priv->attrs);
      break;
    case PROP_AUTO_COMPLETION:
      g_value_set_boolean (value, have_auto_complete ? entry->auto_complete : FALSE);
      break;
    case PROP_CASE_SENSITIVE:
      g_value_set_boolean(value, geda_entry_completion_get_case(entry));
      break;
    case PROP_INPUT_CASE:
      g_value_set_int (value, geda_entry_get_input_case(entry));
      break;
    case PROP_MAX_HISTORY:
      g_value_set_int (value, entry->max_history);
      break;
    case PROP_VALIDATE:
      g_value_set_enum (value, geda_entry_get_valid_input(entry));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
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
      case ACCEPT_COORDINATE:
         if ((text[i] == ASCII_COMMA) || (text[i] == ASCII_LEFT_PARENTHESIS) || (text[i] == ASCII_RIGHT_PARENTHESIS))
           valid = TRUE;
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

static void geda_entry_class_init (GedaEntryClass *class)
{
  GParamSpec     *params;

  GObjectClass   *gobject_class;
  GtkWidgetClass *widget_class;
  GtkBindingSet  *binding_set;

  gobject_class = G_OBJECT_CLASS (class);
  widget_class  = GTK_WIDGET_CLASS (class);

  parent_class       = g_type_class_peek_parent (class);
  entry_parent_class = g_type_class_peek_parent (g_type_class_peek (GTK_TYPE_ENTRY));

  gobject_class->finalize     = geda_entry_finalize;
  gobject_class->set_property = geda_entry_set_property;
  gobject_class->get_property = geda_entry_get_property;

  class->activate             = geda_entry_real_activate;

  /* We over-ride parent's drag&drop, which is over-riding widget class
   * because we support drag&drop stuff other than just text and the
   * stock entry intecepts all of drag&drop signals! */
  class->drag_begin           = widget_class->drag_begin;
  class->drag_end             = widget_class->drag_end;
  class->drag_drop            = widget_class->drag_drop;
  class->drag_motion          = widget_class->drag_motion;
  class->drag_leave           = widget_class->drag_leave;
  class->drag_data_received   = widget_class->drag_data_received;
  class->drag_data_get        = widget_class->drag_data_get;
  class->drag_data_delete     = widget_class->drag_data_delete;

  params = g_param_spec_boolean ("activates-default",
                               _("Activates default"),
                               _("Whether to activate the default widget when Enter is pressed"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ACTIVATES_DEFAULT, params);

  params = g_param_spec_boxed ("attributes",
                             _("Attributes"),
                             _("A list of style attributes to apply to the text"),
                                PANGO_TYPE_ATTR_LIST,
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ATTRIBUTES, params);

  params = g_param_spec_boolean ("auto-completion",
                               _("Auto-Completion"),
                               _("Enable Auto-completion, if installed"),
                                  TRUE, /* default_value */
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_AUTO_COMPLETION, params);

  params = g_param_spec_boolean ("case-sensitive",
                               _("FALSE Auto-Completion in NOT case sensitive"),
                               _("if Auto-completion is enabled, set case compare function"),
                                FALSE, /* default_value */
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_CASE_SENSITIVE, params);

  params = g_param_spec_int ("input-case",
                           _("Set case of input"), /* nick name */
                           _("0 = lower, 1 lower, 2 don't change the case"), /* hint / blurb */
                              0, /* Min value */
                              2, /* Max value */
                              2, /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_INPUT_CASE, params);

  params = g_param_spec_int ("max-history",
                           _("Set maxium history"), /* nick name */
                           _("0 = lower, 1 lower, 2 don't change the case"), /* hint / blurb */
                              0,        /* Min value */
                              G_MAXINT, /* Max value */
                              MAX_ENTRY_HISTORY,      /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MAX_HISTORY, params );

  params = g_param_spec_int ("accept-input-type",
                           _("Set valid input type"), /* nick name */
                           _("0 = All, 1 Alphnumeric, 3 Numeric, 4 Number, 5 Integer, 6 Real"), /* hint / blurb */
                              ACCEPT_ALL_ASCII, /* Min value */
                              ACCEPT_REAL,      /* Max value */
                              ACCEPT_ALL_ASCII, /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_VALIDATE, params);

  /**
   * GtkEntry::activate:
   * @entry: The entry on which the signal is emitted
   *
   * The ::activate signal is emitted when the user hits
   * the Enter key.
   *
   * While this signal is used as a
   * <link linkend="keybinding-signals">keybinding signal</link>,
   * it is also commonly used by applications to intercept
   * activation of entries.
   *
   * The default bindings for this signal are all forms of the Enter key.
   */

  signals[PROCESS_ENTRY] = g_signal_new ("process-entry",
                                    G_TYPE_FROM_CLASS (gobject_class),
                                    G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                                    G_STRUCT_OFFSET (GedaEntryClass, activate),
                                    NULL, NULL,
                                    g_cclosure_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  widget_class->activate_signal = signals[PROCESS_ENTRY];

  /*  Key bindings */
  binding_set = gtk_binding_set_by_class (class);

  /* Activate */
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_Return, 0,    "process-entry", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_ISO_Enter, 0, "process-entry", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_KP_Enter, 0,  "process-entry", 0);

  widget_class->grab_focus = geda_entry_grab_focus;
  widget_class->realize    = geda_entry_realize;

#ifdef DEBUG_GEDA_ENTRY
  fprintf(stderr, "new geda_entry created: history=%d, completion=%d\n",
          have_history, have_auto_complete );
#endif
}

static void geda_entry_init (GedaEntry *entry)
{
  entry->priv = g_new0 (GedaEntryPriv, 1);

  entry->priv->font_map = pango_cairo_font_map_get_default();

  g_signal_connect_after (G_OBJECT (entry), "key_press_event", G_CALLBACK (geda_entry_key_press), NULL);
  if(have_history) {
    //g_signal_connect     (G_OBJECT (entry), "activate",        G_CALLBACK (geda_entry_activate), NULL);
    g_signal_connect     (G_OBJECT (entry), "process-entry",   G_CALLBACK (geda_entry_activate), NULL);
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
    entry->priv->command_completion = geda_completion_new (NULL);
    geda_completion_add_items (entry->priv->command_completion, complete_list);
    entry->auto_complete = TRUE;
  }
  else
    entry->auto_complete = FALSE;

  /* set initial flag state for popup menu*/
  set_auto_complete           = FALSE;
  entry->validation_mode      = ACCEPT_ALL_ASCII;
  entry->text_case            = BOTH_CASES;
  entry->activates_default    = FALSE;
  entry->priv->case_sensitive = FALSE;

  entry->priv->attrs          = NULL;

}

static void geda_entry_finalize (GObject *object)
{
  GedaEntry *entry;

  entry = GEDA_ENTRY (object);

  if (entry->priv->command_completion) {
    geda_completion_free (entry->priv->command_completion);
  }

  /* Save history to caller's glist*/
  if (have_history)
    *history_list_arg  = history_list;

  G_OBJECT_CLASS (parent_class)->finalize (object);

  if (entry->priv->attrs && G_IS_OBJECT(entry->priv->attrs))
    pango_attr_list_unref (entry->priv->attrs);

  if ( entry->priv->font_map && G_IS_OBJECT(entry->priv->font_map) ) {
    g_object_unref (entry->priv->font_map);
    entry->priv->font_map = NULL;
  }

  if (entry->priv) {
    g_free (entry->priv);
  }
}

/*! \brief Entry Stop Activate Default signal Responder
 *  \par Function Description
 *  This function exist to stop GTK-2 from activating the
 *  the default widget when an "Enter" key is press. The
 *  GtkEntry gtk_entry_set_activates_default function does
 *  not work correclty, the default widget will eventually
 *  see the signal regardless of the setting.
 *
 *  \param [in]  object The GedaEntry object
 */
static void
geda_entry_real_activate (GedaEntry *entry)
{
  GtkWindow *window;
  GtkWidget *default_widget, *focus_widget;
  GtkWidget *toplevel;
  GtkWidget *widget;
#ifdef DEBUG_GEDA_ENTRY
  fprintf(stderr, "<geda_entry_real_activate> in over-ride: got <activate> signal\n");
#endif
  widget = GTK_WIDGET (entry);

  if (entry->activates_default) {
    toplevel = gtk_widget_get_toplevel (widget);
    if (GTK_IS_WINDOW (toplevel))
    {
      window = GTK_WINDOW (toplevel);

      if (window) {
        default_widget = gtk_window_get_default_widget (window);
        focus_widget = gtk_window_get_focus (window);
        if (widget != default_widget &&
          !(widget == focus_widget && (!default_widget || !gtk_widget_get_sensitive (default_widget))))
          gtk_window_activate_default (window);
      }
    }
  }
}

static bool
geda_entry_key_press (GedaEntry *entry, GdkEventKey *event, gpointer data)
{
  unsigned int state = event->state & gtk_accelerator_get_default_mod_mask ();
  bool handled = FALSE;

  if (( set_auto_complete ) && ( have_auto_complete )) {/* If somebody wants & we have */
    entry->auto_complete = do_auto_complete;
    set_auto_complete = FALSE; /* We did it so reset flag */
  }

  switch (event->keyval) {
    case GDK_Down:
      if ((state == 0) && (have_history)) {
        geda_entry_history_down (entry);
        handled = TRUE;
      }
      break;
    case GDK_Up:
      if ((state == 0) && (have_history)) {
        geda_entry_history_up (entry);
        handled = TRUE;
      }
      break;
    case GDK_KP_Enter:
    case GDK_KEY_Return:
    case GDK_KEY_ISO_Enter:
      handled = TRUE;
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
static void geda_entry_realize (GtkWidget *widget)
{
  PangoContext         *context;
  PangoLayout          *layout;

  GedaEntry *entry = GEDA_ENTRY (widget);

  GTK_WIDGET_CLASS (parent_class)->realize (widget);

  if (gtk_widget_has_screen(widget)) {
    layout = gtk_entry_get_layout ( (GtkEntry*) widget);
    context = pango_layout_get_context (layout);

    pango_context_set_font_map (context, entry->priv->font_map);
    entry->priv->font_map = g_object_ref (entry->priv->font_map);
    pango_layout_context_changed (layout);

  }
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

static bool geda_entry_tab_complete (GtkEntry *entry)
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

  options = geda_completion_complete (geda_entry->priv->command_completion, s_ptr, &match);

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

  if (have_auto_complete) {
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
}

static void
popup_menu_callback (GtkMenuItem *item, gpointer data)
{
  int menu_option = GPOINTER_TO_INT (data);
  switch(menu_option) {
      case AUTO_COMPLETE_ON:

#ifdef DEBUG_GEDA_ENTRY
        fprintf(stderr, "setting auto complete on\n");
#endif
        set_auto_complete = TRUE;
        do_auto_complete  = TRUE;
        break;
      case AUTO_COMPLETE_OFF:
#ifdef DEBUG_GEDA_ENTRY
        fprintf(stderr, "disabling auto complete\n");
#endif
        set_auto_complete = TRUE;
        do_auto_complete  = FALSE;
        break;
      default:
        break;
  }
}

/* --------------------- Widget Style Functions ----------------- */
/*! \section Entry-Style */

static void
geda_entry_modify_color_component (GtkWidget      *widget,
                                   GtkRcFlags      component,
                                   GtkStateType    state,
                                   const GdkColor *color)
{
  GtkRcStyle *rc_style;

  rc_style = gtk_widget_get_modifier_style (widget);

  if (color)
  {
    switch (component)
    {
      case GTK_RC_FG:
        rc_style->fg[state]   = *color;
        break;
      case GTK_RC_BG:
        rc_style->bg[state]   = *color;
        break;
      case GTK_RC_TEXT:
        rc_style->text[state] = *color;
        break;
      case GTK_RC_BASE:
        rc_style->base[state] = *color;
        break;
      default:
        g_critical ("Internal Error: <%s><geda_entry_modify_color_component>"
                    "unhandled case=%d, line %d.\n",
                     __FILE__, component, __LINE__);
    }

    rc_style->color_flags[state] |= component;
  }
  else
    rc_style->color_flags[state] &= ~component;

  gtk_widget_modify_style (widget, rc_style);
}

void
geda_entry_widget_modify_color (GtkWidget      *widget,
                                GtkRcFlags      component,
                                GtkStateType    state,
                                const GdkColor *color)
{
  g_return_if_fail (GTK_IS_WIDGET (widget));

  if(state >= GTK_STATE_NORMAL || state <= GTK_STATE_INSENSITIVE)
    state = GTK_STATE_NORMAL;

  geda_entry_modify_color_component (widget, component, state, color);

}

void geda_entry_modify_fg (GedaEntry *entry,
                           GtkStateType state,
                           const GdkColor *color)
{
  geda_entry_widget_modify_color (GTK_WIDGET (entry), GTK_RC_FG, state, color);
}

void geda_entry_modify_bg (GedaEntry      *entry,
                           GtkStateType    state,
                           const GdkColor *color)
{
  geda_entry_widget_modify_color (GTK_WIDGET (entry), GTK_RC_BG, state, color);
}

/* -------------------------------------------------------------- */
GtkWidget *geda_entry_new (GList** history, GList** complete)
{
  if ((int)history == -1)
    have_history = FALSE;
  else {
    history_list_arg = history;
    have_history = TRUE;
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
GtkWidget *geda_visible_entry_new (GList** history, GList** complete)
{
  GtkWidget *entry;
  entry = geda_entry_new ( history, complete);
  g_object_set (entry, "visible", TRUE, NULL);
  return entry;
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

