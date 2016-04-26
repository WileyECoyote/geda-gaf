/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_entry.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
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
 * Date: December 31, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include <ctype.h>

#include <gtk/gtk.h>

#include "geda_entry.h"
#include "geda_image_menu_item.h"

#include "gettext.h"

#include <geda_debug.h>

#include <geda_keysyms.h>

/**
 * \brief GedaEntry - A single line text entry field
 * \par
 * The #GedaEntry widget is a single line text entry widget supporting a
 * fairly large set of key bindings by default. If the entered text is
 * longer than the allocation of the widget, the widget will scroll so
 * that the cursor position is visible. For larger allocations the widget
 * should be combined with a text buffer, as is done in the console_init()
 * function.
 * \par
 * An ill effect of having a GList** for the history buffer parameter is
 * that if NULL is passed as the first parameter, the history buffer
 * argument, to the construction utility "new" functions, then the NULL
 * will be interpreted as an empty history list and the history feature
 * will be enabled. Since passing -1 will generate a compiler warning,
 * because the compiler is expecting a GList**, a convienence macro is
 * provided in the header: NO_HISTORY, in order to type cast the argument.
 * A second macro, NO_COMPLETION is also provided for the second argument,
 * which could also be NULL because a NULL completion is interpreted to
 * mean disabling the completion feature.
 * \par
 * example:    entry = geda_entry_new_visible (NO_HISTORY, NO_COMPLETION);
 * \par
 * example:    entry = geda_entry_new(&history_list, &word_list);
 *
 * \sa #GedaCompletion
 *
 * \defgroup GedaEntry Text Entry Field
 * @{
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

static void    geda_entry_real_activate      (GedaEntry        *entry);
static bool    geda_entry_key_press          (GedaEntry        *widget,
                                              GdkEventKey      *event,
                                              void             *data);
static void    geda_entry_grab_focus         (GtkWidget        *widget);
static void    geda_entry_realize            (GtkWidget        *widget);
static void    geda_entry_unrealize          (GtkWidget        *widget);
static void    geda_entry_activate           (GedaEntry        *entry,
                                              void             *data);
static void    geda_entry_history_up         (GedaEntry        *entry);
static void    geda_entry_history_down       (GedaEntry        *entry);
static bool    geda_entry_tab_complete       (GedaEntry        *entry);

static void    geda_entry_populate_popup     (GedaEntry        *entry,
                                              GtkMenu          *menu,
                                              void             *data);
static void    popup_menu_callback           (GtkMenuItem      *item,
                                              void             *data);
static void    geda_entry_get_property       (GObject          *object,
                                              unsigned int      property_id,
                                              GValue           *value,
                                              GParamSpec       *pspec);
static void    geda_entry_set_property       (GObject          *object,
                                              unsigned int      property_id,
                                              const GValue     *value,
                                              GParamSpec       *pspec);
static int     geda_entry_strncmpi           (char             *str1,
                                              char             *str2,
                                              int               n);

static GList  *history_list;
static GList **history_list_arg;
        bool   have_history;

static GList  *complete_list;
static GList **old_complete_list;

static bool    have_auto_complete;
static bool    set_auto_complete;
static bool    do_auto_complete;

static GObjectClass *geda_entry_parent_class = NULL;

struct _GedaEntryPriv
{
  GedaCompletion *command_completion;
  bool            case_sensitive;
  int             change_count;
  PangoAttrList  *attrs;
  PangoFontMap   *font_map;
};

const char *IDS_CONSOLE_POPUP[] = {
  "Auto Complete", "On", "Off", /* Popup Menu Strings*/
  NULL
};

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
begin_change (GedaEntry *entry)
{
  GedaEntryPriv *priv = entry->priv;

  priv->change_count++;

  g_object_freeze_notify (G_OBJECT (entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
end_change (GedaEntry *entry)
{
  GedaEntryPriv *priv = entry->priv;

  g_object_thaw_notify (G_OBJECT (entry));

  priv->change_count--;
/*
  if (priv->change_count == 0) {
    g_signal_emit_by_name (entry, "changed");
  }
*/
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char*
geda_entry_get_text (GedaEntry *entry)
{
  GtkEntryBuffer *buffer;

  g_return_val_if_fail (GEDA_IS_ENTRY (entry), NULL);

  g_object_get (entry, "buffer", &buffer, NULL);

  return gtk_entry_buffer_get_text (buffer);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_set_text (GedaEntry *entry, const char *new_text)
{
  GtkEntryBuffer *buffer;
  const char     *curr_text;
  int             length;

  g_return_if_fail (GEDA_IS_ENTRY (entry));

  if (!new_text) {
    return;
  }

  g_object_get (entry, "buffer", &buffer, NULL);

  curr_text = gtk_entry_buffer_get_text(buffer);

  if (curr_text) {
    if (strcmp (curr_text, new_text) == 0) {
      return;
    }
  }

  length = strlen(new_text);

  begin_change(entry);

  gtk_entry_buffer_set_text (buffer, new_text, length);

  end_change(entry);
}

/*! \brief GedaEntry Get Activates Default
 *  \par Function Description
 *
 * Retrieves the value set by gtk_entry_set_activates_default().
 *
 * \param [in] entry:    Pointer to a #GedaEntry object.
 *
 * Return value: %TRUE if the entry will activate the default widget
 */
bool
geda_entry_get_activates_default (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), FALSE);

  return entry->activates_default;
}

/*! \brief GedaEntry Set Activates Default
 *  \par Function Description
 *
 * If setting is %TRUE, pressing Enter in the entry will activate the
 * default widget for the window containing the entry. This usually means
 * that the dialog box containing the entry will be closed, since the
 * default widget is usually one of the dialog buttons.
 *
 * (For experts: if setting is %TRUE, the entry calls
 * gtk_window_activate_default() on the window containing the entry, in
 * the default handler for the activate signal.)
 *
 * \param [in] entry:    Pointer to a #GedaEntry object.
 * \param [in] setting:  The desired setting.
 */
void
geda_entry_set_activates_default (GedaEntry *entry, bool setting)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));

  setting = setting != FALSE;

  if (setting != entry->activates_default) {
    entry->activates_default = setting;
    g_object_notify (G_OBJECT (entry), "activates-default");
  }
}

/*! \brief Set Entry attribute List
 *  \par Function Description
 * This function applies the PangoAttrList to entry text font
 * description.
 *
 * \param [in] entry: Pointer to a #GedaEntry object.
 * \param [in] attrs: Pointer to a PangoAttrList structure.
 */
void
geda_entry_set_attributes ( GedaEntry *entry, PangoAttrList *attrs)
{
  PangoAttrList *old_attrs;
  PangoLayout   *layout;

  g_return_if_fail (GEDA_IS_ENTRY (entry));

  old_attrs = entry->priv->attrs;

  if (attrs)
    pango_attr_list_ref (attrs);

  if (old_attrs)
    pango_attr_list_unref (old_attrs);

  entry->priv->attrs = attrs;

  layout = gtk_entry_get_layout ((GtkEntry*)entry);

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
 * \param [in] entry: Pointer to a #GedaEntry object.
 *
 * \retval PangoAttrList attribute list, or %NULL
 *         if none was set.
 */
PangoAttrList*
geda_entry_get_attributes (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), NULL);

  return entry->priv->attrs;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_set_max_history (GedaEntry *entry, int value)
{
  entry->max_history = value;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int
geda_entry_get_max_history (GedaEntry *entry)
{
  return entry->max_history;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GedaCompletion*
geda_entry_get_completion (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), NULL);
  return entry->priv->command_completion;
}

/*! \brief Get sensitivity of internal completion algorithms */
bool
geda_entry_completion_get_case (GedaEntry *entry) {

  g_return_val_if_fail (GEDA_IS_ENTRY (entry), FALSE);

  return entry->priv->case_sensitive;
}

/*! \brief Set sensitivity of internal completion algorithms */
void
geda_entry_completion_set_case (GedaEntry *entry, bool sensitive)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));

  if(have_auto_complete) {

    sensitive = sensitive != FALSE;
    entry->priv->case_sensitive = sensitive;
    if (sensitive)
      geda_completion_set_compare( entry->priv->command_completion,
                                   (GedaStrCompareNFunc) strncmp);
    else
      geda_completion_set_compare( entry->priv->command_completion,
                                 (GedaStrCompareNFunc) geda_entry_strncmpi);
  }
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool
geda_entry_get_input_case (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), FALSE);
  return entry->text_case;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_set_input_case  (GedaEntry *entry, int mode)
{
  GEDA_ENTRY(entry)->text_case = mode;
}

GedaEntryAccept
geda_entry_get_valid_input (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), ACCEPT_ALL_ASCII);
  return entry->validation_mode;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_set_valid_input (GedaEntry *entry, GedaEntryAccept mode)
{
  GEDA_ENTRY(entry)->validation_mode = mode;
}

/*! \brief GObject property setter for a GedaEntry Object
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

  switch (property_id) {

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

/*! \brief GObject property getter function for a GedaEntry Object
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

  switch (property_id) {

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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_real_insert_text (GedaEntry  *entry,
                             const char *new_text,
                             int         new_text_length,
                             int        *position)
{
  GtkEntryBuffer *buffer;
  unsigned int n_inserted;
  int n_chars;

  n_chars = g_utf8_strlen (new_text, new_text_length);

  /* The actual insertion into the buffer. This will end up firing the
   * following signal handlers:
   *
   *       buffer_inserted_text(),
   *       buffer_notify_display_text(),
   *       buffer_notify_text(),
   *       buffer_notify_length()
   */
  begin_change (entry);

  g_object_get (entry, "buffer", &buffer, NULL);

  n_inserted = gtk_entry_buffer_insert_text (buffer, *position, new_text, n_chars);

  end_change (entry);

  *position += n_inserted;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_validate_input (GtkEntry    *entry,
                           const char  *text,
                           int          length,
                           int         *position,
                           void        *data)
{
  GedaEntry *geda_entry = GEDA_ENTRY (entry);

  char *result = g_new (char, length);
  bool  valid  = FALSE;
  int   count  = 0;
  int   i;

  for (i = 0; i < length; i++) {

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

    if (geda_entry->text_case == LOWER_CASE) {
      result[count++] = isupper(text[i]) ? tolower(text[i]) : text[i];
    }
    else {
      if (geda_entry->text_case == UPPER_CASE)
        result[count++] = islower(text[i]) ? toupper(text[i]) : text[i];
      else
        result[count++] = text[i];
    }
  }

  if (count > 0) {

    g_signal_handlers_block_by_func (G_OBJECT (geda_entry),
                                     G_CALLBACK (geda_entry_validate_input),
                                     data);

    geda_entry_real_insert_text (geda_entry, result, count, position);

    g_signal_handlers_unblock_by_func (G_OBJECT (geda_entry),
                                       G_CALLBACK (geda_entry_validate_input),
                                       data);
  }
  g_signal_stop_emission_by_name (G_OBJECT (entry), "insert_text");

  g_free (result);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_drag_begin (GtkWidget      *widget,
                       GdkDragContext *context)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_drag_end (GtkWidget      *widget,
                     GdkDragContext *context)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_drag_leave (GtkWidget      *widget,
                       GdkDragContext *context,
                       unsigned int    time)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );
  gtk_widget_queue_draw (widget);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static bool
geda_entry_drag_drop (GtkWidget      *widget,
                      GdkDragContext *context,
                      int             x,
                      int             y,
                      unsigned int    time)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );


  return FALSE; /* No continue */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static bool
geda_entry_drag_motion (GtkWidget       *widget,
                        GdkDragContext  *context,
                        int              x,
                        int              y,
                        unsigned int     time)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );
  return FALSE; /* not here */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_drag_data_received (GtkWidget        *widget,
                               GdkDragContext   *context,
                               int              x,
                               int              y,
                               GtkSelectionData *selection_data,
                               unsigned int     info,
                               unsigned int     time)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );
}

static void
geda_entry_drag_data_get (GtkWidget        *widget,
                          GdkDragContext   *context,
                          GtkSelectionData *selection_data,
                          unsigned int      info,
                          unsigned int      time)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);
  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_drag_data_delete (GtkWidget *widget, GdkDragContext *context)
{
  GedaEntry *geda_entry = GEDA_ENTRY   (widget);

  if(geda_entry->enable_drag_n_drop)
   g_print ("TODO: geda_entry_drag_data_get\n" );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_finalize (GObject *object)
{
  GedaEntry *entry;

  entry = GEDA_ENTRY (object);

  if (entry->priv->command_completion) {
    geda_completion_free (entry->priv->command_completion);
  }

  /* Save history to caller's glist*/
  if (entry->have_history)
    *history_list_arg  = history_list;

  G_OBJECT_CLASS (geda_entry_parent_class)->finalize (object);

  if (entry->priv->attrs && G_IS_OBJECT(entry->priv->attrs))
    pango_attr_list_unref (entry->priv->attrs);

  if (entry->priv->font_map) {
    pango_cairo_font_map_set_default (NULL);
    entry->priv->font_map = NULL;
  }

  g_free (entry->priv);
}

/*! \brief GedaEntry Type Class Initializer
 *
 *  \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 *  \param [in]  g_class     GedaEntry class we are initializing
 *  \param [in]  class_data  GedaEntry structure associated with the class
 */
static void
geda_entry_class_init(void *g_class, void *class_data)
{
  GedaEntryClass *class;
  GParamSpec     *params;
  GObjectClass   *gobject_class;
  GtkWidgetClass *widget_class;
  GtkBindingSet  *binding_set;

  class         = (GedaEntryClass*)g_class;
  gobject_class = G_OBJECT_CLASS (class);
  widget_class  = GTK_WIDGET_CLASS (class);

  geda_entry_parent_class     = g_type_class_peek_parent (class);

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

  widget_class->drag_begin         = geda_entry_drag_begin;
  widget_class->drag_end           = geda_entry_drag_end;
  widget_class->drag_drop          = geda_entry_drag_drop;
  widget_class->drag_motion        = geda_entry_drag_motion;
  widget_class->drag_leave         = geda_entry_drag_leave;
  widget_class->drag_data_received = geda_entry_drag_data_received;
  widget_class->drag_data_get      = geda_entry_drag_data_get;
  widget_class->drag_data_delete   = geda_entry_drag_data_delete;

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

  /*!
   * GedaEntry::activate:
   * entry: The entry on which the signal is emitted
   *
   * The  GedaEntry::activate signal is emitted when the user hits
   * the Enter key.
   *
   * While this signal is used as a keybinding signal, it is also commonly
   * used by applications to intercept activation of entries.
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
  widget_class->unrealize  = geda_entry_unrealize;

#if DEBUG_GEDA_ENTRY
  fprintf(stderr, "%s created: history=%d, completion=%d\n",
          __func__, have_history, have_auto_complete );
#endif
}

/*! \brief Type instance initializer for GedaEntry
 *
 *  \par Function Description
 *  Type instance initializer for GedaEntry, initializes a new empty
 *  GedaEntry object.
 *
 *  \param [in] instance The GedaEntry structure being initialized,
 *  \param [in] g_class  The GedaEntry class we are initializing.
 */
static void
geda_entry_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaEntry     *entry  = (GedaEntry*)instance;
  entry->priv           = GEDA_MEM_ALLOC0 (sizeof(GedaEntryPriv));
  GedaEntryPriv *priv   = entry->priv;
  priv->font_map        = pango_cairo_font_map_get_default();

  entry->instance_type  = geda_entry_get_type();

  g_signal_connect_after (G_OBJECT (entry), "key_press_event",
                          G_CALLBACK (geda_entry_key_press), NULL);

  entry->have_history   = have_history;

  if (have_history) {

    g_signal_connect     (G_OBJECT (entry), "process-entry",
                          G_CALLBACK (geda_entry_activate), NULL);

    if (history_list_arg) {

      history_list         = *history_list_arg;
      entry->history_index = g_list_length (history_list);
      entry->max_history   = MAX_ENTRY_HISTORY;
    }
    else {
      entry->history_index = 0;
    }
  }

  g_signal_connect (G_OBJECT (entry), "populate-popup",
                    G_CALLBACK (geda_entry_populate_popup), NULL);

  g_signal_connect (G_OBJECT (entry), "insert_text",
                    G_CALLBACK (geda_entry_validate_input), NULL);

  /* Initialize & populate a GCompletion for commands */
  if (old_complete_list) {

    complete_list            = *old_complete_list;
    priv->command_completion = geda_completion_new (NULL);

    geda_completion_add_items (priv->command_completion, complete_list);

    entry->auto_complete = TRUE;
  }
  else {
    entry->auto_complete = FALSE;
  }

  /* set initial flag state for popup menu*/
  set_auto_complete         = FALSE;
  entry->enable_drag_n_drop = FALSE;
  entry->validation_mode    = ACCEPT_ALL_ASCII;
  entry->text_case          = BOTH_CASES;
  entry->activates_default  = FALSE;
  priv->case_sensitive      = FALSE;

  entry->priv->attrs        = NULL;

#if DEBUG_GEDA_ENTRY
  fprintf(stderr, "%s exit: history=%d, completion=%d\n",
          __func__, entry->have_history, have_auto_complete );
#endif

}

/*! \brief Function to retrieve GedaEntry's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaEntry Type identifier. When
 *  first called, the function registers a #GedaEntry in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaEntry and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaEntry.
 */
GedaType
geda_entry_get_type (void)
{
  static GedaType geda_entry_type = 0;

  if (g_once_init_enter (&geda_entry_type)) {

    static const GTypeInfo info = {
      sizeof(GedaEntryClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_entry_class_init,           /* (GClassInitFunc)   */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaEntry),
      0,                               /* n_preallocs         */
      geda_entry_instance_init         /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaEntry");
    type   = g_type_register_static (GTK_TYPE_ENTRY, string, &info, 0);

    g_once_init_leave (&geda_entry_type, type);
  }

  return geda_entry_type;
}

/*!
 * \brief Check if an object is a GedaEntry
 * \par Function Description
 *  Ensures \a entry is a valid G_Object and compares signature
 *  to geda entry type.
 * \return TRUE if \a entry is a valid GedaEntry
 */
bool
is_a_geda_entry (GedaEntry *entry)
{
  if (G_IS_OBJECT(entry)) {
    return (geda_entry_get_type() == entry->instance_type);
  }
  return FALSE;
}

/*! \brief Entry Stop Activate Default signal Responder
 *  \par Function Description
 *  This function exist to stop GTK-2 from activating the
 *  the default widget when an "Enter" key is press. The
 *  GtkEntry gtk_entry_set_activates_default function does
 *  not work correclty, the default widget will eventually
 *  see the signal regardless of the setting.
 *
 *  \param [in] entry The GedaEntry object
 */
static void
geda_entry_real_activate (GedaEntry *entry)
{
  GtkWindow *window;
  GtkWidget *default_widget, *focus_widget;
  GtkWidget *toplevel;
  GtkWidget *widget;

#if DEBUG_GEDA_ENTRY
  fprintf(stderr, "<%s> in over-ride: got <activate> signal\n", __func__);
#endif

  widget = GTK_WIDGET (entry);

  if (entry->activates_default) {

    toplevel = gtk_widget_get_toplevel (widget);

    if (GTK_IS_WINDOW (toplevel)) {

      window = GTK_WINDOW (toplevel);

      if (window) {

        default_widget = gtk_window_get_default_widget (window);
        focus_widget   = gtk_window_get_focus (window);

        if (widget != default_widget &&
          !(widget == focus_widget && (!default_widget || !gtk_widget_get_sensitive (default_widget))))
          gtk_window_activate_default (window);
      }
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static bool
geda_entry_key_press (GedaEntry *entry, GdkEventKey *event, void *data)
{
  unsigned int state = event->state & gtk_accelerator_get_default_mod_mask ();
  bool handled = FALSE;

  if (( set_auto_complete ) && ( have_auto_complete )) {/* If somebody wants & we have */
    entry->auto_complete = do_auto_complete;
    set_auto_complete = FALSE; /* We did it so reset flag */
  }

  switch (event->keyval) {
    case GDK_KEY_Down:
      if ((state == 0) && (entry->have_history)) {
        geda_entry_history_down (entry);
        handled = TRUE;
      }
      break;
    case GDK_KEY_Up:
      if ((state == 0) && (entry->have_history)) {
        geda_entry_history_up (entry);
        handled = TRUE;
      }
      break;
    case GDK_KEY_KP_Enter:
    case GDK_KEY_Return:
    case GDK_KEY_ISO_Enter:
      handled = TRUE;
      break;
    case GDK_KEY_Tab:
      if ( (state  == 0) && (entry->auto_complete) ) {
        handled = geda_entry_tab_complete (entry);
      }
      break;
    default:
      break;
  }
  return handled;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_grab_focus (GtkWidget *widget)
{
 /* GtkEntry's grab_focus selects the contents and therefore
  * claims PRIMARY. So we bypass it; see bug #345356 and bug #347067.
  */
  GTK_WIDGET_CLASS (geda_entry_parent_class)->grab_focus (widget);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_realize (GtkWidget *widget)
{
  GTK_WIDGET_CLASS (geda_entry_parent_class)->realize (widget);

  if (gtk_widget_has_screen(widget)) {

    GedaEntry     *entry;
    PangoContext  *context;
    PangoLayout   *layout;

    entry = GEDA_ENTRY (widget);

    layout  = gtk_entry_get_layout ((GtkEntry*) widget);
    context = pango_layout_get_context (layout);

    pango_context_set_font_map (context, entry->priv->font_map);
    entry->priv->font_map = g_object_ref (entry->priv->font_map);
    pango_layout_context_changed (layout);

  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_unrealize (GtkWidget *widget)
{
  GedaEntry *entry = GEDA_ENTRY (widget);

  if (entry->priv->font_map) {
    g_object_unref (entry->priv->font_map);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_activate (GedaEntry *entry, void *data)
{
  int list_length;
  GList *iter;
  GList *prev;

  const char *entry_text = geda_entry_get_text(entry);

  /* if user hit enter with no value then ignore entry */
  if ( (entry_text) && (strlen (entry_text) == 0) ) {
    return;
  }

  if (history_list) {                                         /* if not a new buffer */

    list_length = g_list_length (history_list);   /* hit enter so end is now current */
    iter        = g_list_last(history_list);                   /* get the last entry */

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
        iter        = g_list_last(history_list);         /* get the last entry again */
        iter->data  = geda_utility_string_strdup (entry_text);  /* save the new text */
        list_length = g_list_length (history_list); /* is really ++list_length | max */
      }
      else { /* the buffer is not full so just add to the end */
        char *text   = geda_utility_string_strdup (entry_text);
        history_list = g_list_append(history_list, text);
      }
    }
  }
  else { /* we were created with a NULL list, this means glist is a new buffer list */
    char *text   = geda_utility_string_strdup (entry_text);
    history_list = g_list_append(history_list, text);
    list_length  = 1;
  }
  entry->history_index = list_length;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_history_up (GedaEntry *entry)
{
  char *new_line;

  if (entry->history_index > 0) {
    --entry->history_index;
      new_line = g_list_nth_data(history_list, entry->history_index);
      geda_entry_set_text (entry, new_line);
      gtk_editable_set_position (GTK_EDITABLE (GTK_ENTRY (entry)), -1);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
geda_entry_history_down (GedaEntry *entry)
{
  char *new_line;
  int   list_length;

  GtkEntry *gtk_entry = GTK_ENTRY (entry);

  if (entry->history_index < (entry->max_history - 1)) {

    list_length = g_list_length (history_list);

    if (entry->history_index < list_length) {

      if (g_list_nth_data(history_list, entry->history_index + 1)) {

        ++entry->history_index;
        new_line = g_list_nth_data(history_list, entry->history_index);
        geda_entry_set_text (entry, new_line);
        gtk_editable_set_position (GTK_EDITABLE (gtk_entry), -1);
      }
      else { /* There is no more data so set blank line */
        geda_entry_set_text (entry, "");
      }
    }
    else {
      geda_entry_set_text (entry, "");
    }
  }
  else { /* user hit "down" at the end of the buffer make blank line */
    geda_entry_set_text (entry, "");
  }
}

/*! \brief GedaEntry Internal Compare n characters ignoring case.
 *  \par Function Description
 *  Another garden varity string compare using toupper
 *  on both inputs. This is somthimes found in standard.
 *  libraries but not always.
 *
 *  \param [in] str1  is the string to be search
 *  \param [in] str2  is the string to search for
 *  \param [in] n     is the number of char to compare
 *
 *  \retval 0 if the strings are equivalent, -1 if str2 if
 *  first mis-match is because str2 is greater, or 1 if the
 *  first mis-match is because str1 is greater.
 */
static int
geda_entry_strncmpi(char *str1, char *str2, int n)
{
  unsigned int i = 0;
  if (!str1 || !str2) {
    errno = EINVAL;
    return -2;
  }

  while ((toupper(*str1) == toupper(*str2)) && i < n)
  {
    str1++;
    str2++;
    i++;
  }

  if ( i == n)
    return 0;
  else
    if ((*str1 == *str2 ) && (!*str1))
      return 0;
    else
      if ((*str1) && (!*str2))
        return -1;
      else
        if ((*str2) && (!*str1))
          return 1;
        else
          return ((*str1 > *str2 ) ? -1 : 1);
}

static bool
geda_entry_tab_complete (GedaEntry *entry)
{
  char  *buffer;
  char  *s_ptr;
  char  *match;
  GList *options;

  bool exit ( bool answer ) { free ( buffer ); return answer; }

  buffer = calloc(1, max_command_length);

  if (!buffer)
    return FALSE;

  s_ptr = strcpy(buffer, geda_entry_get_text(entry));   /* get the text */

  while ( *s_ptr != ASCII_NUL) ++s_ptr;     /* advance to end of string */

  if (s_ptr == buffer)  /* if string empty */

  if ( *(--s_ptr) == ASCII_SPACE)       /* If previous char is space then */
    return exit (TRUE);                 /* there is nothing to complete */

  while ((s_ptr != buffer) && *s_ptr != ASCII_SPACE) s_ptr--; /* go backwards */

  if (s_ptr != buffer) ++s_ptr;       /* if compounding then skip space */

  options = geda_completion_complete (entry->priv->command_completion, s_ptr, &match);

  if (g_list_length (options) == 0)                    /* if no matches */
    return exit (TRUE);

  if (g_list_length (options) == 1) {                       /* one match */
    strcpy (s_ptr, options->data);

  }
  else
    strcpy (s_ptr, match);

  geda_entry_set_text (entry, buffer);
  gtk_editable_set_position (GTK_EDITABLE (entry), strlen (buffer));

  g_free (match);
  /* Don't free buffer! */;
  return exit (TRUE);
}

/** \defgroup GedaEntry-Popup-Menu GedaEntry Popup Menu
 *  @{
 */

/*! \brief GedaEntry Internal Populate Popup
 *
 *  \par Function Description
 *
 * This functions add the text strings to the popup menu.
 */
static void
geda_entry_populate_popup (GedaEntry *entry, GtkMenu *menu, void *data)
{
  GtkWidget *item;
  GtkWidget *submenu;

  if (have_auto_complete) {
    item = gtk_menu_item_new_with_mnemonic (_("Auto Complete"));
    gtk_widget_show (item);

    submenu = gtk_menu_new ();
    gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

    item = geda_image_menu_item_new_with_label (_("On"));
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_menu_callback), (void*)(long) (1));
    gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);

    item = geda_image_menu_item_new_with_label (_("Off"));
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_menu_callback), (void*)(long) (2));
    gtk_menu_shell_append (GTK_MENU_SHELL (submenu), item);

    gtk_widget_show_all (submenu);
  }
}

/*! \brief GedaEntry Internal Popup Menu Callback
 *
 *  \par Function Description
 *
 * This functions is call when a menu-item in the popup
 * is selected.
 */
static void
popup_menu_callback (GtkMenuItem *item, void    *data)
{
  int menu_option = (int)(long) (data);
  switch(menu_option) {
      case AUTO_COMPLETE_ON:

#if DEBUG_GEDA_ENTRY
        fprintf(stderr, "setting auto complete on\n");
#endif
        set_auto_complete = TRUE;
        do_auto_complete  = TRUE;
        break;
      case AUTO_COMPLETE_OFF:
#if DEBUG_GEDA_ENTRY
        fprintf(stderr, "disabling auto complete\n");
#endif
        set_auto_complete = TRUE;
        do_auto_complete  = FALSE;
        break;
      default:
        break;
  }
}
/** @} endgroup Entry-Popup-Menu */

/* --------------------- Widget Style Functions ----------------- */

/** \defgroup GedaEntry-Style GedaEntry Style Functions
 *  @{
 */

/*! \brief GedaEntry Internal Modify Color Component
 *
 *  \par Function Description
 *
 * This functions is call to modify different color aspects as
 * specified by the component flag.
 */
static void
geda_entry_modify_color_component (GtkWidget      *widget,
                                   GtkRcFlags      component,
                                   GtkStateType    state,
                                   const GdkColor *color)
{
  GtkRcStyle *rc_style;

  rc_style = gtk_widget_get_modifier_style (widget);

  if (color) {

    switch (component) {

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
        BUG_IMSG ("unhandled case=%d", component);
    }

    rc_style->color_flags[state] |= component;
  }
  else
    rc_style->color_flags[state] &= ~component;

  gtk_widget_modify_style (widget, rc_style);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_modify_fg (GedaEntry *entry,
                      GtkStateType state,
                      const GdkColor *color)
{
  geda_entry_widget_modify_color (GTK_WIDGET (entry), GTK_RC_FG, state, color);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_modify_bg (GedaEntry      *entry,
                      GtkStateType    state,
                      const GdkColor *color)
{
  geda_entry_widget_modify_color (GTK_WIDGET (entry), GTK_RC_BG, state, color);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
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

/** @} endgroup Entry-Style */

/** \defgroup GedaEntry-Widget-Methods GedaEntry Widget Methods
 *  @{
 */

PangoAttrList*
geda_entry_widget_get_attributes (GtkWidget *entry)
{
  return geda_entry_get_attributes (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int
geda_entry_widget_get_max_history (GtkWidget *entry)
{
  return geda_entry_get_max_history (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_set_max_history (GtkWidget *entry, int value)
{
  geda_entry_set_max_history (GEDA_ENTRY(entry), value);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GedaCompletion *
geda_entry_widget_get_completion (GtkWidget *entry)
{
  return geda_entry_get_completion (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool
geda_entry_widget_completion_get_case (GtkWidget *entry)
{
  return geda_entry_completion_get_case (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_completion_set_case (GtkWidget *entry, bool sensitive)
{
  geda_entry_completion_set_case (GEDA_ENTRY(entry), sensitive);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool
geda_entry_widget_get_input_case (GtkWidget *entry)
{
  return geda_entry_get_input_case (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_set_input_case (GtkWidget *entry, int mode)
{
  return geda_entry_set_input_case (GEDA_ENTRY(entry), mode);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char*
geda_entry_widget_get_text (GtkWidget *entry)
{
  return geda_entry_get_text (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool
geda_entry_widget_get_activates_default (GtkWidget *entry)
{
  return geda_entry_get_activates_default (GEDA_ENTRY(entry));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_set_activates_default (GtkWidget *entry, bool  setting)
{
  geda_entry_set_activates_default (GEDA_ENTRY(entry), setting);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_set_attributes (GtkWidget *entry, PangoAttrList *attrs)
{
  geda_entry_set_attributes (GEDA_ENTRY(entry), attrs);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_set_text (GtkWidget *entry, const char *new_text)
{
  geda_entry_set_text (GEDA_ENTRY(entry), new_text);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_set_valid_input (GtkWidget *entry, GedaEntryAccept mode)
{
  geda_entry_set_valid_input (GEDA_ENTRY(entry), mode);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_modify_fg (GtkWidget *entry,
                             GtkStateType state,
                             const GdkColor *color)
{
  geda_entry_widget_modify_color (entry, GTK_RC_FG, state, color);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
geda_entry_widget_modify_bg (GtkWidget      *entry,
                             GtkStateType    state,
                             const GdkColor *color)
{
  geda_entry_widget_modify_color (entry, GTK_RC_BG, state, color);
}

/** @} endgroup GedaEntry-Widget-Methods */

/* -------------------------------------------------------------- */

/** \defgroup GedaEntry-Creators GedaEntry Creator Functions
 *  @{
 */

GtkWidget*
geda_entry_new (GList** history, GList** complete)
{

  if ((int)(long)history == -1)
    have_history = FALSE;
  else {
    history_list_arg = history;
    have_history = TRUE;
  }

  if ((int)(long)complete == -1)
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

GtkWidget*
geda_entry_new_visible (GList** history, GList** complete)
{
  GtkWidget *entry;
  entry = geda_entry_new ( history, complete);
  g_object_set (entry, "visible", TRUE, NULL);
  return entry;
}

/*! \brief Create a New GedaEntry with Auxiliary Text Buffer
 *  \par Function Description
 *
 * Creates a new entry with the specified text buffer.
 *
 * \param [in] buffer:  The buffer to use for the new #GedaEntry.
 *
 * \return a new #GedaEntry
 */
GtkWidget*
geda_entry_new_with_buffer (GtkEntryBuffer *buffer)
{
  g_return_val_if_fail (GTK_IS_ENTRY_BUFFER (buffer), NULL);
  return g_object_new  (GEDA_TYPE_ENTRY, "buffer", buffer, NULL);
}

/*! \brief Create a New GedaEntry specified max length property
 *  \par Function Description
 *
 * Creates a new entry and sets the max-length property to \a max_length,
 * which does not really do much.
 *
 * \param [in] max_length Value to set the Max length property.
 *
 * \return a new #GedaEntry
 */
GtkWidget*
geda_entry_new_with_max_length (int max_length)
{
  GtkWidget *entry;
  entry = geda_entry_new (NO_HISTORY, NO_COMPLETION);
  g_object_set (entry, "max-length", max_length, NULL);
  return entry;
}

/** @} endgroup Entry-Creators */
/** @} end group GedaEntry */
