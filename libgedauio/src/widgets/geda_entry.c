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
 * published by the Free Software Foundation; version 2 of the
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
#include "../../../config.h"
#endif

#include <ctype.h>

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_container.h"
#include "../../include/geda_entry.h"
#include "../../include/geda_image_menu_item.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_marshal.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_widget.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaEntry - A single line text entry field
 * \par
 * The #GedaEntry widget is a single line text entry widget supporting a
 * fairly large set of key bindings by default. If the entered text is
 * longer than the allocation of the widget, the widget will scroll so
 * that the cursor position is visible. For larger allocations the widget
 * should be combined with a text buffer as is done in the console_init()
 * function in gschem.
 * \par
 * An ill effect of having a GList** for the history buffer parameter is
 * that if NULL is passed as the first parameter, the history buffer
 * argument, to the construction "new" functions, then the NULL will be
 * interpreted as an empty history list and the history feature will be
 * enabled. Since passing -1 will generate a compiler warning, because
 * the compiler is expecting a GList**, a convienence macro is provided
 * in the header: NO_HISTORY, in order to type cast the argument. A second
 * macro, NO_COMPLETION is also provided for the second argument, which
 * could also be NULL because a NULL completion is interpreted to mean
 * disabling the completion feature.
 * \par
 * example: entry = geda_entry_new_visible ();
 * \par
 * example: entry = geda_entry_new_history_complete(&history_list, &word_list);
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

static void    geda_entry_get_property       (GObject          *object,
                                              unsigned int      property_id,
                                              GValue           *value,
                                              GParamSpec       *pspec);
static void    geda_entry_set_property       (GObject          *object,
                                              unsigned int      property_id,
                                              const GValue     *value,
                                              GParamSpec       *pspec);

static void    geda_entry_real_activate      (GedaEntry        *entry);

static void    geda_entry_grab_focus         (GtkWidget        *widget);
static void    geda_entry_realize            (GtkWidget        *widget);
static void    geda_entry_unrealize          (GtkWidget        *widget);

static void    geda_entry_process_entry      (GedaEntry        *entry,
                                              void             *data);
static void    geda_entry_history_up         (GedaEntry        *entry);
static void    geda_entry_history_down       (GedaEntry        *entry);
static bool    geda_entry_key_press          (GedaEntry        *widget,
                                              GdkEventKey      *event,
                                              void             *data);
static bool    geda_entry_widget_key_press   (GtkWidget        *widget,
                                              GdkEventKey      *event);
static int     geda_entry_strncmpi           (char             *str1,
                                              char             *str2,
                                              int               n);
static bool    geda_entry_tab_complete       (GedaEntry        *entry);
static void    geda_entry_validate_input     (GtkEntry         *entry,
                                              const char       *text,
                                              int               length,
                                              int              *position,
                                              void             *data);
static void    popup_menu_callback           (GedaMenuItem     *item,
                                              void             *data);
static void    geda_entry_populate_popup     (GedaEntry        *entry,
                                              GtkMenu          *menu,
                                              void             *data);

/* These flags and pointers are used for construction */
static GList **history_list_arg;
static  bool   have_history;

static GList  *complete_list;
static GList **old_complete_list;

static bool    have_auto_complete;

static void *geda_entry_parent_class = NULL;

static GHashTable *entry_hash_table = NULL;

struct _GedaEntryPriv
{
  GedaCompletion *command_completion;
  PangoAttrList  *attrs;
  PangoFontMap   *font_map;
  GList          *history_list;
  GList         **history_store;
  bool            case_sensitive;
  int             change_count;
};

const char *IDS_CONSOLE_POPUP[] = {
  "Auto Complete", "On", "Off", /* Popup Menu Strings*/
  NULL
};

/*!
 * \brief Freeze Notify before Inserting Text
 * \par Function Description
 *  Internal helper to stop emission of signals before inserting text.
 */
static inline void
begin_change (GedaEntry *entry)
{
  GedaEntryPriv *priv = entry->priv;

  priv->change_count++;

  g_object_freeze_notify (G_OBJECT (entry));
}

/*!
 * \brief Thaw Notify After Text Insertion Completed
 * \par Function Description
 *  Re-enable emission of signals after insetion of text completed.
 */
static inline void end_change (GedaEntry *entry)
{
  GedaEntryPriv *priv = entry->priv;

  g_object_thaw_notify (G_OBJECT (entry));

  priv->change_count--;
}

/*!
 * \brief gobject_class->finalize a GedaEntry object
 * \par Function Description
 *  Releases resources associated with the GedaEntry object.
 *  The object should not be referenced after this function
 *  is executes.
 */
static void geda_entry_finalize (GObject *object)
{
  GedaEntry *entry = (GedaEntry*)object;

  if (g_hash_table_remove (entry_hash_table, object)) {
    if (!g_hash_table_size (entry_hash_table)) {
      g_hash_table_destroy (entry_hash_table);
      entry_hash_table = NULL;
    }
  }

  if (entry->priv->command_completion) {
    geda_completion_free (entry->priv->command_completion);
  }

  /* Save history to caller's glist*/
  if (entry->have_history)
    *entry->priv->history_store = entry->priv->history_list;

  ((GObjectClass*)geda_entry_parent_class)->finalize (object);

  if (entry->priv->attrs) {
    pango_attr_list_unref (entry->priv->attrs);
  }

  if (entry->priv->font_map) {

#ifndef __MINGW32__

    pango_cairo_font_map_set_default (NULL);

#endif

    entry->priv->font_map = NULL;
  }

  g_free (entry->priv);
}

/*! gobject_class->get_property
 * \brief GObject property getter function for a GedaEntry Object
 * \par Function Description
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
  GedaEntry *entry = (GedaEntry*)object;

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

/*! gobject_class->set_property
 * \brief GObject property setter for a GedaEntry Object
 * \par Function Description
 *  Setter function for GedaEntry's GObject properties.
 *
 * \param [in] object       The GObject whose properties we are setting
 * \param [in] property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 * \param [in] value        The GValue the property is being set from
 * \param [in] pspec        A GParamSpec describing the property being set
 */
static void
geda_entry_set_property (GObject *object, unsigned int  property_id,
                         const  GValue  *value,  GParamSpec   *pspec)
{
  GedaEntry *entry = (GedaEntry*)object;

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

/*!
 * \brief Entry Stop Activate Default signal Responder
 * \par Function Description
 *  This function exist to stop GTK-2 from activating the
 *  the default widget when an "Enter" key is press. The
 *  GtkEntry gtk_entry_set_activates_default function does
 *  not work correclty, the default widget will eventually
 *  see the signal regardless of the setting.
 *
 * \param [in] entry The GedaEntry object
 */
static void geda_entry_real_activate (GedaEntry *entry)
{
#if DEBUG_GEDA_ENTRY
  fprintf(stderr, "<%s> in over-ride: got <activate> signal\n", __func__);
#endif

  if (entry->activates_default) {

    GtkWidget *widget   = (GtkWidget*)entry;
    GtkWidget *toplevel = gtk_widget_get_toplevel (widget);

    if (GTK_IS_WINDOW (toplevel)) {

      GtkWindow *window = (GtkWindow*)toplevel;

      if (window) {

        GtkWidget *default_widget;
        GtkWidget *focus_widget;

        default_widget = gtk_window_get_default_widget (window);
        focus_widget   = gtk_window_get_focus (window);

        if (widget != default_widget &&
          !(widget == focus_widget && (!default_widget || !gtk_widget_get_sensitive (default_widget))))
        {
          gtk_window_activate_default (window);
        }
      }
    }
  }
}

/** \defgroup geda-entry-wcvo GedaEntry Widget Class Virtual Overrides
  * @{
  */

/*!
 * \brief Drag & Drop Callback when drag Begins
 * \par Function Description
 *  Over-rides widget_class->drag_begin.
 */
static void geda_entry_drag_begin (GtkWidget *widget, GdkDragContext *context)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
}

/*!
 * \brief Drag & Drop Callback when dropped on a GedaEntry
 * \par Function Description
 *  Over-rides widget_class->drag_drop. This is primarily to over-ride
 *  the default behavior of GtkEntry, Drag & Drop is not implemented
 *  for GedaEntry widgets.
 */
static bool geda_entry_drag_drop (GtkWidget      *widget,
                                  GdkDragContext *context,
                                  int             x,
                                  int             y,
                                  unsigned int    time)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }

  return FALSE; /* No continue */
}

/*!
 * \brief Drag & Drop End Callback GedaEntry On End
 * \par Function Description
 *  Over-rides widget_class->drag_end. This is primarily to over-ride
 *  the default behavior of GtkEntry, Drag & Drop is not implemented
 *  for GedaEntry widgets. Normally used to release resources associated
 *  with the prior Drag & Drop operation, may set cursor back to normal
 *  if was changed.
 */
static void geda_entry_drag_end (GtkWidget *widget, GdkDragContext *context)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
}

/*!
 * \brief When Drag for GedaEntry Leaves the Destination
 * \par Function Description
 *  Over-rides widget_class->drag_leave. This is primarily to
 *  over-ride the default behavior of GtkEntry, Drag & Drop is
 *  not implemented for GedaEntry widgets. Called when the drag
 *  leaves the destination.
 */
static void geda_entry_drag_leave (GtkWidget      *widget,
                GdkDragContext *context,
                                   unsigned int    time)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
  gtk_widget_queue_draw (widget);
}

/*!
 * \brief When Drag for GedaEntry Motion is over the Destination
 * \par Function Description
 *  Over-rides widget_class->drag_motion. This is primarily to
 *  over-ride the default behavior of GtkEntry, Drag & Drop is not
 *  implemented for GedaEntry widgets. Called when the drag is over
 *  the destination.
 */
static bool geda_entry_drag_motion (GtkWidget       *widget,
                                    GdkDragContext  *context,
                                    int              x,
                                    int              y,
                                    unsigned int     time)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
  return FALSE; /* not here */
}

/*!
 * \brief Get Drag & Drop Data
 * \par Function Description
 *  Over-rides widget_class->drag_data_get.
 */
static void geda_entry_drag_data_get (GtkWidget        *widget,
                                      GdkDragContext   *context,
                                      GtkSelectionData *selection_data,
                                      unsigned int      info,
                                      unsigned int      time)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
}

/*!
 * \brief Delete Drag & Drop Data
 * \par Function Description
 *  Over-rides widget_class->drag_data_delete.
 */
static void geda_entry_drag_data_delete (GtkWidget *widget, GdkDragContext *context)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;

  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
}

/*!
 * \brief Received Drag & Drop Data
 * \par Function Description
 *  Over-rides widget_class->drag_data_received.
 */
static void geda_entry_drag_data_received (GtkWidget        *widget,
                                           GdkDragContext   *context,
                                           int              x,
                                           int              y,
                                           GtkSelectionData *selection_data,
                                           unsigned int     info,
                                           unsigned int     time)
{
  GedaEntry *geda_entry = (GedaEntry*)widget;
  if (geda_entry->enable_drag_n_drop) {
    fprintf (stderr, "TODO: %s\n", __func__);
  }
}

/*! widget_class->grab_focus
 * \brief GedaEntry Grab Focus
 * \par Function Description
 * Over-rides widget_class->grab_focus. GtkEntry's grab_focus selects
 * the contents and therefore claims PRIMARY. So we bypass it; see
 * bug #345356 and bug #347067.
 */
static void geda_entry_grab_focus (GtkWidget *widget)
{
  ((GtkWidgetClass*)geda_entry_parent_class)->grab_focus (widget);
}

static bool geda_entry_widget_key_press (GtkWidget *widget, GdkEventKey *event)
{
  if (event->keyval == GDK_KEY_KP_Enter ||
      event->keyval == GDK_KEY_Return ||
      event->keyval == GDK_KEY_ISO_Enter)
  {
    GedaEntry *entry = (GedaEntry*)widget;

    if (entry->activates_default) {
      GEDA_OBJECT_NOTIFY (entry, "activate");
    }
  }

  return ((GtkWidgetClass*)geda_entry_parent_class)->key_press_event (widget, event);
}

/*! widget_class->realize
 * \brief GedaEntry Realize
 * \par Function Description
 * Over-rides widget_class->realize, chains-up and then retrieves
 * and saves pointer to the font_map.
 */
static void geda_entry_realize (GtkWidget *widget)
{
  ((GtkWidgetClass*)geda_entry_parent_class)->realize (widget);

  if (gtk_widget_has_screen(widget)) {

    GedaEntry    *entry;
    PangoContext *context;
    PangoLayout  *layout;

    entry = (GedaEntry*)widget;

    layout  = gtk_entry_get_layout ((GtkEntry*)widget);
    context = pango_layout_get_context (layout);

    pango_context_set_font_map (context, entry->priv->font_map);
    entry->priv->font_map = g_object_ref (entry->priv->font_map);
    pango_layout_context_changed (layout);
  }
}

/*! widget_class->unrealize
 * \brief Unrealize a GedaEntry
 * \par Function Description
 * Over-rides widget_class->unrealize to unreference the font_map
 * referenced when realized.
 */
static void geda_entry_unrealize (GtkWidget *widget)
{
  GedaEntry *entry = (GedaEntry*)widget;

  if (entry->priv->font_map) {
    g_object_unref (entry->priv->font_map);
  }

  ((GtkWidgetClass*)geda_entry_parent_class)->unrealize (widget);
}

/** @} geda-entry-wcvo */

/*!
 * \brief GedaEntry Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  klass       GedaEntry class we are initializing
 * \param [in]  class_data  GedaEntry structure associated with the class
 */
static void geda_entry_class_init(void *klass, void *class_data)
{
  GedaEntryClass *class;
  GParamSpec     *params;
  GObjectClass   *gobject_class;
  GtkWidgetClass *widget_class;
  GtkBindingSet  *binding_set;

  class         = (GedaEntryClass*) klass;
  gobject_class = (GObjectClass*)   klass;
  widget_class  = (GtkWidgetClass*) klass;

  geda_entry_parent_class     = g_type_class_peek_parent (klass);

  gobject_class->finalize     = geda_entry_finalize;
  gobject_class->get_property = geda_entry_get_property;
  gobject_class->set_property = geda_entry_set_property;

  class->activate             = geda_entry_real_activate;

  /* We over-ride parent's drag&drop, which is over-riding widget class
   * because we support drag&drop stuff other than just text and the
   * stock entry intecepts all of drag&drop signals! */
  class->drag_begin           = widget_class->drag_begin;
  class->drag_drop            = widget_class->drag_drop;
  class->drag_end             = widget_class->drag_end;
  class->drag_leave           = widget_class->drag_leave;
  class->drag_motion          = widget_class->drag_motion;
  class->drag_data_get        = widget_class->drag_data_get;
  class->drag_data_delete     = widget_class->drag_data_delete;
  class->drag_data_received   = widget_class->drag_data_received;

  widget_class->drag_begin         = geda_entry_drag_begin;
  widget_class->drag_drop          = geda_entry_drag_drop;
  widget_class->drag_end           = geda_entry_drag_end;
  widget_class->drag_leave         = geda_entry_drag_leave;
  widget_class->drag_motion        = geda_entry_drag_motion;
  widget_class->drag_data_get      = geda_entry_drag_data_get;
  widget_class->drag_data_delete   = geda_entry_drag_data_delete;
  widget_class->drag_data_received = geda_entry_drag_data_received;

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
                              LOWER_CASE, /* Min value */
                              BOTH_CASES, /* Max value */
                              BOTH_CASES, /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_INPUT_CASE, params);

  params = g_param_spec_int ("max-history",
                           _("Set maxium history"), /* nick name */
                           _("maximum length of history"), /* hint / blurb */
                              0,        /* Min value */
                              G_MAXINT, /* Max value */
                              MAX_ENTRY_HISTORY,      /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MAX_HISTORY, params );

  params = g_param_spec_int ("accept-type",
                           _("Set valid input type"), /* nick name */
                           _("0 = All, 1 Alphnumeric, 3 Numeric, 4 Number, 5 Integer, 6 Real"), /* hint / blurb */
                              ACCEPT_ALL_ASCII, /* Min value */
                              ACCEPT_REAL,      /* Max value */
                              ACCEPT_ALL_ASCII, /* default_value */
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_VALIDATE, params);

  /*!
   * GedaEntry::process-entry:
   * entry: The entry on which the signal is emitted
   *
   * The GedaEntry::process-entry signal is emitted when the user hits the Enter
   * key. This is the same as process-entry GtkEntry::activate.
   *
   * While this signal is used as a keybinding signal, it is also commonly
   * used by applications to intercept activation of entries.
   *
   * The default bindings for this signal are all forms of the Enter key.
   */
  signals[PROCESS_ENTRY] = g_signal_new ("process-entry",
                                    geda_entry_get_type(),
                                    G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                                    G_STRUCT_OFFSET (GedaEntryClass, activate),
                                    NULL, NULL,
                                    geda_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  widget_class->activate_signal = signals[PROCESS_ENTRY];

  /*  Key bindings */
  binding_set = gtk_binding_set_by_class (class);

  /* Activate */
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_Return, 0,    "process-entry", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_ISO_Enter, 0, "process-entry", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_KP_Enter, 0,  "process-entry", 0);

  widget_class->grab_focus      = geda_entry_grab_focus;
  widget_class->key_press_event = geda_entry_widget_key_press;
  widget_class->realize         = geda_entry_realize;
  widget_class->unrealize       = geda_entry_unrealize;

#if DEBUG_GEDA_ENTRY
  fprintf(stderr, "%s created: history=%d, completion=%d\n",
          __func__, have_history, have_auto_complete);
#endif
}

/*!
 * \brief Type instance initializer for GedaEntry
 * \par Function Description
 *  Type instance initializer for GedaEntry, initializes a new empty
 *  GedaEntry object.
 *
 * \param [in] instance The GedaEntry structure being initialized,
 * \param [in] g_class  The GedaEntry class we are initializing.
 */
static void geda_entry_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaEntry     *entry  = (GedaEntry*)instance;
  entry->priv           = GEDA_MEM_ALLOC0 (sizeof(GedaEntryPriv));
  GedaEntryPriv *priv   = entry->priv;
  priv->font_map        = pango_cairo_font_map_get_default();

  entry->have_history   = have_history;
  entry->auto_complete  = have_auto_complete;

  if (have_history) {

    g_signal_connect     (G_OBJECT (entry), "process-entry",
                          G_CALLBACK (geda_entry_process_entry), NULL);

    if (history_list_arg) {

      priv->history_store  =  history_list_arg;
      priv->history_list   = *history_list_arg;
      entry->history_index =  g_list_length (priv->history_list);
      entry->max_history   =  MAX_ENTRY_HISTORY;
    }
    else {
      entry->history_index = 0;
    }
  }

  /* Initialize & populate a GCompletion for commands */
  if (entry->auto_complete) {

    complete_list            = g_list_copy(*old_complete_list);
    priv->command_completion = geda_completion_new (NULL);

    geda_completion_add_items (priv->command_completion, complete_list);

    entry->completion_enabled = TRUE;
  }
  else {
    entry->completion_enabled = FALSE;
  }

  /* set initial flag states */
  entry->enable_drag_n_drop = FALSE;
  entry->validation_mode    = ACCEPT_ALL_ASCII;
  entry->text_case          = BOTH_CASES;
  entry->activates_default  = FALSE;

  /* priv data already initialized to zeros
  priv->case_sensitive      = FALSE;
  priv->attrs               = NULL; */

  g_signal_connect_after (G_OBJECT (entry), "key-press-event",
                          G_CALLBACK (geda_entry_key_press), NULL);

  g_signal_connect_object (G_OBJECT (entry), "populate-popup",
                           G_CALLBACK (geda_entry_populate_popup), NULL, 0);

  g_signal_connect_object (G_OBJECT (entry), "insert-text",
                           G_CALLBACK (geda_entry_validate_input), NULL, 0);

  if (!entry_hash_table) {
    entry_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (entry_hash_table, instance, instance);
}

/*!
 * \brief Function to retrieve GedaEntry's Type identifier
 * \par Function Description
 *  Function to retrieve a #GedaEntry Type identifier. When
 *  first called, the function registers a #GedaEntry in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaEntry and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaEntry.
 */
GedaType geda_entry_get_type (void)
{
  static volatile GedaType geda_entry_type = 0;

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
 *  Determines if \a entry is valid by verifying \a entry
 *  is included in the hash table of GedaEntry objects.
 *
 * \return TRUE if \a entry is a valid GedaEntry
 */
bool is_a_geda_entry (GedaEntry *entry)
{
  if ((entry != NULL) && (entry_hash_table != NULL)) {
    return g_hash_table_lookup(entry_hash_table, entry) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief On process-entry a GedaEntry
 * \par Function Description
 * This is a callback for the "process-entry" signal. If there is text
 * in the entry and a history_list the history_list is updated.
 */
static void geda_entry_process_entry (GedaEntry *entry, void *data)
{
  GedaEntryPriv *priv = entry->priv;
  int list_length;

  const char *entry_text = geda_entry_get_text(entry);

  /* if user hit enter with no value then ignore entry */
  if ((entry_text) && (strlen (entry_text) == 0) ) {
    return;
  }

  if (priv->history_list) {                                   /* if not a new buffer */

    GList *history;
    GList *iter;

    history     = priv->history_list;
    list_length = g_list_length (history);        /* hit enter so end is now current */
    iter        = g_list_last(history);                        /* get the last entry */

    if (g_ascii_strcasecmp (iter->data, entry_text) != 0) {   /* same as last entry? */

      ++list_length;                                /* if not then increment counter */

      if (list_length > entry->max_history) {                  /* at history limit?  */

        GList *prev;

        iter = g_list_first(history);                          /* get the start */
        g_free(iter->data);                                 /* first the oldest data */
        prev = iter;                       /* but save address that held the pointer */
        for (iter = iter->next; iter != NULL; iter = iter->next) {
          prev->data = iter->data;                           /* rotate pointers down */
          prev       = iter;
        }
        iter        = g_list_last(history);         /* get the last entry again */
        iter->data  = geda_strdup (entry_text);     /* save the new text */
        list_length = g_list_length (history);      /* is really ++list_length | max */
      }
      else { /* the buffer is not full so just add to the end */
        char *text = geda_strdup (entry_text);
        priv->history_list = g_list_append(history, text);
      }
    }
  }
  else { /* we were created with a NULL list, this means glist is a new buffer list */
    char *text         = geda_strdup (entry_text);
    priv->history_list = g_list_append(NULL, text);
    list_length        = 1;
  }

  entry->history_index = list_length;
}

/*!
 * \brief Go back in history
 * \par Function Description
 *  Call when the arrow-up key is press. This function decrements the
 *  history index and replaces the text in the entry with the text at
 *  the resulting index in the history list.
 */
static void geda_entry_history_up (GedaEntry *entry)
{
  if (entry->history_index > 0) {

    char *new_line;

    --entry->history_index;
      new_line = g_list_nth_data(entry->priv->history_list, entry->history_index);
      geda_entry_set_text (entry, new_line);
      gtk_editable_set_position (GTK_EDITABLE (GTK_ENTRY (entry)), -1);
  }
}

/*!
 * \brief Go Forward in history
 * \par Function Description
 *  Called when the arrow-down key is press. This function increments
 *  the history index and replaces the text in the entry with the text
 *  at the resulting index in the history list if such and index exist.
 *  If the history is at end of the list then the entry text is set to
 *  an empty line.
 */
static void geda_entry_history_down (GedaEntry *entry)
{
  if (entry->history_index < (entry->max_history - 1)) {

    GedaEntryPriv *priv = entry->priv;

    int list_length = g_list_length (priv->history_list);

    if (entry->history_index < list_length) {

      if (g_list_nth_data(priv->history_list, entry->history_index + 1)) {

        char *new_line;

        ++entry->history_index;
        new_line = g_list_nth_data(priv->history_list, entry->history_index);
        geda_entry_set_text (entry, new_line);
        gtk_editable_set_position (GTK_EDITABLE (entry), -1);
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

/*!
 * \brief GedaEntry on key-press event
 * \par Function Description
 *  Keyboard hook routine for auto-completion and history.
 */
static bool geda_entry_key_press (GedaEntry *entry, GdkEventKey *event, void *data)
{
  unsigned int state = event->state & gtk_accelerator_get_default_mod_mask ();
  bool handled = FALSE;

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
      if ( (state  == 0) && (entry->completion_enabled) ) {
        handled = geda_entry_tab_complete (entry);
      }
      break;

    default:
      break;
  }

  return handled;
}

/*!
 * \brief GedaEntry Internal Compare n characters ignoring case.
 * \par Function Description
 *  Another garden varity string compare using toupper
 *  on both inputs. This is somthimes found in standard.
 *  libraries but not always.
 *
 * \param [in] str1  is the string to be search
 * \param [in] str2  is the string to search for
 * \param [in] n     is the number of char to compare
 *
 * \retval 0 if the strings are equivalent, -1 if str2 if
 *           first mis-match is because str2 is greater, or 1 if the
 *           first mis-match is because str1 is greater.
 */
static int geda_entry_strncmpi(char *str1, char *str2, int n)
{
  unsigned int i = 0;
  if (!str1 || !str2) {
    errno = EINVAL;
    return -2;
  }

  while ((toupper(*str1) == toupper(*str2)) && i < n) {
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

static bool geda_entry_tab_complete (GedaEntry *entry)
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

  if (g_list_length (options)) {                 /* if there is a match */

    if (g_list_length (options) == 1) {                    /* one match */
      strcpy (s_ptr, options->data);

    }
    else {
      strcpy (s_ptr, match);
    }

    geda_entry_set_text (entry, buffer);
    gtk_editable_set_position (GTK_EDITABLE (entry), strlen (buffer));

    g_free (match);
  }

  /* Don't free buffer! */;
  return exit (TRUE);
}

/*!
 * \brief GedaEntry insert Text
 * \par Function Description
 *  Wrapper for gtk_entry_buffer_insert_text. Inserts \a text_length
 *  characters of \a new_text into the contents of the entry buffer,
 *  starting at position \a position. If \a text_length is negative,
 *  then characters from \a new_text will be inserted until a null
 *  terminator is found. If \a position or the maximum buffer text
 *  length is exceeded, then they are coerced to sane values. Note
 *  that the position and length are in characters not in bytes.
 *
 * \param [in] entry        Pointer to a #GedaEntry object.
 * \param [in] new_text     Pointer to string to be inserted
 * \param [in] text_length  Integer length of the new string
 * \param [in] position     Integer position to insert the text.
 */
static void
geda_entry_real_insert_text (GedaEntry  *entry,
                             const char *new_text,
                             int         text_length,
                             int        *position)
{
  GtkEntryBuffer *buffer;
  unsigned int n_inserted;
  int n_chars;

  n_chars = g_utf8_strlen (new_text, text_length);

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

  n_inserted = gtk_entry_buffer_insert_text (buffer, *position,
                                             new_text, n_chars);

  end_change (entry);

  *position += n_inserted;
}

/*!
 * \brief Validate GedaEntry Input
 * \par Function Description
 *  Callback for insert-text signal, called when text is inserted
 *  into the entry to validate characters based on validation_mode.
 */
static void geda_entry_validate_input (GtkEntry    *entry,
                                       const char  *text,
                                       int          length,
                                       int         *position,
                                       void        *data)
{
  GedaEntry *geda_entry = (GedaEntry*)entry;

  char *result = g_malloc (length + 1);
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

    result[count] = 0;

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

/** \defgroup GedaEntry-Popup-Menu GedaEntry Popup Menu
 *  @{
 */

/*!
 * \brief GedaEntry Internal Popup Menu Callback
 * \par Function Description
 *  This functions is called when a menu-item in the popup
 *  is selected.
 */
static void popup_menu_callback (GedaMenuItem *item, void *data)
{
  GedaEntry *entry;

  int menu_option = (int)(long)data;

  entry = g_object_get_data (G_OBJECT(item), "eda-entry");

  switch (menu_option) {
      case AUTO_COMPLETE_ON:

#if DEBUG_GEDA_ENTRY
        fprintf(stderr, "setting auto complete on\n");
#endif
        entry->completion_enabled  = TRUE;
        break;

      case AUTO_COMPLETE_OFF:

#if DEBUG_GEDA_ENTRY
        fprintf(stderr, "disabling auto complete\n");
#endif
        entry->completion_enabled  = FALSE;
        break;

      default:
        break;
  }
}

/*!
 * \brief GedaEntry Internal Populate Popup
 * \par Function Description
 *  This functions add the text strings to the popup menu. The menu
 *  is a Gtk based menu because GedaEntry is derived from GtkEntry
 *  class, which creates the parent menu.
 *
 * \todo consider replacing submenu with a check menu item
 */
static void geda_entry_populate_popup (GedaEntry *entry, GtkMenu *menu, void *data)
{
  if (entry->auto_complete) {

    GtkWidget *item;
    GtkWidget *submenu;

    item = gtk_menu_item_new_with_mnemonic (_("Auto Complete"));
    gtk_widget_show (item);

    submenu = gtk_menu_new ();
    gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);

    geda_container_add (menu, item);

    item = gtk_menu_item_new_with_label (_("On"));
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_menu_callback), (void*)(long) (1));
    g_object_set_data (G_OBJECT(item), "eda-entry", entry);
    geda_container_add (submenu, item);

    item = gtk_menu_item_new_with_label (_("Off"));
    g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (popup_menu_callback), (void*)(long) (2));
    g_object_set_data (G_OBJECT(item), "eda-entry", entry);
    geda_container_add (submenu, item);

    gtk_widget_show_all (submenu);
  }
}

/** @} endgroup Entry-Popup-Menu */

/** \defgroup GedaEntry-Puplic GedaEntry Puplic
 *  @{
 */

/*!
 * \brief Retrieve the Text from a GedaEntry
 * \par Function Description
 *  This function gets the text characters in a GedaEntry.
 *
 * \param[in] entry Pointer a #GedaEntry object
 *
 * \returns the contents of the text entry buffer
 */
const char *geda_entry_get_text (GedaEntry *entry)
{
  GtkEntryBuffer *buffer;

  g_return_val_if_fail (GEDA_IS_ENTRY (entry), NULL);

  g_object_get (entry, "buffer", &buffer, NULL);

  return gtk_entry_buffer_get_text (buffer);
}

/*!
 * \brief Set the Text in a GedaEntry
 * \par Function Description
 *  This function sets the text in a GedaEntry if \a new_text
 *  is not equivalent to the string already in the buffer. No
 *  signal is emitted.
 *
 * \param[in] entry    Pointer #GedaEntry object
 * \param[in] new_text Text to be loaded into the GedaEntry.
 */
void geda_entry_set_text (GedaEntry *entry, const char *new_text)
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

/*!
 * \brief Get the Text Length in the GedaEntry
 * \par Function Description
 *  Retrieves the string a returns the length.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 *
 * \returns length of the string or -1 if entry is invalid
 */
int geda_entry_get_text_length (GedaEntry *entry)
{
  const char *curr_text;

  curr_text = geda_entry_get_text (entry);

  if (curr_text) {
    return strlen(curr_text);
  }
  return -1;
}

/*!
 * \brief Get GedaEntry Activates Default
 * \par Function Description
 *  Retrieves the value set by gtk_entry_set_activates_default().
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 *
 * \retval %TRUE if the entry will activate the default widget
 */
bool geda_entry_get_activates_default (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), FALSE);

  return entry->activates_default;
}

/*!
 * \brief Set GedaEntry Activates Default
 * \par Function Description
 *  If setting is %TRUE, pressing Enter in the entry will activate the
 *  default widget for the window containing the entry. This usually means
 *  that the dialog box containing the entry will be closed, since the
 *  default widget is usually one of the dialog buttons.
 *
 *  (For experts: if the setting is %TRUE, the entry calls
 *  gtk_window_activate_default() on the window containing the entry,
 *  in the default handler for the activate signal.)
 *
 * \param [in] entry    Pointer to a #GedaEntry object.
 * \param [in] setting  The desired setting.
 */
void geda_entry_set_activates_default (GedaEntry *entry, bool setting)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));

  setting = setting != FALSE;

  if (setting != entry->activates_default) {
    entry->activates_default = setting;
    GEDA_OBJECT_NOTIFY (entry, "activates-default");
  }
}

/*!
 * \brief Get GedaEntry Attribute List
 * \par Function Description
 * Gets the attribute list that was set on the entry using
 * geda_entry_set_attributes(), if any.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 *
 * \retval PangoAttrList attribute list, or %NULL
 *         if none was set.
 */
PangoAttrList *geda_entry_get_attributes (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), NULL);

  return entry->priv->attrs;
}

/*!
 * \brief Set GedaEntry Attribute List
 * \par Function Description
 * This function applies the PangoAttrList to entry text font
 * description.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 * \param [in] attrs Pointer to a PangoAttrList structure.
 */
void geda_entry_set_attributes (GedaEntry *entry, PangoAttrList *attrs)
{
  PangoAttrList *old_attrs;
  PangoLayout   *layout;
  GtkWidget     *widget;

  g_return_if_fail (GEDA_IS_ENTRY (entry));

  widget    = g_object_ref(GTK_WIDGET(entry));
  old_attrs = entry->priv->attrs;

  if (attrs)
    pango_attr_list_ref (attrs);

  if (old_attrs)
    pango_attr_list_unref (old_attrs);

  entry->priv->attrs = attrs;

  layout = gtk_entry_get_layout ((GtkEntry*)entry);

  if (layout) {
    pango_layout_set_attributes (layout, entry->priv->attrs);
    GEDA_OBJECT_NOTIFY (entry, "attributes");
  }

  gtk_widget_queue_resize (widget);
  g_object_unref(widget);
}

/*!
 * \brief Get the length of the entry history
 * \par Function Description
 *  Returns the length of the history list.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 */
unsigned int geda_entry_get_length_history(GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), 0);

  if (entry->have_history) {
    return g_list_length(entry->priv->history_list);
  }
  return 0;
}

/*!
 * \brief Set GedaEntry Max History Property
 * \par Function Description
 *  The maximum length of history property controls the number of
 *  entries stored by the GedaEntry. The max-history property is
 *  only relevant to GedaEntries that were created with history.
 *  History is stored in a GList so in effect max-history controls
 *  the maximum number of items in the list with the oldest item
 *  being removed when the a new item is added should the number of
 *  items reaches \a value.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 * \param [in] value Maximum number of characters the entry is to accept.
 */
void geda_entry_set_max_history (GedaEntry *entry, unsigned int value)
{
  unsigned int len;

  g_return_if_fail (GEDA_IS_ENTRY (entry));
  g_return_if_fail (value > 0);

  entry->max_history = value;

  len = geda_entry_get_length_history(entry);

  if (len > value) {

    GedaEntryPriv *priv = entry->priv;

    while (len > value) {

      GList *history_list = priv->history_list;
      priv->history_list = g_list_delete_link(history_list, history_list);

      len = g_list_length(priv->history_list);
    }
  }
}

/*!
 * \brief Get GedaEntry Max History Property
 * \par Function Description
 *  Gets the current max-history setting.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 */
unsigned int geda_entry_get_max_history (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), 0);
  return entry->max_history;
}

/*!
 * \brief Get the GedaEntry Maximun Allow Length property
 * \par Function Description
 *  Retrieves the maximum allowed length of the text in \a entry.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 *
 * \returns the maximum allowed number of characters or 0 if there
 *          is no maximum.
 *
 * \sa geda_entry_set_max_length
 */
unsigned int geda_entry_get_max_length (GedaEntry *entry)
{
  volatile unsigned int max;

  g_object_get (entry, "max-length", &max, NULL);

  return (unsigned int)max;
}

/*!
 * \brief Set the GedaEntry Maximun Allow Length property
 * \par Function Description
 *  Sets the maximum allowed length of the contents of the widget. If
 *  the current contents are longer than the given length, then they
 *  will be truncated to fit. The value passed in will be clamped to
 *  the range 0-65536.
 *
 *  This is equivalent to:
 *
 *  gtk_entry_buffer_set_max_length (gtk_entry_get_buffer (entry), max);
 *
 * \param [in] entry  Pointer to a #GedaEntry object.
 * \param [in] max   the maximum length of the entry, or 0 for no maximum.
 */
void geda_entry_set_max_length (GedaEntry *entry, unsigned int max)
{
   g_object_set (entry, "max-length", max, NULL);
}

/*!
 * \brief Retrieve the Completion Object from a GedaEntry
 * \par Function Description
 *  Returns the commpletion object without check of the
 *  object exist, i.e. a NULL pointer.
 *
 * \note The returned object is a GedaCompletion structure and not
 *       a g_object.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 */
GedaCompletion *geda_entry_get_completion (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), NULL);
  return entry->priv->command_completion;
}

/*!
 * \brief Set the Completion Object from a GedaEntry
 * \par Function Description
 *  Sets the completion object. If the GedaEntry was not previously
 *  enabled, auto_complete is enabled if completion is non-NULL;
 *
 * \param [in] entry      Pointer to a #GedaEntry object.
 * \param [in] completion Pointer to a GedaCompletion structure.
 */
void geda_entry_set_completion (GedaEntry *entry, GedaCompletion *completion)
{
  GedaEntryPriv *priv;

  g_return_if_fail (GEDA_IS_ENTRY (entry));

  priv = entry->priv;

  if (priv->command_completion) {
    geda_completion_free (entry->priv->command_completion);
  }

  priv->command_completion = completion;

  if (completion && !entry->auto_complete) {
    entry->auto_complete = TRUE;
  }
}

/*!
 * \brief Get sensitivity of internal completion algorithms
 * \par Function Description
 *  Gets the case sensitivity used by the GedaEntry object for
 *  completion comparisons.
 *
 * \param [in] entry  Pointer to a #GedaEntry object.
 */
bool geda_entry_completion_get_case (GedaEntry *entry) {

  g_return_val_if_fail (GEDA_IS_ENTRY (entry), FALSE);

  return entry->priv->case_sensitive;
}

/*!
 * \brief Set sensitivity of internal completion algorithms
 * \par Function Description
 *  Sets the case sensitivity for the GedaEntry object of
 *  completion operations. This set which GedaStrCompareNFunc
 *  algorithms are used by the GedaCompletion.
 *
 * \param [in] entry     Pointer to a #GedaEntry object.
 * \param [in] sensitive Desired boolean case sensitivity setting.
 */
/*! \brief Set sensitivity of internal completion algorithms */
void geda_entry_completion_set_case (GedaEntry *entry, bool sensitive)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));

  if (entry->auto_complete) {

    entry->priv->case_sensitive = sensitive != FALSE;

    if (sensitive) {
      geda_completion_set_compare (entry->priv->command_completion,
                                  (GedaStrCompareNFunc) strncmp);
    }
    else {
      geda_completion_set_compare (entry->priv->command_completion,
                                  (GedaStrCompareNFunc) geda_entry_strncmpi);
    }
  }
}

/*!
 * \brief Get GedaEntry Input Case Property
 * \par Function Description
 *  Returns the text_case member of the GedaEntry.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 *
 * \returns the current text_case setting.
 *
 * \sa geda_entry_set_input_case
 */
bool geda_entry_get_input_case (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), FALSE);
  return entry->text_case;
}

/*!
 * \brief Set GedaEntry Input Case Property
 * \par Function Description
 *  Sets if user input of characters is changed to upper or lower case.
 *  The mode can be one of:
 *  <DL>
 *    <DT>LOWER_CASE</DT>
 *    <DT>UPPER_CASE</DT>
 *    <DT>BOTH_CASES</DT>
 *  </DL>
 *  The default is BOTH_CASES, which means the case of text entered by
 *  the used will not be changed.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 * \param [in] mode  as described above.
 */
void geda_entry_set_input_case  (GedaEntry *entry, int mode)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));
  g_return_if_fail (mode > -1 && mode < 3);

  ((GedaEntry*)entry)->text_case = mode;
}

/*!
 * \brief Get the current GedaEntry Input Validation Mode
 * \par Function Description
 *  The default is ACCEPT_ALL_ASCII.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 */
GedaEntryAccept geda_entry_get_valid_input (GedaEntry *entry)
{
  g_return_val_if_fail (GEDA_IS_ENTRY (entry), ACCEPT_ALL_ASCII);
  return entry->validation_mode;
}

/*!
 * \brief Set GedaEntry Input Validation
 * \par Function Description
 *  Restricts user input to characters of a particular type given
 *  by the GedaEntryAccept \a mode argument. The mode can be one
 *  of:
 *  <DL>
 *    <DT>ACCEPT_ALL_ASCII</DT>
 *    <DT>ACCEPT_ALPHANUMERIC</DT>
 *    <DT>ACCEPT_COORDINATE</DT>
 *    <DT>ACCEPT_NUMERIC</DT>
 *    <DT>ACCEPT_NUMBER</DT>
 *    <DT>ACCEPT_INTEGER</DT>
 *    <DT>ACCEPT_REAL</DT>
 *  </DL>
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 * \param [in] mode  as described above.
 */
void geda_entry_set_valid_input (GedaEntry *entry, GedaEntryAccept mode)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));
  g_return_if_fail (mode >= ACCEPT_ALL_ASCII && mode <= ACCEPT_REAL);

  ((GedaEntry*)entry)->validation_mode = mode;
}

/*!
 * \brief Select All Text in a GedaEntry
 * \par Function Description
 *  Selects all characters in \a entry text.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 */
void
geda_entry_select_all (GedaEntry *entry)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));

  gtk_editable_select_region ((GtkEditable*)entry, 0, -1);
}

/*!
 * \brief Select Specifiy Text in a GedaEntry
 * \par Function Description
 *  Selects a region of text. The characters that are selected are
 *  those characters at positions from \a start_pos up to, but not
 *  including \a end_pos. If \a end_pos is negative, all characters
 *  will be selected from \a start_pos to the end of the text.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 * \param [in] start Starting Position
 * \param [in] end   End Position or -1 to select remain text
 */
void geda_entry_select_region (GedaEntry *entry, int start, int end)
{
  g_return_if_fail (GEDA_IS_ENTRY (entry));

  gtk_editable_select_region ((GtkEditable*)entry, start, end);
}

void geda_entry_set_selected_text (GedaEntry *entry, const char *text)
{
  GtkEditable *editable;
  bool         selected;
  int          start, end;

  g_return_if_fail (GEDA_IS_ENTRY (entry));

  editable = GTK_EDITABLE (entry);
  selected = gtk_editable_get_selection_bounds (editable, &start, &end);

  if (text) {
    if (!selected) {
      geda_entry_set_text (entry, text);
    }
    else {

      gtk_editable_delete_text (editable, start, end);

      gtk_editable_insert_text (editable, text, -1, &start);
    }
  }
}

/* --------------------- Widget Style Functions ----------------- */

/** \defgroup GedaEntry-Style GedaEntry Style Functions
 *  @{
 */

/*!
 * \brief Modify GedaEntry Color Attributes
 * \par Function Description
 *  Validates \a entry and \a state and pass the request to
 *  geda_widget_modify_color_component.
 *
 * \param[in,out] entry      Pointer to a #GedaEntry.
 * \param[in]     component  The component that is being modified.
 * \param[in]     state      The state for which the attribute is to be set.
 * \param[in]     color      Pointer to GdkColor RGB color structure.
 *
 * \sa geda_widget_modify_color_component
 */
void geda_entry_modify_color (GedaEntry      *entry,
                              GtkRcFlags      component,
                              GtkStateType    state,
                              const GdkColor *color)
{
  g_return_if_fail (GTK_IS_WIDGET (entry));

  if (state >= GTK_STATE_NORMAL || state <= GTK_STATE_INSENSITIVE) {
     state = GTK_STATE_NORMAL;
  }

  GtkWidget *widget = (GtkWidget*)entry;

  geda_widget_modify_color_component (widget, component, state, color);
}

/*!
 * \brief Modify GedaEntry Background Color
 * \par Function Description
 *  Calls geda_entry_modify_color to modify the
 *  background color attribute of the \a entry.
 *
 * \param[in,out] entry  Pointer to a #GedaEntry.
 * \param[in]     state  The state for which the attribute is to be set.
 * \param[in]     color  Pointer to GdkColor RGB color structure.
 *
 * \sa geda_entry_modify_fg geda_entry_modify_color
 */
void geda_entry_modify_bg (GedaEntry      *entry,
                           GtkStateType    state,
                           const GdkColor *color)
{
  geda_entry_modify_color (entry, GTK_RC_BG, state, color);
}

/*!
 * \brief Modify GedaEntry Foreground Color
 * \par Function Description
 *  Calls geda_entry_modify_color to modify the
 *  foreground attribute color of the \a entry.
 *
 * \param[in,out] entry  Pointer to a #GedaEntry.
 * \param[in]     state  The state for which the attribute is to be set.
 * \param[in]     color  Pointer to GdkColor RGB color structure.
 *
 * \sa geda_entry_modify_bg geda_entry_modify_color
 */
void geda_entry_modify_fg (GedaEntry *entry,
                           GtkStateType state,
                           const GdkColor *color)
{
  geda_entry_modify_color (entry, GTK_RC_FG, state, color);
}

/** @} endgroup GedaEntry-Style */

/** \defgroup GedaEntry-Widget-Methods GedaEntry Widget Methods
 *  @{
 */

/*!
 * \brief Get GedaEntry Widget Activates Default
 * \par Function Description
 *  Convenience version of #geda_entry_get_activates_default that
 *  accepts a pointer to a GtkWidget, entry
 */
bool geda_entry_widget_get_activates_default (GtkWidget *entry)
{
  return geda_entry_get_activates_default ((GedaEntry*)entry);
}

/*!
 * \brief Set GedaEntry Widget Activates Default Property
 * \par Function Description
 *  Convenience version of #geda_entry_set_activates_default that
 *  accepts a pointer to a GtkWidget, \a entry must be a GedaEntry.
 */
void geda_entry_widget_set_activates_default (GtkWidget *entry, bool  setting)
{
  geda_entry_set_activates_default ((GedaEntry*)entry, setting);
}

/*!
 * \brief Get GedaEntry Widget Attribute List
 * \par Function Description
 *  Convenience version of #geda_entry_get_attributes that accepts
 *  a pointer to a GtkWidget, \a entry must be a GedaEntry.
 *
 * \param [in] entry  Pointer to a #GedaEntry object.
 *
 * \returns PangoAttrList attribute list, or %NULL if none was set.
 */
PangoAttrList *geda_entry_widget_get_attributes (GtkWidget *entry)
{
  return geda_entry_get_attributes ((GedaEntry*)entry);
}

/*!
 * \brief Set GedaEntry Widget Attribute List
 * \par Function Description
 *  Convenience version of #geda_entry_set_attributes that accepts
 *  a pointer to a GtkWidget, \a entry must be a GedaEntry.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 * \param [in] attrs Pointer to a PangoAttrList structure.
 */
void geda_entry_widget_set_attributes (GtkWidget *entry, PangoAttrList *attrs)
{
  geda_entry_set_attributes ((GedaEntry*)entry, attrs);
}

/*!
 * \brief Get the Completion Object from a GedaEntry Wdget
 * \par Function Description
 *  Convenience wrapper that accepts a GedaEntry of type GtkWidget
 *  and returns the GedaCompletion object associated with the entry.
 */
GedaCompletion *geda_entry_widget_get_completion (GtkWidget *entry)
{
  return geda_entry_get_completion ((GedaEntry*)entry);
}

/*!
 * \brief Set the GedaEntry Widget Completion Object
 * \par Function Description
 *  Convenience wrapper that accepts a GedaEntry of type GtkWidget,
 *  which may be assigned a GedaCompletion object after construction
 *  using this function.
 */
void geda_entry_widget_set_completion (GtkWidget      *entry,
                                       GedaCompletion *completion)
{
  geda_entry_set_completion ((GedaEntry*)entry, completion);
}

/*!
 * \brief Get sensitivity of widget completion algorithms
 * \par Function Description
 *  Gets the case sensitivity used by the GedaEntry widget for
 *  completion comparisons.
 *
 * \param [in] entry  Pointer to a #GedaEntry widget.
 */
bool geda_entry_widget_completion_get_case (GtkWidget *entry)
{
  return geda_entry_completion_get_case ((GedaEntry*)entry);
}

/*!
 * \brief Set sensitivity of widget completion algorithms
 * \par Function Description
 *  Sets the case sensitivity for the GedaEntry widget completion
 *  operations. This set which GedaStrCompareNFunc algorithms are
 *  used by the GedaCompletion.
 *
 * \param [in] entry     Pointer to a #GedaEntry widget.
 * \param [in] sensitive Desired boolean case sensitivity setting.
 */
void geda_entry_widget_completion_set_case (GtkWidget *entry, bool sensitive)
{
  geda_entry_completion_set_case ((GedaEntry*)entry, sensitive);
}

/*!
 * \brief Get GedaEntry Widget Input Case Property
 * \par Function Description
 * \returns the current text_case setting.
 * \sa geda_entry_get_input_case
 */
bool geda_entry_widget_get_input_case (GtkWidget *entry)
{
  return geda_entry_get_input_case ((GedaEntry*)entry);
}

/*!
 * \brief Set GedaEntry Widget Input Case Property
 * \par Function Description
 *  Sets if user input of characters is changed to upper or lower case.
 *  The mode can be one of:
 *  <DL>
 *    <DT>LOWER_CASE</DT>
 *    <DT>UPPER_CASE</DT>
 *    <DT>BOTH_CASES</DT>
 *  </DL>
 *  The default is BOTH_CASES, which means the case of text entered by
 *  the used will not be changed.
 */
void geda_entry_widget_set_input_case (GtkWidget *entry, int mode)
{
  return geda_entry_set_input_case ((GedaEntry*)entry, mode);
}

/*!
 * \brief Get Maximum History propertyr from GedaEntry Widget
 * \par Function Description
 *  Retrieve the current max-history setting.
 *
 * \sa geda_entry_get_max_history geda_entry_set_max_history
 */
unsigned int geda_entry_widget_get_max_history (GtkWidget *entry)
{
  return geda_entry_get_max_history ((GedaEntry*)entry);
}

/*!
 * \brief
 * \par Function Description
 *  Set the max-history property.
 *
 * \sa geda_entry_get_max_history geda_entry_set_max_history
 */
void geda_entry_widget_set_max_history (GtkWidget *entry, unsigned int value)
{
  geda_entry_set_max_history ((GedaEntry*)entry, value);
}

/*!
 * \brief Get the GedaEntry Maximun Allow Length property
 * \par Function Description
 *  Retrieves the maximum allowed length of the text in \a entry.
 *
 * \param [in] entry Pointer to a #GedaEntry object.
 *
 * \returns the maximum allowed number of characters or 0 if there
 *          is no maximum.
 *
 * \sa geda_entry_get_max_length
 */
unsigned int geda_entry_widget_get_max_length (GtkWidget *entry)
{
  return geda_entry_get_max_length ((GedaEntry*)entry);
}

/*!
 * \brief  Set the GedaEntry Maximun Allow Length property
 * \par Function Description
 *  Sets the maximum allowed length of the contents of the widget.
 *
 * \param [in] entry Pointer to a GedaEntry widget object.
 * \param [in] max   the maximum length of the entry, or 0 for no maximum.
 *
 * \sa geda_entry_set_max_length
 */
void geda_entry_widget_set_max_length (GtkWidget *entry, unsigned int max)
{
  geda_entry_set_max_length ((GedaEntry*)entry, max);
}

/*!
 * \brief Retrieve the Text from a GedaEntry Widget
 * \par Function Description
 *  This function gets the text characters in a GedaEntry.
 *
 * \param[in] entry Pointer a #GedaEntry expressed as a widget
 *
 * \returns the contents of the text entry buffer
 */
const char *geda_entry_widget_get_text (GtkWidget *entry)
{
  return geda_entry_get_text ((GedaEntry*)entry);
}

/*!
 * \brief Set the Text in a GedaEntry Widget
 * \par Function Description
 *  This function sets the text in a GedaEntry if \a new_text
 *  is not equivalent to the string already in the buffer. No
 *  signal is emitted.
 *
 * \param[in] entry    Pointer #GedaEntry expressed as a widget
 * \param[in] new_text Text to be loaded into the GedaEntry.
 */
void geda_entry_widget_set_text (GtkWidget *entry, const char *new_text)
{
  geda_entry_set_text ((GedaEntry*)entry, new_text);
}

/*!
 * \brief Get the GedaEntry Widget Input Validation Mode
 * \par Function Description
 *  The default is ACCEPT_ALL_ASCII.
 *
 * \sa geda_entry_set_valid_input
 */
GedaEntryAccept geda_entry_widget_get_valid_input (GtkWidget *entry)
{
  return geda_entry_get_valid_input (GEDA_ENTRY (entry));
}

/*!
 * \brief Set GedaEntry Widget Input Validation
 * \par Function Description
 *  Set which type of characters will be accepted as input.
 *
 * \sa geda_entry_set_valid_input
 */
void geda_entry_widget_set_valid_input (GtkWidget *entry, GedaEntryAccept mode)
{
  geda_entry_set_valid_input ((GedaEntry*)entry, mode);
}

/*!
 * \brief Modify GedaEntry Widget Background Color
 * \par Function Description
 *  Calls geda_entry_modify_color to modify the
 *  background color attribute of the \a entry.
 *
 * \param[in,out] entry  Pointer to a #GedaEntry expressed as a widget.
 * \param[in]     state  The state for which the attribute is to be set.
 * \param[in]     color  Pointer to GdkColor RGB color structure.
 *
 * \sa geda_entry_modify_fg geda_entry_modify_color
 */
void geda_entry_widget_modify_bg (GtkWidget      *entry,
                                  GtkStateType    state,
                                  const GdkColor *color)
{
  geda_widget_modify_color (entry, GTK_RC_BG, state, color);
}

/*!
 * \brief Modify GedaEntry Widget Foreground Color
 * \par Function Description
 *  Calls geda_entry_modify_color to modify the
 *  foreground attribute color of the \a entry.
 *
 * \param[in,out] entry  Pointer to a #GedaEntry expressed as a widget.
 * \param[in]     state  The state for which the attribute is to be set.
 * \param[in]     color  Pointer to GdkColor RGB color structure.
 *
 * \sa geda_entry_modify_bg geda_entry_modify_color
 */
void geda_entry_widget_modify_fg (GtkWidget *entry,
                                  GtkStateType state,
                                  const GdkColor *color)
{
  geda_widget_modify_color (entry, GTK_RC_FG, state, color);
}

/** @} endgroup GedaEntry-Widget-Methods */

/* -------------------------------------------------------------- */

/** \defgroup GedaEntry-Creators GedaEntry Creator Functions
 *  @{
 */

/*!
 * \brief Create a New GedaEntry
 * \par Function Description
 * Creates a new entry.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new (void)
{
  have_auto_complete = FALSE;
  have_history       = FALSE;

  return GTK_WIDGET (g_object_new (geda_entry_get_type (), NULL));
}

/*!
 * \brief Create a New GedaEntry with History and Completion
 * \par Function Description
 *  Creates a new entry with history and completion list.
 *
 * \param [in] history  Point to location of GList pointer to store entry strings.
 * \param [in] complete Point to location of GList pointer containing key words.
 *
 * \return a new #GedaEntry
 *
 * \sa geda_entry_new_with_completion geda_entry_new_with_history
 */
GtkWidget *geda_entry_new_history_complete (GList **history, GList **complete)
{
  if ((int)(long)history == -1) {
    have_history = FALSE;
  }
  else {
    history_list_arg = history;
    have_history = TRUE;
  }

  if ((int)(long)complete == -1) {
    have_auto_complete = FALSE;
  }
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
 * \brief Create a New Visible GedaEntry
 * \par Function Description
 *  Creates a new entry, the new widget is set visible.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_visible (void)
{
  GtkWidget *entry;

  entry = geda_entry_new ();

  gtk_widget_show (entry);

  return entry;
}

/*!
 * \brief Create a New Visible GedaEntry with Auxiliary Text Buffer
 * \par Function Description
 * Creates a new entry with the provided \a buffer. If \a buffer
 * is not a GtkEntryBuffer a new empty text buffer is created.
 * The new widget is set visible.
 *
 * \param [in] buffer The buffer to use for the new #GedaEntry
 *                    or NULL to create a new empty buffer;
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_visible_buffer (GtkEntryBuffer *buffer)
{
  GtkWidget *entry;

  entry = geda_entry_new_with_buffer (buffer);

  gtk_widget_show (entry);

  return entry;
}

/*!
 * \brief Create a New Visible GedaEntry with Completion
 * \par Function Description
 *  Creates a new entry with history and completion list. The completion
 *  list should contain complete keyword strings. User can complete an
 *  entry using the TAB key if a suitable match is found in the list.
 *  The new widget is set visible.
 *
 * \param [in] complete Pointer to location of GList pointer containing key words.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_visible_completion (GList **complete)
{
  GtkWidget *entry;

  entry = geda_entry_new_with_completion (complete);

  gtk_widget_show (entry);

  return entry;
}

/*!
 * \brief Create a New Visible GedaEntry with History
 * \par Function Description
 *  Creates a new entry with history list. Strings currently in the
 *  list will be available to users by using the up and down arrow
 *  keys. User input that is not in the list will be appended to the
 *  \a history list. The new widget is set visible.
 *
 * \param [in] history Pointer to location of GList pointer to store entry strings.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_visible_history (GList **history)
{
  GtkWidget *entry;

  entry = geda_entry_new_with_history (history);

  gtk_widget_show (entry);

  return entry;
}

/*!
 * \brief Create a New GedaEntry with Auxiliary Text Buffer
 * \par Function Description
 * Creates a new entry with the provided \a buffer. If \a buffer
 * is not a GtkEntryBuffer a new empty text buffer is created.
 *
 * \param [in] buffer The buffer to use for the new #GedaEntry
 *                    or NULL to create a new empty buffer;
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_with_buffer (GtkEntryBuffer *buffer)
{
  GtkEntryBuffer *real_buffer;

  have_history = FALSE;
  have_auto_complete = FALSE;

  if (GTK_IS_ENTRY_BUFFER (buffer)) {
    real_buffer = buffer;
  }
  else {
    real_buffer = gtk_entry_buffer_new (NULL, -1);
  }

  return g_object_new (GEDA_TYPE_ENTRY, "buffer", real_buffer, NULL);
}

/*!
 * \brief Create a New GedaEntry with Completion
 * \par Function Description
 *  Creates a new entry with history and completion list. The completion
 *  list should contain complete keyword string. User can complete an entry
 *  using the TAB key if a suitable match is found in the list.
 *
 * \param [in] complete Point to location of GList pointer containing key words.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_with_completion (GList **complete)
{
  have_history = FALSE;

  if (complete) {
    old_complete_list  = complete;
    have_auto_complete = TRUE;
  }
  else {
    have_auto_complete = FALSE;
  }

  return g_object_new (geda_entry_get_type (), NULL);
}

/*!
 * \brief Create a New GedaEntry with History
 * \par Function Description
 *  Creates a new entry with history list. Strings currently in the
 *  list will be available to the use using the up and down arrow
 *  key. User input that is not in the list will be appended to the
 *  \a history list.
 *
 * \param [in] history  Point to location of GList pointer to store entry strings.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_with_history (GList **history)
{
  have_auto_complete = FALSE;
  have_history       = TRUE;
  history_list_arg   = history;

  return GTK_WIDGET (g_object_new (geda_entry_get_type (), NULL));
}

/*!
 * \brief Create a New GedaEntry specified max length property
 * \par Function Description
 * Creates a new entry and sets the max-length property to \a max_length,
 * which does not really do much.
 *
 * \param [in] max_length Value to set the Max length property.
 *
 * \return a new #GedaEntry
 */
GtkWidget *geda_entry_new_with_max_length (unsigned int max_length)
{
  GtkWidget *entry;
  entry = geda_entry_new ();
  g_object_set (entry, "max-length", max_length, NULL);
  return entry;
}

/** @} endgroup Entry-Creators */
/** @} endgroup GedaEntry-Puplic */
/** @} end group GedaEntry */
