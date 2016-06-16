/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*! \file geda_menu_shell.c
 *  \brief GedaMenuShell Class Module
 */

/** \defgroup geda-menu-shell GedaMenuShell Object
 * @{
 * \brief Implmentation of GedaMenuShell Class
 *
 * \class GedaMenuShell geda_menu_shell.h "include/geda_menu_shell.h"
 * \implements GtkContainer
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_accel_label.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_menu_enum.h"
#include "../../include/geda_tearoff_menu_item.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_label.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_menu_bar.h"

#include "../../include/gettext.h"

#include <geda_debug.h>

#define MENU_POPUP_DELAY     225
#define MENU_SHELL_TIMEOUT   500

#define PACK_DIRECTION(m)                                 \
   (GEDA_IS_MENU_BAR (m)                                   \
     ? geda_menu_bar_get_pack_direction (GEDA_MENU_BAR (m)) \
     : PACK_DIRECTION_LTR)

/* Terminology:
 *
 * A menu item can be "selected", this means that it is displayed
 * in the prelight state, and if it has a submenu, that submenu
 * will be popped up.
 *
 * A menu is "active" when it is visible onscreen and the user
 * is selecting from it. A menubar is not active until the user
 * clicks on one of its menuitems. When a menu is active,
 * passing the mouse over a submenu will pop it up.
 *
 * menu_shell->active_menu_item, is however, not an "active"
 * menu item (there is no such thing) but rather, the selected
 * menu item in that MenuShell, if there is one.
 *
 * There is also is a concept of the current menu and a current
 * menu item. The current menu item is the selected menu item
 * that is furthest down in the hierarchy. (Every active menu_shell
 * does not necessarily contain a selected menu item, but if
 * it does, then menu_shell->parent_menu_shell must also contain
 * a selected menu item. The current menu is the menu that
 * contains the current menu_item. It will always have a GTK
 * grab and receive all key presses.
 *
 * Action signals:
 *
 *  ::move_current (MenuDirection *dir)
 *     Moves the current menu item in direction 'dir':
 *
 *       MENU_DIR_PARENT: To the parent menu shell
 *       MENU_DIR_CHILD: To the child menu shell (if this item has
 *          a submenu.
 *       MENU_DIR_NEXT/PREV: To the next or previous item
 *          in this menu.
 *
 *     As a a bit of a hack to get movement between menus and
 *     menubars working, if submenu_placement is different for
 *     the menu and its MenuShell then the following apply:
 *
 *       - For 'parent' the current menu is not just moved to
 *         the parent, but moved to the previous entry in the parent
 *       - For 'child', if there is no child, then current is
 *         moved to the next item in the parent.
 *
 *    Note that the above explanation of ::move_current was written
 *    before menus and menubars had support for RTL flipping and
 *    different packing directions, and therefore only applies for
 *    when text direction and packing direction are both left-to-right.
 *
 *  ::activate_current (GBoolean *force_hide)
 *     Activate the current item. If 'force_hide' is true, hide
 *     the current menu item always. Otherwise, only hide
 *     it if menu_item->class->hide_on_activate is true.
 *
 *  ::cancel ()
 *     Cancels the current selection
 */

enum {
  DEACTIVATE,
  SELECTION_DONE,
  MOVE_CURRENT,
  ACTIVATE_CURRENT,
  CANCEL,
  CYCLE_FOCUS,
  MOVE_SELECTED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_TAKE_FOCUS
};

struct _GedaMenuShellPriv
{
  /*GtkWidget *parent_menu_shell;*/

  unsigned int take_focus           : 1;
  unsigned int activated_submenu    : 1;
  unsigned int in_unselectable_item : 1; /* Crutch to keep mnemonics in the
                                          * same menu if the user moves the mouse
                                          * over an unselectable menuitem.*/
  GedaMnemonicHash *mnemonic_hash;
  GedaKeyHash      *key_hash;

  /*GdkDevice *grab_pointer;*/
};

static void geda_menu_shell_set_property      (GObject           *object,
                                               unsigned int        prop_id,
                                               const GValue      *value,
                                               GParamSpec        *pspec);
static void geda_menu_shell_get_property      (GObject           *object,
                                               unsigned int       prop_id,
                                               GValue            *value,
                                               GParamSpec        *pspec);
static void geda_menu_shell_realize           (GtkWidget         *widget);
static void geda_menu_shell_finalize          (GObject           *object);
static int geda_menu_shell_button_press       (GtkWidget         *widget,
                                               GdkEventButton    *event);
static int geda_menu_shell_button_release     (GtkWidget         *widget,
                                               GdkEventButton    *event);
static int geda_menu_shell_key_press          (GtkWidget         *widget,
                                               GdkEventKey       *event);
static int geda_menu_shell_enter_notify       (GtkWidget         *widget,
                                               GdkEventCrossing  *event);
static int geda_menu_shell_leave_notify       (GtkWidget         *widget,
                                               GdkEventCrossing  *event);
static void geda_menu_shell_screen_changed    (GtkWidget         *widget,
                                               GdkScreen         *previous_screen);
static bool geda_menu_shell_grab_broken       (GtkWidget         *widget,
                                               GdkEventGrabBroken *event);
static void geda_menu_shell_add               (GtkContainer      *container,
                                               GtkWidget         *widget);
static void geda_menu_shell_remove            (GtkContainer      *container,
                                               GtkWidget         *widget);
static void geda_menu_shell_forall            (GtkContainer      *container,
                                               bool               include_internals,
                                               GtkCallback        callback,
                                               void              *callback_data);
static void geda_menu_shell_real_insert       (GedaMenuShell     *menu_shell,
                                               GtkWidget         *child,
                                               int                position);
static void geda_real_menu_shell_deactivate   (GedaMenuShell     *menu_shell);
static int  geda_menu_shell_is_item           (GedaMenuShell     *menu_shell,
                                               GtkWidget         *child);
static GtkWidget *geda_menu_shell_get_item    (GedaMenuShell     *menu_shell,
                                               GdkEvent          *event);
static GType geda_menu_shell_child_type       (GtkContainer      *container);
static void geda_menu_shell_real_select_item     (GedaMenuShell     *menu_shell,
                                                  GtkWidget         *menu_item);
static bool geda_menu_shell_select_submenu_first (GedaMenuShell     *menu_shell);

static void geda_real_menu_shell_move_current      (GedaMenuShell      *menu_shell,
                                                    MenuDirection       direction);
static void geda_real_menu_shell_activate_current  (GedaMenuShell      *menu_shell,
                                                    bool               force_hide);
static void geda_real_menu_shell_cancel            (GedaMenuShell      *menu_shell);
static void geda_real_menu_shell_cycle_focus       (GedaMenuShell      *menu_shell,
                                                    GtkDirectionType    dir);

static void geda_menu_shell_reset_key_hash         (GedaMenuShell  *menu_shell);
static bool geda_menu_shell_activate_mnemonic      (GedaMenuShell  *menu_shell,
                                                    GdkEventKey    *event);
static bool geda_menu_shell_real_move_selected     (GedaMenuShell  *menu_shell,
                                                    int             distance);

static unsigned int menu_shell_signals[LAST_SIGNAL] = { 0 };

static void *geda_menu_shell_parent_class = NULL;

static GType
geda_menu_shell_child_type (GtkContainer     *container)
{
  return GEDA_TYPE_MENU_ITEM;
}

static void
geda_menu_shell_set_property (GObject      *object,
                              unsigned int  prop_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (object);

  switch (prop_id) {

    case PROP_TAKE_FOCUS:
      geda_menu_shell_set_take_focus (menu_shell, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_menu_shell_get_property (GObject      *object,
                              unsigned int  prop_id,
                              GValue       *value,
                              GParamSpec   *pspec)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (object);

  switch (prop_id) {

    case PROP_TAKE_FOCUS:
      g_value_set_boolean (value, geda_menu_shell_get_take_focus (menu_shell));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_menu_shell_finalize (GObject *object)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (object);
  GedaMenuShellPriv *priv = menu_shell->priv;

  if (priv->mnemonic_hash) {
    geda_mnemonic_hash_free (priv->mnemonic_hash);
  }

  if (priv->key_hash) {
    geda_key_hash_free (priv->key_hash);
  }

  G_OBJECT_CLASS (geda_menu_shell_parent_class)->finalize (object);
}

/*!
 * \brief GedaMenuShell Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class      GedaMenuShell class we are initializing
 * \param [in]  class_data GedaMenuShell structure associated with the class
 */
static void
geda_menu_shell_class_init(void *class, void *class_data)
{
  GedaMenuShellClass *menu_shell_class;
  GObjectClass       *object_class;
  GtkWidgetClass     *widget_class;
  GtkContainerClass  *container_class;
  GtkBindingSet      *binding_set;

  object_class     = (GObjectClass*) class;
  widget_class     = (GtkWidgetClass*) class;
  container_class  = (GtkContainerClass*) class;
  menu_shell_class = (GedaMenuShellClass*) class;

  object_class->set_property = geda_menu_shell_set_property;
  object_class->get_property = geda_menu_shell_get_property;
  object_class->finalize     = geda_menu_shell_finalize;

  widget_class->realize              = geda_menu_shell_realize;
  widget_class->button_press_event   = geda_menu_shell_button_press;
  widget_class->button_release_event = geda_menu_shell_button_release;
  widget_class->grab_broken_event    = geda_menu_shell_grab_broken;
  widget_class->key_press_event      = geda_menu_shell_key_press;
  widget_class->enter_notify_event   = geda_menu_shell_enter_notify;
  widget_class->leave_notify_event   = geda_menu_shell_leave_notify;
  widget_class->screen_changed       = geda_menu_shell_screen_changed;

  container_class->add        = geda_menu_shell_add;
  container_class->remove     = geda_menu_shell_remove;
  container_class->forall     = geda_menu_shell_forall;
  container_class->child_type = geda_menu_shell_child_type;

  menu_shell_class->submenu_placement    = GTK_TOP_BOTTOM;
  menu_shell_class->deactivate           = geda_real_menu_shell_deactivate;
  menu_shell_class->selection_done       = NULL;
  menu_shell_class->move_current         = geda_real_menu_shell_move_current;
  menu_shell_class->activate_current     = geda_real_menu_shell_activate_current;
  menu_shell_class->cancel               = geda_real_menu_shell_cancel;
  menu_shell_class->select_item          = geda_menu_shell_real_select_item;
  menu_shell_class->insert               = geda_menu_shell_real_insert;
  menu_shell_class->move_selected        = geda_menu_shell_real_move_selected;

  geda_menu_shell_parent_class = g_type_class_peek_parent (class);

  menu_shell_signals[DEACTIVATE] =
    g_signal_new ("deactivate",
                  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuShellClass, deactivate),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  menu_shell_signals[SELECTION_DONE] =
    g_signal_new ("selection-done",
                  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuShellClass, selection_done),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  menu_shell_signals[MOVE_CURRENT] =
    g_signal_new ("move-current",
                  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (GedaMenuShellClass, move_current),
                  NULL, NULL,
                  NULL,
                  G_TYPE_NONE, 0);

  menu_shell_signals[ACTIVATE_CURRENT] =
    g_signal_new ("activate-current",
                  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (GedaMenuShellClass, activate_current),
                  NULL, NULL,
                  geda_marshal_VOID__BOOLEAN,
                  G_TYPE_NONE, 1,
                  G_TYPE_BOOLEAN);

  menu_shell_signals[CANCEL] =
    g_signal_new ("cancel",
                  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (GedaMenuShellClass, cancel),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  menu_shell_signals[CYCLE_FOCUS] =
    g_signal_new_class_handler ("cycle-focus",
                                G_OBJECT_CLASS_TYPE (object_class),
                                G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                                G_CALLBACK (geda_real_menu_shell_cycle_focus),
                                NULL, NULL,
                                NULL,
                                G_TYPE_NONE, 0);

  /*!
   * GedaMenuShell::move-selected:
   * \param [in] menu_shell the object on which the signal is emitted
   * \param [in] distance   +1 to move to the next item, -1 to move to the previous
   *
   * The ::move-selected signal is emitted to move the selection to
   * another item.
   *
   * Returns: %TRUE to stop the signal emission, %FALSE to continue
   */
  menu_shell_signals[MOVE_SELECTED] =
    g_signal_new ("move-selected",
                  G_OBJECT_CLASS_TYPE (object_class),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (GedaMenuShellClass, move_selected),
                  geda_boolean_handled_accumulator, NULL,
                  geda_marshal_BOOL__INT,
                  G_TYPE_BOOLEAN, 1,
                  G_TYPE_INT);

  binding_set = gtk_binding_set_by_class (class);
  gtk_binding_entry_add_signal (binding_set,
                GDK_Escape, 0,
                "cancel", 0);
  gtk_binding_entry_add_signal (binding_set,
                GDK_Return, 0,
                "activate-current", 1,
                G_TYPE_BOOLEAN,
                TRUE);
  gtk_binding_entry_add_signal (binding_set,
                GDK_ISO_Enter, 0,
                "activate-current", 1,
                G_TYPE_BOOLEAN,
                TRUE);
  gtk_binding_entry_add_signal (binding_set,
                GDK_KP_Enter, 0,
                "activate-current", 1,
                G_TYPE_BOOLEAN,
                TRUE);
  gtk_binding_entry_add_signal (binding_set,
                GDK_space, 0,
                "activate-current", 1,
                G_TYPE_BOOLEAN,
                FALSE);
  gtk_binding_entry_add_signal (binding_set,
                GDK_KP_Space, 0,
                "activate-current", 1,
                G_TYPE_BOOLEAN,
                FALSE);
  gtk_binding_entry_add_signal (binding_set,
                GDK_F10, 0,
                "cycle-focus", 1,
                                GTK_TYPE_DIRECTION_TYPE, GTK_DIR_TAB_FORWARD);
  gtk_binding_entry_add_signal (binding_set,
                GDK_F10, GDK_SHIFT_MASK,
                "cycle-focus", 1,
                                GTK_TYPE_DIRECTION_TYPE, GTK_DIR_TAB_BACKWARD);

  /*!
   * GedaMenuShell:take-focus:
   *
   * A boolean that determines whether the menu and its submenus grab the
   * keyboard focus. See geda_menu_shell_set_take_focus() and
   * geda_menu_shell_get_take_focus().
   */
  g_object_class_install_property (object_class,
                                   PROP_TAKE_FOCUS,
                                   g_param_spec_boolean ("take-focus",
                                                         _("Take Focus"),
                                                         _("A boolean that determines whether the menu grabs the keyboard focus"),
                                   TRUE,
                                   G_PARAM_READWRITE));
}

/*!
 * \brief Type instance initializer for GedaMenuShell
 * \par Function Description
 *  Type instance initializer for GedaMenuShell, initializes a new empty
 *  GedaMenuShell object.
 *
 * \param [in] instance The GedaMenuShell structure being initialized,
 * \param [in] class    The GedaMenuShell class being initializing.
 */
static void
geda_menu_shell_instance_init(GTypeInstance *instance, void *class)
{
  GedaMenuShell *menu_shell;

  menu_shell                = (GedaMenuShell*)instance;
  menu_shell->children      = NULL;
  menu_shell->instance_type = geda_menu_shell_get_type();

  menu_shell->priv = g_malloc0 (sizeof(GedaMenuShellPriv));

  menu_shell->priv->take_focus = TRUE;
}

/*!
 * \brief Retrieve GedaMenuShell's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaMenuShell Type identifier. When
 *  first called, the function registers a #GedaMenuShell in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuShell and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaMenuShell.
 */
GedaType
geda_menu_shell_get_type (void)
{
  static GedaType geda_menu_shell_type = 0;

  if (g_once_init_enter (&geda_menu_shell_type)) {

    static const GTypeInfo info = {
      sizeof(GedaMenuShellClass),
      NULL,                           /* base_init           */
      NULL,                           /* base_finalize       */
      geda_menu_shell_class_init,     /* (GClassInitFunc)    */
      NULL,                           /* class_finalize      */
      NULL,                           /* class_data          */
      sizeof(GedaMenuShell),
      0,                              /* n_preallocs         */
      geda_menu_shell_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;
    int         flag = G_TYPE_FLAG_ABSTRACT;

    string = g_intern_static_string ("GedaMenuShell");
    type   = g_type_register_static (GTK_TYPE_CONTAINER, string, &info, flag);

    g_once_init_leave (&geda_menu_shell_type, type);
  }

  return geda_menu_shell_type;
}

bool is_a_geda_menu_shell (GedaMenuShell *menu_shell)
{
  if (G_IS_OBJECT(menu_shell)) {
    return (geda_menu_shell_get_type() == menu_shell->instance_type);
  }
  return FALSE;
}

static void
geda_menu_shell_real_insert (GedaMenuShell *menu_shell,
                             GtkWidget     *child,
                             int            position)
{
  menu_shell->children = g_list_insert (menu_shell->children, child, position);

  gtk_widget_set_parent (child, GTK_WIDGET (menu_shell));
}

void
geda_menu_shell_insert (GedaMenuShell *menu_shell,
                        GtkWidget     *child,
                        int            position)
{
  GedaMenuShellClass *class;

  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));
  g_return_if_fail (GEDA_IS_MENU_ITEM (child));

  class = GEDA_MENU_SHELL_GET_CLASS (menu_shell);

  if (class->insert) {
    class->insert (menu_shell, child, position);
  }
}

void
geda_menu_shell_append (GedaMenuShell *menu_shell, GtkWidget *child)
{
  geda_menu_shell_insert (menu_shell, child, -1);
}

void
geda_menu_shell_prepend (GedaMenuShell *menu_shell, GtkWidget *child)
{
  geda_menu_shell_insert (menu_shell, child, 0);
}

static void
geda_menu_shell_realize (GtkWidget *widget)
{
  GdkWindowAttr attributes;
  int attributes_mask;

  gtk_widget_set_realized (widget, TRUE);

  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= (GDK_EXPOSURE_MASK |
                GDK_BUTTON_PRESS_MASK |
                GDK_BUTTON_RELEASE_MASK |
                GDK_KEY_PRESS_MASK |
                GDK_ENTER_NOTIFY_MASK |
                GDK_LEAVE_NOTIFY_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget), &attributes, attributes_mask);
  gdk_window_set_user_data (widget->window, widget);

  widget->style = gtk_style_attach (widget->style, widget->window);
  gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);
}

void
geda_menu_shell_activate (GedaMenuShell *menu_shell)
{
  if (!menu_shell->active) {

      gtk_grab_add (GTK_WIDGET (menu_shell));
      menu_shell->have_grab = TRUE;
      menu_shell->active = TRUE;
  }
}

static int
geda_menu_shell_button_press (GtkWidget *widget, GdkEventButton *event)
{
  GedaMenuShell *menu_shell;
  GedaMenuShell *parent;
  GtkWidget     *menu_item;


  if (event->type != GDK_BUTTON_PRESS)
    return FALSE;

  menu_shell = GEDA_MENU_SHELL (widget);

  if (menu_shell->parent_menu_shell)
    return gtk_widget_event (menu_shell->parent_menu_shell, (GdkEvent*)event);

  menu_item = geda_menu_shell_get_item (menu_shell, (GdkEvent*)event);

  if (menu_item) {

    parent = GEDA_MENU_SHELL (menu_item->parent);

    if (geda_menu_item_is_selectable (menu_item) &&
        menu_item != parent->active_menu_item)
    {
      /*  select the menu item *before* activating the shell, so submenus
       *  which might be open are closed the friendly way. If we activate
       *  (and thus grab) this menu shell first, we might get grab_broken
       *  events which will close the entire menu hierarchy. Selecting the
       *  menu item also fixes up the state as if enter_notify() would
       *  have run before (which normally selects the item).
       */
      if (GEDA_MENU_SHELL_GET_CLASS (parent)->submenu_placement != GTK_TOP_BOTTOM)
      {
        geda_menu_shell_select_item (parent, menu_item);
      }
    }
  }

  if (!menu_shell->active || !menu_shell->button) {

    geda_menu_shell_activate (menu_shell);

    menu_shell->button = event->button;

    if (menu_item && geda_menu_item_is_selectable (menu_item) &&
        menu_item->parent == widget &&
        menu_item != menu_shell->active_menu_item)
    {
      if (GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement == GTK_TOP_BOTTOM)
      {
        menu_shell->activate_time = event->time;
        geda_menu_shell_select_item (menu_shell, menu_item);
      }
    }
  }
  else {

    widget = gtk_get_event_widget ((GdkEvent*)event);

    if (widget == GTK_WIDGET (menu_shell)) {
      geda_menu_shell_deactivate (menu_shell);
      g_signal_emit (menu_shell, menu_shell_signals[SELECTION_DONE], 0);
    }
  }

  if (menu_item && geda_menu_item_is_selectable (menu_item))  {

    GtkWidget *submenu;

    submenu = geda_menu_item_get_submenu(GEDA_MENU_ITEM (menu_item));

    if (submenu && !gtk_widget_get_visible (submenu)) {

      GedaMenuShellPriv *priv;

      geda_menu_item_popup_submenu (GEDA_MENU_ITEM (menu_item), FALSE);

      parent = GEDA_MENU_SHELL (menu_item->parent);

      priv = parent->priv;
      priv->activated_submenu = TRUE;
    }
  }

  return TRUE;
}

static bool
geda_menu_shell_grab_broken (GtkWidget *widget, GdkEventGrabBroken *event)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (widget);

  if (menu_shell->have_xgrab && event->grab_window == NULL)
    {
      /* Unset the active menu item so gtk_menu_popdown() doesn't see it.
       */

      geda_menu_shell_deselect (menu_shell);

      geda_menu_shell_deactivate (menu_shell);
      g_signal_emit (menu_shell, menu_shell_signals[SELECTION_DONE], 0);
    }

  return TRUE;
}

static int
geda_menu_shell_button_release (GtkWidget      *widget,
                                GdkEventButton *event)
{
  GedaMenuShell     *menu_shell = GEDA_MENU_SHELL (widget);
  GedaMenuShellPriv *priv       = menu_shell->priv;

  if (menu_shell->active) {

    GtkWidget *menu_item;
    bool       deactivate = TRUE;

    if (menu_shell->button && (event->button != menu_shell->button))
    {
      menu_shell->button = 0;
      if (menu_shell->parent_menu_shell)
        return gtk_widget_event (menu_shell->parent_menu_shell, (GdkEvent*) event);
    }

    menu_shell->button = 0;
    menu_item = geda_menu_shell_get_item (menu_shell, (GdkEvent*) event);

    if ((event->time - menu_shell->activate_time) > MENU_SHELL_TIMEOUT)
    {
      if (menu_item && (menu_shell->active_menu_item == menu_item) &&
        geda_menu_item_is_selectable (menu_item))
      {

        GtkWidget *submenu;

        submenu = geda_menu_item_get_submenu(GEDA_MENU_ITEM (menu_item));

        if (submenu == NULL) {

          geda_menu_shell_activate_item (menu_shell, menu_item, TRUE);

          deactivate = FALSE;
        }
        else if (GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement != GTK_TOP_BOTTOM ||
          priv->activated_submenu)
        {
          int popdown_delay;
          GTimeVal *popup_time;
          int64_t usec_since_popup = 0;

          g_object_get (gtk_widget_get_settings (widget),
                        "menu-popdown-delay", &popdown_delay,
                        NULL);

          popup_time = g_object_get_data (G_OBJECT (submenu),
                                          "menu-exact-popup-time");

          if (popup_time) {

            GTimeVal current_time;

            g_get_current_time (&current_time);

            usec_since_popup = ((int64_t) current_time.tv_sec * 1000 * 1000 +
            (int64_t) current_time.tv_usec -
            (int64_t) popup_time->tv_sec * 1000 * 1000 -
            (int64_t) popup_time->tv_usec);

            g_object_set_data (G_OBJECT (submenu),
                               "menu-exact-popup-time", NULL);
          }

          /*  only close the submenu on click if we opened the
           *  menu explicitely (usec_since_popup == 0) or
           *  enough time has passed since it was opened by
           *  GedaMenuItem's timeout (usec_since_popup > delay).
           */
          if (!priv->activated_submenu &&
            (usec_since_popup == 0 ||
            usec_since_popup > popdown_delay * 1000))
          {
            geda_menu_item_popdown_submenu (GEDA_MENU_ITEM (menu_item));
          }
          else
          {
            geda_menu_item_select (GEDA_MENU_ITEM (menu_item));
          }

          deactivate = FALSE;
        }
      }
      else if (menu_item &&
        !geda_menu_item_is_selectable (menu_item) &&
        GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement != GTK_TOP_BOTTOM)
      {
        deactivate = FALSE;
      }
      else if (menu_shell->parent_menu_shell)
      {
        menu_shell->active = TRUE;
        gtk_widget_event (menu_shell->parent_menu_shell, (GdkEvent*)event);
        deactivate = FALSE;
      }

      /* If we ended up on an item with a submenu, leave the menu up.
       */
      if (menu_item && (menu_shell->active_menu_item == menu_item) &&
        GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement != GTK_TOP_BOTTOM)
      {
        deactivate = FALSE;
      }
    }
    else {/* a very fast press-release */

      /* We only ever want to prevent deactivation on the first
       * press/release. Setting the time to zero is a bit of a
       * hack, since we could be being triggered in the first
       * few fractions of a second after a server time wraparound.
       * the chances of that happening are ~1/10^6, without
       * serious harm if we lose.
       */
      menu_shell->activate_time = 0;
      deactivate = FALSE;
    }

    if (deactivate) {

      geda_menu_shell_deactivate (menu_shell);
      g_signal_emit (menu_shell, menu_shell_signals[SELECTION_DONE], 0);
    }

    priv->activated_submenu = FALSE;
  }

  return TRUE;
}

void
geda_menu_shell_set_keyboard_mode (GedaMenuShell *menu_shell,
                                   bool          keyboard_mode)
{
  menu_shell->keyboard_mode = keyboard_mode;
}

bool
geda_menu_shell_get_keyboard_mode (GedaMenuShell *menu_shell)
{
  return menu_shell->keyboard_mode;
}

void
geda_menu_shell_update_mnemonics (GedaMenuShell *menu_shell)
{
  GedaMenuShell *target;
  bool     auto_mnemonics;
  bool     found;
  bool     mnemonics_visible;

  g_object_get (gtk_widget_get_settings (GTK_WIDGET (menu_shell)),
                "gtk-auto-mnemonics", &auto_mnemonics, NULL);

  if (!auto_mnemonics)
    return;

  target = menu_shell;
  found = FALSE;
  while (target) {

    GedaMenuShellPriv *priv = target->priv;
    GtkWidget *toplevel = gtk_widget_get_toplevel (GTK_WIDGET (target));

    /* The idea with keyboard mode is that once you start using
     * the keyboard to navigate the menus, we show mnemonics
     * until the menu navigation is over. To that end, we spread
     * the keyboard mode upwards in the menu hierarchy here.
     * Also see gtk_menu_popup, where we inherit it downwards.
     */
    if (menu_shell->keyboard_mode) {
      target->keyboard_mode = TRUE;
    }

    /* While navigating menus, the first parent menu with an active
     * item is the one where mnemonics are effective, as can be seen
     * in geda_menu_shell_key_press below.
     * We also show mnemonics in context menus. The grab condition is
     * necessary to ensure we remove underlines from menu bars when
     * dismissing menus.
     */
    mnemonics_visible = target->keyboard_mode &&
    (((target->active_menu_item || priv->in_unselectable_item) && !found) ||
    (target == menu_shell &&
    !target->parent_menu_shell &&
    gtk_widget_has_grab (GTK_WIDGET (target))));

    /* While menus are up, only show underlines inside the menubar,
     * not in the entire window.
     */
    if (GEDA_IS_MENU_BAR (target)) {

      gtk_window_set_mnemonics_visible (GTK_WINDOW (toplevel), FALSE);

      geda_label_mnemonics_visible_apply_recursively (GTK_WIDGET (target),
                                                      mnemonics_visible);
    }
    else
      gtk_window_set_mnemonics_visible (GTK_WINDOW (toplevel), mnemonics_visible);

    if (target->active_menu_item || priv->in_unselectable_item)
      found = TRUE;

    target = GEDA_MENU_SHELL (target->parent_menu_shell);
  }
}

static int
geda_menu_shell_key_press (GtkWidget *widget, GdkEventKey *event)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (widget);
  GedaMenuShellPriv *priv = menu_shell->priv;
  bool     enable_mnemonics;

  menu_shell->keyboard_mode = TRUE;

  if (!(menu_shell->active_menu_item || priv->in_unselectable_item) && menu_shell->parent_menu_shell)
    return gtk_widget_event (menu_shell->parent_menu_shell, (GdkEvent *)event);

  if (gtk_bindings_activate_event (GTK_OBJECT (widget), event))
    return TRUE;

  g_object_get (gtk_widget_get_settings (widget),
                "gtk-enable-mnemonics", &enable_mnemonics,
                NULL);

  if (enable_mnemonics) {
    return geda_menu_shell_activate_mnemonic (menu_shell, event);
  }

  return FALSE;
}

static int
geda_menu_shell_enter_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (widget);

  if (event->mode == GDK_CROSSING_GTK_GRAB ||
    event->mode == GDK_CROSSING_GTK_UNGRAB ||
    event->mode == GDK_CROSSING_STATE_CHANGED)
    return TRUE;

  if (menu_shell->active) {

    GtkWidget *menu_item;

    menu_item = gtk_get_event_widget ((GdkEvent*) event);

    if (!menu_item)
      return TRUE;

    if (GEDA_IS_MENU_ITEM (menu_item) &&
      !geda_menu_item_is_selectable (menu_item))
    {
      GedaMenuShellPriv *priv;

      priv = menu_shell->priv;
      priv->in_unselectable_item = TRUE;

      return TRUE;
    }

    if (menu_item->parent == widget && GEDA_IS_MENU_ITEM (menu_item))
    {
      if (menu_shell->ignore_enter)
        return TRUE;

      if (event->detail != GDK_NOTIFY_INFERIOR) {

        GtkWidget *submenu;

        if (gtk_widget_get_state (menu_item) != GTK_STATE_PRELIGHT) {
          geda_menu_shell_select_item (menu_shell, menu_item);
        }

        submenu = geda_menu_item_get_submenu(GEDA_MENU_ITEM (menu_item));

        /* If any mouse button is down, and there is a submenu
         * that is not yet visible, activate it. It's sufficient
         * to check for any button's mask (not only the one
         * matching menu_shell->button), because there is no
         * situation a mouse button could be pressed while
         * entering a menu item where we wouldn't want to show
         * its submenu.
         */
        if ((event->state & (GDK_BUTTON1_MASK | GDK_BUTTON2_MASK | GDK_BUTTON3_MASK)) &&
             submenu != NULL)
        {
          GedaMenuShellPriv *priv;

          priv = GEDA_MENU_SHELL (menu_item->parent)->priv;
          priv->activated_submenu = TRUE;

          if (!gtk_widget_get_visible (submenu)) {

            bool touchscreen_mode;

            g_object_get (gtk_widget_get_settings (widget),
                          "gtk-touchscreen-mode", &touchscreen_mode,
                          NULL);

            if (touchscreen_mode) {
              geda_menu_item_popup_submenu (GEDA_MENU_ITEM (menu_item), TRUE);
            }
          }
        }
      }
    }
    else if (menu_shell->parent_menu_shell) {
      gtk_widget_event (menu_shell->parent_menu_shell, (GdkEvent*) event);
    }
  }

  return TRUE;
}

static int
geda_menu_shell_leave_notify (GtkWidget        *widget,
                 GdkEventCrossing *event)
{
  if (event->mode == GDK_CROSSING_GTK_GRAB ||
      event->mode == GDK_CROSSING_GTK_GRAB ||
      event->mode == GDK_CROSSING_STATE_CHANGED) {
    return TRUE;
  }

  if (gtk_widget_get_visible (widget)) {

    GedaMenuShell *menu_shell   = GEDA_MENU_SHELL (widget);
    GtkWidget     *event_widget = gtk_get_event_widget ((GdkEvent*) event);
    GedaMenuItem  *menu_item;
    GtkWidget     *submenu;

    if (!event_widget || !GEDA_IS_MENU_ITEM (event_widget))
      return TRUE;

    menu_item = GEDA_MENU_ITEM (event_widget);

    if (!geda_menu_item_is_selectable (event_widget)) {

      GedaMenuShellPriv *priv;

      priv = menu_shell->priv;
      priv->in_unselectable_item = TRUE;

      return TRUE;
    }

    submenu = geda_menu_item_get_submenu(GEDA_MENU_ITEM (menu_item));

    if ((menu_shell->active_menu_item == event_widget) && (submenu == NULL))
    {
      if ((event->detail != GDK_NOTIFY_INFERIOR) &&
        (gtk_widget_get_state (GTK_WIDGET (menu_item)) != GTK_STATE_NORMAL))
      {
        geda_menu_shell_deselect (menu_shell);
      }
    }
    else if (menu_shell->parent_menu_shell) {

      gtk_widget_event (menu_shell->parent_menu_shell, (GdkEvent*) event);
    }
  }

  return TRUE;
}

static void
geda_menu_shell_screen_changed (GtkWidget *widget, GdkScreen *previous_screen)
{
  geda_menu_shell_reset_key_hash (GEDA_MENU_SHELL (widget));
}

static void
geda_menu_shell_add (GtkContainer *container, GtkWidget *widget)
{
  geda_menu_shell_append (GEDA_MENU_SHELL (container), widget);
}

static void
geda_menu_shell_remove (GtkContainer *container, GtkWidget *widget)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (container);
  int was_visible;

  was_visible          = gtk_widget_get_visible (widget);
  menu_shell->children = g_list_remove (menu_shell->children, widget);

  if (widget == menu_shell->active_menu_item) {

      g_signal_emit_by_name (menu_shell->active_menu_item, "deselect");
      //gtk_item_deselect (GTK_ITEM (menu_shell->active_menu_item));
      menu_shell->active_menu_item = NULL;
    }

  gtk_widget_unparent (widget);

  /* queue resize regardless of gtk_widget_get_visible (container),
   * since that's what is needed by toplevels.
   */
  if (was_visible) {
    gtk_widget_queue_resize (GTK_WIDGET (container));
  }
}

static void
geda_menu_shell_forall (GtkContainer *container,
               bool          include_internals,
               GtkCallback   callback,
               gpointer      callback_data)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (container);
  GtkWidget     *child;
  GList         *children;

  children = menu_shell->children;
  while (children) {

    child = children->data;
    children = children->next;

    (* callback) (child, callback_data);
  }
}

static void
geda_real_menu_shell_deactivate (GedaMenuShell *menu_shell)
{
  if (menu_shell->active) {

    menu_shell->button = 0;
    menu_shell->active = FALSE;
    menu_shell->activate_time = 0;

    if (menu_shell->active_menu_item) {

      geda_menu_item_deselect (GEDA_MENU_ITEM (menu_shell->active_menu_item));
      menu_shell->active_menu_item = NULL;
    }

    if (menu_shell->have_grab) {

      menu_shell->have_grab = FALSE;
      gtk_grab_remove (GTK_WIDGET (menu_shell));
    }
    if (menu_shell->have_xgrab) {

      GdkDisplay *display = gtk_widget_get_display (GTK_WIDGET (menu_shell));

      menu_shell->have_xgrab = FALSE;
      gdk_display_pointer_ungrab (display, GDK_CURRENT_TIME);
      gdk_display_keyboard_ungrab (display, GDK_CURRENT_TIME);
    }

    menu_shell->keyboard_mode = FALSE;

    geda_menu_shell_update_mnemonics (menu_shell);
  }
}

static int
geda_menu_shell_is_item (GedaMenuShell *menu_shell, GtkWidget *child)
{
  GtkWidget *parent;

  g_return_val_if_fail (GEDA_IS_MENU_SHELL (menu_shell), FALSE);
  g_return_val_if_fail (child != NULL, FALSE);

  parent = child->parent;
  while (GEDA_IS_MENU_SHELL (parent)) {

    if (parent == (GtkWidget*) menu_shell) {
      return TRUE;
    }
    parent = GEDA_MENU_SHELL (parent)->parent_menu_shell;
  }

  return FALSE;
}

static GtkWidget*
geda_menu_shell_get_item (GedaMenuShell *menu_shell,
             GdkEvent     *event)
{
  GtkWidget *menu_item;

  menu_item = gtk_get_event_widget ((GdkEvent*) event);

  while (menu_item && !GEDA_IS_MENU_ITEM (menu_item))
    menu_item = menu_item->parent;

  if (menu_item && geda_menu_shell_is_item (menu_shell, menu_item))
    return menu_item;
  else
    return NULL;
}

/* Handlers for action signals */

void
geda_menu_shell_select_item (GedaMenuShell *menu_shell,
                             GtkWidget     *menu_item)
{
  GedaMenuShellClass *class;

  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));
  g_return_if_fail (GEDA_IS_MENU_ITEM (menu_item));

  class = GEDA_MENU_SHELL_GET_CLASS (menu_shell);

  if (class->select_item &&
      !(menu_shell->active &&
    menu_shell->active_menu_item == menu_item))
    class->select_item (menu_shell, menu_item);
}

static void
geda_menu_shell_real_select_item (GedaMenuShell *menu_shell,
                                  GtkWidget     *menu_item)
{
  PackDirection pack_dir = PACK_DIRECTION (menu_shell);
  GtkWidget    *submenu;

  if (menu_shell->active_menu_item) {

      geda_menu_item_deselect (GEDA_MENU_ITEM (menu_shell->active_menu_item));
      menu_shell->active_menu_item = NULL;
  }

  if (!geda_menu_item_is_selectable (menu_item)) {

      GedaMenuShellPriv *priv = menu_shell->priv;

      priv->in_unselectable_item = TRUE;
      geda_menu_shell_update_mnemonics (menu_shell);

      return;
  }

  menu_shell->active_menu_item = menu_item;
  if (pack_dir == PACK_DIRECTION_TTB || pack_dir == PACK_DIRECTION_BTT) {
    geda_menu_item_set_submenu_placement (GEDA_MENU_ITEM (menu_shell->active_menu_item), GTK_LEFT_RIGHT);
  }
  else {
    geda_menu_item_set_submenu_placement (GEDA_MENU_ITEM (menu_shell->active_menu_item),
                                         GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement);
  }

  geda_menu_item_select (GEDA_MENU_ITEM (menu_shell->active_menu_item));

  geda_menu_shell_update_mnemonics (menu_shell);

  /* This allows the bizarre radio buttons-with-submenus-display-history
   * behavior
   */

  submenu = geda_menu_item_get_submenu(GEDA_MENU_ITEM (menu_shell->active_menu_item));

  if (submenu) {
    gtk_widget_activate (menu_shell->active_menu_item);
  }
}

/*!
 * \brief GedaMenuShell Deselect
 * \par Function Description
 *  Deselects the currently selected item from the menu shell,
 *  if any.
 *
 * \param [in] menu_shell a #GedaMenuShell
 */
void
geda_menu_shell_deselect (GedaMenuShell *menu_shell)
{
  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));

  if (menu_shell->active_menu_item) {

    geda_menu_item_deselect (GEDA_MENU_ITEM (menu_shell->active_menu_item));
    menu_shell->active_menu_item = NULL;
    geda_menu_shell_update_mnemonics (menu_shell);
  }
}

void
geda_menu_shell_activate_item (GedaMenuShell *menu_shell,
                               GtkWidget     *menu_item,
                               bool           force_deactivate)
{
  GSList *slist;
  GSList *shells     = NULL;
  bool    deactivate = force_deactivate;

  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));
  g_return_if_fail (GEDA_IS_MENU_ITEM (menu_item));

  if (!deactivate)
    deactivate = GEDA_MENU_ITEM_GET_CLASS (menu_item)->hide_on_activate;

  g_object_ref (menu_shell);
  g_object_ref (menu_item);

  if (deactivate) {

    GedaMenuShell *parent_menu_shell = menu_shell;

    do {

      g_object_ref (parent_menu_shell);
      shells = g_slist_prepend (shells, parent_menu_shell);
      parent_menu_shell = (GedaMenuShell*) parent_menu_shell->parent_menu_shell;
    }
    while (parent_menu_shell);
    shells = g_slist_reverse (shells);

    geda_menu_shell_deactivate (menu_shell);

    /* flush the x-queue, so any grabs are removed and
     * the menu is actually taken down
     */
    gdk_display_sync (gtk_widget_get_display (menu_item));
  }

  gtk_widget_activate (menu_item);

  for (slist = shells; slist; slist = slist->next) {

    g_signal_emit (slist->data, menu_shell_signals[SELECTION_DONE], 0);
    g_object_unref (slist->data);
  }

  g_slist_free (shells);

  g_object_unref (menu_shell);
  g_object_unref (menu_item);
}

/* Distance should be +/- 1 */
static bool
geda_menu_shell_real_move_selected (GedaMenuShell  *menu_shell,
                                    int             distance)
{
  if (menu_shell->active_menu_item) {

    GList *node = g_list_find (menu_shell->children,
                               menu_shell->active_menu_item);
    GList *start_node = node;
    bool     wrap_around;

    g_object_get (gtk_widget_get_settings (GTK_WIDGET (menu_shell)),
                  "gtk-keynav-wrap-around", &wrap_around,
                  NULL);

    if (distance > 0) {

      node = node->next;

      while (node != start_node &&
            (!node || !geda_menu_item_is_selectable (node->data)))
      {
        if (node)
          node = node->next;
        else if (wrap_around)
          node = menu_shell->children;
        else
        {
          gtk_widget_error_bell (GTK_WIDGET (menu_shell));
          break;
        }
      }
    }
    else {

      node = node->prev;
      while (node != start_node &&
        (!node || !geda_menu_item_is_selectable (node->data)))
      {
        if (node)
          node = node->prev;
        else if (wrap_around)
          node = g_list_last (menu_shell->children);
        else
        {
          gtk_widget_error_bell (GTK_WIDGET (menu_shell));
          break;
        }
      }
    }

    if (node) {
      geda_menu_shell_select_item (menu_shell, node->data);
    }
  }

  return TRUE;
}

/* Distance should be +/- 1 */
static void
geda_menu_shell_move_selected (GedaMenuShell  *menu_shell, int distance)
{
  bool     handled = FALSE;

  g_signal_emit (menu_shell, menu_shell_signals[MOVE_SELECTED], 0,
                 distance, &handled);
}

/*!
 * \brief GedaMenuShell Select First
 * \par Function Description
 * Select the first visible or selectable child of the menu shell;
 * don't select tearoff items unless the only item is a tearoff
 * item.
 *
 * \param [in] menu_shell        a #GedaMenuShell
 * \param [in] search_sensitive: if %TRUE, search for the first selectable
 *                               menu item, otherwise select nothing if
 *                               the first item isn't sensitive. This
 *                               should be %FALSE if the menu is being
 *                               popped up initially.
 */
void
geda_menu_shell_select_first (GedaMenuShell *menu_shell,
                              bool           search_sensitive)
{
  GtkWidget *to_select = NULL;
  GList     *tmp_list;

  tmp_list = menu_shell->children;

  while (tmp_list) {

      GtkWidget *child = tmp_list->data;

      if ((!search_sensitive && gtk_widget_get_visible (child)) ||
      geda_menu_item_is_selectable (child))
    {
      to_select = child;
      if (!GEDA_IS_TEAROFF_MENU_ITEM (child))
        break;
    }

      tmp_list = tmp_list->next;
    }

  if (to_select) {
    geda_menu_shell_select_item (menu_shell, to_select);
  }
}

void
geda_menu_shell_select_last (GedaMenuShell *menu_shell,
                 bool          search_sensitive)
{
  GtkWidget *to_select = NULL;
  GList *tmp_list;

  tmp_list = g_list_last (menu_shell->children);

  while (tmp_list) {

    GtkWidget *child = tmp_list->data;

    if ((!search_sensitive &&
          gtk_widget_get_visible (child)) ||
          geda_menu_item_is_selectable (child))
    {
      to_select = child;
      if (!GEDA_IS_TEAROFF_MENU_ITEM (child))
        break;
    }

    tmp_list = tmp_list->prev;
  }

  if (to_select) {
    geda_menu_shell_select_item (menu_shell, to_select);
  }
}

static bool
geda_menu_shell_select_submenu_first (GedaMenuShell *menu_shell)
{
  GedaMenuItem *menu_item;
  GtkWidget    *submenu;

  if (menu_shell->active_menu_item == NULL)
    return FALSE;

  menu_item = GEDA_MENU_ITEM (menu_shell->active_menu_item);

  submenu   = geda_menu_item_get_submenu(menu_item);

  if (submenu) {

    geda_menu_item_popup_submenu (menu_item, FALSE);
    geda_menu_shell_select_first (GEDA_MENU_SHELL (submenu), TRUE);

    if (GEDA_MENU_SHELL (submenu)->active_menu_item)
      return TRUE;
  }

  return FALSE;
}

static void
geda_real_menu_shell_move_current (GedaMenuShell *menu_shell,
                                   MenuDirection  direction)
{
  GedaMenuShellPriv *priv;
  GedaMenuShell     *parent_menu_shell;
  GedaMenuItem      *active_menu_item;
  bool had_selection;
  bool touchscreen_mode;

  priv              = menu_shell->priv;
  parent_menu_shell = NULL;
  priv->in_unselectable_item = FALSE;

  g_object_get (gtk_widget_get_settings (GTK_WIDGET (menu_shell)),
                "gtk-touchscreen-mode", &touchscreen_mode,
                NULL);

  if (menu_shell->parent_menu_shell) {
    parent_menu_shell = GEDA_MENU_SHELL (menu_shell->parent_menu_shell);
  }

  active_menu_item = GEDA_MENU_ITEM (menu_shell->active_menu_item);

  had_selection = active_menu_item != NULL;

  GtkWidget *submenu;

  if (had_selection) {
    submenu = geda_menu_item_get_submenu(active_menu_item);
  }
  else {
    submenu = NULL;
  }

  switch (direction) {

    case MENU_DIR_PARENT:
      if (touchscreen_mode && had_selection && submenu &&
          gtk_widget_get_visible (submenu))
      {
        /* if we are on a menu item that has an open submenu but the
         * focus is not in that submenu (e.g. because it's empty or
         * has only insensitive items), close that submenu instead
         * of running into the code below which would close *this*
         * menu.
         */
        geda_menu_item_popdown_submenu (active_menu_item);
        geda_menu_shell_update_mnemonics (menu_shell);
      }
      else if (parent_menu_shell) {

        if (touchscreen_mode) {
          GedaMenuItem *menu_item = GEDA_MENU_ITEM(GEDA_MENU(menu_shell)->parent_menu_item);
          /* close menu when returning from submenu. */
          geda_menu_item_popdown_submenu (menu_item);
          geda_menu_shell_update_mnemonics (parent_menu_shell);
          break;
        }

        if (GEDA_MENU_SHELL_GET_CLASS (parent_menu_shell)->submenu_placement ==
            GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement)
        {
          geda_menu_shell_deselect (menu_shell);
        }
        else
        {
          if (PACK_DIRECTION (parent_menu_shell) == PACK_DIRECTION_LTR) {
            geda_menu_shell_move_selected (parent_menu_shell, -1);
          }
          else {
            geda_menu_shell_move_selected (parent_menu_shell, 1);
          }
          geda_menu_shell_select_submenu_first (parent_menu_shell);
        }
      }
      /* If there is no parent and the submenu is in the opposite direction
       * to the menu, then make the PARENT direction wrap around to the
       * bottom of the submenu.
       */
      else if (had_selection &&
               geda_menu_item_is_selectable ((GtkWidget*)active_menu_item) &&
               submenu)
      {
        GedaMenuShellClass *shell_class = GEDA_MENU_SHELL_GET_CLASS (menu_shell);

        if (shell_class->submenu_placement !=
          GEDA_MENU_SHELL_GET_CLASS (submenu)->submenu_placement)
        {
          geda_menu_shell_select_last (GEDA_MENU_SHELL(submenu), TRUE);
        }
      }
      break;

      case MENU_DIR_CHILD:
        if (had_selection &&
            geda_menu_item_is_selectable ((GtkWidget*)active_menu_item) &&
            submenu)
        {
          if (geda_menu_shell_select_submenu_first (menu_shell))
            break;
        }

        /* Try to find a menu running the opposite direction */
        while (parent_menu_shell &&
          (GEDA_MENU_SHELL_GET_CLASS (parent_menu_shell)->submenu_placement ==
           GEDA_MENU_SHELL_GET_CLASS (menu_shell)->submenu_placement))
        {
          parent_menu_shell = GEDA_MENU_SHELL (parent_menu_shell->parent_menu_shell);
        }

        if (parent_menu_shell) {

          if (PACK_DIRECTION (parent_menu_shell) == PACK_DIRECTION_LTR)
            geda_menu_shell_move_selected (parent_menu_shell, 1);
          else
            geda_menu_shell_move_selected (parent_menu_shell, -1);

          geda_menu_shell_select_submenu_first (parent_menu_shell);
        }
        break;

      case MENU_DIR_PREV:
        geda_menu_shell_move_selected (menu_shell, -1);
        if (!had_selection &&
            !menu_shell->active_menu_item &&
             menu_shell->children)
          geda_menu_shell_select_last (menu_shell, TRUE);
        break;

      case MENU_DIR_NEXT:
        geda_menu_shell_move_selected (menu_shell, 1);
        if (!had_selection &&
            !menu_shell->active_menu_item &&
             menu_shell->children)
          geda_menu_shell_select_first (menu_shell, TRUE);
        break;

      default:
        break;
  }
}

static void
geda_real_menu_shell_activate_current (GedaMenuShell *menu_shell,
                                       bool           force_hide)
{
  GedaMenuItem  *menu_item = GEDA_MENU_ITEM (menu_shell->active_menu_item);

  if (menu_item &&
      geda_menu_item_is_selectable (menu_shell->active_menu_item))
  {
    if (!geda_menu_item_get_submenu(GEDA_MENU_ITEM (menu_shell->active_menu_item)))
    {
      geda_menu_shell_activate_item (menu_shell,
                                     menu_shell->active_menu_item,
                                     force_hide);
    }
    else
    {
      geda_menu_item_popup_submenu (menu_item, FALSE);
    }
  }
}

static void
geda_real_menu_shell_cancel (GedaMenuShell *menu_shell)
{
  /* Unset the active menu item so gtk_menu_popdown() doesn't see it.
   */
  geda_menu_shell_deselect (menu_shell);

  geda_menu_shell_deactivate (menu_shell);
  g_signal_emit (menu_shell, menu_shell_signals[SELECTION_DONE], 0);
}

/* Handlers for action signals */

static void
geda_real_menu_shell_cycle_focus (GedaMenuShell   *menu_shell,
                                  GtkDirectionType dir)
{
  while (menu_shell && !GEDA_IS_MENU_BAR (menu_shell)) {

    if (menu_shell->parent_menu_shell) {
      menu_shell = GEDA_MENU_SHELL (menu_shell->parent_menu_shell);
    }
    else {
      menu_shell = NULL;
    }
  }

  if (menu_shell) {
    geda_menu_bar_cycle_focus (GEDA_MENU_BAR (menu_shell), dir);
  }
}

/*!
 * \brief Deactivates a GedaMenuShell
 * \par Function Description
 * Deactivates the menu shell. Typically this results in the menu
 * shell being erased from the screen.
 *
 * \param [in] menu_shell a #GedaMenuShell
 */
void
geda_menu_shell_deactivate (GedaMenuShell *menu_shell)
{
  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));

  if (menu_shell->active)
    g_signal_emit (menu_shell, menu_shell_signals[DEACTIVATE], 0);
}

/*!
 * \brief Retrieve GedaMenuShell Parent
 * \par Function Description
 * Gets the parent menu shell. The parent menu shell of a submenu
 * is the #GedaMenu or #GedaMenuBar from which it was opened up.
 *
 * \param [in] menu_shell a #GedaMenuShell
 *
 * \returns the parent #GedaMenuShell
 */
GtkWidget *
geda_menu_shell_get_parent_shell (GedaMenuShell *menu_shell)
{
  g_return_val_if_fail (GEDA_IS_MENU_SHELL (menu_shell), NULL);

  return menu_shell->parent_menu_shell;
}

int
geda_menu_shell_get_popup_delay (GedaMenuShell *menu_shell)
{
  GedaMenuShellClass *class = GEDA_MENU_SHELL_GET_CLASS (menu_shell);

  if (class->get_popup_delay) {
    return class->get_popup_delay (menu_shell);
  }
  else {

    return MENU_POPUP_DELAY;
  }
}

/*!
 * \brief Cancel GedaMenuShell selection
 * \par Function Description
 * Cancels the selection within the menu shell.
 *
 * \param [in] menu_shell a #GedaMenuShell
 */
void
geda_menu_shell_cancel (GedaMenuShell *menu_shell)
{
  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));

  g_signal_emit (menu_shell, menu_shell_signals[CANCEL], 0);
}

/*--------------------------- Begin Hash Helpers ---------------------------*/

static GedaMnemonicHash *
geda_menu_shell_get_mnemonic_hash (GedaMenuShell *menu_shell, bool create)
{
  GedaMenuShellPriv *priv = menu_shell->priv;

  if (!priv->mnemonic_hash && create)
    priv->mnemonic_hash = geda_mnemonic_hash_new ();

  return priv->mnemonic_hash;
}

static void
menu_shell_add_mnemonic_foreach (unsigned int keyval, GSList *targets, void *data)
{
  GedaKeyHash *key_hash = data;

  geda_key_hash_add_entry (key_hash, keyval, 0, (void*)(unsigned int)keyval);
}

static GedaKeyHash *
geda_menu_shell_get_key_hash (GedaMenuShell *menu_shell, bool create)
{
  GedaMenuShellPriv *priv = menu_shell->priv;
  GtkWidget *widget = GTK_WIDGET (menu_shell);

  if (!priv->key_hash && create && gtk_widget_has_screen (widget)) {

    GedaMnemonicHash *mnemonic_hash;
    GdkScreen       *screen;
    GdkKeymap       *keymap;

    mnemonic_hash = geda_menu_shell_get_mnemonic_hash (menu_shell, FALSE);

    if (!mnemonic_hash)
      return NULL;

    screen = gtk_widget_get_screen (widget);
    keymap = gdk_keymap_get_for_display (gdk_screen_get_display (screen));

    priv->key_hash = geda_key_hash_new (keymap, NULL);

    geda_mnemonic_hash_foreach (mnemonic_hash,
                                menu_shell_add_mnemonic_foreach,
                                priv->key_hash);
  }

  return priv->key_hash;
}
/*---------------------------- End Hash Helpers -----------------------------*/

/* Helper called by:
 *
 *  geda_menu_shell_add_mnemonic()
 *  geda_menu_shell_remove_mnemonic()
 *  geda_menu_shell_screen_changed()
 */
static void
geda_menu_shell_reset_key_hash (GedaMenuShell *menu_shell)
{
  GedaMenuShellPriv *priv = menu_shell->priv;

  if (priv->key_hash) {
    geda_key_hash_free (priv->key_hash);
    priv->key_hash = NULL;
  }
}

static bool
geda_menu_shell_activate_mnemonic (GedaMenuShell *menu_shell,
                                  GdkEventKey   *event)
{
  GedaMnemonicHash *mnemonic_hash;
  GedaKeyHash      *key_hash;
  GSList           *entries;
  bool result = FALSE;

  mnemonic_hash = geda_menu_shell_get_mnemonic_hash (menu_shell, FALSE);
  if (!mnemonic_hash)
    return FALSE;

  key_hash = geda_menu_shell_get_key_hash (menu_shell, TRUE);
  if (!key_hash)
    return FALSE;

  entries = geda_key_hash_lookup (key_hash,
                                  event->hardware_keycode,
                                  event->state,
                                  gtk_accelerator_get_default_mod_mask (),
                                  event->group);
  if (entries) {

      result = geda_mnemonic_hash_activate (mnemonic_hash,
                                            (unsigned int)(long)entries->data);
      //g_slist_free (entries);
  }
  return result;
}

void
geda_menu_shell_add_mnemonic (GedaMenuShell *menu_shell,
                              unsigned int   keyval,
                              GtkWidget    *target)
{
  GedaMnemonicHash *mnemonic_hash;

  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));
  g_return_if_fail (GTK_IS_WIDGET (target));

  mnemonic_hash = geda_menu_shell_get_mnemonic_hash (menu_shell, TRUE);
  geda_mnemonic_hash_add (mnemonic_hash, keyval, target);
  geda_menu_shell_reset_key_hash (menu_shell);
}

void
geda_menu_shell_remove_mnemonic (GedaMenuShell *menu_shell,
                                 unsigned int   keyval,
                                 GtkWidget     *target)
{
  GedaMnemonicHash *mnemonic_hash;

  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));
  g_return_if_fail (GTK_IS_WIDGET (target));

  mnemonic_hash = geda_menu_shell_get_mnemonic_hash (menu_shell, TRUE);

  geda_mnemonic_hash_remove (mnemonic_hash, keyval, target);

  geda_menu_shell_reset_key_hash (menu_shell);
}

/*!
 * \brief geda_menu_shell_get_take_focus
 * \par Function Description
 * Returns %TRUE if the menu shell will take the keyboard focus on popup.
 *
 * \param [in] menu_shell a #GedaMenuShell
 *
 * Returns: %TRUE if the menu shell will take the keyboard focus on popup.
 */
bool
geda_menu_shell_get_take_focus (GedaMenuShell *menu_shell)
{
  GedaMenuShellPriv *priv;

  g_return_val_if_fail (GEDA_IS_MENU_SHELL (menu_shell), FALSE);

  priv = menu_shell->priv;

  return priv->take_focus;
}

/*!
 * \brief geda_menu_shell_set_take_focus
 * \par Function Description
 * If @take_focus is %TRUE (the default) the menu shell will take the keyboard
 * focus so that it will receive all keyboard events which is needed to enable
 * keyboard navigation in menus.
 *
 * Setting @take_focus to %FALSE is useful only for special applications
 * like virtual keyboard implementations which should not take keyboard
 * focus.
 *
 * The @take_focus state of a menu or menu bar is automatically propagated
 * to submenus whenever a submenu is popped up, so you don't have to worry
 * about recursively setting it for your entire menu hierarchy. Only when
 * programmatically picking a submenu and popping it up manually, the
 * @take_focus property of the submenu needs to be set explicitely.
 *
 * Note that setting it to %FALSE has side-effects:
 *
 * If the focus is in some other app, it keeps the focus and keynav in
 * the menu doesn't work. Consequently, keynav on the menu will only
 * work if the focus is on some toplevel owned by the onscreen keyboard.
 *
 * To avoid confusing the user, menus with @take_focus set to %FALSE
 * should not display mnemonics or accelerators, since it cannot be
 * guaranteed that they will work.
 *
 * \param [in] menu_shell The GedaMenuShell
 * \param [in] take_focus %TRUE if the menu shell should take the keyboard
 *                        focus on popup.
 *
 * \sa  gdk_keyboard_grab()
 */
void
geda_menu_shell_set_take_focus (GedaMenuShell *menu_shell,
                                bool           take_focus)
{
  GedaMenuShellPriv *priv;

  g_return_if_fail (GEDA_IS_MENU_SHELL (menu_shell));

  priv = menu_shell->priv;

  if (priv->take_focus != take_focus) {

    priv->take_focus = take_focus;
    g_object_notify (G_OBJECT (menu_shell), "take-focus");
  }
}

/** @} geda-menu-shell */
