/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_combobox.c
 *
 * Copyright (C) 2002, 2003  Kristian Rietveld <kris@gtk.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com> with
 * many modifications, August 16th, 2014.
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_combobox.h"
#include "../../include/geda_entry.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_separator.h"
#include "../../include/geda_tearoff_menu_item.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/*! \brief A widget used to choose from a list of items
 *  \par SECTION:GedaComboBox
 * A GedaComboBox is a widget that allows the user to choose from a list
 * of valid choices. The GedaComboBox displays the selected choice. When
 * activated, the GedaComboBox displays a popup which allows the user to
 * make a new choice. The style in which the selected value is displayed,
 * and the style of the popup is determined by the current theme. It may
 * be similar to a Windows-style combo box.
 * \par
 * The GedaComboBox uses the model-view pattern; the list of valid choices
 * is specified in the form of a tree model, and the display of the choices
 * can be adapted to the data in the model by using cell renderers, as you
 * would in a tree view. This is possible since GedaComboBox implements the
 * GtkCellLayout interface. The tree model holding the valid choices is
 * not restricted to a flat list, it can be a real tree, and the popup will
 * reflect the tree structure.
 * \par
 * To allow the user to enter values not in the model, the 'has-entry'
 * property allows the GedaComboBox to contain a GedaEntry. This entry
 * can be accessed by calling geda_get_child_widget() on the combo box.
 * \par
 * For a simple list of textual choices, the model-view API of GedaComboBox
 * can be a bit overwhelming. In this case, #GedaComboBoxText offers a
 * simple alternative. Both GedaComboBox and #GedaComboBoxText can contain
 * an entry.
 *
 * \defgroup GedaComboBox Combination Box
 * @{
 */

/* WELCOME, to THE house of evil code */

typedef struct _ComboCellInfo ComboCellInfo;
struct _ComboCellInfo
{
  GtkCellRenderer *cell;
  GSList          *attributes;

  GtkCellLayoutDataFunc func;
  void *func_data;
  GDestroyNotify destroy;

  unsigned int expand : 1;
  unsigned int pack : 1;
};

struct _GedaComboBoxData
{
  GtkTreeModel *model;

  int col_column;
  int row_column;

  int wrap_width;
  GtkShadowType shadow_type;

  int active; /* Only temporary */
  GtkTreeRowReference *active_row;

  GtkWidget *tree_view;
  GtkTreeViewColumn *column;

  GtkWidget *cell_view;
  GtkWidget *cell_view_frame;

  GtkWidget *button;
  GtkWidget *box;
  GtkWidget *arrow;
  GtkWidget *separator;

  GtkWidget *popup_widget;
  GtkWidget *popup_window;
  GtkWidget *scrolled_window;

  unsigned int inserted_id;
  unsigned int deleted_id;
  unsigned int reordered_id;
  unsigned int changed_id;
  unsigned int popup_idle_id;
  unsigned int activate_button;
  unsigned int activate_time;
  unsigned int scroll_timer;
  unsigned int toggled_id;
  unsigned int resize_idle_id;

  int width;
  int height;

  /* For "has-entry" specific behavior we track
   * an automated cell renderer and text column */
  int  text_column;
  GtkCellRenderer *text_renderer;

  GSList *cells;

  unsigned int list_view;

  unsigned int as_list : 1;
  unsigned int popup_in_progress : 1;
  unsigned int popup_shown : 1;
  unsigned int add_tearoffs : 1;
  unsigned int has_frame : 1;
  unsigned int is_cell_renderer : 1;
  unsigned int editing_canceled : 1;
  unsigned int auto_scroll : 1;
  unsigned int focus_on_click : 1;
  unsigned int button_sensitivity : 2;
  unsigned int has_entry : 1;

  GtkTreeViewRowSeparatorFunc row_separator_func;
  void           *            row_separator_data;
  GDestroyNotify              row_separator_destroy;

  char *tearoff_title;
};

enum {
  CHANGED,
  MOVE_ACTIVE,
  POPUP,
  POPDOWN,
  FORMAT_ENTRY_TEXT,
  VIEW_CHANGED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_MODEL,
  PROP_WRAP_WIDTH,
  PROP_ROW_SPAN_COLUMN,
  PROP_COLUMN_SPAN_COLUMN,
  PROP_ACTIVE,
  PROP_ADD_TEAROFFS,
  PROP_TEAROFF_TITLE,
  PROP_HAS_FRAME,
  PROP_FOCUS_ON_CLICK,
  PROP_POPUP_SHOWN,
  PROP_BUTTON_SENSITIVITY,
  PROP_EDITING_CANCELED,
  PROP_HAS_ENTRY,
  PROP_LIST_VIEW,
  PROP_ENTRY_TEXT_COLUMN
};

static unsigned int combo_box_signals[LAST_SIGNAL] = {0,};

#define SCROLL_TIME  100

/* common */
static void     geda_combo_box_cell_layout_init     (GtkCellLayoutIface    *iface);
static void     geda_combo_box_cell_editable_init   (GtkCellEditableIface  *iface);
static void     geda_combo_box_dispose              (GObject          *object);
static void     geda_combo_box_finalize             (GObject          *object);
static void     geda_combo_box_destroy              (GtkObject        *object);

static void     geda_combo_box_set_property         (GObject         *object,
                                                     unsigned int     prop_id,
                                                     const GValue    *value,
                                                     GParamSpec      *spec);
static void     geda_combo_box_get_property         (GObject         *object,
                                                     unsigned int     prop_id,
                                                     GValue          *value,
                                                     GParamSpec      *spec);

static void     geda_combo_box_state_changed        (GtkWidget       *widget,
                                                     GtkStateType     previous);
static void     geda_combo_box_grab_focus           (GtkWidget       *widget);
static void     geda_combo_box_style_set            (GtkWidget       *widget,
                                                     GtkStyle        *previous);
static void     geda_combo_box_button_toggled       (GtkWidget       *widget,
                                                     void            *data);
static void     geda_combo_box_button_state_changed (GtkWidget       *widget,
                                                     GtkStateType     previous,
                                                     void            *data);
static void     geda_combo_box_add                  (GtkContainer    *container,
                                                     GtkWidget       *widget);
static void     geda_combo_box_remove               (GtkContainer    *container,
                                                     GtkWidget       *widget);

static ComboCellInfo *geda_combo_box_get_cell_info  (GedaComboBox    *combo_box,
                                                     GtkCellRenderer *cell);

static void     geda_combo_box_menu_show            (GtkWidget       *menu,
                                                     void            *user_data);
static void     geda_combo_box_menu_hide            (GtkWidget       *menu,
                                                     void            *user_data);

static void     geda_combo_box_set_popup_widget     (GedaComboBox    *combo_box,
                                                     GtkWidget       *popup);
static void     geda_combo_box_menu_position_below  (GtkWidget       *menu,
                                                     int             *x,
                                                     int             *y,
                                                     int             *push_in,
                                                     GtkWidget       *combo_box);
static void     geda_combo_box_menu_position_over   (GtkWidget       *menu,
                                                     int             *x,
                                                     int             *y,
                                                     int             *push_in,
                                                     GtkWidget       *combo_box);
static void     geda_combo_box_menu_position        (GedaMenu        *menu,
                                                     int             *x,
                                                     int             *y,
                                                     int             *push_in,
                                                     GtkWidget       *combo_box);

static int      geda_combo_box_calc_requested_width (GedaComboBox    *combo_box,
                                                     GtkTreePath     *path);
static void     geda_combo_box_remeasure            (GedaComboBox    *combo_box);

static void     geda_combo_box_unset_model          (GedaComboBox    *combo_box);

static void     geda_combo_box_size_request         (GtkWidget       *widget,
                                                     GtkRequisition  *requisition);
static void     geda_combo_box_size_allocate        (GtkWidget       *widget,
                                                     GtkAllocation   *allocation);
static void     geda_combo_box_forall               (GtkContainer    *container,
                                                     bool             include_internals,
                                                     GtkCallback      callback,
                                                     void            *callback_data);
static bool     geda_combo_box_expose_event         (GtkWidget       *widget,
                                                     GdkEventExpose  *event);
static bool     geda_combo_box_scroll_event         (GtkWidget       *widget,
                                                     GdkEventScroll  *event);
static void     geda_combo_box_set_active_internal  (GedaComboBox    *combo_box,
                                                     GtkTreePath     *path);

static void     geda_combo_box_check_appearance     (GedaComboBox    *combo_box);
static char    *geda_combo_box_real_get_active_text (GedaComboBox    *combo_box);
static void     geda_combo_box_real_move_active     (GedaComboBox    *combo_box,
                                                     GtkScrollType    scroll);
static void     geda_combo_box_real_popup           (GedaComboBox    *combo_box);
static bool     geda_combo_box_real_popdown         (GedaComboBox    *combo_box);

/* listening to the model */
static void     geda_combo_box_model_row_inserted   (GtkTreeModel    *model,
                                                     GtkTreePath     *path,
                                                     GtkTreeIter     *iter,
                                                     void            *user_data);
static void     geda_combo_box_model_row_deleted    (GtkTreeModel    *model,
                                                     GtkTreePath     *path,
                                                     void            *user_data);
static void     geda_combo_box_model_rows_reordered (GtkTreeModel    *model,
                                                     GtkTreePath     *path,
                                                     GtkTreeIter     *iter,
                                                     int             *new_order,
                                                     void            *user_data);
static void     geda_combo_box_model_row_changed    (GtkTreeModel    *model,
                                                     GtkTreePath     *path,
                                                     GtkTreeIter     *iter,
                                                     void            *data);
static void     geda_combo_box_model_row_expanded   (GtkTreeModel    *model,
                                                     GtkTreePath     *path,
                                                     GtkTreeIter     *iter,
                                                     void            *data);

/* list */
static void     geda_combo_box_list_position        (GedaComboBox    *combo_box,
                                                     int             *x,
                                                     int             *y,
                                                     int             *width,
                                                     int             *height);
static void     geda_combo_box_list_setup           (GedaComboBox    *combo_box);
static void     geda_combo_box_list_destroy         (GedaComboBox    *combo_box);

static bool     geda_combo_box_list_button_released (GtkWidget        *widget,
                                                     GdkEventButton   *event,
                                                     void             *data);
static bool     geda_combo_box_list_key_press       (GtkWidget        *widget,
                                                     GdkEventKey      *event,
                                                     void             *data);
static bool     geda_combo_box_list_enter_notify    (GtkWidget        *widget,
                                                     GdkEventCrossing *event,
                                                     void             *data);
static void     geda_combo_box_list_auto_scroll     (GedaComboBox     *combo,
                                                     int               x,
                                                     int               y);
static bool     geda_combo_box_list_scroll_timeout  (GedaComboBox     *combo);
static bool     geda_combo_box_list_button_pressed  (GtkWidget        *widget,
                                                     GdkEventButton   *event,
                                                     void             *data);

static bool     geda_combo_box_list_select_func     (GtkTreeSelection *selection,
                                                     GtkTreeModel     *model,
                                                     GtkTreePath      *path,
                                                     bool              path_currently_selected,
                                                     void             *data);

static void     geda_combo_box_list_row_changed     (GtkTreeModel     *model,
                                                     GtkTreePath      *path,
                                                     GtkTreeIter      *iter,
                                                     void             *data);
static void     geda_combo_box_list_popup_resize    (GedaComboBox     *combo_box);

/* menu */
static void     geda_combo_box_menu_setup           (GedaComboBox     *combo_box,
                                                     bool              add_children);
static void     geda_combo_box_menu_fill            (GedaComboBox     *combo_box);
static void     geda_combo_box_menu_fill_level      (GedaComboBox     *combo_box,
                                                     GtkWidget        *menu,
                                                     GtkTreeIter      *iter);
static void     geda_combo_box_update_title         (GedaComboBox     *combo_box);
static void     geda_combo_box_menu_destroy         (GedaComboBox     *combo_box);

static void     geda_combo_box_relayout_item        (GedaComboBox     *combo_box,
                                                     GtkWidget        *item,
                                                     GtkTreeIter      *iter,
                                                     GtkWidget        *last);
static void     geda_combo_box_relayout             (GedaComboBox     *combo_box);

static bool     geda_combo_box_menu_button_press    (GtkWidget        *widget,
                                                     GdkEventButton   *event,
                                                     void             *user_data);
static void     geda_combo_box_menu_item_activate   (GtkWidget        *item,
                                                     void             *user_data);

static void     geda_combo_box_update_sensitivity   (GedaComboBox     *combo_box);
static void     geda_combo_box_menu_row_inserted    (GtkTreeModel     *model,
                                                     GtkTreePath      *path,
                                                     GtkTreeIter      *iter,
                                                     void             *user_data);
static void     geda_combo_box_menu_row_deleted     (GtkTreeModel     *model,
                                                     GtkTreePath      *path,
                                                     void             *user_data);
static void     geda_combo_box_menu_rows_reordered  (GtkTreeModel     *model,
                                                     GtkTreePath      *path,
                                                     GtkTreeIter      *iter,
                                                     int              *new_order,
                                                     void             *user_data);
static bool     geda_combo_box_menu_key_press       (GtkWidget        *widget,
                                                     GdkEventKey      *event,
                                                     void             *data);
static void     geda_combo_box_menu_popup           (GedaComboBox     *combo_box,
                                                     unsigned int      button,
                                                     unsigned int      activate_time);
static GtkWidget *_cell_view_menu_item_new          (GedaComboBox     *combo_box,
                                                     GtkTreeModel     *model,
                                                     GtkTreeIter      *iter);

/* cell layout */
static void     geda_combo_box_cell_layout_pack_start         (GtkCellLayout         *layout,
                                                               GtkCellRenderer       *cell,
                                                               bool                   expand);
static void     geda_combo_box_cell_layout_pack_end           (GtkCellLayout         *layout,
                                                               GtkCellRenderer       *cell,
                                                               bool                   expand);
static GList   *geda_combo_box_cell_layout_get_cells          (GtkCellLayout         *layout);
static void     geda_combo_box_cell_layout_clear              (GtkCellLayout         *layout);
static void     geda_combo_box_cell_layout_add_attribute      (GtkCellLayout         *layout,
                                                               GtkCellRenderer       *cell,
                                                               const char            *attribute,
                                                               int                   column);
static void     geda_combo_box_cell_layout_set_cell_data_func (GtkCellLayout         *layout,
                                                               GtkCellRenderer       *cell,
                                                               GtkCellLayoutDataFunc  func,
                                                               void                  *func_data,
                                                               GDestroyNotify         destroy);
static void     geda_combo_box_cell_layout_clear_attributes   (GtkCellLayout         *layout,
                                                               GtkCellRenderer       *cell);
static void     geda_combo_box_cell_layout_reorder            (GtkCellLayout         *layout,
                                                               GtkCellRenderer       *cell,
                                                               int                    position);
static bool     geda_combo_box_mnemonic_activate              (GtkWidget             *widget,
                                                               bool                   group_cycling);

static void     geda_combo_box_sync_cells                     (GedaComboBox    *combo_box,
                                                               GtkCellLayout   *cell_layout);
static void     combo_cell_data_func                          (GtkCellLayout   *cell_layout,
                                                               GtkCellRenderer *cell,
                                                               GtkTreeModel    *tree_model,
                                                               GtkTreeIter     *iter,
                                                               void            *data);
static void     geda_combo_box_child_show                     (GtkWidget       *widget,
                                                               GedaComboBox    *combo_box);
static void     geda_combo_box_child_hide                     (GtkWidget       *widget,
                                                               GedaComboBox    *combo_box);

/* GedaComboBox:has-entry callbacks */
static void     geda_combo_box_entry_contents_changed         (GedaEntry       *entry,
                                                               void            *user_data);

static void     geda_combo_box_entry_active_changed           (GedaComboBox    *combo_box,
                                                               void            *user_data);


/* GtkBuildable method implementation */
static void     geda_combo_box_buildable_init                 (GtkBuildableIface *iface);

static bool     geda_combo_box_buildable_custom_tag_start     (GtkBuildable  *buildable,
                                                               GtkBuilder    *builder,
                                                               GObject       *child,
                                                               const char    *tagname,
                                                               GMarkupParser *parser,
                                                               void         **data);
static void     geda_combo_box_buildable_custom_tag_end       (GtkBuildable  *buildable,
                                                               GtkBuilder    *builder,
                                                               GObject       *child,
                                                               const char    *tagname,
                                                               void         **data);
static GObject *geda_combo_box_buildable_get_internal_child   (GtkBuildable  *buildable,
                                                               GtkBuilder    *builder,
                                                               const char    *childname);


/* GtkCellEditable method implementations */
static void     geda_combo_box_start_editing                  (GtkCellEditable *cell_editable,
                                                               GdkEvent        *event);

static void *geda_combo_box_parent_class = NULL;

static GHashTable *combo_box_hash = NULL;

static GtkBuildableIface *parent_buildable_iface;

/*! \internal Default GedaComboBoxClass->changed */
static void geda_combo_box_on_changed(GedaComboBox *combo_box)
{

}

/*! \internal Default GedaComboBoxClass->view_changed */
static void geda_combo_box_on_view_changed(GedaComboBox *combo_box, unsigned int mode)
{

}

/*! \internal Default GedaComboBoxClass->format_entry_text */
static char *geda_combo_box_real_get_active_text (GedaComboBox *combo_box)
{
  char *text = NULL;

  if (combo_box->priv->has_entry) {

    GtkWidget *child = geda_get_child_widget (combo_box);

    if (child) {
      text = geda_strdup (geda_entry_widget_get_text (child));
    }
  }
  else {

    GtkTreeIter iter;

    g_return_val_if_fail (GTK_IS_LIST_STORE (combo_box->priv->model), NULL);
    g_return_val_if_fail (gtk_tree_model_get_column_type (combo_box->priv->model, 0)
                          == G_TYPE_STRING, NULL);

    if (geda_combo_box_get_active_iter (combo_box, &iter)) {
      gtk_tree_model_get (combo_box->priv->model, &iter, 0, &text, -1);
    }
  }

  return text;
}

/*! \internal Default GedaComboBoxClass->get_active_text */
static char *geda_combo_box_format_entry_text (GedaComboBox *combo_box, const char *path)
{
  GedaComboBoxData *priv = combo_box->priv;

  char *text = NULL;

  if (priv->text_column >= 0) {

    GtkTreeIter iter;

    GtkTreeModel *model = geda_combo_box_get_model (combo_box);

    gtk_tree_model_get_iter_from_string (model, &iter, path);

    gtk_tree_model_get (model, &iter, priv->text_column, &text, -1);
  }

  return text;
}

/*! \internal container_class->add */
static void geda_combo_box_add (GtkContainer *container, GtkWidget *widget)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (container);
  GedaComboBoxData *priv  = combo_box->priv;

  if (priv->has_entry && !GEDA_IS_ENTRY (widget)) {
    fprintf(stderr, "Attempting to add a %s widget", G_OBJECT_TYPE_NAME (widget));
    fprintf(stderr, " to a GedaComboBox that does not have an instance of");
    fprintf(stderr, " GedaEntry or derivative subclass");
    return;
  }

  if (priv->cell_view && gtk_widget_get_parent (priv->cell_view)) {
    gtk_widget_unparent (priv->cell_view);
    geda_set_bin_child (container, NULL);
    gtk_widget_queue_resize ((GtkWidget*)container);
  }

  gtk_widget_set_parent (widget, (GtkWidget*)container);
  geda_set_bin_child (container, widget);

  if (priv->cell_view && widget != priv->cell_view) {

    /* since the cell_view was unparented, it's gone now */
    priv->cell_view = NULL;

    if (!priv->tree_view && priv->separator) {

      GtkWidget *parent = geda_get_widget_parent(priv->separator);

      geda_container_remove (parent, priv->separator);

      priv->separator = NULL;

      gtk_widget_queue_resize ((GtkWidget*)container);
    }
    else if (priv->cell_view_frame) {

      gtk_widget_unparent (priv->cell_view_frame);

      priv->cell_view_frame = NULL;

      priv->box = NULL;
    }
  }

  if (priv->has_entry) {

      /* this flag is a hack to tell the entry to fill its allocation. */
      //((GtkEntry*)widget)->is_cell_renderer = TRUE;

      g_signal_connect (widget, "changed",
                        G_CALLBACK (geda_combo_box_entry_contents_changed),
                        combo_box);

      gtk_entry_set_has_frame ((GtkEntry*)widget, priv->has_frame);
    }
}

/*! \internal container_class->forall */
static void geda_combo_box_forall (GtkContainer *container,
                                           bool  include_internals,
                                    GtkCallback  callback,
                                           void *callback_data)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (container);
  GedaComboBoxData *priv      = combo_box->priv;
  GtkWidget        *child;

  if (include_internals) {

    if (priv->button) {
      (* callback) (priv->button, callback_data);
    }

    if (priv->cell_view_frame) {
      (* callback) (priv->cell_view_frame, callback_data);
    }
  }

  child = geda_get_child_widget(container);

  if (child) {
    (* callback) (child, callback_data);
  }
}

/*! \internal container_class->remove */
static void geda_combo_box_remove (GtkContainer *container, GtkWidget *widget)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (container);
  GedaComboBoxData *priv  = combo_box->priv;

  bool appears_as_list;

  if (priv->has_entry) {

    GtkWidget *child_widget;

    child_widget = geda_get_child_widget (container);

    if (widget && widget == child_widget) {

      g_signal_handlers_disconnect_by_func (widget,
                                            geda_combo_box_entry_contents_changed,
                                            container);
    }
  }

  if (widget == priv->cell_view) {
    priv->cell_view = NULL;
  }

  gtk_widget_unparent (widget);
  geda_set_bin_child (container, NULL);

  if (geda_get_widget_in_destruction(combo_box)) {
    return;
  }

  gtk_widget_queue_resize (GTK_WIDGET (container));

  if (!priv->tree_view) {
    appears_as_list = FALSE;
  }
  else {
    appears_as_list = TRUE;
  }

  if (appears_as_list) {
    geda_combo_box_list_destroy (combo_box);
  }
  else if (GEDA_IS_MENU (priv->popup_widget)) {
    geda_combo_box_menu_destroy (combo_box);
    geda_menu_detach (GEDA_MENU (priv->popup_widget));
    priv->popup_widget = NULL;
  }

  if (!priv->cell_view) {
    priv->cell_view = gtk_cell_view_new ();
    gtk_widget_set_parent (priv->cell_view, GTK_WIDGET (container));
    geda_set_bin_child (container, priv->cell_view);

    gtk_widget_show (priv->cell_view);
    gtk_cell_view_set_model ((GtkCellView*)priv->cell_view, priv->model);
    geda_combo_box_sync_cells (combo_box, (GtkCellLayout*)priv->cell_view);
  }

  if (appears_as_list) {
    geda_combo_box_list_setup (combo_box);
  }
  else {
    geda_combo_box_menu_setup (combo_box, TRUE);
  }

  if (gtk_tree_row_reference_valid (priv->active_row)) {

    GtkTreePath *path;

    path = gtk_tree_row_reference_get_path (priv->active_row);

    geda_combo_box_set_active_internal (combo_box, path);

    gtk_tree_path_free (path);
  }
  else {
    geda_combo_box_set_active_internal (combo_box, NULL);
  }
}

/*! \internal widget_class->expose_event */
static bool geda_combo_box_expose_event (GtkWidget      *widget,
                                         GdkEventExpose *event)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (widget);
  GedaComboBoxData *priv      = combo_box->priv;
  GtkWidget        *child;

  if (gtk_widget_is_drawable(widget) && GTK_SHADOW_NONE != priv->shadow_type)
  {
    GtkAllocation  *allocation;

    allocation = geda_get_widget_allocation (widget);

    gtk_paint_shadow (geda_get_widget_style(widget),
                      geda_get_widget_window(widget),
                      GTK_STATE_NORMAL, priv->shadow_type,
                      NULL, widget, "combobox",
                      allocation->x, allocation->y,
                      allocation->width, allocation->height);
  }

  gtk_container_propagate_expose ((GtkContainer*)widget, priv->button, event);

  if (priv->tree_view && priv->cell_view_frame) {

    gtk_container_propagate_expose ((GtkContainer*)widget,
                                     priv->cell_view_frame, event);
  }

  child = geda_get_child_widget(widget);

  gtk_container_propagate_expose ((GtkContainer*)widget, child, event);

  return FALSE;
}

/*! \internal widget_class->grab_focus */
static void geda_combo_box_grab_focus (GtkWidget *widget)
{
  GedaComboBox *combo_box = (GedaComboBox*)widget;

  if (combo_box->priv->has_entry) {

    GtkWidget *child;

    child = geda_get_child_widget (combo_box);

    if (child) {
      gtk_widget_grab_focus (child);
    }
  }
  else {
    gtk_widget_grab_focus (combo_box->priv->button);
  }
}

/*! \internal widget_class->mnemonic_activate */
static bool geda_combo_box_mnemonic_activate (GtkWidget *widget, bool group_cycling)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (widget);

  if (combo_box->priv->has_entry) {

    GtkWidget* child;

    child = geda_get_child_widget (combo_box);

    if (child) {
      gtk_widget_grab_focus (child);
    }
  }
  else {
    gtk_widget_grab_focus (combo_box->priv->button);
  }

  return TRUE;
}

//scroll event
typedef struct {
  GedaComboBox *combo;
  GtkTreePath *path;
  GtkTreeIter iter;
  bool found;
  bool set;
  bool visible;
} SearchData;

static bool tree_column_row_is_sensitive (GedaComboBox *combo_box, GtkTreeIter *iter)
{
  GedaComboBoxData *priv = combo_box->priv;
  GList *cells, *list;
  bool sensitive;

  if (!priv->column)
    return TRUE;

  if (priv->row_separator_func) {
    if (priv->row_separator_func (priv->model, iter,
      priv->row_separator_data))
      return FALSE;
  }

  gtk_tree_view_column_cell_set_cell_data (priv->column,
                                           priv->model,
                                           iter, FALSE, FALSE);

  cells = gtk_cell_layout_get_cells ((GtkCellLayout*)priv->column);

  sensitive = FALSE;

  for (list = cells; list; list = list->next) {

    g_object_get (list->data, "sensitive", &sensitive, NULL);

    if (sensitive) {
      break;
    }
  }

  g_list_free (cells);

  return sensitive;
}

static bool path_visible (GtkTreeView *view, GtkTreePath *path)
{
  return TRUE;
}

static bool tree_next_func (GtkTreeModel *model,
                            GtkTreePath  *path,
                            GtkTreeIter  *iter,
                            void         *data)
{
  SearchData *search_data = (SearchData*)data;

  if (search_data->found) {

    if (!tree_column_row_is_sensitive (search_data->combo, iter)) {
      return FALSE;
    }

    if (search_data->visible &&
      !path_visible (GTK_TREE_VIEW (search_data->combo->priv->tree_view), path))
    {
      return FALSE;
    }

    search_data->set  = TRUE;
    search_data->iter = *iter;

    return TRUE;
  }

  if (gtk_tree_path_compare (path, search_data->path) == 0) {
    search_data->found = TRUE;
  }

  return FALSE;
}

static bool tree_next (GedaComboBox  *combo,
                       GtkTreeModel  *model,
                       GtkTreeIter   *iter,
                       GtkTreeIter   *next,
                       bool           visible)
{
  SearchData search_data;

  search_data.combo   = combo;
  search_data.path    = gtk_tree_model_get_path (model, iter);
  search_data.visible = visible;
  search_data.found   = FALSE;
  search_data.set     = FALSE;

  gtk_tree_model_foreach (model, tree_next_func, &search_data);

  *next = search_data.iter;

  gtk_tree_path_free (search_data.path);

  return search_data.set;
}

static bool tree_prev_func (GtkTreeModel *model,
                            GtkTreePath  *path,
                            GtkTreeIter  *iter,
                            void         *data)
{
  SearchData *search_data = (SearchData *)data;

  if (gtk_tree_path_compare (path, search_data->path) == 0) {
    search_data->found = TRUE;
    return TRUE;
  }

  if (!tree_column_row_is_sensitive (search_data->combo, iter)) {
    return FALSE;
  }

  if (search_data->visible &&
    !path_visible (GTK_TREE_VIEW (search_data->combo->priv->tree_view), path))
  {
    return FALSE;
  }

  search_data->set  = TRUE;
  search_data->iter = *iter;

  return FALSE;
}

static bool tree_prev (GedaComboBox *combo,
                       GtkTreeModel *model,
                       GtkTreeIter  *iter,
                       GtkTreeIter  *prev,
                       bool          visible)
{
  SearchData search_data;

  search_data.combo   = combo;
  search_data.path    = gtk_tree_model_get_path (model, iter);
  search_data.visible = visible;
  search_data.found   = FALSE;
  search_data.set     = FALSE;

  gtk_tree_model_foreach (model, tree_prev_func, &search_data);

  *prev = search_data.iter;

  gtk_tree_path_free (search_data.path);

  return search_data.set;
}

static bool tree_last_func (GtkTreeModel *model,
                            GtkTreePath  *path,
                            GtkTreeIter  *iter,
                            void         *data)
{
  SearchData *search_data = (SearchData *)data;

  if (!tree_column_row_is_sensitive (search_data->combo, iter)) {
    return FALSE;
  }

  /* Note that we rely on the fact that collapsed rows do not have nodes */
  if (search_data->visible &&
      !path_visible (GTK_TREE_VIEW (search_data->combo->priv->tree_view), path))
  {
    return FALSE;
  }

  search_data->set  = TRUE;
  search_data->iter = *iter;

  return FALSE;
}

static bool tree_last (GedaComboBox  *combo,
                       GtkTreeModel  *model,
                       GtkTreeIter   *last,
                       bool           visible)
{
  SearchData search_data;

  search_data.combo = combo;
  search_data.visible = visible;
  search_data.set = FALSE;

  gtk_tree_model_foreach (model, tree_last_func, &search_data);

  *last = search_data.iter;

  return search_data.set;
}


static bool tree_first_func (GtkTreeModel *model,
                             GtkTreePath  *path,
                             GtkTreeIter  *iter,
                             void         *data)
{
  SearchData *search_data = (SearchData *)data;

  if (!tree_column_row_is_sensitive (search_data->combo, iter))
    return FALSE;

  if (search_data->visible &&
      !path_visible ((GtkTreeView*)search_data->combo->priv->tree_view, path))
    return FALSE;

  search_data->set = TRUE;
  search_data->iter = *iter;

  return TRUE;
}

static bool tree_first (GedaComboBox *combo,
                        GtkTreeModel *model,
                        GtkTreeIter  *first,
                        bool          visible)
{
  SearchData search_data;

  search_data.combo   = combo;
  search_data.visible = visible;
  search_data.set     = FALSE;

  gtk_tree_model_foreach (model, tree_first_func, &search_data);

  *first = search_data.iter;

  return search_data.set;
}

/*! \internal widget_class->scroll_event */
static bool geda_combo_box_scroll_event (GtkWidget      *widget,
                                         GdkEventScroll *event)
{
  GedaComboBox *combo_box = (GedaComboBox*)widget;
  GtkTreeIter   iter;

  if (geda_combo_box_get_active_iter (combo_box, &iter)) {

    GtkTreeIter new_iter;
    bool found;

    if (event->direction == GDK_SCROLL_UP) {
      found = tree_prev (combo_box, combo_box->priv->model,
                         &iter, &new_iter, FALSE);
    }
    else {
      found = tree_next (combo_box, combo_box->priv->model,
                         &iter, &new_iter, FALSE);
    }

    if (found) {
      geda_combo_box_set_active_iter (combo_box, &new_iter);
    }
  }
  return TRUE;
}

/*! \internal widget_class->size_allocate */
#define GEDA_COMBO_BOX_SIZE_ALLOCATE_BUTTON                     \
  gtk_widget_size_request (combo_box->priv->button, &req);  \
  if (is_rtl)                                               \
    child_alloc.x = allocation->x + shadow_width;               \
  else                                      \
    child_alloc.x = allocation->x + allocation->width - req.width - shadow_width;   \
                                            \
  child_alloc.y = allocation->y + shadow_height;                    \
  child_alloc.width = req.width;                            \
  child_alloc.height = allocation->height - 2 * shadow_height;  \
  child_alloc.width = MAX (1, child_alloc.width);                   \
  child_alloc.height = MAX (1, child_alloc.height);             \
                                        \
  gtk_widget_size_allocate (combo_box->priv->button, &child_alloc);

static void geda_combo_box_size_allocate (GtkWidget     *widget,
                                          GtkAllocation *allocation)
{
  GedaComboBox     *combo_box  = (GedaComboBox*)widget;
  GedaComboBoxData *priv       = combo_box->priv;
  GtkAllocation     child_alloc;
  GtkRequisition    req;

  int shadow_width;
  int shadow_height;
  int focus_width;
  int focus_pad;
  int border_width;
  int xthickness;
  int ythickness;

  bool is_rtl;

  is_rtl = gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL;

  geda_set_widget_allocation(widget, allocation);

  gtk_widget_style_get ((GtkWidget*)widget,
                        "focus-line-width", &focus_width,
                        "focus-padding", &focus_pad,
                        NULL);

  if (GTK_SHADOW_NONE != priv->shadow_type) {
    shadow_width  = geda_get_widget_style(widget)->xthickness;
    shadow_height = geda_get_widget_style(widget)->ythickness;
  }
  else {
    shadow_width  = 0;
    shadow_height = 0;
  }

  if (!priv->tree_view) {

    if (priv->cell_view) {

      int width;

      /* menu mode */
      allocation->x      += shadow_width;
      allocation->y      += shadow_height;
      allocation->width  -= shadow_width << 1; /* Multiply by 2 */
      allocation->height -= shadow_height << 1;

      gtk_widget_size_allocate (priv->button, allocation);

      /* set some things ready */
      border_width       = geda_get_container_border_width(priv->button);
      xthickness         = geda_get_widget_style(priv->button)->xthickness;
      ythickness         = geda_get_widget_style(priv->button)->ythickness;

      child_alloc.x      = allocation->x;
      child_alloc.y      = allocation->y;
      width              = allocation->width;
      child_alloc.height = allocation->height;

      if (!priv->is_cell_renderer) {

        child_alloc.x      += border_width + xthickness + focus_width + focus_pad;
        child_alloc.y      += border_width + ythickness + focus_width + focus_pad;
        width              -= (child_alloc.x - allocation->x) << 1;
        child_alloc.height -= (child_alloc.y - allocation->y) << 1;
      }

      /* handle the children */
      gtk_widget_size_request (priv->arrow, &req);
      child_alloc.width = req.width;

      if (!is_rtl) {
        child_alloc.x += width - req.width;
      }

      child_alloc.width  = MAX (1, child_alloc.width);
      child_alloc.height = MAX (1, child_alloc.height);
      gtk_widget_size_allocate (priv->arrow, &child_alloc);

      if (is_rtl) {
        child_alloc.x += req.width;
      }

      gtk_widget_size_request (priv->separator, &req);
      child_alloc.width = req.width;

      if (!is_rtl) {
        child_alloc.x -= req.width;
      }

      child_alloc.width  = MAX (1, child_alloc.width);
      child_alloc.height = MAX (1, child_alloc.height);
      gtk_widget_size_allocate (priv->separator, &child_alloc);

      int offset = border_width + xthickness + focus_width + focus_pad;

      if (is_rtl) {
        child_alloc.x    += req.width;
        child_alloc.width = allocation->x + allocation->width - child_alloc.x - offset;
      }
      else {
        child_alloc.width  = child_alloc.x;
        child_alloc.x      = allocation->x + offset;
        child_alloc.width -= child_alloc.x;
      }

      if (gtk_widget_get_visible (priv->popup_widget)) {

        GtkRequisition requisition;

        /* Warning here, without the check in the position func */
        geda_menu_reposition ((GedaMenu*)priv->popup_widget);

        if (priv->wrap_width == 0) {

          GtkAllocation  *combo_allocation;
          int width;

          combo_allocation = geda_get_widget_allocation (combo_box);
          width            = combo_allocation->width;
          gtk_widget_set_size_request (priv->popup_widget, -1, -1);
          gtk_widget_size_request (priv->popup_widget, &requisition);
          gtk_widget_set_size_request (priv->popup_widget,
                                       MAX (width, requisition.width), -1);
        }
      }

      child_alloc.width  = MAX (1, child_alloc.width);
      child_alloc.height = MAX (1, child_alloc.height);
      gtk_widget_size_allocate (geda_get_child_widget (widget), &child_alloc);
    }
    else {

      GEDA_COMBO_BOX_SIZE_ALLOCATE_BUTTON

      if (is_rtl) {
        child_alloc.x = allocation->x + req.width + shadow_width;
      }
      else {
        child_alloc.x = allocation->x + shadow_width;
      }

      child_alloc.y      = allocation->y + shadow_height;
      child_alloc.width  = allocation->width - req.width - 2 * shadow_width;
      child_alloc.width  = MAX (1, child_alloc.width);
      child_alloc.height = MAX (1, child_alloc.height);
      gtk_widget_size_allocate (geda_get_child_widget (widget), &child_alloc);
    }
  }
  else {

    /* list mode */

    border_width = geda_get_container_border_width(widget);

    /* Combobox thickness + border-width */
    int delta_x = shadow_width  + border_width;
    int delta_y = shadow_height + border_width;

    /* button */
    GEDA_COMBO_BOX_SIZE_ALLOCATE_BUTTON

    /* frame */
    if (is_rtl) {
      child_alloc.x = allocation->x + req.width;
    }
    else {
      child_alloc.x = allocation->x;
    }

    child_alloc.y      = allocation->y;
    child_alloc.width  = allocation->width - req.width;
    child_alloc.height = allocation->height;

    if (priv->cell_view_frame) {

      child_alloc.x     += delta_x;
      child_alloc.y     += delta_y;
      child_alloc.width  = MAX (1, child_alloc.width - delta_x * 2);
      child_alloc.height = MAX (1, child_alloc.height - delta_y * 2);
      gtk_widget_size_allocate (priv->cell_view_frame, &child_alloc);

      /* the widget */
      if (priv->has_frame) {

        border_width = geda_get_container_border_width(priv->cell_view_frame);
        xthickness   = geda_get_widget_style(priv->cell_view_frame)->xthickness;
        ythickness   = geda_get_widget_style(priv->cell_view_frame)->ythickness;

        delta_x = border_width + xthickness;
        delta_y = border_width + ythickness;

        child_alloc.x      += delta_x;
        child_alloc.y      += delta_y;
        child_alloc.width  -= delta_x << 1;
        child_alloc.height -= delta_y << 1;
      }
    }
    else {

      child_alloc.x      += delta_x;
      child_alloc.y      += delta_y;
      child_alloc.width  -= delta_x << 1;
      child_alloc.height -= delta_y << 1;
    }

    if (gtk_widget_get_visible (priv->popup_window)) {

      int x, y, width, height;

      geda_combo_box_list_position (combo_box, &x, &y, &width, &height);
      gtk_window_move ((GtkWindow*)priv->popup_window, x, y);
      gtk_widget_set_size_request (priv->popup_window, width, height);
    }


    child_alloc.width  = MAX (1, child_alloc.width);
    child_alloc.height = MAX (1, child_alloc.height);

    gtk_widget_size_allocate (geda_get_child_widget(combo_box), &child_alloc);
  }
}

#undef GEDA_COMBO_BOX_ALLOCATE_BUTTON

/* widget_class->size_request */
static void geda_combo_box_size_request (GtkWidget      *widget,
                                         GtkRequisition *requisition)
{
  GedaComboBox         *combo_box;
  GedaComboBoxData     *priv;
  PangoContext         *context;
  PangoFontMetrics     *metrics;
  PangoFontDescription *font_desc;
  GtkRequisition        bin_req;
  GtkWidget            *child;

  unsigned int focus_width, focus_pad;
  unsigned int font_size;
  unsigned int arrow_size;
  unsigned int xthickness, ythickness;

  combo_box = (GedaComboBox*)widget;
  priv      = combo_box->priv;
  child     = geda_get_child_widget(widget);

  /* common */
  gtk_widget_size_request (child, &bin_req);

  geda_combo_box_remeasure (combo_box);

  bin_req.width  = MAX (bin_req.width,  priv->width);
  bin_req.height = MAX (bin_req.height, priv->height);

  gtk_widget_style_get ((GtkWidget*)widget,
                        "focus-line-width", &focus_width,
                        "focus-padding", &focus_pad,
                        "arrow-size", &arrow_size,
                        NULL);

  font_desc = geda_get_widget_style(child)->font_desc;
  context   = gtk_widget_get_pango_context (widget);
  metrics   = pango_context_get_metrics (context, font_desc,
                                         pango_context_get_language (context));

  font_size = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
  pango_font_metrics_get_descent (metrics));
  pango_font_metrics_unref (metrics);

  arrow_size = MAX (arrow_size, font_size);

  gtk_widget_set_size_request (priv->arrow, arrow_size, arrow_size);

  if (!priv->tree_view) {    /* if menu mode */

    if (priv->cell_view) {

      int width, height;

      GtkRequisition button_req, sep_req, arrow_req;
      int border_width;

      gtk_widget_size_request (priv->button, &button_req);

      border_width   = geda_get_container_border_width(combo_box);
      xthickness     = geda_get_widget_style(priv->button)->xthickness;
      ythickness     = geda_get_widget_style(priv->button)->ythickness;

      bin_req.width  = MAX (bin_req.width, priv->width);
      bin_req.height = MAX (bin_req.height, priv->height);

      gtk_widget_size_request (priv->separator, &sep_req);
      gtk_widget_size_request (priv->arrow, &arrow_req);

      height  = MAX (sep_req.height, arrow_req.height);
      height  = MAX (height, bin_req.height);

      width   = bin_req.width + sep_req.width + arrow_req.width;

      height += (border_width + ythickness + focus_width + focus_pad) << 1;
      width  += (border_width + xthickness + focus_width + focus_pad) << 1;

      requisition->width = width;
      requisition->height = height;
    }
    else {

      GtkRequisition but_req;

      gtk_widget_size_request (priv->button, &but_req);

      requisition->width  = bin_req.width + but_req.width;
      requisition->height = MAX (bin_req.height, but_req.height);
    }
  }
  else {  /* else list mode */

    GtkRequisition button_req, frame_req;

    /* widget + frame */
    *requisition = bin_req;

    requisition->width += focus_width << 1;

    if (priv->cell_view_frame) {

      gtk_widget_size_request (priv->cell_view_frame, &frame_req);

      if (priv->has_frame){

        GtkWidget    *cell  = (GtkWidget*)priv->cell_view_frame;
        GtkContainer *frame = ((GtkContainer*)priv->cell_view_frame);
        unsigned int  fbw   = geda_get_container_border_width(frame);

        xthickness = geda_get_widget_style(cell)->xthickness;
        ythickness = geda_get_widget_style(cell)->ythickness;

        requisition->width  += (fbw + xthickness) << 1;
        requisition->height += (fbw + ythickness) << 1;
      }
    }

    /* The button */
    gtk_widget_size_request (priv->button, &button_req);

    requisition->height = MAX (requisition->height, button_req.height);
    requisition->width += button_req.width;
  }

  if (GTK_SHADOW_NONE != priv->shadow_type) {
    requisition->height += geda_get_widget_style(widget)->ythickness << 1;
    requisition->width  += geda_get_widget_style(widget)->xthickness << 1;
  }
}

static void geda_combo_box_set_background_color (GtkWidget *widget)
{
  GedaComboBox *combo_box = (GedaComboBox*)widget;
  GedaComboBoxData *priv  = combo_box->priv;

  if (priv->tree_view && priv->cell_view) {

    GdkColor     *color;
    GtkCellView  *cell_view;
    GtkStateType  state;

    cell_view = (GtkCellView*)priv->cell_view;

    state = gtk_widget_get_state (widget);
    color = GEDA_MEM_ALLOC0 (sizeof(GdkColor));

    color->pixel  = geda_get_widget_style(widget)->base[state].pixel;
    color->red    = geda_get_widget_style(widget)->base[state].red;
    color->green  = geda_get_widget_style(widget)->base[state].green;
    color->blue   = geda_get_widget_style(widget)->base[state].blue;

    gtk_cell_view_set_background_color (cell_view, color);
  }
}

/*! \internal widget_class->state_changed */
static void geda_combo_box_state_changed (GtkWidget   *widget,
                                          GtkStateType previous)
{
  if (gtk_widget_get_realized (widget)) {
    geda_combo_box_set_background_color(widget);
  }

  gtk_widget_queue_draw (widget);
}

/*! \internal widget_class->style_set */
static void geda_combo_box_style_set (GtkWidget *widget, GtkStyle *previous)
{
  GedaComboBox     *combo_box = (GedaComboBox*)widget;
  GedaComboBoxData *priv      = combo_box->priv;
  GtkWidget        *child;

  geda_combo_box_check_appearance (combo_box);

  geda_combo_box_set_background_color(widget);

  child = geda_get_child_widget(combo_box);

  if (GEDA_IS_ENTRY (child)) {
    g_object_set (child, "shadow-type",
                  GTK_SHADOW_NONE == priv->shadow_type ?
                  GTK_SHADOW_IN : GTK_SHADOW_NONE, NULL);
  }
}

/*! \internal gtk_object_class->destroy || widget_class->destroy */
static void geda_combo_box_destroy (GtkObject *object)
{
  GedaComboBox *combo_box = (GedaComboBox*)object;

  if (combo_box->priv->popup_idle_id > 0) {
    g_source_remove (combo_box->priv->popup_idle_id);
    combo_box->priv->popup_idle_id = 0;
  }

  if (combo_box->priv->popup_in_progress) {
    geda_combo_box_popdown (combo_box);
    combo_box->priv->popup_in_progress = FALSE;
  }

  if (combo_box->priv->row_separator_destroy) {
    combo_box->priv->row_separator_destroy (combo_box->priv->row_separator_data);
  }

  combo_box->priv->row_separator_func = NULL;
  combo_box->priv->row_separator_data = NULL;
  combo_box->priv->row_separator_destroy = NULL;

#if GTK_MAJOR_VERSION < 3

  ((GtkObjectClass*)geda_combo_box_parent_class)->destroy (object);

#else

  ((GtkWidgetClass*)geda_combo_box_parent_class)->destroy (object);

#endif

  combo_box->priv->cell_view = NULL;
}

/*! \internal object_class->constructor */
static GObject *geda_combo_box_constructor (GType                  type,
                                            unsigned int           n_construct_properties,
                                            GObjectConstructParam *construct_properties)
{
  GObject *object;
  GedaComboBoxData *priv;

  object = ((GObjectClass*)geda_combo_box_parent_class)->
             constructor (type, n_construct_properties, construct_properties);

  priv   = ((GedaComboBox*)object)->priv;

  if (priv->has_entry) {

    GtkWidget *entry;

    entry = geda_entry_new_visible ();

    geda_container_add (object, entry);

    priv->text_renderer = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start ((GtkCellLayout*)object,
                                priv->text_renderer, TRUE);

    geda_combo_box_set_active ((GedaComboBox*)object, -1);

    g_signal_connect (object, "changed",
                      G_CALLBACK (geda_combo_box_entry_active_changed), NULL);
  }

  return object;
}

/*! \internal object_class->dispose */
static void geda_combo_box_dispose(GObject *object)
{
  GedaComboBox *combo_box = (GedaComboBox*)object;

  if (GEDA_IS_MENU (combo_box->priv->popup_widget)) {

    geda_combo_box_menu_destroy (combo_box);
    geda_menu_detach ((GedaMenu*)combo_box->priv->popup_widget);
    combo_box->priv->popup_widget = NULL;
  }

  ((GObjectClass*)geda_combo_box_parent_class)->dispose (object);
}

/*! \internal object_class->finalize */
static void geda_combo_box_finalize (GObject *object)
{
  GedaComboBox *combo_box = (GedaComboBox*)object;
  GSList *iter;

  if (GTK_IS_TREE_VIEW (combo_box->priv->tree_view)) {
    geda_combo_box_list_destroy (combo_box);
  }

  if (combo_box->priv->popup_window) {
    gtk_widget_destroy (combo_box->priv->popup_window);
  }

  geda_combo_box_unset_model (combo_box);

  if (g_hash_table_remove (combo_box_hash, object)) {
    if (!g_hash_table_size (combo_box_hash)) {
      g_hash_table_destroy (combo_box_hash);
      combo_box_hash = NULL;
    }
  }

  for (iter = combo_box->priv->cells; iter; iter = iter->next) {

    ComboCellInfo *info = (ComboCellInfo*)iter->data;
    GSList        *list = info->attributes;

    if (info->destroy) {
      info->destroy (info->func_data);
    }

    while (list && list->next) {
      g_free (list->data);
      list = list->next->next;
    }
    g_slist_free (info->attributes);

    g_object_unref (info->cell);
    g_free (info);
  }
  g_slist_free (combo_box->priv->cells);

  g_free (combo_box->priv->tearoff_title);

  g_free (combo_box->priv);

  ((GObjectClass*)geda_combo_box_parent_class)->finalize (object);
}

/*! \internal object_class->get_property */
static void geda_combo_box_get_property (GObject      *object,
                                         unsigned int  prop_id,
                                         GValue       *value,
                                         GParamSpec   *pspec)
{
  GedaComboBox *combo_box = (GedaComboBox*)object;
  GedaComboBoxData *priv  = combo_box->priv;

  switch (prop_id)
  {
    case PROP_MODEL:
      g_value_set_object (value, combo_box->priv->model);
      break;

    case PROP_WRAP_WIDTH:
      g_value_set_int (value, combo_box->priv->wrap_width);
      break;

    case PROP_ROW_SPAN_COLUMN:
      g_value_set_int (value, combo_box->priv->row_column);
      break;

    case PROP_COLUMN_SPAN_COLUMN:
      g_value_set_int (value, combo_box->priv->col_column);
      break;

    case PROP_ACTIVE:
      g_value_set_int (value, geda_combo_box_get_active (combo_box));
      break;

    case PROP_ADD_TEAROFFS:
      g_value_set_boolean (value, geda_combo_box_get_add_tearoffs (combo_box));
      break;

    case PROP_HAS_FRAME:
      g_value_set_boolean (value, combo_box->priv->has_frame);
      break;

    case PROP_FOCUS_ON_CLICK:
      g_value_set_boolean (value, combo_box->priv->focus_on_click);
      break;

    case PROP_TEAROFF_TITLE:
      g_value_set_string (value, geda_combo_box_get_title (combo_box));
      break;

    case PROP_POPUP_SHOWN:
      g_value_set_boolean (value, combo_box->priv->popup_shown);
      break;

    case PROP_BUTTON_SENSITIVITY:
      g_value_set_enum (value, combo_box->priv->button_sensitivity);
      break;

    case PROP_EDITING_CANCELED:
      g_value_set_boolean (value, priv->editing_canceled);
      break;

    case PROP_HAS_ENTRY:
      g_value_set_boolean (value, priv->has_entry);
      break;

    case PROP_LIST_VIEW:
      g_value_set_boolean (value, priv->list_view);
      break;

    case PROP_ENTRY_TEXT_COLUMN:
      g_value_set_int (value, priv->text_column);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_combo_box_set_list_view(GedaComboBox *combo_box, int mode)
{
  if (mode != combo_box->priv->list_view) {
    combo_box->priv->list_view = mode;
    g_signal_emit (combo_box, combo_box_signals[VIEW_CHANGED], 0, mode);
  }
}

/*! \internal object_class->set_property */
static void geda_combo_box_set_property (GObject      *object,
                                         unsigned int  prop_id,
                                         const GValue *value,
                                         GParamSpec   *pspec)
{
  GedaComboBox *combo_box = (GedaComboBox*)object;

  switch (prop_id)
  {
    case PROP_MODEL:
      geda_combo_box_set_model (combo_box, g_value_get_object (value));
      break;

    case PROP_WRAP_WIDTH:
      geda_combo_box_set_wrap_width (combo_box, g_value_get_int (value));
      break;

    case PROP_ROW_SPAN_COLUMN:
      geda_combo_box_set_row_span_column (combo_box, g_value_get_int (value));
      break;

    case PROP_COLUMN_SPAN_COLUMN:
      geda_combo_box_set_column_span_column (combo_box, g_value_get_int (value));
      break;

    case PROP_ACTIVE:
      geda_combo_box_set_active (combo_box, g_value_get_int (value));
      break;

    case PROP_ADD_TEAROFFS:
      geda_combo_box_set_add_tearoffs (combo_box, g_value_get_boolean (value));
      break;

    case PROP_HAS_FRAME:
      combo_box->priv->has_frame = g_value_get_boolean (value);

      if (combo_box->priv->has_entry) {

        GtkWidget *child;

        child = geda_get_child_widget (combo_box);

        gtk_entry_set_has_frame ((GtkEntry*)child, combo_box->priv->has_frame);
      }
      break;

    case PROP_FOCUS_ON_CLICK:
      geda_combo_box_set_focus_on_click (combo_box,
                                         g_value_get_boolean (value));
      break;

    case PROP_TEAROFF_TITLE:
      geda_combo_box_set_title (combo_box, g_value_get_string (value));
      break;

    case PROP_POPUP_SHOWN:
      if (g_value_get_boolean (value)) {
        geda_combo_box_popup (combo_box);
      }
      else {
        geda_combo_box_popdown (combo_box);
      }
      break;

    case PROP_BUTTON_SENSITIVITY:
      geda_combo_box_set_button_sensitivity (combo_box,
                                             g_value_get_enum (value));
      break;

    case PROP_EDITING_CANCELED:
      combo_box->priv->editing_canceled = g_value_get_boolean (value);
      break;

    case PROP_HAS_ENTRY:
      combo_box->priv->has_entry = g_value_get_boolean (value);
      break;

    case PROP_LIST_VIEW:
      geda_combo_box_set_list_view(combo_box, g_value_get_int (value));
      break;

    case PROP_ENTRY_TEXT_COLUMN:
      geda_combo_box_set_entry_text_column (combo_box, g_value_get_int (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static bool geda_combo_box_buildable_custom_tag_start (GtkBuildable  *buildable,
                                                       GtkBuilder    *builder,
                                                       GObject       *child,
                                                       const char    *tagname,
                                                       GMarkupParser *parser,
                                                       void         **data)
{
  if (parent_buildable_iface->custom_tag_start (buildable, builder, child,
                                                tagname, parser, data))
    return TRUE;
/*
  return _gtk_cell_layout_buildable_custom_tag_start (buildable, builder, child,
                                                      tagname, parser, data);
*/
  return FALSE;
}

static void geda_combo_box_buildable_custom_tag_end (GtkBuildable *buildable,
                                                     GtkBuilder   *builder,
                                                     GObject      *child,
                                                     const char   *tagname,
                                                     void        **data)
{
  parent_buildable_iface->custom_tag_end (buildable, builder, child, tagname, data);
}

static GObject *geda_combo_box_buildable_get_internal_child (GtkBuildable *buildable,
                                                             GtkBuilder   *builder,
                                                             const char   *childname)
{
  GedaComboBox *combo_box = (GedaComboBox*)buildable;
  GObject      *object;

  if (combo_box->priv->has_entry && strcmp (childname, "entry") == 0) {
    object = (GObject*)geda_get_child_widget (buildable);
  }
  else {
    object = parent_buildable_iface->get_internal_child (buildable, builder, childname);
  }
  return object;
}

/*!
 * \brief GedaComboBox Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       GedaComboClass class we are initializing
 * \param [in]  class_data  GedaCombo structure associated with the class
 */
static void geda_combo_box_class_init(void *class, void *class_data)
{
  GedaComboBoxClass *combo_class;
  GObjectClass      *object_class;
  GtkContainerClass *container_class;
  GtkWidgetClass    *widget_class;
  GtkBindingSet     *binding_set;
  GParamSpec        *params;

  combo_class                     = (GedaComboBoxClass*)class;
  combo_class->changed            = geda_combo_box_on_changed;
  combo_class->get_active_text    = geda_combo_box_real_get_active_text;
  combo_class->format_entry_text  = geda_combo_box_format_entry_text;
  combo_class->view_changed       = geda_combo_box_on_view_changed;

  container_class                 = (GtkContainerClass*)class;
  container_class->add            = geda_combo_box_add;
  container_class->forall         = geda_combo_box_forall;
  container_class->remove         = geda_combo_box_remove;

  widget_class                    = (GtkWidgetClass*)class;
  widget_class->expose_event      = geda_combo_box_expose_event;
  widget_class->grab_focus        = geda_combo_box_grab_focus;
  widget_class->mnemonic_activate = geda_combo_box_mnemonic_activate;
  widget_class->scroll_event      = geda_combo_box_scroll_event;
  widget_class->size_allocate     = geda_combo_box_size_allocate;
  widget_class->size_request      = geda_combo_box_size_request;
  widget_class->state_changed     = geda_combo_box_state_changed;
  widget_class->style_set         = geda_combo_box_style_set;

  object_class                    = (GObjectClass*)class;
  object_class->constructor       = geda_combo_box_constructor;
  object_class->dispose           = geda_combo_box_dispose;
  object_class->finalize          = geda_combo_box_finalize;
  object_class->get_property      = geda_combo_box_get_property;
  object_class->set_property      = geda_combo_box_set_property;

#if GTK_MAJOR_VERSION < 3

  GtkObjectClass *gtk_object_class;

  gtk_object_class                = (GtkObjectClass*)class;
  gtk_object_class->destroy       = geda_combo_box_destroy;

#else

  widget_class->destroy           = geda_combo_box_destroy;

#endif

  geda_combo_box_parent_class = g_type_class_peek_parent (class);

  /* signals */

  GedaType type = geda_combo_box_get_type();

  /*!
   * signal "changed": GedaComboBox::changed:
   * \brief emitted when active item is changed.
   * The changed signal is emitted  when the active item is changed.
   * This can be due to the user selecting a different item from the
   * list, or due to a call to geda_combo_box_set_active_iter().
   * The signal is NOT emitted when the user is typing into a
   * GedaComboBoxEntry entry field.
   */
  combo_box_signals[CHANGED] =
      g_signal_new ("changed",
                    type,
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GedaComboBoxClass, changed),
                    NULL, NULL,
                    geda_marshal_VOID__VOID,
                    G_TYPE_NONE, 0);

  /*! signal "move-active": GedaComboBox::move-active
   *  \brief emitted to move the active selection.
   */
  combo_box_signals[MOVE_ACTIVE] =
      g_signal_new_class_handler ("move-active",
                                  type,
                                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                                  G_CALLBACK (geda_combo_box_real_move_active),
                                  NULL, NULL,
                                  geda_marshal_VOID__ENUM,
                                  G_TYPE_NONE, 1,
                                  GTK_TYPE_SCROLL_TYPE);

  /*!
   * signal "popup": GedaComboBox::popup:
   * \brief emitted to popup the combo box list
   * The GedaComboBox::popup signal is emitted to popup the combo box list.
   * The default binding for this signal is Alt+Down.
   */
  combo_box_signals[POPUP] =
      g_signal_new_class_handler ("popup",
                                  type,
                                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                                  G_CALLBACK (geda_combo_box_real_popup),
                                  NULL, NULL,
                                  geda_marshal_VOID__VOID,
                                  G_TYPE_NONE, 0);
  /*!
   * signal "popdown": GedaComboBox::popdown:
   * \brief emitted to popdown the combo box list
   * The GedaComboBox::popdown signal is emitted to popdown the combo box list.
   *
   * The default bindings for this signal are Alt+Up and Escape.
   */
  combo_box_signals[POPDOWN] =
      g_signal_new_class_handler ("popdown",
                                  type,
                                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                                  G_CALLBACK (geda_combo_box_real_popdown),
                                  NULL, NULL,
                                  NULL,
                                  G_TYPE_BOOLEAN, 0);

  /*!
   * signal "format-entry-text":  GedaComboBox::format-entry-text:
   * \brief Allows display of formated entry
   * A signal to change how the text in a combo box's entry is displayed. The
   * signal is only allplicable to combo boxes that are created with an entry
   * (See GedaComboBox:has-entry).
   *
   * Connect a signal handler which returns an allocated string representing
   * path.  That string will then be used to set the text in the combo box's
   * entry. The default signal handler uses the text from the GedaComboBox::
   * entry-text-column model column.
   *
   * Here is an example signal handler which fetches data from the model and
   * displays it in the entry.
   * |[
   * static char*
   * format_entry_text_callback (GedaComboBox *combo,
   *                             const char   *path,
   *                             void         *user_data)
   * {
   *   GtkTreeIter  iter;
   *   GtkTreeModel model;
   *   double       value;
   *
   *   model = geda_combo_box_get_model (combo);
   *
   *   gtk_tree_model_get_iter_from_string (model, &iter, path);
   *   gtk_tree_model_get (model, &iter,
   *                       THE_DOUBLE_VALUE_COLUMN, &value,
   *                       -1);
   *
   *   return geda_sprintf ("&percnt;g", value);
   * }
   * ]|
   *
   * param combo: the object which received the signal
   * param path:  the GtkTreePath string from the combo box's current model
   *              to format text
   *
   * \returns a newly allocated string representing path for the current
   *          GedaComboBox model.
   */
  combo_box_signals[FORMAT_ENTRY_TEXT] =
      g_signal_new ("format-entry-text",
                    type,
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GedaComboBoxClass, format_entry_text),
                    geda_single_string_accumulator, NULL,
                    geda_marshal_STRING__STRING,
                    G_TYPE_STRING, 1, G_TYPE_STRING);

  combo_box_signals[VIEW_CHANGED] =
      g_signal_new ("view-changed",
                    type,
                    G_SIGNAL_RUN_LAST,
                    G_STRUCT_OFFSET (GedaComboBoxClass, view_changed),
                    NULL, NULL,
                    geda_marshal_VOID__UINT,
                    G_TYPE_NONE, 1, G_TYPE_UINT);

  /* key bindings */
  binding_set = gtk_binding_set_by_class (widget_class);

  gtk_binding_entry_add_signal (binding_set, GDK_Down, GDK_MOD1_MASK,
                                "popup", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Down, GDK_MOD1_MASK,
                                "popup", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_Up, GDK_MOD1_MASK,
                                "popdown", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Up, GDK_MOD1_MASK,
                                "popdown", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Escape, 0,
                                "popdown", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_Up, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_STEP_UP);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Up, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_STEP_UP);
  gtk_binding_entry_add_signal (binding_set, GDK_Page_Up, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_PAGE_UP);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Page_Up, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_PAGE_UP);
  gtk_binding_entry_add_signal (binding_set, GDK_Home, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_START);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Home, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_START);

  gtk_binding_entry_add_signal (binding_set, GDK_Down, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_STEP_DOWN);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Down, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_STEP_DOWN);
  gtk_binding_entry_add_signal (binding_set, GDK_Page_Down, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_PAGE_DOWN);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_Page_Down, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_PAGE_DOWN);
  gtk_binding_entry_add_signal (binding_set, GDK_End, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_END);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_End, 0,
                                "move-active", 1,
                                GTK_TYPE_SCROLL_TYPE, GTK_SCROLL_END);

  /* properties */
  g_object_class_override_property (object_class,
                                    PROP_EDITING_CANCELED,
                                    "editing-canceled");

  /*! property "model": GedaComboBox::model
   *  \brief Sets a model for a GedaComboBox.
   *  \par
   * The model from which the combo box takes the values shown
   * in the list.
   */
  params = g_param_spec_object ("model",
                              _("ComboBox model"),
                              _("The model for the combo box"),
                                GTK_TYPE_TREE_MODEL,
                                G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_MODEL, params);

  /*! property "wrap-width": GedaComboBox::wrap-width
   *  \brief Sets the wrap-width for a GedaComboBox.
   *  \par
   * If wrap-width is set to a positive value, the list will be
   * displayed in multiple columns, the number of columns is
   * determined by wrap-width.
   */
  params = g_param_spec_int ("wrap-width",
                           _("Wrap width"),
                           _("Wrap width for laying out the items in a grid"),
                               0,
                               G_MAXINT,
                               0,
                               G_PARAM_READWRITE);
  g_object_class_install_property (object_class, PROP_WRAP_WIDTH, params);

  /*! property "row-span-column": GedaComboBox::row-span-column
   *  \brief Sets the row-span-column for a GedaComboBox.
   *  \par
   * If this is set to a non-negative value, the value must be the index of a
   * column of type %G_TYPE_INT in the model.
   *
   * The values of that column are used to determine how many rows a value in
   * the list will span. Therefore, the values in the model column pointed to
   * by this property must be greater than zero and not larger than wrap-width.
   */
  params = g_param_spec_int ("row-span-column",
                           _("Row span column"),
                           _("TreeModel column containing the row span values"),
                             -1,
                             G_MAXINT,
                             -1,
                             G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_ROW_SPAN_COLUMN, params);

  /*! property "column-span-column": GedaComboBox::column-span-column
   *  \brief Sets the rocolumnw-span-column for a GedaComboBox.
   *  \par
   * If this is set to a non-negative value, the value  must be the index of a
   * column of type G_TYPE_INT in the model.
   *
   * The values of that column are used to determine how many columns a value
   * in the list will span.
   */
  params = g_param_spec_int ("column-span-column",
                           _("Column span column"),
                           _("TreeModel column containing the column span values"),
                             -1,
                             G_MAXINT,
                             -1,
                             G_PARAM_READWRITE);

  g_object_class_install_property (object_class,
                                   PROP_COLUMN_SPAN_COLUMN,
                                   params);

  /*! property "active": GedaComboBox::active
   *  \brief Sets the wrap-width for a GedaComboBox.
   *  \par
   * The item which is currently active. If the model is a non-flat treemodel,
   * and the active item is not an immediate child of the root of the tree,
   * this property has the value <b>gtk_tree_path_get_indices (path)[0]</b>,
   * where <b>path</b> is the GtkTreePath of the active item.
   */
  params = g_param_spec_int ("active",
                           _("Active item"),
                           _("The item which is currently active"),
                             -1,
                             G_MAXINT,
                             -1,
                             G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_ACTIVE, params);

  /*! property "add-tearoffs": GedaComboBox::add-tearoffs
   *  \brief Sets the add-tearoffs for a GedaComboBox.
   *  \par
   * The add-tearoffs property controls whether generated menus
   * have tearoff menu items.
   *
   * \note that this only applicable to menu style combo boxes.
   */
  params = g_param_spec_boolean ("add-tearoffs",
                               _("Add tearoffs to menus"),
                               _("Whether dropdowns should have a tearoff menu item"),
                                 FALSE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_ADD_TEAROFFS, params);

  /*! property "has-frame": GedaComboBox::has-frame
   *  \brief Sets the has-frame for a GedaComboBox.
   *  \par
   * The has-frame property controls whether a frame
   * is drawn around the entry.
   */
  params = g_param_spec_boolean ("has-frame",
                               _("Has Frame"),
                               _("Whether the combo box draws a frame around the child"),
                                 TRUE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_HAS_FRAME, params);

  /*! property "focus-on-click": GedaComboBox::focus-on-click
   *  \brief Sets the focus-on-click for a GedaComboBox.
   *  \par
   * The focus-on-click property controls whether the combo box grabs focus
   * when it is clicked with the mouse.
   */
  params = g_param_spec_boolean ("focus-on-click",
                               _("Focus on click"),
                               _("Whether the combo box grabs focus when it is clicked with the mouse"),
                                 TRUE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (object_class,
                                   PROP_FOCUS_ON_CLICK,
                                   params);

  /*! property "tearoff-title": GedaComboBox::tearoff-title
   *  \brief Sets the tearoff-title for a GedaComboBox.
   *  \par
   * A title that may be displayed by the window manager
   * when the popup is torn-off.
   */
  params = g_param_spec_string ("tearoff-title",
                              _("Tearoff Title"),
                              _("A title that may be displayed by the window manager when the popup is torn-off"),
                                NULL,
                                G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_TEAROFF_TITLE, params);

  /*! property "popup-shown": GedaComboBox::popup-shown
   *  \brief Gets the popup-shown state for a GedaComboBox.
   *  \par
   * Whether the combo boxes dropdown is popped up.
   * Note that this property is mainly useful, because
   * it allows you to connect to notify::popup-shown.
   */
  params = g_param_spec_boolean ("popup-shown",
                               _("Popup shown"),
                               _("Whether the combo's dropdown is shown"),
                                  FALSE,
                                  G_PARAM_READABLE);

  g_object_class_install_property (object_class, PROP_POPUP_SHOWN, params);

  /*! property "button-sensitivity": GedaComboBox::button-sensitivity
   *  \brief Sets or gets the button-sensitivity for a GedaComboBox.
   *  \par
   * Whether the dropdown button is sensitive when
   * the model is empty.
   */
  params = g_param_spec_enum ("button-sensitivity",
                            _("Button Sensitivity"),
                            _("Whether the dropdown button is sensitive when the model is empty"),
                              GTK_TYPE_SENSITIVITY_TYPE,
                              GTK_SENSITIVITY_AUTO,
                              G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_BUTTON_SENSITIVITY, params);

  /*! property "has-entry": GedaComboBox::has-entry
   *  \brief Sets or gets has-entry for a GedaComboBox.
   *  \par
   *  Sets the combo box has an entry.
   */
  params = g_param_spec_boolean ("has-entry",
                                 _("Has Entry"),
                                 _("Whether combo box has an entry"),
                                 FALSE,
                                 G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

  g_object_class_install_property (object_class, PROP_HAS_ENTRY, params);

  /*! property "list-view": GedaComboBox::list-view
   *  \brief Sets or gets list-view for a GedaComboBox.
   *  \par
   * Whether popup should look like lists rather than menus.
   */
  params = g_param_spec_int  ("list-view",
                            _("Appear as list"),
                            _("Whether popup should look like lists rather than menus"),
                              GEDA_VIEW_AUTO,  /* Min */
                              GEDA_VIEW_MENU,  /* Max */
                              GEDA_VIEW_AUTO,  /* Default */
                              G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_LIST_VIEW, params );

  /*! property "entry-text-column": GedaComboBox::entry-text-column
   *  \brief Sets or gets entry-text-column for a GedaComboBox.
   *  \par
   * The column in the combo box's model to associate with strings from the
   * entry if the combo was created with #GedaComboBox:has-entry = %TRUE.
   */
  params = g_param_spec_int ("entry-text-column",
                           _("Entry Text Column"),
                           _("The column in the combo box's model to associate "
                             "with strings from the entry if the combo was "
                             "created with #GedaComboBox:has-entry = %TRUE"),
                             -1, G_MAXINT, -1,
                             G_PARAM_READWRITE);

  g_object_class_install_property (object_class,
                                   PROP_ENTRY_TEXT_COLUMN,
                                   params);

  /*! property "arrow-size": GedaComboBox::arrow-size
   *  \brief Sets or gets entry-text-column for a GedaComboBox.
   *  \par
   * Sets the minimum size of the arrow in the combo box.  Note
   * that the arrow size is coupled to the font size, so in case
   * a larger font is used, the arrow will be larger than set
   * by arrow size.
   */
  params = g_param_spec_int ("arrow-size",
                           _("Arrow Size"),
                           _("The minimum size of the arrow in the combo box"),
                             0, G_MAXINT, 15,
                             G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*! property "appear-as-list": GedaComboBox::appear-as-list
   *  \brief Gets the appear-as-list for a GedaComboBox.
   *  \par
   * Sets whether the widget should appears a menu list or treeview.
   */
  params = g_param_spec_boolean ("appear-as-list",
                               _("List View"),
                               _("When true, the drop-down appears as a list view, otherwise an ugly menus"),
                                 FALSE,
                                 G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*! property "shadow-type": GedaComboBox::shadow-type
   *  \brief Gets theshadow-type for a GedaComboBox.
   *  \par
   * Which kind of shadow to draw around the combo box.
   */
  params = g_param_spec_enum ("shadow-type",
                            _("Shadow type"),
                            _("Which kind of shadow to draw around the combo box"),
                               GTK_TYPE_SHADOW_TYPE,
                               GTK_SHADOW_NONE,
                               G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);
}

static void geda_combo_box_buildable_init (GtkBuildableIface *iface)
{
  parent_buildable_iface     = g_type_interface_peek_parent (iface);

  iface->add_child           = gtk_buildable_add_child;
  iface->custom_tag_start    = geda_combo_box_buildable_custom_tag_start;
  iface->custom_tag_end      = geda_combo_box_buildable_custom_tag_end;
  iface->get_internal_child  = geda_combo_box_buildable_get_internal_child;
}

static void geda_combo_box_cell_layout_init (GtkCellLayoutIface *iface)
{
  iface->pack_start          = geda_combo_box_cell_layout_pack_start;
  iface->pack_end            = geda_combo_box_cell_layout_pack_end;
  iface->get_cells           = geda_combo_box_cell_layout_get_cells;
  iface->clear               = geda_combo_box_cell_layout_clear;
  iface->add_attribute       = geda_combo_box_cell_layout_add_attribute;
  iface->set_cell_data_func  = geda_combo_box_cell_layout_set_cell_data_func;
  iface->clear_attributes    = geda_combo_box_cell_layout_clear_attributes;
  iface->reorder             = geda_combo_box_cell_layout_reorder;
}

static void geda_combo_box_cell_editable_init (GtkCellEditableIface *iface)
{
  iface->start_editing = geda_combo_box_start_editing;
}

/*!
 * \brief Initialize new GedaComboBox data structure instance.
 * \par Function Description
 *  This function is call after the GedaComboBoxClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaComboBox data structure
 * \param [in] class     A GedaComboBoxClass Object
 */
static void geda_combo_box_instance_init(GTypeInstance *instance, void *class)
{
  GedaComboBox     *combo_box;
  GedaComboBoxData *priv;

  combo_box = (GedaComboBox*)instance;

  priv = GEDA_MEM_ALLOC0(sizeof(GedaComboBoxData));

  priv->cell_view = gtk_cell_view_new ();

  gtk_widget_set_parent (priv->cell_view, GTK_WIDGET (combo_box));

  geda_set_bin_child (combo_box, priv->cell_view);

  gtk_widget_show (priv->cell_view);

  priv->width              = 0;
  priv->height             = 0;
  priv->wrap_width         = 0;

  priv->active             = -1;
  priv->active_row         = NULL;
  priv->col_column         = -1;
  priv->row_column         = -1;

  priv->add_tearoffs       = FALSE;
  priv->auto_scroll        = FALSE;
  priv->button_sensitivity = GTK_SENSITIVITY_AUTO;
  priv->editing_canceled   = FALSE;
  priv->focus_on_click     = TRUE;
  priv->has_entry          = FALSE;
  priv->has_frame          = TRUE;
  priv->is_cell_renderer   = FALSE;
  priv->popup_shown        = FALSE;

  priv->text_column        = -1;
  priv->text_renderer      = NULL;

  combo_box->priv = priv;

  if (!combo_box_hash) {
    combo_box_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (combo_box_hash, instance, instance);

  geda_combo_box_check_appearance (combo_box);
}

/*!
 * \brief Retrieve GedaComboBox's Type identifier.
   \par Function Description
 *  Function to retrieve a #GedaComboBox Type identifier. When
 *  first called, the function registers a #GedaComboBox in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaComboBox and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaComboBox.
 */
GedaType geda_combo_box_get_type (void)
{
  static volatile GedaType geda_combo_box_type = 0;

  if (g_once_init_enter (&geda_combo_box_type)) {

    static const GTypeInfo info = {
      sizeof(GedaComboBoxClass),
      NULL,                      /* base_init           */
      NULL,                      /* base_finalize       */
      geda_combo_box_class_init, /* (GClassInitFunc)    */
      NULL,                      /* class_finalize      */
      NULL,                      /* class_data          */
      sizeof(GedaComboBox),
      0,                         /* n_preallocs         */
      geda_combo_box_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaComboBox");
    type   = g_type_register_static (GTK_TYPE_BIN, string, &info, 0);

    const GInterfaceInfo layout_info = {
      (GInterfaceInitFunc) geda_combo_box_cell_layout_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_CELL_LAYOUT, &layout_info);

    const GInterfaceInfo editable_info = {
      (GInterfaceInitFunc) geda_combo_box_cell_editable_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_CELL_EDITABLE, &editable_info);

    const GInterfaceInfo buildable_info = {
      (GInterfaceInitFunc) geda_combo_box_buildable_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_BUILDABLE, &buildable_info);

    g_once_init_leave (&geda_combo_box_type, type);
  }

  return geda_combo_box_type;
}

/*!
 * \brief Check if an object is a GedaComboBox
 * \par Function Description
 *  Determines if \a combo_box is valid by verifying \a combo_box
 *  is included in the hash table of GedaComboBox objects.
 *
 * \return TRUE if \a combo_box is a valid GedaComboBox
 */
bool is_a_geda_combo_box (GedaComboBox *combo_box)
{
  if ((combo_box != NULL) && (combo_box_hash != NULL)) {
    return g_hash_table_lookup(combo_box_hash, combo_box) ? TRUE : FALSE;
  }
  return FALSE;
}

static void geda_combo_box_check_appearance (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  unsigned int as_list;

  if (priv->list_view == GEDA_VIEW_AUTO) {

    /* Retrieve widget style property */

    gtk_widget_style_get (GTK_WIDGET (combo_box),
                          "appear-as-list", &as_list,
                          NULL);
  }
  else if (priv->list_view == GEDA_VIEW_TREE) {
    as_list = TRUE;
  }
  else {
    as_list = FALSE;
  }

  if (priv->as_list != as_list) {
    if (!priv->as_list && as_list) {
      priv->as_list |= 1 ;
    }
    else if (priv->as_list && !as_list) {
      priv->as_list &= ~1;
    }
  }

  if (priv->as_list) {

    /* Destroy all the menu mode widgets, if they exist. */
    if (GEDA_IS_MENU (priv->popup_widget)){
      geda_combo_box_menu_destroy (combo_box);
    }

    /* Create the list mode widgets, if they do not already exist. */
    if (!GTK_IS_TREE_VIEW (priv->tree_view)) {
      geda_combo_box_list_setup (combo_box);
    }
  }
  else {

    /* Destroy all the list mode widgets, if they exist. */
    if (GTK_IS_TREE_VIEW (priv->tree_view)) {
      geda_combo_box_list_destroy (combo_box);
    }

    /* Create the menu mode widgets, if they do not already exist. */
    if (!GEDA_IS_MENU (priv->popup_widget)) {
      geda_combo_box_menu_setup (combo_box, TRUE);
    }
  }

  gtk_widget_style_get (GTK_WIDGET (combo_box),
                        "shadow-type", &priv->shadow_type,
                        NULL);
}

static void geda_combo_box_button_toggled (GtkWidget *widget, void *data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))) {

    if (!combo_box->priv->popup_in_progress) {
      geda_combo_box_popup (combo_box);
    }
  }
  else {
    geda_combo_box_popdown (combo_box);
  }
}

static ComboCellInfo *geda_combo_box_get_cell_info (GedaComboBox    *combo_box,
                                                    GtkCellRenderer *cell)
{
  GSList *i;

  for (i = combo_box->priv->cells; i; i = i->next) {

    ComboCellInfo *info = (ComboCellInfo *)i->data;

    if (info && info->cell == cell) {
      return info;
    }
  }

  return NULL;
}

static void
geda_combo_box_menu_show (GtkWidget *menu, void *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;

  geda_combo_box_child_show (menu, user_data);

  priv->popup_in_progress = TRUE;

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->button), TRUE);

  priv->popup_in_progress = FALSE;
}

static void geda_combo_box_menu_hide (GtkWidget *menu, void *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);

  geda_combo_box_child_hide (menu,user_data);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (combo_box->priv->button),
                                FALSE);
}

static void geda_combo_box_detacher (GtkWidget *widget, GedaMenu *menu)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (widget);
  GedaComboBoxData *priv  = combo_box->priv;

  g_return_if_fail (priv->popup_widget == (GtkWidget*) menu);

  g_signal_handlers_disconnect_by_func (menu->toplevel,
                                        geda_combo_box_menu_show,
                                        combo_box);
  g_signal_handlers_disconnect_by_func (menu->toplevel,
                                        geda_combo_box_menu_hide,
                                        combo_box);

  priv->popup_widget = NULL;
}

static void geda_combo_box_set_popup_widget (GedaComboBox *combo_box, GtkWidget *popup)
{
  GedaComboBoxData *priv = combo_box->priv;

  if (GEDA_IS_MENU (priv->popup_widget)) {
    gtk_widget_set_name (priv->popup_widget, NULL);
    geda_menu_detach ((GedaMenu*)priv->popup_widget);
    priv->popup_widget = NULL;
  }
  else if (priv->popup_widget) {
    geda_container_remove (priv->scrolled_window, priv->popup_widget);
    g_object_unref (priv->popup_widget);
    priv->popup_widget = NULL;
  }

  if (GEDA_IS_MENU (popup))  {

    if (priv->popup_window) {

      gtk_widget_set_name (priv->popup_window, NULL);
      gtk_widget_destroy (priv->popup_window);

      priv->popup_window = NULL;
    }

    priv->popup_widget = popup;

    /*
     * Note that we connect to show/hide on the toplevel, not the
     * menu itself, since the menu is not shown/hidden when it is
     * popped up while torn-off.
     */
    g_signal_connect (((GedaMenu*)popup)->toplevel, "show",
                      G_CALLBACK (geda_combo_box_menu_show), combo_box);
    g_signal_connect (((GedaMenu*)popup)->toplevel, "hide",
                      G_CALLBACK (geda_combo_box_menu_hide), combo_box);

    geda_menu_attach_to_widget ((GedaMenu*)popup,
                                (GtkWidget*)combo_box,
                                 geda_combo_box_detacher);
  }
  else {

    if (!priv->popup_window) {

      GtkWindow *toplevel;
      GtkWindow *window;

      priv->popup_window = gtk_window_new (GTK_WINDOW_POPUP);
      window             = (GtkWindow*)priv->popup_window;

      gtk_widget_set_name (priv->popup_window, "combobox-popup-window");

      gtk_window_set_type_hint (window, GDK_WINDOW_TYPE_HINT_COMBO);

      g_signal_connect (window, "show",
                        G_CALLBACK (geda_combo_box_child_show),
                        combo_box);

      g_signal_connect (window, "hide",
                        G_CALLBACK (geda_combo_box_child_hide),
                        combo_box);

      toplevel = (GtkWindow*)gtk_widget_get_toplevel ((GtkWidget*)combo_box);

      if (GTK_IS_WINDOW (toplevel)) {

        gtk_window_group_add_window (gtk_window_get_group (toplevel), window);

        gtk_window_set_transient_for (window, toplevel);
      }

      gtk_window_set_resizable (window, FALSE);

      gtk_window_set_screen (window, gtk_widget_get_screen((GtkWidget*)combo_box));

      priv->scrolled_window = gtk_scrolled_window_new (NULL, NULL);

      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (priv->scrolled_window),
                                      GTK_POLICY_NEVER,
                                      GTK_POLICY_NEVER);

      gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (priv->scrolled_window),
                                           GTK_SHADOW_IN);

      gtk_widget_show (priv->scrolled_window);

      geda_container_add (window, priv->scrolled_window);
    }

    geda_container_add (priv->scrolled_window, popup);

    gtk_widget_show (popup);
    g_object_ref (popup);
    priv->popup_widget = popup;
  }
}

/*! \internal helper for geda_combo_box_menu_position */
static void geda_combo_box_menu_position_below (GtkWidget *menu,      /* GedaMenu */
                                                int       *x,
                                                int       *y,
                                                int       *push_in,
                                                GtkWidget *combo_widget)
{
  GedaComboBox   *combo_box;
  GtkAllocation  *allocation;
  GtkWidget      *child;
  GdkRectangle    monitor;
  GtkRequisition  req;
  GdkScreen      *screen;
  GdkWindow      *window;

  int monitor_num;
  int sx, sy;

  /* FIXME: is using the size request here broken? WEH: Yes*/
  child = geda_get_child_widget(combo_widget);

  combo_box = (GedaComboBox*)combo_widget;

  sx = sy = 0;

  allocation = geda_get_widget_allocation (child);

  if (!gtk_widget_get_has_window (child)) {
    sx += allocation->x;
    sy += allocation->y;
  }

  window = geda_get_widget_window (child);

  gdk_window_get_root_coords (window, sx, sy, &sx, &sy);

  if (GTK_SHADOW_NONE != combo_box->priv->shadow_type) {
    sx -= geda_get_widget_style(combo_widget)->xthickness;
  }

  gtk_widget_size_request (menu, &req);

  if (gtk_widget_get_direction (combo_widget) == GTK_TEXT_DIR_LTR) {
    *x = sx;
  }
  else {
    *x = sx + allocation->width - req.width;
  }

  *y = sy;

  screen      = gtk_widget_get_screen (combo_widget);
  window      = gtk_widget_get_window (combo_widget);
  monitor_num = gdk_screen_get_monitor_at_window (screen, window);

  gdk_screen_get_monitor_geometry (screen, monitor_num, &monitor);

  if (*x < monitor.x) {
    *x = monitor.x;
  }
  else if (*x + req.width > monitor.x + monitor.width) {
    *x = monitor.x + monitor.width - req.width;
  }

  if (monitor.y + monitor.height - *y - allocation->height >= req.height)
  {
    *y += allocation->height;
  }
  else if (*y - monitor.y >= req.height)
  {
    *y -= req.height;
  }
  else if (monitor.y + monitor.height - *y - allocation->height > *y - monitor.y)
  {
    *y += allocation->height;
  }
  else
  {
    *y -= req.height;
  }

  *push_in = FALSE;
}

/*! \internal helper for geda_combo_box_menu_position */
static void geda_combo_box_menu_position_over (GtkWidget *menu_widget, /* GedaMenu */
                                                     int *x,
                                                     int *y,
                                                    bool *push_in,
                                               GtkWidget *combo_widget)
{
  GtkAllocation  *allocation;
  GtkWidget      *active;
  const GList    *children;
  GtkRequisition  requisition;

  int screen_width;
  int menu_xpos;
  int menu_ypos;
  int menu_width;

  allocation = geda_get_widget_allocation (combo_widget);

  gtk_widget_get_child_requisition (menu_widget, &requisition);

  menu_width = requisition.width;

  menu_xpos  = allocation->x;
  menu_ypos  = allocation->y + allocation->height / 2 - 2;

  active = geda_menu_get_active ((GedaMenu*)menu_widget);

  if (active != NULL)  {
    gtk_widget_get_child_requisition (active, &requisition);
    menu_ypos -= requisition.height >> 1;
  }

  children = geda_menu_shell_get_children((GedaMenuShell*)menu_widget);

  while (children) {

    GtkWidget *child = children->data;

    if (active == child)
      break;

    if (gtk_widget_get_visible (child)) {

      gtk_widget_get_child_requisition (child, &requisition);
      menu_ypos -= requisition.height;
    }

    children = children->next;
  }

  if (gtk_widget_get_direction (combo_widget) == GTK_TEXT_DIR_RTL) {
    menu_xpos = menu_xpos + allocation->width - menu_width;
  }

  gdk_window_get_root_coords (geda_get_widget_window(combo_widget),
                              menu_xpos, menu_ypos,
                              &menu_xpos, &menu_ypos);

  /* Clamp the position on screen */
  screen_width = gdk_screen_get_width (gtk_widget_get_screen (combo_widget));

  if (menu_xpos < 0) {
    menu_xpos = 0;
  }
  else if ((menu_xpos + menu_width) > screen_width) {
    menu_xpos -= ((menu_xpos + menu_width) - screen_width);
  }

  *x = menu_xpos;
  *y = menu_ypos;

  *push_in = TRUE;
}

/*! \internal #MenuPositionFunc for geda_combo_box_menu_popup */
static void geda_combo_box_menu_position (GedaMenu *menu,
                                               int *x,
                                               int *y,
                                               int *push_in,
                                         GtkWidget *combo_widget)
{
  GedaComboBoxData *priv = ((GedaComboBox*)combo_widget)->priv;

  if (priv->wrap_width > 0 || priv->cell_view == NULL) {
    geda_combo_box_menu_position_below ((GtkWidget*)menu, x, y, push_in, combo_widget);
  }
  else {

    GtkWidget *widget_item;

    /* Check if there is an active item and if so then select the item */
    /* FIXME handle nested menus better */
    widget_item = geda_menu_get_active (menu);

    if (widget_item) {
      geda_menu_shell_select_item ((GedaMenuShell*)menu, widget_item);
    }

    geda_combo_box_menu_position_over ((GtkWidget*)menu, x, y, push_in, combo_widget);
  }

  if (!gtk_widget_get_visible (menu->toplevel)) {

    gtk_window_set_type_hint (GTK_WINDOW (menu->toplevel),
                              GDK_WINDOW_TYPE_HINT_COMBO);
  }
}

static void geda_combo_box_list_position (GedaComboBox *combo_box,
                                          int          *x,
                                          int          *y,
                                          int          *width,
                                          int          *height)
{
  GedaComboBoxData *priv = combo_box->priv;
  GtkAllocation    *allocation;
  GtkPolicyType     hpolicy, vpolicy;
  GtkRequisition    popup_req;
  GdkRectangle      monitor;
  GdkScreen        *screen;
  GdkWindow        *window;
  int               monitor_num;

  /* under windows, the drop down list is as wide as the combo box
   * itself. see bug #340204 */
  GtkWidget *widget = GTK_WIDGET (combo_box);

  *x = *y = 0;

  allocation = geda_get_widget_allocation (widget);

  if (!gtk_widget_get_has_window (widget)) {
    *x += allocation->x;
    *y += allocation->y;
  }

  window = geda_get_widget_window (widget);

  gdk_window_get_root_coords (window, *x, *y, x, y);

 *width = allocation->width;

  hpolicy = vpolicy = GTK_POLICY_NEVER;

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (priv->scrolled_window),
                                  hpolicy, vpolicy);

  gtk_widget_size_request (priv->scrolled_window, &popup_req);

  if (popup_req.width > *width) {

    hpolicy = GTK_POLICY_ALWAYS;

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (priv->scrolled_window),
                                    hpolicy, vpolicy);

    gtk_widget_size_request (priv->scrolled_window, &popup_req);
  }

 *height = popup_req.height;

  screen      = gtk_widget_get_screen (GTK_WIDGET (combo_box));
  monitor_num = gdk_screen_get_monitor_at_window (screen, window);
  gdk_screen_get_monitor_geometry (screen, monitor_num, &monitor);

  if (*x < monitor.x) {
    *x = monitor.x;
  }
  else if (*x + *width > monitor.x + monitor.width) {
    *x = monitor.x + monitor.width - *width;
  }

  if (*y + allocation->height + *height <= monitor.y + monitor.height) {
      *y += allocation->height;
  }
  else if (*y - *height >= monitor.y) {
    *y -= *height;
  }
  else if (monitor.y + monitor.height - (*y + allocation->height) > *y - monitor.y)
  {
    *y += allocation->height;
    *height = monitor.y + monitor.height - *y;
  }
  else {
    *height = *y - monitor.y;
    *y = monitor.y;
  }

  if (popup_req.height > *height) {

    vpolicy = GTK_POLICY_ALWAYS;

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (priv->scrolled_window),
                                    hpolicy, vpolicy);
  }
}

static bool cell_view_is_sensitive (GtkCellView *cell_view)
{
  GList *cells;
  GList *iter;
  bool   sensitive;

  cells = gtk_cell_layout_get_cells ((GtkCellLayout*)cell_view);

  sensitive = FALSE;

  for (iter = cells; iter; iter = iter->next) {

    g_object_get (iter->data, "sensitive", &sensitive, NULL);

    if (sensitive) {
      break;
    }
  }

  g_list_free (cells);

  return sensitive;
}

static void update_menu_sensitivity (GedaComboBox *combo_box, GtkWidget *menu)
{
  GedaComboBoxData *priv = combo_box->priv;
  const GList *children, *child;

  if (!priv->model) {
    return;
  }

  children = geda_menu_shell_get_children ((GedaMenuShell*)menu);

  for (child = children; child; child = child->next) {

    GtkWidget *item, *submenu;
    GtkWidget *cell_view;

    item      = GTK_WIDGET (child->data);
    cell_view = geda_get_child_widget (item);

    if (!GTK_IS_CELL_VIEW (cell_view)) {
      continue;
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)item);

    if (submenu != NULL) {

      gtk_widget_set_sensitive (item, TRUE);
      update_menu_sensitivity (combo_box, submenu);

    }
    else {

      bool sensitive;

      sensitive = cell_view_is_sensitive ((GtkCellView*)cell_view);

      /* First iteration of for loop after recursion */
      if (menu != priv->popup_widget && child == children) {

        GtkWidget *separator = GTK_WIDGET (child->next->data);

        g_object_set (item, "visible", sensitive, NULL);
        g_object_set (separator, "visible", sensitive, NULL);

      }
      else {

        gtk_widget_set_sensitive (item, sensitive);

      }
    }
  }
}

static void geda_combo_box_menu_popup (GedaComboBox *combo_box,
                                       unsigned int  button,
                                       unsigned int  activate_time)
{
  GedaComboBoxData *priv = combo_box->priv;

  int active_item;

  update_menu_sensitivity (combo_box, priv->popup_widget);

  active_item = -1;

  if (gtk_tree_row_reference_valid (priv->active_row)) {

    GtkTreePath *path;

    path        = gtk_tree_row_reference_get_path (priv->active_row);
    active_item = gtk_tree_path_get_indices (path)[0];

    gtk_tree_path_free (path);

    if (priv->add_tearoffs) {
      active_item++;
    }
  }

  /* FIXME handle nested menus better */
  geda_menu_set_active ((GedaMenu*)priv->popup_widget, active_item);

  if (priv->wrap_width == 0) {

    GtkRequisition requisition;

    unsigned int width;

    width = geda_widget_get_allocated_width ((GtkWidget*)combo_box);

    gtk_widget_set_size_request (priv->popup_widget, -1, -1);
    gtk_widget_size_request     (priv->popup_widget, &requisition);
    gtk_widget_set_size_request (priv->popup_widget,
                                 MAX (width, requisition.width), -1);
  }

  geda_menu_popup ((GedaMenu*)priv->popup_widget,
                   NULL, NULL, (MenuPositionFunc)
                   geda_combo_box_menu_position, combo_box,
                   button, activate_time);
}

static bool popup_grab_on_window (GdkWindow    *window,
                                  unsigned int  activate_time,
                                  bool          grab_keyboard)
{
  if ((gdk_pointer_grab (window, TRUE, GDK_BUTTON_PRESS_MASK |
                                       GDK_BUTTON_RELEASE_MASK |
                                       GDK_POINTER_MOTION_MASK,
                         NULL, NULL, activate_time) == 0))
  {
    if (!grab_keyboard || gdk_keyboard_grab (window, TRUE, activate_time) == 0)
    {
      return TRUE;
    }
    else
    {
      GdkDisplay *display;

#if GTK_MAJOR_VERSION == 2 && GTK_MINOR_VERSION < 24
      display = gdk_drawable_get_display (GDK_DRAWABLE(window)); /* Since 2.2 */
#else
      display = gdk_window_get_display (GDK_WINDOW(window));     /* Since 2.24 */
#endif

      gdk_display_pointer_ungrab (display, activate_time);

      return FALSE;
    }
  }

  return FALSE;
}

/*!
 * \brief Shows the menu or dropdown list of #GedaComboBox
 * \par Function Description
 *  Pops up the menu or dropdown list of \a combo_box.
 *
 *  \param [in] combo_box a #GedaComboBox
 */
void geda_combo_box_popup (GedaComboBox *combo_box)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  g_signal_emit (combo_box, combo_box_signals[POPUP], 0);
}

static void geda_combo_box_real_popup (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  int x, y, width, height;

  if (!gtk_widget_get_realized ((GtkWidget*)combo_box)) {
    return;
  }

  if (priv->popup_shown) {
    return;
  }

  if (GEDA_IS_MENU (priv->popup_widget)) {
    geda_combo_box_menu_popup (combo_box,
                               priv->activate_button,
                               priv->activate_time);
    return;
  }

  g_return_if_fail (priv->popup_window != NULL);

  GtkTreePath *path;
  GtkTreeView *view;
  GtkWindow   *toplevel;
  GtkWindow   *window;
  GtkWidget   *widget;

  path     = NULL;
  view     = (GtkTreeView*)priv->tree_view;
  toplevel = (GtkWindow*)gtk_widget_get_toplevel ((GtkWidget*)combo_box);
  window   = (GtkWindow*)priv->popup_window;
  widget   = priv->popup_window;

  if (GTK_IS_WINDOW (toplevel)) {
    gtk_window_group_add_window (gtk_window_get_group (toplevel), window);
  }

  gtk_widget_show_all (priv->scrolled_window);
  geda_combo_box_list_position (combo_box, &x, &y, &width, &height);

  gtk_widget_set_size_request (priv->popup_window, width, height);
  gtk_window_move (window, x, y);

  if (gtk_tree_row_reference_valid (priv->active_row)) {

    GtkTreePath *ppath;

    path  = gtk_tree_row_reference_get_path (priv->active_row);
    ppath = gtk_tree_path_copy (path);

    if (gtk_tree_path_up (ppath)) {
      gtk_tree_view_expand_to_path (view, ppath);
    }

    gtk_tree_path_free (ppath);
  }

  gtk_tree_view_set_hover_expand (view, TRUE);

  /* popup */
  gtk_widget_show (widget);

  if (path) {
    gtk_tree_view_set_cursor(view, path,NULL,FALSE);
    gtk_tree_path_free (path);
  }

  gtk_widget_grab_focus (widget);

  if (!gtk_widget_has_focus ((GtkWidget*)view)) {
    gtk_widget_grab_focus ((GtkWidget*)view);
  }

  if (!popup_grab_on_window(geda_get_widget_window(widget), GDK_CURRENT_TIME, TRUE))
  {
    gtk_widget_hide (widget);
    return;
  }

  gtk_grab_add (widget);
}

static bool geda_combo_box_real_popdown (GedaComboBox *combo_box)
{
  if (combo_box->priv->popup_shown) {
    geda_combo_box_popdown (combo_box);
    return TRUE;
  }

  return FALSE;
}

/*!
 * \brief Hides the menu or dropdown list of #GedaComboBox
 * \par Function Description
 * This function is mostly intended for use by accessibility technologies;
 * applications should have little use for it.
 */
void geda_combo_box_popdown (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  priv = combo_box->priv;

  if (GEDA_IS_MENU (priv->popup_widget)) {
    geda_menu_popdown ((GedaMenu*)priv->popup_widget);
    return;
  }

  if (gtk_widget_get_realized ((GtkWidget*)combo_box)) {

    unsigned int active;

    gtk_grab_remove (priv->popup_window);

    gtk_widget_hide (priv->popup_window);

    active = gtk_toggle_button_get_active ((GtkToggleButton*)priv->button);

    if (active) {
      g_signal_handler_block (priv->button, priv->toggled_id);
      gtk_toggle_button_set_active ((GtkToggleButton*)priv->button, FALSE);
      g_signal_handler_unblock (priv->button, priv->toggled_id);
    }
  }
}

static int geda_combo_box_calc_requested_width (GedaComboBox *combo_box,
                                                GtkTreePath  *path)
{
  GedaComboBoxData *priv = combo_box->priv;
  int padding;
  GtkRequisition req;

  if (priv->cell_view) {
    gtk_widget_style_get (priv->cell_view, "focus-line-width", &padding, NULL);
  }
  else {
    padding = 0;
  }

  if (priv->cell_view) {
    gtk_cell_view_get_size_of_row ((GtkCellView*)priv->cell_view, path, &req);
  }
  else {
    req.width = 0;
  }

  return req.width + padding;
}

static inline void geda_combo_box_remeasure (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;
  GtkTreeIter  iter;
  GtkTreePath *path;
  int width, height;

  if (!priv->model || !gtk_tree_model_get_iter_first (priv->model, &iter))
  {
    return;
  }

  width  = 0;
  height = 0;

  path = gtk_tree_path_new_from_indices (0, -1);

  do {

    GtkRequisition req;

    if (priv->cell_view) {

      GtkCellView *cell_view = (GtkCellView*)priv->cell_view;

      gtk_cell_view_get_size_of_row (cell_view, path, &req);

    }
    else {
      req.width  = 0;
      req.height = 0;
    }

    width  = MAX (width, req.width);
    height = MAX (height, req.height);

    gtk_tree_path_next (path);

  } while (gtk_tree_model_iter_next (priv->model, &iter));

  gtk_tree_path_free (path);

  priv->width  = width;
  priv->height = height;

#if DEBUG
  fprintf(stderr, "%p request tree-view width=%d, height=%d\n",
          __func__, width, height);
#endif

}

static void geda_combo_box_unset_model (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  if (priv->cell_view) {
    gtk_cell_view_set_model ((GtkCellView*)priv->cell_view, NULL);
  }

  if (priv->model) {

    g_signal_handler_disconnect (priv->model,
                                 priv->inserted_id);
    g_signal_handler_disconnect (priv->model,
                                 priv->deleted_id);
    g_signal_handler_disconnect (priv->model,
                                 priv->reordered_id);
    g_signal_handler_disconnect (priv->model,
                                 priv->changed_id);

    g_object_unref (priv->model);

    priv->model = NULL;
  }

  /* menu mode */
  if (!priv->tree_view) {
    if (priv->popup_widget) {
      geda_container_foreach (priv->popup_widget, gtk_widget_destroy, NULL);
    }
  }

  if (priv->active_row) {
    gtk_tree_row_reference_free (priv->active_row);
    priv->active_row = NULL;
  }
}

static void geda_combo_box_child_show (GtkWidget *widget, GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  priv->popup_shown = TRUE;
  GEDA_OBJECT_NOTIFY (combo_box, "popup-shown");
}

static void geda_combo_box_child_hide (GtkWidget *widget, GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  priv->popup_shown = FALSE;
  GEDA_OBJECT_NOTIFY (combo_box, "popup-shown");
}

/* menu style */

static void geda_combo_box_sync_cells (GedaComboBox  *combo_box,
                                       GtkCellLayout *cell_layout)
{
  GedaComboBoxData *priv = combo_box->priv;
  GSList *k;

  for (k = priv->cells; k; k = k->next) {

    GSList *j;
    ComboCellInfo *info = (ComboCellInfo *)k->data;

    if (info->pack == GTK_PACK_START) {
      gtk_cell_layout_pack_start (cell_layout, info->cell, info->expand);
    }
    else if (info->pack == GTK_PACK_END) {
      gtk_cell_layout_pack_end (cell_layout, info->cell, info->expand);
    }

    gtk_cell_layout_set_cell_data_func (cell_layout,
                                        info->cell,
                                        combo_cell_data_func, info, NULL);

    for (j = info->attributes; j; j = j->next->next) {

      gtk_cell_layout_add_attribute (cell_layout,
                                     info->cell,
                                     j->data,
                                     (int)(long) (j->next->data));
    }
  }
}

static void geda_combo_box_menu_setup (GedaComboBox *combo_box, bool add_children)
{
  GedaComboBoxData *priv = combo_box->priv;
  GtkWidget *child;
  GtkWidget *menu;

  child = geda_get_child_widget(combo_box);

  priv->button =  gtk_toggle_button_new ();

  gtk_button_set_focus_on_click ((GtkButton*)priv->button,
                                             priv->focus_on_click);

  gtk_widget_set_parent (priv->button, gtk_widget_get_parent(child));

  if (priv->cell_view) {

    priv->box = gtk_hbox_new (FALSE, 0);
    geda_container_add (priv->button, priv->box);

    priv->separator = geda_vseparator_new ();
    geda_container_add (priv->box, priv->separator);

    priv->arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
    geda_container_add (priv->box, priv->arrow);

  }
  else {

    priv->arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
    geda_container_add (priv->button, priv->arrow);

  }

  gtk_widget_show_all (priv->button);

  priv->toggled_id = g_signal_connect (priv->button, "toggled",
                                       G_CALLBACK (geda_combo_box_button_toggled),
                                       combo_box);

  g_signal_connect (priv->button, "button-press-event",
                    G_CALLBACK (geda_combo_box_menu_button_press),
                    combo_box);

  g_signal_connect (priv->button, "state-changed",
                    G_CALLBACK (geda_combo_box_button_state_changed),
                    combo_box);

  /* create our funky menu */
  menu = geda_menu_new ();
  gtk_widget_set_name (menu, "combobox-popup-menu");
  geda_menu_set_reserve_toggle_size (GEDA_MENU (menu), FALSE);

  g_signal_connect (menu, "key-press-event",
                    G_CALLBACK (geda_combo_box_menu_key_press), combo_box);

  geda_combo_box_set_popup_widget (combo_box, menu);

  /* add items */
  if (add_children) {
    geda_combo_box_menu_fill (combo_box);
  }

  /* the column is needed in tree_column_row_is_sensitive() */
  priv->column = gtk_tree_view_column_new ();
  g_object_ref_sink (priv->column);
  geda_combo_box_sync_cells (combo_box, (GtkCellLayout*)priv->column);

  geda_combo_box_update_title (combo_box);
  geda_combo_box_update_sensitivity (combo_box);
}

static void geda_combo_box_menu_fill (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  if (priv->model) {

    GtkWidget *menu = priv->popup_widget;

    if (priv->add_tearoffs) {

      GtkWidget *tearoff = geda_tearoff_menu_item_new ();

      gtk_widget_show (tearoff);

      if (priv->wrap_width) {
        geda_menu_attach (GEDA_MENU (menu), tearoff, 0, priv->wrap_width, 0, 1);
      }
      else {
        geda_menu_append (menu, tearoff);
      }
    }

    geda_combo_box_menu_fill_level (combo_box, menu, NULL);
  }
}

static GtkWidget *_cell_view_menu_item_new (GedaComboBox  *combo_box,
                                            GtkTreeModel  *model,
                                            GtkTreeIter   *iter)
{
  GtkWidget *cell_view;
  GtkWidget *item;
  GtkTreePath *path;
  GtkRequisition req;

  cell_view = gtk_cell_view_new ();
  item = geda_menu_item_new ();
  geda_container_add (item, cell_view);

  gtk_cell_view_set_model ((GtkCellView*)cell_view, model);
  path = gtk_tree_model_get_path (model, iter);
  gtk_cell_view_set_displayed_row ((GtkCellView*)cell_view, path);
  gtk_tree_path_free (path);

  geda_combo_box_sync_cells (combo_box, (GtkCellLayout*)cell_view);
  gtk_widget_size_request (cell_view, &req);
  gtk_widget_show (cell_view);

  return item;
}

static void geda_combo_box_menu_fill_level (GedaComboBox *combo_box,
                                            GtkWidget    *menu,
                                            GtkTreeIter  *parent)
{
  GedaComboBoxData *priv  = combo_box->priv;
  GtkTreeModel     *model = priv->model;
  GtkWidget        *item, *submenu, *subitem, *separator;
  GtkTreeIter       iter;
  GtkWidget        *last;
  GtkTreePath      *path;

  bool is_separator;
  int  i;
  int  n_children;

  n_children = gtk_tree_model_iter_n_children (model, parent);

  last = NULL;
  for (i = 0; i < n_children; i++) {

    gtk_tree_model_iter_nth_child (model, &iter, parent, i);

    if (priv->row_separator_func) {
      is_separator = priv->row_separator_func (priv->model, &iter,
                                               priv->row_separator_data);
    }
    else {
      is_separator = FALSE;
    }

    if (is_separator) {

      item = gtk_separator_menu_item_new ();
      path = gtk_tree_model_get_path (model, &iter);
      g_object_set_data_full ((GObject*)item, "gtk-combo-box-item-path",
                              gtk_tree_row_reference_new (model, path),
                             (GDestroyNotify)gtk_tree_row_reference_free);
                              gtk_tree_path_free (path);
    }
    else {

      item = _cell_view_menu_item_new (combo_box, model, &iter);

      if (gtk_tree_model_iter_has_child (model, &iter)) {

        submenu = geda_menu_new ();
        geda_menu_set_reserve_toggle_size (GEDA_MENU (submenu), FALSE);
        gtk_widget_show (submenu);
        geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (item), submenu);

        /* Ugly - since menus can only activate leafs, we have to
         * duplicate the item inside the submenu.
         */
        subitem = _cell_view_menu_item_new (combo_box, model, &iter);
        separator = gtk_separator_menu_item_new ();
        gtk_widget_show (subitem);
        gtk_widget_show (separator);
        g_signal_connect (subitem, "activate",
                          G_CALLBACK (geda_combo_box_menu_item_activate),
                          combo_box);
        geda_menu_append (submenu, subitem);
        geda_menu_append (submenu, separator);

        geda_combo_box_menu_fill_level (combo_box, submenu, &iter);
      }
      g_signal_connect (item, "activate",
                        G_CALLBACK (geda_combo_box_menu_item_activate),
                        combo_box);
    }

    geda_menu_append (menu, item);
    if (priv->wrap_width && menu == priv->popup_widget) {
      geda_combo_box_relayout_item (combo_box, item, &iter, last);
    }
    gtk_widget_show (item);

    last = item;
  }
}

static void geda_combo_box_menu_destroy (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  g_signal_handlers_disconnect_matched (priv->button,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_menu_button_press, NULL);
  g_signal_handlers_disconnect_matched (priv->button,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_button_state_changed, combo_box);

  /* unparent will remove our latest ref */
  gtk_widget_unparent (priv->button);

  priv->box = NULL;
  priv->button = NULL;
  priv->arrow = NULL;
  priv->separator = NULL;

  g_object_unref (priv->column);
  priv->column = NULL;

  /* changing the popup window will unref the menu and the children */
}

/*
 * grid
 */

static bool menu_occupied (GedaMenu *menu, unsigned int left_attach,
                                           unsigned int right_attach,
                                           unsigned int top_attach,
                                           unsigned int bottom_attach)
{
  GList *i;
  bool   result;

  result = FALSE;

  for (i = GEDA_MENU_SHELL (menu)->children; i; i = i->next) {

    unsigned int l, r, b, t;

    gtk_container_child_get ((GtkContainer*)menu, i->data,
                             "left-attach", &l,
                             "right-attach", &r,
                             "bottom-attach", &b,
                             "top-attach", &t,
                             NULL);

    /* look if this item intersects with the given coordinates */
    if (right_attach > l && left_attach < r &&
        bottom_attach > t && top_attach < b)
    {
      result = TRUE;
      break;
    }
  }

  return result;
}

static void geda_combo_box_relayout_item (GedaComboBox *combo_box,
                                          GtkWidget    *item,
                                          GtkTreeIter  *iter,
                                          GtkWidget    *last)
{
  GedaComboBoxData *priv = combo_box->priv;
  GtkWidget *menu = priv->popup_widget;
  int current_col = 0;
  int current_row = 0;
  int rows = 1;
  int cols = 1;

  if (!GEDA_IS_MENU_SHELL (menu)) {
    return;
  }

  if (priv->col_column == -1 && priv->row_column == -1 && last)
  {
    gtk_container_child_get ((GtkContainer*)menu,
                             last,
                             "right-attach", &current_col,
                             "top-attach", &current_row,
                             NULL);

    if (current_col + cols > priv->wrap_width) {
      current_col = 0;
      current_row++;
    }
  }
  else {

    if (priv->col_column != -1) {
      gtk_tree_model_get (priv->model, iter, priv->col_column, &cols, -1);
    }

    if (priv->row_column != -1) {
      gtk_tree_model_get (priv->model, iter, priv->row_column, &rows, -1);
    }

    while (1) {

      bool occupied;

      if (current_col + cols > priv->wrap_width) {

          current_col = 0;
          current_row++;
      }

      occupied = menu_occupied (GEDA_MENU (menu), current_col,
                                                  current_col + cols,
                                                  current_row,
                                                  current_row + rows);
      if (!occupied) {
          break;
      }

      current_col++;
    }
  }

  /* set attach props */
  geda_menu_attach (GEDA_MENU (menu), item,
                   current_col, current_col + cols,
                   current_row, current_row + rows);
}

static void geda_combo_box_relayout (GedaComboBox *combo_box)
{
  GList     *iter;
  GList     *list;
  GtkWidget *menu;

  menu = combo_box->priv->popup_widget;

  /* do nothing unless we are in menu style and realized */
  if (combo_box->priv->tree_view || !GEDA_IS_MENU_SHELL (menu)) {
    return;
  }

  list = geda_container_get_children (menu);

  for (iter = g_list_last (list); iter; iter = iter->prev) {
    geda_container_remove (menu, iter->data);
  }

  geda_combo_box_menu_fill (combo_box);

  g_list_free (list);
}

/*!
 * \brief Popup Menu Callback; User Clicked View Auto
 * \par Function Description
 *  This functions is called when the user selects View Auto option
 *  on the popup menu. The current system style setting is used for
 *  auto mode.
 */
static void geda_combo_box_clicked_view_auto (GedaMenuItem *menuitem,
                                                      void *user_data)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;

  if (priv->as_list) {

    priv->list_view = GEDA_VIEW_AUTO;

    unsigned int as_list;

    /* Retrieve widget style property */
    gtk_widget_style_get (GTK_WIDGET (combo_box),
                          "appear-as-list", &as_list,
                          NULL);

    if (!priv->as_list && as_list) {
      priv->as_list |= 1 ;
    }
    else if (priv->as_list && !as_list) {
      priv->as_list &= ~1;
    }

    geda_combo_box_check_appearance (combo_box);

    g_signal_emit (combo_box, combo_box_signals[VIEW_CHANGED], 0, priv->list_view);
  }
}

/*!
 * \brief Popup Menu Callback; User Clicked View Menu
 * \par Function Description
 *  This functions is call when the user selects View as Menu on the popup
 *  menu.
 */
static void geda_combo_box_clicked_view_menu (GedaMenuItem *menuitem,
                                                      void *user_data)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;

  if (priv->as_list) {

    priv->list_view = GEDA_VIEW_MENU;

    unsigned int as_list = FALSE;

    if (!priv->as_list && as_list) {
      priv->as_list |= 1 ;
    }
    else if (priv->as_list && !as_list) {
      priv->as_list &= ~1;
    }

    geda_combo_box_check_appearance (combo_box);

    g_signal_emit (combo_box, combo_box_signals[VIEW_CHANGED],
                   0, priv->list_view);
  }
}

/*!
 * \brief Popup Menu Callback; User Clicked View List
 * \par Function Description
 *  This functions call when the user select View as list from the
 *  popup menu.
 */
static void geda_combo_clicked_view_list (GedaMenuItem *menuitem,
                                                  void *user_data)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;

  if (!priv->as_list) {

    priv->list_view = GEDA_VIEW_TREE;

    unsigned int as_list = TRUE;

    if (!priv->as_list && as_list) {
      priv->as_list |= 1 ;
    }
    else if (priv->as_list && !as_list) {
      priv->as_list &= ~1;
    }

    geda_combo_box_check_appearance (combo_box);

    g_signal_emit (combo_box, combo_box_signals[VIEW_CHANGED],
                   0, priv->list_view);
  }
}

/*!
 * \brief GedaCombo Right Mouse Show Popup
 * \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the combo button when the right mouse button is pressed on
 *  both the menu and list views.
 */
static void geda_combo_box_show_popup (GtkWidget      *button,
                                       GdkEventButton *event,
                                       void           *user_data)
{
  GtkWidget *menu;
  GtkWidget *popup_item;

  /* create the context menu */
  menu = geda_menu_new();

  popup_item = geda_menu_item_new_with_label (_("Auto view"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK (geda_combo_box_clicked_view_auto), user_data);

  geda_menu_append (menu, popup_item);

  popup_item = geda_menu_item_new_with_label (_("view as Menu"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK (geda_combo_box_clicked_view_menu), user_data);

  geda_menu_append (menu, popup_item);

  popup_item = geda_menu_item_new_with_label (_("view as List"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK (geda_combo_clicked_view_list), user_data);

  geda_menu_append (menu, popup_item);

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
}

/* callbacks */

static void geda_combo_box_button_state_changed (GtkWidget    *widget,
                                                 GtkStateType  previous,
                                                 void         *data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);
  GedaComboBoxData *priv  = combo_box->priv;

  if (gtk_widget_get_realized (widget)) {

    if (!priv->tree_view && priv->cell_view) {

      if ((gtk_widget_get_state (widget) == GTK_STATE_INSENSITIVE) !=
        (gtk_widget_get_state (priv->cell_view) == GTK_STATE_INSENSITIVE))
      {
        gtk_widget_set_sensitive (priv->cell_view, gtk_widget_get_sensitive (widget));
      }

      gtk_widget_set_state (priv->cell_view, gtk_widget_get_state (widget));
    }
  }

  gtk_widget_queue_draw (widget);
}

/*!
 * \brief GedaCombo Button Pressed on Button Callback
 * \par Function Description
 *  If the button was the left button the drop-down menu is displayed,
 *  if the button was the right button then a popup options menu is
 *  is displayed.
 *
 * \sa geda_combo_box_list_button_pressed geda_combo_box_show_popup
 */
static bool geda_combo_box_menu_button_press (GtkWidget      *widget,
                                              GdkEventButton *event,
                                              void           *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;
  bool ret_val;

  if (event->type == GDK_BUTTON_PRESS && event->button == 3) {

    geda_combo_box_show_popup(widget, event, user_data);
    ret_val = TRUE;
  }
  else if (GEDA_IS_MENU (priv->popup_widget) &&
           event->type == GDK_BUTTON_PRESS && event->button == 1)
  {
    if (priv->focus_on_click && !gtk_widget_has_focus (priv->button)) {
      gtk_widget_grab_focus (priv->button);
    }

    geda_combo_box_menu_popup (combo_box, event->button, event->time);

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

static void geda_combo_box_menu_item_activate (GtkWidget *item,
                                                    void *user_data)
{
  GedaComboBox *combo_box = (GedaComboBox*)user_data;
  GtkWidget    *cell_view;
  GtkTreePath  *path;
  GtkTreeIter   iter;

  cell_view = geda_get_child_widget (item);

  g_return_if_fail (GTK_IS_CELL_VIEW (cell_view));

  path = gtk_cell_view_get_displayed_row ((GtkCellView*)cell_view);

  if (gtk_tree_model_get_iter (combo_box->priv->model, &iter, path)) {
    if (geda_menu_item_get_submenu_widget ((GedaMenuItem*)item) == NULL) {
      geda_combo_box_set_active_iter (combo_box, &iter);
    }
  }

  gtk_tree_path_free (path);

  g_object_set (combo_box,
                "editing-canceled", FALSE,
                NULL);
}

static void geda_combo_box_update_sensitivity (GedaComboBox *combo_box)
{
  if (combo_box->priv->button) {

    GtkTreeIter iter;
    bool sensitive = TRUE; /* fool code checkers */

    switch (combo_box->priv->button_sensitivity) {

      case GTK_SENSITIVITY_ON:
        sensitive = TRUE;
        break;
      case GTK_SENSITIVITY_OFF:
        sensitive = FALSE;
        break;
      case GTK_SENSITIVITY_AUTO:
        sensitive = combo_box->priv->model &&
        gtk_tree_model_get_iter_first (combo_box->priv->model, &iter);
        break;
      default:
        fprintf(stderr, "%s: unhandled case <%u>\n",__func__,
                combo_box->priv->button_sensitivity);
        break;
    }

    gtk_widget_set_sensitive (combo_box->priv->button, sensitive);

    /* In list-mode, we also need to update sensitivity of the event box */
    if (GTK_IS_TREE_VIEW (combo_box->priv->tree_view) &&
                          combo_box->priv->cell_view)
    {
      gtk_widget_set_sensitive (combo_box->priv->box, sensitive);
    }
  }
}

/*! \internal combo_box->priv->model::"row-inserted" callback */
static void geda_combo_box_model_row_inserted (GtkTreeModel *model,
                                               GtkTreePath  *path,
                                               GtkTreeIter  *iter,
                                               void         *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);

  if (combo_box->priv->tree_view) {
    geda_combo_box_list_popup_resize (combo_box);
  }
  else {
    geda_combo_box_menu_row_inserted (model, path, iter, user_data);
  }

  geda_combo_box_update_sensitivity (combo_box);
}

/*! \internal combo_box->priv->model::"row-deleted" callback */
static void geda_combo_box_model_row_deleted (GtkTreeModel *model,
                                              GtkTreePath  *path,
                                              void         *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;

  if (!gtk_tree_row_reference_valid (priv->active_row)) {

    if (priv->cell_view) {
      gtk_cell_view_set_displayed_row ((GtkCellView*)priv->cell_view, NULL);
    }
    g_signal_emit (combo_box, combo_box_signals[CHANGED], 0);
  }

  if (priv->tree_view) {
    geda_combo_box_list_popup_resize (combo_box);
  }
  else {
    geda_combo_box_menu_row_deleted (model, path, user_data);
  }

  geda_combo_box_update_sensitivity (combo_box);
}

/*! \internal combo_box->priv->model::"rows-reordered" callback */
static void geda_combo_box_model_rows_reordered (GtkTreeModel *model,
                                                 GtkTreePath  *path,
                                                 GtkTreeIter  *iter,
                                                 int          *new_order,
                                                 void         *user_data)
{
  GedaComboBox *combo_box = (GedaComboBox*)user_data;

  gtk_tree_row_reference_reordered ((GObject*)user_data, path, iter, new_order);

  if (!combo_box->priv->tree_view) {
    geda_combo_box_menu_rows_reordered (model, path, iter, new_order, user_data);
  }
}

/*! \internal combo_box->priv->model::"row-changed" callback */
static void geda_combo_box_model_row_changed (GtkTreeModel *model,
                                              GtkTreePath  *path,
                                              GtkTreeIter  *iter,
                                              void         *user_data)
{
  GedaComboBox *combo_box = (GedaComboBox*)user_data;
  GedaComboBoxData *priv  = combo_box->priv;

  /* Does this belong to GtkCellView */
  if (gtk_tree_row_reference_valid (priv->active_row)) {

    GtkTreePath *active_path;

    active_path = gtk_tree_row_reference_get_path (priv->active_row);

    if (gtk_tree_path_compare (path, active_path) == 0 && priv->cell_view)
      gtk_widget_queue_resize ((GtkWidget*)priv->cell_view);

    gtk_tree_path_free (active_path);
  }

  if (priv->tree_view) {
    geda_combo_box_list_row_changed (model, path, iter, user_data);
  }
}

static bool list_popup_resize_idle (void  *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;
  int x, y, width, height;

  if (priv->tree_view && gtk_widget_get_mapped (priv->popup_window)) {

    geda_combo_box_list_position (combo_box, &x, &y, &width, &height);

    gtk_widget_set_size_request (priv->popup_window, width, height);
    gtk_window_move (GTK_WINDOW (priv->popup_window), x, y);
  }

  priv->resize_idle_id = 0;

  return FALSE;
}

static void geda_combo_box_list_popup_resize (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  if (!priv->resize_idle_id) {
    priv->resize_idle_id = g_idle_add (list_popup_resize_idle, combo_box);
  }
}

static void geda_combo_box_model_row_expanded (GtkTreeModel  *model,
                                               GtkTreePath   *path,
                                               GtkTreeIter   *iter,
                                               void          *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);

  geda_combo_box_list_popup_resize (combo_box);
}

/*! internal
 * helper called by:
 *    geda_combo_box_menu_row_inserted
 *    geda_combo_box_menu_row_deleted
 */
static GtkWidget *find_menu_by_path (GtkWidget *menu, GtkTreePath *path, bool skip_first)
{
  GList       *iter;
  GList       *list;
  GtkWidget   *item;
  GtkWidget   *submenu;
  GtkTreePath *mpath;
  bool         skip;

  list = geda_container_get_children (menu);
  skip = skip_first;
  item = NULL;

  for (iter = list; iter; iter = iter->next) {

    GedaMenuItem *menu_item = (GedaMenuItem*)iter->data;

    if (GTK_IS_SEPARATOR_MENU_ITEM (menu_item)) {

      GtkTreeRowReference *mref;

      mref = GEDA_OBJECT_GET_DATA (menu_item, "gtk-combo-box-item-path");
      if (!mref) {
        continue;
      }
      else if (!gtk_tree_row_reference_valid (mref)) {
        mpath = NULL;
      }
      else {
        mpath = gtk_tree_row_reference_get_path (mref);
      }
    }
    else if (GTK_IS_CELL_VIEW (geda_get_child_widget(menu_item))) {

      if (skip) {
        skip = FALSE;
        continue;
      }

      GtkCellView *child = (GtkCellView*)geda_get_child_widget (menu_item);

      mpath = gtk_cell_view_get_displayed_row(child);
    }
    else {
      continue;
    }

    /* this case is necessary, since the row reference of
     * the cell view may already be updated after a deletion
     */
    if (!mpath) {
      item = iter->data;
      break;
    }

    if (gtk_tree_path_compare (mpath, path) == 0) {
      gtk_tree_path_free (mpath);
      item = iter->data;
      break;
    }

    if (gtk_tree_path_is_ancestor (mpath, path)) {

      submenu = geda_menu_item_get_submenu_widget (menu_item);

      if (submenu != NULL) {
        gtk_tree_path_free (mpath);
        item = find_menu_by_path (submenu, path, TRUE);
        break;
      }
    }
    gtk_tree_path_free (mpath);
  }

  g_list_free (list);

  return item;
}

/*! \internal helper called by geda_combo_box_model_row_inserted */
static void geda_combo_box_menu_row_inserted (GtkTreeModel *model,
                                              GtkTreePath  *path,
                                              GtkTreeIter  *iter,
                                              void         *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);
  GedaComboBoxData *priv  = combo_box->priv;

  if (priv->popup_widget) {

    GtkWidget   *item, *menu;
    GtkTreeIter  piter;
    bool         is_separator;
    int  depth,  pos;

    depth = gtk_tree_path_get_depth (path);
    pos   = gtk_tree_path_get_indices (path)[depth - 1];

    if (depth > 1) {

      GtkTreePath *ppath;
      GtkWidget   *parent;

      ppath = gtk_tree_path_copy (path);
      gtk_tree_path_up (ppath);

      parent = find_menu_by_path (priv->popup_widget, ppath, FALSE);
      gtk_tree_path_free (ppath);

      menu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)parent);

      if (!menu) {

        GtkCellView *cell_view;
        GtkWidget   *separator;

        menu = geda_menu_new ();

        geda_menu_set_reserve_toggle_size ((GedaMenu*)menu, FALSE);
        gtk_widget_show (menu);

        geda_menu_item_set_submenu_widget ((GedaMenuItem*)parent, menu);

        /* Ugly - since menus can only activate leaves, we have to
         * duplicate the item inside the submenu.
         */
        gtk_tree_model_iter_parent (model, &piter, iter);

        item      = _cell_view_menu_item_new (combo_box, model, &piter);
        separator = gtk_separator_menu_item_new ();

        g_signal_connect (item, "activate",
                          G_CALLBACK (geda_combo_box_menu_item_activate),
                          combo_box);

        geda_menu_append (menu, item);
        geda_menu_append (menu, separator);

        cell_view = (GtkCellView*)geda_get_child_widget (item);

        if (cell_view_is_sensitive (cell_view)) {

          gtk_widget_show (item);
          gtk_widget_show (separator);
        }
      }
      pos += 2;
    }
    else {

      menu = priv->popup_widget;
      if (priv->add_tearoffs)
        pos += 1;
    }

    if (priv->row_separator_func) {
      is_separator = priv->row_separator_func (model, iter, priv->row_separator_data);
    }
    else {
      is_separator = FALSE;
    }

    if (is_separator) {

      item = gtk_separator_menu_item_new ();
      g_object_set_data_full ((GObject*)item, "gtk-combo-box-item-path",
                              gtk_tree_row_reference_new (model, path),
                              (GDestroyNotify)gtk_tree_row_reference_free);
    }
    else {

      item = _cell_view_menu_item_new (combo_box, model, iter);

      g_signal_connect (item, "activate",
                        G_CALLBACK (geda_combo_box_menu_item_activate),
                        combo_box);
    }

    gtk_widget_show (item);
    geda_menu_shell_insert ((GedaMenuShell*)menu, item, pos);
  }
}

static void geda_combo_box_menu_row_deleted (GtkTreeModel *model,
                                             GtkTreePath  *path,
                                             void         *user_data)
{
  GedaComboBox *combo_box = (GedaComboBox*)user_data;
  GedaComboBoxData *priv  = combo_box->priv;

  if (priv->popup_widget) {

    GtkWidget *menu;
    GtkWidget *item;

    item = find_menu_by_path (priv->popup_widget, path, FALSE);
    menu = gtk_widget_get_parent (item);
    geda_container_remove (menu, item);

    if (gtk_tree_path_get_depth (path) > 1) {

      GtkTreePath *parent_path;
      GtkTreeIter  iter;

      parent_path = gtk_tree_path_copy (path);

      gtk_tree_path_up (parent_path);
      gtk_tree_model_get_iter (model, &iter, parent_path);

      if (!gtk_tree_model_iter_has_child (model, &iter)) {

        GtkWidget   *parent;

        parent = find_menu_by_path (priv->popup_widget,
                                    parent_path, FALSE);
        geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (parent), NULL);
      }
    }
  }
}

static void geda_combo_box_menu_rows_reordered  (GtkTreeModel  *model,
                                                 GtkTreePath   *path,
                                                 GtkTreeIter   *iter,
                                                 int           *new_order,
                                                 void          *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);

  geda_combo_box_relayout (combo_box);
}

/*
 * list style
 */

static void geda_combo_box_list_setup (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;
  GtkWidget        *button;
  GtkWidget        *child;
  GtkTreeSelection *sel;
  GtkTreeView      *tree_view;

  button = gtk_toggle_button_new ();
  child  = geda_get_child_widget (combo_box);

  gtk_widget_set_parent (button, geda_get_widget_parent(child));

  g_signal_connect (button, "button-press-event",
                    G_CALLBACK (geda_combo_box_list_button_pressed), combo_box);

  priv->toggled_id = g_signal_connect (button, "toggled",
                                       G_CALLBACK (geda_combo_box_button_toggled),
                                       combo_box);

  priv->arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
  geda_container_add (button, priv->arrow);

  gtk_widget_show_all (button);

  priv->button = button;
  priv->separator = NULL;

  if (priv->cell_view) {

    GtkWidget   *child;
    GtkStyle    *style;
    GtkStateType state;

    style = gtk_widget_get_style ((GtkWidget*)combo_box);
    state = gtk_widget_get_state ((GtkWidget*)combo_box);

    gtk_cell_view_set_background_color ((GtkCellView*)priv->cell_view,
                                        &style->base[state]);

    priv->box = gtk_event_box_new ();
    gtk_event_box_set_visible_window ((GtkEventBox*)priv->box, FALSE);

    if (priv->has_frame) {
      priv->cell_view_frame = gtk_frame_new (NULL);
      gtk_frame_set_shadow_type ((GtkFrame*)priv->cell_view_frame, GTK_SHADOW_IN);
    }
    else {
      combo_box->priv->cell_view_frame = gtk_event_box_new ();
      gtk_event_box_set_visible_window ((GtkEventBox*)combo_box->priv->cell_view_frame,
                                         FALSE);
    }

    child  = geda_get_child_widget (combo_box);

    gtk_widget_set_parent (priv->cell_view_frame, geda_get_widget_parent(child));

    geda_container_add (priv->cell_view_frame, priv->box);
    gtk_widget_show_all (priv->cell_view_frame);

    g_signal_connect (priv->box, "button-press-event",
                      G_CALLBACK (geda_combo_box_list_button_pressed),
                      combo_box);
  }

  tree_view = (GtkTreeView*)gtk_tree_view_new ();

  priv->tree_view = (GtkWidget*)tree_view;

  sel = gtk_tree_view_get_selection (tree_view);

  gtk_tree_selection_set_mode (sel, GTK_SELECTION_BROWSE);
  gtk_tree_selection_set_select_function (sel,
                                          geda_combo_box_list_select_func,
                                          NULL, NULL);

  gtk_tree_view_set_headers_visible (tree_view, FALSE);
  gtk_tree_view_set_hover_selection (tree_view, TRUE);

  if (priv->row_separator_func) {
    gtk_tree_view_set_row_separator_func (tree_view,
                                          priv->row_separator_func,
                                          priv->row_separator_data,
                                          NULL);
  }

  if (priv->model) {
    gtk_tree_view_set_model (tree_view, priv->model);
  }

  priv->column = gtk_tree_view_column_new ();
  gtk_tree_view_append_column (tree_view, priv->column);

  /* sync up */
  geda_combo_box_sync_cells (combo_box, (GtkCellLayout*)priv->column);

  if (gtk_tree_row_reference_valid (priv->active_row)) {

    GtkTreePath *path;

    path = gtk_tree_row_reference_get_path (priv->active_row);

    gtk_tree_view_set_cursor (tree_view, path, NULL, FALSE);
    gtk_tree_path_free (path);
  }

  /* set widget/popup widgets */
  geda_combo_box_set_popup_widget (combo_box, (GtkWidget*)tree_view);

  g_signal_connect (tree_view, "key-press-event",
                    G_CALLBACK (geda_combo_box_list_key_press),
                    combo_box);
  g_signal_connect (tree_view, "enter-notify-event",
                    G_CALLBACK (geda_combo_box_list_enter_notify),
                    combo_box);
  g_signal_connect (tree_view, "row-expanded",
                    G_CALLBACK (geda_combo_box_model_row_expanded),
                    combo_box);
  g_signal_connect (tree_view, "row-collapsed",
                    G_CALLBACK (geda_combo_box_model_row_expanded),
                    combo_box);
  g_signal_connect (priv->popup_window, "button-press-event",
                    G_CALLBACK (geda_combo_box_list_button_pressed),
                    combo_box);
  g_signal_connect (priv->popup_window, "button-release-event",
                    G_CALLBACK (geda_combo_box_list_button_released),
                    combo_box);

  gtk_widget_show ((GtkWidget*)tree_view);

  gtk_tree_view_set_tooltip_column (tree_view, combo_box->tip_column);

  geda_combo_box_update_sensitivity (combo_box);
}

static void geda_combo_box_list_destroy (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;

  /* disconnect signals */
  g_signal_handlers_disconnect_matched (priv->tree_view,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL, NULL, combo_box);
  g_signal_handlers_disconnect_matched (priv->button,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_list_button_pressed,
                                        NULL);
  g_signal_handlers_disconnect_matched (priv->popup_window,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_list_button_pressed,
                                        NULL);
  g_signal_handlers_disconnect_matched (priv->popup_window,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_list_button_released,
                                        NULL);

  g_signal_handlers_disconnect_matched (priv->popup_window,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_child_show,
                                        NULL);

  g_signal_handlers_disconnect_matched (priv->popup_window,
                                        G_SIGNAL_MATCH_DATA,
                                        0, 0, NULL,
                                        geda_combo_box_child_hide,
                                        NULL);

  if (priv->box) {
    g_signal_handlers_disconnect_matched (priv->box,
                                          G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL,
                                          geda_combo_box_list_button_pressed,
                                          NULL);
  }

  /* destroy things (unparent will kill the latest ref from us)
   * last unref on button will destroy the arrow */
  gtk_widget_unparent (priv->button);
  priv->button = NULL;
  priv->arrow = NULL;

  if (priv->cell_view) {
    g_object_set (priv->cell_view, "background-set", FALSE, NULL);
  }

  if (priv->cell_view_frame) {
    gtk_widget_unparent (priv->cell_view_frame);
    priv->cell_view_frame = NULL;
    priv->box = NULL;
  }

  if (priv->scroll_timer) {
    g_source_remove (priv->scroll_timer);
    priv->scroll_timer = 0;
  }

  if (priv->resize_idle_id) {
    g_source_remove (priv->resize_idle_id);
    priv->resize_idle_id = 0;
  }

  gtk_tree_view_set_model ((GtkTreeView*)combo_box->priv->tree_view, NULL);
  gtk_widget_destroy (priv->tree_view);
  priv->tree_view = NULL;

  if (priv->popup_widget) {
    g_object_unref (priv->popup_widget);
    priv->popup_widget = NULL;
  }
}

static bool geda_combo_box_list_button_pressed (GtkWidget      *widget,
                                                GdkEventButton *event,
                                                void           *data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);
  GedaComboBoxData *priv  = combo_box->priv;
  bool ret_val;

  GtkWidget *ewidget = gtk_get_event_widget((GdkEvent *)event);

  if (ewidget == priv->popup_window) {
    ret_val = TRUE;
  }
  else if (event->type == GDK_BUTTON_PRESS && event->button == 3) {

    geda_combo_box_show_popup(widget, event, data);
    ret_val = TRUE;
  }
  else {

    if ((ewidget != priv->button && ewidget != priv->box) ||
         gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->button)))
      return FALSE;

    if (priv->focus_on_click && !gtk_widget_has_focus (priv->button)) {
      gtk_widget_grab_focus (priv->button);
    }

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->button), TRUE);

    priv->auto_scroll = FALSE;
    if (priv->scroll_timer == 0) {
      priv->scroll_timer = gdk_threads_add_timeout (SCROLL_TIME,
                                                    (GSourceFunc) geda_combo_box_list_scroll_timeout,
                                                    combo_box);
    }

    priv->popup_in_progress = TRUE;
    ret_val = TRUE;
  }
  return ret_val;
}

static bool geda_combo_box_list_button_released (GtkWidget      *widget,
                                                 GdkEventButton *event,
                                                 void           *data)
{
  bool ret;
  GtkTreePath *path = NULL;
  GtkTreeIter  iter;

  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);
  GedaComboBoxData *priv  = combo_box->priv;
  bool popup_in_progress  = FALSE;
  GtkWidget *ewidget      = gtk_get_event_widget ((GdkEvent *)event);

  if (priv->popup_in_progress) {

    popup_in_progress = TRUE;
    priv->popup_in_progress = FALSE;
  }

  gtk_tree_view_set_hover_expand (GTK_TREE_VIEW (priv->tree_view), FALSE);

  if (priv->scroll_timer) {

    g_source_remove (priv->scroll_timer);
    priv->scroll_timer = 0;
  }

  if (ewidget != priv->tree_view) {

    if ((ewidget == priv->button ||
         ewidget == priv->box) && !popup_in_progress &&
         gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->button)))
    {
      geda_combo_box_popdown (combo_box);
      return TRUE;
    }

    /* released outside treeview */
    if (ewidget != priv->button && ewidget != priv->box) {
      geda_combo_box_popdown (combo_box);
      return TRUE;
    }

    return FALSE;
  }

  /* select something cool */
  ret = gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (priv->tree_view),
                                       event->x, event->y,
                                       &path,
                                       NULL, NULL, NULL);

  if (ret) { /* clicked inside window? */

    gtk_tree_model_get_iter (priv->model, &iter, path);
    gtk_tree_path_free (path);

    geda_combo_box_popdown (combo_box);

    if (tree_column_row_is_sensitive (combo_box, &iter)) {
      geda_combo_box_set_active_iter (combo_box, &iter);
    }
  }

  return TRUE;
}

/*!
 * \internal "key-press-event" Callback
 * \par
 *  The GedaComboBox is passed as user data. The event is passed to
 *  the combo box if the widget does not handle the event. Widget
 *  could be a menu if callbacked directly or a list if called by
 *  geda_combo_box_list_key_press.
 */
static bool geda_combo_box_menu_key_press (GtkWidget   *widget,
                                           GdkEventKey *event,
                                           void        *combo_box)
{
  /* GedaComboBox *combo_box = GEDA_COMBO_BOX (data); */

  if (!gtk_bindings_activate_event ((GtkObject*)widget, event))  {
    /* The menu hasn't managed the event, forward it to the combobox */
    gtk_bindings_activate_event ((GtkObject*) combo_box, event);
  }

  return TRUE;
}

static bool geda_combo_box_list_key_press (GtkWidget   *widget,
                                           GdkEventKey *event,
                                           void        *data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);

  if (event->keyval == GDK_Return    ||
      event->keyval == GDK_ISO_Enter ||
      event->keyval == GDK_KP_Enter  ||
      event->keyval == GDK_space     ||
      event->keyval == GDK_KP_Space)
  {

    geda_combo_box_popdown (combo_box);

    if (combo_box->priv->model) {

      GtkTreeIter       iter;
      GtkTreeModel     *model;
      GtkTreeView      *tree_view;
      GtkTreeSelection *selection;

      tree_view = (GtkTreeView*)combo_box->priv->tree_view;
      selection = gtk_tree_view_get_selection (tree_view);

      if (gtk_tree_selection_get_selected (selection, &model, &iter)) {
        geda_combo_box_set_active_iter (combo_box, &iter);
      }
    }

    return TRUE;
  }

  return geda_combo_box_menu_key_press (widget, event, combo_box);
}

static void geda_combo_box_list_auto_scroll (GedaComboBox *combo_box, int x, int y)
{
  GtkWidget     *tree_view = combo_box->priv->tree_view;
  GtkAdjustment *adj;
  GtkAllocation *allocation;
  double lower;
  double page_size;
  double upper;
  double value;

  adj = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (combo_box->priv->scrolled_window));

  allocation = geda_get_widget_allocation (tree_view);

  if (adj) {

    upper     = geda_get_adjustment_upper(adj);
    lower     = geda_get_adjustment_lower(adj);
    page_size = geda_get_adjustment_page_size(adj);

    if (upper - lower > page_size) {

      value = geda_get_adjustment_value(adj);

      if (x <= allocation->x && lower < value) {

        value = value - (allocation->x - x + 1);

        gtk_adjustment_set_value (adj, CLAMP (value, lower, upper - page_size));
      }
      else if (x >= allocation->x + allocation->width &&
               upper - page_size > value)
      {
        value = value + (x - allocation->x - allocation->width + 1);

        gtk_adjustment_set_value (adj, CLAMP (value, 0.0, upper - page_size));
      }
    }
  }

  adj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (combo_box->priv->scrolled_window));

  if (adj) {

    upper     = geda_get_adjustment_upper(adj);
    lower     = geda_get_adjustment_lower(adj);
    page_size = geda_get_adjustment_page_size(adj);

    if (upper - lower > page_size) {

      value = geda_get_adjustment_value(adj);

      if (y <= allocation->y && lower < value) {

        value = value - (allocation->y - y + 1);

        gtk_adjustment_set_value (adj, CLAMP (value, lower, upper - page_size));
      }
      else if (y >= allocation->height &&
               upper - page_size > value)
      {
        value = value + (y - allocation->height + 1);

        gtk_adjustment_set_value (adj, CLAMP (value, 0.0, upper - page_size));
      }
    }
  }
}

static bool geda_combo_box_list_scroll_timeout (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv = combo_box->priv;
  int x, y;

  if (priv->auto_scroll) {

    GdkWindow *window;

    window = geda_get_widget_window(priv->tree_view);

    gdk_window_get_pointer (window, &x, &y, NULL);
    geda_combo_box_list_auto_scroll (combo_box, x, y);
  }

  return TRUE;
}

static bool geda_combo_box_list_enter_notify (GtkWidget        *widget,
                                              GdkEventCrossing *event,
                                              void             *data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);

  combo_box->priv->auto_scroll = TRUE;

  return TRUE;
}

static bool geda_combo_box_list_select_func (GtkTreeSelection *selection,
                                             GtkTreeModel     *model,
                                             GtkTreePath      *path,
                                             bool              path_currently_selected,
                                             void             *data)
{
  GList *list, *columns;
  bool  sensitive = FALSE;

  columns = gtk_tree_view_get_columns (gtk_tree_selection_get_tree_view (selection));

  for (list = columns; list && !sensitive; list = list->next) {

    GList             *cell, *cells;
    bool               cell_sensitive;
    bool               cell_visible;
    GtkTreeIter        iter;
    GtkTreeViewColumn *column;

    column = GTK_TREE_VIEW_COLUMN (list->data);

    if (gtk_tree_view_column_get_visible (column)) {

      gtk_tree_model_get_iter (model, &iter, path);
      gtk_tree_view_column_cell_set_cell_data (column, model, &iter,
                                               FALSE, FALSE);

      cell = cells = gtk_cell_layout_get_cells ((GtkCellLayout*)column);
      while (cell) {

        g_object_get (cell->data,
                      "sensitive", &cell_sensitive,
                      "visible", &cell_visible,
                      NULL);

        if (cell_visible && cell_sensitive) {

          sensitive = TRUE;
          break;
        }

        cell = cell->next;
      }

      g_list_free (cells);
    }
  }

  g_list_free (columns);

  return sensitive;
}

static void geda_combo_box_list_row_changed (GtkTreeModel *model,
                                             GtkTreePath  *path,
                                             GtkTreeIter  *iter,
                                             void         *data)
{
  GedaComboBox *combo_box = (GedaComboBox*)data;
  GedaComboBoxData *priv  = combo_box->priv;
  int width;

  width = geda_combo_box_calc_requested_width (combo_box, path);

  if (width > priv->width) {
    if (priv->cell_view) {
      gtk_widget_set_size_request (priv->cell_view, width, -1);
      gtk_widget_queue_resize (priv->cell_view);
    }
    priv->width = width;
  }
}

/* GtkCellLayout implementation */

static void pack_start_recurse (GtkWidget *menu, GtkCellRenderer *cell, bool expand)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *submenu;

    if (GTK_IS_CELL_LAYOUT (geda_get_child_widget(iter->data))) {

      GtkCellLayout *layout;

      layout = geda_get_child_widget(iter->data);

      gtk_cell_layout_pack_start (layout, cell, expand);
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

    if (submenu != NULL) {
      pack_start_recurse (submenu, cell, expand);
    }
  }

  g_list_free (list);
}

static void geda_combo_box_cell_layout_pack_start (GtkCellLayout   *layout,
                                                   GtkCellRenderer *cell,
                                                   bool             expand)
{
  GedaComboBox *combo_box = (GedaComboBox*)layout;
  ComboCellInfo    *info;
  GedaComboBoxData *priv;

  priv = combo_box->priv;

  g_object_ref_sink (cell);

  info         = calloc (1, sizeof(ComboCellInfo));
  info->cell   = cell;
  info->expand = expand;
  info->pack   = GTK_PACK_START;
  priv->cells  = g_slist_append (priv->cells, info);

  if (priv->cell_view) {
    gtk_cell_layout_pack_start ((GtkCellLayout*)priv->cell_view,
                                cell, expand);
  }

  if (priv->column) {
    gtk_tree_view_column_pack_start (priv->column, cell, expand);
  }

  if (GEDA_IS_MENU (priv->popup_widget)) {
    pack_start_recurse (priv->popup_widget, cell, expand);
  }
}

static void pack_end_recurse (GtkWidget *menu, GtkCellRenderer *cell, bool expand)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *submenu;

    if (GTK_IS_CELL_LAYOUT (geda_get_child_widget(iter->data))) {

      GtkCellLayout *layout;

      layout = geda_get_child_widget(iter->data);

      gtk_cell_layout_pack_end (layout, cell, expand);
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

    if (submenu != NULL) {
      pack_end_recurse (submenu, cell, expand);
    }
  }

  g_list_free (list);
}

static void geda_combo_box_cell_layout_pack_end (GtkCellLayout   *layout,
                                                 GtkCellRenderer *cell,
                                                 bool             expand)
{
  GedaComboBox     *combo_box = (GedaComboBox*)layout;
  GedaComboBoxData *priv      = combo_box->priv;
  ComboCellInfo    *info;

  g_object_ref_sink (cell);

  info         = calloc (1, sizeof(ComboCellInfo));
  info->cell   = cell;
  info->expand = expand;
  info->pack   = GTK_PACK_END;
  priv->cells  = g_slist_append (priv->cells, info);

  if (priv->cell_view) {
    gtk_cell_layout_pack_end ((GtkCellLayout*)priv->cell_view, cell, expand);
  }

  if (priv->column) {
    gtk_tree_view_column_pack_end (priv->column, cell, expand);
  }

  if (GEDA_IS_MENU (priv->popup_widget)) {
    pack_end_recurse (priv->popup_widget, cell, expand);
  }
}

static GList *geda_combo_box_cell_layout_get_cells (GtkCellLayout *layout)
{
  GedaComboBox *combo_box = (GedaComboBox*)layout;
  GSList       *list;
  GList        *ret_list = NULL;

  for (list = combo_box->priv->cells; list; list = list->next) {

    ComboCellInfo *info = (ComboCellInfo*)list->data;

    ret_list = g_list_prepend (ret_list, info->cell);
  }

  return g_list_reverse (ret_list);
}

static void clear_recurse (GtkWidget *menu)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *submenu;

    if (GTK_IS_CELL_LAYOUT (geda_get_child_widget(iter->data))) {

      GtkCellLayout *layout;

      layout = geda_get_child_widget(iter->data);

      gtk_cell_layout_clear (layout);
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

    if (submenu != NULL) {
      clear_recurse (submenu);
    }
  }

  g_list_free (list);
}

static void geda_combo_box_cell_layout_clear (GtkCellLayout *layout)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (layout);
  GedaComboBoxData *priv      = combo_box->priv;
  GSList           *iter;

  if (priv->cell_view) {
    gtk_cell_layout_clear ((GtkCellLayout*)priv->cell_view);
  }

  if (priv->column) {
    gtk_tree_view_column_clear (priv->column);
  }

  for (iter = priv->cells; iter; iter = iter->next) {

    ComboCellInfo *info = (ComboCellInfo*)iter->data;

    geda_combo_box_cell_layout_clear_attributes (layout, info->cell);
    g_object_unref (info->cell);
    free (info);
    iter->data = NULL;
  }

  g_slist_free (priv->cells);

  priv->cells = NULL;

  if (GEDA_IS_MENU (priv->popup_widget)) {
    clear_recurse (priv->popup_widget);
  }
}

static void add_attribute_recurse (GtkWidget       *menu,
                                   GtkCellRenderer *cell,
                                   const char      *attribute,
                                   int              column)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *submenu;

    if (GTK_IS_CELL_LAYOUT (geda_get_child_widget(iter->data))) {

      GtkCellLayout *layout;

      layout = geda_get_child_widget(iter->data);

      gtk_cell_layout_add_attribute (layout, cell, attribute, column);
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

    if (submenu != NULL) {
      add_attribute_recurse (submenu, cell, attribute, column);
    }
  }

  g_list_free (list);
}

static void
geda_combo_box_cell_layout_add_attribute (GtkCellLayout   *layout,
                                          GtkCellRenderer *cell,
                                          const char     *attribute,
                                          int             column)
{
  GedaComboBox *combo_box = (GedaComboBox*)layout;
  ComboCellInfo *info;

  info = geda_combo_box_get_cell_info (combo_box, cell);

  if (info != NULL) {

    info->attributes = g_slist_prepend (info->attributes, (void*)(long) (column));
    info->attributes = g_slist_prepend (info->attributes, geda_strdup (attribute));

    if (combo_box->priv->cell_view) {
      gtk_cell_layout_add_attribute ((GtkCellLayout*)combo_box->priv->cell_view,
                                     cell, attribute, column);
    }

    if (combo_box->priv->column) {
      gtk_cell_layout_add_attribute ((GtkCellLayout*)combo_box->priv->column,
                                     cell, attribute, column);
    }

    if (GEDA_IS_MENU (combo_box->priv->popup_widget)) {
      add_attribute_recurse (combo_box->priv->popup_widget, cell, attribute, column);
    }

    gtk_widget_queue_resize ((GtkWidget*)combo_box);
  }
}

static void combo_cell_data_func (GtkCellLayout   *cell_layout,
                                  GtkCellRenderer *cell,
                                  GtkTreeModel    *tree_model,
                                  GtkTreeIter     *iter,
                                  void            *data)
{
  ComboCellInfo *info = (ComboCellInfo *)data;

  if (info->func) {

    GtkWidget *parent = NULL;

    info->func (cell_layout, cell, tree_model, iter, info->func_data);

    if (GTK_IS_WIDGET (cell_layout)) {
      parent = gtk_widget_get_parent (GTK_WIDGET (cell_layout));
    }

    if (GEDA_IS_MENU_ITEM (parent) &&
        geda_menu_item_get_submenu_widget (GEDA_MENU_ITEM (parent)))
    {
      g_object_set (cell, "sensitive", TRUE, NULL);
    }
  }
}


static void set_cell_data_func_recurse (GtkWidget       *menu,
                                        GtkCellRenderer *cell,
                                        ComboCellInfo   *info)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *cell_view = geda_get_child_widget(iter->data);

    if (GTK_IS_CELL_LAYOUT (cell_view)) {

      GtkWidget *submenu;

      /* Override sensitivity for inner nodes; we do not
       * want menuitems with submenus to appear insensitive */
      gtk_cell_layout_set_cell_data_func ((GtkCellLayout*)cell_view,
                                          cell,
                                          combo_cell_data_func,
                                          info, NULL);
      submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

      if (submenu != NULL) {
        set_cell_data_func_recurse (submenu, cell, info);
      }
    }
  }

  g_list_free (list);
}

static void
geda_combo_box_cell_layout_set_cell_data_func (GtkCellLayout         *layout,
                                               GtkCellRenderer       *cell,
                                               GtkCellLayoutDataFunc  func,
                                               void                  *func_data,
                                               GDestroyNotify         destroy)
{
  GedaComboBox     *combo_box = (GedaComboBox*)layout;
  GedaComboBoxData *priv      = combo_box->priv;
  ComboCellInfo    *info;

  info = geda_combo_box_get_cell_info (combo_box, cell);

  if (info != NULL) {

    if (info->destroy) {

      GDestroyNotify d = info->destroy;

      info->destroy = NULL;
      d (info->func_data);
    }

    info->func = func;
    info->func_data = func_data;
    info->destroy = destroy;

    if (priv->cell_view)
      gtk_cell_layout_set_cell_data_func ((GtkCellLayout*)priv->cell_view, cell, func, func_data, NULL);

    if (priv->column)
      gtk_cell_layout_set_cell_data_func ((GtkCellLayout*)priv->column, cell, func, func_data, NULL);

    if (GEDA_IS_MENU (priv->popup_widget))
      set_cell_data_func_recurse (priv->popup_widget, cell, info);

    gtk_widget_queue_resize ((GtkWidget*)combo_box);
  }
}

static void
clear_attributes_recurse (GtkWidget *menu, GtkCellRenderer *cell)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *submenu;

    if (GTK_IS_CELL_LAYOUT (geda_get_child_widget(iter->data))) {

      GtkCellLayout *layout;

      layout = geda_get_child_widget(iter->data);

      gtk_cell_layout_clear_attributes (layout, cell);
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

    if (submenu != NULL) {
      clear_attributes_recurse (submenu, cell);
    }
  }
  g_list_free (list);
}

static void
geda_combo_box_cell_layout_clear_attributes (GtkCellLayout   *layout,
                                             GtkCellRenderer *cell)
{
  GedaComboBox     *combo_box = GEDA_COMBO_BOX (layout);
  GedaComboBoxData *priv;
  ComboCellInfo    *info;
  GSList           *list;

  priv = combo_box->priv;

  info = geda_combo_box_get_cell_info (combo_box, cell);
  g_return_if_fail (info != NULL);

  list = info->attributes;

  while (list && list->next) {
    g_free (list->data);
    list = list->next->next;
  }

  g_slist_free (info->attributes);
  info->attributes = NULL;

  if (priv->cell_view) {
    gtk_cell_layout_clear_attributes ((GtkCellLayout*)priv->cell_view, cell);
  }

  if (priv->column) {
    gtk_cell_layout_clear_attributes ((GtkCellLayout*)priv->column, cell);
  }

  if (GEDA_IS_MENU (priv->popup_widget)) {
    clear_attributes_recurse (priv->popup_widget, cell);
  }

  gtk_widget_queue_resize ((GtkWidget*)combo_box);
}

static void reorder_recurse (GtkWidget       *menu,
                             GtkCellRenderer *cell,
                             int              position)
{
  GList *iter, *list;

  list = geda_container_get_children (menu);

  for (iter = list; iter; iter = iter->next) {

    GtkWidget *submenu;

    if (GTK_IS_CELL_LAYOUT (geda_get_child_widget(iter->data))) {

      GtkCellLayout *layout;

      layout = geda_get_child_widget(iter->data);

      gtk_cell_layout_reorder (layout, cell, position);
    }

    submenu = geda_menu_item_get_submenu_widget ((GedaMenuItem*)iter->data);

    if (submenu != NULL)
      reorder_recurse (submenu, cell, position);
  }

  g_list_free (list);
}

static void
geda_combo_box_cell_layout_reorder (GtkCellLayout   *layout,
                                    GtkCellRenderer *cell,
                                                int  position)
{
  GedaComboBox     *combo_box = (GedaComboBox*)layout;
  GedaComboBoxData *priv;
  ComboCellInfo    *info;
  GSList           *link;

  info = geda_combo_box_get_cell_info (combo_box, cell);

  g_return_if_fail (info != NULL);
  g_return_if_fail (position >= 0);

  priv = combo_box->priv;

  link = g_slist_find (priv->cells, info);

  g_return_if_fail (link != NULL);

  priv->cells = g_slist_delete_link (priv->cells, link);
  priv->cells = g_slist_insert (priv->cells, info, position);

  if (priv->cell_view) {
    gtk_cell_layout_reorder ((GtkCellLayout*)priv->cell_view, cell, position);
  }

  if (priv->column) {
    gtk_cell_layout_reorder ((GtkCellLayout*)priv->column, cell, position);
  }

  if (GEDA_IS_MENU (priv->popup_widget)) {
    reorder_recurse (priv->popup_widget, cell, position);
  }

  gtk_widget_queue_draw ((GtkWidget*)combo_box);
}

/*
 * public API
 */

/*!
 * \brief Creates a new empty #GedaComboBox
 * \par Function Description
 *
 * \returns a new #GedaComboBox.
 */
GtkWidget *geda_combo_box_new (void)
{
  return g_object_new (GEDA_TYPE_COMBO_BOX, NULL);
}

/*!
 * \brief Creates a new #GedaComboBox with an Entry
 * \par Function Description
 *  Creates a new empty #GedaComboBox with an entry.
 *
 * \returns a new #GedaComboBox.
 */
GtkWidget *geda_combo_box_new_with_entry (void)
{
  return g_object_new (GEDA_TYPE_COMBO_BOX, "has-entry", TRUE, NULL);
}

/*!
 * \brief Creates a new #GedaComboBox with a given GtkTreeModel
 * \par Function Description
 *  Creates a new #GedaComboBox with the model initialized to model.
 *  Note that the \a model object type is specified by the gobject
 *  property parameter specifications.
 *
 * \returns a new #GedaComboBox.
 */
GtkWidget *geda_combo_box_new_with_model (GtkTreeModel *model)
{
  GedaComboBox *combo_box;

  combo_box = g_object_new (GEDA_TYPE_COMBO_BOX, "model", model, NULL);

  return GTK_WIDGET (combo_box);
}

/*!
 * \brief Creates a new #GedaComboBox with Entry and GtkTreeModel
 * \par Function Description
 *  Creates a new empty #GedaComboBox with an entry and with the
 *  model initialized to \a model.
 *
 * \returns a new #GedaComboBox
 */
GtkWidget *geda_combo_box_new_with_model_and_entry (GtkTreeModel *model)
{
  return g_object_new (GEDA_TYPE_COMBO_BOX,
                       "has-entry", TRUE,
                       "model", model,
                       NULL);
}

/*!
 * \brief Get Active #GedaComboBox item
 * \par Function Description
 *  Returns the index of the currently active item, or -1 if there is no
 *  active item. If the model is a non-flat treemodel, and the active item
 *  is not an immediate child of the root of the tree, this function returns
 *  <b>gtk_tree_path_get_indices (path)[0]</b>, where
 *  <b>path</b> is the <b>GtkTreePath</b> of the active item.
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \return An integer value which is the index of the currently active item,
 *         or -1 if there's no active item.
 */
int geda_combo_box_get_active (GedaComboBox *combo_box)
{
  GedaComboBoxData *priv;
  int result;

  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), 0);

  priv = combo_box->priv;

  if (gtk_tree_row_reference_valid (priv->active_row)) {

    GtkTreePath *path;

    path = gtk_tree_row_reference_get_path (priv->active_row);
    result = gtk_tree_path_get_indices (path)[0];
    gtk_tree_path_free (path);
  }
  else {
    result = -1;
  }

  return result;
}

static void geda_combo_box_set_active_internal (GedaComboBox *combo_box,
                                                GtkTreePath  *path)
{
  GedaComboBoxData *priv = combo_box->priv;

  /* Remember whether the initially active row is valid. */
  bool is_valid_row_reference = gtk_tree_row_reference_valid (priv->active_row);

  if (path && is_valid_row_reference) {

    GtkTreePath *active_path;
    int          path_cmp;

    active_path = gtk_tree_row_reference_get_path (priv->active_row);
    path_cmp    = gtk_tree_path_compare (path, active_path);
    gtk_tree_path_free (active_path);

    if (!path_cmp)
      return;
  }

  if (priv->active_row) {

    gtk_tree_row_reference_free (priv->active_row);
    priv->active_row = NULL;
  }

  if (!path) {

    if (priv->tree_view) {
      gtk_tree_selection_unselect_all (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (priv->tree_view)));
    }
    else {

      GedaMenu *menu = GEDA_MENU (priv->popup_widget);

      if (GEDA_IS_MENU (menu)) {
        geda_menu_set_active (menu, -1);
      }
    }

    if (priv->cell_view) {
      gtk_cell_view_set_displayed_row ((GtkCellView*)priv->cell_view, NULL);
    }

    /* Do not emit a "changed" signal when an already invalid selection
     * was not set to invalid.
     */
    if (!is_valid_row_reference) {
      return;
    }
  }
  else {

    priv->active_row = gtk_tree_row_reference_new (priv->model, path);

    if (priv->tree_view) {
      gtk_tree_view_set_cursor (GTK_TREE_VIEW (priv->tree_view),
                                path, NULL, FALSE);
    }
    else if (GEDA_IS_MENU (priv->popup_widget)) {
      /* FIXME handle nested menus better */
      geda_menu_set_active (GEDA_MENU (priv->popup_widget),
                           gtk_tree_path_get_indices (path)[0]);
    }

    if (priv->cell_view) {
      gtk_cell_view_set_displayed_row ((GtkCellView*)priv->cell_view, path);
    }
  }

  g_signal_emit (combo_box, combo_box_signals[CHANGED], 0);
  GEDA_OBJECT_NOTIFY (combo_box, "active");
}

/*!
 * \brief Set Active #GedaComboBox item
 * \par Function Description
 *  Sets the active item of \a combo_box to be the item at \a index.
 *
 * \param [in] combo_box  A #GedaComboBox
 * \param [in] index      Index in the model passed during construction,
 *                        or -1 to have no active item
 */
void geda_combo_box_set_active (GedaComboBox *combo_box, int index)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  if (index >= -2) {

    GtkTreePath *path = NULL;

    if (combo_box->priv->model == NULL) {

      /* Save index, in case the model is set after the index */
      combo_box->priv->active = index;
      if (index != -1)
        return;
    }

    if (index != -1) {
      path = gtk_tree_path_new_from_indices (index, -1);
    }

    geda_combo_box_set_active_internal (combo_box, path);

    if (path) {
      gtk_tree_path_free (path);
    }
  }
}

/*!
 * \brief Get Active #GedaComboBox iter
 * \par Function Description
 *  Sets \a iter to point to the current active item, if it exists.
 *
 * \param [in]  combobox  A #GedaComboBox
 * \param [out] iter      The uninitialized <b>GtkTreeIter</b>
 *
 * \retval %TRUE, if \a iter was set
 */
bool geda_combo_box_get_active_iter (GedaComboBox *combobox, GtkTreeIter *iter)
{
  bool result;

  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combobox), FALSE);

  if (!gtk_tree_row_reference_valid (combobox->priv->active_row)) {
    result = FALSE;
  }
  else {

    GtkTreePath *path;

    path = gtk_tree_row_reference_get_path (combobox->priv->active_row);
    result = gtk_tree_model_get_iter (combobox->priv->model, iter, path);
    gtk_tree_path_free (path);

  }
  return result;
}

/*!
 * \brief Set Active #GedaComboBox iter
 * \par Function Description
 *  Sets the current active item to be the one referenced by \a iter, or
 *  unsets the active item if \a iter is %NULL.
 *
 * \param [in] combobox  A #GedaComboBox
 * \param [in] iter       The <b>GtkTreeIter</b>, or %NULL
 */
void geda_combo_box_set_active_iter (GedaComboBox *combobox, GtkTreeIter *iter)
{
  GtkTreePath *path = NULL;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combobox));

  if (iter) {
    path = gtk_tree_model_get_path (geda_combo_box_get_model (combobox), iter);
  }

  geda_combo_box_set_active_internal (combobox, path);
  gtk_tree_path_free (path);
}

/*!
 * \brief Get count of items in GedaComboBox Model
 * \par Function Description
 *  Returns the number of items in the GtkTreeModel acting as data
 *  sources for \a combo_box. If the combo_box has no tree model
 *  then zero is returned.
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \returns Count of GtkTreeModel items or 0.
 */
int geda_combo_box_get_count (GedaComboBox *combo_box)
{
  int count;

  if (GEDA_IS_COMBO_BOX (combo_box)) {

    count = 0;

    if (combo_box->priv->model) {

      bool counter(void *model, void *path, void *iter, void *data) {
         count++;
         return FALSE;
      }

      gtk_tree_model_foreach (combo_box->priv->model,
                             (GtkTreeModelForeachFunc)counter,
                              NULL);
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaComboBox");
    count = -1;
  }
  return count;
}

/*!
 * \brief Get Active #GedaComboBox Model
 * \par Function Description
 *  Returns the GtkTreeModel which is acting as data source for \a combo_box.
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \return A GtkTreeModel which was passed during construction.
 */
GtkTreeModel *geda_combo_box_get_model (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), NULL);

  return combo_box->priv->model;
}

/*!
 * \brief Set Active #GedaComboBox Model
 * \par Function Description
 *  Sets the model used by \a combo_box to be \a model. Will unset a previously
 *  set  model (if applicable). If model is %NULL, then it will unset the model.
 *
 *  Note that this function does not clear the cell renderers, you have to
 *  call gtk_cell_layout_clear() yourself if you need to set up different
 *  cell renderers for the new model.
 *
 *  \param [in] combo_box A #GedaComboBox
 *  \param [in] model     A <b>GtkTreeIter</b>
 */
void geda_combo_box_set_model (GedaComboBox *combo_box, GtkTreeModel *model)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));
  g_return_if_fail (GTK_IS_TREE_MODEL (model));

  if (model == combo_box->priv->model) {
    return;
  }

  geda_combo_box_unset_model (combo_box);

  if (model != NULL) {

    combo_box->priv->model = g_object_ref (model);

    combo_box->priv->inserted_id =
    g_signal_connect (model, "row-inserted",
                      G_CALLBACK (geda_combo_box_model_row_inserted),
                      combo_box);
    combo_box->priv->deleted_id =
    g_signal_connect (model, "row-deleted",
                      G_CALLBACK (geda_combo_box_model_row_deleted),
                      combo_box);
    combo_box->priv->reordered_id =
    g_signal_connect (model, "rows-reordered",
                      G_CALLBACK (geda_combo_box_model_rows_reordered),
                      combo_box);
    combo_box->priv->changed_id =
    g_signal_connect (model, "row-changed",
                      G_CALLBACK (geda_combo_box_model_row_changed),
                      combo_box);

    if (combo_box->priv->tree_view) {
      /* list mode */
      gtk_tree_view_set_model ((GtkTreeView*)combo_box->priv->tree_view, model);
      geda_combo_box_list_popup_resize (combo_box);
    }
    else {
      /* menu mode */
      if (combo_box->priv->popup_widget) {
        geda_combo_box_menu_fill (combo_box);
      }
    }

    if (combo_box->priv->cell_view) {
      gtk_cell_view_set_model ((GtkCellView*)combo_box->priv->cell_view, model);
    }

    if (combo_box->priv->active != -1) {

      /* If an index was set in advance, apply it now */
      geda_combo_box_set_active (combo_box, combo_box->priv->active);
      combo_box->priv->active = -1;
    }
  }

  geda_combo_box_update_sensitivity (combo_box);

  GEDA_OBJECT_NOTIFY (combo_box, "model");
}

/* convenience API for simple text combos */

/*!
 * \brief Get a New #GedaComboBox with Text column
 * \par Function Description
 * Convenience function which constructs a new text combo box, which is a
 * #GedaComboBox displaying only strings. Only manipulate the data source
 * with the following convenience functions:
 *
 *  <DL>
 *    <DT>geda_combo_box_append_text()</DT>
 *    <DT>geda_combo_box_insert_text()</DT>
 *    <DT>geda_combo_box_prepend_text()</DT>
 *    <DT>geda_combo_box_remove_text()</DT>
 *  </DL>
 *
 * \returns A new text combo box.
 *
 * \sa #GedaComboBoxText
 */
GtkWidget *geda_combo_box_new_text (void)
{
  GtkWidget       *combo_box;
  GtkCellRenderer *cell;
  GtkListStore    *store;

  store     = gtk_list_store_new (1, G_TYPE_STRING);
  combo_box = geda_combo_box_new_with_model ((GtkTreeModel*)store);

  g_object_unref (store);

  cell = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start ((GtkCellLayout*)combo_box, cell, TRUE);
  gtk_cell_layout_set_attributes ((GtkCellLayout*)combo_box, cell,
                                  "text", 0,  NULL);

  return combo_box;
}

/*!
 * \brief Get a New #GedaComboBox with Text column and Entry
 * \par Function Description
 *  Constructs a new editable text combo box, which is a #GedaComboBox
 *  displaying only strings. If you use this function to create a text
 *  combo box, you should only manipulate its data source with the
 *  following convenience functions:
 *
 *  <DL>
 *    <DT>geda_combo_box_append_text()</DT>
 *    <DT>geda_combo_box_insert_text()</DT>
 *    <DT>geda_combo_box_prepend_text()</DT>
 *    <DT>geda_combo_box_remove_text()</DT>
 *  </DL>
 *
 *  This is a convenience function utilizing geda_combo_box_new_with_
 *  model_and_entry to create simple editable combo text box by
 *  adding a GtkListStore with a single string column. For a more
 *  advanced editable text combo use a #GedaComboBoxText widget.
 *
 * \returns A new text #GedaComboBox.
 *
 * \sa geda_combo_box_text_new_with_entry
 */
GtkWidget *geda_combo_box_new_text_with_entry (void)
{
  GtkWidget    *entry_box;
  GtkTreeModel *model;

  model     = GTK_TREE_MODEL(gtk_list_store_new (1, G_TYPE_STRING));
  entry_box = geda_combo_box_new_with_model_and_entry (model);

  g_object_unref (model);

  return entry_box;
}

/*!
 * \brief Append Text to a GedaComboBox
 * \par Function Description
 *  Appends \a text to the list of strings stored in \a combo_box. Note
 *  that this function can only be used with combo boxes constructed with
 *  geda_combo_box_new_text().
 *
 * \param [in] combo_box A #GedaComboBox constructed using geda_combo_box_new_text()
 * \param [in] text A string
 *
 * \sa: #GedaComboBoxText
 */
void geda_combo_box_append_text (GedaComboBox *combo_box, const char *text)
{
  GtkTreeIter   iter;
  GtkListStore *store;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));
  g_return_if_fail (GTK_IS_LIST_STORE (combo_box->priv->model));
  g_return_if_fail (gtk_tree_model_get_column_type (combo_box->priv->model, 0)
                    == G_TYPE_STRING);
  g_return_if_fail (g_utf8_validate(text, -1, NULL));

  store = GTK_LIST_STORE (combo_box->priv->model);

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, 0, text, -1);
}

/*!
 * \brief Get Active GedaComboBox Text
 * \par Function Description
 *  Returns the currently active string in \a combo_box or %NULL if none
 *  is selected. Note that you can only use this function with combo
 *  boxes constructed with geda_combo_box_new_text() and with
 *  #GedaEntry
 *
 * \param [in] combo_box A #GedaComboBox constructed with geda_combo_box_new_text()
 *
 * \returns a newly allocated string containing the currently active text,
 *          which should be release when no longer needed.
 *
 * If \a #GedaComboBox was constructed with geda_combo_box_new_text() then use
 * #GedaComboBoxText and geda_combo_box_text_get_active_text() instead. Or if
 * used with a #GedaEntry then use #GedaComboBox with #GedaComboBox:has-entry
 * as %TRUE and use geda_entry_get_text (GEDA_ENTRY (geda_get_child_widget (combobox)).
 */
char *geda_combo_box_get_active_text (GedaComboBox *combo_box)
{
  GedaComboBoxClass *class;

  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), NULL);

  class = GEDA_COMBO_BOX_GET_CLASS (combo_box);

  if (class->get_active_text) {
    return class->get_active_text (combo_box);
  }

  return NULL;
}

/*!
 * \brief Insert Text into a GedaComboBox
 * \par Function Description
 *  Inserts \a text at \a position in the list of strings stored in \a combo_box.
 *  Note that you can only use this function with combo boxes constructed
 *  with geda_combo_box_new_text().
 *
 * \param [in] combo_box A #GedaComboBox constructed using geda_combo_box_new_text()
 * \param [in] position  An index to insert \a text
 * \param [in] text      A string
 *
 * \sa GedaComboBoxText
 */
void geda_combo_box_insert_text (GedaComboBox *combo_box,
                                 int           position,
                                 const char   *text)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));
  g_return_if_fail (GTK_IS_LIST_STORE (combo_box->priv->model));
  g_return_if_fail (gtk_tree_model_get_column_type (combo_box->priv->model, 0)
                    == G_TYPE_STRING);

  if (text != NULL) {

    GtkTreeIter   iter;
    GtkListStore *store;

    g_return_if_fail (g_utf8_validate(text, -1, NULL));

    store = GTK_LIST_STORE (combo_box->priv->model);

    if (position >= 0) {
      gtk_list_store_insert (store, &iter, position);
    }
    else {
      gtk_list_store_append (store, &iter);

    }
    gtk_list_store_set (store, &iter, 0, text, -1);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void
geda_combo_box_remove_index (GedaComboBox *combo_box, int position)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));
  g_return_if_fail (GTK_IS_LIST_STORE (combo_box->priv->model));

  if (position >= 0) {

    GtkTreeIter   iter;
    GtkTreeModel *model;
    int index = 0;
    int next  = 0;

    model = combo_box->priv->model;

    next = gtk_tree_model_get_iter_first (model, &iter);

    while (next) {

      if (index == position) {
        gtk_list_store_remove (GTK_LIST_STORE (model), &iter);
        break;
      }

      next = gtk_tree_model_iter_next (model, &iter);
      index ++;
    }
  }
}

/*!
 * \brief Prepend Text to a GedaComboBox
 * \par Function Description
 *  Prepends \a string to the list of strings stored in \a combo_box. Note that
 *  you can only use this function with combo boxes constructed with
 *  geda_combo_box_new_text().
 *
 * \param [in] combo_box A #GedaComboBox constructed with geda_combo_box_new_text()
 * \param [in] text     A string
 *
 * \sa: GedaComboBoxText
 */
void geda_combo_box_prepend_text (GedaComboBox *combo_box, const char *text)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));
  g_return_if_fail (GTK_IS_LIST_STORE (combo_box->priv->model));
  g_return_if_fail (gtk_tree_model_get_column_type (combo_box->priv->model, 0)
                    == G_TYPE_STRING);

  if (text != NULL) {

    GtkTreeIter   iter;
    GtkListStore *store;

    g_return_if_fail (g_utf8_validate(text, -1, NULL));

    store = GTK_LIST_STORE (combo_box->priv->model);

    gtk_list_store_prepend (store, &iter);
    gtk_list_store_set (store, &iter, 0, text, -1);
  }
}

/*!
 * \brief Remove Text from a GedaComboBox
 * \par Function Description
 *  Removes the string at \a position from \a combo_box. Note that
 *  this function can only used with combo boxes constructed with
 *  geda_combo_box_new_text().
 *
 * \param [in] combo_box A #GedaComboBox constructed with geda_combo_box_new_text()
 * \param [in] position  Index of the item to remove
 *
 * \note Use #GedaComboBoxText
 */
void geda_combo_box_remove_text (GedaComboBox *combo_box, int position)
{
  GtkTreeIter iter;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));
  g_return_if_fail (GTK_IS_LIST_STORE (combo_box->priv->model));
  g_return_if_fail (gtk_tree_model_get_column_type (combo_box->priv->model, 0)
                    == G_TYPE_STRING);

  if (position >= 0) {

    GtkListStore *store = GTK_LIST_STORE (combo_box->priv->model);

    if (gtk_tree_model_iter_nth_child (combo_box->priv->model, &iter,
      NULL, position))
    {
      gtk_list_store_remove (store, &iter);
    }
  }
}

/** \defgroup GedaComboBox-methods GedaComboBox Methods
 *  @{
 */

static void geda_combo_box_real_move_active (GedaComboBox  *combo_box,
                                             GtkScrollType  scroll)
{
  GtkTreeIter iter;
  GtkTreeIter new_iter;

  if (!combo_box->priv->model) {
    gtk_widget_error_bell (GTK_WIDGET (combo_box));
  }
  else {

    bool active_iter;
    bool found;

    active_iter = geda_combo_box_get_active_iter (combo_box, &iter);

    switch (scroll) {

      case GTK_SCROLL_STEP_BACKWARD:
      case GTK_SCROLL_STEP_UP:
      case GTK_SCROLL_STEP_LEFT:
        if (active_iter) {

          found = tree_prev (combo_box, combo_box->priv->model,
                             &iter, &new_iter, FALSE);
          break;
        }
        /* else fall through */

      case GTK_SCROLL_PAGE_FORWARD:
      case GTK_SCROLL_PAGE_DOWN:
      case GTK_SCROLL_PAGE_RIGHT:
      case GTK_SCROLL_END:
        found = tree_last (combo_box, combo_box->priv->model, &new_iter, FALSE);
        break;

      case GTK_SCROLL_STEP_FORWARD:
      case GTK_SCROLL_STEP_DOWN:
      case GTK_SCROLL_STEP_RIGHT:
        if (active_iter) {

          found = tree_next (combo_box, combo_box->priv->model,
                             &iter, &new_iter, FALSE);
          break;
        }
        /* else fall through */

      case GTK_SCROLL_PAGE_BACKWARD:
      case GTK_SCROLL_PAGE_UP:
      case GTK_SCROLL_PAGE_LEFT:
      case GTK_SCROLL_START:
        found = tree_first (combo_box, combo_box->priv->model, &new_iter, FALSE);
        break;

      default:
        return;
    }

    if (found && active_iter) {

      GtkTreePath *old_path;
      GtkTreePath *new_path;

      old_path = gtk_tree_model_get_path (combo_box->priv->model, &iter);
      new_path = gtk_tree_model_get_path (combo_box->priv->model, &new_iter);

      if (gtk_tree_path_compare (old_path, new_path) == 0)
        found = FALSE;

      gtk_tree_path_free (old_path);
      gtk_tree_path_free (new_path);
    }

    if (found) {
      geda_combo_box_set_active_iter (combo_box, &new_iter);
    }
    else {
      gtk_widget_error_bell (GTK_WIDGET (combo_box));
    }
  }
}

static void geda_combo_box_entry_contents_changed (GedaEntry *entry,
                                                   void      *user_data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (user_data);

  if (geda_combo_box_get_active(combo_box) == -1) {
    g_signal_emit_by_name (combo_box, "changed");
  }
  else {
    geda_combo_box_set_active (combo_box, -1);
  }
}

static void
geda_combo_box_entry_active_changed (GedaComboBox *combo_box, void *nothing)
{
  GtkTreeIter iter;

  if (geda_combo_box_get_active_iter (combo_box, &iter)) {

    GedaEntry *entry = GEDA_ENTRY (geda_get_child_widget (combo_box));

    if (entry) {

      GtkTreeModel *model;
      GtkTreePath  *path;
      char         *path_str;
      char         *text = NULL;

      model    = geda_combo_box_get_model (combo_box);
      path     = gtk_tree_model_get_path (model, &iter);
      path_str = gtk_tree_path_to_string (path);

      g_signal_handlers_block_by_func (entry,
                                       geda_combo_box_entry_contents_changed,
                                       combo_box);

      g_signal_emit (combo_box, combo_box_signals[FORMAT_ENTRY_TEXT], 0,
                     path_str, &text);

      geda_entry_set_text (entry, text);

      g_signal_handlers_unblock_by_func (entry,
                                         geda_combo_box_entry_contents_changed,
                                         combo_box);

      gtk_tree_path_free (path);
      g_free (text);
      g_free (path_str);
    }
  }
}

static bool gtk_cell_editable_key_press (GtkWidget   *widget,
                                         GdkEventKey *event,
                                         void        *data)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (data);

  if (event->keyval == GDK_Escape) {

    g_object_set (combo_box,
                  "editing-canceled", TRUE,
                  NULL);
    gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (combo_box));
    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (combo_box));

    return TRUE;
  }
  else if (event->keyval == GDK_Return ||
           event->keyval == GDK_ISO_Enter ||
           event->keyval == GDK_KP_Enter)
  {
    gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (combo_box));
    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (combo_box));

    return TRUE;
  }

  return FALSE;
}

static bool popdown_idle (void * data)
{
  GedaComboBox *combo_box;

  combo_box = GEDA_COMBO_BOX (data);

  gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (combo_box));
  gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (combo_box));

  g_object_unref (combo_box);

  return FALSE;
}

static void popdown_handler (GtkWidget *widget, void *data)
{
  g_idle_add (popdown_idle, g_object_ref (data));
}

static bool popup_idle (void * data)
{
  GedaComboBox *combo_box;

  combo_box = GEDA_COMBO_BOX (data);

  if (GEDA_IS_MENU (combo_box->priv->popup_widget) && combo_box->priv->cell_view)
  {
    g_signal_connect_object (combo_box->priv->popup_widget,
                             "unmap", G_CALLBACK (popdown_handler),
                             combo_box, 0);
  }

  /* Unset this if a menu item is activated */
  g_object_set (combo_box, "editing-canceled", TRUE, NULL);

  geda_combo_box_popup (combo_box);

  combo_box->priv->popup_idle_id   = 0;
  combo_box->priv->activate_button = 0;
  combo_box->priv->activate_time   = 0;

  return FALSE;
}

static void
geda_combo_box_start_editing (GtkCellEditable *cell_editable, GdkEvent *event)
{
  GedaComboBox *combo_box = GEDA_COMBO_BOX (cell_editable);

  combo_box->priv->is_cell_renderer = TRUE;

  if (combo_box->priv->cell_view) {

    g_signal_connect_object (combo_box->priv->button, "key-press-event",
                             G_CALLBACK (gtk_cell_editable_key_press),
                             cell_editable, 0);

    gtk_widget_grab_focus (combo_box->priv->button);
  }
  else {

    GtkWidget *child;

    child = geda_get_child_widget(combo_box);

    g_signal_connect_object (child, "key-press-event",
                             G_CALLBACK (gtk_cell_editable_key_press),
                             cell_editable, 0);

    gtk_widget_grab_focus (child);
    gtk_widget_set_can_focus (combo_box->priv->button, FALSE);
  }

  /* Do the immediate popup only for the optionmenu-like appearance */
  if (combo_box->priv->is_cell_renderer &&
      combo_box->priv->cell_view && !combo_box->priv->tree_view)
  {
    if (event && event->type == GDK_BUTTON_PRESS) {

      GdkEventButton *event_button = (GdkEventButton *)event;

      combo_box->priv->activate_button = event_button->button;
      combo_box->priv->activate_time   = event_button->time;
    }

    combo_box->priv->popup_idle_id = g_idle_add (popup_idle, combo_box);
  }
}

/** @} endgroup GedaComboBox-methods */

/** \defgroup GedaComboBox-properties GedaComboBox Properties
 *  @{
 */

/*!
 * \brief Get Add Tearoff GedaComboBox Property
 * \par Function Description
 *  Gets the current value of the :add-tearoffs property.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \returns current value of the add-tearoffs property.
 *
 * \sa geda_combo_widget_get_add_tearoffs
 */
bool geda_combo_box_get_add_tearoffs (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), FALSE);

  return combo_box->priv->add_tearoffs;
}

/*!
 * \brief Set Add Tearoff GedaComboBox Property
 * \par Function Description
 *  Sets whether or not the popup menu should have a tearoff menu item.
 *
 * \param [in] combo_box    a #GedaComboBox
 * \param [in] add_tearoffs %TRUE to add tearoff menu items
 *
 * \sa geda_combo_widget_set_add_tearoffs
 */
void geda_combo_box_set_add_tearoffs (GedaComboBox *combo_box, bool add_tearoffs)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  add_tearoffs = add_tearoffs != FALSE;

  if (combo_box->priv->add_tearoffs != add_tearoffs) {

      combo_box->priv->add_tearoffs = add_tearoffs;

      geda_combo_box_check_appearance (combo_box);

      geda_combo_box_relayout (combo_box);

      GEDA_OBJECT_NOTIFY (combo_box, "add-tearoffs");
  }
}

/*!
 * \brief Get #GedaComboBox Column Span
 * \par Function Description
 *  Returns the column with column span information for \a combo_box.
 *
 * \param [in] combo_box  A #GedaComboBox.
 *
 * \returns the column span column.
 *
 * \sa geda_combo_widget_get_column_span_column
 */
int geda_combo_box_get_column_span_column (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), -1);

  return combo_box->priv->col_column;
}

/*!
 * \brief Set #GedaComboBox Column Span
 * \par Function Description
 *  Sets the column with column span information for \a combo_box to be
 *  \a column_span. The column span column contains integers indicating
 *  how many columns an item should span.
 *
 * \param [in] combo_box   A #GedaComboBox
 * \param [in] column_span A column in the model passed during construction
 *
 * \sa geda_combo_widget_set_column_span_column
 */
void geda_combo_box_set_column_span_column (GedaComboBox *combo_box, int column_span)
{
  GedaComboBoxData *priv;
  int col;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  priv = combo_box->priv;

  col = gtk_tree_model_get_n_columns (priv->model);

  g_return_if_fail (column_span >= -1 && column_span <= col);

  if (column_span != priv->col_column) {

    priv->col_column = column_span;

    geda_combo_box_relayout (combo_box);

    GEDA_OBJECT_NOTIFY (combo_box, "column-span-column");
  }
}

/*!
 * \brief Get GedaComboBox Focus on Click
 * \par Function Description
 *  Returns whether the combo box grabs focus when it is clicked
 *  with the mouse, \see geda_combo_box_set_focus_on_click().
 *
 * \param [in] combo  Pointer to a #GedaComboBox
 *
 * \retval %TRUE if the combo box grabs focus when it is clicked
 *         with the mouse.
 */
bool geda_combo_box_get_focus_on_click (GedaComboBox *combo)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo), FALSE);

  return combo->priv->focus_on_click;
}

/*!
 * \brief Set GedaComboBox Focus on Click
 * \par Function Description
 *  Sets whether the combo box will grab focus when it is clicked with
 *  the mouse. Making mouse clicks not grab focus is useful in places
 *  like toolbars where you do not want the keyboard focus removed from
 *  the main area of the application.
 *
 * \param [in] combo_box       Pointer to a #GedaComboBox
 * \param [in] focus_on_click  whether the combo box grabs focus when clicked
 *                             with the mouse
 */
void geda_combo_box_set_focus_on_click (GedaComboBox *combo_box, bool focus_on_click)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  focus_on_click = focus_on_click != FALSE;

  if (combo_box->priv->focus_on_click != focus_on_click) {

    combo_box->priv->focus_on_click = focus_on_click;

    if (combo_box->priv->button) {
      gtk_button_set_focus_on_click (GTK_BUTTON (combo_box->priv->button),
                                     focus_on_click);
    }
    GEDA_OBJECT_NOTIFY (combo_box, "focus-on-click");
  }
}

/*!
 * \brief Get #GedaComboBox Row Span
 * \par Function Description
 *  Returns the column with row span information for \a combo_box.
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \returns the row span column.
 *
 * \sa geda_combo_widget_get_row_span_column
 */
int geda_combo_box_get_row_span_column (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), -1);

  return combo_box->priv->row_column;
}

/*!
 * \brief Set #GedaComboBox Row Span
 * \par Function Description
 *  Sets the column with row span information for \a combo_box to
 *  be \a row_span. The row span column contains integers which
 *  indicate how many rows an item should span.
 *
 * \param [in] combo_box A #GedaComboBox.
 * \param [in] row_span  A column in the model passed during construction.
 *
 * \sa geda_combo_widget_set_row_span_column
 */
void geda_combo_box_set_row_span_column (GedaComboBox *combo_box,
                                         int           row_span)
{
  GedaComboBoxData *priv;
  int col;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  priv = combo_box->priv;

  col = gtk_tree_model_get_n_columns (priv->model);

  g_return_if_fail (row_span >= -1 && row_span < col);

  if (row_span != priv->row_column) {

    priv->row_column = row_span;

    geda_combo_box_relayout (combo_box);

    GEDA_OBJECT_NOTIFY (combo_box, "row-span-column");
  }
}

/*!
 * \brief Get Title GedaComboBox Property
 * \par Function Description
 *  Gets the current title of the menu in tearoff mode/
 * \see geda_combo_box_set_add_tearoffs().
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \returns Internal copy of menu's title string in tearoff mode
 *
 * \warning The returned string should NOT be freed.
 *
 * \sa geda_combo_widget_get_title
 */
const char *geda_combo_box_get_title (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), NULL);

  return combo_box->priv->tearoff_title;
}

/*! \internal Called by
 * geda_combo_box_set_title
 * geda_combo_box_menu_setup
 */
static void
geda_combo_box_update_title (GedaComboBox *combo_box)
{
  geda_combo_box_check_appearance (combo_box);

  if (combo_box->priv->popup_widget) {
    if (GEDA_IS_MENU (combo_box->priv->popup_widget)) {
      geda_menu_widget_set_title (combo_box->priv->popup_widget,
                                  combo_box->priv->tearoff_title);
    }
  }
}

/*!
 * \brief Set GedaComboBox Title Property
 * \par Function Description
 *  Sets the menu's title in tearoff mode.
 *
 * \param [in] combo_box a #GedaComboBox
 * \param [in] title: a title for the menu in tearoff mode
 *
 * \sa geda_combo_widget_set_title
 */
void geda_combo_box_set_title (GedaComboBox *combo_box, const char *title)
{
  GedaComboBoxData *priv;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  priv = combo_box->priv;

  if (!title) {
    title = "";
  }

  if (strcmp (title, priv->tearoff_title ? priv->tearoff_title : "") != 0) {

    g_free (priv->tearoff_title);

    priv->tearoff_title = geda_strdup (title);

    geda_combo_box_update_title (combo_box);

    GEDA_OBJECT_NOTIFY (combo_box, "tearoff-title");
  }
}

/*!
 * \brief Set GedaComboBox Tool Tip Column
 * \par Function Description
 *  This function can be used when the the tree-view model contains text-
 *  only tooltips on full rows. \a column should be set to the column in
 *  tree-view model containing the tooltip texts, or -1 to disable this
 *  feature.
 *
 *  When enabled, GtkWidget:has-tooltip will be set to %TRUE and
 *  tree_view will connect a GtkWidget::query-tooltip signal handler.
 *
 * \note the signal handler sets the text with gtk_tooltip_set_markup(),
 *       so &, <, etc have to be escaped in the text.
 *
 * \param [in] combo   Pointer to a #GedaComboBox
 * \param [in] column  Valid integer column number for tree model
 */
void geda_combo_box_set_tooltip_column (GedaComboBox *combo, int column)
{
  GedaComboBoxData *priv;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo));

  combo->tip_column = column;
  priv = combo->priv;

  gtk_tree_view_set_tooltip_column (GTK_TREE_VIEW(priv->tree_view),
                                    combo->tip_column);
}

/*!
 * \brief Get #GedaComboBox Wrap Width
 * \par Function Description
 *  Returns the wrap width which is used to determine the number of columns
 *  for the popup menu. If the wrap width is larger than 1, the combo box
 *  is in table mode.
 *
 * \param [in] combo_box
 *
 * \returns the wrap width.
 *
 * \sa geda_combo_widget_get_wrap_width
 */
int geda_combo_box_get_wrap_width (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), -1);

  return combo_box->priv->wrap_width;
}

/*!
 * \brief Set #GedaComboBox Wrap Width
 * \par Function Description
 *  Sets the wrap width of \a combo_box to be \a width. The wrap width is
 *  basically the preferred number of columns when you want the popup to
 *  be layed out in a table.
 *
 * \param [in] combo_box A #GedaComboBox
 * \param [in] width     Preferred number of columns
 *
 * \sa geda_combo_widget_set_wrap_width
 */
void geda_combo_box_set_wrap_width (GedaComboBox *combo_box, int width)
{
  GedaComboBoxData *priv;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  if (width < 0) {
    width = GEDA_COMBO_DEFAULT_WRAP;
  }

  priv = combo_box->priv;

  if (width != priv->wrap_width) {

    priv->wrap_width = width;

    geda_combo_box_check_appearance (combo_box);
    geda_combo_box_relayout (combo_box);

    GEDA_OBJECT_NOTIFY (combo_box, "wrap-width");
  }
}

/*!
 * \brief Get GedaComboBox Popup Accessible object
 * \par Function Description
 *  Gets the accessible object corresponding to the combo box's popup.
 *
 *  This function is mostly intended for use by accessibility technologies;
 *  applications should have little use for it.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \returns accessible object corresponding to the combo box's popup.
 */
AtkObject *geda_combo_box_get_popup_accessible (GedaComboBox *combo_box)
{
  AtkObject *atk_obj;

  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), NULL);

  if (combo_box->priv->popup_widget) {
    atk_obj = gtk_widget_get_accessible (combo_box->priv->popup_widget);
  }
  else {
    atk_obj = NULL;
  }

  return atk_obj;
}

/*!
 * \brief Get GedaComboBox Row Seperator function
 * \par Function Description
 *  Returns the current row separator function.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \return the current row separator function.
 *
 */
GtkTreeViewRowSeparatorFunc
geda_combo_box_get_row_separator_func (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), NULL);

  return combo_box->priv->row_separator_func;
}

/*!
 * \brief Set GedaComboBox Row Seperator function
 * \par Function Description
 *  Sets the row separator function, which is used to determine whether
 *  a row should be drawn as a separator. If the row separator function
 *  is %NULL, no separators are drawn. This is the default value.
 *
 * \param [in] combo_box a #GedaComboBox
 * \param [in] func      a <b>GtkTreeViewRowSeparatorFunc</b>
 * \param [in] data       user data to pass to \a func, or %NULL
 * \param [in] destroy    destroy notifier for \a data, or %NULL
 */
void geda_combo_box_set_row_separator_func (GedaComboBox                *combo_box,
                                            GtkTreeViewRowSeparatorFunc  func,
                                            void                        *data,
                                            GDestroyNotify               destroy)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  if (combo_box->priv->row_separator_destroy) {
    combo_box->priv->row_separator_destroy (combo_box->priv->row_separator_data);
  }

  combo_box->priv->row_separator_func = func;
  combo_box->priv->row_separator_data = data;
  combo_box->priv->row_separator_destroy = destroy;

  if (combo_box->priv->tree_view) {
    gtk_tree_view_set_row_separator_func (GTK_TREE_VIEW (combo_box->priv->tree_view),
                                          func, data, NULL);
  }

  geda_combo_box_relayout (combo_box);

  gtk_widget_queue_draw (GTK_WIDGET (combo_box));
}

/*!
 * \brief Set GedaComboBox Button Sensitivity
 * \par Function Description
 *  Sets whether the dropdown button of the combo box should be
 *  always sensitive (%GTK_SENSITIVITY_ON), never sensitive (%GTK_SENSITIVITY_OFF)
 *  or only if there is at least one item to display (%GTK_SENSITIVITY_AUTO).
 *
 * \param [in] combo_box  a #GedaComboBox
 * \param [in] sensitivity specify the sensitivity of the dropdown button
 *
 */
void geda_combo_box_set_button_sensitivity (GedaComboBox       *combo_box,
                                            GtkSensitivityType  sensitivity)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  if (combo_box->priv->button_sensitivity != sensitivity) {

    combo_box->priv->button_sensitivity = sensitivity;
    geda_combo_box_update_sensitivity (combo_box);

    GEDA_OBJECT_NOTIFY (combo_box, "button-sensitivity");
  }
}

/*!
 * \brief Get GedaComboBox Button Sensitivity
 * \par Function Description
 *  Returns whether the combo box sets the dropdown button
 *  sensitive or not when there are no items in the model.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \return %GTK_SENSITIVITY_ON if the dropdown button is sensitive
 *         when the model is empty, %GTK_SENSITIVITY_OFF if the button
 *         is always insensitive or %GTK_SENSITIVITY_AUTO if it is only
 *         sensitive as long as the model has one item to be selected.
 */
GtkSensitivityType geda_combo_box_get_button_sensitivity (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), FALSE);

  return combo_box->priv->button_sensitivity;
}

/*!
 * \brief Get GedaComboBox has Entry
 * \par Function Description
 *  Returns whether the combo box has an entry.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \returns whether there is an entry in \a combo_box.
 */
bool geda_combo_box_get_has_entry (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), FALSE);

  return combo_box->priv->has_entry;
}

/*!
 * \brief Retrieve GedaEntry from a GedaComboBox
 * \par Function Description
 *  Returns the combo box entry as a GedaEntry.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \return GedaEntry entry or NULL if the combo has no entry.
 *
 * \sa geda_combo_get_entry_widget
 *     geda_combo_widget_get_entry
 *     geda_combo_widget_get_entry_widget
 */
GedaEntry *geda_combo_get_entry (GedaComboBox *combo_box) {

  GtkWidget *widget = geda_combo_get_entry_widget(combo_box);

  if (widget) {
    return GEDA_ENTRY(widget);
  }
  return NULL;
}

/*!
 * \brief Retrieve Entry Widget from a GedaComboBox
 * \par Function Description
 *  Returns the combo box entry as a GtkWidget.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \return entry widget or NULL if the combo has no entry.
 *
 * \sa geda_combo_get_entry
 *     geda_combo_widget_get_entry
 *     geda_combo_widget_get_entry_widget
 */
GtkWidget *geda_combo_get_entry_widget (GedaComboBox *combo_box) {

  if (GEDA_IS_COMBO_BOX (combo_box)) {
    if (((GedaComboBox*)combo_box)->priv->has_entry) {
      return geda_get_child_widget(combo_box);
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaComboBox");
  }
  return NULL;
}

/*!
 * \brief Get GedaComboBox Entry Text Column
 * \par Function Description
 *  Returns the column which \a combo_box is using to get the strings
 *  from to display in the internal entry.
 *
 * \param [in] combo_box A #GedaComboBox.
 *
 * \returns column number in the data source model of \a combo_box.
 *
 * \sa geda_combo_widget_get_entry_text_column
 */
int geda_combo_box_get_entry_text_column (GedaComboBox *combo_box)
{
  g_return_val_if_fail (GEDA_IS_COMBO_BOX (combo_box), 0);

  return combo_box->priv->text_column;
}

/*!
 * \brief Set GedaComboBox Entry Text Column
 * \par Function Description
 *  Sets the model column which \a combo_box should use to get strings from
 *  to be \a text_column. The column \a text_column in the model of \a combo_box
 *  must be of type %G_TYPE_STRING.
 *
 *  This is only relevant if \a combo_box has been created with
 *  #GedaComboBox:has-entry as %TRUE.
 *
 * \param [in] combo_box    A #GedaComboBox
 * \param [in] text_column  A column in \a model to get the strings from for
 *                          the internal entry
 */
void geda_combo_box_set_entry_text_column (GedaComboBox *combo_box, int text_column)
{
  GedaComboBoxData *priv = combo_box->priv;

  g_return_if_fail (GEDA_IS_COMBO_BOX (combo_box));

  if (text_column >= 0) {

    GtkTreeModel *model = geda_combo_box_get_model (combo_box);

    if (model != NULL) {

      g_return_if_fail (text_column < gtk_tree_model_get_n_columns (model));

      priv->text_column = text_column;

      if (priv->text_renderer) {
        gtk_cell_layout_set_attributes ((GtkCellLayout*)combo_box,
                                        priv->text_renderer,
                                        "text", text_column,
                                        NULL);
      }
    }
  }
}

/* ------------------------ Widget Versions ------------------------ */

/** \defgroup GedaComboBox-widget GedaComboBox Widget Operatives
 *  @{
 */

/*!
 * \brief Get GedaComboBox Widget Add Tearoff Property
 * \par Function Description
 *  Gets the current value of the :add-tearoffs property.
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \returns current value of the add-tearoffs property.
 *
 * \sa geda_combo_widget_set_add_tearoffs
 */
bool geda_combo_widget_get_add_tearoffs (GtkWidget *combo_box) {

  if (GEDA_IS_COMBO_BOX (combo_box)) {
    return ((GedaComboBox*)combo_box)->priv->add_tearoffs;
  }

  BUG_MSG ("Operative is not a GedaComboBox");
  return FALSE;
}

/*!
 * \brief Set GedaComboBox Widget Add Tearoff Property
 * \par Function Description
 *  Sets whether the popup menu should have a tearoff menu item.
 *
 * \param [in] combo        a #GedaComboBox
 * \param [in] add_tearoffs %TRUE to add tearoff menu items
 *
 * \sa geda_combo_box_set_add_tearoffs
 */
void geda_combo_widget_set_add_tearoffs (GtkWidget *combo, bool add_tearoffs)
{
  return geda_combo_box_set_add_tearoffs((GedaComboBox*)combo, add_tearoffs);
}

/*!
 * \brief Get #GedaComboBox Widget Column Span
 * \par Function Description
 *  Returns the column with column span information for \a combo_box.
 *
 * \param [in] combo_box  A #GedaComboBox.
 *
 * \returns the column span column.
 *
 * \sa geda_combo_box_get_column_span_column
 */
int geda_combo_widget_get_column_span_column (GtkWidget *combo_box) {

  if (GEDA_IS_COMBO_BOX (combo_box))
    return ((GedaComboBox*)combo_box)->priv->col_column;

  BUG_MSG ("Operative is not a GedaComboBox");
  return -1;
}

/*!
 * \brief Set #GedaComboBox Widget Column Span
 * \par Function Description
 *  Sets the column with column span information for \a combo_box to be
 *  \a column_span. The column span column contains integers which indicate
 *  how many columns an item should span.
 *
 * \param [in] combo   A #GedaComboBox
 * \param [in] column_span A column in the model passed during construction
 *
 * \sa geda_combo_box_set_column_span_column
 */
void geda_combo_widget_set_column_span_column (GtkWidget *combo, int column_span)
{
  return geda_combo_box_set_column_span_column((GedaComboBox*)combo, column_span);
}

/*!
 * \brief Get #GedaComboBox Widget Row Span
 * \par Function Description
 *  Returns the column with row span information for \a combo_box.
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \returns the row span column.
 *
 * \sa geda_combo_box_get_row_span_column
 */
int geda_combo_widget_get_row_span_column (GtkWidget *combo_box) {

  if (GEDA_IS_COMBO_BOX (combo_box))
    return ((GedaComboBox*)combo_box)->priv->row_column;

  BUG_MSG ("Operative is not a GedaComboBox");
  return -1;
}

/*!
 * \brief Set #GedaComboBox Widget Row Span
 * \par Function Description
 *  Sets the column with row span information for \a combo_box to
 *  be \a row_span. The row span column contains integers which
 *  indicate how many rows an item should span.
 *
 * \param [in] combo     A #GedaComboBox.
 * \param [in] row_span  A column in the model passed during construction.
 *
 * \sa geda_combo_box_set_row_span_column
 */
void geda_combo_widget_set_row_span_column (GtkWidget *combo, int row_span)
{
  return geda_combo_box_set_row_span_column((GedaComboBox*)combo, row_span);
}

/*!
 * \brief Get GedaComboBox Widget Title Property
 * \par Function Description
 *  Gets the current title of the menu in tearoff mode.
 * \see geda_combo_box_set_add_tearoffs().
 *
 * \param [in] combo_box a #GedaComboBox
 *
 * \returns Internal copy of menu's title string in tearoff mode
 *
 * \warning The returned string should NOT be freed.
 *
 * \sa geda_combo_box_get_title
 */
const char *geda_combo_widget_get_title (GtkWidget *combo_box) {
  if (GEDA_IS_COMBO_BOX (combo_box))
    return ((GedaComboBox*)combo_box)->priv->tearoff_title;
  BUG_MSG ("Operative is not a GedaComboBox");
  return NULL;
}

/*!
 * \brief Set GedaComboBox Widget Title Property
 * \par Function Description
 *  Sets the menu's title in tearoff mode.
 *
 * \param [in] combo_box a #GedaComboBox
 * \param [in] title     a title for the menu in tearoff mode
 *
 * \sa geda_combo_box_set_title
 */
void
geda_combo_widget_set_title (GtkWidget *combo_box, const char  *title) {
  return geda_combo_box_set_title((GedaComboBox*)combo_box, title);
}

/*!
 * \brief Set GedaComboBox Tooltip Column
 * \par Function Description
 *  Sets the index of the column containing tooltip text.
 *
 * \param [in] combo_box a #GedaComboBox
 * \param [in] column    Index of column containing tooltip text
 */
void geda_combo_widget_set_tooltip_column (GtkWidget *combo_box, int column) {
  if (GEDA_IS_COMBO_BOX (combo_box)) {
    ((GedaComboBox*)combo_box)->tip_column = column;
  }
  else {
    BUG_MSG ("Operative is not a GedaComboBox");
  }
}

/*!
 * \brief Get #GedaComboBox Widget Wrap Width
 * \par Function Description
 *  Returns the wrap width which is used to determine the number of columns
 *  for the popup menu. If the wrap width is larger than 1, the combo box
 *  is in table mode.
 *
 * \param [in] combo_box
 *
 * \returns the wrap width.
 *
 * \sa geda_combo_box_get_wrap_width
 */
int
geda_combo_widget_get_wrap_width (GtkWidget *combo_box) {
  if (GEDA_IS_COMBO_BOX (combo_box))
    return ((GedaComboBox*)combo_box)->priv->wrap_width;
  BUG_MSG ("Operative is not a GedaComboBox");
  return -1;
}

/*!
 * \brief Set #GedaComboBox Widget Wrap Width
 * \par Function Description
 *  Sets the wrap width of \a combo_box to be \a width. The wrap width is
 *  basically the preferred number of columns when you want the popup to
 *  be layed out in a table.
 *
 * \param [in] combo_box A #GedaComboBox
 * \param [in] width     Preferred number of columns
 *
 * \sa geda_combo_box_set_wrap_width
 */
void geda_combo_widget_set_wrap_width (GtkWidget *combo_box, int width) {
  return geda_combo_box_set_wrap_width((GedaComboBox*)combo_box, width);
}

/** \defgroup GedaComboBox-widget-active GedaComboBox Widget get/set active item
 *  @{
 */

/*!
 * \brief Retrieve the Active #GedaComboBox Widget item
 * \par Function Description
 * \see geda_combo_box_get_active
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \return An integer value which is the index of the currently active item,
 *         or -1 if there's no active item.
 */
int geda_combo_widget_get_active (GtkWidget *combo_box) {
  return geda_combo_box_get_active((GedaComboBox*)combo_box);
}

/*!
 * \brief Set the Active #GedaComboBox Widget item
 * \par Function Description
 * \see geda_combo_box_set_active
 *
 * \param [in] combo_box A #GedaComboBox
 * \param [in] index     Interger index of item to be set active
 */
void geda_combo_widget_set_active (GtkWidget *combo_box, int index) {
  return geda_combo_box_set_active((GedaComboBox*)combo_box, index);
}

/*!
 * \brief Retrieve the Active iter from a #GedaComboBox Widget
 * \par Function Description
 * \see geda_combo_box_get_active_iter
 *
 * \param [in] combo A #GedaComboBox
 * \param [out] iter The uninitialized <b>GtkTreeIter</b>
 *
 * \retval %TRUE, if \a iter was set
 */
bool geda_combo_widget_get_active_iter(GtkWidget *combo, GtkTreeIter *iter)
{
  return geda_combo_box_get_active_iter((GedaComboBox*)combo, iter);
}

/*!
 * \brief Set the #GedaComboBox Widget Active iter
 * \par Function Description
 * \see geda_combo_box_set_active_iter
 *
 * \param [in] combo A #GedaComboBox
 * \param [in] iter  The uninitialized <b>GtkTreeIter</b>
 */
void geda_combo_widget_set_active_iter(GtkWidget *combo, GtkTreeIter *iter)
{
  return geda_combo_box_set_active_iter((GedaComboBox*)combo, iter);
}

/** @} endgroup GedaComboBox-widget-active */

/** \defgroup GedaComboBox-widget-getters-setters GedaComboBox Widget getters and setters
 *  @{
 */

/*!
 * \brief Widget version of #geda_combo_box_get_count
 * \par Function Description
 * \see geda_combo_box_get_count
 */
int geda_combo_widget_get_count(GtkWidget *widget)
{
  return geda_combo_box_get_count((GedaComboBox*)widget);
}

/*!
 * \brief Get GedaComboBox Widget Focus on Click
 * \par Function Description
 *  Returns whether the combo box grabs focus when it is clicked
 *  with the mouse. See geda_combo_box_set_focus_on_click().
 *
 * \param [in] combo_box  Pointer to a #GedaComboBox
 *
 * \retval %TRUE if the combo box grabs focus when it is clicked
 *         with the mouse.
 *
 * \sa geda_combo_box_get_focus_on_click
 */
bool geda_combo_widget_get_focus_on_click (GtkWidget *combo_box) {
  if (GEDA_IS_COMBO_BOX (combo_box))
    return ((GedaComboBox*)combo_box)->priv->focus_on_click;
  BUG_MSG ("Operative is not a GedaComboBox");
  return FALSE;
}

/*!
 * \brief Set GedaComboBox Focus on Click
 * \par Function Description
 *  Sets whether the combo box will grab focus when it is clicked with
 *  the mouse, \see geda_combo_box_set_focus_on_click.
 *
 * \param [in] combo           Pointer to a #GedaComboBox
 * \param [in] focus_on_click  whether the combo box grabs focus when clicked
 *                             with the mouse
 */
void geda_combo_widget_set_focus_on_click (GtkWidget *combo, bool focus_on_click)
{
  return geda_combo_box_set_focus_on_click((GedaComboBox*)combo, focus_on_click);
}

/*!
 * \brief Get GedaFontDialog has Entry Widget
 * \par Function Description
 *  Returns whether or not the GedaComboBox has an entry widget.
 *
 * \param [in] combo_box Pointer to a #GedaComboBox widget
 *
 * \returns TRUE of the GedaComboBox has an entry widget.
 */
bool geda_combo_widget_get_has_entry (GtkWidget *combo_box)
{
  if (GEDA_IS_COMBO_BOX (combo_box))
    return ((GedaComboBox*)combo_box)->priv->has_entry;
  BUG_MSG ("Operative is not a GedaComboBox");
  return FALSE;
}

/*!
 * \brief Retrieve the GedaEntry from a GedaComboBox Widget
 * \par Function Description
 *  Returns the combo box entry as a GedaEntry.
 *
 * \param [in] combo_box a #GedaComboBox widget
 *
 * \return GedaEntry entry or NULL if the combo has no entry.
 *
 * \sa geda_combo_get_entry
 *     geda_combo_get_entry_widget
 *     geda_combo_widget_get_entry_widget
 */
GedaEntry *geda_combo_widget_get_entry (GtkWidget *combo_box)
{
  GtkWidget *widget = geda_combo_widget_get_entry_widget(combo_box);

  if (widget) {
    return GEDA_ENTRY(widget);
  }
  return NULL;
}

/*!
 * \brief Retrieve Entry Widget from a GedaComboBox Widget
 * \par Function Description
 *  Returns the combo box entry as a GtkWidget.
 *
 * \param [in] combo_box a #GedaComboBox widget
 *
 * \return entry widget or NULL if the combo has no entry.
 *
 * \sa geda_combo_get_entry
 *     geda_combo_get_entry_widget
 *     geda_combo_widget_get_entry
 */
GtkWidget *geda_combo_widget_get_entry_widget (GtkWidget *combo_box)
{
  if (GEDA_IS_COMBO_BOX (combo_box)) {
    if (((GedaComboBox*)combo_box)->priv->has_entry) {
      return geda_get_child_widget(combo_box);
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaComboBox");
  }
  return NULL;
}

/*!
 * \brief Get GedaComboBox Widget Entry Text Column
 * \par Function Description
 *  Returns the column which \a combo_box is using to get the strings
 *  from to display in the internal entry.
 *
 * \param [in] combo_box A #GedaComboBox.
 *
 * \returns column number in the data source model of \a combo_box.
 *
 * \sa geda_combo_box_get_entry_text_column
 */
int geda_combo_widget_get_entry_text_column (GtkWidget *combo_box)
{
  if (GEDA_IS_COMBO_BOX (combo_box)){
    return ((GedaComboBox*)combo_box)->priv->text_column;
  }
  BUG_MSG ("Operative is not a GedaComboBox");
  return 0;
}

/*!
 * \brief Set GedaComboBox Widget Entry Text Column
 * \par Function Description
 *  Sets the model column which \a combo_box should use to get strings from
 *  to be \a text_column, \see geda_combo_box_set_entry_text_column.
 *
 *  This is only relevant if \a combo_box has an entry.
 *
 * \param [in] combo        A #GedaComboBox
 * \param [in] text_column  A column in \a model to get the strings from for
 *                          the internal entry
 */
void geda_combo_widget_set_entry_text_column (GtkWidget *combo, int text_column)
{
 return geda_combo_box_set_entry_text_column((GedaComboBox*)combo, text_column);
}

/*!
 * \brief Widget version to Retrieve the #GedaComboBox Tree Model
 * \par Function Description
 *  Returns the GtkTreeModel providing the data source for \a combo_box.
 *
 * \param [in] combo_box A #GedaComboBox
 *
 * \return A GtkTreeModel which was passed during construction.
 */
GtkTreeModel *geda_combo_widget_get_model (GtkWidget *combo_box)
{
  if (GEDA_IS_COMBO_BOX (combo_box)) {
    return ((GedaComboBox*)combo_box)->priv->model;
  }
  BUG_MSG ("Operative is not a GedaComboBox");
  return NULL;
}

/*!
 * \brief Set the #GedaComboBox Widget Active Model
 * \par Function Description
 * \see geda_combo_box_set_model
 */
void geda_combo_widget_set_model (GtkWidget *combo_box, GtkTreeModel *model)
{
  return geda_combo_box_set_model((GedaComboBox*)combo_box, model);
}

/** @} endgroup GedaComboBox-widget-getters-setters */

/** \defgroup GedaComboBox-widget-programmatic GedaComboBox Programmatic Control
 *  @{
 */

/*!
 * \brief Shows the menu or dropdown list of #GedaComboBox Widget
 * \par Function Description
 *  Pops up the menu or dropdown list of \a combo_box.
 *
 *  \param [in] combo_box a #GedaComboBox
 */
void geda_combo_widget_popup (GtkWidget *combo_box) {
  if (GEDA_IS_COMBO_BOX (combo_box)) {
    g_signal_emit (combo_box, combo_box_signals[POPUP], 0);
  }
}

/*!
 * \brief Hides the menu or dropdown list of #GedaComboBox Widget
 * \par Function Description
 *  This function is mostly intended for use by accessibility
 *  technologies; applications should have little use for it.
 */
void geda_combo_widget_popdown (GtkWidget *combo_box) {
  return geda_combo_box_popdown((GedaComboBox*)combo_box);
}

/** @} endgroup GedaComboBox-widget-programmatic */

/** \defgroup GedaComboBox-widget-atk GedaComboBox Widget Accessibility
 *  @{
 */
AtkObject *geda_combo_widget_get_popup_accessible (GtkWidget *combo_box)
{
  return geda_combo_box_get_popup_accessible((GedaComboBox*)combo_box);
}

/** @} endgroup GedaComboBox-widget-atk */
/** @} endgroup GedaComboBox-widget */
/** @} endgroup GedaComboBox-properties */
/** @} endgroup GedaComboBox */
