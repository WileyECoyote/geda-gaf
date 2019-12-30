/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_comboboxtext.c
 *
 * GTK - The GIMP Toolkit
 *
 * Copyright (C) 2010-2014 Christian Dywan
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
 * modifications, October 5th, 2013.
 *
 * This widget is like a the Gtk+3.7.4, except is linkable to 2.x
 * library, and the liststore embed id, (which was not really fully
 * implemented) was reduced to a simple counter varible. When a
 * string is added, the count is incremented, when a string is
 * removed the count is decremented. Also, for convenience, this
 * version has more protocol functions.
 * This version provides a xxx_combo_box_text_get_active_text, for
 * pre 2.6 libraries, so we don't have to selectively compile based
 * on the Gtk version.
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_container.h"
#include "../../include/geda_combobox.h"
#include "../../include/geda_comboboxtext.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaComboBoxText - A text-only combo box
 * \par
 * A GedaComboBoxText is a simple variant of GedaComboBox that hides the
 * model-view complexity for simple text-only use cases. To create a
 * GedaComboBoxText, use geda_combo_box_text_new() or
 * geda_combo_box_text_new_with_entry().
 * \par
 * You can add items to a GedaComboBoxText with geda_combo_box_text_append_text(),
 * geda_combo_box_text_insert_text() or geda_combo_box_text_prepend_text() and
 * remove options with geda_combo_box_text_remove().
 * \par
 * If the GedaComboBoxText contains an entry (via the 'has-entry' property),
 * its contents can be retrieved using geda_combo_box_text_get_active_text().
 * The entry itself can be accessed by calling gtk_bin_get_child() on the
 * combo box.
 * \par
 * You should not call geda_combo_box_set_model() or attempt to pack more cells
 * into this combo box via its GtkCellLayout interface.
 *
 * \brief GedaComboBoxText as GtkBuildable
 * \par
 * The GedaComboBoxText implementation of the GtkBuildable interface supports
 * adding items directly using the &lt;items&gt; element and specifying &lt;
 * item&gt; elements for each item. Each &lt;item&gt; element supports the
 * regular translation attributes "translatable", "context" and "comments".
 *
 * \defgroup GedaComboBoxText Combination Text Box
 * @{
 */

static void geda_combo_box_text_buildable_interface_init    (GtkBuildableIface *iface);
static bool geda_combo_box_text_buildable_custom_tag_start  (GtkBuildable      *buildable,
                                                             GtkBuilder        *builder,
                                                             GObject           *child,
                                                             const char        *tagname,
                                                             GMarkupParser     *parser,
                                                             void *          *data);

static void geda_combo_box_text_buildable_custom_finished   (GtkBuildable      *buildable,
                                                             GtkBuilder        *builder,
                                                             GObject           *child,
                                                             const char        *tagname,
                                                             void *           user_data);

static void *geda_combo_box_text_parent_class = NULL;

static GHashTable *combo_text_box_hash = NULL;

static GtkBuildableIface *buildable_parent_iface = NULL;

static void FixGtkCrap(GtkWidget *widget, void *combo)
{
  if (GTK_IS_BUTTON(widget)) {
    /* Unlike Gtk, we default to no focus for the internal button */
    gtk_widget_set_can_focus(widget, FALSE);
    /* And also unlike Gtk, we provide access to the internal button */
    (GEDA_COMBO_BOX_TEXT(combo))->button = widget;
  }
  else if (GEDA_IS_ENTRY(widget)) {
    (GEDA_COMBO_BOX_TEXT(combo))->entry = widget;
  }
}

static GObject *
geda_combo_box_text_constructor (GType                  type,
                                 unsigned int           n_construct_properties,
                                 GObjectConstructParam *construct_properties)
{
  GObject          *object;
  GedaComboBoxText *self;

  const int  text_column = 0;

  object = G_OBJECT_CLASS (geda_combo_box_text_parent_class)->constructor
                          (type, n_construct_properties, construct_properties);

  self = GEDA_COMBO_BOX_TEXT(object);

  geda_combo_box_set_entry_text_column (GEDA_COMBO_BOX (object), text_column);

  if (!geda_combo_box_get_has_entry (GEDA_COMBO_BOX (object))) {

    GtkCellRenderer *cell;

    cell = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (object), cell, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (object), cell,
                                    "text", text_column,
                                    NULL);
  }

  self->button = NULL;
  self->entry  = NULL;

  geda_container_forall (object, FixGtkCrap, object);

  return object;
}

/*! \internal object_class->finalize */
static void geda_combo_box_text_finalize  (GObject *object)
{
  GedaComboBoxText *self = GEDA_COMBO_BOX_TEXT(object);

  if (g_hash_table_remove (combo_text_box_hash, object)) {
    if (!g_hash_table_size (combo_text_box_hash)) {
      g_hash_table_destroy (combo_text_box_hash);
      combo_text_box_hash = NULL;
    }
  }

  gtk_cell_layout_clear(GTK_CELL_LAYOUT (object));

  g_object_unref (self->store);
  G_OBJECT_CLASS (geda_combo_box_text_parent_class)->finalize (object);
}

static void
geda_combo_box_text_buildable_interface_init (GtkBuildableIface *iface)
{
  buildable_parent_iface  = g_type_interface_peek_parent (iface);

  iface->custom_tag_start = geda_combo_box_text_buildable_custom_tag_start;
  iface->custom_finished  = geda_combo_box_text_buildable_custom_finished;
}

typedef struct {
  GtkBuilder    *builder;
  GObject       *object;
  const char    *domain;
  char          *id;
  GString       *string;
  char          *context;
  unsigned int   translatable : 1;
  unsigned int   is_text : 1;
} ItemParserData;

static void
item_start_element (GMarkupParseContext *context,
                    const char          *element_name,
                    const char         **names,
                    const char         **values,
                    void                *user_data,
                    GError             **error)
{
  ItemParserData *data = (ItemParserData*)user_data;

  if (strcmp (element_name, "item") == 0) {

    unsigned int i;

    data->is_text = TRUE;

    for (i = 0; names[i]; i++) {

      if (strcmp (names[i], "translatable") == 0) {

        if (values[i] != NULL) {

          int len = strlen(values[i]);

          if (gtk_builder_add_from_string (data->builder, values[i], len, error))
          {
            if (error) {
              g_clear_error (error);
            }
            return;
          }
        }
      }
      else if (strcmp (names[i], "comments") == 0) {

        /* do nothing, comments are for translators */
      }
      else if (strcmp (names[i], "context") == 0) {
        data->context = geda_strdup (values[i]);
      }
      else if (strcmp (names[i], "id") == 0) {
        data->id = geda_strdup (values[i]);
      }
      else {
        fprintf(stderr, "Unknown custom combo box attribute: %s", names[i]);
      }
    }
  }
}

static void item_text (GMarkupParseContext *context,
                       const char          *text,
                       size_t               text_len,
                       void                *user_data,
                       GError             **error)
{
  ItemParserData *data = (ItemParserData*)user_data;

  if (data->is_text) {
    g_string_append_len (data->string, text, text_len);
  }
}

static
char *geda_builder_parser_translate (const char *domain,
                                     const char *context,
                                     const char *text)
{
  const char *s;

  if (context)
    s = g_dpgettext2 (domain, context, text);
  else
    s = dgettext (domain, text);

  return geda_strdup (s);
}

static void item_end_element (GMarkupParseContext *context,
                              const char          *element_name,
                              void                *user_data,
                              GError             **error)
{
  ItemParserData *data = (ItemParserData*)user_data;

  /* Append the translated strings */
  if (data->string->len) {

    if (data->translatable) {

      char *translated;

      /* FIXME: This will not use the domain set in the .ui file, since
       * the parser is not telling the builder about the domain. However,
       * this will work for gtk_builder_set_translation_domain() calls.
       */
      translated = geda_builder_parser_translate (data->domain,
                                                  data->context,
                                                  data->string->str);
      g_string_set_size (data->string, 0);
      g_string_append (data->string, translated);
    }

    geda_combo_box_text_append (GEDA_COMBO_BOX_TEXT (data->object), data->string->str);
  }

  data->translatable = FALSE;
  g_string_set_size (data->string, 0);
  g_free (data->context);
  data->context = NULL;
  g_free (data->id);
  data->id = NULL;
  data->is_text = FALSE;
}

static const GMarkupParser item_parser =
{
  item_start_element,
  item_end_element,
  item_text
};

static bool
geda_combo_box_text_buildable_custom_tag_start (GtkBuildable   *buildable,
                                                GtkBuilder     *builder,
                                                GObject        *child,
                                                const char     *tagname,
                                                GMarkupParser  *parser,
                                                void          **data)
{
  if (buildable_parent_iface->custom_tag_start (buildable, builder, child,
                                                tagname,   parser,  data))
    return TRUE;

  if (strcmp (tagname, "items") == 0) {
      ItemParserData *parser_data;

      parser_data          = calloc (1, sizeof(ItemParserData));
      parser_data->builder = g_object_ref (builder);
      parser_data->object  = g_object_ref (G_OBJECT(buildable));
      parser_data->domain  = gtk_builder_get_translation_domain (builder);
      parser_data->string  = g_string_new ("");
     *parser               = item_parser;
     *data                 = parser_data;
      return TRUE;
    }
  return FALSE;
}

static void
geda_combo_box_text_buildable_custom_finished (GtkBuildable *buildable,
                                               GtkBuilder   *builder,
                                               GObject      *child,
                                               const char   *tagname,
                                               void         *user_data)
{
  buildable_parent_iface->custom_finished (buildable, builder, child,
                                           tagname, user_data);
  if (strcmp (tagname, "items") == 0) {

    ItemParserData *parser_data = (ItemParserData*)user_data;

    g_object_unref (parser_data->object);
    g_object_unref (parser_data->builder);
    g_string_free (parser_data->string, TRUE);
    free (parser_data);
  }
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
static void geda_combo_box_text_class_init (void *class, void *class_data)
{
  GObjectClass   *object_class;

  object_class = (GObjectClass*)class;

  object_class->constructor  = geda_combo_box_text_constructor;
  object_class->finalize     = geda_combo_box_text_finalize;

  geda_combo_box_text_parent_class = g_type_class_peek_parent (class);
}

/*!
 * \brief Initialize new GedaComboBoxText data structure instance.
 * \par Function Description
 *  This function is call after the GedaComboBoxTextClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaComboBoxText data structure
 * \param [in] class     A GedaComboBoxTextClass Object
 */
static void
geda_combo_box_text_instance_init (GTypeInstance *instance, void *class)
{
  GedaComboBoxText *combo_box;
  GtkListStore     *store;

  combo_box = (GedaComboBoxText*)instance;

  if (!combo_text_box_hash) {
    combo_text_box_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (combo_text_box_hash, instance, instance);

  store = gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

  geda_combo_box_set_model (GEDA_COMBO_BOX (combo_box), (GtkTreeModel*)store);
  combo_box->count = 0;
  combo_box->store = store;
}

/*!
 * \brief Retrieve GedaComboBoxText's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaComboBoxText Type identifier. When
 *  first called, the function registers a #GedaComboBoxText in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaComboBoxText and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaComboBoxText.
 */
GedaType geda_combo_box_text_get_type (void)
{
  static volatile GedaType geda_combo_box_text_type = 0;

  if (g_once_init_enter (&geda_combo_box_text_type)) {

    static const GTypeInfo info = {
      sizeof(GedaComboBoxTextClass),
      NULL,                               /* base_init           */
      NULL,                               /* base_finalize       */
      geda_combo_box_text_class_init,     /* (GClassInitFunc)    */
      NULL,                               /* class_finalize      */
      NULL,                               /* class_data          */
      sizeof(GedaComboBoxText),
      0,                                  /* n_preallocs         */
      geda_combo_box_text_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaComboBoxText");
    type   = g_type_register_static (GEDA_TYPE_COMBO_BOX, string, &info, 0);

    const GInterfaceInfo buildable_info = {
      (GInterfaceInitFunc) geda_combo_box_text_buildable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_BUILDABLE, &buildable_info);

    g_once_init_leave (&geda_combo_box_text_type, type);
  }

  return geda_combo_box_text_type;
}

/*!
 * \brief Check if an object is a GedaComboBoxText
 * \par Function Description
 *  Determines if \a combo_text_box is valid by verifying \a combo_text_box
 *  is included in the hash table of GedaComboBoxText objects.
 *
 * \return TRUE if \a combo_text_box is a valid GedaComboBoxText
 */
bool
is_a_geda_combo_box_text (GedaComboBoxText *combo_text_box)
{
  if ((combo_text_box != NULL) && (combo_text_box_hash != NULL)) {
    return g_hash_table_lookup(combo_text_box_hash, combo_text_box) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Create a New GedaComboBoxText
 * \par Function Description
 *  Creates a new #GedaComboBoxText, which is a GedaComboBox just
 *  displaying strings.
 *
 * \return new #GedaComboBoxText
 */
GtkWidget *geda_combo_box_text_new (void)
{
  return g_object_new (GEDA_TYPE_COMBO_BOX_TEXT, NULL);
}

/*!
 * \brief Create a New GedaComboBoxText with Entry
 * \par Function Description
 *  Creates a new #GedaComboBoxText, which is a GedaComboBox just displaying
 *  strings. The combo box created by this function has an entry.
 *
 * \return new #GedaComboBoxText
 */
GtkWidget *geda_combo_box_text_new_with_entry (void)
{
  return g_object_new (GEDA_TYPE_COMBO_BOX_TEXT, "has-entry", TRUE, NULL);
}

/*!
 * \brief Create a New GedaComboBoxText with Entry
 * \par Function Description
 * Creates a new #GedaComboBoxText, which is a GedaComboBox just displaying
 * strings. The combo box created by this function has an entry. The parent
 * GedaComboBox rebuilt the widget when the "list-view" property was set,
 * see geda_combo_box_check_appearance. So we have to find the widget again!
 *
 * \return new #GedaComboBoxText
 */
GtkWidget *geda_combo_box_text_list_new(void)
{
  GtkWidget *widget;

  widget = g_object_new (GEDA_TYPE_COMBO_BOX_TEXT, "has-entry", TRUE,
                                                   "list-view", TRUE, NULL);

  geda_container_forall (widget, FixGtkCrap, widget);

  return widget;
}

/*!
 * \internal GedaComboBoxText real insert
 *  Stores text, and optionally text2, in list store at position
 *
 *  Note: this function validates \a combo_box and text for the
 *        higher level functions.
 *
 * Called by:
 *             geda_combo_box_text_insert
 *             geda_combo_box_text_append
 *             geda_combo_box_text_prepend
 *             geda_combo_box_text_append_pair
 *             geda_combo_box_text_insert_pair
 *             geda_combo_box_text_prepend_pair
 *             geda_combo_box_text_append_text
 *             geda_combo_box_text_insert_text
 *             geda_combo_box_text_prepend_text
 *             geda_combo_box_text_set_active_text
 */
static void
geda_combo_box_text_real_insert (GedaComboBoxText *combo_box,
                                 int               position,
                                 const char       *text,
                                 const char       *text2)
{
  GtkTreeIter   iter;
  GtkListStore *store;

  /* No need to check widget here, Gtk will do that */
  g_return_if_fail (GEDA_IS_COMBO_BOX_TEXT (combo_box));
  g_return_if_fail (text != NULL);

  if (position < 0) {
    position = combo_box->count;
  }

  store = GTK_LIST_STORE (combo_box->store);

  gtk_list_store_insert (store, &iter, position);

  if (text2 != NULL){
    gtk_list_store_set (store, &iter, 0, text, 2, text2, -1);
  }
  else {
    gtk_list_store_set (store, &iter, 0, text, -1);
  }

  combo_box->count++;
}

/*!
 * \brief GedaComboBoxText Insert Text
 * \par Function Description
 *  Inserts \a text to the list of strings stored in combo_box at
 *  the given \a position.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer position where text is to be inserted
 * \param [in] text      Pointer to string to display.
 */
void geda_combo_box_text_insert (GedaComboBoxText *combo_box,
                                 int               position,
                                 const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, position, text, NULL);
}

/*!
 * \brief GedaComboBoxText Text Append
 * \par Function Description
 *  Appends text to the list of strings stored in combo_box.
 *  This is the same as calling geda_combo_box_text_insert() with a
 *  position of -1.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to the string to append.
 */
void geda_combo_box_text_append (GedaComboBoxText *combo_box, const char *text)
{
  geda_combo_box_text_real_insert (combo_box, -1, text, NULL);
}

/*!
 * \brief GedaComboBoxText Prepend Text
 * \par Function Description
 *  Prepends text to the list of strings stored in combo_box.
 *  This is the same as calling geda_combo_box_text_insert()
 *  with a position of 0.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to the string to be prepended.
 */
void geda_combo_box_text_prepend (GedaComboBoxText *combo_box, const char *text)
{
  geda_combo_box_text_real_insert (combo_box, 0, text, NULL);
}

/*!
 * \brief GedaComboBoxText Remove Text at Index Position
 * \par Function Description
 *  Removes the string at position from combo_box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer index of the item to remove.
 */
void geda_combo_box_text_remove (GedaComboBoxText *combo_box, int position)
{
  GtkTreeModel *model;
  GtkTreeIter   iter;

  g_return_if_fail (GEDA_IS_COMBO_BOX_TEXT (combo_box));
  g_return_if_fail (position >= 0);

  model = geda_combo_box_get_model (GEDA_COMBO_BOX (combo_box));

  if (gtk_tree_model_iter_nth_child (model, &iter, NULL, position)) {

    GtkListStore *store = GTK_LIST_STORE (model);

    if (GTK_IS_LIST_STORE (store)) {
      gtk_list_store_remove (store, &iter);
      combo_box->count--;
    }
  }
}

/*!
 * \brief GedaComboBoxText Remove All Text
 * \par Function Description
 *  Removes all the text entries from the combo box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 */
void geda_combo_box_text_remove_all (GedaComboBoxText *combo_box)
{
  g_return_if_fail (GEDA_IS_COMBO_BOX_TEXT (combo_box));

  gtk_list_store_clear (combo_box->store);

  combo_box->count = 0;
}

/*!
 * \brief Retrieve index of the Current GedaComboBoxText Active Item
 * \par Function Description
 *  Returns the index of the currently active item, or -1 if there is no
 *  active item.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 *
 * \sa geda_combo_box_get_active
 */
int geda_combo_box_text_get_active (GedaComboBoxText *combo_box)
{
  return geda_combo_box_get_active ((GedaComboBox*)combo_box);
}

/*!
 * \brief Set index of GedaComboBoxText Active Item
 * \par Function Description
 *  Sets the active item of \a combo_box to be the item at \a index.
 *
 * \param [in] combo_box A #GedaComboBoxText object
 * \param [in] position  Index in the model or -1 to have no active item
 *
 * \sa geda_combo_box_set_active
 */
void geda_combo_box_text_set_active (GedaComboBoxText *combo_box, int position)
{
  geda_combo_box_set_active((GedaComboBox*)combo_box, position);
}

/*!
 * \brief Append a Pair of Strings to a GedaComboBoxText object
 * \par Function Description
 *  This is  wrapper for geda_combo_box_text_real_insert, passing -1
 *  as the position to insert.
 *
 *  Note: When inserting pair, the pointer text is stored in the 0 column
 *        while text2 is stored in the 2 column.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to the string to append.
 * \param [in] text2     Pointer to the second string to append at the same row.
 *
 * \sa geda_combo_box_text_real_insert
 */
void geda_combo_box_text_append_pair (GedaComboBoxText *combo_box,
                                      const char       *text,
                                      const char       *text2)
{
  geda_combo_box_text_real_insert (combo_box, -1, text, text2);
}

/*!
 * \brief Insert a Pair of Strings to a GedaComboBoxText object
 * \par Function Description
 *  This is  wrapper for geda_combo_box_text_real_insert, passing all
 *  argument directly.
 *
 *  Note: When inserting pair, the pointer text is stored in the 0 column
 *        while text2 is stored in the 2 column.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Zero based index of where to insert.
 * \param [in] text      Pointer to the string to insert.
 * \param [in] text2     Pointer to the second string to insert at the same row.
 *
 * \sa geda_combo_box_text_real_insert
 */
void geda_combo_box_text_insert_pair (GedaComboBoxText *combo_box,
                                      int               position,
                                      const char       *text,
                                      const char       *text2)
{
  geda_combo_box_text_real_insert (combo_box, position, text, text2);
}

/*!
 * \brief Prepend a Pair of Strings to a GedaComboBoxText object
 * \par Function Description
 *  This is  wrapper for geda_combo_box_text_real_insert, passing 0 as
 *  the position to insert.
 *
 *  Note: When inserting pair, the pointer text is stored in the 0 column
 *        while text2 is stored in the 2 column.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to the string to insert.
 * \param [in] text2     Pointer to the second string to insert at the same row.
 *
 * \sa geda_combo_box_text_real_insert
 */
void geda_combo_box_text_prepend_pair (GedaComboBoxText *combo_box,
                                       const char       *text,
                                       const char       *text2)
{
  geda_combo_box_text_real_insert (combo_box, 0, text, text2);
}

/*!
 * \brief GedaComboBoxText Append Text
 * \par Function Description
 *  Appends text to the list of strings stored in combo_box.
 *  This is the same as calling geda_combo_box_text_insert() with a
 *  position of -1.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to string to be appended.
 *
 * \remark This function is equivalent to geda_combo_box_text_append
 */
void geda_combo_box_text_append_text (GedaComboBoxText *combo_box,
                                      const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, -1, text, NULL);
}

/*!
 * \brief GedaComboBoxText Insert Text
 * \par Function Description
 *  Inserts text at \a position in the list of strings stored in combo_box.
 *  If position is negative then text is appended.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer position where to insert text.
 * \param [in] text      Pointer to string to be inserted.
 *
 * \remark This function is equivalent to geda_combo_box_text_insert
 */
void geda_combo_box_text_insert_text (GedaComboBoxText *combo_box,
                                      int               position,
                                      const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, position, text, NULL);
}

/*!
 * \brief GedaComboBoxText Prepend Text
 * \par Function Description
 *  Prepends text to the list of strings stored in combo_box.
 *  This is the same as calling geda_combo_box_text_insert() with a
 *  position of 0.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to string to be prepended.
 *
 * \remark This function is equivalent to geda_combo_box_text_prepend
 */
void
geda_combo_box_text_prepend_text (GedaComboBoxText *combo_box,
                                  const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, 0, text, NULL);
}

/*!
 * \brief GedaComboBoxText Remove All Text
 * \par Function Description
 *  Removes all the text entries from the combo box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 *
 * \remark This function is equivalent to geda_combo_box_text_remove
 *         and exist for consistency of the API.
 */
void geda_combo_box_text_remove_all_text (GedaComboBoxText *combo_box)
{
  geda_combo_box_text_remove_all (combo_box);
}

/*!
 * \brief GedaComboBoxText Remove Text at Position
 * \par Function Description
 *  Removes the string at position from combo_box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer index of the text to be removed.
 */
void geda_combo_box_text_remove_index (GedaComboBoxText *combo_box,
                                       int               position)
{
  geda_combo_box_text_remove (combo_box, position);
}

/*!
 * \brief  Remove Text from GedaComboBoxText
 * \par Function Description
 *  Removes the given string from combo_box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      String to be removed.
 */
void geda_combo_box_text_remove_text (GedaComboBoxText *combo_box,
                                      const char       *text)
{
  if (GEDA_IS_COMBO_BOX_TEXT (combo_box)) {

    if (combo_box->count) { /* If not Empty? */

      GtkTreeModel *model;
      GtkTreeIter   iter;

      int text_column;
      int index;

      model       = geda_combo_box_get_model ((GedaComboBox*)combo_box);
      text_column = geda_combo_box_get_entry_text_column ((GedaComboBox*)combo_box);

      if (gtk_tree_model_get_iter_first (model, &iter)) {

        for (index = 0; index < combo_box->count; index++) {

          char *str = NULL;

          gtk_tree_model_get (model, &iter, text_column, &str, -1);

          if (str && (strcmp(text, str) == 0)) {
            geda_combo_box_text_remove (combo_box, index);
            g_free(str);
            break;
          }

          g_free(str);

          if (!gtk_tree_model_iter_next (model, &iter)) {
            break;
          }
        }
      }
    }
  }
}

/*!
 * \brief GedaComboBoxText Get Text
 * \par Function Description
 *  Returns the currently active string in Combo_box, or %NULL
 *  if none is selected. If combo_box contains an entry, this
 *  function will return its contents (which will not necessarily
 *  be an item from the list). This function is incoorperated
 *  into GetGedaComboActiveText macro.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 *
 * \returns a newly allocated string containing the currently
 *          active text. Must be freed with g_free().
 */
char *geda_combo_box_text_get_active_text (GedaComboBoxText *combo_box)
{
  char *text= NULL;

  if (GEDA_IS_COMBO_BOX_TEXT (combo_box)) {

    GedaComboBox *combo;
    GtkTreeIter iter;

    combo = GEDA_COMBO_BOX (combo_box);

    if (geda_combo_box_get_has_entry (combo)) {

      GtkWidget *entry;

      entry = gtk_bin_get_child (GTK_BIN (combo_box));
      text = geda_strdup (geda_entry_get_text (GEDA_ENTRY (entry)));
    }
    else if (geda_combo_box_get_active_iter (combo, &iter)) {

      GtkTreeModel *model;
      int text_column;
      int column_type;

      model = geda_combo_box_get_model (combo);
      text_column = geda_combo_box_get_entry_text_column (combo);

      g_return_val_if_fail (text_column >= 0, NULL);

      column_type = gtk_tree_model_get_column_type (model, text_column);

      g_return_val_if_fail (column_type == G_TYPE_STRING, NULL);
      gtk_tree_model_get (model, &iter, text_column, &text, -1);
    }
  }
  return text;
}

/*!
 * \brief Set the active text in a GedaComboBoxText
 * \par Function Description
 *  Sets \a text to be the active text in the \a combo_box. If the
 *  text is not already a member of \a combo_box, the text will be
 *  inserted as the first member.
 */
bool geda_combo_box_text_set_active_text (GedaComboBoxText *combo_box,
                                          const char       *text)
{
  bool added = 0;

  if (GEDA_IS_COMBO_BOX_TEXT (combo_box)) {

    if (!combo_box->count) { /* Is Empty? */

      if (GEDA_IS_ENTRY(combo_box->entry)) {
        geda_entry_set_text((GedaEntry*)combo_box->entry,text);
      }
      else {
        geda_combo_box_text_real_insert (combo_box, 0, text, NULL);
        added = 1;
      }
    }
    else {

      GtkTreeModel *model;
      GtkTreeIter   iter;

      int text_column;
      int found;

      model       = geda_combo_box_get_model (GEDA_COMBO_BOX (combo_box));
      text_column = geda_combo_box_get_entry_text_column (GEDA_COMBO_BOX (combo_box));
      found       = 0;

      if (gtk_tree_model_get_iter_first (model, &iter)) {

        int i;

        for (i = 0; i < combo_box->count; i++) {

          char *str = NULL;

          gtk_tree_model_get (model, &iter, text_column, &str, -1);

          if (str && (strcmp(text, str) == 0)) {
            found = i;
            g_free(str);
            break;
          }

          g_free(str);

          if (!gtk_tree_model_iter_next (model, &iter)) {
            break;
          }
        }
      }

      if (found) {
        geda_combo_box_text_set_active (combo_box, found);
      }
      else {
        if (GEDA_IS_ENTRY(combo_box->entry)) {
          geda_entry_set_text((GedaEntry*)combo_box->entry,text);
        }
        else {
          geda_combo_box_text_real_insert (combo_box, 0, text, NULL);
          geda_combo_box_text_set_active(combo_box, 0);
          added = 1;
        }
      }
    }
  }

  return added;
}

/*!
 * \brief Get the GedaComboBoxText Activate Default property
 * \par Function Description
 *  Returns the value of the "activates-default" property of the
 *  child entry widget or FALSE if \a combo_box does not have an
 *  entry widget.
 */
bool geda_combo_box_text_get_activate_default (GedaComboBoxText *combo_box)
{
  if (GEDA_IS_COMBO_BOX_TEXT (combo_box)) {

    if (GEDA_IS_ENTRY(combo_box->entry)) {
      return geda_entry_get_activates_default (GEDA_ENTRY(combo_box->entry));
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaComboBox");
  }
  return FALSE;
}

/*!
 * \brief Set the GedaComboBoxText activate_default property
 * \par Function Description
 *  Sets the "activates-default" property of the child entry widget
 *  to \a setting. If the GedaComboBoxText has no entry the call is
 *  is ignored.
 */
void geda_combo_box_text_set_activate_default (GedaComboBoxText *combo_box, bool setting)
{
  if (GEDA_IS_COMBO_BOX_TEXT (combo_box)) {

    if (GEDA_IS_ENTRY(combo_box->entry)) {
      geda_entry_set_activates_default (GEDA_ENTRY(combo_box->entry), setting);
    }
  }
  else {
    BUG_MSG ("Operative is not a GedaComboBox");
  }
}

/*!
 * \brief Retrieve the GedaComboBoxText Entry
 * \par Function Description
 *  This function is the same as geda_combo_get_entry_widget but accept a
 *  #GedaComboBoxText as the argument. The returned widget is a GedaEntry
 *  object.
 */
GedaEntry *geda_combo_box_text_get_entry (GedaComboBoxText *combo_box)
{
  GtkWidget *widget = geda_combo_get_entry_widget((GedaComboBox*)combo_box);

  if (widget) {
    return GEDA_ENTRY(widget);
  }
  return NULL;
}

/*!
 * \brief Get the Entry Widget from a GedaComboBoxText
 * \par Function Description
 *  Returns the entry or NULL if \a combo_box has no entry,
 *  this function calls geda_combo_get_entry_widget but
 *  accepts a GedaComboBoxText as the argument.
 *
 * \sa geda_combo_get_entry_widget
 */
GtkWidget *geda_combo_box_text_get_entry_widget (GedaComboBoxText *combo_box)
{
  return geda_combo_get_entry_widget((GedaComboBox*)combo_box);
}

/*!
 * \brief Get the GedaComboBoxText Text Length
 * \par Function Description
 * Returns the length of the active text or o if \a combo_box is
 * invalid.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 */
int geda_combo_box_text_get_text_length (GedaComboBoxText *combo_box)
{
  GedaEntry *entry;
  int length;

  entry = geda_combo_get_entry((GedaComboBox*)combo_box);

  if (entry) {
    length = geda_entry_get_text_length(entry);
  }
  else {

    char *text;

    text = geda_combo_box_text_get_active_text(combo_box);

    if (text) {
      length = strlen(text);
      g_free(text);
    }
    else {
      length = 0;
    }
  }

  return length;
}

/* These are probably more practical, but have longer names */

/*!
 * \brief Append Text to a GedaComboBoxText widget
 * \par Function Description
 *  Appends text to the list of strings stored in combo_box. This
 *  is the same as calling geda_combo_box_text_widget_insert with
 *  a position of -1.
 *
 * \param [in] widget A #GedaComboBoxText widget.
 * \param [in] text   Pointer to the string to append.
 */
void geda_combo_box_text_widget_append (GtkWidget *widget, const char *text)
{
  geda_combo_box_text_real_insert ((GedaComboBoxText*)widget, -1, text, NULL);
}

/*!
 * \brief Insert Text into a GedaComboBoxText widget
 * \par Function Description
 *  Widget version of geda_combo_box_text_insert.
 *
 * \param [in] widget    A #GedaComboBoxText widget.
 * \param [in] position  Zero based index of where to insert the text.
 * \param [in] text      Pointer to the string to append.
 */
void geda_combo_box_text_widget_insert (GtkWidget  *widget, int position,
                                        const char *text)
{
  geda_combo_box_text_real_insert ((GedaComboBoxText*)widget, position, text, NULL);
}

/*!
 * \brief Prepend Text to GedaComboBoxText Widget
 * \par Function Description
 *  Prepends text to the list of strings stored in combo_box. This
 *  is the same as calling geda_combo_box_text_widget_insert with
 *  a position of 0.
 *
 * \param [in] widget A #GedaComboBoxText widget.
 * \param [in] text   Pointer to the string to prepend.
 */
void geda_combo_box_text_widget_prepend (GtkWidget *widget, const char *text)
{
  geda_combo_box_text_real_insert ((GedaComboBoxText*)widget, 0, text, NULL);
}

/*!
 * \brief Remove String from GedaComboBoxText Widget given Position
 * \par Function Description
 *  Removes the string at the given position from the combo_box widget.
 *
 * \param [in] widget    A #GedaComboBoxText widget.
 * \param [in] position  Base zero position of the string to remove.
 */
void geda_combo_box_text_widget_remove (GtkWidget *widget, int position)
{
  geda_combo_box_text_remove (GEDA_COMBO_BOX_TEXT(widget), position);
}

/*!
 * \brief Remove All Text Strings from a GedaComboBoxText Widget
 * \par Function Description
 *  Removes all the text entries from the combo box widget.
 *
 * \param [in] widget A #GedaComboBoxText widget.
 */
void geda_combo_box_text_widget_remove_all (GtkWidget *widget)
{
  geda_combo_box_text_remove_all (GEDA_COMBO_BOX_TEXT(widget));
}

/*!
 * \brief Set GedaComboBoxText Widget index Active Item
 * \par Function Description
 *  Sets the item of \a combo_box at \a position to be the active item.
 *
 * \param [in] widget    A #GedaComboBoxText widget
 * \param [in] position  Index in the model or -1 to have no active item
 *
 * \sa geda_combo_box_set_active
 */
void geda_combo_box_text_widget_set_active (GtkWidget *widget, int position)
{
  geda_combo_box_set_active((GedaComboBox*)widget, position);
}

/*!
 * \brief Get Active #GedaComboBoxText Widget item
 * \par Function Description
 *  Returns the index of the currently active item, or -1 if there's no
 *  active item. This is the same as calling geda_combo_box_get_active
 *  but accepts a widget as an argument.
 *
 * \return An integer value which is the index of the currently active item,
 *         or -1 if there's no active item.
 *
 * \param [in] widget A #GedaComboBoxText widget.
 *
 * \see geda_combo_box_get_active
 */
int geda_combo_box_text_widget_get_active (GtkWidget *widget)
{
  return geda_combo_box_get_active ((GedaComboBox*)widget);
}

/*!
 * \brief Get GedaComboBoxText Widget Text
 * \par Function Description
 *  Returns the currently active string in Combo_box, or %NULL
 *  if none is selected.
 *
 * \param [in] widget A #GedaComboBoxText object.
 *
 * \returns a newly allocated string containing the currently
 *          active text should be freed with g_free.
 *
 * \sa geda_combo_box_text_get_active_text
 */
char *geda_combo_box_text_widget_get_active_text(GtkWidget *widget)
{
  return geda_combo_box_text_get_active_text (GEDA_COMBO_BOX_TEXT(widget));
}

/*!
 * \brief Set the active text in a GedaComboBoxText
 * \par Function Description
 *  Sets \a text to be the active text in the \a combo_box. If the
 *  text is not already a member of \a combo_box, the text will be
 *  inserted as the first member.
 *
 * \param [in] widget A #GedaComboBoxText widget
 * \param [in] text   Pointer to string to display.
 */
bool geda_combo_box_text_widget_set_active_text(GtkWidget *widget, const char *text)
{
  return geda_combo_box_text_set_active_text (GEDA_COMBO_BOX_TEXT(widget), text);
}


bool geda_combo_box_text_widget_get_activate_default (GtkWidget *widget)
{
  return geda_combo_box_text_get_activate_default ((GedaComboBoxText*)widget);
}

void geda_combo_box_text_widget_set_activate_default (GtkWidget *widget,
                                                      bool       setting)
{
  geda_combo_box_text_set_activate_default ((GedaComboBoxText*)widget, setting);
}

GedaEntry *geda_combo_box_text_widget_get_entry (GtkWidget *widget)
{
  return geda_combo_box_text_get_entry ((GedaComboBoxText*)widget);
}

/*!
 * \brief Get the Text Length from a GedaComboBoxText Widget
 * \par Function Description
 *  This is a convenience wrapper for geda_combo_box_text_get_text_length
 *  that accepts a widget as an argument. The function returns the length
 *  of the active text or o if \a combo_box is invalid.
 *
 * \param [in] widget A #GedaComboBoxText widget.
 */
int geda_combo_box_text_widget_get_text_length(GtkWidget *widget)
{
  return geda_combo_box_text_get_text_length ((GedaComboBoxText*)widget);
}

/** @} end group GedaComboBoxText */

