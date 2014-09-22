/* GTK - The GIMP Toolkit
 *
 * Copyright (C) 2010-2014 Christian Dywan
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
 * Boston, MA 02111-1307, US, <http://www.gnu.org/licenses/>.
 *
 * THIS FILE IS LGPL LICENSED, gEDA AS A WHOLE IS GPL LICENSED
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

#include "config.h"
#include <geda.h>

#include <gtk/gtk.h>

#include <string.h>

#include <geda_combobox.h>
#include <geda_comboboxtext.h>

#include "gettext.h"

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

static GtkBuildableIface *buildable_parent_iface = NULL;

void g_type_ensure(GedaType type){return;};

G_DEFINE_TYPE_WITH_CODE (GedaComboBoxText, geda_combo_box_text, GEDA_TYPE_COMBO_BOX,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_BUILDABLE,
                         geda_combo_box_text_buildable_interface_init));

static void FixGtkCrap(GtkWidget *widget, void *combo)
{
  if (GTK_IS_BUTTON(widget)) {
    /* Unlike Gtk, we default to no focus for the internal button */
    gtk_widget_set_can_focus(widget, FALSE);
    /* And also unlike Gtk, we provide access to the internal button */
    (GEDA_COMBO_BOX_TEXT(combo))->button = widget;
  }
  else if (GTK_IS_ENTRY(widget)) {
    (GEDA_COMBO_BOX_TEXT(combo))->entry = widget;
  }
}

static GObject *
geda_combo_box_text_constructor (GedaType               type,
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

  gtk_container_forall (GTK_CONTAINER (object), FixGtkCrap, object);

  return object;
}

static void geda_combo_box_text_finalize  (GObject *object)
{
  GedaComboBoxText *self = GEDA_COMBO_BOX_TEXT(object);

  g_object_unref (self->store);
  G_OBJECT_CLASS (geda_combo_box_text_parent_class)->finalize (object);
}

static void
geda_combo_box_text_init (GedaComboBoxText *combo_box)
{
  GtkListStore *store;

  store  = gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  geda_combo_box_set_model (GEDA_COMBO_BOX (combo_box), GTK_TREE_MODEL (store));
  combo_box->count = 0;
  combo_box->store = store;

}

static void
geda_combo_box_text_class_init (GedaComboBoxTextClass *klass)
{
  GObjectClass   *object_class;

  object_class = (GObjectClass*)klass;

  object_class->constructor  = geda_combo_box_text_constructor;
  object_class->finalize     = geda_combo_box_text_finalize;
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
                    void *             user_data,
                    GError             **error)
{
  ItemParserData *data = (ItemParserData*)user_data;
  unsigned int i;

  if (strcmp (element_name, "item") == 0)
  {
    data->is_text = TRUE;

    for (i = 0; names[i]; i++)
    {
      if (strcmp (names[i], "translatable") == 0)
      {
        if (values[i] != NULL) {
          int len = strlen(values[i]);
          if (gtk_builder_add_from_string (data->builder, values[i], len, error)) {
            if (error) {
              g_clear_error (error);
            }
            return;
          }
        }
      }
      else if (strcmp (names[i], "comments") == 0)
      {
        /* do nothing, comments are for translators */
      }
      else if (strcmp (names[i], "context") == 0)
        data->context = g_strdup (values[i]);
      else if (strcmp (names[i], "id") == 0)
        data->id = g_strdup (values[i]);
      else
        g_warning ("Unknown custom combo box item attribute: %s", names[i]);
    }
  }
}

static void item_text (GMarkupParseContext *context,
                       const char          *text,
                       unsigned int         text_len,
                       void                *user_data,
                       GError             **error)
{
  ItemParserData *data = (ItemParserData*)user_data;

  if (data->is_text)
    g_string_append_len (data->string, text, text_len);
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

  return g_strdup (s);
}
static void item_end_element (GMarkupParseContext *context,
                              const char          *element_name,
                              void                *user_data,
                              GError             **error)
{
  ItemParserData *data = (ItemParserData*)user_data;

  /* Append the translated strings */
  if (data->string->len)
    {
      if (data->translatable)
      {
        char *translated;

        /* FIXME: This will not use the domain set in the .ui file,
         * since the parser is not telling the builder about the domain.
         * However, it will work for gtk_builder_set_translation_domain() calls.
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
geda_combo_box_text_buildable_custom_tag_start (GtkBuildable     *buildable,
                                                GtkBuilder       *builder,
                                                GObject          *child,
                                                const char      *tagname,
                                                GMarkupParser    *parser,
                                                void *         *data)
{
  if (buildable_parent_iface->custom_tag_start (buildable, builder, child,
                                                tagname,   parser,  data))
    return TRUE;

  if (strcmp (tagname, "items") == 0) {
      ItemParserData *parser_data;

      parser_data          = g_slice_new0 (ItemParserData);
      parser_data->builder = g_object_ref (builder);
      parser_data->object  = g_object_ref (buildable);
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
  ItemParserData *data;

  buildable_parent_iface->custom_finished (buildable, builder, child,
                                           tagname, user_data);

  if (strcmp (tagname, "items") == 0) {

    data = (ItemParserData*)user_data;

    g_object_unref (data->object);
    g_object_unref (data->builder);
    g_string_free (data->string, TRUE);
    g_slice_free (ItemParserData, data);
  }
}

/*! \brief Create a New GedaComboBoxText
 *  \par Function Description
 *
 * Creates a new #GedaComboBoxText, which is a GedaComboBox just
 * displaying strings.
 *
 * \return new #GedaComboBoxText
 *
 */
GtkWidget *geda_combo_box_text_new ()
{
  return g_object_new (GEDA_TYPE_COMBO_BOX_TEXT, NULL);
}

/*! \brief Create a New GedaComboBoxText with Entry
 *  \par Function Description
 *
 * Creates a new #GedaComboBoxText, which is a GedaComboBox just displaying
 * strings. The combo box created by this function has an entry.
 *
 * \return new #GedaComboBoxText
 *
 */
GtkWidget *geda_combo_box_text_new_with_entry ()
{
  return g_object_new (GEDA_TYPE_COMBO_BOX_TEXT, "has-entry", TRUE, NULL);
}

/*! \brief Create a New GedaComboBoxText with Entry
 *  \par Function Description
 *
 * Creates a new #GedaComboBoxText, which is a GedaComboBox just displaying
 * strings. The combo box created by this function has an entry.
 *
 * \return new #GedaComboBoxText
 *
 */
GtkWidget *geda_combo_box_text_list_new()
{
  return g_object_new (GEDA_TYPE_COMBO_BOX_TEXT, "has-entry", TRUE,
                                                 "appear-as-list", TRUE,
                       NULL);
}

static void
geda_combo_box_text_real_insert (GedaComboBoxText *combo_box,
                                 int               position,
                                 const char       *text,
                                 const char       *text2)
{
  GtkTreeIter   iter;
  GtkListStore *store;

  if (position < 0)
    position = combo_box->count;

  /* No need to check widget here, Gtk will do that */
  g_return_if_fail (GEDA_IS_COMBO_BOX_TEXT (combo_box));
  g_return_if_fail (text != NULL);

  store = GTK_LIST_STORE (combo_box->store);

  gtk_list_store_insert (store, &iter, position);

  if (text2 != NULL)
    gtk_list_store_set (store, &iter, 0, text, 2, text2, -1);
  else {
    gtk_list_store_set (store, &iter, 0, text, -1);
  }

  combo_box->count++;
}

/*! \brief GedaComboBoxText Text Append
 *  \par Function Description
 *
 * Appends text to the list of strings stored in combo_box.
 *
 * This is the same as calling geda_combo_box_text_insert() with a
 * position of -1.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer position where text is to be inserted
 * \param [in] text      Pointer to string to display.
 */
void
geda_combo_box_text_insert (GedaComboBoxText *combo_box,
                            int               position,
                            const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, position, text, NULL);
}

/*! \brief GedaComboBoxText Text Append
 *  \par Function Description
 *
 * Appends text to the list of strings stored in combo_box.
 *
 * This is the same as calling geda_combo_box_text_insert() with a
 * position of -1.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to string to display.
 */
void
geda_combo_box_text_append (GedaComboBoxText *combo_box,
                            const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, -1, text, NULL);
}


/*! \brief GedaComboBoxText Prepend Text
 *  \par Function Description
 *
 * Prepends text to the list of strings stored in combo_box.
 *
 * This is the same as calling geda_combo_box_text_insert()
 * with a position of 0.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to string to be prepended.
 */
void
geda_combo_box_text_prepend (GedaComboBoxText *combo_box,
                             const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, 0, text, NULL);
}

/*! \brief GedaComboBoxText Remove Text at Index Position
 *  \par Function Description
 *
 * Removes the string at position from combo_box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer index of the item to remove.
 */
void
geda_combo_box_text_remove (GedaComboBoxText *combo_box,
                            int               position)
{
  GtkTreeModel *model;
  GtkListStore *store;
  GtkTreeIter   iter;

  g_return_if_fail (GEDA_IS_COMBO_BOX_TEXT (combo_box));
  g_return_if_fail (position >= 0);

  model = geda_combo_box_get_model (GEDA_COMBO_BOX (combo_box));

  store = GTK_LIST_STORE (model);
  g_return_if_fail (GTK_IS_LIST_STORE (store));

  if (gtk_tree_model_iter_nth_child (model, &iter, NULL, position))
    gtk_list_store_remove (store, &iter);

  combo_box->count--;

}

/*! \brief GedaComboBoxText Remove All Text
 *  \par Function Description
 *
 * Removes all the text entries from the combo box.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 */
void
geda_combo_box_text_remove_all (GedaComboBoxText *combo_box)
{
  GtkListStore *store;

  g_return_if_fail (GEDA_IS_COMBO_BOX_TEXT (combo_box));

  store = GTK_LIST_STORE (geda_combo_box_get_model (GEDA_COMBO_BOX (combo_box)));
  gtk_list_store_clear (store);
  combo_box->count = 0;
}

void geda_combo_box_text_list_append (GedaComboBoxText   *combo_box,
                                      const char         *text,
                                      const char         *text2)
{
  geda_combo_box_text_real_insert (combo_box, -1, text, text2);
}
void geda_combo_box_text_list_insert (GedaComboBoxText   *combo_box,
                                      int                 position,
                                      const char         *text,
                                      const char         *text2)
{
  geda_combo_box_text_real_insert (combo_box, position, text, text2);
}
void geda_combo_box_text_list_prepend (GedaComboBoxText   *combo_box,
                                       const char         *text,
                                       const char         *text2)
{
  geda_combo_box_text_real_insert (combo_box, 0, text, text2);
}

/*! \brief GedaComboBoxText Append Text
 *  \par Function Description
 *
 * Appends text to the list of strings stored in combo_box.
 *
 * This is the same as calling geda_combo_box_text_insert() with a
 * position of -1.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to string to be appended.
 */
void
geda_combo_box_text_append_text (GedaComboBoxText *combo_box,
                                 const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, -1, text, NULL);
}

/*! \brief GedaComboBoxText Insert Text
 *  \par Function Description
 *
 * Inserts text at position in the list of strings stored in combo_box.
 *
 * If position is negative then text is appended.
 *
 * This is the same as calling geda_combo_box_text_insert() with a %NULL
 * ID string.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] position  Integer position where to insert text.
 * \param [in] text      Pointer to string to be inserted.
 */
void
geda_combo_box_text_insert_text (GedaComboBoxText *combo_box,
                                 int                position,
                                 const char        *text)
{
  geda_combo_box_text_real_insert (combo_box, position, text, NULL);
}

/*! \brief GedaComboBoxText Prepend Text
 *  \par Function Description
 *
 * Prepends text to the list of strings stored in combo_box.
 *
 * This is the same as calling geda_combo_box_text_insert() with a
 * position of 0.
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 * \param [in] text      Pointer to string to be prepended.
 *
 */
void
geda_combo_box_text_prepend_text (GedaComboBoxText *combo_box,
                                  const char       *text)
{
  geda_combo_box_text_real_insert (combo_box, 0, text, NULL);
}

void geda_combo_box_text_remove_text (GedaComboBoxText *combo_box,
                                      int               position)
{
  geda_combo_box_text_remove (combo_box, position);
}

void geda_combo_box_text_remove_all_text (GedaComboBoxText *combo_box)
{
  geda_combo_box_text_remove_all (combo_box);
}

/*! \brief GedaComboBoxText Get Text
 *  \par Function Description
 *
 * Returns the currently active string in Combo_box, or %NULL
 * if none is selected. If combo_box contains an entry, this
 * function will return its contents (which will not necessarily
 * be an item from the list).
 *
 * \param [in] combo_box A #GedaComboBoxText object.
 *
 * Returns: a newly allocated string containing the currently
 *          active text. Must be freed with g_free().
 */
char *
geda_combo_box_text_get_active_text (GedaComboBoxText *combo_box)
{
  GtkTreeIter iter;
  char *text = NULL;

  if (GEDA_IS_COMBO_BOX_TEXT (combo_box)) {

    if (geda_combo_box_get_has_entry (GEDA_COMBO_BOX (combo_box)))
    {
      GtkWidget *entry;

      entry = gtk_bin_get_child (GTK_BIN (combo_box));
      text = g_strdup (gtk_entry_get_text (GTK_ENTRY (entry)));
    }
    else if (geda_combo_box_get_active_iter (GEDA_COMBO_BOX (combo_box), &iter))
    {
      GtkTreeModel *model;
      int text_column;
      int column_type;

      model = geda_combo_box_get_model (GEDA_COMBO_BOX (combo_box));
      text_column = geda_combo_box_get_entry_text_column (GEDA_COMBO_BOX (combo_box));
      g_return_val_if_fail (text_column >= 0, NULL);
      column_type = gtk_tree_model_get_column_type (model, text_column);
      g_return_val_if_fail (column_type == G_TYPE_STRING, NULL);
      gtk_tree_model_get (model, &iter, text_column, &text, -1);
    }

    return text;
  }
  else
    return NULL;
}

void
geda_combo_box_text_set_active (GedaComboBoxText *combo_box, int position)
{
  geda_combo_box_set_active((GedaComboBox*)combo_box, position);
}

int geda_combo_box_text_get_active (GedaComboBoxText *combo_box)
{
  return geda_combo_box_get_active ((GedaComboBox*)combo_box);
}

void
geda_combo_box_text_set_activate_default (GedaComboBoxText *combo_box, bool setting)
{
  if (GEDA_COMBO_BOX_TEXT (combo_box)) {

    if (GTK_IS_ENTRY(combo_box->entry)) {
      gtk_entry_set_activates_default (GTK_ENTRY(combo_box->entry), setting);
    }
  }
}


GtkWidget*
geda_combo_box_text_get_entry (GedaComboBoxText *combo_box)
{
  GtkWidget *entry = NULL;

  if (GEDA_COMBO_BOX_TEXT (combo_box)) {
    if (GTK_IS_ENTRY(combo_box->entry)) {
      entry = combo_box->entry;
    }
  }
  return entry;
}

/* These are probably more practical, but have longer names */
void
geda_combo_box_text_widget_append (GtkWidget *widget, const char *text)
{
  geda_combo_box_text_insert (GEDA_COMBO_BOX_TEXT(widget), -1, text);
}

void
geda_combo_box_text_widget_insert (GtkWidget  *widget, int position,
                                   const char *text)
{
  geda_combo_box_text_insert (GEDA_COMBO_BOX_TEXT(widget), position, text);
}
void geda_combo_box_text_widget_prepend (GtkWidget *widget, const char *text)
{
  geda_combo_box_text_insert (GEDA_COMBO_BOX_TEXT(widget), 0, text);
}
void geda_combo_box_text_widget_remove (GtkWidget *widget, int position)
{
  geda_combo_box_text_remove (GEDA_COMBO_BOX_TEXT(widget), position );
}
void geda_combo_box_text_widget_set_active (GtkWidget *widget, int position)
{
  geda_combo_box_set_active((GedaComboBox*)widget, position);
}
int geda_combo_box_text_widget_get_active (GtkWidget *widget)
{
  return geda_combo_box_get_active ((GedaComboBox*)widget);
}
/** @} end group GedaComboBoxText */