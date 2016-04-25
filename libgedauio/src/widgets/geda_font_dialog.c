/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_font_dialog
 *
 * GTK - The GIMP Toolkit
 *
 * Copyright (C) 2011-2015 Alberto Ruiz <aruiz@gnome.org>
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
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include <atk/atk.h>
#include <gtk/gtk.h>

#include "geda_label.h"
#include "geda_font_dialog.h"
#include "geda_entry.h"

#include "gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaFontDialog - A Widget for setting Font Parameters
 * \par
 * A GedaFontDialog is a Dialog object used to allow users to select font and
 * set font parameters such as the font size and style.
 *
 * \defgroup GedaFontDialog Font Dialog
 * @{
 */

enum {
   PROP_0,
   PROP_TITLE,
   PROP_FONT,
   PROP_FONT_DESC,
   PROP_FONT_NAME,
   PROP_FONT_SIZE,
   PROP_PREVIEW_TEXT,
   PROP_SHOW_PREVIEW
};


enum {
  FAMILY_COLUMN,
  FAMILY_NAME_COLUMN
};

enum {
  FACE_COLUMN,
  FACE_NAME_COLUMN
};

enum {
  SIZE_COLUMN
};

/* These are what we use as the standard font sizes, for the size list.
 */
static const unsigned int font_sizes[] = {
  6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28,
  32, 36, 40, 48, 56, 64, 72, 96
};

static GObjectClass *geda_font_dialog_parent_class = NULL;

static PangoFontDescription*
geda_font_dialog_get_font_description (GedaFontDialog *dialog)
{
  g_return_val_if_fail ( GEDA_IS_FONT_DIALOG (dialog), NULL);

  if ( !dialog->font_desc) {
    if (dialog->face) {
      dialog->font_desc = pango_font_face_describe (dialog->face);
      pango_font_description_set_size (dialog->font_desc, dialog->font_size);
    }
    else {
      if (dialog->default_font) {
        dialog->font_desc = pango_font_description_from_string (dialog->default_font);
      }
      else {
        dialog->font_desc = pango_font_description_from_string (DEFAULT_FONT_NAME);
      }
    }
  }

  return dialog->font_desc;
}

static void
geda_font_dialog_change_entry_map (GedaFontDialog *dialog, GtkWidget *widget)
{
  PangoContext *context;
  PangoLayout  *layout;
  PangoFontDescription *desc;

  layout  = gtk_entry_get_layout ( (GtkEntry*) widget);
  context = pango_layout_get_context (layout);
  desc    = pango_font_description_copy (dialog->font_desc);

  pango_context_set_font_map (context, dialog->font_map);

  pango_font_description_set_size (desc, dialog->font_size * PANGO_SCALE);
  pango_context_set_font_description (context, desc);
  pango_layout_context_changed (layout);

  gtk_widget_queue_resize(widget);
  pango_font_description_free(desc);

}
/* This sets the font in the preview entry to the selected font, and tries to
   make sure that the preview entry is a reasonable size, i.e. so that the
   text can be seen with a bit of space to spare. But it tries to avoid
   resizing the entry every time the font changes.
   This also used to shrink the preview if the font size was decreased, but
   that made it awkward if the user wanted to resize the window themself. */
static void
geda_font_dialog_update_preview (GedaFontDialog *dialog)
{
  int new_height;
  GtkRequisition old_requisition;
  GtkWidget *preview_entry;
  const char *text;

  preview_entry = dialog->preview_entry;

  if ( gtk_widget_has_screen (preview_entry)) {

    geda_font_dialog_change_entry_map (dialog, preview_entry);

    gtk_widget_size_request (preview_entry, NULL);
    gtk_widget_get_child_requisition (preview_entry, &old_requisition);

    /* We don't ever want to be over MAX_PREVIEW_HEIGHT pixels high. */
    new_height = CLAMP (preview_entry->requisition.height, INITIAL_PREVIEW_HEIGHT, MAX_PREVIEW_HEIGHT);

    if (new_height > old_requisition.height || new_height < old_requisition.height - 30)
      gtk_widget_set_size_request (preview_entry, -1, new_height);

    /* This sets the preview text, if it hasn't been set already. */
    text = gtk_entry_get_text (GTK_ENTRY (preview_entry));

    if (strlen (text) == 0) {
      gtk_entry_set_text (GTK_ENTRY (dialog->preview_entry),
                          pango_language_get_sample_string (NULL));
    }

    gtk_editable_set_position (GTK_EDITABLE (preview_entry), 0);
  }
}

static void
callback_update_preview (GtkWidget *entry,
                                  GedaFontDialog *dialog)
{
  g_object_notify (G_OBJECT (dialog), "preview-text");
}

static void
geda_font_dialog_take_font_desc (GedaFontDialog *dialog,
                                 PangoFontDescription *new_desc)
{
  PangoFontMask mask;
  PangoFontDescription *curr_desc;
  bool changed;

  g_return_if_fail ( GEDA_IS_FONT_DIALOG (dialog));
  g_return_if_fail ( new_desc == NULL );

  changed = FALSE;
  if ( dialog->font_desc == NULL ) {
    curr_desc = geda_font_dialog_get_font_description (dialog);
    changed = TRUE;
  }
  else {
    curr_desc = dialog->font_desc;
  }

  mask = pango_font_description_get_set_fields (new_desc);

  if (mask & PANGO_FONT_MASK_WEIGHT) {

    PangoWeight weight;

    if ( (weight = pango_font_description_get_weight (new_desc)) !=
                   pango_font_description_get_weight (curr_desc)) {
      pango_font_description_set_weight (curr_desc, weight);
      changed = TRUE;
    }
  }

  if (mask & PANGO_FONT_MASK_FAMILY) {

    const char *cf_name;
    const char *nf_name;

    cf_name = pango_font_family_get_name (dialog->family);
    nf_name = pango_font_description_get_family (new_desc);

    if (strcmp( nf_name, cf_name) != 0) {
      pango_font_description_set_family (curr_desc, nf_name );
      /*do_update_family */
      changed = TRUE;
    }
  }

  if (mask & PANGO_FONT_MASK_STYLE) {

    PangoStyle style;

    if (( style = pango_font_description_get_style (new_desc) ) !=
                  pango_font_description_get_style (curr_desc)) {
      pango_font_description_set_style (curr_desc, style );
      changed = TRUE;
    }
  }

  if (mask & PANGO_FONT_MASK_STRETCH) {

    PangoStretch c_stretch;
    PangoStretch n_stretch;

    c_stretch = pango_font_description_get_stretch(curr_desc);
    n_stretch = pango_font_description_get_stretch(new_desc);

    if (n_stretch != c_stretch ) {
      pango_font_description_set_stretch (curr_desc, n_stretch );
      changed = TRUE;
    }
  }

  if (mask & PANGO_FONT_MASK_VARIANT) {

    PangoVariant c_variant;
    PangoVariant n_variant;

    c_variant = pango_font_description_get_variant(curr_desc);
    n_variant = pango_font_description_get_variant(new_desc);

    if (n_variant != c_variant ) {
      pango_font_description_set_variant (curr_desc, n_variant );
      changed = TRUE;
    }
  }

  if (mask & PANGO_FONT_MASK_SIZE) {

    int font_size;

    if (pango_font_description_get_size_is_absolute (new_desc)) {
      font_size = pango_font_description_get_size (new_desc);
    }
    else {
      font_size = pango_font_description_get_size (new_desc) / PANGO_SCALE;
    }

    if ( font_size != dialog->font_size ) {
      dialog->font_size = font_size;
      changed = TRUE;
    }
  }

  if (dialog->show_preview && changed) {
    geda_font_dialog_update_preview(dialog);
  }
}

static void
geda_font_dialog_ref_family (GedaFontDialog *dialog, PangoFontFamily *family)
{
  if (family) {

    family = g_object_ref (family);

    if (G_IS_OBJECT(dialog->family)) {
      g_object_unref (dialog->family);
    }
  }
  dialog->family = family;
}

static void
geda_font_dialog_ref_face (GedaFontDialog *dialog, PangoFontFace *face)
{
  if (face) {

    face = g_object_ref (face);

    if (G_IS_OBJECT(dialog->face)) {
      g_object_unref (dialog->face);
    }
  }

  dialog->face = face;
}
static void
geda_font_dialog_load_font (GedaFontDialog *dialog)
{
  if (dialog->font) {
    gdk_font_unref (dialog->font);
  }
  dialog->font = NULL;

  if (dialog->show_preview ) {
    geda_font_dialog_update_preview (dialog);
  }

}

static void
geda_font_dialog_font_changed (GtkWidget *entry, GedaFontDialog *dialog)
{
  geda_font_dialog_load_font(dialog);
}


static void
scroll_to_selection (GtkTreeView *tree_view)
{
  GtkTreeSelection *selection = gtk_tree_view_get_selection (tree_view);
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {
    GtkTreePath *path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 0.5, 0.5);
    gtk_tree_path_free (path);
  }
}

static void
geda_font_dialog_scroll_to_selection (GedaFontDialog *dialog)
{
  /* Try to scroll the font family list to the selected item */
  scroll_to_selection (GTK_TREE_VIEW (dialog->family_list));

  /* Try to scroll the font family list to the selected item */
  scroll_to_selection (GTK_TREE_VIEW (dialog->style_list));

  /* Try to scroll the font family list to the selected item */
  scroll_to_selection (GTK_TREE_VIEW (dialog->size_list));
/* This is called when the list is mapped. Here we scroll to the current
   font if necessary. */
}

static void
callback_scroll_on_map (GtkWidget *widget, void * data)
{
  geda_font_dialog_scroll_to_selection (GEDA_FONT_DIALOG (data));
}

static void
set_cursor_to_iter (GtkTreeView *view, GtkTreeIter *iter)
{
  GtkTreeModel *model;
  GtkTreePath  *path;

  model = gtk_tree_view_get_model (view);
  path = gtk_tree_model_get_path (model, iter);

  gtk_tree_view_set_cursor (view, path, NULL, FALSE);

  gtk_tree_path_free (path);
}

static int
cmp_families (const void *a, const void *b)
{
  const char *a_name = pango_font_family_get_name (*(PangoFontFamily **)a);
  const char *b_name = pango_font_family_get_name (*(PangoFontFamily **)b);

  return g_utf8_collate (a_name, b_name);
}

static int
compare_font_descriptions (const PangoFontDescription *a, const PangoFontDescription *b)
{
  int val;

  val = strcmp (pango_font_description_get_family (a), pango_font_description_get_family (b));
  if (val == 0) {

    if (pango_font_description_get_weight (a) != pango_font_description_get_weight (b))
      val = pango_font_description_get_weight (a) - pango_font_description_get_weight (b);
    else if (pango_font_description_get_style (a) != pango_font_description_get_style (b))
      val = pango_font_description_get_style (a) - pango_font_description_get_style (b);
    else if (pango_font_description_get_stretch (a) != pango_font_description_get_stretch (b))
      val = pango_font_description_get_stretch (a) - pango_font_description_get_stretch (b);
    else if (pango_font_description_get_variant (a) != pango_font_description_get_variant (b))
      val = pango_font_description_get_variant (a) - pango_font_description_get_variant (b);
  }

  return val;
}

static bool
font_description_style_equal (const PangoFontDescription *a,
                              const PangoFontDescription *b)
{
  return (pango_font_description_get_weight (a) == pango_font_description_get_weight (b) &&
   pango_font_description_get_style (a) == pango_font_description_get_style (b) &&
   pango_font_description_get_stretch (a) == pango_font_description_get_stretch (b) &&
   pango_font_description_get_variant (a) == pango_font_description_get_variant (b));
}

static int
faces_sort_func (const void *a, const void *b)
{
  PangoFontDescription *desc_a = pango_font_face_describe (*(PangoFontFace **)a);
  PangoFontDescription *desc_b = pango_font_face_describe (*(PangoFontFace **)b);

  int ord;

  ord = compare_font_descriptions (desc_a, desc_b);

  pango_font_description_free (desc_a);
  pango_font_description_free (desc_b);

  return ord;
}

/*
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (dialog->style_list));
  g_signal_handler_block (selection, dialog->face_handler);
  g_signal_handler_unblock (selection, dialog->face_handler);
 * */
static void
geda_font_dialog_select_best_size (GedaFontDialog *dialog)
{
  GtkTreeIter       iter;
  GtkTreeModel     *model;
  GtkTreeSelection *selection;
  bool found;
  int i;

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (dialog->size_list));
  g_signal_handler_block (selection, dialog->size_handler);

  model = gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->size_list));

  found = FALSE;
  if (gtk_tree_model_get_iter_first (model, &iter)) {

    for (i = 0; i < G_N_ELEMENTS (font_sizes); i++) {

      if (font_sizes[i] == dialog->font_size) {
        set_cursor_to_iter (GTK_TREE_VIEW (dialog->size_list), &iter);
        found = TRUE;
        break;
      }
      gtk_tree_model_iter_next (model, &iter);
    }
  }

  if (!found) {
    gtk_tree_selection_unselect_all (selection);
  }
  else {
    scroll_to_selection (GTK_TREE_VIEW (dialog->size_list));
  }

  g_signal_handler_unblock (selection, dialog->size_handler);

  {
    char *size;

    size = geda_sprintf ("%d", dialog->font_size ); //dialog->font_size);

    /* Changing the entry triggers an update to the preview entry*/
    gtk_entry_set_text (GTK_ENTRY (dialog->size_entry), size);
    g_free ( size );

  }
}

static void
geda_font_dialog_show_available_sizes (GedaFontDialog *dialog)
{
  GtkListStore *store;
  int i;

  /* Insert the standard font sizes */
  store = GTK_LIST_STORE (gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->size_list)));
  gtk_list_store_clear (store);

  for (i = 0; i < G_N_ELEMENTS (font_sizes); i++) {
    GtkTreeIter iter;

    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, SIZE_COLUMN, font_sizes[i], -1);

    if (font_sizes[i] == dialog->font_size)
      set_cursor_to_iter (GTK_TREE_VIEW (dialog->size_list), &iter);
  }

}

int valid_font_size (int new_size)
{
  int font_size;
  int min_size;
  int max_size;

  min_size   = font_sizes[0];
  max_size   = font_sizes [ G_N_ELEMENTS (font_sizes) - 1];

  /* check the value obtained from pango */
  if ( new_size < min_size || new_size > max_size) {
    font_size = DEFAULT_FONT_SIZE;
  }
  else {
    font_size = new_size;
  }
  return font_size;
}
/* This is called when a size is selected in the list. */
static void callback_select_size (GtkTreeSelection *selection, void * data)
{
  GedaFontDialog *dialog;
  GtkTreeModel *model;
  GtkTreeIter iter;
  char *size;
  int new_size;

  dialog = GEDA_FONT_DIALOG (data);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    gtk_tree_model_get (model, &iter, SIZE_COLUMN, &new_size, -1);

    if ( new_size != dialog->font_size ) {

      dialog->font_size = new_size;
      pango_font_description_set_size (dialog->font_desc, dialog->font_size);

    }
    size = geda_sprintf ("%d", new_size ); //dialog->font_size);
    /* Changing the entry triggers an update to the preview entry*/
    gtk_entry_set_text (GTK_ENTRY (dialog->size_entry), size);
    g_free ( size );

  }
}


/* This selects a style when the user selects a font. It just uses the first
   available style at present. I was thinking of trying to maintain the
   selected style, e.g. bold italic, when the user selects different fonts.
   However, the interface is so easy to use now I'm not sure it's worth it.
   Note: This will load a font. */
static void
geda_font_dialog_select_best_style (GedaFontDialog *dialog, bool use_first)
{
  GtkTreeIter       iter;
  GtkTreeModel     *model;

  model = gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->style_list));

  if (gtk_tree_model_get_iter_first (model, &iter)) {

    set_cursor_to_iter (GTK_TREE_VIEW (dialog->style_list), &iter);
    scroll_to_selection (GTK_TREE_VIEW (dialog->style_list));
  }
}

/* This fills the font style list with all the possible style combinations
   for the current font family. */
static void
geda_font_dialog_show_available_styles (GedaFontDialog *dialog)
{
  int n_faces, i;
  PangoFontFace       **faces;
  PangoFontFace        *match_face;
  PangoFontDescription *old_desc;
  GtkListStore         *store;
  GtkTreeIter           match_row;

  match_face = NULL;
  faces      = NULL;
  n_faces    = 0;

  store = GTK_LIST_STORE (gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->style_list)));
  gtk_list_store_clear (store);

  if (dialog->family) {
    if (dialog->face)
      old_desc = pango_font_face_describe (dialog->face);
    else
      old_desc= NULL;

    pango_font_family_list_faces (dialog->family, &faces, &n_faces);
    qsort (faces, n_faces, sizeof (PangoFontFace *), faces_sort_func);

    for (i=0; i < n_faces; i++)
    {
      GtkTreeIter iter;
      const char *str;

      str = pango_font_face_get_face_name (faces[i]);

      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter, FACE_COLUMN, faces[i],
                          FACE_NAME_COLUMN, str, -1);
      if (old_desc) {

        PangoFontDescription *tmp_desc = pango_font_face_describe (faces[i]);

        if (font_description_style_equal (tmp_desc, old_desc)) {

          match_row = iter;
          match_face = faces[i];
        }

        pango_font_description_free (tmp_desc);
      }
    }

    if (old_desc)
      pango_font_description_free (old_desc);
    else {
      gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &match_row);
      match_face = faces[0];
    }

    geda_font_dialog_ref_face (dialog, match_face);
    if (match_face) {

      const char *str;

      str = pango_font_face_get_face_name (dialog->face);

      gtk_entry_set_text (GTK_ENTRY (dialog->style_entry), str);

      set_cursor_to_iter (GTK_TREE_VIEW (dialog->style_list), &match_row);
    }

    g_free (faces);
  }
}

/* This is called when a style is selected in the list. */
static void
callback_select_style (GtkTreeSelection *selection, void * data)
{
  GedaFontDialog *dialog;
  GtkTreeIter     iter;
  GtkTreeModel   *model;

  dialog = GEDA_FONT_DIALOG (data);
  model  = NULL;

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    char *face_name;

    if(dialog->face)
      g_object_unref (dialog->face);

    face_name = NULL;

    gtk_tree_model_get (model, &iter, FACE_COLUMN, &dialog->face,
                                      FACE_NAME_COLUMN, &face_name, -1);

    g_object_ref (dialog->face);

    if (dialog->font_desc)
      pango_font_description_free(dialog->font_desc);

    dialog->font_desc = pango_font_face_describe (dialog->face);
    pango_font_description_set_size (dialog->font_desc, dialog->font_size);

    gtk_entry_set_text (GTK_ENTRY (dialog->style_entry), face_name);
    g_free(face_name);
  }
}
/* This selects the appropriate list rows.
   First we check the fontname is valid and try to find the font family
   - i.e. the name in the main list. If we can't find that, then just return.
   Next we try to set each of the properties according to the fontname.
   Finally we select the font family & style in the lists. */
static bool
geda_font_dialog_select_font_desc (GedaFontDialog        *dialog,
                                   PangoFontDescription  *new_desc,
                                   PangoFontFamily      **pfamily,
                                   PangoFontFace        **pface)
{
  PangoFontFamily *new_family = NULL;
  PangoFontFace   *new_face;
  PangoFontFace   *fallback_face;
  GtkTreeModel    *model;
  GtkTreeIter      iter;
  GtkTreeIter      match_iter;
  bool             valid_family;
  bool             valid;
  const char      *new_family_name;
  const char      *tree_name;

  new_face        = NULL;
  fallback_face   = NULL;

  new_family_name = pango_font_description_get_family (new_desc);

  if (!new_family_name)
    return FALSE;

  valid_family = FALSE;

  /* Check to make sure that this is in the list of allowed fonts */
  model = gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->family_list));
  for (valid = gtk_tree_model_get_iter_first (model, &iter); valid;
       valid = gtk_tree_model_iter_next (model, &iter))
  {
    PangoFontFamily *family;

    gtk_tree_model_get (model, &iter, FAMILY_COLUMN, &family, -1);

    tree_name = pango_font_family_get_name (family);

    if (g_ascii_strcasecmp (tree_name, new_family_name) == 0) {

      valid_family = TRUE;
      new_family   = family;
      break;
    }
  }

  if (!valid_family) {
    return FALSE;
  }

  if (pfamily)
    *pfamily = new_family;
  else {
    g_object_unref (new_family);
  }

  set_cursor_to_iter (GTK_TREE_VIEW (dialog->family_list), &iter);

  geda_font_dialog_show_available_styles (dialog);

  model = gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->style_list));
  for (valid = gtk_tree_model_get_iter_first (model, &iter);  valid;
       valid = gtk_tree_model_iter_next (model, &iter))
  {
    PangoFontFace *face;
    PangoFontDescription *tmp_desc;

    gtk_tree_model_get (model, &iter, FACE_COLUMN, &face, -1);
    tmp_desc = pango_font_face_describe (face);

    if (font_description_style_equal (tmp_desc, new_desc))
      new_face = g_object_ref (face);

    if (!fallback_face) {
      fallback_face = g_object_ref (face);
      match_iter = iter;
    }

    pango_font_description_free (tmp_desc);
    g_object_unref (face);

    if (new_face) {
      match_iter = iter;
      break;
    }
  }

  if (!new_face)
    new_face = fallback_face;
  else if (fallback_face) {
    g_object_unref (fallback_face);
  }

  if (pface)
    *pface = new_face;
  else if (new_face) {
    g_object_unref (new_face);
  }

  set_cursor_to_iter (GTK_TREE_VIEW (dialog->style_list), &match_iter);

  return TRUE;
}

static void
geda_font_dialog_show_available_fonts (GedaFontDialog *dialog)
{

  PangoContext     *context;
  PangoFontFamily **families;
  PangoFontFamily  *match_family;

  GtkTreeIter   match_row;
  GtkListStore *model;

  const char *name;
  int n_families, i;

  context = gtk_widget_get_pango_context ( GTK_WIDGET (dialog));

  pango_context_list_families (context, &families, &n_families);

  qsort (families, n_families, sizeof (PangoFontFamily *), cmp_families);

  model = GTK_LIST_STORE (gtk_tree_view_get_model (GTK_TREE_VIEW (dialog->family_list)));
  gtk_list_store_clear (model);

  match_family = NULL;
  /* Load the list of fonts names into the view tree model */
  for (i=0; i<n_families; i++) {
    name = pango_font_family_get_name (families[i]);
    GtkTreeIter iter;

    gtk_list_store_append (model, &iter);
    gtk_list_store_set    (model, &iter, FAMILY_COLUMN, families[i],
                           FAMILY_NAME_COLUMN, name,
                           -1);

    if ( !match_family && dialog->family ) {
      if (cmp_families(dialog->family, families[i]) == 0) {
        match_family = families[i];
        match_row = iter;
      }
    }
    else {
     if ( i == 0 ) {
        match_family = families[i];
        match_row = iter;
        geda_font_dialog_ref_family (dialog, match_family);
      }
    }
  }

  if (match_family != NULL )  {
    set_cursor_to_iter (GTK_TREE_VIEW (dialog->family_list), &match_row);
    gtk_entry_set_text (GTK_ENTRY (dialog->font_entry), pango_font_family_get_name (match_family));
  }

  g_free (families);
}

static void
callback_select_family (GtkTreeSelection *selection, void * data)
{
  GedaFontDialog *dialog;
  GtkTreeModel   *model;
  GtkTreeIter     iter;
  char           *font_name;

  dialog = GEDA_FONT_DIALOG (data);
  model  = NULL;

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    PangoFontFamily *family;

    gtk_tree_model_get (model, &iter, FAMILY_COLUMN, &family, -1);

    if ( family != dialog->family ) {

      const char  *family_name;

      if (dialog->family) {
        g_object_unref (dialog->family);
      }

      dialog->family = g_object_ref (family);
      g_object_unref (family);

      if (dialog->font_desc)
        pango_font_description_free(dialog->font_desc);

      family_name       = pango_font_family_get_name(dialog->family);
      font_name         = geda_sprintf("%s %d",family_name, dialog->font_size);
      dialog->font_desc = pango_font_description_from_string(font_name);
      g_free (font_name);

      gtk_entry_set_text (GTK_ENTRY (dialog->font_entry), family_name);
      geda_font_dialog_show_available_styles (dialog);
      geda_font_dialog_select_best_style (dialog, TRUE);
    }
  }
}

static void
geda_font_dialog_prime_list (GedaFontDialog *dialog)
{
  if (gtk_widget_has_screen (GTK_WIDGET (dialog))) {

    geda_font_dialog_show_available_fonts (dialog);

    geda_font_dialog_show_available_styles (dialog);

    geda_font_dialog_show_available_sizes  (dialog);

  }
}

static void
geda_font_dialog_screen_changed (GtkWidget *widget,
                                 GdkScreen *previous_screen)
{
  if (GTK_WIDGET_CLASS (geda_font_dialog_parent_class)->screen_changed)
    GTK_WIDGET_CLASS (geda_font_dialog_parent_class)->screen_changed (widget, previous_screen);

  if (previous_screen == NULL)
    previous_screen = gdk_screen_get_default ();

  if (previous_screen == gtk_widget_get_screen (widget))
    return;

  geda_font_dialog_prime_list (GEDA_FONT_DIALOG (widget));
}

static char*
geda_font_dialog_get_font_name_internal (GedaFontDialog *dialog)
{
  return pango_font_description_to_string (dialog->font_desc);
}

static void
geda_font_dialog_get_property (GObject *object, unsigned int prop_id,
                               GValue  *value,  GParamSpec   *pspec)
{
  GedaFontDialog *dialog;
  dialog = GEDA_FONT_DIALOG (object);

  switch (prop_id) {
    case PROP_TITLE:
      break;
    case PROP_FONT:
      g_value_set_boxed (value, geda_font_dialog_get_font (dialog));
      break;
    case PROP_FONT_DESC:
      g_value_set_boxed (value, geda_font_dialog_get_font_desc (dialog));
      break;
    case PROP_FONT_NAME:
      g_value_set_string (value, geda_font_dialog_get_font_name_internal (dialog));
      break;
    case PROP_FONT_SIZE:
      g_value_set_int (value, geda_font_dialog_get_font_size (dialog));
      break;
    case PROP_PREVIEW_TEXT:
      g_value_set_string (value, geda_font_dialog_get_preview_text (dialog));
      break;
    case PROP_SHOW_PREVIEW:
      g_value_set_boolean (value, dialog->show_preview);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_font_dialog_set_property (GObject *object, unsigned int prop_id,
                         const GValue  *value,  GParamSpec  *pspec)
{
  GedaFontDialog *dialog;
  dialog = GEDA_FONT_DIALOG (object);

  switch (prop_id)
  {
    case PROP_TITLE:
      gtk_window_set_title (GTK_WINDOW (dialog), g_value_get_string (value));
      break;
    case PROP_FONT_DESC:
      geda_font_dialog_take_font_desc (dialog, g_value_dup_boxed (value));
      break;
    case PROP_FONT_NAME:
      geda_font_dialog_set_font_name (dialog, g_value_get_string (value));
      break;
    case PROP_FONT_SIZE:
      geda_font_dialog_set_font_size (dialog, g_value_get_int(value));
      break;
    case PROP_PREVIEW_TEXT:
      geda_font_dialog_set_preview_text (dialog, g_value_get_string (value));
      break;
    case PROP_SHOW_PREVIEW:
      geda_font_dialog_set_show_preview(dialog, g_value_get_boolean (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/* Handles key press events on the lists, so that we can trap Enter to
 * activate the default button on our own.
 */
static bool list_row_activated (GtkWidget *widget)
{
  GtkWindow *window;

  window = GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (widget)));
  if (!gtk_widget_is_toplevel (GTK_WIDGET (window)))
    window = NULL;

  if (window
      && widget != window->default_widget
      && !(widget == window->focus_widget &&
    (!window->default_widget || !gtk_widget_get_sensitive (window->default_widget))))
    {
      gtk_window_activate_default (window);
    }

  return TRUE;
}

/* If the user hits return in the font size entry, we change to the new font
   size. */
static void
callback_size_entry_activate (GtkWidget *w, void * data)
{
  GedaFontDialog *dialog;
  const char     *text;

  dialog = GEDA_FONT_DIALOG (data);

  text   = gtk_entry_get_text (GTK_ENTRY (dialog->size_entry));

  if ( text[0] != '0') {

    int new_size;

    new_size = MAX (0.1, atof (text) + 0.5);

    if (dialog->font_size != new_size) {
      geda_font_dialog_set_font_size (dialog, new_size);
    }
    else
      list_row_activated (w);

    geda_font_dialog_update_preview (dialog);
  }
}

static bool callback_size_entry_focus_out (GtkWidget     *w,
                                             GdkEventFocus *event,
                                             void *       data)
{
  callback_size_entry_activate (w, data);

  return TRUE;
}
static AtkObject*
atk_widget_linked_label_new( GtkWidget *label, GtkWidget *linkto)
{
  AtkObject *atk_obj;

  atk_obj = gtk_widget_get_accessible (linkto);

  if (GTK_IS_ACCESSIBLE (atk_obj)) {
    /* Accessibility support is enabled.
       Make the label ATK_RELATON_LABEL_FOR for the size list as well. */

    AtkObject      *atk_label;
    AtkRelationSet *relation_set;
    AtkRelation    *relation;
    AtkObject      *obj_array[1];

    atk_label    = gtk_widget_get_accessible (label);

    relation_set = atk_object_ref_relation_set (atk_obj);

    relation     = atk_relation_set_get_relation_by_type (relation_set,
                                                          ATK_RELATION_LABELLED_BY);
    if (relation) {
      atk_relation_add_target (relation, atk_label);
    }
    else {
      obj_array[0] = atk_label;
      relation     = atk_relation_new (obj_array, 1, ATK_RELATION_LABELLED_BY);
      atk_relation_set_add (relation_set, relation);
    }
    g_object_unref (relation_set);

    relation_set = atk_object_ref_relation_set (atk_label);
    relation     = atk_relation_set_get_relation_by_type (relation_set,
                                                          ATK_RELATION_LABEL_FOR);
    if (relation) {
      atk_relation_add_target (relation, atk_obj);
    }
    else {
      obj_array[0] = atk_obj;
      relation = atk_relation_new (obj_array, 1, ATK_RELATION_LABEL_FOR);
      atk_relation_set_add (relation_set, relation);
    }
    g_object_unref (relation_set);
  }
  else
    atk_obj = NULL;

  return atk_obj;
}
static void
geda_font_dialog_add_widgets(GedaFontDialog *dialog)
{
  AtkObject         *atk_font_obj;
  AtkObject         *atk_style_obj;
  AtkObject         *atk_size_obj;
  AtkObject         *atk_preview_obj;

  GtkWidget         *table;
  GtkWidget         *font_label, *style_label, *size_label, *preview_label;

  GtkListStore      *model;
  GtkTreeViewColumn *column;
  GtkCellRenderer   *renderer;
  GtkTreeSelection  *selection;
  GtkWidget         *family_scroll;
  GtkWidget         *style_scroll;
  GtkWidget         *size_scroll;
  GtkWidget         *text_box;
  GtkWidget         *vbox;

  const char        *family_scroll_tip;
  const char        *style_scroll_tip;
  const char        *size_entry_tip;
  const char        *size_scroll_tip;
  const char        *preview_tip;

  family_scroll_tip = _("Select the desired font family");
  style_scroll_tip  = _("Select the style to apply to the font");
  size_entry_tip    = _("Current font size");
  size_scroll_tip   = _("Select the desired font size");
  preview_tip       = _("Editable sample preview of the currently selected font settings");

  /* Create the table of font, style & size. */
  table = gtk_table_new (3, 3, FALSE);
  gtk_table_set_row_spacings (GTK_TABLE (table), 6);
  gtk_table_set_col_spacings (GTK_TABLE (table), 12);
  gtk_box_pack_start (GTK_BOX (dialog->main_vbox), table, TRUE, TRUE, 0);

  font_label = geda_mnemonic_label_new (_("_Family:"));
  gtk_misc_set_alignment (GTK_MISC (font_label), 0.0, 0.5);
  gtk_widget_show (font_label);
  gtk_table_attach (GTK_TABLE (table), font_label, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);

  style_label = geda_mnemonic_label_new (_("_Style:"));
  gtk_misc_set_alignment (GTK_MISC (style_label), 0.0, 0.5);
  gtk_widget_show (style_label);
  gtk_table_attach (GTK_TABLE (table), style_label, 1, 2, 0, 1, GTK_FILL, 0, 0, 0);

  size_label = geda_mnemonic_label_new (_("Si_ze:"));
  gtk_misc_set_alignment (GTK_MISC (size_label), 0.0, 0.5);
  gtk_widget_show (size_label);
  gtk_table_attach (GTK_TABLE (table), size_label, 2, 3, 0, 1, GTK_FILL, 0, 0, 0);

  dialog->font_entry = geda_entry_new ( DISABLE, DISABLE);
  gtk_editable_set_editable (GTK_EDITABLE (dialog->font_entry), TRUE);
  gtk_widget_set_size_request (dialog->font_entry, 20, -1);
  gtk_widget_show (dialog->font_entry);
  gtk_table_attach (GTK_TABLE (table), dialog->font_entry, 0, 1, 1, 2, GTK_FILL, 0, 0, 0);

  dialog->style_entry = geda_entry_new ( DISABLE, DISABLE);
  gtk_editable_set_editable (GTK_EDITABLE (dialog->style_entry), FALSE);
  gtk_widget_set_size_request (dialog->style_entry, 20, -1);
  gtk_widget_show (dialog->style_entry);
  gtk_table_attach (GTK_TABLE (table), dialog->style_entry, 1, 2, 1, 2, GTK_FILL, 0, 0, 0);

  /* Size Entry */
  dialog->size_entry = geda_entry_new ( DISABLE, DISABLE);
  gtk_widget_set_size_request (dialog->size_entry, 20, -1);
  gtk_widget_set_tooltip_text (GTK_WIDGET(dialog->size_entry), size_entry_tip);
  gtk_table_attach (GTK_TABLE (table), dialog->size_entry, 2, 3, 1, 2, GTK_FILL, 0, 0, 0);
  gtk_widget_set_can_default  (dialog->size_entry,FALSE);
  gtk_widget_show (dialog->size_entry);

  /* Our trees are all text with the same attributes, so we're going
   * to reuse this renderer */
  renderer = GTK_CELL_RENDERER ( g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                               "editable", FALSE,
                                                NULL));

  family_scroll = GTK_WIDGET ( g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                             "border-width", 3,
                                             "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                             "vscrollbar-policy", GTK_POLICY_ALWAYS,
                                             "shadow-type",       GTK_SHADOW_IN,
                                             "visible",           TRUE,
                                              NULL));

  /* Create the lists  */
  model = gtk_list_store_new (2, G_TYPE_OBJECT,  /* FAMILY_COLUMN */
                                 G_TYPE_STRING); /* FAMILY_NAME_COLUMN */

  /* Create the treeview */
  dialog->family_list = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                                  "model",           model,
                                                  "rules-hint",      TRUE,
                                                  "headers-visible", TRUE,
                                                  "visible",         TRUE,
                                                   NULL));
  g_object_unref (model);

  column = GTK_TREE_VIEW_COLUMN ( g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                                "title", _("Family"),
                                                "sizing", GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                                                NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", FAMILY_NAME_COLUMN);
  gtk_tree_view_append_column (GTK_TREE_VIEW (dialog->family_list), column);
  gtk_container_add (GTK_CONTAINER (family_scroll), dialog->family_list);
  gtk_widget_set_size_request (family_scroll, FONT_LIST_WIDTH, FONT_LIST_HEIGHT);
  gtk_widget_set_tooltip_text (GTK_WIDGET(family_scroll), family_scroll_tip);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (dialog->family_list));
  gtk_tree_selection_set_mode ( selection, GTK_SELECTION_BROWSE);
  dialog->family_handler = g_signal_connect (selection, "changed",
                           G_CALLBACK ( callback_select_family ), dialog);

  gtk_table_attach (GTK_TABLE (table), family_scroll, 0, 1, 1, 3,
                    GTK_EXPAND | GTK_FILL,
                    GTK_EXPAND | GTK_FILL, 0, 0);

  style_scroll = GTK_WIDGET ( g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                             "border-width", 0,
                                             "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                             "vscrollbar-policy", GTK_POLICY_ALWAYS,
                                             "shadow-type",       GTK_SHADOW_IN,
                                             "visible",           TRUE,
                                             NULL));

  model = gtk_list_store_new (2, G_TYPE_OBJECT,  /* FACE_COLUMN */
                                 G_TYPE_STRING); /* FACE_NAME_COLUMN */

  dialog->style_list = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                                 "model",           model,
                                                 "rules-hint",      TRUE,
                                                 "headers-visible", TRUE,
                                                 "visible",         TRUE,
                                                  NULL));
  g_object_unref (model);

  column = GTK_TREE_VIEW_COLUMN ( g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                                "title", _("Face"),
                                                "sizing", GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                                                NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", FACE_NAME_COLUMN);

  gtk_tree_view_append_column (GTK_TREE_VIEW (dialog->style_list), column);
  gtk_container_add (GTK_CONTAINER (style_scroll), dialog->style_list);
  gtk_widget_set_size_request (style_scroll, FONT_STYLE_LIST_WIDTH, FONT_LIST_HEIGHT);
  gtk_widget_set_tooltip_text (GTK_WIDGET(style_scroll), style_scroll_tip);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (dialog->style_list));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
  dialog->face_handler = g_signal_connect (selection, "changed",
                         G_CALLBACK (callback_select_style), dialog);

  gtk_table_attach (GTK_TABLE (table), style_scroll, 1, 2, 1, 3,
                    GTK_EXPAND | GTK_FILL,
                    GTK_EXPAND | GTK_FILL, 0, 0);

  size_scroll = GTK_WIDGET ( g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                           "border-width", 3,
                                           "hscrollbar-policy", GTK_POLICY_NEVER,
                                           "vscrollbar-policy", GTK_POLICY_ALWAYS,
                                           "shadow-type",       GTK_SHADOW_IN,
                                           "visible",           TRUE,
                                            NULL));

  model = gtk_list_store_new (1, G_TYPE_INT);

  dialog->size_list = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                               "model",           model,
                                               "rules-hint",      TRUE,
                                               "headers-visible", FALSE,
                                               "visible",         TRUE,
                                                NULL));
  g_object_unref (model);

  column = GTK_TREE_VIEW_COLUMN ( g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                                "title", _("Face"),
                                                "sizing", GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                                                NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", SIZE_COLUMN);
  gtk_tree_view_append_column (GTK_TREE_VIEW (dialog->size_list), column);

  gtk_container_add (GTK_CONTAINER (size_scroll), dialog->size_list);
  gtk_widget_set_size_request (size_scroll, -1, FONT_LIST_HEIGHT);
  gtk_widget_set_tooltip_text (GTK_WIDGET(size_scroll), size_scroll_tip);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (dialog->size_list));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
  dialog->size_handler = g_signal_connect (selection, "changed",
                         G_CALLBACK (callback_select_size), dialog);

  gtk_table_attach (GTK_TABLE (table), size_scroll, 2, 3, 2, 3,
                    GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0);

  GList *focus_chain; /* Aka Tab Order */
  focus_chain = NULL;
  focus_chain = g_list_append (focus_chain, family_scroll);
  focus_chain = g_list_append (focus_chain, style_scroll);
  focus_chain = g_list_append (focus_chain, dialog->size_entry);
  focus_chain = g_list_append (focus_chain, size_scroll);
  gtk_container_set_focus_chain (GTK_CONTAINER (table), focus_chain);
  g_list_free (focus_chain);

  vbox = gtk_vbox_new (FALSE, 6);
  gtk_widget_show (vbox);
  gtk_box_pack_start (GTK_BOX (dialog->main_vbox), vbox, FALSE, TRUE, 0);

  /* create the text entry widget */
  preview_label = geda_mnemonic_label_new (_("_Preview:"));
  gtk_misc_set_alignment (GTK_MISC (preview_label), 0.0, 0.5);
  gtk_widget_show (preview_label);
  gtk_box_pack_start (GTK_BOX (vbox), preview_label, FALSE, TRUE, 0);

  text_box = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (text_box);
  gtk_box_pack_start (GTK_BOX (vbox), text_box, FALSE, TRUE, 0);

  dialog->preview_entry = gtk_entry_new ();
  gtk_widget_set_size_request (dialog->preview_entry, -1, INITIAL_PREVIEW_HEIGHT);
  gtk_widget_set_tooltip_text (GTK_WIDGET(dialog->preview_entry),preview_tip);
  gtk_widget_show (dialog->preview_entry);
  gtk_box_pack_start (GTK_BOX (text_box), dialog->preview_entry, TRUE, TRUE, 0);

  /** Set the relationships between the label and their Widgets **/
  geda_label_set_mnemonic_widget (GEDA_LABEL (font_label),    dialog->family_list);
  geda_label_set_mnemonic_widget (GEDA_LABEL (style_label),   dialog->style_list);
  geda_label_set_mnemonic_widget (GEDA_LABEL (size_label),    dialog->size_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL (preview_label), dialog->preview_entry);

  atk_font_obj    = atk_widget_linked_label_new (font_label,    dialog->family_list );
  atk_style_obj   = atk_widget_linked_label_new (style_label,   dialog->style_list );
  atk_size_obj    = atk_widget_linked_label_new (size_label,    dialog->size_list );
  atk_preview_obj = atk_widget_linked_label_new (preview_label, dialog->preview_entry);

  if ( atk_font_obj ) {
    atk_object_set_name        (atk_font_obj,    _("Font scroll list"));
    atk_object_set_description (atk_font_obj,    _(family_scroll_tip));
  }
  if ( atk_style_obj ) {
    atk_object_set_name        (atk_style_obj,   _("Font style scroll list"));
    atk_object_set_description (atk_style_obj,   _(style_scroll_tip));
  }
  if ( atk_size_obj ) {
    atk_object_set_name        (atk_size_obj,    _("Font size scroll list"));
    atk_object_set_description (atk_size_obj,    _(size_scroll_tip));
  }
  if ( atk_preview_obj ) {
    atk_object_set_name        (atk_preview_obj, _("Font preview text entry"));
    atk_object_set_description (atk_preview_obj, _(preview_tip));
  }

  gtk_widget_pop_composite_child();
  gtk_widget_show (table);

  g_signal_connect (dialog->size_entry, "process-entry",
                    G_CALLBACK (callback_size_entry_activate),
                    dialog);

  g_signal_connect_after (dialog->size_entry, "focus-out-event",
                          G_CALLBACK (callback_size_entry_focus_out),
                          dialog);

/*
  g_signal_connect_after (dialog->size_entry, "changed",
                          G_CALLBACK (geda_font_dialog_font_changed),
                          dialog);
*/
  g_signal_connect_after (dialog->font_entry, "changed",
                          G_CALLBACK (geda_font_dialog_font_changed),
                          dialog);

  g_signal_connect_after (dialog->style_entry, "changed",
                          G_CALLBACK (geda_font_dialog_font_changed),
                          dialog);

  g_signal_connect (dialog->family_list, "row-activated",
                    G_CALLBACK (list_row_activated), dialog);

  g_signal_connect (dialog->style_list, "row-activated",
                    G_CALLBACK (list_row_activated), dialog);

  g_signal_connect (dialog->size_list, "row-activated",
                    G_CALLBACK (list_row_activated), dialog);

  g_signal_connect_after (dialog->family_list, "map",
                          G_CALLBACK (callback_scroll_on_map),
                          dialog);

  dialog->preview_handler =
          g_signal_connect (dialog->preview_entry, "changed",
                            G_CALLBACK (callback_update_preview), dialog);

}

static void geda_font_dialog_finalize (GObject *object)
{
  GedaFontDialog *dialog;

  g_return_if_fail (GEDA_IS_FONT_DIALOG (object));

  G_OBJECT_CLASS (geda_font_dialog_parent_class)->finalize (object);

  dialog = GEDA_FONT_DIALOG (object);

  if ( dialog->font && G_IS_OBJECT(dialog->font) )
    gdk_font_unref (dialog->font);

  if ( dialog->family && G_IS_OBJECT(dialog->family) )
    g_object_unref (dialog->family);

  if ( dialog->face && G_IS_OBJECT(dialog->face))
    g_object_unref (dialog->face);

  if (dialog->default_font)
    g_free(dialog->default_font);

  if ( dialog->font_desc )
    pango_font_description_free (dialog->font_desc);

  dialog->font_desc = NULL;
/*
  if ( dialog->font_map && G_IS_OBJECT(dialog->font_map) ) {
    g_object_unref (dialog->font_map);
    dialog->font_map = NULL;
  }
*/
}

/*! \brief GedaFontDialog Type Class Initializer
 *
 *  \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 *  \param [in]  class       GedaFontDialog class being initializing
 *  \param [in]  class_data  GedaFontDialog structure associated with the class
 */
static void
geda_font_dialog_class_init(void *class, void *class_data)
{
  GedaFontDialogClass *dialog_class;
  GObjectClass        *object_class;
  GtkWidgetClass      *widget_class;
  GParamSpec          *params;

  dialog_class = (GedaFontDialogClass*)class;
  object_class = G_OBJECT_CLASS (dialog_class);
  widget_class = GTK_WIDGET_CLASS (dialog_class);

  object_class->get_property = geda_font_dialog_get_property;
  object_class->set_property = geda_font_dialog_set_property;
  object_class->finalize     = geda_font_dialog_finalize;

  widget_class->screen_changed  = geda_font_dialog_screen_changed;

  geda_font_dialog_parent_class = g_type_class_peek_parent(dialog_class);

  /*! property GedaFontDialog::title
   *  \par The title of the font selection dialog.
   */
  params = g_param_spec_string ("title",
                              _("Title"),
                              _("The title of the font selection dialog"),
                              _("Pick a Font"),
                               (G_PARAM_WRITABLE));

  g_object_class_install_property (object_class, PROP_TITLE, params);

  params = g_param_spec_boxed ("font",
                             _("Font"),
                             _("The GdkFont that is currently selected"),
                                GDK_TYPE_FONT,
                               (G_PARAM_READABLE));

  g_object_class_install_property (object_class, PROP_FONT, params);

  params = g_param_spec_boxed ("font-desc",
                             _("Font description"),
                             _("Pango Font Description struct"),
                                PANGO_TYPE_FONT_DESCRIPTION,
                               (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FONT_DESC, params);

  params = g_param_spec_string ("font-name",
                              _("Font name"),
                              _("The string that represents this font"),
                                 DEFAULT_FONT_NAME,
                                (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FONT_NAME, params);

  /*! property GedaFontDialog::font-size:
   *  \par Programmactically set the font size.
   */
  params = g_param_spec_int ("font-size",
                           _("Set Size"), /* nick name */
                           _("Set point size of the font"), /* hint / blurb */
                              6, /* Min value */
                              96, /* Max value */
                              10, /* default_value */
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FONT_SIZE, params);


  params = g_param_spec_string ("preview-text",
                              _("Preview text"),
                              _("The text to display in order to demonstrate the selected font"),
                              _(PREVIEW_TEXT),
                               (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_PREVIEW_TEXT, params);

  params = g_param_spec_boolean ("show-preview",
                               _("Show preview text entry"),
                               _("Whether the preview text entry is displayed"),
                                  TRUE,
                                 (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_SHOW_PREVIEW, params);

}

/*! \brief Type instance initializer for GedaFontDialog
 *
 *  \par Function Description
 *  Type instance initializer for GedaFontDialog, initializes a new empty
 *  GedaFontDialog object.
 *
 *  \param [in] instance The GedaFontDialog structure being initialized,
 *  \param [in] g_class  The GedaFontDialog class we are initializing.
 */
static void geda_font_dialog_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaFontDialog *dialog     = (GedaFontDialog*)instance;
  GtkDialog      *Dialog     = GTK_DIALOG (dialog);
  GtkSettings    *settings;

  dialog->instance_type = geda_font_dialog_get_type();

  dialog->face          = NULL; /* Current face */
  dialog->family        = NULL; /* Current family */
  dialog->font          = NULL; /* Cache for gdk_font_selection_get_font */
  dialog->font_desc     = NULL;

  dialog->default_font  = NULL;
  dialog->show_preview  = TRUE;

  /* Initialize the font */
  if ((settings = gtk_settings_get_default ()) != NULL ) {
    g_object_get (settings, "gtk-font-name", &dialog->default_font, NULL);
  }
  else {
    dialog->default_font  = g_strdup (_(DEFAULT_FONT_NAME));
  }

  dialog->font_map  = pango_cairo_font_map_get_default ();

  dialog->font_desc = geda_font_dialog_get_font_description (dialog);

  { /* Initialize the Font Size */
    int font_size;
    int last_index;
    int min_size;
    int max_size;

    font_size  = pango_font_description_get_size (dialog->font_desc);

    if (!pango_font_description_get_size_is_absolute (dialog->font_desc))
      font_size = font_size / PANGO_SCALE;

    last_index = sizeof(font_sizes)/sizeof(font_sizes[0]) - 1;
    min_size   = font_sizes[0];
    max_size   = font_sizes [last_index];

    /* check the value obtained from pango */
    if ( font_size < min_size || font_size > max_size) {

      dialog->font_size = DEFAULT_FONT_SIZE;

      /* Update the font_description structure */
      pango_font_description_set_size (dialog->font_desc, DEFAULT_FONT_SIZE);
    }
    else {
      dialog->font_size = font_size;
    }
  }

  gtk_widget_push_composite_child ();

  dialog->main_vbox = GTK_DIALOG(Dialog)->vbox;

  /* Create the main content area */
  gtk_container_set_border_width (GTK_CONTAINER (Dialog), 5);

  gtk_box_set_spacing (GTK_BOX (dialog->main_vbox), 2); /* 2 * 5 + 2 = 12 */

  geda_font_dialog_add_widgets(dialog);

  { /* Default preview string  */
    const char *text;

    text = pango_language_get_sample_string (NULL);

    gtk_entry_set_text (GTK_ENTRY (dialog->preview_entry), text);
  }

  /* Setup the Action Area */
  dialog->action_area  = gtk_dialog_get_action_area (Dialog);

  gtk_container_set_border_width (GTK_CONTAINER (dialog->action_area), 5);

  gtk_box_set_spacing (GTK_BOX (dialog->action_area), 6);

  dialog->cancel_button = gtk_dialog_add_button (Dialog,
                                                 GTK_STOCK_CANCEL,
                                                 GEDA_RESPONSE_ACCEPT);

  dialog->ok_button = gtk_dialog_add_button (Dialog,
                                           _("_Select"),
                                             GEDA_RESPONSE_OK);

  gtk_dialog_set_alternative_button_order (Dialog,
                                           GEDA_RESPONSE_OK,
                                           GEDA_RESPONSE_ACCEPT,
                                           -1);

  gtk_widget_pop_composite_child ();

  gtk_window_set_resizable       (GTK_WINDOW (Dialog), TRUE);
  gtk_dialog_set_has_separator   (Dialog, FALSE);
  gtk_widget_grab_default        (dialog->ok_button); /* does not work correct with entry */
  geda_font_dialog_prime_list    (dialog);

  gtk_widget_show                (GTK_WIDGET(Dialog));
}

/*! \brief Function to retrieve GedaFontDialog's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaFontDialog Type identifier. When
 *  first called, the function registers a #GedaFontDialog in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaFontDialog and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaFontDialog.
 */
GedaType geda_font_dialog_get_type (void)
{
  static GedaType geda_font_dialog_type = 0;

  if (g_once_init_enter (&geda_font_dialog_type)) {

    static const GTypeInfo info = {
      sizeof(GedaFontDialogClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_font_dialog_class_init,     /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaFontDialog),
      0,                               /* n_preallocs         */
      geda_font_dialog_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaFontDialog");
    type   = g_type_register_static (GTK_TYPE_DIALOG, string, &info, 0);

    g_once_init_leave (&geda_font_dialog_type, type);
  }

  return geda_font_dialog_type;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool
is_a_geda_font_dialog (GedaFontDialog *font_dialog)
{
  if (G_IS_OBJECT(font_dialog)) {
    return (geda_font_dialog_get_type() == font_dialog->instance_type);
  }
  return FALSE;
}

GtkWidget* geda_font_dialog_new (void)
{
  return g_object_new (GEDA_TYPE_FONT_DIALOG, NULL);
}

GtkWidget* geda_font_dialog_new_with_title (const char *title)
{
  GedaFontDialog *dialog;

  dialog = g_object_new (GEDA_TYPE_FONT_DIALOG, NULL);

  if (title) {
    gtk_window_set_title (GTK_WINDOW (dialog), title);
  }

  return GTK_WIDGET (dialog);
}

GtkWidget* geda_font_dialog_new_with_font_name (const char *font_name)
{
  GedaFontDialog *dialog;

  dialog = g_object_new (GEDA_TYPE_FONT_DIALOG, NULL);

  if (font_name) {
    geda_font_dialog_set_font_name (GEDA_FONT_DIALOG (dialog), font_name);
  }

  return GTK_WIDGET (dialog);
}

GtkWidget*
geda_font_chooser_dialog_new (const char *title, GtkWindow   *parent)
{
  GedaFontDialog *dialog;

  dialog = g_object_new (GEDA_TYPE_FONT_DIALOG,
                         "title", title,
                         "transient-for", parent,
                         NULL);

  return GTK_WIDGET (dialog);
}

GdkFont*
geda_font_dialog_get_font (GedaFontDialog *dialog)
{
  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog), NULL);

  if (!dialog->font && dialog->font_desc) {
    dialog->font = gdk_font_from_description(dialog->font_desc);
  }

  return dialog->font;
}

PangoFontDescription*
geda_font_dialog_get_font_desc (GedaFontDialog *dialog)
{
  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog),  NULL);

  PangoFontDescription *font_desc;

  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog), NULL);

  font_desc = NULL;

  g_object_get (dialog, "font-desc", &font_desc, NULL);

  return font_desc;
}

void
geda_font_dialog_set_font_desc (GedaFontDialog *dialog, const PangoFontDescription *font_desc)
{
  g_return_if_fail (GEDA_IS_FONT_DIALOG (dialog));

  g_object_set (dialog, "font-desc", font_desc, NULL);

}

char *geda_font_dialog_get_font_name (GedaFontDialog *dialog)
{
  char *fontname;

  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog),  NULL);

  fontname = pango_font_description_to_string (dialog->font_desc);

  return fontname;
}

bool
geda_font_dialog_set_font_name (GedaFontDialog *dialog, const char *fontname)
{
  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog), FALSE);
  g_return_val_if_fail (fontname, FALSE);

  bool result;

  if (!gtk_widget_has_screen (GTK_WIDGET (dialog))) {
    result = FALSE;
  }
  else {

    PangoFontFamily      *family;
    PangoFontFace        *face;
    PangoFontDescription *new_desc;

    family   = NULL;
    face     = NULL;

    new_desc = pango_font_description_from_string (fontname);

    if (geda_font_dialog_select_font_desc (dialog, new_desc, &family, &face))
    {
      geda_font_dialog_ref_family (dialog, family);
      if (family) {
        g_object_unref (family);
      }

      geda_font_dialog_ref_face (dialog, face);
      if (face) {
        g_object_unref (face);
      }
    }

    pango_font_description_free (new_desc);
    result = TRUE;
  }
  return result;
}

/*! \brief Get Font Size property from Font Dialog
 *  \par Function Description
 *  This function retrieves the font size property.
 *
 * \retval interger value of the current font size setting.
 *
 */
int geda_font_dialog_get_font_size (GedaFontDialog *dialog)
{
  g_return_val_if_fail ( GEDA_IS_FONT_DIALOG (dialog), -1);

  return (dialog->font_size);
}

/*! \brief Set Font Size property on the Font Dialog
 *  \par Function Description
 *  This function sets the font size property.
 *
 * \param dialog:   Pointer to the dialog object.
 * \param new_size: The integer value of the new font size.
 *
 */
void
geda_font_dialog_set_font_size (GedaFontDialog *dialog, int new_size)
{
  g_return_if_fail (GEDA_IS_FONT_DIALOG (dialog));

  dialog->font_size = valid_font_size(new_size);

  pango_font_description_set_size (dialog->font_desc, dialog->font_size);

  geda_font_dialog_select_best_size (dialog);
}

/*! \brief geda_font_dialog_get_preview_text
 *
 *  \par Function Description
 *
 * Gets the text displayed in the preview area.
 *
 * \param [in] dialog: The GedaFontDialog object
 *
 * Return value: the text displayed in the preview area.
 *     This string is owned by the widget and should not be
 *     modified or freed
 */
const char*
geda_font_dialog_get_preview_text (GedaFontDialog *dialog)
{
  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog), NULL);

  return gtk_entry_get_text (GTK_ENTRY (dialog->preview_entry));
}

/*! \brief geda_font_dialog_set_preview_text
 *
 *  \par Function Description
 *
 * Sets the text displayed in the preview area.
 * The text is used to show how the selected font looks.
 *
 * \param [in] dialog: The GedaFontDialog object
 * \param [in] text:   Pointer to text to display in the preview area
 *
 */
bool
geda_font_dialog_set_preview_text (GedaFontDialog *dialog, const char *text)
{
  g_return_val_if_fail ( GEDA_IS_FONT_DIALOG (dialog), FALSE);
  g_return_val_if_fail (text != NULL, FALSE);

  gtk_entry_set_text (GTK_ENTRY (dialog->preview_entry), text);

  return TRUE;
}

bool
geda_font_dialog_get_show_preview (GedaFontDialog *dialog)
{
  g_return_val_if_fail (GEDA_IS_FONT_DIALOG (dialog), FALSE);

  return dialog->show_preview;
}

/*! \brief geda_font_dialog_set_show_preview_entry
 *
 *  \par Function Description
 *
 * Shows or hides the editable preview entry.
 *
 * \param [in] dialog        The GedaFontDialog object
 * \param [in] show_preview  whether to show the editable preview entry or not
 */
void
geda_font_dialog_set_show_preview (GedaFontDialog *dialog,
                                   bool            show_preview)
{
  g_return_if_fail (GEDA_IS_FONT_DIALOG (dialog));

  show_preview = show_preview != FALSE;
  dialog->show_preview = show_preview;

  g_object_set (dialog->preview_entry, "visible", show_preview, NULL);

}
/** @} end group GedaFontDialog */