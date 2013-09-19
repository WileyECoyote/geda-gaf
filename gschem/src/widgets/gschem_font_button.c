/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
 *
 * Rewrite for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 *
 * Based on gtkfontbutton: 
 *     Copyright (C) 1998, David Abilleira Freijeiro <odaf@nexo.es>
 *     by David Abilleira Freijeiro, based on gnome-color-picker by
 *     Federico Mena <federico@nuclecu.unam.mx>. Modifed by the
 *     GTK+ Team and others, 2003
 */


#include "config.h"

#include "include/geda.h"

#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include <string.h>
#include <stdio.h>

#include "widgets/geda_label.h"
#include "widgets/gschem_font_button.h"
#include "widgets/geda_font_dialog.h"

#include "gettext.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GSCHEM_FONT_BUTTON_GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE ((obj), GSCHEM_TYPE_FONT_BUTTON, GschemFontButtonPrivate))

#define FONT_NAME font_button->priv->font_name

/* Signals */
enum
{
  FONT_SET,
  SIZE_SET,
  LAST_SIGNAL
};

enum
{
  PROP_0,
  PROP_TITLE,
  PROP_FONT,
  PROP_FONT_NAME,

  PROP_USE_FONT,
  PROP_FONT_SIZE,
  PROP_USE_SIZE,

  PROP_PREVIEW_TEXT,
  PROP_SHOW_PREVIEW_ENTRY,

  PROP_SHOW_STYLE,
  PROP_SHOW_SIZE
};

struct _GschemFontButtonPrivate
{
  char         *family;
  char         *font_name;
  int           font_size;

  unsigned int  use_font : 1;
  unsigned int  use_size : 1;

  unsigned int  show_style : 1;
  unsigned int  show_size : 1;
  unsigned int  show_preview : 1;

  GtkWidget    *font_dialog;
  GtkWidget    *inside;
  GtkWidget    *font_label;
  GtkWidget    *size_label;

  char         *preview_text;
  char         *label_text;
  char         *size_text;
};

/* Prototypes */
static void gschem_font_button_finalize          (GObject            *object);

static void gschem_font_button_get_property      (GObject            *object,
                                                  unsigned int        param_id,
                                                  GValue             *value,
                                                  GParamSpec         *pspec);
static void gschem_font_button_set_property      (GObject            *object,
                                                  unsigned int        param_id,
                                                  const GValue       *value,
                                                  GParamSpec         *pspec);

static void gschem_font_button_clicked           (GtkButton          *button);

/* Dialog response functions */
static void dialog_ok_clicked                    (GtkWidget         *widget,
                                                  gpointer           data);
static void dialog_cancel_clicked                (GtkWidget         *widget,
                                                  gpointer           data);
static void dialog_destroy                       (GtkWidget         *widget,
                                                  gpointer           data);

/* Auxiliary functions */
static GtkWidget *gschem_font_button_create_widgets  (GschemFontButton  *gfs);
static void gschem_font_button_label_set_font        (GschemFontButton  *gfs);

static unsigned int font_button_signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE (GschemFontButton, gschem_font_button, GTK_TYPE_BUTTON)

static void
clear_font_data (GschemFontButton *font_button)
{

  if (font_button->font_face && G_IS_OBJECT(font_button->font_face))
    g_object_unref (font_button->font_face);
  font_button->font_face = NULL;

  if (font_button->font_desc)
    pango_font_description_free (font_button->font_desc);
  font_button->font_desc = NULL;

  g_free (font_button->priv->font_name);
  font_button->priv->font_name = NULL;

  g_free (font_button->priv->family);
  font_button->priv->family = NULL;

  g_free(font_button->priv->label_text);
  font_button->priv->label_text = NULL;

  g_free(font_button->priv->size_text);
  font_button->priv->size_text = NULL;
}
/*
static void
clear_font_filter_data (GschemFontButton *font_button)
{
  GschemFontButtonPrivate *priv = font_button->priv;

  if (priv->font_filter_data_destroy)
    priv->font_filter_data_destroy (priv->font_filter_data);
  priv->font_filter = NULL;
  priv->font_filter_data = NULL;
  priv->font_filter_data_destroy = NULL;
}
*/
/*! \brief Font Button Label Use Font
 *  \par Function Description
 *   This function sets the font used by the embeded label widget.
 *  If use_font is TRUE then the label on the botton will be the
 *  drawn using the selected font, otherwise the default font will
 *  be used. If the selected font "style" is used then the size
 *  of the font will be use if use_size is TRUE. If use_font is
 *  false, the use_size is not used.
 * 
 * \param font_button Pointer to a GschemFontButton object
 *
 */
static void
gschem_font_button_label_set_font (GschemFontButton *font_button)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  PangoFontDescription *button_desc;

  if (font_button->priv->use_font) {

    button_desc = pango_font_description_copy (font_button->font_desc);

    if (button_desc) {

      if (!font_button->priv->use_size) {
        pango_font_description_unset_fields (button_desc,
                                             PANGO_FONT_MASK_SIZE);
      }

      if (!font_button->priv->show_style) {
        pango_font_description_unset_fields (button_desc,
                                             PANGO_FONT_MASK_STYLE);
      }

      gtk_widget_modify_font (font_button->priv->font_label, button_desc);

      pango_font_description_free (button_desc);
    }
  }
  else
    gtk_widget_set_style (font_button->priv->font_label, NULL);

}

/*! \brief Font Button Set Text on Font Button Label
 *  \par Function Description
 *   This function is used to update the font name and size label
 *   on the button. The label_text is not freed here.
 *
 * \param font_button Pointer to a GschemFontButton object
 *
 */
static void
gschem_font_button_label_set_text (GschemFontButton *font_button)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  GschemFontButtonPrivate *data;
  data = font_button->priv;

  /* Check for label_text and re-create if needed */
  if ( !data->label_text ) {

    if (data->show_style) {

      const char *face_name;
      face_name = pango_font_face_get_face_name (font_button->font_face);

      data->label_text = g_strconcat( data->family, " ", face_name, NULL);

    }
    else {
      data->label_text = g_strdup (data->family);
    }

  }

  /* Update the label on our button */
  geda_label_set_text ( GEDA_LABEL(data->font_label), data->label_text);

  if (data->show_size)  {
    geda_label_set_text ( GEDA_LABEL(data->size_label), data->size_text);
  }
}

/*! \brief Font Button Update Font Button Label
 *  \par Function Description
 *   This is a convenience function to update both the font and
 *  and the text on the button.
 *
 * \param font_button Pointer to a GschemFontButton object
 *
 */
static void
gschem_font_button_update_label (GschemFontButton *font_button)
{
  gschem_font_button_label_set_font (font_button);
  gschem_font_button_label_set_text (font_button);
}

/*! \brief Compare Font Descriptions
 *  \par Function Description
 *   This function compares members, see code, and returns true
 *  if the font descriptors are equivalent.
 *
 * \Note WEH: This is 1ms faster than using the library function
 *  pango_font_description_equal(), but maybe not enough to matter.
 *  Even with font WenQuanYi Micro Hei Mono 12.
 * 
 * \param a pointer to PangoFontDescription
 * \param b pointer to PangoFontDescription
 *
 * \retval boolean TRUE if descriptors are equivalent, otherwise FALSE
 *
 */
static bool
font_description_style_equal (const PangoFontDescription *a,
                              const PangoFontDescription *b)
{

  return (pango_font_description_get_weight (a)  == pango_font_description_get_weight (b) &&
          pango_font_description_get_style (a)   == pango_font_description_get_style (b) &&
          pango_font_description_get_stretch (a) == pango_font_description_get_stretch (b) &&
          pango_font_description_get_variant (a) == pango_font_description_get_variant (b));

}
/*! \brief Font Button Update Font Data
 *  \par Function Description
 *   This function update the font, font style, font_family, font_face,
 *  and family members based on the current PangoFontDescription.
 *
 * \param font_button Pointer to a GschemFontButton object
 *
 */
static void
gschem_font_button_update_font_data (GschemFontButton *font_button)
{
  PangoFontFamily **families;
  PangoFontFace   **faces;
  int n_families, n_faces, i;

  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));
  g_return_if_fail (font_button->font_desc != NULL);

  GschemFontButtonPrivate *data;
  data = font_button->priv;

  /* De-reference old objects */
  if (font_button->font_face)
    g_object_unref (font_button->font_face);

  /* Update the font name */
  if (data->font_name)
    g_free (data->font_name);

  data->font_name = pango_font_description_to_string (font_button->font_desc);

  /* Update the family */
  if (data->family) g_free (data->family);
  data->family = g_strdup ( pango_font_description_get_family (font_button->font_desc));

  /* Get a list of all families */
  n_families = 0;
  families = NULL;
  pango_context_list_families (gtk_widget_get_pango_context (GTK_WIDGET (font_button)),
                               &families, &n_families);

  /* Find our family */
  n_faces = 0;
  faces = NULL;
  for (i = 0; i < n_families; i++) {

    const char *name = pango_font_family_get_name (families[i]);

    if (!g_ascii_strcasecmp (name, data->family))
    {
      pango_font_family_list_faces (families[i], &faces, &n_faces);
      break;
    }
  }
  g_free (families);

  /* Find our face */
  for (i = 0; i < n_faces; i++) {
    PangoFontDescription *tmp_desc = pango_font_face_describe (faces[i]);

    if (font_description_style_equal (tmp_desc, font_button->font_desc))
    {
      font_button->font_face = g_object_ref (faces[i]);
      pango_font_description_free (tmp_desc);
      break;
    }
    else
      pango_font_description_free (tmp_desc);
  }
  g_free (faces);

  if (pango_font_description_get_size_is_absolute (font_button->font_desc))
    i = pango_font_description_get_size (font_button->font_desc);
  else
    i = pango_font_description_get_size (font_button->font_desc) / PANGO_SCALE;

  if ( i > MIN_FONT_SIZE - 1) {
    data->font_size = i;
  }

  if (data->size_text != NULL)
    g_free(data->size_text);

  data->size_text = gschem_font_button_get_ascii_size(font_button);
}

/*! \brief Font Button Update Font Info
 *  \par Function Description
 *   This function updates the all of the font information
 * based on the font name. If the name is null, a default
 * font name is assigned.
 *
 * \param font_button Pointer to a GschemFontButton object
 *
 */
static void
gschem_font_button_update_from_name (GschemFontButton *font_button)
{
  GschemFontButtonPrivate *data;

  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  data = font_button->priv;

  if ( FONT_NAME == NULL) {

    /* Is saftely net: should not need to do this */
    if (font_button->font_desc) {
      pango_font_description_free (font_button->font_desc);
      font_button->font_desc = NULL;
    }

    FONT_NAME = g_strdup (_(DEFAULT_FONT_NAME));
  }

  font_button->font_desc = pango_font_description_from_string (FONT_NAME);

  {
    int tmp_size;

    if (pango_font_description_get_size_is_absolute (font_button->font_desc))
      tmp_size = pango_font_description_get_size (font_button->font_desc);
    else
      tmp_size = pango_font_description_get_size (font_button->font_desc) / PANGO_SCALE;

    if (( tmp_size >= MIN_FONT_SIZE ) && (tmp_size <= MAX_FONT_SIZE)) {
      data->font_size = tmp_size;
    }
    else {
      if ( ( data->font_size < MIN_FONT_SIZE ) || (data->font_size > MAX_FONT_SIZE)) {
        data->font_size = DEFAULT_FONT_SIZE;
        pango_font_description_set_size (font_button->font_desc, data->font_size);
      }
    }
  }

  g_free(font_button->priv->label_text);
  font_button->priv->label_text = NULL;

  gschem_font_button_update_font_data (font_button);

  g_object_notify (G_OBJECT (font_button), "font-name");

}

/*! \brief GschemFontbuton GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GschemFontbuton GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
gschem_font_button_set_property (GObject *object, unsigned int param_id,
                           const GValue  *value,  GParamSpec *pspec)
{
  GschemFontButton *font_button = GSCHEM_FONT_BUTTON (object);

  switch (param_id) {
    case PROP_TITLE:
      gschem_font_button_set_title (font_button,
                                    g_value_get_string (value));
      break;

    case PROP_FONT:
    case PROP_FONT_NAME:
      gschem_font_button_set_font_name (font_button,
                                        g_value_get_string (value));
      break;

    case PROP_USE_FONT:
      gschem_font_button_set_use_font (font_button,
                                       g_value_get_boolean (value));
      break;
    case PROP_FONT_SIZE:
      gschem_font_button_set_size (font_button,
                                   g_value_get_int (value));
      break;
    case PROP_USE_SIZE:
      gschem_font_button_set_use_size (font_button,
                                       g_value_get_boolean (value));
      break;
    case PROP_PREVIEW_TEXT:
      gschem_font_button_set_preview_text (font_button,
                                           g_value_get_string (value));
      break;

    case PROP_SHOW_PREVIEW_ENTRY:
      gschem_font_button_set_show_preview (font_button,
                                           g_value_get_boolean (value));
      break;
    case PROP_SHOW_STYLE:
      gschem_font_button_set_show_style (font_button,
                                         g_value_get_boolean (value));
      break;
    case PROP_SHOW_SIZE:
      gschem_font_button_set_show_size (font_button,
                                        g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}

/*! \brief GschemFontbuton GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GschemFontbuton's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
gschem_font_button_get_property (GObject *object, unsigned int param_id,
                                 GValue  *value,  GParamSpec *pspec)
{
  GschemFontButton *font_button;
  font_button = GSCHEM_FONT_BUTTON (object);

  switch (param_id)
    {
    case PROP_TITLE:
      g_value_set_string (value, gschem_font_button_get_title (font_button));
      break;
    case PROP_FONT:
    case PROP_FONT_NAME:
      g_value_set_string (value, gschem_font_button_get_font_name (font_button));
      break;
    case PROP_USE_FONT:
      g_value_set_boolean (value, gschem_font_button_get_use_font (font_button));
      break;
    case PROP_FONT_SIZE:
      g_value_set_int (value, gschem_font_button_get_size (font_button));
      break;
    case PROP_USE_SIZE:
      g_value_set_boolean (value, gschem_font_button_get_use_size (font_button));
      break;
    case PROP_PREVIEW_TEXT:
      g_value_set_string (value, gschem_font_button_get_preview_text (font_button));
      break;
    case PROP_SHOW_PREVIEW_ENTRY:
      g_value_set_boolean (value, gschem_font_button_get_show_preview (font_button));
      break;
    case PROP_SHOW_STYLE:
      g_value_set_boolean (value, gschem_font_button_get_show_style (font_button));
      break;
    case PROP_SHOW_SIZE:
      g_value_set_boolean (value, gschem_font_button_get_show_size (font_button));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
    }
}

static void
gschem_font_button_finalize (GObject *object)
{
  GschemFontButton *font_button;
  font_button = GSCHEM_FONT_BUTTON (object);

  if (font_button->priv->font_dialog != NULL)
    gtk_widget_destroy (font_button->priv->font_dialog);

  font_button->priv->font_dialog = NULL;

  g_free (font_button->title);
  font_button->title = NULL;

  g_free(font_button->priv->preview_text);
  font_button->priv->preview_text = NULL;

  clear_font_data (font_button);

  G_OBJECT_CLASS (gschem_font_button_parent_class)->finalize (object);

}

/* Dialog Callback functions */
static void
dialog_ok_clicked (GtkWidget *dialog_apply_button, gpointer data)
{
  GschemFontButton *font_button;
  font_button = GSCHEM_FONT_BUTTON (data);

  gtk_widget_hide (font_button->priv->font_dialog);

  char *tmp_name;
  int   font_size;

  tmp_name   = geda_font_dialog_get_font_name (GEDA_FONT_DIALOG (font_button->priv->font_dialog));
  font_size  = geda_font_dialog_get_font_size(GEDA_FONT_DIALOG (font_button->priv->font_dialog));

  if ( !g_ascii_strcasecmp(tmp_name, font_button->priv->font_name) &&
     ( font_size == font_button->priv->font_size )) {
    g_free(tmp_name);
  }
  else { /* User selected a different font */

    clear_font_data (font_button);

    font_button->priv->font_name = tmp_name;
    font_button->priv->font_size = font_size;

    gschem_font_button_update_from_name (font_button);
    gschem_font_button_update_label (font_button);

    g_object_notify (G_OBJECT (font_button), "font-name");
    g_object_notify (G_OBJECT (font_button), "font-size");

    /* Emit font_set signal */
    g_signal_emit (font_button, font_button_signals[FONT_SET], 0);
  }
}

static void
dialog_cancel_clicked (GtkWidget *widget, gpointer data)
{
  GschemFontButton *font_button;
  font_button = GSCHEM_FONT_BUTTON (data);

  gtk_widget_hide (font_button->priv->font_dialog);
}

static void
dialog_destroy (GtkWidget *widget, gpointer data)
{
  GschemFontButton *font_button;
  font_button = GSCHEM_FONT_BUTTON (data);

  /* Dialog will get destroyed so reference is not valid now */
  font_button->priv->font_dialog = NULL;
}

/* End dialog callbacks */

static void
gschem_font_button_clicked (GtkButton *button)
{
  GedaFontDialog          *font_dialog;
  GschemFontButton        *font_button;
  GschemFontButtonPrivate *priv;

  font_button = GSCHEM_FONT_BUTTON (button);
  priv        = font_button->priv;

  if (!priv->font_dialog) {

    GtkWidget *parent;

    parent = gtk_widget_get_toplevel (GTK_WIDGET (font_button));

    priv->font_dialog = g_object_new (GEDA_TYPE_FONT_DIALOG,
                                      "title",      font_button->title,
                                      "font-name",  priv->font_name,
                                      "font-size",  priv->font_size,
                                      NULL);

    font_dialog = GEDA_FONT_DIALOG (priv->font_dialog);

    if (gtk_widget_is_toplevel (parent) && GTK_IS_WINDOW (parent)) {
      if (GTK_WINDOW (parent) != gtk_window_get_transient_for (GTK_WINDOW (font_dialog))) {
        gtk_window_set_transient_for (GTK_WINDOW (font_dialog), GTK_WINDOW (parent));
      }
      gtk_window_set_modal (GTK_WINDOW (font_dialog),
                            gtk_window_get_modal (GTK_WINDOW (parent)));
    }

    g_signal_connect (font_dialog->ok_button, "clicked",
                      G_CALLBACK (dialog_ok_clicked), font_button);

    g_signal_connect (font_dialog->cancel_button, "clicked",
                      G_CALLBACK (dialog_cancel_clicked), font_button);

    g_signal_connect (font_dialog, "destroy",
                      G_CALLBACK (dialog_destroy), font_button);
  }

  gtk_window_present (GTK_WINDOW (priv->font_dialog));
}

static void
gschem_font_button_class_init (GschemFontButtonClass *class)
{
  GObjectClass   *gobject_class;
  GtkButtonClass *button_class;
  GParamSpec     *params;

  gobject_class   = (GObjectClass *) class;
  button_class    = (GtkButtonClass *) class;

  gobject_class->finalize     = gschem_font_button_finalize;
  gobject_class->set_property = gschem_font_button_set_property;
  gobject_class->get_property = gschem_font_button_get_property;

  /* Virtual over-ride, we ARE a button */
  button_class->clicked       = gschem_font_button_clicked;

  class->font_set = NULL;
  class->size_set = NULL;

  /*! \property GschemFontButton:title:
   *  \par The title of the font selection dialog.
   */
  params = g_param_spec_string ("title",
                              _("Title"),
                              _("The title of the font selection dialog"),
                              _("Pick a Font"),
                               (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_TITLE, params);

  /*! \property GschemFontButton:font-name:
   *  \par The name of the currently selected font.
   */
  params = g_param_spec_string ("font-name",
                              _("Font name"),
                              _("The name of the selected font"),
                              _(DEFAULT_FONT_NAME),
                               (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_FONT_NAME, params);

  /*! \property GschemFontButton:use-font:
   *  \par If this property is set to %TRUE, the label will be drawn
   *       in the selected font.
   */
  params = g_param_spec_boolean ("use-font",
                               _("Use font in label"),
                               _("Whether the label is drawn in the selected font"),
                                  FALSE,
                                 (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_USE_FONT, params);

  /*! \property GschemFontButton:use-size:
   *  \par If this property is set to %TRUE, the label will be drawn
   *       with the selected font size.
   */
  params = g_param_spec_boolean ("use-size",
                               _("Use size in label"),
                               _("Whether the label is drawn with the selected font size"),
                                  FALSE,
                                 (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_USE_SIZE, params);

  /*! \property GschemFontButton:show-style:
   *  \par If this property is set to %TRUE, the name of the selected font style
   *       will be shown in the label. For a more WYSIWYG way to show the selected
   *       style, see the ::use-font property.
   */
  params = g_param_spec_boolean ("show-style",
                               _("Show style"),
                               _("Whether the selected font style is shown in the label"),
                                TRUE,
                               (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_SHOW_STYLE, params);

  /*! \property GschemFontButton:show-size:
   *  \par If this property is set to %TRUE, the selected font size will be shown
   *       in the label. For a more WYSIWYG way to show the selected size, see the
   *       ::use-size property.
   */
  params = g_param_spec_boolean ("show-size",
                               _("Show size"),
                               _("Whether selected font size is shown in the label"),
                                TRUE,
                               (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_SHOW_SIZE, params);

  /*! \property GschemFontButton:font-size:
   *  \par Programmactically set the font size.
   */
  params = g_param_spec_int ("font-size",
                           _("Set Size"), /* nick name */
                           _("Set point size of the font"), /* hint / blurb */
                              6, /* Min value */
                              96, /* Max value */
                              10, /* default_value */
                             (G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_FONT_SIZE, params);

  /*! \signal GschemFontButton::font-set:
   * @widget: the object which received the signal.
   *
   * The ::font-set signal is emitted when the user selects a font.
   * When receiving this signal, use gschem_font_button_get_font_name()
   * to find out which font was just selected.
   *
   * Note that this signal is only emitted when the <emphasis>user</emphasis>
   * changes the font. If you need to react to programmatic font changes
   * as well, use the notify::font-name signal.
   *
   * Since: 2.4
   */
  font_button_signals[FONT_SET] = g_signal_new (_("font-set"),
                                                G_TYPE_FROM_CLASS (gobject_class),
                                                G_SIGNAL_RUN_FIRST,
                                                G_STRUCT_OFFSET (GschemFontButtonClass, font_set),
                                                NULL, NULL,
                                                g_cclosure_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);

  font_button_signals[SIZE_SET] = g_signal_new (_("size-set"),
                                                G_TYPE_FROM_CLASS (gobject_class),
                                                G_SIGNAL_RUN_FIRST,
                                                G_STRUCT_OFFSET (GschemFontButtonClass, size_set),
                                                NULL, NULL,
                                                g_cclosure_marshal_VOID__VOID,
                                                G_TYPE_NONE, 0);

  g_type_class_add_private (gobject_class, sizeof (GschemFontButtonPrivate));
}

static void
gschem_font_button_init (GschemFontButton *font_button)
{
  font_button->priv = GSCHEM_FONT_BUTTON_GET_PRIVATE(font_button);

  GtkSettings *settings;
  const char  *fontbutton_tip;

  fontbutton_tip = _("Open font selection dialog");

  FONT_NAME = NULL;

  gtk_widget_set_tooltip_text (GTK_WIDGET(font_button), fontbutton_tip);

  if ( (settings = gtk_settings_get_default ()) != NULL ) {
    g_object_get (settings, "gtk-font-name", &FONT_NAME, NULL);
  }
  else {
    FONT_NAME    = g_strdup (_(DEFAULT_FONT_NAME));
  }

  /* Initialize fields */

  font_button->priv->use_font     = FALSE;
  font_button->priv->use_size     = FALSE;
  font_button->priv->font_size    = DEFAULT_FONT_SIZE;

  font_button->priv->show_style   = FALSE;
  font_button->priv->show_size    = TRUE;
  font_button->priv->show_preview = TRUE;

  font_button->priv->font_dialog  = NULL;
  font_button->font_face          = NULL;

  font_button->title              = g_strdup ("Select Font"); //(_("Select a Font"));
  font_button->priv->preview_text = g_strdup (DEFAULT_PREVIEW_TEXT);

  gschem_font_button_update_from_name (font_button);

  font_button->priv->inside = gschem_font_button_create_widgets (font_button);

  gtk_container_add (GTK_CONTAINER (font_button), font_button->priv->inside);

  {
    AtkObject *obj;
    obj = gtk_widget_get_accessible(GTK_WIDGET(font_button));
    atk_object_set_name (obj, _("Font button"));
    atk_object_set_description(obj,_(fontbutton_tip));
  }

}

/**
 * gschem_font_button_new:
 *
 * Creates a new font picker widget.
 *
 * Returns: a new font picker widget.
 *
 * Since: 2.4
 */
GtkWidget *
gschem_font_button_new (void)
{
  return g_object_new (GSCHEM_TYPE_FONT_BUTTON, NULL);
}

/**
 * gschem_font_button_new_with_font:
 * @fontname: Name of font to display in font selection dialog
 *
 * Creates a new font picker widget.
 *
 * Returns: a new font picker widget.
 *
 * Since: 2.4
 */
GtkWidget *
gschem_font_button_new_with_font (const char *fontname)
{
  return g_object_new (GSCHEM_TYPE_FONT_BUTTON, "font-name", fontname, NULL);
}

/************** Functions to support Properties ***************/

/**
 * gschem_font_button_set_title:
 * @font_button: a #GschemFontButton
 * @title: a string containing the font selection dialog title
 *
 * Sets the title for the font selection dialog.
 *
 * Since: 2.4
 */
void
gschem_font_button_set_title (GschemFontButton *font_button,
                              const char   *title)
{
  char *old_title;
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  old_title = font_button->title;
  font_button->title = g_strdup (title);
  g_free (old_title);

  if (font_button->priv->font_dialog)
    gtk_window_set_title (GTK_WINDOW (font_button->priv->font_dialog),
                          font_button->title);

  g_object_notify (G_OBJECT (font_button), "title");
}

/**
 * gschem_font_button_get_title:
 * @font_button: a #GschemFontButton
 *
 * Retrieves the title of the font selection dialog.
 *
 * Returns: an internal copy of the title string which must not be freed.
 *
 * Since: 2.4
 */
const char*
gschem_font_button_get_title (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), NULL);

  return font_button->title;
}

/**
 * gschem_font_button_get_use_font:
 * @font_button: a #GschemFontButton
 *
 * Returns whether the selected font is used in the label.
 *
 * Returns: whether the selected font is used in the label.
 *
 * Since: 2.4
 */
bool
gschem_font_button_get_use_font (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), FALSE);

  return font_button->priv->use_font;
}

/**
 * gschem_font_button_set_use_font:
 * @font_button: a #GschemFontButton
 * @use_font: If %TRUE, font name will be written using font chosen.
 *
 * If @use_font is %TRUE, the font name will be written using the selected font.
 *
 * Since: 2.4
 */
void
gschem_font_button_set_use_font (GschemFontButton *font_button, bool use_font)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  use_font = (use_font != FALSE);

  if (font_button->priv->use_font != use_font) {
    font_button->priv->use_font = use_font;

    if (use_font)
      gschem_font_button_label_set_font (font_button);
    else
      gtk_widget_set_style (font_button->priv->font_label, NULL);

    g_object_notify (G_OBJECT (font_button), "use-font");
  }
}

/**
 * gschem_font_button_get_font_name:
 * @font_button: a #GschemFontButton
 *
 * Retrieves the name of the currently selected font. This name includes
 * style and size information as well. If you want to render something
 * with the font, use this string with pango_font_description_from_string() .
 * If you're interested in peeking certain values (family name,
 * style, size, weight) just query these properties from the
 * #PangoFontDescription object.
 *
 * Returns: an internal copy of the font name which must not be freed.
 *
 * Since: 2.4
 */
const char *
gschem_font_button_get_font_name (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), NULL);

  return font_button->priv->font_name;
}

/*! \brief gschem_font_button_set_font_name
 *  \par Function Description
 * This function is used to updates the currently-displayed font in font
 * selector dialog. The return value is the value returned from the
 * geda_font_dialog_set_font_name() if the font selection dialog
 * exists, otherwise FALSE is returned.
 * 
 * \param font_button Pointer to a GschemFontButton object
 * \param font_name   Name of font to display in font selection dialog
 * 
 * \retval boolean    TRUE on success, otherwise FALSE.
 *
 */
bool
gschem_font_button_set_font_name (GschemFontButton *font_button,
                                  const char       *fontname)
{
  bool result;
  char *old_fontname;

  result = FALSE;

  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), result);

  if (g_ascii_strcasecmp (font_button->priv->font_name, fontname)) {
    old_fontname = font_button->priv->font_name;
    font_button->priv->font_name = g_strdup (fontname);
    g_free (old_fontname);

    gschem_font_button_update_from_name (font_button);
    gschem_font_button_update_label (font_button);

    if (font_button->priv->font_dialog)
      result = geda_font_dialog_set_font_name
              (GEDA_FONT_DIALOG (font_button->priv->font_dialog),
               font_button->priv->font_name);
    else
      result = FALSE;

    g_object_notify (G_OBJECT (font_button), "font-name");
  }

  return result;
}

/*! \brief  gschem_font_button_get_use_size
 *  \par Function Description
 *  Returns whether the selected size is used in the label.
 *
 * \param font_button Pointer to a GschemFontButton object
 * 
 * \retval boolean    boolean value of setting of use_size property.
 *
 */
bool gschem_font_button_get_use_size (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), FALSE);
  return font_button->priv->use_size;
}

/*! \brief gschem_font_button_set_use_size
 *  \par Function Description
 *  If use_size, font name on the button label will be written using the
 *  selected size.
 * 
 * \param font_button Pointer to a GschemFontButton object
 * \param use_size    boolean value of setting.
 *
 */
void gschem_font_button_set_use_size (GschemFontButton *font_button,
                                      bool              use_size)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  use_size = (use_size != FALSE);

  if (font_button->priv->use_size != use_size) {

    font_button->priv->use_size = use_size;

    gschem_font_button_label_set_font (font_button);

    g_object_notify (G_OBJECT (font_button), "use-size");
  }
}

/*! \brief Font Button Get Show Size Parameter
 *  \par Function Description
 *  Returns whether the font size will be shown in the label.
 *
 * \param font_button Pointer to a GschemFontButton object
 * 
 * \retval show_size  TRUE if the button font includes the size.
 *
 */
bool gschem_font_button_get_show_size (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), FALSE);

  return font_button->priv->show_size;
}

/*! \brief Font Button Set Show Size
 *  \par Function Description
 *  Set whether the font size will be displayed along with the name
 *  of the selected font on the face of the button.
 *
 * \param font_button Pointer to a GschemFontButton object
 * \param show_size   Boolean, if TRUE the font size should be displayed.
 *
 */
void
gschem_font_button_set_show_size (GschemFontButton *font_button, bool show_size)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  show_size = (show_size != FALSE);

  if (font_button->priv->show_size != show_size) {

    font_button->priv->show_size = show_size;

    gtk_container_remove (GTK_CONTAINER (font_button), font_button->priv->inside);
    gtk_widget_destroy (font_button->priv->inside);

    font_button->priv->inside = gschem_font_button_create_widgets (font_button);
    gtk_container_add (GTK_CONTAINER (font_button), font_button->priv->inside);

    g_object_notify (G_OBJECT (font_button), "show-size");
  }
}

char *gschem_font_button_get_ascii_size (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), NULL);

  return g_strdup_printf ("%d", font_button->priv->font_size);
}
/*! \brief Font Button Get Font Size
 *  \par Function Description
 *  Returns the selected size. Note that the value returned by the
 *  GTK interface may not be usefull, our is.
 *
 * \param  font_button Pointer to a GschemFontButton object
 * \retval int         size of the selected font.
 *
 */
int
gschem_font_button_get_size (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), -1);

  return font_button->priv->font_size;
}

/*! \brief Font Button Set Font Size
 *  \par Function Description
 *  Programmacticaly set the size of the font selected. This is usefull
 *  when setting up the button based on a user selection or a change of
 *  selection if button is part of a modeless dialog. Note that GTK does
 *  not provide this directly,gtk3 requires the font-name to be changed
 *  in order to change the size.
 *
 * \param  font_button Pointer to a GschemFontButton object
 * \retval int         size of the selected font.
 *
 */
void
gschem_font_button_set_size (GschemFontButton *font_button, int font_size)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));
  GschemFontButtonPrivate *data;
  data = font_button->priv;

  if ( data->font_size != font_size) {

    data->font_size = font_size;
    if ( data->use_size ) {
      gschem_font_button_label_set_font (font_button);
    }

    if (data->show_size  ) {
      /* reset the size_text variable */
      if (font_button->priv->size_text != NULL)
        g_free(font_button->priv->size_text);

      data->size_text = gschem_font_button_get_ascii_size(font_button);

      gschem_font_button_label_set_text (font_button);

      /* Check if dialog is loaded an update */
      if ( data->font_dialog ) {

        g_object_set (data->font_dialog, "font-size", data->font_size, NULL);
      }
    }
  }
}

/**
 * gschem_font_button_get_show_style:
 * @font_button: a #GschemFontButton
 *
 * Returns whether the name of the font style will be shown in the label.
 *
 * Return value: whether the font style will be shown in the label.
 *
 * Since: 2.4
 **/
bool gschem_font_button_get_show_style (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), FALSE);

  return font_button->priv->show_style;
}

/**
 * gschem_font_button_set_show_style:
 * @font_button: a #GschemFontButton
 * @show_style: %TRUE if font style should be displayed in label.
 *
 * If @show_style is %TRUE, the font style will be displayed along with name of the selected font.
 *
 * Since: 2.4
 */
void
gschem_font_button_set_show_style (GschemFontButton *font_button,
                                   bool              show_style)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  show_style = (show_style != FALSE);
  if (font_button->priv->show_style != show_style)
    {
      font_button->priv->show_style = show_style;

      g_free(font_button->priv->label_text);
      font_button->priv->label_text = NULL;

      gschem_font_button_label_set_text (font_button);

      g_object_notify (G_OBJECT (font_button), "show-style");
    }
}

bool gschem_font_button_get_show_preview (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), FALSE);
  return font_button->priv->show_preview;
}

void gschem_font_button_set_show_preview (GschemFontButton *font_button, bool enable)
{
  GschemFontButtonPrivate *priv;

  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  priv = font_button->priv;

  bool show_preview;

  show_preview = (enable != FALSE);
  priv->show_preview = show_preview;

  if (show_preview)
     geda_font_dialog_set_preview_text ((GedaFontDialog*)priv->font_dialog,
                                          priv->preview_text);
  else {
    geda_font_dialog_set_preview_text ((GedaFontDialog*)priv->font_dialog,"");
  }
}

const char*
gschem_font_button_get_preview_text (GschemFontButton *font_button)
{
  GschemFontButtonPrivate *priv;

  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), NULL);

  priv = font_button->priv;

  if (priv->font_dialog)
    return geda_font_dialog_get_preview_text ((GedaFontDialog*)priv->font_dialog);

  return g_strdup (priv->preview_text);
}

void
gschem_font_button_set_preview_text (GschemFontButton *font_button,
                                     const char       *preview_text)
{
  GschemFontButtonPrivate *priv;

  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  priv = font_button->priv;

  if (priv->font_dialog) {

    geda_font_dialog_set_preview_text  ((GedaFontDialog *)priv->font_dialog,
                                           preview_text);
  }

  g_free (priv->preview_text);
  priv->preview_text = g_strdup (preview_text);
}

/*! \brief Font Button Get Pango Font Description
 *  \par Function Description
 *   This function returns a pointer to the pango font
 *  description (structure).
 *
 * \param font_button Pointer to a GschemFontButton object
 *
 * \return descr Pointer to Pango Font Description
 */
const PangoFontDescription *
gschem_font_button_get_font_desc (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), NULL);
  return font_button->font_desc;
}
/*! \brief Font Button Set Pango Font Description
 *  \par Function Description
 *   This function set pango font description (structure)
 *  associated with the font button.
 *
 * \param font_button Pointer to a GschemFontButton object
 * \param descr       Pointer to Pango Font Description
 */
void
gschem_font_button_set_font_desc (GschemFontButton *font_button,
                                  PangoFontDescription * pfd)
{
  g_return_if_fail (GSCHEM_IS_FONT_BUTTON (font_button));

  if (font_button->font_desc)
    pango_font_description_free (font_button->font_desc);

  font_button->font_desc = pango_font_description_copy (pfd);

  gschem_font_button_update_font_data (font_button);
}

/*! \brief Font Button Contruct Internal Widgets
 *  \par Function
  widget = gtk_hbox_new (FALSE, 0);

  font_button->priv->font_label = gtk_label_new (_("Font"));
Description
 *   This function set pango font description (structure)
 *  associated with the font button.
 *
 * \param font_button Pointer to a GschemFontButton object
 * \param descr       Pointer to Pango Font Description
 */
static GtkWidget *
gschem_font_button_create_widgets (GschemFontButton *font_button)
{
  g_return_val_if_fail (GSCHEM_IS_FONT_BUTTON (font_button), NULL);

  GtkWidget *widget;
  GtkWidget *font_label;
  GtkWidget *size_label;

  gtk_widget_push_composite_child ();

  widget = gtk_hbox_new (FALSE, 0);
  gtk_widget_show(widget);

  font_label = geda_label_new (_("Font"));

  geda_label_widget_set_justify ( font_label, GTK_JUSTIFY_LEFT);
  gtk_box_pack_start (GTK_BOX (widget), font_label, TRUE, TRUE, 5);
  gtk_widget_show(font_label);

  if (font_button->priv->show_size) {
    gtk_box_pack_start (GTK_BOX (widget), gtk_vseparator_new (), FALSE, FALSE, 0);
    size_label = geda_label_new ("10");

    gtk_box_pack_start (GTK_BOX (widget), size_label, FALSE, FALSE, 5);
    gtk_widget_show(size_label);
    font_button->priv->size_label = size_label;
  }

  font_button->priv->font_label = font_label;

  gtk_widget_pop_composite_child ();

  gschem_font_button_update_label (font_button);

  return widget;
}
#undef FONT_NAME
