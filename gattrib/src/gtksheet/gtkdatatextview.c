/* gtkdatatextview - data textview widget, based on GtkTextView
 * Copyright 2011  Fredy Paquet <fredy@opag.ch>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>

#define __GTKEXTRA_H_INSIDE__

#include <gtksheet/gtkcompat.h>
#include <gtksheet/gtkdatatextview.h>
#include <gtksheet/gtkdataformat.h>

/**
 * SECTION: gtkdatatextview
 * @short_description: a data textview widget, based on
 *      	     GtkTextView
 *
 * GtkDataTextView provides additional properties:
 *
 * - #GtkDataTextView:description - no functionality, a place for
 *   private information that cannot be put anywhere else
 *
 * - #GtkDataTextView:max-length - set the maximum character
 *   length for the contents of the widget.
 *
 * - #GtkDataTextView:max-length-bytes - set the maximum byte
 *   length for the contents of the widget.
 *
 * The main reason for this widget is to provide a length limit
 * for text contents, required by SQL database systems. There
 * is always a limit, no matter what you do.
 *
 * Some database systems may handle character length of UTF-8
 * strings correctly, others may not. Choose the appropriate
 * limit for your system, characters or bytes.
 *
 * Note that setting a byte length limit > 0 on a datatextview
 * may slow down text insertions. The  byte length limit is
 * imposed upon gtk_text_buffer_get_text() including invisible
 * content. See gtk_text_buffer_get_text() for details.
 *
 * Since: 3.0.6
 */

#undef GTK_DATA_TEXT_VIEW_DEBUG

#ifdef DEBUG
#define GTK_DATA_TEXT_VIEW_DEBUG  0  /* define to activate debug output */
#endif

#if GTK_DATA_TEXT_VIEW_DEBUG
#define GTK_DATA_TEXT_VIEW_DEBUG_SIGNAL  0  /* debug signal handlers */
#endif

#define GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE (G_MAXINT / 2)
#define GTK_DATA_TEXT_VIEW_COUNT_HIDDEN_BYTES  TRUE

#define GTK_DATA_TEXT_VIEW_CUT_INSERTED_TEXT 1  /* 1=cut pasted text to max len, 0=refuse */

enum {
    PROP_0,
    PROP_DATA_TEXT_VIEW_DESCRIPTION,
    PROP_DATA_TEXT_VIEW_MAX_LENGTH,
    PROP_DATA_TEXT_VIEW_MAX_LENGTH_BYTES,
} GTK_DATA_TEXT_VIEW_PROPERTIES;

enum {
    LAST_SIGNAL
} GTK_DATA_TEXT_VIEW_SIGNALS;

static void gtk_data_text_view_class_init(GtkDataTextViewClass *klass);
static void gtk_data_text_view_init(GtkDataTextView *data);

static GtkTextViewClass *parent_class = NULL;

/*!
 * \brief gtk_data_text_view_get_description
 * \par Function Description
 *  Retrieves the #GtkDataTextView description.
 *
 * \param [in] data_text_view a #GtkDataTextView
 *
 * \returns a pointer to the contents of the widget as a string. This
 *          string points to internally allocated storage in the widget
 *          and must not be freed, modified or stored.
 *
 * Since: 3.0.6
 */
const char *gtk_data_text_view_get_description(GtkDataTextView *data_text_view)
{
    g_return_val_if_fail(GTK_IS_DATA_TEXT_VIEW(data_text_view), NULL);

    return data_text_view->description;
}

/*!
 * \brief gtk_data_text_view_set_description
 * \par Function Description
 *  Sets the GtkDataTextView description.
 *
 * \param [in] data_text_view  A #GtkDataTextView
 * \param [in] description     The description or NULL
 *
 * Since: 3.0.6
 */
void
gtk_data_text_view_set_description(GtkDataTextView *data_text_view,
                                   const char      *description)
{
  g_return_if_fail(data_text_view != NULL);
  g_return_if_fail(GTK_IS_DATA_TEXT_VIEW(data_text_view));

  if (data_text_view->description) {
    g_free(data_text_view->description);
  }

  data_text_view->description = strdup(description);
}

/*!
 * \brief gtk_data_text_view_get_max_length
 * \par Function Description
 *  Retrieves the maximum character length for the contents of
 *  #GtkDataTextView.
 *
 * \param [in] data_text_view  A #GtkDataTextView
 *
 * \returns maximum byte length or 0.
 *
 * Since: 3.0.6
 **/
int gtk_data_text_view_get_max_length(GtkDataTextView *data_text_view)
{
    g_return_val_if_fail(GTK_IS_DATA_TEXT_VIEW(data_text_view), 0);

    return data_text_view->max_length;
}

/*!
 * \brief gtk_data_text_view_set_max_length
 * \par Function Description
 *  Sets the maximum character length for the contents of the
 *  #GtkDataTextView. Existing content will not be truncted.
 *
 * \param [in] data_text_view  A #GtkDataTextView
 * \param [in] max_length      Maximum character length or 0
 *
 * Since: 3.0.6
 */
void
gtk_data_text_view_set_max_length(GtkDataTextView *data_text_view,
                                  int max_length)
{
  g_return_if_fail(data_text_view != NULL);
  g_return_if_fail(GTK_IS_DATA_TEXT_VIEW(data_text_view));

  if (max_length < 0) {
    max_length = 0;
  }

  if (max_length > GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE) {
    max_length = GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE;
  }

  data_text_view->max_length = max_length;
}

/*!
 * \brief gtk_data_text_view_get_max_length_bytes
 * \par Function Description
 *  Retrieves the maximum byte length for the contents of
 * #GtkDataTextView data_format.
 *
 * \param [in] data_text_view  A #GtkDataTextView
 *
 * \returns  maximum byte length or 0.
 *
 * Since: 3.0.6
 **/
int gtk_data_text_view_get_max_length_bytes(GtkDataTextView *data_text_view)
{
  g_return_val_if_fail(GTK_IS_DATA_TEXT_VIEW(data_text_view), 0);

  return data_text_view->max_length_bytes;
}

/*!
 * \brief gtk_data_text_view_set_max_length_bytes
 * \par Function Description
 *  Sets the maximum byte length for the contents of the
 *  GtkDataTextView. Existing content will not be truncted.
 *
 * \param [in] data_text_view    A #GtkDataTextView
 * \param [in] max_length_bytes  Maximum byte length or 0
 *
 * Since: 3.0.6
 */
void gtk_data_text_view_set_max_length_bytes(GtkDataTextView *data_text_view,
                                             int max_length_bytes)
{
  g_return_if_fail(data_text_view != NULL);
  g_return_if_fail(GTK_IS_DATA_TEXT_VIEW(data_text_view));

  if (max_length_bytes < 0) {
    max_length_bytes = 0;
  }

  if (max_length_bytes > GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE) {
    max_length_bytes = GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE;
  }

  data_text_view->max_length_bytes = max_length_bytes;
}


GType gtk_data_text_view_get_type(void)
{
    static GType data_text_view_type = 0;

    if (!data_text_view_type) {

    static const GInterfaceInfo interface_info = {
        (GInterfaceInitFunc)NULL,
        (GInterfaceFinalizeFunc)NULL,
        (void*)NULL
    };

    data_text_view_type =
      g_type_register_static_simple(gtk_text_view_get_type(),
                                    "GtkDataTextView",
                                    sizeof(GtkDataTextViewClass),
                                    (GClassInitFunc)gtk_data_text_view_class_init,
                                    sizeof(GtkDataTextView),
                                    (GInstanceInitFunc)gtk_data_text_view_init,
                                    0);

    g_type_add_interface_static(data_text_view_type,
                                GTK_TYPE_BUILDABLE,
                                &interface_info);
    }

    return (data_text_view_type);
}

static void
gtk_data_text_view_set_property(GObject      *object,
                                unsigned int  prop_id,
                                const GValue *value,
                                GParamSpec   *pspec)
{
  GtkDataTextView *data_text_view = GTK_DATA_TEXT_VIEW(object);

  switch(prop_id) {

    case PROP_DATA_TEXT_VIEW_DESCRIPTION: {

      const char *description = g_value_get_string(value);

      if (!gtk_widget_get_realized(GTK_WIDGET(data_text_view))) {

        if (data_text_view->description) {
          g_free(data_text_view->description);
        }

        data_text_view->description = strdup(description);
      }
      else {
        gtk_data_text_view_set_description(data_text_view, description);
      }
    }
    break;

    case PROP_DATA_TEXT_VIEW_MAX_LENGTH:
      gtk_data_text_view_set_max_length(data_text_view, g_value_get_int(value));
      break;

    case PROP_DATA_TEXT_VIEW_MAX_LENGTH_BYTES:
      gtk_data_text_view_set_max_length_bytes(data_text_view, g_value_get_int(value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
      break;
  }
}

static void
gtk_data_text_view_get_property(GObject      *object,
                                unsigned int  prop_id,
                                GValue       *value,
                                GParamSpec   *pspec)
{
  GtkDataTextView *data_text_view = GTK_DATA_TEXT_VIEW(object);

  switch(prop_id) {

    case PROP_DATA_TEXT_VIEW_DESCRIPTION:
      g_value_set_string(value, data_text_view->description);
      break;

    case PROP_DATA_TEXT_VIEW_MAX_LENGTH:
      g_value_set_int(value, gtk_data_text_view_get_max_length(data_text_view));
      break;

    case PROP_DATA_TEXT_VIEW_MAX_LENGTH_BYTES:
      g_value_set_int(value, gtk_data_text_view_get_max_length_bytes(data_text_view));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
      break;
  }
}

static void
gtk_data_text_view_class_init(GtkDataTextViewClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
#if 0
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);
    GtkObjectClass *object_class = GTK_OBJECT_CLASS (klass);
    GtkTextViewClass *text_view_class = GTK_TEXT_VIEW_CLASS (klass);
#endif

    parent_class = g_type_class_ref(gtk_text_view_get_type());

    gobject_class->set_property = gtk_data_text_view_set_property;
    gobject_class->get_property = gtk_data_text_view_get_property;

    /**
     * GtkDataTextView:description:
     *
     * Description of the GtkDataTextView, no functionality, a place
     * for private information that cannot be put anywhere else.
    *
    * Since: 3.0.6
     */
    g_object_class_install_property(gobject_class,
                                    PROP_DATA_TEXT_VIEW_DESCRIPTION,
                                    g_param_spec_string("description",
                                                        "Description",
                                                        "Description of textview contents",
                                                        "" /* default value */,
                                                        G_PARAM_READWRITE));

    /**
     * GtkDataTextView:max-length:
     *
     * Set the maximum length in characters for the GtkDataTextView.
     * For details see #gtk_data_text_view_set_max_length.
     *
     * Sometimes, systems cannot handle UTF-8 string length
     * correctly, to overcome this problem, you can use the maximum
     * string length in bytes. When setting both limits, max-length
     *  and max-length-bytes, both must be fulfilled.
     *
     * Since: 3.0.6
     */
    g_object_class_install_property(gobject_class,
                                    PROP_DATA_TEXT_VIEW_MAX_LENGTH,
                                    g_param_spec_int("max-length",
                                                     "Maximum character length",
                                                     "The maximum number of characters for this textview. Zero if no maximum",
                                                     0, GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE,
                                                     0 /* default value */,
                                                     G_PARAM_READWRITE));

    /**
     * GtkDataTextView:max-length-bytes:
     *
     * Set the maximum length in bytes for the GtkDataTextView. For
     * details see #gtk_data_text_view_set_max_length_bytes.
     *
     * Sometimes, systems cannot handle UTF-8 string length
     * correctly, to overcome this problem, you can use the maximum
     * string length in bytes. When setting both limits, max-length
     *  and max-length-bytes, both must be fulfilled.
     *
     * Since: 3.0.6
     */
    g_object_class_install_property(gobject_class,
                                    PROP_DATA_TEXT_VIEW_MAX_LENGTH_BYTES,
                                    g_param_spec_int("max-length-bytes",
                                                     "Maximum bytes length",
                                                     "The maximum number of bytes for this textview. Zero if no maximum",
                                                     0, GTK_DATA_TEXT_VIEW_BUFFER_MAX_SIZE,
                                                     0 /* default value */,
                                                     G_PARAM_READWRITE));
}

/* Signal interception */

static void _gtk_data_text_view_insert_text_handler(GtkTextBuffer *textbuffer,
                                                    GtkTextIter   *location,
                                                    char          *new_text,
                                                    int new_text_len_bytes,
                                                    void *user_data)
{
  GtkDataTextView *data_text_view = GTK_DATA_TEXT_VIEW(user_data);
  GtkTextView *text_view = GTK_TEXT_VIEW(user_data);
  GtkTextBuffer *buffer = gtk_text_view_get_buffer(text_view);

  if (new_text_len_bytes < 0) new_text_len_bytes = strlen(new_text);

  int max_len_chars = data_text_view->max_length;

  if (max_len_chars) {

    int old_length_chars = gtk_text_buffer_get_char_count(buffer);
    int new_text_length_chars = g_utf8_strlen(new_text, new_text_len_bytes);

#if GTK_DATA_TEXT_VIEW_DEBUG_SIGNAL > 0
    g_debug("_gtk_data_text_view_insert_text_handler(chars): cl %d max %d new %d",
    old_length_chars, max_len_chars, new_text_length_chars);
#endif

    if (old_length_chars + new_text_length_chars > max_len_chars) {

#if GTK_DATA_TEXT_VIEW_CUT_INSERTED_TEXT > 0

      int remaining_chars = max_len_chars - old_length_chars;

      if (remaining_chars > 0) {

        char *cp = g_malloc0(new_text_len_bytes);

        g_utf8_strncpy(cp, new_text, remaining_chars);

        gtk_text_buffer_insert(textbuffer, location, cp, -1);

        g_free(cp);
      }

#endif

      gdk_beep();
      g_signal_stop_emission_by_name(textbuffer, "insert-text");
    }
  }

  int max_len_bytes = data_text_view->max_length_bytes;

  if (max_len_bytes) {

    GtkTextIter start, end;

    gtk_text_buffer_get_start_iter(buffer, &start);
    gtk_text_buffer_get_end_iter(buffer, &end);

    char *old_text = gtk_text_buffer_get_text(buffer,
                                              &start, &end,
                                              GTK_DATA_TEXT_VIEW_COUNT_HIDDEN_BYTES);
    int old_length_bytes = old_text ? strlen(old_text) : 0;
    g_free(old_text);

#if GTK_DATA_TEXT_VIEW_DEBUG_SIGNAL > 0
    g_debug("_gtk_data_text_view_insert_text_handler(bytes): cl %d max %d new %d",
    old_length_bytes, max_len_bytes, new_text_len_bytes);
#endif

    if (old_length_bytes + new_text_len_bytes > max_len_bytes) {

#if GTK_DATA_TEXT_VIEW_CUT_INSERTED_TEXT > 0

      int remaining_bytes = max_len_bytes - old_length_bytes;

      if (remaining_bytes > 0) {

        char *bpxx = &new_text[remaining_bytes];  /* byte position, may be invalid */
        char *cpe = g_utf8_find_prev_char(new_text, bpxx);

        if (cpe) {

          char *cpn = g_utf8_find_next_char(cpe, NULL);

          if (cpn && cpn <= bpxx) {
            cpe = bpxx;
          }

          char *cp = g_malloc0(new_text_len_bytes);

          strncpy(cp, new_text, cpe-new_text);

          gtk_text_buffer_insert(textbuffer, location, cp, -1);

          g_free(cp);
        }
      }

#endif

      gdk_beep();

      g_signal_stop_emission_by_name(textbuffer, "insert-text");
    }
  }
}

static void
gtk_data_text_view_init(GtkDataTextView *data_text_view)
{
#if 0
    GtkWidget *widget = GTK_WIDGET(data_text_view);
#endif

    data_text_view->description = NULL;
    data_text_view->max_length = 0;
    data_text_view->max_length_bytes = 0;

#if GTK_DATA_TEXT_VIEW_DEBUG > 0
    g_debug("gtk_data_text_view_init");
#endif

    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(data_text_view));

    g_signal_connect(buffer, "insert-text",
	G_CALLBACK(_gtk_data_text_view_insert_text_handler),
	data_text_view);
}

/*!
 * \brief gtk_data_text_view_new
 * \par Function Description
 *  Creates a new GtkDataTextView Widget.
 *
 * \returns the new GtkDataTextView Widget
 *
 * Since: 3.0.6
 */
GtkDataTextView *
gtk_data_text_view_new(void)
{
    GtkDataTextView *data_text_view = GTK_DATA_TEXT_VIEW(
	gtk_widget_new(gtk_data_text_view_get_type(), NULL));

    return (data_text_view);
}


