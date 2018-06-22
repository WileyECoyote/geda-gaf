/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: prop-editor.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * Code based on GTK 2.24.10 gtk/prop-editor.c (GPL)
 * Copyright (C) 2000 Red Hat, Inc.
 *
 * This Program is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
 * License.
 *
 * This Program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: March, 13, 2016
 * Adapted for gEDA by Wiley Edward Hill <wileyhill AT gmail.com>
 */

#include "../../config.h"

#include <string.h>

#include <gtk/gtk.h>

#include <geda/geda.h>
#include "prop-editor.h"
#include "../include/geda_widgets.h"

typedef struct
{
  void         *instance;
  GObject      *alive_object;
  unsigned int  id;
} DisconnectData;

static void
disconnect_func (void *data)
{
  DisconnectData *dd = data;

  g_signal_handler_disconnect (dd->instance, dd->id);
}

static void
signal_removed (void *data, GClosure *closure)
{
  DisconnectData *dd = data;

  g_object_steal_data (dd->alive_object, "alive-object-data");
  g_free (dd);
}

static int
is_child_property (GParamSpec *pspec)
{
  return g_param_spec_get_qdata (pspec, g_quark_from_string ("is-child-prop")) != NULL;
}

static void
mark_child_property (GParamSpec *pspec)
{
  g_param_spec_set_qdata (pspec, g_quark_from_string ("is-child-prop"),
			  GINT_TO_POINTER (TRUE));
}

static void
g_object_connect_property (GObject     *object,
                           GParamSpec  *spec,
                           GCallback    func,
                           void        *data,
                           GObject     *alive_object)
{
  GClosure       *closure;
  char           *with_detail;
  DisconnectData *dd;

  if (is_child_property (spec)) {
    with_detail = g_strconcat ("child-notify::", spec->name, NULL);
  }
  else {
    with_detail = g_strconcat ("notify::", spec->name, NULL);
  }

  dd = g_malloc (sizeof(DisconnectData));

  closure = g_cclosure_new (func, data, NULL);

  g_closure_add_invalidate_notifier (closure, dd, signal_removed);

  dd->id = g_signal_connect_closure (object, with_detail, closure, FALSE);

  dd->instance = object;
  dd->alive_object = alive_object;

  g_object_set_data_full (G_OBJECT (alive_object),
                          "alive-object-data",
                          dd,
                          disconnect_func);

  g_free (with_detail);
}

typedef struct
{
  GObject    *obj;
  GParamSpec *spec;
  int         modified_id;
} ObjectProperty;

static void
free_object_property (ObjectProperty *p)
{
  g_free (p);
}

static void
connect_controller (GObject     *controller,
                    const char  *signal,
                    GObject     *model,
                    GParamSpec  *spec,
                    GCallback    func)
{
  ObjectProperty *p;

  p = g_new (ObjectProperty, 1);
  p->obj = model;
  p->spec = spec;

  p->modified_id = g_signal_connect_data (controller, signal, func, p,
                                          (GClosureNotify)free_object_property, 0);
  g_object_set_data (controller, "object-property", p);
}

static void
block_controller (GObject *controller)
{
  ObjectProperty *p = g_object_get_data (controller, "object-property");

  if (p) {
    g_signal_handler_block (controller, p->modified_id);
  }
}

static void
unblock_controller (GObject *controller)
{
  ObjectProperty *p = g_object_get_data (controller, "object-property");

  if (p) {
    g_signal_handler_unblock (controller, p->modified_id);
  }
}

static void
int_modified (GtkAdjustment *adj, void *data)
{
  ObjectProperty *p = data;

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, (int) adj->value, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, (int) adj->value, NULL);
  }
}

static void
get_property_value (GObject *object, GParamSpec *pspec, GValue *value)
{
  if (is_child_property (pspec)) {

    GtkWidget *widget = GTK_WIDGET (object);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_get_property (GTK_CONTAINER (parent),
                                      widget, pspec->name, value);
  }
  else {
    g_object_get_property (object, pspec->name, value);
  }
}

static void
int_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (data);
  GValue val = { 0, };

  g_value_init (&val, G_TYPE_INT);

  get_property_value (object, pspec, &val);

  if (g_value_get_int (&val) != (int)adj->value) {

    block_controller (G_OBJECT (adj));
    gtk_adjustment_set_value (adj, g_value_get_int (&val));
    unblock_controller (G_OBJECT (adj));
  }

  g_value_unset (&val);
}

static void
uint_modified (GtkAdjustment *adj, void *data)
{
  ObjectProperty *p = data;

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, (unsigned int) adj->value, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, (unsigned int) adj->value, NULL);
  }
}

static void
uint_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (data);
  GValue val = { 0, };

  g_value_init (&val, G_TYPE_UINT);
  get_property_value (object, pspec, &val);

  if (g_value_get_uint (&val) != (unsigned int)adj->value) {

    block_controller (G_OBJECT (adj));
    gtk_adjustment_set_value (adj, g_value_get_uint (&val));
    unblock_controller (G_OBJECT (adj));
  }

  g_value_unset (&val);
}

static void
float_modified (GtkAdjustment *adj, void *data)
{
  ObjectProperty *p = data;

  if (is_child_property (p->spec))
  {
    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, (float) adj->value, NULL);
  }
  else
    g_object_set (p->obj, p->spec->name, (float) adj->value, NULL);
}

static void
float_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (data);
  GValue val = { 0, };

  g_value_init (&val, G_TYPE_FLOAT);
  get_property_value (object, pspec, &val);

  if (g_value_get_float (&val) != (float) adj->value) {
    block_controller (G_OBJECT (adj));
    gtk_adjustment_set_value (adj, g_value_get_float (&val));
    unblock_controller (G_OBJECT (adj));
  }

  g_value_unset (&val);
}

static void
double_modified (GtkAdjustment *adj, void *data)
{
  ObjectProperty *p = data;

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, (double) adj->value, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, (double) adj->value, NULL);
  }
}

static void
double_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (data);
  GValue val = { 0, };

  g_value_init (&val, G_TYPE_DOUBLE);
  get_property_value (object, pspec, &val);

  if (g_value_get_double (&val) != adj->value) {

    block_controller (G_OBJECT (adj));
    gtk_adjustment_set_value (adj, g_value_get_double (&val));
    unblock_controller (G_OBJECT (adj));
  }

  g_value_unset (&val);
}

static void
string_modified (GtkEntry *entry, void *data)
{
  ObjectProperty *p = data;
  const char *text;

  text = gtk_entry_get_text (entry);

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, text, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, text, NULL);
  }
}

static void
string_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkEntry *entry = GTK_ENTRY (data);
  GValue val = { 0, };
  const char *str;
  const char *text;

  g_value_init (&val, G_TYPE_STRING);
  get_property_value (object, pspec, &val);

  str = g_value_get_string (&val);

  if (str == NULL) {
    str = "";
  }

  text = gtk_entry_get_text (entry);

  if (strcmp (str, text) != 0) {

    block_controller (G_OBJECT (entry));
    gtk_entry_set_text (entry, str);
    unblock_controller (G_OBJECT (entry));
  }

  g_value_unset (&val);
}

static void
bool_modified (GtkToggleButton *tb, void *data)
{
  ObjectProperty *p = data;

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, (int) tb->active, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, (int) tb->active, NULL);
  }
}

static void
bool_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkToggleButton *tb = GTK_TOGGLE_BUTTON (data);
  GValue val = { 0, };

  g_value_init (&val, G_TYPE_BOOLEAN);
  get_property_value (object, pspec, &val);

  if (g_value_get_boolean (&val) != tb->active) {

    block_controller (G_OBJECT (tb));
    gtk_toggle_button_set_active (tb, g_value_get_boolean (&val));
    unblock_controller (G_OBJECT (tb));
  }

  gtk_label_set_text (GTK_LABEL (GTK_BIN (tb)->child), g_value_get_boolean (&val) ?
  "TRUE" : "FALSE");

  g_value_unset (&val);
}


static void
enum_modified (GedaComboBox *cb, void *data)
{
  ObjectProperty *p = data;
  int i;
  GEnumClass *eclass;

  eclass = G_ENUM_CLASS (g_type_class_peek (p->spec->value_type));

  i = geda_combo_box_get_active (cb);

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, eclass->values[i].value, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, eclass->values[i].value, NULL);
  }
}

static void
enum_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GedaComboBox *cb = GEDA_COMBO_BOX (data);
  GValue val = { 0, };
  GEnumClass *eclass;
  int i;

  eclass = G_ENUM_CLASS (g_type_class_peek (pspec->value_type));

  g_value_init (&val, pspec->value_type);
  get_property_value (object, pspec, &val);

  i = 0;
  while (i < eclass->n_values) {

    if (eclass->values[i].value == g_value_get_enum (&val))
      break;
    ++i;
  }

  if (geda_combo_box_get_active (cb) != i) {

    block_controller (G_OBJECT (cb));
    geda_combo_box_set_active (cb, i);
    unblock_controller (G_OBJECT (cb));
  }

  g_value_unset (&val);
}

static void
flags_modified (GtkCheckButton *button, void *data)
{
  ObjectProperty *p = data;
  int active;
  GFlagsClass *fclass;
  unsigned int flags;
  int i;

  fclass = G_FLAGS_CLASS (g_type_class_peek (p->spec->value_type));

  active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button));
  i = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (button), "index"));

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_get (GTK_CONTAINER (parent),
                             widget, p->spec->name, &flags, NULL);
    if (active)
      flags |= fclass->values[i].value;
    else
      flags &= ~fclass->values[i].value;

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, flags, NULL);
  }
  else {

    g_object_get (p->obj, p->spec->name, &flags, NULL);

    if (active)
      flags |= fclass->values[i].value;
    else
      flags &= ~fclass->values[i].value;

    g_object_set (p->obj, p->spec->name, flags, NULL);
  }
}

static void
flags_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GList *children, *c;
  GValue val = { 0, };
  GFlagsClass *fclass;
  unsigned int flags;
  int i;

  fclass = G_FLAGS_CLASS (g_type_class_peek (pspec->value_type));

  g_value_init (&val, pspec->value_type);
  get_property_value (object, pspec, &val);
  flags = g_value_get_flags (&val);
  g_value_unset (&val);

  children = gtk_container_get_children (GTK_CONTAINER (data));

  for (c = children, i = 0; c; c = c->next, i++) {

    block_controller (G_OBJECT (c->data));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (c->data),
                                  (fclass->values[i].value & flags) != 0);
    unblock_controller (G_OBJECT (c->data));
  }

  g_list_free (children);
}

static gunichar
unichar_get_value (GtkEntry *entry)
{
  const char *text = gtk_entry_get_text (entry);

  if (text[0])
    return g_utf8_get_char (text);
  else
    return 0;
}

static void
unichar_modified (GtkEntry *entry, void *data)
{
  ObjectProperty *p = data;
  gunichar val = unichar_get_value (entry);

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, val, NULL);
  }
  else {
    g_object_set (p->obj, p->spec->name, val, NULL);
  }
}

static void
unichar_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkEntry *entry = GTK_ENTRY (data);
  gunichar new_val;
  gunichar old_val = unichar_get_value (entry);
  GValue val = { 0, };
  char buf[7];
  int len;

  g_value_init (&val, pspec->value_type);
  get_property_value (object, pspec, &val);
  new_val = (gunichar)g_value_get_uint (&val);

  if (new_val != old_val) {

    if (!new_val)
      len = 0;
    else
      len = g_unichar_to_utf8 (new_val, buf);

    buf[len] = '\0';

    block_controller (G_OBJECT (entry));
    gtk_entry_set_text (entry, buf);
    unblock_controller (G_OBJECT (entry));
  }
}

static void
pointer_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkLabel *label = GTK_LABEL (data);
  char     *str;
  void     *ptr;

  g_object_get (object, pspec->name, &ptr, NULL);

  str = g_strdup_printf ("Pointer: %p", ptr);

  gtk_label_set_text (label, str);
  g_free (str);
}

static char *
object_label (GObject *obj, GParamSpec *pspec)
{
  const char *name;

  if (obj)
    name = g_type_name (G_TYPE_FROM_INSTANCE (obj));
  else if (pspec)
    name = g_type_name (G_PARAM_SPEC_VALUE_TYPE (pspec));
  else
    name = "unknown";

  return g_strdup_printf ("Object: %p (%s)", obj, name);
}

static void
object_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GList     *children;
  GtkWidget *button;
  GtkWidget *label;
  char      *str;
  GObject   *obj;

  children = gtk_container_get_children (GTK_CONTAINER (data));
  label    = GTK_WIDGET (children->data);
  button   = GTK_WIDGET (children->next->data);

  g_object_get (object, pspec->name, &obj, NULL);
  g_list_free (children);

  str = object_label (obj, pspec);

  gtk_label_set_text (GTK_LABEL (label), str);
  gtk_widget_set_sensitive (button, G_IS_OBJECT (obj));

  if (obj) {
    g_object_unref (obj);
  }

  g_free (str);
}

static void
model_destroy (void *data)
{
  g_object_steal_data (data, "model-object");
  gtk_widget_destroy (data);
}

static void
window_destroy (void *data)
{
  g_object_steal_data (data, "prop-editor-win");
}

static void
object_properties (GtkWidget *button, GObject *object)
{
  char    *name;
  GObject *obj;

  name = (char *) g_object_get_data (G_OBJECT (button), "property-name");

  g_object_get (object, name, &obj, NULL);

  if (G_IS_OBJECT (obj))
    create_prop_editor (obj, 0);
}

static void
color_modified (GtkColorButton *cb, void *data)
{
  ObjectProperty *p = data;
  GdkColor color;

  gtk_color_button_get_color (cb, &color);

  if (is_child_property (p->spec)) {

    GtkWidget *widget = GTK_WIDGET (p->obj);
    GtkWidget *parent = gtk_widget_get_parent (widget);

    gtk_container_child_set (GTK_CONTAINER (parent),
                             widget, p->spec->name, &color, NULL);
  }
  else  {
    g_object_set (p->obj, p->spec->name, &color, NULL);
  }
}

static void
color_changed (GObject *object, GParamSpec *pspec, void *data)
{
  GtkColorButton *cb = GTK_COLOR_BUTTON (data);
  GValue val = { 0, };
  GdkColor *color;
  GdkColor cb_color;

  g_value_init (&val, GDK_TYPE_COLOR);
  get_property_value (object, pspec, &val);

  color = g_value_get_boxed (&val);
  gtk_color_button_get_color (cb, &cb_color);

  if (color != NULL && !gdk_color_equal (color, &cb_color)) {

      block_controller (G_OBJECT (cb));
      gtk_color_button_set_color (cb, color);
      unblock_controller (G_OBJECT (cb));
    }

  g_value_unset (&val);
}

static GtkWidget *
property_widget (GObject *object, GParamSpec *spec, int can_modify)
{
  GtkWidget *prop_edit;
  GtkAdjustment *adj;
  char *msg;
  GType type = G_PARAM_SPEC_TYPE (spec);

  if (type == G_TYPE_PARAM_INT) {

    adj = GTK_ADJUSTMENT (gtk_adjustment_new (G_PARAM_SPEC_INT (spec)->default_value,
                                              G_PARAM_SPEC_INT (spec)->minimum,
                                              G_PARAM_SPEC_INT (spec)->maximum,
                                              1,
                                              MAX ((G_PARAM_SPEC_INT (spec)->maximum -
                                              G_PARAM_SPEC_INT (spec)->minimum) / 10, 1),
                                              0.0));

    prop_edit = gtk_spin_button_new (adj, 1.0, 0);

    g_object_connect_property (object, spec,
                               G_CALLBACK (int_changed),
                               adj, G_OBJECT (adj));

    if (can_modify) {
      connect_controller (G_OBJECT (adj), "value_changed",
                          object, spec, G_CALLBACK (int_modified));
    }
  }
  else if (type == G_TYPE_PARAM_UINT) {

    adj = GTK_ADJUSTMENT (
      gtk_adjustment_new (G_PARAM_SPEC_UINT (spec)->default_value,
                          G_PARAM_SPEC_UINT (spec)->minimum,
                          G_PARAM_SPEC_UINT (spec)->maximum,
                          1,
                          MAX ((G_PARAM_SPEC_UINT (spec)->maximum -
                          G_PARAM_SPEC_UINT (spec)->minimum) / 10, 1),
                          0.0));

    prop_edit = gtk_spin_button_new (adj, 1.0, 0);

    g_object_connect_property (object, spec,
                               G_CALLBACK (uint_changed),
                               adj, G_OBJECT (adj));

    if (can_modify) {
      connect_controller (G_OBJECT (adj), "value_changed",
                          object, spec, G_CALLBACK (uint_modified));
    }
  }
  else if (type == G_TYPE_PARAM_FLOAT) {

    adj = GTK_ADJUSTMENT (gtk_adjustment_new (G_PARAM_SPEC_FLOAT (spec)->default_value,
                                              G_PARAM_SPEC_FLOAT (spec)->minimum,
                                              G_PARAM_SPEC_FLOAT (spec)->maximum,
                                              0.1,
                                              MAX ((G_PARAM_SPEC_FLOAT (spec)->maximum -
                                              G_PARAM_SPEC_FLOAT (spec)->minimum) / 10, 0.1),
                                              0.0));

    prop_edit = gtk_spin_button_new (adj, 0.1, 2);

    g_object_connect_property (object, spec,
                               G_CALLBACK (float_changed),
                               adj, G_OBJECT (adj));

    if (can_modify) {
      connect_controller (G_OBJECT (adj), "value_changed",
                          object, spec, G_CALLBACK (float_modified));
    }
  }
  else if (type == G_TYPE_PARAM_DOUBLE) {

    adj = GTK_ADJUSTMENT (gtk_adjustment_new (G_PARAM_SPEC_DOUBLE (spec)->default_value,
                                              G_PARAM_SPEC_DOUBLE (spec)->minimum,
                                              G_PARAM_SPEC_DOUBLE (spec)->maximum,
                                              0.1,
                                              MAX ((G_PARAM_SPEC_DOUBLE (spec)->maximum -
                                              G_PARAM_SPEC_DOUBLE (spec)->minimum) / 10, 0.1),
                                              0.0));

    prop_edit = gtk_spin_button_new (adj, 0.1, 2);

    g_object_connect_property (object, spec,
                               G_CALLBACK (double_changed),
                               adj, G_OBJECT (adj));

    if (can_modify) {
      connect_controller (G_OBJECT (adj), "value_changed",
                          object, spec, G_CALLBACK (double_modified));
    }
  }
  else if (type == G_TYPE_PARAM_STRING) {

    prop_edit = gtk_entry_new ();

    g_object_connect_property (object, spec,
                               G_CALLBACK (string_changed),
                               prop_edit, G_OBJECT (prop_edit));

    if (can_modify) {
      connect_controller (G_OBJECT (prop_edit), "changed",
                          object, spec, G_CALLBACK (string_modified));
    }
  }
  else if (type == G_TYPE_PARAM_BOOLEAN) {

    prop_edit = gtk_toggle_button_new_with_label ("");

    g_object_connect_property (object, spec,
                               G_CALLBACK (bool_changed),
                               prop_edit, G_OBJECT (prop_edit));

    if (can_modify) {
      connect_controller (G_OBJECT (prop_edit), "toggled",
                          object, spec, G_CALLBACK (bool_modified));
    }
  }
  else if (type == G_TYPE_PARAM_ENUM) {

    GEnumClass *eclass;
    int j;

    prop_edit = geda_combo_box_text_new ();

    eclass = G_ENUM_CLASS (g_type_class_ref (spec->value_type));

    j = 0;
    while (j < eclass->n_values) {

      geda_combo_box_text_append_text (GEDA_COMBO_BOX_TEXT (prop_edit),
                                      eclass->values[j].value_name);
      ++j;
    }

    g_type_class_unref (eclass);

    g_object_connect_property (object, spec,
                               G_CALLBACK (enum_changed),
                               prop_edit, G_OBJECT (prop_edit));

    if (can_modify) {
      connect_controller (G_OBJECT (prop_edit), "changed",
                          object, spec, G_CALLBACK (enum_modified));
    }
  }
  else if (type == G_TYPE_PARAM_FLAGS) {

    GFlagsClass *fclass;
    int j;

    prop_edit = gtk_vbox_new (FALSE, 0);

    fclass = G_FLAGS_CLASS (g_type_class_ref (spec->value_type));

    for (j = 0; j < fclass->n_values; j++) {

      GtkWidget *b;

      b = gtk_check_button_new_with_label (fclass->values[j].value_name);
      g_object_set_data (G_OBJECT (b), "index", GINT_TO_POINTER (j));
      gtk_widget_show (b);
      gtk_box_pack_start (GTK_BOX (prop_edit), b, FALSE, FALSE, 0);
      if (can_modify)
        connect_controller (G_OBJECT (b), "toggled",
                            object, spec, G_CALLBACK (flags_modified));
    }

    g_type_class_unref (fclass);

    g_object_connect_property (object, spec,
                               G_CALLBACK (flags_changed),
                               prop_edit, G_OBJECT (prop_edit));
  }
  else if (type == G_TYPE_PARAM_UNICHAR) {

    prop_edit = gtk_entry_new ();
    gtk_entry_set_max_length (GTK_ENTRY (prop_edit), 1);

    g_object_connect_property (object, spec,
                               G_CALLBACK (unichar_changed),
                               prop_edit, G_OBJECT (prop_edit));

    if (can_modify) {
      connect_controller (G_OBJECT (prop_edit), "changed",
                          object, spec, G_CALLBACK (unichar_modified));
    }
  }
  else if (type == G_TYPE_PARAM_POINTER) {

    prop_edit = gtk_label_new ("");

    g_object_connect_property (object, spec,
                               G_CALLBACK (pointer_changed),
                               prop_edit, G_OBJECT (prop_edit));
  }
  else if (type == G_TYPE_PARAM_OBJECT) {

    GtkWidget *label, *button;

    prop_edit = gtk_hbox_new (FALSE, 5);

    label = gtk_label_new ("");
    button = gtk_button_new_with_label ("Properties");
    g_object_set_data (G_OBJECT (button), "property-name", (char*)spec->name);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (object_properties),
                      object);

    gtk_container_add (GTK_CONTAINER (prop_edit), label);
    gtk_container_add (GTK_CONTAINER (prop_edit), button);

    g_object_connect_property (object, spec,
                               G_CALLBACK (object_changed),
                               prop_edit, G_OBJECT (label));
  }
  else if (type == G_TYPE_PARAM_BOXED &&
           G_PARAM_SPEC_VALUE_TYPE (spec) == GDK_TYPE_COLOR)
  {
    prop_edit = gtk_color_button_new ();

    g_object_connect_property (object, spec,
                               G_CALLBACK (color_changed),
                               prop_edit, G_OBJECT (prop_edit));

    if (can_modify)
      connect_controller (G_OBJECT (prop_edit), "color-set",
                          object, spec, G_CALLBACK (color_modified));
  }
  else {

    msg = g_strdup_printf ("uneditable property type: %s",
                           g_type_name (G_PARAM_SPEC_TYPE (spec)));
    prop_edit = gtk_label_new (msg);
    g_free (msg);
    gtk_misc_set_alignment (GTK_MISC (prop_edit), 0.0, 0.5);
  }

  return prop_edit;
}

static GtkWidget *
properties_from_type (GObject *object, GType type)
{
  GtkWidget   *prop_edit;
  GtkWidget   *label;
  GtkWidget   *sw;
  GtkWidget   *vbox;
  GtkWidget   *table;
  GParamSpec **specs;
  unsigned int n_specs;
  int i;

  if (G_TYPE_IS_INTERFACE (type)) {

    void *vtable;

    vtable = g_type_default_interface_peek (type);
    specs  = g_object_interface_list_properties (vtable, &n_specs);
  }
  else {

    GObjectClass *class;

    class = G_OBJECT_CLASS (g_type_class_peek (type));
    specs = g_object_class_list_properties (class, &n_specs);
  }

  if (n_specs == 0) {
    g_free (specs);
    return NULL;
  }

  table = gtk_table_new (n_specs, 2, FALSE);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
  gtk_table_set_row_spacings (GTK_TABLE (table), 3);

  i = 0;
  while (i < n_specs) {

    GParamSpec *spec = specs[i];
    int can_modify;

    prop_edit = NULL;

    can_modify = ((spec->flags & G_PARAM_WRITABLE) != 0 &&
    (spec->flags & G_PARAM_CONSTRUCT_ONLY) == 0);

    if ((spec->flags & G_PARAM_READABLE) == 0) {

      /* can't display unreadable properties */
      ++i;
      continue;
    }

    if (spec->owner_type != type) {

      /* we're only interested in params of type */
      ++i;
      continue;
    }

    label = gtk_label_new (g_param_spec_get_nick (spec));
    gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.5);
    gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, i, i + 1);

    prop_edit = property_widget (object, spec, can_modify);
    gtk_table_attach_defaults (GTK_TABLE (table), prop_edit, 1, 2, i, i + 1);

    if (prop_edit) {

      if (!can_modify) {
        gtk_widget_set_sensitive (prop_edit, FALSE);
      }

      if (g_param_spec_get_blurb (spec)) {
        gtk_widget_set_tooltip_text (prop_edit, g_param_spec_get_blurb (spec));
      }

      /* set initial value */
      g_object_notify (object, spec->name);
    }

    ++i;
  }

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, FALSE, 0);

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), vbox);

  g_free (specs);

  return sw;
}

static GtkWidget *
child_properties_from_object (GObject *object)
{
  GtkWidget *prop_edit;
  GtkWidget *label;
  GtkWidget *sw;
  GtkWidget *vbox;
  GtkWidget *table;
  GtkWidget *parent;
  GParamSpec **specs;
  unsigned int n_specs;
  int i;

  if (!GTK_IS_WIDGET (object)) {
    return NULL;
  }

  parent = gtk_widget_get_parent (GTK_WIDGET (object));

  if (!parent) {
    return NULL;
  }

  specs = gtk_container_class_list_child_properties (G_OBJECT_GET_CLASS (parent), &n_specs);

  table = gtk_table_new (n_specs, 2, FALSE);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
  gtk_table_set_row_spacings (GTK_TABLE (table), 3);

  i = 0;
  while (i < n_specs) {

    GParamSpec *spec = specs[i];
    int can_modify;

    prop_edit = NULL;

    can_modify = ((spec->flags & G_PARAM_WRITABLE) != 0 &&
    (spec->flags & G_PARAM_CONSTRUCT_ONLY) == 0);

    if ((spec->flags & G_PARAM_READABLE) == 0) {

      /* can't display unreadable properties */
      ++i;
      continue;
    }

    label = gtk_label_new (g_param_spec_get_nick (spec));
    gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.5);
    gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, i, i + 1);

    mark_child_property (spec);
    prop_edit = property_widget (object, spec, can_modify);
    gtk_table_attach_defaults (GTK_TABLE (table), prop_edit, 1, 2, i, i + 1);

    if (prop_edit) {

      if (!can_modify) {
        gtk_widget_set_sensitive (prop_edit, FALSE);
      }

      if (g_param_spec_get_blurb (spec)) {
        gtk_widget_set_tooltip_text (prop_edit, g_param_spec_get_blurb (spec));
      }

      /* set initial value */
      gtk_widget_child_notify (GTK_WIDGET (object), spec->name);
    }

    ++i;
  }

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, FALSE, 0);

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), vbox);

  g_free (specs);

  return sw;
}

static void
child_properties (GtkWidget *button, GObject *object)
{
  create_prop_editor (object, 0);
}

static GtkWidget *
children_from_object (GObject *object)
{
  GList *children, *c;
  GtkWidget *table, *label, *prop_edit, *button, *vbox, *sw;
  char *str;
  int i;

  if (!GTK_IS_CONTAINER (object)) {
    return NULL;
  }

  children = gtk_container_get_children (GTK_CONTAINER (object));

  table = gtk_table_new (g_list_length (children), 2, FALSE);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
  gtk_table_set_row_spacings (GTK_TABLE (table), 3);

  for (c = children, i = 0; c; c = c->next, i++) {

    object = c->data;

    label = gtk_label_new ("Child");
    gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.5);
    gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, i, i + 1);

    prop_edit = gtk_hbox_new (FALSE, 5);

    str = object_label (object, NULL);
    label = gtk_label_new (str);
    g_free (str);
    button = gtk_button_new_with_label ("Properties");
    g_signal_connect (button, "clicked",
                      G_CALLBACK (child_properties),
                      object);

    gtk_container_add (GTK_CONTAINER (prop_edit), label);
    gtk_container_add (GTK_CONTAINER (prop_edit), button);

    gtk_table_attach_defaults (GTK_TABLE (table), prop_edit, 1, 2, i, i + 1);
  }

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, FALSE, 0);

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), vbox);

  g_list_free (children);

  return sw;
}

static GtkWidget *
cells_from_object (GObject *object)
{
  GList *cells, *c;
  GtkWidget *table, *label, *prop_edit, *button, *vbox, *sw;
  char *str;
  int i;

  if (!GTK_IS_CELL_LAYOUT (object)) {
    return NULL;
  }

  cells = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (object));

  table = gtk_table_new (g_list_length (cells), 2, FALSE);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
  gtk_table_set_row_spacings (GTK_TABLE (table), 3);

  for (c = cells, i = 0; c; c = c->next, i++) {

    object = c->data;

    label = gtk_label_new ("Cell");
    gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.5);
    gtk_table_attach_defaults (GTK_TABLE (table), label, 0, 1, i, i + 1);

    prop_edit = gtk_hbox_new (FALSE, 5);

    str = object_label (object, NULL);
    label = gtk_label_new (str);
    g_free (str);
    button = gtk_button_new_with_label ("Properties");
    g_signal_connect (button, "clicked",
                      G_CALLBACK (child_properties),
                      object);

    gtk_container_add (GTK_CONTAINER (prop_edit), label);
    gtk_container_add (GTK_CONTAINER (prop_edit), button);

    gtk_table_attach_defaults (GTK_TABLE (table), prop_edit, 1, 2, i, i + 1);
  }

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, FALSE, 0);

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                  GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), vbox);

  g_list_free (cells);

  return sw;
}

/* Pass zero for type if you want all properties */
GtkWidget*
create_prop_editor (GObject *object, GType type)
{
  GtkWidget *win;
  GtkWidget *notebook;
  GtkWidget *properties;
  GtkWidget *label;
  char *title;
  GType *ifaces;
  unsigned int n_ifaces;

  if ((win = g_object_get_data (G_OBJECT (object), "prop-editor-win"))) {

    gtk_window_present (GTK_WINDOW (win));
    return win;
  }

  win = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  if (GTK_IS_WIDGET (object)) {
    gtk_window_set_screen (GTK_WINDOW (win),
                           gtk_widget_get_screen (GTK_WIDGET (object)));
  }

  /* hold a weak ref to the object we're editing */
  g_object_set_data_full (G_OBJECT (object), "prop-editor-win", win, model_destroy);
  g_object_set_data_full (G_OBJECT (win), "model-object", object, window_destroy);

  if (type == 0) {

    notebook = gtk_notebook_new ();
    gtk_notebook_set_tab_pos (GTK_NOTEBOOK (notebook), GTK_POS_LEFT);

    gtk_container_add (GTK_CONTAINER (win), notebook);

    type = G_TYPE_FROM_INSTANCE (object);

    title = g_strdup_printf ("Properties of %s widget", g_type_name (type));
    gtk_window_set_title (GTK_WINDOW (win), title);
    g_free (title);

    while (type) {

      properties = properties_from_type (object, type);

      if (properties) {
        label = gtk_label_new (g_type_name (type));
        gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                                  properties, label);
      }

      type = g_type_parent (type);
    }

    ifaces = g_type_interfaces (G_TYPE_FROM_INSTANCE (object), &n_ifaces);
    while (n_ifaces--) {

      properties = properties_from_type (object, ifaces[n_ifaces]);

      if (properties) {
        label = gtk_label_new (g_type_name (ifaces[n_ifaces]));
        gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                                  properties, label);
      }
    }

    g_free (ifaces);

    properties = child_properties_from_object (object);
    if (properties) {

      label = gtk_label_new ("Child properties");
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                                properties, label);
    }

    properties = children_from_object (object);
    if (properties) {

      label = gtk_label_new ("Children");
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                                properties, label);
    }

    properties = cells_from_object (object);
    if (properties) {

      label = gtk_label_new ("Cell renderers");
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                                properties, label);
    }
  }
  else {

    properties = properties_from_type (object, type);
    gtk_container_add (GTK_CONTAINER (win), properties);
    title = g_strdup_printf ("Properties of %s", g_type_name (type));
    gtk_window_set_title (GTK_WINDOW (win), title);
    g_free (title);
  }

  gtk_window_set_default_size (GTK_WINDOW (win), -1, 400);

  gtk_widget_show_all (win);

  return win;
}

