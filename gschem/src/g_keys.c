/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"
#include "i_actions.h"

/*
 * #ifdef HAVE_LIBDMALLOC
 * #include <dmalloc.h>
 * #endif
 */
#include <gdk/gdkkeysyms.h>

#define DEFINE_H_KEYS(name)                       \
SCM h_keys_ ## name(SCM rest)                     \
{                                                 \
GSCHEM_TOPLEVEL *w_current = g_current_window (); \
i_callback_ ## name(w_current, 0, NULL);          \
return SCM_BOOL_T;                                \
}

#define DEFINE_BUFFER_KEY_FUNC(name, number)        \
SCM buffer_ ## name ## number(SCM rest)             \
{                                                   \
  GSCHEM_TOPLEVEL *w_current = g_current_window (); \
  char *status_msg_str;                             \
  status_msg_str = g_strconcat ( #name, " ", #number, NULL); \
  i_command_process(w_current, ACTION(EDIT_BUF_##name), number, status_msg_str, ID_ORIGIN_KEYBOARD); \
  g_free (status_msg_str);                          \
  return SCM_BOOL_T;                                \
}
#define DEFINE_BUFFER_MENU_FUNC(name, number)       \
SCM buffer_ ## name ## number ##_menu(SCM rest)     \
{                                                   \
  GSCHEM_TOPLEVEL *w_current = g_current_window (); \
  char *status_msg_str;                             \
  status_msg_str = g_strconcat ( #name, " ", #number, NULL); \
  i_command_process(w_current, ACTION(EDIT_BUF_##name), number, status_msg_str, ID_ORIGIN_MENU); \
  g_free (status_msg_str);                          \
  return SCM_BOOL_T;                                \
}
#define DEFINE_BUFFER_FUNCS(name, number)           \
        DEFINE_BUFFER_KEY_FUNC(name, number)        \
        DEFINE_BUFFER_MENU_FUNC(name, number)

/* Hoykeys */
DEFINE_BUFFER_FUNCS( copy, 1)
DEFINE_BUFFER_FUNCS( copy, 2)
DEFINE_BUFFER_FUNCS( copy, 3)
DEFINE_BUFFER_FUNCS( copy, 4)
DEFINE_BUFFER_FUNCS( copy, 5)

DEFINE_BUFFER_FUNCS( cut, 1)
DEFINE_BUFFER_FUNCS( cut, 2)
DEFINE_BUFFER_FUNCS( cut, 3)
DEFINE_BUFFER_FUNCS( cut, 4)
DEFINE_BUFFER_FUNCS( cut, 5)

DEFINE_BUFFER_FUNCS( paste, 1)
DEFINE_BUFFER_FUNCS( paste, 2)
DEFINE_BUFFER_FUNCS( paste, 3)
DEFINE_BUFFER_FUNCS( paste, 4)
DEFINE_BUFFER_FUNCS( paste, 5)

DEFINE_H_KEYS(view_pan_hotkey)
DEFINE_H_KEYS(view_pan_left)
DEFINE_H_KEYS(view_pan_right)
DEFINE_H_KEYS(view_pan_up)
DEFINE_H_KEYS(view_pan_down)

DEFINE_H_KEYS(misc)
DEFINE_H_KEYS(misc2)
DEFINE_H_KEYS(misc3)

/* be sure that you don't use the widget parameter in this one, since it is
 * being called with a null, I suppose we should call it with the right param.
 * hack */
DEFINE_H_KEYS(cancel)

/*! Contains the smob tag for key smobs */
static scm_t_bits g_key_smob_tag;
#define G_SCM_IS_KEY(x) SCM_SMOB_PREDICATE (g_key_smob_tag, (x))

/*! Type for keybindings. Used internally by gschem key smobs. */
typedef struct {
  unsigned int keyval;
  GdkModifierType modifiers;
  char *str;                 /* UTF-8. Free with g_free(). */
  char *disp_str;            /* UTF-8. Free with g_free(). */
} GschemKey;

/*! \brief Test if a key is valid.
 * \par Function Description
 * Test if the key combination defined by \a keyval and \a modifiers
 * is valid for key binding.  This is a less restrictive version of
 * gtk_accelerator_valid() from GTK 2.
 *
 * \param keyval     The key that was pressed.
 * \param modifiers  The active modifiers when the key was pressed.
 *
 * \return TRUE if the key combination is valid for keybinding.
 */
static bool
g_key_is_valid ( unsigned int keyval, GdkModifierType modifiers)
{
  static const unsigned int invalid_keyvals[] = {
    GDK_Shift_L, GDK_Shift_R, GDK_Shift_Lock, GDK_Caps_Lock, GDK_ISO_Lock,
    GDK_Control_L, GDK_Control_R, GDK_Meta_L, GDK_Meta_R,
    GDK_Alt_L, GDK_Alt_R, GDK_Super_L, GDK_Super_R, GDK_Hyper_L, GDK_Hyper_R,
    GDK_ISO_Level3_Shift, GDK_ISO_Next_Group, GDK_ISO_Prev_Group,
    GDK_ISO_First_Group, GDK_ISO_Last_Group,
    GDK_Mode_switch, GDK_Num_Lock, GDK_Multi_key,
    GDK_Scroll_Lock, GDK_Sys_Req,
    GDK_Tab, GDK_ISO_Left_Tab, GDK_KP_Tab,
    GDK_First_Virtual_Screen, GDK_Prev_Virtual_Screen,
    GDK_Next_Virtual_Screen, GDK_Last_Virtual_Screen,
    GDK_Terminate_Server, GDK_AudibleBell_Enable,
    0
  };
  const unsigned int *val;

  /* Exclude a bunch of control chars */
  if (keyval <= 0xFF) return keyval >= 0x20;

  /* Exclude special & modifier keys */
  val = invalid_keyvals;
  while (*val) {
    if (keyval == *val++) return FALSE;
  }

  return TRUE;
}

/*! \brief Create a new bindable key object.
 * \par Function Description
 * Create and return a new gschem key object from a \a keyval and a
 * set of \a modifiers.  If the key combination is invalid, return
 * SCM_BOOL_F.
 *
 * \param keyval     the pressed key.
 * \param modifiers  the active modifiers for the key.
 *
 * \return a new bindable key object, or SCM_BOOL_F.
 */
static SCM
g_make_key (unsigned int keyval, GdkModifierType modifiers)
{
  SCM result = SCM_BOOL_F;
  if (g_key_is_valid (keyval, modifiers)) {
    GschemKey *k = g_new0 (GschemKey, 1);
    k->keyval = keyval;
    k->modifiers = modifiers & GDK_MODIFIER_MASK;
    SCM_NEWSMOB (result, g_key_smob_tag, k);
  }
  return result;
}

/*! \brief Test if a Scheme value is a bindable key object.
 * \par Function Description
 * Returns SCM_BOOL_T if \a key_s is a gschem key object.  Otherwise,
 * returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %key? procedure in the
 * (gschem core keymap) module.
 *
 * \param key_s          value to test
 * \return SCM_BOOL_T iff value is a key, otherwise SCM_BOOL_F.
 */
SCM_DEFINE (g_keyp, "%key?", 1, 0, 0, (SCM key_s),
            "Test if value is a gschem key.")
{
  if (G_SCM_IS_KEY (key_s)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}

/*! \brief Create a bindable key object from a string.
 * \par Function Description
 * Parse the string key description \a str_s to create and return a
 * new gschem key object.  If \a str_s contains syntax errors, or does
 * not represent a valid bindable key combination, returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %string-key procedure in the
 * (gschem core keymap) module.
 *
 * \param str_s  string to parse.
 * \return a new gschem key object, or SCM_BOOL_F.
 */
SCM_DEFINE (g_string_to_key, "%string->key", 1, 0, 0, (SCM str_s),
            "Create a gschem key by parsing a string.")
{
  SCM_ASSERT (scm_is_string (str_s), str_s, SCM_ARG1, s_g_string_to_key);

  guint keyval;
  GdkModifierType modifiers;
  char *str = scm_to_utf8_string (str_s);
  gtk_accelerator_parse (str, &keyval, &modifiers);
  if ((keyval == 0) && (modifiers == 0)) return SCM_BOOL_F;
  return g_make_key (keyval, modifiers);
}

/*! \brief Convert a bindable key object to a string.
 * \par Function Description
 * Returns a string representation of the gschem key object \a key_s,
 * in a format suitable for parsing with %string->key.
 *
 * \note Scheme API: Implements the %key->string procedure in the
 * (gschem core keymap) module.
 *
 * \param key_s  Bindable key object to convert to string.
 * \return a string representation of the key combination.
 */
SCM_DEFINE (g_key_to_string, "%key->string", 1, 0, 0, (SCM key_s),
            "Create a string from a gschem key.")
{
  SCM_ASSERT (G_SCM_IS_KEY (key_s), key_s, SCM_ARG1, s_g_key_to_string);

  GschemKey *key = (GschemKey *) SCM_SMOB_DATA (key_s);
  if (key->str != NULL) return scm_from_utf8_string (key->str);

  key->str = gtk_accelerator_name (key->keyval, key->modifiers);
  return scm_from_utf8_string (key->str);
}

/*! \brief Convert a bindable key object to a displayable string.
 * \par Function Description
 * Returns a string representation of the gschem key object \a key_s,
 * in a format suitable for display to the user (e.g. as accelerator
 * text in a menu).
 *
 * \note Scheme API: Implements the %key->display-string procedure in
 * the (gschem core keymap) module.
 *
 * \param key_s  Bindable key object to convert to string.
 * \return a string representation of the key combination.
 */
SCM_DEFINE (g_key_to_display_string, "%key->display-string", 1, 0, 0,
            (SCM key_s), "Create a display string from a gschem key.")
{
  SCM_ASSERT (G_SCM_IS_KEY (key_s), key_s, SCM_ARG1,
              s_g_key_to_display_string);

  GschemKey *key = (GschemKey *) SCM_SMOB_DATA (key_s);
  if (key->disp_str != NULL) return scm_from_utf8_string (key->disp_str);

  key->disp_str = gtk_accelerator_get_label (key->keyval, key->modifiers);
  return scm_from_utf8_string (key->disp_str);
}

/*! \brief Print a representation of a key smob
 * \par Function Description
 * Outputs a string representing the \a smob to a Scheme output \a
 * port.  The format used is "#<gschem-key \"Ctrl+A\">".
 *
 * Used internally to Guile.
 */
static int
g_key_print (SCM smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<gschem-key ", port);
  scm_write (g_key_to_display_string (smob), port);
  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

/* \brief Test if two key combinations are equivalent.
 * \par Function Description
 * Tests if the two gschem key objects \a a and \a b represent the
 * same key event.
 *
 * Used internally to Guile.
 */
static SCM
g_key_equalp (SCM a, SCM b)
{
  GschemKey *akey = (GschemKey *) SCM_SMOB_DATA (a);
  GschemKey *bkey = (GschemKey *) SCM_SMOB_DATA (b);
  if (akey->keyval != bkey->keyval) return SCM_BOOL_F;
  if (akey->modifiers != bkey->modifiers) return SCM_BOOL_F;
  return SCM_BOOL_T;
}

/* \brief Destroy a bindable key object
 * \par Function Description
 * Destroys the contents of a gschem key object on garbage collection.
 *
 * Used internally to Guile.
 */
static size_t
g_key_free (SCM key) {
  GschemKey *k = (GschemKey *) SCM_SMOB_DATA (key);
  g_free (k->str);
  g_free (k->disp_str);
  g_free (k);
  return 0;
}

SCM_SYMBOL (reset_keys_sym, "reset-keys");
SCM_SYMBOL (press_key_sym,  "press-key");
SCM_SYMBOL (prefix_sym,     "prefix");

/*! \brief Clear the current key accelerator string.
 * \par Function Description
 * This function clears the current keyboard accelerator string in
 * the status bar of the relevant toplevel.  Called some time after a
 * keystroke is pressed.  If the current key sequence was a prefix,
 * let it persist.
 *
 * \param [in] data a pointer to the GSCHEM_TOPLEVEL to update.
 * \return FALSE (this is a one-shot timer).
 */
static bool clear_keyaccel_string(gpointer data)
{
  GSCHEM_TOPLEVEL *w_current = data;

  /* If the window context has disappeared, do nothing. */
  if (g_list_find(global_window_list, w_current) == NULL) {
    return FALSE;
  }

  g_free(w_current->keyaccel_string);
  w_current->keyaccel_string = NULL;
  w_current->keyaccel_string_source_id = 0;
  i_show_state(w_current, NULL);
  return FALSE;
}

/*! \brief Reset the current key sequence.
 * \par Function Description
 * If any prefix keys are stored in the current key sequence, clears
 * them.
 *
 * \param w_current  The active #GSCHEM_TOPLEVEL context.
 */
void
g_keys_reset (GSCHEM_TOPLEVEL *w_current)
{
  SCM s_expr = scm_list_1 (reset_keys_sym);

  /* Reset the status bar */
  g_free (w_current->keyaccel_string);
  w_current->keyaccel_string = NULL;
  i_show_state(w_current, NULL);

  /* Reset the Scheme keybinding state */
  scm_dynwind_begin (0);
  g_dynwind_window (w_current);
  g_scm_eval_protected (s_expr, scm_interaction_environment ());
  scm_dynwind_end ();
}
/*! \brief Exports the keymap in scheme to a GLib GArray.
 *  \par Function Description
 *  This function converts the list of key sequence/action pairs
 *  returned by the scheme function \c dump-current-keymap into an
 *  array of C structures.
 *
 *  The returned value must be freed by caller.
 *
 *  \return A GArray with keymap data.
 */
GArray* g_keys_dump_keymap (void)
{
  SCM dump_proc = scm_c_lookup ("dump-current-keymap");
  SCM scm_ret;
  GArray *ret = NULL;
  struct keyseq_action_t {
    gchar *keyseq, *action;
  };

  dump_proc = scm_variable_ref (dump_proc);
  g_return_val_if_fail (SCM_NFALSEP (scm_procedure_p (dump_proc)), NULL);

  scm_ret = scm_call_0 (dump_proc);
  g_return_val_if_fail (SCM_CONSP (scm_ret), NULL);

  ret = g_array_sized_new (FALSE,
                           FALSE,
                           sizeof (struct keyseq_action_t),
                           (unsigned int)scm_ilength (scm_ret));
  for (; scm_ret != SCM_EOL; scm_ret = SCM_CDR (scm_ret)) {
    SCM scm_keymap_entry = SCM_CAR (scm_ret);
    struct keyseq_action_t keymap_entry;

    g_return_val_if_fail (SCM_CONSP (scm_keymap_entry) &&
    SCM_SYMBOLP (SCM_CAR (scm_keymap_entry)) &&
    scm_is_string (SCM_CDR (scm_keymap_entry)), ret);
    keymap_entry.action = g_strdup (SCM_SYMBOL_CHARS (SCM_CAR (scm_keymap_entry)));
    keymap_entry.keyseq = g_strdup (SCM_STRING_CHARS (SCM_CDR (scm_keymap_entry)));
    ret = g_array_append_val (ret, keymap_entry);
  }

  return ret;
}
/*
 * int s_g_add_c_string_keys(char* keys, char* func) {
 *
 *  if ("gschem-keymap"
 }
 */
/*! \brief Evaluate a user keystroke.
 * \par Function Description
 * Evaluates the key combination specified by \a event using the
 * current keymap.  Updates the gschem status bar with the current key
 * sequence.
 *
 * \param w_current  The active #GSCHEM_TOPLEVEL context.
 * \param event      A GdkEventKey structure.
 *
 * \return 1 if a binding was found for the keystroke, 0 otherwise.
 */
int g_keys_execute(GSCHEM_TOPLEVEL *w_current, GdkEventKey *event)
{
  SCM s_retval, s_key, s_expr;
  unsigned int key, mods, upper, lower, caps;
  GdkDisplay *display;
  GdkKeymap *keymap;
  GdkModifierType consumed_modifiers;

  g_return_val_if_fail (w_current != NULL, 0);
  g_return_val_if_fail (event != NULL, 0);

  display = gtk_widget_get_display (w_current->main_window);
  keymap = gdk_keymap_get_for_display (display);

  /* Figure out what modifiers went into determining the key symbol */
  gdk_keymap_translate_keyboard_state (keymap,
                                       event->hardware_keycode,
                                       event->state, event->group,
                                       NULL, NULL, NULL, &consumed_modifiers);

  key = event->keyval;
  gdk_keyval_convert_case (event->keyval, &lower, &upper);
  mods = (event->state & gtk_accelerator_get_default_mod_mask () & ~consumed_modifiers);

  /* Handle Caps Lock. The idea is to obtain the same keybindings
   * whether Caps Lock is enabled or not. */
  if (upper != lower) {
    caps = gdk_keymap_get_caps_lock_state (keymap);
    if ((caps && (key == lower)) || (!caps && (key == upper))) {
      mods |= GDK_SHIFT_MASK;
    }
  }

  /* Always process key as lower case */
  key = lower;

  /* Validate the key -- there are some keystrokes we mask out. */
  if (!g_key_is_valid (key, mods)) {
    return FALSE;
  }

  /* Create Scheme key value */
  s_key = g_make_key (key, mods);

  /* Update key hint string for status bar. */
  char *keystr = gtk_accelerator_get_label (key, mods);

  /* If no current hint string, or the hint string is going to be
   * cleared anyway, use key string directly */
  if ((w_current->keyaccel_string == NULL) ||
    w_current->keyaccel_string_source_id) {
    g_free (w_current->keyaccel_string);
  w_current->keyaccel_string = keystr;

    } else {
      char *p = w_current->keyaccel_string;
      w_current->keyaccel_string = g_strconcat (p, " ", keystr, NULL);
      g_free (p);
      g_free (keystr);
    }

    /* Update status bar */
    i_show_state(w_current, NULL);

    /* Build and evaluate Scheme expression. */
    scm_dynwind_begin (0);
    g_dynwind_window (w_current);
    s_expr = scm_list_2 (press_key_sym, s_key);
    s_retval = g_scm_eval_protected (s_expr, scm_interaction_environment ());
    scm_dynwind_end ();

    /* If the keystroke was not part of a prefix, start a timer to clear
     * the status bar display. */
    if (w_current->keyaccel_string_source_id) {
      /* Cancel any existing timers that haven't fired yet. */
      GSource *timer =
      g_main_context_find_source_by_id (NULL,
                                        w_current->keyaccel_string_source_id);
      g_source_destroy (timer);
      w_current->keyaccel_string_source_id = 0;
    }
    if (!scm_is_eq (s_retval, prefix_sym)) {
      w_current->keyaccel_string_source_id =
      g_timeout_add(400, clear_keyaccel_string, w_current);
    }

    return !scm_is_false (s_retval);
}
/* Search the global keymap for a particular symbol and return the
 * keys which execute this hotkey, as a string suitable for display to
 * the user. This is used by the gschem menu system.
 *
 * example: (find-key (quote file-new))
 *
 */
char *g_find_key (char *func_name) {
  SCM s_expr;
  SCM s_iter;
  SCM s_lst;
  char *keys=NULL;

  /* Call Scheme procedure to dump global keymap into list */
  s_expr = scm_list_1 (scm_from_utf8_symbol ("dump-global-keymap"));
  s_lst = g_scm_eval_protected (s_expr, scm_interaction_environment ());

  if (scm_is_true (scm_list_p (s_lst))) {

    for (s_iter = s_lst; !scm_is_null (s_iter); s_iter = scm_cdr (s_iter)) {
      SCM s_binding = scm_caar (s_iter);
      SCM s_keys = scm_cdar (s_iter);
      char *binding;

      binding = scm_to_utf8_string (s_binding);

      if ( strcmp( func_name, binding) == 0) {
        keys = scm_to_utf8_string (s_keys);
        break;
      }
    }
  }
  if ((keys != NULL) && ( !strcmp( keys, "(null)") == 0)) {
    return g_strdup_printf("%s", keys);
  }

  return g_strdup_printf("%s", keys);
}
/*! \brief Exports the keymap in Scheme to a GtkListStore
 *  \par Function Description
 *  This function converts the list of key sequence/action pairs
 *  returned by the Scheme function \c dump-global-keymap into a
 *  GtkListStore with two columns.  The first column contains the name
 *  of the action executed by the keybinding as a string, and the
 *  second contains the keybinding itself as a string suitable for
 *  display.
 *
 *  The returned value must be freed by caller.
 *
 *  \return A GtkListStore containing keymap data.
 */
GtkListStore *
g_keys_to_list_store (void)
{
  SCM s_expr;
  SCM s_lst;
  SCM s_iter;
  GtkListStore *list_store;

  /* Call Scheme procedure to dump global keymap into list */
  s_expr = scm_list_1 (scm_from_utf8_symbol ("dump-global-keymap"));
  s_lst = g_scm_eval_protected (s_expr, scm_interaction_environment ());

  g_return_val_if_fail (scm_is_true (scm_list_p (s_lst)), NULL);

  /* Convert to  */
  scm_dynwind_begin (0);
  list_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  scm_dynwind_unwind_handler (g_object_unref, list_store, 0);

  for (s_iter = s_lst; !scm_is_null (s_iter); s_iter = scm_cdr (s_iter)) {
    SCM s_binding = scm_caar (s_iter);
    SCM s_keys = scm_cdar (s_iter);
    char *binding, *keys;
    GtkTreeIter iter;

    scm_dynwind_begin (0);

    binding = scm_to_utf8_string (s_binding);
    scm_dynwind_free (binding);

    keys = scm_to_utf8_string (s_keys);
    scm_dynwind_free (keys);

    gtk_list_store_insert_with_values (list_store, &iter, -1,
                                       0, binding,
                                       1, keys,
                                       -1);

    scm_dynwind_end ();
  }

  scm_dynwind_end ();
  return list_store;
}

/*! \brief Create the (gschem core keymap) Scheme module
 * \par Function Description
 * Defines procedures in the (gschem core keymap) module.  The module
 * can be accessed using (use-modules (gschem core keymap)).
 */
static void
init_module_gschem_core_keymap ()
{
  /* Register the functions */
  #include "g_keys.x"

  /* Add them to the module's public definitions */
  scm_c_export (s_g_keyp, s_g_string_to_key, s_g_key_to_string,
                s_g_key_to_display_string, NULL);
}

/*! \brief Initialise the key combination procedures
 * \par Function Description
 * Registers some Scheme procedures for working with key combinations.
 * Should only be called by main_prog().
 */
void
g_init_keys ()
{
  /* Register key smob type */
  g_key_smob_tag = scm_make_smob_type ("gschem-key", 0);
  scm_set_smob_print  (g_key_smob_tag, g_key_print);
  scm_set_smob_equalp (g_key_smob_tag, g_key_equalp);
  scm_set_smob_free   (g_key_smob_tag, g_key_free);

  scm_c_define_module ("gschem core keymap",
                       init_module_gschem_core_keymap,
                       NULL);
}
