
#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>

#include <gdk/gdk.h>

#include "../../include/geda_keyboard.h"

bool
geda_keyboard_translate_accel_state (GdkKeymap       *keymap,
                                     unsigned int     hardware_keycode,
                                     GdkModifierType  state,
                                     GdkModifierType  accel_mask,
                                     int              group,
                                     unsigned int    *keyval,
                                     int             *effective_group,
                                     int             *level,
                                     GdkModifierType *consumed_modifiers)
{
  bool group_mask_disabled = FALSE;
  bool retval;

  /* if the group-toggling modifier is part of the accel mod mask, and
   * it is active, disable it for matching
   */

  if (accel_mask & state & TOGGLE_GROUP_MOD_MASK) {

    state &= ~TOGGLE_GROUP_MOD_MASK;
    group = 0;
    group_mask_disabled = TRUE;
  }

  retval = gdk_keymap_translate_keyboard_state (keymap,
                                                hardware_keycode, state, group,
                                                keyval,
                                                effective_group, level,
                                                consumed_modifiers);

  /* add back the group mask, we want to match against the modifier,
   * but not against the keyval from its group
   */
  if (group_mask_disabled) {

    if (effective_group) {
      *effective_group = 1;
    }

    if (consumed_modifiers) {
      *consumed_modifiers &= ~TOGGLE_GROUP_MOD_MASK;
    }
  }

  return retval;
}