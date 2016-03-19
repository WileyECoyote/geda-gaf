
/* Many keyboard shortcuts for Mac are the same as for X
 * except they use Command key instead of Control (e.g. Cut,
 * Copy, Paste). This symbol is for those simple cases. */
#ifndef GDK_WINDOWING_QUARTZ
#define DEFAULT_ACCEL_MOD_MASK GDK_CONTROL_MASK
#define DEFAULT_ACCEL_MOD_MASK_VIRTUAL GDK_CONTROL_MASK
#else
#define DEFAULT_ACCEL_MOD_MASK GDK_MOD2_MASK
#define DEFAULT_ACCEL_MOD_MASK_VIRTUAL GDK_META_MASK
#endif

/* When any of these modifiers are active, a key
 * event cannot produce a symbol, so should be
 * skipped when handling text input
 */
#ifndef GDK_WINDOWING_QUARTZ
#define NO_TEXT_INPUT_MOD_MASK (GDK_MOD1_MASK | GDK_CONTROL_MASK)
#else
#define NO_TEXT_INPUT_MOD_MASK (GDK_MOD2_MASK | GDK_CONTROL_MASK)
#endif

#ifndef GDK_WINDOWING_QUARTZ
#define EXTEND_SELECTION_MOD_MASK GDK_SHIFT_MASK
#define MODIFY_SELECTION_MOD_MASK GDK_CONTROL_MASK
#else
#define EXTEND_SELECTION_MOD_MASK GDK_SHIFT_MASK
#define MODIFY_SELECTION_MOD_MASK GDK_MOD2_MASK
#endif

#ifndef GDK_WINDOWING_QUARTZ
#define TOGGLE_GROUP_MOD_MASK 0
#else
#define TOGGLE_GROUP_MOD_MASK GDK_MOD1_MASK
#endif
bool
geda_keyboard_translate_accel_state (GdkKeymap       *keymap,
                                     unsigned int     hardware_keycode,
                                     GdkModifierType  state,
                                     GdkModifierType  accel_mask,
                                     int              group,
                                     unsigned int    *keyval,
                                     int             *effective_group,
                                     int             *level,
                                     GdkModifierType *consumed_modifiers);