/* This file missing from backup, not sure what's missing */

#define GetComboBoxActive(widget) gtk_combo_box_get_active ((GtkComboBox*)widget)

#define SetWidgetTip(widget, tip) gtk_widget_set_tooltip_text (GTK_WIDGET(widget), tip);

#define EntrySelectAll(widget)  \
            if (GTK_IS_WIDGET(widget))  { \
              const char *text = GetEntryText(widget); \
              if (text != NULL) { \
                gtk_entry_select_region((GtkEntry *) widget, 0, strlen(text)); \
              } \
            }

#define GetEntryText(widget) gtk_entry_get_text ((GtkEntry *) widget)
#define SetEntryText(widget, text) \
            if (text == NULL) \
              gtk_entry_set_text((GtkEntry *) widget, ""); \
            else \
              gtk_entry_set_text((GtkEntry *) widget, text);

#define GetToggleState(widget) gtk_toggle_button_get_active ((GtkToggleButton*)widget)
