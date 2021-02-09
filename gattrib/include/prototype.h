/*! \file
 * This file holds all function prototypes for the entire gattrib
 * project. The file should be included after struct.h.
 */

/* ---------------- gattrib.c ---------------- */
typedef void (*geda_atexit_func)(void* data);
void geda_atexit(geda_atexit_func func, void* data);

bool gattrib_really_quit(void);
void gattrib_quit(int return_code);

/* -------------- parsecmd.c ----------------- */
void usage(char *cmd);
     /* output usage string */
int parse_commandline(int argc, char *argv[]);
     /* run through cmd line options and set mode switches. */

/* -------------- listsort.c ----------------- */
int cmp(STRING_LIST *a, STRING_LIST *b);
STRING_LIST *listsort(STRING_LIST *list, int is_circular, int is_double);

/* ------------- f_export.c ------------- */
void f_export_components(const char *filename);

/* ------------ gattrib_dialog.c ---------- */
GtkWidget* create_pixmap (const char *filename);

/* ------------- g_register.c ------------- */
void g_register_funcs(void);
SCM g_quit(void);

/* ------------- g_rc.c ------------- */
SCM  g_rc_gattrib_version(SCM version);
SCM  g_rc_hide_columns(SCM stringlist);
SCM  g_rc_sort_components(SCM mode);
SCM  g_rc_tearoff_menus(SCM mode);

/* -------------- i_basic.c -------------- */
void i_show_wiki_help(const char *html_file);

/* ------------- s_attrib.c ------------- */
int   s_attrib_name_in_list(STRING_LIST *name_value_list, char *name);
char *s_attrib_get_refdes(GedaObject *object);

/* ------------- s_object.c ------------- */
void s_object_add_attrib_to_object (GedaToplevel *toplevel,
                                    GedaObject *o_current,
                                    char *new_attrib_name,
                                    char *new_attrib_value,
                                    int visibility,
                                    int show_name_value);
void s_object_add_pin_attrib_to_object (GedaToplevel *toplevel,
                                        GedaObject *o_current,
                                        char *new_attrib_name,
                                        char *new_attrib_value);

void s_object_replace_attrib_in_object (GedaToplevel *toplevel,
                                        GedaObject *o_current,
                                        char *new_attrib_name,
                                        char *new_attrib_value,
                                        int visibility,
                                        int show_name_value);
void s_object_release_attrib_in_object (GedaToplevel *toplevel,
                                        GedaObject *o_current,
                                        char *new_attrib_name);

void s_object_attrib_add_attrib_in_object (GedaToplevel *toplevel,
                                           char         *text_string,
                                           int           visibility,
                                           int           show_name_value,
                                           int           color,
                                           GedaObject   *object);

void s_object_delete_text_object(GedaToplevel *toplevel, GedaObject *test_object);

/* ------------- s_misc.c ------------- */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);
char *s_misc_remaining_string(char *string, char delimiter, int count);

/* ------------- s_properties.c ------------- */
int  s_properties_get_fgcolor_index(int visibility, int show_name_value, int is_inherited);
bool s_properties_get_visibility(int row, int col);
int  s_properties_get_show_name_value(int row, int col);
int  s_properties_get_heritence(int row, int col);
void s_properties_set_invisible();
void s_properties_set_visible();
void s_properties_set_name_only();
void s_properties_set_value_only();
void s_properties_set_name_and_value();
void s_properties_promote_attribute();
void s_properties_demote_attribute();

/* ------------- s_rename.c ------------- */
void s_rename_init(void);
void s_rename_destroy_all(void);
void s_rename_next_set(void);
void s_rename_print(void);
int  s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
void s_rename_all(GedaToplevel *toplevel, NETLIST *netlist_head);

/* ------------- s_sheet_data.c ------------- */
PageDataSet *s_sheet_data_new();
void s_sheet_data_free(PageDataSet *PageData);

/*
void s_sheet_data_add_comp(PageDataSet *PageData, char *component_str_name);
void s_sheet_data_add_comp_attrib(PageDataSet *PageData, char *comp_attrib_str_name);
void s_sheet_data_add_net(PageDataSet *PageData, char *net_str_name);
void s_sheet_data_add_net_attrib(PageDataSet *PageData, char *net_attrib_str_name);
void s_sheet_data_add_pin(PageDataSet *PageData, char *pin_str_name);
void s_sheet_data_add_pin_attrib(PageDataSet *PageData, char *pin_attrib_str_name);
*/
void s_sheet_data_load_blank(PageDataSet *PageData);

void s_sheet_data_add_master_comp_list_items(const GList *obj_list);
void s_sheet_data_add_master_comp_attrib_list_items(const GList *obj_list);
void s_sheet_data_add_master_net_list_items(const GList *obj_list);
void s_sheet_data_add_master_net_attrib_list_items(const GList *obj_list);
void s_sheet_data_add_master_pin_list_items(const GList *obj_list);
void s_sheet_data_add_master_pin_attrib_list_items(const GList *obj_list);

/* ------------- s_string_list.c ------------- */
STRING_LIST *s_string_list_new();
STRING_LIST *s_string_list_duplicate_string_list(STRING_LIST *old_string_list);
void s_string_list_free(STRING_LIST *strlist);
void s_string_list_add_item(STRING_LIST *list, int *count, const char *item);
void s_string_list_delete_item(STRING_LIST **list, int *count, char *item);
void s_string_list_insert (STRING_LIST *list, int *old_count, int pos, char *item);
int  s_string_list_in_list(STRING_LIST *list, char *item);
char *s_string_list_get_data_at_index(STRING_LIST *list, int index);

/* \Comment WEH: Do these belong in s_string_list_sort */
void s_string_list_sort_master_comp_list();
void s_string_list_sort_master_comp_attrib_list();
void s_string_list_sort_master_net_list();
void s_string_list_sort_master_net_attrib_list();
void s_string_list_sort_master_pin_list();
void s_string_list_sort_master_pin_attrib_list();
void s_string_list_sort_all_list();

/* ------------- s_table.c ------------- */
TABLE **s_table_new(int rows, int cols);
TABLE **s_table_add_column(TABLE **table, int rows, int Xa, int Xt) GEDA_WARN_UNUSED_RESULT;
//TABLE **s_table_resize(TABLE **table, int rows, int old_cols, int new_cols);
void s_table_destroy(TABLE **table, int row_count, int col_count);
int  s_table_get_index(STRING_LIST *list, char *string);
STRING_LIST *s_table_create_attrib_pair(char *row_name,
                                        TABLE **table,
                                        STRING_LIST *row_list,
                                        int num_attribs);

void s_table_add_items_to_comp_table(const GList *obj_list);
void s_table_add_items_to_net_table(const GList *obj_list);
void s_table_add_items_to_pin_table(const GList *obj_list);
bool s_table_remove_attribute(TABLE **table, int X);

void s_table_gtksheet_to_all_tables();
void s_table_gtksheet_to_table(GtkSheet *local_gtk_sheet,
                               STRING_LIST *master_row_list, STRING_LIST *master_col_list,
                               TABLE **local_table, int num_rows, int num_cols);
void s_table_load_new_page(PageDataSet *PageData);

/* ------------- s_toplevel.c ------------- */
void s_toplevel_close(PageDataSet *PageData);
int  s_toplevel_read_page(GedaToplevel *toplevel, char *filename);
void s_toplevel_verify_design(GedaToplevel *toplevel);
void s_toplevel_gtksheet_to_toplevel(GedaToplevel *toplevel);
void s_toplevel_add_new_attrib(int column_location);
void s_toplevel_delete_attrib_col(GtkSheet *sheet);
void s_toplevel_sheetdata_to_toplevel(GedaToplevel *toplevel, Page *page);

STRING_LIST *s_toplevel_get_component_attribs_in_sheet(char *refdes);
void s_toplevel_update_component_attribs_in_toplevel(
					    GedaToplevel *toplevel,
					    GedaObject *o_current,
					    STRING_LIST *new_comp_attrib_list);
STRING_LIST *s_toplevel_get_net_attribs_in_sheet(char *netname);
void s_toplevel_update_net_attribs_in_toplevel(GedaObject *o_current,
					 STRING_LIST *new_net_attrib_list);
STRING_LIST *s_toplevel_get_pin_attribs_in_sheet(char *refdes, GedaObject *pin);
void s_toplevel_update_pin_attribs_in_toplevel(GedaToplevel *toplevel,
					 char *refdes, GedaObject *pin,
					 STRING_LIST *new_pin_attrib_list);
void s_toplevel_init_data_set(GedaToplevel *toplevel, PageDataSet *PageData);

/* ------------- i_vars.c ------------- */
void  i_vars_set(GedaToplevel *toplevel);
void  i_vars_release_all(void);
//void  i_window_vars_set(GedaToplevel *toplevel);

/* ------------- x_dialog.c ------------- */
void  generic_msg_dialog (const char *msg);
bool  x_dialog_generic_confirm_dialog (const char *msg, int type);
char *x_dialog_new_attrib();
bool  x_dialog_column_visibility (GList *list);
void  x_dialog_missing_sym();
int   x_dialog_file_not_saved();
void  x_dialog_unsaved_data();
void  x_dialog_unimplemented_feature();
void  x_dialog_fatal_error(char *string, int return_code);
void  x_dialog_about_dialog();
void  x_dialog_export_file();


char *x_dialog_get_search_text(const char *prompt);
//void  x_dialog_find_value(char *prompt, SearchRecord *Search);
void  x_dialog_search_replace(SearchRecord *Search, const char *find_text);

/* ------------- x_find.c ------------- */
void  x_find_restore_search_setting(void);
void  x_find_save_search_setting(void);
bool  x_find_main_search(char* text, char *replacement);
void  x_find_attribute_value(void);
void  x_find_replace_attrib_value(void);
void  x_find_attribute(void);
void  x_find_refdes(void);

/* ------------- x_gtksheet.c ------------- */
GtkSheet *x_gtksheet_get_current_sheet();
void  x_gtksheet_destroy_all(void);
void  x_gtksheet_init(PageDataSet *PageData);
void  x_gtksheet_reinititialize(PageDataSet *PageData);
void  x_gtksheet_add_row_labels(GtkSheet *sheet, int count, STRING_LIST *list_head);
void  x_gtksheet_add_col_labels(GtkSheet *sheet, int count, STRING_LIST *list_head);
void  x_gtksheet_set_cell_fgcolor(GtkSheet *sheet, int row, int col, ColorId Color);
void  x_gtksheet_set_cell_bgcolor(GtkSheet *sheet, int row, int col, ColorId Color);
void  x_gtksheet_add_cell_item(GtkSheet *sheet, int i, int j, char *text,
                              int visibility, int show_name_value, int is_inherited);
bool  x_gtksheet_get_is_empty(GtkSheet *sheet, int row, int col);
int   x_gtksheet_get_min_col(GtkSheet *sheet);
int   x_gtksheet_get_max_col(GtkSheet *sheet);
void  x_gtksheet_range_copy(GtkSheetRange *s_range, GtkSheetRange *t_range);
void  x_gtksheet_set_max_range(GtkSheet *sheet, GtkSheetRange *range);

/* ------------- x_fileselect.c ------------- */
bool  x_fileselect (char * filename);
bool  x_fileselect_load_file (char *filename);
bool  x_fileselect_load_files (GSList *filenames);

GSList *x_fileselect_open (void);
void  x_fileselect_save (void);

/* ------------- x_menus.c ------------- */
void  x_menu_file_open();
void  x_menu_file_save();
void  x_menu_file_save_as();
void  x_menu_file_export_csv();
void  x_menu_edit_new_attrib();
void  x_menu_edit_delete_attrib();
void  x_menus_set_sensitivities(GSList *ListMenuItems, int sensitive);
void  x_menu_fix_gtk_recent_submenu();
//GtkActionGroup* x_menu_create_recent_action_group(void);
GtkWidget* x_menu_create_menu(GtkWindow *window);
void  x_menu_release_all(void);

/* ------------- x_toolbars.c ------------- */
void  x_toolbars_init(GtkWidget *parent_container);
void  x_toolbar_set_sensitivities(GSList *ListToolBarItems, int sensitive);
void  x_toolbar_release_all(void);

/* ------------- x_window.c ------------- */
void  x_window_update_title(GedaToplevel *toplevel, PageDataSet *PageData);
void  x_window_clipboard_handler(int do_what);
void  x_window_init();
void  x_window_blank_document(GedaToplevel *toplevel, PageDataSet *PageData);
void  x_window_add_items(PageDataSet *PageData);
void  x_window_finalize_startup(GtkWindow *w_current, PageDataSet *PageData);

void  x_window_attribute_toolbar_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_standard_toolbar_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_editbar_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_attached_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_inherited_toggle(GtkToggleAction *action, GtkWindow *main_window);
void  x_window_autoresize_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_autoscroll_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_grid_toggle(GtkToggleAction *action, GtkWindow *window);
void  x_window_release_all(void);
