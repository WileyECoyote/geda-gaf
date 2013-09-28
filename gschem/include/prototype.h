/* $Id$ */
#define GLT GList   /* Only for this file!    */
#define OBJ OBJECT  /* to improve readability */
/* gschem_toplevel.c */
GSCHEM_TOPLEVEL *gschem_toplevel_new();
void gschem_toplevel_free (GSCHEM_TOPLEVEL *w_current);

/* a_pan.c */
void a_pan_general(GSCHEM_TOPLEVEL *w_current, double world_cx, double world_cy,
                   double relativ_zoom_factor, int flags);
void a_pan(GSCHEM_TOPLEVEL *w_current, int x, int y);
void a_pan_mouse(GSCHEM_TOPLEVEL *w_current, int diff_x, int diff_y);

/* a_zoom.c */
void a_zoom(GSCHEM_TOPLEVEL *w_current, int dir, int selected_from, int pan_flags);
void a_zoom_extents(GSCHEM_TOPLEVEL *w_current, const GList *list, int pan_flags);
void a_zoom_box(GSCHEM_TOPLEVEL *w_current, int pan_flags);
void a_zoom_box_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void a_zoom_box_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void a_zoom_box_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void a_zoom_box_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void a_zoom_box_draw_rubber(GSCHEM_TOPLEVEL *w_current);
void correct_aspect(GSCHEM_TOPLEVEL *w_current);

/* g_action.c */
SCM  g_process_action(SCM action);
bool g_action_eval_by_name (GSCHEM_TOPLEVEL *w_current, const char *action_name);
bool g_action_get_position (bool snap, int *x, int *y);

/* g_attrib.c */
void g_init_attrib ();

/* g_funcs.c */
SCM g_funcs_print(SCM filename);
SCM g_funcs_pdf(SCM filename);
SCM g_funcs_postscript(SCM filename);
SCM g_funcs_image(SCM filename);
SCM g_funcs_exit(void);
SCM g_funcs_log(SCM msg);
SCM g_funcs_msg(SCM msg);
SCM g_funcs_confirm(SCM msg);
SCM g_funcs_filesel(SCM msg, SCM templ, SCM flags);
SCM g_funcs_use_rc_values(void);
SCM get_selected_component_attributes(GSCHEM_TOPLEVEL *w_current);

/* g_hook.c */
void g_init_hook ();
void g_run_hook_object (GSCHEM_TOPLEVEL *w_current, const char *name, OBJECT *obj);
void g_run_hook_object_list (GSCHEM_TOPLEVEL *w_current, const char *name, GList *obj_lst);
void g_run_hook_page (GSCHEM_TOPLEVEL *w_current, const char *name, PAGE *page);

/* g_keys.c */
void  g_keys_reset (GSCHEM_TOPLEVEL *w_current);
int   g_keys_execute(GSCHEM_TOPLEVEL *w_current, GdkEventKey *event);
char *g_find_key (char *func_name);
GtkListStore *g_keys_to_list_store (void);

/* Hoykeys */
SCM buffer_copy1(SCM action);
SCM buffer_copy2(SCM action);
SCM buffer_copy3(SCM action);
SCM buffer_copy4(SCM action);
SCM buffer_copy5(SCM action);

SCM buffer_cut1(SCM action);
SCM buffer_cut2(SCM action);
SCM buffer_cut3(SCM action);
SCM buffer_cut4(SCM action);
SCM buffer_cut5(SCM action);

SCM buffer_paste1(SCM action);
SCM buffer_paste2(SCM action);
SCM buffer_paste3(SCM action);
SCM buffer_paste4(SCM action);
SCM buffer_paste5(SCM action);

SCM buffer_copy1_menu(SCM rest);
SCM buffer_copy2_menu(SCM rest);
SCM buffer_copy3_menu(SCM rest);
SCM buffer_copy4_menu(SCM rest);
SCM buffer_copy5_menu(SCM rest);

SCM buffer_cut1_menu(SCM rest);
SCM buffer_cut2_menu(SCM rest);
SCM buffer_cut3_menu(SCM rest);
SCM buffer_cut4_menu(SCM rest);
SCM buffer_cut5_menu(SCM rest);

SCM buffer_paste1_menu(SCM rest);
SCM buffer_paste2_menu(SCM rest);
SCM buffer_paste3_menu(SCM rest);
SCM buffer_paste4_menu(SCM rest);
SCM buffer_paste5_menu(SCM rest);

SCM h_keys_view_zoom_in_hotkey(SCM rest);
SCM h_keys_view_zoom_out_hotkey(SCM rest);

SCM h_keys_view_pan_hotkey(SCM rest);
SCM h_keys_view_pan_left(SCM rest);
SCM h_keys_view_pan_right(SCM rest);
SCM h_keys_view_pan_up(SCM rest);
SCM h_keys_view_pan_down(SCM rest);

/* Menus and Keyboard */
SCM h_keys_cancel(SCM rest);
void g_init_keys ();

/* g_rc.c */
void g_rc_parse_gtkrc();
int check_and_convert_scm_integer( SCM val2chk, int min_value, int max_value,
                                   int default_value, char* keyword);
SCM g_rc_gschem_version(SCM version);
SCM g_rc_draw_grips(SCM mode);
SCM g_rc_grid_mode(SCM mode);
SCM g_rc_dots_grid_dot_size(SCM dotsize);
SCM g_rc_dots_grid_mode(SCM mode);
SCM g_rc_dots_grid_fixed_threshold(SCM spacing);
SCM g_rc_mesh_grid_threshold(SCM spacing);
SCM g_rc_net_endpoint_mode(SCM mode);
SCM g_rc_net_midpoint_mode(SCM mode);
SCM g_rc_net_direction_mode(SCM mode);
SCM g_rc_net_selection_mode(SCM mode);
SCM g_rc_action_feedback_mode(SCM mode);
SCM g_rc_zoom_with_pan(SCM mode);
SCM g_rc_text_feedback(SCM mode);
SCM g_rc_text_display_zoomfactor(SCM zoomfactor);
SCM g_rc_object_clipping(SCM mode);
SCM g_rc_embed_components(SCM mode);
SCM g_rc_component_dialog_attributes(SCM stringlist);
SCM g_rc_text_case(SCM mode);
SCM g_rc_text_size(SCM size);
SCM g_rc_snap_size(SCM size);
SCM g_rc_attribute_name(SCM path);
SCM g_rc_paper_size(SCM width, SCM height);
SCM g_rc_paper_sizes(SCM papername, SCM scm_width, SCM scm_height);
SCM g_rc_output_type(SCM mode);
SCM g_rc_output_orientation(SCM mode);
SCM g_rc_image_color(SCM mode);
SCM g_rc_invert_images(SCM mode);
SCM g_rc_image_size(SCM width, SCM height);
SCM g_rc_output_color(SCM mode);
SCM g_rc_output_capstyle(SCM mode);
SCM g_rc_logging(SCM mode);
SCM g_rc_log_destiny(SCM mode);
SCM g_rc_console_window(SCM mode);
SCM g_rc_console_window_type(SCM mode);
SCM g_rc_third_button(SCM mode);
SCM g_rc_map_keys(SCM keys, SCM action);
SCM g_rc_middle_button(SCM mode);
SCM g_rc_scroll_wheel(SCM mode);
SCM g_rc_pointer_hscroll(SCM mode);
SCM g_rc_net_consolidate(SCM mode);
SCM g_rc_enforce_hierarchy(SCM mode);
SCM g_rc_text_origin_marker(SCM mode);
SCM g_rc_text_marker_size(SCM mode);
SCM g_rc_fast_mousepan(SCM mode);
SCM g_rc_continue_component_place(SCM mode);
SCM g_rc_undo_levels(SCM levels);
SCM g_rc_undo_control(SCM mode);
SCM g_rc_undo_type(SCM mode);
SCM g_rc_undo_panzoom(SCM mode);
SCM g_rc_netconn_rubberband(SCM mode);
SCM g_rc_magnetic_net_mode(SCM mode);
SCM g_rc_sort_component_library(SCM mode);
SCM g_rc_add_attribute_offset(SCM offset);
SCM g_rc_add_menu(SCM menu_name, SCM menu_items);
SCM g_rc_auto_load_last(SCM mode);
SCM g_rc_attribute_placement_grid(SCM offset);
SCM g_rc_auto_save_interval(SCM seconds);
SCM g_rc_window_size(SCM width, SCM height);
SCM g_rc_warp_cursor(SCM mode);
SCM g_rc_setpagedevice_orientation(SCM mode);
SCM g_rc_setpagedevice_pagesize(SCM mode);
SCM g_rc_bus_ripper_size(SCM size);
SCM g_rc_bus_ripper_type(SCM mode);
SCM g_rc_bus_ripper_rotation(SCM mode);
SCM g_rc_bus_ripper_symname(SCM scmsymname);
SCM g_rc_force_boundingbox(SCM mode);
SCM g_rc_drag_can_move(SCM mode);
SCM g_rc_mousepan_gain(SCM mode);
SCM g_rc_keyboardpan_gain(SCM mode);
SCM g_rc_print_command(SCM mode);
SCM g_rc_select_slack_pixels(SCM pixels);
SCM g_rc_zoom_gain(SCM gain);
SCM g_rc_display_color_map (SCM scm_map);
SCM g_rc_display_outline_color_map (SCM scm_map);

/* System Options */
SCM g_rc_file_preview(SCM mode);
SCM g_rc_handleboxes(SCM mode);
SCM g_rc_raise_dialog_boxes_on_expose(SCM mode);
SCM g_rc_save_ui_settings(SCM mode);
SCM g_rc_toolbars(SCM mode);
SCM g_rc_toolbars_mode(SCM mode);

/* Scrollbar Options */
SCM g_rc_scrollbars(SCM mode);
SCM g_rc_scrollbar_update(SCM mode);
SCM g_rc_scrollbars_visible(SCM mode);
SCM g_rc_scrollpan_steps(SCM steps);

/* g_register.c */
void g_register_funcs(void);

/* g_select.c */
void g_init_select ();

/* g_util.c */
void g_init_util ();
int  g_list_find_string(GList* list, char *str);
void g_list_free_string(void *data);
GLT* g_list_clear(GList* list);
void g_copy_tree_iter(GtkTreeIter *iter1, GtkTreeIter *iter2);
bool g_tree_model_iter_previous (GtkTreeModel *tree_model, GtkTreeIter *iter);

/* g_window.c */
GSCHEM_TOPLEVEL *g_current_window ();
void g_dynwind_window (GSCHEM_TOPLEVEL *w_current);
void g_init_window ();

/* globals.c */
/* gschem.c */
typedef void (*geda_atexit_func)(gpointer data);
void geda_atexit(geda_atexit_func func, gpointer data);
void main_prog(void *closure, int argc, char *argv[]);
int  main(int argc, char *argv[]);
void shut_down_gui(void);

/* i_basic.c */
void i_show_state(GSCHEM_TOPLEVEL *w_current, const char *message);
void i_set_state(GSCHEM_TOPLEVEL *w_current, enum x_states newstate);
void i_set_state_msg(GSCHEM_TOPLEVEL *w_current, enum x_states newstate, const char *message);
void i_update_middle_button(GSCHEM_TOPLEVEL *w_current, const char *string);
void i_update_toolbar(GSCHEM_TOPLEVEL *w_current);
void i_update_sensitivities(GSCHEM_TOPLEVEL *w_current);
void i_set_filename(GSCHEM_TOPLEVEL *w_current, const gchar *string);
void i_update_grid_info(GSCHEM_TOPLEVEL *w_current);

/*! \note WEH: MACRO: I_CALLBACK_ARGUMENTS is left defined and is without a
 *             semi-colon. This macro is re-used in the associated source file */
#define I_CALLBACK_ARGUMENTS (GSCHEM_TOPLEVEL* w_current, unsigned int callback_action, GtkWidget *widget)

/* i_callbacks.c Hotkeys */
void i_callback_buffer_copy1               I_CALLBACK_ARGUMENTS;
void i_callback_buffer_copy2               I_CALLBACK_ARGUMENTS;
void i_callback_buffer_copy3               I_CALLBACK_ARGUMENTS;
void i_callback_buffer_copy4               I_CALLBACK_ARGUMENTS;
void i_callback_buffer_copy5               I_CALLBACK_ARGUMENTS;
void i_callback_buffer_cut1                I_CALLBACK_ARGUMENTS;
void i_callback_buffer_cut2                I_CALLBACK_ARGUMENTS;
void i_callback_buffer_cut3                I_CALLBACK_ARGUMENTS;
void i_callback_buffer_cut4                I_CALLBACK_ARGUMENTS;
void i_callback_buffer_cut5                I_CALLBACK_ARGUMENTS;
void i_callback_buffer_paste1              I_CALLBACK_ARGUMENTS;
void i_callback_buffer_paste2              I_CALLBACK_ARGUMENTS;
void i_callback_buffer_paste3              I_CALLBACK_ARGUMENTS;
void i_callback_buffer_paste4              I_CALLBACK_ARGUMENTS;
void i_callback_buffer_paste5              I_CALLBACK_ARGUMENTS;

void i_callback_clipboard_paste_hotkey     I_CALLBACK_ARGUMENTS;

void i_callback_view_pan_hotkey            I_CALLBACK_ARGUMENTS;
void i_callback_view_pan_left              I_CALLBACK_ARGUMENTS;
void i_callback_view_pan_right             I_CALLBACK_ARGUMENTS;
void i_callback_view_pan_up                I_CALLBACK_ARGUMENTS;
void i_callback_view_pan_down              I_CALLBACK_ARGUMENTS;

void i_callback_file_new                   I_CALLBACK_ARGUMENTS;
void i_callback_file_open                  I_CALLBACK_ARGUMENTS;
void i_callback_file_save                  I_CALLBACK_ARGUMENTS;
void i_callback_page_close                 I_CALLBACK_ARGUMENTS;
void i_callback_page_discard               I_CALLBACK_ARGUMENTS;

void i_callback_cancel                     I_CALLBACK_ARGUMENTS;

bool i_callback_close_wm(GtkWidget *widget, GdkEvent *event, GSCHEM_TOPLEVEL* w_current);

/* i_command.c */
void i_command_engage(GSCHEM_TOPLEVEL *w_current);
void i_command_disengage(bool immediate, bool wait_return);
void i_command_get_action_list(GList** list);
void i_command_get_command_list(GList** list);
bool i_command_is_valid(const char *command);
void i_command_process(GSCHEM_TOPLEVEL *w_current, const char* command, int narg, char *arg, ID_ACTION_ORIGIN who);

/* i_vars.c */
char *i_var_get_gschem_config_string(EdaConfig *cfg, char *str);
void  i_var_restore_gschem_boolean(EdaConfig *cfg, char *key, int *var, bool def_val);
void  i_var_restore_gschem_integer(EdaConfig *cfg, char *key, int *var, int def_val);
void  i_var_restore_gschem_color(EdaConfig *cfg, char *key, GdkColor *var, int index);

void i_vars_set(GSCHEM_TOPLEVEL *w_current);
void i_vars_freenames();
void i_vars_init (GSCHEM_TOPLEVEL *w_current);
void i_vars_atexit_save_user_config (gpointer user_data);

 /* m_basic.c */
int mil_x(GSCHEM_TOPLEVEL *w_current, int val);
int mil_y(GSCHEM_TOPLEVEL *w_current, int val);
int pix_x(GSCHEM_TOPLEVEL *w_current, int val);
int pix_y(GSCHEM_TOPLEVEL *w_current, int val);
void WORLDtoSCREEN(GSCHEM_TOPLEVEL *w_current, int x, int y, int *px, int *py);
void SCREENtoWORLD(GSCHEM_TOPLEVEL *w_current, int mx, int my, int *x, int *y);
int snap_grid(GSCHEM_TOPLEVEL *w_current, int input);
int SCREENabs(GSCHEM_TOPLEVEL *w_current, int val);
int WORLDabs(GSCHEM_TOPLEVEL *w_current, int val);
int WORLDclip_change(GSCHEM_TOPLEVEL *w_current, int *x1, int *y1, int *x2, int *y2);
int clip_nochange(GSCHEM_TOPLEVEL *w_current, int x1, int y1, int x2, int y2);
int visible(GSCHEM_TOPLEVEL *w_current, int wleft, int wtop, int wright, int wbottom);
double round_5_2_1(double unrounded);

/* o_arc.c */
void o_arc_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_arc_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_arc_end1(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_arc_end4(GSCHEM_TOPLEVEL *w_current, int radius, int start_angle, int end_angle);
void o_arc_motion(GSCHEM_TOPLEVEL *w_current, int x, int y, int whichone);
void o_arc_draw_rubber(GSCHEM_TOPLEVEL *w_current);

/* o_attrib.c */
void o_attrib_add_selected(GSCHEM_TOPLEVEL *w_current, SELECTION *selection, OBJECT *selected);
void o_attrib_deselect_invisible(GSCHEM_TOPLEVEL *w_current, SELECTION *selection, OBJECT *selected);
void o_attrib_select_invisible(GSCHEM_TOPLEVEL *w_current, SELECTION *selection, OBJECT *selected);
void o_attrib_toggle_visibility(GSCHEM_TOPLEVEL *w_current, OBJECT *object);
void o_attrib_toggle_show_name_value(GSCHEM_TOPLEVEL *w_current, OBJECT *object, int new_show_name_value);
OBJ *o_attrib_add_attrib(GSCHEM_TOPLEVEL *w_current, const char *text_string, int visibility, int show_name_value, OBJECT *object);

/* o_basic.c */
void o_redraw_rects(GSCHEM_TOPLEVEL *w_current, GdkRectangle *rectangles, int n_rectangles);
int  o_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
int  o_redraw_cleanstates(GSCHEM_TOPLEVEL *w_current);
void o_invalidate_rect(GSCHEM_TOPLEVEL *w_current, int x1, int y1, int x2, int y2);
void o_invalidate_all(GSCHEM_TOPLEVEL *w_current);
void o_invalidate(GSCHEM_TOPLEVEL *w_current, OBJECT *object);
void o_invalidate_glist(GSCHEM_TOPLEVEL *w_current, GList *list);
int  o_drawing_color (GSCHEM_TOPLEVEL *w_current, OBJECT *object);

/* o_box.c */
void o_box_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_box_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_box_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_box_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_box_draw_rubber(GSCHEM_TOPLEVEL *w_current);

/* o_buffer.c */
void o_buffer_copy(GSCHEM_TOPLEVEL *w_current, int buf_num);
void o_buffer_cut(GSCHEM_TOPLEVEL *w_current, int buf_num);
void o_buffer_paste_start(GSCHEM_TOPLEVEL *w_current, int x, int y, int buf_num);
void o_buffer_init(void);
void o_buffer_free(GSCHEM_TOPLEVEL *w_current);

/* o_bus.c */
void o_bus_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_bus_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
int  o_bus_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_bus_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_bus_draw_rubber (GSCHEM_TOPLEVEL *w_current);

/* o_circle.c */
void o_circle_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_circle_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_circle_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_circle_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_circle_draw_rubber (GSCHEM_TOPLEVEL *w_current);

/* o_complex.c */
void o_complex_prepare_place(GSCHEM_TOPLEVEL *w_current, const CLibSymbol *sym);
void o_complex_place_changed_run_hook(GSCHEM_TOPLEVEL *w_current);
void o_complex_translate_all(GSCHEM_TOPLEVEL *w_current, int offset);

/* o_copy.c */
void o_copy_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_copy_end(GSCHEM_TOPLEVEL *w_current);
void o_copy_multiple_end(GSCHEM_TOPLEVEL *w_current);

/* o_delete.c */
void o_delete(GSCHEM_TOPLEVEL *w_current, OBJECT *object);
void o_delete_selected(GSCHEM_TOPLEVEL *w_current);

/* o_find.c */
bool o_find_object(GSCHEM_TOPLEVEL *w_current, int x, int y,
bool deselect_afterwards);
OBJ *o_find_selected_object(GSCHEM_TOPLEVEL *w_current, int x, int y);

/* o_grips.c */
OBJ *o_grips_search_world(GSCHEM_TOPLEVEL *w_current, int x, int y, int *whichone);
OBJ *o_grips_search_arc_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int x, int y, int size, int *whichone);
OBJ *o_grips_search_box_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int x, int y, int size, int *whichone);
OBJ *o_grips_search_path_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int x, int y, int size, int *whichone);
OBJ *o_grips_search_picture_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int x, int y, int size, int *whichone);
OBJ *o_grips_search_circle_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int x, int y, int size, int *whichone);
OBJ *o_grips_search_line_world(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int x, int y, int size, int *whichone);
int  o_grips_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_grips_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_grips_end(GSCHEM_TOPLEVEL *w_current);
void o_grips_cancel(GSCHEM_TOPLEVEL *w_current);
int  o_grips_half_size(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current);
void o_grips_draw_rubber(GSCHEM_TOPLEVEL *w_current);

/* o_line.c */
void o_line_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_line_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_line_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_line_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_line_draw_rubber(GSCHEM_TOPLEVEL *w_current);
int  o_line_visible(GSCHEM_TOPLEVEL *w_current, LINE *line, int *x1, int *y1, int *x2, int *y2);

/* o_misc.c */
void o_edit(GSCHEM_TOPLEVEL *w_current, GList *list);
void o_lock(GSCHEM_TOPLEVEL *w_current);
void o_unlock(GSCHEM_TOPLEVEL *w_current);
void o_rotate_world_update(GSCHEM_TOPLEVEL *w_current, int centerx, int centery, int angle, GList *list);
void o_mirror_world_update(GSCHEM_TOPLEVEL *w_current, int centerx, int centery, GList *list);
void o_edit_show_hidden_lowlevel(GSCHEM_TOPLEVEL *w_current, const GList *o_list);
void o_edit_show_hidden(GSCHEM_TOPLEVEL *w_current, const GList *o_list);
int  o_edit_find_text(GSCHEM_TOPLEVEL *w_current, const GList *o_list, char *stext, int descend, int skip);
void o_edit_hide_specific_text(GSCHEM_TOPLEVEL *w_current, const GList *o_list, char *stext);
void o_edit_show_specific_text(GSCHEM_TOPLEVEL *w_current, const GList *o_list, char *stext);
OBJ *o_update_component(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current);
void o_autosave_backups(GSCHEM_TOPLEVEL *w_current);

/* o_move.c */
void o_move_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_move_end_lowlevel(GSCHEM_TOPLEVEL *w_current, OBJECT *object, int diff_x, int diff_y);
void o_move_end(GSCHEM_TOPLEVEL *w_current);
void o_move_cancel(GSCHEM_TOPLEVEL *w_current);
void o_move_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_move_invalidate_rubber(GSCHEM_TOPLEVEL *w_current, int drawing);
void o_move_draw_rubber (GSCHEM_TOPLEVEL *w_current, int drawing);
int  o_move_return_whichone(OBJECT *object, int x, int y);
void o_move_check_endpoint(GSCHEM_TOPLEVEL *w_current, OBJECT *object);
void o_move_prep_rubberband(GSCHEM_TOPLEVEL *w_current);
int  o_move_zero_length(OBJECT *object);
void o_move_end_rubberband(GSCHEM_TOPLEVEL *w_current, int world_diff_x, int world_diff_y, GList **objects);

/* o_net.c */
void o_net_reset(GSCHEM_TOPLEVEL *w_current);
void o_net_guess_direction(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_net_find_magnetic(GSCHEM_TOPLEVEL *w_current, int event_x, int event_y);
void o_net_finish_magnetic(GSCHEM_TOPLEVEL *w_current);
void o_net_start_magnetic(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_net_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
int  o_net_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_net_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_net_draw_rubber(GSCHEM_TOPLEVEL *w_current);
void o_net_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
int  o_net_add_busrippers(GSCHEM_TOPLEVEL *w_current, OBJECT *net_obj, GList *other_objects);

/* o_path.c */
void o_path_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_path_continue (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y);
void o_path_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y);
bool o_path_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_path_invalidate_rubber (GSCHEM_TOPLEVEL *w_current);
void o_path_draw_rubber (GSCHEM_TOPLEVEL *w_current);
void o_path_invalidate_rubber_grips (GSCHEM_TOPLEVEL *w_current);
void o_path_motion_grips (GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_path_draw_rubber_grips (GSCHEM_TOPLEVEL *w_current);

/* o_picture.c */
void o_picture_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_picture_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_picture_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_picture_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_picture_draw_rubber(GSCHEM_TOPLEVEL *w_current);
bool o_picture_exchange(GSCHEM_TOPLEVEL *w_current, const gchar *filename, GError **error);
void picture_change_filename_dialog (GSCHEM_TOPLEVEL *w_current);
void o_picture_set_pixbuf(GSCHEM_TOPLEVEL *w_current, GdkPixbuf *pixbuf, char *filename);

/* o_pin.c */
void o_pin_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_pin_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_pin_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_pin_draw_rubber(GSCHEM_TOPLEVEL *w_current);
void o_pin_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);

/* o_place.c */
void o_place_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_place_end(GSCHEM_TOPLEVEL *w_current, int x, int y, int continue_placing, GList **ret_new_objects, const char *hook_name);
void o_place_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_place_invalidate_rubber(GSCHEM_TOPLEVEL *w_current, int drawing);
void o_place_draw_rubber(GSCHEM_TOPLEVEL *w_current, int drawing);
void o_place_rotate(GSCHEM_TOPLEVEL *w_current);

/* o_select.c */
void o_select_run_hooks(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int flag);
void o_select_object(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int type, int count);
int  o_select_box_start(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_select_box_end(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_select_box_motion(GSCHEM_TOPLEVEL *w_current, int x, int y);
void o_select_box_invalidate_rubber(GSCHEM_TOPLEVEL *w_current);
void o_select_box_draw_rubber(GSCHEM_TOPLEVEL *w_current);
void o_select_box_search(GSCHEM_TOPLEVEL *w_current);
void o_select_connected_nets(GSCHEM_TOPLEVEL *w_current, OBJECT* o_current);
int  o_select_selected(GSCHEM_TOPLEVEL *w_current);
void o_select_unselect_all(GSCHEM_TOPLEVEL *w_current);
void o_select_visible_unlocked(GSCHEM_TOPLEVEL *w_current);
void o_select_move_to_place_list(GSCHEM_TOPLEVEL *w_current);
GLT* o_select_get_list_selected_objects (GSCHEM_TOPLEVEL *w_current, char otype);
OBJ* o_select_return_first_object(GSCHEM_TOPLEVEL *w_current);

/* o_slot.c */
void o_slot_start(GSCHEM_TOPLEVEL *w_current, OBJECT *object);
void o_slot_end(GSCHEM_TOPLEVEL *w_current, OBJECT *object, const char *string);

/* o_text.c */
int  o_text_get_rendered_bounds(void *user_data, OBJECT *object, int *min_x, int *min_y, int *max_x, int *max_y);
void o_text_prepare_place(GSCHEM_TOPLEVEL *w_current, char *text);
void o_text_edit(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current);
void o_text_edit_end(GSCHEM_TOPLEVEL *w_current, char *string, int text_align,int text_color, int text_size, int rotate);
void o_text_change(GSCHEM_TOPLEVEL *w_current, OBJECT *object, char *string, int visibility, int show);

/* o_undo.c */
void  o_undo_init(GSCHEM_TOPLEVEL *w_current);
void  o_undo_savestate(GSCHEM_TOPLEVEL *w_current, int flag);
char *o_undo_find_prev_filename(UNDO *start);
GLT  *o_undo_find_prev_object_head(UNDO *start);
void  o_undo_callback(GSCHEM_TOPLEVEL *w_current, int type);
void  o_undo_finalize(void);
void  o_undo_remove_last_undo(GSCHEM_TOPLEVEL *w_current);
/* parsecmd.c */
int parse_commandline(int argc, char *argv[]);

/* s_stretch.c */
GLT *s_stretch_add(GList *list, OBJECT *object, int whichone);
GLT *s_stretch_remove(GList *list, OBJECT *object);
void s_stretch_print_all(GList *list);
void s_stretch_destroy_all(GList *list);

/* x_autonumber.c */
void autonumber_text_dialog(GSCHEM_TOPLEVEL *w_current);

/* x_basic.c */
void x_repaint_background_region(GSCHEM_TOPLEVEL *w_current, int x, int y, int width, int height);
void x_hscrollbar_set_ranges(GSCHEM_TOPLEVEL *w_current);
void x_hscrollbar_update(GSCHEM_TOPLEVEL *w_current);
void x_vscrollbar_set_ranges(GSCHEM_TOPLEVEL *w_current);
void x_vscrollbar_update(GSCHEM_TOPLEVEL *w_current);
void x_scrollbars_update(GSCHEM_TOPLEVEL *w_current);
void x_basic_warp_cursor(GtkWidget *widget, int x, int y);
int  tree_view_row_get_visibility(GtkTreeView *tree_view, GtkTreeIter *iter, bool fully_visible);
int  tree_view_row_make_visible(GtkTreeView *tree_view, GtkTreeIter *iter, bool center);

/* x_clipboard.c */
void x_clipboard_init (GSCHEM_TOPLEVEL *w_current);
void x_clipboard_finish (GSCHEM_TOPLEVEL *w_current);
void x_clipboard_query_usable (GSCHEM_TOPLEVEL *w_current, void (*callback) (int, void *), void *userdata);
bool x_clipboard_set (GSCHEM_TOPLEVEL *w_current, const GList *object_list);
GLT *x_clipboard_get (GSCHEM_TOPLEVEL *w_current);

/* x_color.c */
void      x_color_init (void);
void      x_color_free (void);
void      x_color_allocate (void);
GdkColor *x_get_color(int color);
GdkColor *x_get_darkcolor(int color);
COLOR    *x_color_lookup(int color);
COLOR    *x_color_lookup_dark(int color);
char     *x_color_get_name(int index);
bool      x_color_display_enabled (int index);
int       x_load_color_scheme(char * scheme);

/* x_compselect.c */
void x_compselect_open (GSCHEM_TOPLEVEL *w_current);
void x_compselect_deselect (GSCHEM_TOPLEVEL *w_current);

/* x_console.c */
const char  *x_console_get_alphanumeric(void);
const char  *x_console_get_numeric(void);
int          x_console_get_number(void);
int          x_console_get_integer(void);
float        x_console_get_real(void);
const char  *x_console_get_string(void);
void         x_console_open (GSCHEM_TOPLEVEL *w_current);
void         x_console_close ();
void         x_console_init_commands(GSCHEM_TOPLEVEL *w_current, int mode);
void         q_log_message(const char *message);
void         v_log_message(const char *message);
void         x_log_message(const char *log_domain, GLogLevelFlags log_level,
                           const char *message);

/* x_dialog.c */
/* Dialog-Utility functions */
AtkObject* atk_widget_linked_label_new( GtkWidget *label, GtkWidget *linkto);
GtkWidget* create_pixmap  ( const char *filename);
void       destroy_window (GtkWidget *widget, GtkWidget **window);
int        text_view_calculate_real_tab_width (GtkTextView *textview, int tab_size);
void       select_all_text_in_textview        (GtkTextView *textview);
GtkWidget *create_color_menu(GSCHEM_TOPLEVEL * w_current, int color_index);

/* Standard-Dialogs */
void about_dialog     (GSCHEM_TOPLEVEL *w_current);
void snap_size_dialog (GSCHEM_TOPLEVEL *w_current);
void text_size_dialog (GSCHEM_TOPLEVEL *w_current);

/* Editing-Dialogs */
void x_dialog_edit_arc_angle(GSCHEM_TOPLEVEL *w_current, OBJECT *arc_object);
void x_dialog_edit_fill_type(GSCHEM_TOPLEVEL *w_current);
void x_dialog_edit_line_type(GSCHEM_TOPLEVEL *w_current);
void x_dialog_edit_slot (GSCHEM_TOPLEVEL *w_current, const char *string);
void x_dialog_find_text (GSCHEM_TOPLEVEL *w_current);
void x_dialog_hide_text (GSCHEM_TOPLEVEL *w_current);
void x_dialog_show_text (GSCHEM_TOPLEVEL *w_current);
void x_dialog_text_input (GSCHEM_TOPLEVEL *w_current);
void x_dialog_translate (GSCHEM_TOPLEVEL *w_current);

/* Systemic-Dialogs */
void x_dialog_hotkeys (GSCHEM_TOPLEVEL *w_current);
bool x_dialog_close_changed_page (GSCHEM_TOPLEVEL *w_current, PAGE *page);
bool x_dialog_close_window (GSCHEM_TOPLEVEL *w_current);
void x_dialog_coord_display_update(GSCHEM_TOPLEVEL *w_current, int x, int y);
void x_dialog_coord_dialog(GSCHEM_TOPLEVEL *w_current, int x, int y);
void x_dialog_raise_all(GSCHEM_TOPLEVEL *w_current);
void x_dialog_symbol_changed(GSCHEM_TOPLEVEL* w_current);
int  x_dialog_validate_attribute(GtkWindow* parent, char *attribute);

/* Gschem-Generic-Dialogs */
void  gschem_message_dialog(const char *, gEDA_MessageType context, char *title);
int   gschem_confirm_dialog(const char *, gEDA_MessageType context);
char *gschem_filesel_dialog(const char *, const char *, int);

/* x_edit_attrib.c */
int option_menu_get_history(GtkOptionMenu *option_menu);
/*   attrib_edit_dialog_ok(GtkWidget *w, GSCHEM_TOPLEVEL *w_current);*/
void attrib_edit_dialog(GSCHEM_TOPLEVEL *w_current, OBJECT *attr_obj, int flag);

/* x_edit_color.c */
void x_dialog_edit_color(GSCHEM_TOPLEVEL *w_current);

/* x_edit_pin.c */
void x_dialog_edit_pin_type(GSCHEM_TOPLEVEL *w_current);

/* x_edit_text.c */
void x_dialog_edit_text (GSCHEM_TOPLEVEL *w_current, OBJECT *object);

/* x_event.c */
int  x_event_expose(GtkWidget *widget, GdkEventExpose *event, GSCHEM_TOPLEVEL *w_current);
int  x_event_button_pressed(GtkWidget *widget, GdkEventButton *event, GSCHEM_TOPLEVEL *w_current);
int  x_event_button_released(GtkWidget *widget, GdkEventButton *event, GSCHEM_TOPLEVEL *w_current);
int  x_event_motion(GtkWidget *widget, GdkEventMotion *event, GSCHEM_TOPLEVEL *w_current);
bool x_event_configure (GtkWidget *widget, GdkEventConfigure *event, gpointer user_data);
void x_manual_resize(GSCHEM_TOPLEVEL *w_current);
void x_event_hschanged(GtkAdjustment *adj, GSCHEM_TOPLEVEL *w_current);
void x_event_vschanged(GtkAdjustment *adj, GSCHEM_TOPLEVEL *w_current);
int  x_event_enter(GtkWidget *widget, GdkEventCrossing *event, GSCHEM_TOPLEVEL *w_current);
bool x_event_key(GtkWidget *widget, GdkEventKey *event, GSCHEM_TOPLEVEL *w_current);
int  x_event_scroll(GtkWidget *widget, GdkEventScroll *event, GSCHEM_TOPLEVEL *w_current);
bool x_event_get_pointer_position (GSCHEM_TOPLEVEL *w_current, bool snapped, int *wx, int *wy);
void x_event_set_pointer_position (GSCHEM_TOPLEVEL *w_current, int wx, int wy);

/* x_fileselect.c */
void x_fileselect_open(GSCHEM_TOPLEVEL *w_current);
void x_fileselect_save(GSCHEM_TOPLEVEL *w_current);
int  x_fileselect_load_backup(GString *message);

/* x_grid.c */
void x_grid_draw_region(GSCHEM_TOPLEVEL *w_current, int x, int y, int width, int height);
int  x_grid_query_drawn_spacing(GSCHEM_TOPLEVEL *w_current);
void x_draw_tiles(GSCHEM_TOPLEVEL *w_current);

/* x_icons.c */
void x_icons_add_search_path (const char *path);
void x_icons_set_default_icon (const char* icon_name);
void x_icons_initialize( void );

/* x_image.c */
void x_image_lowlevel(GSCHEM_TOPLEVEL *w_current, const char* filename,
                      int desired_width, int desired_height,
                      char *filetype, ImageExtent);
void x_image_setup(GSCHEM_TOPLEVEL *w_current, IMAGE_TYPES default_type);
GdkPixbuf *x_image_get_pixbuf (GSCHEM_TOPLEVEL *w_current, ImageExtent extent);

/* x_misc.c */
bool x_show_uri (const char *str);

/* x_menus.c */
void x_menu_free_all(void);

GtkWidget *get_main_menu(GSCHEM_TOPLEVEL *w_current);
GtkWidget *x_menu_setup_ui(GSCHEM_TOPLEVEL *w_current);

int  x_menu_setup_popup(GSCHEM_TOPLEVEL *w_current);
int  x_menu_display_popup(GSCHEM_TOPLEVEL *w_current, GdkEventButton *event);
void x_menus_sensitivity(GSCHEM_TOPLEVEL *w_current, const char *buf, int flag);
void x_menus_popup_sensitivity(GSCHEM_TOPLEVEL *w_current, const char *buf, int flag);
void x_menu_save_state(GSCHEM_TOPLEVEL *w_current);
void x_menu_set_toggle(GSCHEM_TOPLEVEL *w_current, int toggle_id, bool state);
void x_menu_set_toolbar_toggle(GSCHEM_TOPLEVEL *w_current, int toggle_id, bool state);
void x_menu_attach_recent_files_submenu(GSCHEM_TOPLEVEL *w_current);
void recent_files_load();
void recent_files_save(gpointer user_data);
void recent_files_add(const char *filename);
const char *recent_files_last( void);

/* x_multiattrib.c */
void x_multiattrib_open (GSCHEM_TOPLEVEL *w_current);
void x_multiattrib_close (GSCHEM_TOPLEVEL *w_current);
void x_multiattrib_update (GSCHEM_TOPLEVEL *w_current);

/* x_multimulti.c */

/* x_pagesel.c */
void x_pagesel_open (GSCHEM_TOPLEVEL *w_current);
void x_pagesel_close (GSCHEM_TOPLEVEL *w_current);
void x_pagesel_update (GSCHEM_TOPLEVEL *w_current);

/* x_preview.c */

/* x_print.c */
void x_print_setup(GSCHEM_TOPLEVEL *w_current, char *filename);
bool x_print_export_pdf_page (GSCHEM_TOPLEVEL *w_current, const char *filename);
bool x_print_export_pdf (GSCHEM_TOPLEVEL *w_current, const char *filename);
void x_print (GSCHEM_TOPLEVEL *w_current);

/* x_rc.c */
void x_rc_parse_gschem (GSCHEM_TOPLEVEL *w_current, const char *rcfile);

/* x_settings.c */
void x_configure_settings (GSCHEM_TOPLEVEL *w_current);
void x_settings_save_settings(GSCHEM_TOPLEVEL *w_current);
int  x_settings_lookup_cursor(int offset);
bool x_settings_set_scm_int(char *symbol_name, int value );

/* x_stroke.c */
void x_stroke_init (void);
void x_stroke_free (void);
void x_stroke_record (GSCHEM_TOPLEVEL *w_current, int x, int y);
int  x_stroke_translate_and_execute (GSCHEM_TOPLEVEL *w_current);

/* x_toolbars.c */
void x_toolbars_save_state(GSCHEM_TOPLEVEL *w_current);
void x_toolbars_restore_state(GSCHEM_TOPLEVEL *w_current);
void x_toolbars_finialize(GSCHEM_TOPLEVEL *w_current);
void x_toolbars_free_window(GSCHEM_TOPLEVEL *w_current);
void x_toolbars_init_window(GSCHEM_TOPLEVEL *w_current);
void x_toolbars_init_top(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container);
void x_toolbars_init_left(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container);
void x_toolbars_init_bottom(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container);
void x_toolbars_set_sensitivities(GSCHEM_TOPLEVEL *w_current, ID_SENITIVITY_MODE mode, bool state);
void x_toolbar_icons_only(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_toolbar_text_only(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_toolbar_display_both(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_toolbar_display_horiz(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_toolbars_turn_off_all_radios ( GSCHEM_TOPLEVEL *w_current );
void x_toolbars_activate_select ( GSCHEM_TOPLEVEL *w_current );
void x_toolbars_update(GSCHEM_TOPLEVEL *w_current);

/* x_window.c */
void x_window_setup (GSCHEM_TOPLEVEL *w_current);
void x_window_setup_gc(GSCHEM_TOPLEVEL *w_current);
void x_window_free_gc(GSCHEM_TOPLEVEL *w_current);
/*   x_window_create_drawing(GtkWidget *drawbox, GSCHEM_TOPLEVEL *w_current);*/
void x_window_restore_settings(GSCHEM_TOPLEVEL *w_current);
void x_window_save_settings(GSCHEM_TOPLEVEL *w_current);
void x_window_setup_draw_events(GSCHEM_TOPLEVEL *w_current);
void x_window_create_main(GSCHEM_TOPLEVEL *w_current);
void x_window_close(GSCHEM_TOPLEVEL *w_current);
void x_window_close_all(GSCHEM_TOPLEVEL *w_current);
PAGE *x_window_open_page (GSCHEM_TOPLEVEL *w_current, const char *filename);
void x_window_set_current_page (GSCHEM_TOPLEVEL *w_current, PAGE *page);
int  x_window_save_page (GSCHEM_TOPLEVEL *w_current, PAGE *page, const char *filename);
void x_window_close_page (GSCHEM_TOPLEVEL *w_current, PAGE *page);
void x_window_set_cursor(GSCHEM_TOPLEVEL *w_current, int cursor_id);
void x_window_add_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_attribute_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_gridsnap_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_edit_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_page_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_standard_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_select_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
void x_window_zoom_toolbar_toggle(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current);
#undef GLT
#undef OBJ