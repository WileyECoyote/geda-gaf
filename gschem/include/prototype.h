/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */

#define GLT GList   /* Only for this file,     */
#define GSL GSList  /* to improve readability! */
#define GHT GHashTable
#define GLS GtkListStore
#define OBJ GedaObject
#define EHP EdascmHookProxy
#define GAY GArray

BEGIN_DECLS

/* gschem_toplevel.c - refer to gschem_toplevel.h */

/* g_action.c */
SCM   g_process_action            (SCM action);
SCM   g_process_anonymous_action  (void);
bool  g_action_eval_by_name       (GschemToplevel *w_current, const char *action_name);
bool  g_action_get_position       (bool snap, int *x, int *y);

/* g_attrib.c */
void  g_init_attrib ();

/* g_builtins.c */
void  g_init_builtins (void);

/* g_funcs.c */
SCM   g_funcs_bmp_image      (SCM filename);
SCM   g_funcs_confirm        (SCM msg);
SCM   g_funcs_confirm_cancel (SCM msg);
SCM   g_funcs_exit           (SCM status);
SCM   g_funcs_filesel        (SCM msg, SCM templ, SCM flags);
SCM   g_funcs_jpeg_image     (SCM filename);
SCM   g_funcs_log            (SCM msg);
SCM   g_funcs_msg            (SCM msg);
SCM   g_funcs_output_type    (void);
SCM   g_funcs_pdf            (SCM filename);
SCM   g_funcs_png_image      (SCM filename);
SCM   g_funcs_postscript     (SCM filename);
SCM   g_funcs_print          (SCM filename);
SCM   g_funcs_save_file      (void);
SCM   g_funcs_tiff_image     (SCM filename);
SCM   g_funcs_use_rc_values  (void);

SCM   get_selected_component_attributes     (GschemToplevel *w_current);

/* g_hook.c */
void  g_hook_init                 (void);
void  g_hook_run_object           (GschemToplevel *w_current, Hooker id, GedaObject *obj);
void  g_hook_run_object_list      (GschemToplevel *w_current, Hooker id, GList *obj_lst);
void  g_hook_run_page             (GschemToplevel *w_current, Hooker id, Page *page);
EHP  *g_hook_new_proxy_by_name    (const char *name);

/* g_keys.c */
void  g_keys_reset                (GschemToplevel *w_current);
int   g_keys_execute              (GschemToplevel *w_current, GdkEventKey *event);
char *g_keys_find_key             (char *func_name);
GLS  *g_keys_to_new_list_store    (void);
GHT  *g_keys_to_new_hash_table    (void);

/* Hoykeys */
SCM   buffer_copy1           (SCM action);
SCM   buffer_copy2           (SCM action);
SCM   buffer_copy3           (SCM action);
SCM   buffer_copy4           (SCM action);
SCM   buffer_copy5           (SCM action);

SCM   buffer_cut1            (SCM action);
SCM   buffer_cut2            (SCM action);
SCM   buffer_cut3            (SCM action);
SCM   buffer_cut4            (SCM action);
SCM   buffer_cut5            (SCM action);

SCM   buffer_paste1          (SCM action);
SCM   buffer_paste2          (SCM action);
SCM   buffer_paste3          (SCM action);
SCM   buffer_paste4          (SCM action);
SCM   buffer_paste5          (SCM action);

SCM   buffer_copy1_menu      (SCM rest);
SCM   buffer_copy2_menu      (SCM rest);
SCM   buffer_copy3_menu      (SCM rest);
SCM   buffer_copy4_menu      (SCM rest);
SCM   buffer_copy5_menu      (SCM rest);

SCM   buffer_cut1_menu       (SCM rest);
SCM   buffer_cut2_menu       (SCM rest);
SCM   buffer_cut3_menu       (SCM rest);
SCM   buffer_cut4_menu       (SCM rest);
SCM   buffer_cut5_menu       (SCM rest);

SCM   buffer_paste1_menu     (SCM rest);
SCM   buffer_paste2_menu     (SCM rest);
SCM   buffer_paste3_menu     (SCM rest);
SCM   buffer_paste4_menu     (SCM rest);
SCM   buffer_paste5_menu     (SCM rest);

SCM   h_keys_view_zoom_in_hotkey  (SCM rest);
SCM   h_keys_view_zoom_out_hotkey (SCM rest);

SCM   h_keys_view_pan_hotkey (SCM rest);
SCM   h_keys_view_pan_left   (SCM rest);
SCM   h_keys_view_pan_right  (SCM rest);
SCM   h_keys_view_pan_up     (SCM rest);
SCM   h_keys_view_pan_down   (SCM rest);

/* Menus and Keyboard */
SCM   h_keys_cancel          (SCM rest);
void  g_keys_init            (void);

/* g_rc.c */
void  g_rc_parse_gtkrc();
int   check_and_convert_scm_integer    (SCM val2chk, int min_value, int max_value,
                                        int default_value, char* keyword);
SCM   g_rc_gschem_version              (SCM version);
SCM   g_rc_render_adaptor              (SCM mode);
SCM   g_rc_action_color                (SCM mode);
SCM   g_rc_anti_aliasing               (SCM mode);
SCM   g_rc_draw_grips                  (SCM mode);
SCM   g_rc_draw_complex_grips          (SCM mode);
SCM   g_rc_grips_pixel_size            (SCM size);
SCM   g_rc_grid_mode                   (SCM mode);
SCM   g_rc_dots_grid_minor_color       (SCM red, SCM green, SCM blue);
SCM   g_rc_dots_grid_major_color       (SCM red, SCM green, SCM blue);
SCM   g_rc_dots_grid_dot_size          (SCM dotsize);
SCM   g_rc_dots_grid_mode              (SCM mode);
SCM   g_rc_dots_grid_threshold         (SCM spacing);
SCM   g_rc_dots_grid_minor_alpha       (SCM percent);
SCM   g_rc_dots_grid_major_alpha       (SCM percent);
SCM   g_rc_mesh_grid_threshold         (SCM spacing);
SCM   g_rc_mesh_line_width_factor      (SCM width);
SCM   g_rc_mesh_grid_minor_alpha       (SCM percent);
SCM   g_rc_mesh_grid_major_alpha       (SCM percent);
SCM   g_rc_mesh_grid_minor_color       (SCM red, SCM green, SCM blue);
SCM   g_rc_mesh_grid_major_color       (SCM red, SCM green, SCM blue);
SCM   g_rc_net_endpoint_mode           (SCM mode);
SCM   g_rc_net_midpoint_mode           (SCM mode);
SCM   g_rc_net_direction_mode          (SCM mode);
SCM   g_rc_net_selection_mode          (SCM mode);
SCM   g_rc_action_feedback_mode        (SCM mode);
SCM   g_rc_zoom_with_pan               (SCM mode);
SCM   g_rc_text_feedback               (SCM mode);
SCM   g_rc_text_display_zoomfactor     (SCM zoomfactor);
SCM   g_rc_object_clipping             (SCM mode);
SCM   g_rc_embed_components            (SCM mode);
SCM   g_rc_component_dialog_attributes (SCM stringlist);
SCM   g_rc_text_case                   (SCM mode);
SCM   g_rc_text_size                   (SCM size);
SCM   g_rc_snap_size                   (SCM size);
SCM   g_rc_attribute_name              (SCM path);
SCM   g_rc_paper_size                  (SCM width, SCM height);
SCM   g_rc_paper_sizes                 (SCM papername, SCM scm_width, SCM scm_height);
SCM   g_rc_output_extents              (SCM mode);
SCM   g_rc_output_orientation          (SCM mode);
SCM   g_rc_image_color                 (SCM mode);
SCM   g_rc_invert_images               (SCM mode);
SCM   g_rc_image_size                  (SCM width, SCM height);
SCM   g_rc_output_color                (SCM mode);
SCM   g_rc_output_capstyle             (SCM mode);
SCM   g_rc_logging                     (SCM mode);
SCM   g_rc_log_destiny                 (SCM mode);
SCM   g_rc_console_window              (SCM mode);
SCM   g_rc_console_window_type         (SCM mode);
SCM   g_rc_third_button                (SCM mode);
SCM   g_rc_third_button_cancel         (SCM mode);
SCM   g_rc_map_icon                    (SCM icon, SCM action);
SCM   g_rc_map_keys                    (SCM keys, SCM action);
SCM   g_rc_middle_button               (SCM mode);
SCM   g_rc_scroll_wheel                (SCM mode);
SCM   g_rc_pointer_hscroll             (SCM mode);
SCM   g_rc_net_consolidate             (SCM mode);
SCM   g_rc_enforce_hierarchy           (SCM mode);
SCM   g_rc_text_origin_marker          (SCM mode);
SCM   g_rc_text_marker_size            (SCM mode);
SCM   g_rc_text_marker_threshold       (SCM mode);
SCM   g_rc_fast_mousepan               (SCM mode);
SCM   g_rc_continue_component_place    (SCM mode);
SCM   g_rc_undo_levels                 (SCM levels);
SCM   g_rc_undo_control                (SCM mode);
SCM   g_rc_undo_type                   (SCM mode);
SCM   g_rc_undo_panzoom                (SCM mode);
SCM   g_rc_undo_preserve               (SCM mode);
SCM   g_rc_netconn_rubberband          (SCM mode);
SCM   g_rc_magnetic_net_mode           (SCM mode);
SCM   g_rc_sort_component_library      (SCM mode);
SCM   g_rc_add_attribute_offset        (SCM offset);
SCM   g_rc_add_menu                    (SCM menu_name, SCM menu_items);
SCM   g_rc_attribute_placement_grid    (SCM offset);
SCM   g_rc_auto_load_last              (SCM mode);
SCM   g_rc_auto_pan                    (SCM mode);
SCM   g_rc_auto_pan_step               (SCM step);
SCM   g_rc_auto_save_interval          (SCM seconds);
SCM   g_rc_window_size                 (SCM width, SCM height);
SCM   g_rc_world_size                  (SCM width, SCM height, SCM border);
SCM   g_rc_warp_cursor                 (SCM mode);
SCM   g_rc_setpagedevice_orientation   (SCM mode);
SCM   g_rc_setpagedevice_pagesize      (SCM mode);
SCM   g_rc_bus_ripper_size             (SCM size);
SCM   g_rc_bus_ripper_type             (SCM mode);
SCM   g_rc_bus_ripper_rotation         (SCM mode);
SCM   g_rc_bus_ripper_symname          (SCM scmsymname);
SCM   g_rc_force_boundingbox           (SCM mode);
SCM   g_rc_drag_can_move               (SCM mode);
SCM   g_rc_mousepan_gain               (SCM mode);
SCM   g_rc_keyboardpan_gain            (SCM mode);
SCM   g_rc_print_command               (SCM mode);
SCM   g_rc_select_slack_pixels         (SCM pixels);
SCM   g_rc_zoom_gain                   (SCM gain);
SCM   g_rc_display_color_map           (SCM scm_map);
SCM   g_rc_display_outline_color_map   (SCM scm_map);

/* System Options */
SCM   g_rc_file_preview                (SCM mode);
SCM   g_rc_handleboxes                 (SCM mode);
SCM   g_rc_raise_dialog_boxes_on_expose(SCM mode);
SCM   g_rc_save_ui_settings            (SCM mode);
SCM   g_rc_toolbars                    (SCM mode);
SCM   g_rc_toolbars_mode               (SCM mode);
SCM   g_rc_show_toolbar_tips           (SCM mode);

/* Scrollbar Options */
SCM   g_rc_scrollbars                  (SCM mode);
SCM   g_rc_scrollbar_update            (SCM mode);
SCM   g_rc_scrollbars_visible          (SCM mode);
SCM   g_rc_scrollpan_steps             (SCM steps);

/* g_register.c */
void  g_register_funcs   (void);

/* g_select.c */
void  g_init_select      (void);

/* g_util.c */
void  g_init_util ();

/* g_window.c */
GschemToplevel *g_current_window  (void);
void  g_dynwind_window            (GschemToplevel *w_current);
void  g_init_window               (void);

typedef void (*geda_atexit_func) (void *);
typedef void (*geda_predicator)  (GschemToplevel *);

/* globals.c */
/* gschem.c */
void gschem_atexit (geda_atexit_func func, void * data);
void shut_down_gui (void);
void gschem_quit   (void);

/*! \note WEH: MACRO: I_CALLBACK_ARGUMENTS is left defined and is without a
 *             semi-colon. This macro is re-used in the associated source file */
#define I_CALLBACK_ARGUMENTS (GschemToplevel* w_current, unsigned int callback_action, GtkWidget *widget)

/* i_callbacks.c Hotkeys */
void       i_callback_view_pan_hotkey        I_CALLBACK_ARGUMENTS;
void       i_callback_view_pan_left          I_CALLBACK_ARGUMENTS;
void       i_callback_view_pan_right         I_CALLBACK_ARGUMENTS;
void       i_callback_view_pan_up            I_CALLBACK_ARGUMENTS;
void       i_callback_view_pan_down          I_CALLBACK_ARGUMENTS;

void       i_callback_cancel                 I_CALLBACK_ARGUMENTS;

/* i_command.c */
void        i_command_engage                (GschemToplevel *w_current);
void        i_command_disengage             (bool immediate, bool wait_return);
void        i_command_get_action_list       (GList** list);
const char *i_command_get_action_icon       (const char *command);
void        i_command_get_command_list      (GList** list);
bool        i_command_is_valid              (const char *command);
bool        i_command_map_icon              (const char *command, const char *icon);
void        i_command_process               (GschemToplevel *w_current, const char* command, int narg, char *arg, EID_ACTION_ORIGIN who);
void        i_command_shutdown              (void);

/* i_event.c */
void       i_event_block_buttons            (GschemToplevel *w_current);
void       i_event_unblock_buttons          (GschemToplevel *w_current);
void       i_event_block_handler            (GschemToplevel *w_current, EventHandler id);
void       i_event_unblock_handler          (GschemToplevel *w_current, EventHandler id);
void       i_event_setup_handlers           (GschemToplevel *w_current);
void       i_event_cancel_action_handler    (GschemToplevel *w_current);
void       i_event_end_action_handler       (GschemToplevel *w_current);
void       i_event_start_adder_handler      (GschemToplevel *w_current, ActionInit ifunc, ActionAdder resolver);
void       i_event_start_paster_handler     (GschemToplevel *w_current, ActionPaster resolver);
void       i_event_stop_action_handler      (GschemToplevel *w_current);
bool       i_event_close_wm                 (GschemToplevel* w_current, GdkEvent *event, GtkWidget *widget);

/* i_pan_world.c */
void       i_pan_auto                       (GschemToplevel *w_current, GdkEventCrossing *event);
void       i_pan_warp_cursor                (GtkWidget *widget, int x, int y);
void       i_pan_world_general              (GschemToplevel *w_current, Page *page, double world_cx, double world_cy,
                                             double relativ_zoom_factor, int flags);
void       i_pan_world                      (GschemToplevel *w_current, int x, int y);
void       i_pan_world_mouse                (GschemToplevel *w_current, int diff_x, int diff_y);

/* i_sessions.c */
int        i_sessions_delete_session        (GschemToplevel *w_current, const char *name);
void       i_sessions_destroy_sessions      (void);
int        i_sessions_export_session        (const char *name, const char *filename);
void       i_sessions_list_sessions         (void);
int        i_sessions_new_session           (GschemToplevel *w_current, const char *name);
bool       i_sessions_open_session          (GschemToplevel *w_current, const char *name);
int        i_sessions_rename_session        (GschemToplevel *w_current, const char *old_name, const char *new_name);
int        i_sessions_save_session          (GschemToplevel *w_current, const char *name);
GAY       *i_sessions_get_sessions          (void);
bool       i_sessions_is_enabled            (void);
bool       i_sessions_get_show_at_startup   (void);
void       i_sessions_set_show_at_startup   (bool show);
bool       i_sessions_present_at_startup    (void);
void       i_sessions_update_menus          (GschemToplevel *w_current);
void       i_sessions_init                  (GschemToplevel *w_current);

/* i_status.c */
void       i_status_set_state_msg           (GschemToplevel *w_current, enum x_states newstate, const char *message);
void       i_status_set_state               (GschemToplevel *w_current, enum x_states newstate);
void       i_status_show_msg                (GschemToplevel *w_current, const char *message);
void       i_status_show_state              (GschemToplevel *w_current, const char *message);
void       i_status_update_action_state     (GschemToplevel *w_current, int state);
void       i_status_update_coordinates      (GschemToplevel *w_current, int x, int y);
void       i_status_update_grid_info        (GschemToplevel *w_current);
void       i_status_update_sensitivities    (GschemToplevel *w_current);
void       i_status_update_title            (GschemToplevel *w_current);

/* i_threads.c */
void          gschem_threads_enter          (void);
void          gschem_threads_leave          (void);
bool          gschem_threads_init           (void);
unsigned int  gschem_threads_idle_add       (GschemSourceFunc function, void *data);
bool          gschem_threads_is_locked      (void);

/* i_vars.c */
char      *i_var_get_global_config_string   (EdaConfig *cfg, const char *str);
void       i_var_restore_group_color        (EdaConfig *cfg, const char *group, const char *key, GdkColor *var, int    index);
bool       i_var_restore_group_boolean      (EdaConfig *cfg, const char *group, const char *key, int      *var, int    def_val);
bool       i_var_restore_group_double       (EdaConfig *cfg, const char *group, const char *key, double   *var, double def_val);
bool       i_var_restore_group_integer      (EdaConfig *cfg, const char *group, const char *key, int      *var, int    def_val);
void       i_var_restore_global_boolean     (EdaConfig *cfg, const char *key,   int      *var, bool   def_val);
void       i_var_restore_global_double      (EdaConfig *cfg, const char *key,   double   *var, double def_val);
void       i_var_restore_global_integer     (EdaConfig *cfg, const char *key,   int      *var, int    def_val);
void       i_var_restore_global_color       (EdaConfig *cfg, const char *key,   GdkColor *var, int    index);

void       i_var_restore_window_boolean     (EdaConfig *cfg, const char *key, int      *var, bool def_val);
void       i_var_restore_window_integer     (EdaConfig *cfg, const char *key, int      *var, int  def_val);
void       i_var_restore_window_color       (EdaConfig *cfg, const char *key, GdkColor *var, int  index);

void       i_vars_set                       (GschemToplevel *w_current);
void       i_vars_freenames                 (void);
void       i_vars_init                      (GschemToplevel *w_current);
void       i_vars_atexit_save_user_config   (void * user_data);

/* i_window.c */
void       i_window_close_page              (GschemToplevel *w_current);
bool       i_window_get_pointer_position    (GschemToplevel *w_current, bool snapped, int *wx, int *wy);
void       i_window_on_page_changed         (GschemToplevel *w_current);
void       i_window_revert_page             (GschemToplevel *w_current);
void       i_window_set_cursor              (GschemToplevel *w_current, int cursor_id);
void       i_window_set_grid_type           (GschemToplevel *w_current);
void       i_window_set_pointer_position    (GschemToplevel *w_current, int wx, int wy);
void       i_window_set_viewport_size       (GschemToplevel *w_current);
void       i_window_show_attributes         (GschemToplevel *w_current, int scope);
void       i_window_zoom_all_pages          (GschemToplevel *w_current);

/* i_zoom_world.c */
void       i_zoom_world                      (GschemToplevel *w_current, EID_ZOOM_DIRECTIVE dir,
                                                                         EID_ACTION_ORIGIN  selected_from,
                                                                         EID_PAN_DIRECTIVES pan_flags);
void       i_zoom_world_extents              (GschemToplevel *w_current, const GList *list, int pan_flags);
void       i_zoom_world_specify              (GschemToplevel *w_current, double mag, int x, int y,
                                                                         EID_ACTION_ORIGIN  specified_from);
void       i_zoom_world_box                  (GschemToplevel *w_current, int pan_flags);
void       i_zoom_world_box_start            (GschemToplevel *w_current, int x, int y);
void       i_zoom_world_box_end              (GschemToplevel *w_current, int x, int y);
void       i_zoom_world_box_motion           (GschemToplevel *w_current, int x, int y);
void       i_zoom_world_box_invalidate_rubber(GschemToplevel *w_current);
void       i_zoom_world_box_draw_rubber      (GschemToplevel *w_current);

 /* m_basic.c */
int        mil_x                        (GschemToplevel *w_current, int val);
int        mil_y                        (GschemToplevel *w_current, int val);
int        pix_x                        (GschemToplevel *w_current, int val);
int        pix_y                        (GschemToplevel *w_current, int val);
void       WORLDtoSCREEN                (GschemToplevel *w_current, int x,  int y,  int *px, int *py);
void       SCREENtoWORLD                (GschemToplevel *w_current, int mx, int my, int *x,  int *y);
int        snap_grid                    (GschemToplevel *w_current, int input);
int        SCREENabs                    (GschemToplevel *w_current, int val);
int        WORLDabs                     (GschemToplevel *w_current, int val);
int        WORLDclip_change             (GschemToplevel *w_current, int *x1,   int *y1,  int *x2,    int *y2);
int        clip_nochange                (GschemToplevel *w_current, int x1,    int y1,   int x2,     int y2);
int        visible                      (GschemToplevel *w_current, int wleft, int wtop, int wright, int wbottom);
double     m_round_5_2_1                (double unrounded);

/* o_arc.c */
void       o_arc_end4                   (GschemToplevel *w_current, int radius, int start_angle, int arc_sweep);
void       o_arc_draw_rubber            (GschemToplevel *w_current);
void       o_arc_invalidate_rubber      (GschemToplevel *w_current);
void       o_arc_motion                 (GschemToplevel *w_current, int x, int y);
void       o_arc_start                  (GschemToplevel *w_current, int x, int y);

/* o_attrib.c */
void       o_attrib_attached_2_selection     (GschemToplevel *w_current, SELECTION *selection, GedaObject *selected);
void       o_attrib_attach_list_2_object     (GschemToplevel *w_current, GList *list);
void       o_attrib_deselect_invisible       (GschemToplevel *w_current, SELECTION *selection, GedaObject *selected);
void       o_attrib_select_invisible         (GschemToplevel *w_current, SELECTION *selection, GedaObject *selected);
void       o_attrib_toggle_visibility        (GschemToplevel *w_current, GedaObject *object);
void       o_attrib_toggle_show_name_value   (GschemToplevel *w_current, GedaObject *object, int new_show_name_value);
OBJ       *o_attrib_add_attrib               (GschemToplevel *w_current, const char *text_string, int visibility,
                                              int show_name_value, int color, GedaObject *object);
bool       o_attrib_reset_position           (GschemToplevel *w_current, GedaObject *parent, GedaObject *attrib);

/* o_box.c */
void       o_box_draw_rubber            (GschemToplevel *w_current);
void       o_box_invalidate_rubber      (GschemToplevel *w_current);
void       o_box_motion                 (GschemToplevel *w_current, int x, int y);
void       o_box_start                  (GschemToplevel *w_current, int x, int y);

/* o_break.c */
int        o_break_start                (GschemToplevel *w_current, int w_x, int w_y);
int        o_break_end                  (GschemToplevel *w_current, int x, int y);
void       o_break_hot                  (GschemToplevel *w_current, GList *object_list, int x, int y);
int        o_break_interrogate          (GschemToplevel *w_current, GList *object_list);

/* o_buffer.c */
void       o_buffer_copy                (GschemToplevel *w_current, int buf_num);
void       o_buffer_clear               (GschemToplevel *w_current, int buf_num);
void       o_buffer_cut                 (GschemToplevel *w_current, int buf_num);
bool       o_buffer_paste_start         (GschemToplevel *w_current, int x, int y);
void       o_buffer_init                (void);
void       o_buffer_free                (GschemToplevel *w_current);

/* o_bus.c */
void       o_bus_draw_rubber            (GschemToplevel *w_current);
void       o_bus_invalidate_rubber      (GschemToplevel *w_current);
void       o_bus_motion                 (GschemToplevel *w_current, int x, int y);
void       o_bus_start                  (GschemToplevel *w_current, int x, int y);

/* o_circle.c */
void       o_circle_draw_rubber         (GschemToplevel *w_current);
void       o_circle_invalidate_rubber   (GschemToplevel *w_current);
void       o_circle_motion              (GschemToplevel *w_current, int x, int y);
void       o_circle_start               (GschemToplevel *w_current, int x, int y);

/* o_complex.c */
void       o_complex_export                  (GschemToplevel *w_current, GedaObject *o_current);
void       o_complex_invalidate_rubber       (GschemToplevel *w_current, GedaObject *o_current);
void       o_complex_place_changed_run_hook  (GschemToplevel *w_current);
void       o_complex_start                   (GschemToplevel *w_current, const CLibSymbol *sym, int state);
bool       o_complex_reset_attrib_positions  (GschemToplevel *w_current, GedaObject *complex);
void       o_complex_translate_all           (GschemToplevel *w_current, int offset, const GList *list);
void       o_complex_translate_list          (GschemToplevel *w_current, const GList *o_list, int x_offset, int y_offset);


/* o_copy.c */
void       o_copy_cancel                (GschemToplevel *w_current);
void       o_copy_end                   (GschemToplevel *w_current);
void       o_copy_multiple_end          (GschemToplevel *w_current);
void       o_copy_multiple_start        (GschemToplevel *w_current, int x, int y);
void       o_copy_start                 (GschemToplevel *w_current, int x, int y);

/* o_delete.c */
void       o_delete                     (GschemToplevel *w_current, GedaObject *object);
void       o_delete_selected            (GschemToplevel *w_current);

/* o_edit.c */
bool       o_edit_add_titleblock        (GschemToplevel *w_current, Page *page, const char *tblock);
void       o_edit_objects               (GschemToplevel *w_current, GList *list, int who);
void       o_edit_lock_selection        (GschemToplevel *w_current);
void       o_edit_unlock_selection      (GschemToplevel *w_current);
void       o_edit_mirror_world          (GschemToplevel *w_current, int centerx, int centery, GList *list);
void       o_edit_offset_hot            (GschemToplevel *w_current, int x, int y, GList *list);
void       o_edit_offset_world          (GschemToplevel *w_current, int x, int y, GList *list);
void       o_edit_rotate_world          (GschemToplevel *w_current, int centerx, int centery, int angle, GList *list);
void       o_edit_set_selectable        (GschemToplevel *w_current, GedaObject *object, bool state);
bool       o_edit_show_hidden           (GschemToplevel *w_current, const GList *o_list, int inherited);
void       o_edit_show_netnames         (GschemToplevel *w_current, const GList *o_list);
int        o_edit_find_text             (GschemToplevel *w_current, const GList *o_list, const char *stext, int descend, int skip);
void       o_edit_hide_specific_text    (GschemToplevel *w_current, const GList *o_list, const char *stext);
void       o_edit_show_specific_text    (GschemToplevel *w_current, const GList *o_list, const char *stext);
void       o_edit_snap                  (GschemToplevel *w_current, const GList *o_list);
OBJ       *o_edit_update_component      (GschemToplevel *w_current, GedaObject *o_current);

/* o_extend.c */
int        o_extend_end                 (GschemToplevel *w_current, int x, int y);
void       o_extend_hot                 (GschemToplevel *w_current, GList *object_list, int x, int y);
int        o_extend_interrogate         (GschemToplevel *w_current, GList *object_list);
int        o_extend_start               (GschemToplevel *w_current, int w_x, int w_y);

/* o_find.c */
OBJ       *o_find_get_hit               (GschemToplevel *w_current, int w_x, int w_y);
bool       o_find_object                (GschemToplevel *w_current, int x, int y, bool deselect_afterwards);
OBJ       *o_find_selected_object       (GschemToplevel *w_current, int x, int y);

/* o_grips.c */
/*
OBJ       *o_grips_search_world         (GschemToplevel *w_current, int x, int y, int *whichone);
OBJ       *o_grips_search_arc_world     (GschemToplevel *w_current, GedaObject *o_current, int x, int y, int size, int *whichone);
OBJ       *o_grips_search_box_world     (GschemToplevel *w_current, GedaObject *o_current, int x, int y, int size, int *whichone);
OBJ       *o_grips_search_path_world    (GschemToplevel *w_current, GedaObject *o_current, int x, int y, int size, int *whichone);
OBJ       *o_grips_search_picture_world (GschemToplevel *w_current, GedaObject *o_current, int x, int y, int size, int *whichone);
OBJ       *o_grips_search_circle_world  (GschemToplevel *w_current, GedaObject *o_current, int x, int y, int size, int *whichone);
OBJ       *o_grips_search_line_world    (GschemToplevel *w_current, GedaObject *o_current, int x, int y, int size, int *whichone);
*/
int        o_grips_compute_drawn_size   (GschemToplevel *w_current);
bool       o_grips_start                (GschemToplevel *w_current, int x, int y);
void       o_grips_motion               (GschemToplevel *w_current, int x, int y);
void       o_grips_end                  (GschemToplevel *w_current);
void       o_grips_cancel               (GschemToplevel *w_current);
void       o_grips_draw_rubber          (GschemToplevel *w_current);

/* o_invalidate.c */
void       o_invalidate_all             (GschemToplevel *w_current);
int        o_invalidate_rubber          (GschemToplevel *w_current);
void       o_invalidate_rectangle       (GschemToplevel *w_current, int x1, int y1, int x2, int y2);
void       o_invalidate_object          (GschemToplevel *w_current, GedaObject *object);
void       o_invalidate_force           (GschemToplevel *w_current, GedaObject *object);
void       o_invalidate_list            (GschemToplevel *w_current, GList *list);

/* o_line.c */
void       o_line_draw_rubber           (GschemToplevel *w_current);
void       o_line_invalidate_rubber     (GschemToplevel *w_current);
void       o_line_motion                (GschemToplevel *w_current, int x, int y);
void       o_line_start                 (GschemToplevel *w_current, int x, int y);

/* o_move.c */
void       o_move_cancel                (GschemToplevel *w_current);
void       o_move_draw_rubber           (GschemToplevel *w_current, int drawing);
void       o_move_end                   (GschemToplevel *w_current);
void       o_move_end_lowlevel          (GschemToplevel *w_current, GedaObject *object, int diff_x, int diff_y);
void       o_move_end_rubberband        (GschemToplevel *w_current, int world_diff_x, int world_diff_y, GList **objects);
void       o_move_invalidate_rubber     (GschemToplevel *w_current, int drawing);
void       o_move_motion                (GschemToplevel *w_current, int x, int y);
void       o_move_prep_rubberband       (GschemToplevel *w_current);
//int        o_move_return_whichone       (GedaObject     *object, int x, int y);
//GLT       *o_move_stretch_add           (GList *list, GedaObject *object, int whichone);
//GLT       *o_move_stretch_remove        (GList *list, GedaObject *object);
//void       o_move_stretch_print_all     (GList *list);
//void       o_move_stretch_destroy_all   (GList *list);
void       o_move_start                 (GschemToplevel *w_current, int x, int y);
void       o_move_start_drag            (GschemToplevel *w_current, int w_x, int w_y);

/* o_net.c */
int        o_net_add_busrippers         (GschemToplevel *w_current, GedaObject *net_obj, GList *other_objects);
void       o_net_draw_rubber            (GschemToplevel *w_current );
void       o_net_guess_direction        (GschemToplevel *w_current, int x, int y);
void       o_net_motion                 (GschemToplevel *w_current, int x, int y);
void       o_net_invalidate_rubber      (GschemToplevel *w_current);
bool       o_net_reset                  (GschemToplevel *w_current);
void       o_net_start                  (GschemToplevel *w_current, int x, int y);
void       o_net_start_magnetic         (GschemToplevel *w_current, int x, int y);

/* o_page.c */
void       o_page_draw_after                 (GschemToplevel *w_current);
void       o_page_draw_before                (GschemToplevel *w_current);
void       o_page_draw_first                 (GschemToplevel *w_current, GList *object_list);
void       o_page_draw_last                  (GschemToplevel *w_current, GList *object_list);

/* o_path.c */
void       o_path_continue                   (GschemToplevel *w_current, int w_x, int w_y);
void       o_path_close                      (GschemToplevel *w_current);
void       o_path_draw_rubber                (GschemToplevel *w_current);
void       o_path_draw_rubber_grips          (GschemToplevel *w_current);
void       o_path_end                        (GschemToplevel *w_current, int x, int y);
void       o_path_invalidate_rubber          (GschemToplevel *w_current);
void       o_path_invalidate_rubber_grips    (GschemToplevel *w_current);
void       o_path_motion                     (GschemToplevel *w_current, int w_x, int w_y);
void       o_path_motion_grips               (GschemToplevel *w_current, int x, int y);
void       o_path_start                      (GschemToplevel *w_current, int x, int y);
void       o_path_undo                       (GschemToplevel *w_current);

/* o_picture.c */
void       o_picture_draw_rubber             (GschemToplevel *w_current);
bool       o_picture_exchange                (GschemToplevel *w_current, const char *filename, GedaObject *o_current, GError **error);
void       o_picture_exchange_file           (GschemToplevel *w_current, GedaObject *o_current);
void       o_picture_export                  (GschemToplevel *w_current, GedaObject *o_current);
void       o_picture_invalidate_rubber       (GschemToplevel *w_current);
void       o_picture_motion                  (GschemToplevel *w_current, int x, int y);
bool       o_picture_set_pixbuf              (GschemToplevel *w_current, char *filename);
void       o_picture_start                   (GschemToplevel *w_current, int x, int y);

/* o_pin.c */
void       o_pin_draw_rubber                 (GschemToplevel *w_current);
void       o_pin_invalidate_rubber           (GschemToplevel *w_current);
void       o_pin_motion                      (GschemToplevel *w_current, int x, int y);
void       o_pin_start                       (GschemToplevel *w_current, int x, int y);

/* o_place.c */
void       o_place_end                       (GschemToplevel *w_current, int continue_placing, GList **ret_new_objects, Hooker id);
void       o_place_component_end             (GschemToplevel *w_current);
void       o_place_motion                    (GschemToplevel *w_current, int x, int y);
void       o_place_invalidate_rubber         (GschemToplevel *w_current, int drawing);
void       o_place_draw_rubber               (GschemToplevel *w_current, int drawing);
void       o_place_mirror                    (GschemToplevel *w_current);
void       o_place_rotate                    (GschemToplevel *w_current, int angle);
bool       o_place_start                     (GschemToplevel *w_current, int x, int y);

/* o_redraw.c */
int        o_redraw_cleanstates              (GschemToplevel *w_current);
void       o_redraw_rectangle                (GschemToplevel *w_current, GdkRectangle *rectangle);
void       o_redraw_list                     (GschemToplevel *w_current, GList *list);

/* o_select.c */
void       o_select_object                   (GschemToplevel *w_current, GedaObject *o_current, int type, int count);
void       o_select_add_object               (GschemToplevel *w_current, GedaObject *object);
void       o_select_add_list                 (GschemToplevel *w_current, GList *list);
void       o_select_start                    (GschemToplevel *w_current, int wx, int wy);
void       o_select_end                      (GschemToplevel *w_current, int wx, int wy);
bool       o_select_motion                   (GschemToplevel *w_current, int wx, int wy);
int        o_select_box_start                (GschemToplevel *w_current, int x, int y);
void       o_select_box_end                  (GschemToplevel *w_current, int x, int y);
void       o_select_box_motion               (GschemToplevel *w_current, int x, int y);
void       o_select_box_invalidate_rubber    (GschemToplevel *w_current);
void       o_select_box_draw_rubber          (GschemToplevel *w_current);
void       o_select_box_search               (GschemToplevel *w_current);
void       o_select_connected_nets           (GschemToplevel *w_current, GedaObject *o_current);
int        o_select_get_count                (GschemToplevel *w_current);
bool       o_select_is_selection             (GschemToplevel *w_current);
void       o_select_unselect_all             (GschemToplevel *w_current);
void       o_select_visible_unlocked         (GschemToplevel *w_current);
void       o_select_move_to_place_list       (GschemToplevel *w_current);
GLT*       o_select_get_list_selected        (GschemToplevel *w_current, char otype);
OBJ*       o_select_return_first_object      (GschemToplevel *w_current);
void       o_select_cancel_events            (GschemToplevel *w_current);
void       o_select_connect_selector         (GschemToplevel *w_current, geda_predicator func);

/* o_slot.c */
void       o_slot_start                      (GschemToplevel *w_current, GedaObject *object);
void       o_slot_end                        (GschemToplevel *w_current, GedaObject *object, const char *string);

/* o_text.c */
int        o_text_get_rendered_bounds        (void *user_data, GedaObject *object, int *min_x, int *min_y, int *max_x, int *max_y);
void       o_text_prepare_place              (GschemToplevel *w_current, char *text);
void       o_text_edit                       (GschemToplevel *w_current, GedaObject *o_current);
void       o_text_edit_end                   (GschemToplevel *w_current, char *string, int text_align,int text_color, int text_size, int rotate);
void       o_text_change                     (GschemToplevel *w_current, GedaObject *object, char *string, int visibility, int show);

/* o_undo.c */
void        o_undo_init                      (GschemToplevel *w_current);
void        o_undo_savestate                 (GschemToplevel *w_current, int flag);
void        o_undo_savestate_object          (GschemToplevel *w_current, int flag, GedaObject *object);
char       *o_undo_find_prev_filename        (UNDO *start);
GLT        *o_undo_find_prev_object_head     (UNDO *start);
void        o_undo_callback                  (GschemToplevel *w_current, int type);
void        o_undo_finalize                  (void);
void        o_undo_remove_last_undo          (GschemToplevel *w_current);

/* gschem_parsecmd.c */
int         gschem_parse_commandline        (int argc, char *argv[]);

/* x_autonumber.c */
void        autonumber_text_dialog          (GschemToplevel *w_current);

/* x_clipboard.c */
void          x_clipboard_init              (GschemToplevel *w_current);
void          x_clipboard_finish            (GschemToplevel *w_current);
void          x_clipboard_query_usable      (GschemToplevel *w_current, void (*callback) (int, void *), void *userdata);
bool          x_clipboard_set               (GschemToplevel *w_current, const GList *object_list);
GLT          *x_clipboard_get               (GschemToplevel *w_current);

/* x_color.c */ /*
void          x_color_init                  (void);
void          x_color_free                  (void);
void          x_color_allocate              (void);
GdkColor     *x_color_get_color_from_index  (int color);
GArray       *x_color_get_display_color_map (void);
GArray       *x_color_get_outline_color_map (void);
COLOR        *x_color_lookup                (int color);
bool          x_color_get_state             (int color);
void          x_color_set_state             (int color, int state);
bool          x_color_display_enabled       (int index);
int           x_color_load_scheme           (char * scheme);
*/
/* x_compselect.c */
void          x_compselect_open             (GschemToplevel *w_current);
void          x_compselect_deselect         (GschemToplevel *w_current);

/* x_console.c */
const char   *x_console_get_alphanumeric    (void);
const char   *x_console_get_numeric         (void);
int           x_console_get_number          (void);
int           x_console_get_integer         (void);
float         x_console_get_real            (void);
const char   *x_console_get_string          (void);
void          x_console_open                (GschemToplevel *w_current);
void          x_console_close               (void);
void          x_console_init_commands       (GschemToplevel *w_current, int mode);
void          x_console_update_decorated    (GschemToplevel *w_current);
void          q_log_message                 (const char *format, ...);
void          v_log_message                 (const char *format, ...);
void          x_log_message                 (const char *log_domain, GLogLevelFlags log_level,
                                             const char *message);

/* x_coord.c */
void          x_dialog_coord_update_display (GschemToplevel *w_current, int sx, int sy, int wx, int wy);
void          x_dialog_coord_dialog         (GschemToplevel *w_current);

/* x_dialog.c */
  /* Dialog-Utility functions */
AtkObject    *atk_widget_linked_label_new           (GtkWidget *label, GtkWidget *linkto);
GtkWidget    *create_pixmap                         (const char *filename);
void          x_dialog_set_icon                     (GtkWidget *dialog, const char *icon_name);
void          destroy_gschem_dialog                 (GtkWidget *widget, GtkWidget **window);
GtkWidget    *x_dialog_get_bulb_image               (bool WhichState);
void          x_dialog_set_bulb_on                  (GtkWidget *widget);
void          x_dialog_set_bulb_off                 (GtkWidget *widget);
void          x_dialog_bulb_group_set_active        (GSList *RadioGroupList, int value);

void          select_all_text_in_textview           (GtkTextView *textview);
int           text_view_calculate_real_tab_width    (GtkTextView *textview, int tab_size);
GtkWidget    *create_color_menu                     (GschemToplevel * w_current, int color_index);

  /* Standard-Dialogs */
void          about_dialog                  (GschemToplevel *w_current);
void          snap_size_dialog              (GschemToplevel *w_current);
void          text_size_dialog              (GschemToplevel *w_current);

  /* Editing-Dialogs */
void          x_dialog_edit_arc_angle       (GschemToplevel *w_current, GedaObject *arc_object);
void          x_dialog_edit_fill_type       (GschemToplevel *w_current);
void          x_dialog_edit_line_type       (GschemToplevel *w_current);
void          x_dialog_find_text            (GschemToplevel *w_current);
void          x_dialog_hide_text            (GschemToplevel *w_current);
void          x_dialog_patch_text           (GschemToplevel *w_current);
void          x_dialog_show_text            (GschemToplevel *w_current);
void          x_dialog_text_input           (GschemToplevel *w_current);
void          x_dialog_translate            (GschemToplevel *w_current);

  /* Systemic-Dialogs */
void          x_dialog_hotkeys              (GschemToplevel *w_current);
void          x_dialog_raise_all            (GschemToplevel *w_current);
void          x_dialog_symbol_changed       (GschemToplevel *w_current);
int           x_dialog_validate_attribute   (GtkWindow      *parent, char *attribute);

  /* Gschem-Generic-Dialogs */
int           x_dialog_confirmation         (const char *, IDE_MESSAGE_TYPE context, bool thread);
int           x_dialog_confirm_with_cancel  (const char *, IDE_MESSAGE_TYPE context, bool thread);
void          x_dialog_message_with_markup  (const char *msg1, const char *msg2,
                                             IDE_MESSAGE_TYPE context, const char *title);
char         *x_dialog_select_file          (GschemToplevel *w_current, const char *, const char *, int);
void          x_dialog_show_message         (const char *, IDE_MESSAGE_TYPE context, const char *title);
GLT          *x_dialog_sym_not_embedded     (GschemToplevel *w_current, GList *sym);

/* x_dnd.c */
bool          x_dnd_receive_string          (GschemToplevel *w_current, int x, int y, const char *string, int where);
void          x_dnd_setup_event_handlers    (GschemToplevel *w_current);

/* x_draw.c */
void          x_draw_box                    (GschemToplevel *w_current);
void          x_draw_object                 (GschemToplevel *w_current, GedaObject *object);
void          x_draw_set_surface            (GschemToplevel *w_current);
char         *x_draw_get_font               (void);
void          x_draw_set_font               (const char *font_name, int size);
GArray       *x_draw_get_font_list          (const char *pattern);
char         *x_draw_strip_font_provider    (const char *font_string);
int           x_draw_set_text_bounds        (GedaObject *object);
void          x_draw_initialize             (GschemToplevel *w_current);
void          x_draw_shutdown               (void *user_data);

/* x_edit_attrib.c */
//int         option_menu_get_history       (GedaOptionMenu *option_menu);
/*    attrib_edit_dialog_ok                 (GtkWidget *w, GschemToplevel *w_current);*/
/*    attrib_edit_dialog                    (GschemToplevel *w_current, GedaObject *attr_obj, int flag); */
void          x_attrib_add_dialog           (GschemToplevel *w_current, GedaObject *object);
void          x_attrib_edit_dialog          (GschemToplevel *w_current, GedaObject *object);

/* x_array.c */
void        x_dialog_array_edit             (GschemToplevel *w_current);

/* x_confirm_close */
bool        x_confirm_close_changed_page    (GschemToplevel *w_current, Page *page);
bool        x_confirm_close_window          (GschemToplevel *w_current);

/* x_edit_color.c */
void        x_dialog_edit_color             (GschemToplevel *w_current);

/* x_edit_pin.c */
void        x_dialog_edit_pin_type          (GschemToplevel *w_current);

/* x_edit_property.c */
void        x_dialog_edit_properties        (GschemToplevel *w_current, GedaObject *o_current);

/* x_edit_slot.c */
void        x_dialog_edit_slot              (GschemToplevel *w_current, const char *slots, const char *slot);

/* x_edit_text.c */
void        x_dialog_edit_text              (GschemToplevel *w_current, GedaObject *o_current);

/* x_event.c */
bool       x_event_button_pressed           (GtkWidget     *widget, GdkEventButton    *event, GschemToplevel *w_current);
bool       x_event_button_released          (GtkWidget     *widget, GdkEventButton    *event, GschemToplevel *w_current);
bool       x_event_configure                (GtkWidget     *widget, GdkEventConfigure *event, GschemToplevel *w_current);
void       x_event_governor                 (GschemToplevel *w_current);
bool       x_event_expose                   (GtkWidget     *widget, GdkEventExpose    *event, GschemToplevel *w_current);
bool       x_event_key                      (GtkWidget     *widget, GdkEventKey       *event, GschemToplevel *w_current);
bool       x_event_motion                   (GtkWidget     *widget, GdkEventMotion    *event, GschemToplevel *w_current);
bool       x_event_scroll                   (GtkWidget     *widget, GdkEventScroll    *event, GschemToplevel *w_current);

void       x_event_hschanged                (GtkAdjustment *adjust, GschemToplevel    *w_current);
void       x_event_vschanged                (GtkAdjustment *adjust, GschemToplevel    *w_current);

/* x_fileselect.c */
GSL       *x_fileselect_list                (GschemToplevel *w_current);
void       x_fileselect_save                (GschemToplevel *w_current);
char      *x_fileselect_select_image        (GschemToplevel *w_current, const char *filename);
bool       x_fileselect_load_backup         (const char *message, GschemToplevel *w_current);

/* x_grid.c */
void       x_grid_draw_grid_region          (GschemToplevel *w_current, GdkRectangle *rectangle);
void       x_grid_configure_variables       (GschemToplevel *w_current);
int        x_grid_query_drawn_spacing       (GschemToplevel *w_current);
void       x_grid_repaint_background        (GschemToplevel *w_current, GdkRectangle *rectangle);
void       x_grid_draw_tiles                (GschemToplevel *w_current);

/* x_guile.c */
void       x_guile_dialog                   (GschemToplevel *w_current);

/* x_icons.c */
void       x_icons_add_search_path          (const char *path);
bool       x_icons_factory_lookup           (const char *icon_id);
GtkWidget *x_icons_get_action_icon          (const char *action, int size);
GtkWidget *x_icons_get_factory_icon         (const char *icon_id, int size);
void       x_icons_set_default_icon         (const char *icon_name);
void       x_icons_initialize               (void);

/* x_image.c */
void       x_image_init                     (void);

void       x_image_lowlevel                 (GschemToplevel *w_current, const char* filename,
                                             int desired_width, int desired_height,
                                             const char *filetype, ImageExtent extent,
                                             bool use_print_map, bool invert_color_bw );

void       x_image_setup                    (GschemToplevel *w_current, IMAGE_TYPES default_type);
GdkPixbuf *x_image_get_pixbuf               (GschemToplevel *w_current, ImageExtent extent,
                                             bool use_print_map, bool invert_color_bw);

/* x_misc.c */
bool       x_show_uri                       (const char *str);

/* x_menus.c */
void        x_menu_free_all(void);
GtkWidget  *x_menu_get_main_menu            (GschemToplevel *w_current);
GtkWidget  *x_menu_setup_ui                 (GschemToplevel *w_current);
int         x_menu_setup_popup              (GschemToplevel *w_current);
int         x_menu_display_main_popup       (GschemToplevel *w_current, GdkEventButton *event);
int         x_menu_display_path_popup       (GschemToplevel *w_current, GdkEventButton *event);
void        x_menu_sensitivity              (GschemToplevel *w_current, const char *buf, int flag);
void        x_menu_popup_sensitivity        (GschemToplevel *w_current, const char *name, int flag);
void        x_menu_save_state               (GschemToplevel *w_current);
const char *x_menu_get_buffer_menu          (GschemToplevel *w_current);
void        x_menu_set_grid_radio           (GschemToplevel *w_current);
void        x_menu_set_icon_visibility      (GschemToplevel *w_current, bool state);
void        x_menu_set_togglable            (GschemToplevel *w_current, int toggle_id, bool state);
void        x_menu_set_toolbar_toggle       (GschemToplevel *w_current, int toggle_id, bool state);
void        x_menu_set_toolbar_toggle_tips  (GschemToplevel *w_current, bool state);
void        x_menu_attach_recent_submenu    (GschemToplevel *w_current);
void        x_menu_recent_files_load        (void);
void        x_menu_recent_files_save        (void       *user_data);
void        x_menu_recent_files_add         (const char *filename);
const char *x_menu_recent_files_last        (void);

/* x_multiattrib.c */
void       x_multiattrib_open               (GschemToplevel *w_current);
void       x_multiattrib_close              (GschemToplevel *w_current);
void       x_multiattrib_update             (GschemToplevel *w_current);

/* x_multimulti.c */

/* x_pagesel.c */
void       x_pagesel_open                   (GschemToplevel *w_current);
void       x_pagesel_close                  (GschemToplevel *w_current);
void       x_pagesel_update                 (GschemToplevel *w_current);

/* x_preview.c */

/* x_print.c */
void       x_print_setup                    (GschemToplevel *w_current, char *filename);
bool       x_print_export_pdf_page          (GschemToplevel *w_current, const char *filename);
bool       x_print_export_pdf               (GschemToplevel *w_current, const char *filename);
void       x_print                          (GschemToplevel *w_current);

/* x_rc.c */
void       x_rc_parse_gschem                (GschemToplevel *w_current, const char *rcfile);

/* x_scroll.c */
void       x_hscrollbar_set_ranges          (GschemToplevel *w_current);
void       x_hscrollbar_update              (GschemToplevel *w_current);
void       x_vscrollbar_set_ranges          (GschemToplevel *w_current);
void       x_vscrollbar_update              (GschemToplevel *w_current);
void       x_scrollbars_update              (GschemToplevel *w_current);

/* x_sessions.c */
void       x_sessions_manage_dialog         (GschemToplevel *w_current);
void       x_sessions_open_dialog           (GschemToplevel *w_current);
void       x_sessions_new_dialog            (GschemToplevel *w_current);
void       x_sessions_save_as_dialog        (GschemToplevel *w_current);
void       x_sessions_save_settings         (GschemToplevel *w_current);

/* x_settings.c */
void       x_configure_settings             (GschemToplevel *w_current);
void       x_settings_save_settings         (GschemToplevel *w_current);
bool       x_settings_set_scm_int           (char *symbol_name, int value );

/* x_settings_dialog.c */
int        x_settings_lookup_cursor         (int offset);

/* x_status_bar.c */
void       x_status_bar_update_grid_label   (GschemToplevel *w_current);
void       x_status_bar_update_middle_mouse (GschemToplevel *w_current, const char *string);
void       x_status_bar_update_third_mouse  (GschemToplevel *w_current);
GtkWidget *x_status_bar_create              (GschemToplevel *w_current);

/* x_stroke.c */
void       x_stroke_init                    (void);
void       x_stroke_free                    (void);
void       x_stroke_record                  (GschemToplevel *w_current, int x, int y);
int        x_stroke_translate_and_execute   (GschemToplevel *w_current);

/* x_toolbars.c */
void       x_toolbars_save_state            (GschemToplevel *w_current);
void       x_toolbars_restore_state         (GschemToplevel *w_current);
void       x_toolbars_finalize              (GschemToplevel *w_current);
void       x_toolbars_free_window           (GschemToplevel *w_current);
void       x_toolbars_init_window           (GschemToplevel *w_current);
void       x_toolbars_init_top              (GschemToplevel *w_current, GtkWidget *parent_container);
void       x_toolbars_init_left             (GschemToplevel *w_current, GtkWidget *parent_container);
void       x_toolbars_init_bottom           (GschemToplevel *w_current, GtkWidget *parent_container);
void       x_toolbars_set_sensitivities     (GschemToplevel *w_current, EID_SENITIVITY_MODE mode, bool state);
void       x_toolbars_set_show_tooltips     (GschemToplevel *w_current, bool show_tips);
void       x_toolbar_icons_only             (GtkWidget *widget, GschemToplevel *w_current);
void       x_toolbar_text_only              (GtkWidget *widget, GschemToplevel *w_current);
void       x_toolbar_display_both           (GtkWidget *widget, GschemToplevel *w_current);
void       x_toolbar_display_horiz          (GtkWidget *widget, GschemToplevel *w_current);
void       x_toolbars_turn_off_all_radios   (GschemToplevel *w_current);
void       x_toolbars_activate_select       (GschemToplevel *w_current);
void       x_toolbars_set_grid_radio        (GschemToplevel *w_current);
void       x_toolbars_update                (GschemToplevel *w_current);

/* x_window.c */
void       x_window_setup                   (GschemToplevel *w_current);
bool       x_window_setup_context           (GschemToplevel *w_current);
void       x_window_restore_settings        (GschemToplevel *w_current);
void       x_window_save_settings           (GschemToplevel *w_current);
void       x_window_create_main             (GschemToplevel *w_current);
void       x_window_close_edit_dialogs      (GschemToplevel *w_current);
void       x_window_close_all_dialogs       (GschemToplevel *w_current);
void       x_window_close                   (GschemToplevel *w_current);
void       x_window_close_all               (GschemToplevel *w_current);
Page      *x_window_open_page               (GschemToplevel *w_current, const char *filename);
void       x_window_close_page              (GschemToplevel *w_current, Page *page);
void       x_window_reset_page_geometry     (GschemToplevel *w_current, Page *page);
int        x_window_save_page               (GschemToplevel *w_current, Page *page, const char *filename);
void       x_window_set_current_page        (GschemToplevel *w_current, Page *page);
void       x_window_set_scroll_visibility   (GschemToplevel *w_current);
void       x_window_setup_page              (GschemToplevel *w_current, Page *page, int xmin, int xmax, int ymin, int ymax);
void       x_window_update_title            (GschemToplevel *w_current);

void       x_window_add_toolbar_toggle       (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_attribute_toolbar_toggle (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_gridsnap_toolbar_toggle  (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_edit_toolbar_toggle      (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_page_toolbar_toggle      (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_standard_toolbar_toggle  (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_select_toolbar_toggle    (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_symbol_toolbar_toggle    (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_zoom_toolbar_toggle      (GtkWidget *widget, GschemToplevel *w_current);
void       x_window_toolbar_tips_toggle      (GtkWidget *widget, GschemToplevel *w_current);

END_DECLS

#undef GAY
#undef EHP
#undef GHT
#undef GLS
#undef GSL
#undef GLT
#undef OBJ
