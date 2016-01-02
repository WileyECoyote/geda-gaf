/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */

BEGIN_DECLS

/* f_basic.c */
         bool    f_has_active_autosave           (const char   *filename, GError **err);
          int    f_open                          (GedaToplevel *toplevel, Page *page, const char *filename, GError **err);
          int    f_open_flags                    (GedaToplevel *toplevel, Page *page, const char *filename, const int flags, GError **err);
         void    f_close                         (GedaToplevel *toplevel);
         bool    f_save                          (GedaToplevel *toplevel, Page *page, const char *filename, GError **error);
         void    f_remove_backup_file            (const char *filename);

/* f_file.c */
          int    f_file_copy                     (const char *source, const char *target);
          int    f_file_cmp_mod_time             (const char *filename, time_t ref_time);
         char   *f_file_follow_symlinks          (const char *filename, GError **error);
         char   *f_file_normalize_name           (const char *filename, GError **error);
          int    f_file_remove                   (const char *pathname);
         bool    f_file_remove_extension         (      char *filename);

/* f_get.c */
         char   *f_get_autosave_filename         (const char *filename);
         char   *f_get_basename                  (const char *path);
         char   *f_get_bitmap_filespec           (const char *filename);
         char   *f_get_data_filespec             (const char *filename);
       GSList   *f_get_dir_list_files            (      char *path, char *filter);

         bool    f_get_file_contents             (const char *filename, char **contents, size_t *length, GError **err);
   const char   *f_get_filename_ext              (const char *filename);
   const char   *f_get_format_header             (void);
         bool    f_get_is_path_absolute          (const char *filename);

/* f_path.c */
         void    f_path_free                     (void);
         char   *f_path_get_dirname              (const char *filename);
   const char   *f_path_sys_data                 (void);
   const char   *f_path_sys_doc                  (void);
   const char   *f_path_sys_config               (void);
   const char   *f_path_user_config              (void);
          int    f_path_create                   (const char *path, mode_t mode);

/* f_print.c */
          int    f_print_file                    (GedaToplevel *toplevel, Page *page, const char *filename);
          int    f_print_command                 (GedaToplevel *toplevel, Page *page, const char *command);
          int    f_print_stream                  (GedaToplevel *toplevel, Page *page, FILE *fp);
         void    f_print_set_type                (GedaToplevel *toplevel, int type);

/* g_evaluate.c */
          SCM    g_scm_eval_action               (SCM action);
          SCM    g_scm_eval_protected            (SCM exp, SCM module_or_state);
          SCM    g_scm_eval_string_protected     (SCM str);
          SCM    g_scm_c_eval_string_protected   (const char *str);
         bool    g_read_scheme_file              (const char *filename, GError **err);

/* g_rc_color.c */
          SCM    g_rc_color_map_to_scm           (const COLOR *map);
         void    g_rc_color_map_from_scm         (COLOR *map, SCM lst, const char *scheme_proc_name);

/* g_rc_parse.c */
          SCM    g_rc_parse_mode                 (SCM scmmode, const char *rc_name, int *mode_var, const vstbl_entry *table, int table_size);
         bool    g_rc_parse_file                 (const char *rcfile, EdaConfig *cfg, GError **err);
         bool    g_rc_parse_system               (const char *rcname, GError **err);
         bool    g_rc_parse_user                 (const char *rcname, GError **err);
         bool    g_rc_parse_local                (const char *rcname, const char *path,    GError       **err);
         bool    g_rc_parse                      (const char *pname,  const char *rcname,  const char    *rcfile);
         void    g_rc_gafrc_parse_handler        (const char *dummy,  ConfigParseErrorFunc handler, void *user_data);
         void    g_rc_rcname_parse_handler       (const char *rcname, ConfigParseErrorFunc handler, void *user_data);
         void    g_rc_rcfile_parse_handler       (const char *rcfile, ConfigParseErrorFunc handler, void *user_data);
         void    g_rc_parse_handler              (const char *rcname, const char *rcfile, ConfigParseErrorFunc handler, void *user_data);
          SCM    g_rc_rc_filename                (void);
          SCM    g_rc_rc_config                  (void);

/* i_menu.c */
          int    i_menu_return_num               (void);
          SCM    i_menu_return_entry             (int index, char **menu_name);
          int    i_menu_add_entry                (char *new_menu, SCM menu_items);
         void    i_menu_print                    (void);
         void    i_menu_free                     (void);
         void    i_menu_init                     (void);

/* i_vars.c */
         void    i_vars_libgeda_set              (GedaToplevel *toplevel);
         void    i_vars_libgeda_freenames        (void);

/* libgeda.c */
         void    libgeda_init                    (int argc, char **argv);
         void    libgeda_release                 (void);

/* m_basic.c */
       double    m_degrees_to_radians            (double degrees);
       double    m_distance                      (int x1, int y1, int x2, int y2);
         void    m_papersize_to_world            (int width, int height, int border, int *right, int *bottom);
          int    m_random_number                 (int min_num, int max_num);
       double    m_radians_to_degrees            (double radians);
         void    m_rotate_point                  (int x, int y, int angle, int *newx, int *newy);
         void    m_rotate_point_90               (int x, int y, int angle, int *newx, int *newy);

/* m_box.c */
       double    m_box_shortest_distance         (Box *box, int x, int y, int solid);

/* m_bounds.c */
         void    m_bounds_init                   (BOUNDS *bounds);
         void    m_bounds_of_points              (BOUNDS *bounds, POINT points[], int count);

/* m_arc.c */
       double    m_arc_length                    (int radius, int sweep);
       bool      m_arc_includes_point            (Arc *arc, POINT *point);

/* m_circle.c */
       double    m_circle_circumference          (int radius);
       bool      m_circle_includes_point         (Circle *circle, POINT *point);
       double    m_circle_shortest_distance      (Circle *circle, int x, int y, int solid);

/* m_hatch.c */
         void    m_hatch_box                     (Box    *box,    int angle, int pitch, GArray *lines);
         void    m_hatch_circle                  (Circle *circle, int angle, int pitch, GArray *lines);
         void    m_hatch_path                    (Path   *path,   int angle, int pitch, GArray *lines);
         void    m_hatch_polygon                 (GArray *points, int angle, int pitch, GArray *lines);
       GArray   *m_hatch_object                  (Object *object);

/* m_line.c */
         bool    m_line_get_intersection         (Line *line1, Line *line2, POINT *point);
         bool    m_line_includes_point           (Line *line, POINT *point);
         bool    m_line_intersection             (LINE *line1, LINE *line2, POINT *point);
          int    m_line_length                   (int x1, int y1, int x2, int y2);
       double    m_line_shortest_distance        (Line *line, int x, int y);

/* m_polygon.c */
         bool    m_polygon_interior_point        (GArray *points, int x, int y);
       double    m_polygon_shortest_distance     (GArray *points, int x, int y, bool closed);
         void    m_polygon_append_bezier         (GArray *points, BEZIER *bezier, int segments);
         void    m_polygon_append_point          (GArray *points, int x, int y);

/* o_arc_basic.c */
       Object   *o_arc_new                       (int color, int x, int y, int radius, int start_angle, int arc_sweep);
       Object   *o_arc_copy                      (Object *o_current) GEDA_WARN_UNUSED_RESULT;
         void    o_arc_mirror                    (Object *object, int center_x, int center_y);
         void    o_arc_modify                    (Object *object, int x, int y, int whichone);
         void    o_arc_rotate                    (Object *object, int center_x, int center_y, int angle);
         void    o_arc_translate                 (Object *object, int dx, int dy);
         bool    o_arc_get_nearest_point         (Object *object, int x, int y, int *nx, int *ny);

/* o_attrib.c */
         void    o_attrib_add                              (Object *object, Object *item);
        GList   *o_attrib_get_attached_attribs             (const Object *object);
         bool    o_attrib_is_attached_to                   (const Object *attrib, const Object *object);
         void    o_attrib_attach                           (Object *object, Object *attrib, int set_color);
         void    o_attrib_attach_list                      (Object *object, const GList *attr_list, int set_color);
         void    o_attrib_detach                           (Object *object);
         void    o_attrib_detach_all                       (Object *object);
       Object   *o_attrib_new_attached                     (Object *object, const char *name, const char *value, int visibility, int show_name_value);
         void    o_attrib_print                            (const GList  *attributes);
         void    o_attrib_remove                           (GList **list, Object *remove);
         bool    o_attrib_string_get_name_value            (const char   *string, char **name_ptr,  char **value_ptr);
         bool    o_attrib_get_name_value                   (const Object *attrib, char **name_ptr,  char **value_ptr);
         void    o_attrib_set_value                        (const Object *attrib, const char *name_ptr, const char *value_ptr);
         void    o_attrib_set_integer_value                (const Object *attrib, const char *name_ptr, int value);
        GList   *o_attrib_find_floating_attribs            (const GList  *list);
       Object   *o_attrib_find_attrib_by_name              (const GList  *list,   const char *name, int count);
       Object   *o_attrib_first_attrib_by_name             (const Object *object,       char *name);
         char   *o_attrib_search_floating_attribs_by_name  (const GList  *list,   const char *name, int counter);
         char   *o_attrib_search_attached_attribs_by_name  (const Object *object, const char *name, int counter);
         char   *o_attrib_search_inherited_attribs_by_name (const Object *object, const char *name, int counter);
         char   *o_attrib_search_object_attribs_by_name    (const Object *object, const char *name, int counter);
        GList   *o_attrib_return_attribs                   (const Object *object);
          int    o_attrib_is_inherited                     (const Object *attrib);
         void    o_attrib_append_attribs_changed_hook      (Page *page, AttribsChangedFunc func, void *data);
         void    o_attrib_emit_attribs_changed             (Object *object);
         void    o_attrib_freeze_hooks                     (Object *object);
         void    o_attrib_thaw_hooks                       (Object *object);

/* o_basic.c */
        GList   *o_read_buffer                   (GedaToplevel *toplevel, GList *object_list, const char *buffer,
                                                  const int size, const char *name, GError **err);
        GList   *o_read                          (GedaToplevel *toplevel, GList *object_list, char *filename, GError **err);
         void    o_scale                         (GList *list, int x_scale, int y_scale);
       Object   *o_copy_object                   (Object *o_current) GEDA_WARN_UNUSED_RESULT;
         void    o_mirror_object                 (Object *object, int center_x, int center_y);
         void    o_rotate_object                 (Object *object, int center_x, int center_y, int angle);
         void    o_translate_object              (Object *object, int dx, int dy);

/* o_box_basic.c */
       Object   *o_box_new                       (int color, int x1, int y1, int x2, int y2);
       Object   *o_box_copy                      (Object *o_current) GEDA_WARN_UNUSED_RESULT;
         void    o_box_modify_all                (Object *object, int x1, int y1, int x2, int y2);
         void    o_box_modify                    (Object *object, int x, int y, int whichone);
         void    o_box_mirror                    (Object *object, int center_x, int center_y);
         void    o_box_rotate                    (Object *object, int center_x, int center_y, int angle);
         void    o_box_translate                 (Object *object, int dx, int dy);
         bool    o_box_get_nearest_point         (Object *object, int x, int y, int *nx, int *ny);

/* o_bus_basic.c */
       Object   *o_bus_new                       (int color, int x1, int y1, int x2, int y2, int bus_ripper_direction);
       Object   *o_bus_copy                      (Object *o_current) GEDA_WARN_UNUSED_RESULT;
         void    o_bus_modify                    (Object *object, int x, int y, int whichone);
         void    o_bus_mirror                    (Object *object, int center_x, int center_y);
         void    o_bus_rotate                    (Object *object, int center_x, int center_y, int angle);
         void    o_bus_translate                 (Object *object, int dx, int dy);
          int    o_bus_orientation               (Object *object);
          int    o_bus_get_direction             (Object *object);
         void    o_bus_consolidate               (void);

/* o_circle_basic.c */
       Object   *o_circle_new                    (int color, int x, int y, int radius);
       Object   *o_circle_copy                   (Object *o_current);
         void    o_circle_modify                 (Object *object, int x, int y, int whichone);
         void    o_circle_mirror                 (Object *object, int center_x, int center_y);
         void    o_circle_rotate                 (Object *object, int center_x, int center_y, int angle);
         void    o_circle_translate              (Object *object, int dx, int dy);
         bool    o_circle_get_nearest_point      (Object *object, int x, int y, int *nx, int *ny);

/* o_complex_basic.c */
       Object   *o_complex_new                   (GedaToplevel *toplevel, int x, int y, int angle, int mirror,
                                                  const CLibSymbol *clib_sym, const char *basename, int selectable);
       Object   *o_complex_new_embedded          (int x, int y, int angle, int mirror, const char *basename, int selectable);
       Object   *o_complex_copy                  (Object *o_current);
         void    o_complex_mirror                (Object *object, int center_x, int center_y);
        GList   *o_complex_promote_attribs       (GedaToplevel *toplevel, Object *object);
         void    o_complex_reset_refdes          (Object *object);
         void    o_complex_rotate                (Object *object, int center_x, int center_y, int angle);
         void    o_complex_translate             (Object *object, int dx, int dy);
          int    o_complex_is_embedded           (Object *o_current);
         bool    o_complex_get_nearest_point     (Object *object, int x, int y, int *nx, int *ny);
       Object   *o_complex_find_pin_by_attribute (Object *object, char *name, char *wanted_value);
         void    o_complex_check_symbol_version  (GedaToplevel *toplevel, Object *object);

/* o_color.c */
          int    o_color_get_object_default      (char type);

/* o_embed.c */
         bool    o_embed                         (GedaToplevel *toplevel, Object *object);
         void    o_unembed                       (GedaToplevel *toplevel, Object *object);

/* o_get.c */
          int    o_get_attached_parent_id        (Object *object);
         bool    o_get_fill_options              (Object *object, OBJECT_FILLING *type, int *width, int *pitch1,
                                                  int *angle1, int *pitch2, int *angle2);
         bool    o_get_has_slope                 (Object *object);
         bool    o_get_is_attached               (Object *object);
         bool    o_get_is_bus_related            (Object *object);
         bool    o_get_is_embedded               (Object *object);
         bool    o_get_is_inside_region          (int xmin, int ymin, int xmax, int ymax, int x, int y);
         bool    o_get_is_selectable             (Object *object);
         bool    o_get_is_selected               (Object *object);
         bool    o_get_is_valid_attribute        (Object *object);
         bool    o_get_is_visible                (Object *object);
     LINE_END    o_get_line_end                  (int capstyle);
         bool    o_get_line_options              (Object *object, LINE_END *end, LINE_TYPE *type, int *width, int *length, int *space);
         bool    o_get_nearest_point             (Object *object, int x, int y, int *nx, int *ny);
          int    o_get_num_text_lines            (const char *string);
   const char   *o_get_object_attrib_value       (Object *object, const char *name);
        GList   *o_get_objects_by_type           (GList  *object_list, int type);
         Page   *o_get_page                      (Object *object);
       Object   *o_get_parent                    (Object *object);
          int    o_get_parent_id                 (Object *object);
         bool    o_get_position                  (Object *object, int *x, int *y);
       double    o_get_shortest_distance         (Object *object, int x, int y);
       double    o_get_shortest_distance_full    (Object *object, int x, int y, int force_solid);
          int    o_get_bounds                    (Object *o_current, int *left, int *top, int *right, int *bottom);
          int    o_get_bounds_list               (const GList *o_list, int *left, int *top, int *right, int *bottom);

/* o_line_basic.c */
       Object   *o_line_new                      (int color, int x1, int y1, int x2, int y2) GEDA_WARN_UNUSED_RESULT;
       Object   *o_line_copy                     (Object *object) GEDA_WARN_UNUSED_RESULT;
         void    o_line_modify                   (Object *object, int x, int y, int whichone);
         void    o_line_mirror                   (Object *object, int center_x, int center_y);
         void    o_line_rotate                   (Object *object, int center_x, int center_y, int angle);
         void    o_line_translate                (Object *object, int dx, int dy);
         void    o_line_scale                    (Object *object, int x_scale, int y_scale);
         bool    o_line_is_endpoint              (Object *object, POINT *point);
         int     o_line_get_closest_endpoint     (Object *object, int x, int y);
         bool    o_line_get_intersection         (Object *object1, Object *object2, POINT *point);
         bool    o_line_get_midpoint             (Object *object, POINT *point);
         bool    o_line_get_nearest_point        (Object *object, int x, int y, int *nx, int *ny);
         bool    o_line_get_slope                (Object *object, double *anwser);
       double    o_line_length                   (Object *object);

/* o_list.c */
        GList   *o_list_copy_all                 (const GList *src_list, GList *dest_list);
         void    o_list_mirror                   (const GList *list, int x, int y);
         void    o_list_rotate                   (const GList *list, int x, int y, int angle);
         void    o_list_translate                (const GList *list, int dx, int dy);
         void    o_list_set_color                (const GList *list, int color);

/* o_net_basic.c */
       Object   *o_net_new                       (int color, int x1, int y1, int x2, int y2);
       Object   *o_net_copy                      (Object *o_current);
         void    o_net_modify                    (Object *object, int x, int y, int whichone);
         void    o_net_mirror                    (Object *object, int center_x, int center_y);
         void    o_net_rotate                    (Object *object, int center_x, int center_y, int angle);
         void    o_net_translate                 (Object *object, int dx, int dy);
          int    o_net_orientation               (Object *object);
         void    o_net_consolidate               (GedaToplevel *toplevel, Page *page);
         void    o_net_refresh_conn_cache        (Object *object);
         bool    o_net_is_fully_connected        (Object *object);

/* o_notify.c */
         void    o_notify_change_add             (Page *page, ChangeNotifyFunc pre_change_func,
                                                  ChangeNotifyFunc change_func, void *user_data);
         void    o_notify_change_remove          (Page *page, ChangeNotifyFunc pre_change_func,
                                                  ChangeNotifyFunc change_func, void *user_data);
         void    o_notify_change_remove_all      (Page *page);

/* o_path_basic.c */
       Object   *o_path_new                      (int color, const char *path_string);
       Object   *o_path_new_from_polygon         (GArray *points,  int color);
       Object   *o_path_new_take_path            (int color, Path *path_data);
       Object   *o_path_copy                     (Object *o_current);
         void    o_path_modify                   (Object *object, int x, int y, int whichone);
         void    o_path_mirror                   (Object *object, int center_x, int center_y);
         void    o_path_rotate                   (Object *object, int center_x, int center_y, int angle);
         void    o_path_translate                (Object *object, int x, int y);
         bool    o_path_get_nearest_point        (Object *object, int x, int y, int *nx, int *ny);

/* o_picture.c */
       Object   *o_picture_new                   (const char *file_content, unsigned int file_length,
                                                  const char *filename, int x1, int y1, int x2, int y2, int angle, int mirrored,
                                                  int embedded)      GEDA_WARN_UNUSED_RESULT;
       Object   *o_picture_copy                  (Object *o_current) GEDA_WARN_UNUSED_RESULT;
         void    o_picture_modify                (Object *object, int x, int y, int whichone);
         void    o_picture_modify_all            (Object *object, int x1, int y1, int x2, int y2);
         void    o_picture_mirror                (Object *object, int center_x, int center_y);
         void    o_picture_rotate                (Object *object, int center_x, int center_y, int angle);
         void    o_picture_translate             (Object *object, int dx, int dy);

         bool    o_picture_export_object         (Object *o_current, const char *filename, const char *type, ...);
         bool    o_picture_export_orginal        (Object *o_current, const char *filename, const char *type, ...);
         bool    o_picture_is_embedded           (Object *object);
   const char   *o_picture_get_data              (Object *object, size_t *length);
   const char   *o_picture_get_filename          (Object *object);
          int    o_picture_get_height            (Object *object);
          int    o_picture_get_width             (Object *object);
       double    o_picture_get_effective_ratio   (Object *object);
         bool    o_picture_get_nearest_point     (Object *object, int x, int y, int *nx, int *ny);
         void    o_picture_print                 (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
         bool    o_picture_set_from_buffer       (Object *object, const char *filename, const char *data, unsigned int length, GError **error);
         bool    o_picture_set_from_file         (Object *object, const char *filename, GError **error);

#ifdef GDK_PIXBUF_H
         bool    o_picture_export_pixbuf         (GdkPixbuf *pixbuf, const char *filename, const char *type, ...);
    GdkPixbuf   *o_picture_get_fallback_pixbuf   (void)           GEDA_WARN_UNUSED_RESULT;
    GdkPixbuf   *o_picture_get_pixbuf            (Object *object) GEDA_WARN_UNUSED_RESULT;
    GdkPixbuf   *o_picture_get_pixbuf_fit        (Object *object, int interpolate) GEDA_WARN_UNUSED_RESULT;
unsigned char   *o_picture_get_rgb_data          (Object *object) GEDA_WARN_UNUSED_RESULT;
        uint8   *o_picture_get_mask_data         (Object *object) GEDA_WARN_UNUSED_RESULT;
#endif

/* o_pin_basic.c */
       Object   *o_pin_new                       (int color, int x1, int y1, int x2, int y2, PIN_NODE node_type, int whichend);
       Object   *o_pin_copy                      (Object *o_current) GEDA_WARN_UNUSED_RESULT;
         void    o_pin_mirror                    (Object *object, int center_x, int center_y);
         void    o_pin_modify                    (Object *object, int x, int y, int whichone);
         void    o_pin_normalize                 (Object *object);
         void    o_pin_rotate                    (Object *object, int center_x, int center_y, int angle);
         void    o_pin_translate                 (Object *object, int dx, int dy);
         void    o_pin_update_whichend           (GList *object_list, int num_pins);
         bool    o_pin_set_elect_type            (Object *o_current, PIN_ELECT e_type);
         bool    o_pin_set_mech_type             (Object *o_current, PIN_MECH m_type);
         void    o_pin_set_node_type             (Object *o_current, PIN_NODE node_type);
         bool    o_pin_get_attributes            (Object *object, const char **label, const char **number, int *sequence,
                                                  PIN_ELECT *e_type, PIN_MECH *m_type, PIN_NODE *type);
         void    o_pin_set_attributes            (Object *object, const char *label_str, const char *number, int sequence,
                                                  PIN_ELECT e_type, PIN_MECH m_type, PIN_NODE type);
       Object   *o_pin_create_elect_attrib       (GedaToplevel *toplevel, Object *object, const char *descr, int x, int y);
       Object   *o_pin_create_label_attrib       (GedaToplevel *toplevel, Object *object, const char *label, int x, int y);
       Object   *o_pin_create_mech_attrib        (GedaToplevel *toplevel, Object *object, const char *descr, int x, int y);
       Object   *o_pin_create_number_attrib      (GedaToplevel *toplevel, Object *object, const char *number, int x, int y);
       Object   *o_pin_create_seq_attrib         (GedaToplevel *toplevel, Object *object, int sequence, int x, int y);
        GList   *o_pin_realize_attributes        (GedaToplevel *toplevel, Object *object);
   const char   *o_pin_get_electrical            (Object *object);
   const char   *o_pin_get_label                 (Object *object);
   const char   *o_pin_get_mechanical            (Object *object);

/* o_save.c */
         void    o_save_auto_backup              (GedaToplevel *toplevel);
         char   *o_save_objects                  (const GList *object_list, bool save_attribs);
         char   *o_save_buffer                   (const GList *object_list);
          int    o_save                          (const GList *object_list, const char *filename, GError **err);

/* o_selection.c */
    SELECTION   *o_selection_new                 (void);
         void    o_selection_add                 (SELECTION *selection, Object *o_selected);
       Object   *o_selection_get_first_object    (SELECTION *selection);
         void    o_selection_print_all           (const SELECTION *selection);
          int    o_selection_remove              (SELECTION *selection, Object *o_selected);
          int    o_selection_select              (Object *object);
          int    o_selection_unselect            (Object *object);
          int    o_selection_unselect_all        (SELECTION *selection);

/* o_set.c */
         void    o_set_bounds_invalid            (Object *object);
         void    o_set_color                     (Object *object, int color);
         void    o_set_fill_options              (Object *o_current, FILL_OPTIONS *fill_options);
         void    o_set_line_options              (Object *o_current, LINE_OPTIONS *line_options);
         void    o_set_visibility                (Object *object, int visibility);

/* o_style.c */
          int    o_style_get_bus_width           (GedaToplevel *toplevel);
          int    o_style_get_line_width          (GedaToplevel *toplevel);
          int    o_style_get_net_width           (GedaToplevel *toplevel);
          int    o_style_get_pin_width           (GedaToplevel *toplevel, int type);
         void    o_style_set_object              (GedaToplevel *toplevel, Object *o_current);

/* o_text_basic.c */
       Object   *o_text_new                      (int color, int x, int y, int alignment, int angle,
                                                  int size, int visibility, int show_name_value, const char *string);
         void    o_text_recreate                 (Object *o_current);
         void    o_text_mirror                   (Object *object, int center_x, int center_y);
       Object   *o_text_copy                     (Object *object) GEDA_WARN_UNUSED_RESULT;
         void    o_text_translate                (Object *object, int dx, int dy);
         void    o_text_rotate                   (Object *object, int center_x, int center_y, int angle);

       double    o_text_get_font_size_in_points  (Object *object);
         bool    o_text_get_nearest_point        (Object *object, int x, int y, int *nx, int *ny);
   const char   *o_text_get_string               (Object *object);
         void    o_text_set_rendered_bounds_func (Object *object, RenderedBoundsFunc func, void *user_data);
         void    o_text_set_string               (Object *object, const char *new_string);

/* s_attrib.c */
          int    s_attrib_add_entry              (char *new_attrib);
          int    s_attrib_count                  (void);
         void    s_attrib_print                  (void);
          int    s_attrib_uniq                   (char *name);
         void    s_attrib_init                   (void);
         char   *s_attrib_get                    (int counter);

/* s_basic.c */
         void    print_struct_forw               (GList *list);
         void    print_struct                    (Object *ptr);

/* s_clib.c */
         void     s_clib_flush_cache             (void);
         void     s_clib_refresh                 (void);
        GList    *s_clib_get_sources             (const bool sorted);
         bool     s_clib_source_name_exist       (const char *name);
         bool     s_clib_source_path_exist       (const char *path);
const CLibSource *s_clib_get_source_by_name      (const char *name);
const CLibSource *s_clib_add_directory           (const char *directory, const char *name);
const CLibSource *s_clib_add_command             (const char *list_cmd,  const char *get_cmd, const char *name);
const CLibSource *s_clib_add_scm                 (SCM         listfunc,  SCM         getfunc, const char *name);

   const char    *s_clib_source_get_name         (const CLibSource *source);
        GList    *s_clib_source_get_symbols      (const CLibSource *source);
   const char    *s_clib_symbol_get_name         (const CLibSymbol *symbol);
         char    *s_clib_symbol_get_filename     (const CLibSymbol *symbol);
const CLibSource *s_clib_symbol_get_source       (const CLibSymbol *symbol);
         char    *s_clib_symbol_get_data         (const CLibSymbol *symbol);
        GList    *s_clib_search                  (const char *pattern, const CLibSearchMode mode);

         void     s_clib_symbol_invalidate_data  (const CLibSymbol *symbol);
const CLibSymbol *s_clib_get_symbol_by_name      (const char *name);
         char    *s_clib_symbol_get_data_by_name (const char *name);

/* s_color.c */
         char   *s_color_get_colorname           (int index, GArray *map, GError **err);
       GArray   *s_color_get_standard_names      (void);
       GArray   *s_color_get_print_color_map     (void);
         bool    s_color_load_scheme             (const char *scheme);
         void    s_color_map_defaults            (COLOR *map);

/* s_conn.c */
         void    s_conn_remove_object            (Object *to_remove);
         void    s_conn_update_linear_object     (Object *object);
         void    s_conn_update_object            (Object *object);
         void    s_conn_print                    (GList  *conn_list);
          int    s_conn_net_search               (Object *new_net,    int     whichone, GList *conn_list);
        GList   *s_conn_return_others            (GList  *input_list, Object *object);

         void    s_conn_append_conns_changed_hook(Page *page,   ConnsChangedFunc func, void *data);
         void    s_conn_emit_conns_changed       (Object *object);
         void    s_conn_freeze_hooks             (Object *object);
         void    s_conn_thaw_hooks               (Object *object);

/* s_cue.c */
         void    s_cue_get_locations             (const GList *objects, GArray *junctions, GArray *unconnected);
         void    s_cue_output_all                (GedaToplevel *toplevel, const GList *obj_list, FILE *fp, int type);
         void    s_cue_output_single             (GedaToplevel *toplevel, Object *object, FILE *fp, int type);

/* s_hierarchy.c */
         Page   *s_hierarchy_down_schematic_single  (GedaToplevel *toplevel, const char *filename, Page *parent,
                                                     int page_control, int flag, GError **err);
         Page   *s_hierarchy_down_symbol         (GedaToplevel *toplevel, const CLibSymbol *symbol, Page *parent);
         Page   *s_hierarchy_find_up_page        (PageList *page_list, Page *current_page);
        GList   *s_hierarchy_traverse_pages      (GedaToplevel *toplevel, Page *p_current, int flags);
          int    s_hierarchy_print_page          (Page *p_current, void *data);
         Page   *s_hierarchy_find_prev_page      (PageList *page_list, Page *current_page);
         Page   *s_hierarchy_find_next_page      (PageList *page_list, Page *current_page);

/*  s_object.c */
        Object  *s_object_new                    (int type, char const *name);
         void    s_object_add_child              (Object *parent, Object *child);
         void    s_object_release                (Object *object);
         void    s_object_release_objects        (GList *list);
         void    s_object_set_page_changed       (Object *obj);

/* s_page.c */
         Page   *s_page_new                      (GedaToplevel *toplevel, const char *filename);
         Page   *s_page_new_with_notify          (GedaToplevel *toplevel, const char *filename);

         void    s_page_autosave_init            (GedaToplevel *toplevel);
          int    s_page_autosave                 (GedaToplevel *toplevel);

         bool    s_page_check_changed            (PageList *list);
         void    s_page_clear_changed            (PageList *list);

         void    s_page_delete                   (GedaToplevel *toplevel, Page *page, int previous);
         void    s_page_delete_list              (GedaToplevel *toplevel);
         Page   *s_page_get_current              (GedaToplevel *toplevel);
         bool    s_page_set_current              (GedaToplevel *toplevel, Page *page);
   const char   *s_page_get_file_extension       (Page *page);
         bool    s_page_goto                     (Page *page);
         bool    s_page_is_symbol_file           (Page *page);
         void    s_page_print_all                (GedaToplevel *toplevel);
         void    s_page_resequence_by_ids        (GedaToplevel *toplevel);
          int    s_page_save_all                 (GedaToplevel *toplevel);
          int    s_page_save_all_changed         (GedaToplevel *toplevel);
         Page   *s_page_search                   (GedaToplevel *toplevel, const char *filename);
         Page   *s_page_search_by_page_id        (PageList *list, int pid);
         void    s_page_set_bounds_func          (Page *page, RenderedBoundsFunc func, void *user_data);

         void    s_page_append_object            (Page *page, Object *object);
         void    s_page_append_list              (Page *page, GList *obj_list);

         void    s_page_remove_object            (Page *page, Object *object);
         void    s_page_replace_object           (Page *page, Object *object1, Object *object2);
         void    s_page_delete_objects           (Page *page);
       Object   *s_page_get_object               (Page *page, int sid);
        GList   *s_page_get_objects              (Page *page);
        GList   *s_page_objects_in_region        (Page *page, int min_x, int min_y, int max_x, int max_y);
        GList   *s_page_objects_in_regions       (Page *page, RECTANGLE *rects, int n_rects);

/* s_papersizes.c */
          int    s_papersizes_add_entry          (char *new_papersize, int width, int height);
         void    s_papersizes_print              (void);
          int    s_papersizes_uniq               (char *name);
         void    s_papersizes_init               (void);
         char   *s_papersizes_get                (int counter);
         void    s_papersizes_get_size           (char *string, int *width, int *height);

/* s_path.c */
         Path   *s_path_copy_modify              (Path *path, int dx, int dy, int new_x, int new_y, int whichone);
         Path   *s_path_parse                    (const char *path_str);
         char   *s_path_string_from_path         (const Path *path);
          int    s_path_to_polygon               (Path *path, GArray *points);
       double    s_path_shortest_distance        (Path *path, int x, int y, int solid);

/* s_place.c */
         void    s_place_free_place_list         (GedaToplevel *toplevel);
         GList  *s_place_get_place_list          (GedaToplevel *toplevel);
         void    s_place_set_place_list          (GedaToplevel *toplevel, GList *new_place_list );

/* s_slib.c */
          int    s_slib_add_entry                (const char *new_path);
         char   *s_slib_get_basename             (const char *rawname);
         char   *s_slib_get_dir                  (int index);
         void    s_slib_init                     (void);
          int    s_slib_search_for_dirname       (const char *dir_name);
         char   *s_slib_search_for_file          (const char *filename)GEDA_WARN_UNUSED_RESULT;
         char   *s_slib_search_dirs              (const char *basename)GEDA_WARN_UNUSED_RESULT;
         void    s_slib_print                    (void);
         void    s_slib_print_dirs               (void);
          int    s_slib_unique_dir_exist         (const char *path);

/* s_slot.c */
         char   *s_slot_search_slot              (Object *object, Object **return_found);
         void    s_slot_update_object            (Object *object);

/* s_tile.c */
         void    s_tile_update_object            (Object *object);
         GList  *s_tile_get_objectlists          (Page   *p_current, int world_x1, int world_y1, int world_x2, int world_y2);

/* s_toplevel.c */
        GList   *s_toplevel_get_symbols                  (const GedaToplevel *toplevel);
         void    s_toplevel_release                      (GedaToplevel *toplevel);
         void    s_toplevel_set_backup_loader_query_func (GedaToplevel *toplevel, void *func, ...);
         void    s_toplevel_set_rendered_bounds_func     (GedaToplevel *toplevel, RenderedBoundsFunc func, void *user_data);

/* s_undo.c */
         UNDO   *s_undo_return_tail              (UNDO *head);
         UNDO   *s_undo_return_head              (UNDO *tail);
         UNDO   *s_undo_new_head                 (void);
         void    s_undo_destroy_head             (UNDO *u_head);
         UNDO   *s_undo_add_disk                 (int type, char *filename, Page *page);
         UNDO   *s_undo_add_memory               (int type, Page *page);
         UNDO   *s_undo_add                      (UNDO *head,  int type,  char *filename, GList *object_list, int left, int top,
                                                  int right, int bottom, int  page_control, int up);
         void    s_undo_print_all                (UNDO *head);
         void    s_undo_destroy_all              (UNDO *head);
         void    s_undo_remove                   (UNDO *head, UNDO *u_tos);
         void    s_undo_remove_rest              (UNDO *head);
          int    s_undo_levels                   (UNDO *head);
         void    s_undo_update_modified          (Page *p_current);
         void    s_undo_init                     (Page *p_current);
         void    s_undo_free_all                 (Page *p_current);

/* ---------------- Utilities -------------- */

/* u_basic.c */
         char   *u_expand_env_variable           (const char *string);
         void    u_print_object                  (Object *object);

/* u_color.c */
         bool    u_color_rgba_decode             (const char *rgba, guchar *r, guchar *g, guchar *b, guchar *a);
         char   *u_color_rgba_encode             (uint8 r, uint8 g, uint8 b, uint8 a);
         char   *u_color_get_hex                 (COLOR *c);
         char   *u_color_lookup_colorname        (COLOR *c1, GError **err);

/* u_glist.c */
        GList   *u_glist_clear                   (GList* list);
          int    u_glist_find_string             (GList* list, char *str);
         void    u_glist_free_full               (GList* list, GDestroyNotify free_func);
         void    u_glist_free_strings            (void *data);

       GSList   *u_gslist_clear                  (GSList* list);
         int     u_gslist_find_string            (GSList* list, char *str);
         void    u_gslist_free_full              (GSList* list, GDestroyNotify free_func);
         void    u_gslist_free_strings           (void *data);

/* u_log.c */
         void    u_log_init                      (const char *app_prefix);
         void    u_log_close                     (void);
         char   *u_log_read                      (void);
          int    u_log_get_log_time              (void);
         void    u_log_set_log_time              (int mode);
         void    u_log_set_update_func           (LogUpdateFunc func);
         void    u_log_qmessage                  (const char *format, ...);
         void    u_log_vmessage                  (const char *format, ...);

/* u_string.c */
         char   *u_string_concat                 (const char *string1, ...)  GEDA_WARN_UNUSED_RESULT;
   const char   *u_string_istr                   (const char *str1, const char *str2);
         char   *u_string_remove_nl              (char *string);
         char   *u_string_remove_last_nl         (char *string);
         char   *u_string_int2str                (int value, char *str, int radix);
         bool    u_string_isalnum                (const char *string);
         bool    u_string_parse_xy               (const char *string, int *x, int *y) GEDA_WARN_UNUSED_RESULT;
         char   *u_string_scm2c                  (char *scm_str_name) GEDA_WARN_UNUSED_RESULT;
         void    u_string_sort_array             (char *strings[], size_t strings_size);
         char   *u_string_sprintf                (const char *format, ...)   GEDA_WARN_UNUSED_RESULT;
         char   *u_string_strdup                 (const char *str)           GEDA_WARN_UNUSED_RESULT;
         char   *u_string_strndup                (const char *str, size_t n) GEDA_WARN_UNUSED_RESULT;
         int     u_string_stristr                (const char *haystack, const char *needle);
         bool    u_string_strequal               (const char *str1, const char *str2) GEDA_WARN_UNUSED_RESULT;
         int     u_string_strsize                (const char *format, va_list args);
         char   *u_string_strstr_rep             (char *original,   const char *old_str, const char *new_str);
         int     u_string_stricmp                (const char *str1, const char *str2);
         int     u_string_strncmpi               (const char *str1, const char *str2, int n);
         char   *u_string_strsubst               (char *source, char *old_str, char *new_str);
         char   *u_string_strisubst              (char *source, char *old_str, char *new_str);
         char   *u_string_split                  (char *string, char delimiter, int count) GEDA_WARN_UNUSED_RESULT;
         int     u_string_word_count             (char *str);

/* u_program.c */
         void    u_program_backtrace             (void);
         void    u_program_mem_set_vtable        (void);

/* u_refdes.c */
const GedaRefDes *u_refdes_get_standard_designators (void);
const GedaRefDes *u_refdes_get_spice_designators    (void);
const GedaRefDes *u_refdes_get_ieee_designators     (void);
         void     u_refdes_reset                    (Object *object);
         char    *u_refdes_return_numeric           (void *text);

END_DECLS
