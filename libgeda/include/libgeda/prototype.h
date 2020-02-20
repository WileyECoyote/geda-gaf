/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */

#ifdef __cplusplus
extern "C" {
#endif

/* f_file.c */
         bool    geda_file_has_active_autosave   (const char   *filename, GError **err);
          int    geda_file_open                  (GedaToplevel *toplevel, Page *page, const char *filename, GError **err);
          int    geda_file_open_flags            (GedaToplevel *toplevel);
         void    geda_file_close                 (GedaToplevel *toplevel);
         void    geda_file_remove_backup         (const char *filename);
         bool    geda_file_save                  (GedaToplevel *toplevel, Page *page, const char *filename, GError **error);

/* f_get.c */
         char   *geda_file_get_autosave_filename (const char *filename);
   const char   *geda_file_get_basename          (const char *path);
         char   *geda_file_get_basename_dup      (const char *path);
         char   *geda_file_get_bitmap_filespec   (const char *filename);
         bool    geda_file_get_contents          (const char *filename, char **contents, size_t *length, GError **err);
         char   *geda_file_get_data_filespec     (const char *filename);
       GSList   *geda_file_get_dir_list_files    (      char *path, char *filter,  GError **err);
   const char   *geda_file_get_filename_ext      (const char *filename);
   const char   *geda_file_get_format_header     (void);
         char   *geda_file_get_name              (const char *filename);
         bool    geda_file_get_is_path_absolute  (const char *filename);

/* f_path.c */
          int    geda_file_path_create           (const char *path, mode_t mode);
         void    geda_file_path_free             (void);
         char   *geda_file_path_get_dirname      (const char *filename);
   const char   *geda_file_path_sys_data         (void);
   const char   *geda_file_path_sys_doc          (void);
   const char   *geda_file_path_sys_config       (void);
   const char   *geda_file_path_user_config      (void);
   const char   *geda_file_path_user_cache       (void);

/* f_print.c */
          int    geda_file_print_command         (GedaToplevel *toplevel, Page *page, GArray *color_map, const char *command);
          int    geda_file_print_file            (GedaToplevel *toplevel, Page *page, GArray *color_map, const char *filename);
          int    geda_file_print_stream          (GedaToplevel *toplevel, Page *page, FILE *fp);
         void    geda_file_print_set_type        (GedaToplevel *toplevel, int type);

/* f_sys.c */
          int    geda_file_copy                  (const char *source, const char *target);
          int    geda_file_sys_ckmod             (const char *pathname, mode_t mode);
          int    geda_file_sys_cmp_mod_time      (const char *filename, time_t ref_time);
         char   *geda_file_sys_follow_symlinks   (const char *filename, GError **error);
         char   *geda_file_sys_normalize_name    (const char *filename, GError **error);
          int    geda_file_sys_remove            (const char *pathname);
         bool    geda_file_sys_remove_extension  (      char *filename);

/* g_evaluate.c */
          SCM    g_evaluate_scm_action           (SCM action);
          SCM    g_evaluate_scm_protected        (SCM exp, SCM module_or_state);
          SCM    g_evaluate_scm_string_protected (SCM str);
          SCM    g_evaluate_c_string_protected   (const char *str);
         bool    g_evaluate_scheme_file          (const char *filename, GError **err);

/* g_rc_parse.c */
          SCM    g_rc_parse_mode                 (SCM scmmode, const char *rc_name, int *mode_var, const vstbl_entry *table, int table_size);
         bool    g_rc_parse_file                 (const char *rcfile, EdaConfig *cfg, GError **err);
         bool    g_rc_parse_system               (const char *rcname, GError **err);
         bool    g_rc_parse_user                 (const char *rcname, GError **err);
         bool    g_rc_parse_local                (const char *rcname, const char *path,    GError       **err);
         bool    g_rc_parse                      (const char *pname,  const char *rcname,  const char    *rcfile);
         void    g_rc_parse_gafrc_handler        (const char *dummy,  ConfigParseErrorFunc handler, void *user_data);
         void    g_rc_parse_rcname_handler       (const char *rcname, ConfigParseErrorFunc handler, void *user_data);
         void    g_rc_parse_rcfile_handler       (const char *rcfile, ConfigParseErrorFunc handler, void *user_data);
         void    g_rc_parse_handler              (const char *rcname, const char *rcfile, ConfigParseErrorFunc handler, void *user_data);
          SCM    g_rc_parse_rc_filename          (void);
          SCM    g_rc_parse_rc_config            (void);

/* i_menu.c */
          int    geda_iface_menu_return_num      (void);
          SCM    geda_iface_menu_return_entry    (int index, char **menu_name);
          int    geda_iface_menu_add_entry       (char *new_menu, SCM menu_items);
         void    geda_iface_menu_print           (void);
         void    geda_iface_menu_free            (void);
         void    geda_iface_menu_init            (void);

/* i_vars.c */
         void    geda_iface_vars_set             (GedaToplevel *toplevel);
         void    geda_iface_vars_freenames       (void);

/* libgeda.c */
         void    libgeda_init                    (int argc, char **argv);
         void    libgeda_release                 (void);

/* m_math.c */
       double    geda_math_degrees_to_radians            (double degrees);
       double    geda_math_distance                      (int x1, int y1, int x2, int y2);
         void    geda_math_papersize_to_world            (int width, int height, int border, int *right, int *bottom);
          int    geda_math_random_number                 (int min_num, int max_num);
       double    geda_math_radians_to_degrees            (double radians);
         void    geda_math_rotate_point                  (int x, int y, int angle, int *newx, int *newy);
         void    geda_math_rotate_point_90               (int x, int y, int angle, int *newx, int *newy);

/* m_box.c */
         int     geda_math_box_area                      (GedaBox *area);
       double    geda_math_box_shortest_distance         (GedaBox *box, int x, int y, int solid);

/* m_bounds.c */
         void    geda_math_bounds_init                   (BOUNDS *bounds);
         void    geda_math_bounds_of_points              (BOUNDS *bounds, GedaPoint points[], int count);

/* m_angle.c */
         bool    geda_math_angle_is_normal               (int angle);
         bool    geda_math_angle_is_ortho                (int angle);
          int    geda_math_angle_make_ortho              (int angle);
          int    geda_math_angle_normalize               (int angle);

/* m_arc.c */
         void    geda_math_arc_chord                     (GedaArc *arc, LINE *line);
       double    geda_math_arc_length                    (GedaArc *arc);
         bool    geda_math_arc_includes_point            (GedaArc *arc, GedaPoint *point);
       double    geda_math_arc_shortest_distance         (GedaArc *arc, int x, int y, int solid);

/* m_circle.c */
       double    geda_math_circle_circumference          (int radius);
         bool    geda_math_circle_includes_point         (GedaCircle *circle, GedaPoint *point);
       double    geda_math_circle_shortest_distance      (GedaCircle *circle, int x, int y, int solid);

/* m_hatch.c */
         void    geda_math_hatch_box                     (GedaBox    *box,    int angle, int pitch, GArray *lines);
         void    geda_math_hatch_circle                  (GedaCircle *circle, int angle, int pitch, GArray *lines);
         void    geda_math_hatch_path                    (GedaPath   *path,   int angle, int pitch, GArray *lines);
         void    geda_math_hatch_polygon                 (GArray     *points, int angle, int pitch, GArray *lines);
       GArray   *geda_math_hatch_object                  (GedaObject *object);

/* m_line.c */
         bool    geda_math_line_get_intersection         (GedaLine *line1, GedaLine *line2, GedaPoint *point);
         bool    geda_math_line_includes_point           (GedaLine *line, GedaPoint *point);
         bool    geda_math_line_intersection             (LINE *line1, LINE *line2, GedaPoint *point);
          int    geda_math_line_length                   (int x1, int y1, int x2, int y2);
       double    geda_math_line_shortest_distance        (GedaLine *line, int x, int y);

/* m_polygon.c */
         bool    geda_math_polygon_interior_point        (GArray *points, int x, int y);
       double    geda_math_polygon_shortest_distance     (GArray *points, int x, int y, bool closed);
         void    geda_math_polygon_append_bezier         (GArray *points, BEZIER *bezier, int segments);
         void    geda_math_polygon_append_point          (GArray *points, int x, int y);

/* o_arc_object.c */
   GedaObject   *geda_arc_object_copy                 (GedaObject *o_current) WARN_UNUSED;
          int    geda_arc_object_get_arc_sweep        (const GedaObject *object);
          int    geda_arc_object_get_center_x         (const GedaObject *object);
          int    geda_arc_object_get_center_y         (const GedaObject *object);
          int    geda_arc_object_get_end_cap          (const GedaObject *object);
          int    geda_arc_object_get_fill_angle1      (const GedaObject *object);
          int    geda_arc_object_get_fill_angle2      (const GedaObject *object);
          int    geda_arc_object_get_fill_pitch1      (const GedaObject *object);
          int    geda_arc_object_get_fill_pitch2      (const GedaObject *object);
          int    geda_arc_object_get_fill_type        (const GedaObject *object);
          int    geda_arc_object_get_fill_width       (const GedaObject *object);
          int    geda_arc_object_get_line_length      (const GedaObject *object);
          int    geda_arc_object_get_line_space       (const GedaObject *object);
          int    geda_arc_object_get_line_type        (const GedaObject *object);
          int    geda_arc_object_get_line_width       (const GedaObject *object);
         bool    geda_arc_object_get_nearest_point    (const GedaObject *object, int x, int y, int *nx, int *ny);

          int    geda_arc_object_get_radius           (const GedaObject *object);
          int    geda_arc_object_get_start_angle      (const GedaObject *object);
         void    geda_arc_object_mirror               (GedaObject *object, int center_x, int center_y);
         void    geda_arc_object_modify               (GedaObject *object, int x, int y, int whichone);
   GedaObject   *geda_arc_object_new                  (int color, int x, int y, int radius, int start_angle, int arc_sweep);

         void    geda_arc_object_rotate               (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_arc_object_scale                (GedaObject *object, int scale);
         void    geda_arc_object_set_arc_sweep        (GedaObject *object, int sweep);
         void    geda_arc_object_set_center_x         (GedaObject *object, int x);
         void    geda_arc_object_set_center_y         (GedaObject *object, int y);
         void    geda_arc_object_set_end_cap          (GedaObject *object, int cap);
         void    geda_arc_object_set_fill_angle1      (GedaObject *object, int angle);
         void    geda_arc_object_set_fill_angle2      (GedaObject *object, int angle);
         void    geda_arc_object_set_fill_pitch1      (GedaObject *object, int pitch);
         void    geda_arc_object_set_fill_pitch2      (GedaObject *object, int pitch);
         void    geda_arc_object_set_fill_type        (GedaObject *object, int type);
         void    geda_arc_object_set_fill_width       (GedaObject *object, int width);
         void    geda_arc_object_set_line_length      (GedaObject *object, int length);
         void    geda_arc_object_set_line_space       (GedaObject *object, int space);
         void    geda_arc_object_set_line_type        (GedaObject *object, int type);
         void    geda_arc_object_set_line_width       (GedaObject *object, int width);
         void    geda_arc_object_set_radius           (GedaObject *object, int radius);
         void    geda_arc_object_set_start_angle      (GedaObject *object, int angle);
         void    geda_arc_object_translate            (GedaObject *object, int dx, int dy);
         bool    geda_arc_object_within_sweep         (GedaObject *object, int x, int y);

/* o_attrib_object.c */
         void    geda_attrib_object_add                      (GedaObject *object, GedaObject *item);
         void    geda_attrib_object_append_changed_hook      (Page *page, AttribsChangedFunc func, void *data);
         void    geda_attrib_object_attach                   (GedaObject *object, GedaObject *attrib, int set_color);
         void    geda_attrib_object_attach_list              (GedaObject *object, const GList *attr_list, int set_color);
         void    geda_attrib_object_detach                   (GedaObject *object);
         void    geda_attrib_object_detach_all               (GedaObject *object);
   GedaObject   *geda_attrib_object_first_attrib_by_name     (const GedaObject *object, const char *name);
         void    geda_attrib_object_freeze_hooks             (GedaObject *object);
         bool    geda_attrib_object_get_name_value           (const GedaObject *attrib, char **name_ptr,  char **value_ptr);
         bool    geda_attrib_object_is_attached_to           (const GedaObject *attrib, const GedaObject *object);
          int    geda_attrib_object_is_inherited             (const GedaObject *attrib);
   GedaObject   *geda_attrib_object_new_attached             (GedaObject *object, const char *name, const char *value, int visibility, int show_name_value);
         void    geda_attrib_object_print                    (const GList  *attributes);
         void    geda_attrib_object_remove                   (GList **list, GedaObject *remove);
        GList   *geda_attrib_object_return_attribs           (const GedaObject *object);
         char   *geda_attrib_object_search_attached_by_name  (const GedaObject *object, const char *name, int counter);
         char   *geda_attrib_object_search_floating_by_name  (const GList      *list,   const char *name, int counter);
         char   *geda_attrib_object_search_inherited_by_name (const GedaObject *object, const char *name, int counter);
         char   *geda_attrib_object_search_object_by_name    (const GedaObject *object, const char *name, int counter);
        GList   *geda_attrib_object_search_object_string     (const GedaObject *object, const char *string, int exact);
         void    geda_attrib_object_set_integer_value        (GedaObject *attrib, const char *name_ptr, int value);
         void    geda_attrib_object_set_value                (GedaObject *attrib, const char *name_ptr, const char *value_ptr);
         bool    geda_attrib_object_string_get_name_value    (const char *string, char **name_ptr,  char **value_ptr);
         void    geda_attrib_object_thaw_hooks               (GedaObject *object);

/* o_object.c */
        GList   *geda_object_read_buffer              (GedaToplevel *toplevel, GList *object_list, const char *buffer,
                                                       const int size, const char *name, GError **err);
        GList   *geda_object_read                     (GedaToplevel *toplevel, GList *object_list, char *filename, GError **err);
   GedaObject   *geda_object_copy                     (GedaObject *o_current) WARN_UNUSED;
         void    geda_object_mirror                   (GedaObject *object, int center_x, int center_y);
         void    geda_object_rotate                   (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_object_translate                (GedaObject *object, int dx, int dy);
         void    geda_object_update                   (GedaObject *object);

/* o_box_object.c */
   GedaObject   *geda_box_object_copy                 (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_end_cap          (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_fill_angle1      (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_fill_angle2      (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_fill_pitch1      (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_fill_pitch2      (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_fill_type        (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_fill_width       (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_line_length      (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_line_space       (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_line_type        (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_line_width       (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_lower_x          (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_lower_y          (const GedaObject *object) WARN_UNUSED;
         bool    geda_box_object_get_nearest_point    (const GedaObject *object, int x, int y, int *nx, int *ny);
          int    geda_box_object_get_upper_x          (const GedaObject *object) WARN_UNUSED;
          int    geda_box_object_get_upper_y          (const GedaObject *object) WARN_UNUSED;
         void    geda_box_object_modify               (GedaObject *object, int x, int y, int whichone);
         void    geda_box_object_modify_all           (GedaObject *object, int x1, int y1, int x2, int y2);
         void    geda_box_object_mirror               (GedaObject *object, int center_x, int center_y);
   GedaObject   *geda_box_object_new                  (int color, int x1, int y1, int x2, int y2);
         void    geda_box_object_rotate               (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_box_object_scale                (GedaObject *object, int x_scale, int y_scale);
         void    geda_box_object_set_end_cap          (GedaObject *object, int cap);
         void    geda_box_object_set_fill_angle1      (GedaObject *object, int angle);
         void    geda_box_object_set_fill_angle2      (GedaObject *object, int angle);
         void    geda_box_object_set_fill_pitch1      (GedaObject *object, int pitch);
         void    geda_box_object_set_fill_pitch2      (GedaObject *object, int pitch);
         void    geda_box_object_set_fill_type        (GedaObject *object, int type);
         void    geda_box_object_set_fill_width       (GedaObject *object, int width);
         void    geda_box_object_set_line_length      (GedaObject *object, int length);
         void    geda_box_object_set_line_space       (GedaObject *object, int space);
         void    geda_box_object_set_line_type        (GedaObject *object, int type);
         void    geda_box_object_set_line_width       (GedaObject *object, int width);
         void    geda_box_object_set_lower_x          (GedaObject *object, int x);
         void    geda_box_object_set_lower_y          (GedaObject *object, int y);
         void    geda_box_object_set_upper_x          (GedaObject *object, int x);
         void    geda_box_object_set_upper_y          (GedaObject *object, int y);
         void    geda_box_object_translate            (GedaObject *object, int dx, int dy);

/* o_bus_object.c */
         void    geda_bus_object_consolidate          (GedaToplevel *toplevel, Page *page);
   GedaObject   *geda_bus_object_copy                 (const GedaObject *o_current) WARN_UNUSED;
          int    geda_bus_object_get_direction        (const GedaObject *object) WARN_UNUSED;
          int    geda_bus_object_get_ripper_direction (const GedaObject *object) WARN_UNUSED;
          int    geda_bus_object_get_x1               (const GedaObject *object) WARN_UNUSED;
          int    geda_bus_object_get_x2               (const GedaObject *object) WARN_UNUSED;
          int    geda_bus_object_get_y1               (const GedaObject *object) WARN_UNUSED;
          int    geda_bus_object_get_y2               (const GedaObject *object) WARN_UNUSED;
         void    geda_bus_object_mirror               (GedaObject *object, int center_x, int center_y);
         void    geda_bus_object_modify               (GedaObject *object, int x, int y, int whichone);
   GedaObject   *geda_bus_object_new                  (int color, int x1, int y1, int x2, int y2, int ripper_direction);
          int    geda_bus_object_orientation          (const GedaObject *object);
         void    geda_bus_object_rotate               (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_bus_object_set_ripper_direction (GedaObject *object, int direction);
         void    geda_bus_object_set_x1               (GedaObject *object, int x);
         void    geda_bus_object_set_x2               (GedaObject *object, int x);
         void    geda_bus_object_set_y1               (GedaObject *object, int y);
         void    geda_bus_object_set_y2               (GedaObject *object, int y);

         void    geda_bus_object_translate            (GedaObject *object, int dx, int dy);

/* o_circle_object.c */

   GedaObject   *geda_circle_object_copy              (GedaObject *o_current);
          int    geda_circle_object_get_center_x      (const GedaObject *object);
          int    geda_circle_object_get_center_y      (const GedaObject *object);
          int    geda_circle_object_get_end_cap       (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_fill_angle1   (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_fill_angle2   (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_fill_pitch1   (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_fill_pitch2   (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_fill_type     (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_fill_width    (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_line_length   (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_line_space    (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_line_type     (const GedaObject *object) WARN_UNUSED;
          int    geda_circle_object_get_line_width    (const GedaObject *object) WARN_UNUSED;
         bool    geda_circle_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny);
          int    geda_circle_object_get_radius        (const GedaObject *object);
         void    geda_circle_object_modify            (GedaObject *object, int x, int y, int whichone);
         void    geda_circle_object_mirror            (GedaObject *object, int center_x, int center_y);
   GedaObject   *geda_circle_object_new               (int color, int x, int y, int radius);
         void    geda_circle_object_rotate            (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_circle_object_scale             (GedaObject *object, int scale);
         void    geda_circle_object_set_center_x      (GedaObject *object, int x);
         void    geda_circle_object_set_center_y      (GedaObject *object, int y);
         void    geda_circle_object_set_end_cap       (GedaObject *object, int cap);
         void    geda_circle_object_set_fill_angle1   (GedaObject *object, int angle);
         void    geda_circle_object_set_fill_angle2   (GedaObject *object, int angle);
         void    geda_circle_object_set_fill_pitch1   (GedaObject *object, int pitch);
         void    geda_circle_object_set_fill_pitch2   (GedaObject *object, int pitch);
         void    geda_circle_object_set_fill_type     (GedaObject *object, int type);
         void    geda_circle_object_set_fill_width    (GedaObject *object, int width);
         void    geda_circle_object_set_line_length   (GedaObject *object, int length);
         void    geda_circle_object_set_line_space    (GedaObject *object, int space);
         void    geda_circle_object_set_line_type     (GedaObject *object, int type);
         void    geda_circle_object_set_line_width    (GedaObject *object, int width);
         void    geda_circle_object_set_radius        (GedaObject *object, int r);
         void    geda_circle_object_translate         (GedaObject *object, int dx, int dy);

/* o_complex_object.c */
   GedaObject   *geda_complex_object_copy                  (GedaObject *o_current);
         void    geda_complex_object_check_symbol_version  (GedaToplevel *toplevel, GedaObject *object);
   const char   *geda_complex_object_get_filename          (GedaObject *object);
         bool    geda_complex_object_get_nearest_point     (const GedaObject *object, int x, int y, int *nx, int *ny);
        GList   *geda_complex_object_get_pin_objs          (GedaObject *object);

        GList   *geda_complex_object_get_prim_objs         (GedaObject *object);
   GedaObject   *geda_complex_object_find_pin_by_attribute (GedaObject *object, char *name, char *wanted_value);
          int    geda_complex_object_is_embedded           (GedaObject *o_current);
         void    geda_complex_object_mirror                (GedaObject *object, int center_x, int center_y);
         void    geda_complex_object_modify                (GedaObject *object, int x, int y);
   GedaObject   *geda_complex_object_new                   (GedaToplevel *toplevel, int x, int y, int angle, int mirror,
                                                            const CLibSymbol *clib_sym, const char *basename, int selectable);
   GedaObject   *geda_complex_object_new_embedded          (int x, int y, int angle, int mirror, const char *basename, int selectable);
        GList   *geda_complex_object_promote_attribs       (GedaToplevel *toplevel, GedaObject *object);
         void    geda_complex_object_reset_refdes          (GedaObject *object);
         void    geda_complex_object_rotate                (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_complex_object_translate             (GedaObject *object, int dx, int dy);

/* o_color.c */
          int    geda_object_color_get_default             (char type);

/* o_embed.c */
         bool    geda_object_embed                         (GedaObject *object);
          int    geda_object_unembed                       (GedaObject *object);

/* o_get.c */
          int    geda_object_get_attached_parent_id        (GedaObject *object);
          int    geda_object_get_bounds                    (const GedaObject *o_current, int *left, int *top, int *right, int *bottom);
          int    geda_object_get_bounds_list               (const GList *o_list, int *left, int *top, int *right, int *bottom);
         bool    geda_object_get_fill_options              (GedaObject *object, OBJECT_FILLING *type, int *width, int *pitch1,
                                                            int *angle1, int *pitch2, int *angle2);
         bool    geda_object_get_has_slope                 (GedaObject *object);
         bool    geda_object_get_is_attached               (GedaObject *object);
         bool    geda_object_get_bus_related               (GedaObject *object);
         bool    geda_object_get_is_embedded               (GedaObject *object);
         bool    geda_object_get_is_inside_region          (int xmin, int ymin, int xmax, int ymax, int x, int y);
         bool    geda_object_get_is_selectable             (GedaObject *object);
         bool    geda_object_get_is_selected               (GedaObject *object);
         bool    geda_object_get_is_valid_attribute        (GedaObject *object);
         bool    geda_object_get_is_visible                (const GedaObject *object);
     LINE_END    geda_object_get_line_cap_style            (int          capstyle);
         bool    geda_object_get_line_options              (GedaObject  *object, LINE_END *end, LINE_TYPE *type, int *width, int *length, int *space);
         bool    geda_object_get_nearest_point             (const GedaObject  *object, int x, int y, int *nx, int *ny);
          int    geda_object_get_num_text_lines            (const char  *string);
   const char   *geda_object_get_attrib_value              (GedaObject  *object, const char *name);
        GList   *geda_object_get_objects_by_type           (const GList *object_list, int type);
   GedaObject   *geda_object_get_parent                    (GedaObject  *object);
          int    geda_object_get_parent_id                 (GedaObject  *object);
         bool    geda_object_get_position                  (GedaObject  *object, int *x, int *y);
       double    geda_object_get_shortest_distance         (GedaObject  *object, int x, int y);
       double    geda_object_get_shortest_distance_full    (GedaObject  *object, int x, int y, int force_solid);

/* o_line_object.c */
   GedaObject   *geda_line_object_copy                     (GedaObject *object) WARN_UNUSED;
         bool    geda_line_object_is_endpoint              (GedaObject *object, GedaPoint *point);
         int     geda_line_object_get_closest_endpoint     (const GedaObject *object, int x, int y);
         bool    geda_line_object_get_intersection         (GedaObject *object1, GedaObject *object2, GedaPoint *point);
          int    geda_line_object_get_end_cap              (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_line_length          (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_line_space           (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_line_type            (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_line_width           (const GedaObject *object) WARN_UNUSED;
         bool    geda_line_object_get_midpoint             (GedaObject *object, GedaPoint *point);
         bool    geda_line_object_get_nearest_point        (const GedaObject *object, int x, int y, int *nx, int *ny);
         bool    geda_line_object_get_slope                (GedaObject *object, double *anwser);
          int    geda_line_object_get_x1                   (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_x2                   (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_y1                   (const GedaObject *object) WARN_UNUSED;
          int    geda_line_object_get_y2                   (const GedaObject *object) WARN_UNUSED;
       double    geda_line_object_length                   (GedaObject *object);
         void    geda_line_object_modify                   (GedaObject *object, int x, int y, int whichone);
         void    geda_line_object_mirror                   (GedaObject *object, int center_x, int center_y);
   GedaObject   *geda_line_object_new                      (int color, int x1, int y1, int x2, int y2) WARN_UNUSED;
         void    geda_line_object_rotate                   (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_line_object_set_end_cap              (GedaObject *object, int cap);
         void    geda_line_object_set_line_length          (GedaObject *object, int length);
         void    geda_line_object_set_line_space           (GedaObject *object, int space);
         void    geda_line_object_set_line_type            (GedaObject *object, int type);
         void    geda_line_object_set_line_width           (GedaObject *object, int width);
         void    geda_line_object_set_x1                   (GedaObject *object, int x);
         void    geda_line_object_set_x2                   (GedaObject *object, int x);
         void    geda_line_object_set_y1                   (GedaObject *object, int y);
         void    geda_line_object_set_y2                   (GedaObject *object, int y);
         void    geda_line_object_scale                    (GedaObject *object, int x_scale, int y_scale);
         void    geda_line_object_translate                (GedaObject *object, int dx, int dy);

/* o_list.c */
        GList   *geda_object_list_copy_all                 (const GList *src_list, GList *dest_list);
   GedaObject   *geda_object_list_find_attrib_by_name      (const GList *list, const char *name, int count);
        GList   *geda_object_list_find_floating            (const GList *list);
         void    geda_object_list_mirror                   (const GList *list, int x, int y);
         void    geda_object_list_rotate                   (const GList *list, int x, int y, int angle);
         void    geda_object_list_scale                    (const GList *list, int x_scale, int y_scale);
         void    geda_object_list_set_color                (const GList *list, int color);
         void    geda_object_list_translate                (const GList *list, int dx, int dy);

/* o_net_object.c */
         void    geda_net_object_consolidate               (GedaToplevel *toplevel, Page *page);
   GedaObject   *geda_net_object_copy                      (GedaObject *o_current);

          int    geda_net_object_get_x1                    (const GedaObject *object) WARN_UNUSED;
          int    geda_net_object_get_x2                    (const GedaObject *object) WARN_UNUSED;
          int    geda_net_object_get_y1                    (const GedaObject *object) WARN_UNUSED;
          int    geda_net_object_get_y2                    (const GedaObject *object) WARN_UNUSED;
         bool    geda_net_object_is_fully_connected        (GedaObject *object);
         void    geda_net_object_modify                    (GedaObject *object, int x, int y, int whichone);
         void    geda_net_object_mirror                    (GedaObject *object, int center_x, int center_y);
   GedaObject   *geda_net_object_new                       (int color, int x1, int y1, int x2, int y2);
          int    geda_net_object_orientation               (GedaObject *object);

         void    geda_net_object_rotate                    (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_net_object_refresh_conn_cache        (GedaObject *object);
         void    geda_net_object_set_x1                    (GedaObject *object, int x);
         void    geda_net_object_set_x2                    (GedaObject *object, int x);
         void    geda_net_object_set_y1                    (GedaObject *object, int y);
         void    geda_net_object_set_y2                    (GedaObject *object, int y);

         void    geda_net_object_translate                 (GedaObject *object, int dx, int dy);

/* o_notify.c */
         void    geda_object_notify_change_add             (Page *page, ChangeNotifyFunc pre_change_func,
                                                            ChangeNotifyFunc change_func, void *user_data);
         void    geda_object_notify_change_remove          (Page *page, ChangeNotifyFunc pre_change_func,
                                                            ChangeNotifyFunc change_func, void *user_data);
         void    geda_object_notify_change_remove_all      (Page *page);

/* o_path_object.c */
   GedaObject   *geda_path_object_copy                     (const GedaObject *o_current);
         bool    geda_path_object_get_nearest_point        (const GedaObject *object, int x, int y, int *nx, int *ny);

         void    geda_path_object_modify                   (GedaObject *object, int x, int y, int whichone);
         void    geda_path_object_mirror                   (GedaObject *object, int center_x, int center_y);
   GedaObject   *geda_path_object_new                      (int color, const char *path_string);
   GedaObject   *geda_path_object_new_from_polygon         (GArray *points,  int color);
   GedaObject   *geda_path_object_new_take_path            (int color, GedaPath *path_data);
         void    geda_path_object_rotate                   (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_path_object_translate                (GedaObject *object, int x, int y);

/* o_picture.c */
   GedaObject   *geda_picture_object_copy                  (GedaObject *o_current) WARN_UNUSED;

         bool    geda_picture_object_export_object         (GedaObject *o_current, const char *filename, const char *type, ...);
         bool    geda_picture_object_export_orginal        (GedaObject *o_current, const char *filename, const char *type, ...);

   const char   *geda_picture_object_get_data              (GedaObject *object, size_t *length);
       double    geda_picture_object_get_effective_ratio   (GedaObject *object);

   const char   *geda_picture_object_get_filename          (const GedaObject *object);
          int    geda_picture_object_get_height            (const GedaObject *object);
          int    geda_picture_object_get_lower_x           (const GedaObject *object) WARN_UNUSED;
        uint8   *geda_picture_object_get_mask_data         (const GedaObject *object) WARN_UNUSED;
         bool    geda_picture_object_get_nearest_point     (const GedaObject *object, int x, int y, int *nx, int *ny);

       double    geda_picture_object_get_ratio             (GedaObject *object);
unsigned char   *geda_picture_object_get_rgb_data          (GedaObject *object) WARN_UNUSED;
          int    geda_picture_object_get_width             (GedaObject *object);
         bool    geda_picture_object_is_embedded           (GedaObject *object);
         void    geda_picture_object_mirror                (GedaObject *object, int center_x, int center_y);
         void    geda_picture_object_modify                (GedaObject *object, int x, int y, int whichone);
         void    geda_picture_object_modify_all            (GedaObject *object, int x1, int y1, int x2, int y2);
   GedaObject   *geda_picture_object_new                   (const char *file_content, unsigned int file_length,
                                                            const char *filename, int x1, int y1, int x2, int y2, int angle, int mirrored,
                                                            int embedded) WARN_UNUSED;
         void    geda_picture_object_print                 (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);

         void    geda_picture_object_rotate                (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_picture_object_scale                 (GedaObject *object, int x_scale, int y_scale);
         bool    geda_picture_object_set_from_buffer       (GedaObject *object, const char *filename, const char *data, unsigned int length, GError **error);
         bool    geda_picture_object_set_from_file         (GedaObject *object, const char *filename, GError **error);

         void    geda_picture_object_translate             (GedaObject *object, int dx, int dy);

#ifdef GDK_PIXBUF_H
         bool    geda_picture_object_export_pixbuf         (GdkPixbuf *pixbuf, const char *filename, const char *type, ...);
    GdkPixbuf   *geda_picture_object_get_fallback_pixbuf   (void) WARN_UNUSED;
    GdkPixbuf   *geda_picture_object_get_pixbuf            (GedaObject *object) WARN_UNUSED;
    GdkPixbuf   *geda_picture_object_get_pixbuf_fit        (GedaObject *object, int interpolate) WARN_UNUSED;
#endif

/* o_pin_object.c */

   GedaObject   *geda_pin_object_copy                      (GedaObject *o_current) WARN_UNUSED;
   GedaObject   *geda_pin_object_create_elect_attrib       (GedaToplevel *toplevel, GedaObject *object, const char *descr, int x, int y);
   GedaObject   *geda_pin_object_create_label_attrib       (GedaToplevel *toplevel, GedaObject *object, const char *label, int x, int y);
   GedaObject   *geda_pin_object_create_mech_attrib        (GedaToplevel *toplevel, GedaObject *object, const char *descr, int x, int y);
   GedaObject   *geda_pin_object_create_number_attrib      (GedaToplevel *toplevel, GedaObject *object, const char *number, int x, int y);
   GedaObject   *geda_pin_object_create_seq_attrib         (GedaToplevel *toplevel, GedaObject *object, int sequence, int x, int y);
         bool    geda_pin_object_get_attributes            (GedaObject *object, const char **label, const char **number, int *sequence,
                                                            PIN_ELECT *e_type, PIN_MECH *m_type, PIN_NODE *type);
   const char   *geda_pin_object_get_electrical            (GedaObject *object);
   const char   *geda_pin_object_get_label                 (GedaObject *object);
   const char   *geda_pin_object_get_mechanical            (GedaObject *object);
         void    geda_pin_object_mirror                    (GedaObject *object, int center_x, int center_y);
         void    geda_pin_object_modify                    (GedaObject *object, int x, int y, int whichone);
   GedaObject   *geda_pin_object_new                       (int color, int x1, int y1, int x2, int y2, PIN_NODE node_type, int whichend);
         void    geda_pin_object_normalize                 (GedaObject *object);
        GList   *geda_pin_object_realize_attributes        (GedaToplevel *toplevel, GedaObject *object);
         void    geda_pin_object_rotate                    (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_pin_object_set_attributes            (GedaObject *object, const char *label_str, const char *number, int sequence,
                                                            PIN_ELECT e_type, PIN_MECH m_type, PIN_NODE type);
         bool    geda_pin_object_set_elect_type            (GedaObject *o_current, PIN_ELECT e_type);
         bool    geda_pin_object_set_mech_type             (GedaObject *o_current, PIN_MECH m_type);
         void    geda_pin_object_set_node_type             (GedaObject *o_current, PIN_NODE node_type);
         void    geda_pin_object_translate                 (GedaObject *object, int dx, int dy);
         void    geda_pin_object_update_whichend           (GList *object_list, int num_pins);


/* geda_object_save.c */
         void    geda_object_save_auto_backup              (GedaToplevel *toplevel);
         char   *geda_object_save_objects                  (const GList *object_list, bool save_attribs);
         char   *geda_object_save_buffer                   (const GList *object_list);
          int    geda_object_save                          (const GList *object_list, const char *filename, GError **err);

/* o_selection.c */
    SELECTION   *geda_object_selection_new                 (void);
         void    geda_object_selection_add                 (SELECTION *selection, GedaObject *o_selected);
   GedaObject   *geda_object_selection_get_first           (SELECTION *selection);
         void    geda_object_selection_print_all           (const SELECTION *selection);
          int    geda_object_selection_remove              (SELECTION *selection, GedaObject *o_selected);
          int    geda_object_selection_select              (GedaObject *object);
          int    geda_object_selection_unselect            (GedaObject *object);
          int    geda_object_selection_unselect_all        (SELECTION *selection);

/* o_set.c */
         void    geda_set_object_bounds_invalid            (GedaObject *object);
         void    geda_set_object_color                     (GedaObject *object, int color);
         void    geda_set_object_fill_options              (GedaObject *o_current, FILL_OPTIONS *fill_options);
         void    geda_set_object_line_options              (GedaObject *o_current, LINE_OPTIONS *line_options);
         void    geda_set_object_list_invalid              (GList      *list);
         void    geda_set_object_selected                  (GedaObject *object);
         void    geda_set_object_visibility                (GedaObject *object, int visibility);

/* o_style.c */
          int    geda_object_style_get_bus_width           (GedaToplevel *toplevel);
          int    geda_object_style_get_line_width          (GedaToplevel *toplevel);
          int    geda_object_style_get_net_width           (GedaToplevel *toplevel);
          int    geda_object_style_get_pin_width           (GedaToplevel *toplevel, int type);
         void    geda_object_style_set_line_width          (GedaToplevel *toplevel, GedaObject *o_current);

/* o_text_object.c */
   GedaObject   *geda_text_object_copy                     (const GedaObject *object) WARN_UNUSED;
          int    geda_text_object_get_alignment            (const GedaObject *object) WARN_UNUSED;
          int    geda_text_object_get_angle                (const GedaObject *object) WARN_UNUSED;
         bool    geda_text_object_get_nearest_point        (const GedaObject *object, int x, int y, int *nx, int *ny);
          int    geda_text_object_get_size                 (const GedaObject *object) WARN_UNUSED;
       double    geda_text_object_get_size_in_points       (const GedaObject *object) WARN_UNUSED;
   const char   *geda_text_object_get_string               (const GedaObject *object);
         int     geda_text_object_get_x                    (const GedaObject *object) WARN_UNUSED;
         int     geda_text_object_get_y                    (const GedaObject *object) WARN_UNUSED;
         void    geda_text_object_mirror                   (GedaObject *object, int center_x, int center_y);
   GedaObject   *geda_text_object_new                      (int color, int x, int y, int alignment, int angle,
                                                            int size, int visibility, int show_name_value, const char *string);
         void    geda_text_object_recreate                 (GedaObject *o_current);
         void    geda_text_object_rotate                   (GedaObject *object, int center_x, int center_y, int angle);
         void    geda_text_object_set_alignment            (GedaObject *object, int alignment);
         void    geda_text_object_set_angle                (GedaObject *object, int angle);
         void    geda_text_object_set_rendered_bounds_func (GedaObject *object, RenderedBoundsFunc func, void *user_data);
         void    geda_text_object_set_size                 (GedaObject *object, int size);
         void    geda_text_object_set_string               (GedaObject *object, const char *new_string);
         void    geda_text_object_set_x                    (GedaObject *object, int x);
         void    geda_text_object_set_y                    (GedaObject *object, int y);
          int    geda_text_object_strcmp                   (const GedaObject *object1, const GedaObject *object2) WARN_UNUSED;
         void    geda_text_object_translate                (GedaObject *object, int dx, int dy);

/* s_attrib.c */
          int    geda_struct_attrib_add_entry              (char *new_attrib);
         void    geda_struct_attrib_clear                  (void);
          int    geda_struct_attrib_count                  (void);
         void    geda_struct_attrib_print                  (void);
          int    geda_struct_attrib_uniq                   (char *name);
         char   *geda_struct_attrib_get                    (int index);

/* s_print.c */
         void    geda_struct_print_forw               (GList *list);
         void    geda_struct_print                    (GedaObject *ptr);

/* s_clib.c */
         void     geda_struct_clib_flush_cache             (void);
         void     geda_struct_clib_refresh                 (void);
        GList    *geda_struct_clib_get_sources             (const bool sorted);
         bool     geda_struct_clib_source_name_exist       (const char *name);
         bool     geda_struct_clib_source_path_exist       (const char *path);
const CLibSource *geda_struct_clib_get_source_by_name      (const char *name);
const CLibSource *geda_struct_clib_add_directory           (const char *directory, const char *name);
const CLibSource *geda_struct_clib_add_command             (const char *list_cmd,  const char *get_cmd, const char *name);
const CLibSource *geda_struct_clib_add_scm                 (SCM         listfunc,  SCM         getfunc, const char *name);

   const char    *geda_struct_clib_source_get_name         (const CLibSource *source);
        GList    *geda_struct_clib_source_get_symbols      (const CLibSource *source);
   const char    *geda_struct_clib_symbol_get_name         (const CLibSymbol *symbol);
         char    *geda_struct_clib_symbol_get_filename     (const CLibSymbol *symbol);
const CLibSource *geda_struct_clib_symbol_get_source       (const CLibSymbol *symbol);
         char    *geda_struct_clib_symbol_get_data         (const CLibSymbol *symbol);
        GList    *geda_struct_clib_search                  (const char *pattern, const CLibSearchMode mode);

         void     geda_struct_clib_symbol_invalidate_data  (const CLibSymbol *symbol);
const CLibSymbol *geda_struct_clib_get_symbol_by_name      (const char *name);
         char    *geda_struct_clib_symbol_get_data_by_name (const char *name);

/* s_conn.c */
         void    geda_struct_conn_remove_object            (GedaObject *to_remove);
         void    geda_struct_conn_update_linear_object     (GedaObject *object);
         void    geda_struct_conn_update_object            (GedaObject *object);
         void    geda_struct_conn_print                    (GList  *conn_list);
          int    geda_struct_conn_net_search               (GedaObject *new_net,    int     whichone, GList *conn_list);
        GList   *geda_struct_conn_return_others            (GList  *input_list, GedaObject *object);

         void    geda_struct_conn_append_conns_changed_hook(Page *page,   ConnsChangedFunc func, void *data);
         void    geda_struct_conn_emit_conns_changed       (GedaObject *object);
         void    geda_struct_conn_freeze_hooks             (GedaObject *object);
         void    geda_struct_conn_thaw_hooks               (GedaObject *object);

/* s_cue.c */
         void    geda_struct_cue_get_locations             (const GList *objects, GArray *junctions, GArray *unconnected);
         void    geda_struct_cue_output_all                (GedaToplevel *toplevel, const GList *obj_list, FILE *fp, int type);
         void    geda_struct_cue_output_single             (GedaToplevel *toplevel, GedaObject *object, FILE *fp, int type);

/* s_hierarchy.c */
         Page   *geda_struct_hierarchy_down_single         (GedaToplevel *toplevel, const char *filename, Page *parent,
                                                            int page_control, int flag, GError **err);
         Page   *geda_struct_hierarchy_down_symbol         (GedaToplevel *toplevel, const CLibSymbol *symbol, Page *parent);
         Page   *geda_struct_hierarchy_find_up_page        (PageList *page_list, Page *current_page);
         Page   *geda_struct_hierarchy_load_subpage        (Page *page, const char *filename, GError **error);
        GList   *geda_struct_hierarchy_traverse_pages      (GedaToplevel *toplevel, Page *p_current, int flags);
          int    geda_struct_hierarchy_print_page          (Page *p_current, void *data);
         Page   *geda_struct_hierarchy_find_prev_page      (PageList *page_list, Page *current_page);
         Page   *geda_struct_hierarchy_find_next_page      (PageList *page_list, Page *current_page);

/*  s_object.c */
    GedaObject  *geda_struct_object_new                    (int type);
         void    geda_struct_object_add_child              (GedaObject *parent, GedaObject *child);
         void    geda_struct_object_release                (GedaObject *object);
         void    geda_struct_object_release_objects        (GList *list);
         void    geda_struct_object_set_page_changed       (const GedaObject *object);

/* s_page.c */
         Page   *geda_struct_page_new                      (GedaToplevel *toplevel, const char *filename);
         Page   *geda_struct_page_new_with_notify          (GedaToplevel *toplevel, const char *filename);

         void    geda_struct_page_autosave_init            (GedaToplevel *toplevel);
          int    geda_struct_page_autosave                 (GedaToplevel *toplevel);

         bool    geda_struct_page_check_changed            (PageList *list);
         void    geda_struct_page_clear_changed            (PageList *list);

         void    geda_struct_page_delete                   (GedaToplevel *toplevel, Page *page, int previous);
         void    geda_struct_page_delete_list              (GedaToplevel *toplevel);
         Page   *geda_struct_page_get_current              (GedaToplevel *toplevel);
         bool    geda_struct_page_set_current              (GedaToplevel *toplevel, Page *page);
   const char   *geda_struct_page_get_file_extension       (Page *page);
    SELECTION   *geda_struct_page_get_selection            (Page *page);
         bool    geda_struct_page_goto                     (Page *page);
         bool    geda_struct_page_is_symbol_file           (Page *page);
         void    geda_struct_page_print_all                (GedaToplevel *toplevel);
         void    geda_struct_page_resequence_by_ids        (GedaToplevel *toplevel);
          int    geda_struct_page_save_all                 (GedaToplevel *toplevel);
          int    geda_struct_page_save_all_changed         (GedaToplevel *toplevel);
         Page   *geda_struct_page_search                   (GedaToplevel *toplevel, const char *filename);
         Page   *geda_struct_page_search_by_page_id        (PageList *list, int pid);
         void    geda_struct_page_set_bounds_func          (Page *page, RenderedBoundsFunc func, void *user_data);

         void    geda_struct_page_append_object            (Page *page, GedaObject  *object);
         void    geda_struct_page_append_list              (Page *page, const GList *obj_list);

         void    geda_struct_page_remove_object            (Page *page, GedaObject *object);
         void    geda_struct_page_replace_object           (Page *page, GedaObject *object1, GedaObject *object2);
         void    geda_struct_page_delete_objects           (Page *page);
   GedaObject   *geda_struct_page_get_object               (Page *page, int sid);
        GList   *geda_struct_page_get_objects              (Page *page);
        GList   *geda_struct_page_objects_in_region        (Page *page, int min_x, int min_y, int max_x, int max_y);
        GList   *geda_struct_page_objects_in_regions       (Page *page, RECTANGLE *rects, int n_rects);

/* s_papersizes.c */
          int    geda_struct_papersizes_add_entry          (char *new_papersize, int width, int height);
         void    geda_struct_papersizes_print              (void);
          int    geda_struct_papersizes_uniq               (char *name);
         void    geda_struct_papersizes_init               (void);
         char   *geda_struct_papersizes_get                (int counter);
         void    geda_struct_papersizes_get_size           (char *string, int *width, int *height);

/* s_path.c */
     GedaPath   *geda_struct_path_copy_modify              (GedaPath *path, int dx, int dy, int new_x, int new_y, int whichone);
     GedaPath   *geda_struct_path_parse                    (const char *path_str);
         char   *geda_struct_path_string_from_path         (const GedaPath *path);
          int    geda_struct_path_to_polygon               (const GedaPath *path, GArray *points);
       double    geda_struct_path_shortest_distance        (GedaPath *path, int x, int y, int solid);

/* s_place.c */
         void    geda_struct_place_free_place_list         (GedaToplevel *toplevel);
         GList  *geda_struct_place_get_place_list          (GedaToplevel *toplevel);
         void    geda_struct_place_set_place_list          (GedaToplevel *toplevel, GList *new_place_list );

/* s_encoding.c */
         char   *geda_struct_encoding_base64_encode        (char *src, unsigned int srclen, unsigned int *dstlenp, bool strict);
         char   *geda_struct_encoding_base64_decode        (char *src, unsigned int srclen, unsigned int *dstlenp);


/* s_slib.c */
          int    geda_struct_slib_add_entry                (const char *new_path);

         char   *geda_struct_slib_get_basename             (const char *rawname);
         char   *geda_struct_slib_get_dir                  (int index);

          int    geda_struct_slib_search_for_dirname       (const char *dir_name);
         char   *geda_struct_slib_search_for_file          (const char *filename)WARN_UNUSED;
         char   *geda_struct_slib_search_dirs              (const char *basename)WARN_UNUSED;
         void    geda_struct_slib_print                    (void);
         void    geda_struct_slib_print_dirs               (void);
          int    geda_struct_slib_unique_dir_exist         (const char *path);

/* s_slot.c */
         char   *geda_struct_slot_search_slot              (GedaObject *object, GedaObject **return_found);
         void    geda_struct_slot_update_object            (GedaObject *object);

/* s_textbuffer.c */
   TextBuffer   *geda_struct_textbuffer_new                (const char *data, const int size)WARN_UNUSED;
   TextBuffer   *geda_struct_textbuffer_free               (TextBuffer *tb);
          int    geda_struct_textbuffer_get_line_count     (TextBuffer *tb)WARN_UNUSED;
   const char   *geda_struct_textbuffer_next               (TextBuffer *tb, const int count);
   const char   *geda_struct_textbuffer_next_line          (TextBuffer *tb);
         void    geda_struct_textbuffer_seek               (TextBuffer *tb, int offset);

/* s_tile.c */
         void    geda_struct_tile_update_object            (GedaObject *object);
         GList  *geda_struct_tile_get_objectlists          (Page *p_current, int world_x1, int world_y1, int world_x2, int world_y2);

/* s_toplevel.c */
        GList   *geda_toplevel_struct_get_selection        (const GedaToplevel *toplevel);
        GList   *geda_toplevel_struct_get_symbols          (const GedaToplevel *toplevel);
         Page   *geda_toplevel_struct_get_page_by_name     (const GedaToplevel *toplevel, const char *filename);
        GList   *geda_toplevel_struct_get_pages            (const GedaToplevel *toplevel);
         void    geda_toplevel_struct_release              (GedaToplevel *toplevel);
         void    geda_toplevel_struct_set_rbounds_func     (GedaToplevel *toplevel, RenderedBoundsFunc func, void *user_data);

/* s_undo.c */
         UNDO   *geda_struct_undo_return_tail              (UNDO *head);
         UNDO   *geda_struct_undo_return_head              (UNDO *tail);
         UNDO   *geda_struct_undo_new_head                 (void);
         void    geda_struct_undo_destroy_head             (UNDO *u_head);
         UNDO   *geda_struct_undo_add_disk                 (int type, char *filename, Page *page);
         UNDO   *geda_struct_undo_add_memory               (int type, Page *page);
         UNDO   *geda_struct_undo_add                      (UNDO *head,  int type,  char *filename, GList *object_list, int left, int top,
                                                            int right, int bottom, int  page_control, int up);
         void    geda_struct_undo_print_all                (UNDO *head);
         void    geda_struct_undo_destroy_all              (UNDO *head);
         void    geda_struct_undo_remove                   (UNDO *head, UNDO *u_tos);
         void    geda_struct_undo_remove_rest              (UNDO *head);
          int    geda_struct_undo_levels                   (UNDO *head);
         void    geda_struct_undo_update_modified          (Page *p_current);
         void    geda_struct_undo_init                     (Page *p_current);
         void    geda_struct_undo_free_all                 (Page *p_current);

/* ---------------- Utilities -------------- */

/* u_basic.c */
         char   *geda_utility_expand_env_variable(const char *string);
         void    geda_utility_print_object       (GedaObject *object);

/* u_glist.c */
        GList   *geda_utility_glist_clear        (GList *list) WARN_UNUSED;
          int    geda_utility_glist_find_string  (GList *list, const char *string);
         void    geda_utility_glist_free_all     (void  *data);
         void    geda_utility_glist_free_full    (GList *list, GDestroyNotify free_func);
         bool    geda_utility_glist_str_inlist   (GList *list, const char *string);
         bool    geda_utility_glist_stri_inlist  (GList *list, const char *string);

       GSList   *geda_utility_gslist_clear       (GSList *list);
          int    geda_utility_gslist_find_string (GSList *list, const char *string) WARN_UNUSED;
         void    geda_utility_gslist_free_all    (void   *data);
         void    geda_utility_gslist_free_full   (GSList *list, GDestroyNotify free_func);
         bool    geda_utility_gslist_str_inlist  (GSList *list, const char *string);
         bool    geda_utility_gslist_stri_inlist (GSList *list, const char *string);

/* u_log.c */
         void    geda_utility_log_close               (void);
          int    geda_utility_log_get_log_time        (void);
          int    geda_utility_log_get_quiet_mode      (void);
          int    geda_utility_log_get_verbose_mode    (void);
         void    geda_utility_log_init                (const char *app_prefix);
         void    geda_utility_log_quite               (const char *format, ...);
         char   *geda_utility_log_read                (void);
      LogFunc    geda_utility_log_set_default_handler (LogFunc log_func, void *user_data);
         void    geda_utility_log_set_log_time        (int mode);
         void    geda_utility_log_set_quiet_mode      (int mode);
         void    geda_utility_log_set_update_func     (LogUpdateFunc func);
         void    geda_utility_log_set_verbose_mode    (int mode);
         void    geda_utility_log_system              (const char *format, ...);
         void    geda_utility_log_verbose             (const char *format, ...);

/* u_string.c */
         char   *geda_utility_string_concat           (const char *string1, ...) WARN_UNUSED;
         char   *geda_utility_string_get_valid_utf8   (const char *string) WARN_UNUSED;
         char   *geda_utility_string_int2str          (int value, char *str, int radix);
         bool    geda_utility_string_isalnum          (const char *string);
   const char   *geda_utility_string_istr             (const char *str1, const char *str2);
         bool    geda_utility_string_parse_xy         (const char *string, int *x, int *y) WARN_UNUSED;
         char   *geda_utility_string_remove_last_nl   (char *string);
         char   *geda_utility_string_remove_nl        (char *string);
         char   *geda_utility_string_scm2c            (SCM   scm_str_name) WARN_UNUSED;
         void    geda_utility_string_sort_array       (char *strings[], size_t strings_size);
         char   *geda_utility_string_split            (const char *string, char delimiter, int count) WARN_UNUSED;
         char   *geda_utility_string_sprintf          (const char *format, ...) WARN_UNUSED;
         char   *geda_utility_string_strdup           (const char *str) WARN_UNUSED;
         bool    geda_utility_string_strequal         (const char *str1, const char *str2) WARN_UNUSED;
         bool    geda_utility_string_stricmp          (const char *str1, const char *str2);
         int     geda_utility_string_stristr          (const char *haystack, const char *needle);
         char   *geda_utility_string_strisubst        (char *source, char *old_str, char *new_str);
         int     geda_utility_string_strncmpi         (const char *str1, const char *str2, int n);
         char   *geda_utility_string_strndup          (const char *str, int n) WARN_UNUSED;
         int     geda_utility_string_strsize          (const char *format, va_list args);
         char   *geda_utility_string_strstr_rep       (char *original,   const char *old_str, const char *new_str);
         char   *geda_utility_string_strsubst         (char *source, char *old_str, char *new_str);
         char   *geda_utility_string_strtrim          (const char *string);
         int     geda_utility_string_word_count       (char *str);

/* u_program.c */
         void    geda_utility_program_backtrace       (void);
         void   *geda_utility_program_mem_alloc       (unsigned int amount);
         void   *geda_utility_program_mem_calloc      (unsigned int amount);
         void    geda_utility_program_mem_free        (void *mem);
         void    geda_utility_program_mem_set_vtable  (void);

/* u_refdes.c */
const GedaRefDes *geda_utility_refdes_get_ieee        (void);
const GedaRefDes *geda_utility_refdes_get_spice       (void);
const GedaRefDes *geda_utility_refdes_get_standard    (void);
         void     geda_utility_refdes_reset           (GedaObject *object);
         char    *geda_utility_refdes_return_numeric  (const void *text);
         char    *geda_utility_refdes_return_prefix   (const void *text);

#ifdef __cplusplus
}
#endif /* __cplusplus */
