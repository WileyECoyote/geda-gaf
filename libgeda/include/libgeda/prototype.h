G_BEGIN_DECLS

/* f_basic.c */
      char *f_get_autosave_filename        (const char *filename);
      bool  f_get_file_contents            (const char *filename, char **contents, size_t *length, GError **err);
      void  f_set_backup_loader_query_func (GedaToplevel *toplevel, void *func, ...);
      bool  f_has_active_autosave          (const char *filename, GError **err);
      int   f_open                         (GedaToplevel *toplevel, Page *page, const char *filename, GError **err);
      int   f_open_flags                   (GedaToplevel *toplevel, Page *page, const char *filename, const int flags, GError **err);
      void  f_close                        (GedaToplevel *toplevel);
      bool  f_save                         (GedaToplevel *toplevel, Page *page, const char *filename, GError **error);
      char *f_normalize_filename           (const char *filename, GError **error);
      char *follow_symlinks                (const char *filename, GError **error);

/* f_path.c */
      void   f_path_free                   (void);
const char  *f_path_sys_data               (void);
const char  *f_path_sys_doc                (void);
const char  *f_path_sys_config             (void);
const char  *f_path_user_config            (void);

/* f_print.c */
      int   f_print_file                    (GedaToplevel *toplevel, Page *page, const char *filename);
      int   f_print_command                 (GedaToplevel *toplevel, Page *page, const char *command);
      int   f_print_stream                  (GedaToplevel *toplevel, Page *page, FILE *fp);
      void  f_print_set_type                (GedaToplevel *toplevel, int type);

/* f_utilities.c */
      int     f_copy                        (const char *source, const char *target);
const char   *get_filename_ext              (const char *filename);
const char   *geda_basename                 (const char *path);
      void    remove_ext_from_basename      (char *filename);
      GSList *f_get_dir_list_files          (char *path, char *filter);
      int     f_file_remove                 (const char *pathname);

/* g_basic.c */
      SCM  g_scm_eval_protected                  (SCM exp, SCM module_or_state);
      SCM  g_scm_eval_string_protected           (SCM str);
      SCM  g_scm_c_eval_string_protected         (const char *str);
      bool g_read_scheme_file                    (const char *filename, GError **err);

/* g_rc_parse.c */
      SCM  g_rc_mode_general      (SCM       scmmode,  const char *rc_name, int *mode_var, const vstbl_entry *table, int table_size);
      bool g_rc_parse_file        (const char *rcfile, EdaConfig *cfg, GError **err);
      bool g_rc_parse_system      (const char *rcname, GError **err);
      bool g_rc_parse_user        (const char *rcname, GError **err);
      bool g_rc_parse_local       (const char *rcname, const char *path,    GError       **err);
      bool g_rc_parse             (const char *pname,  const char *rcname,  const char    *rcfile);
      void g_gafrc_parse_handler  (const char *dummy,  ConfigParseErrorFunc handler, void *user_data);
      void g_rcname_parse_handler (const char *rcname, ConfigParseErrorFunc handler, void *user_data);
      void g_rcfile_parse_handler (const char *rcfile, ConfigParseErrorFunc handler, void *user_data);
      void g_rc_parse_handler     (const char *rcname, const char *rcfile, ConfigParseErrorFunc handler, void *user_data);
      SCM g_rc_rc_filename        (void);
      SCM g_rc_rc_config          (void);

/* i_menu.c */
      int       i_menu_return_num             (void);
      SCM       i_menu_return_entry           (int index, char **menu_name);
      int       i_menu_add_entry              (char *new_menu, SCM menu_items);
      void      i_menu_print                  (void);
      void      i_menu_free                   (void);
      void      i_menu_init                   (void);

/* i_vars.c */
      void    i_vars_libgeda_set            (GedaToplevel *toplevel);
      void    i_vars_libgeda_freenames      (void);

/* libgeda.c */
      void    libgeda_init                  (void);
      void    libgeda_release               (void);

/* m_basic.c */
    //void    set_window                    (GedaToplevel *toplevel, Page *page, int xmin, int xmax, int ymin, int ymax);
      void    rotate_point                  (int x, int y, int angle, int *newx, int *newy);
      void    rotate_point_90               (int x, int y, int angle, int *newx, int *newy);
      void    PAPERSIZEtoWORLD              (int width, int height, int border, int *right, int *bottom);
      int     m_dist                        (int x1, int y1, int x2, int y2);

/* m_hatch.c */
      void    m_hatch_box                   (Box    *box,    int angle, int pitch, GArray *lines);
      void    m_hatch_circle                (Circle *circle, int angle, int pitch, GArray *lines);
      void    m_hatch_path                  (Path   *path,   int angle, int pitch, GArray *lines);
      GArray *m_hatch_object                (Object *object);

/* m_polygon.c */
      void    m_polygon_append_bezier       (GArray *points, BEZIER *bezier, int segments);
      void    m_polygon_append_point        (GArray *points, int x, int y);

/* o_arc_basic.c */
      Object *o_arc_new                     (int color, int x, int y, int radius, int start_angle, int end_angle);
      Object *o_arc_copy                    (Object *o_current);
      void    o_arc_modify                  (Object *object, int x, int y, int whichone);
      void    o_arc_translate_world         (int dx, int dy, Object *object);
      void    o_arc_rotate_world            (int world_centerx, int world_centery, int angle, Object *object);
      void    o_arc_mirror_world            (int world_centerx, int world_centery, Object *object);

/* o_attrib.c */
      void    o_attrib_add                              (Object *object, Object *item);
      bool    o_attrib_is_attached                      (Object *attrib, Object *object);
      void    o_attrib_attach                           (Object *attrib, Object *object, int set_color);
      void    o_attrib_attach_list                      (GList  *attr_list, Object *object, int set_color);
      void    o_attrib_detach_all                       (Object *object);
      Object *o_attrib_new_attached                     (Object *object, const char *name, const char *value, int visibility, int show_name_value);
      void    o_attrib_print                            (GList  *attributes);
      void    o_attrib_remove                           (GList **list, Object *remove);
      bool    o_attrib_string_get_name_value            ( const char *string, char **name_ptr, char **value_ptr);
      bool    o_attrib_get_name_value                   (Object *attrib, char **name_ptr,  char **value_ptr);
      void    o_attrib_set_value                        (Object *attrib, const char  *name_ptr, const char *value_ptr);
      void    o_attrib_set_integer_value                (Object *attrib, char  *name_ptr,  int   value);
      GList  *o_attrib_find_floating_attribs            (const GList *list);
      Object *o_attrib_find_attrib_by_name              (const GList *list, const char *name, int count);
      Object *o_attrib_first_attrib_by_name             (Object *object,    char *name);
      char   *o_attrib_search_floating_attribs_by_name  (const GList *list, const char *name, int counter);
      char   *o_attrib_search_attached_attribs_by_name  (Object *object, const char *name, int counter);
      char   *o_attrib_search_inherited_attribs_by_name (Object *object, const char *name, int counter);
      char   *o_attrib_search_object_attribs_by_name    (Object *object, const char *name, int counter);
      GList  *o_attrib_return_attribs                   (Object *object);
      int     o_attrib_is_inherited                     (Object *attrib);
      void    o_attrib_append_attribs_changed_hook      (Page *page, AttribsChangedFunc func, void *data);
      void    o_attrib_emit_attribs_changed             (Object *object);
      void    o_attrib_freeze_hooks                     (Object *object);
      void    o_attrib_thaw_hooks                       (Object *object);

/* o_basic.c */
const char   *o_file_format_header     (void);
      char   *o_save_buffer            (const GList *object_list);
      int     o_save                   (GedaToplevel *toplevel, const GList *object_list, const char *filename, GError **err);
      GList  *o_read_buffer            (GedaToplevel *toplevel, GList *object_list, const char *buffer,
                                        const int size, const char *name, GError **err);
      GList  *o_read                   (GedaToplevel *toplevel, GList *object_list, char *filename, GError **err);
      void    o_scale                  (GList *list, int x_scale, int y_scale);
      int     inside_region            (int xmin, int ymin, int xmax, int ymax, int x, int y);
      Object *o_object_copy            (Object *selected);
      void    o_translate_world        (int dx, int dy, Object *object);
      void    o_rotate_world           (int world_centerx, int world_centery,
                                        int angle, Object *object);
      void    o_mirror_world           (int world_centerx, int world_centery, Object *object);

/* o_box_basic.c */
      Object *o_box_new                (int color, int x1, int y1, int x2, int y2);
      Object *o_box_copy               (Object *o_current);
      void    o_box_modify_all         (Object *object, int x1, int y1, int x2, int y2);
      void    o_box_modify             (Object *object, int x, int y, int whichone);
      void    o_box_translate_world    (int dx, int dy, Object *object);
      void    o_box_rotate_world       (int world_centerx, int world_centery, int angle, Object *object);
      void    o_box_mirror_world       (int world_centerx, int world_centery, Object *object);

/* o_bus_basic.c */
      Object *o_bus_new                (int color, int x1, int y1, int x2, int y2, int bus_ripper_direction);
      Object *o_bus_copy               (Object *o_current);
      void    o_bus_translate_world    (int dx, int dy, Object *object);
      void    o_bus_rotate_world       (int world_centerx, int world_centery, int angle, Object *object);
      void    o_bus_mirror_world       (int world_centerx, int world_centery, Object *object);
      int     o_bus_orientation        (Object *object);
      int     o_bus_get_direction      (Object *object);
      void    o_bus_consolidate        (void);
      void    o_bus_modify             (Object *object, int x, int y, int whichone);

/* o_circle_basic.c */
      Object *o_circle_new             (int color, int x, int y, int radius);
      Object *o_circle_copy            (Object *o_current);
      void    o_circle_modify          (Object *object, int x, int y, int whichone);
      void    o_circle_translate_world (int dx, int dy, Object *object);
      void    o_circle_rotate_world    (int world_centerx, int world_centery, int angle, Object *object);
      void    o_circle_mirror_world    (int world_centerx, int world_centery, Object *object);

/* o_complex_basic.c */
      int        world_get_single_object_bounds (Object *o_current, int *left, int *top, int *right, int *bottom);
      int        world_get_object_glist_bounds  (const GList *o_list, int *left, int *top, int *right, int *bottom);
      int        o_complex_is_embedded          (Object *o_current);
      GList     *o_complex_promote_attribs      (GedaToplevel *toplevel, Object *object);
      Object    *o_complex_new                  (GedaToplevel *toplevel, int x, int y, int angle, int mirror,
                                                 const CLibSymbol *clib_sym, const char *basename, int selectable);
      Object    *o_complex_new_embedded         (int x, int y, int angle, int mirror, const char *basename, int selectable);
//     void       o_complex_set_filename         (GedaToplevel *toplevel, const char *basename);
      void       o_complex_translate_world      (int dx, int dy, Object *object);
      Object    *o_complex_copy                 (Object *o_current);
      void       o_complex_reset_refdes         (Object *object);
      void       o_complex_rotate_world         (int world_centerx, int world_centery, int angle, Object *object);
      void       o_complex_mirror_world         (int world_centerx, int world_centery, Object *object);
      Object    *o_complex_find_pin_by_attribute(Object *object, char *name, char *wanted_value);
      void       o_complex_check_symversion     (Object *object);

/* o_color.c */
      int        o_color_get_object_default (char type);

/* o_embed.c */
      void       o_embed                    (GedaToplevel *toplevel, Object *o_current);
      void       o_unembed                  (GedaToplevel *toplevel, Object *o_current);

/* o_get.c */
      bool        o_get_fill_options        (Object *object, OBJECT_FILLING *type, int *width, int *pitch1,
                                             int *angle1, int *pitch2, int *angle2);
      bool        o_get_is_bus_related      (Object *object);
      bool        o_get_is_selectable       (Object *object);
      bool        o_get_is_selected         (Object *object);
      bool        o_get_is_visible          (Object *object);
      LINE_END    o_get_line_end            (int capstyle);
      bool        o_get_line_options        (Object *object, LINE_END *end, LINE_TYPE *type, int *width, int *length, int *space);
      GList      *o_get_objects_by_type     (const GList *object_list, int type);
      Page       *o_get_page                (Object *obj);
      Object     *o_get_parent              (Object *object);
      int         o_get_parent_id           (Object *object);
      bool        o_get_position            (int *x, int *y, Object *object);
      double      o_get_shortest_distance   (Object *object, int x, int y);

/* o_line_basic.c */
      Object     *o_line_new                (int color, int x1, int y1, int x2, int y2);
      Object     *o_line_copy               (Object *o_current);
      void        o_line_modify             (Object *object, int x, int y, int whichone);
      void        o_line_translate_world    (int dx, int dy, Object *object);
      void        o_line_rotate_world       (int world_centerx, int world_centery, int angle, Object *object);
      void        o_line_mirror_world       (int world_centerx, int world_centery, Object *object);
      void        o_line_scale_world        (int x_scale, int y_scale, Object *object);
      double      o_line_length             (Object *object);

/* o_list.c */
      GList      *o_glist_copy_all          (const GList *src_list, GList *dest_list);
      void        o_glist_translate_world   (int dx, int dy, const GList *list);
      void        o_glist_rotate_world      (int x, int y, int angle, const GList *list);
      void        o_glist_mirror_world      (int x, int y, const GList *list);
      void        o_glist_set_color         (const GList *list, int color);

/* o_net_basic.c */
      Object    *o_net_new                  (int color, int x1, int y1, int x2, int y2);
      Object    *o_net_copy                 (Object *o_current);
      void       o_net_translate_world      (int dx, int dy, Object *object);
      void       o_net_rotate_world         (int world_centerx, int world_centery, int angle, Object *object);
      void       o_net_mirror_world         (int world_centerx, int world_centery, Object *object);
      int        o_net_orientation          (Object *object);
      void       o_net_consolidate          (GedaToplevel *toplevel, Page *page);
      void       o_net_modify               (Object *object, int x, int y, int whichone);
      void       o_net_refresh_conn_cache   (Object *object);
      bool       o_net_is_fully_connected   (Object *object);

/* o_notify.c */
      void       o_add_change_notify        (Page *page, ChangeNotifyFunc pre_change_func,
                                             ChangeNotifyFunc change_func, void *user_data);
      void       o_remove_change_notify     (Page *page, ChangeNotifyFunc pre_change_func,
                                             ChangeNotifyFunc change_func, void *user_data);
      void       o_change_notify_remove_all (Page *page);

/* o_path_basic.c */
      Object    *o_path_new                 (int color, const char *path_string);
      Object    *o_path_new_take_path       (int color, Path *path_data);
      Object    *o_path_copy                (Object *o_current);
      void       o_path_modify              (Object *object, int x, int y, int whichone);
      void       o_path_translate_world     (int x, int y, Object *object);
      void       o_path_rotate_world        (int world_centerx, int world_centery, int angle, Object *object);
      void       o_path_mirror_world        (int world_centerx, int world_centery, Object *object);

/* o_picture.c */
      Object    *o_picture_new              (const char *file_content, gsize file_length,
                                             const char *filename, int x1, int y1, int x2, int y2, int angle, int mirrored,
                                             int embedded) G_GNUC_WARN_UNUSED_RESULT;
      float      o_picture_get_ratio        (Object *object);
      void       o_picture_modify           (Object *object, int x, int y, int whichone);
      void       o_picture_modify_all       (Object *object, int x1, int y1, int x2, int y2);
      void       o_picture_rotate_world     (int world_centerx, int world_centery, int angle,Object *object);
      void       o_picture_mirror_world     (int world_centerx, int world_centery, Object *object);
      void       o_picture_translate_world  (int dx, int dy, Object *object);
      Object    *o_picture_copy             (Object *o_current) G_GNUC_WARN_UNUSED_RESULT;
      bool       o_picture_is_embedded      (Object *object);
      GdkPixbuf *o_picture_get_pixbuf       (Object *object) G_GNUC_WARN_UNUSED_RESULT;
const char      *o_picture_get_data         (Object *object, size_t *len);
      bool       o_picture_set_from_buffer  (Object *object, const char *filename, const char *data, size_t len, GError  **error);
      bool       o_picture_set_from_file    (Object *object, const char *filename, GError **error);
const char      *o_picture_get_filename     (Object *object);
      GdkPixbuf *o_picture_get_fallback_pixbuf (GedaToplevel *toplevel) G_GNUC_WARN_UNUSED_RESULT;

/* o_pin_basic.c */
      Object    *o_pin_new                  (int color, int x1, int y1, int x2, int y2, PIN_NODE node_type, int whichend);
      Object    *o_pin_copy                 (Object *o_current);
      void       o_pin_translate_world      (int dx, int dy, Object *object);
      void       o_pin_rotate_world         (int world_centerx, int world_centery, int angle, Object *object);
      void       o_pin_mirror_world         (int world_centerx, int world_centery, Object *object);
      void       o_pin_modify               (Object *object, int x, int y, int whichone);
      void       o_pin_normalize            (Object *object);
      void       o_pin_update_whichend      (GList *object_list, int num_pins);
      bool       o_pin_set_elect_type       (Object *o_current, PIN_ELECT e_type);
      bool       o_pin_set_mech_type        (Object *o_current, PIN_MECH m_type);
      void       o_pin_set_node_type        (Object *o_current, PIN_NODE node_type);
      bool       o_pin_get_attributes       (Object *object, const char **label, int *number, int *sequence,
                                             PIN_ELECT *e_type, PIN_MECH *m_type, PIN_NODE *type);
      void       o_pin_set_attributes       (Object *object, const char *label_str, int number, int sequence,
                                             PIN_ELECT e_type, PIN_MECH m_type, PIN_NODE type);
      Object    *o_pin_create_label_attrib  (GedaToplevel *toplevel, Object *object, const char *label, int x, int y);
      Object    *o_pin_create_number_attrib (GedaToplevel *toplevel, Object *object, int number, int x, int y);
      Object    *o_pin_create_seq_attrib    (GedaToplevel *toplevel, Object *object, int sequence, int x, int y);
      Object    *o_pin_create_elect_attrib  (GedaToplevel *toplevel, Object *object, const char *descr, int x, int y);
      Object    *o_pin_create_mech_attrib   (GedaToplevel *toplevel, Object *object, const char *descr, int x, int y);
      GList     *o_pin_realize_attributes   (GedaToplevel *toplevel, Object *object);
const char      *o_pin_get_electrical       (Object *object);
const char      *o_pin_get_label            (Object *object);
const char      *o_pin_get_mechanical       (Object *object);

/* o_selection.c */
      SELECTION *o_selection_new            ( void );
      void       o_selection_add            (SELECTION *selection, Object *o_selected);
      void       o_selection_print_all      (const SELECTION *selection);
      void       o_selection_remove         (SELECTION *selection, Object *o_selected);
      void       o_selection_select         (Object *object);
      void       o_selection_unselect       (Object *object);

/* o_set.c */
      void    o_set_line_options            (Object *o_current, LINE_OPTIONS *line_options);
      void    o_set_fill_options            (Object *o_current, FILL_OPTIONS *fill_options);
      void    o_set_color                   (Object *object, int color);
      void    o_set_visibility              (Object *object, int visibility);

/* o_style.c */
      int     o_style_get_bus_width         (GedaToplevel *toplevel);
      int     o_style_get_line_width        (GedaToplevel *toplevel);
      int     o_style_get_net_width         (GedaToplevel *toplevel);
      int     o_style_get_pin_width         (GedaToplevel *toplevel, int type);
      void    o_style_set_object            (GedaToplevel *toplevel, Object *o_current);

/* o_text_basic.c */
      int     o_text_num_lines              (const char *string);
      Object *o_text_new                    (int color, int x, int y, int alignment, int angle,
                                             const char *string, int size, int visibility, int show_name_value);
      void    o_text_recreate               (Object *o_current);
      void    o_text_translate_world        (int dx, int dy, Object *o_current);
      Object *o_text_copy                   (Object *o_current);
      void    o_text_reset_refdes           (Object *object);
      void    o_text_rotate_world           (int world_centerx, int world_centery, int angle, Object *object);
      void    o_text_mirror_world           (int world_centerx, int world_centery, Object *object);

      double  o_text_get_font_size_in_points     (Object *object);
const char   *o_text_get_string                  (Object *obj);
      void    o_text_set_rendered_bounds_func    (Object *object, RenderedBoundsFunc func, void *user_data);
      void    o_text_set_string                  (Object *obj, const char *new_string);

/* s_attrib.c */
      int     s_attrib_add_entry       (char *new_attrib);
      int     s_attrib_count           (void);
      void    s_attrib_print           (void);
      int     s_attrib_uniq            (char *name);
      void    s_attrib_free            (void);
      void    s_attrib_init            (void);
      char    *s_attrib_get            (int counter);

/* s_basic.c */
      void    print_struct_forw        (GList *list);
      void    print_struct             (Object *ptr);

/* s_clib.c */
      void        s_clib_free                    (void);
      void        s_clib_refresh                 (void);
      GList      *s_clib_get_sources             (const bool sorted);
      bool        s_clib_source_name_exist       (const char *name);
      bool        s_clib_source_path_exist       (const char *path);
const CLibSource *s_clib_get_source_by_name      (const char *name);
const CLibSource *s_clib_add_directory           (const char *directory, const char *name);
const CLibSource *s_clib_add_command             (const char *list_cmd,  const char *get_cmd, const char *name);
const CLibSource *s_clib_add_scm                 (SCM         listfunc,  SCM         getfunc, const char *name);

const char       *s_clib_source_get_name         (const CLibSource *source);
      GList      *s_clib_source_get_symbols      (const CLibSource *source);
const char       *s_clib_symbol_get_name         (const CLibSymbol *symbol);
      char       *s_clib_symbol_get_filename     (const CLibSymbol *symbol);
const CLibSource *s_clib_symbol_get_source       (const CLibSymbol *symbol);
      char       *s_clib_symbol_get_data         (const CLibSymbol *symbol);
      GList      *s_clib_search                  (const char *pattern, const CLibSearchMode mode);
      void        s_clib_flush_search_cache      (void);
      void        s_clib_flush_symbol_cache      (void);
      void        s_clib_symbol_invalidate_data  (const CLibSymbol *symbol);
const CLibSymbol *s_clib_get_symbol_by_name      (const char *name);
      char       *s_clib_symbol_get_data_by_name (const char *name);
      GList      *s_toplevel_get_symbols         (const GedaToplevel *toplevel);


/* s_conn.c */
      void   s_conn_remove_object                (Object *to_remove);
      void   s_conn_update_linear_object         (Object *object);
      void   s_conn_update_object                (Object *object);
      void   s_conn_print                        (GList  *conn_list);
      int    s_conn_net_search                   (Object *new_net,    int     whichone, GList *conn_list);
      GList *s_conn_return_others                (GList  *input_list, Object *object);

      void   s_conn_append_conns_changed_hook    (Page *page,   ConnsChangedFunc func, void *data);
      void   s_conn_emit_conns_changed           (Object *object);
      void   s_conn_freeze_hooks                 (Object *object);
      void   s_conn_thaw_hooks                   (Object *object);

/* s_cue.c */
      void   s_cue_postscript_fillbox            (GedaToplevel *toplevel, FILE *fp, int x, int y);
      void   s_cue_postscript_junction           (GedaToplevel *toplevel, FILE *fp, int x, int y, int bus_involved);
      void   s_cue_output_all                    (GedaToplevel *toplevel, const GList *obj_list, FILE *fp, int type);
      void   s_cue_output_lowlevel               (GedaToplevel *toplevel, Object *object, int whichone, FILE *fp, int output_type);
      void   s_cue_output_lowlevel_midpoints     (GedaToplevel *toplevel, Object *object, FILE *fp, int output_type);
      void   s_cue_output_single                 (GedaToplevel *toplevel, Object *object, FILE *fp, int type);

/* s_hierarchy.c */
      Page  *s_hierarchy_down_schematic_single   (GedaToplevel *toplevel, const char *filename, Page *parent,
                                                  int page_control, int flag, GError **err);
      void   s_hierarchy_down_symbol             (GedaToplevel *toplevel, const CLibSymbol *symbol, Page *parent);
      Page  *s_hierarchy_find_up_page            (PageList *page_list, Page *current_page);
      GList *s_hierarchy_traverse_pages          (GedaToplevel *toplevel, Page *p_current, int flags);
      int    s_hierarchy_print_page              (Page *p_current, void *data);
      Page  *s_hierarchy_find_prev_page          (PageList *page_list, Page *current_page);
      Page  *s_hierarchy_find_next_page          (PageList *page_list, Page *current_page);

/*  s_object.c */
      Object *s_object_new                  (int type, char const *name);
      void    s_object_add                  (Object *parent, Object *child);
      void    s_object_release              (Object *object);
      void    s_object_release_objects      (GList *list);
      void    s_object_set_page_changed     (Object *obj);

/* s_page.c */
      Page   *s_page_new                    (GedaToplevel *toplevel, const char *filename);
const char   *s_page_get_file_extension     (Page *page);
      bool    s_page_is_symbol_file         (Page *page);

      void    s_page_delete                 (GedaToplevel *toplevel, Page *page);
      void    s_page_delete_list            (GedaToplevel *toplevel);
      Page   *s_page_get_current            (GedaToplevel *toplevel);
      bool    s_page_set_current            (GedaToplevel *toplevel, Page *page);
      bool    s_page_goto                   (GedaToplevel *toplevel, Page *page);
      Page   *s_page_search                 (GedaToplevel *toplevel, const char *filename);
      Page   *s_page_search_by_page_id      (PageList *list, int pid);
      void    s_page_print_all              (GedaToplevel *toplevel);
      int     s_page_save_all               (GedaToplevel *toplevel);

      bool    s_page_check_changed          (PageList *list);
      void    s_page_clear_changed          (PageList *list);

      void    s_page_autosave_init          (GedaToplevel *toplevel);
      int     s_page_autosave               (GedaToplevel *toplevel);

      void    s_page_append_object          (Page *page, Object *object);
      void    s_page_append_list            (Page *page, GList *obj_list);

      void    s_page_remove_object          (Page *page, Object *object);
      void    s_page_replace_object         (Page *page, Object *object1, Object *object2);
      void    s_page_delete_objects         (Page *page);
      Object *s_page_get_object             (Page *page, int sid);
      GList  *s_page_get_objects            (Page *page);
      GList  *s_page_objects_in_region      (Page *page, int min_x, int min_y, int max_x, int max_y);
      GList  *s_page_objects_in_regions       (Page *page, RECTANGLE *rects, int n_rects);
      void    s_page_set_bounds_func (Page *page, RenderedBoundsFunc func, void *user_data);

/* s_papersizes.c */
     int   s_papersizes_add_entry           (char *new_papersize, int width, int height);
     void  s_papersizes_print               (void);
     int   s_papersizes_uniq                (char *name);
     void  s_papersizes_free                (void);
     void  s_papersizes_init                (void);
     char *s_papersizes_get                 (int counter);
     void  s_papersizes_get_size            (char *string, int *width, int *height);

/* s_path.c */
     Path *s_path_parse                     (const char *path_str);
     char *s_path_string_from_path          (const Path *path);
     Path *s_path_copy_modify               (Path *path, int dx, int dy, int new_x, int new_y, int whichone);

/* s_place.c */
     void s_place_free_place_list           (GedaToplevel *toplevel);
     void s_place_set_place_list            (GedaToplevel *toplevel, GList *new_place_list );

/* s_slib.c */
     int   s_slib_add_entry                 (char *new_path);
     int   s_slib_search_for_dirname        (char *dir_name);
     char *s_slib_search_dirs               (const char *basename);
     char *s_slib_search_lowlevel           (const char *basename);
     char *s_slib_getbasename               (const char *rawname);
     char *s_slib_search                    (const char *filename, int flag);
     char *s_slib_search_single             (const char *filename);
     void  s_slib_free                      (void);
     void  s_slib_init                      (void);
     char *s_slib_getdir                    (int index);
     char *s_slib_getfiles                  (char *directory, int flag);
     void  s_slib_print                     (void);
     int   s_slib_uniq                      (char *path);
     void  s_slib_print_dirs                (void);

/* s_slot.c */
     char *s_slot_search_slot               (Object *object, Object **return_found);
     void  s_slot_update_object             (Object *object);

/* s_tile.c */
     void   s_tile_update_object            (Object *object);
     GList *s_tile_get_objectlists          (Page   *p_current, int world_x1, int world_y1, int world_x2, int world_y2);

/* s_toplevel.c */
GedaToplevel *s_toplevel_new (void);
      void    s_toplevel_release                      (GedaToplevel *toplevel);
      void    s_toplevel_set_rendered_bounds_func     (GedaToplevel *toplevel, RenderedBoundsFunc func, void *user_data);

/* s_undo.c */
     UNDO    *s_undo_return_tail       (UNDO *head);
     UNDO    *s_undo_return_head       (UNDO *tail);
     UNDO    *s_undo_new_head          (void);
     void     s_undo_destroy_head      (UNDO *u_head);
     UNDO    *s_undo_add               (UNDO *head,  int type,  char *filename, GList *object_list, int left, int top,
                                                     int right, int bottom, int  page_control, int up);
    void      s_undo_print_all         (UNDO *head);
    void      s_undo_destroy_all       (GedaToplevel *toplevel, UNDO *head);
    void      s_undo_remove            (GedaToplevel *toplevel, UNDO *head, UNDO *u_tos);
    void      s_undo_remove_rest       (GedaToplevel *toplevel, UNDO *head);
    int       s_undo_levels            (UNDO *head);
    void      s_undo_init              (Page *p_current);
    void      s_undo_free_all          (GedaToplevel *toplevel, Page *p_current);

/* scheme_color.c */
    SCM       s_color_map_to_scm       (const COLOR *map);
    void      s_color_map_from_scm     (COLOR *map, SCM lst, const char *scheme_proc_name);

/* ---------------- u_basic.c -------------- */
      char   *remove_nl                (char *string);
      char   *remove_last_nl           (char *string);
      char   *int2str                  (int value, char *str, int radix);
      char   *geda_sprintf             (const char *format, ...);
      char   *geda_strdup              (const char *str);
      char   *geda_strndup             (const char *str, size_t n);
      int     geda_stristr             (const char *haystack, const char *needle);
      char   *scm_2_cstring            (char *scm_str_name) G_GNUC_WARN_UNUSED_RESULT;
      void    sort_string_array        (char *strings[], size_t strings_size);
      bool    strequal                 (const char *str1, const char *str2) G_GNUC_WARN_UNUSED_RESULT;
      char   *strstr_rep               (char *original,   const char *old, const char *new);
      int     stricmp                  (const char *str1, const char *str2);
      int     strncmpi                 (const char *str1, const char *str2, int n);
const char   *stristr                  (const char *str1, const char *str2);
      char   *strsubst                 (char *source, char *old_str, char *new_str);
      char   *strisubst                (char *source, char *old_str, char *new_str);
      char   *u_basic_breakup_string   (char *string, char delimiter, int count);
      char   *u_expand_env_variables   (const char *string);
      int     word_count               (char *str);
      void    u_print_object           (Object *object);

/* u_color.c */
      void    u_color_map_defaults     (COLOR *map);
      bool    u_color_rgba_decode      (const char *rgba, guchar *r, guchar *g, guchar *b, guchar *a);
      char   *u_color_rgba_encode      (uint8 r, uint8 g, uint8 b, uint8 a);

/* u_log.c */
      void    u_log_init               (const char *filename);
      void    u_log_close              (void);
      char   *u_log_read               (void);

/* u_program.c */
      void    geda_backtrace           (void);
      void    geda_mem_set_vtable      (void);

G_END_DECLS
