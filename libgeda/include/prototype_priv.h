/* f_print.c */
void     f_print_set_line_width              (FILE *fp, int width);
int      f_print_set_color                   (GedaToplevel *toplevel, FILE *fp, int color);
int      f_print_header                      (GedaToplevel *toplevel, Page *page, FILE *fp, int paper_size_x, int paper_size_y, int eps, bool landscape);
void     f_print_footer                      (FILE *fp);
void     f_print_objects                     (GedaToplevel *toplevel, FILE *fp, const GList *obj_list,
                                              int start_x, int start_y, float scale,
                                              int unicode_count, gunichar *unicode_table);
int      f_print_initialize_glyph_table      (void);

/* g_rc.c */
SCM      g_rc_component_groups               (SCM stringlist);
SCM      g_rc_component_library              (SCM path, SCM name);
SCM      g_rc_component_library_command      (SCM listcmd, SCM getcmd, SCM name);
SCM      g_rc_component_library_funcs        (SCM listfunc, SCM getfunc, SCM name);
SCM      g_rc_component_search_directory     (SCM path, SCM name);

SCM      g_rc_source_library                 (SCM path);
SCM      g_rc_source_library_search          (SCM path);

SCM      g_rc_reset_component_library        (void);
SCM      g_rc_reset_source_library           (void);

SCM      g_rc_bus_style                      (SCM mode);
SCM      g_rc_line_style                     (SCM mode);
SCM      g_rc_net_style                      (SCM mode);
SCM      g_rc_pin_style                      (SCM mode);

SCM      g_rc_thick_bus_width                (SCM mode);
SCM      g_rc_thick_line_width               (SCM mode);
SCM      g_rc_thick_net_width                (SCM mode);
SCM      g_rc_thick_pin_width                (SCM mode);

SCM      g_rc_thin_bus_width                 (SCM mode);
SCM      g_rc_thin_line_width                (SCM mode);
SCM      g_rc_thin_net_width                 (SCM mode);
SCM      g_rc_thin_pin_width                 (SCM mode);

SCM      g_rc_always_promote_attributes      (SCM scmsymname);
SCM      g_rc_attribute_promotion            (SCM mode);
SCM      g_rc_keep_invisible                 (SCM mode);

SCM      g_rc_bitmap_directory               (SCM path);
SCM      g_rc_log_directory                  (SCM path);
SCM      g_rc_scheme_directory               (SCM path);

SCM      g_rc_check_symbol_version           (SCM mode);
SCM      g_rc_log_time                       (SCM mode);
SCM      g_rc_make_backup_files              (SCM mode);
SCM      g_rc_postscript_prolog              (SCM scmsymname);
SCM      g_rc_promote_invisible              (SCM mode);

SCM      g_rc_untitled_name                  (SCM name);
SCM      g_rc_show_full_path                 (SCM mode);

/* g_register.c */
void     g_register_rc_handlers              (void);
void     g_register_libgeda_dirs             (void);

/* geda_object.c */
int      geda_object_get_next_sid            (void);
void     geda_object_append_new_hook         (NewObjectFunc func, void *data);
void     geda_object_unref                   (GedaObject *object);

/* geda_page.c */
void     geda_page_append_new_hook           (NewPageFunc func, void *data);
void     geda_page_append_conns_changed_hook (Page *page, ConnsChangedFunc func, void *data);
void     geda_page_unref                     (Page *page);
void     geda_page_set_toplevel              (Page *page, GedaToplevel *toplevel);
GedaToplevel  *geda_page_get_toplevel        (Page *page);

/* geda_toplevel.c */
void     geda_toplevel_append_new_hook       (NewToplevelFunc func, void *data);
bool     geda_toplevel_set_text_bounds       (GedaToplevel *toplevel, GedaObject *o_current);
void     geda_toplevel_unref                 (GedaToplevel *toplevel);

/* m_transform.c */
void     m_transform_combine      (TRANSFORM *result, TRANSFORM *a, TRANSFORM *b );
void     m_transform_init         (TRANSFORM *transform);
void     m_transform_invert       (TRANSFORM *transform, TRANSFORM *inverse);
void     m_transform_line         (TRANSFORM *transform, LINE *line );
void     m_transform_lines        (TRANSFORM *transform, GArray *lines);
void     m_transform_point        (TRANSFORM *transform, int *x, int *y);
void     m_transform_points       (TRANSFORM *transform, GArray *points);
void     m_transform_rotate       (TRANSFORM *transform, double angle);
void     m_transform_scale        (TRANSFORM *transform, double factor);
void     m_transform_translate    (TRANSFORM *transform, double dx, double dy);

/* o_arc_object.c */
bool        geda_arc_object_get_position         (GedaObject *object, int *x, int *y);
void        geda_arc_object_print                (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
void        geda_arc_object_print_center         (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                                  int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_arc_object_print_dashed         (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                                  int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_arc_object_print_dotted         (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                                  int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_arc_object_print_phantom        (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                                  int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_arc_object_print_solid          (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                                  int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
GedaObject *geda_arc_object_read                 (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
double      geda_arc_object_shortest_distance    (ConstObject *object, int x, int y, int force_soild);
char       *geda_arc_object_to_buffer            (GedaObject *object);

/* o_attrib.c */
GList      *geda_attrib_object_read              (GedaToplevel *toplevel, GedaObject *receiving_object, TextBuffer *tb,
                                                  unsigned int release_ver,  unsigned int fileformat_ver, GError **err);

/* o_box_basic.c */
bool        geda_box_object_get_position         (GedaObject *object, int *x, int *y);
void        geda_box_object_print                (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
void        geda_box_object_print_center         (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_box_object_print_dashed         (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_box_object_print_dotted         (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_box_object_print_filled         (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void        geda_box_object_print_hatch          (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void        geda_box_object_print_mesh           (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void        geda_box_object_print_phantom        (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_box_object_print_solid          (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
GedaObject *geda_box_object_read                 (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
double      geda_box_object_shortest_distance    (ConstObject *object, int x, int y, int force_soild);
char       *geda_box_object_to_buffer            (GedaObject *object);

/* o_bus_object.c */
bool        geda_bus_object_get_position         (GedaObject *object, int *x, int *y);
void        geda_bus_object_print                (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
GedaObject *geda_bus_object_read                 (const char buf[], unsigned int release_ver,
                                                  unsigned int fileformat_ver, GError **err);
char       *geda_bus_object_to_buffer            (GedaObject *object);

/* o_circle_object.c */
bool        geda_circle_object_get_position      (GedaObject *object, int *x, int *y);
void        geda_circle_object_print             (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
void        geda_circle_object_print_center      (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_circle_object_print_dashed      (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_circle_object_print_dotted      (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_circle_object_print_filled      (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void        geda_circle_object_print_hatch       (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void        geda_circle_object_print_mesh        (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void        geda_circle_object_print_phantom     (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_circle_object_print_solid       (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                                  int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
GedaObject *geda_circle_object_read              (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
double      geda_circle_object_shortest_distance (ConstObject *object, int x, int y, int force_soild);
char       *geda_circle_object_to_buffer         (GedaObject *object);

/* o_complex_object.c */
int         geda_complex_object_get_bounds       (GedaObject *complex);
bool        geda_complex_object_get_position     (GedaObject *object, int *x, int *y);
GList      *geda_complex_object_get_promotable   (GedaToplevel *toplevel, GedaObject *object, int detach);
GedaObject *geda_complex_object_read             (GedaToplevel *toplevel, const char buf[], unsigned int release_ver,
                                                  unsigned int fileformat_ver, GError **err);
double      geda_complex_object_shortest_distance(ConstObject *object, int x, int y, int force_soild);
char       *geda_complex_object_to_buffer        (GedaObject *object);

/* o_get.c */
int         geda_object_get_capstyle             (LINE_END end);

/* o_line_object.c */
bool        geda_line_object_get_position        (GedaObject *object, int *x, int *y);
void        geda_line_object_print               (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
void        geda_line_object_print_center        (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_line_object_print_dashed        (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_line_object_print_dotted        (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_line_object_print_phantom       (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void        geda_line_object_print_solid         (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
GedaObject *geda_line_object_read                (const char buf[], unsigned int release_ver,
                                                  unsigned int fileformat_ver, GError **err);
double      geda_line_object_shortest_distance   (ConstObject *object, int x, int y, int force_soild);
char       *geda_line_object_to_buffer           (GedaObject *object);

/* o_net_object.c */
bool        geda_net_object_get_position         (GedaObject *object, int *x, int *y);
void        geda_net_object_print                (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
GedaObject *geda_net_object_read                 (const char buf[], unsigned int release_ver,
                                                  unsigned int fileformat_ver, GError **err);
char       *geda_net_object_to_buffer            (GedaObject *object);

/* o_notify.c */
void        geda_object_notify_emit_pre_change   (GedaObject *object);
void        geda_object_notify_emit_change       (GedaObject *object);

/* o_object.c */
bool        geda_object_show_buffer_err          (const char *msg, const char *buffer);

/* o_path_object.c */
bool        geda_path_object_get_position        (GedaObject *object, int *x, int *y);
void        geda_path_object_print               (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
GedaObject *geda_path_object_read                (const char *first_line, TextBuffer *tb,
                                                  unsigned int release_ver, unsigned int fileformat_ver, GError **err);
double      geda_path_object_shortest_distance   (ConstObject *object, int x, int y, int force_soild);
char       *geda_path_object_to_buffer           (GedaObject *object);

/* o_picture.c */
bool        geda_picture_object_embed            (GedaObject *object);
bool        geda_picture_object_get_position     (GedaObject *object, int *x, int *y);
GedaObject *geda_picture_object_read             (const char *first_line, TextBuffer *tb,
                                                  unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char       *geda_picture_object_save             (GedaObject  *object);
double      geda_picture_object_shortest_distance(ConstObject *object, int x, int y, int force_soild);
bool        geda_picture_object_unembed          (GedaObject  *object);

/* o_pin_object.c */
bool        geda_pin_object_get_position         (GedaObject *object, int *x, int *y);
void        geda_pin_object_print                (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y);
GedaObject *geda_pin_object_read                 (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char       *geda_pin_object_save                 (GedaObject *object);
void        geda_pin_object_update_read_property (GedaObject *o_pin, GedaObject *o_text);

/* o_text_object.c */
bool        geda_text_object_get_position            (GedaObject *object, int *x, int *y);
void        geda_text_object_print_text_string       (FILE *fp, char *string, int unicode_count, gunichar *unicode_table);
void        geda_text_object_print                   (GedaToplevel *toplevel, FILE *fp, GedaObject *o_current, int origin_x, int origin_y,
                                                      int unicode_count, gunichar *unicode_table);
GedaObject *geda_text_object_read                    (const char *first_line, TextBuffer *tb, unsigned int release_ver,
                                                      unsigned int fileformat_ver, GError **err);
char       *geda_text_object_save                    (GedaObject  *object);
double      geda_text_object_shortest_distance       (ConstObject *object, int x, int y, int force_soild);
void        geda_text_object_update_disp_string      (GedaObject  *object);

/* s_attrib.c */
 void       geda_struct_attrib_init                  (void);
void        geda_struct_attrib_free                  (void);

/* s_clib.c */
void        geda_struct_clib_init                    (void);
void        geda_struct_clib_free                    (void);
GList      *geda_struct_clib_get_symbols             (const GedaToplevel *toplevel);

/* s_conn.c */
CONN       *geda_struct_conn_return_new              (GedaObject *other_object, int type, int x, int y, int whichone, int other_whichone);
int         geda_struct_conn_uniq                    (GList  *conn_list, CONN *input_conn);
int         geda_struct_conn_remove_other            (GedaObject *other_object, GedaObject *to_remove);
GedaObject *geda_struct_conn_check_midpoint          (GedaObject *o_current, int x, int y);
void        geda_struct_conn_init                    (void);

/* s_papersizes.c */
void        geda_struct_papersizes_free     (void);

/* s_slib.c */
void        geda_struct_slib_free           (void);
void        geda_struct_slib_init           (void);

/* s_tile.c */
void        geda_struct_tile_init                   (Page *p_current);
void        geda_struct_tile_add_object             (GedaObject *object);
void        geda_struct_tile_remove_object          (GedaObject *object);
void        geda_struct_tile_print                  (GedaToplevel *toplevel, Page *page);
void        geda_struct_tile_free_all               (Page *p_current);

/* s_weakref.c */
void     s_weakref_notify              (void *dead_ptr, GList *weak_refs);
GList   *s_weakref_add                 (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList   *s_weakref_remove              (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList   *s_weakref_add_ptr             (GList *weak_refs, void **weak_pointer_loc);
GList   *s_weakref_remove_ptr          (GList *weak_refs, void **weak_pointer_loc);
