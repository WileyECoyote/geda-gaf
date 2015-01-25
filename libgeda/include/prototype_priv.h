/* f_print.c */
void f_print_set_line_width            (FILE *fp, int width);
int  f_print_set_color                 (GedaToplevel *toplevel, FILE *fp, int color);
int  f_print_header                    (GedaToplevel *toplevel, Page *page, FILE *fp, int paper_size_x, int paper_size_y, int eps, bool landscape);
void f_print_footer                    (FILE *fp);
void f_print_objects                   (GedaToplevel *toplevel, FILE *fp, const GList *obj_list,
                                        int start_x, int start_y, float scale,
                                        int unicode_count, gunichar *unicode_table);
int  f_print_initialize_glyph_table    (void);

/* g_rc.c */
int  vstbl_lookup_str                  (const vstbl_entry *table, int size, const char *str);
int  vstbl_get_val                     (const vstbl_entry *table, int index);

SCM  g_rc_component_groups             (SCM stringlist);
SCM  g_rc_component_library            (SCM path, SCM name);
SCM  g_rc_component_library_command    (SCM listcmd, SCM getcmd, SCM name);
SCM  g_rc_component_library_funcs      (SCM listfunc, SCM getfunc, SCM name);

SCM  g_rc_source_library               (SCM path);
SCM  g_rc_source_library_search        (SCM path);

SCM  g_rc_reset_component_library      (void);
SCM  g_rc_reset_source_library         (void);

SCM  g_rc_bus_style                    (SCM mode);
SCM  g_rc_line_style                   (SCM mode);
SCM  g_rc_net_style                    (SCM mode);
SCM  g_rc_pin_style                    (SCM mode);

SCM  g_rc_thick_bus_width              (SCM mode);
SCM  g_rc_thick_line_width             (SCM mode);
SCM  g_rc_thick_net_width              (SCM mode);
SCM  g_rc_thick_pin_width              (SCM mode);

SCM  g_rc_thin_bus_width               (SCM mode);
SCM  g_rc_thin_line_width              (SCM mode);
SCM  g_rc_thin_net_width               (SCM mode);
SCM  g_rc_thin_pin_width               (SCM mode);

SCM  g_rc_always_promote_attributes    (SCM scmsymname);
SCM  g_rc_attribute_promotion          (SCM mode);
SCM  g_rc_bitmap_directory             (SCM path);
SCM  g_rc_keep_invisible               (SCM mode);
SCM  g_rc_make_backup_files            (SCM mode);
SCM  g_rc_postscript_prolog            (SCM scmsymname);
SCM  g_rc_print_color_map              (SCM scm_map);
SCM  g_rc_promote_invisible            (SCM mode);
SCM  g_rc_scheme_directory             (SCM path);
SCM  g_rc_untitled_name                (SCM name);
SCM  g_rc_show_full_path               (SCM mode);

/* g_register.c */
void g_register_libgeda_funcs          (void);
void g_register_libgeda_dirs           (void);

/* geda_object.c */
int            geda_object_get_next_sid            (void);
void           geda_object_append_new_hook         (NewObjectFunc func, void *data);
void           geda_object_unref                   (Object *object);

/* geda_page.c */
void           geda_page_append_new_hook           (NewPageFunc func, void *data);
void           geda_page_append_conns_changed_hook (Page *page, ConnsChangedFunc func, void *data);
void           geda_page_unref                     (Page *page);
void           geda_page_set_toplevel              (Page *page, GedaToplevel *toplevel);
GedaToplevel  *geda_page_get_toplevel              (Page *page);

/* geda_toplevel.c */
void           geda_toplevel_append_new_hook       (NewToplevelFunc func, void *data);
bool           geda_toplevel_set_bounds            (GedaToplevel *toplevel, Object *o_current);
void           geda_toplevel_unref                 (GedaToplevel *toplevel);

/* m_bounds.c */
void   m_bounds_init                   (BOUNDS *bounds);
/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */

/* m_transform.c */
void m_transform_combine       (TRANSFORM *result, TRANSFORM *a, TRANSFORM *b );
void m_transform_init          (TRANSFORM *transform);
void m_transform_invert        (TRANSFORM *transform, TRANSFORM *inverse);
void m_transform_line          (TRANSFORM *transform, LINE *line );
void m_transform_lines         (TRANSFORM *transform, GArray *lines);
void m_transform_point         (TRANSFORM *transform, int *x, int *y);
void m_transform_points        (TRANSFORM *transform, GArray *points);
void m_transform_rotate        (TRANSFORM *transform, double angle);
void m_transform_scale         (TRANSFORM *transform, double factor);
void m_transform_translate     (TRANSFORM *transform, double dx, double dy);

/* o_arc_basic.c */
Object  *o_arc_read               (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char    *o_arc_save               (Object *object);
void     o_arc_print              (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
void     o_arc_print_solid        (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                   int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_arc_print_dotted       (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                   int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_arc_print_dashed       (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                   int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_arc_print_center       (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                   int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_arc_print_phantom      (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2,
                                   int color, int arc_width, int capstyle, int length, int space, int origin_x, int origin_y);
double   o_arc_shortest_distance  (Object *object, int x, int y, int force_soild);
bool     o_arc_within_sweep       (Arc *arc, int x, int y);
bool     o_arc_get_position       (int *x, int *y, Object *object);

/* o_attrib.c */
GList   *o_read_attribs           (GedaToplevel *toplevel, Object *receiving_object, TextBuffer *tb,
                                   unsigned int release_ver,  unsigned int fileformat_ver, GError **err);

/* o_box_basic.c */
Object  *o_box_read              (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char    *o_box_save              (Object *object);
void     o_box_print             (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
void     o_box_print_solid       (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_box_print_dotted      (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_box_print_dashed      (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                 int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_box_print_center      (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_box_print_phantom     (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_box_print_filled      (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void     o_box_print_mesh        (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void     o_box_print_hatch       (GedaToplevel *toplevel, FILE *fp, int x, int y, int width, int height, int color,
                                  int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
double   o_box_shortest_distance  (Object *object, int x, int y, int force_soild);
bool     o_box_get_position       (int *x, int *y, Object *object);

/* o_bus_basic.c */
Object  *o_bus_read               (const char buf[], unsigned int release_ver,
                                   unsigned int fileformat_ver, GError **err);
char    *o_bus_save               (Object *object);
void     o_bus_print              (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
bool     o_bus_get_position       (int *x, int *y, Object *object);

/* o_circle_basic.c */
Object  *o_circle_read            (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char    *o_circle_save            (Object *object);
void     o_circle_print           (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
void     o_circle_print_solid     (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_circle_print_dotted    (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_circle_print_dashed    (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_circle_print_center    (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_circle_print_phantom   (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int circle_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_circle_print_filled    (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void     o_circle_print_mesh      (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void     o_circle_print_hatch     (GedaToplevel *toplevel, FILE *fp, int x, int y, int radius, int color,
                                   int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
double   o_circle_shortest_distance    (Object *object, int x, int y, int force_soild);
bool     o_circle_get_position         (int *x, int *y, Object *object);

/* o_complex_basic.c */
Object  *o_complex_read                (GedaToplevel *toplevel, const char buf[], unsigned int release_ver,
                                        unsigned int fileformat_ver, GError **err);
char    *o_complex_save                (Object *object);
double   o_complex_shortest_distance   (Object *object, int x, int y, int force_soild);
int      o_complex_get_world_bounds    (Object *complex);
bool     o_complex_get_position        (int *x, int *y, Object *object);
GList   *o_complex_get_promotable      (GedaToplevel *toplevel, Object *object, int detach);

/* o_get.c */
int      o_get_capstyle                (LINE_END end);
double   o_get_shortest_distance_full  (Object *object, int x, int y, int force_solid);

/* o_line_basic.c */
Object  *o_line_read                   (const const char buf[], unsigned int release_ver,
                                        unsigned int fileformat_ver, GError **err);
char    *o_line_save                   (Object *object);
void     o_line_print                  (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
void     o_line_print_solid            (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                        int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_line_print_dotted           (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                        int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_line_print_dashed           (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                        int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_line_print_center           (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                        int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
void     o_line_print_phantom          (GedaToplevel *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color,
                                        int line_width, int capstyle, int length, int space, int origin_x, int origin_y);
double   o_line_shortest_distance      (Object *object, int x, int y, int force_soild);
bool     o_line_get_position           (int *x, int *y, Object *object);

/* o_net_basic.c */
Object  *o_net_read                    (const char buf[], unsigned int release_ver,
                                        unsigned int fileformat_ver, GError **err);
char    *o_net_save                    (Object *object);
void     o_net_print                   (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
bool     o_net_get_position            (int *x, int *y, Object *object);

/* o_notify.c */
void     o_notify_emit_pre_change      (Object *object);
void     o_notify_emit_change          (Object *object);

/* o_path_basic.c */
Object  *o_path_read                   (const char *first_line, TextBuffer *tb,
                                        unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char    *o_path_save                   (Object *object);
void     o_path_print                  (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
double   o_path_shortest_distance      (Object *object, int x, int y, int force_soild);
bool     o_path_get_position           (int *x, int *y, Object *object);

/* o_picture.c */
Object  *o_picture_read                (const char *first_line, TextBuffer *tb,
                                        unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char    *o_picture_save                (Object *object);
double   o_picture_shortest_distance   (Object *object, int x, int y, int force_soild);
bool     o_picture_embed               (Object *object);
bool     o_picture_unembed             (Object *object);
bool     o_picture_get_position        (int *x, int *y, Object *object);

/* o_pin_basic.c */
bool     o_pin_get_position            (int *x, int *y, Object *object);
void     o_pin_print                   (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y);
Object  *o_pin_read                    (const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);
char    *o_pin_save                    (Object *object);
void     o_pin_update_read_property    (Object *o_pin, Object *o_text);

/* o_text_basic.c */
Object  *o_text_read                   (const char *first_line, TextBuffer *tb, unsigned int release_ver,
                                        unsigned int fileformat_ver, GError **err);
char    *o_text_save                   (Object *object);
void     o_text_print_text_string      (FILE *fp, char *string, int unicode_count, gunichar *unicode_table);
void     o_text_print                  (GedaToplevel *toplevel, FILE *fp, Object *o_current, int origin_x, int origin_y,
                                        int unicode_count, gunichar *unicode_table);
double   o_text_shortest_distance      (Object *object, int x, int y, int force_soild);
bool     o_text_get_position           (int *x, int *y, Object *object);

/* s_clib.c */
void     s_clib_init                   (void);

/* s_color.c */
void     u_color_init                  (void);
char    *u_color_postscript_string     (int color);

/* s_conn.c */
CONN    *s_conn_return_new             (Object *other_object, int type, int x, int y, int whichone, int other_whichone);
int      s_conn_uniq                   (GList  *conn_list, CONN *input_conn);
int      s_conn_remove_other           (Object *other_object, Object *to_remove);
Object  *s_conn_check_midpoint         (Object *o_current, int x, int y);
void     s_conn_print                  (GList  *conn_list);
void     s_conn_init                   (void);

/* s_encoding.c */
char    *s_encoding_base64_encode      (char* src, unsigned int srclen, unsigned int* dstlenp, bool strict);
char    *s_encoding_base64_decode      (char* src, unsigned int srclen, unsigned int* dstlenp);

/* s_textbuffer.c */
TextBuffer   *s_textbuffer_new         (const char *data, const int size);
TextBuffer   *s_textbuffer_free        (TextBuffer *tb);
const char   *s_textbuffer_next        (TextBuffer *tb, const gssize count);
const char   *s_textbuffer_next_line   (TextBuffer *tb);

/* s_tile.c */
void     s_tile_init                   (Page *p_current);
void     s_tile_add_object             (Object *object);
void     s_tile_remove_object          (Object *object);
void     s_tile_print                  (GedaToplevel *toplevel, Page *page);
void     s_tile_free_all               (Page *p_current);

/* s_weakref.c */
void     s_weakref_notify              (void *dead_ptr, GList *weak_refs);
GList   *s_weakref_add                 (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList   *s_weakref_remove              (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList   *s_weakref_add_ptr             (GList *weak_refs, void **weak_pointer_loc);
GList   *s_weakref_remove_ptr          (GList *weak_refs, void **weak_pointer_loc);
