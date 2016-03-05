
/* color_guile.c */
bool     geda_color_guile_load_scheme            (const char *scheme);
void     geda_color_guile_register_handlers      (void);

/* color_struct.c */
void     geda_color_struct_init                  (void);
void     geda_color_struct_release_resources     (void);

/* xcolor.c */
void     geda_color_x11_allocate                 (void);
void     geda_color_x11_free                     (void);
void     geda_color_x11_init                     (void);
int      geda_color_x11_load_scheme              (char *scheme);
void     geda_color_x11_release_resources        (void);