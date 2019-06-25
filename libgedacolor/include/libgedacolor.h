/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */

#ifndef __LIBGEDACOLOR_H__
#define __LIBGEDACOLOR_H__

#ifndef WITHOUT_GUILE
#  include <libguile.h>
#else
#define SCM void*
#endif

#include "geda_colormaps.h"
#include "geda_colors.h"

#ifdef __cplusplus
extern "C" {
#endif

/* libgedacolor.c */

         int     libgedacolor_init               (int *argc, char **argv);
         int     geda_color_load_display_scheme  (char *scheme);
         int     geda_color_load_print_scheme    (char *scheme);
         void    libgedacolor_release            (void);

/* guile.c */
          SCM    geda_color_guile_display_map    (SCM scm_map);
          SCM    geda_color_guile_outline_map    (SCM scm_map);
          SCM    geda_color_guile_print_map      (SCM scm_map);

/* color_guile.c */
          SCM    geda_color_guile_map_to_scm     (const COLOR *map);
         void    geda_color_guile_map_from_scm   (COLOR *map, SCM lst, const char *scheme_proc_name);

/* color_get.c */
         char   *geda_color_get_color_name       (int index, GArray *map, GError **err);
          int    geda_color_get_object_default   (char type);

       GArray   *geda_color_get_display_map      (void);
       GArray   *geda_color_get_standard_names   (void);
       GArray   *geda_color_get_outline_map      (void);
       GArray   *geda_color_get_print_map        (void);
         char   *geda_color_get_print_color      (int color);

/* color_key.c */
          int    geda_color_key_get_index        (const char *name);

/* ---------------- Utilities -------------- */

/* color_utility.c */
         bool    geda_color_utility_decode_rgba  (const char *rgba, guchar *r, guchar *g, guchar *b, guchar *a);
         char   *geda_color_utility_encode_rgba  (uint8 r, uint8 g, uint8 b, uint8 a);
         char   *geda_color_utility_get_hex      (COLOR *c);
         char   *geda_color_utility_lookup_name  (COLOR *c1, GError **err);
         char   *geda_color_utility_postscript   (int color);

/* xcolor.c */
#ifdef __GDK_COLOR_H__
     GdkColor   *geda_color_x11_color_from_index (int color);
#endif
        COLOR   *geda_color_x11_display_lookup   (int color);
         bool    geda_color_x11_get_state        (int color);
         void    geda_color_x11_set_state        (int color, int state);

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* !__LIBGEDACOLOR_H__ */
