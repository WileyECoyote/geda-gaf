
#include <config.h>

#include <geda/geda_idefines.h>       /* for MAX_COLORS */

#include "../../include/geda_color.h" /* for st_object_color */

int display_cmap_flag = 0;
int outline_cmap_flag = 0;
int print_cmap_flag   = 0;

/* Color Map Structures */
COLOR display_colors[MAX_COLORS];
COLOR outline_colors[MAX_COLORS];
COLOR print_colors[MAX_COLORS];