#  ifdef HAVE_CONFIG_H
#    include "config.h"
#  endif

/* System headers which gschem headers rely on */
#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>

#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>
#include <libgedacairo.h>

#include <geda_uio_functions.h>

/* gschem headers */
#include "gschem_event.h"
#include "gschem_idefines.h"       /* integer defines */
#include "gschem_sdefines.h"       /* string defines  */
#include "gschem_toplevel.h"
#include "gschem_struct.h"
#include "gschem_status_bar.h"
#include "gschem_types.h"
#include "gschem_page_geometry.h"
#include "gschem_page_view.h"
#include "gschem_macro_widget.h"
#include "gschem_main_window.h"
#include "i_vars.h"                /* declarations of structures */
#include "x_preview.h"
#include "x_status_bar.h"

#include "globals.h"
#include "prototype.h"

