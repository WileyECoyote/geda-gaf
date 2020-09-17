#include <config.h>

/* System headers which gschem headers rely on */
#include <glib.h>
#include <gtk/gtk.h>

/*! \cond */
#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>
#include <libgedacairo.h>
#include <libgedacolor.h>
#include <libgedauio.h>
/*! \endcond */

/* gschem headers */
#include "gschem_event.h"
#include "gschem_globals.h"
#include "gschem_idefines.h"       /* integer defines */
#include "gschem_macros.h"
#include "gschem_sdefines.h"       /* string defines  */
#include "gschem_page_history.h"
#include "gschem_toplevel.h"
#include "gschem_struct.h"
#include "gschem_search_widget.h"
#include "gschem_status_bar.h"
#include "gschem_types.h"
#include "gschem_page_geometry.h"
#include "gschem_page_view.h"
#include "gschem_patch_widget.h"
#include "gschem_preview.h"
#include "gschem_macro_widget.h"
#include "gschem_main_window.h"
#include "i_vars.h"                /* declarations of structures */
#include "x_status_bar.h"

#include "prototype.h"
