/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */

/* System headers which libgeda headers rely on */

#include <glib.h>
#include <glib-object.h>
#include <libguile.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <glib/gstdio.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include <geda_colors.h>

/* Public libgeda headers */
#include "libgeda/defines.h"

#include "libgeda/g_types.h"
#include "libgeda/o_types.h"

#include "libgeda/s_struct.h"
#include "libgeda/g_struct.h"

#include "libgeda/f_types.h"

#include "libgeda/geda_config.h"
#include "libgeda/geda_errors.h"

#include "libgeda/prototype.h"

#include "libgeda/geda_attrib.h"
#include "libgeda/geda_object_list.h"
#include "libgeda/geda_utility.h"

#include "libgeda/geda_list.h"
#include "libgeda/geda_notify.h"
#include "libgeda/geda_object.h"
#include "libgeda/geda_page.h"
#include "libgeda/geda_line.h"
#include "libgeda/geda_arc.h"
#include "libgeda/geda_box.h"
#include "libgeda/geda_bus.h"
#include "libgeda/geda_circle.h"
#include "libgeda/geda_complex.h"
#include "libgeda/geda_net.h"
#include "libgeda/geda_path.h"
#include "libgeda/geda_picture.h"
#include "libgeda/geda_pin.h"
#include "libgeda/geda_text.h"
#include "libgeda/geda_toplevel.h"

/* Private libgeda headers */
#include "prototype_priv.h"
#include "i_vars_priv.h"

/* Gettext translation */
#include "gettext_priv.h"
