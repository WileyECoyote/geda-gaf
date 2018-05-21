## $Id$
##
## There does not seem to be any reason to use Auto-tools here
## Just use static Makefile

gedaincludedir = $(includedir)/geda

gedainclude_HEADERS = \
			geda/ansi.h \
			geda/ascii.h \
			geda/geda.h \
			geda/geda_bitmaps.h \
			geda/geda_dialog_controls.h \
			geda/geda_enum.h \
			geda/geda_gui_funcs.h \
			geda/geda_help.h \
			geda/geda_idefines.h \
			geda/geda_sdefines.h \
			geda/geda_macros.h \
			geda/geda_stat.h \
			geda/geda_standard.h \
			geda/geda_struct.h \
			geda/geda_system.h \
			geda/geda_toolbars.h \
			geda/geda_types.h \
			geda/geda_wrap.h \
			geda/missing.h

noinst_HEADERS  = \
			geda_debug.h \
			geda_diagnostics.h \
			gettext.h

EXTRA_DIST      = $(noinst_HEADERS)

distcheck:

MOSTLYCLEANFILES        = *~ geda/*~
CLEANFILES              = *~ geda/*~
DISTCLEANFILES          = *~ geda/*~
MAINTAINERCLEANFILES    = *~ geda/*~ Makefile.in ChangeLog
