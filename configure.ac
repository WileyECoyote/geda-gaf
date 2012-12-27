dnl                                                      -*-Autoconf-*-
dnl Process this file with autoconf to generate configure.
dnl
dnl The AX_* macros are defined in files in the top-level m4
dnl directory.

#####################################################################
# Set up configuration system
#####################################################################

AC_INIT([gEDA/gaf], [1.9.0], [https://bugs.launchpad.net/geda], [geda-gaf])
AC_PREREQ([2.60])

AC_CONFIG_SRCDIR([libgeda/src/libgeda.c])
AC_CONFIG_AUX_DIR([build-tools])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADER([config.h])

# Honor aclocal flags
ACLOCAL="$ACLOCAL $ACLOCAL_FLAGS"

AM_INIT_AUTOMAKE

AC_GNU_SOURCE # FIXME for some reason this is needed?

#####################################################################
# Do version number magic
#####################################################################

AX_GIT_VERSION([20121212])

# This is used for keeping the ChangeLog files up-to-date
AC_SUBST([CHANGELOG_BASE], [1.0-20121031])

#####################################################################
# Windows/MinGW/Cygwin support
#####################################################################

AX_WINDOWS_FLAGS

#####################################################################
# Check for compiling & linking tools
#####################################################################

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CPP

AC_LIBTOOL_WIN32_DLL
AC_DISABLE_STATIC
AC_PROG_LIBTOOL

# If automake 1.11 shave the output to look nice
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

# Temporaily Turn Warnings into errors with -Werror option
#AX_GCC_FLAGS([-Wall -Werror])
AX_GCC_FLAGS([-Wall])

AC_PROG_MKDIR_P
AM_PROG_LEX
AX_PROG_AWK
AX_PROG_GROFF
AC_PATH_PROGS([M4], [gm4 m4], [m4])

#####################################################################
# Internationalisation
#####################################################################

AM_NLS
AM_GNU_GETTEXT
AM_GNU_GETTEXT_VERSION([0.16])
AX_DESKTOP_I18N

#####################################################################
# Check for libraries that use pkg-config
#####################################################################

PKG_PROG_PKG_CONFIG

#####################################################################
# Guile stuff
#####################################################################

AX_CHECK_GUILE([1.8.0])

#####################################################################
# Cairo stuff
#####################################################################

AX_CHECK_CAIRO

PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.20.0], ,
  AC_MSG_ERROR([GLib 2.20.0 or later is required.]))

PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.16.0], ,
  AC_MSG_ERROR([GTK+ 2.16.0 or later is required.]))

PKG_CHECK_MODULES(GDK_PIXBUF, [gdk-pixbuf-2.0 >= 2.16.0], ,
  AC_MSG_ERROR([GDK_PIXBUF 2.16.0 or later is required.]))

PKG_CHECK_MODULES(PANGOCAIRO, [pangocairo >= 0.26], ,
  AC_MSG_ERROR([PANGOCAIRO 0.26 or later is required.]))

AX_CHECK_PANGO

#####################################################################
# Header files & particular functions
#####################################################################

AC_HEADER_SYS_WAIT

# These checks are mostly obsolete because every plausible platform
# provides what they check for but we will do check:
AC_HEADER_STDC

# FIXME these checks may be pointless for one of three reasons: every
# even vaguely sane libc implementation provides them; we don't use
# any of their functions in favour of GLib alternatives; or we don't
# provide an alternative, so the build will fail anyway.
AC_CHECK_HEADERS([stdlib.h string.h unistd.h])

# Since we don't have a sensible alternative, if these headers are
# missing we should fail. Also need to remove HAVE_ERRNO_H tests in
# the C source code, since if we *don't* have it the build will fail.
AC_CHECK_HEADERS([errno.h fcntl.h])

# On a system without locale.h, the user may have just disabled NLS
# to be able to build.  But are there known systems with working NLS but
# without a locale.h?  We do need to include locale.h on some systems
# to be able to build gschem/src/gschem.c
AC_CHECK_HEADERS([locale.h])

# to be able to build gschem/src/gschem.c
#AC_CHECK_HEADERS([gdl.h])

# Check for lrint in math library.
AC_CHECK_LIB([m], [lrint],
             AC_DEFINE([HAVE_LRINT], 1,
                       [If your math library has lrint in it, define this]))

AC_CHECK_LIB([m], [atan2])

AC_CHECK_FUNCS([chown getlogin])

# Check if the getopt header is present
AC_CHECK_HEADERS([getopt.h])
# Check for getopt_long
# On FreeBSD the 'gnugetopt' library is needed.
AC_SEARCH_LIBS([getopt_long], [gnugetopt],
               AC_DEFINE([HAVE_GETOPT_LONG], 1,
                         [Define to 1 if you have the `getopt_long' function.]))

# Check for misc features of awk
AX_AWK_FEATURES

#####################################################################
# Optional things
#####################################################################

# Assertions
AX_OPTION_ASSERT
# Viewing documentation
AX_OPTION_URI_VIEWER
# API documentation
AX_OPTION_DOXYGEN
# XDG desktop files
AX_OPTION_XDG_DATA
# KDE 3 desktop files
AX_OPTION_KDE3_DATA
# XDG database updates
AX_OPTION_XDG_DB
# libstroke support
AX_OPTION_STROKE
# gattrib
AX_OPTION_GATTRIB

#####################################################################
# Tool-specific setup
#####################################################################
# Setup the Symbol Source Directories
AX_SYM_DIRS
# Where should data files be installed/searched for?
AX_DATA_DIRS
# Where should PCB footprints be searched for?
AX_PCB_DIRS
# Set up libgeda with the correct ld version number
AX_LIBGEDA([43:0:0])
# Set up libgedacairo with the correct ld version number
AX_LIBGEDACAIRO([1:0:0])

#####################################################################
# Generate output
#####################################################################

AC_CONFIG_FILES([Makefile
                 bitmaps/Makefile
                 intl/Makefile

                 libgeda/Makefile
                 libgeda/libgeda.pc
                 libgeda/po/domain.mak
                 libgeda/po/Makefile.in
                 libgeda/shell/Makefile
                 libgeda/data/Makefile
                 libgeda/docs/Makefile
                 libgeda/docs/images/Makefile
                 libgeda/include/Makefile
                 libgeda/lib/Makefile
                 libgeda/scheme/Makefile
                 libgeda/share/Makefile
                 libgeda/src/Makefile

                 libgedacairo/Makefile
                 libgedacairo/libgedacairo.pc
                 libgedacairo/include/Makefile
                 libgedacairo/src/Makefile

                 gaf/Makefile
                 gaf/po/Makefile.in

                 gschem/Makefile
                 gschem/po/Makefile.in
                 gschem/docs/Makefile
                 gschem/include/Makefile
                 gschem/scripts/Makefile
                 gschem/src/widgets/makefile
                 gschem/src/Makefile
                 gschem/examples/Makefile
                 gschem/lib/Makefile
                 gschem/data/Makefile
                 gschem/bitmap/Makefile
                 gschem/tests/Makefile
                 gschem/scheme/Makefile

                 gattrib/Makefile
                 gattrib/po/Makefile.in
                 gattrib/design/Makefile
                 gattrib/docs/Makefile
                 gattrib/include/Makefile
                 gattrib/include/gtksheet/Makefile
                 gattrib/lib/Makefile
                 gattrib/lib/system-gattribrc
                 gattrib/src/Makefile
                 gattrib/src/gtksheet/Makefile
                 gattrib/data/Makefile

                 gsymcheck/docs/Makefile
                 gsymcheck/include/Makefile
                 gsymcheck/lib/Makefile
                 gsymcheck/lib/system-gsymcheckrc
                 gsymcheck/po/Makefile.in
                 gsymcheck/src/Makefile
                 gsymcheck/tests/Makefile
                 gsymcheck/Makefile

                 gnetlist/Makefile
                 gnetlist/src/Makefile
                 gnetlist/include/Makefile
                 gnetlist/scheme/Makefile
                 gnetlist/examples/Makefile
                 gnetlist/tests/Makefile
                 gnetlist/tests/hierarchy/Makefile
                 gnetlist/tests/hierarchy2/Makefile
                 gnetlist/tests/drc2/Makefile
                 gnetlist/tests/common/Makefile
                 gnetlist/docs/Makefile
                 gnetlist/docs/vams/Makefile
                 gnetlist/examples/vams/Makefile
                 gnetlist/examples/vams/vhdl/Makefile
                 gnetlist/examples/vams/vhdl/basic-vhdl/Makefile
                 gnetlist/examples/vams/vhdl/new-vhdl/Makefile
                 gnetlist/examples/switcap/Makefile
                 gnetlist/lib/Makefile
                 gnetlist/utils/Makefile
                 gnetlist/scripts/Makefile
                 gnetlist/scripts/annotate.sh
                 gnetlist/scripts/bom_xref.sh
                 gnetlist/scripts/bompp.sh
                 gnetlist/lib/system-gnetlistrc

                 utils/Makefile
                 utils/docs/Makefile
                 utils/examples/Makefile
                 utils/examples/gmk_sym/Makefile
                 utils/examples/gsch2pcb/Makefile
                 utils/examples/tragesym/Makefile
                 utils/gsch2pdf/Makefile
                 utils/gschlas/Makefile
                 utils/gxyrs/Makefile
                 utils/include/Makefile
                 utils/lib/Makefile
                 utils/lib/system-gschlasrc
                 utils/scripts/Makefile
                 utils/scripts/sw2asc
                 utils/src/Makefile
                 utils/tests/Makefile
                 utils/tests/gxyrs/Makefile
                 utils/tests/gxyrs/inputs/Makefile
                 utils/tests/gxyrs/outputs/Makefile
                 utils/tests/refdes_renum/Makefile
                 utils/tests/refdes_renum/inputs/Makefile
                 utils/tests/refdes_renum/outputs/Makefile

                 symbols/Makefile

                 docs/Makefile
                 docs/scheme-api/Makefile
                 docs/symbols/Makefile
                 docs/toplevel/Makefile
                 docs/toplevel/gedadocs.html
                 docs/wiki/Makefile

                 examples/Makefile
                 examples/RF_Amp/Makefile
                 examples/RF_Amp/model/Makefile
                 examples/RF_Amp/sym/Makefile
                 examples/TwoStageAmp/Makefile
                 examples/gTAG/Makefile
                 examples/lightning_detector/Makefile
])
AC_OUTPUT
echo "
Configuration:      ${PACKAGE} Ver ${VERSION} ${DATE_VERSION}
                 -------------------------------------
                        Compiler:     ${CC}
                        CPPFLAGS:     ${CPPFLAGS}
                          CFLAGS:     ${CFLAGS}
                         LDFLAGS:     ${LDFLAGS}

                           Guile:     ${GUILE_PKG}
                           Cairo:     ${CAIRO_PKG}
                           Pango:     ${PANGO_PKG}

                     Destination:     ${prefix}
                  LibGeda_Domain:     ${LIBGEDA_GETTEXT_DOMAIN}
                    LibGedaCairo:     ${LIBGEDACAIRO_SHLIB_VERSION}
"
