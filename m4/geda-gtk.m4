# geda-gtk.m4              -*-Autoconf-*-
# serial 1.4

dnl gEDA Prebuild checks for GTK Library Headers and Functions
dnl
dnl Copyright (C) 2013-2015  Wiley Edward Hill <wileyhill@gmail.com>
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA


dnl##################################################################
#                     ***  Check GTK stuff   ***
dnl##################################################################
AC_DEFUN([AX_CHECK_GTK],
[
  AC_PREREQ([2.50])dnl

  min_ver=$1

  if test x"$min_ver" = x; then
    AC_MSG_ERROR([Configuration error: minimum GTK+2 version was not specified.])
  fi

  echo "Checking for installed versions of GTK and gdk_pixbuff"

  PKG_CHECK_MODULES(GTK, gtk+-2.0 >= $min_ver, ,
  AC_MSG_ERROR([GTK+ $1 or later is required.]))

  GDK_CFLAGS="`pkg-config --cflags gdk-2.0`"
  GDK_LIBS="`pkg-config --libs gdk-2.0`"

  AC_SUBST([GDK_CFLAGS])
  AC_SUBST([GDK_LIBS])

   # Search for gthread
  PKG_CHECK_MODULES(GTHREAD, gthread-2.0, GTHREAD="yes", no_GTHREAD="yes")
  if test "$GTHREAD" = "yes"
  then
       AC_DEFINE(HAVE_GTHREAD, 1, [If gthread support is installed, define this])
  fi

  PKG_CHECK_MODULES(GDK_PIXBUF, [gdk-pixbuf-2.0 >= $1], ,
  AC_MSG_ERROR([GDK_PIXBUF $1 or later is required.]))

  AC_ARG_ENABLE(
        [deprecated],
        AC_HELP_STRING(
                [--disable-deprecated],
                [enable/disable deprecated functions (e.g. Glib/Gtk) @<:@default=yes@:>@]
        ),
        [
         if test "x$enableval" = "xno"; then
                 CFLAGS="$CFLAGS -DG_DISABLE_SINGLE_INCLUDES -DGTK_DISABLE_SINGLE_INCLUDES"
                 CFLAGS="$CFLAGS -DGDK_DISABLE_DEPRECATED -DGDK_PIXBUF_DISABLE_DEPRECATED"
                 CFLAGS="$CFLAGS -DG_DISABLE_DEPRECATED"
                 CFLAGS="$CFLAGS -DGTK_DISABLE_DEPRECATED"
                 CFLAGS="$CFLAGS -DGSEAL_ENABLE"
         fi
        ]
  )

  save_CFLAGS="$CFLAGS"
  save_LIBS="$LIBS"

  CFLAGS="$CFLAGS $GDK_PIXBUF_CFLAGS"
  LIBS="$LIBS $GDK_PIXBUF_LIBS"

  AC_CHECK_FUNCS([gdk_pixbuf_loader_write])

  CFLAGS="$CFLAGS $GTK_CFLAGS"
  CPPFLAGS="$CPPFLAGS $GTK_CFLAGS"
  LIBS="$LIBS $GTK_LIBS"

  AC_CHECK_HEADERS([gdk/gdkkeysyms-compat.h])

  AC_CHECK_FUNCS([gtk_window_group_get_current_grab])
  AC_CHECK_FUNCS([gtk_show_uri gdk_window_get_width])

  CFLAGS="$save_CFLAGS"
  LIBS="$save_LIBS"

  []dnl
])dnl AX_CHECK_GTK




