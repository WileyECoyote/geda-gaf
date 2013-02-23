# geda-gtk.m4              -*-Autoconf-*-
# serial 1.0

dnl gEDA Prebuild checks for GTK Library Headers and Functions
dnl
dnl Copyright (C) 2013  Wiley Edward Hill <wileyhill@gmail.com>
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
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


dnl##################################################################
#                     ***  Check GTK stuff   ***
dnl##################################################################
AC_DEFUN([AX_CHECK_GTK],
[
  AC_PREREQ([2.50])dnl
  echo "Checking for installed versions of GTK and gdk_pixbuff"

  PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.16.0], ,
  AC_MSG_ERROR([GTK+ 2.16.0 or later is required.]))

   # Search for gthread
   PKG_CHECK_MODULES(GTHREAD, gthread-2.0, GTHREAD="yes", no_GTHREAD="yes")
   if test "$GTHREAD" = "yes"
   then
       AC_DEFINE(HAVE_GTHREAD, 1, [If gthread support is installed, define this])
   fi

  PKG_CHECK_MODULES(GDK_PIXBUF, [gdk-pixbuf-2.0 >= 2.16.0], ,
  AC_MSG_ERROR([GDK_PIXBUF 2.16.0 or later is required.]))

  []dnl
])dnl AX_CHECK_GTK




