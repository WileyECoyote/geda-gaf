# geda-cairo.m4                                           -*-Autoconf-*-
# serial 1
dnl Check for CAIRO
dnl
dnl gEDA - GPL Electronic Design Automation
dnl
dnl Copyright (C) 2009  Dan McMahill <dan@mcmahill.net>
dnl Copyright (C) 2010-2013  Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2012-2013 gEDA Contributors (see ChangeLog for details)
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

AC_DEFUN([AX_CHECK_CAIRO],
[
  AC_PREREQ([2.52])dnl

  dnl In Cairo >= 1.10, need to check for Cairo PDF/SVG/PS/PNG support
  dnl separately.
  PKG_CHECK_MODULES([CAIRO], [cairo >= 1.8], [CAIRO=yes], [CAIRO=no])
  if test "$CAIRO" = "yes"; then
    PKG_CHECK_MODULES([CAIRO_PNG], [cairo-png >= 1.8], ,
      AC_MSG_ERROR([Cairo PNG support 1.8.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_PDF], [cairo-pdf >= 1.8], ,
      AC_MSG_ERROR([Cairo PDF support 1.8.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_PS], [cairo-ps >= 1.8], ,
      AC_MSG_ERROR([Cairo PostScript support 1.8.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_SVG], [cairo-svg >= 1.8], ,
      AC_MSG_ERROR([Cairo SVG support 1.8.0 or later is required.]))
      CAIRO_PKG=`$PKG_CONFIG --modversion cairo`
  else
    PKG_CHECK_MODULES([cairo_ok], [cairo >= 1.8], [],
      AC_MSG_ERROR([Cairo 1.8.0 or later is required.]))
  fi
  AC_SUBST([CAIRO_PKG])
  []dnl
])dnl AX_CHECK_CAIRO
