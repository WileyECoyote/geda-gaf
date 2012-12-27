# geda-cairo.m4                                           -*-Autoconf-*-
# serial 1
# Check for CAIRO
#
# gEDA - GPL Electronic Design Automation
#
# Copyright (C) 2009  Dan McMahill <dan@mcmahill.net>
# Copyright (C) 2010-2013  Peter Brett <peter@peter-b.co.uk>
# Copyright (C) 2012-2013 gEDA Contributors (see ChangeLog for details)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

AC_DEFUN([AX_CHECK_CAIRO],
[
  AC_PREREQ([2.60])dnl

  # In Cairo >= 1.10, need to check for Cairo PDF/SVG/PS/PNG support
  # separately.
  PKG_CHECK_MODULES([CAIRO], [cairo >= 1.10], [CAIRO=yes], [CAIRO=no])
  if test "$CAIRO" = "yes"; then
    PKG_CHECK_MODULES([CAIRO_PNG], [cairo-png >= 1.10], ,
      AC_MSG_ERROR([Cairo PNG support 1.10.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_PDF], [cairo-pdf >= 1.10], ,
      AC_MSG_ERROR([Cairo PDF support 1.10.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_PS], [cairo-ps >= 1.10], ,
      AC_MSG_ERROR([Cairo PostScript support 1.10.0 or later is required.]))
    PKG_CHECK_MODULES([CAIRO_SVG], [cairo-svg >= 1.10], ,
      AC_MSG_ERROR([Cairo SVG support 1.10.0 or later is required.]))
      CAIRO_PKG=`$PKG_CONFIG --modversion cairo`
  else
    PKG_CHECK_MODULES([cairo_ok], [cairo >= 1.8], [],
      AC_MSG_ERROR([Cairo 1.8.0 or later is required.]))
  fi
  AC_SUBST([CAIRO_PKG])
])# AX_CHECK_CAIRO
