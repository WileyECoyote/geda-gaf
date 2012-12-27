# geda-pango.m4                                           -*-Autoconf-*-
# serial 1
# Check for PANGO
#
# gEDA - GPL Electronic Design Automation
#
# Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
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
#
# Date: December, 25, 2012
# Contributing Author: Wiley Edward Hill

AC_DEFUN([AX_CHECK_PANGO],
[
  AC_PREREQ([2.60])

  # In Pango >= 1.23.0, check for PangoCairo separately.
  PKG_CHECK_MODULES([PANGO], [pango >= 1.23], [PANGO=yes], [PANGO=no])
  if test "$PANGO" = "yes"; then
      PANGO_PKG=`$PKG_CONFIG --modversion pango`
  else
    PKG_CHECK_MODULES([pango_ok], [pango >= 1.23], [],
      AC_MSG_ERROR([Pango 1.23.0 or later is required.]))
  fi
  AC_SUBST([PANGO_PKG])
])# AX_CHECK_PANGO
