# geda-pango.m4                                           -*-Autoconf-*-
# serial 1

dnl Prebuild Check for PANGO
dnl
dnl gEDA - GPL Electronic Design Automation
dnl
dnl Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
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
dnl
dnl Date: December, 25, 2012
dnl Contributing Author: Wiley Edward Hill

AC_DEFUN([AX_CHECK_PANGO],
[
  AC_PREREQ([2.52])

  # In Pango >= 1.23.0, check for PangoCairo separately.
  PKG_CHECK_MODULES([PANGO], [pango >= 1.23], [PANGO=yes], [PANGO=no])
  if test "$PANGO" = "yes"; then
      PANGO_PKG=`$PKG_CONFIG --modversion pango`
  else
    PKG_CHECK_MODULES([pango_ok], [pango >= 1.23], [],
      AC_MSG_ERROR([Pango 1.23.0 or later is required.]))
  fi
  AC_SUBST([PANGO_PKG])
  []dnl
])dnl AX_CHECK_PANGO
