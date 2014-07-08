# geda-automake-options.m4                           -*-Autoconf-*-
# serial 1.0

dnl Conditionally set AutoMake Options Variable
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
dnl Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA

AC_DEFUN([AX_SET_AM_OPTIONS],
[
  AC_PREREQ([2.50])dnl

  m4_ifdef([GEDA_AUTOMAKE_OPTIONS], [m4_undefine([GEDA_AUTOMAKE_OPTIONS])])
  m4_ifdef([GEDA_AM_TEST_OPTIONS], [m4_undefine([GEDA_AM_TEST_OPTIONS])])

  m4_ifdef([AM_SILENT_RULES], ver_flag=yes, ver_flag=no)

dnl 'serial-tests' option disables support for parallel testsuites present
dnl  in recent versions of Automake. Use AM_SILENT_RULES to detect if the
dnl  Automake version >= 1.11 and enable for supporting Automake's.

  if test "x$ver_flag" = "xyes"; then
    GEDA_AUTOMAKE_OPTIONS="1.11 subdir-objects"
    GEDA_AM_TEST_OPTIONS="parallel-tests color-tests"
    AC_SUBST([GEDA_AM_TEST_OPTIONS])
  else
    GEDA_AUTOMAKE_OPTIONS= "1.6 subdir-objects"
  fi
  AC_SUBST([GEDA_AUTOMAKE_OPTIONS])
  []dnl
])dnl

