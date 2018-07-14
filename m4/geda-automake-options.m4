# geda-automake-options.m4                           -*-Autoconf-*-
# serial 0.2

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

AC_DEFUN([AX_INIT_AUTOMAKE],
[
  AC_PREREQ([2.50])dnl

  AM_INIT_AUTOMAKE(
    m4_esyscmd([

      GEDA_NEW_AM_OPTS="parallel-tests color-tests"
      GEDA_OLD_AM_OPTS="serial-tests"

      am_version=$(automake --version | { read ver && echo "${ver#*) }"; })

      case $am_version in
        1.1[1-9]*|[2-9]*) echo $GEDA_NEW_AM_OPTS ;;
                       *) echo $GEDA_OLD_AM_OPTS ;;
      esac])
  )

  m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])dnl # make --enable-silent-rules the default.

  []dnl
])
