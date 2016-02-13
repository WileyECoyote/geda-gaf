# geda-libgedacolor.m4                                  -*-Autoconf-*-
# serial 1

dnl libgedacolor-specific setup
dnl
dnl Copyright (C) 2015  Peter Brett <peter@peter-b.co.uk>
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
dnl
dnl Date: 09/21/15, Wiley E. Hill
dnl

AC_DEFUN([AX_LIBGEDACOLOR],
[
  AC_PREREQ([2.60])dnl

  # First argument is the shared library version to use.
  AC_MSG_CHECKING([libgedacolor shared library version])
  AC_MSG_RESULT($1)
  AC_SUBST([LIBGEDACOLOR_SHLIB_VERSION], $1)

  # Work out the gettext domain to use
  AC_MSG_CHECKING([libgedacolor gettext domain])
  so_major=`echo $LIBGEDACOLOR_SHLIB_VERSION | sed -e "s/:.*//"`
  LIBGEDACOLOR_GETTEXT_DOMAIN="libgedacolor$so_major"
  AC_MSG_RESULT([$LIBGEDACOLOR_GETTEXT_DOMAIN])
  AC_SUBST([LIBGEDACOLOR_GETTEXT_DOMAIN])
  AC_DEFINE_UNQUOTED([LIBGEDACOLOR_GETTEXT_DOMAIN], ["$LIBGEDACOLOR_GETTEXT_DOMAIN"],
    "Name of libgedacolor's gettext domain.")

  []dnl
])dnl
