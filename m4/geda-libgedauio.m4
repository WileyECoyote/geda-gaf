# geda-libgedauio.m4                                  -*-Autoconf-*-
# serial 1.2

dnl libgedacairo-specific setup
dnl Copyright (C) 2010-2014  Peter Brett <peter@peter-b.co.uk>
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
dnl Adlutecated for libgedauio 03/12/14, Wiley E. Hill
dnl
# Work out the gettext domain that libgedacairo should use
AC_DEFUN([AX_LIBGEDAUIO],
[
  AC_PREREQ([2.60])dnl

  # First argument is the shared library version to use.
  AC_MSG_CHECKING([libgedauio shared library version])
  AC_MSG_RESULT($1)
  AC_SUBST([LIBGEDAUIO_SHLIB_VERSION], $1)

  # Work out the gettext domain to use
  AC_MSG_CHECKING([libgedauio gettext domain])
  so_major=`echo $LIBGEDAUIO_SHLIB_VERSION | sed -e "s/:.*//"`
  LIBGEDAUIO_GETTEXT_DOMAIN="libgedauio$so_major"
  AC_MSG_RESULT([$LIBGEDAUIO_GETTEXT_DOMAIN])
  AC_SUBST([LIBGEDAUIO_GETTEXT_DOMAIN])
  AC_DEFINE_UNQUOTED([LIBGEDAUIO_GETTEXT_DOMAIN], ["$LIBGEDAUIO_GETTEXT_DOMAIN"],
    "Name of libgedauio's gettext domain.")

  LIBGEDAUIO_DOT_VERSION=`echo $LIBGEDAUIO_SHLIB_VERSION | sed -e "y/:/./"`
  AC_SUBST([LIBGEDAUIO_DOT_VERSION])

  []dnl
])dnl
