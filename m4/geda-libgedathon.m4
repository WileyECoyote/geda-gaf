# geda-libgedathon.m4                                 -*-Autoconf-*-
# serial 1

dnl libgedathon-specific setup
dnl
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
dnl Adlutecated for libgedathon 10/31/13, Wiley E. Hill
dnl
AC_DEFUN([AX_LIBGEDATHON],
[
  AC_PREREQ([2.60])dnl

  # First argument is the shared library version to use.
  AC_MSG_CHECKING([libgedathon shared library version])
  AC_MSG_RESULT($1)
  AC_SUBST([LIBGEDATHON_SHLIB_VERSION], $1)

  # ATTENTION: Trailing spaces are intentional to reserve memory for
  #            the run-time file extension, either .so or .dll
  installpath='/lib/libgedathon    '
  AC_DEFINE_DIR([LIBGEDATHON_PATH], [exec_prefix$installpath],
  [Define to $exec_prefix/lib/libgedathon plus spaces for extension.])

  []dnl
])dnl
