# geda-intl
# serial 1.1 (gettext-0.18)
dnl
dnl Copyright (C) 2017 Wiley Edward Hill <wileyhill@gmail.com>
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License version 2 as
dnl published by the Free Software Foundation.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
dnl 02110-1301 USA
dnl
dnl Authors:
dnl   Wiley Edward Hill <wileyhill@gmail.com>, Jan 08, 2017

AC_PREREQ([2.50])

AC_DEFUN([AX_LOCAL_LIBINTL],
[
  dnl Set USE_NLS.
  AC_REQUIRE([AM_NLS])

  if test "X$USE_NLS" = "Xyes"; then

    dnl Set USE_INCLUDED_LIBINTL.
    AC_REQUIRE([AM_GNU_GETTEXT])dnl

    AC_MSG_CHECKING([if building with local libintl])

    if test "$USE_INCLUDED_LIBINTL" = yes; then
      with_local_libintl=yes
    fi
    AC_MSG_RESULT([$with_local_libintl])

  fi

  AM_CONDITIONAL([LOCAL_LIBINTL], test "X$with_local_libintl" = "Xyes")

  if test "X$with_local_libintl" = "Xyes"; then
    AC_DEFINE(USE_LOCAL_LIBINTL, 1, [Define to 0 when using the system libintl.])
  fi
  []dnl
])dnl
