# geda-linker-flags.m4              -*-Autoconf-*-
# serial 1.0

dnl gEDA Prebuild checks for Library Headers and Functions
dnl
dnl Copyright (C) 2013-2014  Wiley Edward Hill <wileyhill@gmail.com>
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
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
dnl MA 02110-1301 USA

m4_define([AX_LD_FLAG],
[
  original_LDFLAGS="$LDFLAGS"

  LDFLAGS="$LDFLAGS $1"

  AC_LINK_IFELSE([AC_LANG_PROGRAM([])], [ld_flags_ok=yes], [ld_flags_ok=no])
  if test "x$ld_flags_ok" != "xyes"; then
    LDFLAGS="$1 $original_LDFLAGS"
  else
    LDFLAGS="$original_LDFLAGS"
  fi
  []dnl
])dnl AX_LD_FLAG

AC_DEFUN([AX_SETUP_LD_FLAGS],
[
  AC_PREREQ([2.5])dnl
  AC_REQUIRE([AC_PROG_LD])dnl

  # Have linker produce read-only relocations, if it knows how
  AC_MSG_CHECKING([linker tolerates -z relro])
  AX_LD_FLAG([-Wl,-z,relro])
  AC_MSG_RESULT([$ld_flags_ok])

  #LDFLAGS="-Wl,--no-undefined $LDFLAGS"

  AC_SUBST([LDFLAGS])
  []dnl
])dnl AX_SETUP_LD_FLAGS
