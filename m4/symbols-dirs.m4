# symbols-dirs.m4                                     -*-Autoconf-*-
# serial 1.0

dnl gEDA Symbol Directories
dnl Copyright (C) 2012-2013  Wiley Edward Hill <wileyhill@gmail.com>
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

# Get list of symbol directories.
AC_DEFUN([AX_SYM_DIRS],
[
  AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([Getting symbol directories])
  GEDASYMDIRS=`find symbols/* -maxdepth 0 -type d -printf "%f "`

  AC_MSG_RESULT([$GEDASYMDIRS])

  AC_SUBST([symbol_dirs], [$GEDASYMDIRS])
  []dnl
])dnl AX_DATA_DIRS
