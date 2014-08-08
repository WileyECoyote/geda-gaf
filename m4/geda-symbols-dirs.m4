# symbols-dirs.m4                                     -*-Autoconf-*-
# serial 1.1

dnl gEDA Symbol Directories
dnl Copyright (C) 2012-2014  Wiley Edward Hill <wileyhill@gmail.com>
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
dnl AX_SYM_DIRS Does NOT set the target locations for the symbol library!
dnl This macro reads subdirectories under the source symbol directories
dnl into the variable GEDASYMDIRS, which is used in the make file to install
dnl the library. This is done here so to allow for new directories to be
dnl added without editing any build files,name the Makefile.am in the symbol
dnl source directory. (The current implementation requires editing geda-clib
dnl .scm or some other initialization file so the directories are scan in at
dnl runtime).

# Get list of symbol directories.
AC_DEFUN([AX_SYM_DIRS],
[
  AC_PREREQ([2.60])dnl

  AC_MSG_CHECKING([Getting symbol directories])
  GEDASYMDIRS=`find symbols/* -maxdepth 0 -type d -printf "%f "`

  if test x$verbose != x ; then
    AC_MSG_RESULT([$GEDASYMDIRS])
  else
    AC_MSG_RESULT([Done])
  fi

  AC_SUBST([symbol_dirs], [$GEDASYMDIRS])
  []dnl
])dnl AX_DATA_DIRS
