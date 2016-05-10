# geda-x11.m4              -*-Autoconf-*-
# serial 1.2

dnl gEDA Prebuild checks for GTK Library Headers and Functions
dnl
dnl Copyright (C) 2014-2016  Wiley Edward Hill <wileyhill@gmail.com>
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

AC_DEFUN([AX_CHECK_X11],
[
  AC_PREREQ([2.52])dnl

  AC_MSG_NOTICE(checking whether to build X11 bindings)

  AC_ARG_WITH([x], AS_HELP_STRING([--with-x], [Enable X11 routines]), [with_x=$withval], [with_x=yes])

  if test x$with_x = xyes; then

    AC_CHECK_LIB([X11], [XInitThreads], have_x11="yes")

    if test x$have_x11 = xyes ; then
      X11_LIBS="-lX11"
      AC_DEFINE([HAVE_X11], [1], [Define to 1 if have X11])
    else
      with_x=no
      AC_MSG_ERROR(X11 support explicitly requested but X11 was detected, pkg-config --modversion x11)
    fi
  fi

  AC_ARG_ENABLE([Xft], AS_HELP_STRING([--enable-Xft], [Enable FreeType fonts for X11, default no]),
    [case "${enableval}" in
        no) enable_Xft=no ;;
         *) enable_Xft=yes;;
     esac])

  AH_TEMPLATE([HAVE_XFT], [Whether FreeType is installed])

  if test x"$enable_Xft" = xyes -a x"$with_x" = xno; then
      AC_MSG_ERROR(Xft support explicitly requested but X11 was explicitly disabled)
  else
    if test "x$enable_Xft" = xyes; then
      if test "$OS_LINUX"  = yes; then
        AC_CHECK_LIB([Xft], [XftDrawCreate], [XFT=yes], [XFT=no])
      fi
      if test "$XFT" = "yes" ; then
        X11_LIBS="$X11_LIBS -lXft"
        AC_DEFINE([HAVE_XFT], [1], [Define to 1 if have Xft])
      else
        AC_MSG_RESULT([got Xft-dev? pkg-config --modversion xft])
        AC_MSG_ERROR([Xft support explicitly requested but Xft was not found,
 try configuring without --enable-Xft=yes or --with-x --enable-Xft=yes])
      fi
    fi #endif Xft was requested
  fi
  AC_SUBST(X11_LIBS)
  []dnl
])dnl AX_CHECK_X11
