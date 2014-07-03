# geda-stroke.m4                                       -*-Autoconf-*-
# serial 1.1
dnl Check for CAIRO interface library
dnl
dnl gEDA - GPL Electronic Design Automation
dnl
dnl Copyright (C) 2009-2014 Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2012-2014 gEDA Contributors (see ChangeLog for details)
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
dnl
dnl WEH 04/07/14: Changed AC_SUBST([LIBSTROKE_CPPFLAGS]) to AC_SUBST([LIBSTROKE_CFLAGS])
dnl               This is the only occurence of "LIBSTROKE_CPPFLAGS" in the source-tree
dnl               How long has Gschem used libstrokes? Amazingly, strokes has worked all
dnl               these years, even without pkg-config! Breaking the cardinal rule "if it
dnl               ain't broke", just this one time.
dnl               also changed test "X$HAVE_LIBSTROKE" != "Xyes" to 1 in AC_DEFINE, tired
dnl               seeing this in config.h

# Check whether we should use libstroke, and if so if libraries are available.
AC_DEFUN([AX_OPTION_STROKE],
[
  AC_PREREQ([2.60])dnl
  AC_MSG_CHECKING([whether to use libstroke])

  dnl Check what the user wants
  AC_ARG_WITH([libstroke],
    AS_HELP_STRING([--with-libstroke[[[=DIR]]]],
      [use libstroke (search in [[DIR]])]),
    [ if test "X$with_libstroke" = "Xno"; then
        libstroke_use=no
      else
        libstroke_use=yes
        if test "X$with_libstroke" != "Xyes"; then
          libstroke_prefix=$with_libstroke
        fi
      fi
      AC_MSG_RESULT([$libstroke_use]) ],
    [ AC_MSG_RESULT([if present])
  ])

  dnl Check if libstroke is actually available!
  if test "X$libstroke_use" != "Xno"; then

    # If a prefix to search was specified, then add the appropriate flags.
    if test "X$libstroke_use" = "X"; then
      LIBSTROKE_LDFLAGS="-L$libstroke_prefix/lib"
      LIBSTROKE_CFLAGS="-I$libstroke_prefix/include"
    fi

    # Check that the library and header file are available. Save and
    # restore CPPFLAGS and LDFLAGS variables.
    save_CPPFLAGS="$CPPFLAGS"
    save_LDFLAS="$LDFLAGS"
    CPPFLAGS="$CPPFLAGS $LIBSTROKE_CFLAGS"
    LDFLAGS="$LDFLAGS $LIBSTROKE_LDFLAGS"
    HAVE_LIBSTROKE=yes
    AC_CHECK_LIB([stroke], [stroke_init], [], [HAVE_LIBSTROKE=no])
    AC_CHECK_HEADER([stroke.h], [], [HAVE_LIBSTROKE=no
    CPPFLAGS="$save_CPPDFLAGS"
    LDFLAGS="$save_LDFLAGS"

    LIBSTROKE_LDFLAGS="$LIBSTROKE_LDFLAGS -lstroke"])

    dnl If --with-libstroke was specified, then we *must* have a usable
    dnl libstroke.
    if test "X$libstroke_use" = "Xyes" -a "X$HAVE_LIBSTROKE" = "Xno"; then
      AC_MSG_ERROR([You specified that libstroke should be used, but libstroke could not
be found. Ensure that all libstroke development files are installed,
or configure without --with-libstroke.])
    fi
  fi

  # If we don't have libstroke, clear its flags variables
  if test "X$HAVE_LIBSTROKE" != "Xyes"; then
    LIBSTROKE_LDFLAGS=""
    LIBSTROKE_CFLAGS=""
  else
    AC_DEFINE([HAVE_LIBSTROKE], [1], [Define to 1 if libstroke is available])
  fi

  AC_SUBST([LIBSTROKE_CFLAGS])
  AC_SUBST([LIBSTROKE_LDFLAGS])
  []dnl
])
dnl  AX_OPTION_STROKE
