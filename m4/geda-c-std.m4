# geda-c-std.m4                                  -*-Autoconf-*-
# serial 1.0

dnl gEDA Prebuild checks for Library Headers and Functions
dnl
dnl Copyright (C) 2017  Wiley Edward Hill <wileyhill@gmail.com>
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

AC_DEFUN([AX_GCC_STD],
[
  AC_PREREQ([2.50])dnl
  AC_REQUIRE([AX_GCC_FLAGS])dnl

  echo -n "Checking which code standard to enforce... "

  AC_ARG_WITH([std-c99], AS_HELP_STRING([--with-std-c99],
    [Enforce c99 instead of gnu99 standards]),

    [ #action-if-present
       AX_GCC_FLAGS([-std=c99])
       mesg="c99"
    ],

    [#action-if-not-present
       AX_GCC_FLAGS([-std=gnu99])
       mesg="gnu99"
    ]
  )
  AC_MSG_RESULT([$mesg])
  []dnl
])dnl AX_GCC_STD
