# geda-std-checks.m4                                  -*-Autoconf-*-
# serial 1.1

dnl gEDA Prebuild checks for Library Headers and Functions
dnl
dnl Copyright (C) 2013-2015  Wiley Edward Hill <wileyhill@gmail.com>
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

dnl ####################################################################
#   ***  Checks for typedefs, structures, and compiler characteristics ***
dnl ####################################################################
m4_define([AX_GEDA_TYPES],
[
  AC_PREREQ([2.50])dnl

  echo "Checking Compiler Types used by gEDA programs"

  AC_HEADER_STDBOOL

  dnl Potentially used for olib.c
  AC_TYPE_INT8_T
  AC_TYPE_UINT8_T

  AC_TYPE_INT16_T
  AC_TYPE_UINT16_T

  AC_TYPE_INT32_T
  AC_TYPE_UINT32_T

  AC_TYPE_SIZE_T
  AC_TYPE_SSIZE_T       dnl scheme_object.c
  AC_TYPE_MODE_T        dnl Used in gschem/src/o_misc.c

  AC_CHECK_TYPES([ptrdiff_t])

  []dnl
])dnl AX_GEDA_TYPES

dnl #####################################################################
#                     ***  Check Math stuff   ***
dnl #####################################################################
m4_define([AX_GEDA_MATH],
[
  AC_PREREQ([2.50])dnl

  echo "Checking for Math functions used by gEDA programs"
  dnl Check for rint & lrint in math library.

  AC_CHECK_LIB([m], [lrint],
     AC_DEFINE([HAVE_LRINT], 1,
               [If local math library has lrint, define this]))

  AC_CHECK_LIB([m], [rint],
     AC_DEFINE([HAVE_RINT], 1,
               [If local math library has rint, define this]))

  AC_CHECK_LIB([m], [atan2])

  AC_CHECK_HEADERS([float.h])

  AC_CHECK_FUNCS([hypot])              dnl is not ANSI C
  AC_CHECK_FUNCS([pow])                dnl used by gschem & libgeda
  AC_CHECK_FUNCS([floor])              dnl used by pango
  AC_CHECK_FUNCS([sqrt])


  []dnl
])dnl AX_GEDA_MATH

dnl##################################################################
#                    ***  Check Memory stuff   ***
dnl##################################################################
m4_define([AX_GEDA_MEMORY],
[
  AC_PREREQ([2.50])dnl
  echo "Checking for Memory functions used by gEDA programs"

  AC_CHECK_HEADERS([malloc.h])

  AC_CHECK_FUNCS([alloc])
  AC_CHECK_FUNCS([malloc])
  AC_CHECK_FUNCS([realloc])
  AC_CHECK_FUNCS([memmove])
  AC_FUNC_MMAP

  []dnl
])dnl AX_GEDA_MEMORY

dnl##################################################################
#                     ***  Check File stuff   ***
dnl##################################################################
m4_define([AX_GEDA_FSC],
[
  AC_PREREQ([2.50])dnl
  echo "Checking for File Systems functions used by gEDA programs"

  dnl Since we don't have a sensible alternative, if these headers are
  dnl missing we should fail. Also need to remove HAVE_ERRNO_H tests in
  dnl the C source code, since if we *don't* have it the build will fail.
  AC_CHECK_HEADERS([errno.h fcntl.h])

  AC_CHECK_FUNCS([mkdir])

  AX_FUNC_MKDIR

  AC_FUNC_CHOWN                         # used to manipulate ownership in f_basic.c

  AC_CHECK_FUNCS([getlogin])            # used in f_print.c

  AC_CHECK_FUNCS([popen])                # used in f_print.c

  AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK # used by f_basic.c

  AC_CHECK_FUNCS([realpath])            # used by geda_config.c (and libtool)

  []dnl
])dnl AX_GEDA_FSC

dnl##################################################################
#                     ***  Check String stuff   ***
dnl##################################################################
m4_define([AX_GEDA_STR_FUNCS],
[
  AC_PREREQ([2.50])dnl

  echo "Checking for String functions used by gEDA programs"

  AC_CHECK_HEADERS([string.h strings.h])

  AC_FUNC_STRTOD

  AC_CHECK_FUNCS([strerror strstr])

  AC_CHECK_FUNCS([strchr strrchr strndup])

  AC_CHECK_FUNCS([strcspn])

  AC_CHECK_FUNCS([vasprintf])

  AX_FUNC_SNPRINTF      dnl This also tests vsnprintf

  []dnl
])dnl AX_GEDA_STR_FUNCS

dnl##################################################################
#                   ***  Check Miscellaneous stuff   ***
dnl##################################################################
m4_define([AX_GEDA_MISC],
[
  AC_PREREQ([2.50])dnl

  dnl Standard Stuff that we reference throughout the tree
  AC_CHECK_HEADERS([stdlib.h unistd.h stddef.h stdint.h stdio_ext.h ])

  dnl On a system without locale.h, the user may have just disabled NLS
  dnl to be able to build.  But are there known systems with working NLS but
  dnl without a locale.h?  We do need to include locale.h on some systems
  dnl to be able to build gschem/src/gschem.c
  AC_CHECK_HEADERS([locale.h])

  dnl Used in gschem/x/x_misc.c
  AC_HEADER_SYS_WAIT

  dnl Used by gschem/src/x_dialog.c::about_dialog
  AC_CHECK_HEADERS([gnu/libc-version.h])

  dnl Used by libgeda\src\utility\u_log.c
  AC_CHECK_HEADERS([syslog.h])

  dnl Used by utils/src/gmk_sym.c & libgeda/src/o_basic.c
  AC_CHECK_HEADERS([sys/time.h])

  dnl Check if the getopt header is present
  AC_CHECK_HEADERS([getopt.h])

  dnl Check for getopt_long, 'gnugetopt' library is needed on FreeBSD.
  AC_SEARCH_LIBS([getopt_long], [gnugetopt],
               AC_DEFINE([HAVE_GETOPT_LONG], 1,
                         [Define to 1 if you have the `getopt_long' function.]))

  dnl Check for misc features of awk
  AX_AWK_FEATURES

  AC_CHECK_FUNCS([atexit])

  []dnl
])dnl AX_GEDA_STR_MISC

AC_DEFUN([AX_GEDA_STD_CHECKS],
[
  AC_PREREQ([2.50])dnl
  AX_GEDA_TYPES
  AX_GEDA_MATH
  AX_GEDA_MEMORY
  AX_GEDA_FSC
  AX_GEDA_STR_FUNCS
  AX_GEDA_MISC
  []dnl
])dnl AX_GEDA_STD_CHECKS

