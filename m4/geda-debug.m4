# geda-debug.m4                                        -*-Autoconf-*-
# serial 1.0

dnl gEDA Prebuild checks for Library Headers and Functions
dnl
dnl Copyright (C) 2014  Wiley Edward Hill <wileyhill@gmail.com>
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

dnl##################################################################
#                       ***  Dmalloc   ***
dnl##################################################################
AC_DEFUN([AX_GEDA_DMALLOC],
[
  AC_PREREQ([2.50])dnl

  with_dmalloc=no

  AC_MSG_CHECKING([if dmalloc debugging should be enabled])

       AC_ARG_ENABLE([dmalloc],
       [  --enable-dmalloc        Compile and link with dmalloc for malloc debugging [default=no]],
       [
       if test "X$enable_dmalloc" != "Xno" ; then
	        AC_MSG_RESULT([yes])
	        AC_CHECK_HEADER(dmalloc.h,,
		AC_MSG_ERROR([You have requested dmalloc debugging but dmalloc.h could not be found]))
	        AC_CHECK_LIB(dmalloc,main,,
		AC_MSG_ERROR([You have requested dmalloc debugging but -ldmalloc could not be found]))
	        DMALLOC_LIBS="-ldmalloc"
	        with_dmalloc=yes
        else
	        AC_MSG_RESULT([no])
	        DMALLOC_LIBS=""
        fi
       ],
       [
	        AC_MSG_RESULT([no])
	        DMALLOC_LIBS=""
        ])

  []dnl
])dnl AX_GEDA_DMALLOC


dnl##################################################################
#                     ***  ElectricFence   ***
dnl##################################################################

dnl# Used to detect buffer owerflows or touch-after-free cases

AC_DEFUN([AX_ELECTRIC_FENCE],
[
  AC_PREREQ([2.50])dnl

  with_efence=no

  AC_MSG_CHECKING([if ElectricFence debugging should be enabled])
	AC_ARG_ENABLE([efence],
	[  --enable-efence         Link with ElectricFence for malloc debugging [default=no]],
	[
	if test "X$enable_efence" != "Xno" ; then
		AC_MSG_RESULT([yes])
		AC_CHECK_LIB(efence,main,,
			AC_MSG_ERROR([You have requested ElectricFence debugging but -lefence could not be found]))
			with_efence=yes
	else
		AC_MSG_RESULT([no])
	fi
	],
	[
		AC_MSG_RESULT([no])
	])

  []dnl
])dnl AX_ELECTRIC_FENCE

AX_CHECK_DEBUGGERS

dnl##################################################################
#           ***  Check wheather to include Debug Code  ***
dnl##################################################################

AC_DEFUN([AX_ENABLE_DEBUGGING],
[
  AC_PREREQ([2.50])dnl

  AC_MSG_CHECKING([for whether to enable debugging code])
  	AC_ARG_ENABLE([debug],
	[  --enable-debug          Enable debugging code],
	[],[enable_debug=no])

	AC_MSG_RESULT([$enable_debug])
	AM_CONDITIONAL(DEBUG_BUILD, test x$enable_debug = xyes)

  []dnl
])dnl AX_ENABLE_DEBUGGING

dnl##################################################################
#          ***  Check wheather to enable Debugging Tools   ***
dnl##################################################################

AC_DEFUN([AX_CHECK_DEBUGGING],
[
  AC_PREREQ([2.50])dnl

# --------------- Enable Debug code? -----------------
  AX_ENABLE_DEBUGGING

# ------------- Link with ElectricFence? -------------
  AX_ELECTRIC_FENCE

# ----------------- Enable Dmalloc? ------------------
  AX_GEDA_DMALLOC

  []dnl
])dnl AX_CHECK_DEBUGGING
