# geda-check-program.m4                               -*-Autoconf-*-
# serial 1.0

dnl gEDA Prebuild Checks and Setup for Programs
dnl
dnl Copyright (C) 2013  Wiley Edward Hill <wileyhill@gmail.com>
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
dnl These checks were relocated from configure.ac to this file in order
dnl suppress output to the configure file. The programs listed here are
dnl required for the build but not necessarily required for run-time.

AC_DEFUN([AX_CHECK_PROGRAMS],
[
  AC_PREREQ([2.50])dnl

  AC_PROG_MKDIR_P
  AM_PROG_LEX
  AX_PROG_GROFF
  AC_PATH_PROGS([M4], [gm4 m4], [m4])dnl
  AC_PROG_YACC
  AX_PROG_AWK

  []dnl
])dnl
