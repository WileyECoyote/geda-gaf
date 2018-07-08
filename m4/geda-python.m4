# geda-python.m4                                      -*-Autoconf-*-
# serial 1.4

dnl gEDA Prebuild Checks and Setup Options for Python
dnl
dnl Copyright (C) 2013-2016  Wiley Edward Hill <wileyhill@gmail.com>
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

dnl **************************************************
dnl * optional python bindings
dnl **************************************************

AC_DEFUN([AX_PYTHON_MOD_PATH],
[
  AC_REQUIRE([AX_HOST])dnl

  AC_MSG_CHECKING(getting the path to python)

  if test "$OS_WIN32" = "yes"; then
    py_prefix=`$PYTHON -c "import os, sys; print sys.prefix.replace(os.sep,'/')"`
    PYTHON_MODDIR="${py_prefix}/Lib/site-packages"
  else
    py_prefix=`$PYTHON -c "import sys; print sys.prefix"`
    PYTHON_MODDIR="${py_prefix}/local/lib/python${PYTHON_VERSION}/site-packages"
  fi

  AC_SUBST([PYTHON_MODDIR])
  []dnl
])dnl

dnl copied from pygtk
dnl
dnl a macro to check for ability to create python extensions
dnl  AX_CHECK_PYTHON_HEADERS([ACTION-IF-POSSIBLE], [ACTION-IF-NOT-POSSIBLE])
dnl function also defines PYTHON_IFLAGS

AC_DEFUN([AX_CHECK_PYTHON_HEADERS],
[
  AC_REQUIRE([AM_PATH_PYTHON])
  AC_REQUIRE([AX_HOST])dnl

  AC_MSG_CHECKING(for python development headers)

  dnl deduce PYTHON_IFLAGS

  if test "$OS_WIN32" = "yes"; then
    py_prefix=`$PYTHON -c "import os, sys; print sys.prefix.replace(os.sep,'/')"`
    py_exec_prefix=`$PYTHON -c "import os, sys; print sys.exec_prefix.replace(os.sep,'/')"`
    PYTHON_IFLAGS="-I${py_prefix}/include"
  else
    py_prefix=`$PYTHON -c "import sys; print sys.prefix"`
    py_exec_prefix=`$PYTHON -c "import sys; print sys.exec_prefix"`
    PYTHON_IFLAGS="-I${py_prefix}/include/python${PYTHON_VERSION}"
  fi

  if test "$py_prefix" != "$py_exec_prefix"; then
    if test "$OS_WIN32" = "yes"; then
      PYTHON_IFLAGS="$PYTHON_IFLAGS -I${py_exec_prefix}/include"
    else
      PYTHON_IFLAGS="$PYTHON_IFLAGS -I${py_exec_prefix}/include/python${PYTHON_VERSION}"
    fi
  fi
  AC_SUBST(PYTHON_IFLAGS)

  dnl check if the headers exist:
  save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$PYTHON_IFLAGS"
  AC_TRY_CPP([#include <Python.h>],dnl
  [AC_MSG_RESULT(found)
    $1],dnl
  [AC_MSG_RESULT(not found)
    $2])
  CPPFLAGS="$save_CPPFLAGS"
  []dnl
])dnl

dnl **************************************************
dnl * PYGOBJECT is optional
dnl **************************************************

AC_DEFUN([AX_CHECK_PYOBJECT],
[

  AC_ARG_ENABLE([pygobject], AS_HELP_STRING([--disable-pygobject], [Python API will lack GdkPixbuf support without PyGOBJECT]),
  [case "${enableval}" in
    no) enable_pygobject=no ;;
     *) enable_pygobject=yes;;
   esac])

   AH_TEMPLATE([HAVE_PYGOBJECT], [Whether pygobject is installed, Python API will lack GdkPixbuf support without PyGOBJECT])
   if test x$enable_pygobject != xno; then
     PKG_CHECK_MODULES(PYGOBJECT, pygobject-2.0 >= 2.8.0, have_pygobject=yes, have_pygobject=no)
     if test x"$enable_pygobject" = xyes -a x"$have_pygobject" = xno; then
       AC_MSG_ERROR([pygobject support explicitly requested but pygobject couldn't be found])
     fi
  fi

  if test x"$have_pygobject" != xyes; then
    AC_DEFINE_UNQUOTED(HAVE_PYGOBJECT, 1)
  else
    AC_DEFINE_UNQUOTED(HAVE_PYGOBJECT, 0)
  fi

  AC_SUBST(PYGOBJECT_CFLAGS)
  AC_SUBST(PYGOBJECT_LIBS)
  []dnl
])dnl

dnl copied and modified from gnome-python
dnl
dnl AX_CHECK_PYMOD(MODNAME [,VERSION, VERSION_MATCHER [,ACTION-IF-FOUND [,ACTION-IF-NOT-FOUND]]])
dnl Check if a module of a particular version is visible to python.

AC_DEFUN([AX_CHECK_PYMOD],
  [AC_REQUIRE([AM_PATH_PYTHON])
  py_mod_var=`echo $1`
  AC_MSG_CHECKING(for python module $1 ifelse([$2],[],,[>= $2]))
  AC_CACHE_VAL(py_cv_mod_$py_mod_var, [
    ifelse([$2],[], [prog="
    import sys
    try:
           import $1
    except ImportError:
           sys.exit(1)
    except:
           sys.exit(0)
    sys.exit(0)"], [prog="
    import sys, string, $1
    curverstr = $3

    # use method from AM_PYTHON_CHECK_VERSION
    minver = map(int, string.split('$2', '.'))
    length = len[(minver)]
    minver += [[0, 0, 0]]
    minverhex = 0
    for i in xrange(0, 4): minverhex = (minverhex << 8) + minver[[i]]
    curver = map(int, string.split(curverstr, '.')[[:length]])
    curver += [[0, 0, 0]]
    curverhex = 0
    for i in xrange(0, 4): curverhex = (curverhex << 8) + curver[[i]]
      if (curverhex >= minverhex):
          sys.exit(0)
      else:
          sys.exit(1)
      sys.exit(0)"])
    if $PYTHON -c "$prog" 1>&AC_FD_CC 2>&AC_FD_CC
      then
        eval "py_cv_mod_$py_mod_var=yes"
      else
        eval "py_cv_mod_$py_mod_var=no"
    fi
  ])

  py_val=`eval "echo \`echo '$py_cv_mod_'$py_mod_var\`"`
  if test "x$py_val" != xno; then
    AC_MSG_RESULT(yes)
  ifelse([$4], [],, [$4
    ])dnl
  else
    AC_MSG_RESULT(no)
    ifelse([$5], [],, [$5
    ])dnl
  fi
  []dnl
])dnl

dnl If flags for either compiler or linker are changed here, don't forget
dnl to update the REF notes in the Makefile.am files
AC_DEFUN([AX_CHECK_PYTHON],
[
    dnl aclocal-1.7 is missing this when version is used in AM_PATH_PYTHON, fudge it
    am_display_PYTHON=python

    AC_ARG_WITH(python, AC_HELP_STRING([--with-python=PATH], [build python bindings [[default=yes]]]),
    [with_python=$withval], [with_python=yes])

    AC_MSG_CHECKING(whether to build python bindings)
    if test "X$with_python" != Xno; then
       if test "X$with_python" != Xyes; then
          PYTHON=$with_python
       fi
       with_python=yes
    fi
    AC_MSG_RESULT($with_python)

    if test "X$with_python" = Xyes; then
        if test -z "$PYTHON"; then
            AC_PATH_PROG(PYTHON, python)
        fi

        if test -n "$PYTHON"; then

            AM_PATH_PYTHON($PYTHON_MIN_VERSION)
            AX_PYTHON_MOD_PATH
            AX_CHECK_PYTHON_HEADERS(have_python="yes",have_python="no")
            AX_CHECK_PYOBJECT

            echo "test for python ldflags"
            if test "X$have_python" = Xyes; then

                dnl copied from the Redland RDF bindings, http://librdf.org/
                if test `uname` = Darwin; then
                    PYTHON_LDFLAGS="-Wl,-F. -bundle"
                    if $PYTHON -c 'import sys, string; sys.exit(string.find(sys.prefix,"Framework")+1)'; then
                        :
                    else
                        PYTHON_LDFLAGS="$PYTHON_LDFLAGS -framework Python"
                        PYTHON_LIBS="-lpython$PYTHON_VERSION"
                    fi
                else
                    PYTHON_LIBS="-lpython$PYTHON_VERSION"
                    dnl #-pthread
                    PYTHON_LDFLAGS="-shared -Bsymbolic-functions"
                fi
                PYTHON_CFLAGS="-Wstrict-prototypes -pthread -fno-strict-aliasing -fwrapv -fPIC"
                AC_SUBST(PYTHON_LDFLAGS)
                AC_SUBST([PYTHON_LIBS])
                AC_SUBST([PYTHON_CFLAGS])

                dnl check for mutagen module >= $PYTHON_MUTAGEN_MIN_VERSION
                dnl AX_CHECK_PYMOD(mutagen,$PYTHON_MUTAGEN_MIN_VERSION,mutagen.version_string,,have_python=no)
                dnl this test should perhaps be re-enabled, but only produce a warning -- tmz

                if test "X$have_gdkpixbuf" = "Xyes" -a "X$have_pygobject" = "Xyes"; then
                    dnl check for gtk module >= $PYTHON_GTK_MIN_VERSION
                    AX_CHECK_PYMOD(gtk,$PYTHON_GTK_MIN_VERSION,'.'.join(map(str, gtk.ver)),,have_python=no)
                fi
            fi
        else
            AC_MSG_WARN(python not found.  try --with-python=/path/to/python)
            have_python="no"
        fi
    fi
    AM_CONDITIONAL(HAVE_PYTHON, test x$have_python = xyes)
  []dnl
])dnl
