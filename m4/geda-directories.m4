# geda-directories.m4                                    -*-Autoconf-*-
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
dnl WEH: Note 1.) Functions, other than the last one (AX_EXPAND_DIRS)
dnl               do not get loaded, so comments that look like they
dnl               will be loaded, really aren't.

AC_PREFIX_DEFAULT(/)

AC_ARG_WITH(docdir, AS_HELP_STRING([--with-docdir],[set path for documentation]),docdir="$withval",docdir="")
AC_ARG_WITH(logdir, AS_HELP_STRING([--with-logdir],[set path for log files]),logdir="$withval",logdir="")
AC_ARG_WITH(templatedir, AS_HELP_STRING([--with-templatedir],[set path for template documents]),templatedir="$withval",templatedir="")

dnl##################################################################
#                    ***  Systemic Paths   ***
dnl##################################################################

dnl Fix "prefix" variable if it hasn't been specified...
if test "$prefix" = "NONE"; then
        prefix="/"#fi

dnl Fix "exec_prefix" variable if it hasn't been specified...
if test "$exec_prefix" = "NONE"; then
        if test "$prefix" = "/"; then
                exec_prefix="/usr"
        else
                exec_prefix="$prefix"
        fi
fi

dnl Fix "sysconfdir" variable if it hasn't been specified...
if test "$sysconfdir" = "\${prefix}/etc"; then
        if test "$prefix" = "/"; then
                sysconfdir="/etc"
        else
                sysconfdir="$prefix/etc"
        fi
fi

dnl##################################################################
#                      ***  Data Directory   ***
dnl##################################################################
AC_DEFUN([AX_DATA_DIRS],
[
  AC_PREREQ([2.60])dnl

  GEDA_DATA_DIR=$1

  # Check where to install ordinary data files (e.g. symbols and bitmaps)

  AC_MSG_CHECKING([where to install gEDA shared data])
  datadir="$datarootdir/$GEDA_DATA_DIR"

  AC_DEFINE_DIR([GEDADATADIR], [datadir], [gEDA/gaf shared data directory])
  AC_MSG_NOTICE([only libgeda should use this - apps should use geda_sys_data_path()])
  AC_MSG_RESULT([$GEDADATADIR])
  AC_SUBST([GEDADATADIR])

  []dnl
])dnl AX_DATA_DIRS

dnl##################################################################
#                 ***  Documentation Directory   ***
dnl##################################################################
m4_define([AX_DOC_DIRS],
[
  AC_PREREQ([2.60])dnl

  # Check where to install doc files. Note that Autoconf will overide
  # this to ${datarootdir}/doc/${PACKAGE_TARNAME}

  AC_MSG_CHECKING([where to install Help Documents])
  if test "x$docdir" = "x"; then
     docdir="/doc/geda-gaf"
     AC_DEFINE_DIR([GEDADOCDIR], [docdir], [Location for Help documents])
  else
     docdir="$datarootdir/doc/geda-gaf"
     AC_DEFINE_DIR([GEDADOCDIR], [docdir], [Help Documents directory])
  fi

  AC_MSG_RESULT([$GEDADOCDIR])

  []dnl
])dnl AX_DOC_DIRS

dnl##################################################################
#                   ***  Log Directory   ***
dnl##################################################################
m4_define([AX_LOG_DIRS],
[
  AC_PREREQ([2.60])dnl

  # Check where to write log files.

  GEDA_LOG_DIR=$1

  AC_MSG_CHECKING([where to write log files])
  if test "x$logdir" = "x"; then
     logdir="~/.$GEDA_LOG_DIR/logs"
     AC_DEFINE_DIR([GEDALOGDIR], [logdir], [Logging location])
  else
     GEDALOGDIR="$logdir"
     AC_DEFINE_UNQUOTED(GEDALOGDIR, "$logdir")
  fi
  AC_MSG_RESULT([$GEDALOGDIR])

  []dnl
])dnl AX_LOG_DIRS

m4_define([AX_RC_DIRS],
[
  # Check where to install rc files.
  expand=yes;

  dnl The rc directory should *default* to "$sysconfdir/geda-gaf" in
  dnl order to comply with the GNU & Linux FHS guidelines.
  GEDA_RC_DIR=$1

  AC_MSG_CHECKING([where to install gEDA rc files])
  AC_ARG_WITH([rcdir], AS_HELP_STRING([--with-rcdir[[[=DIR]]]],
    [install config in specific DIR | yes, no, sys, etc]),

    [ #action-if-present

      if test "X$with_rcdir" = "Xno"; then
        # no means someone is disabling this "feature", which we are not
        # actually going to do, instead we: (do the old way)
        rcdir="$datarootdir/$GEDA_RC_DIR"
        mesg="using location:"

      elif test "X$with_rcdir" = "Xyes"; then
        # yes means the rc will go where to AutoConf want them, only not
        # in this case, but maybe not with the package name.
        rcdir="$sysconfdir/$GEDA_RC_DIR"
        mesg="to system:"

      elif test "X$with_rcdir" = "Xsys"; then
        # this is a new option stating with 2.0.8, the is like yes but
        # with the package name, aka
        rcdir="$sysconfdir/${PACKAGE_TARNAME}"
        mesg="to system default:"

      elif test "X$with_rcdir" = "Xetc"; then
        # this is a new option stating with 2.0.8, this allows local
        # builds to use the root etc using the package name
        rcdir="/etc/${PACKAGE_TARNAME}"
        mesg="native location:"
        expand=no;
        AC_DEFINE([GEDA_USE_HOME_ETC], 1,
                [Define to 1 if configuration files are to be in ~/etc])
      else
        # this option allow the person configure to specifiy the location
        # where the rc files will be installed
        # in this case, but maybe not with the package name.
        rcdir="$with_rcdir"
        mesg="as specified:"
        expand=no;
      fi ],

    [#action-if-not-present
       mesg="not specified:"
       rcdir="$datarootdir/$GEDA_RC_DIR"
    ]
  )

  echo -n ${mesg}

  if test "x$expand" = "xyes"; then
    AS_AC_EXPAND(GEDARCDIR, $rcdir)
  else
    echo "not expanding"
    GEDARCDIR=${rcdir}
  fi

  AC_DEFINE_DIR([GEDARCDIR], [rcdir], [rc directory])

  #dnl AC_SUBST([GEDARCDIR])
  AC_MSG_RESULT([$GEDARCDIR])

  []dnl
])dnl AX_RC_DIRS

dnl##################################################################
#                   ***  TEMPLATE Directory   ***
dnl##################################################################
m4_define([AX_TEMPLATE_DIRS],
[
  AC_PREREQ([2.60])dnl

  AC_ARG_VAR([XDG_USER_DIR], [Path to xdg-user-dir templates directory])

  # Check for xdg-user-dir
  AC_CHECK_PROG([XDG_USER_DIR], [xdg-user-dir],
                  [xdg-user-dir], [no])

  # Check where to put template documents.

  AC_MSG_CHECKING([where to put template documents])
  if test "x$templatedir" = "x"; then
     if test "X$XDG_USER_DIR" = "Xno"; then
        templatedir="~/Templates"
     else
        templatedir=$($XDG_USER_DIR TEMPLATES)
     fi
     AC_DEFINE_DIR([GEDATEMPLATEDIR], [templatedir], [Templates directory])
  else
     GEDATEMPLATEDIR="$templatedir"
     AC_DEFINE_UNQUOTED(GEDATEMPLATEDIR, "$templatedir")
  fi
  AC_MSG_RESULT([$GEDATEMPLATEDIR])

  []dnl
])dnl AX_TEMPLATE_DIRS

dnl In order to comply with the GNU & Linux FHS guidelines program
dnl directories should make the package name. These are "guideline"
dnl not mandates and generally be followed, But rules are made too
dnl be broken...

AC_DEFUN([AX_GEDA_DIRS],
[
  AC_PREREQ([2.60])dnl

  THIS_GEDA=gEDA

# Where should data files be installed/searched for?
  AX_DATA_DIRS([$THIS_GEDA])

# Where to install documention?
  AX_DOC_DIRS

# Where to write log files
  AX_LOG_DIRS([$THIS_GEDA])

# Where should PCB footprints be searched for?
  AX_PCB_DIRS

# Where to install rc files.
  AX_RC_DIRS([$THIS_GEDA])

# Where to install template documents?
  AX_TEMPLATE_DIRS

  []dnl
])dnl AX_GEDA_DIRS

AC_DEFUN([AX_EXPAND_DIRS],
[
  AC_PREREQ([2.60])dnl

  AS_AC_EXPAND(EXPANDED_PREFIX,     "$prefix")
  AS_AC_EXPAND(EXPANDED_EXEPREFIX,  "$exec_prefix")
  AS_AC_EXPAND(EXPANDED_LIBDIR,     "$libdir")
  AS_AC_EXPAND(EXPANDED_LIBEXECDIR, "$libexecdir")
  AS_AC_EXPAND(EXPANDED_LOCALEDIR,  "$localedir")
  AS_AC_EXPAND(EXPANDED_BINDIR,     "$bindir")
  AS_AC_EXPAND(EXPANDED_DATADIR,    "$GEDADATADIR")
  AS_AC_EXPAND(EXPANDED_DOCDIR,     "$GEDADOCDIR")
  AS_AC_EXPAND(EXPANDED_TEMPLATEDIR,"$GEDATEMPLATEDIR")
])dnl AX_EXPAND_DIRS
