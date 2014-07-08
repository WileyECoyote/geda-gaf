## $Id$
##
## Process this file with automake to produce Makefile.in

egginfodir            = $(PYTHON_MODDIR)

egginfo_DATA          = geda_gaf_scripts-1.0.egg-info

EXTRA_DIST            = $(egginfo_DATA)

#enble this for distcheck
distcheck:
dist_bin_SCRIPTS      = $(PY_SCRIPTS)

MOSTLYCLEANFILES      = *.log core FILE *~
CLEANFILES            = *.log core FILE *~
DISTCLEANFILES        = *.log core FILE *~
MAINTAINERCLEANFILES  = *.log core FILE *~ Makefile.in
