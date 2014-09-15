# Work around for Autoobsolete distro check is as follows: This file
# does not need to be modified for make install for make distcheck to
# work. Three scripts are built two are not. For the two that are not
# there should be a soft link .in to the .sh files, then distrocheck
# include them in the builddir.

SUFFIXES = .sh

TARGET_DIR = $(bindir)

SHdir	 =  $(TARGET_DIR)

SH_FILES = 	bompp \
		bom_xref \
		annotate \
		geda-bom \
		unannotate \
		sch2eaglepos

#EXTRA_DIST = unannotate.sh sch2eaglepos.sh

distcheck:

dist_bin_SCRIPTS=$(SH_FILES)

CLEANFILES           = *.log *~ $(SH_FILES)
MOSTLYCLEANFILES     = *.log *~ $(CONFIG_CLEAN) $(SH_FILES)
DISTCLEANFILES       = *.log *~ $(SH_FILES)
MAINTAINERCLEANFILES = *.log *~ $(SH_FILES) Makefile.in 
