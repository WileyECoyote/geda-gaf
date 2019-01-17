## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in
SUFFIXES = .pl .py .sh

TARGET_DIR = $(bindir)

SHdir	 =  $(TARGET_DIR)
PERLdir	 =  $(TARGET_DIR)
PYdir	 =  $(TARGET_DIR)

SH_SCRIPTS	 = gcheck-library \
		   gfix-library \
		   gnet_hier_verilog \
		   gpstoimage

PERL_SCRIPTS	 = gsymfix.pl gschupdate.pl gsymupdate.pl \
		   pads_backannotate.pl pcb_backannotate.pl \
		   refdes_renum.pl \
		   sarlacc_sym.pl \
		   scale-schematic.pl \
		   schdiff.pl \
		   verilog2vhdl.pl

PY_SCRIPTS 	 = garchive.py \
		   gclean-symbol.py \
		   tragesym.py

installer	 = geda-install \
		   geda-install-bin

noinst_SCRIPTS	 = convert_sym.awk sw2asc.awk $(installer)

#enable this for distcheck
distcheck:
dist_bin_SCRIPTS = $(PERL_SCRIPTS) $(PY_SCRIPTS) $(SH_SCRIPTS) \
		   $(noinst_SCRIPTS)

.pl:
	@for script in $(PERL_SCRIPTS); do \
		@if test -f $(TARGET_DIR)/$script; then \
			rm -f $(TARGET_DIR)/$script; \
			cp -u (srcdir)/$script $(TARGET_DIR)/$script; \
		fi \
	done

MOSTLYCLEANFILES     = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS)
CLEANFILES 	     = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS)
DISTCLEANFILES	     = *.log      core FILE tmp *~ $(bin_SCRIPTS) $(installer)
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS) $(installer) Makefile.in

