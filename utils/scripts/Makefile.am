## $Id$
##
## Process this file with automake to produce Makefile.in
SUFFIXES = .pl .py .sh

TARGET_DIR = $(bindir)

SHdir	 =  $(TARGET_DIR)
PERLdir	 =  $(TARGET_DIR)
PYdir	 =  $(TARGET_DIR)

SH_SCRIPTS	     = gcheck-library \
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

PY_SCRIPTS 	     = garchive.py \
                   gclean-symbol.py \
                   tragesym.py

installer	     = geda-install \
                   geda-install-bin

noinst_SCRIPTS	 = convert_sym.awk sw2asc.awk $(installer)

#enble this for distcheck
distcheck:
dist_bin_SCRIPTS = $(PERL_SCRIPTS) $(PY_SCRIPTS) $(SH_SCRIPTS) \
				   $(noinst_SCRIPTS)

MOSTLYCLEANFILES     = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS)
CLEANFILES 	         = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS)
DISTCLEANFILES	     = *.log      core FILE tmp *~ $(bin_SCRIPTS) $(installer)
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS) $(installer) Makefile.in

