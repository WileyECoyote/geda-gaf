## $Id$
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
                   gpstoimage \
                   ginstall_local

PERL_SCRIPTS	 = gsymfix.pl gschupdate.pl gsymupdate.pl \
		   pads_backannotate.pl pcb_backannotate.pl \
		   refdes_renum.pl \
                   sarlacc_sym.pl \
                   scale-schematic.pl \
                   schdiff.pl

PY_SCRIPTS 	 = garchive.py \
                   gclean-symbol.py \
                   tragesym.py
		
noinst_SCRIPTS	 = convert_sym.awk sw2asc.awk

dist_bin_SCRIPTS = $(PERL_SCRIPTS) $(PY_SCRIPTS) $(SH_SCRIPTS) \
		   $(noinst_SCRIPTS)

##  SDB's stuff to enable garchive.  This hardcodes the path to the
##  system libraries into garchive prior to installation.
garchive: $(srcdir)/garchive.py
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/garchive.py > tmp$@
	@chmod a+x tmp$@
	@mv -f tmp$@ $@

MOSTLYCLEANFILES     = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS)
CLEANFILES 	     = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS)
DISTCLEANFILES	     = *.log      core FILE tmp *~ $(bin_SCRIPTS) ginstall_local
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ $(bin_SCRIPTS) ginstall_local Makefile.in
