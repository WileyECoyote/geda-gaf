## $Id$
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = garchive pads_backannotate pcb_backannotate refdes_renum \
              sarlacc_sym schdiff sw2asc tragesym

noinst_SCRIPTS = gnet_hier_verilog.sh gschupdate gsymupdate \
                 gcheck-library gclean-symbol gfix-library gsymfix

EXTRA_DIST = convert_sym.awk gpstoimage mk_char_tab.pl \
	     sarlacc_sym gschupdate gsymupdate refdes_renum gsymfix \
	     tragesym pads_backannotate pcb_backannotate garchive.py \
	     gnet_hier_verilog.sh schdiff sw2asc.in

##  SDB's stuff to enable garchive.  This hardcodes the path to the
##  system libraries into garchive prior to installation.
garchive: $(srcdir)/garchive.py
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/garchive.py > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ garchive sw2asc
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in garchive sw2asc
