## Process this file with automake to produce Makefile.in

SUBDIRS = bom hierarchy hierarchy2 drc2 scripts common

EXTRA_DIST = runtest.sh gafrc \
	     7447.vhdl README amp.spice cascade.sch cascade.cascade \
	     darlington.spice netattrib.geda \
	     netattrib.sch powersupply.PCB powersupply.allegro \
             powersupply.bae powersupply.geda powersupply.maxascii \
             powersupply.pads powersupply.protelII powersupply.sch \
             powersupply.tango singlenet.PCB singlenet.geda singlenet.pads \
	     powersupply.liquidpcb \
             singlenet.pcbpins \
             singlenet.protelII singlenet.sch stack_1.geda amp.spice-sdb \
	     singlenet.liquidpcb \
	     darlington.spice-sdb skt.switcap test.ana multiequal.sch \
	     multiequal.spice-sdb gnetlistrc.vhdl gnetlistrc.orig

check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)
GNETLIST=$(builddir)/../src/gnetlist

tests:

#notest:
# cleanup
	@rm -rf $(BUILDDIR)/devfiles $(BUILDDIR)/new_*
	@mkdir $(BUILDDIR)/devfiles
	@cp -f $(srcdir)/gnetlistrc.orig $(builddir)/gnetlistrc

# netattrib gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/netattrib.sch geda \
		$(BUILDDIR) $(SRCDIR)

# stack gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/../examples/stack_1.sch geda \
		$(BUILDDIR) $(SRCDIR)

# singlenet gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch geda \
		$(BUILDDIR) $(SRCDIR)

# singlenet PCB
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch PCB \
		$(BUILDDIR) $(SRCDIR)

# singlenet pcbpins
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch pcbpins \
		$(BUILDDIR) $(SRCDIR)

# singlenet PADS
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch pads \
		$(BUILDDIR) $(SRCDIR)

# singlenet protelII
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch protelII \
		$(BUILDDIR) $(SRCDIR)

# singlenet liquidpcb
	$(SRCDIR)/runtest.sh $(SRCDIR)/singlenet.sch liquidpcb \
		$(BUILDDIR) $(SRCDIR)

# powersupply PCB
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch PCB \
		$(BUILDDIR) $(SRCDIR)

# powersupply Allegro
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch allegro \
		$(BUILDDIR) $(SRCDIR)

# powersupply bae
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch bae \
		$(BUILDDIR) $(SRCDIR)

# powersupply gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch geda \
		$(BUILDDIR) $(SRCDIR)

# powersupply maxascii
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch maxascii \
		$(BUILDDIR) $(SRCDIR)

# powersupply pads
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch pads \
		$(BUILDDIR) $(SRCDIR)

# powersupply protelII
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch protelII \
		$(BUILDDIR) $(SRCDIR)

# powersupply tango
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch tango \
		$(BUILDDIR) $(SRCDIR)

# powersupply liquidpcb
	$(SRCDIR)/runtest.sh $(SRCDIR)/powersupply.sch liquidpcb \
		$(BUILDDIR) $(SRCDIR)

# multiple equals in attributes SPICE-SDB
	$(SRCDIR)/runtest.sh $(SRCDIR)/multiequal.sch spice-sdb \
		$(BUILDDIR) $(SRCDIR)

# cascade backend
	$(SRCDIR)/runtest.sh $(SRCDIR)/cascade.sch cascade \
		$(BUILDDIR) $(SRCDIR)

# Cleanup
	@rm -rf $(BUILDDIR)/devfiles $(BUILDDIR)/new_*
	@rm -rf $(builddir)/logs $(builddir)/gnetlistrc

clean-local: clean-local-check
.PHONY: clean-local-check
clean-local-check:
	@rm -rf logs

MOSTLYCLEANFILES     = new_* core *.log FILE *.ps *~
CLEANFILES           = new_* core *.log FILE *.ps *~
DISTCLEANFILES       = new_* core *.log FILE *.ps *~
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in

