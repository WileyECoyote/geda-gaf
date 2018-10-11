## $Id$
##

M4         =	@M4@
PCBM4DIR   =	@PCBM4DIR@
PCBCONFDIR =	@PCBCONFDIR@

scmdatadir =	@GEDADATADIR@/scheme

nobase_scmdata_DATA = $(DIST_SCM) $(BUILT_SCM)

DIST_SCM   =	gnet-PCB.scm \
		gnet-allegro.scm \
		gnet-bae.scm \
		gnet-bom.scm \
		gnet-bom2.scm \
		gnet-calay.scm \
		gnet-cascade.scm \
		gnet-drc.scm \
		gnet-drc2.scm \
		gnet-eagle.scm \
		gnet-ewnet.scm \
		gnet-futurenet2.scm \
		gnet-geda.scm \
		gnet-gossip.scm \
		gnetlist.scm \
		gnetlist-post.scm \
		gnet-liquidpcb.scm \
		gnet-mathematica.scm \
		gnet-osmond.scm \
		gnet-pads.scm \
		gnet-partslist1.scm \
		gnet-partslist2.scm \
		gnet-partslist3.scm \
		gnet-pcbpins.scm \
		gnet-protelII.scm \
		gnet-makedepend.scm \
		gnet-maxascii.scm \
		gnet-redac.scm \
		gnet-shell.scm \
		gnet-spice.scm \
		gnet-spice-anise.scm \
		gnet-spice-noqsi.scm \
		gnet-spice-sdb.scm \
		gnet-systemc.scm \
		gnet-switcap.scm   \
		gnet-tango.scm \
		gnet-vams.scm \
		gnet-verilog.scm \
		gnet-vhdl.scm \
		gnet-vipec.scm \
		partslist-common.scm \
		spice-common.scm \
		gnetlist/backend-getopt.scm

# Guile 1.8 does not have srfi-64
if HAVE_SRFI_64

  TESTS = unit-tests/test-partlist.scm
  # TEST_EXTENSIONS = .scm
  # $(srcdir) and $(builddir) are added here and not in
  # AM_SCM_LOG_FLAGS below because guile must know where to find
  # gnetlist modules before it runs tests
  SCM_LOG_DRIVER = $(GUILE) -L $(srcdir) -L $(builddir) --no-auto-compile -e main -s unit-test.scm
  AM_SCM_LOG_FLAGS = --
  AM_TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0

  dist_noinst_DATA = unit-test.scm $(TESTS)
else !HAVE_SRFI_64
	@echo 'srfi-64 is require for scheme unit tests, skipping.'
endif

EXTRA_DIST = $(DIST_SCM) $(SCM_SRCS)

SCM_SRCS   =	gnet-gsch2pcb.scm.in

BUILT_SCM  =	gnet-gsch2pcb.scm

gnet-gsch2pcb.scm: $(srcdir)/gnet-gsch2pcb.scm.in
	sed \
		-e 's;@m4@;${M4};g' \
		-e 's;@pcbm4dir@;${PCBM4DIR};g' \
		$(srcdir)/gnet-gsch2pcb.scm.in > $@

all-local:
	@if test "$(srcdir)" != "@builddir@" ; then \
		echo "copying scheme files" ; \
		cp -f $(srcdir)/*.scm @builddir@/ ; \
	else \
		echo "source directory = build directory" ; \
	fi

clean-local:
	@if test "$(srcdir)" != "@builddir@" ; then \
		echo "cleaning scheme files" ; \
		rm -f @builddir@/*.scm ; \
	else \
		echo "source directory = build directory" ; \
	fi

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~ ${BUILT_SCM}
DISTCLEANFILES       = *.log *~ ${BUILT_SCM} core FILE
MAINTAINERCLEANFILES = *.log *~ ${BUILT_SCM} Makefile.in
