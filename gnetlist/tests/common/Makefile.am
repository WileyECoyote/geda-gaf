input_files = \
	inputs/gafrc \
	inputs/LVDfoo.sch \
	inputs/Simulation.cmd \
	inputs/TwoStageAmp.sch \
	inputs/SlottedOpamps.sch \
	inputs/cascade.sch \
	inputs/multiequal.sch \
	inputs/netattrib.sch \
	inputs/powersupply.sch \
	inputs/singlenet.sch \
	inputs/stack-torture.sch \
	inputs/attribs \
	\
	inputs/models/2N3904.mod \
	inputs/models/openIP_5.cir \
	\
	inputs/sym/LVD.sym \
	inputs/sym/rectifier-bridge-1.sym \
	inputs/sym/transistor.sym \
	inputs/sym/LM324_slotted-1.sym

AM_TESTS_ENVIRONMENT= PERL=${PERL}

check-local:
	$(AM_TESTS_ENVIRONMENT) srcdir=$(srcdir) $(SHELL) $(srcdir)/run_tests.sh

EXTRA_DIST= run_backend_tests.sh run_tests.sh tests.list backends.list \
	always-copy.list $(input_files)

dist-hook: tests.list backends.list
	@tests=`cat $(srcdir)/tests.list | sed -e "s:#.*$$::" | sed -e "s:|.*$$::"`; \
	backends=`cat $(srcdir)/backends.list`; \
	$(MKDIR_P) $(distdir)/outputs || exit $$?; \
	for backend in $$backends; do \
	  dir=outputs/$$backend; \
	  $(MKDIR_P) $(distdir)/$$dir; \
	  for test in $$tests; do \
	    cp -p $(srcdir)/$$dir/$$test.retcode $(distdir)/$$dir \
	      || exit $$?; \
	    if test -f $(srcdir)/$$dir/$$test-output.net ; then \
	      cp -p $(srcdir)/$$dir/$$test-output.net $(distdir)/$$dir \
	        || exit $$?; \
	    fi; \
	  done; \
	done;

clean-local:
	test -d run && rm -rf run || : ;

MOSTLYCLEANFILES     = core *.diff *.log FILE *.ps *~
CLEANFILES           = core *.diff *.log FILE *.ps *~
DISTCLEANFILES       = core *.diff *.log FILE *.ps *~
MAINTAINERCLEANFILES = core *.diff *.log FILE *.ps *~ Makefile.in
