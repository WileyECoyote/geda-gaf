## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = \
	geda.scm color-map.scm geda/log.scm geda/object.scm geda/page.scm \
	geda/attrib.scm geda/deprecated.scm geda/os.scm geda/config.scm \
	geda/version.scm

nobase_scmdata_DATA = geda/core/gettext.scm

check-am: update-gaf-tool

# Unit test support.  The unit tests are run using a geda batch
# program, with config loading disabled (-q) so that user config
# already on the system can't interfere with the test process.
#
# Note that if GUILE_LOAD_PATH is required to be set in order for
# geda to run properly, then this will also be true for these tests.

# The new commanline tool uses --no-rcfiles
#GAF_SHELL = $(top_builddir)/gaf/src/.libs/gaf --no-rcfiles shell

GAF_SHELL = $(builddir)/../shell/geda-shell -i

TEST_EXTENSIONS  = .scm
SCM_LOG_COMPILER = $(GAF_SHELL)
AM_SCM_LOG_FLAGS = -p $(srcdir) -p $(builddir) \
	-e '(use-modules (unit-test)) (load (list-ref (command-line) 1)) (exit (if (tests-passed?) 0 1))'

TESTS = unit-tests/t0001-geda.scm \
	unit-tests/t0002-g_rc.scm \
	unit-tests/t0100-object-line.scm \
	unit-tests/t0101-object-box.scm \
	unit-tests/t0102-object-circle.scm \
	unit-tests/t0103-object-arc.scm\
	unit-tests/t0104-object-text.scm \
	unit-tests/t0105-object-complex.scm \
	unit-tests/t0106-object-bounds.scm \
	unit-tests/t0107-object-stroke-fill.scm \
	unit-tests/t0108-object-connections.scm \
	unit-tests/t0109-object-copy.scm \
	unit-tests/t0110-object-transform.scm \
	unit-tests/t0111-object-path.scm \
	unit-tests/t0112-object-picture.scm \
	unit-tests/t0113-object-library.scm \
	unit-tests/t0200-page.scm \
	unit-tests/t0201-page-remove.scm \
	unit-tests/t0202-page-dirty.scm \
	unit-tests/t0203-page-string.scm \
	unit-tests/t0204-page-parse-ordering.scm \
	unit-tests/t0205-page-parse-unterminated-attribute-list.scm \
	unit-tests/t0206-page-parse-garbage-attribute.scm \
	unit-tests/t0207-page-parse-line-endings.scm \
	unit-tests/t0208-page-parse-embed-no-complex.scm \
	unit-tests/t0300-attribute.scm \
	unit-tests/t0301-promotable-attributes.scm \
	unit-tests/t0400-os.scm \
	unit-tests/t0401-os-expand-env-variables.scm \
	unit-tests/t0402-config.scm \
	unit-tests/t0403-log.scm \
	unit-tests/t1000-deprecated.scm

XFAIL_TESTS = \
		unit-tests/t0301-promotable-attributes.scm

dist_noinst_DATA = geda/core/gettext.scm.in unit-test.scm $(TESTS)

geda/core/gettext.scm: $(srcdir)/geda/core/gettext.scm.in Makefile
	@domain=$(LIBGEDA_GETTEXT_DOMAIN); \
	$(MKDIR_P) geda/core; \
	sed -e"s:[@]LIBGEDA_GETTEXT_DOMAIN@:$$domain:" < $(srcdir)/$@.in > $@.new; \
	if cmp $@ $@.new > /dev/null 2>&1; then \
	  rm $@.new; echo "$@ is unchanged"; \
	else \
	  echo "Recreating $@"; mv $@.new $@; \
	fi

DEPENDS=libgeda/src libgedacairo/src gaf/src

update-gaf-tool:
	@for d in $(DEPENDS); do \
	  (cd $(top_builddir)/$$d && $(MAKE) $(AM_MAKEFLAGS) all); \
	done

.PHONY: update-gaf-tool

MOSTLYCLEANFILES     = *.log */*~ *~
CLEANFILES           = *.log */*~ *~ geda/core/gettext.scm
DISTCLEANFILES       = *.log */*~ *~ core FILE
MAINTAINERCLEANFILES = *.log */*~ *~ Makefile.in

