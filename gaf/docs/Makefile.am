## Process this file with automake to produce Makefile.in

EXTRA_DIST = $(html_man_files) gaf.1.in

all:	$(MANS)

.1.in.1:
	$(AM_V_GEN)\
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

if ENABLE_GROFF_HTML

# Rules for converting man->html
dist_man_MANS  = gaf.1
html_man_files = gaf.html

docsmandir     = $(docdir)/man

docsman_DATA   = $(html_man_files)

.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif

SUFFIXES = .1.in .1 .html

MOSTLYCLEANFILES       = *.log *~
CLEANFILES             = *.log *~
DISTCLEANFILES         = *.log core FILE *~ \
			*.dvi *.info *.info-* *.aux *.fn \
			*.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr
MAINTAINERCLEANFILES   = *.log *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
