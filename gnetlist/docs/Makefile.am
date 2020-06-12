
SUBDIRS = vams

# These "extra" files are for the Doxygen source documentation
GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = gnetlist.dox gnetlist-main.txt

LOCAL_HTML  = $(html_man_files) what-is-gnetlist.html

LOCAL_MAN   = gnetlist.1.in annotate.1.in unannotate.1.in

LOCAL_TEX   = what-is-gnetlist.tex

EXTRA_DIST  = $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_MAN) $(LOCAL_TEX)

docsreadmedir = $(docdir)/readmes
dist_docsreadme_DATA = \
	README.bom README.pcb README.switcap README.verilog \
	README.vhdl README.sysc README.eagle

dist_man_MANS = gnetlist.1 gnetlist.5 mk_verilog_syms.1 annotate.1 sch2eaglepos.1 \
		unannotate.1

SUFFIXES    = .1.in .5.in .1 .5 .html .tex .pdf

all:	$(MANS)
if ENABLE_DOXYGEN
	@echo 'Type: make doxygen to create doxygen documentation for gnetlist'
endif

.1.in.1:
	$(AM_V_GEN) \
	@printf "" ; \
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

.5.in.5:
	@printf "" ; \
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

if ENABLE_GROFF_HTML

# Rules for converting man->html
html_man_files = $(dist_man_MANS:%.1=%.html)

docsmandir=$(docdir)/man
docsman_DATA=$(html_man_files)

.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif

# When generating doxygen documentation we check for the existence of the
# output directory and create if needed. After doxygen executes, we check
# for the latex directory and skip latex if the diretory does not exist.
doxygen: gnetlist.dox $(docsman_DATA)
	@echo 'Creating doxygen documentation for gnetlist...'
if ENABLE_DOXYGEN
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/gnetlist; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) gnetlist.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) gnetlist.dox; \
	    test -d latex && (cd latex && $(MAKE)) || : ;\
	fi

#	-(cd ..)
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~ core FILE
MAINTAINERCLEANFILES = *.log *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
