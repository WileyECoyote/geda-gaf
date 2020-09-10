## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = utils.dox utils-main.txt

LOCAL_HTML  = what-is-utils.html

LOCAL_TEX   = what-is-utils.tex

READ_MES    = README.gsch2pcb README.olib README.tragesym README.grenum

EXTRA_DIST  = TODO.grenum why.grenum gsch2pcb.1.in gsch2pdf.1.in gschemdoc.1.in\
	      grenum.1.in gxyrs.1.in  olib.1.in pads_backannotate.1.in \
	      pcb_backannotate.1.in  refdes_renum.1.in schdiff.1.in sw2asc.1.in \
	      tragesym.1.in smash_megafile.1.in convert_sym.1.in garchive.1.in \
	      gmk_sym.1.in gschlas.1.in gsymfix.1.in sarlacc_sym.1.in sarlacc_schem.1.in \
	      gnet_hier_verilog.1.in geda-install.1.in geda-install-bin.1.in \
	      $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_TEX) \
	      $(READ_MES)

dist_man_MANS = gsch2pcb.1 gsch2pdf.1 gschemdoc.1 grenum.1 gxyrs.1 olib.1 \
	        pads_backannotate.1 pcb_backannotate.1 refdes_renum.1 \
	        schdiff.1 sw2asc.1 tragesym.1 smash_megafile.1 convert_sym.1 \
	        garchive.1 gmk_sym.1 gschlas.1 sarlacc_sym.1 sarlacc_schem.1 \
	        gclean-symbol.1 gsymfix.1 gfix-library.1 gcheck-library.1 \
	        gmake-caps-sym.1 gmake-set-resistors.1 gmake-transistors-sym.1 \
	        gnet_hier_verilog.1 geda-install.1 geda-install-bin.1

docsreadmedir   = $(docdir)/readmes
docsreadme_DATA = $(READ_MES)

all:	$(MANS)
if ENABLE_DOXYGEN
	@echo 'Type: make doxygen to create doxygen documentation for gEDA utilities'
endif

# When generating doxygen documentation we check for the existence of the
# output directory and create if needed. After doxygen executes, we check
# for the latex directory and skip latex if the diretory does not exist.
doxygen: utils.dox
if ENABLE_DOXYGEN
	@echo 'Creating doxygen documentation for utilities...'
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/utils; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) utils.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) utils.dox; \
	    test -d latex && (cd latex && $(MAKE)) || : ;\
	fi

	-(cd ..)
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

.1.in.1:
	$(AM_V_GEN)\
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

if ENABLE_GROFF_HTML

# Rules for converting man->html
html_man_files = $(dist_man_MANS:%.1=%.html)
SUFFIXES = .1.in .1 .html

docsmandir=$(docdir)/man
dist_docsman_DATA=$(html_man_files)

.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	sed '/\b\(Creator\|CreationDate\)\b/d' $@.tmp >$(basename $@).html
	@rm -f $@.tmp 2>/dev/null
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif

MOSTLYCLEANFILES     = *.log *.tmp *~
CLEANFILES           = *.log *.tmp *~
DISTCLEANFILES       = *.log *.tmp *~ core FILE
MAINTAINERCLEANFILES = *.log *.tmp *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
