SUBDIRS     = images

# These "extra" files are for the Doxygen source documentation
GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = gschem.dox gschem-main.txt

LOCAL_HTML  = $(html_man_files) what-is-gschem.html

LOCAL_MAN   = gschem.1.in

LOCAL_TEX   = what-is-gschem.tex

EXTRA_DIST  = $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_MAN) $(LOCAL_TEX)

all:
	@echo 'Type: make doxygen to create doxygen documentation for gschem'

# When generating doxygen documentation we check for the existence of the
# output directory and create if needed. After doxygen executes, we check
# for the latex directory is skip the latex if the diretory does not exist.
doxygen: gschem.dox
	@echo 'Creating doxygen documentation for gschem...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/gschem; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) gschem.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) gschem.dox; \
	    test -d latex && (cd latex && $(MAKE)) || : ;\
	fi

else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

dist_man_MANS = gschem.1

.1.in.1:
	d=`LANG=C date -d $(DATE_VERSION) "+%B %-d, %Y"` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

# Rules for converting man->html
html_man_files = gschem.html
SUFFIXES = .1.in .1 .html

docsmandir=$(docdir)/man
docsman_DATA=$(html_man_files)
if ENABLE_GROFF_HTML
.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif


MOSTLYCLEANFILES = 	*.log *~
CLEANFILES       = 	*.log *~
DISTCLEANFILES   = 	*.log core FILE *~ \
			        *.dvi *.info *.info-* *.aux *.fn \
			        *.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr
MAINTAINERCLEANFILES = *.log *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
