SUBDIRS     = images

# These "extra" files are all for the Doxygen source documentation
GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = libgedathon.dox libgedathon-main.txt

LOCAL_HTML  = guide-libgedathon.html include-libgedathon.html \
	      welcome_libgedathon.png  what-is-libgedathon.html

LOCAL_TEX   = what-is-libgedathon.tex include-libgedathon.tex \
	      guide-libgedathon.tex reference-libgedathon.tex

EXTRA_DIST  = $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_TEX)

SUFFIXES    = .html .tex .pdf

all: 
	@echo 'Type: make doxygen  to create doxygen documentation for libgedathon'

doxygen:
	@echo 'Creating doxygen documentation for libgedathon...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/libgedathon; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) libgedathon.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) libgedathon.dox; \
	    test -d latex && (cd latex && $(MAKE)) || : ;\
	fi

	-(cd ..)
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MOSTLYCLEANFILES     = *.log *~ *.bak
CLEANFILES           = *.log *~ *.bak
DISTCLEANFILES       = *.log *~ *.bak
MAINTAINERCLEANFILES = *.log *~ *.bak Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
