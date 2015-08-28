SUBDIRS     = images

# These "extra" files are all for the Doxygen source documentation
GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = libgedadraw.dox libgedadraw-main.txt

LOCAL_HTML  = what-is-libgedadraw.html

LOCAL_TEX   = what-is-libgedadraw.tex

EXTRA_DIST  = $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_TEX)

SUFFIXES    = .html .tex .pdf

all:
if ENABLE_DOXYGEN
	@echo 'Type: make doxygen to create doxygen documentation for libgedadraw'
endif

# When generating doxygen documentation we check for the existence of the
# output directory and create if needed. After doxygen executes, we check
# for the latex directory and skip latex if the diretory does not exist.
doxygen: libgedadraw.dox

	@echo 'Creating doxygen documentation for libgedadraw...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/libgedadraw; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) libgedadraw.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) libgedadraw.dox; \
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
