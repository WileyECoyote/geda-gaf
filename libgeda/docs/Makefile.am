SUBDIRS     = images

# These "extra" files are all for the Doxygen source documentation
GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = libgeda.dox libgeda-main.txt

LOCAL_HTML  = what-is-libgeda.html

LOCAL_MAN   = libgeda.1.in

LOCAL_TEX   = what-is-libgeda.tex

EXTRA_DIST  = $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_MAN) $(LOCAL_TEX)

SUFFIXES    = .html .tex .pdf

all:	$(MANS)
if ENABLE_DOXYGEN
	@echo 'Type: make doxygen to create doxygen documentation for libgeda'
endif

# When generating doxygen documentation we check for the existence of the
# output directory and create if needed. After doxygen executes, we check
# for the latex directory and skip latex if the diretory does not exist.
doxygen: libgeda.dox
	@echo 'Creating doxygen documentation for libgeda...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/libgeda; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) libgeda.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) libgeda.dox; \
	    test -d latex && (cd latex && $(MAKE)) || : ;\
	fi

	-(cd ..)
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

dist_man_MANS = libgeda.1

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(LIBGEDA_DOT_VERSION)-$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

.3.in.3:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(LIBGEDA_DOT_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
