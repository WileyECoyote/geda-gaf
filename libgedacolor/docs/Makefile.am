SUBDIRS     = images

# These "extra" files are all for the Doxygen source documentation
GEDA_EXTRA  = logo_256x101.png logo_512x208.png what-is-geda.html \
	      what-is-geda.tex

LOCAL_DOX   = libgedacolor.dox libgedacolor-main.txt

LOCAL_HTML  = what-is-libgedacolor.html libgedacolor-man-1.html

LOCAL_MAN   = libgedacolor.1.in libgedacolor.3.in

LOCAL_TEX   = what-is-libgedacolor.tex libgedacolor-man-1.tex

EXTRA_DIST  = $(GEDA_EXTRA) $(LOCAL_DOX) $(LOCAL_HTML) $(LOCAL_MAN) $(LOCAL_TEX)

SUFFIXES    = .1.in .3.in .1 .3 .html .tex .pdf

all:	$(MANS)
if ENABLE_DOXYGEN
	@echo 'Type: make doxygen to create doxygen documentation for libgedacolor'
endif

# When generating doxygen documentation we check for the existence of the
# output directory and create if needed. After doxygen executes, we check
# for the latex directory and skip latex if the diretory does not exist.
doxygen: libgedacolor.dox $(dist_docsman_DATA) $(text_man_files)
	@echo 'Creating doxygen documentation for libgedacolor...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	@if test "X@DOXYGEN_PATH_OUT@" != "X./" ; then \
	    DOXYGEN_OUTDIR=@DOXYGEN_PATH_OUT@/libgedacolor; \
	    test -d "$$DOXYGEN_OUTDIR" || $(MKDIR_P) "$$DOXYGEN_OUTDIR"; \
	    export "DOXYGEN_OUTDIR=$$DOXYGEN_OUTDIR"; \
	    $(DOXYGEN) libgedacolor.dox; \
	    echo "Doxygen output generated in $$DOXYGEN_OUTDIR"; \
	    test -d "$$DOXYGEN_OUTDIR"/latex && \
	    (cd $$DOXYGEN_OUTDIR/latex && $(MAKE)) || : ; \
	else \
	    DOXYGEN_OUTDIR="./"; \
	    $(DOXYGEN) libgedacolor.dox; \
	    test -d latex && (cd latex && $(MAKE)) || : ;\
	fi

	-(cd ..)
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

dist_man_MANS = libgedacolor.1 libgedacolor.3

.1.in.1:
	$(AM_V_GEN)  \
	@printf "" ; \
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(LIBGEDACOLOR_DOT_VERSION)-$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

.3.in.3:
	@printf "" ; \
	d=`$(GUILE) -c '(setlocale LC_ALL "C") (display (strftime "%B %-d, %Y" \
	    (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(LIBGEDACOLOR_DOT_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

if ENABLE_GROFF_HTML

# Rules for converting man->html
html_man_files = $(dist_man_MANS:%.1=%.html)
text_man_files = $(dist_man_MANS:%.1=%.tex)

docsmandir=$(docdir)/man

dist_docsman_DATA=$(html_man_files)

.1.html: $(dist_man_MANS)
	@rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	cp -f $@.tmp $@ && \
	sed '/\b\(Creator\|CreationDate\)\b/d' $@.tmp >$(basename $@)-man-1.html
	@rm -f $@.tmp 2>/dev/null

.1.tex: $(dist_man_MANS)
	@rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -t -e -mandoc -Tascii $(dist_man_MANS) | col -bx > $@.tmp && \
	mv -f $@.tmp $(basename $@)-man-1.tex
else

.1.html:
	@echo "No working groff available: cannot create $@"
	@! :

.1.tex:
	@echo "No working groff available: cannot create $@"
	@! :
endif

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~ $(dist_man_MANS)
MAINTAINERCLEANFILES = *.log *~ $(dist_man_MANS) Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
