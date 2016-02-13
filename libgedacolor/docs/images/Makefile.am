
sch_images  = libgedacolor.sch

svg_images = color_palette.svg

EXTRA_DIST = $(sch_images) $(svg_images)

pdf_images = $(EXTRA_DIST:.sch=.pdf)

png_images = $(EXTRA_DIST:.svg=.png)

all:
	@echo 'Type: make images  to create all the png images'

images: $(png_images) $(pdf_images)

clean-local:
	rm -rf $(png_images) $(pdf_images)

.svg.png:
	@if [ -x "`which inkscape`" ]; then inkscape -e $@ $< ; else  convert $< $@; fi

.svg.pdf:
	@if [ -x "`which inkscape`" ]; then inkscape -A $@ $< ; else  convert $< $@; fi

.sch.pdf:
	@if [ -x "`which gaf`" ]; then gaf export --color -o $@ $< ; fi

SUFFIXES = .svg .pdf .png

MOSTLYCLEANFILES     = *~ $(png_images) $(pdf_images)
CLEANFILES           = *~ $(png_images) $(pdf_images)
DISTCLEANFILES       = *~ $(png_images) $(pdf_images)
MAINTAINERCLEANFILES = *~ Makefile.in $(png_images) $(pdf_images)
