sch_images  = \
		array_dialog.sch \
		break.sch \
		break_box.sch \
		paths.sch projections.sch \
		projections2.sch

EXTRA_DIST = $(sch_images)

png_images = $(EXTRA_DIST:.sch=.png)
pdf_images = $(EXTRA_DIST:.sch=.pdf)

SUFFIXES = .sch

all:
	@echo 'Type: make images to create all the png/pdf images'

images: $(png_images) $(pdf_images)

clean-local:
	rm -rf $(png_images) $(pdf_images)

.sch.png:
	@if [ -x "`which gaf`" ]; then gaf export --color -o $@ $< ; fi

.sch.pdf:
	@if [ -x "`which gaf`" ]; then gaf export --color -o $@ $< ; fi

MOSTLYCLEANFILES     = *~ $(png_images) $(pdf_images)
CLEANFILES           = *~ $(png_images) $(pdf_images)
DISTCLEANFILES       = *~ $(png_images) $(pdf_images)
MAINTAINERCLEANFILES = *~ $(png_images) $(pdf_images) Makefile.in
