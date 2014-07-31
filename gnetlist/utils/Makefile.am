## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA                = -I$(top_srcdir) -I$(top_srcdir)/include
INC_LIBGEDA             = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES           = $(INC_GEDA) $(INC_LIBGEDA)

bin_PROGRAMS            = mk_verilog_syms

mk_verilog_syms_SOURCES = mk_verilog_syms.c

mk_verilog_syms_CFLAGS  = $(INC_GEDA) $(GCC_CFLAGS) $(MINGW_CFLAGS)
mk_verilog_syms_LDFLAGS = -lm

MOSTLYCLEANFILES        = *.log *.ps core FILE *~
CLEANFILES              = *.log *.ps core FILE *~
DISTCLEANFILES          = *.log *.ps core FILE *~
MAINTAINERCLEANFILES    = *.log *.ps core FILE *~ Makefile.in

