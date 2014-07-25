
#ifndef __GEDA_DEBUG__
#  define __GEDA_DEBUG__


#define DEBUG_DND_EVENTS 0
#define DEBUG_EVENTS     0
#define DEBUG_IMAGING    0
#define DEBUG_TOOLBARS   0

#  ifdef HAVE_LIBDMALLOC
#    include <dmalloc.h>
#  endif
#endif