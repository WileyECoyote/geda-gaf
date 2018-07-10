
#ifdef OS_WIN32

#if HAVE_STRERROR
#  define GEDA_HAVE_STRERROR 1
#  undef HAVE_STRERROR
#endif

#if HAVE_HYPOT
#  define GEDA_HAVE_HYPOT 1
#  undef HAVE_HYPOT
#endif

#if HAVE_PUTENV
#  define GEDA_HAVE_PUTENV 1
#  undef HAVE_PUTENV
#endif

#endif /* OS_WIN32 */

/* Include the crummy header */
#include <Python.h>

#ifdef OS_WIN32

#if GEDA_HAVE_STRERROR
#  ifdef HAVE_STRERROR
#    undef HAVE_STRERROR
#  endif
#  define HAVE_STRERROR 1
#endif

#if GEDA_HAVE_HYPOT
#  ifdef HAVE_HYPOT
#    undef HAVE_HYPOT
#  endif
#  define HAVE_HYPOT 1
#endif

#if GEDA_HAVE_PUTENV
#  ifdef HAVE_PUTENV
#    undef HAVE_PUTENV
#  endif
#  define HAVE_PUTENV 1
#endif

#endif /* OS_WIN32 */
