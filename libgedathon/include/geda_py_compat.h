
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
#  ifndef HAVE_STRERROR
#    define HAVE_STRERROR 1
#  endif
#endif

#if GEDA_HAVE_HYPOT
#  ifndef HAVE_HYPOT
#    define HAVE_HYPOT 1
#  endif
#endif

#if GEDA_HAVE_PUTENV
#  ifndef HAVE_PUTENV
#    define HAVE_PUTENV 1
#  endif
#endif

#endif /* OS_WIN32 */
