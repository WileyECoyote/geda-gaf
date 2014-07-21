
#ifndef __GEDA_SYSTEM__
#define __GEDA_SYSTEM__

#if defined(WIN32) || defined(_WIN32)

/* On Win32, the directory separator is the backslash, and the search path
 * separator is the semicolon. Note that also the (forward) slash works as
 * directory separator.
 */
#define DIR_SEPARATOR '\\'
#define DIR_SEPARATOR_S "\\"
#define IS_DIR_SEPARATOR(c) ((c) == DIR_SEPARATOR || (c) == '/')
#define SEARCHPATH_SEPARATOR ';'
#define SEARCHPATH_SEPARATOR_S ";"

#elif defined(__linux__) || defined(UNIX)

#define DIR_SEPARATOR 0x2F
#define DIR_SEPARATOR_S "/"
#define IS_DIR_SEPARATOR(c) ((c) == DIR_SEPARATOR)
#define SEARCHPATH_SEPARATOR ':'
#define SEARCHPATH_SEPARATOR_S ":"

#else
#error "No suitable OS defined"
#endif

#endif
