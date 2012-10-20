
#include <missing.h>
#include <geda_types.h>
#include <geda_macros.h>

#ifdef __GNUC__

#define WARN_UNUSED __attribute__((warn_unused_result))

#define MAX_FILE 255

#endif

/* These are for where status information goes */
#define LOG_WINDOW		0
#define STDOUT_TTY		1
#define BOTH_LOGWIN_STDOUT	2


