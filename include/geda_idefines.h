
#ifdef __GNUC__

  #define WARN_UNUSED __attribute__((warn_unused_result))
  #define NOWARN_UNUSED __attribute__((unused))

  #define MAX_FILE 255

#endif

#ifndef _WIN32
  #define MAX_PATH 248
#endif

#define MAX_FILENAME 64

#define NO_ERROR 0

/* These are for where status information goes */
#define CONSOLE_WINDOW		0
#define STDOUT_TTY		1
#define BOTH_CONWIN_STDOUT	2


/* for text cap style */
#define LOWER_CASE      0
#define UPPER_CASE      1
#define BOTH_CASES      2
