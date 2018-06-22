#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>

#ifdef OS_WIN32_NATIVE

typedef int test_jmp_buf[17];

test_jmp_buf point;

#define SETUP_SIGSEGV_HANDLER

#else

sigaction  point;

static void handle_signal(int sig, siginfo_t *dont_care, void *dont_care_either)
{
   longjmp(point, 1);
}

#define SETUP_SIGSEGV_HANDLER \
  struct sigaction sa; \
  memset(&sa, 0, sizeof(sigaction)); \
  sigemptyset(&sa.sa_mask); \
  sa.sa_flags     = SA_NODEFER; \
  sa.sa_sigaction = handle_signal; \
  sigaction(SIGSEGV, &sa, NULL); /* ignore whether it works or not */

#endif