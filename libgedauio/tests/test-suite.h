#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>

sigjmp_buf point;

static void handler(int sig, siginfo_t *dont_care, void *dont_care_either)
{
   longjmp(point, 1);
}

#define SETUP_SIGSEGV_HANDLER \
  struct sigaction sa; \
  memset(&sa, 0, sizeof(sigaction)); \
  sigemptyset(&sa.sa_mask); \
  sa.sa_flags     = SA_NODEFER; \
  sa.sa_sigaction = handler; \
  sigaction(SIGSEGV, &sa, NULL); /* ignore whether it works or not */
