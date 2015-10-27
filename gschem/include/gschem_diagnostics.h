/* -*- i_diagnostics.h -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: January, 28, 2015
 *
 */

#ifndef __GSCHEM_DIAGNOSTICS__
#define __GSCHEM_DIAGNOSTICS__

# include <sys/time.h>
# include <sys/resource.h>
# include "rusage/tlpi_hdr.h"

#ifndef DECLARE_RUSAGE_DATA
#define DECLARE_RUSAGE_DATA \
  struct rusage before; \
  struct rusage after;
#endif

#define START_PERFORMANCE(w) \
        DECLARE_RUSAGE_DATA \
        gschem_begin_performance(w, &before);

#define STOP_PERFORMANCE(w) \
        gschem_end_performance(w, &after); \
        gschem_show_performance(&before, &after);

bool  old_do_autosave_backup;

static void gschem_show_performance(struct rusage *before, struct rusage *after)__attribute__((unused));
static void printRusage(const char *leader, const struct rusage *ru)__attribute__((unused));

static void printRusage(const char *leader, const struct rusage *ru)
{
    const char *ldr;

    ldr = (leader == NULL) ? "" : leader;

    printf("%sCPU time (secs):         user=%.3f; system=%.3f\n", ldr,
            ru->ru_utime.tv_sec + ru->ru_utime.tv_usec / 1000000.0,
            ru->ru_stime.tv_sec + ru->ru_stime.tv_usec / 1000000.0);
    printf("%sMax resident set size:   %ld\n", ldr, ru->ru_maxrss);
    printf("%sIntegral shared memory:  %ld\n", ldr, ru->ru_ixrss);
    printf("%sIntegral unshared data:  %ld\n", ldr, ru->ru_idrss);
    printf("%sIntegral unshared stack: %ld\n", ldr, ru->ru_isrss);
    printf("%sPage reclaims:           %ld\n", ldr, ru->ru_minflt);
    printf("%sPage faults:             %ld\n", ldr, ru->ru_majflt);
    printf("%sSwaps:                   %ld\n", ldr, ru->ru_nswap);
    printf("%sBlock I/Os:              input=%ld; output=%ld\n",
            ldr, ru->ru_inblock, ru->ru_oublock);
    printf("%sSignals received:        %ld\n", ldr, ru->ru_nsignals);
    printf("%sIPC messages:            sent=%ld; received=%ld\n",
            ldr, ru->ru_msgsnd, ru->ru_msgrcv);
    printf("%sContext switches:        voluntary=%ld; "
            "involuntary=%ld\n", ldr, ru->ru_nvcsw, ru->ru_nivcsw);
    printf("------------- End Report-------------\n\n");
}

static void gschem_diagnostics_prep_for_test (GschemToplevel *w_current)
{
  /* Temporarily disable backups for the current page */
  old_do_autosave_backup = Current_Page->do_autosave_backup;
}
static void gschem_diagnostics_post_test (GschemToplevel *w_current)
{
  /* Restore the backup flag to the previous value */
  Current_Page->do_autosave_backup = old_do_autosave_backup;
}

inline static void gschem_begin_performance(GschemToplevel *w_current, struct rusage *before)
{
  gschem_diagnostics_prep_for_test (w_current);
  getrusage(RUSAGE_SELF, before);
}
inline static void gschem_end_performance(GschemToplevel *w_current, struct rusage *after)
{
  getrusage(RUSAGE_SELF, after);
  gschem_diagnostics_post_test (w_current);
}

static void gschem_show_performance(struct rusage *before, struct rusage *after)
{
  float a_cputime, b_cputime, e_cputime;
  float a_systime, b_systime, e_systime;

  a_cputime = after->ru_utime.tv_sec + after->ru_utime.tv_usec / 1000000.0;
  b_cputime = before->ru_utime.tv_sec + before->ru_utime.tv_usec / 1000000.0;
  e_cputime = a_cputime - b_cputime;
  a_systime = after->ru_stime.tv_sec + after->ru_stime.tv_usec / 1000000.0;
  b_systime = before->ru_stime.tv_sec + before->ru_stime.tv_usec / 1000000.0;
  e_systime = a_systime - b_systime;

  printf("CPU time (secs): user=%.4f; system=%.4f\n", e_cputime, e_systime);
}

#endif /*__GSCHEM_DIAGNOSTICS__ */
