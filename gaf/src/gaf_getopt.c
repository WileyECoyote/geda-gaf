/*
 * Copyright (c) 2002 Todd C. Miller <Todd.Miller@courtesan.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "../../config.h"
#endif

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "../include/gaf_getopt.h"

int     gaf_opterr = 1;        /* if error message should be printed */
int     gaf_optind = 1;        /* index into parent argv vector, 1003.2 must be 1 before any call. */
int     gaf_optopt;            /* character checked for validity */
int     gaf_optreset;          /* reset getopt */
char   *gaf_optarg;            /* argument associated with option */

#define PRINT_ERROR	((gaf_opterr) && (*options != ':'))

#define FLAG_PERMUTE	0x01	/* permute non-options to the end of argv */
#define FLAG_ALLARGS	0x02	/* treat non-options as args to option "-1" */
#define FLAG_LONGONLY	0x04	/* operate as getopt_long_only */

/* return values */
#define	BADCH		(int)'?'
#define	BADARG		((*options == ':') ? (int)':' : (int)'?')
#define	INORDER 	(int)1

#ifdef __CYGWIN__
static char EMSG[] = "";
#else
#define	EMSG		""
#endif

/* permitted values for its `has_arg' field...  */
enum
{
  no_argument = 0,      /* option never takes an argument	*/
  required_argument,    /* option always requires an argument	*/
  optional_argument     /* option may take an argument		*/
};

static int parse_long_options(char * const *, const char *,
                              const gaf_option *, int *, int);
static int gcd(int, int);

static char *place = EMSG;    /* option letter processing */

/* XXX: set gaf_optreset to 1 rather than these two */
static int nonopt_start = -1; /* first non option argument (for permute) */
static int nonopt_end = -1;   /* first option after non options (for permute) */

/* Error messages */
static const char recargchar[] = "option requires an argument -- %c";
static const char recargstring[] = "option requires an argument -- %s";
static const char ambig[] = "ambiguous option -- %.*s";
static const char noarg[] = "option doesn't take an argument -- %.*s";
static const char illoptchar[] = "unknown option -- %c";
static const char illoptstring[] = "unknown option -- %s";

static void _vwarnx(const char *fmt,va_list ap)
{
  (void)fprintf(stderr,"gaf: ");

  if (fmt != NULL) {
    (void)vfprintf(stderr,fmt,ap);
  }

  (void)fprintf(stderr,"\n");
}

static void warnx(const char *fmt,...)
{
  va_list ap;
  va_start(ap,fmt);
  _vwarnx(fmt,ap);
  va_end(ap);
}

/* Compute the greatest common divisor of a and b */
static int gcd(int a, int b)
{
  int c;

  c = a % b;

  while (c != 0) {
    a = b;
    b = c;
    c = a % b;
  }

  return (b);
}

/*! Helper for getopt_internal
 * Exchange the block from nonopt_start to nonopt_end with the block
 * from nonopt_end to opt_end while keeping the same order of arguments
 * in each block.
 */
static void
permute_args(int panonopt_start, int panonopt_end, int opt_end, char * const *nargv)
{
  int cyclelen, i, j, ncycle, nnonopts, nopts;
  char *swap;

  /* compute lengths of blocks and number and size of cycles */
  nnonopts = panonopt_end - panonopt_start;
  nopts = opt_end - panonopt_end;
  ncycle = gcd(nnonopts, nopts);
  cyclelen = (opt_end - panonopt_start) / ncycle;

  for (i = 0; i < ncycle; i++) {

    int cstart, pos;

    cstart = panonopt_end+i;
    pos = cstart;

    for (j = 0; j < cyclelen; j++) {

      if (pos >= panonopt_end) {
        pos -= nnonopts;
      }
      else {
        pos += nopts;
      }

      swap = nargv[pos];

      /* LINTED const cast */
      ((char **) nargv)[pos] = nargv[cstart];

      /* LINTED const cast */
      ((char **)nargv)[cstart] = swap;
    }
  }
}

/*! \internal Parse argc/argv argument vector.
 * Called by user level routines, this is a standard get options code
 * that is included here for use by the gaf back-end routines because
 * the getopt on MinGW/MSYS does not work correctly. Under MinGW back
 * end routines are passed the entire command-line again, including
 * options previously options parsed by main. This does not occur on
 * other platforms.
 */
static int
getopt_internal(int nargc, char * const *nargv, const char *options,
                const gaf_option *long_options, int *idx, int flags)
{
  char *oli;                 /* option letter list index */
  int optchar, short_too;

  static int posixly_correct = -1;

  if (options == NULL) {
    return (-1);
  }

  /* XXX Some GNU programs (like cvs) set gaf_optind to 0 instead of
   * XXX using gaf_optreset.  Work around this...
   */
  if (gaf_optind == 0) {
    gaf_optind = gaf_optreset = 1;
  }

  /* Disable GNU extensions if POSIXLY_CORRECT is set or options
   * string begins with a '+'.
   *
   * CV, 2009-12-14: Check POSIXLY_CORRECT anew if gaf_optind == 0 or
   *                 gaf_optreset != 0 for GNU compatibility.
   */
  if (posixly_correct == -1 || gaf_optreset != 0) {
    posixly_correct = (getenv("POSIXLY_CORRECT") != NULL);
  }

  if (*options == '-') {
    flags |= FLAG_ALLARGS;
  }
  else if (posixly_correct || *options == '+') {
    flags &= ~FLAG_PERMUTE;
  }

  if (*options == '+' || *options == '-') {
    options++;
  }

  gaf_optarg = NULL;

  if (gaf_optreset) {
    nonopt_start = nonopt_end = -1;
  }

  start:

  if (gaf_optreset || !*place) {		/* update scanning pointer */

    gaf_optreset = 0;

    if (gaf_optind >= nargc) {          /* end of argument vector */

      place = EMSG;

      if (nonopt_end != -1) {
        /* do permutation, if we have to */
        permute_args(nonopt_start, nonopt_end,
                     gaf_optind, nargv);
        gaf_optind -= nonopt_end - nonopt_start;
      }
      else if (nonopt_start != -1) {
        /*
         * If we skipped non-options, set gaf_optind
         * to the first of them.
         */
        gaf_optind = nonopt_start;
      }
      nonopt_start = nonopt_end = -1;
      return (-1);
    }

    if (*(place = nargv[gaf_optind]) != '-' ||
      (place[1] == '\0' && strchr(options, '-') == NULL))
    {
      place = EMSG;		/* found non-option */
      if (flags & FLAG_ALLARGS) {
        /*
         * GNU extension:
         * return non-option as argument to option 1
         */
        gaf_optarg = nargv[gaf_optind++];
        return (INORDER);
      }

      if (!(flags & FLAG_PERMUTE)) {
        /*
         * If no permutation wanted, stop parsing
         * at first non-option.
         */
        return (-1);
      }

      /* do permutation */
      if (nonopt_start == -1) {
        nonopt_start = gaf_optind;
      }
      else if (nonopt_end != -1) {
        permute_args(nonopt_start, nonopt_end,
                     gaf_optind, nargv);
        nonopt_start = gaf_optind -
        (nonopt_end - nonopt_start);
        nonopt_end = -1;
      }
      gaf_optind++;
      /* process next argument */
      goto start;
    }

    if (nonopt_start != -1 && nonopt_end == -1) {
      nonopt_end = gaf_optind;
    }

    /*
     * If we have "-" do nothing, if "--" we are done.
     */
    if (place[1] != '\0' && *++place == '-' && place[1] == '\0') {
      gaf_optind++;
      place = EMSG;
      /*
       * We found an option (--), so if we skipped
       * non-options, we have to permute.
       */
      if (nonopt_end != -1) {
        permute_args(nonopt_start, nonopt_end,
                     gaf_optind, nargv);
        gaf_optind -= nonopt_end - nonopt_start;
      }
      nonopt_start = nonopt_end = -1;
      return (-1);
    }
  }

  /*
   * Check long options if:
   *  1) we were passed some
   *  2) the arg is not just "-"
   *  3) either the arg starts with -- we are getopt_long_only()
   */
  if (long_options != NULL && place != nargv[gaf_optind] &&
    (*place == '-' || (flags & FLAG_LONGONLY)))
  {
    short_too = 0;
    if (*place == '-') {
      place++;		/* --foo long option */
    }
    else if (*place != ':' && strchr(options, *place) != NULL) {
      short_too = 1;		/* could be short option too */
    }
    optchar = parse_long_options(nargv, options, long_options,
                                 idx, short_too);
    if (optchar != -1) {
      place = EMSG;
      return (optchar);
    }
  }

  if ((optchar = (int)*place++) == (int)':' ||
    (optchar == (int)'-' && *place != '\0') ||
    (oli = (char*)strchr(options, optchar)) == NULL)
  {
    /*
     * If the user specified "-" and  '-' is not listed in
     * options, return -1 (non-option) as per POSIX.
     * Otherwise, it is an unknown option character (or ':').
     */
    if (optchar == (int)'-' && *place == '\0') {
      return (-1);
    }

    if (!*place) {
      ++gaf_optind;
    }

    if (PRINT_ERROR) {
      warnx(illoptchar, optchar);
    }
    gaf_optopt = optchar;
    return (BADCH);
  }

  if (long_options != NULL && optchar == 'W' && oli[1] == ';') {

    /* -W long-option */
    if (*place) {                        /* no space */
      /* NOTHING */;
    }
    else if (++gaf_optind >= nargc) {	/* no arg */
      place = EMSG;
      if (PRINT_ERROR)
        warnx(recargchar, optchar);
      gaf_optopt = optchar;
      return (BADARG);
    } else				/* white space */
      place = nargv[gaf_optind];
    optchar = parse_long_options(nargv, options, long_options,
                                 idx, 0);
    place = EMSG;
    return (optchar);
  }

  if (*++oli != ':') {     /* doesn't take argument */
    if (!*place)
      ++gaf_optind;
  }
  else { /* takes (optional) argument */

    gaf_optarg = NULL;

    if (*place)	{		/* no white space */
      gaf_optarg = place;
    }
    else if (oli[1] != ':') {	/* arg not optional */

      if (++gaf_optind >= nargc) {	/* no arg */

        place = EMSG;

        if (PRINT_ERROR) {
          warnx(recargchar, optchar);
        }
        gaf_optopt = optchar;
        return (BADARG);
      }
      else {
        gaf_optarg = nargv[gaf_optind];
      }
    }

    place = EMSG;
    ++gaf_optind;

  }

  /* dump back option letter */
  return (optchar);
}

/*!
 * \brief gaf get options
 * \par Function Description
 *  getopt -- arse argc/argv argument vector.
 *
 * [eventually this will replace the BSD getopt]
 *
 * \param [in] nargc    Integer count of command line arguments
 * \param [in] nargv    Pointer to an array of pointers to arguments
 * \param [in] options  Pointer to short options specifications
 */
int gaf_getopt(int nargc, char * const *nargv, const char *options)
{
  /* We don't pass FLAG_PERMUTE to getopt_internal() since
   * the BSD getopt(3) (unlike GNU) has never done this.
   *
   * Furthermore, since many privileged programs call getopt()
   * before dropping privileges it makes sense to keep things
   * as simple (and bug-free) as possible.
   */
  return (getopt_internal(nargc, nargv, options, NULL, NULL, 0));
}

/*!
 * \brief Parse long options
 * \par Function Description
 *  Parse long options in argc/argv argument vector.
 *
 * \param [in] nargv         Pointer to an array of pointers to arguments
 * \param [in] options       Pointer to short options specifications
 * \param [in] long_options  Pointer to gaf_option structure
 * \param [in] idx           Pointer to integer set to the index of the long
 *                           option relative to long_options or NULL.
 * \param [in] short_too     Integer flag could be short option
 *
 * \returns -1 if short_too is set and the option does not match long_options.
 */
static int
parse_long_options(char * const *nargv, const char *options,
                   const gaf_option *long_options, int *idx, int short_too)
{
  char  *current_argv, *has_equal;
  size_t current_argv_len;
  int i, ambiguous, match;

#define IDENTICAL_INTERPRETATION(_x, _y)                         \
  (long_options[(_x)].has_arg == long_options[(_y)].has_arg &&   \
  long_options[(_x)].flag == long_options[(_y)].flag &&          \
  long_options[(_x)].val == long_options[(_y)].val)

  current_argv = place;
  match = -1;
  ambiguous = 0;

  gaf_optind++;

  if ((has_equal = strchr(current_argv, '=')) != NULL) {
    /* argument found (--option=arg) */
    current_argv_len = has_equal - current_argv;
    has_equal++;
  }
  else {
    current_argv_len = strlen(current_argv);
  }

  for (i = 0; long_options[i].name; i++) {

    /* Find matching long option */
    if (strncmp(current_argv, long_options[i].name, current_argv_len))
      continue;

    /* Check for exact match */
    if (strlen(long_options[i].name) == current_argv_len) {
      match = i;
      ambiguous = 0;
      break;
    }

    /* If this is a known short option, don't allow
     * a partial match of a single character.
     */
    if (short_too && current_argv_len == 1)
      continue;

    if (match == -1) {	/* partial match */
      match = i;
    }
    else if (!IDENTICAL_INTERPRETATION(i, match)) {
      ambiguous = 1;
    }
  }

  if (ambiguous) {

    /* ambiguous abbreviation */
    if (PRINT_ERROR) {
      warnx(ambig, (int)current_argv_len, current_argv);
    }

    gaf_optopt = 0;
    return (BADCH);
  }

  if (match != -1) {     /* option found */

    if (long_options[match].has_arg == no_argument  && has_equal) {

      if (PRINT_ERROR) {
        warnx(noarg, (int)current_argv_len, current_argv);
      }

      /* XXX: GNU sets gaf_optopt to val regardless of flag */

      if (long_options[match].flag == NULL) {
        gaf_optopt = long_options[match].val;
      }
      else {
        gaf_optopt = 0;
      }
      return (BADARG);
    }

    if (long_options[match].has_arg == required_argument ||
      long_options[match].has_arg == optional_argument)
    {
      if (has_equal) {
        gaf_optarg = has_equal;
      }
      else if (long_options[match].has_arg == required_argument) {
        /*  optional argument doesn't use next nargv */
        gaf_optarg = nargv[gaf_optind++];
      }
    }

    if ((long_options[match].has_arg == required_argument) &&
      (gaf_optarg == NULL))
    {

      /*  Missing argument; leading ':' indicates no error
       * should be generated.
       */
      if (PRINT_ERROR) {
        warnx(recargstring, current_argv);
      }

      /* XXX: GNU sets gaf_optopt to val regardless of flag
       */
      if (long_options[match].flag == NULL) {
        gaf_optopt = long_options[match].val;
      }
      else {
        gaf_optopt = 0;
      }

      --gaf_optind;
      return (BADARG);
    }
  }
  else {          /* unknown option */

    if (short_too) {
      --gaf_optind;
      return (-1);
    }

    if (PRINT_ERROR) {
      warnx(illoptstring, current_argv);
    }

    gaf_optopt = 0;
    return (BADCH);
  }

  if (idx) {
    *idx = match;
  }

  if (long_options[match].flag) {
    *long_options[match].flag = long_options[match].val;
    return (0);
  }
  else {
    return (long_options[match].val);
  }

#undef IDENTICAL_INTERPRETATION
}

/*!
 * \brief Custom Get long options
 * \par Function Description
 *  This is just a wrapper for getopt_internal
 */
int gaf_getopt_long(int nargc, char * const *nargv, const char *options, const gaf_option *long_options, int *idx)
{
  return (getopt_internal(nargc, nargv, options, long_options, idx, FLAG_PERMUTE));
}
