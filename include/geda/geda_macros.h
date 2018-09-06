/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_macros.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill
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
 */

/** \file geda_macros.h
 *
 *  \remarks Need conditional inclusion but holding off since these macros
 *           will cause preprocessor to yelp if there is a double inclusion
 *           of this header file
 *
 *    \defgroup geda-global-macros Global General Purpose Macros
 *  @{\par This group contains general purpose Preprocessor Macros
 *    \ingroup geda-globals
 */

#ifndef lambda
#define lambda GFunc lambda_func
#define foreach(glist) g_list_foreach((GList*)glist, (GFunc) lambda_func, NULL);
#define mapcar(gslist) g_slist_foreach((GSList*)gslist, (GFunc) lambda_func, NULL);
#else
#error lambda already defined!
#endif

/* Guard C code in headers, while including them from C++ */
#ifdef  __cplusplus
# define BEGIN_DECLS  extern "C" {
# define END_DECLS    }
#else
# define BEGIN_DECLS
# define END_DECLS
#endif

#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 10000  + __GNUC_MINOR__ * 100  + __GNUC_PATCHLEVEL__)
#endif /* GCC_VERSION */

/* Define some gcc extension __attribute__ macros for portability
 * so they can be used on gcc without breaking non-gcc
 */
#if GCC_VERSION > 20700
#define GEDA_UNUSED  __attribute__((__unused__))
#else
#define GEDA_UNUSED
#endif

#if GCC_VERSION > 40407
#define GEDA_CONST  __attribute__((__const__))
#define GEDA_NO_INSTRUMENT __attribute__((__no_instrument_function__))
#else   /* !__GNUC__ */
#define GEDA_CONST
#define GEDA_NO_INSTRUMENT
#endif

#if     __GNUC__ >= 4
#define GEDA_NULL_TERMINATED __attribute__((__sentinel__))
#else
#define GEDA_NULL_TERMINATED
#endif

#ifdef __GNUC__
  #define WARN_UNUSED __attribute__((warn_unused_result))
  #define NOWARN_UNUSED __attribute__((unused))
#endif

#if GCC_VERSION > 30400
#define GEDA_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define GEDA_WARN_UNUSED_RESULT
#endif /* __GNUC__ */

/*! \def GedaMutex
 * Macro to define an anonymous mutex, GStaticMutex < 2.32
 * cannot be initialized to NULL. The second member i, is
 * padded 2 integers.
 */
#define GedaMutex(lv) \
  union { \
    void *p; \
    unsigned int i[4]; \
  } lv;

/*! \def GCC_DIAGNOSTIC_AWARE
 *  \brief Used to suppress some warnings in some header and implementation files.
 *  \par
 *   Some platforms, like CentOS and OpenBSD, use old compilers that do not
 *   understand -Wno-unknown-pragma.
 */
#define GCC_DIAGNOSTIC_AWARE (GCC_VERSION > 40603)

#define GLIST_PREVIOUS(lst) ((lst) ? (((GList *)(lst))->prev) : NULL)
#define GLIST_NEXT(lst) ((lst) ? (((GList *)(lst))->next) : NULL)

#define NEXT(i) i = GLIST_NEXT(i)
#define PREVIOUS(i) i = GLIST_PREVIOUS(i)

#define SOURCE_CONTINUE  TRUE
#define SOURCE_REMOVE    FALSE

#define BUG_MSG(mesg) fprintf(stderr, "File %s, <%s> at line %d: %s\n", \
                                   __FILE__, __func__, __LINE__, mesg);


#define BUG_IMSG(mesg, val) fprintf(stderr, "File %s, <%s> at line %d: %s=%d\n", \
                                        __FILE__, __func__, __LINE__, mesg, val);

#define BUG_PMSG(mesg, val) fprintf(stderr, "File %s, <%s> at line %d: %s=<%p>\n", \
                                        __FILE__, __func__, __LINE__, mesg, val);

#define BUG_TRACE(mesg) fprintf (stderr, "File %s, <%s> at line %d: %s\n", \
                                 __FILE__, __func__, __LINE__, mesg); \
                                 geda_utility_program_backtrace();

#define BUG_ITRACE(mesg, val) fprintf (stderr, "File %s, <%s> at line %d: %s=%d\n", \
                                       __FILE__, __func__, __LINE__, mesg, val); \
                                       geda_utility_program_backtrace();

/** @} endgroup geda-global-macros */