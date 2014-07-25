/* C header                                           -*- geda_macros.h -*-
 * file: geda_macros.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2014 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/* Need conditional inclusion but holding off since this macros will yelp
   if there is a double inclusion of a header file */

#ifndef lambda
#define lambda GFunc lambda_func
#define foreach(glist) g_list_foreach(glist, (GFunc) lambda_func, NULL);
#define mapcar(gslist) g_slist_foreach(gslist, (GFunc) lambda_func, NULL);
#else
#error lambda already defined!
#endif

#define GLIST_PREVIOUS(lst) ((lst) ? (((GList *)(lst))->prev) : NULL)
#define GLIST_NEXT(lst) ((lst) ? (((GList *)(lst))->next) : NULL)

#define NEXT(i) i = GLIST_NEXT(i)
#define PREVIOUS(i) i = GLIST_PREVIOUS(i)

#define BUG_MSG(mesg) fprintf (stderr, "File %s, <%s> at line %d: %s\n", \
                                     __FILE__, __func__, __LINE__, mesg);


#define BUG_IMSG(mesg, val) fprintf (stderr, "File %s, <%s> at line %d: %s=%d\n", \
                                     __FILE__, __func__, __LINE__, mesg, val);

#define BUG_TRACE(mesg) fprintf (stderr, "File %s, <%s> at line %d: %s\n", \
                                     __FILE__, __func__, __LINE__, mesg); \
                        geda_backtrace();

#define BUG_ITRACE(mesg, val) fprintf (stderr, "File %s, <%s> at line %d: %s=%d\n", \
                                     __FILE__, __func__, __LINE__, mesg, val); \
                              geda_backtrace();
