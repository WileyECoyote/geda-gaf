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
