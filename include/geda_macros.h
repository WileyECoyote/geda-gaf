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
#define GLIST_NEXT(lst)     ((lst) ? (((GList *)(lst))->next) : NULL)

#define NEXT(i) i = GLIST_NEXT(i)
#define PREVIOUS(i) i = GLIST_PREVIOUS(i)