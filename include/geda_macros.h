/* Need conditional inclusion but holding off since this macros will yelp
   if there is a double inclusion of a header file */

#ifndef lambda
#define lambda GFunc lambda_func
#define foreach(glist) g_list_foreach(glist, (GFunc) lambda_func, NULL);
#else
  #error lambda already defined!
#endif
