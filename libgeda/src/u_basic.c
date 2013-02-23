/* gEDA - GPL Electronic Design Automation

 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
 */
/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * Date: November, 17, 2012
 * Contributing Author: Wiley Edward Hill
 */

#include <config.h>

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <ctype.h>
#include <ascii.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief itoa() for c
 *  \par Function Description
 *  Translate an integer to askii, like itoa cpp function
 *
 * \copyright public domain
 * \author ArkM
 *
 *  @param[in]  value  int to value to convert.
 *  @param[in]  str    ptr to array for the results
 *  @param[in]  radix  int base to resolve.
 *
 * \usage
 *
 *  char s_val[digits];  <-- Declare char array, digits could be
 *                           macro subsitution or literal value.
 *  int number = 4;      <-- Some integer declared somewhere.
 *
 *  *str = int2str( number, s_val, 10 ));
 *
 * \example
 *  strcat(strbuffer, int2str( total, s_val, 10 ));
 */
char* int2str(int value, char* str, int radix) {

  static char dig[] ="0123456789"
                     "abcdefghijklmnopqrstuvwxyz";
  int n = 0, neg = 0;
  unsigned int v;
  char* p, *q;
  char c;

  if (radix == 10 && value < 0) {
    value = -value;
    neg = 1;
  }
  v = value;
  do {
    str[n++] = dig[v%radix];
    v /= radix;
  } while (v);
  if (neg)
  str[n++] = '-';
  str[n] = '\0';
  for (p = str, q = p + (n-1); p < q; ++p, --q)
  c = *p, *p = *q, *q = c;
  return str;
}

char *gh_scm2newstr (SCM str, unsigned int *lenp);
/*!
 * \brief return c pointer to SCM string.
 * \par Function Description
 * String utility function to get a c pointer to a scm string.
 * The caller is responsible for freeing the pointer.
 */
char *scm_2_cstring( char* scm_str_name) /* WEH: couldn't find it, made it */
{
  SCM s_symbol, s_value;
  size_t len;

  /* Now get string */
  s_symbol = scm_c_lookup(scm_str_name);
  s_value = scm_variable_ref(s_symbol);
  return (char*) gh_scm2newstr(s_value, &len);
}

void sort_string_array( char *strings[], size_t strings_size) {
    int cstring_cmp(const void *a, const void *b)
    {
       const char **ia = (const char **)a;
       const char **ib = (const char **)b;
       return strcmp(*ia, *ib);
        /* strcmp functions works exactly as expected from
        comparison function */
   }
    size_t strings_len = strings_size / sizeof(char *);

   /* sort array using qsort functions */
    qsort(strings, strings_len, sizeof(char *), cstring_cmp);
}

/* WEH: Maybe should be in <string.h> as inline */
/*! \brief Check for equal strings.
 *
 *  \par Function Description
 *  This function compares to strings and returns true if
 *  they are equal or fals if they are not.
 *
 *  \param [in] char* str1 is the string to be search
 *  \param [in] char* str1 is the string to search for
 *
 *  \retval TRUE if strings are equivalent, otherwise FALSE.
 */
bool strequal(const char *str1, const char *str2) /* WEH: Maybe should be in <string.h> */
{
  while ((*str1 == *str2) && (*str1)) { str1++; str2++; }
  return ((*str1 == (unsigned)NULL) && (*str2 == (unsigned)NULL));
}

/*! \brief strstr_rep for c
 *  \par Function Description
 *  replace substring in string with new string
 *
 *  @param[in]  original ptr to input string.
 *  @param[in]  old      ptr to the string to be replaced
 *  @param[in]  new      ptr to the replacement string.
 *
 * \example
 *           str = strstr_rep(str, "  ", " ");
 */
char *strstr_rep(char *original, const char *old, const char *new)
{
  char *str;
  char *temp;

    void do_replace(char *s_ptr, const char *old, const char *new) {
      int lenOld = strlen(old);
      int lenNew = strlen(new);
      char *ptr;

      while (*s_ptr)
      {
        ptr = strstr(s_ptr, old);
        if (ptr) {
            strcpy(temp, ptr + lenOld);
            *ptr = '\0';
            strcat(s_ptr, new);
            strcat(s_ptr, temp);
            s_ptr = ptr + lenNew;
        }
        else
            break;
      }
    }

  if ((temp = (char*)malloc(sizeof(char)*(strlen(original)+1))) == NULL)
    return(NULL);

  while(strstr(original, old)) {
     str = original;
     do_replace(str, old, new);
  }
  free(temp);
  return original;
}

/*! \brief Compare strings ignoring case.
 *
 *  \par Function Description
 *  This is a garden varity string compare using toupper
 *  on both inputs. This is commonly in standard libraries.
 *  but not all.
 *
 *  \param [in] char* str1 is the string to be search
 *  \param [in] char* str1 is the string to search for
 *
 *  \retval TRUE if strings are equivalent, otherwise FALSE.
 */
int stricmp(char *str1, char *str2)
{
  while (( toupper(*str1) == toupper(*str2)) && (*str1))
  {
    str1++; str2++;
  }
  return !((*str1 == '\0') && (*str2 == '\0'));
}

/*! \brief Compare n characters ignoring case.
 *
 *  \par Function Description
 *  Another garden varity string compare using toupper
 *  on both inputs. This is somthimes found in standard.
 *  libraries but not always.
 *
 *  \param [in] char* str1 is the string to be search
 *  \param [in] char* str1 is the string to search for
 *  \param [in] int   n is the number of char to compare
 *
 *  \retval 0 if the strings are equivalent, -1 if str2 if
 *  first mis-match is because str2 is greater, or 1 if the
 *  first mis-match is because str1 is greater.
 */
int strncmpi(char *str1, char *str2, int n)
{
  int i = 0;
  while (( toupper(*str1) == toupper(*str2)) && i < n)
  {
    str1++;
    str2++;
    i++;
  }
  if ( i == n)
    return 0;
  else
    if ((*str1 == *str2 ) && (!str1))
      return 0;
    else
      if ((*str1) && (!str2))
        return -1;
      else
        if ((*str2) && (!str1))
          return 1;
        else
          return ((*str1 > *str2 ) ? -1 : 1);
}

/*! \brief Find substring in string, ignore case.
 *
 *  \par Function Description
 *  This function uses stricmp or the previously defined  strncmpi
 *  to locate a substring in a string. This is not normally found in
 *  standard libraries but sometimes is. The difference between the
 *  stristr and stricmp returns a pointer rather than an integer.
 *
 *  \param [in] char* str1 is the string to be search
 *  \param [in] char* str1 is the string to search for
 *
 *  \retval char* to the first occurance of str2 in str2 or
 *  NULL if str2 is not contained in str1.
 */
char *stristr(char *str1, char *str2)
{
  char *ptr;

  /* if 2 is longer than 1, 2 can not be IN 1 */
  if (strlen(str2) > strlen(str1)) return NULL;

  /* if strings are the same length then can do */
  if (strlen(str2) == strlen(str1))
    return (!stricmp ( str1, str2)) ? str1 : NULL;

  for (ptr = str1; *ptr ; ++ptr) {
    if (strncmpi(ptr, str2, strlen(str2)) == 0)
      return ptr;
  }
  return NULL;
}

/*! \brief Replace substring in string.
 *
 *  \par Function Description
 *  This function replaces the first occurance of str1 with str2
 *  in the source. This version uses array indexes and dynamically
 *  allocates tempory storage. The Caller is responsible for insuring
 *  source is sufficiently large enough to hold the new string, ie
 *  original - old + new + 1.
 *
 *  \param [in] char* source is the string to be modified
 *  \param [in] char* old_str is the string to be replaced
 *  \param [in] char* new_str is the replacement for old_str
 *
 *  \retval char* source (the orginal pointer) or NULL if old_str
 *  was not not found in the source string or if there was a error
 *  allocating memory.
 *
 */
char *strsubst(char *source, char *old_str, char *new_str)
{
  unsigned int i, j, k;
  int position = -1;
  int length   = strlen(old_str);
  char *temp = NULL;
  size_t size;

  size = strlen (source)- length + strlen (new_str) + 1;
  temp = malloc(size);

  if (temp) { /* If memory was allocated */
    memset(temp, 0, size); /* initialize new memory */
    /* Getting starting position for replacement */
    for(i=0; source[i] && ( position == -1 ); ++i)
      for(j=i,k=0; source[j]==old_str[k]; j++,k++)
        if(!old_str[k+1]) position = i;

    /* Start replacing */
    if(position!=-1) {            /* if we found position */
      for(j=0; j<position; j++)      /* copy the prefix   */
        temp[j] = source[j];
      for(i=0; new_str[i]; i++,j++)      /* add the new string and */
        temp[j] = new_str[i];
      for(k=position+length; source[k]; k++,j++) /* remainder of source */
        temp[j] = source[k];
      temp[j] = '\0';                      /* then add terminator  */
      for(i=0; (source[i]=temp[i]); i++);  /* write back to source */
    }
    free(temp);
    return source;
  }
  else
    fprintf(stderr, "strsubst: Memory allocation error\n");
  return NULL;
}

/*! \brief Replace substring in string ignoring case
 *
 *  \par Function Description
 *  This function replaces the first occurance of str1 with str2
 *  in the source. This version dynamically allocates tempory storage
 *  and uses pointer returned from the previously defined stristr to
 *  get the first position of old_str in the source. The Caller is
 *  responsible for insuring source is sufficiently large enough to
 *  hold the new string, ie original - old + new + 1.
 *
 *  \param [in] char* source is the string to be modified
 *  \param [in] char* old_str is the string to be replaced
 *  \param [in] char* new_str is the replacement for old_str
 *
 *  \retval char* source (the orginal pointer) or NULL if old_str
 *  was not not found in the source string or if there was a error
 *  allocating memory.
 *
 */
char *strisubst(char *source, char *old_str, char *new_str)
{
  int length   = strlen(old_str);
  char *temp = NULL;
  char *ptr1;
  char *ptr2;
  size_t size;

  size = strlen (source)- strlen (old_str) + 1;
  temp = malloc(size);   /* assume all of the old is prefixed */
  if (temp) { /* If memory was allocated */
    memset(temp, 0, size); /* initialize new memory */

      /* Get pointer to the old string */
    if (!(ptr1 = stristr(source, old_str))) return NULL;

      /* get pointer to the end of the old string in the source */
    ptr2 = ptr1 + length; /* pointing to the old last char */

    if (*ptr2) /* if there are characters after the old string */
      strcpy(temp, ptr2); /* save them in the temp buffer */

    /* copy the new string to the source starting add the old position*/
    strcpy(ptr1, new_str); /* This also terminated the string for us */

    /* If there was as suffix, then add it */
    if (strlen (temp))
      strcat(ptr1, temp);

    free(temp);
    return source;
  }
  else
    fprintf(stderr, "strsubst: Memory allocation error\n");
  return NULL;
}

/* Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  The delimiter is what is passed in or spaces count starts at zero
 */
char *u_basic_breakup_string(char *string, char delimiter, int count)
{
  int i=0, j=0;
  int internal_counter=0;
  int done=FALSE;
  char *return_value;

  g_return_val_if_fail ((string != NULL),
                        NULL);

  /* skip over any leading white space */
  while(string[i] == ' ' && !string[i]) {
    i++;
  }

  /* Allocate space for temp string storage (+1 for null character) */
  return_value = malloc(sizeof(char)*(strlen(string) + 1));

  while(!done) {

    /* oops, ran out of string before we found what we were */
    /* looking for */
    if (i > strlen(string)) {
      free(return_value);
      return(NULL);
    }

    /* skip over any leading white space */
    while(string[i] == ' ' && string[i] != '\0') {
      i++;
    }

    j = 0;

    /* Old forgiving parsing */
    /*		while(string[i] != ',' && string[i] != ';' && */
    /*		      string[i] != ' ' && string[i] != '\0') {*/

    while(string[i] != delimiter && string[i] != '\0') {
      return_value[j] = string[i];
      i++; j++;
    }

    if (internal_counter == count)  {
      done = TRUE;
    } else {
      internal_counter++;
      i++; /* skip the offending character */
    }
  }

  return_value[j] = '\0';
  return(return_value);
}
int word_count(char* str) {
    int count = 0;
    while ( *str != ASCII_NUL) {
      if (*str == ASCII_SPACE ) ++count;
      ++str;
    }
    return count;

}
