/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2012-2016 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2012-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: November, 17, 2012
 * Contributing Author: Wiley Edward Hill
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <ctype.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

/**
 * \brief String Utility Function
 * \par
 *  A collection of general string utility functions for geda-gaf
 *  applications.
 *
 * \note There is no Scheme API for these functions.
 *
 * \defgroup Libgeda-String-Utilities String Utility Functions
 * @{
 */

#define USS_BUFFER_SIZE 256

/*! U0601
 * \brief Append variable number of strings together
 * \par Function Description
 *  The string are not formated.
 *
 * \param [in] string1 The first string,
 * \param [in] ...     NULL terminated list strings.
 *
 * \retval char* to all of the string as a new allocation.
 * \remarks Caller should GEDA_FREE returned pointer.
 *
 * \warning **** the last argument MUST be NULL! ****
 */
char *geda_utility_string_concat (const char *string1, ...)
{
  char *concat;
  char *ptr;
  char *str;

  unsigned int length;
  va_list args;

  if (!string1) {
    return (NULL);
  }

  /* Determine memory requirements */
  length = strlen (string1) + 1;

  va_start (args, string1);
  str = va_arg (args, char*);

  /* Loop thru each optional argument and accumulate length */
  while (str) {
    length += strlen (str);
    str     = va_arg (args, char*);
  }
  va_end (args);

  /* Allocate the memory need for the final string */
  if ((concat = (char*)malloc(sizeof(char)*length)) == NULL)
    return (NULL);

  ptr = concat; /* get ptr to start of new allocation */

  /* Copy characters from string 1 */
  do
    *ptr++ = *string1;
  while (*string1++ != '\0');

  ptr--;

  va_start (args, string1);
  str = va_arg (args, char*);

  /* Loop thru each optional argument again and copy each */
  while (str) {
    do
      *ptr++ = *str;
    while (*str++ != '\0');
    ptr--;
    str = va_arg (args, char*);
  }
  va_end (args);

  return concat;
}

/*! U0602
 * \brief Get Valid UTF8 string from string
 * \par Function Description
 *  Returns a new allocated utf8 encoding string containing valid
 *  characters from the array pointed to by \a instr. Characters
 *  following an invalid char are omitted.
 *  This function provides the same functionality as g_utf8_make_valid
 *  which is not avaliable until glib 2.52.
 */
char *geda_utility_string_get_valid_utf8 (const char *instr)
{
  if (instr != NULL) {

    const char *remainder;
    const char *invalid;
          char *outstr;
          char *string;

    int remaining_bytes;

    string = NULL;
    remainder = instr;
    remaining_bytes = strlen (instr);

    while (remaining_bytes != 0) {

      int valid_bytes;

      if (g_utf8_validate (remainder, remaining_bytes, &invalid))
        break;

      valid_bytes = invalid - remainder;

      if (string == NULL) {
        string =  malloc(remaining_bytes + valid_bytes + 7);
      }
      else {

          char *buffer;

          size_t len = strlen(string) + valid_bytes + 6;

          buffer = (char*)realloc(string, len);

          if (!buffer)
            break;

          string = buffer;
          strncat(string, remainder, len);
      }

      /* append U+FFFD REPLACEMENT CHARACTER */
      strcat(string, "\357\277\275");

      remaining_bytes -= valid_bytes + 1;
      remainder = invalid + 1;
    }

    if (string == NULL) {
      outstr = geda_utility_string_strdup (instr);
    }
    else {

      outstr = geda_utility_string_concat(string, remainder, NULL);

      free (string);
    }

    return outstr;
  }
  return NULL;
}


/*! U0603
 * \brief itoa for c
 * \par Function Description
 *  Translate an integer to askii, like itoa cpp function
 *
 * \copyright public domain
 * \author ArkM
 *
 * \param[in]  value  int value to convert.
 * \param[in]  str    ptr to array for the results
 * \param[in]  radix  int base to resolve.
 *
 * usage:
 *
 *  char s_val[digits];  <-- Declare char array, digits could be
 *                           macro subsitution or literal value.
 *  int number = 4;      <-- Some integer declared somewhere.
 *
 *  *str = geda_utility_string_int2str( number, s_val, 10 ));
 *
 *  example:  strcat(strbuffer, geda_utility_string_int2str( total, s_val, 10 ));
 */
char *geda_utility_string_int2str(int value, char *str, int radix) {

  if (str) {

    static char dig[] = "0123456789"
                        "abcdefghijklmnopqrstuvwxyz";
    int n = 0, neg = 0;
    unsigned int v;
    char *p, *q;
    char  c;

    if (radix == 10 && value < 0) {
      value = -value;
      neg   = 1;
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
  }
  return str;
}

/*! U0604
 * \brief Interrogate string for alpha-numeric characters
 * \par Function Description
 *  Determines if a string contains only alpha-numeric character.
 *
 * \param[in] str  The string to parse.
 *
 * \returns TRUE if all of the characters in \a str are alpha-numeric.
 */
bool geda_utility_string_isalnum (const char *str)
{
  int i;

  if (!str)
    return FALSE;

  for (i = 0; str[i]; i++)
    if (!isalnum(str[i]))
      return FALSE;

  return TRUE;
}

/*! U0605
 * \brief Find substring in string, ignore case
 * \par Function Description
 *  Uses geda_utility_string_stricmp or geda_utility_string_strncmpi to
 *  locate a substring in a string. This is not normally found in standard
 *  libraries but sometimes is. The difference between geda_utility_string_istr
 *  and geda_utility_string_stristr is that geda_utility_string_istr returns a
 *  pointer rather than an integer.
 *
 * \param [in] str1 is the string to be search
 * \param [in] str2 is the string to search for
 *
 * \retval char* to the first occurance of str2 in str1 or
 *               NULL if str2 is not contained in str1.
 *
 * \sa geda_utility_string_stristr
 */
const char *geda_utility_string_istr(const char *str1, const char *str2)
{
  const char *ptr;
  int len1, len2;

  if ( ! str1 || ! str2)
    return NULL;

  ptr  = NULL;

  len1 = strlen(str1);
  len2 = strlen(str2);

  /* if 2 is longer than 1, 2 can not be IN 1 */
  if (len2 > len1) return NULL;

  /* if strings are the same length then can do */
  if (len2 == len1)
    return (!geda_utility_string_stricmp ( str1, str2)) ? str1 : NULL;

  for (ptr = str1; *ptr ; ++ptr) {
    if (geda_utility_string_strncmpi(ptr, str2, len2) == 0)
      return ptr;
  }

  return NULL;
}

/*! U0606
 * \brief Parse a c String for X and Y Integer Pair
 * \par Function Description
 *  Iterates over a string looking for askii digits, parenthesis are
 *  ignored. The first character digits are interpreted as the string for
 *  X unless a comma was previously encountered, in which case the string
 *  is interpreted as the Y. If Y is set and X has not been set then X is
 *  presumed zero, and this allows input such as ",600" to mean (0,600).
 *  If only one value was interpreted and a comma had not be encountered,
 *  the Y is presumed to be zero. If the string contains two sets of valid
 *  digits, X and Y, then the comma may also be an ASKii SPACE character.
 *  If neither X nor Y is interpreted then False is returned. Either X or
 *  Y, or both, can be NULL, the returned value will still indicate the
 *  validity of the \a string.
 *
 * \param[in]  string  The string to parse.
 * \param[out] *x      Set to integer X value
 * \param[out] *y      Set to integer Y value
 *
 * \return True of the string was accepted and the X,Y value are valid
 *
 *  acceptable formats: "(4500,380)", "4500,380", "5", ",72"
 */
int geda_utility_string_parse_xy(const char *string, int *x, int *y)
{
  char *x_str, *y_str;
  int   x_sig,  y_sig;
  int   icomma;
  int   valid;

  icomma = -1;
  valid  = FALSE;
  x_sig  = 1;
  y_sig  = 1;
  x_str  = NULL;
  y_str  = NULL;

  if (string) {

    char *buffer;
    int   index;
    int   length;

    buffer = geda_utility_string_strdup(string);
    length = strlen(string);

    for (index = 0; index < length; index++) {

      if (!buffer[index])
        break;

      if (isdigit(buffer[index])) {
        if (!x_str && icomma < 0) {
          x_str = &buffer[index];
        }
        else if (!y_str && icomma >= 0) {
          y_str = &buffer[index];
          if (!x_str) {
            x_str = "0";
          }
        }
      }
      else if (buffer[index] == ASCII_HYPHEN_MINUS) {
        if (icomma < 0) {
          x_sig = -1;
        }
        else {
          y_sig = -1;
        }
      }
      else if (buffer[index] == ASCII_COMMA) {
        icomma = index;
      }
      else if (buffer[index] == ASCII_SPACE) {
        icomma = index;
      }
      else if (buffer[index] == ASCII_LEFT_PARENTHESIS ||
               buffer[index] == ASCII_RIGHT_PARENTHESIS ) {
        buffer[index] = ASCII_SPACE;
      }
    }

    if (!y_str) {
      y_str = "0";
    }

    if (x_str && y_str) {
      if (icomma >= 0) {
        buffer[icomma] = '\0';
      }
      if (x)
        *x = atoi(x_str) * x_sig;
      if (y)
        *y = atoi(y_str) * y_sig;
      valid = 1;
    }
    free(buffer);
  }
  else {
    valid = 0;
  }
  return valid;
}

/*! U0607
 * \brief Remove Last Line Feed and Carriage Return from a String
 * \par Function Description
 *  This function replaces trailing 0x0D and 0x0A, Carriage Return
 *  and Line feed characters respectively, with a NULL.
 *
 * \sa geda_utility_string_remove_nl
 *
 * \note used by geda_text_object_read
 */
char *geda_utility_string_remove_last_nl(char *string)
{
  unsigned int length;

  if (!string)
    return NULL;

  length = strlen(string);

  if (length) {
    if (string[length-1] == '\n' || string[length-1] == '\r') {
      string[length-1] = '\0';
    }
  }

  return(string);
}

/*! U0608
 * \brief Remove Line Feed and Carriage Return Characters from string
 * \par Function Description
 *  This function searches a string and replaces the first occurrences of
 *  0x0D and 0x0A, Carriage Return and Line feed characters respectively,
 *  with a NULL.
 *
 * \sa geda_utility_string_remove_last_nl
 *
 * \note used by geda_text_object_read
 */
char *geda_utility_string_remove_nl(char *string)
{
  if (string) {

    int i = 0;
    while(string[i] != '\0' && string[i] != '\n' && string[i] != '\r') {
      i++;
    }

    string[i] = '\0';
  }

  return (string);
}

/*! U0609
 * \brief Return c pointer to SCM string
 * \par Function Description
 *  String utility function to get a c pointer to a scm string.
 * \remarks  caller is responsible for freeing the pointer.
 */
char *geda_utility_string_scm2c(SCM scm_str)
{
  if (scm_str) {

    char *s_str, *c_str;

    s_str = scm_to_utf8_string (scm_str);
    c_str = geda_utility_string_strdup (s_str);

    free (s_str);

    return c_str;
  }
  return NULL;
}

/*! U0610
 * \brief Sort an array of Characters
 * \par Function Description
 *  sort array using qsort functions
 */
void geda_utility_string_sort_array( char *strings[], size_t strings_size) {

  int cstring_cmp(const void *a, const void *b)
  {
    const char **ia = (const char **)a;
    const char **ib = (const char **)b;
    return strcmp(*ia, *ib);
    /* strcmp functions works exactly as expected from
     * comparison function */
  }
  size_t strings_len = strings_size / sizeof(char *);

  /* sort array using qsort functions */
  qsort(strings, strings_len, sizeof(char*), cstring_cmp);
}

/* Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda */
/*! U0611
 * \brief Split a string using an optional delimiter
 * \par Function Description
 *  If \a delimiter is zero a copy of string is returned.
 *
 * \note The orginal string is NOT altered.
 *
 * \remarks returned pointer should be freed.
 *
 * \param [in] string     is the string to parse
 * \param [in] delimiter  optional delimiter
 * \param [in] count      which base 0 splits to return
 *
 * \returns pointer to allocated string[count], or NULL if \a string
 *          is NULL, or \a string if \a delimiter is zero.
 */
char *geda_utility_string_split(const char *string, char delimiter, int count)
{
  if (string != NULL) {

    char *return_value;
    int done=FALSE;
    int internal_counter = 0;
    int i = 0, j = 0;

    /* skip over any leading white space */
    while (string[i] == ' ' && !string[i]) {
      i++;
    }

    /* Allocate space for temp string storage (+1 for null character) */
    return_value = GEDA_MEM_ALLOC(sizeof(char)*(strlen(string) + 1));

    while (!done) {

      /* oops, ran out of string before we found what we were */
      /* looking for */
      if (i > strlen(string)) {
        GEDA_FREE(return_value);
        return(NULL);
      }

      /* skip over any leading white space */
      while(string[i] == ' ' && string[i] != '\0') {
        i++;
      }

      j = 0;

      /* Old forgiving parsing */
      /*          while(string[i] != ',' && string[i] != ';' && */
      /*                string[i] != ' ' && string[i] != '\0') {*/

      while(string[i] != delimiter && string[i] != '\0') {
        return_value[j] = string[i];
        i++; j++;
      }

      if (internal_counter == count)  {
        done = TRUE;
      }
      else {
        internal_counter++;
        i++; /* skip the offending character */
      }
    }

    return_value[j] = '\0';

    return(return_value);
  }
  return NULL;
}

/*! U0612 - geda_sprintf
 * \brief  Get formated string using printf like specifiers
 * \par Function Description
 * \returns a newly allocated string that is the result of
 *  the substitution of the variable arguments into \a format.
 *
 * \remarks returned string must be freed using GEDA_FREE.
 */
char *geda_utility_string_sprintf (const char *format, ...)
{
  char *buffer;
  int   size;

  va_list args;

  if (!format) {
    return (NULL);
  }

  va_start (args, format);
  size = geda_utility_string_strsize(format, args);
  va_end (args);

  if (size < 0) {
    return NULL;
  }

  size++;

  va_start (args, format);

  if (size < USS_BUFFER_SIZE) {

    char local_buffer[USS_BUFFER_SIZE];

    vsprintf (&local_buffer[0], format, args);

    buffer = (char*)GEDA_MEM_ALLOC(size);

    if (buffer) {
      buffer = memcpy(buffer, &local_buffer[0], size);
    }
  }
  else {

    buffer = (char*)GEDA_MEM_ALLOC(size);

    if (buffer) {
      buffer[size - 1] = '\0';
      vsprintf (buffer, format, args);
    }
  }

  va_end (args);

  return buffer;
}

/*! U0613
 * \brief  Get a Duplicate string
 * \par Function Description
 * \returns a newly allocated string copy of \a str.
 * \remarks returned string must be freed using GEDA_FREE.
 */
char *geda_utility_string_strdup (const char *str)
{
  if (!str)
    return NULL;

  size_t len = 1 + strlen(str);

  char  *ptr = (char*)GEDA_MEM_ALLOC(len);

  return ptr ? (char*)memcpy(ptr, str, len) : NULL;
}

/*! U0614
 * \brief Check for equal strings
 * \par Function Description
 *  This function compares two strings and returns TRUE if
 *  they are equal or FALSE if they are not.
 *
 * \param [in] str1 is the string to be search
 * \param [in] str2 is the string to search for
 *
 * \retval TRUE if strings are equivalent, otherwise FALSE.
 */
bool geda_utility_string_strequal(const char *str1, const char *str2)
{
  if (!str1 || !str2)
    return -0;
  while ((*str1 == *str2) && (*str1 != '\0')) { str1++; str2++; }
  return ((*str1 == '\0') && (*str2 == '\0'));
}

/*! U0615
 * \brief Compare strings ignoring case
 * \par Function Description
 *  This is a garden varity string compare using toupper on both
 *  inputs. This is commonly in standard libraries, but not always.
 *
 * \param [in] str1 is the string to be search
 * \param [in] str2 is the string to search for
 *
 * \remarks str1 and str2 must not be NULL!
 *
 * \retval 0 if the strings are equivalent, or
 *         1 if the strings are NOT equivalent.
 *
 * \note This is not the same as strcasecmp, strcasecmp returns an
 *       integer. geda_utility_string_stricmp returns a Boolean!
 */
bool geda_utility_string_stricmp(const char *str1, const char *str2)
{
  while ((toupper(*str1) == toupper(*str2)) && (*str1))
  {
    str1++; str2++;
  }
  return !((*str1 == '\0') && (*str2 == '\0'));
}

/*! U0616
 * \brief Non case sensitive search for string in a string
 * \par Function Description
 * \retval A non negative result is the position needle was found
 *  in haystack, a negative result means needle was not found OR
 *  one of the two inputs is NULL.
 */
int geda_utility_string_stristr ( const char *haystack, const char *needle)
{
  int result;

  if (!needle || !haystack) {
    result = -1;
  }
  else {

    char *ptr = strstr(haystack, needle);

    if (!ptr) {

      char *upper_needle;
      char *upper_haystack;

      upper_needle   = g_ascii_strup( needle, -1);
      upper_haystack = g_ascii_strup( haystack, -1);

      ptr = strstr(upper_haystack, upper_needle);

      result = ptr ? ptr - upper_haystack : -1;

      GEDA_FREE(upper_needle);
      GEDA_FREE(upper_haystack);
    }
    else {
      result = ptr - haystack;
    }
  }
  return result;
}

/*! U0617
 * \brief Replace substring in string ignoring case
 * \par Function Description
 *  This function replaces the first occurrence of str1 with str2 in
 *  the \a source string. This version dynamically allocates temporary
 *  storage and uses pointer returned from the geda_utility_string_istr
 *  to get the starting position of \a old_str in the source. The Caller
 *  is responsible for insuring source is sufficiently large enough to
 *  hold the new string, ie original - old + new + 1.
 *
 * \param [in] source  source is the string to be modified
 * \param [in] old_str old_str is the string to be replaced
 * \param [in] new_str is the replacement for old_str
 *
 * \retval char* source (the orginal pointer) or NULL if old_str
 *         was not found in the source string or if there was a error
 *         allocating memory.
 */
char *geda_utility_string_strisubst(char *source, char *old_str, char *new_str)
{
  if (source && old_str && new_str) {

    unsigned int length;
    unsigned int size;

    char *ptr1;
    char *temp;

    length = strlen(old_str);

    size = strlen (source) - length + 1;

    temp = malloc(size);     /* assume all of the old is prefixed */

    if (temp) {              /* If memory was allocated */

      char *ptr2;

      memset(temp, 0, size); /* initialize new memory */

      /* Get pointer to the old string */
      if (!(ptr1 = (char*)geda_utility_string_istr(source, old_str))) {
        free(temp);
        return NULL;
      }

      /* get pointer to the end of the old string in the source */
      ptr2 = ptr1 + length;  /* pointing to the old last char */

      if (*ptr2) {/* if there are characters after the old string */
        strcpy(temp, ptr2);  /* save them in the temp buffer */
      }

      /* copy the new string to the source starting add the old position*/
      strcpy(ptr1, new_str); /* This also terminates the string for us */

      /* If there was as suffix, then add it */
      if (strlen (temp))
        strcat(ptr1, temp);

      free(temp);
      return source;
    }
    else {
      fprintf(stderr, "%s: Memory allocation error\n", __func__);
    }
  }
  return NULL;
}

/*! U0618
 * \brief Compare n characters ignoring case
 * \par Function Description
 *  Another garden varity string compare using toupper on both inputs.
 *  This is sometimes found in standard libraries but not always.
 *
 * \param [in] str1  is the string to be search
 * \param [in] str2  is the string to search for
 * \param [in] n     is the number of char to compare
 *
 * \retval 0 if the strings are equivalent, or
 *        -1 if str2 if first mis-match is because str2 is greater, or
 *         1 if the first mis-match is because str1 is greater.
 */
int geda_utility_string_strncmpi(const char *str1, const char *str2, int n)
{
  unsigned int i = 0;
  if (!str1 || !str2) {
    errno = EINVAL;
    return -2;
  }

  while ((toupper(*str1) == toupper(*str2)) && i < n)
  {
    str1++;
    str2++;
    i++;
  }
  if (i == n)
    return 0;
  else
    if ((*str1 == *str2 ) && (!*str1))
      return 0;
    else
      if ((*str1) && (!*str2))
        return -1;
      else
        if ((*str2) && (!*str1))
          return 1;
        else
          return ((*str1 > *str2 ) ? -1 : 1);
}

/*! U0619
 * \brief Duplicate a specified number of characters
 * \par Function Description
 *  The newly allocated NULL terminated string.
 *
 * \returns string containing the first \a n characters of \a str.
 *
 * \remarks returned string must be freed using GEDA_FREE.
 */
char *geda_utility_string_strndup(const char *str, int n)
{
  char *ptr;

  if (n >= 0 && str) {

    ptr = GEDA_MEM_ALLOC(n+1);

    if (ptr != NULL) {
       memcpy(ptr, str, n);
       ptr[n] = '\0';
    }
    else {
      ptr = NULL;
    }
  }
  else {
    ptr = NULL;
  }

  return ptr;
}

/*! U0620
 * \brief Get the formated size of a string
 * \par Function Description
 *  Returns the number of bytes needed to hold the string formed
 *  after substituting variable arguments into the format specifier.
 */
int geda_utility_string_strsize (const char *format, va_list args)
{
  int size;

#if defined(HAVE_VSNPRINTF)

  size = vsnprintf (0, 0, format, args);

#elif (HAVE_VASPRINTF)

  size = vasprintf (0, 0, format, args);

#else

  char *string;

  va_list args2;
  va_copy(args2, args);
  string = (char*)GEDA_MEM_ALLOC(4 * USS_BUFFER_SIZE);
  size = vsprintf(string, format, args2);
  va_end(args2);
  GEDA_FREE(string);

#endif

  return size;
}

/*! U0621
 * \brief strstr_rep for c
 * \par Function Description
 *  Recursively replace substring in string with new string. This
 *  function can be used to reduce multiple occurrence of a pattern
 *  in a string to a single occurrence. For general string substitution
 *  see geda_utility_string_strsubst.
 *
 * \warning if \a new is the same pattern as \a old, new should have
 *          fewer characters than \a old, i.e. do not use to add
 *          characters like strstr_rep(str, " ", "  ")!
 *
 * \param[in]  original ptr to input string.
 * \param[in]  old      ptr to the string to be replaced
 * \param[in]  new      ptr to the replacement string.
 *
 *  example: str = strstr_rep(str, "  ", " ");
 *
 * \sa geda_utility_string_strsubst
 */
char *geda_utility_string_strstr_rep(char *original, const char *old, const char *new)
{
  char *temp;

  void do_replace(char *s_ptr, const char *old, const char *new) {

    unsigned int lenOld = strlen(old);
    unsigned int lenNew = strlen(new);

    while (*s_ptr) {

      char *ptr = strstr(s_ptr, old);

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

  if (!original || !old || !new)
    return(NULL);

  if ((temp = (char*)malloc(sizeof(char)*(strlen(original)+1))) == NULL)
    return(NULL);

  while (strstr(original, old)) {
    char *str = original;
    do_replace(str, old, new);
  }
  free(temp);
  return original;
}

/*! U0621
 * \brief Replace substring in a String
 * \par Function Description
 *  This function replaces the first occurrence of str1 with str2
 *  in the source. This version uses array indexes and dynamically
 *  allocates temporary storage. The Caller is responsible for insuring
 *  source is sufficiently large enough to hold the new string, ie
 *  original - old + new + 1.
 *
 * \param [in] source   is the string to be modified
 * \param [in] old_str  is the string to be replaced
 * \param [in] new_str  is the replacement for old_str
 *
 * \retval char* source (the orginal pointer) or NULL if old_str was not
 *         not found in the source string or if there was a error
 *         allocating memory.
 *
 * \todo less than idea.
 */
char *geda_utility_string_strsubst(char *source, char *old_str, char *new_str)
{
  if (source && old_str && new_str) {

    char *temp;
    int   position;

    unsigned int length;
    unsigned int size;

    length   = strlen (old_str);
    size     = strlen (source)- length + strlen (new_str) + 1;
    temp     = malloc (size);
    position = -1;

    if (temp) { /* If memory was allocated */

      unsigned int i, j, k;

      memset(temp, 0, size); /* initialize new memory */

      /* Getting starting position for replacement */
      for(i = 0; source[i] && ( position == -1 ); ++i) {
        for(j = i,k = 0; source[j] == old_str[k]; j++, k++) {
          if(!old_str[k+1]) position = i;
        }
      }

      /* Start replacing */
      if (position!=-1) {               /* if we found position   */

        for (j = 0; j < position; j++)      /* copy the prefix        */
          temp[j] = source[j];

        for(i = 0; new_str[i]; i++, j++)  /* add the new string and */
          temp[j] = new_str[i];

        for(k = position + length; source[k]; k++, j++) /* remainder of source */
          temp[j] = source[k];

        temp[j] = '\0';                          /* then add terminator  */

        for(i = 0; (source[i] = temp[i]); i++);  /* write back to source */

        free(temp);
        return source;
      }

      free(temp);
    }
    else {
      fprintf(stderr, "%s: Memory allocation error\n", __func__);
    }
  }
  return NULL;
}

/*! U0623
 * \brief Remove Leading and Trailing Spaces and TAB's from a String
 * \par Function Description
 *  Returns the input string with any leading or trailing spaces or
 *  removed.
 */
char *geda_utility_string_strtrim (const char *string)
{
  if (string) {

    unsigned int len;

    len = strlen(string);

    if (len > 0) {

      const char *start = string;
      const char *end   = string + len - 1;

      char *new_str;
      int i;
      /* Advance start to the first character */
      while ((*start == SPACE    ||
              *start == ASCII_HT ||
              *start == ASCII_LF ||
              *start == ASCII_CR ) &&
              *start != ASCII_NUL) { ++start; }

      /* Make end point to the last character */
      while ((*end == SPACE || *end == ASCII_HT) && end > start) { --end; }

      len = end - start + 1;

      /* Add 1 for the terminator */
      new_str = geda_malloc (len + 1);

      /* Copy the string to the new allocation */
      for(i = 0; i < len; i++) {
        new_str[i] = start[i];
      }

      /* Add the terminator */
      new_str[len] = '\0';

      return new_str;
    }
    else {
      return geda_strdup("");
    }
  }
  return NULL;
}

/*! U0624
 * \brief Get Word Count
 * \par Function Description
 *  returns the number of spaces in a string plus one.
 */
int geda_utility_string_word_count(char *str)
{
  if (str) {
    int count = 1;
    while ( *str != ASCII_NUL) {
      if (*str == ASCII_SPACE ) ++count;
      ++str;
    }
    return count;
  }
  return -1;
}

/** @} endgroup Libgeda-String-Utilities */

#undef USS_BUFFER_SIZE
