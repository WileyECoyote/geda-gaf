/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2012-2014 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2012-2014 gEDA Contributors (see ChangeLog for details)
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

#include <geda_standard.h>
#include <ctype.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

/*! \brief Expand environment variables in string.
 *  \par Function Description
 *  This function returns the passed string with environment variables
 *  expanded.
 *
 *  The invocations of environment variable MUST be in the form
 *  '${variable_name}', '$variable_name' is not valid here. Environment
 *  variable names consists solely of letters, digits and '_'. It is
 *  possible to escape a '$' character in the string by repeating it
 *  twice.
 *
 *  It outputs error messages to console and leaves the malformed and
 *  bad variable names in the returned string.
 *
 *  \param [in] string The string with variables to expand.
 *
 *  \return A newly-allocated string with variables expanded or NULL
 *  if input string was NULL.
 */
char*
u_expand_env_variable (const char *string)
{
  GString *gstring;
  int i;

  if (string == NULL) {
    return NULL;
  }

  gstring = g_string_sized_new (strlen (string));
  i = 0;
  while (TRUE) {
    int start;

    start = i;
    /* look for end of string or possible variable name start */
    while (string[i] != '\0' && string[i] != '$') i++;
    g_string_append_len (gstring, string + start, i - start);
    if (string[i] == '\0') {
      /* end of string, return built string */
      return g_string_free (gstring, FALSE);
    }

    i++;
    switch (string[i]) {
        case ('{'):
          /* look for the end of the variable name */
          start = i;
          while (string[i] != '\0' && string[i] != '}') i++;
          if (string[i] == '\0') {
            /* problem: no closing '}' to variable */
            fprintf (stderr,
                     "Found malformed environment variable in '%s'\n",
                     string);
            g_string_append (gstring, "$");
            g_string_append_len (gstring, string + start, i - start + 1);
          } else {
            int j;

            /* discard "${" */
            start = start + 1;
            /* test characters of variable name */
            for (j = start;
                 j < i && (g_ascii_isalnum (string[j]) || string[j] == '_');
                 j++);
            if (i != j) {
              /* illegal character detected in variable name */
              fprintf (stderr,
                       "Found bad character [%c] in variable name.\n",
                       string[j]);
              g_string_append (gstring, "${");
              g_string_append_len (gstring, string + start, i - start + 1);
            } else {
              /* extract variable name from string and expand it */
              char *variable_name = u_string_strndup (string + start, i - start);
              const char *env = g_getenv (variable_name);
              GEDA_FREE (variable_name);
              g_string_append (gstring, (env == NULL) ? "" : env);
            }
            i++;
          }
          break;

        case ('$'):
          /* an escaped '$' */
          g_string_append_c (gstring, string[i++]);
          break;

        default:
          /* an isolated '$', put it in output */
          g_string_append_c (gstring, '$');
    }
  }

  /* never reached */
  return NULL;
}

void u_print_object(Object *object)
{
  int top    = object->top;
  int left   = object->left;
  int right  = object->right;
  int bottom = object->bottom;

  if (object->type == OBJ_TEXT) {
    fprintf(stderr, "Object Name=<%s>, str=<%s>, visibility=%d, top=%d, left=%d, right=%d, bottom=%d\n",
            object->name, object->text->string, object->visibility, top, left, right, bottom);
    return;
  }

  if (object->type != OBJ_COMPLEX) {
    fprintf(stderr, "Object Name=<%s>, visibility=%d\n", object->name, object->visibility);
    return;
  }

  GList *prim_objs = object->complex->prim_objs;
  GList *attribs   = object->attribs;
  GList *iter;

  int num_prim = g_list_length(prim_objs);
  int num_atts = g_list_length(attribs);

  fprintf(stderr, "Object Name=<%s>", object->name);
  fprintf(stderr, " has %d prim_objs,", num_prim);
  fprintf(stderr, " and %d attached attributes\n", num_atts);

  int i = 1;
  for(iter = prim_objs; iter; NEXT(iter)) {
    Object *o_current = iter->data;
    if (o_current->type == OBJ_TEXT)
      fprintf(stderr, "prim_objs %d) Name=<%s>, str=<%s>, visibility=%d\n",
              i, o_current->name, o_current->text->string, o_current->visibility);
    else
      fprintf(stderr, "prim_objs %d) Name=<%s>, visibility=%d\n", i, o_current->name, o_current->visibility);
    i++;
  }
  fprintf(stderr, "\n");
  i = 1;
  for(iter = attribs; iter; NEXT(iter)) {
    Object *o_current = iter->data;
    fprintf(stderr, "attrib %d) Name=<%s>, str=<%s>, visibility=%d\n",
            i, o_current->name, o_current->text->string, o_current->visibility);
    i++;
  }
  fprintf(stderr, "\n");
}
