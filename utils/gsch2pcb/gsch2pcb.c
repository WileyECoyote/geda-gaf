/* gsch2pcb
 *
 *  Bill Wilson    billw@wt.net
 *
 *  This program is free software which I release under the GNU General Public
 *  License. You may redistribute and/or modify this program under the terms
 *  of that license as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.  Version 2 is in the
 *  COPYRIGHT file in the top level directory of this distribution.
 *
 *  To get a copy of the GNU General Puplic License, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301 USA
 */

#include "../../config.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#include "../../include/gettext.h"

#define WITHOUT_GDK_PIX_BUFFER 1
#define WITHOUT_GUILE 1

#include <geda/geda_stat.h>

#include <libgeda/libgeda.h>

#include <glib.h>

#include <ctype.h>

#define GSC2PCB_VERSION "1.14.0"

#define DEFAULT_PCB_INC    "pcb.inc"

#define PCB_PATH_DELIMETER ":"

#define SEP_STRING         "--------\n"

typedef struct
{
  char *refdes, *value, *description, *changed_description, *changed_value;
  char *flags, *tail;
  char *x, *y;
  char *pkg_name_fix;
  char res_char;

  _Bool still_exists, new_format, hi_res_format, quoted_flags, omit_PKG;
}
PcbElement;

static GList *pcb_element_list,
             *element_directory_list,
             *extra_gnetlist_list,
             *extra_gnetlist_arg_list;

static char  *sch_basename;

static GList *schematics;

static char *m4_pcbdir;
static char *default_m4_pcbdir;
static char *m4_files;
static char *m4_override_file;

static _Bool use_m4 = TRUE;

static char *empty_footprint_name;

static int verbose,
  n_deleted,
  n_added_m4,
  n_added_ef,
  n_fixed,
  n_PKG_removed_new,
  n_PKG_removed_old,
  n_preserved, n_changed_value, n_not_found, n_unknown, n_none, n_empty;

static _Bool remove_unfound_elements = TRUE,
             quiet_mode = FALSE,
             force_element_files,
             preserve, fix_elements, bak_done, need_PKG_purge;


static void create_m4_override_file ()
{
  FILE *f;

  m4_override_file = "gnet-gsch2pcb-tmp.scm";

  f = fopen (m4_override_file, "wb");

  if (!f) {
    m4_override_file = NULL;
    return;
  }

  if (m4_pcbdir) {
    fprintf (f, "(define gsch2pcb:pcb-m4-dir \"%s\")\n", m4_pcbdir);
  }

  if (m4_files) {
    fprintf (f, "(define gsch2pcb:m4-files \"%s\")\n", m4_files);
  }

  fprintf (f, "(define gsch2pcb:use-m4 %s)\n", use_m4 == TRUE ? "#t" : "#f");

  fclose (f);

  if (verbose) {

    printf ("Default m4-pcbdir: %s\n", default_m4_pcbdir);
    printf ("--------\n%s override file:\n", m4_override_file);

    if (m4_pcbdir)
      printf ("    (define gsch2pcb:pcb-m4-dir \"%s\")\n", m4_pcbdir);

    if (m4_files)
      printf ("    (define gsch2pcb:m4-files \"%s\")\n", m4_files);

    printf ("    (define gsch2pcb:use-m4 %s)\n", use_m4 == TRUE ? "#t" : "#f");
  }
}

/**
 * Build and run a command. No redirection or error handling is
 * done.  Format string is split on whitespace. Specifiers %l and %s
 * are replaced with contents of positional args. To be recognized,
 * specifiers must be separated from other arguments in the format by
 * whitespace.
 *  - %l expects a GList, contents used as separate arguments
 *  - %s expects a char*, contents used as a single argument
 * @param[in] format  used to specify command to be executed
 * @param[in] ...     positional parameters
 */
static _Bool
build_and_run_command (const char *format, ...)
{
  va_list vargs;
  char  **split;
  GList  *tmp = NULL;

  int num_split;
  int i;

  _Bool result = FALSE;

  va_start (vargs, format);

  split     = g_strsplit_set (format, " \t\n\v", 0);
  num_split = g_strv_length (split);

  for (i = 0; i < num_split; ++i) {

    char *chunk = split[i];

    if (strcmp (chunk, "%l") == 0) {

      /* append contents of list into command args - shared data */
      tmp = g_list_concat (tmp, g_list_copy (va_arg (vargs, GList*)));
    }
    else if (strcmp (chunk, "%s") == 0) {

      /* insert contents of string into output */
      tmp = g_list_append (tmp, va_arg (vargs, char*));
    }
    else {
      /* bare string, use as is */
      tmp = g_list_append (tmp, chunk);
    }
  }
  va_end (vargs);

  if (tmp) {

    /* we have something in the list, build & call command */

    GError *error           = NULL;
    char   *standard_output = NULL;
    char   *standard_error  = NULL;

    GList *p;
    char **args;
    int    status;

    if (verbose) {
      printf ("%s:\n\t", _("Running command"));
    }

    args = GEDA_MEM_ALLOC0(sizeof(char*) * g_list_length (tmp) + 1);

    i = 0;

    for (p = tmp; p; p = g_list_next (p)) {
      args[i++] = p->data;
      if (verbose) {
        printf ("%s ", (char*)p->data);
      }
    }

    if (verbose) {
      printf ("\n%s", SEP_STRING);
    }

    if (g_spawn_sync (".",                  /* Working directory */
                      args,                /* argv */
                      NULL,                 /* envp */
                      G_SPAWN_SEARCH_PATH,  /* flags */
                      NULL,                 /* child_setup */
                      NULL,                 /* user data */
                      &standard_output,     /* standard output */
                      &standard_error,      /* standard error */
                      &status,              /* exit status return */
                      &error))              /* GError return */
    {
      if (verbose) {
        fputs(standard_output, stdout);
      }

      if (status == 0) {
        result = TRUE;
      }
      else {
        if (standard_error)
          fputs(standard_error, stderr);
      }
    }
    else {
      const char *msg = _("Failed to execute external program");
      fprintf(stderr, "%s: %s\n", msg, error->message);
      g_error_free(error);
    }

    if (verbose) {
      printf ("\n%s", SEP_STRING);
    }

    GEDA_FREE(standard_error);
    GEDA_FREE(standard_output);

    GEDA_FREE (args);

    /* free the list, but leave data untouched */
    g_list_free (tmp);
  }

  g_strfreev (split);

  return result;
}

/* Run gnetlist to generate a netlist and a PCB board file.  gnetlist
 * has exit status of 0 even if it's given an invalid arg, so do some
 * stat() hoops to decide if gnetlist successfully generated the PCB
 * board file (only gnetlist >= 20030901 recognizes -m).
 */
static _Bool
run_gnetlist (char *pins_file, char *net_file, char *pcb_file,
              char *basename, GList *largs)
{
  struct stat st;
  static const char *gnetlist = NULL;
  GList *verboseList = NULL;
 _Bool   result;

  /* Allow the user to specify a full path or a different name for
   * the gnetlist command.  Especially useful if multiple copies
   * are installed at once.
   */
  if (gnetlist == NULL)
    gnetlist = g_getenv ("GNETLIST");

  if (gnetlist == NULL)
    gnetlist = "gnetlist";

  if (!verbose) {
    verboseList = g_list_append (verboseList, "-q");
  }

  result = build_and_run_command ("%s %l -g pcbpins -o %s %l %l",
                                  gnetlist,
                                  verboseList,
                                  pins_file,
                                  extra_gnetlist_arg_list,
                                  largs);
  if (result) {

    result = build_and_run_command ("%s %l -g PCB -o %s %l %l",
                                    gnetlist,
                                    verboseList,
                                    net_file,
                                    extra_gnetlist_arg_list,
                                    largs);
    if (result) {

      GList *args1 = NULL;
      time_t mtime;

      create_m4_override_file ();

      if (m4_override_file) {
        args1 = g_list_append (args1, "-m");
        args1 = g_list_append (args1, m4_override_file);
      }

      mtime = (stat (pcb_file, &st) == 0) ? st.st_mtime : 0;

      result = build_and_run_command ("%s %l -g gsch2pcb -o %s %l %l %l",
                                      gnetlist,
                                      verboseList,
                                      pcb_file,
                                      args1,
                                      extra_gnetlist_arg_list,
                                      largs);

      if (!result) {

        if (stat (pcb_file, &st) != 0 || mtime == st.st_mtime) {

          const char *msg_1 = _("command failed");
          const char *msg_2 = _("not updated");

          fprintf (stderr, "gsch2pcb: gnetlist %s, \"%s\" %s\n", msg_1, pcb_file, msg_2);

          if (m4_override_file) {

            fprintf (stderr,
                     "    At least gnetlist 20030901 is required for m4-xxx options.\n");
          }
        }
      }
      else {

        GList *list = NULL;

        if (m4_override_file) {
          unlink (m4_override_file);
        }

        for (list = extra_gnetlist_list; list; list = g_list_next (list)) {

          const char *s1 = (char*) list->data;
          const char *s2 = strstr (s1, " -o ");
          char *out_file;
          char *backend;

          if (!s2) {
            out_file = geda_strconcat (basename, ".", s1, NULL);
            backend  = geda_utility_string_strdup (s1);
          }
          else {
            out_file = geda_utility_string_strdup (s2 + 4);
            backend  = geda_utility_string_strndup (s1, s2 - s1);
          }

          result = build_and_run_command ("%s %l -g %s -o %s %l %l",
                                          gnetlist,
                                          verboseList,
                                          backend,
                                          out_file,
                                          extra_gnetlist_arg_list,
                                          largs);
          GEDA_FREE (out_file);
          GEDA_FREE (backend);
        }
      }
      g_list_free (args1);
    }
  }

  g_list_free (verboseList);

  return result;
}

static char *token (char *string, char **next, _Bool *quoted_ret)
{
  static char *str;
  char *s, *ret;
  _Bool quoted = FALSE;

  if (string)
    str = string;

  if (!str || !*str) {
    if (next)
      *next = str;
    return geda_utility_string_strdup ("");
  }

  while (*str == ' ' || *str == '\t' || *str == ',' || *str == '\n')
    ++str;

  if (*str == '"') {

    quoted = TRUE;

    if (quoted_ret)
      *quoted_ret = TRUE;

    ++str;

    for (s = str; *s && *s != '"' && *s != '\n'; ++s);
  }
  else {

    if (quoted_ret)
      *quoted_ret = FALSE;

    for (s = str;
         *s && (*s != ' ' && *s != '\t' && *s != ',' && *s != '\n'); ++s);
  }

  ret = geda_utility_string_strndup (str, s - str);
  str = (quoted && *s) ? s + 1 : s;

  if (next)
    *next = str;

  return ret;
}

static char *fix_spaces (char *str)
{
  char *s;

  if (!str)
    return NULL;

  for (s = str; *s; ++s)
    if (*s == ' ' || *s == '\t')
      *s = '_';
  return str;
}

  /* As of 1/9/2004 CVS hi_res Element[] line format:
   *   Element[element_flags, description, pcb-name, value, mark_x, mark_y,
   *       text_x, text_y, text_direction, text_scale, text_flags]
   *   New PCB 1.7 / 1.99 Element() line format:
   *   Element(element_flags, description, pcb-name, value, mark_x, mark_y,
   *       text_x, text_y, text_direction, text_scale, text_flags)
   *   Old PCB 1.6 Element() line format:
   *   Element(element_flags, description, pcb-name, value,
   *       text_x, text_y, text_direction, text_scale, text_flags)
   *
   *   (mark_x, mark_y) is the element position (mark) and (text_x,text_y)
   *   is the description text position which is absolute in pre 1.7 and
   *   is now relative.  The hi_res mark_x,mark_y and text_x,text_y resolutions
   *   are 100x the other formats.
   */
PcbElement *pcb_element_line_parse (char *line)
{
  PcbElement *el;
  char *t;

  if (strncmp (line, "Element", 7) == 0) {

    el = GEDA_MEM_ALLOC0 (sizeof(PcbElement));

    if (el) {

      char *s = line + 7;                    /* Skip the word "Element" */

      while (*s == ' ' || *s == '\t')  /* Skip over spaces and tabs */
        ++s;

      if (*s == '[') {
        el->hi_res_format = TRUE;
      }
      else if (*s != '(') {
        GEDA_FREE (el);
      }

      if (el) {

        char close_char;

        int  state      = 0;
        int  elcount    = 0;

        el->res_char    = el->hi_res_format ? '[' : '(';
        close_char      = el->hi_res_format ? ']' : ')';

        el->flags       = token (s + 1, NULL, &el->quoted_flags);
        el->description = token (NULL, NULL, NULL);
        el->refdes      = token (NULL, NULL, NULL);
        el->value       = token (NULL, NULL, NULL);

        el->x           = token (NULL, NULL, NULL);
        el->y           = token (NULL, &t, NULL);

        el->tail = geda_utility_string_strdup (t ? t : "");

        if ((s = strrchr (el->tail, (int) '\n')) != NULL)
          *s = '\0';

        /* Count the tokens in tail to decide if it's new or old format.
         * Old format will have 3 tokens, new format will have 5 tokens.
         */
        for (s = el->tail; *s && *s != close_char; ++s) {

          if (*s != ' ') {
            if (state == 0)
              ++elcount;
            state = 1;
          }
          else {
            state = 0;
          }
        }

        if (elcount > 4)
          el->new_format = TRUE;

        fix_spaces (el->description);
        fix_spaces (el->refdes);
        fix_spaces (el->value);

        /* Don't allow elements with no refdes to ever be deleted because
         * they may be desired pc board elements not in schematics. So
         * initialize still_exists to TRUE if empty or non-alphanumeric
         * refdes.
         */
        if (!*el->refdes || !isalnum ((int) (*el->refdes))) {
          el->still_exists = TRUE;
        }
      }
    }
  }
  else {
    el = NULL;
  }
  return el;
}

static void
pcb_element_free (PcbElement *el)
{
  if (el) {
    GEDA_FREE (el->flags);
    GEDA_FREE (el->description);
    GEDA_FREE (el->changed_description);
    GEDA_FREE (el->changed_value);
    GEDA_FREE (el->refdes);
    GEDA_FREE (el->value);
    GEDA_FREE (el->x);
    GEDA_FREE (el->y);
    GEDA_FREE (el->tail);
    GEDA_FREE (el->pkg_name_fix);
    GEDA_FREE (el);
  }
}

static void get_pcb_element_list (char *pcb_file)
{
  FILE *f;
  PcbElement *el;

  char *s, buf[1024];

  if ((f = fopen (pcb_file, "r")) == NULL)
    return;

  while ((fgets (buf, sizeof(buf), f)) != NULL) {

    for (s = buf; *s == ' ' || *s == '\t'; ++s);

    if (!strncmp (s, "PKG_", 4)) {
      need_PKG_purge = TRUE;
      continue;
    }

    el = pcb_element_line_parse (s);

    if (el) {
      pcb_element_list = g_list_append (pcb_element_list, el);
    }
  }

  fclose (f);
}

static PcbElement *pcb_element_exists (PcbElement * el_test, _Bool record)
{
  GList *list;

  for (list = pcb_element_list; list; list = g_list_next (list)) {

    PcbElement *el = (PcbElement *) list->data;

    if (strcmp (el_test->refdes, el->refdes))
      continue;
    if (strcmp (el_test->description, el->description)) { /* footprint */
      if (record)
        el->changed_description = geda_utility_string_strdup (el_test->description);
    } else {
      if (record) {
        if (strcmp (el_test->value, el->value))
          el->changed_value = geda_utility_string_strdup (el_test->value);
        el->still_exists = TRUE;
      }
      return el;
    }
  }
  return NULL;
}

/* A problem is that new PCB 1.7 file elements have the
 * (mark_x,mark_y) value set to wherever the element was created and
 * no equivalent of a gschem translate symbol was done.
 *
 * So, file elements inserted can be scattered over a big area and
 * this is bad when loading a file.new.pcb into an existing PC
 * board.  So, do a simple translate if (mark_x,mark_y) is
 * (arbitrarily) over 1000.  I'll assume that for values < 1000 the
 * element creator was concerned with a sane initial element
 * placement.  Unless someone has a better idea?  Don't bother with
 * pre PCB 1.7 formats as that would require parsing the mark().
 * Current m4 elements use the old format but they seem to have a
 * reasonable initial mark().
 */
static void simple_translate (PcbElement * el)
{
  g_free(el->x);
  g_free(el->y);

  el->x=strdup("0");
  el->y=strdup("0");
}

static _Bool
insert_element (FILE *f_out,     char *element_file,
                char *footprint, char *refdes, char *value)
{
  FILE       *f_in;
  PcbElement *el;
  char       *str;
  char        buf[1024];
  _Bool       retval;

  retval = FALSE;

  if ((f_in = fopen (element_file, "r")) == NULL) {
    str = geda_sprintf ("insert_element() can't open %s", element_file);
    perror (str);
    GEDA_FREE (str);
  }
  else {

    /* Scan the file to detect whether it is actually a PCB
     * layout. Assumes that a PCB layout will have a "PCB" line. */
    while ((fgets (buf, sizeof(buf), f_in)) != NULL) {

      for (str = buf; *str == ' ' || *str == '\t'; ++str);

      str[3] = 0;                   /* Truncate line */

      if (strncmp ("PCB", str, 3) == 0) {
        const char *warn = _("WARNING");
        const char *msg  = _("appears to be a PCB layout file. Skipping");
        printf ("%s: %s %s.\n", warn, element_file, msg);
        fclose (f_in);
        return FALSE;
      }
    }
    rewind (f_in);

    /* Copy the file element lines.  Substitute new parameters into the
     * Element() or Element[] line and strip comments.
     */
    while ((fgets (buf, sizeof(buf), f_in)) != NULL) {

      for (str = buf; *str == ' ' || *str == '\t'; ++str);

      if ((el = pcb_element_line_parse (str)) != NULL) {

        char *fmt;

        simple_translate (el);

        fmt = el->quoted_flags ?
               "Element%c\"%s\" \"%s\" \"%s\" \"%s\" %s %s%s\n" :
               "Element%c%s \"%s\" \"%s\" \"%s\" %s %s%s\n";

        fprintf (f_out, fmt,
                 el->res_char, el->flags, footprint, refdes, value,
                 el->x, el->y, el->tail);
        retval = TRUE;
      }
      else if (*str != '#') {
        fputs (buf, f_out);
      }
      pcb_element_free (el);
    }
    fclose (f_in);
  }
  return retval;
}

char *find_element (char *dir_path, char *element)
{
  GDir *dir;
  char *name;
  char *found = NULL;

  if ((dir = g_dir_open (dir_path, 0, NULL)) == NULL) {

    char *str;

    str = geda_sprintf ("find_element can't open dir \"%s\"", dir_path);

    perror (str);
    GEDA_FREE (str);
    return NULL;
  }

  if (verbose > 1) {
    const char *msg = _("Searching");
    printf ("\t  %s: \"%s\" %s \"%s\"\n", msg, dir_path, _("for"), element);
  }

  while ((name = (char *) g_dir_read_name (dir)) != NULL) {

    char *path;

    path  = geda_strconcat (dir_path, "/", name, NULL);
    found = NULL;

    /* if we got a directory name, then recurse down into it */
    if (g_file_test (path, G_FILE_TEST_IS_DIR)) {
      found = find_element (path, element);
    }
    else {

      /* assume it is a file and see if it is the one we want */
      if (verbose > 1) {
        printf ("\t           : %s\t", name);
      }

      if (!strcmp (name, element)) {
        found = geda_utility_string_strdup (path);
      }
      else {

        char *tmps;

        tmps = geda_strconcat (element, ".fp", NULL);

        if (!strcmp (name, tmps)) {
          found = geda_utility_string_strdup (path);
        }
        GEDA_FREE (tmps);
      }

      if (verbose > 1) {
        printf ("%s\n", found ? _("Yes") : _("No"));
      }
    }

    GEDA_FREE (path);

    if (found) {
      break;
    }
  }
  g_dir_close (dir);

  return found;
}

char *search_element_directories (PcbElement *el)
{
  GList *list;
  char  *elname = NULL;
  char  *path   = NULL;

  /* See comment before pkg_to_element() */
  if (el->pkg_name_fix) {

    if (strchr (el->description, '-')) {

      char *str;
      int   n1, n2;

      n1  = strlen (el->description);
      n2  = strlen (el->pkg_name_fix);
      str = el->description + n1 - n2 - 1;

      if (n1 > 0 && n2 < n1 && *str == '-' && *(str + 1) == *el->pkg_name_fix)
      {
        str = geda_utility_string_strndup (el->description, n1 - n2 - 1);
        elname = geda_strconcat (str, " ", el->pkg_name_fix, NULL);
        GEDA_FREE (str);
      }
    }

    if (!elname) {

      const char *msg1 = _("Warning: argument passing may have been confused by");
      const char *msg2 = _("a comma in a component value");
      const char *msg3 = _("Check");
      const char *msg4 = _("Maybe just use a space instead of a comma");

      printf ("%s\n", msg1);
      printf ("         %s:\n", msg2);
      printf ("         %s %s %s %s\n", msg3, el->refdes, el->description, el->value);
      printf ("         %s?\n", msg4);
    }
  }

  if (!elname)
    elname = geda_utility_string_strdup (el->description);

  if (!strcmp (elname, "unknown")) {
    GEDA_FREE (elname);
    return NULL;
  }

  if (verbose > 1) {
    printf ("\t%s: %s\n", _("Searching directories looking for file element"), elname);
  }

  for (list = element_directory_list; list; list = g_list_next (list)) {

    char *dir_path = list->data;

    if (verbose > 1) {
      printf ("\t%s: \"%s\"\n", _("Looking in directory"), dir_path);
    }

    path = find_element (dir_path, elname);

    if (path) {
      if (verbose) {
        printf ("\t%s: %s\n", _("Found"), path);
      }
      break;
    }
  }

  GEDA_FREE (elname);

  return path;
}

/* The gnetlist backend gnet-gsch2pcb.scm generates PKG_ lines:
 *
 *        PKG_footprint(footprint{-fp0-fp1},refdes,value{,fp0,fp1})
 *
 * where fp1 and fp2 (if they exist) are the extra footprint
 * components when specifying footprints like "DIL 14 300".  This is
 * needed for m4 macros.
 *
 * A complication is if the footprint references a file element with
 * spaces embedded in the name.  The gnetlist backend will interpret
 * these as fp0, fp1, ... args and the footprint will in this case
 * incorrectly have '-' inserted where the spaces should be.  So, if
 * there are additional args, reconstruct the portion of the name
 * given by the args with spaces for later use.  Eg. if the footprint
 * is "100 Pin jack", we will have
 *
 *      PKG_100-Pin-jack(100-Pin-jack,refdes,value,Pin,jack)
 *
 *  So put "Pin jack" into pkg_name_fix so if this element is searched
 *  as a file element we can munge the description to what it should
 *  be, eg:
 *
 *      100-Pin-jack -> 100 Pin jack
 */
static PcbElement *pkg_to_element (FILE *f, char *pkg_line)
{
  PcbElement   *el;
  char **args, *s;
  int n, n_extra_args, n_dashes;

  if (strncmp (pkg_line, "PKG_", 4)
      || (s = strchr (pkg_line, (int) '(')) == NULL)
    return NULL;

  args = g_strsplit (s + 1, ",", 12);

  if (!args[0] || !args[1] || !args[2]) {
    fprintf (stderr, "%s: %s\n", _("Bad package line"), pkg_line);
    return NULL;
  }

  fix_spaces (args[0]);
  fix_spaces (args[1]);
  fix_spaces (args[2]);

  el = GEDA_MEM_ALLOC0 (sizeof(PcbElement));
  el->description = geda_utility_string_strdup (args[0]);
  el->refdes = geda_utility_string_strdup (args[1]);
  el->value = geda_utility_string_strdup (args[2]);

  if ((s = strchr (el->value, (int) ')')) != NULL) {
    *s = '\0';
  }

  /* If the component value has a comma, eg "1k, 1%", the gnetlist generated
   * PKG line will be
   *
   *   PKG_XXX(`R0w8',`R100',`1k, 1%'),
   *
   * but after processed by m4, the input to gsch2pcb will be
   *
   *   PKG_XXX(R0w8,R100,1k, 1%).
   *
   * So the quoting info has been lost when processing for file
   * elements.  So here try to detect and fix this.  But I can't
   * handle the situation where the description has a '-' and the
   * value has a comma because gnet-gsch2pcb.scm munges the
   * description with '-' when there are extra args.
   */
  for (n_extra_args = 0; args[3 + n_extra_args] != NULL; ++n_extra_args);

  s = el->description;

  for (n_dashes = 0; (s = strchr (s + 1, '-')) != NULL; ++n_dashes);

  n = 3;

  if (n_extra_args == n_dashes + 1) { /* Assume there was a comma in the value, eg "1K, 1%" */
    s = el->value;
    el->value = geda_strconcat (s, ",", fix_spaces (args[n]), NULL);
    GEDA_FREE (s);
    if ((s = strchr (el->value, (int) ')')) != NULL)
      *s = '\0';
    n = 4;
  }

  if (args[n]) {
    el->pkg_name_fix = geda_utility_string_strdup (args[n]);
    for (n += 1; args[n] != NULL; ++n) {
      s = el->pkg_name_fix;
      el->pkg_name_fix = geda_strconcat (s, " ", args[n], NULL);
      GEDA_FREE (s);
    }
    if ((s = strchr (el->pkg_name_fix, (int) ')')) != NULL)
      *s = '\0';
  }

  g_strfreev (args);

  /* Common Transalable strings */
  const char *warn = _("WARNING");
  const char *msg2 = _("so will not be in the layout");

  if (empty_footprint_name && !strcmp (el->description, empty_footprint_name)) {
    if (verbose) {

      const char *msg1 = _("has the empty footprint attribute");

      printf ("%s: %s \"%s\" %s.\n", el->refdes, msg1, el->description, msg2);
    }
    n_empty += 1;
    el->omit_PKG = TRUE;
  }
  else if (!strcmp (el->description, "none")) {

    const char *msg1 = _("has a footprint attribute");

    fprintf (stderr,  "%s: %s %s \"%s\" %s.\n", warn, el->refdes, msg1, el->description, msg2);
    n_none += 1;
    el->omit_PKG = TRUE;
  }
  else if (!strcmp (el->description, "unknown")) {

    const char *msg1 = _("has no footprint attribute");
    fprintf (stderr, "%s: %s %s %s.\n", warn, el->refdes, msg1, msg2);
    n_unknown += 1;
    el->omit_PKG = TRUE;
  }

  return el;
}

/* Process the newly created pcb file which is the output from
 *     gnetlist -g gsch2pcb ...
 *
 * It will have elements found via the m4 interface and PKG_ lines for
 * elements not found.  Insert pcb file elements for PKG_ lines if
 * file elements can be found.  If there was an existing pcb file,
 * strip out any elements if they are already present so that the new
 * pcb file will only have new elements.
 */
static int add_elements (char *pcb_file)
{
  FILE       *f_in, *f_out;
  PcbElement *el = NULL;
  char       *p, *tmp_file, *s, buf[1024];
  int         total, paren_level = 0;
  _Bool       is_m4, skipping = FALSE;

  if ((f_in = fopen (pcb_file, "r")) == NULL)
    return 0;

  tmp_file = geda_strconcat (pcb_file, ".tmp", NULL);

  if ((f_out = fopen (tmp_file, "wb")) == NULL) {
    fclose (f_in);
    GEDA_FREE (tmp_file);
    return 0;
  }

  while ((fgets (buf, sizeof(buf), f_in)) != NULL) {

    for (s = buf; *s == ' ' || *s == '\t'; ++s);

    if (skipping) {
      if (*s == '(')
        ++paren_level;
      else if (*s == ')' && --paren_level <= 0)
        skipping = FALSE;
      continue;
    }

    is_m4 = FALSE;

    if ((el = pcb_element_line_parse (s)) != NULL) {
      is_m4 = TRUE;
    }
    else { /* get the element */
      el = pkg_to_element (f_out, s);
    }

    if (el && pcb_element_exists (el, TRUE)) {
      skipping = is_m4;
      pcb_element_free (el);
      continue;
    }

    if (!el || el->omit_PKG) {
      if (el) {
         pcb_element_free (el);
      }
      else {
        fputs (buf, f_out);
      }
      continue;
    }

    if (!is_m4 || force_element_files) {

      if (verbose && !is_m4) {
        printf ("%s: need new file element for footprint  %s (value=%s)\n",
                el->refdes, el->description, el->value);
      }

      if (verbose && is_m4 && force_element_files) {
        printf
          ("%s: have m4 element %s, but trying to replace with a file element.\n",
           el->refdes, el->description);
      }

      p = search_element_directories (el);

      if (!p && verbose && is_m4 && force_element_files) {
        printf ("\t%s.\n", _("No file element found"));
      }

      if (p && insert_element (f_out, p,
                               el->description, el->refdes, el->value))
      {
        skipping = is_m4;
        is_m4 = FALSE;
        ++n_added_ef;
        if (verbose) {
        const char *msg = _("added new file element for footprint");
          printf ("%s: %s %s (value=%s)\n",
                  el->refdes, msg, el->description, el->value);
        }
      }
      else if (!is_m4) {

        const char *msg = _("cannot find PCB element for footprint");

        fprintf (stderr, "%s: %s %s (value=%s)\n",
                 el->refdes, msg, el->description, el->value);

        if (remove_unfound_elements && !fix_elements) {

          fprintf (stderr,
                 _("So device %s will not be in the layout.\n"), el->refdes);
          ++n_PKG_removed_new;
        }
        else {
          ++n_not_found;
          fputs (buf, f_out);   /* Copy PKG_ line */
        }
      }
      GEDA_FREE (p);
    }

    if (is_m4) {

      fputs (buf, f_out);
      ++n_added_m4;

      if (verbose) {

        const char *msg = _("added new m4 element for footprint");

        printf ("%s: %s   %s (value=%s)\n", el->refdes, msg, el->description, el->value);
      }
    }

    pcb_element_free (el);

    if (verbose) {
      printf ("----\n");
    }
  }   /* wend read file */

  fclose (f_in);
  fclose (f_out);

  total = n_added_ef + n_added_m4 + n_not_found;

  if (total == 0)
    build_and_run_command ("rm %s", tmp_file);
  else
    build_and_run_command ("mv %s %s", tmp_file, pcb_file);

  GEDA_FREE (tmp_file);

  return total;
}

static void update_element_descriptions (char *pcb_file, char *bak)
{
  FILE       *f_in;
  FILE       *f_out;
  GList      *list;
  PcbElement *el;
  char       *fmt, *tmp, *s;
  char        buf[1024];

  for (list = pcb_element_list; list; list = g_list_next (list)) {
    el = (PcbElement *) list->data;
    if (el->changed_description)
      ++n_fixed;
  }

  if (!pcb_element_list || n_fixed == 0) {
    fprintf (stderr, _("Could not find any elements to fix.\n"));
    return;
  }

  if ((f_in = fopen (pcb_file, "r")) == NULL)
    return;
  tmp = geda_strconcat (pcb_file, ".tmp", NULL);

  if ((f_out = fopen (tmp, "wb")) == NULL) {
    fclose (f_in);
    return;
  }

  while ((fgets (buf, sizeof(buf), f_in)) != NULL) {

    for (s = buf; *s == ' ' || *s == '\t'; ++s) {

      PcbElement *el_exists;

      if ((el = pcb_element_line_parse (s)) != NULL &&
          (el_exists = pcb_element_exists (el, FALSE)) != NULL &&
           el_exists->changed_description)
      {
        const char *msg = _("updating element Description");

        fmt = el->quoted_flags ?
          "Element%c\"%s\" \"%s\" \"%s\" \"%s\" %s %s%s\n" :
          "Element%c%s \"%s\" \"%s\" \"%s\" %s %s%s\n";

        fprintf (f_out, fmt, el->res_char, el->flags, el_exists->changed_description,
                 el->refdes, el->value, el->x, el->y, el->tail);
        printf ("%s: %s: %s -> %s\n",
                 el->refdes, msg, el->description, el_exists->changed_description);
        el_exists->still_exists = TRUE;
      }
      else {
        fputs (buf, f_out);
      }
    }
    pcb_element_free (el);
  }

  fclose (f_in);
  fclose (f_out);

  if (!bak_done) {
    build_and_run_command ("mv %s %s", pcb_file, bak);
    bak_done = TRUE;
  }

  build_and_run_command ("mv %s %s", tmp, pcb_file);
  GEDA_FREE (tmp);
}

static void prune_elements (char *pcb_file, char *bak)
{
  FILE       *f_in, *f_out;
  GList      *list;
  PcbElement *el, *el_exists;
  char       *fmt, *tmp, *s, buf[1024];
  int         paren_level = 0;
  _Bool       skipping = FALSE;

  for (list = pcb_element_list; list; list = g_list_next (list)) {
    el = (PcbElement *) list->data;
    if (!el->still_exists) {
      if (preserve) {

        const char *msg1 = _("Preserving PCB element not in the schematic");
        const char *msg2 = _("element");

        ++n_preserved;

        fprintf (stderr, "%s:    %s (%s   %s)\n", msg1, el->refdes, msg2, el->description);
      }
      else
        ++n_deleted;
    }
    else if (el->changed_value)
      ++n_changed_value;
  }

  if (!pcb_element_list ||
      (n_deleted == 0 && !need_PKG_purge && n_changed_value == 0))
    return;

  if ((f_in = fopen (pcb_file, "r")) == NULL)
    return;

  tmp = geda_strconcat (pcb_file, ".tmp", NULL);

  if ((f_out = fopen (tmp, "wb")) == NULL) {
    fclose (f_in);
    return;
  }

  while ((fgets (buf, sizeof(buf), f_in)) != NULL) {

    for (s = buf; *s == ' ' || *s == '\t'; ++s);

    if (skipping) {
      if (*s == '(')
        ++paren_level;
      else if (*s == ')' && --paren_level <= 0)
        skipping = FALSE;
      continue;
    }

    el_exists = NULL;

    if ((el = pcb_element_line_parse (s)) != NULL
        && (el_exists = pcb_element_exists (el, FALSE)) != NULL
        && !el_exists->still_exists && !preserve) {
      skipping = TRUE;
      if (verbose) {
        const char *msg = _("deleted element");
        printf ("%s: %s %s (value=%s)\n", el->refdes, msg, el->description, el->value);
      }
      pcb_element_free (el);
      continue;
    }

    if (el_exists && el_exists->changed_value) {
      fmt = el->quoted_flags ?
                            "Element%c\"%s\" \"%s\" \"%s\" \"%s\" %s %s%s\n" :
                            "Element%c%s \"%s\" \"%s\" \"%s\" %s %s%s\n";
      fprintf (f_out, fmt,
               el->res_char, el->flags, el->description, el->refdes,
               el_exists->changed_value, el->x, el->y, el->tail);
      if (verbose) {
        const char *msg1 = _("changed element");
        const char *msg2 = _("value");
        printf ("%s: %s %s %s: %s -> %s\n", el->refdes, msg1, el->description, msg2,
                                            el->value, el_exists->changed_value);
      }
    }
    else if (!strncmp (s, "PKG_", 4)) {
      ++n_PKG_removed_old;
    }
    else {
      fputs (buf, f_out);
    }
    pcb_element_free (el);
  }

  fclose (f_in);
  fclose (f_out);

  if (!bak_done) {
    build_and_run_command ("mv %s %s", pcb_file, bak);
    bak_done = TRUE;
  }

  build_and_run_command ("mv %s %s", tmp, pcb_file);
  GEDA_FREE (tmp);
}

static void add_m4_file (char *arg)
{
  if (!m4_files)
    m4_files = geda_utility_string_strdup (arg);
  else {
    char *s = m4_files;
    m4_files = geda_strconcat (m4_files, " ", arg, NULL);
    GEDA_FREE (s);
  }
}

static inline const char *get_home_dir(void) {

  char *home_dir;

#ifdef OS_LINUX

  home_dir = getenv ("HOME");

#else

  home_dir = (char*) g_get_home_dir ();

#endif

  return home_dir;
}

static char *expand_dir (char *dir)
{
  char *path;

  if (dir == NULL) {
    path = NULL;
  }
  else if (*dir == '~') {

    path =  geda_strconcat(get_home_dir(), DIR_SEPARATOR_S, dir + 1, NULL);
  }
  else {
    path = geda_utility_string_strdup (dir);
  }

  return path;
}

static void add_default_m4_files (void)
{
  char *path;

  path = geda_strconcat(get_home_dir(), DIR_SEPARATOR_S,
                       ".pcb", DIR_SEPARATOR_S, DEFAULT_PCB_INC, NULL);

  if (g_file_test (path, G_FILE_TEST_IS_REGULAR))
    add_m4_file (path);

  GEDA_FREE (path);

  if (g_file_test (DEFAULT_PCB_INC, G_FILE_TEST_IS_REGULAR))
    add_m4_file (DEFAULT_PCB_INC);

}

static void add_schematic (char *sch)
{
  const char *s;

  schematics = g_list_append (schematics, geda_utility_string_strdup (sch));

  if (!sch_basename && (s = strstr (sch, ".sch")) != NULL && strlen(s) == 4)
  {
    sch_basename = geda_utility_string_strndup (sch, s - sch);
  }
}

static void add_multiple_schematics (char *sch)
{
  /* parse the string using shell semantics */
  int      count;
  char   **args = NULL;
  GError  *error = NULL;

  if (g_shell_parse_argv (sch, &count, &args, &error)) {

    int i;

    for (i = 0; i < count; ++i) {

      schematics = g_list_append (schematics, geda_utility_string_strdup (args[i]));

    }
    g_strfreev (args);
  }
  else {
    const char *msg = _("invalid schematics option");
    fprintf (stderr, "%s: %s\n", msg, error->message);
    g_error_free (error);
  }
}

static int parse_config (char *config, char *arg)
{
  int   result;

  /* remove trailing white space otherwise strange things can happen */
  if ((arg != NULL) && (strlen (arg) >= 1)) {

    char *s = arg + strlen (arg) - 1;

    while ((*s == ' ' || *s == '\t') && (s != arg))
      s--;
    s++;
    *s = '\0';
  }

  if (verbose) {
    printf ("    %s \"%s\"\n", config, arg ? arg : "");
  }

  if (!strcmp (config, "remove-unfound") || !strcmp (config, "r")) {

    /* This is default behavior set in header section */
    remove_unfound_elements = TRUE;
    return 0;
  }

  if (!strcmp (config, "keep-unfound") || !strcmp (config, "k")) {
    remove_unfound_elements = FALSE;
    return 0;
  }

  if (!strcmp (config, "quiet") || !strcmp (config, "q")) {
    quiet_mode = TRUE;
    return 0;
  }

  if (!strcmp (config, "preserve") || !strcmp (config, "p")) {
    preserve = TRUE;
    return 0;
  }

  if (!strcmp (config, "use-files") || !strcmp (config, "f")) {
    force_element_files = TRUE;
    return 0;
  }

  if (!strcmp (config, "skip-m4") || !strcmp (config, "s")) {
    use_m4 = FALSE;
    return 0;
  }

  result = 1;

  if (!strcmp (config, "elements-dir") || !strcmp (config, "d")) {

    char *elements_dir = expand_dir (arg);

    if (verbose > 1)  {
      const char *msg = _("Adding directory to file element directory list");
      printf ("\t%s: %s\n", msg, elements_dir);
    }
    element_directory_list =
      g_list_prepend (element_directory_list, elements_dir);
  }
  else if (!strcmp (config, "schematics")) {
      add_multiple_schematics (arg);
  }
  else if (!strcmp (config, "m4-pcbdir")) {
        GEDA_FREE (m4_pcbdir);
        m4_pcbdir = geda_utility_string_strdup (arg);
  }
  else if (!strcmp (config, "m4-file")) {
        add_m4_file (arg);
  }
  else if (!strcmp (config, "gnetlist")) {
          extra_gnetlist_list = g_list_append (extra_gnetlist_list, geda_strdup(arg));
  }
  else if (!strcmp (config, "empty-footprint")) {
            empty_footprint_name = geda_utility_string_strdup (arg);
  }
  else if (!strcmp (config, "output-name") || !strcmp (config, "o")) {
    if (sch_basename) {
      fprintf (stderr, "Warning multiple \"output-name\" options\n");
      GEDA_FREE(sch_basename);
    }
    sch_basename = geda_utility_string_strdup (arg);

  }
  else {  /* else was an unknown option*/
    result = -1;
  }

  return result;
}

/*! \brief gsch2pcb load a project file
 *  \par Function Description
 *   Attempts to load project file \a filename.
 */
static void load_project (char *filename)
{
  FILE *f;
  char *s, buf[1024], config[32], arg[768];

  f = fopen (filename, "r");

  if (!f)
    return;

  if (verbose) {
    printf ("%s: %s\n", _("Reading project file"), filename);
  }

  while (fgets (buf, sizeof(buf), f)) {

    for (s = buf; *s == ' ' || *s == '\t' || *s == '\n'; ++s);

    if (!*s || *s == '#' || *s == '/' || *s == ';')
      continue;

    arg[0] = '\0';

    sscanf (s, "%31s %767[^\n]", config, arg);

    parse_config (config, arg);
  }

  fclose (f);
}

/*! \brief gsch2pcb load extra project files
 *  \par Function Description
 *  This function calls load_project in an attempt to load global
 *  "gsch2pcb" files.
 */
static void load_extra_project_files (void)
{
  const char *config_dir;
  const char *home_dir;
  char       *path;
  static _Bool  done = FALSE;

  if (done)
    return;

  load_project ("/etc/gsch2pcb");
  load_project ("/usr/local/etc/gsch2pcb");

  home_dir = get_home_dir();

  path =  geda_strconcat(home_dir, DIR_SEPARATOR_S, "/etc/gsch2pcb", NULL);

  load_project (path);
  GEDA_FREE (path);

  config_dir = geda_user_config_path();

  path = geda_strconcat(config_dir, DIR_SEPARATOR_S, "gsch2pcb", NULL);

  load_project (path);
  GEDA_FREE (path);

  done = TRUE;
}

static char *usage_string0 =
  "usage: gsch2pcb [options] {project | foo.sch [foo1.sch ...]}\n"
  "\n"
  "   Generate a PCB layout file from a set of geda schematics.\n"
  "   gnetlist -g PCB is run to generate foo.net from the schematics.\n"
  "\n"
  "   gnetlist -g gsch2pcb is run to get PCB m4 derived elements which\n"
  "   match schematic footprints.  For schematic footprints which don't match\n"
  "   any PCB m4 layout elements, search a set of file element directories in\n"
  "   an attempt to find matching PCB file elements.\n"
  "   Output to foo.pcb if it doesn't exist.  If there is a current foo.pcb,\n"
  "   output only new elements to foo.new.pcb.\n"
  "   If any elements with a non-empty element name in the current foo.pcb\n"
  "   have no matching schematic component, then remove those elements from\n"
  "   foo.pcb and rename foo.pcb to a foo.pcb.bak sequence.\n"
  "\n"
  "   gnetlist -g pcbpins is run to get a PCB actions file which will rename all\n"
  "   of the pins in a .pcb file to match pin names from the schematic.\n"
  "\n"
  "   \"project\" is a file (not ending in .sch) containing a list of\n"
  "   schematics to process and some options.  A schematics line is like:\n"
  "       schematics foo1.sch foo2.sch ...\n"
  "   Options in a project file are like command line args without the \"-\":\n"
  "       output-name myproject\n"
  "\n"
  "options (may be included in a project file):\n"
  "   -d, --elements-dir D  Search D for PCB file elements.  These defaults\n"
  "                         are searched if they exist: ./packages,\n"
  "                         /usr/local/share/pcb/newlib, /usr/share/pcb/newlib,\n"
  "                         (old pcb) /usr/local/lib/pcb_lib, /usr/lib/pcb_lib,\n"
  "                         (old pcb) /usr/local/pcb_lib\n"
  "   -o, --output-name N   Use output file names N.net, N.pcb, and N.new.pcb\n"
  "                         instead of foo.net, ... where foo is the basename\n"
  "                         of the first command line .sch file.\n"
  "   -f, --use-files       Force using file elements over m4 PCB elements\n"
  "                         for new footprints even though m4 elements are\n"
  "                         searched for first and may have been found.\n"
  "   -r, --remove-unfound  Don't include references to unfound elements in\n"
  "                         the generated .pcb files.  Use if you want PCB to\n"
  "                         be able to load the (incomplete) .pcb file.\n"
  "                         This is the default behavior.\n"
  "   -k, --keep-unfound    Keep include references to unfound elements in\n"
  "                         the generated .pcb files.  Use if you want to hand\n"
  "                         edit or otherwise preprocess the generated .pcb file\n"
  "                         before running pcb.\n"
  "   -p, --preserve        Preserve elements in PCB files which are not found\n"
  "                         in the schematics.  Note that elements with an empty\n"
  "                         element name (schematic refdes) are never deleted,\n"
  "                         so you really shouldn't need this option.\n"
  "   -q, --quiet           Don't tell the user what to do next after running gsch2pcb.\n"
  "\n"
  "   -s, --skip-m4         Skip m4 when looking for footprints.  The default is to use\n"
  "                         m4 (which is what previous versions did).\n"
  "       --m4-file F.inc   Use m4 file F.inc in addition to the default m4\n"
  "                         files ./pcb.inc and ~/.pcb/pcb.inc.\n"
  "       --m4-pcbdir D     Use D as the PCB m4 files install directory\n"
  "                         instead of the default:\n";

static char *usage_string1 =
  "   --gnetlist backend    A convenience run of extra gnetlist -g commands.\n"
  "                         Example:  gnetlist partslist3\n"
  "                         Creates:  myproject.partslist3\n"
  " --empty-footprint name  See the project.sample file.\n"
  "\n"
  "options (not recognized in a project file):\n"
  "   --gnetlist-arg arg    Allows additional arguments to be passed to gnetlist.\n"
  "       --fix-elements    If a schematic component footprint is not equal\n"
  "                         to its PCB element Description, update the\n"
  "                         Description instead of replacing the element.\n"
  "                         Do this the first time gsch2pcb is used with\n"
  "                         PCB files originally created with gschem2pcb.\n"
  "   -v, --verbose         Use -v -v for additional file element debugging.\n"
  "   -V, --version\n\n"
  "environment variables:\n"
  "   GNETLIST              If set, this specifies the name of the gnetlist program\n"
  "                         to execute.\n"
  "\n"
  "Additional Resources:\n"
  "\n"
  "  gnetlist user guide:  http://wiki.geda-project.org/geda:gnetlist_ug\n"
  "  gEDA homepage:        http://www.geda-project.org\n"
  "  PCB homepage:         http://pcb.geda-project.org\n"  "\n";

static void usage ()
{
  puts (usage_string0);
  printf ("                         %s\n\n", default_m4_pcbdir);
  puts (usage_string1);
  exit (0);
}

static void get_args (int argc, char **argv)
{
  int i, r;

  for (i = 1; i < argc; ++i) {

    char *opt, *arg;

    opt = argv[i];
    arg = argv[i + 1];

    if (*opt == '-') {

      ++opt;

      if (*opt == '-') {
        ++opt;
      }

      if (!strcmp (opt, "version") || !strcmp (opt, "V")) {
        if (!quiet_mode) {
          printf ("gsch2pcb %s\n", GSC2PCB_VERSION);
        }
        else {
          printf (GSC2PCB_VERSION "\n");
        }
        exit (0);
      }
      else if (!strcmp (opt, "verbose") || !strcmp (opt, "v")) {
        verbose += 1;
        continue;
      }
      else if (!strcmp (opt, "fix-elements")) {
        fix_elements = TRUE;
        continue;
      }
      else if (!strcmp (opt, "gnetlist-arg")) {
        extra_gnetlist_arg_list =
          g_list_append (extra_gnetlist_arg_list, geda_utility_string_strdup (arg));
        i++;
        continue;
      }
      else if (!strcmp (opt, "help") || !strcmp (opt, "h")) {
        usage ();
      }
      else if (i < argc &&
              ((r = parse_config (opt, (i < argc - 1) ? arg : NULL)) >= 0))
        {
        i += r;
        continue;
      }
      printf ("gsch2pcb: %s %s\n", _("bad or incomplete arg:"), argv[i]);
      usage ();
    }
    else if (!g_str_has_suffix (argv[i], ".sch")) {
        load_extra_project_files ();
        load_project (argv[i]);
    }
    else {
        add_schematic (argv[i]);
    }
  }
}

/*! \brief gsch2pcb main executable entrance point.
 *  \par Function Description
 *  This is the main function for gsch2pcb.
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
int main (int argc, char **argv)
{
  char *pcb_file_name,  *pcb_new_file_name, *bak_file_name,
       *pins_file_name, *net_file_name, *tmp;
  char *path, *p;
  int   i, exit_code;
  _Bool initial_pcb = TRUE;
  _Bool created_pcb_file = TRUE;

  const char *pcbdata_path;

  sch_basename = NULL;
  schematics   = NULL;

#if ENABLE_NLS

  setlocale(LC_ALL, "");
  setlocale(LC_NUMERIC, "C");
  bindtextdomain("geda-utils", LOCALEDIR);
  textdomain("geda-utils");
  bind_textdomain_codeset("geda-utils", "UTF-8");

#endif

  pcbdata_path = getenv ("PCBDATA");  /* do not free return value */

  if (pcbdata_path != NULL) {

    /* If PCBDATA is set, use the value */
    m4_pcbdir = geda_strconcat (pcbdata_path, "/m4", NULL);

  }
  else {

    /* else try PCBDATADIR */
    m4_pcbdir = geda_strconcat (PCBDATADIR, "/m4", NULL);
  }

  default_m4_pcbdir = geda_utility_string_strdup (m4_pcbdir);

  if (argc < 2)
    usage ();

  get_args (argc, argv);

  load_extra_project_files ();
  add_default_m4_files ();

  if (!schematics)
    usage ();

  /* Defaults for the search path if not configured in the project file */
  if (g_file_test ("packages", G_FILE_TEST_IS_DIR))
    element_directory_list = g_list_append (element_directory_list, "packages");

  if (verbose) {
    printf ("%s \"%s\"\n", _("Processing PCBLIBPATH="), PCBLIBPATH);
  }

  path = geda_utility_string_strdup (PCBLIBPATH);

  for (p = strtok (path, PCB_PATH_DELIMETER); p && *p; p = strtok (NULL, PCB_PATH_DELIMETER))
  {
    if (g_file_test (p, G_FILE_TEST_IS_DIR)) {
      if (verbose) {
        printf (_("Adding %s to the newlib search path\n"), p);
      }
      element_directory_list = g_list_append (element_directory_list,
                                              geda_utility_string_strdup (p));
    }
  }
  GEDA_FREE (path);

  pins_file_name = geda_strconcat (sch_basename, ".cmd", NULL);
  net_file_name  = geda_strconcat (sch_basename, ".net", NULL);
  pcb_file_name  = geda_strconcat (sch_basename, ".pcb", NULL);
  bak_file_name  = geda_strconcat (sch_basename, ".pcb.bak", NULL);

  tmp = geda_utility_string_strdup (bak_file_name);

  for (i = 0; g_file_test (bak_file_name, G_FILE_TEST_EXISTS); ++i) {
    GEDA_FREE (bak_file_name);
    bak_file_name = geda_sprintf ("%s%d", tmp, i);
  }
  GEDA_FREE (tmp);

  if (g_file_test (pcb_file_name, G_FILE_TEST_EXISTS)) {
    initial_pcb = FALSE;
    pcb_new_file_name = geda_strconcat (sch_basename, ".new.pcb", NULL);
    get_pcb_element_list (pcb_file_name);
  }
  else {
    pcb_new_file_name = geda_utility_string_strdup (pcb_file_name);
  }

  if (!run_gnetlist (pins_file_name, net_file_name, pcb_new_file_name,
       sch_basename, schematics))
  {
    fprintf(stderr, _("Failed to run gnetlist\n"));
    exit_code = 1;
  }
  else {

    exit_code = 0;

    if (add_elements (pcb_new_file_name) == 0) {
      build_and_run_command ("rm %s", pcb_new_file_name);
      if (initial_pcb) {
        printf (_("No elements found, so nothing to do.\n"));
      }
    }
    else {

      if (fix_elements)
        update_element_descriptions (pcb_file_name, bak_file_name);

      prune_elements (pcb_file_name, bak_file_name);

      /* Report work done during processing */
      if (verbose) {
        printf ("\n");
      }

      printf ("\n----------------------------------\n");
      printf (_("Done processing. Work performed:\n"));

      if (n_deleted > 0 || n_fixed > 0 || need_PKG_purge || n_changed_value > 0)
      {
        const char *msg = _("is backed up as");
        printf ("%s %s %s.\n", pcb_file_name, msg, bak_file_name);
      }

      if (pcb_element_list && n_deleted > 0) {
        const char *msg = _("elements deleted from");
        printf ("%d %s %s.\n", n_deleted, msg, pcb_file_name);
      }

      if (n_added_ef + n_added_m4 > 0) {
        const char *msg1 = _("file elements and");
        const char *msg2 = _("elements added to");
        printf ("%d %s %d m4 %s %s.\n",
                n_added_ef, msg1, n_added_m4, msg2, pcb_new_file_name);
      }
      else if (n_not_found == 0) {
        const char *msg = _("No elements to add, not creating");
        printf ("%s %s\n", msg, pcb_new_file_name);
        created_pcb_file = FALSE;
      }

      if (n_not_found > 0) {
        const char *msg = _("not found elements added to");
        printf ("%d %s %s.\n",
        n_not_found, msg, pcb_new_file_name);
      }

      if (n_unknown > 0) {
        const char *msg = _("components had no footprint attribute and are omitted");
        printf ("%d %s.\n", n_unknown, msg);
      }

      if (n_none > 0) {
        const char *msg = _("components with footprint \"none\" omitted from");
        printf ("%d %s %s.\n", n_none, msg, pcb_new_file_name);
      }

      if (n_empty > 0){
        const char *msg1 = _("components with empty footprint");
        const char *msg2 = _("omitted from");
        printf ("%d %s \"%s\" %s %s.\n",
        n_empty, msg1, empty_footprint_name, msg2, pcb_new_file_name);
      }

      if (n_changed_value > 0) {
        const char *msg = _("elements had a value change in");
        printf ("%d %s %s.\n", n_changed_value, msg, pcb_file_name);
      }

      if (n_fixed > 0) {
        const char *msg = _("elements fixed in");
        printf ("%d %s %s.\n", n_fixed, msg, pcb_file_name);
      }

      if (n_PKG_removed_old > 0) {
        const char *msg1 = _("elements could not be found");
        printf ("%d %s.", n_PKG_removed_old, msg1);
        if (created_pcb_file) {
          const char *msg2 = _("is incomplete");
          printf ("     %s %s.\n", pcb_file_name, msg2);
        }
        else {
          printf ("\n");
        }
      }

      if (n_PKG_removed_new > 0) {
        const char *msg1 = _("elements could not be found");
        printf ("%d %s.", n_PKG_removed_new, msg1);
        if (created_pcb_file) {
          const char *msg2 = _("is incomplete");
          printf ("     %s %s.\n", pcb_new_file_name, msg2);
        }
        else {
          printf ("\n");
        }
      }

      if (n_preserved > 0) {
        const char *msg = _("elements not in the schematic preserved in");
        printf ("%d %s %s.\n", n_preserved, msg, pcb_file_name);
      }

      /* Tell user what to do next */
      if (verbose) {
        printf ("\n");
      }

      if (n_added_ef + n_added_m4 > 0) {
          printf ("\n%s:\n",      _("Next step"));
          printf ("1.  %s %s.\n", _("Run pcb on your file"), pcb_file_name);
        if (initial_pcb) {
          printf ("    %s\n",     _("You will find all your footprints in a bundle ready for you to place"));
          printf ("    %s.\n\n",  _("or disperse with \"Select -> Disperse all elements\" in PCB"));
          printf ("2.  %s \n",    _("from within PCB, select \"File -> Load netlist file\" and select"));
          printf ("    %s %s.\n\n", net_file_name, _("to load the netlist"));
          printf ("3.  %s\n\n",   _("From within PCB, enter"));
          printf ("           :ExecuteFile(%s)\n\n", pins_file_name);
          printf ("    %s.\n\n",  _("to propagate the pin names of all footprints to the layout"));
        }
        else if (quiet_mode == FALSE) {
          printf ("2.  %s\n",     _("From within PCB, select \"File -> Load layout data to paste buffer\""));
          printf ("    %s %s ",   _("    and select"), pcb_new_file_name);
          printf (                _("to load the new footprints into your existing layout.\n"));
          printf ("3.  %s\n",     _("From within PCB, select \"File -> Load netlist file\" and select"));
          printf ("    %s %s.\n\n", net_file_name, _("to load the updated netlist"));

          printf ("4.  %s\n\n",   _("From within PCB, enter"));
          printf ("           :ExecuteFile(%s)\n\n", pins_file_name);
          printf ("    %s.\n\n",  _("to update the pin names of all footprints"));
        }
      }
    }
  }

  GEDA_FREE (default_m4_pcbdir);
  GEDA_FREE (pcb_new_file_name);
  GEDA_FREE (net_file_name);
  GEDA_FREE (pins_file_name);
  GEDA_FREE (pcb_file_name);
  GEDA_FREE (bak_file_name);

  GEDA_FREE (sch_basename);

  if (schematics) {
    geda_utility_glist_free_full(schematics, g_free);
  }

  if (pcb_element_list) {

    if (g_list_length(pcb_element_list)) {

      /* This only occurs when running gsch2pcb with an existing
       * PCB project and elements were added or replaced */

      GList *list;

      /* Free all allocated PcbElement structures in list */
      for (list = pcb_element_list; list; list = g_list_next (list)) {
        pcb_element_free(list->data);
      }
    }

    g_list_free(pcb_element_list);
  }

  geda_free_path();

  return exit_code;
}

#undef PCB_PATH_DELIMETER
