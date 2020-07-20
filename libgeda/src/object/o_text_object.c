/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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

/*! \file o_text_object.c
 *  \brief functions for the text and fonts
 *
 *  \par The font definitions
 *
 *  Each letter of the font is defined in a single font symbol file. In
 *  the font symbol file, the character width is defined in the second
 *  line. The first line contains the file format version.
 *
 *  All remaining lines are basic graphical lines. They build the
 *  appearance of the character.
 *
 *  \image html o_text_font_overview.png
 *  \image latex o_text_font_overview.pdf "font overview" width=14cm
 *
 *  The height of capital characters in the font files is 26. The size
 *  of small letters is 16. The space below the zero line is used by
 *  characters like <b>g</b>, <b>p</b> or <b>q</b>. The space above 26
 *  is used by diacritic marks like accents, breve, circumflex mostly in
 *  european characters.
 *
 *  \par The text definitions
 *
 *  The text is stored and printed in several different representations.
 *
 *  In the gEDA files the text is just a string. It is stored unmodified
 *  in <b>GedaObject->text->string</b>.
 *
 *  If the string is an attribute with an equal sign as delimiter between
 *  an attribute name and an attribute value, then it is possible to
 *  hide some parts of the text. The still visible part of an attribute
 *  is stored in <b>GedaObject->text->disp_string</b>.
 *
 *  \image html o_text_text_overview.png
 *  \image latex o_text_text_overview.pdf "text overview" width=14cm
 *
 *  To draw the text in gschem, the string is interpreted and converted
 *  to a list of basic graphical objects. The basic line objects are
 *  collected from the font character objects.
 */

#include "../../../config.h"

#include <math.h>          /* sqrt/hypot */

#include <libgeda_priv.h>

/*!
 * \brief Scale factor between legacy gschem font units and postscript points.
 * \par Description
 *  gschem fonts are nominally specified in points, however there is a
 *  difference in how the specified font size corresponds to the metrics of
 *  the font when compared to typical typographic usage.
 *
 *  The following factor was empirically determined to approximately match the
 *  cap-height between the legacy gschem font, and fonts rendered using pango.
 *  TODO Should this be dynamically determined based upon DPI of host
 */
#define GEDA_FONT_FACTOR 1.3

/*!
 * \brief Scale factor font height and line-spacing (for print only)
 * \par Description
 *  Specifies the scale factor between the nominal font size and the inter-
 *  line spacing used to render it when printing.
 */
#define PRINT_LINE_SPACING 1.12

/*!
 * \brief Create a copy of a text object
 * \par Function Description
 *  This function creates a copy of the text object \a o_current.
 *
 * \param [in] o_current  The object that is copied
 *
 * \return a new text object
 */
GedaObject *geda_text_object_copy(const GedaObject *o_current)
{
  if (GEDA_IS_TEXT(o_current)) {

    GedaObject *new_obj;
    GedaText   *text_obj;

    new_obj = geda_text_object_new (o_current->color,
                                    o_current->text->x, o_current->text->y,
                                    o_current->text->alignment,
                                    o_current->text->angle,
                                    o_current->text->size,
                                    geda_object_get_is_visible (o_current) ? VISIBLE : INVISIBLE,
                                    o_current->show_name_value,
                                    o_current->text->string);

    text_obj = GEDA_TEXT(o_current);

    if (text_obj->rendered_text_bounds_func != NULL) {
      geda_text_object_set_rendered_bounds_func (new_obj,
                                       text_obj->rendered_text_bounds_func,
                                       text_obj->rendered_text_bounds_data);
    }
    return new_obj;
  }
  return NULL;
}

/*!
 * \brief Get the text alignment
 * \par Function Description
 *  Returns the test alignment.
 *
 * \param [in] object The text object
 *
 * \return The text alignmemt
 */
int geda_text_object_get_alignment (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_TEXT(object), LOWER_LEFT);

  return object->text->alignment;
}

/*!
 * \brief Get the text angle
 * \par Function Description
 *  Returns the angle of the text.
 *
 * \param [in] object The text object
 * \return The text angle
 */
int geda_text_object_get_angle (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_TEXT(object), 0.0);

  return object->text->angle;
}

/*!
 * \brief Get Point on the bounds of a GedaText object Nearest a Given Point
 * \par Function Description
 *  This function locate a point on the boundary of the Text object given
 *  a point \a x, \a y, that is on or about the vicinity of \a object. If
 *  True is returned, <B>nx</B> and <B>ny</B> are set to a point on the boundary
 *  that is the closest point on the boundary to the point given by
 *  \a x, \a y.
 *
 * \param [in]  object  Pointer to a GedaText object
 * \param [in]  x       Integer x of point near or on the text
 * \param [in]  y       Integer y of point near or on the text
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE is the results are valid, FALSE if \a object was not a
 *          GedaText object or if the bounds is not set on the Text.
 */
bool geda_text_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny)
{
  bool result;

  if (GEDA_IS_TEXT(object)) {

    int left, top, right, bottom;

    if (geda_object_get_bounds(object, &left, &top, &right, &bottom)) {

      GedaObject *tmp = geda_box_new();

      tmp->box->upper_x = left;
      tmp->box->upper_y = top;
      tmp->box->lower_x = right;
      tmp->box->lower_y = bottom;

      result = geda_box_object_get_nearest_point (tmp, x, y, nx, ny);

      g_object_unref(tmp);
    }
    else { /* Could not get text bounds */
      *nx = x;
      *ny = y;
      result = FALSE;
    }
  }
  else { /* was not a GedaText object */
   *nx = x;
   *ny = y;
    result = FALSE;
  }

  return result;
}

/*!
 * \brief get the position of a text object
 * \par Function Description
 *  This function gets the position of the base point of a text object.
 *
 * \param [in]  object  Pointer to a #GedaText object
 * \param [out] x       pointer to the x-position
 * \param [out] y       pointer to the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_text_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_TEXT(object)) {

    if (x)
      *x = object->text->x;

    if (y)
      *y = object->text->y;

    return TRUE;
  }
  return FALSE;
}

/*!
 * \brief Get the text size
 * \par Function Description
 *  Get the test size.
 *
 * \param [in] object The text object
 *
 * \return The text size
 */
int geda_text_object_get_size (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_TEXT(object), DEFAULT_TEXT_SIZE);

  return object->text->size;
}

/*!
 * \brief Return font size of a text object in postscript points.
 * \par Function Description
 *  gEDA fonts are specified in a non-standard unit. This
 *  function applies an appropriate scaling to return the
 *  font size in postscript points.
 *
 * \param [in] object  The text Object whos font size to return
 *
 * \return The font size converted to postscript points.
 */
double geda_text_object_get_size_in_points (const GedaObject *object)
{
  g_return_val_if_fail (object->type == OBJ_TEXT, 0.0);

  return object->text->size * GEDA_FONT_FACTOR;
}

/*!
 * \brief Get the string displayed by a text object
 * \par Function Description
 *  Retrieve the text string from a text object. The returned string
 *  should be treated as constant.
 *
 * \param [in]  object  The text object
 *
 * \return The text object's string, or NULL on failure.
 */
const char *geda_text_object_get_string (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_TEXT(object), NULL);
  return object->text->string;
}

/*!
 * \brief Get the x coordinate of the text insertion point
 * \par Function Description
 *  Returns the x coordinate of the insertion point.
 *
 * \param [in] object The text object
 * \return x coordinate of the insertion point
 */
int geda_text_object_get_x (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_TEXT(object), 0);

  return object->text->x;
}

/*!
 * \brief Get the y coordinate of the text insertion point
 * \par Function Description
 *  Returns the y coordinate of the insertion point.
 *
 * \param [in] object The text object
 * \return y coodinate of the insertion point
 */
int geda_text_object_get_y (const GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_TEXT(object), 0);

  return object->text->y;
}

/*!
 * \brief mirror a text object horizontaly at a centerpoint
 * \par Function Description
 *  This function mirrors a text \a object horizontaly at the
 *  point (\a center_x, \a center_y).
 *
 * \param [in,out] object    The text object
 * \param [in]     center_x  x-coord of the mirror position
 * \param [in]     center_y  y-coord of the mirror position
 */
void geda_text_object_mirror(GedaObject *object, int center_x, int center_y)
{
  int origx, origy;
  int x, y;

  origx = object->text->x;
  origy = object->text->y;

  x = origx + (-center_x);
  y = origy + (-center_y);

  if ((object->text->angle%180)==0) {
    switch(object->text->alignment) {
      case(LOWER_LEFT):
        object->text->alignment=LOWER_RIGHT;
        break;

      case(MIDDLE_LEFT):
        object->text->alignment=MIDDLE_RIGHT;
        break;

      case(UPPER_LEFT):
        object->text->alignment=UPPER_RIGHT;
        break;

      case(LOWER_RIGHT):
        object->text->alignment=LOWER_LEFT;
        break;

      case(MIDDLE_RIGHT):
        object->text->alignment=MIDDLE_LEFT;
        break;

      case(UPPER_RIGHT):
        object->text->alignment=UPPER_LEFT;
        break;

      default:
        break;
    }
  }
  else {
    switch(object->text->alignment) {
      case(LOWER_LEFT):
      object->text->alignment=UPPER_LEFT;
      break;

      case(UPPER_LEFT):
      object->text->alignment=LOWER_LEFT;
      break;

      case(LOWER_RIGHT):
      object->text->alignment=UPPER_RIGHT;
      break;

      case(UPPER_RIGHT):
      object->text->alignment=LOWER_RIGHT;
      break;

      case(LOWER_MIDDLE):
      object->text->alignment=UPPER_MIDDLE;
      break;

      case(UPPER_MIDDLE):
      object->text->alignment=LOWER_MIDDLE;
      break;

      default:
      break;
    }
  }

  object->text->x = -x + (center_x);
  object->text->y =  y + (center_y);

  geda_text_object_recreate(object);
}

/*!
 * \brief Creates a text Object and the graphical objects representing it
 * \par Function Description
 *  Create an GedaObject of type OBJ_TEXT.
 *
 * \param [in]  color                  The color of the text
 * \param [in]  x                      x coord of text
 * \param [in]  y                      y coord of text
 * \param [in]  alignment              How text bounding box aligns on (x, y)
 * \param [in]  angle                  Angle at which text will appear
 * \param [in]  size                   Text size
 * \param [in]  visibility             VISIBLE or INVISIBLE
 * \param [in]  show_name_value        SHOW_NAME_VALUE
 * \param [in]  string                 The text
 * \return Pointer to text Object.
 *
 * \note Caller is responsible for string; this function allocates its own copy.
 */
GedaObject *geda_text_object_new(int color, int x, int y, int alignment, int angle, int size,
                                 int visibility, int show_name_value, const char *string)
{
  GedaObject *new_obj=NULL;
  GedaText   *text;

  if (string == NULL) {
    return(NULL);
  }

  new_obj = geda_text_new();

  text = GEDA_TEXT(new_obj);

  text->string      = geda_strdup (string);
  text->disp_string = NULL; /* We'll fix this up later */
  text->length      = strlen(string);
  text->size        = size;
  text->alignment   = alignment;
  text->x           = x;
  text->y           = y;
  text->angle       = angle;

  new_obj->color    = color;

  geda_set_object_visibility (new_obj, visibility);
  new_obj->show_name_value = show_name_value;

  /* Call directly so no emmision */
  geda_text_object_update_disp_string (new_obj);

  return new_obj;
}

/*! \brief print a text object into a postscript file
 *  \par Function Description
 *  This function writes the postscript representation of the text object
 *  \a o_current into the the file \a fp.
 *  \param [in] toplevel     The GedaToplevel object
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] o_current    The GedaObject to print
 *  \param [in] origin_x     x-coord of the postscript origin
 *  \param [in] origin_y     y-coord of the postscript origin
 *  \param [in] unicode_count Number of items in the unicode table
 *  \param [in] unicode_table Table of unicode items
 */
void geda_text_object_print(GedaToplevel *toplevel, FILE *fp,
                            GedaObject *o_current,
                            int origin_x, int origin_y,
                            int unicode_count,
                            gunichar *unicode_table)
{
  int alignment;
  char *centering_control = NULL;
  char *p,*s;
  char *output_string = NULL;
  char *name = NULL;
  char *value = NULL;
  int x, y, angle, len;
  float font_size;

  if (!o_current->text->string) {
    return;
  }

  f_print_set_color(toplevel, fp, o_current->color);

  if (geda_attrib_object_get_name_value (o_current, &name, &value)) {
    switch(o_current->show_name_value) {
      case(SHOW_NAME_VALUE):
        output_string = geda_strdup(o_current->text->string);
        break;

      case(SHOW_NAME):
        if (name[0] != '\0') {
          output_string = geda_strdup(name);
        }
        else {
          fprintf(stderr,"Got an improper attribute: %s\n",
                  o_current->text->string);
          output_string = geda_strdup("invalid");
        }
        break;

      case(SHOW_VALUE):
        if (value[0] != '\0') {
          output_string = geda_strdup(value);
        } else {
          /* you probably can remove this now... */
          /* since improper attributes will never get here */
          fprintf(stderr, "Got an improper attribute: %s\n",
                  o_current->text->string);
          output_string = geda_strdup("invalid");
        }
        break;

    default:
      BUG_IMSG("unhandled case", o_current->show_name_value);

    }
  }
  else {
    output_string = geda_strdup(o_current->text->string);
  }

  /* Apply alignment map to apply when text is 180 degrees rotated.
   * We want the text on the printer to appear upside right, even
   * though mathematically it aught to be upside down.  To make this
   * work, we will reset the angle to 0, when it's equal to 180
   * degrees, then apply a transformation to the origin location as if
   * the text was rotated about that point.  E.g. if the text origin
   * was at the lower left, and the text was rotated by 180 degrees,
   * it would be as if the origin was at the upper right. The same
   * reasoning has been applied to all 8 other text origins.
   * MIDDLE_MIDDLE maps to itself.
   */
  alignment = o_current->text->alignment;
  angle     = o_current->text->angle;

  if(angle == 180) {

    angle = 0;        /* reset angle to 0 to make text upright */

    switch(alignment) {
      case(LOWER_LEFT):    alignment = UPPER_RIGHT;
      break;

      case(MIDDLE_LEFT):   alignment = MIDDLE_RIGHT;
      break;

      case(UPPER_LEFT):    alignment = LOWER_RIGHT;
      break;

      case(LOWER_MIDDLE):  alignment = UPPER_MIDDLE;
      break;

      case(MIDDLE_MIDDLE): alignment = MIDDLE_MIDDLE;
      break;

      case(UPPER_MIDDLE):  alignment = LOWER_MIDDLE;
      break;

      case(LOWER_RIGHT):   alignment = UPPER_LEFT;
      break;

      case(MIDDLE_RIGHT):  alignment = MIDDLE_LEFT;
      break;

      case(UPPER_RIGHT):   alignment = LOWER_LEFT;
      break;
    }
  }

  /* Create an appropriate control string for the centering. */
  switch(alignment) {
    /* hcenter rjustify vcenter vjustify */
    case(LOWER_LEFT):    centering_control = "false false false false";
    break;

    case(MIDDLE_LEFT):   centering_control = "false false true false";
    break;

    case(UPPER_LEFT):    centering_control = "false false false true";
    break;

    case(LOWER_MIDDLE):  centering_control = "true false false false";
    break;

    case(MIDDLE_MIDDLE): centering_control = "true false true false";
    break;

    case(UPPER_MIDDLE):  centering_control = "true false false true";
    break;

    case(LOWER_RIGHT):   centering_control = "false true false false";
    break;

    case(MIDDLE_RIGHT):  centering_control = "false true true false";
    break;

    case(UPPER_RIGHT):   centering_control = "false true false true";
    break;
  }

  font_size = geda_text_object_get_size_in_points (o_current) / 72.0 * 1000.0;

  fprintf(fp,"%s %f [",centering_control, font_size * PRINT_LINE_SPACING);

  /* split the line at each newline and print them */
  p   = output_string;             /* Current point */
  s   = output_string;             /* Start of the current string */
  len = strlen(output_string) + 1;

  while(len != 0) {
    /* Have we reached the end of a line? */
    if((*p == '\n') || (*p == '\0')) {
      /* Yes, replace the newline with a NULL and output the string */
      *p = '\0';
      geda_text_object_print_text_string(fp,s,unicode_count,unicode_table);
      /* Update output string start for next string */
      s = p + 1; /* One past the current character. */
    }
    p++;   /* Advance to next character */
    len--; /* Keep track of how many characters left to process */
  }

  /* Finish up with the rest of the text print command */
  /* Collect pertinent info about the text location */
  x = o_current->text->x;
  y = o_current->text->y;

  fprintf(fp,"] %d %d %d %f text\n",angle,x,y,font_size);

  GEDA_FREE(output_string);
  GEDA_FREE(name);
  GEDA_FREE(value);
}

/*! \brief write a text string to a postscript file
 *  \par Function Description
 *  This function writes the single \a string into the postscript file \a fp.
 *
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] string       The string to print
 *  \param [in] unicode_count Number of items in the unicode table
 *  \param [in] unicode_table Table of unicode items
 *
 *  \todo investigate whether the TAB character is handled correctly
 */
void geda_text_object_print_text_string(FILE *fp, char *string, int unicode_count,
                                        gunichar *unicode_table)
{
  int j;
  char *aux;
  gunichar current_char, c;

  if (!string)
  {
    return;
  }

  aux = string;

  fprintf(fp, "(");

  while (aux && ((gunichar) (*aux) != 0)) {

    current_char = g_utf8_get_char_validated(aux, -1);

    if (current_char == '(' || current_char == ')' || current_char == '\\') {
      fprintf(fp, "\\");
    }

    c = current_char;

    if (c >= 128) {

      current_char = '?';

      if (unicode_count)  {

        for (j = 0; j < unicode_count; j++) {

          if (c == unicode_table[j]) {
            current_char = j + 128;
            break;
          }
        }
      }
    }


    if (current_char == '\t') {
      /* Output eight spaces instead of the tab character */
      fprintf(fp, "       ");
    }
    else {
      fprintf(fp, "%c", current_char);
    }

    aux = g_utf8_find_next_char(aux, NULL);
  }

  fprintf(fp,") ");
}

/*! \brief read a text object from a char buffer
 *
 *  \par Function Description
 *  This function reads a text object from the textbuffer \a tb and
 *  the text starting with the line \a firstline.
 *  If the line object was read successfully, a new object is
 *  create and appended to the \a object_list.
 *
 *  \param [in] first_line     the first line of the text
 *  \param [in] tb             Text buffer (usually a line of a schematic file)
 *  \param [in] release_ver    The release number gEDA
 *  \param [in] fileformat_ver A integer value of the file format
 *
 *  \param [out] err           A GError obejct
 *
 *  \return The object list, or NULL on error.
 */
GedaObject *geda_text_object_read (const char *first_line, TextBuffer *tb,
                                   unsigned int release_ver,
                                   unsigned int fileformat_ver, GError **err)
{
  GedaObject *new_obj;
  char       *string;
  unsigned    allocated;

  char type;
  int x, y;
  int color;
  int size;
  int visibility;
  int show_name_value;
  int angle;
  int alignment;
  int num_lines = 0;
  int i;

  if (fileformat_ver >= 1) {
    if (sscanf(first_line, "%c %d %d %d %d %d %d %d %d %d\n", &type, &x, &y,
      &color, &size, &visibility, &show_name_value, &angle, &alignment, &num_lines) != 10)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse text object"));
      return NULL;
    }
  }
  else if (release_ver < VERSION_20000220) {
    /* yes, above less than (not less than and equal) is correct. The format */
    /* change occurred in 20000220 */
    if (sscanf(first_line, "%c %d %d %d %d %d %d %d\n", &type, &x, &y,
      &color, &size,
      &visibility, &show_name_value,
      &angle) != 8)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse text object"));
      return NULL;
    }
    alignment = LOWER_LEFT; /* older versions didn't have this */
    num_lines = 1; /* only support a single line */
  }
  else {
    if (sscanf(first_line, "%c %d %d %d %d %d %d %d %d\n", &type, &x, &y,
               &color, &size,
               &visibility, &show_name_value,
               &angle, &alignment) != 9)
    {
      g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse text object"));
      return NULL;
    }
    num_lines = 1; /* only support a single line */
  }

  if (size == 0) {
    const char *msg = _("Found a text string zero size");
    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s\n", msg);
    }
    geda_log_w (_("Setting text to default size\n"));
    size = DEFAULT_TEXT_SIZE;
  }

  switch(alignment) {
    case(LOWER_LEFT):
    case(MIDDLE_LEFT):
    case(UPPER_LEFT):
    case(LOWER_MIDDLE):
    case(MIDDLE_MIDDLE):
    case(UPPER_MIDDLE):
    case(LOWER_RIGHT):
    case(MIDDLE_RIGHT):
    case(UPPER_RIGHT):
      break;

    default:
    {
      const char *msg = _("Found an unsupported text alignment");
      if (geda_object_show_buffer_err(msg, first_line)) {
        geda_log_w("%s: %d.\n", msg, alignment);
      }
      geda_log_v(_("Setting alignment to LOWER_LEFT\n"));
      alignment = LOWER_LEFT;
    }
    break;
  }

  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = DEFAULT_TEXT_COLOR_INDEX;
  }

  allocated = 0;
  string    = NULL;

  for (i = 0; i < num_lines; i++) {

    const char *line;

    line = geda_struct_textbuffer_next_line (tb);

    if (line == NULL) {
      GEDA_FREE(string);
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Unexpected end-of-file after %d lines"), i);
      return NULL;
    }

    size_t len = strlen(line);

    if (!allocated) {
      string = (char*)malloc(len + 1);
      strncpy(string, line, len);
      allocated = ++len;
      string[allocated - 1] = '\0';
    }
    else {

      char *buffer;

      allocated = allocated + len;

      buffer = (char*)realloc(string, allocated);

      if (!buffer)
        break;

      string = buffer;
      strncat(string, line, len);
    }
  }

  string = geda_utility_string_remove_last_nl(string);

  /* convert the character string to UTF-8 if necessary */
  if (!g_utf8_validate (string, -1, NULL)) {

    /* if it is not utf-8, it is ISO_8859-15 */
    char *tmp = g_convert (string, strlen (string),
                           "UTF-8", "ISO_8859-15",
                           NULL, NULL, NULL);
    if (tmp == NULL) {
      fprintf (stderr, "Failed to convert text to UTF-8: %s.\n", string);
    }
    else {
      /* successfully converted string, now use tmp as string */
      GEDA_FREE (string);
      string = tmp;
    }
  }

  new_obj = geda_text_object_new(color, x, y, alignment, angle, size,
                                 visibility, show_name_value, string);
  GEDA_FREE(string);

  return new_obj;
}

/*!
 * \brief recreate the graphics of a text object
 * \par Function Description
 *  This function updates the underlying primary of the text object
 *  \a o_current.
 *
 * \param o_current The text object to update
 */
void geda_text_object_recreate(GedaObject *o_current)
{
  Page *page;

  geda_object_notify_emit_pre_change (o_current);
  geda_text_object_update_disp_string (o_current);

  if (!geda_object_bounds (o_current)) {
    o_current->bounds_valid = FALSE;
  }

  geda_object_notify_emit_change (o_current);

  page = geda_object_get_page (o_current);

  geda_page_set_changed (page, TRUE); /* set CHANGED flag */
}

/*!
 * \brief rotate a text object around a centerpoint
 * \par Function Description
 *  This function rotates a text \a object around the point
 *  (\a center_x, \a center_y).
 *
 * \param [in,out] object    The text object
 * \param [in]     center_x  x-coord of the rotation center
 * \param [in]     center_y  y-coord of the rotation center
 * \param [in]     angle     The angle to rotate the text object
 *
 * \note only steps of 90 degrees are allowed for the \a angle
 */
void geda_text_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  if (GEDA_IS_TEXT(object)) {

    int newx, newy;
    int x, y;

    object->text->angle = ( object->text->angle + angle ) % 360;

    x = object->text->x + (-center_x);
    y = object->text->y + (-center_y);

    geda_math_rotate_point_90(x, y, angle, &newx, &newy);

    x = newx + (center_x);
    y = newy + (center_y);

    geda_text_object_translate(object, x-object->text->x, y-object->text->y);

    geda_text_object_recreate(object);
  }
  else {
    BUG_MSG("GEDA_IS TEXT failed");
  }
}

/*!
 * \brief Create a string representation of the text object
 * \par Function Description
 *  This function takes a text \a object and return a string
 *  according to the file format definition.
 * \note object was validated by geda_object_save_objects
 * \param [in] object  a text Object
 * \return the string representation of the text Object
 */
char *geda_text_object_save(GedaObject *object)
{
  char *buf;
  char *string;

  int   x, y;
  int   size;
  int   num_lines;
  int   visibility;

  g_return_val_if_fail (GEDA_IS_TEXT(object), NULL);

  x = object->text->x;
  y = object->text->y;

  string = object->text->string;
  size   = object->text->size;

  /* string can have multiple lines (separated by \n's) */
  num_lines = geda_object_get_num_text_lines(string);

  /* Don't save invisible == 2 as visible */
  visibility = (object->visibility == VISIBLE) ? VISIBLE : INVISIBLE;

  buf = geda_sprintf ("%c %d %d %d %d %d %d %d %d %d\n%s", object->type,
                           x, y, object->color, size, visibility,
                           object->show_name_value, object->text->angle,
                           object->text->alignment, num_lines, string);

  return(buf);
}

void geda_text_object_scale (GedaObject *object, int scale)
{
  if (GEDA_IS_TEXT(object)) {

    int new_size;

    new_size = object->text->size * scale;

    if (new_size < MINIMUM_TEXT_SIZE) {
      object->text->size = MINIMUM_TEXT_SIZE;
    }
    else {
      object->text->size = new_size;
    }

    object->bounds_valid = FALSE;
  }
  else {
    BUG_MSG("GEDA_IS TEXT failed");
  }
}

/*!
 * \brief Set the text alignment
 * \par Function Description
 *  In case of an invalid text alignment, the property remains unchanged.
 *
 * \param [in,out] object    The text object
 * \param [in]     alignment The text alignmemt
 */
void geda_text_object_set_alignment (GedaObject *object, int alignment)
{
  g_return_if_fail (GEDA_IS_TEXT(object));

  if (object->text->alignment != alignment) {

    g_return_if_fail (alignment >= LOWER_LEFT);
    g_return_if_fail (alignment <= UPPER_RIGHT);

    object->text->alignment = alignment;
    object->bounds_valid = FALSE;
  }
}

/*!
 * \brief Set the text angle
 * \par Function Description
 *  Set the text angle.
 *
 * \param [in,out] object The text object
 * \param [in]     angle  The text angle in degrees.
 */
void geda_text_object_set_angle (GedaObject *object, int angle)
{
  if (GEDA_IS_TEXT(object)) {
    object->text->angle = angle;
    object->bounds_valid = FALSE;
  }
  else {
    BUG_MSG("GEDA_IS TEXT failed");
  }
}

/*!
 * \brief Set the font-renderer-specific bounds function.
 * \par Function Description
 *  Set the function to be used to calculate text bounds for a given
 *  #GedaText Object. This allow a per text object renderer function to
 *  be defined. If the function is not defined the renderer for the
 *  Page will be used instead, if the Page rennderer is defined.
 *
 * \param [in] object    The GedaToplevel object
 * \param [in] func      Function to use.
 * \param [in] user_data User data to be passed to the function.
 */
void
geda_text_object_set_rendered_bounds_func (GedaObject         *object,
                                 RenderedBoundsFunc  func,
                                 void               *user_data)
{
  g_return_if_fail (GEDA_IS_TEXT(object));
  GedaText *text = GEDA_TEXT(object);
  text->rendered_text_bounds_func = func;
  text->rendered_text_bounds_data = user_data;
}

/*!
 * \brief Set the text size
 * \par Function Description
 *  The text size must be greater than or equal to the MINUMUM_TEXT_SIZE.
 *  In the case of an invalid text size, the property remains unchanged.
 *
 * \param [in,out] object The text object
 * \param [in]     size   The text size
 */
void geda_text_object_set_size (GedaObject *object, int size)
{
  if (GEDA_IS_TEXT(object)) {

    if (object->text->size != size) {

      if (size < MINIMUM_TEXT_SIZE) {
        object->text->size = MINIMUM_TEXT_SIZE;
      }
      else {
        object->text->size = size;
      }
      object->bounds_valid = FALSE;
    }
  }
  else {
    BUG_MSG("GEDA_IS TEXT failed");
  }
}

/*!
 * \brief Set the string displayed by a text object.
 * \par Function Description
 *  Updates the text object with a new text string.
 *
 * \param [in] object      The text object.
 * \param [in] new_string  The new value.
 */
void geda_text_object_set_string (GedaObject *object, const char *new_string)
{
  g_return_if_fail (GEDA_IS_TEXT(object));
  g_return_if_fail (new_string != NULL);

  GEDA_FREE (object->text->string);
  object->text->string = geda_strdup (new_string);

  geda_text_object_recreate (object);

}

/*!
 * \brief Set the x coordinate of the text insertion point
 *
 * \param [in,out] object The text object
 * \param [in]     x      New x coordinate of the text insertion point
 */
void geda_text_object_set_x (GedaObject *object, int x)
{
  if (GEDA_IS_TEXT(object)) {
    object->text->x = x;
    object->bounds_valid = FALSE;
  }
  else {
    BUG_MSG("GEDA_IS TEXT failed");
  }
}

/*!
 * \brief Set the y coordinate of the text insertion point
 *
 * \param [in,out] object The text object
 * \param [in]     y      New y coordinate of the text insertion point
 */
void geda_text_object_set_y (GedaObject *object, int y)
{
  if (GEDA_IS_TEXT(object)) {
    object->text->y = y;
    object->bounds_valid = FALSE;
  }
  else {
    BUG_MSG("GEDA_IS TEXT failed");
  }
}

/*!
 * \brief Calculates distance between a given point and the closest
 *        point on the text.
 *
 *  This function will calculate the distance to the text regardless
 *  if the text is visible or not.
 *
 * \param [in] object       A text Object.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point. If the
 *         distance cannot be calculated, this function returns a really
 *         large number (G_MAXDOUBLE).  With an invalid parameter, this
 *         funciton returns G_MAXDOUBLE.
 */
double geda_text_object_shortest_distance (ConstObject *object, int x, int y, int force_solid)
{
  int left, top, right, bottom;
  double dx, dy;

  g_return_val_if_fail (object->text != NULL, G_MAXDOUBLE);

  if (!geda_object_get_bounds(object, &left, &top, &right, &bottom))
    return G_MAXDOUBLE;

  dx = min (x - left, right - x);
  dy = min (y - top, bottom - y);

  dx = min (dx, 0);
  dy = min (dy, 0);

#if HAVE_HYPOT
  return hypot (dx, dy);
#else
  return sqrt ((dx * dx) + (dy * dy));
#endif
}

/*!
 * \brief Compare the text of two GedaText objects
 *  This function will compare the text of \a object1 with the text of
 *  object \a object2 after validating that both objects are GedaText
 *  objects. This function can be used as a GCompareFunc type.
 *
 * \param [in] object1 Text Object 1
 * \param [in] object2 Text Object 2
 *
 * \return result of strcmp if both objects are GedaText objects,
 *         returns G_MAXINT if either object is not a GedaText object.
 */
int geda_text_object_strcmp(const GedaObject *object1, const GedaObject *object2)
{
  if (GEDA_IS_TEXT(object1) && GEDA_IS_TEXT(object2)) {
    return strcmp (object1->text->string, object2->text->string);
  }
  return G_MAXINT;
}

/*!
 * \brief Translate a text object
 * \par Function Description
 *  This function changes the position of a text object \a object.
 *
 * \param [in] object  The text Object to be moved
 * \param [in] dx      The x-distance to move the object
 * \param [in] dy      The y-distance to move the object
 */
void geda_text_object_translate(GedaObject *object, int dx, int dy)
{
  object->text->x = object->text->x + dx;
  object->text->y = object->text->y + dy;

  /* Update bounding box */
  object->bounds_valid = FALSE;
}

/*!
 * \brief update the visible part of a string
 * \par Function Description
 *  If a string is an attribute, then it is possible to hide the name or
 *  the value part of the attribute string. This functions updates the
 *  text->disp_string according to the object->show_name_value settings
 *
 * \param [in] object  The GedaObject to update
 */
void geda_text_object_update_disp_string (GedaObject *object)
{
  char     *name  = NULL;
  char     *value = NULL;
  GedaText *text  = object->text;

  GEDA_FREE (text->disp_string);

  if (geda_attrib_object_get_name_value (object, &name, &value)) {
    switch (object->show_name_value) {
      case (SHOW_NAME_VALUE):
        text->disp_string = geda_strdup (text->string);
        break;

      case (SHOW_NAME):
        if (name[0] != '\0') {
          text->disp_string = geda_strdup (name);
        }
        else {
          g_critical ("Got an improper attribute: %s\n", text->string);
          text->disp_string = geda_strdup ("invalid");
        }
        break;

      case (SHOW_VALUE):
        if (value[0] != '\0') {
          text->disp_string = geda_strdup(value);
        }
        else {
          g_critical ("Got an improper attribute: %s\n", text->string);
          text->disp_string = geda_strdup ("invalid");
        }
        break;
    }
    /* free the strings allocated by geda_attrib_object_get_name_value */
    GEDA_FREE(name);
    GEDA_FREE(value);
  }
  else {
    text->disp_string = geda_strdup (text->string);
  }
}
