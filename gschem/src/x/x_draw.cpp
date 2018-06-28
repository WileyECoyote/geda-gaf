/* -*- C++ o_draw.cpp indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: o_draw.cpp
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: Nov, 27, 2014
 * Contributing Author: Wiley Edward Hill
 *
*/
/*!
 * \file o_draw.cpp
 * \brief Interface for Drawing routines in Libgedadraw.
 *
 * \remarks This module is under development
 *
 */

#define WITHOUT_GUILE 1

#include <string>

#include "../../include/gschem.h"

#include <geda_draw.h>
#include <geda_debug.h>

//#include <valgrind/callgrind.h>

static EdaX11Render *RenderAdaptor;

static void
x_draw_set_object_color(GschemToplevel *w_current)
{
  GdkColor    *color;
  ConstObject *o_current = RenderAdaptor->object;

  if (!o_current->selected) {
    color = geda_color_x11_color_from_index(o_current->color);
  }
  else {
    color = geda_color_x11_color_from_index(SELECT_COLOR);
  }

  RenderAdaptor->geda_draw_set_color(color->red, color->green, color->blue);
}

static void
x_draw_arc_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_ARC(o_current)) {

    GedaArc *o_arc;
    int scx, scy;
    int radius;
    int sradi;
    int angle1;
    int angle2;

    o_arc   = o_current->arc;
    radius  = o_arc->radius;
    sradi   = SCREENabs(w_current, radius);
    angle1  = o_arc->start_angle;
    angle2  = o_arc->arc_sweep;

    WORLDtoSCREEN (w_current, o_arc->x,  o_arc->y, &scx,  &scy);

    RenderAdaptor->geda_draw_arc (scx, scy, sradi, angle1, angle2);
  }
}

static void
x_draw_circle_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_CIRCLE(o_current)) {

    GedaCircle *o_circle;
    int scx, scy;
    int radius;

    o_circle = o_current->circle;

    radius = SCREENabs(w_current, o_current->circle->radius);

    WORLDtoSCREEN (w_current, o_circle->center_x, o_circle->center_y, &scx,  &scy);

    RenderAdaptor->geda_draw_circle (scx, scy, radius);
  }
}

static void
x_draw_box_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_BOX(o_current)) {

    GedaBox *o_box;
    int sx1, sy1, sx2, sy2;
    int width, height;

    o_box = o_current->box;

    WORLDtoSCREEN (w_current, o_box->upper_x, o_box->upper_y, &sx1, &sy1);
    WORLDtoSCREEN (w_current, o_box->lower_x, o_box->lower_y, &sx2, &sy2);

    width  = sx2 - sx1;
    height = sy2 - sy1;

    RenderAdaptor->geda_draw_box (sx1, sy1, width, height);
  }
}

static void
x_draw_line_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_LINE(o_current)) {

    GedaLine *o_line;
    int sx1, sy1, sx2, sy2;

    o_line = o_current->line;

    WORLDtoSCREEN (w_current, o_line->x[0],  o_line->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_line->x[1],  o_line->y[1], &sx2,  &sy2);

    RenderAdaptor->geda_draw_line (sx1, sy1, sx2, sy2);
  }
}

static void
x_draw_bus_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_LINE(o_current)) {

    GedaLine *o_line;
    int sx1, sy1, sx2, sy2;

    o_line = o_current->line;

    WORLDtoSCREEN (w_current, o_line->x[0],  o_line->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_line->x[1],  o_line->y[1], &sx2,  &sy2);

    RenderAdaptor->geda_draw_net (sx1, sy1, sx2, sy2);
  }
}

static void
x_draw_net_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_LINE(o_current)) {

    GedaLine *o_line;
    int sx1, sy1, sx2, sy2;

    o_line = o_current->line;

    WORLDtoSCREEN (w_current, o_line->x[0],  o_line->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_line->x[1],  o_line->y[1], &sx2,  &sy2);

    RenderAdaptor->geda_draw_net (sx1, sy1, sx2, sy2);
  }
}

static void
x_draw_path_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_PATH(o_current)) {

    GedaObject *tmp_obj;
    GedaPath   *s_path;
    int         nsections;
    int         i;

    tmp_obj   = geda_path_object_copy (o_current);
    s_path    = tmp_obj->path;
    nsections = s_path->num_sections;

    for (i = 0; i < nsections; i++) {

      PATH_SECTION *section = &s_path->sections[i];
      int tmp_x, tmp_y;

      tmp_x = section->x1;
      tmp_y = section->y1;
      WORLDtoSCREEN (w_current, tmp_x, tmp_y, &section->x1,  &section->y1);

      tmp_x = section->x2;
      tmp_y = section->y2;
      WORLDtoSCREEN (w_current, tmp_x, tmp_y, &section->x2,  &section->y2);

      tmp_x = section->x3;
      tmp_y = section->y3;
      WORLDtoSCREEN (w_current, tmp_x, tmp_y, &section->x3,  &section->y3);
    }

    RenderAdaptor->geda_draw_path (nsections, s_path->sections);
    g_object_unref(tmp_obj);
  }
}

static void
x_draw_pin_object (GschemToplevel *w_current)
{
  x_draw_line_object (w_current);
}

static void
x_draw_picture_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  if (GEDA_IS_PICTURE(o_current)) {

    GedaPicture *o_pic;
    int sx1, sy1, sx2, sy2;
    int width, height;

    o_pic = o_current->picture;

    WORLDtoSCREEN (w_current, o_pic->upper_x,  o_pic->upper_y, &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_pic->lower_x,  o_pic->lower_y, &sx2,  &sy2);

    width  = sx2 - sx1;
    height = sy2 - sy1;

    RenderAdaptor->geda_draw_picture (sx1, sy1, width, height);
  }
}

static void
x_draw_text_object (GschemToplevel *w_current)
{
  ConstObject *o_current = RenderAdaptor->object;

  /* First check if this is visible */
  if ( geda_object_get_is_visible(o_current)) {

    if (GEDA_IS_TEXT(o_current)) {

      GedaText *o_text;
      int   sx, sy;

      o_text = o_current->text;

      WORLDtoSCREEN (w_current, o_text->x,  o_text->y, &sx,  &sy);

      RenderAdaptor->geda_draw_text (sx, sy);
    }
  }
}

static void
x_draw_complex (GschemToplevel *w_current)
{
  GList *iter = RenderAdaptor->object->complex->prim_objs;

  while (iter) {
    GedaObject *o_child = (GedaObject*)iter->data;
    x_draw_object(w_current, o_child);
    iter = iter->next;
  }
}

extern "C" void
x_draw_object (GschemToplevel *w_current, GedaObject *o_current)
{
  g_return_if_fail (o_current != NULL);

  if (w_current->render_adaptor == CAIRO_ADAPTOR) {
    eda_renderer_draw (CairoRenderer, o_current);
  }
  else {

    void (*draw_func)(GschemToplevel *w_current);

    switch (o_current->type) {
      case OBJ_LINE:        draw_func = x_draw_line_object;    break;
      case OBJ_NET:         draw_func = x_draw_net_object;     break;
      case OBJ_BUS:         draw_func = x_draw_bus_object;     break;
      case OBJ_PIN:         draw_func = x_draw_pin_object;     break;
      case OBJ_BOX:         draw_func = x_draw_box_object;     break;
      case OBJ_ARC:         draw_func = x_draw_arc_object;     break;
      case OBJ_CIRCLE:      draw_func = x_draw_circle_object;  break;
      case OBJ_PATH:        draw_func = x_draw_path_object;    break;
      case OBJ_TEXT:        draw_func = x_draw_text_object;    break;
      case OBJ_PICTURE:     draw_func = x_draw_picture_object; break;
      case OBJ_COMPLEX:
      case OBJ_PLACEHOLDER: draw_func = x_draw_complex;
      break;

      default:
        BUG_IMSG("unhandled case <%d>", o_current->type);
        return;
    }

    RenderAdaptor->object = o_current;

    //CALLGRIND_START_INSTRUMENTATION;

    x_draw_set_object_color (w_current);

    draw_func (w_current);

    //CALLGRIND_STOP_INSTRUMENTATION;
  }
}

extern "C" void
x_draw_set_surface(GschemToplevel *w_current)
{
  if (Current_Page && w_current->render_adaptor == X11_ADAPTOR) {

    double kx = Current_Page->to_screen_x_constant;

    RenderAdaptor->geda_draw_set_surface(w_current->cr, kx);
  }
}

extern "C" char*
x_draw_get_font(void)
{
  char strBuffer[128];
  RenderAdaptor->geda_draw_set_font(&strBuffer[0], sizeof(strBuffer));
  return geda_utility_string_strdup(&strBuffer[0]);
}

extern "C" void
x_draw_set_font(const char *font_string, int size)
{
  if (font_string) {

#ifdef HAVE_XFT

    RenderAdaptor->geda_draw_set_font(font_string, size);

#else

    char *font_name;

    font_name = x_draw_strip_font_provider(font_string);
    RenderAdaptor->geda_draw_set_font(font_name, size);
    GEDA_FREE(font_name);

#endif

  }
}

/*! \brief Get Font Listing
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a list of font names.
 *
 *  \note the returned color-map MUST be freed using g_array_free.
 */
extern "C" GArray*
x_draw_get_font_list(const char *pattern)
{
#if HAVE_XFT
  const char *default_pattern = "*";
#else
  const char *default_pattern = "-*-*-medium-r-normal--0-0-0-0-p-0-iso10646-1";
#endif

  GArray     *font_list;

  if (pattern == NULL) {
    pattern = default_pattern;
  }

  font_list = g_array_sized_new (FALSE, FALSE, sizeof(char *), 256);

  if (font_list) {

    if (!RenderAdaptor->geda_draw_get_font_list (pattern, font_list)) {
      font_list = NULL;
    }
  }
  else {
    BUG_MSG("Unable to get font list from RenderAdaptor");
  }
  return font_list;
}

extern "C" char*
x_draw_strip_font_provider(const char *font_string)
{
  char  strBuffer[128];
  char *font_name;
  int   length;
  int   index;

  /* This ensures we return at least what was passed in */
  font_name = &strBuffer[0];

  length = strlen ( strcpy (&strBuffer[0], font_string));

  for(index = 0; index < length; index++) {
    if (strBuffer[index] == ASCII_COMMA) {
      font_name = &strBuffer[index + 2];  /* Save pointer to font name */
      continue;
    }
    if (isupper(strBuffer[index])) {
      strBuffer[index] = strBuffer[index] ^ 0x20; /* Make lower case*/
    }
  }
  return geda_utility_string_strdup(font_name);
}

extern "C" int
x_draw_set_text_bounds(GedaObject *object)
{
  int result;

  RenderAdaptor->object = object;

  result = RenderAdaptor->geda_draw_get_text_bounds (&object->left,
                                                     &object->top,
                                                     &object->right,
                                                     &object->bottom);
  return result;
}

extern "C" void
x_draw_initialize(GschemToplevel *w_current)
{
  EdaConfig  *cfg      = eda_config_get_user_context();
  const char *group    = IVAR_CONFIG_GROUP;
        char *font_string;
        char *font_name;

  v_log_message(_("Initializing: Graphics Renderer Adaptor...."));

  font_string   = eda_config_get_string (cfg, group, "default-font-name", NULL);

#ifdef WITH_LIBGEDADRAW

  font_name     = x_draw_strip_font_provider(font_string);

  RenderAdaptor = new EdaX11Render(font_name, w_current->text_size);

  RenderAdaptor->geda_draw_set_surface(w_current->cr, 5.5);

  gschem_atexit(x_draw_shutdown, NULL);

#else

  RenderAdaptor =NULL;

#endif

  GEDA_FREE(font_name);

  GEDA_FREE(font_string);

  v_log_message("%s\n", _("done"));
}

extern "C" void
x_draw_shutdown(void *user_data)
{
  v_log_message(_("Shutting down: Graphics Renderer Adaptor..."));

#ifdef WITH_LIBGEDADRAW

  delete RenderAdaptor;

#endif

  v_log_message("%s\n", _("Done, renderer is down"));
}
