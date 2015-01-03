/* -*- C++ o_draw.cpp indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: o_draw.cpp
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
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

#include <string>

#include <gschem.h>
#include <geda_draw.h>

#include <geda_debug.h>

#include <valgrind/callgrind.h>

static EdaX11Render *RenderAdaptor;

static void
x_draw_set_color(GschemToplevel *w_current)
{
  GdkColor *color;
  Object   *o_current = RenderAdaptor->object;

  if (!o_current->selected) {
    color = x_color_get_color_from_index(o_current->color);
  }
  else {
    color = x_color_get_color_from_index(SELECT_COLOR);
  }

  RenderAdaptor->geda_draw_set_color(color->red, color->green, color->blue);
}

static void
x_draw_arc (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_ARC(o_current)) {

    Arc *o_arc;
    int scx, scy;
    int radius;
    int sradi;
    int angle1;
    int angle2;

    x_draw_set_color (w_current);

    o_arc   = o_current->arc;
    radius  = o_arc->width / 2;
    sradi   = SCREENabs(w_current, radius);
    angle1  = o_arc->start_angle;
    angle2  = o_arc->arc_sweep;

    WORLDtoSCREEN (w_current, o_arc->x,  o_arc->y, &scx,  &scy);

    RenderAdaptor->geda_draw_arc (scx, scy, sradi, angle1, angle2);
  }
}

static void
x_draw_circle (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_CIRCLE(o_current)) {

    Circle *o_circle;
    int scx, scy;
    int radius;

    x_draw_set_color (w_current);

    o_circle = o_current->circle;

    radius = SCREENabs(w_current, o_current->circle->radius);

    WORLDtoSCREEN (w_current, o_circle->center_x, o_circle->center_y, &scx,  &scy);

    RenderAdaptor->geda_draw_circle (scx, scy, radius);
  }
}

static void
x_draw_box (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_BOX(o_current)) {

    Box *o_box;
    int sx1, sy1, sx2, sy2;
    int width, height;

    x_draw_set_color (w_current);

    o_box = o_current->box;

    WORLDtoSCREEN (w_current, o_box->upper_x,  o_box->upper_y, &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_box->lower_x,  o_box->lower_y, &sx2,  &sy2);

    width  = sx2 - sx1;
    height = sy2 - sy1;

    RenderAdaptor->geda_draw_box (sx1, sy1, width, height);
  }
}

static void
x_draw_line (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_LINE(o_current)) {

    Line *o_line;
    int sx1, sy1, sx2, sy2;

    x_draw_set_color (w_current);

    o_line = o_current->line;

    WORLDtoSCREEN (w_current, o_line->x[0],  o_line->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_line->x[1],  o_line->y[1], &sx2,  &sy2);

    RenderAdaptor->geda_draw_line (sx1, sy1, sx2, sy2);
  }
}

static void
x_draw_bus (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_LINE(o_current)) {

    Line *o_line;
    int sx1, sy1, sx2, sy2;

    x_draw_set_color (w_current);

    o_line = o_current->line;

    WORLDtoSCREEN (w_current, o_line->x[0],  o_line->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_line->x[1],  o_line->y[1], &sx2,  &sy2);

    RenderAdaptor->geda_draw_net (sx1, sy1, sx2, sy2);
  }
}

static void
x_draw_net (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_LINE(o_current)) {

    Line *o_line;
    int sx1, sy1, sx2, sy2;

    x_draw_set_color (w_current);

    o_line = o_current->line;

    WORLDtoSCREEN (w_current, o_line->x[0],  o_line->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_line->x[1],  o_line->y[1], &sx2,  &sy2);

    RenderAdaptor->geda_draw_net (sx1, sy1, sx2, sy2);
  }
}

static void
x_draw_path (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_PATH(o_current)) {

    Object *tmp_obj;
    Path   *s_path;
    int     nsections;
    int     i;

    x_draw_set_color (w_current);

    tmp_obj   = o_path_copy (o_current);
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
x_draw_pin (GschemToplevel *w_current)
{
  x_draw_line (w_current);
}
static void
x_draw_picture (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  if (GEDA_IS_PICTURE(o_current)) {

    Picture *o_pic;
    int sx1, sy1, sx2, sy2;
    int width, height;

    //x_draw_set_color (w_current);

    o_pic = o_current->picture;

    WORLDtoSCREEN (w_current, o_pic->upper_x,  o_pic->upper_y, &sx1,  &sy1);
    WORLDtoSCREEN (w_current, o_pic->lower_x,  o_pic->lower_y, &sx2,  &sy2);

    width  = sx2 - sx1;
    height = sy2 - sy1;

    RenderAdaptor->geda_draw_picture (sx1, sy1, width, height);
  }
}

static void
x_draw_text (GschemToplevel *w_current)
{
  Object *o_current = RenderAdaptor->object;

  /* First check if this is visible */
  if ( o_get_is_visible(o_current)) {

    if (GEDA_IS_TEXT(o_current)) {

      Text *o_text;
      int   sx, sy;

      x_draw_set_color (w_current);

      o_text = o_current->text;

      WORLDtoSCREEN (w_current, o_text->x,  o_text->y, &sx,  &sy);

      RenderAdaptor->geda_draw_text (sx, sy);
    }
  }
}

static void
x_draw_complex (GschemToplevel *w_current)
{
  GList    *iter;

  iter = RenderAdaptor->object->complex->prim_objs;

  while (iter) {
    Object *o_child = (Object*)iter->data;
    x_draw_object(w_current, o_child);
    iter = iter->next;
  }
}

extern "C" void
x_draw_object (GschemToplevel *w_current, Object *o_current)
{
  void (*draw_func)(GschemToplevel *w_current);

  g_return_if_fail (o_current != NULL);

  if(w_current->render_adaptor == CAIRO_ADAPTOR) {
    eda_renderer_draw (CairoRenderer, o_current);
  }
  else {

    switch (o_current->type) {
      case OBJ_LINE:        draw_func = x_draw_line;    break;
      case OBJ_NET:         draw_func = x_draw_net;     break;
      case OBJ_BUS:         draw_func = x_draw_bus;     break;
      case OBJ_PIN:         draw_func = x_draw_pin;     break;
      case OBJ_BOX:         draw_func = x_draw_box;     break;
      case OBJ_ARC:         draw_func = x_draw_arc;     break;
      case OBJ_CIRCLE:      draw_func = x_draw_circle;  break;
      case OBJ_PATH:        draw_func = x_draw_path;    break;
      case OBJ_TEXT:        draw_func = x_draw_text;    break;
      case OBJ_PICTURE:     draw_func = x_draw_picture; break;
      case OBJ_COMPLEX:
      case OBJ_PLACEHOLDER: draw_func = x_draw_complex;
      break;

      default:
        g_return_if_reached ();
    }

    RenderAdaptor->object = o_current;

    CALLGRIND_START_INSTRUMENTATION;

    draw_func (w_current);

    CALLGRIND_STOP_INSTRUMENTATION;
  }
}

extern "C" void
x_draw_set_surface(GschemToplevel *w_current)
{
  if (Current_Page) {
    RenderAdaptor->geda_draw_set_surface(w_current->cr,
                                         Current_Page->to_world_x_constant);
  }
  else {
    BUG_MSG("Current page is invalid");
  }
}

extern "C" char*
x_draw_get_font(void)
{
  char strBuffer[128];
  RenderAdaptor->geda_draw_set_font(&strBuffer[0], sizeof(strBuffer));
  return u_string_strdup(&strBuffer[0]);
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

  bool result;

  if (pattern == NULL) {
    pattern = default_pattern;
  }

  font_list = g_array_sized_new (FALSE, FALSE, sizeof(char *), 256);

  if (font_list) {
    result = RenderAdaptor->geda_draw_get_font_list (pattern, font_list);
    if (!result) {
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
  return u_string_strdup(font_name);
}

extern "C" int
x_draw_set_text_bounds(Object *object)
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

  font_name     = x_draw_strip_font_provider(font_string);

  RenderAdaptor = new EdaX11Render(font_name, w_current->text_size);

  RenderAdaptor->geda_draw_set_surface(w_current->cr, 5.5);

  GEDA_FREE(font_name);

  GEDA_FREE(font_string);

  geda_atexit(x_draw_shutdown, NULL);

  v_log_message(_("done\n"));
}

extern "C" void
x_draw_shutdown(void *user_data)
{
  v_log_message(_("Shutting down: Graphics RenderAdaptorer..."));
  delete RenderAdaptor;
  v_log_message(_("done\n"));
}
