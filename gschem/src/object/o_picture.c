/* -*- C o_picture.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
/*!
 * file o_picture.c
 * \brief Low-level module for manipulating Picture objects
 * \todo o_picture.c conflicts with o_picture.c in libgeda
 */
#include <gschem.h>
#include <geda_debug.h>

/* This works, but using one macro inside of other doesn't */
#define GET_PICTURE_WIDTH(w) abs((w)->second_wx - (w)->first_wx)
#define GET_PICTURE_HEIGHT(w) \
  (w)->pixbuf_wh_ratio == 0 ? 0 : abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio
#define GET_PICTURE_LEFT(w) min((w)->first_wx, (w)->second_wx)
#define GET_PICTURE_TOP(w) (w)->first_wy > (w)->second_wy ? (w)->first_wy  :  \
                           (w)->first_wy+abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio

/*! \brief Start process to input a new picture.
 *  \par Function Description
 *  This function starts the process to input a new picture. Parameters
 *  for this picture are put into/extracted from the <B>w_current</B> toplevel
 *  structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in world
 *  coordinates.
 *
 *  The first step is to input one corner of the picture. This corner is
 *  (<B>w_x</B>,<B>w_y</B>) snapped to the grid and saved in
 *  <B>w_current->first_wx</B> and <B>w_current->first_wy</B>.
 *
 *  The other corner will be saved in (<B>w_current->second_wx</B>,
 *  <B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_start(GschemToplevel *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  o_picture_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a circle.
 *
 *  \par Function Description
 *  This function ends the input of the second corner of a picture.
 *  The picture is defined by (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>
 *  and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  The temporary picture frame is erased ; a new picture object is allocated,
 *  initialized and linked to the object list ; The object is finally
 *  drawn on the current sheet.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_picture_end(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object *new_obj;
  int picture_width, picture_height;
  int picture_left, picture_top;

  if (w_current->inside_action == 0) {
    u_log_message("Internal Error Detected: <o_picture_end> Not inside action\n");
    return;
  }

  /* erase the temporary picture */
  /* o_picture_draw_rubber(w_current); */
  w_current->rubber_visible = 0;

  picture_width  = GET_PICTURE_WIDTH (w_current);
  picture_height = GET_PICTURE_HEIGHT(w_current);
  picture_left   = GET_PICTURE_LEFT  (w_current);
  picture_top    = GET_PICTURE_TOP   (w_current);

  /* pictures with null width and height are not allowed */
  if ((picture_width == 0) && (picture_height == 0)) {
    /* cancel the object creation */
    return;
  }

  /* create the object */
  new_obj = o_picture_new(NULL, 0, w_current->pixbuf_filename,
                          picture_left, picture_top,
                          picture_left + picture_width,
                          picture_top - picture_height,
                          0, FALSE, FALSE);

  s_page_append_object (toplevel->page_current, new_obj);

  /* Run %add-objects-hook */
  g_run_hook_object (w_current, "%add-objects-hook", new_obj);

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw temporary picture while dragging edge
 *
 *  \par Function Description
 *  This function is used to draw the box while dragging one of its edge or
 *  angle. It erases the previous temporary box drawn before, and draws
 *  a new updated one. <B>w_x</B> and <B>w_y</B> are the new position of the mobile
 *  point, ie the mouse.
 *
 *  The old values are inside the <B>w_current</B> pointed structure. Old
 *  width, height and left and top values are recomputed by the corresponding
 *  macros.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_motion (GschemToplevel *w_current, int w_x, int w_y)
{
#if DEBUG
  printf("o_picture_rubberbox called\n");
#endif
  if (w_current->inside_action == 0) {
    u_log_message("Internal Error Detected: <o_picture_motion> Not inside action\n");
    return;
  }

  /* erase the previous temporary box */
  if (w_current->rubber_visible)
    o_picture_invalidate_rubber (w_current);

  /*
   * New values are fixed according to the <B>w_x</B> and <B>w_y</B> parameters.
   * These are saved in <B>w_current</B> pointed structure as new temporary values.
   * The new box is then drawn.
   */

  /* update the coords of the corner */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* draw the new temporary box */
  o_picture_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Invalidate the Rubber for Picture Objects
 *
 *  \par Function Description
 *
 *  \note used in button cancel code in x_events.c
 */
void o_picture_invalidate_rubber (GschemToplevel *w_current)
{
  int left, top, width, height;

  WORLDtoSCREEN (w_current,
                 GET_PICTURE_LEFT(w_current), GET_PICTURE_TOP(w_current),
                 &left, &top);
  width = SCREENabs (w_current, GET_PICTURE_WIDTH (w_current));
  height = SCREENabs (w_current, GET_PICTURE_HEIGHT(w_current));

  o_invalidate_rectangle (w_current, left, top, left + width, top);
  o_invalidate_rectangle (w_current, left, top, left, top + height);
  o_invalidate_rectangle (w_current, left + width, top, left + width, top + height);
  o_invalidate_rectangle (w_current, left, top + height, left + width, top + height);
}

/*! \brief Draw picture from GschemToplevel object
 *
 *  \par Function Description
 *  This function draws the box from the variables in the GschemToplevel
 *  structure <B>*w_current</B>.
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void o_picture_draw_rubber (GschemToplevel *w_current)
{
  int left, top, width, height;
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags = eda_renderer_get_cairo_flags (CairoRenderer);

  /* get the width/height and the upper left corner of the picture */
  left =   GET_PICTURE_LEFT (w_current);
  top =    GET_PICTURE_TOP (w_current);
  width =  GET_PICTURE_WIDTH (w_current);
  height = GET_PICTURE_HEIGHT (w_current);

  eda_cairo_box (cr, flags, wwidth, left, top - height, left + width, top);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

/*! \brief Replace all selected pictures with a new picture
 *
 * \par Function Description
 * Replaces all pictures in the current selection with a new image.
 *
 * \param [in] w_current  The GschemToplevel object
 * \param [in] filename   The filename of the new picture
 * \param [out] error     The location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
bool o_picture_exchange (GschemToplevel *w_current,
                    const gchar *filename, GError **error)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList *iter;

  for (iter = geda_list_get_glist (Top_Selection); iter != NULL; NEXT(iter))
  {
    Object *object = (Object *) iter->data;
    if (object == NULL) {
      u_log_message("Internal Error Detected: <o_picture_exchange> object = NULL\n");
      return FALSE;
    }

    if (object->type == OBJ_PICTURE) {

      bool status;

      /* Erase previous picture */
      o_invalidate_object (w_current, object);

      status = o_picture_set_from_file (object, filename, error);
      if (!status) return FALSE;

      /* Draw new picture */
      o_invalidate_object (w_current, object);
    }
  }
  return TRUE;
}

/*! \brief Create dialog to exchange picture objects
 *
 *  \par Function Description
 *  This function opens a file chooser and replaces all pictures of the selections
 *  with the new picture.
 *
 *  \todo Maybe merge this dialog function with picture_selection_dialog()
 */
void o_picture_change_filename_dialog (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GError       *err      = NULL;
  GtkWidget    *dialog;

  char  *cwd;
  char  *filename;
  bool   result;


  dialog = gtk_file_chooser_dialog_new (_("Select a picture file..."),
                                        GTK_WINDOW(w_current->main_window),
                                        0,
                                        GTK_STOCK_CANCEL,
                                        GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,
                                        GTK_RESPONSE_ACCEPT,
                                        NULL);


  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  if (w_current->pixbuf_filename) {
    gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(dialog),
                                  w_current->pixbuf_filename);
  }
  else { /* no filename then get current working dir */
    cwd = getcwd(0,0);
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
    free (cwd);
  }

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {

    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    gtk_widget_destroy(dialog);
    dialog=NULL;

    /* Actually update the pictures */
    result = o_picture_exchange (w_current, filename, &err);

    if (!result) {

      /* Log the error */
      u_log_message( _("%s: Failed to replace picture: %s"), __func__, err->message);

      /* inform the user */
      pango_error_dialog ( _("<b>Failed to replace picture</b>"), err->message );

      /* clear error */
      g_error_free(err);
    }
    else {
      toplevel->page_current->CHANGED=1;
    }
    GEDA_FREE (filename);
  }

  if (dialog) {
    gtk_widget_destroy(dialog);
    dialog=NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] pixbuf
 *  \param [in] filename
 */
void o_picture_set_pixbuf(GschemToplevel *w_current,
                          GdkPixbuf *pixbuf, char *filename)
{

  /* need to put an error messages here */
  if (pixbuf == NULL)  {
    fprintf(stderr, "error! picture in set pixbuf was NULL\n");
    return;
  }

  if (w_current->current_pixbuf != NULL) {
    GEDA_UNREF(w_current->current_pixbuf);
    w_current->current_pixbuf=NULL;
  }

  if (w_current->pixbuf_filename != NULL) {
    GEDA_FREE(w_current->pixbuf_filename);
    w_current->pixbuf_filename=NULL;
  }

  w_current->current_pixbuf = pixbuf;
  w_current->pixbuf_filename = (char *) u_string_strdup(filename);

  w_current->pixbuf_wh_ratio = gdk_pixbuf_get_width(pixbuf) /
                                        gdk_pixbuf_get_height(pixbuf);

  /* be sure to free this pixbuf somewhere */
}
