/* -*- C o_picture.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 *
 * This module contains routines to add or replace picture objects, and to
 * draw and remove out-line boxes, referred to as "rubber", during placement
 * and editing. Users are free to change the aspect ratio of the image when
 * placing or re-sizing. Users can also press the control key to constrain
 * the ratio to the original value. For new pictures, this will be the same
 * as the file version. The original aspect ratio when re-sizing is the ratio
 * before re-sizing and thus the resulting ratio depends on whether the image
 * had been previously distorted.
 *
 * \todo There is currently no direct means to restore the orginal aspect
 *       ratio, which is save in the picture object.
 */

#include <math.h>
#include <errno.h>

#include <gschem.h>
#include <geda_image_chooser.h>

/* Include GUI functions in libgedauio */
#include <geda/geda_gui_funcs.h>
#include <geda_debug.h>

/* These are just used to shorten lines, no magic */
#define QUAD_14_ASPECT(w) ((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio
#define QUAD_23_ASPECT(w) ((w)->first_wx - (w)->second_wx)/(w)->pixbuf_wh_ratio

/*! \brief End the input of a new Picture.
 *
 *  \par Function Description
 *  This function completes the input of the second corner of a picture. The
 *  picture is defined by (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>
 *  and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  The temporary picture frame is erased, a new picture object is allocated,
 *  initialized and linked to the object list. The object is finally drawn on
 *  the current sheet.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (not used)
 *  \param [in] w_y        (not used)
 */
void
o_picture_end(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int picture_left,  picture_top;
  int picture_width, picture_height;

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  i_status_action_stop(w_current);

  /* erase the temporary picture */
  w_current->rubber_visible = FALSE;

  picture_left   = w_current->rubber_x1;
  picture_top    = w_current->rubber_y1;
  picture_width  = w_current->rubber_x2;
  picture_height = w_current->rubber_y2;

  /* pictures with null width and height are not allowed */
  if ((picture_width != 0) && (picture_height != 0)) {

    GedaObject *new_obj;

    /* Create a new picture object */
    new_obj = geda_picture_object_new(NULL, 0, w_current->pixbuf_filename,
                                      picture_left, picture_top,
                                      picture_left + picture_width,
                                      picture_top - picture_height,
                                      0, FALSE, FALSE);

    geda_struct_page_append_object (toplevel->page_current, new_obj);

    /* Run %add-objects-hook */
    g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);

    o_undo_savestate_object(w_current, UNDO_ALL, new_obj);

  }  /* else cancel creation of object */
}

/*! \brief Draw picture from GschemToplevel object
 *
 *  \par Function Description
 *  This function draws the box from the variables in the GschemToplevel
 *  structure <B>*w_current</B>.
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *Save h & w in case CONTROLKEY changes
 *  \param [in] w_current  The GschemToplevel object.
 */
void
o_picture_draw_rubber (GschemToplevel *w_current)
{
  GArray  *color_map;
  cairo_t *cr;

  int flags;
  int left, right, top, bottom;

  double
  wwidth     = 0;

  cr         = eda_renderer_get_cairo_context (CairoRenderer);
  color_map  = eda_renderer_get_color_map     (CairoRenderer);
  flags      = eda_renderer_get_cairo_flags   (CairoRenderer);

  if (w_current->second_wy > w_current->first_wy) {    /* Quad 1 & 2 Box upper */
    if (w_current->second_wx > w_current->first_wx) {  /* Quad 1 Box upper to right */
      left    = w_current->first_wx;
      bottom  = w_current->first_wy;
      right   = w_current->second_wx;
      if (!w_current->CONTROLKEY) {
        top   = w_current->second_wy;
      }
      else {
        top   = bottom + QUAD_14_ASPECT (w_current);
      }
    }
    else {                                             /* Quad 2 Box upper to left */
      bottom  = w_current->first_wy;
      right   = w_current->first_wx;
      left    = w_current->second_wx;
      if (!w_current->CONTROLKEY) {
        top   = w_current->second_wy;
      }
      else {
        top   = bottom + QUAD_23_ASPECT (w_current);
      }
    }
  }
  else {                                               /* Quad 3 & 4 Box below */
    if (w_current->first_wx > w_current->second_wx) {  /* Quad 3 Box below to left */
      right     = w_current->first_wx;
      top       = w_current->first_wy;
      left      = w_current->second_wx;
      if (!w_current->CONTROLKEY) {
        bottom  = w_current->second_wy;
      }
      else {
        bottom  = top - QUAD_23_ASPECT (w_current);
      }
    }
    else {                                             /* Quad 4 Box below to right */
      left      = w_current->first_wx;
      top       = w_current->first_wy;
      right     = w_current->second_wx;
      if (!w_current->CONTROLKEY) {
        bottom  = w_current->second_wy;
      }
      else {
        bottom  = top - QUAD_14_ASPECT(w_current);
      }
    }
  }

  /* Save where we drew the rubber */
  w_current->rubber_x1 = left;
  w_current->rubber_y1 = top;
  w_current->rubber_x2 = right - left;   /* save the width */
  w_current->rubber_y2 = top - bottom;   /* and the height */

  eda_cairo_box (cr, flags, wwidth, left, bottom, right, top);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

#undef QUAD_14_ASPECT
#undef QUAD_23_ASPECT

/*! \brief Replace pictures with a new picture
 *
 * \par Function Description
 *  This function updates \a o_current picture object or all picture objects
 *  in the current selection if \a o_current is NULL, with the image specified
 *  by \a filename.
 *
 * \param [in]  w_current  The GschemToplevel object
 * \param [in]  filename   The filename of the new picture
 * \param [in]  o_current  The picture to update or NULL to use selection
 * \param [out] error      The location to return error information.
 *
 * \return TRUE on success, FALSE on failure.
 */
bool
o_picture_exchange (GschemToplevel *w_current,
                    const char     *filename,
                    GedaObject     *o_current,
                    GError        **error)
{
  bool result = TRUE;

  if (o_current) {

    if (o_current->type == OBJ_PICTURE) {

      if (geda_picture_object_set_from_file (o_current, filename, error)) {
        o_invalidate_object (w_current, o_current); /* Draw new picture */
      }
      else {
        result = FALSE;
      }

    }
  }
  else {

    GList *list;
    GList *iter;

    list = o_select_get_list_selected (w_current, OBJ_PICTURE);

    for (iter = list; iter != NULL; NEXT(iter)) {

      GedaObject *object = (GedaObject*) iter->data;

      if (geda_picture_object_set_from_file (object, filename, error)) {
        o_invalidate_object (w_current, object); /* Draw new picture */
      }
      else {
        result = FALSE;
        break;
      }
    }

    g_list_free(list);
  }

  if (result) {
    o_undo_savestate(w_current, UNDO_ALL);
  }

  return result;
}

/*! \brief Display an Image Chooser Dialog to exchange picture image
 *
 *  \par Function Description
 *  This function opens a file chooser to allow users to select an image file.
 *  A check-button (box) is added to the dialog allowing users to specify if
 *  all selected Pictures should be updated. If a file name is returned from
 *  the chooser dialog, the filename is passed to o_picture_exchange, with
 *  the object if the check-box is NOT checked. If the check-box is checked
 *  \a o_current is not passed to o_picture_exchange.
 *
 * \param [in] w_current  Pointer to GschemToplevel structure
 * \param [in] o_current  Picture Object to update or NULL to use selection
 */
void
o_picture_exchange_file (GschemToplevel *w_current, GedaObject *o_current)
{
  GedaToplevel *toplevel  = w_current->toplevel;
  GList        *iter;
  const char   *oldfilename;
  char         *filename;
  int           count;

  if (o_current && o_current->type == OBJ_PICTURE) {
    oldfilename = geda_picture_object_get_filename(o_current);
  }
  else {
    oldfilename = NULL;
  }

  /* Get count of picture objects in selection set */
  count = 0;
  iter  = geda_list_get_glist (toplevel->page_current->selection_list);

  while(iter != NULL) {

    GedaObject *obj = (GedaObject*) iter->data;

    if (obj->type == OBJ_PICTURE) {
      count++;
    }
    NEXT(iter);
  }

  if (count == 1) {
    filename = x_fileselect_select_image(w_current, oldfilename);
  }
  else { /* Use a custom image chooser */

    EdaConfig *cfg;
    GtkWidget *cb_all;
    GtkWidget *dialog;
    bool       update_all;

    cfg  = eda_config_get_user_context ();

    i_var_restore_window_boolean(cfg, "update-all-pitures", &update_all, FALSE);

    dialog = geda_image_chooser_new (w_current->main_window,
                                     IMAGE_CHOOSER_ACTION_OPEN);

    g_object_set (dialog, "select-multiple", FALSE, NULL);
    /* "local-only", TRUE, */

    /* If a file name was provided then use the path from the file
     * name and the user has read-access to the directory, then use
     * this directory as the starting point, and fill in the name if
     * there is only one picture selected */
    if (oldfilename) {

      char *filepath = geda_get_dirname (oldfilename);

      if (filepath && g_file_test (filepath, G_FILE_TEST_IS_DIR)) {

        errno = 0;
        access(filepath, R_OK);
        if (!errno) {
          geda_image_chooser_set_current_folder(dialog, filepath);
        }
        GEDA_FREE(filepath);
      }
      geda_image_chooser_set_filename (dialog, geda_file_get_basename(oldfilename));
    }
    else { /* start in current working directory, NOT in 'Recently Used' */

      char *cwd = g_get_current_dir ();

      geda_image_chooser_set_current_folder (dialog, cwd);
      GEDA_FREE (cwd);
    }

    cb_all = gtk_check_button_new_with_label (_("Update All"));
    gtk_widget_show (cb_all);
    gtk_widget_set_tooltip_text(cb_all, _("Update all selected Picture images"));
    SetToggleState (cb_all, update_all);
    geda_image_chooser_append_extra (dialog, cb_all);

    gtk_widget_show (dialog);

    if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {
      filename = geda_utility_string_strdup(geda_image_chooser_get_filename (dialog));
    }
    else {
      filename = NULL;
    }

    /* Save the check-box state even if the user canceled the dialog */
    if (GetToggleState (cb_all) != update_all) {
      update_all = !update_all;
      eda_config_set_boolean (cfg, WINDOW_CONFIG_GROUP,
                              "update-all-pitures", update_all);
    }

    gtk_widget_destroy (dialog);

    if (update_all) {
      o_current = NULL; /* Erase pointer to force using selection set */
    }
  }

  if (filename) {

    GError *err = NULL;

    /* Actually update the pictures */
    if (!o_picture_exchange (w_current, filename, o_current, &err)) {

      const char *message = err->message;

      /* Log the error */
      geda_log("%s: %s\n", _("Failed to replace picture"), message);

      /* inform the user */
      pango_error_dialog ( _("<b>Failed to replace picture</b>"), message);

      /* clear error */
      g_error_free(err);
    }

    GEDA_FREE (filename);
  }
}

void
o_picture_export (GschemToplevel *w_current, GedaObject *o_current)
{
  GtkWidget   *dialog;
  GtkWidget   *cb_aspect;
  bool         old_preview_enabled;
  static bool  orgin_size = FALSE;

  dialog = geda_image_chooser_new (w_current->main_window,
                                   IMAGE_CHOOSER_ACTION_SAVE);

  old_preview_enabled = geda_chooser_get_preview_widget_active(dialog);

  geda_chooser_set_preview_widget_active(dialog, FALSE);

  cb_aspect = gtk_check_button_new_with_label (_("Original size"));
  gtk_widget_show (cb_aspect);
  gtk_widget_set_tooltip_text(cb_aspect, _("When checked the image size will be exported"));
  SetToggleState (cb_aspect, orgin_size);
  geda_image_chooser_append_extra (dialog, cb_aspect);

  geda_chooser_set_do_overwrite_confirmation(dialog, TRUE);
  geda_image_chooser_set_filter (dialog, FILTER_IMAGES);
  geda_image_chooser_set_filename (dialog, o_current->picture->filename);

  gtk_widget_show (dialog);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {

    const char *file_ext;
    char       *filename;
    int         result;

    filename = geda_image_chooser_get_filename (dialog);
    file_ext = geda_file_get_filename_ext(filename);

    orgin_size = GetToggleState (cb_aspect);

    if (orgin_size) {
      result = geda_picture_object_export_orginal(o_current, filename, file_ext, NULL);
    }
    else {
      result = geda_picture_object_export_object(o_current, filename, file_ext, NULL);
    }

    if (!result) {
      pango_error_dialog("Failed to export symbol:", strerror (errno));
    }

    GEDA_FREE (filename);
  }

  /* Restore the enable preview setting */
  GEDA_IMAGE_CHOOSER(dialog)->preview_enabled = old_preview_enabled;

  gtk_widget_destroy (dialog);
}

/*! \brief Initialize Variables to input a new picture object.
 *  \par Function Description
 *  This function initialize variables to input a new picture. Parameters for
 *  the picture are stored in the <B>w_current</B> toplevel structure.
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
static void o_picture_init(GschemToplevel *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  w_current->rubber_visible = TRUE;
  o_box_invalidate_rubber (w_current);
}

/*! \brief Invalidate the Rubber for Picture Objects
 *
 *  \par Function Description
 *  This function invalidates the regions where the temporary box
 *  was drawn when sizing or re-sizing picture objects. The width
 *  and height are determined based on what was drawn, rather than
 *  using macros GET_PICTURE_WIDTH and GET_PICTURE_HEIGHT because
 *  the state of CONTROLKEY state may have changed during motion
 *  and we need to invalidate the rubber that what actually drawn.
 */
void
o_picture_invalidate_rubber (GschemToplevel *w_current)
{
  int left, top, width, height;

  WORLDtoSCREEN (w_current, w_current->rubber_x1,
                            w_current->rubber_y1, &left, &top);

  width  = SCREENabs (w_current, w_current->rubber_x2);
  height = SCREENabs (w_current, w_current->rubber_y2);

  o_invalidate_rectangle (w_current, left, top, left + width, top);
  o_invalidate_rectangle (w_current, left, top, left, top + height);
  o_invalidate_rectangle (w_current, left + width, top, left + width, top + height);
  o_invalidate_rectangle (w_current, left, top + height, left + width, top + height);

#if DEBUG
  fprintf(stderr, "%s \tleft  %d, top %d, width %d, height %d\n", __func__,
                        left,     top,    width,    height);
#endif
}

/*! \brief Draw temporary picture out-line while sizing pictures
 *
 *  \par Function Description
 *  This function is called to update the coordinates of the pointer
 *  position. New world coordinates second_wx, and second_wy  are set
 *  according to the <B>w_x</B> and <B>w_y</B> parameters.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void
o_picture_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action) {

    /* erase the previous temporary box */
    if (w_current->rubber_visible) {
      o_picture_invalidate_rubber (w_current);
    }

    /* update the points with pointer/mouse coordinates */
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    /* set flag to draw the new temporary box */
    w_current->rubber_visible = 1;
  }
}

/*! \brief Set active pixbuf top-level parameterd
 *
 *  \par Function Description
 *  The function is used when inserting new picture objects in order
 *  to temporily store picture object properties, until o_picture_end
 *  actually creates a GedaPicture Object to hold this data.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] filename
 */
bool
o_picture_set_pixbuf(GschemToplevel *w_current, char *filename)
{
  GError    *error = NULL;
  GdkPixbuf *pixbuf;

  int result;

  /* Make sure any old pixbuf info is release */
  if (w_current->current_pixbuf != NULL) {
    GEDA_UNREF(w_current->current_pixbuf);
    w_current->current_pixbuf = NULL;
    BUG_MSG("w_current->current_pixbuf was not NULL")
  }

  GEDA_FREE(w_current->pixbuf_filename);

  /*  w_current_pixbuf is unreferenced in 3 places:
   *
   *    1. i_status_set_state_msg  This would be the NORMAL method
   *    2. x_event_button_pressed  if user pushed the middle mouse
   *    3. Above in this function  Should not happen
   */

  pixbuf = gdk_pixbuf_new_from_file (filename, &error);

  if (pixbuf) {

    int height;
    int width;

    w_current->current_pixbuf  = pixbuf;
    w_current->pixbuf_filename = (char *) geda_utility_string_strdup(filename);

    width  = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);

    w_current->pixbuf_wh_ratio = (double) width / height;

    result = TRUE;
  }
  else {

    char *errmsg;

    if (error) {
      errmsg = geda_sprintf ( "%s: %s.", _("Error"), error->message);
      g_error_free(error);
    }
    else {
      errmsg = geda_sprintf ("%s: %s\n%s.", _("Error"), filename, _("An unknown error occurred"));
    }

    titled_pango_error_dialog ( _("<b>Failed to load picture</b>"), errmsg,
                                _("Load failed"));
    GEDA_FREE(errmsg);
    result = FALSE;
  }
  return result;
}

/*! \brief Start process to input a new picture.
 *  \par Function Description
 *  This function starts the process to input a new picture.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_picture_init(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_picture_init, o_picture_end);
}
