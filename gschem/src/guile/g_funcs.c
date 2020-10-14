/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_funcs.c
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */

#include <gschem.h>
#include <geda_debug.h>

/*!
 * \file g_funcs.c
 * \brief Scheme General API functions
 *  This module contains routines for general Scheme API functions
 *  to dialogs and options under the file menu.
 */

/*!
 * \internal function: g_funcs_common_image
 * \par Function Description
 *
 * called by: g_funcs_bmp_image
 *            g_funcs_jpeg_image
 *            g_funcs_png_image
 *            g_funcs_tiff_image
 *
 * param scm_filename scm string passed that was passed to caller.
 * param func         c string of scheme API function name.
 * param type         c string is basically the file extension.
 */
static
SCM  g_funcs_common_image(SCM scm_filename, const char *func, const char *type)
{
  SCM_ASSERT (scm_is_string (scm_filename), scm_filename, SCM_ARG1, func);

  GschemToplevel *w_current = g_current_window ();

  if (w_current) {

    int use_color = 0;
    int invert    = 0;

    if (output_filename) {

      x_image_lowlevel (w_current, output_filename,
                        w_current->image_width,
                        w_current->image_height,
                        type,
                        Image_All,
                        use_color,   /* Don't use print colors */
                        invert);     /* Dont invert bw only */
    }
    else  {

      char *filename;

      filename = scm_to_utf8_string (scm_filename);

      x_image_lowlevel (w_current, filename,
                        w_current->image_width,
                        w_current->image_height,
                        type,
                        Image_All,
                        use_color,
                        invert);
      free(filename);
    }

    return SCM_BOOL_T;
  }
  else {
    return SCM_BOOL_F;
  }
}

/*!
 * \brief SCM API Export BMP image
 * \par Function Description
 *  Scheme API to export the current document as a BMP image.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if success, SCM_BOOL_F on failure.
 */
SCM g_funcs_bmp_image(SCM scm_filename)
{
  return g_funcs_common_image (scm_filename, "gschem-bmp-image", "bmp");
}

/*!
 * \brief SCM API Yes No Confirmation Dialog
 * \par Function Description
 *  Launches a confirmation dialog with the given \a scm_msg
 *  and returns True or False based on the users response.
 *
 * \param scm_msg Scheme string with message to display in dialog
 *
 * \returns SCM_BOOL_T if response was yes, otherwise SCM_BOOL_F.
 */
SCM g_funcs_confirm(SCM scm_msg)
{
  int response;
  char *msg;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg, SCM_ARG1, "gschem-msg");

  msg = scm_to_utf8_string (scm_msg);
  response = x_dialog_confirmation (msg, GTK_MESSAGE_INFO, FALSE);
  free(msg);

  if (response == GEDA_RESPONSE_YES)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/*!
 * \brief SCM API Yes No Cancel Confirmation Dialog
 * \par Function Description
 *  Launches a confirmation dialog with the given \a scm_msg
 *  and returns True or False based on the users response.
 *  If the user chooses to cancel, then -1 is returned.
 *
 * \param scm_msg Scheme string with message to display in dialog
 *
 * \returns 1 if response was yes, 0 if no, or -1 for user pressed cancel.
 */
SCM g_funcs_confirm_cancel(SCM scm_msg)
{
  int   response;
  char *msg;
  SCM   scm_response;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg, SCM_ARG1, "gschem-msg");

  msg = scm_to_utf8_string (scm_msg);
  response = x_dialog_confirm_with_cancel (msg, GTK_MESSAGE_INFO, FALSE);
  free(msg);

  scm_response  = scm_from_int (-1);

  switch (response) {
    case GEDA_RESPONSE_YES:
      scm_response  = scm_from_int (1);
      break;
    case GEDA_RESPONSE_NO:
      scm_response  = scm_from_int (0);
      break;
    case GEDA_RESPONSE_CANCEL:
      default: /* The default response is cancel */
      scm_response  = scm_from_int (-1);
      break;
  }
  return scm_response;
}

/*!
 * \brief SCM API terminate program
 * \par Function Description
 *  Scheme API function to terminate gschem without saving any
 *  open documents.
 *
 *  Should this be g_funcs_abort
 */
SCM g_funcs_exit(SCM status)
{
  int status_code;

  x_window_close_all(g_current_window ());

  gschem_quit();

  if (scm_is_integer (status)) {
    status_code = scm_to_int(status);
  }
  else {
    status_code = 0;
  }
  exit(status_code);
}

/*!
 * \brief SCM API File Select Dialog
 * \par Function Description
 *  This function launches a file select dialog and returns the
 *  selected filename to the calling Scheme procedure. If the
 *  user cancels the filename is NULL.
 */
SCM g_funcs_filesel(SCM scm_msg, SCM scm_templ, SCM scm_flags)
{
  int c_flags;
  char *r, *msg, *templ;
  SCM v;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg,
              SCM_ARG1, "gschem-filesel");

  SCM_ASSERT (scm_is_string (scm_templ), scm_templ,
              SCM_ARG2, "gschem-filesel");

  /*! \bug FIXME -- how to deal with conflicting flags?
   * Should I throw a scheme error?  Just deal in the c code?
   */
  for (c_flags = 0; scm_is_pair (scm_flags); scm_flags = SCM_CDR (scm_flags)) {

    char *flag;
    SCM scm_flag = SCM_CAR (scm_flags);

    flag = scm_to_utf8_string (scm_flag);

    if (strcmp (flag, "may_exist") == 0) {
      c_flags |= FSB_MAY_EXIST;

    }
    else if (strcmp (flag, "must_exist") == 0) {
      c_flags |= FSB_MUST_EXIST;

    }
    else if (strcmp (flag, "must_not_exist") == 0) {
      c_flags |= FSB_SHOULD_NOT_EXIST;

    }
    else if (strcmp (flag, "save") == 0) {
      c_flags |= FSB_SAVE;

    }
    else if (strcmp (flag, "open") == 0) {
      c_flags |= FSB_LOAD;

    }
    else {
      free(flag);
      scm_wrong_type_arg ("gschem-filesel", SCM_ARG3, scm_flag);
    }
    free(flag);
  }

  scm_dynwind_begin (0);
  msg = scm_to_utf8_string (scm_msg);
  scm_dynwind_free (msg);

  templ = scm_to_utf8_string (scm_templ);
  scm_dynwind_free (templ);

  r = x_dialog_select_file (NULL, msg, templ, c_flags);
  scm_dynwind_unwind_handler (g_free, r, SCM_F_WIND_EXPLICITLY);

  v = scm_from_utf8_string (r);

  scm_dynwind_end();

  return v;
}

/*! \brief SCM API Export JPEG image
 *  \par Function Description
 *   Scheme API to write the current document as a JPEG image.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if success, SCM_BOOL_F on failure.
 */
SCM g_funcs_jpeg_image(SCM scm_filename)
{
  return g_funcs_common_image (scm_filename, "gschem-jpeg-image", "jpeg");
}

/*!
 * \brief SCM API Write to log
 * \par Function Description
 * \todo Should this be in libgeda?
 */
SCM g_funcs_log(SCM scm_msg)
{
  char *msg;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg, SCM_ARG1, "gschem-log");

  msg = scm_to_utf8_string (scm_msg);
  u_log_message ("%s", msg);
  free(msg);

  return SCM_BOOL_T;
}

/*!
 * \brief  SCM API Show message dialog
 * \par Function Description
 *  Allows Scheme routines to present a dialog to display a text message.
 */
SCM g_funcs_msg(SCM scm_msg)
{
  char *msg;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg, SCM_ARG1, "gschem-msg");

  msg = scm_to_utf8_string (scm_msg);

  x_dialog_show_message(msg, GEDA_MESSAGE_INFO, NULL);

  free(msg);

  return SCM_BOOL_T;
}

/*!
 * \brief SCM API Returns the Output Type
 * \par Function Description
 *  Returns the extension of output_filename or "pdf" if output_filename
 *  is not set.
 */
SCM g_funcs_output_type(void)
{
  SCM scm_return_value;

  if (output_filename) {

    const char *return_value;

    return_value     = geda_file_get_filename_ext (output_filename);
    scm_return_value = scm_from_utf8_string (return_value);
  }
  else {
    scm_return_value = scm_from_utf8_string ("pdf");
  }

  return (scm_return_value);
}

/*!
 * \brief SCM API Export PDF Document
 * \par Function Description
 *  Scheme API to export the current document as a PDF document.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if success, SCM_BOOL_F on failure.
 */
SCM g_funcs_pdf (SCM scm_filename)
{
  GschemToplevel *w_current;
  bool status;

  SCM_ASSERT (scm_is_string (scm_filename), scm_filename, SCM_ARG1,
              "gschem-pdf");

  w_current= g_current_window ();

  if (output_filename) {
    status = x_print_export_pdf (w_current, output_filename);
  }
  else  {

    char *filename;

    filename = scm_to_utf8_string(scm_filename);
    status   = x_print_export_pdf (w_current, filename);
    free(filename);
  }

  return (status ? SCM_BOOL_T : SCM_BOOL_F);
}

/*!
 * \brief SCM API Export PNG image
 * \par Function Description
 *  Scheme API to write the current document as a PNG image.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if success, SCM_BOOL_F on failure.
 */
SCM g_funcs_png_image(SCM scm_filename)
{
  return g_funcs_common_image (scm_filename, "gschem-png-image", "png");
}

/*!
 * \brief SCM API Export Postscript image
 * \par Function Description
 *  Allows Scheme routines to export the current document
 *  as a Postscript image.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if failure, SCM_BOOL_F on success.
 */
SCM g_funcs_postscript(SCM scm_filename)
{
  GedaToplevel *toplevel;
  Page         *page;
  GArray       *color_map;
  bool          success;

  SCM_ASSERT (scm_is_string (scm_filename), scm_filename,
              SCM_ARG1, "gschem-postscript");

  toplevel  = edascm_c_current_toplevel ();
  page      = geda_toplevel_get_current_page(toplevel);
  color_map = geda_color_get_print_map();
  success   = FALSE;

  if (output_filename) {
    if (!geda_file_print_file (toplevel, page, color_map, output_filename)) {
      success = TRUE;
    }
  }
  else  {

    char *filename;

    filename = scm_to_utf8_string(scm_filename);

    if (!geda_file_print_file (toplevel, page, color_map, filename)) {
      success = TRUE;
    }
    free(filename);
  }

  g_array_free (color_map, TRUE);

  /* SCM_BOOL_F = success, SCM_BOOL_T = fail */
  return success ? SCM_BOOL_F : SCM_BOOL_T;
}

/*!
 * \brief SCM API Print Current Document
 * \par Function Description
 *  Scheme API to print the current document.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if failure, SCM_BOOL_F on success.
 */
SCM g_funcs_print(SCM scm_filename)
{
  GedaToplevel *toplevel;
  Page         *page;
  GArray       *color_map;
  bool          success;

  SCM_ASSERT (scm_is_string (scm_filename), scm_filename,
              SCM_ARG1, "gschem-print");

  toplevel  = edascm_c_current_toplevel ();
  page      = geda_toplevel_get_current_page(toplevel);
  color_map = geda_color_get_print_map();
  success   = FALSE;

  if (output_filename) {
    if (!geda_file_print_file (toplevel, page, color_map, output_filename)) {
      success = TRUE;
    }
  }
  else  {

    char *filename;

    filename = scm_to_utf8_string(scm_filename);

    if (!geda_file_print_file (toplevel, page, color_map, filename)) {
      success = TRUE;
    }
    free(filename);
  }

  /* SCM_BOOL_F = success, SCM_BOOL_T = fail */
  return success ? SCM_BOOL_F : SCM_BOOL_T;
}

/*!
 * \brief Scheme API Save the Current File
 * \par Function Description
 *  This function accomplishes the same effect as using the builtin
 *  "file-save" function, which is to save the active document. The
 *  difference is that "file-save" is threaded and control will likely
 *  be returned to Scheme before the io operation completes. If Scheme
 *  needs to process the file imediately after saving, then use this
 *  function instead. Control will not return to Scheme until after
 *  the save operation.
 */
SCM g_funcs_save_file(void)
{
  GschemToplevel *w_current;
  bool            status;

  w_current = g_current_window ();

  if (w_current) {

    Page *p_current;

    p_current = gschem_toplevel_get_current_page (w_current);

    if (p_current) {

      if(p_current->filename == NULL)
        w_current->force_save_as = TRUE;

      if (strstr(p_current->filename, w_current->toplevel->untitled_name))
      {
        w_current->force_save_as = TRUE;
      }

      if (w_current->force_save_as) {
        x_fileselect_save (w_current);
      }
      else {
        x_window_save_page (w_current, p_current, p_current->filename);
      }
      status = TRUE;
    }
    else {
      status = FALSE;
    }
  }
  else {
    BUG_MSG("Bad pointer to toplevel");
    status = FALSE;
  }

  return (status ? SCM_BOOL_T : SCM_BOOL_F);
}

/*! \brief SCM API Export TIFF image
 *  \par Function Description
 *   Scheme API to write the current document as a TIFF image.
 *
 * \param scm_filename Scheme filename string is ignored if output
 *                     output specified on the command-line.
 *
 * \returns SCM_BOOL_T if success, SCM_BOOL_F on failure.
 */
SCM g_funcs_tiff_image(SCM scm_filename)
{
  return g_funcs_common_image (scm_filename, "gschem-tiff-image", "tiff");
}

/*!
 * \brief Scheme API Set Top Level Variables
 * \par Function Description
 *  This functions calls i_vars_set and this will reset the value of
 *  top-level variables to the current "defaults", which can be set
 *  individually using scheme. Some variables may not be set if the
 *  "default" is set to RC_NIL = -1, in which case the value assigned
 *  by configuration will be retain, if default is a non RC_NIL value
 *  the current "default" will over-ride values set during configuration.
 */
SCM g_funcs_use_rc_values(void)
{
  i_vars_set(g_current_window ());

  return SCM_BOOL_T;
}
