/* -*- C x_settings.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_settings.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2012-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
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
 * Date: Aug, 17, 2012
 * Contributing Author: Wiley Edward Hill
 *
*/
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/17/12 |  Initial release.
 * ------------------------------------------------------------------
 * WEH | 12/02/12 |  Added function x_settings_set_scm_int, revised
 *                |  generate_rc to use the template rc file and to
 *                |  write to the user's home directory. (This resolves
 *                |  implementation issues on Linux system that were due
 *                |  to permissions associated with /etc/geda.)
 * ------------------------------------------------------------------
 * WEH | 12/04/12 |  Added function x_settings_set_scm_int, revised
 * ------------------------------------------------------------------
 * WEH | 01/06/13 |  Completed code on handlers for bus_ripper_rotation
 *     |          |  bus_ripper_size, bus_ripper_type, and added handler
 *     |          |  for bus_ripper_symname (to complete the integration of
 *     |          |  the Ripper settings on the configure settings dailog.
 * ------------------------------------------------------------------
 * WEH | 07/19/13 |  fixed stack overflow in get_titleblock_list fix
 *                |  i < MAX_FILENAME instead if MAX_PATH
 * ------------------------------------------------------------------
 * WEH | 09/25/13 |  Add function x_settings_save_settings
 * ------------------------------------------------------------------
 * WEH | 03/10/14 |  Add call to function x_status_bar_update_middle_mouse
 *     |          |  in function configure_dialog_response (to update the
 *     |          |  status-bar when the dialog closes)
 * ------------------------------------------------------------------
 * WEH | 01/14/15 |  Add call to function i_status_update_grid_info in
 *     |          |  function configure_dialog_response (to update the
 *     |          |  status-bar when the dialog closes)
 * ---------------|--------------------------------------------------
 * WEH | 03/13/15 | Add saving text-marker-threshold to key file, add
 *                | keyword writer text marker threshold as a double.
 * ---------------|--------------------------------------------------
 * WEH | 04/12/15 | Add configure_dialog_response, set the toggle menu
 *     |          | options without checking dialog set or was canceled.
 * ---------------|--------------------------------------------------
 * WEH | 06/28/15 | Add saving Log & Console Related to key file,
 *     |          | (previously the variables were not  being saved).
 * ---------------|--------------------------------------------------
 * WEH | 06/28/15 | Add keyword writers auto_pan and auto_pan_step
 *                | (to support new keywors).
 * ---------------|--------------------------------------------------
*/

/*!
 * \file x_settings.c
 * \brief Program configuration settings support module
 */

#include <gtk/gtk.h>

#include <sys/types.h>
#include <dirent.h>
#include <libgen.h>

#include "../../include/gschem.h"
#include "../../include/gschem_xdefines.h"
#include "../../include/x_menus.h"          /* MenuToggleItem enumeration */
#include "../../include/x_settings.h"
#include "../../include/keywords.h"

#include <geda/geda_standard.h>
#include <geda/geda_stat.h>
#include <geda_debug.h>

/** \defgroup Settings-Auxillary-Module Settings Auxiliary Module
 *  @{
 *  \ingroup Settings-Dialog
 *  \par
 *   This module contains routines for the user's preference settings,
 *   primarily support functions for the preference Settings dialog.
 *   This module is reponsible for generating or regenerating user
 *   RC scripts/files that serve to establish program settings
 *   during startup as well as saving all settings restored by
 *   i_vars.c but not the settings restored by x_windows.c, which
 *   saves all settings that it restores.
 */

#define RC_INPUT_BUFFER_SIZE 256

#ifdef __GNUC__
  #define LineTerminator { LINE_FEED, ASCII_NUL};
#else
  #define LineTerminator { CARRIAGE_RETURN, LINE_FEED, ASCII_NUL};
#endif

#define FilterList w_current->component_select_attrlist

#define KEYWORD(symbol, narg, sarg, func) [ kw_##func ] = { #symbol, do_kw_##func, narg, sarg },

char output_buffer[RC_INPUT_BUFFER_SIZE];
extern const char* DefaultFilterList[];

struct {
   const char *name;
   void (*func) (GschemToplevel *w_current, FILE* input, FILE* output);
   unsigned int narg;
   char *strBuffer;
} keyword_struc[KEYWORD_COUNT] = {
 [ kw_unknown ] = { "unknown", 0, 0, 0 },
#include <keywords.h>
};

#define KEY_FUNC(symbol)keyword_struc[kw_##symbol].func
#define KEY_BUFFER(symbol)keyword_struc[kw_##symbol].strBuffer
#define KEY_NAME(symbol)keyword_struc[kw_##symbol].name
#define KEY_INTEGER(symbol)keyword_struc[kw_##symbol].narg

#define kw_integer(var)keyword_struc[var].narg

/*! \brief Response function for the configure_settings dialog
 *  \par Function Description
 *  This function destroys the configure_settings dialog. Depending
 *  on how the dialog was terminated, GatherSettings maybe call to
 *  retrieve values from widgets and possiably generate or regenerate
 *  the users RC file. Changes to certain variables require modules
 *  to be updated and these modules are called before exiting.
 */
void configure_dialog_response(GtkWidget *Dialog, int response,
                               GschemToplevel *w_current)
{
  switch (response) {
    case GEDA_RESPONSE_APPLY:
      GatherSettings (w_current);
      i_window_set_grid_type (w_current);
      generate_rc(w_current, "gschemrc");
      break;

    case GEDA_RESPONSE_OK:
      GatherSettings (w_current);
      i_window_set_grid_type (w_current);
    case GEDA_RESPONSE_DELETE_EVENT:
    case GEDA_RESPONSE_CANCEL:
      /* void */
      break;

    default:
      BUG_IMSG ("unhandled case", response);
  }

  if (w_current->save_ui_settings) {

    GtkWidget  *notebook;
    EdaConfig  *cfg;
    const char *group;
    int         note_tab;

    notebook = GEDA_OBJECT_GET_DATA(Dialog, "notebook");
    note_tab = gtk_notebook_get_current_page (GTK_NOTEBOOK(notebook));
    cfg      = eda_config_get_user_context();
    group    = IVAR_CONFIG_GROUP;

    eda_config_set_integer (cfg, group, "pref-tab", note_tab);
  }
  gtk_widget_destroy(w_current->cpwindow);

  i_status_update_grid_info(w_current);

  /* Call to update the middle-mouse label on the status bar, which may or
   * may not need updating, is not worth tracking here. If the middle mouse
   * was not "changed" to "repeat and the current setting is not "repeat,
   * then the string being passed will be ignored */
  x_status_bar_update_middle_mouse(w_current, "Preferences");
  x_status_bar_update_third_mouse(w_current);

  /* Update toggle option in menu, not worth checking for changes */
  x_menu_set_togglable(w_current, AUTO_PAN_TOGGLE, w_current->auto_pan);
  x_menu_set_togglable(w_current, DRAG_CAN_MOVE,   w_current->drag_can_move);
  x_menu_set_togglable(w_current, MAGNETIC_TOGGLE, w_current->magnetic_net_mode);
  x_menu_set_togglable(w_current, OUTLINE_TOGGLE,  w_current->action_feedback_mode);
  x_menu_set_togglable(w_current, RUBBER_TOGGLE,   w_current->netconn_rubberband);
  x_menu_set_togglable(w_current, SNAP_TOGGLE,     w_current->snap);
}

/* ----------------- Start Attribute TAB Support Functions ------------------ */

/*! \brief Create the configure_settings dialog and activates it
 *  \par Function Description
 *  This function is call to creates the configure_settings dialog.
 */
void x_configure_settings (GschemToplevel* w_current)
{
  EdaConfig *cfg;
  int        last_tab;

  w_current->cpwindow = create_settings_dialog( w_current);

  gtk_window_set_destroy_with_parent (GTK_WINDOW(w_current->cpwindow), TRUE);

  gtk_window_set_position (GTK_WINDOW (w_current->cpwindow), GTK_WIN_POS_MOUSE);

  load_settings_dialog (w_current);

  cfg      = eda_config_get_user_context();
  last_tab = eda_config_get_integer (cfg, IVAR_CONFIG_GROUP, "pref-tab", NULL);
  if (last_tab >= 0) {
    GtkNotebook *notebook = GEDA_OBJECT_GET_DATA(w_current->cpwindow, "notebook");
    gtk_notebook_set_current_page (notebook, last_tab);
  }

  g_signal_connect (w_current->cpwindow, "response",
                    G_CALLBACK(configure_dialog_response), w_current);

  gtk_widget_show(w_current->cpwindow);

  return;

}

void x_settings_save_settings(GschemToplevel *w_current)
{
  GedaToplevel *toplevel   = w_current->toplevel;
  EdaConfig    *cfg        = eda_config_get_user_context ();
  const char   *group_name = IVAR_CONFIG_GROUP;
  int           array[4];

  /* Image Related */
  eda_config_set_boolean (cfg, group_name, "image-color",    toplevel->image_color);
  eda_config_set_boolean (cfg, group_name, "invert-images",  toplevel->invert_images);
  eda_config_set_integer (cfg, group_name, "image-width",    w_current->image_width);
  eda_config_set_integer (cfg, group_name, "image-height",   w_current->image_height);

  /* Miscellaneous - in  alphabetical order - Restored by i_vars_recall_user_settings */
  eda_config_set_integer (cfg, group_name, "auto-save-interval",       toplevel->auto_save_interval);
  eda_config_set_boolean (cfg, group_name, "continue-component-place", w_current->continue_component_place);
  eda_config_set_boolean (cfg, group_name, "embed-components",         w_current->embed_components);
  eda_config_set_boolean (cfg, group_name, "enforce-hierarchy",        w_current->enforce_hierarchy);
  eda_config_set_boolean (cfg, group_name, "force-boundingbox",        w_current->force_boundingbox);
  eda_config_set_integer (cfg, group_name, "keyboardpan-gain",         w_current->keyboardpan_gain);
  eda_config_set_boolean (cfg, group_name, "netconn-rubberband",       w_current->netconn_rubberband);
  eda_config_set_integer (cfg, group_name, "select-slack-pixels",      w_current->select_slack_pixels);
  eda_config_set_integer (cfg, group_name, "snap-size",                w_current->snap_size);
  eda_config_set_boolean (cfg, group_name, "sort-component-library",   w_current->sort_component_library);

  /* Save text related stuff  - Restored by i_vars_recall_user_settings */
  eda_config_set_integer (cfg, group_name, "text-case",       w_current->text_case);
  eda_config_set_integer (cfg, group_name, "text-zoomfactor", w_current->text_display_zoomfactor);
  eda_config_set_integer (cfg, group_name, "text-feedback",   w_current->text_feedback);
  eda_config_set_integer (cfg, group_name, "text-size",       w_current->text_size);

  /* text-marker probably belong in the x_window_save group but for now well keep
   * text-marks stuff with text stuff */
  eda_config_set_boolean (cfg, group_name, "text-origin-marker",    CairoRenderer->text_origin_marker);
  eda_config_set_integer (cfg, group_name, "text-marker-size",      CairoRenderer->text_marker_size);
  eda_config_set_double  (cfg, group_name, "text-marker-threshold", CairoRenderer->text_marker_threshold);

  array[0] = CairoRenderer->text_marker_color.pixel;
  array[1] = CairoRenderer->text_marker_color.red;
  array[2] = CairoRenderer->text_marker_color.green;
  array[3] = CairoRenderer->text_marker_color.blue;
  eda_config_set_int_list (cfg, group_name, "text_marker_color", array, 4);

  /* Undo Sub-System - Restored by o_undo_init */
  eda_config_set_boolean (cfg, group_name, "undo-control",  w_current->undo_control);
  eda_config_set_integer (cfg, group_name, "undo-levels",   w_current->undo_levels);
  eda_config_set_boolean (cfg, group_name, "undo-panzoom",  w_current->undo_panzoom);
  eda_config_set_boolean (cfg, group_name, "undo-preserve", w_current->undo_preserve);
  eda_config_set_integer (cfg, group_name, "undo-type",     w_current->undo_type);

  /* Log Console Related */
  group_name = LOG_CONFIG_GROUP;
  eda_config_set_integer (cfg, group_name, "logging",             logging);
  eda_config_set_integer (cfg, group_name, "log-destiny",         log_destiny);

  group_name = IDS_CONSOLE;
  eda_config_set_integer (cfg, group_name, "console-window",      console_window);
  eda_config_set_integer (cfg, group_name, "console-window-type", console_window_type);
}

/** @brief function change_default_titleblock in GatherSettings */
bool x_settings_set_scm_int(char *symbol_name, int value )
{
  if (symbol_name) {

    char  s_val[5];
    char  buffer[128];
    char *str;
    char *strbuff;

    /* Get string representation of the integer */
    strbuff = &buffer[0];
    str     = geda_utility_string_int2str(value, s_val, 10);

    strcpy(strbuff, "(define ");
    strcat(strbuff, symbol_name );
    strcat(strbuff, " " );
    strcat(strbuff, str );
    strcat(strbuff, ")" );
    g_evaluate_c_string_protected(strbuff);
  }
  else
    return FALSE;
  return TRUE;
}

/** @} endgroup X_Settings_Attribute */

/* ------------------ End Attribute TAB Support Functions ------------------ */

/** \defgroup Settings-Dialog-Titleblock Auxillary Support for Titleblocks
 *  @{
 *  \ingroup Settings-Auxillary-Module
 *  \par Contains function to provide additional support for titleblock
 *       related items on the Settings Dialog
 */

/*! \brief Returns count files in title-block directory.
 *  \par Function Description
 *  This functions returns the numbers files located in the title-block
 *  folder. This is used to determine how char array to allocate.
 *
 * \retval Returns integer count or -1 if the folder is inaccessible.
*/
int get_titleblock_cnt(void) {

  char TitleBlockPath[MAX_PATH];
  int  count = 0;
  DIR *dirp;

  strcpy (TitleBlockPath, geda_sys_data_path());
  strcat (TitleBlockPath, TITLE_BLOCK_PATH);

  dirp = opendir (TitleBlockPath);

  if (dirp != NULL) {

     struct dirent *ent;

     /* get all the files within directory */
     while ((ent = readdir (dirp)) != NULL) {

       const char *suffix = geda_file_get_filename_ext(ent->d_name);

       if (suffix && strcmp (suffix, SYMBOL_FILE_SUFFIX) == 0) {
         count++;
       }
     }
     closedir (dirp);
  }
  else {

    /* Could not open directory */
    geda_log("%s: %s\n", _("error opening"), TitleBlockPath);
    count--; /* decrement to -1 */
  }
  return count;
}

/*! \brief Fills char buffer with names of all standard title-blocks.
 *  \par Function Description
 *  This functions compiles a list of all of the symbol files in the
 *  title-block folder, adding each symbol file name to the supplied
 *  char array.
 *
 * \retval Returns TRUE on success, otherwise FALSE.
*/
bool get_titleblock_list(char **Buffer) {

  bool result;
  char TitleBlockPath[MAX_PATH];
  DIR *dirp;

  strcpy (TitleBlockPath, geda_sys_data_path());
  strcat (TitleBlockPath, TITLE_BLOCK_PATH);

  dirp   = opendir (TitleBlockPath);

  if (dirp != NULL) {

    struct dirent *ent;
    int index =0;

    /* get all the files within directory */
    while ((ent = readdir (dirp)) != NULL) {

      const char *suffix = geda_file_get_filename_ext(ent->d_name);

      if (suffix && !geda_utility_string_stricmp (suffix, SYMBOL_FILE_SUFFIX))
      {
        char tmpbuff[MAX_FILENAME];
        int  namelen;
        int  i;

        strcpy (tmpbuff, basename(ent->d_name));
        namelen = strlen (tmpbuff) - 4; /* substract the extension */

        for (i = namelen; i < MAX_FILENAME - namelen; i++) {
          tmpbuff[i] = '\0';
        }
        strcpy (Buffer[index++], tmpbuff);
      }
    }
    closedir (dirp);
    result = TRUE;
  }
  else { /* could not open directory */
    geda_log("%s: \"%s\", %s\n", _("Failed to open"), TitleBlockPath,
                  strerror(errno));
    result = FALSE;
  }

  return result;
}

/** @} endgroup Settings-Dialog-Titleblock */

/* --------------------- Begin Read and Write RC File -------------------- */

/** \defgroup Settings-Read-Write Settings Read Write RC File
 *  @{
 *  \ingroup Settings-Auxillary-Module
 *  \par Read and Write RC File
 */

/** @brief function process_rc_buffer in Settings-Read-Write
 * \par Function Description
 *      Pre-parser to check input line read in from template rc file. This
 *      function performs an inital interrogation of lines read in from a
 *      file. If \a keyword format is detected then the string is copied to
 *      the supplied address \a strbuffer and returns SUBSTITUTE. Otherwise
 *      LINE_FEED is returned to indicate the line should be written back
 *      out.
 */

static int process_rc_buffer(char *strbuffer, char *keyword) {

  char *s_ptr;
  char *e_ptr;

  s_ptr = strbuffer;

  if ( *s_ptr == ASCII_CR)        /* if blank line then just write it back out */
    return LINE_FEED;

  while ( *s_ptr == SPACE) { ++s_ptr; } /* increment passed any leading spaces */
  e_ptr = s_ptr;                        /* reset starting ptr to 1st char  */
  e_ptr++;                              /* increment end ptr so is in front of start ptr */

  if ( *e_ptr == ASCII_CR)              /* if line was only spaces, just ignore it */
    return LINE_FEED;

  while (( *e_ptr == SPACE) && ( *e_ptr != ASCII_CR) && ( *e_ptr != ASCII_NUL)) { ++e_ptr; } /* next char or bust */

  if (( *s_ptr == '(')  || (( *s_ptr == ';') && ( *e_ptr == '('))) /* if begin active or commented keyword */
  {
    int token_len;

    if (( *s_ptr == ';') && ( *e_ptr == '(')) {                  /* if commented keyword advance start ptr */
      s_ptr = ++e_ptr;
    }
    else if ( *s_ptr == '(') {                               /* else advance start ptr to 1st char of word */
      s_ptr++;
    }
    /* advance end ptr to end of keyword */
    while (( *e_ptr != SPACE) && ( *e_ptr != ASCII_CR) && ( *e_ptr != ASCII_NUL))
      { ++e_ptr; }

    token_len = e_ptr - s_ptr;

    strncpy(keyword, s_ptr, token_len);   /* extract the key-word string from the buffer */
    keyword[token_len ] = ASCII_NUL;      /* add terminator */
    keyword[token_len + 1] = ASCII_NUL;   /* add another terminator */

    return SUBSTITUTE;
  } /* end if begin active or commented keyword */

  return LINE_FEED;

}

/** @brief function generate_rc in Settings-Read-Write */
/*! \brief Main function generate new RC file.
 *  \par Function Description
 *       This function reads an RC file passed as an argument. The entire file
 *       is read one line at a time, searching for keywords and other lines
 *       interest. The function creates a new file, outputting all input lines
 *       that are not of interest, such as blank lines and commented remarks.
 *       Processing of line-of-interest is delegated to the Keyword_Handlers
 *       functions. After completion the original file is replaced by the new
 *       file.
 *
 */

int generate_rc(GschemToplevel *w_current, const char *rcname)
{
  char *inputfile;           /* Name of the input file */

  char *outputfile;          /* Name of the output file */

  FILE* input;               /* Input file handle */
  FILE* output;              /* Output file handle */

  char keyword[MAX_KEYWORD]; /* Buffer containing potential keyword */
  char strbuffer[RC_INPUT_BUFFER_SIZE];	/* Read Buffer */

  int result;                /* Our exit code */

  /* Build path for user config file */
  inputfile = geda_strconcat (geda_user_config_path(), DIR_SEPARATOR_S,
                              rcname, NULL);

  /* Check for existence of user config file */
  if(access(inputfile, R_OK) != 0) {

    char *templatefile;      /* Name of the Template file */

    /* Copy the template user config file to user's folder */
    templatefile = geda_strconcat (geda_sys_config_path (), DIR_SEPARATOR_S,
                                   "user-", rcname, NULL);
    result = geda_file_copy(templatefile, inputfile);
  }

  if (inputfile == NULL) {
    geda_log("%s-%s\n", _("File Name error! system"), rcname);
    return -1;
  }

  outputfile = geda_strconcat (geda_user_config_path(), DIR_SEPARATOR_S,
                               rcname, ".tmp", NULL);

  if ((input = fopen (inputfile, "r" )) == NULL) {

    const char *log_msg = _("File open for read-only error");

    geda_log("%s: \"%s\", %s\n", log_msg, inputfile, strerror(errno));
    result = errno;
  }
  else if ((output = fopen (outputfile, "w" )) == NULL) {

    const char *log_msg = _("Error opening output");

    geda_log("%s: \"%s\", %s\n", log_msg, inputfile, strerror(errno));
    fclose(input);
    result = -1;
  }
  else {

    int khandle;               /* Index of handler for found keyword */
    int last;                  /* Index of the handler called previously */
    int lc;                    /* Line counter */

    last = 0;
    lc   = 1;

    while (fgets(strbuffer, sizeof(strbuffer), input)) {

      if ((result = process_rc_buffer (strbuffer, keyword)) == LINE_FEED) {
        fputs(strbuffer, output);
      }
      else {                   /* found a keyword */

        int j;                 /* Index for enumerated keywords */


        khandle = kw_unknown;

        for (j = 0; j < KEYWORD_COUNT; j++) {               /* for all our keywords */
          if (!strcmp (keyword, keyword_struc[j].name)) {   /* see if match input keyword */
            khandle = j;
            break;
          }
        }     /* next j */

        if (khandle) {

          if ((khandle != last) || (kw_integer(khandle) != 0)) {
            /* if was not the same as the last one read  */
            last = khandle;            /* remember this keyword index */
            keyword_struc[khandle].strBuffer = strbuffer; /* provide handler ptr to raw input buffer */
            keyword_struc[khandle].func(w_current, input, output);  /* call handler */
          }
        }
        else { /* same as last keyword */
          fputs(strbuffer, output);
        }
      }
      ++lc;
    }
    fclose(input);
    fclose(output);
    result = EXIT_SUCCESS;
  }

  if (result == EXIT_SUCCESS) {
    if ((result = remove(inputfile)) == 0) {
      result = rename(outputfile, inputfile);
      /* Is inputfile or outputfile? */
      geda_log("%s \"%s\"\n", _("Wrote configuration to"), inputfile);
    }
    else {
      geda_log("%s: \"%s\", %s\n", _("File error"), inputfile, strerror(errno));
      result = errno;
    }
  }
  return result;
}

/** @brief function is_enabled in Settings-Read-Write
 *  \par Function Description
 *       Utility function used by keyword handlers to check for an
 *       initial semicolon. Returns TRUE is a semicolon is the first
 *       character, ignoring spaces.
 */
static bool is_enabled(const char* ptr) {
  /* Check if this entry is commented out */
  while (*ptr == SPACE)  { ++ptr; }
  if (*ptr == ASCII_OP)
    return TRUE;
  return FALSE;
}
/** @} endgroup Settings-Read-Write */

#define KEYWORD(func) void do_kw_##func(GschemToplevel *w_current, FILE* input, FILE* output)

/* --------------------- Begin Keyword Handler Functions ------------------- */

/** \defgroup Settings-Keyword-Handlers Settings Keyword Handlers
 *  @{
 *  \ingroup Settings-Auxillary-Module
 *  \par
 *  Read and Write RC File Keywords
 *
 */

/*!\warning: { Do not point to another KEY_BUFFER unless you know what your doing }*/

/** @brief function do_kw_load_in_rc in Settings-Keyword-Handlers */
/*! \brief keyword handler functions to process "load" entries in RC file.
 *  \par Function Description
 *       Currently supports load entries for color maps, but could be expanded
 *       as needed. All other load lines are written to the output unchanged.
 */
#define map_index rc_options.color_scheme_index
KEYWORD (load_in_rc) {

  char *ptr;
  bool cat_flag=FALSE;

/* \remark:
  local function either increments source ptr or adds a ";" to the output */
  void fix (int this)
  {
    if ((map_index == this) && !is_enabled(ptr))
      ptr++;  /* increment source pointer passed the semi-colon*/
    else
      if ((map_index != this) && is_enabled(ptr)) {
        strcpy (output_buffer, ";"); /* add a new semi-colon*/
        cat_flag=TRUE;               /* and set flag that we did this */
      }
  }

  ptr = KEY_BUFFER(load_in_rc);     /* get our pointer to the input buffer */

  /* The hard-coded integer below refers to the index from the combo box for
     the color map scheme, now stored in rc_options.color_scheme_index
   */
  if (strstr(ptr, DARK_DISPLAY_MAP) != NULL)
    fix(0);
  else
    if (strstr(ptr, LIGHT_DISPLAY_MAP) != NULL)
      fix(1);
    else
      if (strstr(ptr, BW_DISPLAY_MAP) != NULL)
        fix(2);
      else
        if (strstr(ptr, CUSTOM_DISPLAY_MAP) != NULL)
          fix(3);
        /* else do nothing, but could check for other "loads" */

  if (cat_flag)                  /* is flag set? */
    strcat (output_buffer, ptr); /* copy after the added semi-colon */
  else
    strcpy (output_buffer, ptr); /* copy to input to output */

  fputs(output_buffer, output);  /* write line to output file*/
}
#undef map_index

/** @brief function do_kw_define_in_rc in Settings-Keyword-Handlers */
KEYWORD (define_in_rc) {

  char *ptr;
  ptr = KEY_BUFFER(define_in_rc);   /* get our pointer to the input buffer */
  strcpy (output_buffer, ptr);      /* Assume that were are not changing   */

  if (rc_options.titleblock_index == -1) {

    if (strstr(ptr, "define default-titleblock" ) != NULL) {

      char Terminator[]= LineTerminator;

      /* could do this: ptr = geda_utility_string_scm2c( "default-titleblock" ) */
      strcpy (output_buffer, "(define default-titleblock \""); /* add a new semi-colon*/
      strcat (output_buffer, rc_options.titleblock_fname );
      strcat (output_buffer, "\")" );
      strcat (output_buffer, Terminator );
    }
  }
  fputs(output_buffer, output);  /* write line to output file*/
}

/** @brief function do_kw_draw_grips in Settings-Keyword-Handlers  */
KEYWORD (draw_grips) {
  RC_BOOLEAN_ROUT (draw_grips);
}


/** @brief function do_kw_logging in Settings-Keyword-Handlers */
KEYWORD (logging) {
  RC_BOOLEAN_GOUT (logging);
}

/** @brief function do_kw_render_adaptor in Settings-Keyword-Handlers */
KEYWORD (render_adaptor) {
  RC_RENDER_ADAPTOR_STRINGS;
  RC_STRING_TABLE_W2OUT (render_adaptor);
}

/** @brief function do_kw_action_color in Settings-Keyword-Handlers */
KEYWORD (action_color) {
  RC_INTEGER_WOUT (action_color);
}

/** @brief function do_kw_anti_aliasing in Settings-Keyword-Handlers */
KEYWORD (anti_aliasing) {
  RC_ANTI_ALIASING_STRINGS;
  RC_STRING_TABLE_W7OUT (anti_aliasing);
}

/** @brief function do_kw_grid_mode in Settings-Keyword-Handlers */
KEYWORD (grid_mode) {
  RC_GRID_MODE_STRINGS;
  RC_STRING_TABLE_W3OUT (grid_mode);
}

/** @brief function do_kw_grip_size in Settings-Keyword-Handlers */
KEYWORD (grip_size) {
  RC_INTEGER_WOUT (grip_size);
}

/** @brief function do_kw_dots_grid_dot_size in Settings-Keyword-Handlers */
KEYWORD (dots_grid_dot_size) {
  RC_INTEGER_TRIAD_WOUT (dots_grid_dot_size, 1, 2, 3);
}

/** @brief function do_kw_dots_grid_threshold in Settings-Keyword-Handlers */
KEYWORD (dots_grid_threshold) {
  RC_INTEGER_WOUT (dots_grid_threshold);
}

/** @brief function do_kw_dots_grid_mode in Settings-Keyword-Handlers */
KEYWORD (dots_grid_mode) {
  RC_DOTS_GRID_MODE_STRINGS
  RC_STRING_TABLE_W2OUT (dots_grid_mode)
}

/** @brief function do_kw_mesh_grid_threshold in Settings-Keyword-Handlers */
KEYWORD (mesh_grid_threshold) {
  RC_INTEGER_WOUT (mesh_grid_threshold);
}

/** @brief function do_kw_dots_grid_minor_alpha in Settings-Keyword-Handlers */
KEYWORD (dots_grid_minor_alpha) {
  RC_INTEGER_WOUT (dots_grid_minor_alpha);
}

/** @brief function do_kw_dots_grid_major_alpha in Settings-Keyword-Handlers */
KEYWORD (dots_grid_major_alpha) {
  RC_INTEGER_WOUT (dots_grid_major_alpha);
}

/** @brief function do_kw_mesh_line_width_factor in Settings-Keyword-Handlers */
KEYWORD (mesh_line_width_factor) {
  RC_INTEGER_WOUT (mesh_line_width_factor);
}

/** @brief function do_kw_mesh_grid_minor_alpha in Settings-Keyword-Handlers */
KEYWORD (mesh_grid_minor_alpha) {
  RC_INTEGER_WOUT (mesh_grid_minor_alpha);
}

/** @brief function do_kw_mesh_grid_major_alpha in Settings-Keyword-Handlers */
KEYWORD (mesh_grid_major_alpha) {
  RC_INTEGER_WOUT (mesh_grid_major_alpha);
}

/** @brief function do_kw_object_clipping in Settings-Keyword-Handlers */
KEYWORD (object_clipping) {
  RC_BOOLEAN_WOUT (object_clipping);
}

/** @brief function do_kw_scrollbars in Settings-Keyword-Handlers */
KEYWORD (scrollbars) {
  RC_BOOLEAN_WOUT (scrollbars);
}

/** @brief function do_kw_scrollbar_update in Settings-Keyword-Handlers */
KEYWORD (scrollbar_update) {
  RC_BARS_UPDATE_STRINGS
  RC_STRING_TABLE_W2OUT (scrollbar_update)
}

/** @brief function do_kw_scrollbars_visible in Settings-Keyword-Handlers */
KEYWORD (scrollbars_visible) {
  RC_BOOLEAN_WOUT (scrollbars_visible)
}

/** @brief function do_kw_scrollpan_steps in Settings-Keyword-Handlers */
KEYWORD (scrollpan_steps) {
  RC_INTEGER_WOUT (scrollpan_steps);
}

/** @brief function do_kw_warp_cursor in Settings-Keyword-Handlers */
KEYWORD (warp_cursor) {
  RC_BOOLEAN_WOUT (warp_cursor);
}

/** @brief function do_kw_window_size in Settings-Keyword-Handlers */
KEYWORD (window_size) {

  if(rc_options.custom_window_size == 1) {
     fputs(KEY_BUFFER(window_size), output);
  }
  else {

    int i;

    char *strings[] = {RC_STR_WINDOW_W650H487, RC_STR_WINDOW_W900H650,
                       RC_STR_WINDOW_W950H712, RC_STR_WINDOW_W1100H825};

    for (i = 0; i < 4; i++) {
      strcpy(output_buffer, (rc_options.window_size == i) ? "(" : ";(");
      strcat(output_buffer, strings[i]);
      strcat(output_buffer, "\n");
      fputs(output_buffer, output);
    }
  }
}

/** @brief function do_kw_world_size in Settings-Keyword-Handlers */
KEYWORD (world_size) {
  RC_WORLD_SIZE_STRINGS
  RC_STRING_TABLE_NQ_ROUT (world_size);
}

/** @brief function do_kw_zoom_gain in Settings-Keyword-Handlers */
KEYWORD (zoom_gain) {
  RC_INTEGER_WOUT (zoom_gain);
}

/** @brief function do_kw_zoom_with_pan in Settings-Keyword-Handlers */
KEYWORD (zoom_with_pan) {
  RC_BOOLEAN_WOUT (zoom_with_pan);
}

/** @brief function do_kw_log_destiny in Settings-Keyword-Handlers */
KEYWORD (log_destiny) {
  RC_LOG_DESTINY_STRINGS;
  RC_STRING_TABLE_G3OUT (log_destiny);
}

/** @brief function do_kw_console_window in Settings-Keyword-Handlers */
KEYWORD (console_window) {
  RC_BOOLEAN_GOUT (console_window);
}

/** @brief function do_kw_console_window_type in Settings-Keyword-Handlers */
KEYWORD (console_window_type) {
  RC_CONSOLE_WINTYPE_STRINGS
  RC_STRING_TABLE_G2OUT (console_window_type)
}

/** @brief function do_kw_action_feedback_mode in Settings-Keyword-Handlers */
KEYWORD (action_feedback_mode) {
  RC_ACTION_FEEDBACK_STRINGS
  RC_STRING_TABLE_W2OUT (action_feedback_mode)
}

/** @brief function do_kw_add_attribute_offset in Settings-Keyword-Handlers */
KEYWORD (add_attribute_offset) {
  RC_INTEGER_WOUT (add_attribute_offset);
}

/** @brief function do_kw_attribute_placement_grid in Settings-Keyword-Handlers */
KEYWORD (attribute_placement_grid) {
  RC_INTEGER_WOUT (attribute_placement_grid);
}

/** @brief function do_kw_auto_load_last in Settings-Keyword-Handlers */
KEYWORD (auto_load_last) {
  RC_BOOLEAN_GOUT (auto_load_last);
}

/** @brief function do_kw_auto_pan in Settings-Keyword-Handlers */
KEYWORD (auto_pan) {
  RC_BOOLEAN_WOUT (auto_pan);
}

/** @brief function do_kw_auto_pan_step in Settings-Keyword-Handlers */
KEYWORD (auto_pan_step) {
  RC_INTEGER_WOUT (auto_pan_step);
}

/** @brief function do_kw_auto_save_interval in Settings-Keyword-Handlers */
KEYWORD (auto_save_interval) {
  RC_INTEGER_TOUT (auto_save_interval);
}

/** @brief function do_kw_component_dialog_attributes in Settings-Keyword-Handlers */
KEYWORD (component_dialog_attributes) {

  static int list_length;
  bool flushed = FALSE;

  char *ptr_first_char;
  char *ptr;
  int show_all = FALSE;
  int add_default_list = FALSE;
  int index =0;

  void cat_some_attribs ( int start, int stop ) {
    for (index = start; index < stop; index++) {
      strcat ( ptr,  "\"");
      strcat ( ptr,  DefaultFilterList[index]);
      strcat ( ptr,  "\" ");
    }
  }

  if (FilterList != NULL) {
    list_length = g_list_length(FilterList); /* is also used later if filtering */
    if ((list_length == 1) && (strlen( g_list_nth_data(FilterList, 0)) == 1)) {
      add_default_list = TRUE;
      show_all = TRUE;
    }
  }
  else { /* is NULL so filtering is disabled */
    add_default_list = TRUE;
  }

  ptr_first_char = KEY_BUFFER(component_dialog_attributes);
  while (*ptr_first_char == SPACE)  { ++ptr_first_char; } /* next char, nothing else since we were called */

  ptr = ptr_first_char;
  while ((*ptr != ASCII_NUL ) && (*ptr != ASCII_APO )) { ptr++; } /* find apostrophe? */

  if (*ptr == ASCII_APO ) {

    char Terminator[]= LineTerminator;

    ptr++; ptr++;

    if (*ptr == ASCII_CP ) {                     /* if is entry for None */
      if (FilterList == NULL)                     /* and still is None */
        strcpy (ptr_first_char, "(");
      else
        strcpy (ptr_first_char, ";(");
      strcat ( ptr_first_char,  "component-dialog-attributes '())");
      strcat (ptr_first_char, Terminator);
      fputs(ptr_first_char, output);
    }
    else {

      while (*ptr == SPACE)  { ++ptr; }

      ++ptr;

      if (*ptr == '*' ) {                        /* if entry for ALL */
        if (show_all)                             /* and set to All   */
          strcpy (ptr_first_char, "(");
        else
          strcpy (ptr_first_char, ";(");
        strcat ( ptr_first_char,  "component-dialog-attributes '(\"*\"))");
        strcat (ptr_first_char, Terminator);
        fputs(ptr_first_char, output);
      }
      else {

        int po = 0;                  /* counter for parenthesis open */
        int pc = 0;                  /* counter for parenthesis close */

        ptr = ptr_first_char;

        while ( ASCII_NUL != *ptr++) { if (*ptr == ASCII_OP) ++po; } /* count opening parenthesis */
        ptr = ptr_first_char;
        while ( ASCII_NUL != *ptr++) { if (*ptr == ASCII_CP) ++pc; } /* count close parenthesis   */
        while ( po > pc ) {                                          /* while count does not match */
          ptr = fgets(ptr_first_char, RC_INPUT_BUFFER_SIZE, input);  /* read in the next line  */
          while ( ASCII_NUL != *ptr++) {                             /* search the entire string */
            if (*ptr == ASCII_CP) ++pc; }  /* while counting close parenthesis */
        }

        /* old list is gone, write a new one */
        ptr = ptr_first_char;

        if (add_default_list) {
           /* Write "commented-out" default attribute filter list to the RC file */
            strcpy ( ptr, ";(component-dialog-attributes '(");
            cat_some_attribs( 0, 4);
            strcat ( ptr, Terminator);
            fputs(ptr, output);
            strcpy ( ptr, ";                               ");
            cat_some_attribs( 4, 9);
            strcat ( ptr, Terminator);
            fputs(ptr, output);
            strcpy ( ptr, ";                               ");
            cat_some_attribs( 9, 13);
            strcat ( ptr, Terminator);
            fputs(ptr, output);
            strcpy ( ptr, ";                               ");
            cat_some_attribs( 13, 16);
            strcat ( ptr, Terminator);
            fputs(ptr, output);
            strcpy ( ptr, ";                               ");
            cat_some_attribs( 16, 17);
            strcat ( ptr,  "))");
            strcat ( ptr, Terminator);
            fputs(ptr, output);
        }
        else {

          /* Write the active attribute filter list to the RC file */
          if (FilterList != NULL) {

            strcpy ( ptr, "(component-dialog-attributes '(");

            for (index = 0; index < list_length; index++) {

              strcat ( ptr,  "\"");
              strcat ( ptr,  g_list_nth_data(FilterList, index));
              strcat ( ptr,  "\"");          /* don't add space here */

              if (strlen (ptr) > 66 ) {

                if (index == list_length ) { /* if is the end of the list   */
                  strcat ( ptr,  "))");      /* just close all on this line */
                  strcat ( ptr, Terminator);
                  fputs(ptr, output);
                  flushed = TRUE;
                }
                else {
                  strcat (ptr,Terminator);   /* else just add a line feed */
                  fputs(ptr, output);
                  strcpy ( ptr, "                              ");
                }
              }
              strcat ( ptr,  " ");           /* put space between members */
            }

            if ( !flushed ) {                /* if did not close out then */
              strcat ( ptr,  "))");          /* add final parenthesis and */
              strcat ( ptr, Terminator);     /* a new sequence and then   */
              fputs(ptr, output);            /* write out the buffer      */
            }
          }
        }
      }
    }
  }
}

/** @brief function do_kw_continue_component_place in Settings-Keyword-Handlers */
KEYWORD (continue_component_place) {
  RC_BOOLEAN_WOUT (continue_component_place);
}

/** @brief function do_kw_embed_components in Settings-Keyword-Handlers */
KEYWORD (embed_components) {
  RC_BOOLEAN_WOUT (embed_components);
}

/** @brief function do_kw_enforce_hierarchy in Settings-Keyword-Handlers */
KEYWORD (enforce_hierarchy) {
  RC_BOOLEAN_WOUT (enforce_hierarchy);
}

/** @brief function do_kwfile_preview in Settings-Keyword-Handlers */
KEYWORD (file_preview) {
  RC_BOOLEAN_WOUT (file_preview);
}

/** @brief function do_kw_force_boundingbox in Settings-Keyword-Handlers */
KEYWORD (force_boundingbox) {
  RC_BOOLEAN_WOUT (force_boundingbox);
}

/** @brief function do_kw_keyboardpan_gain in Settings-Keyword-Handlers */
KEYWORD (keyboardpan_gain) {
  RC_INTEGER_WOUT (keyboardpan_gain);
}

/** @brief function do_kw_magnetic_net_mode in Settings-Keyword-Handlers */
KEYWORD (magnetic_net_mode) {
  RC_BOOLEAN_WOUT (magnetic_net_mode);
}

/** @brief function do_kw_netconn_rubberband in Settings-Keyword-Handlers */
KEYWORD (netconn_rubberband) {
  RC_BOOLEAN_WOUT (netconn_rubberband);
}

/** @brief function do_kw_raise_dialog_boxes in Settings-Keyword-Handlers */
KEYWORD (raise_dialog_boxes) {
  RC_BOOLEAN_WOUT (raise_dialog_boxes);
}

/** @brief function do_kw_select_slack_pixels in Settings-Keyword-Handlers */
KEYWORD (select_slack_pixels) {
  RC_INTEGER_WOUT (select_slack_pixels);
}

/** @brief function do_kw_snap_size in Settings-Keyword-Handlers */
KEYWORD (snap_size) {
  RC_INTEGER_WOUT (snap_size);
}

/** @brief function do_kw_sort_component_library in Settings-Keyword-Handlers */
KEYWORD (sort_component_library) {
  RC_BOOLEAN_WOUT (sort_component_library);
}

/** @brief function do_kw_untitled_name in Settings-Keyword-Handlers
KEYWORD(untitled_name) {
  RC_STRING_TOUT(untitled_name)
}
  */
/** @brief function do_kw_net_consolidate in Settings-Keyword-Handlers */
KEYWORD (net_consolidate) {
  RC_BOOLEAN_TOUT (net_consolidate);
}

/** @brief function do_kw_net_endpoint_mode in Settings-Keyword-Handlers */
KEYWORD (net_endpoint_mode) {
  RC_NET_MAKER_STRINGS;
  RC_STRING_TABLE_W3OUT (net_endpoint_mode);
}

/** @brief function do_kw_net_midpoint_mode in Settings-Keyword-Handlers */
KEYWORD (net_midpoint_mode) {
  RC_NET_MAKER_STRINGS;
  RC_STRING_TABLE_W3OUT (net_midpoint_mode);
}

/** @brief function do_kw_net_direction_mode in Settings-Keyword-Handlers */
KEYWORD (net_direction_mode) {
  RC_BOOLEAN_WOUT (net_direction_mode);
}

/** @brief function do_kw_net_selection_mode in Settings-Keyword-Handlers */
KEYWORD (net_selection_mode) {
  int net_selection_mode = w_current->net_selection_mode;
  net_selection_mode = net_selection_mode == 0 ? 0 : (net_selection_mode -1);
  RC_NET_SELECTION_STRINGS
  RC_STRING_TABLE_G3OUT (net_selection_mode)
}

/** @brief function do_kw_bus_style in Settings-Keyword-Handlers */
KEYWORD ( bus_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( bus_style )
}
/** @brief function do_kw_net_style in Settings-Keyword-Handlers */
KEYWORD ( net_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( net_style )
}
/** @brief function do_kw_pin_style in Settings-Keyword-Handlers */
KEYWORD ( pin_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( pin_style )
}
/** @brief function do_kw_line_style in Settings-Keyword-Handlers */
KEYWORD ( line_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( line_style )
}
/** @brief function do_kw_thick_bus_width in Settings-Keyword-Handlers */
KEYWORD ( thick_bus_width ) {
  RC_INTEGER_TOUT ( thick_bus_width )
}
/** @brief function do_kw_thick_line_width in Settings-Keyword-Handlers */
KEYWORD ( thick_line_width ) {
  RC_INTEGER_TOUT ( thick_line_width )
}
/** @brief function do_kw_thick_line_width in Settings-Keyword-Handlers */
KEYWORD ( thick_net_width ) {
  RC_INTEGER_TOUT ( thick_net_width )
}
/** @brief function do_kw_thick_pin_width in Settings-Keyword-Handlers */
KEYWORD ( thick_pin_width ) {
  RC_INTEGER_TOUT ( thick_pin_width )
}
/** @brief function do_kw_thick_line_width in Settings-Keyword-Handlers */
KEYWORD ( thin_bus_width ) {
  RC_INTEGER_TOUT ( thin_bus_width )
}
/** @brief function do_kw_thin_line_width in Settings-Keyword-Handlers */
KEYWORD ( thin_line_width ) {
  RC_INTEGER_TOUT ( thin_line_width )
}
/** @brief function do_kw_thin_net_width in Settings-Keyword-Handlers */
KEYWORD ( thin_net_width ) {
  RC_INTEGER_TOUT ( thin_net_width )
}
/** @brief function do_kw_thin_pin_width in Settings-Keyword-Handlers */
KEYWORD ( thin_pin_width ) {
  RC_INTEGER_TOUT ( thin_pin_width )
}

/** @brief function do_kw_bus_ripper_rotation in Settings-Keyword-Handlers */
KEYWORD ( bus_ripper_rotation ) {
  RC_RIPPER_ROTATION_STRINGS
  RC_STRING_TABLE_W2OUT (bus_ripper_rotation)
}

/** @brief function do_kw_bus_ripper_size in Settings-Keyword-Handlers */
KEYWORD ( bus_ripper_size ) {
  RC_INTEGER_WOUT ( bus_ripper_size )
}
/** @brief function do_kw_bus_ripper_type in Settings-Keyword-Handlers */
KEYWORD ( bus_ripper_type ) {
  RC_RIPPER_TYPE_STRINGS
  RC_STRING_TABLE_W2OUT (bus_ripper_type)
}
/** @brief function do_kw_bus_ripper_symname in Settings-Keyword-Handlers */
KEYWORD(bus_ripper_symname) {
  RC_STRING_WOUT(bus_ripper_symname)
}

/** @brief function do_kw_fast_mousepan in Settings-Keyword-Handlers  */
KEYWORD ( fast_mousepan ) {
  RC_BOOLEAN_WOUT (fast_mousepan)
}

/** @brief function do_kw_drag_can_move in Settings-Keyword-Handlers  */
KEYWORD ( drag_can_move ) {
  RC_BOOLEAN_WOUT (drag_can_move)
}

/** @brief function do_kw_middle_button in Settings-Keyword-Handlers */
KEYWORD ( middle_button ) {
  RC_MIDDLE_MOUSE_STRINGS
  RC_STRING_TABLE_W4OUT(middle_button)
}

/** @brief function do_kw_third_button in Settings-Keyword-Handlers */
KEYWORD ( third_button ) {
  RC_3RD_BUTT_STRINGS
  RC_STRING_TABLE_W2OUT (third_button)
}

/** @brief function do_kw_mousepan_gain in Settings-Keyword-Handlers  */
KEYWORD ( mousepan_gain ) {
  RC_INTEGER_WOUT (mousepan_gain)
}

/** @brief function do_kw_scroll_wheel in Settings-Keyword-Handlers */
KEYWORD ( scroll_wheel ) {
  RC_SCROLL_STRINGS
  RC_STRING_TABLE_W2OUT (scroll_wheel)
}

/** @brief function do_kw_image_color in Settings-Keyword-Handlers */
KEYWORD (image_color) {
  RC_BOOLEAN_TOUT (image_color)
}
/** @brief function do_kw_invert_images in Settings-Keyword-Handlers */
KEYWORD (invert_images) {
  RC_BOOLEAN_TOUT (invert_images)
}
/** @brief function do_kw_text_case in Settings-Keyword-Handlers */
KEYWORD ( text_case ) {
  RC_TEXT_CASE_STRINGS
  RC_STRING_TABLE_W3OUT (text_case)
}

/** @brief function do_kw_text_display_zoomfactor in Settings-Keyword-Handlers */
KEYWORD ( text_display_zoomfactor ) {
  RC_INTEGER_WOUT (text_display_zoomfactor)
}

/** @brief function do_kw_text_feedback in Settings-Keyword-Handlers */
KEYWORD ( text_feedback ) {
  RC_TXT_FEEDBACK_STRINGS
  RC_STRING_TABLE_W2OUT (text_feedback)
}

/** @brief function do_kw_text_origin_marker in Settings-Keyword-Handlers */
KEYWORD ( text_origin_marker ) {
  RC_BOOLEAN_ROUT (text_origin_marker)
}
/** @brief function do_kw_text_marker_size in Settings-Keyword-Handlers */
KEYWORD ( text_marker_size ) {
  RC_INTEGER_ROUT (text_marker_size)
}
/** @brief function do_kw_text_marker_threshold in Settings-Keyword-Handlers */
KEYWORD ( text_marker_threshold ) {

  char s_val[4];

  int number = CairoRenderer->text_marker_threshold * 10;

  strcpy(output_buffer, "(" );
  strcat(output_buffer, KEY_NAME(text_marker_threshold));
  strcat(output_buffer, " ");
  strcat(output_buffer, geda_utility_string_int2str( number, s_val, 10 ));
  strcat(output_buffer, ")\n");
  fputs(output_buffer, output);
}
/** @brief function do_kw_text_size in Settings-Keyword-Handlers */
KEYWORD ( text_size ) {
  RC_INTEGER_WOUT (text_size)
}

/** @brief function do_kw_undo_control in Settings-Keyword-Handlers */
KEYWORD ( undo_control ) {
  RC_BOOLEAN_WOUT (undo_control)
}

/** @brief function do_kw_undo_levels in Settings-Keyword-Handlers */
KEYWORD ( undo_levels ) {
  RC_INTEGER_WOUT (undo_levels)
}

/** @brief function do_kw_undo_type in Settings-Keyword-Handlers */
KEYWORD ( undo_type ) {
  RC_UNDO_TYPE_STRINGS
  RC_STRING_TABLE_W2OUT (undo_type)
}

/** @brief function do_kw_undo_panzoom in Settings-Keyword-Handlers */
KEYWORD ( undo_panzoom ) {
  RC_BOOLEAN_WOUT (undo_panzoom)
}

/** @brief function do_kw_undo_preserve in Settings-Keyword-Handlers */
KEYWORD ( undo_preserve ) {
  RC_BOOLEAN_WOUT (undo_preserve)
}

/** @brief function do_kw_attribute_name in Settings-Keyword-Handlers */
KEYWORD ( attribute_name ) {

  int count;
  int i;
  char *ptrBuffer;

  count = geda_struct_attrib_count();

  ptrBuffer = KEY_BUFFER(attribute_name);

  for (i = 0; i < count; i++) {

    /* set pointer to beginning of ptrBuffer if sucessful */
    char *ptr = fgets(ptrBuffer, RC_INPUT_BUFFER_SIZE, input);   /* read in the next line  */

    while ((*ptr != ASCII_NUL ) && (*ptr != ASCII_OP )) { ptr++; } ptr++;

    if (!strncmp (ptr, KEY_NAME(attribute_name), strlen(KEY_NAME(attribute_name))))  /* see if match our keyword */
      break; /* should not do this as long as count in RC file = count in memory */
  }
  for (i = 0; i < count; i++) {
    RC_STRING_GOUT(attribute_name, geda_struct_attrib_get(i))
  }
}

/** @} endgroup Settings-Keyword-Handlers */
/** @} endgroup Settings-Auxillary-Module */

#undef RC_INPUT_BUFFER_SIZE
#undef FilterList
#undef KEY_FUNC
#undef KEY_BUFFER
#undef KEY_NAME
#undef KEY_INTEGER
#undef KEYWORD

/*********** End of Configure Settings dialog box *******/











