/* C
;  File: x_settings.c
;;
;;; gEDA - GPL Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2012 Ales Hvezda
;;; Copyright (C) 1998-2012 gEDA Contributors (see ChangeLog for details)
;;
;;; Copyright (C) 2012 Wiley Edward Hill <wileyhill@gmail.com>
;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;
;;; Date: Aug, 17, 2012
;;; Contributing Author: Wiley Edward Hill
;;
*/
/************************ REVISION HISTORY *************************
;; Who |   When   |  What (Why)
;; ------------------------------------------------------------------
;; WEH | 09/17/12 |  Initial release.
;; ------------------------------------------------------------------
;; WEH | 12/02/12 |  Added function x_settings_set_scm_int, revised
;;                |  generate_rc to use the template rc file and to
;;                |  write to the user's home directory. (This resolves
;;                |  implementation issues on Linux system that were due
;;                |  to permissions associated with /etc/geda.)
;; ------------------------------------------------------------------
;; WEH | 12/04/12 |  Added function x_settings_set_scm_int, revised
;; ------------------------------------------------------------------
*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#define USE_POSIX

#include <stdio.h>
//#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ascii.h>

#include <gtk/gtk.h>

#include <sys/types.h>
#include <dirent.h>

#include "gschem.h"
#include "gschem_dialog.h"

#include "x_settings.h"
#include "keywords.h"

#ifdef __GNUC__
  #define LineTerminator { LINE_FEED, ASCII_NUL};
#else
  #define LineTerminator { CARRIAGE_RETURN, LINE_FEED, ASCII_NUL};
#endif

#define FilterList w_current->component_select_attrlist

#define KEYWORD(symbol, narg, sarg, func) [ kw_##func ] = { #symbol, do_kw_##func, narg, sarg },

char output_buffer[255];
extern const char* DefaultFilterList[];

struct {
   const char *name;
   void (*func) (GSCHEM_TOPLEVEL *w_current, FILE* input, FILE* output);
   unsigned int narg;
   char *strBuffer;
} keyword_struc[KEYWORD_COUNT] = {
 [ kw_unknown ] = { "unknown", 0, 0, 0 },
#include "keywords.h"
};
#define KEY_FUNC(symbol)keyword_struc[kw_##symbol].func
#define KEY_BUFFER(symbol)keyword_struc[kw_##symbol].strBuffer
#define KEY_NAME(symbol)keyword_struc[kw_##symbol].name
#define KEY_INTEGER(symbol)keyword_struc[kw_##symbol].narg

#define kw_integer(var)keyword_struc[var].narg

/*! \brief Response function for the configure_settings dialog
 *  \par Function Description
 *  This function destroys the configure_settings dialog.
 */
void configure_dialog_response(GtkWidget *w, gint response,
			       GSCHEM_TOPLEVEL *w_current)
{
  switch (response) {
  case GTK_RESPONSE_APPLY:
    GatherSettings (w_current);
    generate_rc(w_current, "gschemrc");
    break;
  case GTK_RESPONSE_OK:
    GatherSettings (w_current);
  case GTK_RESPONSE_CANCEL:
    /* void */
    break;
  default:
    printf("configure_dialog_response(): strange signal %d\n",response);
  }

  gtk_widget_destroy(w_current->stwindow);
  w_current->stwindow = NULL;

}
/* ----------------- Start Attribute TAB Support Functions ------------------ */

/* \defgroup X_Settings_Attribute Read and Write RC File
 *  @{
 */

/*              Bulb */
GtkWidget*
get_geda_bulb_image (gboolean WhichState)
{
   GtkWidget* image;

   if (WhichState)
     image = create_pixmap (BULB_ON_IMAGE );
   else
     image = create_pixmap ( BULB_OFF_IMAGE);

   return image;
}
void bulb_on( GtkWidget *widget) {

  GList* button   =gtk_container_get_children (GTK_CONTAINER(widget));
  GList* align    =gtk_container_get_children (GTK_CONTAINER (button->data));
  GList* lightbox =gtk_container_get_children (GTK_CONTAINER (align->data));

  GtkWidget* BulbOnImage = lightbox->data;
  lightbox = lightbox->next;
  GtkWidget* BulbOffImage = lightbox->data;

  gtk_widget_set_visible (BulbOnImage, TRUE);
  gtk_widget_set_visible (BulbOffImage, FALSE);
}

void bulb_off( GtkWidget *widget) {

  GList* button   =gtk_container_get_children (GTK_CONTAINER(widget));
  GList* align    =gtk_container_get_children (GTK_CONTAINER (button->data));
  GList* lightbox =gtk_container_get_children (GTK_CONTAINER (align->data));

  GtkWidget* BulbOnImage = lightbox->data;
  lightbox = lightbox->next;
  GtkWidget* BulbOffImage = lightbox->data;

  gtk_widget_set_visible (BulbOnImage, FALSE);
  gtk_widget_set_visible (BulbOffImage, TRUE);
}

void gtk_bulb_group_set_active(GSList *RadioGroupList, int value)
{
  GtkToggleButton *button;
  int length;
  int index;
  int pos = GPOINTER_TO_UINT(value);
  int j;

  /* Get number of buttons in group */
  length = g_slist_length (RadioGroupList);

  /* new buttons are *prepended* to the list, so buttons added as
   * first have last position in the list and using glist reverse
   * confuses gtk */
  index = (length - 1) - pos;

  if (index < 0 || index >= length) {

     return; 
  } /* should not to happen */

  for (j = 0; j < length; j++) {
     button = GTK_TOGGLE_BUTTON (g_slist_nth_data (RadioGroupList, j));
     if (button == NULL) return;
     if ( j == index) {
       if (gtk_toggle_button_get_active (button) == FALSE) {
           gtk_toggle_button_set_active (button, TRUE);
       }
           bulb_on(GTK_WIDGET(button));
     }else bulb_off(GTK_WIDGET(button));
  }    

  return;
}

/*! \brief Create the configure_settings dialog and activates it
 *  \par Function Description
 *  This function is call to creates the configure_settings dialog.
 */
void x_configure_settings (GSCHEM_TOPLEVEL* w_current)
{

  w_current->stwindow = create_settings_dialog( w_current);

  gtk_window_set_destroy_with_parent (GTK_WINDOW(w_current->stwindow), TRUE);

  gtk_window_position (GTK_WINDOW (w_current->stwindow), GTK_WIN_POS_MOUSE);

  load_settings_dialog (w_current);

  g_signal_connect (GTK_OBJECT (w_current->stwindow), "response",
                    GTK_SIGNAL_FUNC(configure_dialog_response), w_current);
/*
  g_signal_connect (GTK_OBJECT (w_current->stwindow), "destroy",
                    GTK_SIGNAL_FUNC(destroy_all_radios), w_current);
*/
  gtk_container_border_width (GTK_CONTAINER(w_current->stwindow),
                                DIALOG_BORDER_SPACING);

  gtk_widget_show(w_current->stwindow);

  return;

}

/** @brief function change_default_titleblock in GatherSettings */
bool x_settings_set_scm_int(char *symbol_name, int value ) {
  
  char s_val[5];
  char buffer[128];
  char *str;
  char *strbuff;

  if (symbol_name) {
    strbuff = &buffer[0];
    str = int2str(value, s_val, 10); /* convert the integer to a string */
    strcpy(strbuff, "(define ");
    strcat(strbuff, symbol_name );
    strcat(strbuff, " " );
    strcat(strbuff, str );
    strcat(strbuff, ")" );
    g_scm_c_eval_string_protected(strbuff);
  }
  else
    return FALSE;
  return TRUE;
}

/* ----------------- Start Attribute TAB Support Functions ------------------ */

/* \defgroup X_Settings_Attribute Attribute Settings File Support Functions
 *  @{
 */

/*! \brief Returns count files in title-block directory.
 *  \par Function Description
 *  This functions returns the numbers files located in the title-block
 *  folder. This is used to determine how char array to allocate.
 *  
 * \retval Returns integer count or -1 if the folder is inaccessible.
*/
int get_titleblock_cnt(void) {

  int count=0;
  char TitleBlockPath[MAX_PATH];
  
  DIR *dirp;
  struct dirent *ent;

  strcpy (TitleBlockPath, s_path_sys_data());
  strcat (TitleBlockPath, TITLE_BLOCK_PATH);

  dirp = opendir (TitleBlockPath);
  if (dirp != NULL)
  {
     /* get all the files within directory */
     while ((ent = readdir (dirp)) != NULL) {
       if (strcmp (get_filename_ext(ent->d_name), SYMBOL_FILE_SUFFIX) == 0){ count++;}
     }
     closedir (dirp);
  } else
  { /* could not open directory */
     s_log_message("get_titleblock_cnt: error opening: %s\n", TitleBlockPath);
     count--; /* decement to -1 */
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

  int  i, index =0;
  char TitleBlockPath[MAX_PATH];
  bool result = TRUE;
  int  namelen;

  char tmpbuff[MAX_FILENAME];

  DIR *dirp;
  struct dirent *ent;

  strcpy (TitleBlockPath, s_path_sys_data());
  strcat (TitleBlockPath, TITLE_BLOCK_PATH);

  dirp = opendir (TitleBlockPath);
  if (dirp != NULL)
  {
     /* get all the files within directory */
     while ((ent = readdir (dirp)) != NULL)
     {
       if (strcmp (get_filename_ext(ent->d_name), SYMBOL_FILE_SUFFIX) == 0)
       {
          strcpy(tmpbuff, basename(ent->d_name));
          namelen = strlen( tmpbuff) - 4; /* substract the extension */
          for (i = namelen; i < MAX_PATH - namelen; i++) {
               tmpbuff[i] = '\0';
          }
          strcpy (Buffer[index++], tmpbuff);
       }
     }
     closedir (dirp);
  } else
  { /* could not open directory */
      s_log_message("get_titleblock_list: error opening: %s\n", TitleBlockPath);
      result = FALSE;
  }
  return result;
}

/** @} END Group X_Settings_Attribute */

/* --------------------- Begin Read and Write RC File -------------------- */

/* \defgroup X_Settings_Read_Write Read and Write RC File
 *  @{
 */
/** @brief function process_rc_buffer in X_Settings_Read_Write
 * \par Function Description
 *      Pre-parser to check input line read in from template rc file. This
 *      function performs an inital interrogation of lines read in from a
 *      file. If a keyword format is detected then the string is copied to
 *      the supplied address and returns SUBSTITUTE. Otherwise LINE_FEED
 *      is returned to indicate the line should be written back out.
 */

static int process_rc_buffer(char *strbuffer, char *keyword) {

  char *s_ptr;
  char *e_ptr;

  int token_len;
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
    if (( *s_ptr == ';') && ( *e_ptr == '(')) {                  /* if commented keyword advance start ptr */
      s_ptr = ++e_ptr;
    }
    else if ( *s_ptr == '(') {                               /* else advance start ptr to 1st char of word */
      s_ptr++;
    }
    /* advance end ptr to end of keyword */
    while (( *e_ptr != SPACE) && ( *e_ptr != ASCII_CR) && ( *e_ptr != ASCII_NUL)) { ++e_ptr; }
    token_len = e_ptr - s_ptr;

    strncpy(keyword, s_ptr, token_len);   /* extract the key-word string from the buffer */
    keyword[token_len ] = ASCII_NUL;      /* add terminator */
    keyword[token_len + 1] = ASCII_NUL;   /* add another terminator */

    return SUBSTITUTE;
  } /* end if begin active or commented keyword */

  return LINE_FEED;

}

/** @brief function generate_rc in X_Settings_Read_Write */
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
#define RC_INPUT_BUFFER_SIZE 256
int generate_rc(GSCHEM_TOPLEVEL *w_current, const char *rcname)
{
  char *inputfile;			/* Name of the input file */
  char *templatefile;                   /* Name of the Template file */
  char *outputfile;			/* Name of the output file */

  FILE* input;				/* Input file handle */
  FILE* output;				/* Output file handle */

  char keyword[MAX_KEYWORD];		/* Buffer containing potential keyword */
  char strbuffer[RC_INPUT_BUFFER_SIZE];	/* Read Buffer */

  int lc;				/* Line counter */
  int j;				/* Index for enumerated keywords */
  int khandle;				/* Index of handler for found keyword */
  int last=0;				/* Index of the handler called previously */
  int result;				/* Our exit code */

  /* Build path for user config file */
  inputfile = g_strconcat (s_path_user_config (), G_DIR_SEPARATOR_S, rcname, NULL);
  
  /* Check for existence of user config file */
  if(access(inputfile, R_OK) != 0) {
    /* Copy the template user config file to user's folder */
    templatefile = g_strconcat (s_path_sys_config (), G_DIR_SEPARATOR_S,
                             "user-", rcname, ".scm", NULL);
    result = fcopy(templatefile, inputfile);
  }
  
  if (inputfile == NULL) {
    s_log_message("File Name error! system-%s", rcname);
    return -1;
  }

  outputfile = g_strconcat (s_path_user_config (), G_DIR_SEPARATOR_S,
                            rcname, ".tmp", NULL);

  s_log_message("Writing configuration to [%s]", outputfile);
  
  if (( input = fopen (inputfile, "r" )) == NULL) {
    s_log_message("File open for read-only error: \"%s\", %s\n", inputfile, strerror( errno ));
    result = errno;
  }
  else 
    if (( output = fopen (outputfile, "w" )) == NULL)
    {
      s_log_message("Error, opening output \"%s\", %s\n", inputfile, strerror( errno ));
      fclose(input);
      result = -1;
    }
    else
    {
      lc = 1;
      while (fgets(strbuffer, sizeof(strbuffer), input)) {
        if ((result = (process_rc_buffer (strbuffer, keyword)) == LINE_FEED)) {
          fputs(strbuffer, output);
        }
        else { /* found a keyword */
          khandle = kw_unknown;
          for (j = 0; j < KEYWORD_COUNT; j++) {               /* for all our keywords */
            if (!strcmp (keyword, keyword_struc[j].name)) {   /* see if match input keyword */
              khandle = j;
              break;
            }
          }     /* next j */
          if(khandle) {
            if ( (khandle != last) || (kw_integer(khandle) != 0) )
            { /* if was not the same as the last one read  */
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
    if ((result = remove(inputfile)) == 0)
      result = rename(outputfile, inputfile);
    else {
      s_log_message("File error: \"%s\", %s\n", inputfile, strerror( errno ));
      result = errno;
    }
  }
  return result;
}

/** @brief function is_enabled in X_Settings_Read_Write
 *  \par Function Description
 *       Utility function used by keyword handlers to check for an
 *       initial semicolon. Returns TRUE is a semicolon is the first
 *       character, ignoring spaces.
 */
static bool is_enabled(const char* ptr) {
  /* Check if this entry is commented out */
  while ( *ptr == SPACE)  { ++ptr; }
  if ( *ptr == ASCII_OP)
    return TRUE;
  return FALSE;
}
/** @} END Group X_Settings_Read_Write */

#define KEYWORD(func) void do_kw_##func(GSCHEM_TOPLEVEL *w_current, FILE* input, FILE* output)

/* --------------------- Begin Keyword Handler Functions ------------------- */

/* \defgroup X_Settings_Keyword_Handlers Read and Write RC File Keywords
 *  @{
 */

/*!\WARNING: { Do not point to another KEY_BUFFER unless you know what your doing }*/

/** @brief function do_kw_load_in_rc in X_Settings_Keyword_Handlers */
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
  if (strstr(ptr, DARK_COLOR_MAP ) != NULL)
    fix(0);
  else
    if (strstr(ptr, LIGHT_COLOR_MAP ) != NULL)
      fix(1);
    else
      if (strstr(ptr, BW_COLOR_MAP ) != NULL)
        fix(2);
      else
        if (strstr(ptr, CUSTOM_COLOR_MAP ) != NULL)
          fix(3);
        /* else do nothing, but could check for other "loads" */

  if (cat_flag)                  /* is flag set? */
    strcat (output_buffer, ptr); /* copy after the added semi-colon */
  else
    strcpy (output_buffer, ptr); /* copy to input to output */

  fputs(output_buffer, output);  /* write line to output file*/
}
#undef map_index

/** @brief function do_kw_define_in_rc in X_Settings_Keyword_Handlers */
KEYWORD (define_in_rc) {

  char Terminator[]= LineTerminator;
  char *ptr;
  ptr = KEY_BUFFER(load_in_rc);     /* get our pointer to the input buffer */
  strcpy (output_buffer, ptr );     /* Assume that were are not changing   */

  if (rc_options.titleblock_index == -1) {
    if (strstr(ptr, "define default-titleblock" ) != NULL) {
      /* could do this: ptr = scm_2_cstring( "default-titleblock" ) */
      strcpy (output_buffer, "(define default-titleblock \""); /* add a new semi-colon*/
      strcat (output_buffer, rc_options.titleblock_fname );
      strcat (output_buffer, "\")" );
      strcat (output_buffer, Terminator );
    }
  }
  fputs(output_buffer, output);  /* write line to output file*/
}

/** @brief function do_kw_draw_grips in X_Settings_Keyword_Handlers */
KEYWORD (draw_grips) {
  RC_BOOLEAN_WOUT (draw_grips);
}

/** @brief function do_kw_logging in X_Settings_Keyword_Handlers */
KEYWORD (logging) {
  RC_BOOLEAN_GOUT (logging);
}

/** @brief function do_kw_grid_mode in X_Settings_Keyword_Handlers */
KEYWORD (grid_mode) {
  RC_GRID_MODE_STRINGS;
  RC_STRING_TABLE_W3OUT (grid_mode);
}

/** @brief function do_kw_dots_grid_dot_size in X_Settings_Keyword_Handlers */
KEYWORD (dots_grid_dot_size) {
  RC_INTEGER_TRIAD_WOUT (dots_grid_dot_size, 1, 2, 3);
}

/** @brief function do_kw_dots_grid_fixed_threshold in X_Settings_Keyword_Handlers */
KEYWORD (dots_grid_fixed_threshold) {
  RC_INTEGER_WOUT (dots_grid_fixed_threshold);
}

/** @brief function do_kw_dots_grid_mode in X_Settings_Keyword_Handlers */
KEYWORD (dots_grid_mode) {
  RC_DOTS_GRID_MODE_STRINGS
  RC_STRING_TABLE_W2OUT (dots_grid_mode)
}

/** @brief function do_kw_mesh_grid_threshold in X_Settings_Keyword_Handlers */
KEYWORD (mesh_grid_threshold) {
  RC_INTEGER_WOUT (mesh_grid_threshold);
}

/** @brief function do_kw_object_clipping in X_Settings_Keyword_Handlers */
KEYWORD (object_clipping) {
  RC_BOOLEAN_TOUT (object_clipping);
}

/** @brief function do_kw_scrollbars in X_Settings_Keyword_Handlers */
KEYWORD (scrollbars) {
  RC_BOOLEAN_WOUT (scrollbars);
}

/** @brief function do_kw_scrollbar_update in X_Settings_Keyword_Handlers */
KEYWORD (scrollbar_update) {
  RC_BARS_UPDATE_STRINGS
  RC_STRING_TABLE_W2OUT (scrollbar_update)
}

/** @brief function do_kw_scrollpan_steps in X_Settings_Keyword_Handlers */
KEYWORD (scrollpan_steps) {
  RC_INTEGER_WOUT (scrollpan_steps);
}

/** @brief function do_kw_warp_cursor in X_Settings_Keyword_Handlers */
KEYWORD (warp_cursor) {
  RC_BOOLEAN_WOUT (warp_cursor);
}

/** @brief function do_kw_window_size in X_Settings_Keyword_Handlers */
KEYWORD (window_size) {
  char *strings[] = {RC_STR_WINDOW_W650H487, RC_STR_WINDOW_W900H650,
                     RC_STR_WINDOW_W950H712, RC_STR_WINDOW_W1100H825};
  int i;

  if(rc_options.custom_window_size == 1)
     fputs(KEY_BUFFER(window_size), output);
  else {
    for( i = 0; i < 4; i++) {
      strcpy(output_buffer, (rc_options.window_size == i) ? "(" : ";(");
      strcat(output_buffer, strings[i]);
      strcat(output_buffer, "\n");
      fputs(output_buffer, output);
    }
  }
}

/** @brief function do_kw_world_size in X_Settings_Keyword_Handlers */
KEYWORD (world_size) {
  RC_WORLD_SIZE_STRINGS
  RC_STRING_TABLE_NQ_ROUT (world_size);
}

/** @brief function do_kw_zoom_gain in X_Settings_Keyword_Handlers */
KEYWORD (zoom_gain) {
  RC_INTEGER_WOUT (zoom_gain);
}

/** @brief function do_kw_zoom_with_pan in X_Settings_Keyword_Handlers */
KEYWORD (zoom_with_pan) {
  RC_BOOLEAN_WOUT (zoom_with_pan);
}

/** @brief function do_kw_log_destiny in X_Settings_Keyword_Handlers */
KEYWORD (log_destiny) {
  RC_LOG_DESTINY_STRINGS;
  RC_STRING_TABLE_G3OUT (log_destiny);
}

/** @brief function do_kw_log_window in X_Settings_Keyword_Handlers */
KEYWORD (log_window) {
  RC_BOOLEAN_GOUT (log_window);
}

/** @brief function do_kw_log_window_type in X_Settings_Keyword_Handlers */
KEYWORD (log_window_type) {
  RC_LOG_WINTYPE_STRINGS
  RC_STRING_TABLE_G2OUT (log_window_type)
}

/** @brief function do_kw_action_feedback_mode in X_Settings_Keyword_Handlers */
KEYWORD (action_feedback_mode) {
  RC_ACTION_FEEDBACK_STRINGS
  RC_STRING_TABLE_W2OUT (action_feedback_mode)
}

/** @brief function do_kw_add_attribute_offset in X_Settings_Keyword_Handlers */
KEYWORD (add_attribute_offset) {
  RC_INTEGER_WOUT (add_attribute_offset);
}

/** @brief function do_kw_auto_load_last in X_Settings_Keyword_Handlers */
KEYWORD (auto_load_last) {
  RC_BOOLEAN_GOUT (auto_load_last);
}

/** @brief function do_kw_auto_save_interval in X_Settings_Keyword_Handlers */
KEYWORD (auto_save_interval) {
  RC_INTEGER_TOUT (auto_save_interval);
}

/** @brief function do_kw_attribute_placement_grid in X_Settings_Keyword_Handlers */
KEYWORD (attribute_placement_grid) {
  RC_INTEGER_WOUT (attribute_placement_grid);
}

/** @brief function do_kw_component_dialog_attributes in X_Settings_Keyword_Handlers */
KEYWORD (component_dialog_attributes) {

  static int list_length;
  bool flushed = FALSE;

  char Terminator[]= LineTerminator;

  char *ptr_first_char;
  char *ptr;
  int show_all = FALSE;
  int add_default_list = FALSE;
  int index =0;
  int po =0;                  /* counter for parenthesis open */
  int pc =0;                  /* counter for parenthesis close */

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
  } else { /* is NULL so filtering is disabled */
    add_default_list = TRUE;
  }

  ptr_first_char = KEY_BUFFER(component_dialog_attributes);
  while ( *ptr_first_char == SPACE)  { ++ptr_first_char; } // next char, nothing else since we were called

  ptr = ptr_first_char;
  while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_APO )) { ptr++; } // find apostrophe?

  if ( *ptr == ASCII_APO ) {
    ptr++; ptr++;
    if ( *ptr == ASCII_CP ) {                     /* if is entry for None */
      if (FilterList == NULL)                     /* and still is None */
        strcpy (ptr_first_char, "(");
      else
        strcpy (ptr_first_char, ";(");
      strcat ( ptr_first_char,  "component-dialog-attributes '())");
      strcat (ptr_first_char, Terminator);
      fputs(ptr_first_char, output);
    }
    else {
      while ( *ptr == SPACE)  { ++ptr; }
      ++ptr;
      if ( *ptr == '*' ) {                        /* if entry for ALL */
        if (show_all)                             /* and set to All   */
          strcpy (ptr_first_char, "(");
        else
          strcpy (ptr_first_char, ";(");
        strcat ( ptr_first_char,  "component-dialog-attributes '(\"*\"))");
        strcat (ptr_first_char, Terminator);
        fputs(ptr_first_char, output);
      }
      else {
        ptr = ptr_first_char;
        while ( ASCII_NUL != *ptr++) { if ( *ptr == ASCII_OP) ++po; } /* count opening parenthesis */
        ptr = ptr_first_char;
        while ( ASCII_NUL != *ptr++) { if ( *ptr == ASCII_CP) ++pc; } /* count close parenthesis   */
        while ( po > pc ) {                                          /* while count does not match */
          fgets(ptr_first_char, RC_INPUT_BUFFER_SIZE, input);       /* read in the next line  */
          ptr = ptr_first_char;                                    /* set point to beginning  */
          while ( ASCII_NUL != *ptr++) {                          /* search the entire string */
            if ( *ptr == ASCII_CP) ++pc; }  /* while counting close parenthesis */
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

/** @brief function do_kw_continue_component_place in X_Settings_Keyword_Handlers */
KEYWORD (continue_component_place) {
  RC_BOOLEAN_WOUT (continue_component_place);
}

/** @brief function do_kw_embed_components in X_Settings_Keyword_Handlers */
KEYWORD (embed_components) {
  RC_BOOLEAN_WOUT (embed_components);
}

/** @brief function do_kw_enforce_hierarchy in X_Settings_Keyword_Handlers */
KEYWORD (enforce_hierarchy) {
  RC_BOOLEAN_WOUT (enforce_hierarchy);
}

/** @brief function do_kwfile_preview in X_Settings_Keyword_Handlers */
KEYWORD (file_preview) {
  RC_BOOLEAN_WOUT (file_preview);
}

/** @brief function do_kw_force_boundingbox in X_Settings_Keyword_Handlers */
KEYWORD (force_boundingbox) {
  RC_BOOLEAN_TOUT (force_boundingbox);
}

/** @brief function do_kw_keyboardpan_gain in X_Settings_Keyword_Handlers */
KEYWORD (keyboardpan_gain) {
  RC_INTEGER_WOUT (keyboardpan_gain);
}

/** @brief function do_kw_magnetic_net_mode in X_Settings_Keyword_Handlers */
KEYWORD (magnetic_net_mode) {
  RC_BOOLEAN_WOUT (magnetic_net_mode);
}

/** @brief function do_kw_netconn_rubberband in X_Settings_Keyword_Handlers */
KEYWORD (netconn_rubberband) {
  RC_BOOLEAN_WOUT (netconn_rubberband);
}

/** @brief function do_kw_raise_dialog_boxes in X_Settings_Keyword_Handlers */
KEYWORD (raise_dialog_boxes) {
  RC_BOOLEAN_WOUT (raise_dialog_boxes);
}

/** @brief function do_kw_select_slack_pixels in X_Settings_Keyword_Handlers */
KEYWORD (select_slack_pixels) {
  RC_INTEGER_WOUT (select_slack_pixels);
}

/** @brief function do_kw_snap_size in X_Settings_Keyword_Handlers */
KEYWORD (snap_size) {
  RC_INTEGER_WOUT (snap_size);
}

/** @brief function do_kw_sort_component_library in X_Settings_Keyword_Handlers */
KEYWORD (sort_component_library) {
  RC_BOOLEAN_WOUT (sort_component_library);
}

/** @brief function do_kw_untitled_name in X_Settings_Keyword_Handlers */
KEYWORD(untitled_name) {
  RC_STRING_TOUT(untitled_name)
}

/** @brief function do_kw_net_consolidate in X_Settings_Keyword_Handlers */
KEYWORD (net_consolidate) {
  RC_BOOLEAN_TOUT (net_consolidate);
}

/** @brief function do_kw_net_endpoint_mode in X_Settings_Keyword_Handlers */
KEYWORD (net_endpoint_mode) {
  RC_NET_MAKER_STRINGS;
  RC_STRING_TABLE_W3OUT (net_endpoint_mode);
}

/** @brief function do_kw_net_midpoint_mode in X_Settings_Keyword_Handlers */
KEYWORD (net_midpoint_mode) {
  RC_NET_MAKER_STRINGS;
  RC_STRING_TABLE_W3OUT (net_midpoint_mode);
}

/** @brief function do_kw_net_direction_mode in X_Settings_Keyword_Handlers */
KEYWORD (net_direction_mode) {
  RC_BOOLEAN_WOUT (net_direction_mode);
}

/** @brief function do_kw_net_selection_mode in X_Settings_Keyword_Handlers */
KEYWORD (net_selection_mode) {
  int net_selection_mode = w_current->net_selection_mode;
  net_selection_mode = net_selection_mode == 0 ? 0 : (net_selection_mode -1);
  RC_NET_SELECTION_STRINGS
  RC_STRING_TABLE_G3OUT (net_selection_mode)
}

/** @brief function do_kw_bus_style in X_Settings_Keyword_Handlers */
KEYWORD ( bus_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( bus_style )
}
/** @brief function do_kw_net_style in X_Settings_Keyword_Handlers */
KEYWORD ( net_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( net_style )
}
/** @brief function do_kw_pin_style in X_Settings_Keyword_Handlers */
KEYWORD ( pin_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( pin_style )
}
/** @brief function do_kw_line_style in X_Settings_Keyword_Handlers */
KEYWORD ( line_style ) {
  RC_STYLES_STRINGS
  RC_STRING_TABLE_T3OUT ( line_style )
}
/** @brief function do_kw_thick_bus_width in X_Settings_Keyword_Handlers */
KEYWORD ( thick_bus_width ) {
  RC_INTEGER_TOUT ( thick_bus_width )
}
/** @brief function do_kw_thick_line_width in X_Settings_Keyword_Handlers */
KEYWORD ( thick_line_width ) {
  RC_INTEGER_TOUT ( thick_line_width )
}
/** @brief function do_kw_thick_line_width in X_Settings_Keyword_Handlers */
KEYWORD ( thick_net_width ) {
  RC_INTEGER_TOUT ( thick_net_width )
}
/** @brief function do_kw_thick_pin_width in X_Settings_Keyword_Handlers */
KEYWORD ( thick_pin_width ) {
  RC_INTEGER_TOUT ( thick_pin_width )
}
/** @brief function do_kw_thick_line_width in X_Settings_Keyword_Handlers */
KEYWORD ( thin_bus_width ) {
  RC_INTEGER_TOUT ( thin_bus_width )
}
/** @brief function do_kw_thin_line_width in X_Settings_Keyword_Handlers */
KEYWORD ( thin_line_width ) {
  RC_INTEGER_TOUT ( thin_line_width )
}
/** @brief function do_kw_thin_net_width in X_Settings_Keyword_Handlers */
KEYWORD ( thin_net_width ) {
  RC_INTEGER_TOUT ( thin_net_width )
}
/** @brief function do_kw_thin_pin_width in X_Settings_Keyword_Handlers */
KEYWORD ( thin_pin_width ) {
  RC_INTEGER_TOUT ( thin_pin_width )
}

/** @brief function do_kw_bus_ripper_rotation in X_Settings_Keyword_Handlers */
KEYWORD ( bus_ripper_rotation ) {
 return;
}

/** @brief function do_kw_bus_ripper_size in X_Settings_Keyword_Handlers */
KEYWORD ( bus_ripper_size ) {
 return;
}
/** @brief function do_kw_bus_ripper_type in X_Settings_Keyword_Handlers */
KEYWORD ( bus_ripper_type ) {
 return;
}

/** @brief function do_kw_fast_mousepan in X_Settings_Keyword_Handlers */
KEYWORD ( fast_mousepan ) {
  RC_BOOLEAN_WOUT (fast_mousepan)
}

/** @brief function do_kw_drag_can_move in X_Settings_Keyword_Handlers */
KEYWORD ( drag_can_move ) {
  RC_BOOLEAN_WOUT (drag_can_move)
}

/** @brief function do_kw_middle_button in X_Settings_Keyword_Handlers */
KEYWORD ( middle_button ) {
  RC_MIDDLE_MOUSE_STRINGS
  RC_STRING_TABLE_W4OUT(middle_button)
}

/** @brief function do_kw_third_button in X_Settings_Keyword_Handlers */
KEYWORD ( third_button ) {
  RC_3RD_BUTT_STRINGS
  RC_STRING_TABLE_W2OUT (third_button)
}

/** @brief function do_kw_mousepan_gain in X_Settings_Keyword_Handlers */
KEYWORD ( mousepan_gain ) {
  RC_INTEGER_WOUT (mousepan_gain)
}

/** @brief function do_kw_scroll_wheel in X_Settings_Keyword_Handlers */
KEYWORD ( scroll_wheel ) {
  RC_SCROLL_STRINGS
  RC_STRING_TABLE_W2OUT (scroll_wheel)
}

/** @brief function do_kw_image_color in X_Settings_Keyword_Handlers */
KEYWORD (image_color) {
  RC_BOOLEAN_TOUT (image_color)
}
/** @brief function do_kw_invert_images in X_Settings_Keyword_Handlers */
KEYWORD (invert_images) {
  RC_BOOLEAN_TOUT (invert_images)
}
/** @brief function do_kw_text_case in X_Settings_Keyword_Handlers */
KEYWORD ( text_case ) {
  RC_TEXT_CASE_STRINGS
  RC_STRING_TABLE_W3OUT (text_case)
}

/** @brief function do_kw_text_display_zoomfactor in X_Settings_Keyword_Handlers */
KEYWORD ( text_display_zoomfactor ) {
  RC_INTEGER_WOUT (text_display_zoomfactor)
}

/** @brief function do_kw_text_feedback in X_Settings_Keyword_Handlers */
KEYWORD ( text_feedback ) {
  RC_TXT_FEEDBACK_STRINGS
  RC_STRING_TABLE_W2OUT (text_feedback)
}

/** @brief function do_kw_text_origin_markerin X_Settings_Keyword_Handlers */
KEYWORD ( text_origin_marker ) {
  RC_BOOLEAN_WOUT (text_origin_marker)
}

/** @brief function do_kw_text_size in X_Settings_Keyword_Handlers */
KEYWORD ( text_size ) {
  RC_INTEGER_WOUT (text_size)
}

/** @brief function do_kw_undo_control in X_Settings_Keyword_Handlers */
KEYWORD ( undo_control ) {
  RC_BOOLEAN_WOUT (undo_control)
}

/** @brief function do_kw_undo_levels in X_Settings_Keyword_Handlers */
KEYWORD ( undo_levels ) {
  RC_INTEGER_WOUT (undo_levels)
}

/** @brief function do_kw_undo_type in X_Settings_Keyword_Handlers */
KEYWORD ( undo_type ) {
  RC_UNDO_TYPE_STRINGS
  RC_STRING_TABLE_W2OUT (undo_type)
}

/** @brief function do_kw_undo_panzoom in X_Settings_Keyword_Handlers */
KEYWORD ( undo_panzoom ) {
  RC_BOOLEAN_WOUT (undo_panzoom)
}

/** @brief function do_kw_attribute_name in X_Settings_Keyword_Handlers */
KEYWORD ( attribute_name ) {
  int count;
  int i;
  char *ptrBuffer;
  char *ptr;

  count = s_attrib_count();

  ptrBuffer = KEY_BUFFER(attribute_name);
  for (i = 0; i < count; i++) {
    fgets(ptrBuffer, RC_INPUT_BUFFER_SIZE, input);   /* read in the next line  */
    ptr = ptrBuffer;                                 /* set pointer to beginning  */
    while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_OP )) { ptr++; } ptr++;
    if (!strncmp (ptr, KEY_NAME(attribute_name), strlen(KEY_NAME(attribute_name))))  /* see if match our keyword */
      break; /* should not do this as long as count in RC file = count in memory */
  }
  for (i = 0; i < count; i++) {
    RC_STRING_GOUT(attribute_name, s_attrib_get(i))
  }
}
/** @} END Group X_Settings_Keyword_Handlers */

#undef RC_INPUT_BUFFER_SIZE
#undef FilterList
#undef KEY_FUNC
#undef KEY_BUFFER
#undef KEY_NAME
#undef KEY_INTEGER
#undef KEYWORD

/*********** End of Configure Settings dialog box *******/











