/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_macros.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
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
 *
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 * Date Contributed: December, 29, 2014
 *
 */
/*!
 *  \file gschem_macros.h
 *
 *  \brief Pre-Processor Marcos for gschem
 *  The file contains macros used by gschem.
 *
 *  \note Presently, this header is only included by o_box.c and o_grips.c.
 */

/* Macro for inserting and (grip) editing Box Objects. */
#define GET_BOX_WIDTH(w)  (w)->second_wx - (w)->first_wx > 0 ? \
                          (w)->second_wx - (w)->first_wx : \
                          (w)->first_wx  - (w)->second_wx
#define GET_BOX_HEIGHT(w) (w)->second_wy - (w)->first_wy > 0 ? \
                          (w)->second_wy - (w)->first_wy : \
                          (w)->first_wy  - (w)->second_wy
#define GET_BOX_LEFT(w)   (w)->first_wx  > (w)->second_wx ? (w)->second_wx : (w)->first_wx
#define GET_BOX_TOP(w)    (w)->first_wy  > (w)->second_wy ? (w)->first_wy  : (w)->second_wy

/* Macro for inserting and (grip) editing Picture Objects. */
#define GET_PICTURE_WIDTH GET_BOX_WIDTH

#define GET_CONFINED_HEIGHT(w) abs((w)->second_wx - (w)->first_wx) / (w)->pixbuf_wh_ratio

#define GET_PICTURE_HEIGHT(w) (w)->CONTROLKEY && (w)->pixbuf_wh_ratio != 0 ? \
                                   GET_CONFINED_HEIGHT(w) : GET_BOX_HEIGHT(w)

/* Grips Related */
#define GET_GRIP_SIZE(w)           gschem_toplevel_get_grips_size(w)

/* Utility Macros for Message Dialogs */
#define message_dialog(text, type) \
        x_dialog_show_message(text, type, NULL);

#define titled_message_dialog(text, type, title) \
        x_dialog_show_message(text, type, title);

#define pango_message_dialog(text1, text2, type, title) \
        x_dialog_message_with_markup(text1, text2, type, title);

/* These don't really justify their own functions - just do inline */
/* Plain Text dialogs */
#define information_dialog(...) { \
        char *msg = (char*)geda_sprintf(__VA_ARGS__); \
        message_dialog(msg, GEDA_MESSAGE_INFO) \
        GEDA_FREE (msg); }
#define warning_dialog(...) { \
        char *msg = (char*)geda_sprintf(__VA_ARGS__); \
        message_dialog(msg, GEDA_MESSAGE_WARNING) \
        GEDA_FREE (msg); }
#define error_dialog(...) { \
        char *msg = (char*)geda_sprintf(__VA_ARGS__); \
        titled_message_dialog(msg, GEDA_MESSAGE_ERROR, NULL) \
        GEDA_FREE (msg); }

/* Titled Plain Text dialogs */
#define titled_information_dialog(title, ...) { \
        char *msg = (char*)geda_sprintf(__VA_ARGS__); \
        titled_message_dialog(msg, GEDA_MESSAGE_INFO, title) \
        GEDA_FREE (msg); }
#define titled_warning_dialog(title, ...) { \
        char *msg = (char*)geda_sprintf(__VA_ARGS__); \
        titled_message_dialog(msg, GEDA_MESSAGE_WARNING, title) \
        GEDA_FREE (msg); }
#define titled_error_dialog(title, ...) { \
        char *msg = (char*)geda_sprintf(__VA_ARGS__); \
        titled_message_dialog(msg, GEDA_MESSAGE_ERROR, title) \
        GEDA_FREE (msg); }

/* Pango Text dialogs - markup is optional for messages */
#define pango_info_dialog(msg1, msg2) \
        pango_message_dialog( msg1, msg2, GEDA_MESSAGE_INFO, NULL)

#define pango_warning_dialog(msg1, msg2) \
        pango_message_dialog( msg1, msg2, GEDA_MESSAGE_WARNING, NULL)

#define pango_error_dialog(msg1, msg2) \
        pango_message_dialog( msg1, msg2, GEDA_MESSAGE_ERROR, NULL)

/* Titled Pango Text dialogs */
#define titled_pango_info_dialog(msg1, msg2, title) \
        pango_message_dialog( msg1, msg2, GEDA_MESSAGE_INFO, title)

#define titled_pango_warning_dialog(msg1, msg2, title) \
        pango_message_dialog( msg1, msg2, GEDA_MESSAGE_WARNING, title)

#define titled_pango_error_dialog(msg1, msg2, title) \
        pango_message_dialog( msg1, msg2, GEDA_MESSAGE_ERROR, title)
