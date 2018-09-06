/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_bitmaps.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef __GEDA_BITMAPS_H__
#define __GEDA_BITMAPS_H__

/**
 * \file geda_bitmaps.h
 * \brief Global String Defines for gEDA Bit-Maps
 *
 * \remarks
 *  Exported from OpenOfficefile bitmaps/prog/geda_gaf_bitmaps.ods
 *  The first section defines all bitmap files. Section two defines selected
 *  bitmaps. For most bitmaps the 2nd definition should be used as this will
 *  normally be shorter.
 *
 *   \defgroup geda-global-bitmaps Global Bitmap String Name
 * @{\par This group defines strings to reference global bitmap image
 *   \ingroup geda-globals
 */

/*! \internal Macro for quoting - Don't use this!*/
#define QUOTE_BITMAP(name) #name

/*!
 * \brief Bitmap Stringification
 *
 * example BITMAP(GSCHEM_SELECT_BITMAP)
 */
#define BITMAP(...) QUOTE_BITMAP(__VA_ARGS__)

#define GAF_MAP(image_name)    GAF_##image_name##_BITMAP
#define GEDA_MAP(image_name)   GEDA_##image_name##_BITMAP
#define GSCHEM_MAP(image_name) GSCHEM_##image_name##_BITMAP
#define STOCK_MAP(image_name)  GTK_STOCK_##image_name

/*! \brief Bitmap Files Listing */
#define GAF_BITMAP_ADD_ATTRIBUTE_TINY                    "22x22/gaf_add_attribute_22x22.png"
#define GAF_BITMAP_ADD_ATTRIBUTE_SMALL                   "24x24/gaf_add_attribute_24x24.png"
#define GAF_BITMAP_ADD_ATTRIBUTE_MEDIUM                  "26x26/gaf_add_attribute_26x26.png"
#define GAF_BITMAP_ADD_ATTRIBUTE_LARGE                   "28x28/gaf_add_attribute_28x28.png"
#define GAF_BITMAP_ADD_ATTRIBUTE_XLARGE                  "32x32/gaf_add_attribute_32x32.png"
#define GAF_BITMAP_DEMOTE_TINY                           "22x22/gaf_demote_22x22.png"
#define GAF_BITMAP_DEMOTE_SMALL                          "24x24/gaf_demote_24x24.png"
#define GAF_BITMAP_DEMOTE_MEDIUM                         "26x26/gaf_demote_26x26.png"
#define GAF_BITMAP_DEMOTE_LARGE                          "28x28/gaf_demote_28x28.png"
#define GAF_BITMAP_DEMOTE_XLARGE                         "32x32/gaf_demote_32x32.png"
#define GAF_BITMAP_PDF_TINY                              "22x22/gaf_pdf_22x22.xpm"
#define GAF_BITMAP_PDF_SMALL                             "24x24/gaf_pdf_24x24.xpm"
#define GAF_BITMAP_PDF_MEDIUM                            "26x26/gaf_pdf_26x26.xpm"
#define GAF_BITMAP_PDF_LARGE                             "28x28/gaf_pdf_28x28.xpm"
#define GAF_BITMAP_PDF_XLARGE                            "32x32/gaf_pdf_32x32.xpm"
#define GAF_BITMAP_PDF_JUMBO                             "128x128/gaf_PDF_128x128.xpm"
#define GAF_BITMAP_PROMOTE_TINY                          "22x22/gaf_promote_22x22.png"
#define GAF_BITMAP_PROMOTE_SMALL                         "24x24/gaf_promote_24x24.png"
#define GAF_BITMAP_PROMOTE_MEDIUM                        "26x26/gaf_promote_26x26.png"
#define GAF_BITMAP_PROMOTE_LARGE                         "28x28/gaf_promote_28x28.png"
#define GAF_BITMAP_PROMOTE_XLARGE                        "32x32/gaf_promote_32x32.png"
#define GAF_BITMAP_SEE_NOTES_MEDIUM                      "28x28/gaf_see_notes_28x20.png"
#define GAF_BITMAP_SEE_NOTES_LARGE                       "32x32/gaf_see_notes_32x23.png"
#define GAF_BITMAP_SEE_NOTES_XLARGE                      "36x36/gaf_see_notes_36x26.png"
#define GAF_BITMAP_TOOLS_TINY                            "22x22/gaf_tools_22x22.xpm"
#define GAF_BITMAP_TOOLS_SMALL                           "24x24/gaf_tools_24x24.xpm"
#define GAF_BITMAP_TOOLS_MEDIUM                          "26x26/gaf_tools_26x26.xpm"
#define GAF_BITMAP_TOOLS_LARGE                           "28x28/gaf_tools_28x28.xpm"
#define GAF_BITMAP_TOOLS_XLARGE                          "32x32/gaf_tools_32x32.xpm"
#define GAF_BITMAP_TOOLS_JUMBO                           "128x128/gaf_tools_128x128.xpm"
#define GEDA_BITMAP_ARC_TINY                             "22x22/geda_arc_22x22.box"
#define GEDA_BITMAP_ARC_SMALL                            "24x24/geda_arc_24x24.box"
#define GEDA_BITMAP_ARC_MEDIUM                           "26x26/geda_arc_26x26.box"
#define GEDA_BITMAP_ARC_LARGE                            "28x28/geda_arc_28x28.box"
#define GEDA_BITMAP_ARC_XLARGE                           "32x32/geda_arc_32x32.box"
#define GEDA_BITMAP_COPY_TINY                            "22x22/geda_copy_22x22.png"
#define GEDA_BITMAP_COPY_SMALL                           "24x24/geda_copy_24x24.png"
#define GEDA_BITMAP_COPY_MEDIUM                          "26x26/geda_copy_26x26.png"
#define GEDA_BITMAP_COPY_LARGE                           "28x28/geda_copy_28x28.png"
#define GEDA_BITMAP_COPY_XLARGE                          "32x32/geda_copy_32x32.png"
#define GEDA_BITMAP_BOX_TINY                             "22x22/geda_box_22x22.png"
#define GEDA_BITMAP_BOX_SMALL                            "24x24/geda_box_24x24.png"
#define GEDA_BITMAP_BOX_MEDIUM                           "26x26/geda_box_26x26.png"
#define GEDA_BITMAP_BOX_LARGE                            "28x28/geda_box_28x28.png"
#define GEDA_BITMAP_BOX_XLARGE                           "32x32/geda_box_32x32.png"
#define GEDA_BITMAP_BULB_OFF                             "geda_bulb_24x26_off.png"
#define GEDA_BITMAP_BULB_ON                              "geda_bulb_24x26_on.png"
#define GEDA_BITMAP_CIRCLE_TINY                          "22x22/geda_circle_22x22.xpm"
#define GEDA_BITMAP_CIRCLE_SMALL                         "24x24/geda_circle_24x24.xpm"
#define GEDA_BITMAP_CIRCLE_MEDIUM                        "26x26/geda_circle_26x26.xpm"
#define GEDA_BITMAP_CIRCLE_LARGE                         "28x28/geda_circle_28x28.xpm"
#define GEDA_BITMAP_CIRCLE_XLARGE                        "32x32/geda_circle_32x32.xpm"
#define GEDA_BITMAP_CIRCLES_TINY                         "22x22/geda_circles_22x22.png"
#define GEDA_BITMAP_CIRCLES_SMALL                        "24x24/geda_circles_24x24.png"
#define GEDA_BITMAP_CIRCLES_MEDIUM                       "26x26/geda_circles_26x26.png"
#define GEDA_BITMAP_CIRCLES_LARGE                        "28x28/geda_circles_28x28.png"
#define GEDA_BITMAP_CIRCLES_XLARGE                       "32x32/geda_circles_32x32.png"
#define GEDA_BITMAP_DEMOTE_TINY                          "22x22/geda_demote_22x22.png"
#define GEDA_BITMAP_DEMOTE_SMALL                         "24x24/geda_demote_24x24.png"
#define GEDA_BITMAP_DEMOTE_MEDIUM                        "26x26/geda_demote_26x26.png"
#define GEDA_BITMAP_DEMOTE_LARGE                         "28x28/geda_demote_28x28.png"
#define GEDA_BITMAP_DEMOTE_XLARGE                        "32x32/geda_demote_32x32.png"
#define GEDA_BITMAP_DEMOTE_GRN_TINY                      "22x22/geda_demote_grn_22x22.png"
#define GEDA_BITMAP_DEMOTE_GRN_SMALL                     "24x24/geda_demote_grn_24x24.png"
#define GEDA_BITMAP_DEMOTE_GRN_MEDIUM                    "26x26/geda_demote_grn_26x26.png"
#define GEDA_BITMAP_DEMOTE_GRN_LARGE                     "28x28/geda_demote_grn_28x28.png"
#define GEDA_BITMAP_DEMOTE_GRN_XLARGE                    "32x32/geda_demote_grn_32x32.png"
#define GEDA_BITMAP_DESELECT_TINY                        "22x22/geda_deselect_22x22.png"
#define GEDA_BITMAP_DESELECT_SMALL                       "24x24/geda_deselect_24x24.png"
#define GEDA_BITMAP_DESELECT_MEDIUM                      "26x26/geda_deselect_26x26.png"
#define GEDA_BITMAP_DESELECT_LARGE                       "28x28/geda_deselect_28x28.png"
#define GEDA_BITMAP_DESELECT_XLARGE                      "32x32/geda_deselect_32x32.png"
#define GEDA_BITMAP_DISPLAY_COLOR_TINY                   "22x22/geda_display_color_22x22.xpm"
#define GEDA_BITMAP_DISPLAY_COLOR_SMALL                  "24x24/geda_display_color_24x24.xpm"
#define GEDA_BITMAP_DISPLAY_COLOR_MEDIUM                 "26x26/geda_display_color_26x26.xpm"
#define GEDA_BITMAP_DISPLAY_COLOR_LARGE                  "28x28/geda_display_color_28x28.xpm"
#define GEDA_BITMAP_DISPLAY_COLOR_XLARGE                 "32x32/geda_display_color_32x32.xpm"
#define GEDA_BITMAP_DISPLAY_COLOR_JUMBO                  "128x128/geda_display_color_128x128.xpm"
#define GEDA_BITMAP_EYE_GLASSES_MEDIUM                   "28x28/geda_eye_glasses_28x20.png"
#define GEDA_BITMAP_EYE_GLASSES_LARGE                    "32x32/geda_eye_glasses_32x23.png"
#define GEDA_BITMAP_EYE_GLASSES_XLARGE                   "36x36/geda_eye_glasses_36x26.png"
#define GEDA_BITMAP_FILM_ROLL_JUMBO                      "128x128/geda_film_roll_128x128.xpm"
#define GEDA_BITMAP_FILM_ROLL_SMALL                      "24x24/geda_film_roll_24x24.xpm"
#define GEDA_BITMAP_FILM_ROLL_MEDIUM                     "26x26/geda_film_roll_26x26.xpm"
#define GEDA_BITMAP_FILM_ROLL_LARGE                      "28x28/geda_film_roll_28x28.xpm"
#define GEDA_BITMAP_FILM_ROLL_XLARGE                     "32x32/geda_film_roll_32x32.xpm"
#define GEDA_BITMAP_FIND_JUMBO                           "128x128/geda_find_128x128.xpm"
#define GEDA_BITMAP_FIND_MINI                            "16x16/geda_find_16x16.xpm"
#define GEDA_BITMAP_FIND_TINY                            "22x22/geda_find_22x22.xpm"
#define GEDA_BITMAP_FIND_SMALL                           "24x24/geda_find_24x24.xpm"
#define GEDA_BITMAP_FIND_MEDIUM                          "26x26/geda_find_26x26.xpm"
#define GEDA_BITMAP_FIND_LARGE                           "28x28/geda_find_28x28.xpm"
#define GEDA_BITMAP_FIND_XLARGE                          "32x32/geda_find_32x32.xpm"
#define GEDA_BITMAP_FIND_XXLARGE                         "36x36/geda_find_36x36.xpm"
#define GEDA_BITMAP_FIND_XXXLARGE                        "48x48/geda_find_48x48.xpm"
#define GEDA_BITMAP_FIND_HUGE                            "64x64/geda_find_64x64.xpm"
#define GEDA_BITMAP_FIND_XHUGE                           "96x96/geda_find_96x96.xpm"
#define GEDA_BITMAP_FIND_REPLACE_JUMBO                   "128x128/geda_find_and_replace_128x128.xpm"
#define GEDA_BITMAP_FIND_REPLACE_MINI                    "16x16/geda_find_and_replace_16x16.xpm"
#define GEDA_BITMAP_FIND_REPLACE_TINY                    "22x22/geda_find_and_replace_22x22.xpm"
#define GEDA_BITMAP_FIND_REPLACE_SMALL                   "24x24/geda_find_and_replace_24x24.xpm"
#define GEDA_BITMAP_FIND_REPLACE_MEDIUM                  "26x26/geda_find_and_replace_26x26.xpm"
#define GEDA_BITMAP_FIND_REPLACE_LARGE                   "28x28/geda_find_and_replace_28x28.xpm"
#define GEDA_BITMAP_FIND_REPLACE_XLARGE                  "32x32/geda_find_and_replace_32x32.xpm"
#define GEDA_BITMAP_FIND_REPLACE_XXLARGE                 "36x36/geda_find_and_replace_36x36.xpm"
#define GEDA_BITMAP_FIND_REPLACE_XXXLARGE                "48x48/geda_find_and_replace_48x48.xpm"
#define GEDA_BITMAP_FIND_REPLACE_HUGE                    "64x64/geda_find_and_replace_64x64.xpm"
#define GEDA_BITMAP_FIND_REPLACE_XHUGE                   "96x96/geda_find_and_replace_96x96.xpm"
#define GEDA_BITMAP_FIND_ATTRIBUTE_TINY                  "22x22/geda_find_attribute_22x22.png"
#define GEDA_BITMAP_FIND_ATTRIBUTE_SMALL                 "24x24/geda_find_attribute_24x24.png"
#define GEDA_BITMAP_FIND_ATTRIBUTE_MEDIUM                "26x26/geda_find_attribute_26x26.png"
#define GEDA_BITMAP_FIND_ATTRIBUTE_LARGE                 "28x28/geda_find_attribute_28x28.png"
#define GEDA_BITMAP_FIND_ATTRIBUTE_XLARGE                "32x32/geda_find_attribute_32x32.png"
#define GEDA_BITMAP_LINE_TINY                            "22x22/geda_line_22x22.xpm"
#define GEDA_BITMAP_LINE_SMALL                           "24x24/geda_line_24x24.xpm"
#define GEDA_BITMAP_LINE_MEDIUM                          "26x26/geda_line_26x26.xpm"
#define GEDA_BITMAP_LINE_LARGE                           "28x28/geda_line_28x28.xpm"
#define GEDA_BITMAP_LINE_XLARGE                          "32x32/geda_line_32x32.xpm"
#define GEDA_BITMAP_LINE_TYPE_TINY                       "22x22/geda_line_type_22x22.png"
#define GEDA_BITMAP_LINE_TYPE_SMALL                      "24x24/geda_line_type_24x24.png"
#define GEDA_BITMAP_LINE_TYPE_MEDIUM                     "26x26/geda_line_type_26x26.png"
#define GEDA_BITMAP_LINE_TYPE_LARGE                      "28x28/geda_line_type_28x28.png"
#define GEDA_BITMAP_LINE_TYPE_XLARGE                     "32x32/geda_line_type_32x32.png"
#define GEDA_BITMAP_LOCATE_REFERENCE_TINY                "22x22/geda_locate_reference_22x22.xpm"
#define GEDA_BITMAP_LOCATE_REFERENCE_SMALL               "24x24/geda_locate_reference_24x24.xpm"
#define GEDA_BITMAP_LOCATE_REFERENCE_MEDIUM              "26x26/geda_locate_reference_26x26.xpm"
#define GEDA_BITMAP_LOCATE_REFERENCE_LARGE               "28x28/geda_locate_reference_28x28.xpm"
#define GEDA_BITMAP_LOCATE_REFERENCE_XLARGE              "32x32/geda_locate_reference_32x32.xpm"
#define GEDA_BITMAP_LOCK_TINY                            "22x22/geda_lock_22x22.png"
#define GEDA_BITMAP_LOCK_SMALL                           "24x24/geda_lock_24x24.png"
#define GEDA_BITMAP_LOCK_MEDIUM                          "26x26/geda_lock_26x26.png"
#define GEDA_BITMAP_LOCK_LARGE                           "28x28/geda_lock_28x28.png"
#define GEDA_BITMAP_LOCK_XLARGE                          "32x32/geda_lock_32x32.png"
#define GEDA_BITMAP_MESH_TINY                            "22x22/geda_mesh_22x22.png"
#define GEDA_BITMAP_MESH_SMALL                           "24x24/geda_mesh_24x24.png"
#define GEDA_BITMAP_MESH_MEDIUM                          "26x26/geda_mesh_26x26.png"
#define GEDA_BITMAP_MESH_LARGE                           "28x28/geda_mesh_28x28.png"
#define GEDA_BITMAP_MESH_XLARGE                          "32x32/geda_mesh_32x32.png"
#define GEDA_BITMAP_MIRROR_TINY                          "22x22/geda_mirror_22x22.png"
#define GEDA_BITMAP_MIRROR_SMALL                         "24x24/geda_mirror_24x24.png"
#define GEDA_BITMAP_MIRROR_MEDIUM                        "26x26/geda_mirror_26x26.png"
#define GEDA_BITMAP_MIRROR_LARGE                         "28x28/geda_mirror_28x28.png"
#define GEDA_BITMAP_MIRROR_XLARGE                        "32x32/geda_mirror_32x32.png"
#define GEDA_BITMAP_MOVE_TINY                            "22x22/geda_move_22x22.png"
#define GEDA_BITMAP_MOVE_SMALL                           "24x24/geda_move_24x24.png"
#define GEDA_BITMAP_MOVE_MEDIUM                          "26x26/geda_move_26x26.png"
#define GEDA_BITMAP_MOVE_LARGE                           "28x28/geda_move_28x28.png"
#define GEDA_BITMAP_MOVE_XLARGE                          "32x32/geda_move_32x32.png"
#define GEDA_BITMAP_MULTI_TINY                           "22x22/geda_multi_22x22.png"
#define GEDA_BITMAP_MULTI_SMALL                          "24x24/geda_multi_24x24.png"
#define GEDA_BITMAP_MULTI_MEDIUM                         "26x26/geda_multi_26x26.png"
#define GEDA_BITMAP_MULTI_LARGE                          "28x28/geda_multi_28x28.png"
#define GEDA_BITMAP_MULTI_XLARGE                         "32x32/geda_multi_32x32.png"
#define GEDA_BITMAP_NAME_TAG_MINI                        "16x16/geda_name_tag_16x16.xpm"
#define GEDA_BITMAP_NAME_TAG_TINY                        "22x22/geda_name_tag_22x22.xpm"
#define GEDA_BITMAP_NAME_TAG_SMALL                       "24x24/geda_name_tag_24x24.xpm"
#define GEDA_BITMAP_NAME_TAG_MEDIUM                      "26x26/geda_name_tag_26x26.xpm"
#define GEDA_BITMAP_NAME_TAG_LARGE                       "28x28/geda_name_tag_28x28.xpm"
#define GEDA_BITMAP_NAME_TAG_XLARGE                      "32x32/geda_name_tag_32x32.xpm"
#define GEDA_BITMAP_NAME_TAG_XXLARGE                     "36x36/geda_name_tag_36x36.xpm"
#define GEDA_BITMAP_NAME_VALUE_TINY                      "22x22/geda_name_value_22x22.xpm"
#define GEDA_BITMAP_NAME_VALUE_SMALL                     "24x24/geda_name_value_24x24.xpm"
#define GEDA_BITMAP_NAME_VALUE_MEDIUM                    "26x26/geda_name_value_26x26.xpm"
#define GEDA_BITMAP_NAME_VALUE_LARGE                     "28x28/geda_name_value_28x28.xpm"
#define GEDA_BITMAP_NAME_VALUE_XLARGE                    "32x32/geda_name_value_32x32.xpm"
#define GEDA_BITMAP_NAME_VALUE_XXLARGE                   "36x36/geda_name_value_36x36.xpm"
#define GEDA_BITMAP_NUMBER_TINY                          "22x22/geda_number_22x22.png"
#define GEDA_BITMAP_NUMBER_SMALL                         "24x24/geda_number_24x24.png"
#define GEDA_BITMAP_NUMBER_MEDIUM                        "26x26/geda_number_26x26.png"
#define GEDA_BITMAP_NUMBER_LARGE                         "28x28/geda_number_28x28.png"
#define GEDA_BITMAP_NUMBER_XLARGE                        "32x32/geda_number_32x32.png"
#define GEDA_BITMAP_PIN_TINY                             "22x22/geda_pin_22x22.png"
#define GEDA_BITMAP_PIN_SMALL                            "24x24/geda_pin_24x24.png"
#define GEDA_BITMAP_PIN_MEDIUM                           "26x26/geda_pin_26x26.png"
#define GEDA_BITMAP_PIN_LARGE                            "28x28/geda_pin_28x28.png"
#define GEDA_BITMAP_PIN_XLARGE                           "32x32/geda_pin_32x32.png"
#define GEDA_BITMAP_PROMOTE_TINY                         "22x22/geda_promote_22x22.png"
#define GEDA_BITMAP_PROMOTE_SMALL                        "24x24/geda_promote_24x24.png"
#define GEDA_BITMAP_PROMOTE_MEDIUM                       "26x26/geda_promote_26x26.png"
#define GEDA_BITMAP_PROMOTE_LARGE                        "28x28/geda_promote_28x28.png"
#define GEDA_BITMAP_PROMOTE_XLARGE                       "32x32/geda_promote_32x32.png"
#define GEDA_BITMAP_REDCROSS_JUMBO                       "128x128/geda_redcross_128x128.xpm"
#define GEDA_BITMAP_REDCROSS_TINY                        "22x22/geda_redcross_22x22.xpm"
#define GEDA_BITMAP_REDCROSS_SMALL                       "24x24/geda_redcross_24x24.xpm"
#define GEDA_BITMAP_REDCROSS_MEDIUM                      "26x26/geda_redcross_26x26.xpm"
#define GEDA_BITMAP_REDCROSS_LARGE                       "28x28/geda_redcross_28x28.xpm"
#define GEDA_BITMAP_REDCROSS_XLARGE                      "32x32/geda_redcross_32x32.xpm"
#define GEDA_BITMAP_REDCROSS_XXLARGE                     "36x36/geda_redcross_36x36.xpm"
#define GEDA_BITMAP_REDCROSS_XXXLARGE                    "48x48/geda_redcross_48x48.xpm"
#define GEDA_BITMAP_REDCROSS_HUGE                        "64x64/geda_redcross_64x64.xpm"
#define GEDA_BITMAP_REDCROSS_XHUGE                       "96x96/geda_redcross_96x96.xpm"
#define GEDA_BITMAP_ROTATE_TINY                          "22x22/geda_rotate_left22x22.png"
#define GEDA_BITMAP_ROTATE_SMALL                         "24x24/geda_rotate_left24x24.png"
#define GEDA_BITMAP_ROTATE_MEDIUM                        "26x26/geda_rotate_left26x26.png"
#define GEDA_BITMAP_ROTATE_LARGE                         "28x28/geda_rotate_left28x28.png"
#define GEDA_BITMAP_ROTATE_XLARGE                        "32x32/geda_rotate_left32x32.png"
#define GEDA_BITMAP_SHEETS_TINY                          "22x22/geda_sheets_22x22.xpm"
#define GEDA_BITMAP_SHEETS_SMALL                         "24x24/geda_sheets_24x24.xpm"
#define GEDA_BITMAP_SHEETS_MEDIUM                        "26x26/geda_sheets_26x26.xpm"
#define GEDA_BITMAP_SHEETS_LARGE                         "28x28/geda_sheets_28x28.xpm"
#define GEDA_BITMAP_SHEETS_XLARGE                        "32x32/geda_sheets_32x32.xpm"
#define GEDA_BITMAP_SELECT_TINY                          "22x22/geda_select_22x22.png"
#define GEDA_BITMAP_SELECT_SMALL                         "24x24/geda_select_24x24.png"
#define GEDA_BITMAP_SELECT_MEDIUM                        "26x26/geda_select_26x26.png"
#define GEDA_BITMAP_SELECT_LARGE                         "28x28/geda_select_28x28.png"
#define GEDA_BITMAP_SELECT_XLARGE                        "32x32/geda_select_32x32.png"
#define GEDA_BITMAP_SWITCH_OFF                           "geda_switch_48x36_off.png"
#define GEDA_BITMAP_SWITCH_ON                            "geda_switch_48x36_on.png"
#define GEDA_BITMAP_TOOLS_TINY                           "22x22/geda_tools_22x22.xpm"
#define GEDA_BITMAP_TOOLS_SMALL                          "24x24/geda_tools_24x24.xpm"
#define GEDA_BITMAP_TOOLS_MEDIUM                         "26x26/geda_tools_26x26.xpm"
#define GEDA_BITMAP_TOOLS_LARGE                          "28x28/geda_tools_28x28.xpm"
#define GEDA_BITMAP_TOOLS_XLARGE                         "32x32/geda_tools_32x32.xpm"
#define GEDA_BITMAP_TOOLS_JUMBO                          "128x128/geda_tools_128x128.png"
#define GEDA_BITMAP_TRANSLATE_TINY                       "22x22/geda_translate_22x22.png"
#define GEDA_BITMAP_TRANSLATE_SMALL                      "24x24/geda_translate_24x24.png"
#define GEDA_BITMAP_TRANSLATE_MEDIUM                     "26x26/geda_translate_26x26.png"
#define GEDA_BITMAP_TRANSLATE_LARGE                      "28x28/geda_translate_28x28.png"
#define GEDA_BITMAP_TRANSLATE_XLARGE                     "32x32/geda_translate_32x32.png"
#define GEDA_BITMAP_UNLOCK_TINY                          "22x22/geda_unlock_22x22.png"
#define GEDA_BITMAP_UNLOCK_SMALL                         "24x24/geda_unlock_24x24.png"
#define GEDA_BITMAP_UNLOCK_MEDIUM                        "26x26/geda_unlock_26x26.png"
#define GEDA_BITMAP_UNLOCK_LARGE                         "28x28/geda_unlock_28x28.png"
#define GEDA_BITMAP_UNLOCK_XLARGE                        "32x32/geda_unlock_32x32.png"
#define GEDA_BITMAP_VALUE_MINI                           "16x16/geda_value_16x16.xpm"
#define GEDA_BITMAP_VALUE_TINY                           "22x22/geda_value_22x22.xpm"
#define GEDA_BITMAP_VALUE_SMALL                          "24x24/geda_value_24x24.xpm"
#define GEDA_BITMAP_VALUE_MEDIUM                         "26x26/geda_value_26x26.xpm"
#define GEDA_BITMAP_VALUE_LARGE                          "28x28/geda_value_28x28.xpm"
#define GEDA_BITMAP_VALUE_XLARGE                         "32x32/geda_value_32x32.xpm"
#define GEDA_BITMAP_VALUE_XXLARGE                        "36x36/geda_value_36x36.xpm"
#define GEDA_BITMAP_VALUE_256x256                        "256x256/geda_value_256x256.png"
#define GEDA_BITMAP_VIEW_REDRAW_TINY                     "22x22/geda_view_redraw_22x22.png"
#define GEDA_BITMAP_VIEW_REDRAW_SMALL                    "24x24/geda_view_redraw_24x24.png"
#define GEDA_BITMAP_VIEW_REDRAW_MEDIUM                   "26x26/geda_view_redraw_26x26.png"
#define GEDA_BITMAP_VIEW_REDRAW_LARGE                    "28x28/geda_view_redraw_28x28.png"
#define GEDA_BITMAP_VIEW_REDRAW_XLARGE                   "32x32/geda_view_redraw_32x32.png"
#define GEDA_BITMAP_ZOOM_BOX_TINY                        "22x22/geda_zoom_box_22x22.png"
#define GEDA_BITMAP_ZOOM_BOX_SMALL                       "24x24/geda_zoom_box_24x24.png"
#define GEDA_BITMAP_ZOOM_BOX_MEDIUM                      "26x26/geda_zoom_box_26x26.png"
#define GEDA_BITMAP_ZOOM_BOX_LARGE                       "28x28/geda_zoom_box_28x28.png"
#define GEDA_BITMAP_ZOOM_BOX_XLARGE                      "32x32/geda_zoom_box_32x32.png"
#define GEDA_BITMAP_ZOOM_EXTENTS_TINY                    "22x22/geda_zoom_extents_22x22.png"
#define GEDA_BITMAP_ZOOM_EXTENTS_SMALL                   "24x24/geda_zoom_extents_24x24.png"
#define GEDA_BITMAP_ZOOM_EXTENTS_MEDIUM                  "26x26/geda_zoom_extents_26x26.png"
#define GEDA_BITMAP_ZOOM_EXTENTS_LARGE                   "28x28/geda_zoom_extents_28x28.png"
#define GEDA_BITMAP_ZOOM_EXTENTS_XLARGE                  "32x32/geda_zoom_extents_32x32.png"
#define GEDA_BITMAP_ZOOM_IN_TINY                         "22x22/geda_zoom_in_22x22.png"
#define GEDA_BITMAP_ZOOM_IN_SMALL                        "24x24/geda_zoom_in_24x24.png"
#define GEDA_BITMAP_ZOOM_IN_MEDIUM                       "26x26/geda_zoom_in_26x26.png"
#define GEDA_BITMAP_ZOOM_IN_LARGE                        "28x28/geda_zoom_in_28x28.png"
#define GEDA_BITMAP_ZOOM_IN_XLARGE                       "32x32/geda_zoom_in_32x32.png"
#define GEDA_BITMAP_ZOOM_OUT_TINY                        "22x22/geda_zoom_out_22x22.png"
#define GEDA_BITMAP_ZOOM_OUT_SMALL                       "24x24/geda_zoom_out_24x24.png"
#define GEDA_BITMAP_ZOOM_OUT_MEDIUM                      "26x26/geda_zoom_out_26x26.png"
#define GEDA_BITMAP_ZOOM_OUT_LARGE                       "28x28/geda_zoom_out_28x28.png"
#define GEDA_BITMAP_ZOOM_OUT_XLARGE                      "32x32/geda_zoom_out_32x32.png"
#define GEDA_BITMAP_ZOOM_LIMITS_TINY                     "22x22/geda_zoom_limits_22x22.png"
#define GEDA_BITMAP_ZOOM_LIMITS_SMALL                    "24x24/geda_zoom_limits_24x24.png"
#define GEDA_BITMAP_ZOOM_LIMITS_MEDIUM                   "26x26/geda_zoom_limits_26x26.png"
#define GEDA_BITMAP_ZOOM_LIMITS_LARGE                    "28x28/geda_zoom_limits_28x28.png"
#define GEDA_BITMAP_ZOOM_LIMITS_XLARGE                   "32x32/geda_zoom_limits_32x32.png"
#define GEDA_BITMAP_ZOOM_PAN_TINY                        "22x22/geda_zoom_pan_22x22.png"
#define GEDA_BITMAP_ZOOM_PAN_SMALL                       "24x24/geda_zoom_pan_24x24.png"
#define GEDA_BITMAP_ZOOM_PAN_MEDIUM                      "26x26/geda_zoom_pan_26x26.png"
#define GEDA_BITMAP_ZOOM_PAN_LARGE                       "28x28/geda_zoom_pan_28x28.png"
#define GEDA_BITMAP_ZOOM_PAN_XLARGE                      "32x32/geda_zoom_pan_32x32.png"
#define GEDA_BITMAP_GHOST_INVISIBLE_TINY                 "22x22/ghost_invisible_22x22.xpm"
#define GEDA_BITMAP_GHOST_INVISIBLE_SMALL                "24x24/ghost_invisible_24x24.xpm"
#define GEDA_BITMAP_GHOST_INVISIBLE_256X256              "256x256/ghost_invisible_256x256.xpm"
#define GEDA_BITMAP_GHOST_INVISIBLE_MEDIUM               "26x26/ghost_invisible_26x26.xpm"
#define GEDA_BITMAP_GHOST_INVISIBLE_LARGE                "28x28/ghost_invisible_28x28.xpm"
#define GEDA_BITMAP_GHOST_INVISIBLE_XLARGE               "32x32/ghost_invisible_32x32.xpm"

#define GSCHEM_BITMAP_ABOUT_LOGO_PNG                     "gschem_about_logo.png"
#define GSCHEM_BITMAP_ABOUT_LOGO_XCF                     "gschem_about_logo.xcf"
#define GSCHEM_BITMAP_ALIGN_BOTTOM_CENTER_XLARGE         "gschem_alignment_bottomcenter.png"
#define GSCHEM_BITMAP_ALIGN_BOTTOM_LEFT_XLARGE           "gschem_alignment_bottomleft.png"
#define GSCHEM_BITMAP_ALIGN_BOTTOM_RIGHT_XLARGE          "gschem_alignment_bottomright.png"
#define GSCHEM_BITMAP_ALIGN_MIDDLE_CENTER_XLARGE         "gschem_alignment_middlecenter.png"
#define GSCHEM_BITMAP_ALIGN_MIDDLE_LEFT_XLARGE           "gschem_alignment_middleleft.png"
#define GSCHEM_BITMAP_ALIGN_MIDDLE_RIGHT_XLARGE          "gschem_alignment_middleright.png"
#define GSCHEM_BITMAP_ALIGN_TOP_CENTER_XLARGE            "gschem_alignment_topcenter.png"
#define GSCHEM_BITMAP_ALIGN_TOP_LEFT_XLARGE              "gschem_alignment_topleft.png"
#define GSCHEM_BITMAP_ALIGN_TOP_RIGHT_XLARGE             "gschem_alignment_topright.png"
#define GSCHEM_BITMAP_ALIGN_UNCHANGED_XLARGE             "gschem_alignment_unchanged.png"
#define GSCHEM_BITMAP_ALIGN_BOTTOM_2_TOP_XLARGE          "gschem_bottom2top.png"
#define GSCHEM_BITMAP_BUS_SMALL                          "gschem_bus.xpm"
#define GSCHEM_BITMAP_COMP_SMALL                         "gschem_comp_24x24.xpm"
#define GSCHEM_BITMAP_COMP_MEDIUM                        "gschem_comp_26x26.xpm"
#define GSCHEM_BITMAP_COPY_SMALL                         "gschem_copy.xpm"
#define GSCHEM_BITMAP_DELETE_SMALL                       "gschem_delete.xpm"
#define GSCHEM_BITMAP_DIAGONAL_XLARGE                    "gschem_diagonal.png"
#define GSCHEM_BITMAP_EDIT_SMALL                         "gschem_edit.xpm"
#define GSCHEM_BITMAP_FILE_ORDER_XLARGE                  "gschem_fileorder.png"
#define GSCHEM_BITMAP_FILL_TYPE_FILLED                   "gschem_filltype_filled.png"
#define GSCHEM_BITMAP_FILL_TYPE_HATCH                    "gschem_filltype_hatch.png"
#define GSCHEM_BITMAP_FILL_TYPE_HOLLOW                   "gschem_filltype_hollow.png"
#define GSCHEM_BITMAP_FILL_TYPE_MESH_SMALL               "gschem_filltype_mesh_24x24.xpm"
#define GSCHEM_BITMAP_FILL_TYPE_MESH_MEDIUM              "gschem_filltype_mesh_26x26.xpm"
#define GSCHEM_BITMAP_FILL_TYPE_MESH_LARGE               "gschem_filltype_mesh_28x28.xpm"
#define GSCHEM_BITMAP_FILL_TYPE_MESH_PNG                 "gschem_filltype_mesh.png"
#define GSCHEM_BITMAP_INVERT_TINY                        "gschem_invert_22x22.png"
#define GSCHEM_BITMAP_INVERT_SMALL                       "gschem_invert_24x24.png"
#define GSCHEM_BITMAP_INVERT_MEDIUM                      "gschem_invert_26x26.png"
#define GSCHEM_BITMAP_INVERT_LARGE                       "gschem_invert_28x28.png"
#define GSCHEM_BITMAP_INVERT_XLARGE                      "gschem_invert_32x32.png"
#define GSCHEM_BITMAP_INVERT_BORDER                      "gschem-invert.png"
#define GSCHEM_BITMAP_LEFT_2_RIGHT_XLARGE                "gschem_left2right.png"
#define GSCHEM_BITMAP_MIRROR_SMALL                       "gschem_mirror.xpm"
#define GSCHEM_BITMAP_MOVE_SMALL                         "gschem_move.xpm"
#define GSCHEM_BITMAP_NET_SMALL                          "gschem_net.xpm"
#define GSCHEM_BITMAP_NEW_SMALL                          "gschem_new.xpm"
#define GSCHEM_BITMAP_OPEN_SMALL                         "gschem_open.xpm"
#define GSCHEM_BITMAP_PRINT_XXXLARGE                     "gschem-print-document.png"
#define	GSCHEM_BITMAP_PROJECT_CLOSE_TINY                 "22x22/gschem_project_close_22x22.png"
#define	GSCHEM_BITMAP_PROJECT_CLOSE_SMALL                "24x24/gschem_project_close_24x24.png"
#define	GSCHEM_BITMAP_PROJECT_CLOSE_MEDIUM               "26x26/gschem_project_close_26x26.png"
#define	GSCHEM_BITMAP_PROJECT_CLOSE_LARGE                "28x28/gschem_project_close_28x28.png"
#define	GSCHEM_BITMAP_PROJECT_CLOSE_XLARGE               "32x32/gschem_project_close_32x32.png"
#define GSCHEM_BITMAP_REDO_SMALL                         "gschem_redo.xpm"
#define GSCHEM_BITMAP_RIGHT_2_LEFT_XLARGE                "gschem_right2left.png"
#define GSCHEM_BITMAP_ROTATE_SMALL                       "gschem_rotate.xpm"
#define GSCHEM_BITMAP_SAVE_SMALL                         "gschem_save.xpm"
#define GSCHEM_BITMAP_SAVE_AS_SMALL                      "gschem_save_as.xpm"
#define GSCHEM_BITMAP_SELECT_TINY                        "gschem_select_22x22.png"
#define GSCHEM_BITMAP_SELECT_SMALL                       "gschem_select_24x24.png"
#define GSCHEM_BITMAP_SELECT_MEDIUM                      "gschem_select_26x26.png"
#define GSCHEM_BITMAP_SELECT_LARGE                       "gschem_select_28x28.png"
#define GSCHEM_BITMAP_SELECT_XLARGE                      "gschem_select_32x32.png"
#define GSCHEM_BITMAP_SELECT_BORDER                      "gschem-select.png"
#define GSCHEM_BITMAP_SELECT_ALL_TINY                    "gschem_select_all_22x22.png"
#define GSCHEM_BITMAP_SELECT_ALL_SMALL                   "gschem_select_all_24x24.png"
#define GSCHEM_BITMAP_SELECT_ALL_MEDIUM                  "gschem_select_all_26x26.png"
#define GSCHEM_BITMAP_SELECT_ALL_LARGE                   "gschem_select_all_28x28.png"
#define GSCHEM_BITMAP_SELECT_ALL_XLARGE                  "gschem_select_all_32x32.png"
#define GSCHEM_BITMAP_SELECT_ALL_BORDER                  "gschem-select-all.png"
#define GSCHEM_BITMAP_TEXT_SMALL                         "gschem_text_24x24.png"
#define GSCHEM_BITMAP_TEXT_MEDIUM                        "gschem_text_26x26.png"
#define GSCHEM_BITMAP_TRANSISTOR_LARGE                   "gschem_transistor_28x28.png"
#define GSCHEM_BITMAP_TRANSISTOR_XLARGE                  "gschem_transistor_32x32.png"
#define GSCHEM_BITMAP_TOP_2_BOTTOM_XLARGE                "gschem_top2bottom.png"
#define GSCHEM_BITMAP_UNDO_SMALL                         "gschem_undo.xpm"
#define GSCHEM_BITMAP_UNSELECT_TINY                      "gschem_unselect_22x22.png"
#define GSCHEM_BITMAP_UNSELECT_SMALL                     "gschem_unselect_24x24.png"
#define GSCHEM_BITMAP_UNSELECT_MEDIUM                    "gschem_unselect_26x26.png"
#define GSCHEM_BITMAP_UNSELECT_LARGE                     "gschem_unselect_28x28.png"
#define GSCHEM_BITMAP_UNSELECT_XLARGE                    "gschem_unselect_32x32.png"
#define GSCHEM_BITMAP_UNSELECT_BORDER                    "gschem-unselect.png"

#define GSCHEM_BITMAP_WARNING_XXXLARGE                   "gschem_warning.png"

/*!\brief Selected Bitmaps defined using above defines! */
#define GAF_ADD_ATTRIBUTE_BITMAP        GAF_BITMAP_ADD_ATTRIBUTE_MEDIUM
#define GAF_TOOLS_BITMAP                GAF_BITMAP_TOOLS_MEDIUM
#define GAF_PDF_BITMAP                  GAF_BITMAP_PDF_SMALL
#define GAF_DEMOTE_BITMAP               GAF_BITMAP_DEMOTE_SMALL
#define GAF_PROMOTE_BITMAP              GAF_BITMAP_PROMOTE_SMALL
#define GAF_SEE_NOTES_BITMAP            GAF_BITMAP_SEE_NOTES_MEDIUM
#define GEDA_ARC_BITMAP                 GEDA_BITMAP_ARC_MEDIUM
#define GEDA_BOX_BITMAP                 GEDA_BITMAP_BOX_SMALL
#define GEDA_CIRCLE_BITMAP              GEDA_BITMAP_CIRCLE_MEDIUM
#define GEDA_CIRCLES_BITMAP             GEDA_BITMAP_CIRCLES_MEDIUM
#define GEDA_COPY_BITMAP                GEDA_BITMAP_COPY_MEDIUM
#define GEDA_DEMOTE_SCH_BITMAP          GEDA_BITMAP_DEMOTE_LARGE
#define GEDA_DEMOTE_SYM_BITMAP          GEDA_BITMAP_DEMOTE_GRN_LARGE
#define GEDA_DESELECT_BITMAP            GEDA_BITMAP_DESELECT_TINY
#define GEDA_DISPLAY_COLOR_BITMAP       GEDA_BITMAP_DISPLAY_COLOR_MEDIUM
#define GEDA_EYE_GLASSES_BITMAP         GEDA_BITMAP_EYE_GLASSES_MEDIUM
#define GEDA_FILM_ROLL_BITMAP           GEDA_BITMAP_FILM_ROLL_MEDIUM
#define GEDA_FIND_BITMAP                GEDA_BITMAP_FIND_MEDIUM
#define GEDA_FIND_REPLACE_BITMAP        GEDA_BITMAP_FIND_REPLACE_MEDIUM
#define GEDA_FIND_ATTRIBUTE_BITMAP      GEDA_BITMAP_FIND_ATTRIBUTE_LARGE
#define GEDA_LINE_BITMAP                GEDA_BITMAP_LINE_TINY
#define GEDA_LINE_TYPE_BITMAP           GEDA_BITMAP_LINE_TYPE_MEDIUM
#define GEDA_LOCATE_REFERENCE_BITMAP    GEDA_BITMAP_LOCATE_REFERENCE_LARGE
#define GEDA_LOCK_BITMAP                GEDA_BITMAP_LOCK_MEDIUM
#define GEDA_MESH_BITMAP                GEDA_BITMAP_MESH_MEDIUM
#define GEDA_MIRROR_BITMAP              GEDA_BITMAP_MIRROR_LARGE
#define GEDA_MOVE_BITMAP                GEDA_BITMAP_MOVE_SMALL
#define GEDA_MULTI_BITMAP               GEDA_BITMAP_MULTI_LARGE
#define GEDA_NAME_TAG_BITMAP            GEDA_BITMAP_NAME_TAG_MEDIUM
#define GEDA_NAME_VALUE_BITMAP          GEDA_BITMAP_NAME_VALUE_MEDIUM
#define GEDA_NUMBER_BITMAP              GEDA_BITMAP_NUMBER_SMALL
#define GEDA_PIN_BITMAP                 GEDA_BITMAP_PIN_MEDIUM
#define GEDA_PROMOTE_BITMAP             GEDA_BITMAP_PROMOTE_LARGE
#define GEDA_REDCROSS_BITMAP            GEDA_BITMAP_REDCROSS_TINY
#define GEDA_ROTATE_BITMAP              GEDA_BITMAP_ROTATE_SMALL
#define GEDA_SHEETS_BITMAP              GEDA_BITMAP_SHEETS_SMALL
#define GEDA_SELECT_BITMAP              GEDA_BITMAP_SELECT_TINY
#define GEDA_TOOLS_BITMAP               GEDA_BITMAP_TOOLS_SMALL
#define GEDA_TRANSLATE_BITMAP           GEDA_BITMAP_TRANSLATE_SMALL
#define GEDA_UNLOCK_BITMAP              GEDA_BITMAP_UNLOCK_MEDIUM
#define GEDA_VALUE_BITMAP               GEDA_BITMAP_VALUE_MEDIUM
#define GEDA_VIEW_REDRAW_BITMAP         GEDA_BITMAP_VIEW_REDRAW_MEDIUM
#define GEDA_ZOOM_BOX_BITMAP            GEDA_BITMAP_ZOOM_BOX_LARGE
#define GEDA_ZOOM_EXTENTS_BITMAP        GEDA_BITMAP_ZOOM_EXTENTS_LARGE
#define GEDA_ZOOM_IN_BITMAP             GEDA_BITMAP_ZOOM_IN_LARGE
#define GEDA_ZOOM_OUT_BITMAP            GEDA_BITMAP_ZOOM_OUT_LARGE
#define GEDA_ZOOM_LIMITS_BITMAP         GEDA_BITMAP_ZOOM_LIMITS_MEDIUM
#define GEDA_ZOOM_PAN_BITMAP            GEDA_BITMAP_ZOOM_PAN_MEDIUM
#define GEDA_GHOST_INVISIBLE_BITMAP             GEDA_BITMAP_GHOST_INVISIBLE_LARGE
#define GSCHEM_ABOUT_LOGO_BITMAP                GSCHEM_BITMAP_ABOUT_LOGO_PNG
#define GSCHEM_ALIGN_BOTTOM_CENTER_BITMAP       GSCHEM_BITMAP_ALIGN_BOTTOM_CENTER_XLARGE
#define GSCHEM_ALIGN_BOTTOM_LEFT_BITMAP         GSCHEM_BITMAP_ALIGN_BOTTOM_LEFT_XLARGE
#define GSCHEM_ALIGN_BOTTOM_RIGHT_BITMAP        GSCHEM_BITMAP_ALIGN_BOTTOM_RIGHT_XLARGE
#define GSCHEM_ALIGN_MIDDLE_CENTER_BITMAP       GSCHEM_BITMAP_ALIGN_MIDDLE_CENTER_XLARGE
#define GSCHEM_ALIGN_MIDDLE_LEFT_BITMAP         GSCHEM_BITMAP_ALIGN_MIDDLE_LEFT_XLARGE
#define GSCHEM_ALIGN_MIDDLE_RIGHT_BITMAP        GSCHEM_BITMAP_ALIGN_MIDDLE_RIGHT_XLARGE
#define GSCHEM_ALIGN_TOP_CENTER_BITMAP          GSCHEM_BITMAP_ALIGN_TOP_CENTER_XLARGE
#define GSCHEM_ALIGN_TOP_LEFT_BITMAP            GSCHEM_BITMAP_ALIGN_TOP_LEFT_XLARGE
#define GSCHEM_ALIGN_TOP_RIGHT_BITMAP           GSCHEM_BITMAP_ALIGN_TOP_RIGHT_XLARGE
#define GSCHEM_ALIGN_UNCHANGED_BITMAP           GSCHEM_BITMAP_ALIGN_UNCHANGED_XLARGE
#define GSCHEM_ALIGN_BOTTOM_2_TOP_BITMAP        GSCHEM_BITMAP_ALIGN_BOTTOM_2_TOP_XLARGE
#define GSCHEM_BUS_BITMAP                       GSCHEM_BITMAP_BUS_SMALL
#define GSCHEM_COMP_BITMAP              GSCHEM_BITMAP_COMP_MEDIUM
#define GSCHEM_COPY_BITMAP              GSCHEM_BITMAP_COPY_SMALL
#define GSCHEM_DELETE_BITMAP            GSCHEM_BITMAP_DELETE_SMALL
#define GSCHEM_DIAGONAL_BITMAP          GSCHEM_BITMAP_DIAGONAL_XLARGE
#define GSCHEM_EDIT_BITMAP              GSCHEM_BITMAP_EDIT_SMALL
#define GSCHEM_FILE_ORDER_BITMAP        GSCHEM_BITMAP_FILE_ORDER_XLARGE
#define GSCHEM_FILL_TYPE_MESH_BITMAP    GSCHEM_BITMAP_FILL_TYPE_MESH_LARGE
#define GSCHEM_LEFT_2_RIGHT_BITMAP      GSCHEM_BITMAP_LEFT_2_RIGHT_XLARGE
#define GSCHEM_MIRROR_BITMAP            GSCHEM_BITMAP_MIRROR_SMALL
#define GSCHEM_MOVE_BITMAP              GSCHEM_BITMAP_MOVE_SMALL
#define GSCHEM_NET_BITMAP               GSCHEM_BITMAP_NET_SMALL
#define GSCHEM_NEW_BITMAP               GSCHEM_BITMAP_NEW_SMALL
#define GSCHEM_OPEN_BITMAP              GSCHEM_BITMAP_OPEN_SMALL
#define GSCHEM_PRINT_BITMAP             GSCHEM_BITMAP_PRINT_XXXLARGE
#define	GSCHEM_PROJECT_CLOSE_BITMAP     GSCHEM_BITMAP_PROJECT_CLOSE_SMALL
#define GSCHEM_REDO_BITMAP              GSCHEM_BITMAP_REDO_SMALL
#define GSCHEM_RIGHT_2_LEFT_BITMAP      GSCHEM_BITMAP_RIGHT_2_LEFT_XLARGE
#define GSCHEM_ROTATE_BITMAP            GSCHEM_BITMAP_ROTATE_SMALL
#define GSCHEM_SAVE_BITMAP              GSCHEM_BITMAP_SAVE_SMALL
#define GSCHEM_SAVE_AS_BITMAP           GSCHEM_BITMAP_SAVE_AS_SMALL
#define GSCHEM_SELECT_BITMAP            GSCHEM_BITMAP_SELECT_TINY
#define GSCHEM_SELECT_ALL_BITMAP        GSCHEM_BITMAP_SELECT_ALL_SMALL
#define GSCHEM_TEXT_BITMAP              GSCHEM_BITMAP_TEXT_SMALL
#define GSCHEM_TRANSISTOR_BITMAP        GSCHEM_BITMAP_TRANSISTOR_LARGE
#define GSCHEM_TOP_2_BOTTOM_BITMAP      GSCHEM_BITMAP_TOP_2_BOTTOM_XLARGE
#define GSCHEM_UNDO_BITMAP              GSCHEM_BITMAP_UNDO_SMALL
#define GSCHEM_UNSELECT_BITMAP          GSCHEM_BITMAP_UNSELECT_TINY
#define GSCHEM_WARNING_BITMAP           GSCHEM_BITMAP_WARNING_XXXLARGE

/*!\defgroup geda-global-special-bitmaps Global Special Bitmaps
 * @{\par This group defines strings to reference special bitmap image
 */

/*! \def CLOSE_TOOLBAR_BITMAP     The little red "x" in each tool-bar */
#define CLOSE_TOOLBAR_BITMAP            "close_box.png"

/** @} endgroup geda-global-special-bitmaps */
/** @} endgroup geda-global-bitmaps */

#endif /* __GEDA_BITMAPS_H__ */
