/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_dialog_controls.h
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 *
 * Date: Aug, 22, 2012
 * Contributing Author: Wiley Edward Hill
 *
*/
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/17/12 |  Inital release.
 * ------------------------------------------------------------------
 * WEH | 11/03/12 |  Added macros GEDA_SWITCH & GEDA_CALLBACK_SWITCH
 *                | to support routine with passing parameters and
 *                | having independent callbacks. Added GEDA_NUMERIC_SPIN
 *                | to support routines not using embed labels.
 * ------------------------------------------------------------------
 * WEH | 11/16/12 | Added GEDA_NEW_COMBO macro which is functionally
 *                | equivalent to GTK_NEW_COMBO but does not have the
 *                | localization so that the embed widgets, such as the
 *                | label, are accessible to implementation code.
 * ------------------------------------------------------------------
 * WEH | 12/12/12 | Added DialogFont "Monospace 13.3" and PANGO_R5_LABEL,
 *                | Changed GEDA_SWITCH and GTK_LABEL_HBOX so primary controls
 *                | use the new PANGO_R5_LABEL macro instead of GTK_R5_LABEL.
 * ------------------------------------------------------------------
 * WEH | 09/08/13 | Changed all macros to use g_object_set instead of gtk
 *                | _widget_show. Added GSCHEM_SWITCH macro for switches
 *                | in tables.
 * ------------------------------------------------------------------
 * WEH | 09/20/13 | Added GEDA_FRAME to extend macro library
 * ------------------------------------------------------------------
 * WEH | 09/27/13 | Replaced older gtk_tooltips_set_tip function with
 *                | newer gtk_widget_set_tooltip_text function.
 * ------------------------------------------------------------------
 * WEH | 10/06/13 | Relocated function macros to geda_gui_funcs.h
 *                |
 * ------------------------------------------------------------------
 * WEH | 06/23/14 | Replaced gtk_label with geda_label_new or variants.
 *                |
 * ------------------------------------------------------------------
 * WEH | 08/21/14 | Added EDA_BULB_x and corresponding cluster type for
 *                | labeless groups
 * ------------------------------------------------------------------
 * WEH | 09/21/14 | Renamed GEDA_SWITCH->EDA_SWITCH
 * ------------------------------------------------------------------
 * WEH | 03/01/15 | Add Macro GET_EDA_OBJECT
 * ------------------------------------------------------------------
 * WEH | 09/11/15 | Rename GTK_HOOKUP_OBJECT GEDA_HOOKUP_OBJECT
 * ------------------------------------------------------------------
 * WEH | 09/21/15 | Add Macro GetGedaComboActiveText, remove g_object_ref
 *                | from HOOKUP_GEDA_OBJECT_NO_REF, append GTK_ICALLBACK_
 *                | COMBO to GEDA_NEW_COMBO and GEDA_NEW_TEXT_ENTRY_COMBO
 * ------------------------------------------------------------------
 * WEH | 04/29/17 | Remove first argument of macros EDA_SWITCH, GSCHEM_SWITCH.
 *                | The dialog argument was not used by create_geda_switch.
 * ------------------------------------------------------------------
 * WEH | 05/13/17 | Go back to gtk_widget_show instead of g_object_set.
 * ------------------------------------------------------------------
 * WEH | 02/27/17 | Replace gtk_container_add with geda_container_add macro.
 *                | Rename GTK_CALLBACK_ENABLER-> GTK_CALLBACK_TOGGLED.
 * ------------------------------------------------------------------
 * WEH | 09/07/18 | Add Macro HD_ACTION_SEPARATOR.
 * ------------------------------------------------------------------
 * WEH | 04/23/19 | Rename macro PACK_BOX->PACK_START.
 * ------------------------------------------------------------------
 * WEH | 07/29/20 | Add Macro LOAD_GEDA_COMBO_STR, revise LOAD_COMBO_STR
 *                | to use inline code instead of calling load_combo_str.
 *                | Revise LOAD_GEDA_TEXT_COMBO to utilize gettext.
 * ------------------------------------------------------------------
*/

#pragma once

#ifndef GetEntryText
  #include "geda_gui_funcs.h"
#endif

/**
 * \file geda_dialog_controls.h
 *   \defgroup geda-dialog-controls gEDA Dialog controls
 * @{\par This group contains Macros for controls used in Dialogs
 *   \ingroup geda-globals
 */

/*! \def DialogFont Defines the default font used in gEDA dialogs */
#if defined (OS_WIN32_NATIVE)
#define DialogFont "Monospace 13.3"
#else
#define DialogFont "Liberation Mono Bold 14"
#endif

typedef struct
{
   const char *Widget;
   const char *Label;
   const char *Tip;
} WidgetStringData;

#define DECLARE_TOOPTIPS \
  GtkTooltips *tooltips; \
  tooltips = gtk_tooltips_new (); \
  tooltips = tooltips;

/* Access Macros for String Structures */
#define WIDGET(member)    DialogStrings[member].Widget
#define LABEL(member)     DialogStrings[member].Label
#define TOOLTIP(member)   DialogStrings[member].Tip
#define TAB_LABEL(member) DialogTabData[member].Label

#define _LABEL(member)     (gettext (LABEL (member)))
#define _TOOLTIP(member)   (gettext (TOOLTIP (member)))
#define _TAB_LABEL(member) (gettext (TAB_LABEL(member)))

/* Use this macro like  Debug_IMAGE(AttributesTab_vbox, _6); */
#define Debug_IMAGE(Parent, Number) \
  GtkWidget *BugImage##Number= create_pixmap ( "gschem-delete.xpm"); \
             gtk_widget_show(BugImage##Number); \
             gtk_box_pack_start (GTK_BOX (Parent), BugImage##Number, FALSE, FALSE, 0); \
             gtk_widget_set_tooltip_text ( BugImage##Number, _("Debugging This"));

#define NOT_BELOW_ZERO(padding) padding < 0 ? 0 : padding

/* Set Widget Values, see also geda_gui_macros.h */
#define GetGedaCombo( name) geda_combo_box_get_active (GEDA_COMBO_BOX(name##Combo))
#define SetGedaCombo( name, var) geda_combo_box_set_active (GEDA_COMBO_BOX(name##Combo), var);
#define GetGedaComboActiveText( name) geda_combo_box_text_get_active_text (GEDA_COMBO_BOX_TEXT(name##Combo))

//const char *depth = gtk_entry_get_text( GTK_ENTRY(GTK_COMBO(textureDepthCombo)->entry) );
#define SetCombo( name, var)  gtk_combo_box_set_active (GTK_COMBO_BOX(name##Combo), var);
#define SetSwitch( name, var) gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (name##Switch), var);
#define SetRadio( name, var) gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (name##Radio), var);

#define SetRadioGroup(group, var) geda_bulb_group_set_active_index(group##RadioGroup, var);
#define SetBulbGroup(group, var) x_dialog_bulb_group_set_active(group##RadioGroup, var);
#define SetSpin( name, var) gtk_spin_button_set_value (GTK_SPIN_BUTTON (name##Spin), var);

/* This macro helps reduce line length */
#define SetWidgetSize( widget, x_size, y_size) \
    gtk_widget_set_size_request (GTK_WIDGET (widget), x_size, y_size);

#define GET_SPIN_DVALUE(spinner) gtk_spin_button_get_value(GTK_SPIN_BUTTON (spinner))
#define GET_SPIN_IVALUE(spinner) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON (spinner))
#define GET_SWITCH_STATE(switch) gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (switch))

#define GET_EDA_OBJECT(name) \
    g_object_get_data ((GObject*)ThisDialog, WIDGET(name))

#define GEDA_HOOKUP_OBJECT(component, widget, name)       \
    g_object_set_data_full ((GObject*)component, name,   \
    g_object_ref (widget), (GDestroyNotify) g_object_unref);

#define HOOKUP_GEDA_OBJECT(name, type) \
    gtk_widget_set_tooltip_text ( name##type, _(TOOLTIP (name))); \
    g_object_set_data_full ((GObject*)ThisDialog, WIDGET(name), \
    g_object_ref(name##type), (GDestroyNotify) g_object_unref);

#define HOOKUP_GEDA_OBJECT_NO_REF(name, type) \
    gtk_widget_set_tooltip_text ( name##type, _(TOOLTIP (name))); \
    g_object_set_data ((GObject*)ThisDialog, WIDGET(name), name##type);

/* Tabs Related (not Tables) */
#define GTK_START_TAB(name) \
  GtkWidget *name##Tab_vbox = gtk_vbox_new (FALSE, 0); \
  gtk_widget_show(name##Tab_vbox);     \
  geda_container_add (notebook, name##Tab_vbox);

#define GTK_END_TAB(name) \
  GtkWidget *name##Tab = geda_visible_label_new (_TAB_LABEL(name));     \
  gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook), \
                              gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), name), \
                              name##Tab);

/* Widget Size and Padding Related */
#define SET_WIDGET_SIZE( name, xsize, ysize) \
        gtk_widget_set_size_request (name, xsize, ysize);

#define SET_WIDGET_PADDING( name, xpad, ypad) \
        gtk_misc_set_padding (GTK_MISC (name), xpad, ypad);

/* Section Division Related  */
#define SEPARATOR(parent, suffix, dir, isexpandable, isfilled, xpad, ypad) { \
        GtkWidget *dir##Separator##suffix; \
        dir##Separator##suffix = gtk_##dir##separator_new (); \
        gtk_widget_show(dir##Separator##suffix); \
        gtk_box_pack_start (GTK_BOX (parent), dir##Separator##suffix, isexpandable, isfilled, 0); \
        gtk_widget_set_size_request (dir##Separator##suffix, xpad, ypad); \
}

#define H_SEPARATOR(parent, suffix) \
        SEPARATOR (parent, suffix, h, FALSE, TRUE, 0, 0)

#define HX_SEPARATOR(parent, suffix) \
        SEPARATOR (parent, suffix, h, TRUE, TRUE, 0, 0)

#define HD_SEPARATOR(parent, suffix) \
        SEPARATOR (parent, suffix, h, FALSE, TRUE, 0, DEFAULT_SEPERATOR_SPACING)

#define HD_ACTION_SEPARATOR(parent) \
        SEPARATOR (parent, Actions, h, FALSE, TRUE, 0, DEFAULT_SEPERATOR_SPACING)

#define HYP_SEPARATOR(parent, suffix, ypad) \
        SEPARATOR (parent, suffix, h, FALSE, TRUE, 0, ypad)

#define HXXP_SEPARATOR(parent, suffix, xpad) \
        SEPARATOR (parent, suffix, h, TRUE, TRUE, xpad, 0)

#define HXYP_SEPARATOR(parent, suffix, ypad) \
        SEPARATOR (parent, suffix, h, TRUE, TRUE, 0, ypad)

#define V_SEPERATOR(parent, suffix) \
        SEPARATOR (parent, suffix, h, FALSE, TRUE, 0, 0)

#define VX_SEPERATOR(parent, suffix) \
        SEPARATOR (parent, suffix, v, TRUE, TRUE, 0, 0)

#define VD_SEPERATOR(parent, suffix) \
        SEPARATOR (parent, suffix, v, FALSE, TRUE, DEFAULT_SEPERATOR_SPACING, 0)

#define VYP_SEPERATOR(parent, suffix, ypad) \
        SEPARATOR (parent, suffix, v, FALSE, TRUE, 0, ypad)

#define VXP_SEPERATOR(parent, suffix, xpad) \
        SEPARATOR (parent, suffix, v, FALSE, TRUE, xpad, 0)

#define VXYP_SEPERATOR(parent, suffix, ypad) \
        SEPARATOR (parent, suffix, v, TRUE, TRUE, 0, ypad)

#define VXXP_SEPERATOR(parent, suffix, xpad) \
        SEPARATOR (parent, suffix, v, TRUE, TRUE, xpad, 0)

/* Box Widgets */
/* Note: Boxes do not have local braces and this allows for post macro modifiers */

/* Level 1 Boxes - use in Major Widget Controls (MWC) */
#define BASE_BOX( name, type, homo, spacing) \
        name##_##type##box = gtk_##type##box_new (homo, NOT_BELOW_ZERO (spacing)); \
        gtk_widget_show(name##_##type##box);

#define LOCAL_BASE_BOX( name, type, homo, spacing) \
        GtkWidget *name##_##type##box; \
        BASE_BOX( name, type, homo, spacing)

#define GTK_NEW_hBOX(name, homo, spacing) LOCAL_BASE_BOX(name, h, homo, spacing)
#define GTK_NEW_vBOX(name, homo, spacing) LOCAL_BASE_BOX(name, v, homo, spacing)

/* End Base Boxes */

#define PACK_START(box, item, isexpandable, isfilled, spacing) \
        gtk_box_pack_start ((GtkBox*)box, (GtkWidget*)item, isexpandable, isfilled, NOT_BELOW_ZERO (spacing));

#define PACK_hBOX(box, item, isexpandable, isfilled, spacing) \
        PACK_START (box##_hbox, item, isexpandable, isfilled, spacing)

#define PACK_vBOX(box, item, isexpandable, isfilled, spacing) \
        PACK_START (box##_vbox, item, isexpandable, isfilled, spacing)

/* These are used by CSECTION_OPTIONS */
#define HPACK_START PACK_hBOX
#define VPACK_START PACK_vBOX

/* End Pack Boxes */

/* Level 2 Box - these combine Box and Packing */
#define NEW_HCONTROL_BOX(parent, name, spacing); \
        BASE_BOX( name, h, FALSE, DEFAULT_WIDGET_SPACING) \
        PACK_START(parent, name##_hbox, FALSE, TRUE, spacing)

//All Parameters
#define SECTION_BOX(parent, name, type, homo, spacing, isexpandable, isfilled) \
        LOCAL_BASE_BOX (name, type, homo,  DEFAULT_WIDGET_SPACING) \
        PACK_START(parent, name##_##type##box, isexpandable, isfilled, spacing)

//Eliminate type
#define H_SECTION(parent, name, homo, spacing, isexpandable, isfilled) SECTION_BOX (parent, name, h, homo, spacing, isexpandable, isfilled)
#define V_SECTION(parent, name, homo, spacing, isexpandable, isfilled) SECTION_BOX (parent, name, v, homo, spacing, isexpandable, isfilled)

//Eliminate Filled
#define HF_SECTION(parent, name, homo, spacing, isexpandable) H_SECTION(parent, name, homo, spacing, isexpandable, FALSE)
#define VF_SECTION(parent, name, homo, spacing, isexpandable) V_SECTION(parent, name, homo, spacing, isexpandable, FALSE)

//Eliminate Expand & homo - Note this assume homogeneously filled
#define HXF_SECTION(parent, name, spacing) H_SECTION(parent, name, TRUE, spacing, TRUE, TRUE)
#define VXF_SECTION(parent, name, spacing) V_SECTION(parent, name, TRUE, spacing, TRUE, TRUE)

//Eliminate spacing - Note we use V spacing in H-box, and H spacing in V-Box
#define HXSECTION(parent, name)HXF_SECTION(parent, name, DIALOG_V_SPACING)
#define VXSECTION(parent, name)VXF_SECTION(parent, name, DIALOG_H_SPACING)

//Eliminate Expand & homo - Note these are not homogeneously filled
#define HPSECTION(parent, name, spacing) \
        HF_SECTION(parent, name, FALSE, spacing, FALSE) \

#define VPSECTION(parent, name, spacing) \
        VF_SECTION(parent, name, FALSE, spacing, FALSE) \

/* Packed Boxes for use in-line, staring with "easy mode" */
#define HSECTION(parent, name) HPSECTION(parent, name, DIALOG_V_SPACING)
#define VSECTION(parent, name) VPSECTION(parent, name, DIALOG_H_SPACING)

// More Controllable Section Boxes
#define HXPSECTION(parent, name, pad) \
        H_SECTION(parent, name, FALSE, DIALOG_V_SPACING, FALSE, TRUE) \
        SET_WIDGET_PADDING ( name##_hbox, pad, 0)

#define VXPSECTION(parent, name, pad) \
        V_SECTION(parent, name, FALSE, DIALOG_H_SPACING, FALSE, TRUE) \
        SET_WIDGET_PADDING ( name##_hbox, pad, 0)

#define HYPSECTION(parent, name, pad) \
        H_SECTION(parent, name, FALSE, DIALOG_V_SPACING, FALSE, TRUE) \
        SET_WIDGET_PADDING ( name##_hbox, 0, pad)

#define VYPSECTION(parent, name, pad) \
        V_SECTION(parent, name, FALSE, DIALOG_H_SPACING, FALSE, TRUE) \
        SET_WIDGET_PADDING ( name##_vbox, 0, pad)

#define HZSECTION(parent, name, xsize, ysize) \
        H_SECTION(parent, name, FALSE, DIALOG_V_SPACING, FALSE, TRUE) \
        SET_WIDGET_SIZE ( name##_hbox, xsize, ysize)

#define HZPSECTION(parent, name, xsize, ysize, xpad, ypad) \
        HZSECTION (parent, name, xsize, ysize) \
        SET_WIDGET_PADDING ( name##_hbox, xpad, ypad)

#define VZSECTION(parent, name, xsize, ysize) \
        V_SECTION(parent, name, FALSE, DIALOG_H_SPACING, FALSE, TRUE) \
        SET_WIDGET_SIZE ( name##_vbox, xsize, ysize)

#define VZPSECTION(parent, name, xsize, ysize, xpad, ypad) \
        VZSECTION (parent, name, xsize, ysize)             \
        SET_WIDGET_PADDING ( name##_vbox, xpad, ypad)

#define VZXSECTION(parent, name, ysize, vspacing, ispacing ) \
        LOCAL_BASE_BOX (name, v, FALSE, vspacing)            \
        PACK_START(parent, name##_vbox, TRUE, TRUE, ispacing)  \
        SET_WIDGET_SIZE ( name##_vbox, -1, ysize)

/*  Label Widget */
#define GEDA_NEW_LABEL(name, isexpandable, isfilled, type) \
        name##Label = geda_visible_label_new (_(LABEL (name)));    \
                      PACK_START(name##_##type##box, name##Label, isexpandable, \
                               isfilled, DEFAULT_WIDGET_SPACING)

#define GEDA_PADDED_LABEL(name, hpad, vpad, isexpandable, isfilled, type) \
        GEDA_NEW_LABEL (name, isexpandable, isfilled, type);              \
        SET_WIDGET_PADDING( name##Label, hpad, vpad);

#define GTK_C0_LABEL(name) GEDA_PADDED_LABEL (name, 0, 0, TRUE, TRUE, v); \
        geda_label_widget_set_justify (name##Label, GTK_JUSTIFY_CENTER);

#define GTK_R5_LABEL(name) GEDA_PADDED_LABEL (name, 5, 0, FALSE, FALSE, h) \
        geda_label_widget_set_justify (name##Label, GTK_JUSTIFY_RIGHT);

#define GTK_R10_LABEL(name) GEDA_PADDED_LABEL (name, 10, 0, FALSE, FALSE, h) \
        geda_label_widget_set_justify (name##Label, GTK_JUSTIFY_RIGHT);

#define GTK_RS_LABEL(name, spacing) \
        GEDA_PADDED_LABEL (name, spacing, 0, FALSE, FALSE, h) \
        geda_label_widget_set_justify (name##Label, GTK_JUSTIFY_RIGHT);

#define PANGO_R5_LABEL(name) GEDA_PADDED_LABEL (name, 5, 0, FALSE, FALSE, h) \
        geda_label_widget_set_justify (name##Label, GTK_JUSTIFY_RIGHT); \
        { \
          PangoFontDescription *font_desc = pango_font_description_from_string (DialogFont); \
          gtk_widget_modify_font (name##Label, font_desc); \
          pango_font_description_free (font_desc); \
        }

/* Combine Label and Box */
#define GTK_LABEL_HBOX(parent, name, spacing)  \
        NEW_HCONTROL_BOX (parent, name, spacing); \
        PANGO_R5_LABEL(name)

#define CSECTION_OPTIONS(parent, name, ysize, pad, type) \
          type##ZSECTION(parent, name##Options, -1, ysize) \
          GtkWidget *name##Label=geda_visible_label_new (_(LABEL (name))); \
          gtk_widget_set_tooltip_text ( name##Label, _(TOOLTIP (name))); \
          type##PACK_START (name##Options, name##Label, FALSE, FALSE, pad); \
          geda_label_widget_set_justify (name##Label, GTK_JUSTIFY_CENTER);

#define GTK_NEW_SCROLL( parent, name, spacing, xsize, ysize, bars, theme) \
          GtkWidget *name = gtk_scrolled_window_new (NULL, NULL); \
          gtk_widget_show (name); \
          gtk_box_pack_start (GTK_BOX (parent), name, FALSE, TRUE, spacing); \
          gtk_widget_set_size_request (name, xsize, ysize); \
          gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (name), GTK_POLICY_NEVER, bars); \
          gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (name), theme);

#define GTK_NEW_SCROLL_OUT( parent, name, spacing, xsize, ysize, bars) \
        GTK_NEW_SCROLL( parent, name, spacing, xsize, ysize, bars, GTK_SHADOW_ETCHED_OUT)

/* Frame Widget DIALOG_V_SPACING*/
#define GEDA_FRAME(parent, name, width, height, xalign, yalign, space) \
        /* Create outer alignment to hold the Frame */ \
        GtkWidget *name##Align1 = gtk_alignment_new (xalign, yalign, 0, 0); \
        gtk_widget_set_size_request (name##Align1, width, height + space); \
        geda_container_add (parent, name##Align1);  \
        gtk_widget_show (name##Align1);    \
        /* Create a Frame and put in the outer alignment */ \
        GtkWidget *name##Frame = gtk_frame_new (_(#name)); \
        gtk_widget_set_size_request (name##Frame, width, height); \
        geda_container_add (name##Align1, name##Frame); \
        gtk_widget_show (name##Frame); \
        /* Create inner alignment and put inside the Frame */ \
        GtkWidget *name##Align2 = gtk_alignment_new (0, 0, 0, 0); \
        geda_container_add (name##Frame, name##Align2);  \
        gtk_widget_show (name##Align2); \
        /* Create a horizontal box and put inside the inner alignment */ \
        GtkWidget *name##_hbox = gtk_hbox_new (FALSE, NOT_BELOW_ZERO (DIALOG_H_SPACING)); \
        geda_container_add (name##Align2, name##_hbox); \
        gtk_widget_show (name##_##hbox); \
        gtk_box_set_spacing(GTK_BOX(name##_##hbox), space); \

/* Widget Callbacks */
#define GTK_CONNECT_CALLBACK(name, signal, function, data) \
        g_signal_connect (name, signal, G_CALLBACK(function), data);

#define GTK_CALLBACK_TOGGLED(name, function, target) \
        GTK_CONNECT_CALLBACK(name, "toggled", function, target)

#define GTK_ICALLBACK(name, signal, function, data) \
        g_signal_connect (name, signal, G_CALLBACK(function), \
                         (void*)(unsigned int)(data));

#define GTK_ICALLBACK_COMBO(name) \
        GTK_ICALLBACK (name##Combo, "changed", Combo_Responder, name)

#define GTK_ICALLBACK_BUTT(name) \
        GTK_ICALLBACK (name##Butt, "button_press_event", Butt_Responder, name)

#define GTK_ICALLBACK_CBUTT(name) \
        GTK_ICALLBACK (name##Butt, "button_press_event", Color_Butt_Responder, name)

#define GTK_ICALLBACK_RADIO(name, function) \
        GTK_ICALLBACK (name##Radio, "pressed", function, name)

#define GTK_CALLBACK_RTRIAD(name, R1, R2, R3, function, data) \
        GTK_CONNECT_CALLBACK (name##R1, "pressed", function, data) \
        GTK_CONNECT_CALLBACK (name##R2, "pressed", function, data) \
        GTK_CONNECT_CALLBACK (name##R3, "pressed", function, data )

#define GTK_ICALLBACK_SWITCH(name) \
        GTK_ICALLBACK (name##Switch, "toggled", Switch_Responder, name)

#define GEDA_CALLBACK_SWITCH(name, func, data) \
        GTK_CONNECT_CALLBACK (name##Switch, "toggled", func, data)

/* Major Controls/Widgets */
/* The controls that have "user values" MUST be pre-defined, the macros
 * define the  other widgets locally (and are therefore not accessible
 * directly.) So the Control can be disabled but their labels can not.
 * For info on labels and tooltips strings see the dialog header files
 * and Access Macros for String Structures above.
*/
#define GTK_NEW_ARROW(parent, name, dir, style, isexpandable, isfilled, spacing ) \
        name##Arrow = gtk_arrow_new ( dir, style); \
        gtk_widget_show (name##Arrow); \
        gtk_box_pack_start (GTK_BOX (parent), name##Arrow, isexpandable, isfilled, spacing);

#define GTK_STD_ARROW(parent, name, dir, style) \
        GTK_NEW_ARROW(parent, name, dir, style, TRUE, TRUE, DIALOG_BUTTON_SPACING)

#define GTK_NEW_BUTTON(parent, name, isexpandable, isfilled, focus, spacing) \
        name##Butt = gtk_button_new_with_mnemonic (_(LABEL (name)));  \
        gtk_widget_show ( name##Butt); \
        gtk_box_pack_start (GTK_BOX (parent), name##Butt, isexpandable, isfilled, spacing); \
        gtk_button_set_focus_on_click (GTK_BUTTON (name##Butt), focus); \
        HOOKUP_GEDA_OBJECT(name, Butt)

#define GTK_STD_BUTTON(parent, name) \
        GTK_NEW_BUTTON (parent, name, TRUE, TRUE, TRUE, DIALOG_BUTTON_SPACING) \
        GTK_ICALLBACK_BUTT (name)

#define GEDA_COLOR_BUTTON(parent, name, width, height, spacing) \
        GtkWidget *name##_hbox=NULL;                    \
        GtkWidget *name##Label=NULL;                    \
        NEW_HCONTROL_BOX (parent, name, spacing)        \
        PANGO_R5_LABEL (name)                           \
        name##Butt = gtk_color_button_new ();  \
        gtk_color_button_set_title((GtkColorButton*)name##Butt, _(WIDGET (name)));\
        SET_WIDGET_SIZE(name##Butt, width, height) \
        gtk_widget_show (name##Butt); \
        gtk_box_pack_start (GTK_BOX ( name##_hbox), name##Butt, FALSE, FALSE, DIALOG_BUTTON_SPACING); \
        gtk_widget_set_tooltip_text ( name##Butt, _(TOOLTIP (name))); \
        GTK_ICALLBACK_CBUTT (name)

#define GTK_NEW_CHECKBOX(parent, name) \
        name##CheckBox = gtk_check_button_new_with_mnemonic (_(LABEL (name))); \
        gtk_widget_show (name##CheckBox); \
        gtk_box_pack_start (GTK_BOX (parent), name##CheckBox, FALSE, FALSE, 0);

#define GTK_NEW_COMBO(parent, name, width, hpad)    \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, hpad); /* create hbox and label */     \
        name##Combo = gtk_combo_box_entry_new_text(); \
        gtk_widget_show ( name##Combo); \
        PACK_hBOX(name, name##Combo, FALSE, FALSE, 0) \
        SET_WIDGET_SIZE ( name##Combo, width, 32) \
        HOOKUP_GEDA_OBJECT(name, Combo) \
        GTK_ICALLBACK_COMBO (name)

/* These macros use GedaComboBox and GedaComboBoxText, NOT derived from Gtk */

#define GEDA_NEW_COMBO(parent, name, width, hpad)    \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, hpad); /* create hbox and label */     \
        name##Combo = geda_combo_box_new(); \
        gtk_widget_show (name##Combo); \
        PACK_hBOX(name, name##Combo, FALSE, FALSE, 0) \
        SET_WIDGET_SIZE ( name##Combo, width, 34) \
        HOOKUP_GEDA_OBJECT(name, Combo) \
        GTK_ICALLBACK_COMBO (name)

#define GEDA_NEW_TEXT_COMBO(parent, name, width, hpad)    \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, hpad); /* create hbox and label */     \
        name##Combo = geda_combo_box_text_new(); \
        gtk_widget_show (name##Combo); \
        PACK_hBOX(name, name##Combo, FALSE, FALSE, 0) \
        SET_WIDGET_SIZE ( name##Combo, width, 34) \
        HOOKUP_GEDA_OBJECT(name, Combo) \
        GTK_ICALLBACK_COMBO (name)

#define GEDA_NEW_TEXT_ENTRY_COMBO(parent, name, width, hpad)    \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, hpad); /* create hbox and label */     \
        name##Combo = geda_combo_box_text_new_with_entry(); \
        gtk_widget_show (name##Combo); \
        PACK_hBOX(name, name##Combo, FALSE, FALSE, 0) \
        SET_WIDGET_SIZE ( name##Combo, width, 34) \
        HOOKUP_GEDA_OBJECT(name, Combo) \
        GTK_ICALLBACK_COMBO (name)

#define GEDA_NEW_LIST_ENTRY_COMBO(parent, name, width, hpad)    \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, hpad); /* create hbox and label */     \
        name##Combo = geda_combo_box_text_list_new(); \
        gtk_widget_show (name##Combo); \
        PACK_hBOX(name, name##Combo, FALSE, FALSE, 0) \
        SET_WIDGET_SIZE ( name##Combo, width, 34) \
        HOOKUP_GEDA_OBJECT(name, Combo)

#define LOAD_GEDA_TEXT_COMBO(name, text) \
        geda_combo_box_text_append (GEDA_COMBO_BOX_TEXT (name##Combo), _(text));

#define LOAD_GEDA_COMBO_STR(name, strings) { \
  int i=0; \
  while (strings[i]) { LOAD_GEDA_TEXT_COMBO (name, strings[i++]);} }

#define GTK_LOAD_COMBO(name, text) gtk_combo_box_append_text (GTK_COMBO_BOX (name##Combo), _(text));
#define LOAD_STD_COMBO(name, text) gtk_combo_box_append_text (GTK_COMBO_BOX (name##Combo), text);

#define LOAD_COMBO_STR(name, strings) { \
  int i=0; \
  while (list[i]) { GTK_LOAD_COMBO (name, strings[i++]);} }

#define LOAD_COMBO_GL(name, glist)gtk_combo_set_popdown_strings (GTK_COMBO (name##Combo), glist);

/* Radio Widget Controls */

#define DECLARE_RADIO(name)static GtkWidget *name##Radio=NULL;

#define DECLARE_RADIO_TRIAD(group, R1, R2, R3) \
        GSList *group##RadioGroup = NULL; \
        DECLARE_RADIO(group##R1) \
        DECLARE_RADIO(group##R2) \
        DECLARE_RADIO(group##R3)

#define DECLARE_QUAD_RADIO(group, R1, R2, R3, R4) \
        DECLARE_RADIO_TRIAD (group, R1, R2, R3) \
        DECLARE_RADIO(group##R4)

#define GEDA_RADIO_GROUP( group, dir) \
        GSList *group##Group = NULL; \
        LOCAL_BASE_BOX(group##Group, dir, TRUE, 0) \
        PACK_START(group##_hbox, group##Group##_##dir##box, FALSE, FALSE, 0)

#define GTK_RADIO_GROUP( group, dir) \
        GSList *group##Group = NULL; \
        GtkWidget *group##Label=NULL;         /* define Label */ \
        GEDA_PADDED_LABEL (group, 5, 0, FALSE, FALSE, h); \
        gtk_widget_set_tooltip_text ( group##Label, _(TOOLTIP (group)));  \
        LOCAL_BASE_BOX(group##Group, dir, TRUE, 0) \
        PACK_START(group##_hbox, group##Group##_##dir##box, FALSE, TRUE, DEFAULT_WIDGET_SPACING)

#define GTK_RADIO( group, name, dir) \
        name##Radio = gtk_radio_button_new_with_mnemonic (group##Group, _(LABEL (name))); \
        gtk_widget_show (name##Radio); \
        gtk_box_pack_start (GTK_BOX (group##Group_##dir##box), name##Radio, FALSE, FALSE, 0); \
        group##Group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (name##Radio)); \
        HOOKUP_GEDA_OBJECT(name, Radio)

/* Note that if Dialog uses EDA_BULB  then don't use show_all short cut, use
 * traditional g_object_set(widget) because both the off and on images are
 * inside the controls, show_all will cause both images to be displayed.
 * One might also consider not defining groups for Bulbs, radio groups are a
 * MAJOR hassle with GTK-2. Since button callbacks must manage the images it
 * would be much easier to just manage the button states (while managing images
 * rather than deal with GTK-2 radio groups.
 */
#define GEDA_BASE_BULB( group, name, dir) \
        name##Radio = gtk_radio_button_new(group##Group); \
        gtk_widget_show (name##Radio); \
        gtk_box_pack_start (GTK_BOX (group##Group_##dir##box), name##Radio, FALSE, FALSE, 0); \
        group##Group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (name##Radio)); \
        gtk_toggle_button_set_inconsistent (GTK_TOGGLE_BUTTON (name##Radio), TRUE); \
        gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (name##Radio), FALSE); /* turn off the indicator, ie circle */ \
        gtk_button_set_relief(GTK_BUTTON (name##Radio), GTK_RELIEF_NONE); \
        gtk_button_set_focus_on_click(GTK_BUTTON (name##Radio), FALSE); { \
        GtkWidget *alignment;\
        GtkWidget *hbox; \
        GtkWidget *LightOn; \
        GtkWidget *LightOff; \
        alignment = gtk_alignment_new (0, 0, 1, 0);                    /* Create new Alignment Widget */ \
        gtk_widget_show (alignment); \
        geda_container_add (name##Radio, alignment);                   /* Put Alignment Widget Inside the Radio */ \
        hbox = gtk_hbox_new (FALSE, 2);                                /* Create new Box container */ \
        gtk_widget_show (hbox); \
        geda_container_add (alignment, hbox);                          /* Put box container inside the Alignment */ \
        LightOn = x_dialog_get_bulb_image(TRUE); \
        gtk_box_pack_start (GTK_BOX (hbox), LightOn, FALSE, FALSE, 0); /* Put both images inside box container */ \
        GEDA_HOOKUP_OBJECT (ThisDialog, LightOn, "On"); \
        LightOff = x_dialog_get_bulb_image(FALSE); \
        gtk_widget_show (LightOff); \
        gtk_box_pack_start (GTK_BOX (hbox), LightOff, FALSE, FALSE, 0); \
        GEDA_HOOKUP_OBJECT (ThisDialog, LightOff, "Off"); \
        GtkWidget *name##Label = geda_visible_label_new (_(LABEL (name))); \
        gtk_box_pack_start (GTK_BOX (hbox), name##Label, FALSE, FALSE, 0); \
        gtk_misc_set_padding (GTK_MISC (name##Label), 0, 0); }\
        HOOKUP_GEDA_OBJECT(name, Radio)

#define GTK_BULB( group, name, dir) \
        GEDA_BASE_BULB( group, name, dir) \
        GTK_ICALLBACK_RADIO (name, Radio_Responder)

#define EDA_BULB( group, name, dir) \
        GEDA_BASE_BULB( group, name, dir) \
        GTK_ICALLBACK_RADIO (name, Radio_Responder)

/* These are labeless because EDA_BULB_ type all use GEDA_RADIO_GROUP */
#define EDA_BULB_TRIAD(parent, group, dir, spacing, R1, R2, R3, Default) \
        HPSECTION (parent, group, spacing); \
        GEDA_RADIO_GROUP (group, dir);      \
        EDA_BULB (group, group##R1, dir);  \
        EDA_BULB (group, group##R2, dir);  \
        EDA_BULB (group, group##R3, dir);  \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        x_dialog_set_bulb_on(group##Default##Radio); \
        group##RadioGroup=group##Group;

#define GEDA_RADIO_TRIAD(parent, group, dir, spacing, R1, R2, R3, Default) \
        HPSECTION (parent, group, spacing); \
        GEDA_RADIO_GROUP (group, dir);     { \
        GTK_RADIO (group, group##R1, dir);  \
        GTK_RADIO (group, group##R2, dir);  \
        GTK_RADIO (group, group##R3, dir);  \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        group##RadioGroup=group##Group; \
}

#define GEDA_QUAD_BULB(parent, group, dir, spacing, R1, R2, R3, R4, Default) \
        HPSECTION (parent, group, spacing); \
        GEDA_RADIO_GROUP (group, dir);     { \
        EDA_BULB (group, group##R1, dir);  \
        EDA_BULB (group, group##R2, dir);  \
        EDA_BULB (group, group##R3, dir);  \
        EDA_BULB (group, group##R4, dir);  \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        x_dialog_set_bulb_on(group##Default##Radio); \
        group##RadioGroup=group##Group; \
}

#define GEDA_QUAD_RADIO(parent, group, dir, spacing, R1, R2, R3, R4, Default) \
        HPSECTION (parent, group, spacing); \
        GEDA_RADIO_GROUP (group, dir);     { \
        GTK_RADIO (group, group##R1, dir);  \
        GTK_RADIO (group, group##R2, dir);  \
        GTK_RADIO (group, group##R3, dir);  \
        GTK_RADIO (group, group##R4, dir);  \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        group##RadioGroup=group##Group; \
}

/* These have label because GTK_BULB_ types all use GTK_RADIO_GROUP */
#define GTK_BULB_TRIAD(parent, group, dir, spacing, R1, R2, R3, Default) \
        HPSECTION (parent, group, spacing); \
        GTK_RADIO_GROUP (group, dir); { \
        GTK_BULB (group, group##R1, dir); \
        GTK_BULB (group, group##R2, dir); \
        GTK_BULB (group, group##R3, dir); \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        x_dialog_set_bulb_on(group##Default##Radio); \
        group##RadioGroup=group##Group; \
}

#define GTK_RADIO_TRIAD(parent, group, dir, spacing, R1, R2, R3, Default) { \
        HPSECTION (parent, group, spacing); \
        GTK_RADIO_GROUP (group, dir); \
        GTK_RADIO (group, group##R1, dir); \
        GTK_RADIO (group, group##R2, dir); \
        GTK_RADIO (group, group##R3, dir); \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        group##RadioGroup=group##Group; \
}

#define GTK_QUAD_BULB(parent, group, dir, spacing, R1, R2, R3, R4, Default) { \
        HPSECTION (parent, group, spacing); \
        GTK_RADIO_GROUP (group, dir); \
        GTK_BULB (group, group##R1, dir); \
        GTK_BULB (group, group##R2, dir); \
        GTK_BULB (group, group##R3, dir); \
        GTK_BULB (group, group##R4, dir); \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        x_dialog_set_bulb_on(group##Default##Radio); \
        group##RadioGroup=group##Group; \
}
#define GTK_QUAD_RADIO(parent, group, dir, spacing, R1, R2, R3, R4, Default) { \
        HPSECTION (parent, group, spacing); \
        GTK_RADIO_GROUP (group, dir); \
        GTK_RADIO (group, group##R1, dir); \
        GTK_RADIO (group, group##R2, dir); \
        GTK_RADIO (group, group##R3, dir); \
        GTK_RADIO (group, group##R4, dir); \
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (group##Default##Radio), TRUE); \
        group##RadioGroup=group##Group; \
}

/* Clusters of bulbs without a group label */
#define GEDA_V_BULB_TRIAD(parent, group, spacing, R1, R2, R3, Default) \
        GTK_BULB_TRIAD (parent, group, v, spacing, R1, R2, R3, Default)

#define GEDA_V_RADIO_TRIAD(parent, group, spacing, R1, R2, R3, Default) \
        GEDA_RADIO_TRIAD (parent, group, v, spacing, R1, R2, R3, Default)

#define GEDA_V_QUAD_BULB(parent, group, spacing, R1, R2, R3, R4, Default) \
        GEDA_QUAD_BULB (parent, group, v, spacing, R1, R2, R3, R4, Default)

#define GEDA_V_QUAD_RADIO(parent, group, spacing, R1, R2, R3, R4, Default) \
        GEDA_QUAD_RADIO (parent, group, v, spacing, R1, R2, R3, R4, Default)

#define GEDA_H_BULB_TRIAD(parent, group, R1, R2, R3, Default) \
        EDA_BULB_TRIAD (parent, group, h, DIALOG_V_SPACING, R1, R2, R3, Default)

#define GEDA_H_RADIO_TRIAD(parent, group, R1, R2, R3, Default) \
        GEDA_RADIO_TRIAD (parent, group, h, DIALOG_V_SPACING, R1, R2, R3, Default)

#define GEDA_H_QUAD_BULB(parent, group, spacing, R1, R2, R3, R4, Default) \
        GEDA_QUAD_BULB (parent, group, h, spacing, R1, R2, R3, R4, Default)

#define GEDA_H_QUAD_RADIO(parent, group, spacing, R1, R2, R3, R4, Default) \
        GEDA_QUAD_RADIO (parent, group, h, spacing, R1, R2, R3, R4, Default)

/* Clusters of bulbs with a group label */
#define GTK_V_BULB_TRIAD(parent, group, spacing, R1, R2, R3, Default) \
        GTK_BULB_TRIAD (parent, group, v, spacing, R1, R2, R3, Default)

#define GTK_V_RADIO_TRIAD(parent, group, spacing, R1, R2, R3, Default) \
        GTK_RADIO_TRIAD (parent, group, v, spacing, R1, R2, R3, Default)

#define GTK_V_QUAD_BULB(parent, group, spacing, R1, R2, R3, R4, Default) \
        GTK_QUAD_BULB (parent, group, v, spacing, R1, R2, R3, R4, Default)

#define GTK_V_QUAD_RADIO(parent, group, spacing, R1, R2, R3, R4, Default) \
        GTK_QUAD_RADIO (parent, group, v, spacing, R1, R2, R3, R4, Default)

#define GTK_H_BULB_TRIAD(parent, group, R1, R2, R3, Default) \
        GTK_BULB_TRIAD (parent, group, h, DIALOG_V_SPACING, R1, R2, R3, Default)

#define GTK_H_RADIO_TRIAD(parent, group, R1, R2, R3, Default) \
        GTK_RADIO_TRIAD (parent, group, h, DIALOG_V_SPACING, R1, R2, R3, Default)

#define GTK_H_QUAD_BULB(parent, group, spacing, R1, R2, R3, R4, Default) \
        GTK_QUAD_BULB (parent, group, h, spacing, R1, R2, R3, R4, Default)

#define GTK_H_QUAD_RADIO(parent, group, spacing, R1, R2, R3, R4, Default) \
        GTK_QUAD_RADIO (parent, group, h, spacing, R1, R2, R3, R4, Default)

/* Horizontal Radio Triad in Vertical parent */
#define GTK_HV_BULB_TRIAD(parent, group, R1, R2, R3, Default) \
        GTK_BULB_TRIAD (parent, group, h, DIALOG_H_SPACING, R1, R2, R3, Default)

#define GTK_HV_RADIO_TRIAD(parent, group, R1, R2, R3, Default) \
        GTK_RADIO_TRIAD (parent, group, h, DIALOG_H_SPACING, R1, R2, R3, Default)

/* ivalue : initial value */
#define GTK_NUMERIC_SPIN(parent, name, spacing, ivalue, minval, maxval)  { \
        double step = (maxval > 100 ? 5 : 1 ); \
        double page = (maxval > 100 ? 25 : 10 ); \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, spacing); /* create hbox and label */  \
        GtkAdjustment *name##Spin_adj = geda_adjustment_new (ivalue, minval, maxval, step, page, 0); \
        name##Spin = gtk_spin_button_new (name##Spin_adj, 1, 0); \
        gtk_widget_show (name##Spin); \
        SET_WIDGET_SIZE (name##Spin, -1, 33)  \
        PACK_hBOX(name, name##Spin, FALSE, TRUE, 0) \
        gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (name##Spin), TRUE); \
        HOOKUP_GEDA_OBJECT(name, Spin) \
}

#define GEDA_NUMERIC_SPIN(name, ivalue, minval, maxval) { \
        double step = (maxval > 100 ? 5 : 1 ); \
        double page = (maxval > 100 ? 25 : 10 ); \
                page = (maxval > 1000 ? 50 : 10 ); \
        GtkAdjustment *name##Spin_adj = geda_adjustment_new (ivalue, minval, maxval, step, page, 0); \
        name##Spin = gtk_spin_button_new (name##Spin_adj, 1, 0); \
        gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (name##Spin), TRUE); \
        gtk_entry_set_activates_default(GTK_ENTRY(name##Spin), TRUE); \
        gtk_widget_show (name##Spin); \
        HOOKUP_GEDA_OBJECT(name, Spin) \
}

#define EDA_SWITCH(parent, name, spacing, state)  {     \
        GtkWidget *name##_hbox=NULL;                    \
        GtkWidget *name##Label=NULL;                    \
        GtkWidget *name##Image=NULL;                    \
        NEW_HCONTROL_BOX (parent, name, spacing)        \
        PANGO_R5_LABEL (name)                           \
        name##Switch = create_geda_switch (name##_hbox, name##Image, state); \
        HOOKUP_GEDA_OBJECT(name, Switch) \
}
#define GSCHEM_SWITCH(table, name, left, top, state)  {     \
        GtkWidget *name##_hbox=NULL;                    \
        GtkWidget *name##Label=NULL;                    \
        GtkWidget *name##Image=NULL;                    \
        BASE_BOX( name, h, FALSE, DEFAULT_WIDGET_SPACING) \
        gtk_table_attach(GTK_TABLE(table), name##_hbox, left, left+1, top, top+1, GTK_SHRINK, GTK_FILL,0,0); \
        GTK_RS_LABEL (name, 2)                          \
        name##Switch = create_geda_switch (name##_hbox, name##Image, state); \
        HOOKUP_GEDA_OBJECT(name, Switch) \
}

#define GTK_SWITCH(parent, name, spacing, state) \
        EDA_SWITCH (parent, name, spacing, state) \
        GTK_ICALLBACK_SWITCH (name)

#define TOGGLE_SWITCH( switch ) { \
        GtkWidget *SwitchImage = get_geda_switch_image(GET_SWITCH_STATE (switch)); \
        gtk_button_set_image(GTK_BUTTON (switch), SwitchImage); \
}

#define GTK_TEXT_ENTRY(parent, name, hpad, itext)  {    \
        GtkWidget *name##_hbox=NULL; /* declare hbox widget (alias gint) */  \
        GtkWidget *name##Label=NULL;         /* declare Label */             \
        GTK_LABEL_HBOX (parent, name, hpad);    /* create hbox and label */  \
        name##Entry = gtk_entry_new (); \
        gtk_widget_show ( name##Entry); \
        PACK_hBOX(name, name##Entry, FALSE, FALSE, 0) \
        gtk_entry_set_text (GTK_ENTRY (name##Entry), _(itext)); \
        HOOKUP_GEDA_OBJECT(name, Entry) \
}

#define GTK_EDITITABLE(widget) \
        gtk_editable_select_region( GTK_EDITABLE (widget), 0, -1);

/* View Trees load_tree_view_##source (ThisDialog, GTK_TREE_VIEW(name##View), data);*/
#define GTK_VIEW_TREE( parent, name, data, source, xsize, ysize) \
        name##View = GTK_WIDGET( gtk_tree_view_new()); \
        gtk_widget_show (name##View); \
        geda_container_add (parent, name##View); \
        initialize_tree_View(GTK_TREE_VIEW(name##View), 0, 1, G_TYPE_STRING); \
        load_tree_view_##source (GTK_TREE_VIEW(name##View), data); \
        connect_list_view( ThisDialog, GTK_TREE_VIEW(name##View)); \
        SET_WIDGET_SIZE ( name##View, xsize, ysize)  \
        HOOKUP_GEDA_OBJECT(name, View)

/** @} endgroup geda-dialog-controls */