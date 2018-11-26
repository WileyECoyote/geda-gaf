/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file geda_module.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 17, 2013
 */
 /*! \warning Unless you really know what your doing - Don't do it.
  *           This file is included twice in module/geda.c AND twice
  *           in src/libgedathon.h" and is compiled differently each
  *           time the file loaded!
  */
#ifndef METHOD

#define MODULE_WAIT_INTERVAL      100   /* microseconds */
#define MAX_WAIT_FOR_MODULE        10

/* The next block is only seen by the MODULE file on the first pass */
#ifdef FIRST_PASS_METHODS  /* Macro Definition doubles as a flag */

#ifndef QUOTE_SYMBOL
#define QUOTE_SYMBOL(symbol) #symbol
#define ADD_QUOTE(...) QUOTE_SYMBOL(__VA_ARGS__)
#define ASSOCIATED_METHOD(method) ADD_QUOTE(method), do_##method
#endif

PyMODINIT_FUNC initgeda (void);
void initConstants(PyObject *module);
void initFunctions(PyObject *module);

static PyObject *do_unknown(PyObject *self, PyObject *args);

const char shutdown_docs[]           = N_("Close all files and deinitialize the library.");

const char append_symbol_path_docs[] = N_("Append a folder to the library search path.");
const char declare_local_sym_docs[]  = N_("Creates a gafrc file in the current directory.");

/* Page Object operations/Methods */
const char get_pages_docs[]          = N_("Retrieve list of all pages.");
const char get_active_page_docs[]    = N_("Get pointer to the active page.");
const char set_active_page_docs[]    = N_("Set the active page.");
const char is_page_modified_docs[]   = N_("Returns True if page has been modified and not saved.");
const char goto_page_docs[]          = N_("Set the active page, and changes current directory.");
const char open_page_docs[]          = N_("Open a page.");
const char close_page_docs[]         = N_("Close page.");
const char new_page_docs[]           = N_("Create a new empty page.");
const char rename_page_docs[]        = N_("Rename a page changes the filename.");
const char save_page_docs[]          = N_("Save the page by writing to storage.");
const char save_page_as_docs[]       = N_("Save the page using a new file name.");
const char save_all_pages_docs[]     = N_("Save all opened pages to storage.");

/* Page Level Object operations/Methods */
const char GedaCapsule_Type_docs[]   = N_("Returns True is object is a GedaCapsule, otherwise False.");
const char get_bounds_docs[]         = N_("Returns the rectangular boundaries of an object.");
const char get_object_docs[]         = N_("Retrieve an object from a GedaCapsule.");
const char get_objects_docs[]        = N_("Retrieve all objects belonging to a page.");
const char add_object_docs[]         = N_("Add an object to a page or another object.");
const char add_objects_docs[]        = N_("Add multiple objects to a page or another object.");
const char copy_object_docs[]        = N_("Make a copy an object.");
const char remove_object_docs[]      = N_("Remove object from a page.");
const char remove_objects_docs[]     = N_("Remove multiple objects from a page.");

const char delete_object_docs[]      = N_("Delete an object from a page.");
const char delete_objects_docs[]     = N_("Delete multiple object from a page.");
const char sync_object_docs[]        = N_("Remove object from a page.");

/* Object Constructor */
const char new_arc_docs[]            = N_("Create a new arc object.");
const char new_box_docs[]            = N_("Create a new box object.");
const char new_bus_docs[]            = N_("Create a new bus object.");
const char new_circle_docs[]         = N_("Create a new circle object.");
const char new_complex_docs[]        = N_("Create a new complex object.");
const char new_line_docs[]           = N_("Create a new line object.");
const char new_net_docs[]            = N_("Create a new net object.");
const char new_path_docs[]           = N_("Create a new path object.");
const char new_picture_docs[]        = N_("Create a new picture object.");
const char new_pin_docs[]            = N_("Create a new pin object.");
const char new_text_docs[]           = N_("Create a new text object.");
const char new_attrib_docs[]         = N_("Create a new attribute object.");

/* Base Object Methods */
const char object_help_docs[]        = N_("New object type syntax");
const char object_name_docs[]        = N_("The object name");
const char add_attrib_docs[]         = N_("Add an attribute object to this object");
const char object_copy_docs[]        = N_("Copy the object");
const char rotate_object_docs[]      = N_("Rotate the object");
const char select_object_docs[]      = N_("Select this Object");
const char unselect_object_docs[]    = N_("UnSelect this Object");

const char clear_selection_docs[]    = ".";
const char get_selection_docs[]      = ".";
const char selection_remove_docs[]   = ".";
const char append_selection_docs[]   = ".";
const char delete_selected_docs[]    = ".";

/* Line Object Methods */
const char line_length_docs[]        = N_("Returns the length of a linear object.");

/* Page Object Methods */
const char filename_docs[]           = N_("File Name of this Page.");

/* Path Object Attributes */
const char path_string_docs[]        = N_("String describing a geometric element");

/* Module Attribute Object Methods */
const char get_attrib_docs[]         = N_("Retrieve a specific attribute Object");
const char get_attribs_docs[]        = N_("Retrieve list of associated attribute Objects");
const char set_attrib_docs[]         = N_("Set the value of a specified attribute Object");
const char refresh_attribs_docs[]    = N_("Refresh Objects attributes");

/* Connection and Nodes */
const char get_network_docs[]        = N_("Retrieve all objects connect to specific Object");
const char get_junctions_docs[]      = N_("Retrieve list of points for junction for specified Objects");
const char get_unconnected_docs[]    = N_("Retrieve list of points of unconnect nodes for specified Objects");

/* define Macro for declarations because METHOD was not defined */
#define METHOD(method) FIRST_PASS_METHODS(method);

        METHOD ( shutdown )

        METHOD ( append_symbol_path )
        METHOD ( declare_local_sym )

        METHOD ( get_pages )
        METHOD ( get_active_page )
        METHOD ( set_active_page )
        METHOD ( is_page_modified )
        METHOD ( goto_page )

        METHOD ( open_page )
        METHOD ( close_page )
        METHOD ( new_page )
        METHOD ( rename_page )
        METHOD ( save_page )
        METHOD ( save_page_as )
        METHOD ( save_all_pages )

        METHOD ( GedaCapsule_Type )
        METHOD ( get_bounds )
        METHOD ( get_object )
        METHOD ( get_objects )
        METHOD ( add_object )
        METHOD ( add_objects )
        METHOD ( copy_object )
        METHOD ( remove_object )
        METHOD ( remove_objects )

        METHOD ( delete_object )
        METHOD ( delete_objects )
        METHOD ( sync_object )

/* Creators*/
        METHOD ( new_arc )
        METHOD ( new_box )
        METHOD ( new_bus )
        METHOD ( new_circle )
        METHOD ( new_complex )
        METHOD ( new_line )
        METHOD ( new_net )
        METHOD ( new_path )
        METHOD ( new_picture )
        METHOD ( new_pin )
        METHOD ( new_text )
        METHOD ( new_attrib )

/* Attributes */
        METHOD ( get_attrib )
        METHOD ( get_attribs )
        METHOD ( set_attrib )
        METHOD ( refresh_attribs)

/* Connection and Nodes */
        METHOD ( get_network )
        METHOD ( get_junctions )
        METHOD ( get_unconnected )
#undef  METHOD

#endif

/* Both the Library and the Module see this on the first load */
/* Define a flag so that closing brace and semicolon is added on this pass */
#define _MAKE_METHOD_ENUM_

/* MACRO Enumerate function base name on this pass */
#define METHOD(method, aflag ) e##method,

enum {
        method_unknown,

#endif  /* METHOD was not defined when the header was loaded so enumerate */

     METHOD ( shutdown,           METH_NOARGS )

     METHOD ( append_symbol_path, METH_VARARGS|METH_KEYWORDS )
     METHOD ( declare_local_sym,  METH_VARARGS|METH_KEYWORDS )

     METHOD ( get_pages,          METH_NOARGS  )
     METHOD ( get_active_page,    METH_NOARGS  )
     METHOD ( set_active_page,    METH_VARARGS )
     METHOD ( is_page_modified,   METH_VARARGS )
     METHOD ( goto_page,          METH_VARARGS )

     METHOD ( open_page,          METH_VARARGS|METH_KEYWORDS )
     METHOD ( close_page,         METH_VARARGS )
     METHOD ( new_page,           METH_VARARGS|METH_KEYWORDS )
     METHOD ( rename_page,        METH_VARARGS )
     METHOD ( save_page,          METH_VARARGS )
     METHOD ( save_page_as,       METH_VARARGS )
     METHOD ( save_all_pages,     METH_VARARGS ) /* Optional list of Pages */

     METHOD ( GedaCapsule_Type,   METH_O)
     METHOD ( get_bounds,         METH_VARARGS )
     METHOD ( get_object,         METH_VARARGS )
     METHOD ( get_objects,        METH_VARARGS )
     METHOD ( add_object,         METH_VARARGS )
     METHOD ( add_objects,        METH_VARARGS )
     METHOD ( copy_object,        METH_VARARGS )
     METHOD ( remove_object,      METH_VARARGS )
     METHOD ( remove_objects,     METH_VARARGS )

/* End Page level Methods */
     METHOD ( delete_object,      METH_VARARGS )
     METHOD ( delete_objects,     METH_VARARGS )
     METHOD ( sync_object,        METH_VARARGS )

     METHOD ( new_arc,            METH_VARARGS )
     METHOD ( new_box,            METH_VARARGS )
     METHOD ( new_bus,            METH_VARARGS )
     METHOD ( new_circle,         METH_VARARGS )
     METHOD ( new_complex,        METH_VARARGS )
     METHOD ( new_line,           METH_VARARGS )
     METHOD ( new_net,            METH_VARARGS )
     METHOD ( new_path,           METH_VARARGS )
     METHOD ( new_picture,        METH_VARARGS )
     METHOD ( new_pin,            METH_VARARGS )
     METHOD ( new_text,           METH_VARARGS )
     METHOD ( new_attrib,         METH_VARARGS )

/* Attributes */
     METHOD ( get_attrib,         METH_VARARGS )
     METHOD ( get_attribs,        METH_VARARGS )
     METHOD ( set_attrib,         METH_VARARGS )
     METHOD ( refresh_attribs,    METH_VARARGS )

/* Connection and Nodes */
     METHOD ( get_network,        METH_VARARGS )
     METHOD ( get_junctions,      METH_VARARGS )
     METHOD ( get_unconnected,    METH_VARARGS )

#ifdef _MAKE_METHOD_ENUM_
     METHOD_COUNT,
};
#undef _MAKE_METHOD_ENUM_
#endif /* _MAKE_METHOD_ENUM_ */
#undef METHOD
