/* C header                                          -*- libgedathon.h -*- */
/*!
 * \file libgedathon.h
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

#ifndef LIBGEDATHON_H
#define LIBGEDATHON_H

enum {
      ZERO_STATUS = 0,
      METHOD_ACTIVE = 1,
};

typedef void (*GedaLibFunc)(void);
typedef void (*GedaCloserFunc)(long*);

struct st_PyGedaFunc {
   const char  *name;
   GedaLibFunc  func;
   int          status;
};

typedef struct st_PyGedaFunc PyGedaFunc;

typedef PyGedaFunc API_FunctionTable[METHOD_COUNT];
typedef void (*GedaLibInitializer)( API_FunctionTable* user_table);

void      initialize ( API_FunctionTable* user_table);
void      PyGeda_shutdown           ( void );

int       PyGeda_append_symbol_path ( const char *path );
int       PyGeda_declare_local_sym  ( const char *filename );

PyObject *PyGeda_get_pages          ( void );
PyObject *PyGeda_get_active_page    ( void );
int       PyGeda_set_active_page    ( int pid );
int       PyGeda_goto_page          ( int pid );

PyObject *PyGeda_open_page          ( const char *filename );
int       PyGeda_close_page         ( int pid );
PyObject *PyGeda_new_page           ( const char *filename, int over_write);
int       PyGeda_save_page          ( int pid );
int       PyGeda_save_all_pages     ( PyObject * );
int       PyGeda_save_all_pages     ( PyObject * );

int       PyGeda_GedaCapsule_Type   ( PyObject * );
PyObject *PyGeda_get_object         ( PyObject * );
PyObject *PyGeda_get_objects        ( int pid, int sid );
int       PyGeda_add_object         ( PyObject *, PyObject *, PyObject * );
int       PyGeda_add_objects        ( PyObject *, PyObject *, PyObject * );
PyObject *PyGeda_copy_object        ( PyObject *, int dx, int dy);
int       PyGeda_remove_object      ( PyObject * );
int       PyGeda_remove_objects     ( PyObject * );
/* End page object manipulators */

int       PyGeda_delete_object      ( PyObject * );
int       PyGeda_delete_objects     ( PyObject * );
int       PyGeda_sync_object        ( PyObject * );

PyObject *PyGeda_new_arc            ( int x, int y, int radius, int start_angle, int end_angle, PyObject *color);
PyObject *PyGeda_new_box            ( int lower_x, int lower_y, int upper_x, int upper_y, PyObject *color);
PyObject *PyGeda_new_bus            ( const char *busname, int x1, int y1, int x2, int y2, PyObject *color);
PyObject *PyGeda_new_circle         ( int x, int y, int radius, PyObject *color);
PyObject *PyGeda_new_complex        ( const char *filename, int x, int y, int angle, int mirror, int embed);
PyObject *PyGeda_new_line           ( int x1, int y1, int x2, int y2, PyObject *color);
PyObject *PyGeda_new_net            ( const char *netname, int x1, int y1, int x2, int y2, PyObject *color);
PyObject *PyGeda_new_path           ( const char *path_string );
PyObject *PyGeda_new_picture        ( const char *filepath, int x1, int y1, int x2, int y2,
                                      int angle, int mirror,int embedded );
PyObject *PyGeda_new_pin            ( const char *label, const char *number, int x1, int y1, int x2, int y2,
                                      int whichend, int etype, int mtype, int ntype);
PyObject *PyGeda_new_text           ( const char *text, int x, int y, int size, int align, int angle, PyObject *color);
PyObject *PyGeda_new_attrib         ( const char *name, const char *value, int x, int y, int visible, int show, int align, int angle, PyObject *color);
PyObject *PyGeda_get_attrib         ( PyObject *py_object, const char *name);
PyObject *PyGeda_get_attribs        ( PyObject *py_object );
PyObject *PyGeda_set_attrib         ( PyObject *py_complex, PyObject *py_attrib, const char *name, const char *value, int ret_obj);
int       PyGeda_refresh_attribs    ( PyObject *py_object );
#endif


