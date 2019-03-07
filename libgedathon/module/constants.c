/* C source                                            -*- constants.c -*- */
/*!
 * \file constants.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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

#include <Python.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

static PyObject *self;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PyMODINIT_FUNC initConstants(PyObject *module)
{
  PyObject *main_dic, *con_dic;

  self = Py_InitModule3("geda.constants", NULL, "Geda Module constants layer.");
  if (self == NULL)
    return;

  /* Entity Types */
  PyModule_AddIntMacro(self, OBJ_ARC);
  PyModule_AddIntMacro(self, OBJ_BOX);
  PyModule_AddIntMacro(self, OBJ_BUS);
  PyModule_AddIntMacro(self, OBJ_CIRCLE);
  PyModule_AddIntMacro(self, OBJ_COMPLEX);
  PyModule_AddIntMacro(self, OBJ_LINE);
  PyModule_AddIntMacro(self, OBJ_NET);
  PyModule_AddIntMacro(self, OBJ_PATH);
  PyModule_AddIntMacro(self, OBJ_PICTURE);
  PyModule_AddIntMacro(self, OBJ_PIN);
  PyModule_AddIntMacro(self, OBJ_PLACEHOLDER);
  PyModule_AddIntMacro(self, OBJ_TEXT);

  /* Line End Types from LINE_END enumerators */
  PyModule_AddIntMacro(self, END_NONE);
  PyModule_AddIntMacro(self, END_SQUARE);
  PyModule_AddIntMacro(self, END_ROUND);
  PyModule_AddIntMacro(self, END_VOID);

  /* Line style for lines, rect, circles, arcs; from LINE_TYPE enumerators */
  PyModule_AddIntMacro(self, TYPE_SOLID);
  PyModule_AddIntMacro(self, TYPE_DOTTED);
  PyModule_AddIntMacro(self, TYPE_DASHED);
  PyModule_AddIntMacro(self, TYPE_CENTER);
  PyModule_AddIntMacro(self, TYPE_PHANTOM);
  PyModule_AddIntMacro(self, TYPE_ERASE);

  /* fill style of objects like cirle, rect, path; from OBJECT_FILLING enumerators */
  PyModule_AddIntMacro(self, FILLING_HOLLOW);
  PyModule_AddIntMacro(self, FILL_SOLID);
  PyModule_AddIntMacro(self, FILLING_MESH);
  PyModule_AddIntMacro(self, FILLING_HATCH);
  PyModule_AddIntMacro(self, FILLING_VOID);

  /* Text Alignment */
  PyModule_AddIntMacro(self, LOWER_LEFT);
  PyModule_AddIntMacro(self, MIDDLE_LEFT);
  PyModule_AddIntMacro(self, UPPER_LEFT);
  PyModule_AddIntMacro(self, LOWER_MIDDLE);
  PyModule_AddIntMacro(self, MIDDLE_MIDDLE);
  PyModule_AddIntMacro(self, UPPER_MIDDLE);
  PyModule_AddIntMacro(self, LOWER_RIGHT);
  PyModule_AddIntMacro(self, MIDDLE_RIGHT);
  PyModule_AddIntMacro(self, UPPER_RIGHT);

  /* For Attributes */
  PyModule_AddIntConstant(self, "ATTRIBUTE_OFFSET", DEFAULT_ATTRIBUTE_OFFSET);
  PyModule_AddIntConstant(self, "ATTRIBUTE_SIZE",   DEFAULT_ATTRIBUTE_SIZE);

  PyModule_AddIntMacro(self, SHOW_NAME_VALUE);
  PyModule_AddIntMacro(self, SHOW_VALUE);
  PyModule_AddIntMacro(self, SHOW_NAME);

  PyModule_AddIntMacro(self, INVISIBLE);
  PyModule_AddIntMacro(self, VISIBLE);

  /* For Pin Attributes */
  PyModule_AddIntMacro(self, PIN_ELECT_IN);
  PyModule_AddIntMacro(self, PIN_ELECT_OUT);
  PyModule_AddIntMacro(self, PIN_ELECT_IO);
  PyModule_AddIntMacro(self, PIN_ELECT_OC);
  PyModule_AddIntMacro(self, PIN_ELECT_OE);
  PyModule_AddIntMacro(self, PIN_ELECT_PAS);
  PyModule_AddIntMacro(self, PIN_ELECT_TP);
  PyModule_AddIntMacro(self, PIN_ELECT_TRI);
  PyModule_AddIntMacro(self, PIN_ELECT_CLK);
  PyModule_AddIntMacro(self, PIN_ELECT_PWR);
  PyModule_AddIntMacro(self, PIN_ELECT_VOID);


  PyModule_AddIntConstant(self, "GEDA_FILTER_ALL",     GEDA_FILTER_ALL);
  PyModule_AddIntConstant(self, "GEDA_FILTER_ARC",     GEDA_FILTER_ARC);
  PyModule_AddIntConstant(self, "GEDA_FILTER_BOX",     GEDA_FILTER_BOX);
  PyModule_AddIntConstant(self, "GEDA_FILTER_BUS",     GEDA_FILTER_BUS);
  PyModule_AddIntConstant(self, "GEDA_FILTER_CIRCLE",  GEDA_FILTER_CIRCLE);
  PyModule_AddIntConstant(self, "GEDA_FILTER_COMPLEX", GEDA_FILTER_COMPLEX);
  PyModule_AddIntConstant(self, "GEDA_FILTER_LINE",    GEDA_FILTER_LINE);
  PyModule_AddIntConstant(self, "GEDA_FILTER_NET",     GEDA_FILTER_NET);
  PyModule_AddIntConstant(self, "GEDA_FILTER_PATH",    GEDA_FILTER_PATH);
  PyModule_AddIntConstant(self, "GEDA_FILTER_PICTURE", GEDA_FILTER_PICTURE);
  PyModule_AddIntConstant(self, "GEDA_FILTER_PIN",     GEDA_FILTER_PIN);
  PyModule_AddIntConstant(self, "GEDA_FILTER_TEXT",    GEDA_FILTER_TEXT);

  /* Make geda.constants.XXX legal */
  PyModule_AddObject(module, "constants", self);

  /* Make geda.XXX legal */
  main_dic = PyModule_GetDict(module);
  con_dic = PyModule_GetDict(self);
  PyDict_Merge(con_dic, main_dic, 1);
}
