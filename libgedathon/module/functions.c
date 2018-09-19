/* C source                                            -*- functions.c -*- */
/*!
 * \file functions.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
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

#define FIRST_PASS_FUNCTIONS(func) static PyObject *do_##func(PyObject *self, PyObject *args, PyObject *kwds)

#include "../include/gettext.h"
#include "../include/functions.h"

enum {
      ZERO_STATUS = 0,
      FUNCTION_ACTIVE = 1,
};

static PyObject *self;
static PyObject* geda_module;

static int C_REF = 1;
static int L_REF = 1;
static int R_REF = 1;
static int U_REF = 1;

static PyObject* CapacitorSymbol;
static PyObject* ElectrolyticSymbol;
static PyObject* InductorSymbol;
static PyObject* OpAmpSymbol;
static PyObject* ResistorSymbol;
static PyObject* TitleblockSymbol;

#define FUNCTION(symbol, aflag ) [ e##symbol ] = \
 { ASSOCIATED_FUNCTION(symbol), aflag, symbol##_docs },

#define PyCKFunction PyCFunctionWithKeywords

/* Function Table */
static struct {
   const char   *name;
   PyCKFunction  func;
   int           aflag;
   const char   *docstring;

} GedaFunctions[FUNCTION_COUNT + 1] = {
 [ function_unknown ] = { "unknown", do_unknown, 0, ""},
 #include "../include/functions.h"
};

/* Auto Referencing Utilities */
typedef char* (*AutoDesignator)(int);

static char *GetNextCapRef(int number)
{
  int value;
  if (number) {
    value = number;
    C_REF = number;
  }
  else {
    value = C_REF;
  }
  C_REF++;
  return g_strdup_printf ("C%d", value);
}

static char *GetNextInductorRef(int number)
{
  int value;
  if (number) {
    value = number;
    L_REF = number;
  }
  else {
    value = L_REF;
  }
  L_REF++;
  return g_strdup_printf ("L%d", value);
}

static char *GetNextResistorRef(int number)
{
  int value;
  if (number) {
    value = number;
    R_REF = number;
  }
  else {
    value = R_REF;
  }
  R_REF++;
  return g_strdup_printf ("R%d", value);
}

static char *GetNextURef(int number)
{
  int value;
  if (number) {
    value = number;
    U_REF = number;
  }
  else {
    value = U_REF;
  }
  U_REF++;
  return g_strdup_printf ("U%d", value);
}

static char *AutoDesignate(AutoDesignator GetNextRef, PyObject *py_refdes)
{
  int   number = 0;
  char *refdes;

  if (PyInt_Check(py_refdes)) {
    number = PyInt_AS_LONG(py_refdes);
  }
  else {
    if (PyString_Check(py_refdes)) {
      refdes = PyString_AsString(py_refdes);
      if (refdes) {
        number = atoi(refdes);
        if (!number) {           /* then we shouldn't call GetNextRef yet */
          char *ptr = refdes;
          while (*ptr) {         /* try and index past non digits */
            if (isdigit(*ptr)) {  /* and stop if we find one */
              break;
            }
            ptr++;
          }
          if (*ptr) {            /* then we did not reach the terminator */
            number = atoi(ptr);  /* whether converted or not */
          }
        }
      }
    }
    else {
      fprintf(stderr, "Type error, ignoring invalid optional parameter 5\n");
    }
  }

  return GetNextRef(number);
}

#ifndef PyMODINIT_FUNC  /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif

/*!
 * \brief Geda Python Module Initializer
 * \par Function Description
 *  This function is the local initializer for this Python module. The
 *  function registers methods and sets strings for the default symbols.
 */
PyMODINIT_FUNC
initFunctions(PyObject *module)
{
  PyObject *main_dic, *func_dic;

  geda_module = module;

  GedaFunctions[FUNCTION_COUNT].name  = NULL;
  GedaFunctions[FUNCTION_COUNT].aflag = 0;

  self = Py_InitModule("geda.functions", (PyMethodDef*)GedaFunctions);

  if (self == NULL)
    return;

  CapacitorSymbol    = PyString_FromString(DEFAULT_CAPACITOR_SYMBOL);
  ElectrolyticSymbol = PyString_FromString(DEFAULT_ELECTROLYTIC_SYMBOL);
  InductorSymbol     = PyString_FromString(DEFAULT_INDUCTOR_SYMBOL);
  OpAmpSymbol        = PyString_FromString(DEFAULT_OPAMP_SYMBOL);
  ResistorSymbol     = PyString_FromString(DEFAULT_RESISTOR_SYMBOL);
  TitleblockSymbol   = PyString_FromString(DEFAULT_TITLEBLOCK_SYMBOL);

  Py_XINCREF(CapacitorSymbol);
  Py_XINCREF(ElectrolyticSymbol);
  Py_XINCREF(InductorSymbol);
  Py_XINCREF(OpAmpSymbol);
  Py_XINCREF(ResistorSymbol);
  Py_XINCREF(TitleblockSymbol);

  /* Make geda.functions.XXX legal */
  PyModule_AddObject(module, "functions", self);

  /* Make geda.XXX legal */
  main_dic = PyModule_GetDict(module);
  func_dic = PyModule_GetDict(self);
  PyDict_Merge(main_dic, func_dic, 1);
}

/*!
 * \brief Helper function to interface with Module for Complex Objects
 * \par Function Description
 *  This function invokes the "new_complex" methods using only the "set"
 *  arguments and returns the resulting PyGedaComplexObject to the caller.
 *
 *  Called by Functions: AddComponent, AddCapacitor,  AddElectrolytic,
 *  AddInductor, AddOpAmp, AddResistor, AddSource
 *
 * \param py_symbol  Pointer to PyString
 * \param py_x       Pointer to PyInt
 * \param py_y       Pointer to PyInt
 * \param angle      Integer
 * \param mirror     Integer
 * \param embed      Integer
 */
/* Helper function call_pysymbol_xyi3 in Python_Geda_Module_Functions */
/* Called by Functions: AddComponent, AddCapacitor,  AddElectrolytic,
 * AddInductor, AddOpAmp, AddResistor, AddSource */
PyObject *call_pysymbol_xyi3(PyObject *py_symbol, PyObject *py_x, PyObject *py_y, int angle, int mirror,int embed)
{
  PyObject *py_component;

  if (embed > 0) {
    py_component = PyObject_CallMethod(geda_module, "new_complex", "SOOiii", py_symbol,
                                       py_x, py_y, angle, mirror, embed);
  }
  else if (mirror > 0) {
    py_component = PyObject_CallMethod(geda_module, "new_complex", "SOOii", py_symbol,
                                       py_x, py_y, angle, mirror);
  }
  else if (angle > 0) {
    py_component = PyObject_CallMethod(geda_module, "new_complex", "SOOi", py_symbol,
                                       py_x, py_y, angle);
  }
  else {
    py_component = PyObject_CallMethod(geda_module, "new_complex", "SOO", py_symbol,
                                       py_x, py_y);
  }
  return py_component;
}

/*! \brief Helper function to interface with Module to Create an Object
 *  \par Function Description
 *  This function invokes the method specifed by the first argument, using
 *  the remain arguments as parameters. The first 4 are required arguments,
 *  the 5th argument is optional. Returns the resulting PyGedaObject to the
 *  caller.
 *
 *  Called by Functions: AddBox, AddLine
 *
 *  \param [in] method  char pointer to name of the method to invoke
 *  \param [in] O1      Pointer to PyObject
 *  \param [in] O2      Pointer to PyObject
 *  \param [in] O3      Pointer to PyObject
 *  \param [in] O4      Pointer to PyObject
 *  \param [in] O5      Pointer to PyObject
 *
 *  \return [out] PyObject result of method
 */
/* Called by Functions: AddBox, */
PyObject *call_module_4IO(char *method, PyObject *O1, PyObject *O2, PyObject *O3, PyObject *O4, PyObject *O5)
{
  PyObject *py_object;

  if (O5) {
    py_object = PyObject_CallMethod(geda_module, method, "OOOOO",
                                    O1, O2, O3, O4, O5);
  }
  else {
    py_object = PyObject_CallMethod(geda_module, method, "OOOO",
                                    O1, O2, O3, O4);
  }
  return py_object;
}

/*!
 * \brief Helper function to interface with Module to Create an Object
 * \par Function Description
 *  This function invokes the method specifed by the first argument, using
 *  the remaining arguments as prarmeters. The first 4 are required arguments,
 *  the remaining arguments are optional. Returns the resulting PyGedaObject
 *  to the caller.
 *
 *  Called by Functions: AddBox, AddLine
 *
 * \param [in] method   char* to name of the method to invoke
 * \param [in] O1       Pointer to PyObject
 * \param [in] O2       Pointer to PyObject
 * \param [in] O3       Pointer to PyObject
 * \param [in] O4       Pointer to PyObject
 * \param [in] S5       Pointer to PyObject
 * \param [in] O6       Pointer to PyObject
 *
 * \return [out] PyObject result of method
 */
/* Called by Functions: AddBus, AddNet*/
PyObject *call_module_4ISO(char *method, PyObject *O1, PyObject *O2, PyObject *O3, PyObject *O4, PyObject *S5, PyObject *O6)
{
  PyObject *py_object;

  if (O6) {
    py_object = PyObject_CallMethod(geda_module, method, "OOOOOO",
                                    O1, O2, O3, O4, S5, O6);
  }
  else if (S5) {
    py_object = PyObject_CallMethod(geda_module, method, "OOOOO",
                                    O1, O2, O3, O4, S5);
  }
  else {
    py_object = PyObject_CallMethod(geda_module, method, "OOOO",
                                    O1, O2, O3, O4);
  }
  return py_object;
}

#define FUNCTION(func) FIRST_PASS_FUNCTIONS(func)
#define FUNCTION_NAME(symbol)GedaFunctions[e##symbol].name
#define FUNCTION_HELP(symbol)GedaFunctions[e##symbol].docstring

#define ABORT_SYNTAX_ERROR(token) \
    int len_name = strlen(FUNCTION_NAME(token)); \
    int len_doc  = strlen(FUNCTION_HELP(token)); \
    char *syntax = malloc(10 + len_name + len_doc); \
    syntax = strcat( strcat (strcpy(syntax, "syntax: "), FUNCTION_NAME(token)), FUNCTION_HELP(token)); \
    PyErr_SetString(PyExc_TypeError, syntax); \
    free(syntax); \
    return NULL;

/** \defgroup Python_Geda_Module_Functions Geda Python API Module Functions
 *  @{
 */

FUNCTION(unknown)
{
  fprintf(stderr, "unknown method handler\n");
  Py_INCREF(Py_None);
  return Py_None;
}

/*!
 * This module provides utility functions to facilitate and enhance accessing
 * the <b>Geda-Python</b> API's as a higher-level front-end to the geda module.
 * This module performs minimal type checking, often passing the Python objects
 * with out any conversions, and only those objects that were provided. Functions
 * in this module do not typically provide default parameters. The exception would
 * seem to be reference designators. Designators are not provided by the Library
 * and are a convenience feature of this module. The auto-referencing can be over
 * -riden by passing the reference designation. As a convenience for clients, the
 * designators can be integer OR string types. A 0 (zero) value will automatically
 * be assigned the next designation for that particular part type. Non-zero values
 * not only set the designation for the current component, the value is also used
 * to update the auto-referencing value.
 * \code
 * example:  AddCapacitor(schematic,  5400, 8600)         # Auto Assigned ref=C1 \n
 *           AddCapacitor(schematic,  6700, 8600 100)     # Assigned ref=C100 \n
 *           AddCapacitor(schematic,  7000, 6300, 0, 90)  # Auto Assigned ref=C101 \n
 *           AddCapacitor(schematic, 11500, 8600)         # Auto Assigned ref=C102 \n
 *           AddCapacitor(schematic, 12000, 8600 "C201")  # Assigned ref=C201 \n
 *           AddCapacitor(schematic, 13000, 8400)         # Auto Assigned ref=C202 \n
 * \endcode
 *  The reference designator for C101 must be provided because the component is
 *  being add with a 90 degree rotation, so a value of zero is used, to enable
 *  auto-referencing, which assigns the value of 101 because the previous Cap
 *  specified the reference designator was 100.
 *
 *  In general, this module is designed for speed not size. Many of the functions
 *  contain redundant code. All of the functions in this module could easily have
 *  been implemented in Python, (and original they were). The basic idea is to do
 *  as much as possible in C and as little as possible in Python, due to Python's
 *  general poor performence in terms of speed.
 */

/** \defgroup Python_API_Symbols_For_GedaFunctions Geda Python Module Symbol Functions
 *  @{
 */

/*!
 * \brief Get or Set the name of default Capacitor symbol
 * \par Function Description
 *  This function is used to set or get the name of the symbol inserted by
 *  the <b>AddCapacitor</b> function. The default symbol name is "capacitor-1".
 *  Symbol names are not checked, but must be a valid symbol known to the
 *  <b>Library</b> when <b>AddCapacitor</b> is used or PyGeda_new_complex
 *  will generate an error. Note that the current string is returned whether
 *  or not the optional string argument is provided. Using the reserved
 *  keyword "default" will restore the default symbol to "capacitor-1".
 *  This function provides a method to over-ride the default symbol is
 *  not required.
 *
 * \sa DefaultElectrolyticSymbol
 *
 *  [in] symbol Optional Python string to set the symbol name
 *
 * \return Pointer to PyObject of PyString_Type, containing the symbol name.
 * \code
 *  example: DefaultCapacitorSymbol("capacitor-2")
 * \endcode
 */
FUNCTION(DefaultCapacitorSymbol)
{
  PyObject *py_symbol = NULL;

  if (!PyArg_ParseTuple(args, "|S:geda.DefaultCapacitorSymbol", &py_symbol))
  {
    PyErr_SetString(PyExc_TypeError, "set: DefaultCapacitorSymbol(name) Or get: DefaultCapacitorSymbol()");
    return NULL;
  }
  if (py_symbol) {

    const char *tmp_str;

    tmp_str = PyString_AsString(py_symbol);

    Py_XDECREF(CapacitorSymbol);

    if ((tmp_str && !strcmp(tmp_str, "default")) || !tmp_str) {
      CapacitorSymbol = PyString_FromString(DEFAULT_TITLEBLOCK_SYMBOL);
    }
    else {
      CapacitorSymbol = PyString_FromString(tmp_str);
    }
  }
  Py_XINCREF(CapacitorSymbol);

  return CapacitorSymbol;
}

/*!
 * \brief Get or Set the name of default Electrolytic Capacitor symbol
 * \par Function Description
 *  This function is used to set or get the name of the symbol inserted by
 *  the <b>AddElectrolytic</b> function. The default symbol name is "electrolytic-1".
 *  Symbol names are not checked, but must be a valid symbol known to the
 *  <b>Library</b> when <b>AddElectrolytic</b> is used or PyGeda_new_complex
 *  will generate an error. Note that the current string is returned whether
 *  or not the optional string argument is provided. Using the reserved
 *  keyword "default" will restore the default symbol to "electrolytic-1".
 *  This function provides a method to over-ride the default symbol is
 *  not required.
 *
 * \sa DefaultCapacitorSymbol
 *
 *  [in] symbol Optional Python string to set the symbol name
 *
 * \return Pointer to PyObject of PyString_Type, containing the symbol name.
 *
 * \code
 *  example: DefaultElectrolyticSymbol("tantalum-1") # set default to tantalum-1
 * \endcode
 */
FUNCTION(DefaultElectrolyticSymbol)
{
  PyObject *py_symbol = NULL;

  if (!PyArg_ParseTuple(args, "|S:geda.DefaultElectrolyticSymbol", &py_symbol))
  {
    PyErr_SetString(PyExc_TypeError, "set: DefaultElectrolyticSymbol(name) Or get: DefaultElectrolyticSymbol()");
    return NULL;
  }

  if (py_symbol) {

    const char *tmp_str;

    tmp_str = PyString_AsString(py_symbol);

    Py_XDECREF(ElectrolyticSymbol);

    if ((tmp_str && !strcmp(tmp_str, "default")) || !tmp_str) {
      ElectrolyticSymbol = PyString_FromString(DEFAULT_ELECTROLYTIC_SYMBOL);
    }
    else {
      ElectrolyticSymbol = PyString_FromString(tmp_str);
    }
  }
  Py_XINCREF(ElectrolyticSymbol);

  return ElectrolyticSymbol;
}

/*!
 * \brief Get or Set the name of default Inductor symbol
 * \par Function Description
 *  This function is used to set or get the name of the symbol inserted by
 *  the <b>AddInductor</b> function. The default symbol name is "inductor-1".
 *  Symbol names are not checked, but must be a valid symbol known to the
 *  <b>Library</b> when <b>AddInductor</b> is used or <b>PyGeda_new_complex</b>
 *  will generate an error. Note that the current string is returned whether
 *  or not the optional string argument is provided. This function provides
 *  a method to over-ride the default symbol, it is not required to call the
 *  function. Using the reserved keyword "default" will restore the default
 *  symbol to "inductor-1".
 *
 * \sa DefaultTransformerSymbol
 *
 *  [in] symbol Optional Python string to set the symbol name
 *
 * \return Pointer to PyObject of PyString_Type, containing the symbol name.
 *
 * \code
 *  example: current_inductor = DefaultInductorSymbol("tantalum-1") # get inductor symbol name
 * \endcode
 */
FUNCTION(DefaultInductorSymbol)
{
  PyObject *py_symbol = NULL;

  if (!PyArg_ParseTuple(args, "|S:geda.DefaultInductorSymbol", &py_symbol))
  {
    PyErr_SetString(PyExc_TypeError, "set: DefaultInductorSymbol(name) Or get: DefaultInductorSymbol()");
    return NULL;
  }

  if (py_symbol) {

    const char *tmp_str;

    tmp_str = PyString_AsString(py_symbol);

    Py_XDECREF(InductorSymbol);

    if ((tmp_str && !strcmp(tmp_str, "default")) || !tmp_str) {
      InductorSymbol = PyString_FromString(DEFAULT_INDUCTOR_SYMBOL);
    }
    else {
      InductorSymbol = PyString_FromString(tmp_str);
    }
  }
  Py_XINCREF(InductorSymbol);

  return InductorSymbol;
}

/*!
 * \brief Get or Set the name of default Operational Amplifier symbol
 * \par Function Description
 *  This function is used to set or get the name of the symbol inserted by
 *  the <b>AddOpAmp</b> function. The default symbol name is "opamp-1". Symbol
 *  names are not checked, but must be a valid symbol known to the <b>Library</b>
 *  when <b>AddOpAmp</b> is used or <b>PyGeda_new_complex</b> will generate an
 *  error. Note that the current string is returned whether or not the optional
 *  string argument is provided. This function provides a method to over-ride
 *  the default symbol, it is not required to call this function. Using the
 *  reserved keyword "default" will restore the default symbol to "opamp-1".
 *
 *  [in] symbol Optional Python string to set the symbol name
 *
 * \return Pointer to PyObject of PyString_Type, containing the symbol name.
 *
 * \code
 *  example: DefaultOpAmpSymbol("default")  # restore the default symbol "dual-opamp-1"
 * \endcode
 */
FUNCTION(DefaultOpAmpSymbol)
{
  PyObject *py_symbol = NULL;

  if (!PyArg_ParseTuple(args, "|S:geda.DefaultOpAmpSymbol", &py_symbol))
  {
    PyErr_SetString(PyExc_TypeError, "set: DefaultOpAmpSymbol(name) Or get: DefaultOpAmpSymbol()");
    return NULL;
  }

  if (py_symbol) {

    const char *tmp_str;

    tmp_str = PyString_AsString(py_symbol);

    Py_XDECREF(OpAmpSymbol);

    if ((tmp_str && !strcmp(tmp_str, "default")) || !tmp_str) {
      OpAmpSymbol = PyString_FromString(DEFAULT_OPAMP_SYMBOL);
    }
    else {
      OpAmpSymbol = PyString_FromString(tmp_str);
    }
  }
  Py_XINCREF(OpAmpSymbol);
  return OpAmpSymbol;
}

/*!
 * \brief Get or Set the name of default Resistor symbol
 * \par Function Description
 *  This function is used to set or get the name of the symbol inserted by
 *  the <b>AddResistor</b> function. The default symbol name is "resistor-1".
 *  Symbol names are not checked, but must be a valid symbol known to the
 *  <b>Library</b> when <b>AddResistor</b> is used or <b>PyGeda_new_complex</b>
 *  will generate an error. Note that the current string is returned whether
 *  or not the optional string argument is provided. This function provides
 *  a method to over-ride the default symbol, it is not required to call the
 *  function. Using the reserved keyword "default" will restore the default
 *  symbol to "resistor-1".
 *
 * \sa DefaultCapacitorSymbol, DefaultElectrolyticSymbol
 *
 *  [in] symbol Optional Python string to set the symbol name
 *
 * \return Pointer to PyObject of PyString_Type, containing the symbol name.
 * \code
 *  example: DefaultResistorSymbol("metal-film-1")
 * \endcode
 */
FUNCTION(DefaultResistorSymbol)
{
  PyObject *py_symbol = NULL;


  if (!PyArg_ParseTuple(args, "|S:geda.DefaultResistorSymbol", &py_symbol))
  {
    PyErr_SetString(PyExc_TypeError, "set: DefaultResistorSymbol(name) Or get: DefaultResistorSymbol()");
    return NULL;
  }

  if (py_symbol) {

    const char *tmp_str;

    tmp_str = PyString_AsString(py_symbol);

    Py_XDECREF(ResistorSymbol);

    if ((tmp_str && !strcmp(tmp_str, "default")) || !tmp_str) {
      ResistorSymbol = PyString_FromString(DEFAULT_RESISTOR_SYMBOL);
    }
    else {
      ResistorSymbol = PyString_FromString(tmp_str);
    }
  }
  Py_XINCREF(ResistorSymbol);
  return ResistorSymbol;
}

/*!
 * \brief Get or Set the name of default Titleblock symbol
 * \par Function Description
 *  This function is used to set or get the name of the symbol inserted by
 *  the <b>AddTitleblock</b> function. The default symbol name is "title-B".
 *  Symbol names are not checked, but must be a valid symbol known to the
 *  <b>Library</b> when <b>AddTitleblock</b> is used or <b>PyGeda_new_complex</b>
 *  will generate an error. Note that the current string is returned whether or
 *  not the optional string argument is provided.
 *  This function provides a method to over-ride the default symbol, it is
 *  not required to call this function.
 *
 *  [in] symbol Optional Python string to set the symbol name
 *
 * \return Pointer to PyObject of PyString_Type, containing the symbol name.
 */
FUNCTION(DefaultTitleblockSymbol)
{
  PyObject *py_symbol = NULL;

  if (!PyArg_ParseTuple(args, "|S:geda.DefaultTitleblockSymbol", &py_symbol))
  {
    PyErr_SetString(PyExc_TypeError, "set: DefaultTitleblockSymbol(name) Or get: DefaultTitleblockSymbol()");
    return NULL;
  }

  if (py_symbol) {

    const char *tmp_str;

    tmp_str = PyString_AsString(py_symbol);

    Py_XDECREF(TitleblockSymbol);

    if ((tmp_str && !strcmp(tmp_str, "default")) || !tmp_str) {
      TitleblockSymbol = PyString_FromString(DEFAULT_TITLEBLOCK_SYMBOL);
    }
    else {
      TitleblockSymbol = PyString_FromString(tmp_str);
    }
  }
  Py_XINCREF(TitleblockSymbol);
  return TitleblockSymbol;
}

/** @} END Group Python_API_Symbols_For_GedaFunctions */

/*!
 * \brief Create and Add an Arc Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaArcObject</b> and attaches the Arc to the Page object,
 *  which must be provided. There are no default values for an Arc
 *  Object other then color.
 *
 *  [in] page        A Page object
 *  [in] x           Integer center X location
 *  [in] y           Integer center Y location
 *  [in] radius      Integer radius of the arc sector
 *  [in] start_angle Integer start angle of the sector
 *  [in] arc_sweep   Integer ending angle of the sector
 *
 *  Optional parameters:
 *
 *  [in] color       integer color property
 *
 * \return [out] PyGedaArcObject or Py_None if an error occurred
 *
 * \code
 *  example: AddArc(schematic, 2300, 2700, 500, 0, 90)
 * \endcode
 */
FUNCTION ( AddArc)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_radius;
  PyObject *py_s_angle;      /* start_angle */
  PyObject *py_e_angle;      /* arc_sweep */
  PyObject *py_color = NULL;
  PyObject *py_arc   = NULL;

  static char *kwlist[] = {"page", "name", "x", "y",
                           "radius", "start_angle", "arc_sweep", "color",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOOOOO|O:AddArc, Bad Arguments",
                                    kwlist, &py_page, &py_x, &py_y, &py_radius,
                                    &py_s_angle, &py_e_angle, &py_color))
  {
    ABORT_SYNTAX_ERROR(AddArc);
  }

  if (py_color) {
    py_arc = PyObject_CallMethod(geda_module, "new_arc", "OOOOOO",
                                 py_x, py_y, py_radius, py_s_angle, py_e_angle, py_color);
  }
  else {
    py_arc = PyObject_CallMethod(geda_module, "new_arc", "OOOOO",
                                 py_x, py_y, py_radius, py_s_angle, py_e_angle);
  }

  if (py_arc) {
    Py_XINCREF(py_arc);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_arc);
  }
  else {
    fprintf(stderr, "AddArc: An error occurred, Arc object was not created\n");
    py_arc = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_arc;
}

/*!
 * \brief Create and Add an attribute Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaTextObject</b> and attaches the object to the given Page object.
 *  This function creates the same type of object as the AddText function but
 *  with different arguments. This functions allow the name value strings
 *  to be passed separately, and setting the show-name-value property. This
 *  can also be done with a <b>AddText</b> by changing the property after the
 *  object is create. This function is equivent to calling new_attrib method
 *  followed by add_object, and provided as an alternative for convenience.
 *
 * \note  An Attribute object is a Text object whose string is in the
 *        form: "name=value".
 *
 * [in] Object      Object  a Complexobject
 * [in] name        String  the name property
 * [in] value       String  the value property
 * [in] x           Integer X location
 * [in] y           Integer Y location
 *
 *  Optional parameters:
 *
 * [in] visible     Boolean 0 = invisible, 1 = visible
 * [in] show        Integer the show-name-value code
 * [in] alignment   Integer text alignment code
 * [in] angle       integer text orientation property
 * [in] color       Object  color object
 *
 * \return [out] PyGedaTextObject or Py_None if an error occurred
 *
 * \code
 *  example 1: amp = AddComponent(circuit, "dual-opamp-py", "U1", 7700, 7300)
 *              AddAttribute(amp, "slot", "2",  7650, 7250)
 * \endcode
 */
FUNCTION ( AddAttribute )
{
  PyObject *py_page;
  PyObject *py_name;
  PyObject *py_value;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_color   = NULL;
  PyObject *py_text    = NULL;

  int visible = -1;
  int show    = -1;
  int align   = -1;
  int angle   = -1;

  static char *kwlist[] = {"page", "name", "value", "x", "y",
                           "visible", "show", "alignment", "angle", "color",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OSSOO|iiiiO:AddAttribute, Bad Arguments",
                                    kwlist, &py_page, &py_name, &py_value, &py_x, &py_y,
                                    &visible, &show, &align, &angle, &py_color))
  {
    ABORT_SYNTAX_ERROR(AddAttribute);
  }

  if (py_color) {
    py_text = PyObject_CallMethod(geda_module, "new_attrib", "OOOOiiiiO",
                                  py_name, py_value, py_x, py_y,
                                  visible, show, align, angle, py_color);
  }

  if (angle > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_attrib", "OOOOiiii",
                                  py_name, py_value, py_x, py_y,
                                  visible, show, align, angle);
  }
  else if (align > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_attrib", "OOOOiii",
                                  py_name, py_value, py_x, py_y,
                                  visible, show, align);
  }
  else if (show > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_attrib", "OOOOii",
                                  py_name, py_value, py_x, py_y,
                                  visible, show);
  }
  else if (visible > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_attrib", "OOOOi",
                                  py_name, py_value, py_x, py_y,
                                  visible);
  }
  else {
    py_text = PyObject_CallMethod(geda_module, "new_attrib", "OOO0",
                                  py_name, py_value, py_x, py_y);
  }

  if (py_text) {
    Py_XINCREF(py_text);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_text);
  }
  else {
    fprintf(stderr, "AddAttribute: An error occurred, Text Attribute object was not created\n");
    py_text = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_text;
}

/*!
 * \brief Create and Add a Box Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaBoxObject</b> and attaches the object to the given Page object.
 *  This function is equivent to calling new_box method followed by add_object,
 *  and provided as an alternative for convenience.
 *
 * [in] page        Object  a Page object
 * [in] py_lower_x  Integer lower left X location
 * [in] py_lower_y  Integer lower left Y location
 * [in] py_lower_x  Integer upper right X location
 * [in] py_lower_y  Integer upper right Y location
 *
 *  Optional parameters:
 *
 * [in] color       Object  color object
 *
 * \return [out] PyGedaBoxObject or Py_None if an error occurred
 * \code
 *  example 1: AddBox(schematic, 7500, 2500, 14500, 9500)
 * \endcode
 * \code
 *  example 2: AddBox(schematic, 800, 1500, 5200, 8625, Blue)
 * \endcode
 */
FUNCTION ( AddBox )
{
  PyObject *py_page;
  PyObject *py_lower_x;
  PyObject *py_lower_y;
  PyObject *py_upper_x;
  PyObject *py_upper_y;
  PyObject *py_color = NULL;
  PyObject *py_box   = NULL;

  static char *kwlist[] = {"page",
                           "lower_x", "lower_y", "upper_x", "upper_y",
                           "color",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOOOO|O:geda.AddBox, Bad Arguments",
                                    kwlist, &py_page, &py_lower_x, &py_lower_y,
                                                      &py_upper_x, &py_upper_y,
                                                      &py_color))
  {
    ABORT_SYNTAX_ERROR(AddBox);
  }

  py_box = call_module_4IO("new_box", py_lower_x, py_lower_y, py_upper_x, py_upper_y, py_color);

  if (py_box) {
    Py_XINCREF(py_box);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_box);
  }
  else {
    fprintf(stderr, "AddBox: An error occurred, Box object was not created\n");
    py_box = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_box;
}

/*!
 * \brief Create and Add a Bus Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new <b>PyGedaBusObject</b>
 *  and attaches the object to the given Page object. Bus objects are drawn
 *  as a line but represent a set of electrical paths, like cables. A bus is
 *  similar to a Net object but represent multiple wires, not just one.
 *  If the optional busname argument is given then a separate text attribute
 *  object will be created and attached to the Bus object with the value given
 *  by the string and the name "netname". The optional color argument applies
 *  to the the Bus object, the hidden netname attribute would be assigned the
 *  default attached attribute color.
 *
 * [in] page    Object  a Page object
 * [in] x1      Integer from X location
 * [in] y1      Integer from Y location
 * [in] x2      Integer to X location
 * [in] y2      Integer to Y location
 *
 *  Optional parameters:
 *
 *  [in] busname String  A name for the Bus object
 *  [in] color   Object  color object
 *
 * \return [out] PyGedaBusObject or Py_None if an error occurred
 * \code
 *  example 1: AddBus(phone, 3700, 3100, 3700, 4200, "LowAddress)
 * \endcode
 * \code
 *  example 2: AddBus(phone, 2000, 4400, 2700, 4400, Blue)
 * \endcode
 */
FUNCTION(AddBus)
{
  PyObject *py_page;
  PyObject *py_x1;
  PyObject *py_y1;
  PyObject *py_x2;
  PyObject *py_y2;
  PyObject *py_name  = NULL;
  PyObject *py_color = NULL;
  PyObject *py_bus   = NULL;

  static char *kwlist[] = {"page",
                           "x1", "y1", "x2", "y2",
                           "busname", "color",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOOOO|SO:geda.AddBus, Bad Arguments",
                                    kwlist, &py_page, &py_x1, &py_y1, &py_x2, &py_y2,
                                    &py_name, &py_color))

  {
    ABORT_SYNTAX_ERROR(AddBus);
  }

  py_bus = call_module_4ISO("new_bus", py_x1, py_y1, py_x2, py_y2, py_name, py_color);

  if (py_bus) {
    Py_XINCREF(py_bus);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_bus);
  }
  else {
    fprintf(stderr, "AddBus: An error occurred, Bus object was not created\n");
    py_bus = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_bus;
}

/*!
 * \brief Create and Add a Component Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> and attaches the object to the given Page
 *  object. The function uses the set_attrib API Library method to attach
 *  the refdes attribute if refdes is a non empty string.
 *
 *  [in] page        Object  a Page object
 *  [in] symbol      String  the name of the symbol to add
 *  [in] refdes      String  the reference designator
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *
 *  Optional parameters:
 *
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embeded into the schmatic
 *
 * \return [out] PyGedaComplexObject or Py_None if an error occurred
 * \code
 *  example 1: AddComponent(schematic, "lm2902-1", "U1", 1000, 4000)
 * \endcode
 * \code
 *  example 2: IC15 = AddComponent(schematic, "XC2S150-5PQ208I", "IC15", 115000, 18000, -1, -1, 1)
 * \endcode
 */
FUNCTION(AddComponent)
{
  PyObject *py_page;
  PyObject *py_symbol;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_component  = NULL;
  const char *refdes      = NULL;
  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "symbol", "refdes", "x", "y",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OSsOO|iii:AddAttribute, Bad Arguments",
                                   kwlist, &py_page, &py_symbol, &refdes, &py_x, &py_y,
                                   &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddComponent);
  }

  py_component = call_pysymbol_xyi3(py_symbol, py_x, py_y, angle, mirror, embed);

  if (py_component) {
     Py_XINCREF(py_component);
     PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_component);
     if (refdes && (strlen(refdes) > 0))
       PyObject_CallMethod(geda_module, "set_attrib", "Ossi", py_component, "refdes", refdes, FALSE);
  }
  else {
    fprintf(stderr, "AddComponent: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(py_symbol));
    py_component = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_component;
}

/*!
 * \brief Create and Add a Capacitor component Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> using the symbol whose name is pointer to by the
 *  <b>CapacitorSymbol</b> variable. The default symbol name is "capacitor-1".
 *  The Object is attached to the given Page object. A text attribute
 *  is attached to the object for the refdes. The value of refdes is
 *  optional and will be auto assigned if not specified using the
 *  letter "C" as a prefix. When specifying refdes as a string, refdes
 *  does not need to include the character prefix.
 *
 *  [in] page        A Page object
 *  [in] x           Integer center X location
 *  [in] y           Integer center Y location
 *  [in] value       String representing the capacitance value
 *
 *  Optional parameters:
 *
 *  [in] refdes      String Or Integer reference designator
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embedded into the schematic
 *
 * \return [out] PyGedaComplexObject representing the capacitor or Py_None if
 *               an error occurred.
 * \code
 *  example 1: AddCapacitor(page,  6700, 8600, "20nF")
 * \endcode
 * \code
 *  example 2: AddCapacitor(filter, 14300, 9800, ".01uF", 0, 270)
 * \endcode
 */
FUNCTION(AddCapacitor)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_value;
  PyObject *py_capacitor = NULL;
  PyObject *py_refdes    = NULL;

  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "x", "y", "value", "refdes",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OOOS|Oiii:geda.AddCapacitor, Bad Arguments",
                                   kwlist, &py_page, &py_x, &py_y, &py_value,
                                   &py_refdes, &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddCapacitor);
  }

  py_capacitor = call_pysymbol_xyi3(CapacitorSymbol, py_x, py_y, angle, mirror, embed);

  if (py_capacitor) {
    char *refdes;
    if (py_refdes) {
      refdes = AutoDesignate(&GetNextCapRef, py_refdes);
    }
    else {
      refdes = GetNextCapRef(0);
    }
    Py_XINCREF(py_capacitor);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_capacitor);
    PyObject_CallMethod(geda_module, "set_attrib", "Ossi", py_capacitor, "refdes", refdes, FALSE);
    PyObject_CallMethod(geda_module, "set_attrib", "OsOi", py_capacitor, "value", py_value, FALSE);
    g_free(refdes);
  }
  else {
    fprintf(stderr, "AddCapacitor: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(CapacitorSymbol));
    py_capacitor = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_capacitor;
}

/*!
 * \brief Create and Add a Circle Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaCircleObject</b> and attaches the Circle to the Page object,
 *  which must be provided. There are no default values for an Circle
 *  Object other then color.
 *
 *  [in] page        A Page object
 *  [in] x           Integer center X location
 *  [in] y           Integer center Y location
 *  [in] radius      Integer radius of the arc sector
 *
 *  Optional parameters:
 *
 *  [in] color       integer color property
 *
 * \return [out] PyGedaCircleObject or Py_None if an error occurred
 *
 * \code
 *  example: circle1 = AddCircle(schematic, 1800, 3100, 500)
 *           circle2 = geda.copy_object(circle1, 1000, -900)
 *           circle3 = circle2.copy(1000, -900)
 * \endcode
 */
FUNCTION ( AddCircle )
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_radius;
  PyObject *py_color = NULL;
  PyObject *py_circle  = NULL;

  static char *kwlist[] = {"page", "x", "y", "radius", "color",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOOO|O:geda.AddCircle, Bad Arguments",
                                    kwlist, &py_page, &py_x, &py_y, &py_radius, &py_color))
  {
    ABORT_SYNTAX_ERROR(AddCircle);
  }

  if (py_color) {
    py_circle = PyObject_CallMethod(geda_module, "new_circle", "OOOO",
                                 py_x, py_y, py_radius, py_color);
  }
  else {
    py_circle = PyObject_CallMethod(geda_module, "new_circle", "OOO",
                                 py_x, py_y, py_radius);
  }

  if (py_circle) {
     Py_XINCREF(py_circle);
     PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_circle);
  }
  else {
    fprintf(stderr, "AddCircle: An error occurred, Circle object was not created\n");
    py_circle = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_circle;
}

/*!
 * \brief Create and Add an Electrolytic Capacitor Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> using the symbol whose name is pointer to by the
 *  <b>ElectrolyticSymbol</b> variable. The default symbol name is "electrolytic-1".
 *  The Object is attached to the given Page object. A text attribute
 *  is attached to the object for the refdes. The value of refdes is
 *  optional and will be auto assigned if not specified using the
 *  letter "C" as a prefix. When specifying refdes as a string, refdes
 *  does not need to include the character prefix.
 *
 *  [in] page        A Page object
 *  [in] x           Integer center X location
 *  [in] y           Integer center Y location
 *  [in] value       String representing the capacitance value
 *
 *  Optional parameters:
 *
 *  [in] refdes      String Or Integer reference designator
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embedded into the schematic
 *
 * \return [out] PyGedaComplexObject representing the capacitor or Py_None if
 *               an error occurred.
 * \code
 *  example 1: AddElectrolytic(page,  6700, 8600, "20uF")
 * \endcode
 * \code
 *  example 2: AddElectrolytic(filter, 14300, 9800, "4.7uF 16V")
 * \endcode
 */
FUNCTION(AddElectrolytic)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_value;
  PyObject *py_electrolytic = NULL;
  PyObject *py_refdes       = NULL;

  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "x", "y", "value", "refdes",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OOOS|Oiii:geda.AddElectrolytic, Bad Arguments",
                                   kwlist, &py_page, &py_x, &py_y, &py_value,
                                   &py_refdes, &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddElectrolytic);
  }

  py_electrolytic = call_pysymbol_xyi3(ElectrolyticSymbol, py_x, py_y, angle, mirror, embed);

  if (py_electrolytic) {
    char *refdes;
    if (py_refdes) {
      refdes = AutoDesignate(&GetNextCapRef, py_refdes);
    }
    else {
      refdes = GetNextCapRef(0);
    }
    Py_XINCREF(py_electrolytic);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_electrolytic);
    PyObject_CallMethod(geda_module, "set_attrib", "Ossi", py_electrolytic, "refdes", refdes, FALSE);
    PyObject_CallMethod(geda_module, "set_attrib", "OsOi", py_electrolytic, "value", py_value, FALSE);
    g_free(refdes);
  }
  else {
    fprintf(stderr, "AddElectrolytic: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(ElectrolyticSymbol));
    py_electrolytic = Py_BuildValue("");
    PyErr_Clear();
  }

  return py_electrolytic;
}

/*!
 * \brief Create and Add an Inductor component Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> using the symbol whose name is pointer to by the
 *  <b>InductorSymbol</b> variable. The default symbol name is "inductor-1".
 *  The Object is attached to the given Page object. A text attribute
 *  is attached to the object for the refdes. The value of refdes is
 *  optional and will be auto assigned if not specified using the
 *  letter "L" as a prefix. When specifying refdes as a string, refdes
 *  does not need to include the character prefix.
 *
 *  [in] page        A Page object
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *  [in] value       String representing the inductor value
 *
 *  Optional parameters:
 *
 *  [in] refdes      String Or Integer reference designator
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embedded into the schematic
 *
 * \return [out] PyGedaComplexObject representing the capacitor or Py_None if
 *               an error occurred.
 * \code
 *  example 1: AddInductor(page,  5000, 6500, "20mH")
 *  example 2: AddInductor(filter, 14000, 9800, "4.7uH")
 * \endcode
 */
FUNCTION(AddInductor)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_value;
  PyObject *py_inductor = NULL;
  PyObject *py_refdes    = NULL;

  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "x", "y", "value", "refdes",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OOOS|Oiii:geda.AddInductor, Bad Arguments",
                                   kwlist, &py_page, &py_x, &py_y, &py_value,
                                   &py_refdes, &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddInductor);
  }

  py_inductor = call_pysymbol_xyi3(InductorSymbol, py_x, py_y, angle, mirror, embed);

  if (py_inductor) {
    char *refdes;
    if (py_refdes) {
      refdes = AutoDesignate(&GetNextInductorRef, py_refdes);
    }
    else {
      refdes = GetNextInductorRef(0);
    }
    Py_XINCREF(py_inductor);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_inductor);
    PyObject_CallMethod(geda_module, "set_attrib", "Ossi", py_inductor, "refdes", refdes, FALSE);
    PyObject_CallMethod(geda_module, "set_attrib", "OsOi", py_inductor, "value", py_value, FALSE);
    g_free(refdes);
  }
  else {
    fprintf(stderr, "AddInductor: An error occurred, inductor object was not created, symbol=%s\n",
            PyString_AsString(InductorSymbol));
    py_inductor = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_inductor;
}

/*!
 * \brief Create and Add a Line Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaLineObject</b> and attaches the Line to the Page object, which must
 *  be provided. There are no optional arguments for a Line Object
 *  other then color. Line type properties can be set after the Line
 *  is created.
 *
 *  [in] page        A Page object
 *  [in] x1      Integer from X location
 *  [in] y1      Integer from Y location
 *  [in] x2      Integer to X location
 *  [in] y2      Integer to Y location
 *
 *  Optional parameters:
 *
 *  [in] color       integer color property
 *
 * \return [out] PyGedaCircleObject or Py_None if an error occurred
 *
 * \code
 *  example 1: AddLine(schematic, 2600, 3100, 3700, 3100)
 *
 *  example 2: line = AddLine(schematic, 2200, 5200, 2800, 5200)
 *              line.line_type = TYPE_DASHED
 *              line.line_space = 25
 * \endcode
 */
FUNCTION(AddLine)
{
  PyObject *py_page;
  PyObject *py_x1;
  PyObject *py_y1;
  PyObject *py_x2;
  PyObject *py_y2;
  PyObject *py_color = NULL;
  PyObject *py_line  = NULL;

  static char *kwlist[] = {"page", "x1", "y1", "x2", "y2", "color", NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOOOO|O:geda.AddLine, Bad Arguments",
                                    kwlist, &py_page, &py_x1, &py_y1, &py_x2, &py_y2,
                                    &py_color))
  {
    ABORT_SYNTAX_ERROR(AddLine);
  }

  py_line = call_module_4IO("new_line", py_x1, py_y1, py_x2, py_y2, py_color);

  if (py_line) {
    Py_XINCREF(py_line);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_line);
  }
  else {
    fprintf(stderr, "AddLine: An error occurred, line object was not created\n");
    py_line = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_line;
}

/*!
 * \brief Create and Add a Net Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new <b>PyGedaNetObject</b>
 *  and attaches the object to the given Page object. If the optional netname
 *  argument is given then a separate text attribute object will be created
 *  and attached to the Net object with the value given by the string and the
 *  name "netname". The optional color argument applies to the the Net object,
 *  the hidden netname attribute would be assigned the default attribute color.
 *
 *  [in] page    Object  a Page object
 *  [in] x1      Integer from X location
 *  [in] y1      Integer from Y location
 *  [in] x2      Integer to X location
 *  [in] y2      Integer to Y location
 *
 *  Optional parameters:
 *
 *  [in] netname String  A name for the Bus object
 *  [in] color   Object  color object
 *
 * \return [out] PyGedaNetObject or Py_None if an error occurred
 *
 * \code
 *  example 1: AddNet(circuit, 12400,  8800, 13900,  8800)
 *
 *  example 2: AddNet(schematic,2000, 4400, 2700, 4400, "U1_1_U1_5")
 * \endcode
 */
FUNCTION(AddNet)
{
  PyObject *py_page;
  PyObject *py_x1;
  PyObject *py_y1;
  PyObject *py_x2;
  PyObject *py_y2;
  PyObject *py_name  = NULL;
  PyObject *py_color = NULL;
  PyObject *py_net   = NULL;

  static char *kwlist[] = {"page",
                           "x1", "y1", "x2", "y2",
                           "netname", "color",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OOOOO|SO:geda.AddNet, Bad Arguments",
                                    kwlist, &py_page, &py_x1, &py_y1, &py_x2, &py_y2,
                                    &py_name, &py_color))
  {
    ABORT_SYNTAX_ERROR(AddNet);
  }

  py_net = call_module_4ISO("new_net", py_x1, py_y1, py_x2, py_y2, py_name, py_color);

  if (py_net) {
    Py_XINCREF(py_net);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_net);
  }
  else {
    fprintf(stderr, "AddNet: An error occurred, Net object was not created\n");
    py_net = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_net;
}

/*!
 * \brief Create and Add an OpAmp component Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> using the symbol whose name is pointer
 *  to by the <b>OpAmpSymbol</b> variable. The default symbol name is
 *  "opamp-1". The Object is attached to the given Page object. A text
 *  attribute is attached to the object for the refdes. The value of
 *  refdes is optional and will be auto assigned if not specified using
 *  the letter "U" as a prefix. When specifying refdes as a string, refdes
 *  does not need to include the character prefix. If the slot argument
 *  is an integer equal to -1, then the slot attribute will not be
 *  created.
 *
 *  [in] page        A Page object
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *
 *  Optional parameters:
 *
 *  [in] refdes      String Or Integer reference designator
 *  [in] slot        String Or Integer slot assignment
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embedded into the schematic
 *
 * \return [out] PyGedaComplexObject representing the OpAmp or Py_None if
 *               an error occurred.
 * \code
 *  example 1: AddOpAmp(page, 5000, 6500)
 *
 *  example 2: AddOpAmp(schematic, 6800, 8000, "U1", 0, 0, 0, 0)
 * \endcode
 */
FUNCTION(AddOpAmp)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_slot   = NULL;
  PyObject *py_opamp  = NULL;
  PyObject *py_refdes = NULL;

  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "x", "y", "refdes", "slot",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OOO|SOiii:geda.AddOpAmp, Bad Arguments",
                                   kwlist, &py_page, &py_x, &py_y,
                                   &py_refdes, &py_slot, &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddOpAmp);
  }

  py_opamp = call_pysymbol_xyi3(OpAmpSymbol, py_x, py_y, angle, mirror, embed);

  if (py_opamp) {
    char *refdes;
    if (py_refdes) {
      refdes = AutoDesignate(GetNextURef, py_refdes);
    }
    else {
      refdes = GetNextURef(0);
    }
    Py_XINCREF(py_opamp);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_opamp);
    PyObject_CallMethod(geda_module, "set_attrib", "Ossi", py_opamp, "refdes", refdes, FALSE);

    if (py_slot !=NULL) {

      int slot = 0;

      if (PyInt_Check(py_slot)) {
        slot = PyInt_AsLong(py_slot);
      }

      if (slot >= 0 ) {
        PyObject_CallMethod(geda_module, "set_attrib", "OsOi", py_opamp, "slot", py_slot, FALSE);
      }
    }
    g_free(refdes);
  }
  else {
    fprintf(stderr, "AddOpAmp: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(OpAmpSymbol));
    py_opamp = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_opamp;
}

/*!
 * \brief Create and Add an Path figure Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaPathObject</b> based on the given path string. The Object
 *  is attached to the given Page object. A Path object is graphical
 *  figure and does not represent an electrical path. Path have line-
 *  type properties and can have fill-patterns when the path is closed.
 *
 *  [in] page    A Page object
 *  [in] path    String  The SVG path string
 *
 *  Optional parameters:
 *
 *  [in] color   Object  color object
 *
 * \return [out] PyGedaPathObject or Py_None if an error occurred.
 *
 * \code
 *  example 1: path = AddPath(page, "M100,200 C100,100 250,100 250,200 S400,300 400,200")
 *
 *  example 2: AddPath(page, "M 100,200, C 100,100 250,100 250,200 C 250,300 400,300 400,200")
 * \endcode
 */
FUNCTION(AddPath)
{
  PyObject *py_page;
  PyObject *py_string = NULL;
  PyObject *py_color  = NULL;
  PyObject *py_path   = NULL;

  if (!PyArg_ParseTuple(args, "OS|O:geda.AddPath, Bad Arguments",
                       &py_page, &py_string, &py_color))
  {
    ABORT_SYNTAX_ERROR(AddPath);
  }

  if (py_color) {
    py_path = PyObject_CallMethod(geda_module, "new_path", "OO",
                                  py_string, py_color);
  }
  else {
    py_path = PyObject_CallMethod(geda_module, "new_path", "O",
                                  py_string);
  }

  if (py_path) {
    Py_XINCREF(py_path);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_path);
  }
  else {
    fprintf(stderr, "AddPath: An error occurred, path object was not created\n");
    py_path = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_path;
}

/*!
 * \brief Create and Add a Picture image Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaPictureObject</b> and attaches the object to the given Page object.
 *  This function is equivent to calling new_picture method followed by add_object,
 *  and provided as an alternative for convenience.
 *
 *  [in] page        Object  a Page object
 *  [in] file        String  the image file name
 *  [in] x1          Integer lower left X location
 *  [in] y1          Integer lower left Y location
 *  [in] x2          Integer upper right X location
 *  [in] y2          Integer upper right Y location
 *
 *  Optional parameters:
 *
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, image data will be embedded into the schematic
 *
 * \return [out] PyGedaPictureObject or Py_None if an error occurred
 * \code
 *  example: AddPicture(page, "./pic/1377339524810.jpg", 8000, 3000, 14000, 9000)
 * \endcode
 */
FUNCTION(AddPicture)
{
  PyObject *py_page;
  PyObject *py_file;
  PyObject *py_x1;
  PyObject *py_y1;
  PyObject *py_x2;
  PyObject *py_y2;
  PyObject *py_picture  = NULL;

  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "file", "x1", "y1", "x2", "y2",
                           "angle", "mirror", "embed",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OSOOOO|iii:geda.AddPicture, Bad Arguments",
                                    kwlist, &py_page, &py_file, &py_x1, &py_y1, &py_x2, &py_y2,
                                    &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddPicture);
  }

  if (embed > 0) {
    py_picture = PyObject_CallMethod(geda_module, "new_picture", "OOOOOiii",
                                       py_file, py_x1, py_y1, py_x2, py_y2, angle, mirror, embed);
  }
  else if (mirror > 0) {
    py_picture = PyObject_CallMethod(geda_module, "new_picture", "OOOOOii",
                                       py_file, py_x1, py_y1, py_x2, py_y2, angle, mirror);
  }
  else if (angle > 0) {
    py_picture = PyObject_CallMethod(geda_module, "new_picture", "OOOOOi",
                                       py_file, py_x1, py_y1, py_x2, py_y2, angle);
  }
  else {
    py_picture = PyObject_CallMethod(geda_module, "new_picture", "OOOOO",
                                       py_file, py_x1, py_y1, py_x2, py_y2);
  }

  if (py_picture) {
    Py_XINCREF(py_picture);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_picture);
  }
  else {
    fprintf(stderr, "AddPicture: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(py_file));
    py_picture = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_picture;
}

/*!
 * \brief Create and Add a Pin Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaPinObject</b> and attaches the object to the given Page
 *  object.Normally, PyGedaPinObject are only added to symbol files,
 *  not schematic files..
 *
 *  [in] page    Object  a Page object
 *  [in] x1      Integer from X location
 *  [in] y1      Integer from Y location
 *  [in] x2      Integer to X location
 *  [in] y2      Integer to Y location
 *
 *  Optional parameters:
 *
 *  [in] whichend     Boolean Which end gets connected (either 0 or 1)
 *  [in] number       string  pin number attribute
 *  [in] label        string  pin label attribute
 *  [in] elect_type   integer electrical type attribute (formally pin type)
 *  [in] mech_type    integer mechanical type attribute
 *  [in] node_type    integer node type property ( 0=normal, 1=bus type)
 *
 * \return [out] PyGedaPinObject or Py_None if an error occurred
 * \code
 *  example 1: AddPin(resistor, 0, 100, 100, 100)
 *
 *  example 2: pin = AddPin(symbol, 0, 100, 100, 100, 0, "1", "1", PIN_ELECT_PAS, 0, 0)
 * \endcode
 */
FUNCTION(AddPin)
{
  PyObject *py_page;
  PyObject *py_x1;
  PyObject *py_y1;
  PyObject *py_x2;
  PyObject *py_y2;
  PyObject *py_pin   = NULL;

  int whichend       = -1;

  int etype          = -1;
  int mtype          = -1;
  int ntype          = -1;

  const char *label  = NULL;
  const char *number = NULL;

  static char *kwlist[] = {"page", "x1", "y1", "x2", "y2",
                           "whichend", "number", "label",
                           "elect_type", "mech_type", "node_type",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OOOOO|issiii:AddPin, Bad Arguments",
                                   kwlist, &py_page, &py_x1, &py_y1, &py_x2, &py_y2,
                                   &whichend, &number, &label,
                                   &etype, &mtype, &ntype))
  {
    ABORT_SYNTAX_ERROR(AddPin);
  }

  if (ntype > 0) {
    PyObject *py_label;
    PyObject *py_number;
    if (label)
       py_label = PyString_FromString(label);
    else
       py_label = PyString_FromString("");
    if (number)
       py_number = PyString_FromString(number);
    else
       py_number = PyString_FromString("");
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOOiOOiii", py_x1, py_y1, py_x2, py_y2,
                                 whichend, py_number, py_label, etype, mtype, ntype);
  }
  else if (mtype > 0) {
    PyObject *py_label;
    PyObject *py_number;
    if (label)
       py_label = PyString_FromString(label);
    else
       py_label = PyString_FromString("");
    if (number)
       py_number = PyString_FromString(number);
    else
       py_number = PyString_FromString("");
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOOiOOii", py_x1, py_y1, py_x2, py_y2,
                                 whichend, py_number, py_label, etype, mtype);
  }
  else if (etype > 0) {
    PyObject *py_label;
    PyObject *py_number;
    if (label)
       py_label = PyString_FromString(label);
    else
       py_label = PyString_FromString("");
    if (number)
       py_number = PyString_FromString(number);
    else
       py_number = PyString_FromString("");
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOOiOOi", py_x1, py_y1, py_x2, py_y2,
                                 whichend, py_number, py_label, etype);
  }
  else if (label != NULL) {
    PyObject *py_label = PyString_FromString(label);
    PyObject *py_number;
    if (number)
       py_number = PyString_FromString(number);
    else
       py_number = PyString_FromString("");
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOOiOO", py_x1, py_y1, py_x2, py_y2,
                                 whichend, py_number, py_label);
  }
  else if (number!= NULL)
  {
    PyObject *py_number = PyString_FromString(number);
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOOiO", py_x1, py_y1, py_x2, py_y2,
                                 whichend, py_number);
  }
  else if (whichend > 0) {
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOOi", py_x1, py_y1, py_x2, py_y2,
                                 whichend);
  }
  else {
    py_pin = PyObject_CallMethod(geda_module, "new_pin", "OOOO", py_x1, py_y1, py_x2, py_y2);
  }

  if (py_pin) {
    Py_XINCREF(py_pin);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_pin);
  }
  else {
    fprintf(stderr, "AddPin: An error occurred, pin object was not created\n");
    py_pin = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_pin;
}

/*!
 * \brief Create and Add an Resistor component Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> using the symbol whose name is pointer to by
 *  the <b>ResistorSymbol</b> variable. The default symbol name is "resistor-1".
 *  The Object is attached to the given Page object. A text attribute is
 *  attached to the object for the refdes. The value of refdes is optional
 *  and will be auto assigned if not specified using the letter "R" as a
 *  prefix. When specifying refdes as a string, refdes does not need to
 *  include the character prefix. If the slot argument is an integer equal
 *  to -1, then the slot attribute will not be created.
 *
 *  [in] page        A Page object
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *  [in] value       String representing the resistor value
 *
 *  Optional parameters:
 *
 *  [in] refdes      String Or Integer reference designator
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embedded into the schematic
 *
 * \return [out] PyGedaComplexObject representing the Resistor or Py_None if
 *               an error occurred.
 * \code
 *  example 1: AddResistor(schematic, 4300,  7800, "560")
 *
 *  example 2: AddResistor(lpbf, 13800, 7100, "22k", "angle=270")
 * \endcode
 */
FUNCTION(AddResistor)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_value;
  PyObject *py_resistor = NULL;
  PyObject *py_refdes   = NULL;

  int angle  = -1;
  int mirror = -1;
  int embed  = -1;

  static char *kwlist[] = {"page", "x", "y", "value", "refdes",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OOOS|Oiii:geda.AddResistor, Bad Arguments",
                                   kwlist, &py_page, &py_x, &py_y, &py_value,
                                   &py_refdes, &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddResistor);
  }

  py_resistor = call_pysymbol_xyi3(ResistorSymbol, py_x, py_y, angle, mirror, embed);

  if (py_resistor) {
    char *refdes;
    if (py_refdes) {
      refdes = AutoDesignate(&GetNextResistorRef, py_refdes);
    }
    else {
      refdes = GetNextResistorRef(0);
    }
    Py_XINCREF(py_resistor);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_resistor);
    PyObject_CallMethod(geda_module, "set_attrib", "Ossi", py_resistor, "refdes", refdes, FALSE);
    PyObject_CallMethod(geda_module, "set_attrib", "OsOi", py_resistor, "value", py_value, FALSE);
    g_free(refdes);
  }
  else {
    fprintf(stderr, "AddResistor: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(ResistorSymbol));
    py_resistor = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_resistor;
}

/*!
 * \brief Create and Add a Source Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> and attaches the object to the given Page object.
 *  AddSource is similar to AddComponent except that there is no
 *  refdes argument.
 *
 *  [in] page        Object  a Page object
 *  [in] symbol      String  the name of the symbol to add
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *
 *  Optional parameters:
 *
 *  [in] angle       integer orientation property
 *  [in] mirror      If true the object will be mirrored
 *  [in] embed       If true, symbol data will be embeded into the schmatic
 *
 * \return [out] PyGedaComplexObject or Py_None if an error occurred
 *
 * \code
 *  example 1: AddSource(lpbf, "12V-plus-1", 12800, 10300)
 *
 *  example 2: AddSource(lpbf, "12V-minus-1", 8400,  5100, 180)
 * \endcode
 */
FUNCTION(AddSource)
{
  PyObject *py_page;
  PyObject *py_symbol;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_source  = NULL;

  int angle; int mirror; int embed;

  angle  = -1;
  mirror = -1;
  embed  = -1;

  static char *kwlist[] = {"page", "symbol", "x", "y",
                           "angle", "mirror", "embed",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "OSOO|iii:geda.AddSource, Bad Arguments",
                                   kwlist, &py_page, &py_symbol, &py_x, &py_y,
                                   &angle, &mirror, &embed))
  {
    ABORT_SYNTAX_ERROR(AddSource);
  }

  py_source = call_pysymbol_xyi3(py_symbol, py_x, py_y, angle, mirror, embed);

  if (py_source) {
    Py_XINCREF(py_source);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_source);
  }
  else {
    fprintf(stderr, "AddSource: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(py_symbol));
    py_source = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_source;
}

/*!
 * \brief Create and Add a Text Object Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaTextObject</b> and attaches the object to the given Page object.
 *  This function creates the same type of object as the AddAttribute function
 *  but with different arguments. This function is equivent to calling new_text
 *  method followed by add_object.
 *
 * \note  An Attribute object is a Text object whose string is in the
 *        form: "name=value".
 *
 *  [in] Object      Object  a Complexobject
 *  [in] string      String  the name property
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *
 *  Optional parameters:
 *
 *  [in] size        Integer the font size
 *  [in] alignment   Integer text alignment code
 *  [in] angle       integer text orientation property
 *  [in] color       Object  color object
 *
 * \return [out] PyGedaTextObject or Py_None if an error occurred
 *
 * \code
 *  example 1: amp = AddComponent(circuit, "dual-opamp-py", "U1", 7700, 7300)
 *              AddAttribute(amp, "slot", "2",  7650, 7250)
 * \endcode
 */
FUNCTION ( AddText )
{
  PyObject *py_page;
  PyObject *py_string;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_color   = NULL;
  PyObject *py_text    = NULL;

  int size   = -1;
  int align  = -1;
  int angle  = -1;

  static char *kwlist[] = {"page", "string", "x", "y",
                           "size", "alignment", "angle", "color", NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "OSOO|iiiO:AddText, Bad Arguments",
                                    kwlist, &py_page, &py_string, &py_x, &py_y,
                                    &size, &align, &angle, &py_color))
  {
    ABORT_SYNTAX_ERROR(AddText);
  }

  if (py_color) {
    py_text = PyObject_CallMethod(geda_module, "new_text", "OOOiii",
                                  py_string, py_x, py_y,
                                  size, align, angle, py_color);
  }
  else if (angle > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_text", "OOOiii",
                                  py_string, py_x, py_y,
                                  size, align, angle);
  }
  else if (align > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_text", "OOOii",
                                  py_string, py_x, py_y,
                                  size, align);
  }
  else if (size > 0) {
    py_text = PyObject_CallMethod(geda_module, "new_text", "OOOi",
                                  py_string, py_x, py_y,
                                  size);
  }
  else {
    py_text = PyObject_CallMethod(geda_module, "new_text", "OOO",
                                  py_string, py_x, py_y);
  }

  if (py_text) {
    Py_XINCREF(py_text);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_text);
  }
  else {
    fprintf(stderr, "AddText: An error occurred, Text Attribute object was not created\n");
    py_text = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_text;
}

/*!
 * \brief Create and Add an Titleblock component Python API Function
 * \par Function Description
 *  This function calls Geda API Library routines to create a new
 *  <b>PyGedaComplexObject</b> using the symbol whose name is pointer to by the
 *  <b>TitleblockSymbol</b> variable. The default symbol name is "title-B".
 *  The Object is attached to the given Page object.
 *
 *  [in] page        A Page object
 *  [in] x           Integer X location
 *  [in] y           Integer Y location
 *
 *  Optional parameters:
 *
 *  [in] embed       If true, symbol data will be embedded into the schematic
 *  [in] lock        integer orientation property
 *
 * \return [out] PyGedaComplexObject representing the Titleblock or
 *               Py_None if an error occurred.
 * \code
 *  example 1: AddOpAmp(page, 5000, 6500)
 *
 *  example 2: AddOpAmp(schematic, 6800, 8000, "U1", 0, 0, 0, 0)
 * \endcode
 */
FUNCTION(AddTitleblock)
{
  PyObject *py_page;
  PyObject *py_x;
  PyObject *py_y;
  PyObject *py_titleblock = NULL;
  PyObject *py_lock       = NULL;

  int embed  = -1;

  if (!PyArg_ParseTuple(args, "OOO|iO:AddTitleblock, Bad Arguments",
                       &py_page, &py_x, &py_y, &embed, &py_lock))
  {
    ABORT_SYNTAX_ERROR(AddTitleblock);
  }

  if (embed > 0) {
    py_titleblock = PyObject_CallMethod(geda_module, "new_complex", "SOOiii", TitleblockSymbol,
                                        py_x, py_y, 0, 0, embed);
  }
  else {
    py_titleblock = PyObject_CallMethod(geda_module, "new_complex", "SOO", TitleblockSymbol,
                                        py_x, py_y);
  }

  if (py_titleblock) {
    Py_XINCREF(py_titleblock);
    PyObject_CallMethod(geda_module, "add_object", "OO", py_page, py_titleblock);
    if (py_lock) {
      if (PyInt_Check(py_lock)) {
        PyObject_SetAttrString(py_titleblock, "locked", py_lock);
      }
      else {
        fprintf(stderr, "AddTitleblock: ignoring bad lock type\n");
      }
    }
  }
  else {
    fprintf(stderr, "AddTitleblock: An error occurred, object was not created, symbol=%s\n",
            PyString_AsString(TitleblockSymbol));
    py_titleblock = Py_BuildValue("");
    PyErr_Clear();
  }
  return py_titleblock;
}

/** @} END Group Python_Geda_Module_Functions */
