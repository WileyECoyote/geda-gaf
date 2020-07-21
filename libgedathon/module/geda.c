/* C source                                                 -*- geda.c -*- */
/*!
 * \file geda.c
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

#include <config.h>

#include <geda_py_compat.h>

#include <structmember.h>

#include <dlfcn.h>
#include <unistd.h>

#define FIRST_PASS_METHODS(func) static PyObject *do_##func(PyObject *self, PyObject *args)

#include <geda/geda.h>
#include "../include/gettext.h"
#include "../include/geda_module.h"
#include "../include/geda_capsule.h"

#include <libgeda/o_types.h>
#include <libgedathon.h>

#include <geda_py_struct.h>
#include <geda_py_page.h>
#include <geda_py_object.h>

#if defined (OS_WIN32)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>                 /* HMODULE */
#endif

extern PyTypeObject PyGedaPageObjectType;

static PyObject *ThisModule;

static PyObject *GedaError;
static char     *libgedapath;

#ifdef OS_WIN32
static HMODULE libgedathon;
#else
static void *libgedathon;
#endif

static char *LIB_LOCATION_ERROR = "error";

/* Note: LIBGEDATHON_PATH was defined during compilation
 * with trialing spaces for the file extension to be filled in */
static char installed_library_path[] = LIBGEDATHON_PATH;

static union { GedaCloserFunc func; void * obj; } closer;
static union { GedaLibInitializer func; void * obj; } initializer;

#define METHOD(symbol, aflag ) [ e##symbol ] = \
 { ASSOCIATED_METHOD(symbol), aflag, symbol##_docs },

/* Methods Table */
static struct {
   const char  *name;
   PyCFunction  func;
   int          aflag;
   const char  *docstring;

} GedaMethods[METHOD_COUNT + 1] = {
 [ method_unknown ] = { "unknown", do_unknown, 0, ""},
 #include "../include/geda_module.h"
};

/*!
 * \brief Add Libgedathon suffix
 * \par Function Description
 *  This is function adds the extension characters of the filename to
 *  the preset string that was statically allocated with enough space
 *  for these additional bytes.
 */
static const char *suffix_installed_library(const char *ext)
{


  libgedapath = installed_library_path;

  if (ext != NULL) {

    if (!strstr (installed_library_path, ext)) {

      char *ptr;

      ptr = installed_library_path;

      while (*ptr != '\0') ptr++;
      while (*ptr != 'n') ptr--;       /* last char in libgedathon */

      ptr++;

      while (*ext != '\0') *ptr++ = *ext++;
      *ptr = '\0';
    }
  }

  return libgedapath;
}

#ifdef OS_WIN32_NATIVE

char *get_lib_path(void)
{
  char *lib_dir;
  int   i, len;

  /* Allocate space to save the library string */
  lib_dir = malloc(MAX_PATH);

  len = strlen(installed_library_path);

  strcpy (lib_dir, installed_library_path);

  for (i = len - 1; i; i--) {
    if (lib_dir[i] == '/' || lib_dir[i] == '\\') {
      lib_dir[i] = '\0';
      break;
    }
  }

  return lib_dir;
}

/*!
 * \brief Find Libgedathon on a MS Windows system
 * \par Function Description
 *  This function returns the installed location of the library.
 *
 * \return [out] string file name of the library including the extension.
 */
static const char *find_library(void) {

  char  suffix[16];

  memset(&suffix[0], 0, 16);

  suffix[0] = '-';

  strcat (&suffix[0], LIBGEDATHON_VERSION);

  strcpy (strstr(&suffix[0], ":"), ".dll");

  suffix_installed_library(&suffix[0]);

  if (libgedapath == NULL)
    libgedapath = LIB_LOCATION_ERROR;

  return libgedapath;
}

#endif

/*!
 * \brief Find Libgedathon on a Linux system
 * \par Function Description
 *  This function returns the installed location of the library.
 *
 * \return [out] string file name of the library including the extension.
 */
#ifdef OS_LINUX
static const char *find_library(void) {

  suffix_installed_library(".so");

  if (libgedapath == NULL)
    libgedapath = LIB_LOCATION_ERROR;

  return libgedapath;
}
#endif

/*!
 * \brief Open Libgedathon
 * \par Function Description
 *  This is function attempts to establish a dynamic link to Libgedathon.
 *  This is unconvientional for nix's but the primary method used on MS
 *  Windows systems. Upon success, static initializer and closer unions
 *  are assigned functional address "values". Otherwise the unions are
 *  set to NULL pointers.
 *
 * \return [out] True if successful, otherwise False.
 */
static int open_library (void)
{
  int result;

#ifdef OS_WIN32_NATIVE

  /* libgeda is a dependency of libgedathon, obviously since libgedathon is an
   * interface for libgeda, so libgeda needs to be "found" to load libgedathon
   * This can be accomplished several different way;
   *   1. Load libgeda directly with something like:
   *           LoadLibrary(TEXT("c:/geda/bin/libgeda-52.dll"))
   *   2. Modify the PATH evironment variable to include the directory containing
   *      libgeda; $PATH:"/c/geda/bin", "c:\geda\bin" will not work.
   *   3. Make the directory containing libgeda the current directory before
   *      attempting to load libgedathon.
   *
   *  All three will work and all are a bit of hassle but the later seems the
   *  easiest approach.
   */

  char *cwd;
  char *dir_sav;
  char *lib_dir;

  /* Allocate space to save the current working directory */
  cwd = malloc(MAX_PATH);

  /* Save the current working directory */
  dir_sav = getcwd(cwd, MAX_PATH - 1);

  /* Retrieve the installed path */
  lib_dir = get_lib_path();

  /* Temporarily change working directory to installed location */
  chdir(lib_dir);

  free(lib_dir);

  /* Attempt to load using installed location */
  libgedathon = LoadLibrary(find_library());

  if (libgedathon == NULL) {

    /* Attempt to load using LD_LIBRARY_PATH or system, unlikely to work */
    libgedathon = LoadLibrary("libgedathon.dll");

    if (libgedathon == NULL) {
      fprintf(stderr, "Maybe try appending PATH with path to geda runtime libraries; export $PATH:\"/c/path/bin\"\n");
      fprintf(stderr, "If there was a message stating unable to find file \"ice-9/boot-9.scm\" in load path then\n");
      fprintf(stderr, "try setting GUILE_LOAD_PATH=c:/path/to/guile/1.8\"\n");
    }
  }

  /* Restore the original working directory */
  chdir(dir_sav);
  free(cwd);

  if (libgedathon != NULL) {

    /* is long* (LPFNDLLFUNC1) */
    initializer.obj = GetProcAddress(libgedathon, "initialize");

    if (initializer.obj != NULL) {
      result = 1;
      closer.obj = FreeLibrary;
    }
    else {
      FreeLibrary(libgedathon);
      closer.obj  = NULL;
      libgedathon = NULL;
      fprintf(stderr, "Error Could not initialize library: libgedathon.dll\n");
      result = 0;
    }
  }
  else

#elif defined OS_LINUX

  /* Attempt to load using LD_LIBRARY_PATH or system */
  libgedathon = dlopen("libgedathon.so", RTLD_LAZY);

  if (libgedathon == NULL) {

    /* Attempt to load using installed location */
    libgedathon = dlopen(find_library(), RTLD_LAZY);

    if (libgedathon == NULL) {
      fprintf(stderr, "Error dlopen: %s\n", dlerror());
    }
  }

  if (libgedathon != NULL) {

    initializer.obj = dlsym(libgedathon, "initialize");

    if (initializer.obj != NULL) {
      result = 1;
      closer.obj = dlclose;
    }
    else {
      dlclose(libgedathon);
      closer.obj  = NULL;
      libgedathon = NULL;
      fprintf(stderr, "Error Could not initialize library: %s\n", find_library());
      result = 0;
    }
  }
  else

#endif
  {
    fprintf(stderr, "Error Could not load library:libgedathon\n");
    result = 0;
  }

  return result;
}

/*!
 * \brief Close the Library Module
 * \par Function Description
 *  This is function calls the shutdown method in this module and then
 *  the libgedathon closer function.
 */
static void close_library (void)
{
  do_shutdown(NULL, NULL);

  if (closer.obj != NULL) {
    closer.func((void*)libgedathon);
  }
}

static API_FunctionTable PyGeda_API[METHOD_COUNT];

#ifndef PyMODINIT_FUNC  /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif

/*!
 * \brief Geda Python Module Initializer
 * \par Function Description
 *  This is function serves to initialize the Python module and all
 *  sub-modules. The function registers methods and calls the initializer
 *  in Libgedathon passing a method table to be filled in by the library.
 *  This function is the only function symbolically exported from the
 *  module.
 */
PyMODINIT_FUNC initgeda(void)
{
  int i;

  GedaMethods[METHOD_COUNT].name  = NULL;
  GedaMethods[METHOD_COUNT].aflag = 0;

  if (!open_library())
    return;

  atexit(close_library);

  initializer.func(PyGeda_API);

  /* Remap docstrings in the table, note underscore macro MUST be used here */
  for ( i = 1; i < METHOD_COUNT; i++) {
   GedaMethods[i].docstring = _(GedaMethods[i].docstring);
  }

  ThisModule = Py_InitModule("geda", (PyMethodDef*)GedaMethods);
  if (ThisModule == NULL)
    return;

  initConstants(ThisModule);
  initPage(ThisModule);
  initPyGedaObject(ThisModule);
  initFunctions(ThisModule);

  GedaError = PyErr_NewException("Geda.error", NULL, NULL);
  Py_INCREF(GedaError);

  /* Register Object Types */
  PyModule_AddObject(ThisModule, "error", GedaError);

}

/*!
 * \brief BlockMethod function in Geda Libgedathon API Library
 * \par Function Description
 *  This is an internal function called at the beginning of each method
 *  with the use of the ON_METHOD_ENTRY macro. The function blocks calling
 *  threads until any existing thread complete execution of the method and
 *  this allow the module to be somewhat re-entrant. After a pre-determine
 *  time period the function will give up and return to the caller, assuming
 *  the thread is locked by a dead thread.
 */
static inline void BlockMethod (int index)
{
  int deadman = 0;
  while (PyGeda_API[index]->status && METHOD_ACTIVE) {
    sleep (MODULE_WAIT_INTERVAL); /* sleep for Some time for action to complete. */
    deadman++;
    if (deadman == MAX_WAIT_FOR_MODULE) {
      fprintf (stderr, "Error: Method <%s> is not releasing status flag, likely is hung\n", PyGeda_API[index]->name);
      return;
    }
  }
  PyGeda_API[index]->status |= METHOD_ACTIVE; /* Block this Method */
}

#define METHOD_HELP(symbol)GedaMethods[e##symbol].docstring
#define METHOD(func) static PyObject *do_##func(PyObject *self, PyObject *args)

#define ON_METHOD_ENTRY(returns, token, needs) BlockMethod(e##token); \
        union { PyGeda_##returns##_##needs##_type func; void * obj; } library; \
        library.obj = PyGeda_API[e##token]->func;

#define ON_METHOD_EXIT(token) \
        PyGeda_API[e##token]->status &= ~METHOD_ACTIVE; /* Unblock this Method */

#define TYPE_INT_C1(symbol)            ON_METHOD_ENTRY(int, symbol, c1)
#define TYPE_INT_INT(symbol)           ON_METHOD_ENTRY(int, symbol, i1)
#define TYPE_INT_I1C1(symbol)          ON_METHOD_ENTRY(int, symbol, i1c1)
#define TYPE_INT_P1(symbol)            ON_METHOD_ENTRY(int, symbol, p1)
#define TYPE_INT_P3(symbol)            ON_METHOD_ENTRY(int, symbol, p3)

#define TYPE_PYOBJECT_I1(symbol)       ON_METHOD_ENTRY(pyobject, symbol, i1)
#define TYPE_PYOBJECT_I2(symbol)       ON_METHOD_ENTRY(pyobject, symbol, i2)
#define TYPE_PYOBJECT_I3(symbol)       ON_METHOD_ENTRY(pyobject, symbol, i3)
#define TYPE_PYOBJECT_I3P1(symbol)     ON_METHOD_ENTRY(pyobject, symbol, i3p1)
#define TYPE_PYOBJECT_I4P1(symbol)     ON_METHOD_ENTRY(pyobject, symbol, i4p1)
#define TYPE_PYOBJECT_I5P1(symbol)     ON_METHOD_ENTRY(pyobject, symbol, i5p1)

#define TYPE_PYOBJECT_C1(symbol)       ON_METHOD_ENTRY(pyobject, symbol, c1)
#define TYPE_PYOBJECT_C1I1(symbol)     ON_METHOD_ENTRY(pyobject, symbol, c1i1)
#define TYPE_PYOBJECT_C1I4P1(symbol)   ON_METHOD_ENTRY(pyobject, symbol, c1i4p1)
#define TYPE_PYOBJECT_C1I5(symbol)     ON_METHOD_ENTRY(pyobject, symbol, c1i5)
#define TYPE_PYOBJECT_C1I5P1(symbol)   ON_METHOD_ENTRY(pyobject, symbol, c1i5p1)
#define TYPE_PYOBJECT_C1I6(symbol)     ON_METHOD_ENTRY(pyobject, symbol, c1i6)
#define TYPE_PYOBJECT_C1I7(symbol)     ON_METHOD_ENTRY(pyobject, symbol, c1i7)
#define TYPE_PYOBJECT_C1I9(symbol)     ON_METHOD_ENTRY(pyobject, symbol, c1i9)
#define TYPE_PYOBJECT_C2I8(symbol)     ON_METHOD_ENTRY(pyobject, symbol, c2i8)
#define TYPE_PYOBJECT_C2I6P1(symbol)   ON_METHOD_ENTRY(pyobject, symbol, c2i6p1)

#define TYPE_PYOBJECT_P1(symbol)       ON_METHOD_ENTRY(pyobject, symbol, p1)
#define TYPE_PYOBJECT_P1I2(symbol)     ON_METHOD_ENTRY(pyobject, symbol, p1i2)
#define TYPE_PYOBJECT_P1C1(symbol)     ON_METHOD_ENTRY(pyobject, symbol, p1c1)

#define TYPE_PYOBJECT_P2C2I1(symbol)   ON_METHOD_ENTRY(pyobject, symbol, p2c2i1)

#define TYPE_PYOBJECT_V1(symbol)       ON_METHOD_ENTRY(pyobject, symbol, v1)

#define TYPE_VOID_V1(symbol)           ON_METHOD_ENTRY(void, symbol, v1)

/** *********************** Begin Wrapper Functions **************************/

typedef int       (*PyGeda_int_c1_type)          ( const char* );
typedef int       (*PyGeda_int_i1_type)          ( int );
typedef int       (*PyGeda_int_i1c1_type)        ( int, const char* );
typedef int       (*PyGeda_int_p1_type)          ( PyObject * );
typedef int       (*PyGeda_int_p3_type)          ( PyObject *, PyObject *, PyObject * );

typedef PyObject* (*PyGeda_pyobject_i1_type)     ( int pid );
typedef PyObject* (*PyGeda_pyobject_i2_type)     ( int, int);
typedef PyObject* (*PyGeda_pyobject_i3_type)     ( int, int, int);
typedef PyObject* (*PyGeda_pyobject_i3p1_type)   ( int, int, int, PyObject *);
typedef PyObject* (*PyGeda_pyobject_i4p1_type)   ( int, int, int, int, PyObject * );
typedef PyObject* (*PyGeda_pyobject_i5p1_type)   ( int, int, int, int, int, PyObject * );

typedef PyObject* (*PyGeda_pyobject_c1_type)     ( const char* );
typedef PyObject* (*PyGeda_pyobject_c1i1_type)   ( const char*, int);
typedef PyObject* (*PyGeda_pyobject_c1i4p1_type) ( const char*, int, int, int, int, PyObject * );
typedef PyObject* (*PyGeda_pyobject_c1i5_type)   ( const char*, int, int, int, int, int);
typedef PyObject* (*PyGeda_pyobject_c1i5p1_type) ( const char*, int, int, int, int, int, PyObject * );
typedef PyObject* (*PyGeda_pyobject_c1i6_type)   ( const char*, int, int, int, int, int, int);
typedef PyObject* (*PyGeda_pyobject_c1i7_type)   ( const char*, int, int, int, int, int, int, int);
typedef PyObject* (*PyGeda_pyobject_c1i9_type)   ( const char*, int, int, int, int, int, int, int, int, int);
typedef PyObject* (*PyGeda_pyobject_c2i8_type)   ( const char*, const char*, int, int, int, int, int, int, int, int);
typedef PyObject* (*PyGeda_pyobject_c2i6p1_type) ( const char*, const char*, int, int, int, int, int, int, PyObject * );

typedef PyObject* (*PyGeda_pyobject_p1_type)     ( PyObject * );
typedef PyObject* (*PyGeda_pyobject_p1i2_type)   ( PyObject *, int, int);
typedef PyObject* (*PyGeda_pyobject_p1c1_type)   ( PyObject *, const char*);

typedef PyObject* (*PyGeda_pyobject_p2c2i1_type) ( PyObject *, PyObject *, const char*, const char*, int);

typedef PyObject* (*PyGeda_pyobject_v1_type)     ( void );

typedef void      (*PyGeda_void_v1_type)         ( void );

/** \defgroup Python_API_Methods Geda Python Module API Methods
 *  @{
 */

/*!
 * \brief Unknown function Geda Libgedathon API Library
 * \par Function Description
 *  This function is a place holder in the Module's function table and
 *  would be only be called if a NULL function pointer was used to access
 *  a method within this module.
 *
 * \warning Do not call this method!.
 */
METHOD(unknown)
{
  fprintf(stderr, "unknown method handler\n");

  Py_INCREF(Py_None);

  return Py_None;
}

/*!
 * \brief Shutdown the Geda Libgedathon API Library
 * \par Function Description
 *  This function is automatically called by Python when the program is
 *  terminated, either normally or due to an error condition.
 *
 * \warning Do not call this method!.
 */
METHOD(shutdown)
{
  TYPE_VOID_V1(shutdown)

  library.func();

  ON_METHOD_EXIT(shutdown);

  Py_INCREF(Py_None);

  return Py_None;
}

/*!
 * \brief Append a Path to the Library Search Path
 * \par Method Description
 *  This function provides a method to add a directory to the library's
 *  search path. This will remains valid for the current session of the
 *  library.
 *
 * \sa declare_local_sym
 *
 *  param [in] path String, a valid directory path.
 *
 * \return [out] True if successful, otherwise False.
 *
 *  example:
 * \code
 *          geda.append_symbol_path(p1sym/)  # Add "p1sym/" folder to library search path
 * \endcode
 */
METHOD(append_symbol_path)
{
  TYPE_INT_C1(append_symbol_path);
  const char *path;
  int         status;

  if (!PyArg_ParseTuple(args, "s:geda.append_symbol_path", &path)) {

    const char *syntax = "syntax: append_symbol_path(valid path)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(path);

  ON_METHOD_EXIT(append_symbol_path);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Declare Local Symbols
 * \par Method Description
 *  This function provides a convenience method to create both a directory in
 *  the current working directory with the given name and to create a file with
 *  the name "gafrc" in the current directory containing instructions to search
 *  the subdirectory for symbols. If the directory argument is not specified
 *  then the directory will be named "sym". The purpose of gafrc files is to
 *  dynamically modify configuration environments, in this case to allow custom
 *  symbols, but other configuration keywords can also be set using the gafrc
 *  files, see the document for LibGeda for more information.
 *  If a gafrc file already is exist, the file will be parsed and amended to
 *  include the the given directory in the search library path as required.
 *
 * \sa append_symbol_path
 *
 *  Optional argument:
 *
 *  param [in] directory String, a directory where "gafrc" file be created.
 *
 * \return [out] Returns FALSE if the path was appended, a non-zero return
 *               value indicates an error occurred.
 *
 * example:
 * \code
 *          geda.declare_local_sym()  # create a "sym/" folder and local rc file
 * \endcode
 *
 * \note 1. The gafrc files are only acknowledged when a schematic file is opened
 *          in the same directory. The new gafrc fill will contain a single line:
 *
 *          (component-library "./sym")
 */
METHOD(declare_local_sym)
{
  TYPE_INT_C1(declare_local_sym);
  const char *directory = NULL;
  int         status;

  if (!PyArg_ParseTuple(args, "|s:geda.declare_local_sym", &directory)) {

    const char *syntax = "syntax: declare_local_sym([folder])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(directory);

  ON_METHOD_EXIT(declare_local_sym);

  /* If status is not 1 there was an error */
  if (status != 1) {
    PyErr_SetString(PyExc_StandardError, strerror(status));
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Get a list of all Pages open by the Library
 * \par Method Description
 *  This function provides a method to get a list of Page objects of
 *  all of the currently opened pages.
 *
 * \sa goto_page
 *
 * \returns [out] a PyList of Geda Page objects.
 */
METHOD(get_pages)
{
  TYPE_PYOBJECT_V1(get_pages);
  PyObject *py_input_list;
  PyObject *py_output_list;

  py_input_list  = library.func();
  py_output_list = PyList_New(0);

  if (PyList_Check(py_input_list)) {

    int i, count;
    count = (int) PyList_GET_SIZE(py_input_list);

    for (i = 0; i < count ; i++) {

      PyObject *page_info;
      PyObject *py_page;

      page_info = PyList_GET_ITEM(py_input_list, i);
      py_page   = PyObject_CallObject((PyObject*)PyGedaPageClass(), page_info);

      if(py_page && PyObject_Type(py_page)) {
        PyList_Append(py_output_list, py_page);
      }
    }
  }

  Py_XDECREF(py_input_list);  /* Destroy the input list */

  ON_METHOD_EXIT(get_pages);  /* re-enable this method */

  return py_output_list;      /* return the Python list of page Objects */
}

/*!
 * \brief Get the Active Page
 * \par Method Description
 *  This function provides a method to get Page object representing the
 *  current active page.
 *
 * \sa goto_page
 *
 * \return [out] Returns a Geda Page object or Py_None if there no pages
 *               a currently opened.
 */
METHOD(get_active_page)
{
  TYPE_PYOBJECT_V1(get_active_page);
  PyObject *info;
  PyObject *page;

  info = library.func();
  if (info) {
    page = PyObject_CallObject((PyObject*)PyGedaPageClass(), info);
  }
  else {
    Py_INCREF(Py_None);
    page = Py_None;
  }

  ON_METHOD_EXIT(get_active_page);

  return page;
}

/*!
 * \brief Set an Existing Page to be the Active Page
 * \par Method Description
 *  This function provides a method to set a page object to be the
 *  Active Page Object. This method is similar to goto_page but does
 *  not restore the current working directory of the page.
 *
 * \sa goto_page
 *
 * [in] page    A GedaPage object.
 *
 * \return [out] True if successful, otherwise False.
 *
 * \note 1. Although this Library feature is utilize in Geda application
 *          like gschem, the Python API library does not currently exploit
 *          this feature. All methods requiring a Page object must be passed
 *          a Page object, no assumption is made regarding which Page object
 *          to use for a particular operation.
 *
 * TODO: change to also except an integer PID
 */
METHOD(set_active_page)
{
  TYPE_INT_INT(set_active_page);
  PyObject *page;
  int       pid;
  int       status;

  if(!PyArg_ParseTuple(args, "O!:geda.set_active_page", PyGedaPageClass(), &page))
  {
    const char *syntax = "syntax: set_active_page(PyGedaPageObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  pid = ((PyGedaPageObject*)page)->pid;
  status = library.func(pid);

  ON_METHOD_EXIT(set_active_page);

  if (!status) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Get is Page Modified
 * \par Method Description
 *  This function provides a method to retrieve a Page is_modified flag.
 *
 *  [in] PyPage page A GedaPage Object
 *
 * \return [out] True if page has unsaved changes, otherwise False.
 */
METHOD(is_page_modified)
{
  TYPE_INT_INT(is_page_modified);
  PyGedaPageObject *page;

  if(!PyArg_ParseTuple(args, "O!:geda.is_page_modified", PyGedaPageClass(), &page))
  {
    const char *syntax = "syntax: is_page_modified(PyGedaPageObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  page->modified = library.func(page->pid);

  ON_METHOD_EXIT(is_page_modified);

  if (!page->modified) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Set an Existing Page to be the Current Page
 * \par Method Description
 *  This function provides a method to set a page object to be the
 *  Current Page Object. This method is similar to set_active_page but
 *  unlike set_active_page, this method restores the current working
 *  directory of the page.
 *
 * \sa set_active_page
 *
 *  [in] page    A GedaPage object.
 *
 * \return [out] True if successfull, otherwise False.
 *
 * \note 1. Although this Library feature is utilize in Geda application
 *          like gschem, the Python API library does not currently exploit
 *          this feature. All methods requiring a Page object must be passed
 *          a Page object, no assumption is made regarding which Page object
 *          is to be used for a particular operation.
 */
METHOD(goto_page)
{
  TYPE_INT_INT(goto_page);
  PyObject *page;
  int       pid;
  int       status;

  if(!PyArg_ParseTuple(args, "O!:geda.goto_page", PyGedaPageClass(), &page))
  {
    const char *syntax = "syntax: goto_page(PyGedaPageObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  pid = ((PyGedaPageObject*)page)->pid;
  status = library.func(pid);

  ON_METHOD_EXIT(goto_page);

  if (!status) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Open an Existing Page
 * \par Method Description
 *  This function provides a method to open a existing schematic or symbol
 *  file. If a file name is not provided a default name will be used, usually
 *  "untitled".
 *
 * \sa close_page
 *
 *  [in] name    String file name of the file to open
 *
 * \return [out] page  A GedaPage object.
 *
 *  example:
 * \code
 *          schematic = geda.open_page("~/geda/filters/lowpass/butherworth.sch")
 * \endcode
 */
METHOD(open_page)
{
  TYPE_PYOBJECT_C1(open_page);

  const char *filename = NULL;
  PyObject   *info;
  PyObject   *page;

  if (!PyArg_ParseTuple(args, "|s:geda.open_page", &filename)) {

    const char *syntax = "syntax: open_page(filename)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  info = library.func(filename);
  page = PyObject_CallObject((PyObject*)PyGedaPageClass(), info);

  ON_METHOD_EXIT(open_page);
  if (page) {
      PyErr_Clear();
  }

  return page;
}

/*!
 * \brief Create a New Page Object
 * \par Method Description
 *  This function provides a method to create a new empty Page object.
 *
 *  [in] page PyGedaPage Object to be closed
 *
 *  optional argument:
 *
 *  [in] save Integer flag, if True then the page is saved before closing
 *
 * \return [out] status True if success, otherwise False.
 *
 * \note 1. If the overwrite flag is not provide, any existing file might still
 *          be backed-up depending on the configuration variable "make_backup_files",
 *          which default to True. However, use of the overwrite flag will over-ride
 *          the configuration setting, so if the second argument evaluate to True
 *          then any existing files will be over-written without making a backup.
 *
 *  example:
 * \code
 *          geda.close_page(schematic)
 * \endcode
 */
METHOD(close_page)
{
  TYPE_INT_INT(close_page);
  PyObject *page;
  int       do_save = 0;
  int       status  = 1;

  if(!PyArg_ParseTuple(args, "O!|i:geda.close_page", PyGedaPageClass(), &page, &do_save))
  {
    const char *syntax = "syntax: close_page(PyGedaPageObject [, save])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (do_save) {
    status = PyInt_AsLong(PyObject_CallMethod(ThisModule, "save_page", "O", page));
  }

  if (status) {
    library.func(((PyGedaPageObject*)page)->pid);
  }

  ON_METHOD_EXIT(close_page);
  if (status == 0) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Create a New Page Object
 * \par Method Description
 *  This function provides a method to create a new empty Page object.
 *
 *  [in] file     String name if the file for the Page
 *
 *  optional argument:
 *
 *  [in] overwrite Integer flag if existing file should be over-written
 *
 * \return [out] A GedaPage Object if success, otherwise Py_None.
 *
 * \note 1. If the overwrite flag is not provided, an existing file might still
 *          be backed-up depending on the configuration variable "make_backup_files",
 *          which defaults to True. However, use of the overwrite flag will over-ride
 *          the configuration setting, so if the second argument evaluate to True
 *          then any existing files will be over-written without making a backup.
 *  example:
 * \code
 *          schematic = geda.new_page("~/sch/oscillator.sch", True)
 * \endcode
 */
METHOD(new_page)
{
  TYPE_PYOBJECT_C1I1(new_page);

  const char *filename;
  PyObject   *info;
  PyObject   *page;

  int over_write = -1;

  if (!PyArg_ParseTuple(args, "|si:geda.new_page", &filename, &over_write))
  {
    const char *syntax = "syntax: new_page([filename] [, over_write])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  info = library.func(filename, over_write);

#if DEBUG
  char *name;
  int   pid;
  if (PyArg_ParseTuple(info, "si:geda.new_page", &name, &pid))
    fprintf(stderr, "new_page: info filename=%s, pid=%d\n", name, pid);
#endif

  page = PyObject_CallObject((PyObject*)PyGedaPageClass(), info);

  ON_METHOD_EXIT(new_page);

  return page;
}

/*!
 * \brief Rename a Page Objects
 * \par Method Description
 *  This function provides a method to rename a Page; in effect change
 *  the file name. This method corresponds to the libgedathon function
 *  PyGeda_rename_page.
 *
 *  [in] PyPage page A GedaPage Object
 *  [in] name   String the new name property
 *
 * \return [out] status True if success, otherwise False.
 */
METHOD(rename_page)
{
  TYPE_INT_I1C1(rename_page);
  PyObject *page;
  int       status;
  char     *new_name;

  if(!PyArg_ParseTuple(args, "O!s:geda.rename_page", PyGedaPageClass(),
                       &page, &new_name))
  {
    const char *syntax = "syntax: rename_page(PyGedaPageObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(((PyGedaPageObject*)page)->pid, new_name);

  /* If success then update the PyGedaPage object */
  if (status) {

    Py_DECREF(((PyGedaPageObject*)page)->filename);

    ((PyGedaPageObject*)page)->filename = PyString_FromString(new_name);

  }
  else {
    const char *fail = "Error: rename page failed";
    PyErr_SetString(PyExc_StandardError, fail);
  }

  ON_METHOD_EXIT(rename_page);

  if (status == 0) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Save Page Objects
 * \par Method Description
 *  This function provides a method to save a Page type objects to
 *  disk.
 *
 *  [in] PyPage page A GedaPage Object
 *
 * \return [out] status True if success, otherwise False.
 */
METHOD(save_page)
{
  TYPE_INT_INT(save_page);
  PyObject *page=NULL;
  int       status;

  if (!PyArg_ParseTuple(args, "O!:geda.save_page", PyGedaPageClass(), &page))
  {
    const char *syntax = "syntax: save_page(PyGedaPageObject)";
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(((PyGedaPageObject*)page)->pid);

  ON_METHOD_EXIT(save_page);
  if (status != 0) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Save a Page Objects with a new file name
 * \par Method Description
 *  This function provides a method to rename and save a Page single
 *  call to the library. This method corresponds to the libgedathon
 *  function PyGeda_save_page_as.
 *
 *  [in] PyPage page A GedaPage Object
 *  [in] name   String the new name property
 *
 * \return [out] status True if success, otherwise False.
 */
METHOD(save_page_as)
{
  TYPE_INT_I1C1(save_page_as);
  PyObject *page;
  int       status;
  char     *new_name;

  if(!PyArg_ParseTuple(args, "O!s:geda.save_page_as", PyGedaPageClass(),
                       &page, &new_name))
  {
    const char *syntax = "syntax: save_page_as(PyGedaPageObject, string)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(((PyGedaPageObject*)page)->pid, new_name);

  /* If success then update the PyGedaPage object */
  if (status) {

    Py_DECREF(((PyGedaPageObject*)page)->filename);

    ((PyGedaPageObject*)page)->filename = PyString_FromString(new_name);

  }
  else {
    const char *fail = "Error: save_page_as page failed";
    PyErr_SetString(PyExc_StandardError, fail);
  }

  ON_METHOD_EXIT(save_page_as);
  if (status == 0) {
    Py_RETURN_FALSE;
  }

  PyErr_Clear();

  Py_RETURN_TRUE;
}

/*!
 * \brief Save All Page Objects
 * \ingroup Python_API_Methods
 * \par Method Description
 *  This function provides a method to save all Page type objects
 *  that are opened or optionally, all Page objects in a given list.
 *
 *  [in] PyList Optional list of Page objects
 *
 * \return [out] integer equal to the number of errors, FALSE means there
 *               were no errors.
 */
METHOD(save_all_pages)
{
  TYPE_INT_P1(save_all_pages);
  PyObject *pages;
  int       status;

  if(!PyArg_ParseTuple(args, "|O!:geda.save_all_pages", &PyList_Type, &pages))
  {
    const char *syntax = "syntax: save_all_pages(PyList_Type[PyGedaPageObject,...])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }
  else{
    Py_INCREF(Py_None);
    pages = Py_None;
  }

  status = library.func(pages);

  ON_METHOD_EXIT(save_all_pages);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Determine if object is a type of GedaCapsule Object
 * \par Method Description
 *  This function provides a method to check an object is a GedaCapsule type.
 *  This method is used by other methods internally and is not normally used by
 *  Python programs.
 *
 *  [in] PyObject object PyObject to be checked
 *
 * \return [out] True if the object is GedaCapsule, otherwise False.
 */
METHOD(GedaCapsule_Type)
{
  TYPE_INT_P1(GedaCapsule_Type);

  int answer = library.func(args);

  ON_METHOD_EXIT(GedaCapsule_Type);

  return Py_BuildValue("i", answer);
}

/*!
 * \brief Get an the Bounds of a GedaObject
 * \par Method Description
 *  This function provides a method to retrieve the bounds of an object.
 *  The object can be a PyGedaPageObject, PyGedaObject or PyGedaCapsule.
 *  If the argument is a PyGedaPageObject, the bounds of all object on
 *  the page is returned or an empty list if there are no object on the
 *  page. If argument is a valid PyGedaObject or PyGedaCapsule, then the
 *  bounds of the object is returned. If the argument is not a valid object,
 *  or an error is encountered, then NULL is returned.
 *
 *  [in] PyObject A Page, object or capsule containing an object
 *
 * \return [out] A PyList of four integers; left, top, right, bottom,
 *               or and empty list if there is no bounds, or NULL if
 *               an error occurred.
 */
METHOD(get_bounds)
{
  TYPE_PYOBJECT_I2(get_bounds);
  PyObject *unknown;
  PyObject *py_object;
  PyObject *list;
  int       pid;      /* Page Id     */
  int       sid;      /* Sequence Id */

  const char *syntax = "syntax: get_bounds(PyGedaPageObject | PyGedaObject)";

  if(!PyArg_ParseTuple(args, "O:geda.get_bounds", &unknown)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(unknown, PyGedaPageClass())) {
    pid = ((PyGedaPageObject*)unknown)->pid;
    sid = -1;
  }
  else if (PyObject_TypeCheck(unknown, PyGedaObjectClass())) {
    pid = ((PyGedaObject*)unknown)->pid; /* which could be -1 (not on a page) */
    sid = ((PyGedaObject*)unknown)->sid;
  }
  else if (do_GedaCapsule_Type(self, args)) {
    py_object = do_get_object(self, args);
    pid = ((PyGedaObject*)py_object)->pid;
    sid = ((PyGedaObject*)py_object)->sid;
  }
  else {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  list = library.func(pid, sid);

  ON_METHOD_EXIT(get_bounds);

  return list;
}

/*!
 * \brief Get an Object from GedaCapsuleObject
 * \par Method Description
 *  This function provides a method to create PyPyGedaObjects from a GedaCapsule
 *  object but is not normally called directly. This method is used by other methods
 *  to get an Python version of the object contained within a Geda capsule.
 *
 *  [in] PyObject capsule  The container object
 *
 * \return [out] A PyGedaObject.
 */
METHOD(get_object)
{
  TYPE_PYOBJECT_P1(get_object);

  PyObject *py_capsule;
  PyObject *object_data;
  PyObject *py_object;
  int type;

  const char *syntax = "syntax: get_object(GedaCapsuleObject)";

  if (!PyArg_ParseTuple(args, "O:geda.get_object", &py_capsule)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(py_capsule, PyGedaObjectClass())) {
    py_object = py_capsule; /* was an object not capsule so give back */
  }
  else {

    if (!do_GedaCapsule_Type(self, py_capsule)) {
      PyErr_SetString(PyExc_TypeError, syntax);
      py_object = NULL;
    }
    else {

      object_data = library.func(py_capsule);

      type = ((GedaCapsule*)py_capsule)->type;

      switch (type) {
        case OBJ_PLACEHOLDER:
        case OBJ_COMPLEX:
          py_object = PyObject_CallObject((PyObject*)PyGedaComplexClass(), object_data);
          break;
        case OBJ_TEXT:
          py_object = PyObject_CallObject((PyObject*)PyGedaTextClass(), object_data);
          break;
        case OBJ_NET:
          py_object = PyObject_CallObject((PyObject*)PyGedaNetClass(), object_data);
          break;
        case OBJ_LINE:
          py_object = PyObject_CallObject((PyObject*)PyGedaLineClass(), object_data);
          break;
        case OBJ_PATH:
          py_object = PyObject_CallObject((PyObject*)PyGedaPathClass(), object_data);
          break;
        case OBJ_BOX:
          py_object = PyObject_CallObject((PyObject*)PyGedaBoxClass(), object_data);
          break;
        case OBJ_PICTURE:
          py_object = PyObject_CallObject((PyObject*)PyGedaPictureClass(), object_data);
          break;
        case OBJ_CIRCLE:
          py_object = PyObject_CallObject((PyObject*)PyGedaCircleClass(), object_data);
          break;
        case OBJ_BUS:
          py_object = PyObject_CallObject((PyObject*)PyGedaBusClass(), object_data);
          break;
        case OBJ_PIN:
          py_object = PyObject_CallObject((PyObject*)PyGedaPinClass(), object_data);
          break;
        case OBJ_ARC:
          py_object = PyObject_CallObject((PyObject*)PyGedaArcClass(), object_data);
          break;
        default:
          PyErr_SetString(PyExc_TypeError, "Bad Capsule object");
          py_object = NULL;
      }
    }
  }

  ON_METHOD_EXIT(get_object);

  return py_object;
}

/*!
 * \brief Get a List Objects from an Object
 * \par Method Description
 *    This function provides a method to get a list of existing objects from another
 *  object. The source object can be a Page or another object. Returned capsule items
 *  can be extracted and converted to PyGedaObject using the get_object method.
 *
 *    Encapsulation of objects is performed for efficiency and memory management.
 *  If real PyGedaObjects had to be created with a statement like the one used in
 *  example 1, the time required for Python to manage the memory for large schematics,
 *  would approach "hard-disk" access times. And when the list was later dereferenced,
 *  similar delays would occur while Python was performing garbage collection.
 *
 *  [in] PyObject source  The object from which to obtain the sub-objects
 *
 * \return [out] PyList list of GedaCapsule Objects or Py_None if the source object
 *                      did not contain any objects.
 *
 *  example:
 * \code
 *          objects = geda.get_objects(schematic)
 *          print("objects on page with ID={0}, filename={1}".format(schematic.pid,schematic.filename()))
 *          print objects # prints information for all object on a page
 * \endcode
 *
 * \note 1. The GedaCapsules contained in the returned list indirectly references
 *          objects. Destroying the capsule does not destroy the objects referenced
 *          within. GedaCapsule are not derived from Python Capsules, but instead are
 *          derived from Python's base class, PyObject. There is no reason to explicitly
 *          destroy a GedaCapsule object. The dynamically allocated memory referenced
 *          within a GedaCapsule is managed by LibGeda and not by Python. All such
 *          memory is released though gobject destructor mechanisms.
 *
 */
METHOD(get_objects)
{
  TYPE_PYOBJECT_I2(get_objects);
  PyObject *unknown;
  PyObject *list;
  int       pid;
  int       sid;

  const char *syntax = "syntax: get_objects(PyGedaPageObject | PyGedaObject)";

  if(!PyArg_ParseTuple(args, "O:geda.get_objects", &unknown)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(unknown, PyGedaPageClass())) {
    pid = ((PyGedaPageObject*)unknown)->pid;
    sid = -1;
  }
  else if (PyObject_TypeCheck(unknown, PyGedaObjectClass())) {
    pid = ((PyGedaObject*)unknown)->pid; /* which could be -1 (not on a page) */
    sid = ((PyGedaObject*)unknown)->sid;
  }
  else {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  list = library.func(pid, sid);

  ON_METHOD_EXIT(get_objects);

  return list;
}

/*!
 * \brief Add an Object to an Object
 * \par Method Description
 *  This function provides a method to add an existing object to another object.
 *  The target object can be a Page or another object.
 *
 *  [in] PyObject parent The Geda object to receive the child
 *  [in] PyObject child The Geda object to be add to the parent
 *
 * \return [out] status True if successful, otherwise False.
 *
 *  example 1.
 * \code
 *           geda.add_object(schematic, titleblock) # Add titleblock complex to a Page object
 * \endcode
 *  example 2.
 * \code
 *           geda.add_object(pin, pinlabel) # Add label (text) attribute to Pin object
 * \endcode
 * \note 1. At this time, only attributes, aka Text Objects, can be added to other
 *          other objects. The parent object need not be a Complex, attributes can
 *          be attached to other object type including Graphical objects.
 *
 * \note 2. For symbol files, objects that comprise the symbol, like pins, circles, lines,
 *          etc., are added to the page not a complex.
 */
METHOD(add_object)
{
  TYPE_INT_P3(add_object);
  PyObject *page;
  PyObject *py_object_A;
  PyObject *py_object_B;
  int       status;

  if(!PyArg_ParseTuple(args, "OO!:geda.add_object",
                       &py_object_A,
                       PyGedaObjectClass(), &py_object_B))
  {
    const char *syntax = "syntax: add_object(Page || PyGedaObject, PyGedaObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  page = NULL;

  if (PyObject_TypeCheck(py_object_A, PyGedaPageClass())) {
    page = py_object_A;
    py_object_A = NULL;
  }
  else {
    if (!PyObject_TypeCheck(py_object_A, PyGedaObjectClass())) {
      PyErr_SetString(PyExc_TypeError, "parameter 1 is not a Page nor a PyGedaObject");
    }
  }

  status = library.func(page, py_object_A, py_object_B);

  ON_METHOD_EXIT(add_object);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Add Objects to an Object
 * \par Method Description
 *  This function provides a method to add a list of existing object to another
 *  object. The target object can be a Page or another object.
 *
 * \sa add_object
 *
 *  [in] PyObject parent The Geda object to receive the child
 *  [in] PyList   list   The list of Geda objects to be add to the parent
 *
 * \return [out] status True if successful, otherwise False.
 *
 *  example 1.
 * \code
 *          geda.add_objects(schematic, ResistorList)
 * \endcode
 */
METHOD(add_objects)
{
  TYPE_INT_P3(add_objects);
  PyObject *page;
  PyObject *py_object_A;
  PyObject *py_object_B;
  int       status;

  if (!PyArg_ParseTuple(args, "OO!:geda.add_objects, Object PyList",
                        &py_object_A, &PyList_Type, &py_object_B))
  {
    const char *syntax = "syntax: add_objects(Page || PyGedaObject, PyList_Type[PyGedaObject,...])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(py_object_A, PyGedaPageClass())) {
    page = py_object_A;
    py_object_A = NULL;
  }
  else {
    page = NULL;
  }

  status = library.func(page, py_object_A, py_object_B);

  ON_METHOD_EXIT(add_objects);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Copy an Object
 * \par Method Description
 *  This function provides a method to Copy an existing object. The Object
 *  does not have to be on a Page. The object argument can be an actual
 *  PyGedaObject, such as PyGedaComplexObject_type, or the object can be
 *  a GedaCapsule object, such as those return by get_objects.
 *
 *  [in] PyObject The Geda object to be copied
 *
 *  Optional arguments:
 *
 *  [in] x  Integer X offset relative to the source object's location
 *  [in] y  Integer X offset relative to the source object's location
 *
 * \note 1. The target offset arguments are mutually optional, either both must be
 *          provided or neither. If offsets arguments are not provide the copy will
 *          coincide with the original.
 *
 * \return [out] PyObject if successful otherwise False. The returned PyGedaObject
 *               is the real instance of the copied object, even if the argument
 *               was a capsule object.
 *
 *  example 1.
 * \code
 *          amp2 = amp1.copy(4800, -200)
 * \endcode
 *
 *  example 2.
 * \code
 *          x = RightSpeaker.x
 *          y = RightSpeaker.y + 1000
 *          LeftSpeaker = geda.copy_object(RightSpeaker, x, y)
 * \endcode
 */
METHOD(copy_object)
{
  TYPE_PYOBJECT_P1I2(copy_object);
  PyObject *py_capsule;
  PyObject *py_object;
  PyObject *py_object_A;
  PyObject *py_object_B;
  PyObject *py_object_D;
  int       dx = -1;
  int       dy = -1;

  const char *syntax = "syntax: copy_object(PyGedaObject || GedaCapsuleObject [, dx, dy])";

  if (!PyArg_ParseTuple(args, "O|ii:geda.copy_object, Object PyList", &py_object, &dx, &dy))
  {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(py_object, PyGedaObjectClass())) {
    py_object_A = py_object;
    py_object_D = NULL;
  }
  else {
    if (do_GedaCapsule_Type(self, py_object)) {
      py_object_A = py_object_D = do_get_object(self, py_object);
      py_object_B = NULL;
    }
    else {
      PyErr_SetString(PyExc_TypeError, syntax);
      py_object_A = NULL;
      py_object_B = NULL;
    }
  }

  if (py_object_A) { /* Either passed-in or extracted from capsule */

    py_capsule = library.func(py_object_A, dx, dy);

    if (py_capsule) {

      if (py_object_D) { /* If passed capsule return capsule */
        py_object_B = py_capsule;
      }
      else { /* extract new object from capsule */

        py_object_B = do_get_object(self, py_capsule);

        if (py_object_B) {
          Py_DECREF(py_capsule);
        }
        else {
          PyErr_SetString(PyExc_RuntimeError, "copy_object: unknown error during decapsulation");
        }
      }
    }
    else {
      PyErr_SetString(PyExc_RuntimeError, "copy_object: library function returned unknown error");
      py_object_B = NULL;
    }

    if (py_object_D) {
      Py_XDECREF(py_object_D);
    }
  }

  ON_METHOD_EXIT(copy_object);

  return py_object_B;
}

/*!
 * \brief Remove an Object from a Page
 * \par Method Description
 *  This function provides a method to remove an existing object from
 *  the page which the object is associated. Removing an object does
 *  not destroy the object.
 *
 *  [in] PyObject The Geda object to be removed
 *
 * \return [out] status True if success otherwise False, False
 *               would only be returned if an object did not exist.
 *
 *  example:
 * \code
 *          geda.remove_object(py_symbol)
 * \endcode
 */
METHOD(remove_object)
{
  TYPE_INT_P1(remove_object);
  PyObject *py_object;
  int       status;

  if (!PyArg_ParseTuple(args, "O:geda.remove_object", &py_object))
  {
    const char *syntax = "syntax: remove_object(PyGedaObject || GedaCapsuleObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(py_object);

  ON_METHOD_EXIT(remove_object);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Remove a PyList of Objects from a Page
 * \par Method Description
 *  This function provides a method to remove a list of existing objects from
 *  a page which the objects are associated. The objects in the list do not
 *  have to be the same page and removing an object from a page does not
 *  destroy the object. (But, a reference to the objects must be maintained
 *  or Python will destroy the objects when collecting garbage)
 *
 *  [in] PyList Of Geda objects to be removed
 *
 * \return [out] status True if success otherwise False, False
 *               would only be returned if an object did not exist.
 *
 *  example:
 * \code
 *          geda.remove_objects(ObjectList)
 * \endcode
 */
METHOD(remove_objects)
{
  TYPE_INT_P1(remove_objects);
  PyObject *objects;
  int       status;

  if(!PyArg_ParseTuple(args, "O!:geda.remove_objects, Bad Argument", &PyList_Type, &objects))
  {
    const char *syntax = "syntax: remove_objects(PyList of PyGedaObjects)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(objects);

  ON_METHOD_EXIT(remove_objects);

  return Py_BuildValue("i", status);
}

/* End Page level Methods */

/*!
 * \brief Delete an Object
 * \par Method Description
 *  This function provides a method to delete an object
 *
 *  [in] PyObject The Geda object to be deleted
 *
 * \return [out] status True if success otherwise False, False
 *               would only be returned if an object did not exist.
 *
 *  example:
 * \code
 *          geda.delete_object(tmpObject)
 * \endcode
 */
METHOD(delete_object)
{
  TYPE_INT_P1(delete_object);
  PyObject  *object;
  int        status;

  if(!PyArg_ParseTuple(args, "O!:geda.delete_object, Bad Argument", PyGedaObjectClass(), &object))
  {
    const char *syntax = "syntax: delete_object(PyGedaObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(object);

  if (status > 0) {
    Py_DECREF(object);
  }

  ON_METHOD_EXIT(delete_object);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Delete a List of Objects
 * \par Method Description
 *  This function provides a method to delete a list objects
 *
 *  [in] PyObject of type PyList container with PyPyGedaObjects
 *
 * \return [out] status True if success otherwise False, False
 *               would only be returned if an object in the list
 *               did not exist.
 *
 *  example:
 * \code
 *          geda.delete_objects(Old_Objects)
 * \endcode
 *
 * TODO: See get_junctions for a friendly method of accepting arguments
 */
METHOD(delete_objects)
{
  TYPE_INT_P1(delete_objects);
  PyObject *objects;
  int       status;

  if(!PyArg_ParseTuple(args, "O!:geda.delete_objects, Bad Argument", &PyList_Type, &objects))
  {
    const char *syntax = "syntax: delete_objects(PyList of PyGedaObjects)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }
  status = library.func(objects);

  ON_METHOD_EXIT(delete_objects);

  return Py_BuildValue("i", status);
}

/*!
 * \brief Synchronize GedaObject with a PyGedaObject
 * \par Method Description
 *  This is an internal function used by the PyGedaObject module
 *  to update the actual GedaObject when PyGedaObject are modified
 *  in python. that are not already committed to page, aka floating
 *  objects. This allows the python objects to be modified before
 *  the object is committed to a page.
 *
 *  [in] PyObject of type PyPyGedaObject, checked by orginal receptor
 *
 * \return [out] status True if the object was updated or was not dirty,
 *               Otherwise False.
 *
 *  example:
 * \code
 *          geda.sync_object(ModifiedObject)
 * \endcode
 */
METHOD(sync_object)
{
  TYPE_INT_P1(sync_object);
  PyObject *py_object;
  int       status;

  if (!PyArg_ParseTuple(args, "O:geda.sync_object", &py_object))
  {
    const char *syntax = "syntax: sync_object(PyGedaObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(py_object);
  ON_METHOD_EXIT(sync_object);
  return Py_BuildValue("i", status);
}


/** \defgroup Python_API_Create_Methods  Geda Python Module API Creation Methods
 *  @{
 */

/*!
 * \brief Create a New Arc Object
 * \par Method Description
 *  This function provides a method to create new Arc. An Arc is a
 *  graphical figure. Arcs have line-type properties.
 *
 *  [in] x           Integer center X location
 *  [in] y           Integer center Y location
 *  [in] radius      Integer radius of the arc sector
 *  [in] start_angle Integer start angle of the sector
 *  [in] arc_sweep   Integer ending angle of the sector
 *
 *  Optional argument:
 *
 *  [in] color PyObject a color object (not implemented yet)
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example :
 * \code
 *          arc = geda.new_arc(4000, 3300, 500, 0, 90)
 * \endcode
 */
METHOD(new_arc)
{
  TYPE_PYOBJECT_I5P1(new_arc);

  PyObject *py_arc;
  PyObject *py_color = NULL;
  PyObject *object_data;

  int x; int y; int radius; int start_angle; int arc_sweep;

  if(!PyArg_ParseTuple(args, "iiiii|O:geda.new_arc, Bad Arc Arguments", &x, &y, &radius,
                       &start_angle, &arc_sweep, &py_color))
  {
    const char *syntax = "syntax: new_arc(x, y, radius, start_angle, arc_sweep [, color])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(x, y, radius, start_angle, arc_sweep, py_color);

  py_arc = PyObject_CallObject((PyObject*)PyGedaArcClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_arc);

  return py_arc;
}

/*!
 * \brief Create a New Box Object
 * \par Method Description
 *  This function provides a method to create new Box. A Box is closes
 *  graphical figure. Boxes have line-type properties and can be filled
 *  with a fill pattern or not.
 *
 *  [in] lower_x  Integer lower X corner
 *  [in] lower_y  Integer lower Y corner
 *  [in] upper_x  Integer upper X corner
 *  [in] upper_y  Integer upper Y corner
 *
 *  Optional argument:
 *
 *  [in] color PyObject a color object (not fully implemented yet)
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example:
 * \code
 *          frame1 = geda.new_box(7200, 5500, 7800, 5500)
 * \endcode
 */
METHOD(new_box)
{
  TYPE_PYOBJECT_I4P1(new_box);

  PyObject *py_box;
  PyObject *py_color = NULL;
  PyObject *object_data;

  int lower_x; int lower_y; int upper_x; int upper_y;

  if(!PyArg_ParseTuple(args, "iiii|O:geda.new_box, Bad Arguments",
                       &lower_x, &lower_y, &upper_x, &upper_y, &py_color))
  {
    const char *syntax = "syntax: new_box(lower_x, lower_y, upper_x, upper_y [, color])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(lower_x, lower_y, upper_x, upper_y, py_color);

  py_box = PyObject_CallObject((PyObject*)PyGedaBoxClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_box);

  return py_box;
}

/*!
 * \brief Create a New Bus Object
 * \par Method Description
 *  This function provides a method to create new Bus. A Bus is a line that
 *  basically represent an electrical path, like wires.A bus is simular to
 *  a Net object but represent mutible wires, not just one.
 *
 *  [in] x1  Integer from X location
 *  [in] y1  Integer from Y location
 *  [in] x2  Integer to X location
 *  [in] y2  Integer to Y location
 *
 *  Optional arguments:
 *
 *  [in] name string bus_name attribute
 *  [in] color PyObject a color object (not fully implemented yet)
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example:
 * \code
 *          bus = geda.new_bus(7200, 5500, 7800, 5500)
 * \endcode
 */
METHOD(new_bus)
{
  TYPE_PYOBJECT_C1I4P1(new_bus);

  PyObject *py_bus;
  PyObject *py_color   = NULL;
  PyObject *object_data;
  const char *bus_name = NULL;
  int x1; int y1; int x2; int y2;

  if(!PyArg_ParseTuple(args, "iiii|sO:geda.new_bus, Bad Arguments",
                       &x1, &y1, &x2, &y2, &bus_name, &py_color))
  {
    const char *syntax = "syntax: new_bus(x1, y1, x2, y2 [, bus_name [, color]])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }
  object_data = library.func(bus_name, x1, y1, x2, y2, py_color);

  py_bus = PyObject_CallObject((PyObject*)PyGedaBusClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_bus);

  return py_bus;
}

/*!
 * \brief Create a New Circle Object
 * \par Method Description
 *  This function provides a method to create new Circle. Circle objects
 *  are graphical drawing objects. Circle have line-type properties and
 *  can be filled with fill patterns or not.
 *
 *  [in] x       Integer center X location
 *  [in] y       Integer center Y location
 *  [in] radius  Integer to X location
 *
 *  optional:
 *
 *  [in] color  PyObject a color object (not implemented yet)
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example:
 * \code
 *          circle = geda.new_circle(7200, 5200, 1000)
 * \endcode
 */
METHOD(new_circle)
{
  TYPE_PYOBJECT_I3P1(new_circle);

  PyObject *py_circle;
  PyObject *py_color   = NULL;
  PyObject *object_data;

  int x; int y; int radius;

  if(!PyArg_ParseTuple(args, "iii|O:geda.new_circle, Bad Arguments",
                       &x, &y, &radius, &py_color))
  {
    const char *syntax = "syntax: new_circle(x, y, radius [, color])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(x, y, radius, py_color);

  py_circle = PyObject_CallObject((PyObject*)PyGedaCircleClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_circle);

  return py_circle;
}

/*!
 * \brief Create a New Complex Object
 * \par Method Description
 *  This function provides a method to create new Complex object. A Complex
 *  is a symbol to known to the Library that normally represents a component.
 *
 *  [in] basename string  The base file name of the symbol without the extension
 *  [in] x        Integer X insertion location
 *  [in] y        Integer Y insertion location
 *
 *  Optional arguments:
 *
 *  [in] angle    integer orientation property
 *  [in] mirror   integer property whether to mirror the symbol
 *  [in] embedded integer property whether to embed the symbol data
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 * \note 1. Preceeding optional arguments must be provided, use a value of
 *          -1 for defaults when it is not desired to "set" the proceeding
 *          arguments.
 *
 *  example 1.
 * \code
 *          titleblock = geda.new_complex("title-B", 1000, 1000)
 *          titleblock.locked = True
 * \endcode
 *  example 2.
 * \code
 *          amp1 = geda.new_complex("lm2902-1", 1000, 4000, -1, 1)
 * \endcode
 */
METHOD(new_complex)
{
  TYPE_PYOBJECT_C1I5(new_complex);

  PyObject   *py_complex;
  PyObject   *object_data;
  const char *basename = NULL;

  int x; int y; int angle; int mirror; bool embed;

  angle  = -1;
  mirror = -1;
  embed  = -1;

  if (! PyArg_ParseTuple(args, "sii|iii:geda.new_complex, Bad Arguments",
                         &basename, &x, &y, &angle, &mirror, &embed))
  {
    const char *syntax = "syntax: new_complex(name, x, y [, angle [, mirror [, embed]])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(basename, x, y, angle, mirror, embed);

  if (object_data != NULL ) {
    py_complex = PyObject_CallObject((PyObject*)PyGedaComplexClass(), object_data);
    Py_DECREF(object_data);
  }
  else {
    PyErr_Format(GedaError, "<new_complex>, An error occurred, object was not created, name=%s\n", basename);
    py_complex = NULL;
  }

  ON_METHOD_EXIT(new_complex);

  return py_complex;
}

/*!
 * \brief Create a New Line Object
 * \par Method Description
 *  This function provides a method to create new Line. Line object are
 *  graphical drawing objects and do not represent an electrical path,
 *  although lines are used inside of symbols to indicate signal paths
 *  or internal connections but these lines are never associated with
 *  nodes within a schematic.
 *
 *  [in] x1  Integer from X location
 *  [in] y1  Integer from Y location
 *  [in] x2  Integer to X location
 *  [in] y2  Integer to Y location
 *
 *  Optional argument:
 *
 *  [in] color PyObject a color object (not fully implemented yet)
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example:
 * \code
 *          line = geda.new_line(7200, 5200, 7800, 5200)
 * \endcode
 */
METHOD(new_line)
{
  TYPE_PYOBJECT_I4P1(new_line);

  PyObject *py_line;
  PyObject *py_color    = NULL;
  PyObject *object_data;

  int x1; int y1; int x2; int y2;

  if(!PyArg_ParseTuple(args, "iiii|O:geda.new_line, Bad Arguments",
                       &x1, &y1, &x2, &y2, &py_color))
  {
    const char *syntax = "syntax: new_line(x1, y1, x2, y2 [, color])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(x1, y1, x2, y2, py_color);

  py_line = PyObject_CallObject((PyObject*)PyGedaLineClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_line);

  return py_line;
}

/*!
 * \brief Create a New Net Object
 * \par Method Description
 *  This function provides a method to create new Net. A Net is a line that
 *  basically represent an electrical path, like a wire. One or more Net
 *  object are need to describe the electrical connection between pins and
 *  other nets.
 *
 *  [in] x1  Integer from X location
 *  [in] y1  Integer from Y location
 *  [in] x2  Integer to X location
 *  [in] y2  Integer to Y location
 *
 *  Optional arguments:
 *
 *  [in] name  string net_name attribute
 *  [in] color A PyGedaColorObject or Integer color code
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example 1.
 * \code
 *          net = geda.new_net(7200, 5500, 7800, 5500)
 *          geda.add_object(schematic, net)
 * \endcode
 */
METHOD(new_net)
{
  TYPE_PYOBJECT_C1I4P1(new_net);

  PyObject *py_net;
  PyObject *py_color    = NULL;
  PyObject *object_data;
  const char *net_name  = NULL;

  int x1; int y1; int x2; int y2;

  if(!PyArg_ParseTuple(args, "iiii|sO:geda.new_net, Bad Arguments",
                       &x1, &y1, &x2, &y2, &net_name, &py_color))
  {
    const char *syntax = "syntax: new_net(x1, y1, x2, y2 [, net_name [, color]])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(net_name, x1, y1, x2, y2, py_color);

  py_net = PyObject_CallObject((PyObject*)PyGedaNetClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_net);

  return py_net;
}

/*!
 * \brief Create a New Path Object
 * \par Method Description
 *  This function provides a method to create new Path. A Path object is
 *  graphical figure and does not represent an electrical path. Path have
 *  line-type properties and can have fill-patterns when the path is closed.
 *
 *  [in] path string  The SVG path string
 *
 *  Optional argument:
 *
 *  [in] color A PyGedaColorObject or Integer color code
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 *  example 1.
 * \code
 *          path = geda.new_path("M 1000 2500 L 2000 3500")
 * \endcode
 */
METHOD(new_path)
{
  TYPE_PYOBJECT_C1(new_path);

  PyObject *py_color = NULL;
  PyObject *py_path;
  PyObject *object_data;

  const char *path_string = NULL;

  if(!PyArg_ParseTuple(args, "s|O:geda.new_path, Bad Arguments", &path_string, &py_color))
  {
    const char *syntax = "syntax: new_path(path_string [, color])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(path_string);

  if (object_data != NULL ) {
    py_path = PyObject_CallObject((PyObject*)PyGedaPathClass(), object_data);
    Py_DECREF(object_data);
  }
  else {
    PyErr_Format(GedaError, "<new_path>, An error occurred, object was not created, path=%s\n", path_string);
    py_path = NULL;
  }

  ON_METHOD_EXIT(new_path);

  return py_path;
}

/*!
 * \brief Create a New Picture Object
 * \par Method Description
 *  This function provides a method to create new Picture.
 *
 *  [in] filepath string  The file name of the image
 *  [in] x1       Integer lower left X location
 *  [in] y1       Integer lower left Y location
 *  [in] x2       Integer top right X location
 *  [in] y2       Integer top right Y location
 *
 *  Optional arguments:
 *
 *  [in] angle    integer orientation property
 *  [in] mirror   integer property whether to mirror the image
 *  [in] embedded integer property whether to embed the image data
 *
 *  \return [out] PyObject Or NULL if an error occurs.
 *
 *  \note   Preceeding optional arguments must be provided, use a value of
 *          -1 for defaults when it is not desired to "set" the proceeding
 *          arguments.
 *
 *  example 1.
 * \code
 *          picture = geda.new_picture("/home/pictures/1377395281.jpg", 5200, 5000, 8200, 2000)
 *                                       picture.locked = True
 * \endcode
 */
METHOD(new_picture)
{
  TYPE_PYOBJECT_C1I7(new_picture);

  PyObject   *py_picture;
  PyObject   *object_data;
  const char *filepath = NULL;

  int x1; int y1; int x2; int y2; int angle; int mirror; bool embedded;

  angle    = -1;
  mirror   = -1;
  embedded = -1;

  if (! PyArg_ParseTuple(args, "siiii|iii:geda.new_picture, Bad Arguments",
                               &filepath, &x1, &y1, &x2, &y2, &angle, &mirror, &embedded))
  {
    const char *syntax = "syntax: new_picture(filename, x1, y1, x2, y2 [, angle [, mirror [, embedded]]])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(filepath, x1, y1, x2, y2, angle, mirror, embedded);

  if (object_data != NULL ) {
    py_picture = PyObject_CallObject((PyObject*)PyGedaPictureClass(), object_data);
    Py_DECREF(object_data);
  }
  else {
    PyErr_Format(GedaError, "<new_picture>, An error occurred, object was not created, filepath=%s\n", filepath);
    py_picture = NULL;
  }

  ON_METHOD_EXIT(new_picture);

  return py_picture;
}

/*!
 * \brief Create a New Pin Object
 * \par Method Description
 *  This function provides a method to create a new Pin Object.
 *
 *  [in] x1     Integer from X location
 *  [in] y1     Integer from Y location
 *  [in] x2     Integer to X location
 *  [in] y2     Integer to Y location
 *
 *  Optional arguments:
 *
 *  [in] whichend Integer Which end gets connected (either 0 or 1)
 *  [in] number   String  pin number attribute
 *  [in] label    String  pin label attribute
 *  [in] etype    Integer electrical type attribute (formally pin type)
 *  [in] mtype    Integer mechanical type attribute
 *  [in] ntype    Integer node type property ( 0=normal, 1=bus type)
 *
 * \return [out] PyObject Or NULL if an error occurs.
 *
 * \note 1. Preceeding optional arguments must be provided, use a value of
 *          -1 for defaults when it is not desired to "set" the proceeding
 *          arguments.
 *
 * \note 2. New Pin objects are created with the auto-attributes property set to
 *          True, in which case all "normal" pin attribute objects are automatically
 *          created and attached to the pin when the pin is commited to a page. If
 *          auto-attributes is turned off by using something like pin.auto-attributes
 *           = False, the pin attributes must be create separately and attached to the
 *          pin object. Another options is to leave attributes turned on and modify the
 *          attributes after the pin has been committed to a Page object, see example 2
 *          below.
 *
 *  example 1.
 * \code
 *            pin = geda.new_pin(900, 200, 700, 200, 0, "2", "2", PIN_ELECT_PAS, 0, 0)
 * \endcode
 *  example 2.
 * \code
 *            pin = geda.new_pin(0, 100, 100, 100, 0, "1", "1", PIN_ELECT_PAS, 0, 0)
 *            geda.add_object(resistor, pin)
 *            butes = geda.get_attribs(pin)
 *            for attrib in butes:
 *                if attrib.name() == "pinlabel":
 *                    attrib.visible = 0
 *                if attrib.name() == "pinnumber":
 *                    attrib.visible = 0
 * \endcode
 */
METHOD(new_pin)
{
  TYPE_PYOBJECT_C2I8(new_pin);

  PyObject   *py_pin;
  PyObject   *object_data;
  PyObject   *py_number;

  int x1; int y1; int x2; int y2;

  int whichend      = -1;
  int etype         = -1;
  int mtype         = -1;
  int ntype         = -1;

  const char *number = NULL;
  const char *label = NULL;

  if (! PyArg_ParseTuple(args, "iiii|iOsiii:geda.new_pin, Bad Arguments",
                         &x1, &y1, &x2, &y2, &whichend, &py_number, &label, &etype, &mtype, &ntype))
  {
    const char *syntax = "syntax: new_pin(x1, y1, x2, y2 [, whichend [, number [, label [, etype [, mtype [, ntype ]]]]]])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyInt_Check(py_number)) {

    int inumber;

    inumber   = PyInt_AS_LONG(py_number);
    py_number = PyString_FromFormat("%d", inumber);
  }

  if (PyString_Check(py_number)) {
    number = PyString_AsString(py_number);
  }

  object_data = library.func(label, number, x1, y1, x2, y2, whichend, etype, mtype, ntype);

  if (object_data != NULL ) {
    py_pin = PyObject_CallObject((PyObject*)PyGedaPinClass(), object_data);
    Py_DECREF(object_data);
  }
  else {
    PyErr_SetString(GedaError, "<new_pin>, An error occurred, object was not created\n");
    py_pin = NULL;
  }

  ON_METHOD_EXIT(new_pin);

  return py_pin;
}

/*!
 * \brief Create a New Text Object
 * \par Method Description
 *  This function provides a method to create a new Text Object.
 *
 *  [in] text  String for the name property
 *  [in] x     Integer X location
 *  [in] y     Integer Y location
 *
 *  optional:
 *
 *  [in] size    integer font size property
 *  [in] align   integer alignment property
 *  [in] angle   integer orientation property
 *  [in] color   A PyGedaColorObject or Integer color code
 *
 * \return [out] PyObject or NULL if an error occurs.
 *
 * \note  Preceeding optional arguments must be provided, use a value of
 *        -1 for defaults when it is not desired to "set" the proceeding
 *        arguments.
 *
 *  example 1.
 * \code
 *            text = geda.new_text("Gyro", 2800, 3000, -1, 0, 90)
 *            text.locked = True
 *            geda.add_object(schematic, text)
 * \endcode
 *  example 2.
 * \code
 *            text = geda.new_text("LM353", 250, 350, 8)
 *            geda.add_object(opamp, text)
 * \endcode
 */
METHOD(new_text)
{
  TYPE_PYOBJECT_C1I5P1(new_text);

  PyObject   *py_color      = NULL;
  PyObject   *py_text;
  PyObject   *object_data;

  const char *text;
  int x; int y;

  int size   = -1;
  int align  = -1;
  int angle  = -1;

  if (!PyArg_ParseTuple(args, "sii|iiiO:geda.new_text, Bad Argument",
                        &text, &x, &y, &size, &align, &angle, &py_color))
  {
    const char *syntax = "syntax: new_text(string, x, y, [, size [, align [, angle] [, color]]]";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(text, x, y, size, align, angle, py_color);

  py_text = PyObject_CallObject((PyObject*)PyGedaTextClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_text);

  return py_text;
}

/*!\brief Create a New Attribute Object
 * \par Method Description
 *  This function provides a method to a new attribute Text Object. The new_text
 *  method could also be used to create a new Text but this method is more
 *  convenience for Text object that are to be used as Attributes as indicated
 *  by the parameter arguments.
 *
 *  [in] Name  String for the name property
 *  [in] Value String for the Value property
 *  [in] x     Integer X location
 *  [in] y     Integer Y location
 *
 *  optional:
 *
 *  [in] visible Boolean visibility property
 *  [in] show    integer show-name-value property
 *  [in] align   integer alignment property
 *  [in] angle   integer orientation property
 *  [in] color   A PyGedaColorObject or Integer color code
 *
 *  \return [out] GedaPyGedaTextObject
 *
 *  \note  Preceeding optional arguments must be provided, use a value of
 *         -1 for defaults when it is not desired to "set" the proceeding
 *          arguments.
 *
 *  example 1.
 * \code
 *            device = geda.new_attrib("device", "resistor", x, y)
 * \endcode
 *  example 2.
 * \code
 *            pinlabel = geda.new_attrib("pinlabel", "2", x, y1, -1, -1, LOWER_RIGHT)
 * \endcode
 */
METHOD(new_attrib)
{
  TYPE_PYOBJECT_C2I6P1(new_attrib);

  PyObject   *py_text;
  PyObject   *object_data;
  PyObject   *py_color     = NULL;
  const char *name         = NULL;
  const char *value        = NULL;

  int x; int y;

  int visible = -1;
  int show    = -1;
  int align   = -1;
  int angle   = -1;

  if (! PyArg_ParseTuple(args, "ssii|iiiiO:geda.new_attrib, Bad Argument",
                         &name, &value, &x, &y, &visible, &show, &align, &angle, &py_color))
  {
    const char *syntax = "syntax: new_attrib(name, value, x, y, [, visible [, show-option [, align [, angle [, color]]]]]}";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data = library.func(name, value, x, y, visible, show, align, angle, py_color);

  py_text = PyObject_CallObject((PyObject*)PyGedaTextClass(), object_data);

  Py_XDECREF(object_data);

  ON_METHOD_EXIT(new_attrib);

  return py_text;
}
/** @} END Group Python_API_Create_Methods */

/** \defgroup Python_Attribute_Handlers  Geda Python Module API Attribute Manipulators
 *  @{
 */

/*!
 * \brief Get a Single Attribute Object by Name
 * \par Method Description
 *  This function provides a method to get a single attribute associated with
 *  a given Gedaobject. If found, the returned attribute may be attached or
 *  floating.
 *
 *  [in] py_parent PyGedaObject to be search for the attribute
 *  [in] name      string name of the attribute to be returned
 *
 * \return [out] Pyattribute if found or Py_None if an attribute was not found
 *               with the given name.
 */
METHOD(get_attrib)
{
  TYPE_PYOBJECT_P1C1(get_attrib);
  PyObject *py_parent;
  PyObject *object_data;
  PyObject *py_attrib;
  const char *name;

  if (!PyArg_ParseTuple(args, "Os:geda.get_attrib", PyGedaObjectClass(), &py_parent, &name)) {

    const char *syntax = "syntax: get_attrib(PyGedaObject, name)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  object_data  = library.func(py_parent, name);

  if (object_data) {
    py_attrib = PyObject_CallObject((PyObject*)PyGedaTextClass(), object_data);
    Py_DECREF(object_data);
  }
  else {
    Py_INCREF(Py_None);
    py_attrib = Py_None;
  }

  ON_METHOD_EXIT(get_attrib);

  return py_attrib;
}

/*!
 * \brief Get Attribute Objects Value
 * \par Method Description
 *  This function provides a method to get all of the attributes attached to
 *  given Gedaobject.
 *
 *  [in] PyPyGedaObject whose attributes are to be returned
 *
 *  \return [out] PyList of Pyattributes attached to the PyObject.
 */
METHOD(get_attribs)
{
  TYPE_PYOBJECT_P1(get_attribs);
  PyObject *unknown;
  PyObject *parent;

  PyObject *py_output_list;

  const char *syntax = "syntax: get_attribs(PyGedaObject || GedaCapsuleObject)";

  if(!PyArg_ParseTuple(args, "O:geda.get_attribs", &unknown)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(unknown, PyGedaObjectClass())) { /* Accept PyGedaObject */
    parent = unknown;
  }
  else if (do_GedaCapsule_Type(self, unknown)) {          /* Or GedaCapsuleObject */
    parent =  do_get_object(self, args);
  }
  else {
    PyErr_SetString(PyExc_TypeError, syntax);             /* Set error message */
    py_output_list = NULL;
    parent = NULL;
  }

  if (parent) {

    PyObject *py_input_list;

    py_input_list  = library.func(parent);
    py_output_list = PyList_New(0);

    if (PyList_Check(py_input_list)) {

      int i, count = (int)PyList_GET_SIZE(py_input_list);

      for (i = 0; i < count ; i++) {

        PyObject *object_data;
        PyObject *py_text;

        object_data  = PyList_GET_ITEM(py_input_list, i);
        py_text      = PyObject_CallObject((PyObject*)PyGedaTextClass(), object_data);

        if(py_text && PyObject_Type(py_text)) {
          PyList_Append(py_output_list, py_text);
        }
      }
    }

    Py_XDECREF(py_input_list);
  }
  else {
    PyErr_SetString(PyExc_TypeError, "get_attribs: Bad GedaCapsuleObject");
    py_output_list = NULL;
  }

  ON_METHOD_EXIT(get_attribs);

  return py_output_list;
}

/*!
 * \brief Set Attribute Objects Value
 * \par Method Description
 *  This function provides a method to set the value of an attribute given it
 *  name. If the Attribute being set is a floating type, then the attribute is
 *  promoted, also referred to as being attached, to the parent.
 *
 *  If the Attribute does not exist, whether inherited or not, a new attribute
 *  is created and attached to the parent.
 *
 *  [in] py_object PyObject Complex or Text object for which attribute is to be set
 *  [in] name      PyString name of the attribute which is to be set or created
 *  [in] py_value  PyString or PyInt value of the attribute
 *  [in] ret_obj   PyBoolean if True the attribute will be returned
 *
 * \return PyAttribute object associated with the changes if the 4th argument
 *         evaluated as True, otherwise Py_None is returned.
 */
METHOD(set_attrib)
{
  TYPE_PYOBJECT_P2C2I1(set_attrib);
  PyObject *py_object_A;
  PyObject *py_object_B = NULL; /* appease compiler */
  PyObject *py_value;           /* convertible */
  PyObject *object_data;
  PyObject *py_ret      = NULL;

  const char *name;
  const char *value;
  int         ret_obj = 0;

  if(!PyArg_ParseTuple(args, "OsO|i:set_attrib", &py_object_A, &name, &py_value, &ret_obj))
  {

    const char *syntax = "syntax: set_attrib(PyGedaObject, name, value [, return-object])";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }
  if (PyObject_TypeCheck(py_object_A, PyGedaComplexClass())) {
    py_object_B = NULL;
  }
  else if (PyObject_TypeCheck(py_object_A, PyGedaTextClass())) {
    py_object_B = py_object_A;
    py_object_A = NULL;
  }
  else {
    PyErr_SetString(PyExc_TypeError, "parameter 1 is not a PyGedaObject or Attribute object");
    return NULL;
  }

  if (PyInt_Check(py_value)) {
    long long_val        = PyInt_AsLong(py_value);
    PyObject *py_tmp_val = PyString_FromFormat("%ld", long_val);
    Py_XDECREF(py_value);
    py_value = py_tmp_val;
  }

  value = PyString_AsString(py_value);

  object_data  = library.func(py_object_A, py_object_B, name, value, ret_obj);

  if (ret_obj && object_data) {
    py_ret = PyObject_CallObject((PyObject*)PyGedaTextClass(), object_data);
    Py_DECREF(object_data);
  }
  else {
    Py_INCREF(Py_None);
    py_ret = Py_None;
  }

  //Py_XDECREF(py_value);

  ON_METHOD_EXIT(set_attrib);

  return py_ret;
}

/*!
 * \brief Refresh Attribute Objects Method
 * \par Method Description
 *  This function provides a method to update attributes that were modified
 *  in Python scripts after a complex object associates with the attributes was
 *  placed (on a page). If attributes were modified or added to an object before
 *  committing the object to a page, then it is not required to use this method.
 *  If attributes whose parent object has been placed on a page are modified
 *  without calling this function, only the Python version of the object is
 *  modified, hence the changes will not be saved when the file is saved.
 *
 *  [in] PyObject Complex for which attributes are to be synchronized
 *
 * \return status True on success, otherwise False.
 */
METHOD(refresh_attribs)
{
  TYPE_INT_P1(refresh_attribs);
  PyObject *object;
  int       status;

  if (!PyArg_ParseTuple(args, "O!:geda.refresh_attribs", PyGedaObjectClass(), &object))
  {
    const char *syntax = "syntax: refresh_attribs(PyGedaObject)";

    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  status = library.func(object);

  ON_METHOD_EXIT(refresh_attribs);

  return Py_BuildValue("i", status);
}

/** @} END Group Python_Attribute_Handlers */

/** \defgroup Python_Connections  Geda Python Module API Network Methods
 *  @{
 */

/*!
 * \brief Get all Objects Connect to a Given PyGedaObject
 * \par Method Description
 *    This function provides a method to obtain a list of all of objects
 *  connected with a given PyGedaObject or GedaCapsule. The returned list
 *  contains GedaCapsules and is guaranteed to contain at least one object
 *  -- the object used as an argument, unless the optional filter argument
 *  excludes the object used as an argument.
 *
 *  [in] PyObject object is a PyGedaObject or GedaCapsuleObject
 *
 * \return [out] PyList of GedaCapsules.
 *
 *  example 1:
 * \code
 *            sub_net = geda.get_network(capsule)
 *            for node in sub_net:
 *                print node.name
 * \endcode
 *  example 2:
 * \code
 *            U10_SCI = geda.get_network(Pin, GEDA_FILTER_NET)
 *            for net in U10_SCI:
 *                set_attrib(net, "netname", "U10_SCI")
 * \endcode
 *  note: In example 2, the pin does not get a netname attribute
 *        attached to the pin because pins were excluded by the NET
 *        filter.
 */
METHOD(get_network)
{
  TYPE_PYOBJECT_I3(get_network);
  PyObject *unknown;
  PyObject *list;
  int       pid;
  int       sid;
  int       filter = GEDA_FILTER_ALL;

  const char *syntax = "syntax: get_network(PyGedaObject || GedaCapsuleObject [, filter])";

  if(!PyArg_ParseTuple(args, "O|i:geda.get_network", &unknown, &filter)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  if (PyObject_TypeCheck(unknown, PyGedaObjectClass()) ||
    do_GedaCapsule_Type(self, unknown))
  {
    pid = ((PyGedaObject*)unknown)->pid;
    sid = ((PyGedaObject*)unknown)->sid;
  }
  else {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  list = library.func(pid, sid, filter);

  ON_METHOD_EXIT(get_network);

  return list;
}

/*!
 * \brief Get all Junctions Associated with PyGedaObjects
 * \par Method Description
 *  This function provides a method to obtain X-Y coordinates data
 *  of connection junctions.
 *
 *  [in] PyObject object is a PyGedaObject or GedaCapsuleObject
 *
 * \return [out] PyList of Points; integer X-Y pairs or an empty
 *               list if no junctions were found.
 *
 */
METHOD(get_junctions)
{
  TYPE_PYOBJECT_P1(get_junctions);
  PyObject *unknown;
  PyObject *py_source_list;
  PyObject *py_output_list;
  PyObject *py_tmp;

  const char *syntax = "syntax: get_junctions(PyList || PyGedaObject)";

  if(!PyArg_ParseTuple(args, "O:geda.get_junctions", &unknown)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  py_tmp = NULL;
  py_source_list = PyList_New(0);

  if (PyObject_TypeCheck(unknown, &PyList_Type)) {

    int i;
    int count = PyList_GET_SIZE(unknown);

    for (i = 0; i < count; i++) {

      PyObject *py_object = PyList_GET_ITEM(unknown, i);

      if (PyObject_TypeCheck(py_object, PyGedaObjectClass())) { /* PyGedaObject was in list */
        PyList_Append(py_source_list, py_object);
      }
      else if (do_GedaCapsule_Type(self, unknown)) {          /* Capsule was in list */
        py_tmp = Py_BuildValue("(O)", py_object);
        py_object = do_get_object(self, py_tmp);
        if (py_object) {
          PyList_Append(py_source_list, py_object);
        }
      }
    }
  }
  else if (PyObject_TypeCheck(unknown, PyGedaObjectClass())) {  /* Just 1 Object not in list */
    PyList_Append(py_source_list, unknown);
  }
  else if (do_GedaCapsule_Type(self, unknown)) {              /* Just 1 Capsule not in list */
    py_tmp = do_get_object(self, args);
    PyList_Append(py_source_list, py_tmp);
  }
  else {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  py_output_list = library.func(py_source_list);

  if (py_tmp) {
    Py_DECREF(py_tmp);
  }

  Py_DECREF(py_source_list);

  ON_METHOD_EXIT(get_junctions);

  return py_output_list;
}

/*!
 * \brief Get Points Associated with Unconnected PyGedaObjects
 * \par Method Description
 *  This function provides a method to obtain X-Y coordinates data
 *  of unconnected object, normally pins and nets.
 *
 *  [in] PyObject can be PyList of PyObjects that can be any combination
 *       of PyGedaObjects and or GedaCapsuleObjects, or a single PyGedaObject
 *       or single GedaCapsuleObject not in a PyList. All objects types are
 *       accepted but graphical objects will always return an empty list.
 *
 * \return [out] PyList of Points; integer X-Y pairs or an empty list
 *               if no unconnected nodes were found.
 */
METHOD(get_unconnected)
{
  TYPE_PYOBJECT_P1(get_unconnected);
  PyObject *unknown;
  PyObject *py_source_list;
  PyObject *py_output_list;
  PyObject *py_tmp;

  const char *syntax = "syntax: get_unconnected(PyList || PyGedaObject)";

  if(!PyArg_ParseTuple(args, "O:geda.get_unconnected:", &unknown)) {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  py_tmp = NULL;
  py_source_list = PyList_New(0);

  if (PyObject_TypeCheck(unknown, &PyList_Type)) {

    int i;
    int count = PyList_GET_SIZE(unknown);

    for (i = 0; i < count; i++) {

      PyObject *py_object = PyList_GET_ITEM(unknown, i);

      if (PyObject_TypeCheck(py_object, PyGedaObjectClass())) { /* PyGedaObject was in list */
        PyList_Append(py_source_list, py_object);
      }
      else if (do_GedaCapsule_Type(self, unknown)) {          /* Capsule was in list */
        py_tmp = Py_BuildValue("(O)", py_object);
        py_object = do_get_object(self, py_tmp);
        if (py_object) {
          PyList_Append(py_source_list, py_object);
        }
      }
    }
  }
  else if (PyObject_TypeCheck(unknown, PyGedaObjectClass())) {  /* Just 1 Object not in list */
    PyList_Append(py_source_list, unknown);
  }
  else if (do_GedaCapsule_Type(self, unknown)) {              /* Just 1 Capsule not in list */
    py_tmp = do_get_object(self, args);
    PyList_Append(py_source_list, py_tmp);
  }
  else {
    PyErr_SetString(PyExc_TypeError, syntax);
    return NULL;
  }

  py_output_list = library.func(py_source_list);

  if (py_tmp) {
    Py_DECREF(py_tmp);
  }

  Py_DECREF(py_source_list);

  ON_METHOD_EXIT(get_unconnected);

  return py_output_list;
}

/** @} END Group Python_Connections */

/** @} END Group Python_Method_Handlers */
