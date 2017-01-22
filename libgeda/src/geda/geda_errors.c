/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 2011-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this Library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
#include "../../../config.h"
#include <libgeda_priv.h>

/** \defgroup geda-errors Libgeda Error Module
 * @{
 * \brief Contains routines for handling errors in libgeda
 * \par
 */

/*!
 * \brief EDA Error type Identifier
 * \par Function Description
 * Used by EDA_ERROR. Returns a GQuark used to identify GErrors
 * originating in libgeda. Should not be called directly.
 */
GQuark
eda_error_quark (void)
{
  return g_quark_from_static_string ("eda-error-quark");
}

/*!
 * \brief Output Error when an invalid argument passed as Object
 * \par Function Description
 *  Common helper function to display errors when a pointer is passed
 *  to a function that is not the type the function required or is
 *  NULL.
 *
 * \param [in] file   The file where the error was detected.
 * \param [in] func   The function that detect the error.
 * \param [in] object Pointer that was  passed to the function.
 * \param [in] type   The object type that was expected.
 */
void
geda_error_object_argument(const char *file,
                           const char *func,
                           const void *object,
                           const char  type)
{
  if (type <= GEDA_OBJECT_TEXT) {

    const char *type_string[] = { "Object",
                                  "Arc",
                                  "Box",
                                  "Bus",
                                  "Circle",
                                  "Complex",
                                  "Line",
                                  "Net",
                                  "Path",
                                  "Picture",
                                  "Pin",
                                  "Text",
                                   NULL};

    fprintf(stderr, "File %s, <%s>: ", file, func);

    if (!object) {
      fprintf(stderr, "Geda%s object argument is NULL\n", type_string[(int)type]);
    }
    else {
      fprintf(stderr, "Not a valid Geda%s <%p>\n", type_string[(int)type], object);
    }
  }
  else {
    fprintf(stderr, "Invalid type: %s <%s> <%c>\n", file, func, type);
  }
}

/** @} endgroup geda-errors */
