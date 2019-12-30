/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/*!
 * \file scheme_os.c
 * \brief Scheme API functions for misc. OS-related stuff.
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

SCM_SYMBOL (carbon_sym, "carbon");
SCM_SYMBOL (cygwin_sym, "cygwin");
SCM_SYMBOL (linux_sym, "linux");
SCM_SYMBOL (win32_sym, "win32");
SCM_SYMBOL (win32_native_sym, "win32-native");

/*!
 * \brief Get host operating system information.
 * \par Function Description
 * Returns a list of symbols describing the operating system.
 * The symbols may include:
 * \par
 *  <DL>
 *    <DT>win32 -- Windows</DT>
 *    <DT>win32-native -- Windows, not via Cygwin</DT>
 *    <DT>cygwin -- Cygwin</DT>
 *    <DT>carbon -- Mac OS X Carbon</DT>
 *    <DT>linux -- Linux</DT>
 *  </DL>
 * \return a list of symbols.
 */
EDA_SCM_DEFINE (os_platform, "%platform", 0, 0, 0,
               (), "Return a list of symbols describing the host platform.")

{
  SCM result = SCM_EOL;

# if defined (OS_CARBON)
  result = scm_cons (carbon_sym, result);
# endif

# if defined (OS_CYGWIN)
  result = scm_cons (cygwin_sym, result);
# endif

# if defined (OS_LINUX)
  result = scm_cons (linux_sym, result);
# endif

# if defined (OS_WIN32)
  result = scm_cons (win32_sym, result);
# endif

# if defined (OS_WIN32_NATIVE)
  result = scm_cons (win32_native_sym, result);
# endif

  return result;
}

/*!
 * \brief Get System Configuration directory directories.
 * \par Function Description
 * Returns a list of directories to be searched for system
 * configuration information.
 *
 * \note Scheme API: Implements the %sys-config-dirs procedure in the
 * (geda core os) module.
 *
 * \return a Scheme list of 1 string.
 */
EDA_SCM_DEFINE (os_sys_config_dirs, "%sys-config-dirs", 0, 0, 0, (),
               "Return a list of search directories for system configuration.")
{
  const char *path = geda_sys_config_path();
  SCM dir = scm_from_locale_string (path);
  return scm_list_1 (dir);
}

/*!
 * \brief Get System Data directory directories.
 * \par Function Description
 *  Returns a list of directories to be searched for system data.
 *
 * \note Scheme API: Implements the %sys-data-dirs procedure in the
 *       (geda core os) module.
 *
 * \return a Scheme list of strings.
 */
EDA_SCM_DEFINE (os_sys_data_dirs, "%sys-data-dirs", 0, 0, 0, (),
               "Return a list of search directories for system data.")
{
  /* geda_sys_data_path() returns a raw environment string, so assume
   * it's in the current locale's encoding. */
  const char *path = geda_sys_data_path();
  SCM dir = scm_from_locale_string (path);
  return scm_list_1 (dir);
}

/*!
 * \brief Get User Configuration directory.
 * \par Function Description
 *  Returns a list of directories to be searched for user configuration.
 *
 * \note Scheme API: Implements the %user-config-dir procedure in the
 *       (geda core os) module.
 *
 * \return a Scheme list of 1 string.
 */
EDA_SCM_DEFINE (os_user_config_dir, "%user-config-dir", 0, 0, 0, (),
               "Return a list of search directories for user configuration.")
{
  const char *path = geda_user_config_path();
  SCM dir = scm_from_locale_string (path);
  return scm_list_1 (dir);
}

/*!
 * \brief Get User Data directory.
 * \par Function Description
 *  Returns a list of directories to be searched for user data.
 *
 * \note gEDA stores user data in the same directory as user config
 *
 * \note Scheme API: Implements the %user-data-dir procedure in the
 *      (geda core os) module.
 *
 * \return a Scheme list of 1 string.
 */
EDA_SCM_DEFINE (os_user_data_dir, "%user-data-dir", 0, 0, 0, (),
                "Return a list of search directories for user data.")
{
  const char *path = geda_user_config_path();
  SCM dir = scm_from_locale_string (path);
  return scm_list_1 (dir);
}

/*!
 * \brief Create the (geda core os) Scheme module.
 * \par Function Description
 *  Defines procedures in the (geda core os) module. The module can be
 *  accessed using (use-modules (geda core os)).
 */
static void
init_module_geda_core_os (void *nothing)
{
  /* Register the functions and symbols */
  #include "scheme_os.x"

  scm_c_export (scheme_os_platform,
                scheme_os_sys_config_dirs,
                scheme_os_sys_data_dirs,
                scheme_os_user_config_dir,
                scheme_os_user_data_dir,
                NULL);
}

/*!
 * \brief Initialize the host platform support procedures.
 * \par Function Description
 *  Registers some Scheme procedures that provide cross-platform
 *  support. Should only be called by edascm_init().
 */
void
edascm_init_os (void)
{
  /* Define the (geda core os) module */
  scm_c_define_module ("geda core os", init_module_geda_core_os, NULL);

}
