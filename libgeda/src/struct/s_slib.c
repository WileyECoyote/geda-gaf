/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_slib.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

/*! \file s_slib.c
 *  \brief Manage Source Library directories data
 *   slib stands for source (project/schematic/hdl/model source) library
 */

#include "../../../config.h"

#include <sys/types.h>
#include <ctype.h>

#include <dirent.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

/*! \brief */
struct st_slib {
  char *dir_name;
};

/*! \brief */
static int slib_index=0;

/*! \brief */
#define MAX_SLIBS	128

/*! \brief
 * \todo eventually make this unlimited hack hack
 * \todo test everything at boundary conditions (exceed cache size etc...)
 */
static struct st_slib slib[MAX_SLIBS];

/*!
 * \brief Add a Path to Search for Schematics
 * \par Function Description
 *  Add \a new_path to the table of directories to search
 *  for schematics if the directory is not already in the
 *  table.
 */
int geda_struct_slib_add_entry(const char *new_path)
{
  if (new_path == NULL) {
    return (-1);
  }

  if (slib_index >= MAX_SLIBS) {
    return (-1);
  }

  if (!geda_struct_slib_search_for_dirname(new_path)) {
    slib[slib_index].dir_name = geda_utility_string_strdup (new_path);
    slib_index++;
  }

  return (slib_index);
}

/*!
 * \brief Release Source Library resources
 * \par Function Description
 *  Free the strings in the Component Library path table.
 */
void geda_struct_slib_free()
{
  int i;

  for (i = 0; i < slib_index; i++) {
    GEDA_FREE(slib[i].dir_name);
  }

  slib_index=0;
}

/*!
 * \brief Get the base file name from a raw file name string.
 * \par Function Description
 *  Creates an returns a file name based on the given \a rawname. The raw
 *  file name is copied up to the first period and any _# are removed (where
 *  # is any number of digits.
 *
 * \param [in] rawname  Character string with the raw file name to parse.
 *
 * \returns The base file name in a character string.
 *
 * \remarks Caller should GEDA_FREE returned pointer.
 */
char *geda_struct_slib_get_basename(const char *rawname)
{
  char *return_filename;
  int i;
  int done;
  int lastchar;
  int len;
  int seen_underscore;
  int valid;

  if (!rawname)
    return (NULL);

  len = strlen(rawname) + 1;

  return_filename = (char*)GEDA_MEM_ALLOC(sizeof(char)*len);

  i = done = seen_underscore = valid = 0;

  /* first get everything up to the leading dot */
  while (rawname[i] != '\0' && rawname[i] != '.') {
    return_filename[i] = rawname[i];
    i++;
  }

  return_filename[i] = '\0';

  /* skip null terminator */
  i--;

  lastchar = i;

  /* this is a quick and dirty state machine to */
  /* go back and strip off any _#'s */
  /* if there is a better way let me know */
  while (i >= 0 && !done) {

    /* first we need to check to see if we have seen the first '_' */
    /* if we have then we already removing chars, continue with that */
    if (seen_underscore) {

      if (return_filename[i] == '_') {
        done = 1;
      }

      return_filename[i] = '\0';
    }
    else {
      /* we are still searching for the first underscore */

      /* first make sure char is a number */
      if (isdigit((int) return_filename[i])) {
        valid = 1;
      }
      else if (return_filename[i] == '_' && valid) {

        /* yes it is okay to delete the chars */
        seen_underscore=1;
        /* incremented, since it is then */
        /* decremented */
        i = lastchar+1;
      }
      else {
        valid = 0;
        done  = 1;
      }
    }

    i--;
  }

  /* be sure to GEDA_FREE this somewhere */
  return (return_filename);
}

/*!
 * \brief Get the Component Library directory at the Given index
 * \par Function Description
 *  Returns the directory entry in the Component Library path
 *  table at the given index, which could be NULL.
 *
 * \returns Component Library directory at \a index
 *
 * \warning Caller must NOT free the returned pointer.
 */
char *geda_struct_slib_get_dir(int index)
{
  if (slib[index].dir_name != NULL)
    return (slib[index].dir_name);
  else
    return (NULL);
}

/*!
 * \brief Initialize the Source Library
 * \par Function Description
 *  Sets each entry in the Component Library path table to NULL.
 */
void geda_struct_slib_init()
{
  int i;
  for (i = 0; i < MAX_SLIBS; i++) {
    slib[i].dir_name = NULL;
  }
}

/*!
 * \brief Write contents of Component Libraries
 * \par Function Description
 *  Write Component Library directory names and containing files in the
 *  component library search path to standard out.
 */
void geda_struct_slib_print(void)
{
  int i;

  for (i = 0; i < slib_index; i++) {
    printf ("%s\n", slib[i].dir_name);
  }
}

/*!
 * \brief Write contents of Component Libraries
 * \par Function Description
 *  Write the names of directories and containing files in the
 *  component library search path to standard out.
 */
void geda_struct_slib_print_dirs(void)
{
  GError *err;
  char   *directory;
  int     i;

  i = 0;

  directory = geda_struct_slib_get_dir(i);

  while (directory != NULL) {

    err = NULL;

    printf("Opened %s\n", directory);

    GSList *files = geda_file_get_dir_list_files (directory, NULL, &err);

    if (err) {
      geda_log_w ("%s\n", err->message);
      g_error_free(err);
    }
    else {

      GSList *iter = files;

      while (iter) {

        char *file = iter->data;

        printf("file: %s\n", file);

        iter = iter->next;
      }

      geda_utility_gslist_free_full (files, g_free);
    }

    printf("Closed %s\n", directory);

    GEDA_FREE(directory);

    i++;
    directory = geda_struct_slib_get_dir(i);
  }
}

/*!
 * \brief Search SLIB for a particular file name.
 * \par Function Description
 *  This function will search the SLIB for a particular file name.
 *  Filename is the raw symbol/whatever file name.
 *
 * \param [in] basename  Character string with file name to search for.
 *
 * \remarks Caller should GEDA_FREE returned pointer.
 */
char *geda_struct_slib_search_for_file (const char *basename)
{
  char *slib_path = geda_struct_slib_search_dirs(basename);

  if (slib_path) {

    char *file_path = geda_file_sys_normalize_name (slib_path, NULL);

    char *full_path = g_build_filename (file_path, basename, NULL);

    GEDA_FREE(file_path);
    GEDA_FREE(slib_path);

    return (full_path);
  }
  else {
    return (NULL);
  }
}

/*!
 * \brief Search Source Library for a Directory
 * \par Function Description
 *
 * \return 1 if directory is found, zero otherwise.
 */
int geda_struct_slib_search_for_dirname(const char *dir_name)
{
  if (dir_name) {

    int i;

    for (i = 0; i < slib_index; i++) {
      if (strcmp(slib[i].dir_name, dir_name) == 0) {
        return (1);
      }
    }
  }
  return (0);
}

/*!
 * \brief Search Source Library Directories for a File name
 * \par Function Description
 *  Looks for a file in each directory in the slib table whose name
 *  contains the string \a basename, if found then that directory is
 *  returned.
 *
 * \remarks Caller should GEDA_FREE returned pointer.
 */
char *geda_struct_slib_search_dirs(const char *basename)
{
  int i;

  /* Search slib paths backwards */
  for (i = slib_index - 1 ; i >= 0; i--) {

    DIR  *ptr;

    struct dirent *dptr;

#if DEBUG
    printf("searching: %d %s\n", i, slib[i].dir_name);
#endif

    ptr = opendir(slib[i].dir_name);

    g_return_val_if_fail ((ptr != NULL), NULL);

    dptr = readdir (ptr);

    while (dptr != NULL) {

      /* Note: readdir returns both the "." and ".." entries, the former
       * ia a problem because the dot is likely in file name as .sch */
      if (dptr->d_name[1] != '\0') {

        /* Do a substring comp for a match */
        if (strstr (dptr->d_name, basename) != 0)  {

          char *slib_path = geda_utility_string_strdup (slib[i].dir_name);

          if (ptr) {
            closedir (ptr);
          }

          return (slib_path);
        }
      }
      dptr = readdir(ptr);
    }

    if (ptr) {
      closedir (ptr);
      ptr = NULL;
    }
  }

  return (NULL);
}

/*!
 * \brief Check is Path exist and is not in the search path.
 * \par Function Description
 *  TRUE if path exist and is not in the component library search
 *  path.
 * \param [in] path The path to test.
 *
 * \returns TRUE if path exist and is not in search path.
 */
int geda_struct_slib_unique_dir_exist(const char *path)
{
  if (g_file_test (path, G_FILE_TEST_IS_DIR)) {
    return !(geda_struct_slib_search_for_dirname(path));
  }
  return 0;
}
