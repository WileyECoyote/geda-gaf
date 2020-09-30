/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
#include "../../../config.h"

#include <stdio.h>
#include <ctype.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <libgeda_priv.h>

/*!
 * \todo Finish function documentation!!!
 * \brief
 * \par Function Description
 *
 */
void
geda_struct_print_forw (GList *list)
{
  GList *iter = list;

  printf("Printing ...\n");

  while (iter != NULL) {

    GedaObject *o_current = (GedaObject *)iter->data;
    printf("Address: %p\n", o_current);
    printf("Name: %s \n", o_current->name);
    printf("Type: %d\n", o_current->type);
    printf("Sid: %d\n",  o_current->sid);

    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      geda_struct_print_forw(o_current->complex->prim_objs);
    }

    geda_attrib_object_print (o_current->attribs);

    printf("----\n");
    iter = g_list_next (iter);
  }
}

/*!
 * \todo Finish function documentation!!!
 * \brief
 * \par Function Description
 *
 */
void
geda_struct_print(GedaObject *ptr)
{
  GedaObject *o_current=NULL;

  o_current = ptr;

  if (o_current != NULL) {
    printf("Name: %s\n", o_current->name);
    printf("Type: %d\n", o_current->type);
    printf("Sid: %d\n", o_current->sid);
    if (o_current->line != NULL) {
      printf("Line points.x1: %d\n", o_current->line->x[0]);
      printf("Line points.y1: %d\n", o_current->line->y[0]);
      printf("Line points.x2: %d\n", o_current->line->x[1]);
      printf("Line points.y2: %d\n", o_current->line->y[1]);
    }

    geda_attrib_object_print (o_current->attribs);

    printf("----\n");
  }
}
