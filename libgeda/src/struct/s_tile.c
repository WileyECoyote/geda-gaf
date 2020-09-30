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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */
#include "../../../config.h"

#include <ctype.h>
#include <math.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

/*! \file s_tile.c
 *  \brief Splits a page into tiles
 *
 *  With the <b>tiles</b> a <b>Page</b> is splitted into several smaller areas.
 *  The number of tiles is defined by <b>MAX_TILES_X</b> and <b>MAX_TILES_Y</b>.
 *
 *  Each <b>TILE</b> (st_tile) can contain zero to many <b>GedaObjects</b> and
 *  and each <b>GedaObject</b> can be in one or more TILES.
 *
 *  The usage of tiles makes it easier to find geometrical connections between
 *  the line objects; OBJ_NET, OBJ_PIN, OBJ_BUS.
 *
 *  \image html s_tile_overview.png
 *  \image latex s_tile_overview.pdf "Tile overview" width=14cm
 */

/*!
 * \brief Initialize Array of Tiles
 * \par Function Description
 *  This function splits the page size into tiles and initializes
 *  every tile.
 *
 * \param p_current The page that gets the tiles.
 */
void
geda_struct_tile_init(Page * p_current)
{

  int i, j;
  TILE *tile_current;
  int x_size = p_current->width / MAX_TILES_X;
  int y_size = p_current->height / MAX_TILES_Y;
  int x_sum = 0;
  int y_sum = 0;

#if DEBUG_TILES
  fprintf(stderr, "World (%d,%d), Tile size X, Y: %d %d\n",
         p_current->width, p_current->height, x_size, y_size);
#endif

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      tile_current = &p_current->world_tiles[i][j];

      tile_current->objects = NULL;

      tile_current->left = x_sum;
      tile_current->right = x_sum + x_size;

      tile_current->top = y_sum;
      tile_current->bottom = y_sum + y_size;

      x_sum = x_sum + x_size;
    }
    x_sum = 0;
    y_sum = y_sum + y_size;
  }

#if DEBUG_TILES
  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      tile_current = &p_current->world_tiles[i][j];
      printf("\n%d %d\n", i, j);
      printf("----------------\n");
      printf("left %d top %d\n", tile_current->left, tile_current->top);
      printf("right %d bottom %d\n", tile_current->right,
             tile_current->bottom);
    }
  }
#endif
}

/*!
 * \brief Add a Linear GedaObject to the Tile Array
 * \par Function Description
 *  This function takes a single <b>Line GedaObject</b> and adds it to every
 *  tile that is touched by the line. The function also adds all tiles that
 *  are touched by the object to the objects tile list.
 *
 *  \param object   The line Object to add
 */
static void
geda_struct_tile_add_linear_object (GedaObject *object)
{
  TILE *tile_current;
  Page *p_current;
  GList *found;
  int j;
  int v, w;
  double x1, y1, x2, y2;
  double bottom;
  double x_size, y_size;
  double x, y;
  int start, end;

  g_return_if_fail (GEDA_IS_LINE(object));

#if DEBUG_TILES
  printf("<%s> name: %s\n", __func__, object->name);
#endif

  p_current = geda_object_get_page (object);

  if (p_current == NULL) {
    return;
  }

  x_size = (double) p_current->width / (double) MAX_TILES_X;
  y_size = (double) p_current->height / (double) MAX_TILES_Y;

  x1 = (int) (object->line->x[0] / x_size);
  x2 = (int) (object->line->x[1] / x_size);
  y1 = (int) (object->line->y[0] / y_size);
  y2 = (int) (object->line->y[1] / y_size);

  bottom = x2 - x1;

  if (bottom != 0.0) {

    int i;
    double m, b;

    m = (double) (y2 - y1) / bottom;
    b = y1 - m * x1;

    start = /* min */ x2 > x1 ? x1 : x2;
    end   = /* max */ x1 > x2 ? x1 : x2;

    /* Ensure both abscissae are in bounds */
    if (start < 0 || end > MAX_TILES_X - 1) {
      return;
    }

    for (i = start; i <= end; i++) {

      x = v = i;
      y = m * x + b;

      if (floor(y) != ceil(y)) {

        w = (int) floor(y);

        if (w < 0 || w > MAX_TILES_Y - 1) {
          return;
        }

        tile_current = &p_current->world_tiles[v][w];
        found = g_list_find(tile_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          tile_current->objects = g_list_append(tile_current->objects, object);
          object->tiles = g_list_append(object->tiles, tile_current);
        }
      }

      w = (int) ceil(y);

      if (w < 0 || w > MAX_TILES_Y - 1) {
        return;
      }

      tile_current = &p_current->world_tiles[v][w];
      found = g_list_find(tile_current->objects, object);

      if (!found) {
        /*printf("%d %d\n", v, w);*/
        tile_current->objects = g_list_append(tile_current->objects, object);
        object->tiles = g_list_append(object->tiles, tile_current);
      }
    }

    if (m != 0.0) {

      start = min((int) y1, (int) y2);
      end   = max((int) y1, (int) y2);

      /* Ensure both ordinates are in bounds */
      if (start < 0 || end > MAX_TILES_Y - 1) {
        return;
      }

      for (j = start; j <= end; j++) {

        y = w = j;
        x = (y - b) / m;

        if (floor(x) != ceil(x)) {

          v = (int) floor(x);

          if (v < 0 || v > MAX_TILES_X - 1) {
            return;
          }

          tile_current = &p_current->world_tiles[v][w];
          found = g_list_find(tile_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            tile_current->objects =
            g_list_append(tile_current->objects, object);
            object->tiles = g_list_append(object->tiles, tile_current);
          }
        }

        v = (int) ceil(x);

        if (v < 0 || v > MAX_TILES_X - 1) {
          return;
        }

        tile_current = &p_current->world_tiles[v][w];
        found = g_list_find(tile_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          tile_current->objects =
          g_list_append(tile_current->objects, object);
          object->tiles = g_list_append(object->tiles, tile_current);
        }
      }
    }
  }
  else {

    start = min((int) y1, (int) y2);
    end = max((int) y1, (int) y2);

    for (j = start; j <= end; j++) {

      y = j;
      x = x1;
      v = (int) x;
      w = (int) y;

      if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
        return;
      }

      tile_current = &p_current->world_tiles[v][w];
      found = g_list_find(tile_current->objects, object);

      if (!found) {
        /*printf("%d %d\n", v, w);*/
        tile_current->objects = g_list_append(tile_current->objects, object);
        object->tiles = g_list_append(object->tiles, tile_current);
      }
    }
  }
}

/*!
 * \brief Add an GedaObject to the Tile System
 * \par Function Description
 *
 *  This is a dispatch function that passes the object to the
 *  correct handler function, depending on its type.
 *
 * \param object   The line Object to add
 */
void
geda_struct_tile_add_object (GedaObject *object)
{
  GList *iter;

  switch (object->type) {
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      geda_struct_tile_add_linear_object(object);
      break;

  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
    for (iter = object->complex->prim_objs;
         iter != NULL;
         iter = g_list_next(iter)) {
      geda_struct_tile_add_object (iter->data);
    }
  }
}

/*!
 * \brief remove an object from the tiles
 * \par Function Description
 *
 *  This function remove an object from all tiles that are referenced
 *  by the object, including the object from each tile that contained
 *  the object. For compound objects, this function uses recursion to
 *  remove the nested objects. Compound objects are any object which
 *  reference another other object.
 *
 * \param object The object to remove
 */
void
geda_struct_tile_remove_object(GedaObject *object)
{
  GList *tl_current;

  /* Correctly deal with compound objects */
  if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER) {

    GList *iter;

    for (iter  = object->complex->prim_objs;
         iter != NULL;
         iter  = g_list_next (iter)) {
      geda_struct_tile_remove_object (iter->data);
    }
  }

  for (tl_current  = object->tiles;
       tl_current != NULL;
       tl_current  = g_list_next (tl_current)) {
    TILE *tile_current = (TILE*)tl_current->data;

    /* remove object from the list of objects for this tile */
    tile_current->objects = g_list_remove(tile_current->objects, object);
  }

  /* reset the list of tiles for this object appears in */
  g_list_free(object->tiles);
  object->tiles = NULL;
}

/*!
 * \brief Update the Tile Information of an GedaObject
 * \par Function Description
 *  This function updates the tile information of an <b>GedaObject</b> by
 *  removing and the re-adding the <b>GedaObject</b> to the Tile <b>System</b>.
 *  This function can be used if an object has been moved on the page.
 *
 *  \param object The GedaObject to update
 */
void
geda_struct_tile_update_object(GedaObject * object)
{
  geda_struct_tile_remove_object (object);
  geda_struct_tile_add_object (object);
}

/*!
 * \brief Get a List of Objects List of all Tiles Inside a Region
 * \par Function Description
 *  This functions collects all object lists of the tiles that are touched
 *  by the given rectangle (x1,y1), (x2,y2).
 *
 * \note The caller has to g_list_free() the returned list.
 */
GList*
geda_struct_tile_get_objectlists(Page *p_current, int world_x1, int world_y1,
                                                  int world_x2, int world_y2)
{
  TILE *tile_current;
  int x1, x2, y1, y2, x, y;
  double x_size, y_size;
  GList *objectlists = NULL;

  x_size = (double) p_current->width / (double) MAX_TILES_X;
  y_size = (double) p_current->height / (double) MAX_TILES_Y;

  x1 = (int) (world_x1 / x_size);
  x2 = (int) (world_x2 / x_size);
  y1 = (int) (world_y1 / y_size);
  y2 = (int) (world_y2 / y_size);

  /* limit all tile ranges to [0, MAX_TILES-1]
     (paranoid error checking) */
  x1= max(x1, 0);  x1 = min(x1, MAX_TILES_X-1);
  x2= max(x2, 0);  x2 = min(x2, MAX_TILES_X-1);
  y1= max(y1, 0);  y1 = min(y1, MAX_TILES_Y-1);
  y2= max(y2, 0);  y2 = min(y2, MAX_TILES_Y-1);

  /* Check and correct the order of the coordinates */
  if (x1 > x2) {
    x = x1;  x1 = x2;  x2 = x;
  }
  if (y1 > y2) {
    y = y1;  y1 = y2;  y2 = y;
  }

  /* finally, collect all object lists from the tiles */
  for (x = x1; x <= x2; x++) {
    for (y = y1; y <= y2; y++) {
      tile_current = &p_current->world_tiles[x][y];
      objectlists = g_list_append(objectlists, tile_current->objects);
    }
  }

  return objectlists;
}

/*!
 * \brief Print all GedaObjects for each Tile
 * \par Function Description
 *  Debugging function to print all object names that are inside
 *  the tiles.
 */
void
geda_struct_tile_print(GedaToplevel * toplevel, Page *page)
{
  TILE *tile;
  GList *tile_list;
  GedaObject *o_current;
  int i, j;

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      printf("\nTile %d %d\n", i, j);

      tile = &page->world_tiles[i][j];

      tile_list = tile->objects;
      while (tile_list) {
        o_current = (GedaObject *) tile_list->data;

        printf("%s\n", o_current->name);

        NEXT(tile_list);
      }

      printf("------------------\n");
    }
  }

}

/*!
 * \brief free all object links from the tiles
 * \par Function Description
 *  This function removes all objects from the tiles of the given \a page.
 *
 * \param [in] p_current The Page to clean up the tiles
 *
 * \note In theory, calling this function is not required. If all objects
 *       have been removed from a page, all object lists of the tiles should
 *       be empty.
 */
void
geda_struct_tile_free_all(Page * p_current)
{
  int i, j;
  TILE *tile;

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      tile = &p_current->world_tiles[i][j];
      if (g_list_length(tile->objects) != 0) {
        fprintf(stderr,
                "OOPS! tile->objects had something in it when it was freed!\n");
        fprintf(stderr, "Length: %d\n", g_list_length(tile->objects));
      }
      g_list_free(tile->objects);
    }
  }
}
