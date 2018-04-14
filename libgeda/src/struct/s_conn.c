/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

#include <config.h>
#include <libgeda_priv.h>
#include <geda_debug.h>

/*! \file s_conn.c
 *  \brief The connection system
 *
 *  The connection system stores and tracks the connections between
 *  connected <b>GedaObjectS</b>. The connected GedaObjectS are either
 *  <b>pins</b>, <b>nets</b> and <b>busses</b>.
 *
 *  Each connection object with the type <b>st_conn</b> represents a
 *  single unidirectional relation to another object.
 *
 *  The following figure with two nets and a pin shows the relations
 *  between connections and GedaObjectS:
 *
 *  \image html s_conn_overview.png
 *  \image latex s_conn_overview.pdf "Connection overview" width=14cm
 */


/*!
 * \brief create a new connection object
 * \par Function Description
 *  create a single st_conn object and initialize it with the
 *  given parameters.
 *
 * \remark This is the only routine that assigns a value to the
 *         field conn->other_object
 *
 * \return The new connection object
 */
CONN *geda_struct_conn_return_new(GedaObject * other_object, int type, int x, int y,
                        int whichone, int other_whichone)
{
  CONN *new_conn;

  new_conn = (CONN *) GEDA_MEM_ALLOC(sizeof(CONN));

#if DEBUG_CONNS
  printf("** creating: %s %d %d\n", other_object->name, x, y);
#endif

  new_conn->other_object = other_object;
  new_conn->type = type;
  new_conn->x = x;
  new_conn->y = y;
  new_conn->whichone = whichone;
  new_conn->other_whichone = other_whichone;

  return (new_conn);
}

/*!
 * \brief check if a connection is uniq in a list
 * \par Function Description
 *  This function checks if there's no identical connection
 *  in the list of connections.
 *
 * \param conn_list list of connection objects
 * \param input_conn single connection object.
 *
 * \return TRUE if the CONN structure is unique, FALSE otherwise.
 */
int geda_struct_conn_uniq(GList * conn_list, CONN * input_conn)
{
  GList *c_current = conn_list;

  while (c_current != NULL) {

    CONN *conn = (CONN*)c_current->data;

    if (conn->other_object == input_conn->other_object &&
        conn->x == input_conn->x && conn->y == input_conn->y &&
        conn->type == input_conn->type) {
      return (FALSE);
    }

    c_current = g_list_next(c_current);
  }

  return (TRUE);
}

/*!
 * \brief remove a object from the connection list of another object
 * \par Function Description
 *  This function removes the Object <b>to_remove</b> from the connection
 *  <b>other_object</b> list of objects.
 *
 * \param other_object Object from which to_remove Object needs to be removed
 * \param to_remove    Object to remove
 *
 * \return TRUE if a connection has been deleted, FALSE otherwise
 */
int geda_struct_conn_remove_other (GedaObject *other_object, GedaObject *to_remove)
{
  GList *c_current;

  geda_object_notify_emit_pre_change (other_object);

  c_current = other_object->conn_list;

  /* Loop over list of connections in other_object */
  while (c_current != NULL) {

    CONN *conn = (CONN*)c_current->data;

    /* Compare if this is the connect to be removed */
    if (conn->other_object == to_remove) {
      other_object->conn_list =
      g_list_remove(other_object->conn_list, conn);

#if DEBUG_CONNS
      printf("Found other_object in remove_other\n");
      printf("Freeing other: %s %d %d\n", conn->other_object->name,
             conn->x, conn->y);
#endif

      /* Release the CONN structure */
      GEDA_FREE(conn);

#if 0 /* this does not work right */
      if (other_object->type == OBJ_BUS &&
        other_object->conn_list == NULL) {
        other_object->bus_ripper_direction = 0;
        }
#endif
        geda_struct_conn_emit_conns_changed (other_object);

      return (TRUE);
    }

    c_current = g_list_next(c_current);
  }

  geda_object_notify_emit_change (other_object);

  return (FALSE);
}

/*!
 * \brief removes a GList of Objects from the connection system
 * \par Function Description
 *  This function removes all connections from and to the GedaObjects
 *  in the given GList.
 *
 * \param obj_list  GList of Objects to unconnected from all other objects
 */
static void geda_struct_conn_remove_glist (GList *obj_list)
{
  GList *iter;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {

    GedaObject *o_current = iter->data;

    geda_struct_conn_remove_object (o_current);
  }
}

/*!
 * \brief remove an GedaObject from the connection system
 * \par Function Description
 *  This function removes all connections from and to the GedaObject
 *  <b>to_remove</b>.
 *
 * \param to_remove GedaObject to unconnected from all other objects
 */
void geda_struct_conn_remove_object (GedaObject *to_remove)
{
  GList *c_iter;
  int changed = FALSE;

  switch (to_remove->type) {
    case OBJ_PIN:
    case OBJ_NET:
    case OBJ_BUS:
      for (c_iter = to_remove->conn_list; c_iter != NULL; NEXT(c_iter)) {

        CONN  *conn = c_iter->data;

        geda_struct_conn_freeze_hooks (conn->other_object);

        /* keep calling this till it returns false (all refs removed) */
        /* there is NO body to this while loop */
        while (geda_struct_conn_remove_other (conn->other_object, to_remove));

        c_iter->data = NULL;
        geda_struct_conn_thaw_hooks (conn->other_object);
        GEDA_FREE (conn);
      }

      if (to_remove->conn_list) {
        g_list_free (to_remove->conn_list);
        to_remove->conn_list = NULL;
        changed = TRUE;
      }
      break;

    case OBJ_COMPLEX:
    case OBJ_PLACEHOLDER:
      geda_struct_conn_remove_glist (to_remove->complex->prim_objs);
      changed = TRUE;
      break;
  }

  if (changed) {
    geda_struct_conn_emit_conns_changed (to_remove);
  }
}

/*!
 * \brief Checks if a point is a midpoint of an GedaObject
 * \par Function Description
 *  Checks if the point (<b>x</b>,<b>y</b>) is on the GedaObject
 *  and between it's endpoints.
 *
 * \return TRUE if the point is a midpoint of the GedaObject. FALSE if the
 *         point is not a midpoint or if the GedaObject is not a NET a PIN
 *         or a BUS or if the GedaObject is not orthogonally oriented.
 */
GedaObject *geda_struct_conn_check_midpoint(GedaObject *o_current, int x, int y)
{
  int min_x, min_y, max_x, max_y;

  switch(o_current->type) {
    case(OBJ_NET):
    case(OBJ_PIN):
    case(OBJ_BUS):
      min_y = min(o_current->line->y[0],
                  o_current->line->y[1]);
      max_y = max(o_current->line->y[0],
                  o_current->line->y[1]);

                /* vertical */
      if ( (o_current->line->x[0] == x) &&
           (y > min_y) && (y < max_y) &&
           (o_current->line->x[0] ==
            o_current->line->x[1])) {

#if DEBUG_CONNS
        printf("Found vertical point\n");
#endif

        return(o_current);
      }

      min_x = min(o_current->line->x[0],
                  o_current->line->x[1]);
      max_x = max(o_current->line->x[0],
                  o_current->line->x[1]);

                /* horizontal */
      if ((o_current->line->y[0] == y) &&
          (x > min_x) && (x < max_x) &&
          (o_current->line->y[0] ==
           o_current->line->y[1])) {

#if DEBUG_CONNS
        printf("Found horizontal point\n");
#endif

        return(o_current);
      }

      break;
  }
  return(NULL);
}

/*!
 * \brief adds a GList of Objects to the connection system
 * \par Function Description
 *  This function adds all connections from and to the GedaObjects
 *  in the given GList.
 *
 * \param obj_list  GList of Objects to add into the connection system
 */
void geda_struct_conn_update_glist (GList *obj_list)
{
  GList *iter;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {

    GedaObject *o_current = iter->data;

    geda_struct_conn_update_object (o_current);
  }
}

/*!
 * \brief Checks if two objects are of compatible types to be connected
 * \par Function Description
 *  Checks if two objects are legal to be connected together
 *
 * \param object1  First GedaObject
 * \param object2  Second GedaObject
 *
 * \return TRUE if the objects are compatible, FALSE if not
 */
static int check_direct_compat (GedaObject *object1, GedaObject *object2)
{
  return (geda_object_get_bus_related (object1) == geda_object_get_bus_related (object2));
}

static void add_connection (GedaObject *object, GedaObject *other_object,
                            int type, int x, int y,
                            int whichone, int other_whichone)
{
  /* Describe the connection */
  CONN *new_conn =
    geda_struct_conn_return_new (other_object, type, x, y,  whichone, other_whichone);

  /* Do uniqness check */
  if (geda_struct_conn_uniq (object->conn_list, new_conn)) {
    object->conn_list = g_list_append (object->conn_list, new_conn);
    geda_struct_conn_emit_conns_changed ( object);
    geda_struct_conn_emit_conns_changed ( other_object);
  }
  else {
    GEDA_FREE (new_conn);
  }
}

/*!
 * \brief add a line Object to the connection system
 * \par Function Description
 *  This function searches for all geometrical connections of the GedaObject
 *  <b>object</b> to all other connectable objects. It adds connections
 *  to the object and from all other objects to this one.
 *
 * \param object GedaObject to add into the connection system
 */
void geda_struct_conn_update_linear_object (GedaObject *object)
{
  /* There is no point in looking for objects not on a page
   * since the tile system does not add pageless objects */
  if (geda_object_get_page (object)) {

    TILE   *t_current;
    GList  *tl_current;
    GList  *object_list;
    GedaObject *complex;
    GedaObject *found;
    GedaObject *other_object;
    GedaObject *other_complex;

    complex = geda_object_get_parent (object);

    geda_struct_conn_freeze_hooks (object);

    /* loop over all tiles which object appears in */
    for (tl_current = object->tiles; tl_current != NULL; NEXT(tl_current))
    {
      t_current = tl_current->data;

      for (object_list = t_current->objects; object_list != NULL; NEXT(object_list))
      {
        int k;

        other_object = object_list->data;

        /* Don't connect object to itself */
        if (object == other_object)
          continue;

        other_complex = geda_object_get_parent (other_object);

        /* An object inside a symbol can only be connected up to another
         * object if they are (a) both inside the same object, or (b)
         * the object inside a symbol is a pin. */

        /* 1. Both objects are inside a symbol */
        if (complex && other_complex) {
          /* If inside different symbols, both must be pins to connect. */
          if (complex != other_complex &&
             (object->type != OBJ_PIN || other_object->type != OBJ_PIN)) {
            continue;
          }

          /* 2. Updating object is inside a symbol, but other object is not. */
        }
        else if (complex && !other_complex) {
          if (object->type != OBJ_PIN) continue;
          /* 3. Updating object not inside symbol, but other object is. */
        }
        else if (!complex && other_complex) {
          if (other_object->type != OBJ_PIN) continue;
        }

        geda_struct_conn_freeze_hooks (other_object);

        /* TODO: One would think there is a less loopy way to check endpoints */

        /* Check both end points of the other object */
        for (k = 0; k < 2; k++) {

          int j;

          /* If the other object is a pin, only check the correct end */
          if (other_object->type == OBJ_PIN && other_object->pin->whichend != k)
            continue;

          /* Check both end points of the object */
          for (j = 0; j < 2; j++) {

            /* If the object is a pin, only check the correct end */
            if (object->type == OBJ_PIN && object->pin->whichend != j)
              continue;

            /* Check for coincidence and compatibility between
             *            the objects being tested. */
            if (object->line->x[j] == other_object->line->x[k] &&
                object->line->y[j] == other_object->line->y[k] &&
                check_direct_compat (object, other_object))
            {
              geda_object_notify_emit_pre_change (other_object);

              add_connection (object, other_object, CONN_ENDPOINT,
                              other_object->line->x[k],
                              other_object->line->y[k], j, k);

              add_connection (other_object, object, CONN_ENDPOINT,
                              object->line->x[j],
                              object->line->y[j], k, j);

              geda_object_notify_emit_change (other_object);
            }
          }
        }

        /* Check both end points of the object against midpoints of the other */
        for (k = 0; k < 2; k++) {

          /* If the object is a pin, only check the correct end */
          if (object->type == OBJ_PIN && object->pin->whichend != k)
            continue;

          /* check for midpoint of other object, k endpoint of current obj*/
          found = geda_struct_conn_check_midpoint (other_object, object->line->x[k],
                                         object->line->y[k]);

          /* Pins are not allowed midpoint connections onto them. */
          /* Allow nets to connect to the middle of buses. */
          /* Allow compatible objects to connect. */
          if (found && other_object->type != OBJ_PIN &&
            ((object->type == OBJ_NET && other_object->type == OBJ_BUS) ||
            check_direct_compat (object, other_object))) {

            add_connection (object, other_object, CONN_MIDPOINT,
                            object->line->x[k],
                            object->line->y[k], k, -1);

            add_connection (other_object, object, CONN_MIDPOINT,
                            object->line->x[k],
                            object->line->y[k], -1, k);
            }
        }

        /* Check both end points of the other object against midpoints of the first */
        for (k = 0; k < 2; k++) {

          /* If the other object is a pin, only check the correct end */
          if (other_object->type == OBJ_PIN && other_object->pin->whichend != k)
            continue;

          /* do object's endpoints cross the middle of other_object? */
          /* check for midpoint of other object, k endpoint of current obj*/
          found = geda_struct_conn_check_midpoint (object, other_object->line->x[k],
                                         other_object->line->y[k]);

          /* Pins are not allowed midpoint connections onto them. */
          /* Allow nets to connect to the middle of buses. */
          /* Allow compatible objects to connect. */
          if (found && object->type != OBJ_PIN &&
            ((object->type == OBJ_BUS && other_object->type == OBJ_NET) ||
            check_direct_compat (object, other_object))) {

            add_connection (object, other_object, CONN_MIDPOINT,
                            other_object->line->x[k],
                            other_object->line->y[k], -1, k);

            add_connection (other_object, object, CONN_MIDPOINT,
                            other_object->line->x[k],
                            other_object->line->y[k], k, -1);
            }
        }

        geda_struct_conn_thaw_hooks (other_object);
      }
    }

#if DEBUG_CONNS
    geda_struct_conn_print(object->conn_list);
#endif

    geda_struct_conn_thaw_hooks (object);
  }
}

/*!
 * \brief Update an GedaObject in the connection system
 * \par Function Description
 *  This function searches for all geometrical connections of the GedaObject
 *  <b>object</b> to all other connectable objects. The function updates
 *  connections to the object and from all other objects to this given
 *  object.
 *
 * \param object GedaObject to update into the connection system
 */
void geda_struct_conn_update_object (GedaObject *object)
{
  switch (object->type) {
    case OBJ_PIN:
    case OBJ_NET:
    case OBJ_BUS:
      geda_struct_conn_update_linear_object (object);
      break;

    case OBJ_COMPLEX:
    case OBJ_PLACEHOLDER:
      geda_struct_conn_update_glist (object->complex->prim_objs);
      break;
  }
}

/*!
 * \brief print all connections of a connection list
 * \par Function Description
 *  This is a debugging function to print a List of connections.
 *
 * \param conn_list GList of connection objects
 */
void geda_struct_conn_print(GList *conn_list)
{
  GList *cl_current = conn_list;

  printf("\nStarting geda_struct_conn_print\n");

  while (cl_current != NULL) {

    CONN *conn = (CONN*)cl_current->data;

    printf("-----------------------------------\n");
    printf("other object: %s\n", conn->other_object->name);
    printf("type: %d\n", conn->type);
    printf("x: %d y: %d\n", conn->x, conn->y);
    printf("whichone: %d\n", conn->whichone);
    printf("other_whichone: %d\n", conn->other_whichone);
    printf("-----------------------------------\n");

    cl_current = g_list_next(cl_current);
  }
}

/*!
 * \brief Search for net in existing connections.
 * \par Function Description
 *  This method searches the connection list for the first matching
 *  connection with the given x, y, and whichone endpoint.
 *
 * \param [in] new_net    Net GedaObject to compare to.
 * \param [in] whichone   The connection number to check.
 * \param [in] conn_list  List of existing connections to compare
 *                        <B>new_net</B> to.
 * \return TRUE if a matching connection is found, FALSE otherwise.
 */
int geda_struct_conn_net_search(GedaObject *new_net, int whichone, GList *conn_list)
{
  GList *cl_current = conn_list;

  while (cl_current != NULL) {

    CONN *conn = (CONN*)cl_current->data;

    if (conn != NULL && conn->whichone == whichone &&
        conn->x == new_net->line->x[whichone] &&
        conn->y == new_net->line->y[whichone])
    {
       return TRUE;
    }

    cl_current = g_list_next(cl_current);
  }

  return FALSE;
}

/*!
 * \brief get a list of all objects connected to a list of Objects.
 * \par Function Description
 *  This function gets all other_object from the connection
 *  list of the GedaObjects in the pased list.
 *
 * \param [in] input_list GList of GedaObject's or NULL
 * \param [in] obj_list   The GList of GedaObject to get connections from
 *
 * \return A GList of objects
 *
 * \warning Caller must g_list_free returned GList pointer.
 *          Do not free individual data items in list.
 */
static GList *geda_struct_conn_return_glist_others (GList *input_list, GList *obj_list)
{
  GList *return_list;
  GList *iter;

  return_list = input_list;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {

    GedaObject *o_current = iter->data;

    return_list = geda_struct_conn_return_others (return_list, o_current);
  }

  return return_list;
}

/*!
 * \brief get a list of all objects connected to this one
 * \par Function Description
 *  This function gets all other_object from the connection list of the
 *  current object.  COMPLEX objects are entered, and their prim_objs
 *  processed. If an <b>input_list</b> is given, the other objects are
 *  appended to that list.
 *
 * \param [in] input_list   GList of GedaObject's
 * \param [in] object       GedaObject to get other GedaObjects from
 *
 * \return A GList of Objects
 *
 * \warning Caller must g_list_free returned GList pointer.
 *          Do not free individual data items in list.
 */
GList *geda_struct_conn_return_others(GList *input_list, GedaObject *object)
{
  GList *c_iter;
  GList *return_list;

  return_list = input_list;

  switch (object->type) {
    case OBJ_PIN:
    case OBJ_NET:
    case OBJ_BUS:
      for (c_iter  = object->conn_list;
           c_iter != NULL; c_iter = g_list_next (c_iter)) {
        CONN *conn = c_iter->data;

        if (conn->other_object && conn->other_object != object) {
          return_list = g_list_append(return_list, conn->other_object);
        }
      }
      break;

    case OBJ_COMPLEX:
    case OBJ_PLACEHOLDER:
      return_list = geda_struct_conn_return_glist_others (return_list,
                                                object->complex->prim_objs);
      break;
  }

  return return_list;
}

/*****************************************************************************/

typedef struct {
  ConnsChangedFunc func;
  void *data;
} ConnsChangedHook;

/*!
 * \brief Add connection change notification handlers to a Page.
 * \par Function Description
 *  Adds a set of change notification handlers to a#Page instance.
 *  \a func will be called each time a connection is modified, with
 *  the affected object and the given \a user_data.
 *
 * \param page     #Page structure to add handlers to.
 * \param func     Function to be called when changes are made.
 * \param data     User data to be passed to callback functions.
 *
 * TODO: Does not check for uniquness!
 */
void
geda_struct_conn_append_conns_changed_hook (Page *page, ConnsChangedFunc func, void *data)
{
  ConnsChangedHook *new_hook;

  new_hook = GEDA_MEM_ALLOC0 (sizeof(ConnsChangedHook));
  new_hook->func = func;
  new_hook->data = data;

  page->conns_changed_hooks =
    g_list_append (page->conns_changed_hooks, new_hook);
}


static void call_conns_changed_hook (void *data, void *user_data)
{
  ConnsChangedHook *hook = data;
  GedaObject *object         = user_data;

  hook->func (hook->data, object);
}

/*!
 * \brief Emit Connection change notification.
 * \par Function Description
 *  Calls each change callback function registered with #Page to
 *  notify listeners that \a connection has just been modified.  All
 *  libgeda functions that modify #GedaObject structures should call
 *  this just after making a change to an #GedaObject.
 *
 * \param object      #GedaObject structure to emit notifications for.
 */
void geda_struct_conn_emit_conns_changed (GedaObject *object)
{
  if (object != NULL) {

    if (object->conn_notify_freeze_count > 0) {
      object->conn_notify_pending = 1;
      return;
    }

    object->conn_notify_pending = 0;

    if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {
      g_list_foreach (object->page->conns_changed_hooks,
                      call_conns_changed_hook, object);
    }
  }
}

/*!
 * \brief Suspense GedaObject Connection Notification for an GedaObject
 * \par Function Description
 *  This function increments the freeze count of an #GedaObject.
 *  Notification of connection changes is suspended until the
 *  freeze is reduced to zero.
 *
 * \sa geda_struct_conn_thaw_hooks
 *
 * \param object #GedaObject to freeze notifications for.
 */
void geda_struct_conn_freeze_hooks (GedaObject *object)
{
  if (object != NULL) {
    object->conn_notify_freeze_count += 1;
  }
}
/*!
 * \brief Thaw Connection Notification for an GedaObject
 * \par Function Description
 *  This function add a hook to each new page object so that the
 *  function refresh_connectivity_cache is whenever an object on
 *  the page is created or modified.
 *
 * \sa geda_struct_conn_freeze_hooks
 *
 * \param object #GedaObject to thaw notifications for.
 */
void geda_struct_conn_thaw_hooks (GedaObject *object)
{
  if (object != NULL) {
    g_return_if_fail (object->conn_notify_freeze_count > 0);

    object->conn_notify_freeze_count -= 1;

    if (object->conn_notify_freeze_count == 0 &&
      object->conn_notify_pending)
    geda_struct_conn_emit_conns_changed (object);
  }
}

static void refresh_connectivity_cache (GedaToplevel *toplevel, GedaObject *object)
{
    if (object->type == OBJ_NET) {
        /* FIXME: suboptimal to refresh cache every time */
        /* better approach would invalidate cache without refresh */
        /* refresh would be done on redraw of pin cues */
        geda_net_object_refresh_conn_cache (object);
    }
}

/*!
 * \brief Initialize Page Connection Notification System
 * \par Function Description
 *  This function add a hook to each new page object so that the
 *  function refresh_connectivity_cache is called whenever an object
 *  on the page is created or modified.
 *
 * \param page #Page Object to emit notifications for.
 */
static void geda_struct_conn_init_page (Page *page)
{
  GedaToplevel *toplevel;

  toplevel = geda_page_get_toplevel(page);

  /* Connect the hooks for tracking net connectivity here */
  geda_struct_conn_append_conns_changed_hook (page,
                  (ConnsChangedFunc)refresh_connectivity_cache,
                                    toplevel);

  geda_attrib_append_changed_hook (page,
                                  (AttribsChangedFunc)refresh_connectivity_cache,
                                   toplevel);
}

/*!
 * \brief Initialize LibGeda Connection system
 * \par Function Description
 *  This function add a hook to New page object so that the
 *  function geda_struct_conn_init_page is called each time a new page
 *  object is created.
 */
void geda_struct_conn_init (void)
{
    geda_page_append_new_hook ((NewPageFunc) geda_struct_conn_init_page, NULL);
}
