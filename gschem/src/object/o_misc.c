/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <libgen.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* break with the tradition here and input a list */
/*! \todo probably should go back and do the same for o_copy o_move
 *  o_delete...
 */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit(GschemToplevel *w_current, GList *list, int who)
{
  Object     *o_current;
  bool        isSymbol;
  const char *str = NULL;

  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  o_current = (Object *) list->data;
  if (o_current == NULL) {
    fprintf(stderr, _("o_edit: Got an unexpected NULL in o_edit\n"));
    return;
  }

  isSymbol = s_page_is_symbol_file (Current_Page);

  /* for now deal with only the first item */
  switch(o_current->type) {

    case(OBJ_ARC):
      x_dialog_edit_arc_angle (w_current, o_current);
      break;
    case OBJ_BOX:
      x_dialog_edit_fill_type (w_current);
      break;
    case OBJ_CIRCLE:
    case (OBJ_LINE):
      x_dialog_edit_line_type (w_current);
      break;
    case(OBJ_COMPLEX):
      x_multiattrib_open (w_current);
      break;
    case(OBJ_PLACEHOLDER):
    case(OBJ_NET):
    case(OBJ_BUS):
      x_attrib_add_dialog(w_current, o_current);
      break;
    case(OBJ_PICTURE):
      o_picture_change_filename_dialog(w_current);
      break;
    case(OBJ_PIN):
      if(isSymbol) {
        x_dialog_edit_pin_type(w_current);
      }
      else {
        x_multiattrib_open (w_current);
      }
      break;
    case(OBJ_TEXT):
      str = o_text_get_string (o_current);
      if (o_attrib_get_name_value (o_current, NULL, NULL) &&
        /* attribute editor only accept 1-line values for attribute */
        o_text_num_lines (str) == 1) {
        x_attrib_edit_dialog(w_current, o_current);
      }
      else {
        o_text_edit(w_current, o_current);
      }
      break;
  }

}

/*! \todo Finish function documentation!!!
 *  \brief Lock an Object
 *  \par Function Description
 *  Locks an object by setting the selectable property to FALSE. The
 *  color is also set to the locked_color
 */
/* This locks the entire selected list.  It does lock components, but does
 * NOT change the color (of primatives of the components) though this cannot
 * be called recursively
 */
void o_lock(GschemToplevel *w_current)
{
  Object *object = NULL;
  GList *s_current = NULL;

  /* skip over head */
  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  while(s_current != NULL) {
    object = (Object *) s_current->data;
    if (object) {
      object->selectable   = FALSE;
      if (object->color   != LOCK_COLOR)
      object->locked_color = object->color;
      object->color        = LOCK_COLOR;
      w_current->toplevel->page_current->CHANGED=1;
    }

    s_current = g_list_next(s_current);
  }

  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_sensitivities(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* You can unlock something by selecting it with a bounding box... */
/* this will probably change in the future, but for now it's a
   something.. :-) */
/* this cannot be called recursively */
void o_unlock(GschemToplevel *w_current)
{
  Object *object    = NULL;
  GList  *s_current = NULL;

  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  while(s_current != NULL) {
    object = (Object *) s_current->data;
    if (object) {
        object->selectable        = TRUE;
        if (object->locked_color != LOCK_COLOR && object->locked_color > 0)
        object->color             = object->locked_color;
        else object->color        = o_color_get_object_default(object->type);
        w_current->toplevel->page_current->CHANGED = 1;
    }
    NEXT(s_current);
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Rotate all objects in list.
 *  \par Function Description
 *  Given an object <B>list</B>, and the center of rotation
 *  (<B>centerx</B>,<B>centery</B>, this function traverses all the selection
 *  list, rotating each object through angle <B>angle</B>.
 *  The list contains a given object and all its attributes
 *  (refdes, pinname, pinlabel, ...).
 *  There is a second pass to run the rotate hooks of non-simple objects,
 *  like pin or complex objects, for example.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] centerx    Center x coordinate of rotation.
 *  \param [in] centery    Center y coordinate of rotation.
 *  \param [in] angle      Angle to rotate the objects through.
 *  \param [in] list       The list of objects to rotate.
 */
void o_rotate_world_update(GschemToplevel *w_current,
                           int centerx, int centery, int angle, GList *list)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object   *o_current;
  GList    *o_iter;

  /* this is okay if you just hit rotate and have nothing selected */
  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_glist (w_current, list);

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; NEXT(o_iter)) {
    o_current = o_iter->data;

    s_conn_remove_object (o_current);
  }

  o_glist_rotate_world(centerx, centery, angle, list );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; NEXT(o_iter)) {
    o_current = o_iter->data;

    s_conn_update_object (o_current);
  }

  o_invalidate_glist (w_current, list);

  /* Run rotate-objects-hook */
  g_run_hook_object_list (w_current, "%rotate-objects-hook", list);

  /* Don't save the undo state if we are inside an action */
  /* This is useful when rotating the selection while moving, for example */
  toplevel->page_current->CHANGED = 1;
  if (!w_current->inside_action) {
    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_mirror_world_update(GschemToplevel *w_current, int centerx, int centery, GList *list)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object   *o_current;
  GList    *o_iter;

  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_glist (w_current, list);

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; NEXT(o_iter)) {
    o_current = o_iter->data;
    s_conn_remove_object (o_current);
  }

  o_glist_mirror_world(centerx, centery, list );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; NEXT(o_iter)) {
    o_current = o_iter->data;
    s_conn_update_object (o_current);
  }

  o_invalidate_glist (w_current, list);

  /* Run mirror-objects-hook */
  g_run_hook_object_list (w_current, "%mirror-objects-hook", list);

  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/* This is a utility function to report the number of objects whose
 * visibility was changed, this function is called by:
 *
 *      o_edit_show_inherited_attrib,
 *      o_edit_show_hidden_attrib,
 *      o_edit_show_netnames
 */
static void log_visibility (int set_hidden, int set_visible)
{
  if (set_hidden > 0) {
    q_log_message(_("%d attributes were hidden\n"), set_hidden);
  }

  if (set_visible > 0) {
    q_log_message(_("%d hidden attributes were revealed\n"), set_visible);
  }
}

/*! \brief Reveal Complexes Inheritied Attributes attached to
 *  \par Function Description
 *   This function causes hidden text for inherited attributes to be redrawn,
 *   This is accomplished by setting the object visibility to 2, which results
 *   in o_get_is_visible returning true to the renderer, but is not saved when
 *   the schematic is saved. The function returns a list object that were set
 *   to be displayed, which does not include objects set to invisible, since
 *   these would not be redrawn. Instead, when text.object.visibility is set to
 *   0, the area is redrawn immediately by calling o_invalidate_force.
 */
static GList*
o_edit_show_inherited_attrib (GschemToplevel *w_current,  const GList *o_list)
{
  Object *o_current;
  GList  *iter        = (GList*)o_list;
  GList  *redraw      = NULL;
  int     set_hidden  = 0;
  int     set_visible = 0;

  while (iter != NULL) {

    o_current = (Object *)iter->data;

    if (!o_current->selectable) {
      NEXT(iter);
      continue;
    }

    if (o_current->type == OBJ_COMPLEX) {
      GList  *iter2;
      for(iter2 = o_current->complex->prim_objs; iter2; NEXT(iter2)) {
        Object *sub_obj = iter2->data;
        if (sub_obj->type == OBJ_TEXT) {
          if (sub_obj->visibility == INVISIBLE) {
            sub_obj->visibility = 2;
            redraw = g_list_prepend(redraw, sub_obj);
            ++set_visible;
          }
          else if (sub_obj->visibility == 2) {
            sub_obj->visibility = INVISIBLE;
            /* Since now invisible, renderer won't return a bounds, so... */
            o_invalidate_force(w_current, sub_obj);
            ++set_hidden;
          }
        }
      }
    }

    NEXT(iter);
  }

  log_visibility (set_hidden, set_visible);
  return redraw;
}

/*! \brief Reveal Hidden Attributes attached to Complexes
 *  \par Function Description
 *   This function causes hidden text for invisible attributes to be redrawn,
 *   This is accomplished by setting the object visibility to 2, which results
 *   in o_get_is_visible returning true to the renderer, but is not saved when
 *   the schematic is saved. The function returns a list of objects that were
 *   set to be displayed, which does not include objects set to invisible, since
 *   these would not be redrawn. Instead, when text.object.visibility is set to
 *   INVISIBLE, the area is redrawn immediately by calling o_invalidate_force.
 */
static GList*
o_edit_show_hidden_attrib (GschemToplevel *w_current,  const GList *o_list)
{
  Object *o_current;
  GList  *iter        = (GList*)o_list;
  GList  *redraw      = NULL;
  int     set_hidden  = 0;
  int     set_visible = 0;

  while (iter != NULL) {

    o_current = (Object *)iter->data;

    if (!o_current->selectable) {
      NEXT(iter);
      continue;
    }

    if (o_current->type == OBJ_COMPLEX) {
      GList  *iter2;
      for(iter2 = o_current->complex->prim_objs; iter2; NEXT(iter2)) {
        Object *sub_obj = iter2->data;
        if (sub_obj->type == OBJ_PIN) {
          GList  *pa_iter;
          for( pa_iter = sub_obj->attribs; pa_iter; NEXT(pa_iter)) {
            Object *p_attrib = pa_iter->data;
            if (p_attrib->type == OBJ_TEXT) {
              if(strncmp(p_attrib->text->string, "pinseq", 6) == 0)
                continue;
              if (p_attrib->visibility == INVISIBLE) {
                p_attrib->visibility = 2;
                redraw = g_list_prepend(redraw, p_attrib);
                ++set_visible;
              }
              else if (p_attrib->visibility == 2) {
                p_attrib->visibility = INVISIBLE;
                /* Invisible set, renderer won't return a bounds, so... */
                o_invalidate_force(w_current, p_attrib);
                ++set_hidden;
              }
            }
          }
        }
      }
    }

    if (o_current->type == OBJ_TEXT) {
      /* If the parent is not selectable then don't display this attribute */
      if (o_current->attached_to && !o_current->attached_to->selectable) {
        NEXT(iter);
        continue;
      }

      if (o_current->visibility == INVISIBLE) {
        o_current->visibility = 2;
        redraw = g_list_prepend(redraw, o_current);
        ++set_visible;
      }
      else if (o_current->visibility == 2) {
        o_current->visibility = INVISIBLE;
        /* Since now invisible, renderer won't return a bounds, so... */
        o_invalidate_force(w_current, o_current);
        ++set_hidden;
      }
    }
    NEXT(iter);
  }

  log_visibility (set_hidden, set_visible);
  return redraw;
}

/*! \brief Toggle Visibility of Hidden Attributes
 *  \par Function Description
 *  The function causes the object visibility property of all
 *  attribute text objects in the given list to be modified,
 *  based on the current visibility and the inherited flag.
 *
 *  \sa o_edit_show_hidden_attrib
 */
void o_edit_show_hidden (GschemToplevel *w_current, const GList *o_list, int inherited)
{
  if (o_list != NULL) {
    GList *modified;

    if (inherited)
      modified = o_edit_show_inherited_attrib(w_current, o_list);
    else
      modified = o_edit_show_hidden_attrib(w_current, o_list);

    if (modified) {
       o_invalidate_glist(w_current, modified);
       g_list_free(modified);
    }
  }

  o_invalidate_glist(w_current, (GList*)o_list);

  Current_Page->show_hidden_text = ! Current_Page->show_hidden_text;
  i_show_state(w_current, NULL); /* update screen status */

  if (Current_Page->show_hidden_text) {
    q_log_message(_("Hidden text is now visible\n"));
  }
  else {
    q_log_message(_("Hidden text is now invisible\n"));
  }
}

/*! \brief Toggle Visibility of Hidden Netname Attribute
 *  \par Function Description
 *  The function modifies the object visibilty property of all
 *  netname attribute text objects in the given list, searching
 *  each complex for a netname attribute. If the visibility is
 *  #INVISIBLE the value is set to 2 and vise-versa. If the text
 *  is VISIBLE the attribute is not modified.
 *
 *  \sa o_edit_show_hidden_attrib
 *
 *  \param w_current  Pointer to GschemToplevel object
 *  \param o_list     Pointer to a GList of object to check for
 *                    netname attributes
 *
 */
void o_edit_show_netnames (GschemToplevel *w_current, const GList *o_list)
{
  GList  *iter        = (GList*)o_list;
  GList  *redraw      = NULL;
  Object *a_current;
  Object *o_current;
  int     set_hidden  = 0;
  int     set_visible = 0;
  char   *name;
  char   *value;

  while (iter != NULL) {

    o_current = (Object *)iter->data;

    if (o_current->type == OBJ_TEXT) {

      /* If the parent is not selectable then don't display this attribute */
      if (o_current->attached_to && o_current->attached_to->selectable) {
        if (o_attrib_string_get_name_value(o_current->text->string, &name, &value)) {
          if( strcmp(name, "netname") == 0) {
            if (o_current->visibility == INVISIBLE) {
              o_current->visibility = 2;
              redraw = g_list_prepend(redraw, o_current);
              ++set_visible;
            }
            else if (o_current->visibility == 2) {
              o_current->visibility = INVISIBLE;
              /* Since now invisible, renderer won't return a bounds, so... */
              o_invalidate_force(w_current, o_current);
              ++set_hidden;
            }
          }
          GEDA_FREE(name);
          GEDA_FREE(value);
        }
      }
    }
    else if (o_current->type == OBJ_COMPLEX) {
      a_current = o_attrib_first_attrib_by_name (o_current, "netname");
      if ( a_current != NULL) {
        if (a_current->visibility == INVISIBLE) {
          a_current->visibility = 2;
          redraw = g_list_prepend(redraw, a_current);
          ++set_visible;
        }
        else if (a_current->visibility == 2) {
          a_current->visibility = INVISIBLE;
          /* Since now invisible, renderer won't return a bounds, so... */
          o_invalidate_force(w_current, a_current);
          ++set_hidden;
        }
      }
    }
    NEXT(iter);
  }

  log_visibility (set_hidden, set_visible);
  o_invalidate_glist(w_current, (GList*)redraw);
}

Object *last_o = NULL;
int skiplast;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \todo Only descends into the first source schematic
 *
 */
int o_edit_find_text (GschemToplevel *w_current, const GList *o_list,
                      const char     *stext,       int descend, int skip)
{
  GedaToplevel *toplevel     = w_current->toplevel;
  Page     *parent           = NULL;
  char     *attrib           = NULL;
  char     *current_filename = NULL;
  int       count            = 0;
  int       page_control     = 0;
  int       pcount           = 0;
  int       rv;
  int       text_screen_height;

  const     GList *iter;

  Object *o_current;

  skiplast = skip;

  iter = o_list;
  while (iter != NULL) {
    o_current = (Object *)iter->data;

    if (descend) {
      if (o_current->type == OBJ_COMPLEX) {
        parent = toplevel->page_current;
        attrib = o_attrib_search_attached_attribs_by_name (o_current, "source", count);

        /* if above is null, then look inside symbol */
        if (attrib == NULL) {
          attrib = o_attrib_search_inherited_attribs_by_name (o_current, "source", count);
          /*          looking_inside = TRUE; */
        }

        if (attrib) {
          pcount = 0;
          current_filename = u_basic_breakup_string(attrib, ',', pcount);
          if (current_filename != NULL) {
            Page *child_page =
            s_hierarchy_down_schematic_single(toplevel, current_filename, parent, page_control, HIERARCHY_NORMAL_LOAD, NULL);

            if (child_page != NULL) {
              page_control = child_page->page_control;
              rv = o_edit_find_text (w_current, s_page_get_objects (child_page), stext, descend, skiplast);
              if (!rv) {
                s_page_goto( toplevel, child_page );
                return 0;
              }
            }
          }
        }
      }
    }

    if (o_current->type == OBJ_TEXT &&
       (o_get_is_visible (o_current) ||
        Current_Page->show_hidden_text))
    {
      const char *str = o_text_get_string (o_current);

      /* replaced strcmp with strstr to simplify the search */
      if (strstr (str,stext)) {
        if (!skiplast) {
          int x1, y1, x2, y2;

          i_zoom_world(w_current, ZOOM_FULL_DIRECTIVE, DONTCARE, I_PAN_DONT_REDRAW);

          if (!world_get_single_object_bounds (o_current, &x1, &y1, &x2, &y2)) {
            u_log_message("Internal Error Detected: <o_edit_find_text> world object bounds returned FALSE\n");
            return 0;
          }

          text_screen_height = SCREENabs (w_current, y2 - y1);

          /* this code will zoom/pan till the text screen height is about */
          /* 50 pixels high, perhaps a future enhancement will be to make */
          /* this number configurable */
          while (text_screen_height < 50) {
            i_zoom_world(w_current, ZOOM_IN_DIRECTIVE, DONTCARE, I_PAN_DONT_REDRAW);
            text_screen_height = SCREENabs (w_current, y2 - y1);
          }

          i_pan_world_general(w_current, o_current->text->x, o_current->text->y, 1, 0);

          /* Make sure the titlebar and scrollbars are up-to-date */
          x_window_set_current_page(w_current, Current_Page);

          last_o = o_current;
          break;
        }

        if (last_o == o_current) {
          skiplast = 0;
        }
      }           /* endif (strstr(o_current->text->string,stext)) */
    }             /* endif (o_current->type == OBJ_TEXT) */
    NEXT(iter);

    if (iter == NULL) {
      return 1;
    }
  }

  return (iter == NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_hide_specific_text (GschemToplevel *w_current,
                                const GList *o_list,
                                const char *stext)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object   *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (Object *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      const char *str = o_text_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (o_get_is_visible (o_current)) {
          o_set_visibility (o_current, INVISIBLE);
          o_text_recreate(o_current);
          toplevel->page_current->CHANGED = 1;
        }
      }
    }
    NEXT(iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
  o_invalidate_all (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_specific_text (GschemToplevel *w_current,
                                const GList *o_list,
                                const char *stext)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (Object *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      const char *str = o_text_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (!o_get_is_visible (o_current)) {
          o_set_visibility (o_current, VISIBLE);
          o_text_recreate(o_current);

          toplevel->page_current->CHANGED = 1;
        }
      }
    }
    NEXT(iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] w_current  The GschemToplevel object to search for autosave's.
 */
void o_autosave_backups(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList    *iter;
  Page     *p_save, *p_current;
  char     *backup_filename;
  char     *real_filename;
  char     *only_filename;
  char     *dirname;
  mode_t    saved_umask;
  mode_t    mask;
  struct    stat st;

  /* save current page */
  p_save = toplevel->page_current;

  for (iter = geda_list_get_glist(toplevel->pages); iter != NULL; NEXT(iter))
  {
    p_current = (Page *)iter->data;

    if (p_current->do_autosave_backup == 0) {
      continue;
    }
    if (p_current->ops_since_last_backup != 0) {
      /* make p_current the current page of toplevel */
      s_page_goto (toplevel, p_current);

      /* Get the real filename and file permissions */
      real_filename = follow_symlinks (p_current->filename, NULL);

      if (real_filename == NULL) {
        u_log_message (_("o_autosave_backups: Can't get the real filename of %s."), p_current->filename);
      } else {
        /* Get the directory in which the real filename lives */
        dirname = g_path_get_dirname (real_filename);
        only_filename = g_path_get_basename(real_filename);

        backup_filename = g_strdup_printf("%s%c"AUTOSAVE_BACKUP_FILENAME_STRING,
                                          dirname, DIR_SEPARATOR, only_filename);

        /* If there is not an existing file with that name, compute the
         * permissions and uid/gid that we will use for the newly-created file.
         */

        if (stat (real_filename, &st) != 0) {
#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
            struct stat dir_st;
            int result;
#endif

            /* Use default permissions */
            saved_umask = umask(0);
            st.st_mode = 0666 & ~saved_umask;
            umask(saved_umask);
#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
            st.st_uid = getuid ();

            result = stat (dirname, &dir_st);

            if (result == 0 && (dir_st.st_mode & S_ISGID))
              st.st_gid = dir_st.st_gid;
            else
              st.st_gid = getgid ();
#endif
          }
        GEDA_FREE (dirname);
        GEDA_FREE (only_filename);
        GEDA_FREE (real_filename);

        /* Make the backup file writable before saving a new one */
        if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
             (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR))) {
          saved_umask = umask(0);
          if (chmod(backup_filename, (S_IWRITE|S_IWGRP|S_IWOTH) &
                    ((~saved_umask) & 0777)) != 0) {
            u_log_message (_("Could NOT set previous backup file [%s] read-write\n"),
                           backup_filename);
          }
          umask(saved_umask);
        }

        if (o_save (toplevel,
                    s_page_get_objects (toplevel->page_current),
                    backup_filename, NULL)) {

          p_current->ops_since_last_backup = 0;
                p_current->do_autosave_backup = 0;

          /* Make the backup file readonly so a 'rm *' command will ask
             the user before deleting it */
          saved_umask = umask(0);
          mask = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
          mask = (~mask)&0777;
          mask &= ((~saved_umask) & 0777);
          if (chmod(backup_filename,mask) != 0) {
            u_log_message (_("Could NOT set backup file [%s] readonly\n"),
                           backup_filename);
          }
          umask(saved_umask);
        } else {
          u_log_message (_("Could NOT save backup file [%s]\n"),
                         backup_filename);
        }
        GEDA_FREE (backup_filename);
      }
    }
  }
  /* restore current page */
  s_page_goto (toplevel, p_save);
}
