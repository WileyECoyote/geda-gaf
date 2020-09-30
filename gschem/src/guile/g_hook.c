/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_hook.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */

#include "../../include/gschem.h"
#include <geda_debug.h>

SCM_SYMBOL (at_sym,       "@");
SCM_SYMBOL (gschem_sym,   "gschem");
SCM_SYMBOL (core_sym,     "core");
SCM_SYMBOL (hook_sym,     "hook");
SCM_SYMBOL (run_hook_sym, "run-hook");
SCM_SYMBOL (list_sym,     "list");

/*! \brief */
struct ghook_t {
  const char* name;
  int arity;
  /* WEH: should a mutex be here? */
};

/*! \brief */
static struct ghook_t gschem_hooks[] = {
  /* Hooks */
  { "%invalid-hook",          1 },
  { "%action-property-hook",  3 },
  { "%add-objects-hook",      1 },
  { "%attach-attribs-hook",   1 },
  { "%bind-keys-hook",        3 },
  { "%change-page-hook",      1 },
  { "%copy-objects-hook",     1 },
  { "%close-page-hook",       1 },
  { "%deselect-objects-hook", 1 },
  { "%detach-attribs-hook",   1 },
  { "%mirror-objects-hook",   1 },
  { "%move-objects-hook",     1 },
  { "%new-page-hook",         1 },
  { "%open-page-hook",        1 },
  { "%paste-objects-hook",    1 },
  { "%remove-objects-hook",   1 },
  { "%rotate-objects-hook",   1 },
  { "%select-objects-hook",   1 },
  { NULL }
};

/*!
 * \brief Gets a Scheme hook object by name.
 * \par Function Description
 *  Returns the contents of variable with the given name in the (gschem
 *  core hook). Used for looking up hook objects.
 *
 * \param name name of hook to lookup.
 * \return value found in the (gschem core hook) module.
 */
SCM
g_get_hook_by_name (const char *name)
{
#if DEBUG || DEBUG_HOOKS
  fprintf(stderr, "g_get_hook_by_name: begin, name=%s\n", name);
#endif

  SCM exp = scm_list_3 (at_sym,
                        scm_list_3 (gschem_sym, core_sym, hook_sym),
                        scm_from_utf8_symbol (name));

#if DEBUG || DEBUG_HOOKS
  fprintf(stderr, "g_get_hook_by_name: exit\n");
#endif
  return g_evaluate_scm_protected (exp, SCM_UNDEFINED);
}

/*!
 * \brief Runs a object hook with a single Object.
 * \par Function Description
 *  Runs a hook called \a name, which should expect a list of Object
 *  smobs as its argument, with a single-element list containing only \a obj.
 *
 * \sa g_hook_run_object_list()
 *
 * \param w_current Gschem Toplevel object
 * \param name      name of hook to run.
 * \param obj       GedaObject argument for hook.
 */
static void
g_hook_idle_run_object(GschemToplevel *w_current, const char *name, GedaObject *obj)
{
  scm_dynwind_begin (0);

  g_dynwind_window (w_current);

  SCM expr = scm_list_3 (run_hook_sym,
                         g_get_hook_by_name (name),
                         scm_list_2 (list_sym, edascm_from_object (obj)));

#if DEBUG || DEBUG_HOOKS
  fprintf(stderr,"%s running hook <%s>\n",__func__, name);
#endif

  g_evaluate_scm_protected (expr, scm_interaction_environment ());
  scm_dynwind_end ();
  scm_remember_upto_here_1 (expr);
}

/*!
 * \brief Runs a object hook for a list of objects.
 * \par Function Description
 *  Runs a hook called \a name, which should expect a list of Object
 *  smobs as its argument, with \a obj_lst as the argument list.
 *
 * \sa g_hook_run_object() g_hook_idle_run_object()
 *
 * \param w_current Gschem Toplevel object
 * \param name      name of hook to run.
 * \param obj_lst   list of Object smobs as hook argument.
 */
static void
g_hook_idle_run_object_list (GschemToplevel *w_current, const char *name,
                             GList *obj_lst)
{
  GList *iter;
  int    count = 0;
  SCM    lst   = SCM_EOL;

  scm_dynwind_begin (0);
  g_dynwind_window (w_current);

  for (iter = obj_lst; iter != NULL; NEXT(iter)) {

    GedaObject *object = iter->data;

    if (GEDA_IS_OBJECT(object)) {
      lst = scm_cons (edascm_from_object (object), lst);
      count++;
    }
  }

  if (count) {

    SCM expr = scm_list_3 (run_hook_sym,
                           g_get_hook_by_name (name),
                           scm_cons (list_sym,
                                     scm_reverse_x (lst, SCM_EOL)));

    g_evaluate_scm_protected (expr, scm_interaction_environment ());

    scm_remember_upto_here_1 (expr);
  }

  scm_dynwind_end ();
}

/*!
 * \brief Runs a page hook.
 * \par Function Description
 *  Runs a hook called \a name, which should expect the single Page \a
 *  page as an argument.
 *
 * \param [in] w_current Gschem Toplevel object
 * \param [in] name      name of hook to run
 * \param [in] page      Page argument for hook.
 */
void
g_hook_idle_run_page (GschemToplevel *w_current, const char *name, Page *page)
{
  scm_dynwind_begin (0);
  g_dynwind_window (w_current);

  SCM expr = scm_list_3 (run_hook_sym,
                         g_get_hook_by_name (name),
                         edascm_from_page (page));

  g_evaluate_scm_protected (expr, scm_interaction_environment ());
  scm_dynwind_end ();
  scm_remember_upto_here_1 (expr);
}

/*!
 * \brief Creates an EdascmHookProxy for a named hook.
 * \par Function Description
 *  Return a newly-created hook proxy object for the hook called \a name.
 *
 * \param name  name of the hook for which to create a proxy.
 * \return      newly-created EdascmHookProxy instance.
 */
EdascmHookProxy *
g_hook_new_proxy_by_name (const char *name)
{
  SCM hook = g_get_hook_by_name (name);
  return edascm_hook_proxy_new_with_hook (hook);
}

/* Anonymous Static Mutex */
static GedaMutex (g_lock_is_busy);

static int is_busy = 0;

static void set_is_busy(int value)
{
  g_mutex_lock((GMutex*)&g_lock_is_busy);
    is_busy = value;
  g_mutex_unlock((GMutex*)&g_lock_is_busy);
}

static int get_is_busy()
{
  int ret_val;

  g_mutex_lock((GMutex*)&g_lock_is_busy);
    ret_val = is_busy;
  g_mutex_unlock((GMutex*)&g_lock_is_busy);

  return ret_val;
}

/*!
 * \brief Dispatch Idle Hook Notify Source is Destroyed
 * \par Function Description
 *  Callback handler for notication that the main loop source
 *  g_hook_run_idle_callback has been destroyed; removes reference
 *  on incapsulated objects and releases the memory allocated by
 *  with g_hook_get_new_capsule
 */
static void g_hook_run_idle_notify (void *data)
{
  IdleHookData *capsule = data;;

  set_is_busy(FALSE);

  if (capsule->type == LIST_HOOK) {

    GList *hook_list = capsule->data.list;
    GList *iter;

    for (iter = hook_list; iter != NULL; NEXT(iter)) {

      GedaObject *object = iter->data;

      if (GEDA_IS_OBJECT(object)) {
        g_object_unref (object);
      }
    }
    g_list_free(hook_list);
  }
  else { /* Page or single object */
    g_object_unref (capsule->data.object);
  }

  GEDA_FREE(capsule->name);
  GEDA_FREE(data);
}

/*!
 * \brief Callback Dispatch Idle Hook
 * \par Function Description
 *  This is a main-loop task instigated to run SCM hooks.
 *
 * \warning Should never be called from outside the main context.
 *          This should not be a problem since the function is a
 *          callback.
 *
 * \param [in] data  IdleHookData record for hook arguments
 *
 * \returns False so source is destroyed automatically
 */
static bool g_hook_run_idle_callback (void *data)
{
  struct ghook_t *record = gschem_hooks;

  int status;

  if (get_is_busy()) {
    status = TRUE;
  }
  else {

    set_is_busy(TRUE);

    IdleHookData   *capsule   = data;
    GschemToplevel *w_current = capsule->w_current;
    const char     *hooker;

    hooker = capsule->name = geda_strdup (record[capsule->hook].name);

    switch (capsule->type) {
      case LIST_HOOK:
        g_hook_idle_run_object_list(w_current, hooker, capsule->data.list);
        break;

      case OBJECT_HOOK:
        g_hook_idle_run_object(w_current, hooker, capsule->data.object);
        break;

      case PAGE_HOOK:
        g_hook_idle_run_page(w_current, hooker, capsule->data.page);

      default:
        break;
    }
    status = FALSE;
  }
  return status;
}

/*!
 * \brief Allocate and Load new Idle Hook Data structure
 * \par Function Description
 *  Returns allocated st_idle_hook_data structure after setting
 *  top-level pointer and obtaining copy of name string.
 */
static inline IdleHookData*
g_hook_get_new_capsule(GschemToplevel *w_current, Hooker hook)
{
  IdleHookData *capsule;

  capsule            = g_malloc(sizeof(IdleHookData));
  capsule->w_current = w_current;
  capsule->hook      = hook;

  return capsule;
}

/*!
 * \brief Schedule Run Hook for Object List
 * \par Function Description
 *  Spawns idle thread to run object hooks. This is done, not because
 *  Guile is slow, but because these task need to be ran in the main
 *  loop and the callers could be running as worker threads.
 *
 * \param [in] wc        Gschem Toplevel object,
 * \param [in] hook      Enumerated hook to run,
 * \param [in] list      List of Object smobs as hook argument.
 */
void g_hook_run_object_list (GschemToplevel *wc, Hooker hook, GList *list)
{
  if (list) {

    GList *hook_list = NULL;
    GList *iter;

    for (iter = list; iter != NULL; NEXT(iter)) {

      GedaObject *object = iter->data;

      if (GEDA_IS_OBJECT(object)) {
        hook_list = g_list_append(hook_list, g_object_ref (G_OBJECT(object)));
      }
    }

    IdleHookData *capsule;

    capsule             = g_hook_get_new_capsule(wc, hook);
    capsule->data.list  = hook_list;
    capsule->type       = LIST_HOOK;
    capsule->source_id  = g_idle_add_full (G_PRIORITY_DEFAULT,
                                           g_hook_run_idle_callback,
                                           capsule,
                                           g_hook_run_idle_notify);
  }
}

/*!
 * \brief Schedule Run Object Hook
 * \par Function Description
 *  Spawns idle thread to run object hooks. This is done, not because
 *  Guile is slow, but because the task needs to be ran in the main
 *  context and the thread that triggered the hook may not have been
 *  running in the main loop.
 *
 * \param [in] w_current Gschem Toplevel object,
 * \param [in] hook      Enumerated hook to run,
 * \param [in] object    Object argument for hook.
 */
void g_hook_run_object(GschemToplevel *w_current, Hooker hook, GedaObject *object)
{
  if (object) {

    IdleHookData *capsule;

    capsule              = g_hook_get_new_capsule(w_current, hook);
    capsule->data.object = g_object_ref (object);
    capsule->type        = OBJECT_HOOK;
    capsule->source_id   = g_idle_add_full (G_PRIORITY_DEFAULT,
                                            g_hook_run_idle_callback,
                                            capsule,
                                            g_hook_run_idle_notify);
  }
}

/*!
 * \brief Schedule Run Page Hooks
 * \par Function Description
 *  Spawns idle thread to run page hooks. This is done, not because
 *  Guile is slow, but because the tasks needs to be ran in the main
 *  context.
 *
 * \param [in] w_current Gschem Toplevel object,
 * \param [in] hook      Enumerated hook to run,
 * \param [in] page      Page argument for hook.
 */
void g_hook_run_page(GschemToplevel *w_current, Hooker hook, Page *page)
{
  if (page) {

    IdleHookData *capsule;

    capsule             = g_hook_get_new_capsule(w_current, hook);
    capsule->data.page  = g_object_ref (page);
    capsule->type       = PAGE_HOOK;
    capsule->source_id  = g_idle_add_full (G_PRIORITY_DEFAULT,
                                           g_hook_run_idle_callback,
                                           capsule,
                                           g_hook_run_idle_notify);
  }
}

/*!
 * \brief Create the (gschem core hook) Scheme module.
 * \par Function Description
 *  Defines some hooks in the (gschem core hook) module. These hooks
 *  allow Scheme callbacks to be triggered on certain gschem actions.
 *  For a description of the arguments and behaviour of these hooks,
 *  please see ../scheme/gschem/hook.scm.
 */
static void
init_module_gschem_core_hook ()
{

#include "g_hook.x"

  struct ghook_t *hook = gschem_hooks;

  int i = 1;                 /* Skip, we do not have an invalid-hook */
  while(hook[i].name) {
    scm_c_define (hook[i].name, scm_make_hook (scm_from_int (hook[i].arity)));
    scm_c_export (hook[i].name, NULL);
    i++;
  }
}

/*!
 * \brief Initialize the gschem hooks.
 * \par Function Description
 *  Registers gschem's Guile hooks for various events.. Should only be
 *  called by main_prog().
 */
void
g_hook_init ()
{
  /* Define the (gschem core hook) module */
  scm_c_define_module ("gschem core hook",
                       init_module_gschem_core_hook,
                       NULL);
}
