/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010-2015 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_smob.c
 * \brief Scheme representations of gEDA C structures
 *
 * In order for Scheme code to be able to manipulate libgeda data
 * structures, it is convenient for the code to be able to get handles
 * to several of the different C GedaObject types used by libgeda, in
 * particular #GedaToplevel, #Page and #GedaObject.
 *
 * A particular issue is that, in principle, Guile can stash a
 * variable somewhere and only try and access it much later, possibly
 * after the underlying C structure has already been freed.
 *
 * In order to avoid this situation causing a segmentation fault, weak
 * references are used. In the case of #Page and #GedaToplevel handles,
 * the usage is quite straightforward; Scheme code can never create nor
 * destroy a #GedaToplevel; and although a #Page can be created by Scheme
 * code, the Page Object must explicitly be destroyed if the Scheme code
 * does not want the #Page to hang around after it returns.
 *
 * #GedaObject handles are a more complex case. It is possible that Scheme
 * code may legitimately want to create an #GedaObject and do something
 * with it (or, similarly, pull an #GedaObject out of a #Page), without
 * needing to carefully keep track of the #GedaObject to avoid dropping it
 * on the floor. In that case, users should be able to rely on the
 * garbage collector.
 *
 * For that reason, an #GedaObject is marked to be destroyed by
 * garbage-collection in two cases:
 *
 * -# If they have been created by Scheme code, but not yet added to a
 *    Page.
 * -# If they have been removed from a #Page by Scheme code, but not
 *    yet re-added to a #Page.
 *
 * \sa geda_page.c
 *
 * This file also provides support for a variety of GObject-based gEDA
 * types, including EdaConfig instances.
 *
 */

#include "../../../config.h"

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

scm_t_bits geda_smob_tag;

/*!
 * \brief Weak reference notify function for gEDA smobs.
 * \par Function Description
 *  Clears a gEDA smob's pointer when the target object is destroyed.
 */
static void smob_weakref_notify (void *target, void *smob) {
  SCM s = (SCM) smob;
  SCM_SET_SMOB_DATA (s, NULL);
}

/*!
 * \brief Weak reference notify function for double-length gEDA smobs.
 * \par Function Description
 *  Clears a gEDA smob's second pointer when the target object is
 *  destroyed.
 *
 * \see edascm_from_object().
 */
static void smob_weakref2_notify (void *target, void *smob) {
  SCM s = (SCM) smob;
  SCM_SET_SMOB_DATA_2 (s, NULL);
}

/*!
 * \brief Free a gEDA smob.
 * \par Function Description
 *  Finalizes a gEDA smob for deletion, removing the weak reference.
 *
 *  Used internally in Guile.
 */
static size_t smob_free (SCM smob)
{
  GedaToplevel*toplevel;
  void *obj;

  /* If the weak reference has already been cleared, do nothing */
  if (!EDASCM_SMOB_VALIDP(smob)) return 0;

  obj = (void *) SCM_SMOB_DATA (smob);

  /* Otherwise, clear the weak reference */
  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_TOPLEVEL:

    if (GEDA_IS_TOPLEVEL(obj)) {
      geda_toplevel_weak_unref ((GedaToplevel *) obj, smob_weakref_notify, smob);
    }
    break;

  case GEDA_SMOB_PAGE:
      geda_page_weak_unref ((Page *) obj, (WeakNotifyFunc) smob_weakref_notify, smob);
    break;

  case GEDA_SMOB_OBJECT:
    /* See edascm_from_object() for an explanation of why GedaObject
     * smobs store a GedaToplevel in the second obj word */
    if (GEDA_IS_OBJECT(obj)) {
      geda_object_weak_unref   ((GedaObject *) obj, smob_weakref_notify, smob);
    }
    toplevel = (GedaToplevel *) SCM_SMOB_DATA_2 (smob);
    if (GEDA_IS_TOPLEVEL(toplevel)) {
      geda_toplevel_weak_unref ( toplevel, smob_weakref2_notify, smob);
    }
    break;

  case GEDA_SMOB_CONFIG:
  case GEDA_SMOB_CLOSURE:
    break;
  default:
    /* This should REALLY definitely never be run */
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
  }

  /* If the smob is marked as garbage-collectable, destroy its
   * contents.
   *
   * Because Pages and GedaToplevels should never be garbage collected,
   * emit critical warnings if the GC tries to free them.
   */
  if (EDASCM_SMOB_GCP (smob)) {
    switch (EDASCM_SMOB_TYPE (smob)) {
    case GEDA_SMOB_TOPLEVEL:
      g_critical ("%s: Blocked garbage-collection of GedaToplevel %p",
                 __FUNCTION__, obj);
      break;
    case GEDA_SMOB_PAGE:
      g_critical ("%s: Blocked garbage-collection of Page %p",
                 __FUNCTION__, obj);
      break;
    case GEDA_SMOB_OBJECT:
      /* See edascm_from_object() for an explanation of why GedaObject
       * smobs store a GedaToplevel in the second data word */
      geda_struct_object_release ( (GedaObject *) obj);
      break;
    case GEDA_SMOB_CONFIG:
      /* These are reference counted, so the structure will have
       * already been destroyed above if appropriate. */
      break;
    case GEDA_SMOB_CLOSURE:
      break;
    default:
      /* This should REALLY definitely never be run */
      g_critical ("%s: received bad smob flags.", __FUNCTION__);
    }
  }

  return 0;
}

/*!
 * \brief Print a representation of a gEDA smob.
 * \par Function Description
 *  Outputs a string representing the gEDA \a smob to a Scheme output
 *  \a port. The format used is "#<geda-TYPE b7ef65d0>", where TYPE is
 *  a string describing the C structure represented by the gEDA smob.
 *
 *  Used internally in Guile.
 */
static int smob_print (SCM smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<geda-", port);

  switch (EDASCM_SMOB_TYPE (smob)) {
  case GEDA_SMOB_TOPLEVEL:
    scm_puts ("toplevel", port);
    break;
  case GEDA_SMOB_PAGE:
    scm_puts ("page", port);
    break;
  case GEDA_SMOB_OBJECT:
    scm_puts ("object", port);
    break;
  case GEDA_SMOB_CONFIG:
    scm_puts ("config", port);
    break;
  case GEDA_SMOB_CLOSURE:
    scm_puts ("closure", port);
    break;
  default:
    g_critical ("%s: received bad smob flags.", __FUNCTION__);
    scm_puts ("unknown", port);
  }

  if (SCM_SMOB_DATA (smob) != 0) {

    char *hexstring;

    scm_dynwind_begin (0);

    hexstring = geda_sprintf (" %p", (void *) SCM_SMOB_DATA (smob));

    scm_dynwind_unwind_handler (g_free, hexstring, SCM_F_WIND_EXPLICITLY);
    scm_puts (hexstring, port);
    scm_dynwind_end ();
  }
  else {
    scm_puts (" (null)", port);
  }

  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

/*!
 * \brief Check gEDA smobs for equality.
 * \par Function description
 *  Returns SCM_BOOL_T if \a obj1 represents the same gEDA structure as
 *  \a obj2 does. Otherwise, returns SCM_BOOL_F.
 *
 *  Used internally in Guile.
 */
static SCM smob_equalp (SCM obj1, SCM obj2)
{
  EDASCM_ASSERT_SMOB_VALID (obj1);
  EDASCM_ASSERT_SMOB_VALID (obj2);

  if (SCM_SMOB_DATA (obj1) == SCM_SMOB_DATA (obj2)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}

/* libgedaguile-priv.h */

/*!
 * \brief Get the smob for a GedaToplevel.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Create a new smob representing \a toplevel.
 *
 * \param toplevel #GedaToplevel to create a smob for.
 * \return a smob representing \a toplevel.
 */
SCM edascm_from_toplevel (GedaToplevel *toplevel)
{
  SCM smob;

  SCM_NEWSMOB (smob, geda_smob_tag, toplevel);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_TOPLEVEL);

  /* Set weak reference */
  geda_toplevel_weak_ref (toplevel, smob_weakref_notify, smob);

  return smob;
}

/* libgedaguile.h */

/*!
 * \brief Get a smob for a page.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Create a new smob representing \a page.
 *
 * \param page #Page to create a smob for.
 *
 * \return a smob representing \a page.
 */
SCM edascm_from_page (Page *page)
{
  SCM smob;

  SCM_NEWSMOB        (smob, geda_smob_tag, page);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_PAGE);

  /* Set weak reference */
  geda_page_weak_ref (page, (WeakNotifyFunc) smob_weakref_notify, smob);

  return smob;
}

/*!
 * \brief Get a page from a smob.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Return the #Page represented by \a smob.
 *
 * \param [in] smob Guile value to retrieve #Page from.
 * \return the #Page represented by \a smob.
 */
Page *edascm_to_page (SCM smob)
{

#ifndef NDEBUG
  SCM_ASSERT (EDASCM_PAGEP (smob), smob, SCM_ARG1, "edascm_to_page");
#endif

  EDASCM_ASSERT_SMOB_VALID (smob);

  return GEDA_PAGE(SCM_SMOB_DATA (smob));
}

/*!
 * \brief Get a smob for a schematic object.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Create a new smob representing \a object.
 *
 * \warning The returned smob is initially marked as owned by the C
 *          code. If it should be permitted to be garbage-collected,
 *          you should set the garbage-collectable flag by calling:
 *
 * \code
 *   SCM x = edascm_from_object (object);
 *   edascm_c_set_gc (x, 1);
 * \endcode
 *
 * \note We currently have to bake a GedaToplevel pointer into the smob,
 *       so that if the object becomes garbage-collectable we can obtain
 *       a GedaToplevel to use for deleting the smob without accessing the
 *       GedaToplevel fluid and potentially causing a race condition (see
 *       bug 909358).
 *
 * \param object #GedaObject to create a smob for.
 * \return a smob representing \a object.
 */
SCM edascm_from_object (GedaObject *object)
{
  SCM smob;
  GedaToplevel *toplevel = edascm_c_current_toplevel ();

  SCM_NEWSMOB2       (smob, geda_smob_tag, object, toplevel);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_OBJECT);

  /* Set weak references */
  geda_object_weak_ref   (object,   smob_weakref_notify, smob);
  geda_toplevel_weak_ref (toplevel, smob_weakref2_notify, smob);

  return smob;
}

/*!
 * \brief Get a schematic object from a smob.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Return the #GedaObject represented by \a smob.
 *
 * \param [in] smob Guile value to retrieve #GedaObject from.
 * \return the #GedaObject represented by \a smob.
 */
GedaObject *edascm_to_object (SCM smob)
{

#ifndef NDEBUG
  SCM_ASSERT (EDASCM_OBJECTP (smob), smob, SCM_ARG1, "edascm_to_object");
#endif

  EDASCM_ASSERT_SMOB_VALID (smob);

  return GEDA_OBJECT(SCM_SMOB_DATA (smob));
}

/*!
 * \brief Get a smob for a configuration context.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Create a new smob representing \a cfg.
 *
 * \param cfg Configuration context to create a smob for.
 * \return a smob representing \a cfg.
 */
SCM edascm_from_config (EdaConfig *cfg)
{
  SCM smob;
  SCM_NEWSMOB (smob, geda_smob_tag, cfg);
  SCM_SET_SMOB_FLAGS (smob, GEDA_SMOB_CONFIG);
  return smob;
}

/*!
 * \brief Get a configuration context from a smob.
 * \ingroup guile_c_iface
 * \par Function Description
 *  Return the #EdaConfig represented by \a smob.
 *
 * \param [in] smob Guile value to retrieve #EdaConfig from.
 * \return the #EdaConfig represented by \a smob.
 */
EdaConfig *edascm_to_config (SCM smob)
{

#ifndef NDEBUG
  SCM_ASSERT (EDASCM_CONFIGP (smob), smob, SCM_ARG1, "edascm_to_object");
#endif

  EDASCM_ASSERT_SMOB_VALID (smob);

  return EDA_CONFIG (SCM_SMOB_DATA (smob));
}

/*!
 * \brief Test whether a smob is a #GedaObject instance
 * \ingroup guile_c_iface
 * \par Function Description
 *  If \a smob is a #GedaObject instance, returns non-zero. Otherwise,
 *  returns zero.
 *
 * \param [in] smob Guile value to test.
 *
 * \return non-zero iff \a smob is a #GedaObject instance.
 */
int edascm_is_object (SCM smob)
{
  return EDASCM_OBJECTP (smob);
}

/*!
 * \brief Test whether a smob is a #Page instance
 * \ingroup guile_c_iface
 * \par Function Description
 *  If \a smob is a #Page instance, returns non-zero. Otherwise,
 *  returns zero.
 *
 * \param [in] smob Guile value to test.
 *
 * \return non-zero iff \a smob is a #Page instance.
 */
int edascm_is_page (SCM smob)
{
  return EDASCM_PAGEP (smob);
}

/*!
 * \brief Test whether a smob is an #EdaConfig instance.
 * \ingroup guile_c_iface
 * \par Function Description
 *  If \a smob is a configuration context, returns non-zero. Otherwise,
 *  returns zero.
 *
 * \param [in] smob Guile value to test.
 * \return non-zero iff \a smob is an #EdaConfig instance.
 */
int edascm_is_config (SCM smob)
{
  return EDASCM_CONFIGP (smob);
}

/*!
 * \brief Set whether a gEDA object may be garbage collected.
 * \ingroup guile_c_iface
 * \par Function Description
 *  If \a gc is non-zero, allow the structure represented by \a smob to
 *  be destroyed when \a smob is garbage-collected.
 *
 * \param [in,out] smob Smob for which to set garbage-collection
 *                      permission.
 * \param [in]     gc    If non-zero, permit garbage collection.
 */
void edascm_c_set_gc (SCM smob, int gc)
{

  EDASCM_ASSERT_SMOB_VALID (smob);
  EDASCM_SMOB_SET_GC (smob, gc);
/*
  if (!gc) {

    GedaToplevel* toplevel;
    void* obj = (void *) SCM_SMOB_DATA (smob);

    switch (EDASCM_SMOB_TYPE (smob)) {
      case GEDA_SMOB_TOPLEVEL:
        if (GEDA_IS_TOPLEVEL(obj)) {
          geda_toplevel_weak_unref ((GedaToplevel *) obj, smob_weakref_notify, smob);
        }
        break;

      case GEDA_SMOB_PAGE:
        geda_page_weak_unref ((Page *) obj, (WeakNotifyFunc)smob_weakref_notify, smob);
        break;

      case GEDA_SMOB_OBJECT:
        // See edascm_from_object() for an explanation of why GedaObject
        // smobs store a GedaToplevel in the second obj word
        if (GEDA_IS_OBJECT(obj)) {
          geda_object_weak_unref   ((GedaObject *) obj, smob_weakref_notify, smob);
        }
        toplevel = (GedaToplevel *) SCM_SMOB_DATA_2 (smob);
        if (GEDA_IS_TOPLEVEL(toplevel)) {
          geda_toplevel_weak_unref ( toplevel, smob_weakref2_notify, smob);
        }
        break;

      case GEDA_SMOB_CONFIG:
        GEDA_UNREF (G_OBJECT (obj));
        break;

      case GEDA_SMOB_CLOSURE:
        break;
      default:
        // This should REALLY definitely never be run
        g_critical ("%s: received bad smob flags.", __FUNCTION__);
    }
  }*/
}

/* ---------------------------------------------------------------- */

/*!
 * \brief Test whether a smob is a #Page instance.
 * \par Function Description
 *  If \a page_smob is a #Page instance, returns \b SCM_BOOL_T;
 *  otherwise returns \b SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %page? procedure in the (geda
 *       core smob) module.
 *
 * param [in] page_smob Guile value to test.
 *
 * \return SCM_BOOL_T iff \a page_smob is a #Page instance.
 */
EDA_SCM_DEFINE (smob_page_p, "%page?", 1, 0, 0,
               (SCM page_smob),
               "Test whether the value is a gEDA Page instance.")
{
  return (EDASCM_PAGEP (page_smob) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*!
 * \brief Test whether a smob is an #GedaObject instance.
 * \par Function Description
 *  If \a object_smob is an #GedaObject instance, returns \b SCM_BOOL_T;
 *  otherwise returns \b SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %object? procedure in the (geda
 *       core smob) module.
 *
 * param [in] object_smob Guile value to test.
 *
 * \return SCM_BOOL_T iff \a object_smob is an #GedaObject instance.
 */
EDA_SCM_DEFINE (smob_object_p, "%object?", 1, 0, 0, (SCM object_smob),
               "Test whether the value is a gEDA GedaObject instance.")
{
  return (EDASCM_OBJECTP (object_smob) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*!
 * \brief Test whether a smob is an #EdaConfig instance.
 * \par Function Description
 *  If \a config_smob is a configuration context, returns \b
 *  SCM_BOOL_T; otherwise returns \b SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %config? procedure in the (geda
 *       core smob) module.
 *
 * param [in] config_smob Guile value to test.
 *
 * \return SCM_BOOL_T iff \a config_smob is an #EdaConfig instance.
 */
EDA_SCM_DEFINE (smob_config_p, "%config?", 1, 0, 0,
               (SCM config_smob),
               "Test whether the value is a gEDA configuration context.")
{
  return (EDASCM_CONFIGP (config_smob) ? SCM_BOOL_T : SCM_BOOL_F);
}

/*!
 * \brief Create the (geda core smob) Scheme module.
 * \par Function Description
 *  Defines procedures in the (geda core smob) module. The module can
 *  be accessed using (use-modules (geda core smob)).
 */
static void init_module_geda_core_smob (void *nothing)
{
  /* Register the functions. */
  #include "scheme_smob.x"

  /* Add them to the module's public definitions. */
  scm_c_export (scheme_smob_page_p,
                scheme_smob_object_p,
                scheme_smob_config_p, NULL);
}

/*!
 * \brief Initialize the basic gEDA smob types.
 * \par Function Description
 *  Registers the gEDA core smob types and some procedures acting on
 *  them.  gEDA only uses a single Guile smob, and uses the flags field
 *  to multiplex the several different underlying C structures that may
 *  be represented by that smob. Should only be called by
 *  edascm_init().
 */
void edascm_init_smob (void)
{
  /* Register gEDA smob type */
  geda_smob_tag = scm_make_smob_type ("geda", 0);

  scm_set_smob_free (geda_smob_tag, smob_free);
  scm_set_smob_print (geda_smob_tag, smob_print);
  scm_set_smob_equalp (geda_smob_tag, smob_equalp);

  /* Define the (geda core smob) module */
  scm_c_define_module ("geda core smob",
                       init_module_geda_core_smob,
                       NULL);
}
