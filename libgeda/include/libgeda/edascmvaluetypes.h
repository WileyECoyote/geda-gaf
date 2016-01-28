/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: edascmvaluetypes.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 *
 * Copyright (C) 2013-2015 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#ifndef __EDASCM_VALUE_H__
#define __EDASCM_VALUE_H__

/* ---------------------------------------------------------------- */
/* GValue support */
/*! \class EdascmSCM edascmvaluetypes.h "libgeda/edascmvaluetypes.h"
 * \ingroup guile_c_iface
 * \brief Fundamental GedaType for Guile Scheme objects.
 *
 * This GedaType provides an easy way for Guile's SCM values to be used
 * directly with the GValue polymorphic type system.
 *
 * \since 1.10.
 */

#define EDASCM_TYPE_SCM (edascm_scm_get_type ())
#define EDASCM_VALUE_HOLDS_SCM(value) (G_TYPE_CHECK_VALUE_TYPE ((value), EDASCM_TYPE_SCM))

#ifdef __cplusplus
extern "C" {
#endif

GedaType edascm_scm_get_type (void) GEDA_CONST;

void edascm_value_set_scm (GValue *value, SCM v_scm);
SCM edascm_value_get_scm (const GValue *value);

/* ---------------------------------------------------------------- */
/* GParamSpec support */

/*! \class EdascmParamSCM edascmvaluetypes.h "libgeda/edascmvaluetypes.h"
 * \ingroup guile_c_iface
 * \brief Parameter specification metadata type for Guile Scheme objects.
 *
 * This GParamSpec can be used with #EdascmSCM and the GObject
 * property system to define GObjects that have Guile Scheme objects
 * as properties.
 *
 * \since 1.10.
 */

#define EDASCM_TYPE_PARAM_SCM (edascm_param_spec_scm_get_type ())
#define EDASCM_IS_PARAM_SPEC_SCM(pspec) (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), EDASCM_TYPE_PARAM_SCM))
#define EDASCM_PARAM_SPEC_SCM(pspec) (G_TYPE_CHECK_INSTANCE_CAST ((pspec), EDASCM_TYPE_PARAM_SCM, EdascmParamSpecSCM))

GedaType edascm_param_spec_scm_get_type (void) GEDA_CONST;

typedef struct _EdascmParamSpecSCM EdascmParamSpecSCM;

struct _EdascmParamSpecSCM
{
  GParamSpec parent_instance;
};

GParamSpec *edascm_param_spec_scm (const char *name,
                                   const char *nick,
                                   const char *blurb,
                                   GParamFlags flags) GEDA_WARN_UNUSED_RESULT;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* ! __EDASCM_VALUE_H__ */
