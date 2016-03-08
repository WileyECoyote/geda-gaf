/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: edascmhookproxy.h
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

#ifndef __EDASCM_HOOK_PROXY_H__
#define __EDASCM_HOOK_PROXY_H__

/* ---------------------------------------------------------------- */

/*! \class EdascmHookProxy edascmhookproxy.h "libgeda/edascmhookproxy.h"
 *  \ingroup guile_c_iface
 *  \brief Object that connects a Guile Scheme level hook to GObject signals.
 *
 * Sometimes, it is useful to be able to manipulate GObjects (for
 * example, GTK+ UI elements) when a Guile Scheme-level hook is run.
 * Unfortunately, this can be tricky to do directly, not least since
 * the <tt>scm_add_hook_x()</tt> procedure requires a closure as an argument.
 *
 * The #EdascmHookProxy makes things much easier by allowing GObjects
 * to simply connect to its \b "run" signal in order to receive a
 * callback whenever the target hook is run.
 *
 * \code
 * void
 * my_handler (EdascmHookProxy *proxy, SCM args, void *user_data)
 * {
 *   // ... //
 * }
 * \endcode
 *
 * \b Signals: One signal, \b "run".  Called with one parameter, the
 * Scheme argument list passed to the handler as an unpacked Scheme
 * value.
 *
 * \b Properties: One property, \b "hook", the hook to proxy as an
 * unpacked Scheme value.
 *
 * \since 1.10.
 */

#define EDASCM_TYPE_HOOK_PROXY (edascm_hook_proxy_get_type ())
#define EDASCM_HOOK_PROXY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDASCM_TYPE_HOOK_PROXY, EdascmHookProxy))
#define EDASCM_HOOK_PROXY_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDASCM_TYPE_HOOK_PROXY, EdascmHookProxyClass))
#define EDASCM_IS_HOOK_PROXY(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDASCM_TYPE_HOOK_PROXY))
#define EDASCM_HOOK_PROXY_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDASCM_TYPE_HOOK_PROXY, EdascmHookProxy))

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _EdascmHookProxyClass EdascmHookProxyClass;
typedef struct _EdascmHookProxy      EdascmHookProxy;
typedef struct _EdascmHookProxyData  EdascmHookProxyData;

struct _EdascmHookProxyClass
{
  GObjectClass parent_class;

  /* signals */
  void (*run)(EdascmHookProxy *proxy, SCM args);
};

struct _EdascmHookProxy
{
  GObject parent_instance;

  /* Private members */
  EdascmHookProxyData *priv;
};

GedaType edascm_hook_proxy_get_type (void) GEDA_CONST;

EdascmHookProxy *edascm_hook_proxy_new_with_hook (SCM hook_s) GEDA_WARN_UNUSED_RESULT;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* ! __EDASCM_HOOK_PROXY_H__ */
