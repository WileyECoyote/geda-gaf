/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: geda_marshal.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 * Date: August 18, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_MARSHAL_H__
#define __GEDA_MARSHAL_H__

#include	<glib-object.h>

#ifdef __cplusplus
extern "C" {
#endif

/* BOOL:BOXED (geda-marshal.list:1) */
extern void geda_marshal_BOOLEAN__BOXED (GClosure     *closure,
                                         GValue       *return_value,
                                         unsigned int  n_param_values,
                                         const GValue *param_values,
                                         void         *invocation_hint,
                                         void         *marshal_data);
#define geda_marshal_BOOL__BOXED        geda_marshal_BOOLEAN__BOXED

/* BOOL:BOXED,BOXED (geda-marshal.list:2) */
extern void geda_marshal_BOOLEAN__BOXED_BOXED (GClosure     *closure,
                                               GValue       *return_value,
                                               unsigned int  n_param_values,
                                               const GValue *param_values,
                                               void         *invocation_hint,
                                               void         *marshal_data);
#define geda_marshal_BOOL__BOXED_BOXED  geda_marshal_BOOLEAN__BOXED_BOXED

/* BOOL:BOXED,DOUBLE,DOUBLE (geda-marshal.list:3) */
extern void geda_marshal_BOOLEAN__BOXED_DOUBLE_DOUBLE (GClosure     *closure,
                                                       GValue       *return_value,
                                                       unsigned int  n_param_values,
                                                       const GValue *param_values,
                                                       void         *invocation_hint,
                                                       void         *marshal_data);
#define geda_marshal_BOOL__BOXED_DOUBLE_DOUBLE  geda_marshal_BOOLEAN__BOXED_DOUBLE_DOUBLE

/* BOOL:BOXED,POINTER (geda-marshal.list:4) */
extern void geda_marshal_BOOLEAN__BOXED_POINTER (GClosure     *closure,
                                                 GValue       *return_value,
                                                 unsigned int  n_param_values,
                                                 const GValue *param_values,
                                                 void         *invocation_hint,
                                                 void         *marshal_data);
#define geda_marshal_BOOL__BOXED_POINTER    geda_marshal_BOOLEAN__BOXED_POINTER

/* BOOL:BOXED,STRING (geda-marshal.list:5) */
extern void geda_marshal_BOOLEAN__BOXED_STRING (GClosure     *closure,
                                                GValue       *return_value,
                                                unsigned int  n_param_values,
                                                const GValue *param_values,
                                                void         *invocation_hint,
                                                void         *marshal_data);
#define geda_marshal_BOOL__BOXED_STRING geda_marshal_BOOLEAN__BOXED_STRING

/* BOOL:INT (geda-marshal.list:6) */
extern void geda_marshal_BOOLEAN__INT (GClosure     *closure,
                                       GValue       *return_value GEDA_UNUSED,
                                       unsigned int  n_param_values,
                                       const GValue *param_values,
                                       void         *invocation_hint GEDA_UNUSED,
                                       void         *marshal_data);
#define geda_marshal_BOOL__INT  geda_marshal_BOOLEAN__INT

/* BOOL:INT,INT (geda-marshal.list:7) */
extern void geda_marshal_BOOLEAN__INT_INT (GClosure     *closure,
                                           GValue       *return_value,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint,
                                           void         *marshal_data);

#define geda_marshal_BOOL__INT_INT geda_marshal_BOOLEAN__INT_INT

/* BOOL:INT,INT,POINTER,POINTER (geda-marshal.list:8) */
extern void geda_marshal_BOOLEAN__INT_INT_POINTER_POINTER (GClosure     *closure,
                                                           GValue       *return_value,
                                                           unsigned int  n_param_values,
                                                           const GValue *param_values,
                                                           void         *invocation_hint,
                                                           void         *marshal_data);

#define geda_marshal_BOOL__INT_INT_POINTER_POINTER	geda_marshal_BOOLEAN__INT_INT_POINTER_POINTER

/* BOOL:POINTER (geda-marshal.list:9) */
extern void geda_marshal_BOOLEAN__POINTER (GClosure     *closure,
                                           GValue       *return_value,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint,
                                           void         *marshal_data);
#define geda_marshal_BOOL__POINTER  geda_marshal_BOOLEAN__POINTER

/* BOOL:POINTER,BOXED (geda-marshal.list:10) */
extern void geda_marshal_BOOLEAN__POINTER_BOXED (GClosure     *closure,
                                                 GValue       *return_value,
                                                 unsigned int  n_param_values,
                                                 const GValue *param_values,
                                                 void         *invocation_hint,
                                                 void         *marshal_data);
#define geda_marshal_BOOL__POINTER_BOXED    geda_marshal_BOOLEAN__POINTER_BOXED

/* BOOL:POINTER,POINTER (geda-marshal.list:11) */
extern void geda_marshal_BOOLEAN__POINTER_POINTER (GClosure     *closure,
                                                   GValue       *return_value,
                                                   unsigned int  n_param_values,
                                                   const GValue *param_values,
                                                   void         *invocation_hint,
                                                   void         *marshal_data);
#define geda_marshal_BOOL__POINTER_POINTER geda_marshal_BOOLEAN__POINTER_POINTER

/* BOOL:POINTER,STRING (geda-marshal.list:12) */
extern void geda_marshal_BOOLEAN__POINTER_STRING (GClosure     *closure,
                                                  GValue       *return_value,
                                                  unsigned int  n_param_values,
                                                  const GValue *param_values,
                                                  void         *invocation_hint,
                                                  void         *marshal_data);

/* BOOLEAN:VOID (./gtkmarshalers.list:13) */
extern void geda_marshal_BOOLEAN__VOID (GClosure     *closure,
                                        GValue       *return_value,
                                        unsigned int  n_param_values,
                                        const GValue *param_values,
                                        void         *invocation_hint,
                                        void         *marshal_data);
#define geda_marshal_BOOL__VOID geda_marshal_BOOLEAN__VOID

/* INT:INT (geda-marshal.list:14) */
extern void geda_marshal_INT__INT (GClosure     *closure,
                                   GValue       *return_value GEDA_UNUSED,
                                   unsigned int  n_param_values,
                                   const GValue *param_values,
                                   void         *invocation_hint GEDA_UNUSED,
                                   void         *marshal_data);


/* INT:OBJECT,OBJECT,POINTER (geda-marshal.list:15) */
extern void geda_marshal_INT__OBJECT_OBJECT_POINTER (GClosure     *closure,
                                                     GValue       *return_value GEDA_UNUSED,
                                                     unsigned int  n_param_values,
                                                     const GValue *param_values,
                                                     void         *invocation_hint GEDA_UNUSED,
                                                     void         *marshal_data);

/* STRING:STRING (geda-marshal.list:16) */
extern void geda_marshal_STRING__STRING (GClosure     *closure,
                                         GValue       *return_value,
                                         unsigned int  n_param_values,
                                         const GValue *param_values,
                                         void         *invocation_hint,
                                         void         *marshal_data);

/* VOID:BOOL (geda-marshal.list:17) */
#define geda_marshal_VOID__BOOLEAN  g_cclosure_marshal_VOID__BOOLEAN
#define geda_marshal_VOID__BOOL     geda_marshal_VOID__BOOLEAN

/* VOID:BOXED (geda-marshal.list:18) */
#define geda_marshal_VOID__BOXED    g_cclosure_marshal_VOID__BOXED

/* VOID:BOXED,BOXED (geda-marshal.list:19) */
extern void geda_marshal_VOID__BOXED_BOXED (GClosure     *closure,
                                            GValue       *return_value,
                                            unsigned int  n_param_values,
                                            const GValue *param_values,
                                            void         *invocation_hint,
                                            void         *marshal_data);

/* VOID:BOXED,POINTER (geda-marshal.list:20) */
extern void geda_marshal_VOID__BOXED_POINTER (GClosure     *closure,
                                              GValue       *return_value,
                                              unsigned int  n_param_values,
                                              const GValue *param_values,
                                              void         *invocation_hint,
                                              void         *marshal_data);

/* VOID:DOUBLE,DOUBLE,DOUBLE,DOUBLE (geda-marshal.list:21) */
extern void
geda_marshal_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE (GClosure     *closure,
                                                GValue       *return_value,
                                                unsigned int  n_param_values,
                                                const GValue *param_values,
                                                void         *invocation_hint,
                                                void         *marshal_data);

/* VOID:ENUM (geda-marshal.list:22) */
extern void geda_marshal_VOID__ENUM (GClosure     *closure,
                                     GValue       *return_value GEDA_UNUSED,
                                     unsigned int  n_param_values,
                                     const GValue *param_values,
                                     void         *invocation_hint GEDA_UNUSED,
                                     void         *marshal_data);

/* VOID:INT (geda-marshal.list:23) */
extern void geda_marshal_VOID__INT (GClosure     *closure,
                                    GValue       *return_value GEDA_UNUSED,
                                    unsigned int  n_param_values,
                                    const GValue *param_values,
                                    void         *invocation_hint GEDA_UNUSED,
                                    void         *marshal_data);


/* VOID:INT,INT (geda-marshal.list:24) */
extern void geda_marshal_VOID__INT_INT (GClosure     *closure,
                                        GValue       *return_value,
                                        unsigned int  n_param_values,
                                        const GValue *param_values,
                                        void         *invocation_hint,
                                        void         *marshal_data);

/* VOID:INT,POINTER (geda-marshal.list:25) */
extern void geda_marshal_VOID__INT_POINTER (GClosure     *closure,
                                            GValue       *return_value,
                                            unsigned int  n_param_values,
                                            const GValue *param_values,
                                            void         *invocation_hint,
                                            void         *marshal_data);

/* VOID:INT,STRING (geda-marshal.list:26) */
extern void geda_marshal_VOID__INT_STRING (GClosure     *closure,
                                           GValue       *return_value,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint,
                                           void         *marshal_data);

/* VOID:OBJECT (geda-marshal.list:27) */
extern void geda_marshal_VOID__OBJECT (GClosure     *closure,
                                       GValue       *return_value GEDA_UNUSED,
                                       unsigned int  n_param_values,
                                       const GValue *param_values,
                                       void         *invocation_hint GEDA_UNUSED,
                                       void         *marshal_data);

/* VOID:OBJECT,ENUM,BOXED (geda-marshal.list:28) */
extern void geda_marshal_VOID__OBJECT_ENUM_BOXED (GClosure     *closure,
                                                  GValue       *return_value,
                                                  unsigned int  n_param_values,
                                                  const GValue *param_values,
                                                  void         *invocation_hint,
                                                  void         *marshal_data);

/* VOID:OBJECT,INT (geda-marshal.list:29) */
extern void geda_marshal_VOID__OBJECT_INT (GClosure     *closure,
                                           GValue       *return_value GEDA_UNUSED,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint GEDA_UNUSED,
                                           void         *marshal_data);

/* VOID:OBJECT,OBJECT (geda-marshal.list:30) */
extern void geda_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                              GValue       *return_value GEDA_UNUSED,
                                              unsigned int  n_param_values,
                                              const GValue *param_values,
                                              void         *invocation_hint GEDA_UNUSED,
                                              void         *marshal_data);

/* VOID:OBJECT,OBJECT,OBJECT (geda-marshal.list:31) */
extern void
geda_marshal_VOID__OBJECT_OBJECT_OBJECT (GClosure     *closure,
                                          GValue       *return_value GEDA_UNUSED,
                                          unsigned int  n_param_values,
                                          const GValue *param_values,
                                          void         *invocation_hint GEDA_UNUSED,
                                          void         *marshal_data);

/* VOID:OBJECT,OBJECT (geda-marshal.list:32) */
extern
void geda_marshal_VOID__OBJECT_POINTER_INT (GClosure     *closure,
                                            GValue       *return_value GEDA_UNUSED,
                                            unsigned int  n_param_values,
                                            const GValue *param_values,
                                            void         *invocation_hint GEDA_UNUSED,
                                            void         *marshal_data);

/* VOID:POINTER (geda-marshal.list:33) */
#define geda_marshal_VOID__POINTER  g_cclosure_marshal_VOID__POINTER

/* VOID:POINTER,POINTER (geda-marshal.list:34) */
extern void geda_marshal_VOID__POINTER_POINTER (GClosure     *closure,
                                                GValue       *return_value,
                                                unsigned int  n_param_values,
                                                const GValue *param_values,
                                                void         *invocation_hint,
                                                void         *marshal_data);

/* VOID:POINTER,STRING (geda-marshal.list:35) */
extern void geda_marshal_VOID__POINTER_STRING (GClosure     *closure,
                                               GValue       *return_value,
                                               unsigned int  n_param_values,
                                               const GValue *param_values,
                                               void         *invocation_hint,
                                               void         *marshal_data);

/* VOID:STRING (geda-marshal.list:36) */
#define geda_marshal_VOID__STRING g_cclosure_marshal_VOID__STRING

/* VOID:STRING,STRING (geda-marshal.list:37) */
extern void
geda_marshal_VOID__STRING_STRING (GClosure     *closure,
                                  GValue       *return_value GEDA_UNUSED,
                                  unsigned int  n_param_values,
                                  const GValue *param_values,
                                  void         *invocation_hint GEDA_UNUSED,
                                  void         *marshal_data);

/* VOID:UINT (geda-marshal.list:38) */
#define geda_marshal_VOID__UINT g_cclosure_marshal_VOID__UINT

/* VOID:UINT,UINT (geda-marshal.list:39) */
extern void
geda_marshal_VOID__UINT_UINT (GClosure     *closure,
                              GValue       *return_value,
                              unsigned int  n_param_values,
                              const GValue *param_values,
                              void         *invocation_hint,
                              void         *marshal_data);

/* VOID:VOID (geda-marshal.list:40) */
void geda_marshal_VOID__VOID (GClosure     *closure,
                              GValue       *return_value GEDA_UNUSED,
                              unsigned int  n_param_values,
                              const GValue *param_values,
                              void         *invocation_hint GEDA_UNUSED,
                              void         *marshal_data);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MARSHAL_H__ */
