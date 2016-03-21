/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */

#ifndef __GEDA_MARSHAL_H__
#define __GEDA_MARSHAL_H__

#include	<glib-object.h>

#ifdef __cplusplus
extern "C" {
#endif

/* BOOL:INT,INT,POINTER,POINTER (geda-marshal.list:1) */
extern void geda_marshal_BOOLEAN__INT_INT_POINTER_POINTER (GClosure     *closure,
                                                           GValue       *return_value,
                                                           unsigned int  n_param_values,
                                                           const GValue *param_values,
                                                           void         *invocation_hint,
                                                           void         *marshal_data);
#define geda_marshal_BOOL__INT_INT_POINTER_POINTER	geda_marshal_BOOLEAN__INT_INT_POINTER_POINTER

/* BOOL:BOXED,POINTER (geda-marshal.list:2) */
extern void geda_marshal_BOOLEAN__BOXED_POINTER (GClosure     *closure,
                                                 GValue       *return_value,
                                                 unsigned int  n_param_values,
                                                 const GValue *param_values,
                                                 void         *invocation_hint,
                                                 void         *marshal_data);
#define geda_marshal_BOOL__BOXED_POINTER	geda_marshal_BOOLEAN__BOXED_POINTER

/* BOOL:BOXED,STRING (geda-marshal.list:3) */
extern void geda_marshal_BOOLEAN__BOXED_STRING (GClosure     *closure,
                                                GValue       *return_value,
                                                unsigned int  n_param_values,
                                                const GValue *param_values,
                                                void         *invocation_hint,
                                                void         *marshal_data);
#define geda_marshal_BOOL__BOXED_STRING	geda_marshal_BOOLEAN__BOXED_STRING

/* BOOL:BOXED,BOXED (geda-marshal.list:4) */
extern void geda_marshal_BOOLEAN__BOXED_BOXED (GClosure     *closure,
                                               GValue       *return_value,
                                               unsigned int  n_param_values,
                                               const GValue *param_values,
                                               void         *invocation_hint,
                                               void         *marshal_data);
#define geda_marshal_BOOL__BOXED_BOXED	geda_marshal_BOOLEAN__BOXED_BOXED

/* BOOL:BOXED,DOUBLE,DOUBLE (geda-marshal.list:5) */
extern void geda_marshal_BOOLEAN__BOXED_DOUBLE_DOUBLE (GClosure     *closure,
                                                       GValue       *return_value,
                                                       unsigned int  n_param_values,
                                                       const GValue *param_values,
                                                       void         *invocation_hint,
                                                       void         *marshal_data);
#define geda_marshal_BOOL__BOXED_DOUBLE_DOUBLE	geda_marshal_BOOLEAN__BOXED_DOUBLE_DOUBLE

/* BOOL:POINTER,POINTER (geda-marshal.list:6) */
extern void geda_marshal_BOOLEAN__POINTER_POINTER (GClosure     *closure,
                                                   GValue       *return_value,
                                                   unsigned int  n_param_values,
                                                   const GValue *param_values,
                                                   void         *invocation_hint,
                                                   void         *marshal_data);
#define geda_marshal_BOOL__POINTER_POINTER	geda_marshal_BOOLEAN__POINTER_POINTER

/* BOOL:POINTER,BOXED (geda-marshal.list:7) */
extern void geda_marshal_BOOLEAN__POINTER_BOXED (GClosure     *closure,
                                                 GValue       *return_value,
                                                 unsigned int  n_param_values,
                                                 const GValue *param_values,
                                                 void         *invocation_hint,
                                                 void         *marshal_data);
#define geda_marshal_BOOL__POINTER_BOXED	geda_marshal_BOOLEAN__POINTER_BOXED

/* BOOL:POINTER,STRING (geda-marshal.list:8) */
extern void geda_marshal_BOOLEAN__POINTER_STRING (GClosure     *closure,
                                                  GValue       *return_value,
                                                  unsigned int  n_param_values,
                                                  const GValue *param_values,
                                                  void         *invocation_hint,
                                                  void         *marshal_data);
#define geda_marshal_BOOL__POINTER_STRING	geda_marshal_BOOLEAN__POINTER_STRING

/* BOOL:POINTER (geda-marshal.list:9) */
extern void geda_marshal_BOOLEAN__POINTER (GClosure     *closure,
                                           GValue       *return_value,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint,
                                           void         *marshal_data);
#define geda_marshal_BOOL__POINTER	geda_marshal_BOOLEAN__POINTER

/* BOOL:BOXED (geda-marshal.list:10) */
extern void geda_marshal_BOOLEAN__BOXED (GClosure     *closure,
                                         GValue       *return_value,
                                         unsigned int  n_param_values,
                                         const GValue *param_values,
                                         void         *invocation_hint,
                                         void         *marshal_data);
#define geda_marshal_BOOL__BOXED        geda_marshal_BOOLEAN__BOXED

/* BOOL:INT (geda-marshal.list:11) */
extern void geda_marshal_BOOLEAN__INT (GClosure     *closure,
                                       GValue       *return_value GEDA_UNUSED,
                                       unsigned int  n_param_values,
                                       const GValue *param_values,
                                       void         *invocation_hint GEDA_UNUSED,
                                       void         *marshal_data);
#define geda_marshal_BOOL__INT  geda_marshal_BOOLEAN__INT

/* BOOL:INT,INT (geda-marshal.list:12) */
extern void geda_marshal_BOOLEAN__INT_INT (GClosure     *closure,
                                           GValue       *return_value,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint,
                                           void         *marshal_data);
#define geda_marshal_BOOL__INT_INT geda_marshal_BOOLEAN__INT_INT

/* STRING:STRING (geda-marshal.list:13) */
extern void geda_marshal_STRING__STRING (GClosure     *closure,
                                         GValue       *return_value,
                                         unsigned int  n_param_values,
                                         const GValue *param_values,
                                         void         *invocation_hint,
                                         void         *marshal_data);

/* VOID:INT (geda-marshal.list:14) */
#define geda_marshal_VOID__INT	g_cclosure_marshal_VOID__INT

/* VOID:INT,STRING (geda-marshal.list:15) */
extern void geda_marshal_VOID__INT_STRING (GClosure     *closure,
                                           GValue       *return_value,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint,
                                           void         *marshal_data);

/* VOID:BOXED (geda-marshal.list:16) */
#define geda_marshal_VOID__BOXED	g_cclosure_marshal_VOID__BOXED

/* VOID:VOID (geda-marshal.list:17) */
#define geda_marshal_VOID__VOID	g_cclosure_marshal_VOID__VOID

/* VOID:BOOL (geda-marshal.list:18) */
#define geda_marshal_VOID__BOOLEAN	g_cclosure_marshal_VOID__BOOLEAN
#define geda_marshal_VOID__BOOL	geda_marshal_VOID__BOOLEAN

/* VOID:POINTER (geda-marshal.list:19) */
#define geda_marshal_VOID__POINTER	g_cclosure_marshal_VOID__POINTER

/* VOID:INT,INT (geda-marshal.list:20) */
extern void geda_marshal_VOID__INT_INT (GClosure     *closure,
                                        GValue       *return_value,
                                        unsigned int  n_param_values,
                                        const GValue *param_values,
                                        void         *invocation_hint,
                                        void         *marshal_data);

/* VOID:INT,POINTER (geda-marshal.list:21) */
extern void geda_marshal_VOID__INT_POINTER (GClosure     *closure,
                                            GValue       *return_value,
                                            unsigned int  n_param_values,
                                            const GValue *param_values,
                                            void         *invocation_hint,
                                            void         *marshal_data);

/* VOID:POINTER,POINTER (geda-marshal.list:22) */
extern void geda_marshal_VOID__POINTER_POINTER (GClosure     *closure,
                                                GValue       *return_value,
                                                unsigned int  n_param_values,
                                                const GValue *param_values,
                                                void         *invocation_hint,
                                                void         *marshal_data);

/* VOID:POINTER,STRING (geda-marshal.list:23) */
void geda_marshal_VOID__POINTER_STRING (GClosure     *closure,
                                        GValue       *return_value,
                                        unsigned int  n_param_values,
                                        const GValue *param_values,
                                        void         *invocation_hint,
                                        void         *marshal_data);


/* VOID:BOXED,POINTER (geda-marshal.list:24) */
extern void geda_marshal_VOID__BOXED_POINTER (GClosure     *closure,
                                              GValue       *return_value,
                                              unsigned int  n_param_values,
                                              const GValue *param_values,
                                              void         *invocation_hint,
                                              void         *marshal_data);

/* VOID:BOXED,BOXED (geda-marshal.list:25) */
extern void geda_marshal_VOID__BOXED_BOXED (GClosure     *closure,
                                            GValue       *return_value,
                                            unsigned int  n_param_values,
                                            const GValue *param_values,
                                            void         *invocation_hint,
                                            void         *marshal_data);


/* VOID:OBJECT,INT (geda-marshal.list:26) */
extern void geda_marshal_VOID__OBJECT_INT (GClosure     *closure,
                                           GValue       *return_value GEDA_UNUSED,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint GEDA_UNUSED,
                                           void         *marshal_data);

/* VOID:OBJECT,OBJECT (geda-marshal.list:27) */
extern void geda_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                              GValue       *return_value GEDA_UNUSED,
                                              unsigned int  n_param_values,
                                              const GValue *param_values,
                                              void         *invocation_hint GEDA_UNUSED,
                                              void         *marshal_data);

/* VOID:DOUBLE,DOUBLE,DOUBLE,DOUBLE (geda-marshal.list:28) */
extern void geda_marshal_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE (GClosure     *closure,
                                                            GValue       *return_value,
                                                            unsigned int  n_param_values,
                                                            const GValue *param_values,
                                                            void         *invocation_hint,
                                                            void         *marshal_data);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MARSHAL_H__ */

