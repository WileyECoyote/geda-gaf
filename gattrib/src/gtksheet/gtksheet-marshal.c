/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include    <glib-object.h>


#ifdef G_ENABLE_DEBUG
#define g_marshal_value_peek_boolean(v)  g_value_get_boolean (v)
#define g_marshal_value_peek_char(v)     g_value_get_schar (v)
#define g_marshal_value_peek_uchar(v)    g_value_get_uchar (v)
#define g_marshal_value_peek_int(v)      g_value_get_int (v)
#define g_marshal_value_peek_uint(v)     g_value_get_uint (v)
#define g_marshal_value_peek_long(v)     g_value_get_long (v)
#define g_marshal_value_peek_ulong(v)    g_value_get_ulong (v)
#define g_marshal_value_peek_int64(v)    g_value_get_int64 (v)
#define g_marshal_value_peek_uint64(v)   g_value_get_uint64 (v)
#define g_marshal_value_peek_enum(v)     g_value_get_enum (v)
#define g_marshal_value_peek_flags(v)    g_value_get_flags (v)
#define g_marshal_value_peek_float(v)    g_value_get_float (v)
#define g_marshal_value_peek_double(v)   g_value_get_double (v)
#define g_marshal_value_peek_string(v)   (char*) g_value_get_string (v)
#define g_marshal_value_peek_param(v)    g_value_get_param (v)
#define g_marshal_value_peek_boxed(v)    g_value_get_boxed (v)
#define g_marshal_value_peek_pointer(v)  g_value_get_pointer (v)
#define g_marshal_value_peek_object(v)   g_value_get_object (v)
#define g_marshal_value_peek_variant(v)  g_value_get_variant (v)
#else /* !G_ENABLE_DEBUG */
/* WARNING: This code accesses GValues directly, which is UNSUPPORTED API.
 *          Do not access GValues directly in your code. Instead, use the
 *          g_value_get_*() functions
 */
#define g_marshal_value_peek_boolean(v)  (v)->data[0].v_int
#define g_marshal_value_peek_char(v)     (v)->data[0].v_int
#define g_marshal_value_peek_uchar(v)    (v)->data[0].v_uint
#define g_marshal_value_peek_int(v)      (v)->data[0].v_int
#define g_marshal_value_peek_uint(v)     (v)->data[0].v_uint
#define g_marshal_value_peek_long(v)     (v)->data[0].v_long
#define g_marshal_value_peek_ulong(v)    (v)->data[0].v_ulong
#define g_marshal_value_peek_int64(v)    (v)->data[0].v_int64
#define g_marshal_value_peek_uint64(v)   (v)->data[0].v_uint64
#define g_marshal_value_peek_enum(v)     (v)->data[0].v_long
#define g_marshal_value_peek_flags(v)    (v)->data[0].v_ulong
#define g_marshal_value_peek_float(v)    (v)->data[0].v_float
#define g_marshal_value_peek_double(v)   (v)->data[0].v_double
#define g_marshal_value_peek_string(v)   (v)->data[0].v_pointer
#define g_marshal_value_peek_param(v)    (v)->data[0].v_pointer
#define g_marshal_value_peek_boxed(v)    (v)->data[0].v_pointer
#define g_marshal_value_peek_pointer(v)  (v)->data[0].v_pointer
#define g_marshal_value_peek_object(v)   (v)->data[0].v_pointer
#define g_marshal_value_peek_variant(v)  (v)->data[0].v_pointer
#endif /* !G_ENABLE_DEBUG */


/* BOOL:INT,INT,POINTER,POINTER (gtkextra-marshal.list:1) */
void
gtksheet_BOOLEAN__INT_INT_POINTER_POINTER (GClosure     *closure,
                                           GValue       *return_value G_GNUC_UNUSED,
                                           unsigned int  n_param_values,
                                           const GValue *param_values,
                                           void         *invocation_hint G_GNUC_UNUSED,
                                           void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__INT_INT_POINTER_POINTER) (void *data1,
                                                                 int  arg_1,
                                                                 int  arg_2,
                                                                 void *arg_3,
                                                                 void *arg_4,
                                                                 void *data2);
  register GMarshalFunc_BOOLEAN__INT_INT_POINTER_POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 5);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__INT_INT_POINTER_POINTER) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_int (param_values + 1),
                       g_marshal_value_peek_int (param_values + 2),
                       g_marshal_value_peek_pointer (param_values + 3),
                       g_marshal_value_peek_pointer (param_values + 4),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:BOXED,POINTER (gtkextra-marshal.list:2) */
void
gtksheet_BOOLEAN__BOXED_POINTER (GClosure     *closure,
                                 GValue       *return_value G_GNUC_UNUSED,
                                 unsigned int  n_param_values,
                                 const GValue *param_values,
                                 void         *invocation_hint G_GNUC_UNUSED,
                                 void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__BOXED_POINTER) (void *data1,
                                                       void *arg_1,
                                                       void *arg_2,
                                                       void *data2);
  register GMarshalFunc_BOOLEAN__BOXED_POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
  }

  callback = (GMarshalFunc_BOOLEAN__BOXED_POINTER) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_boxed (param_values + 1),
                       g_marshal_value_peek_pointer (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:BOXED,STRING (gtkextra-marshal.list:3) */
void
gtksheet_BOOLEAN__BOXED_STRING (GClosure     *closure,
                                GValue       *return_value G_GNUC_UNUSED,
                                unsigned int  n_param_values,
                                const GValue *param_values,
                                void         *invocation_hint G_GNUC_UNUSED,
                                void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__BOXED_STRING) (void *data1,
                                                      void *arg_1,
                                                      void *arg_2,
                                                      void *data2);
  register GMarshalFunc_BOOLEAN__BOXED_STRING callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__BOXED_STRING) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_boxed (param_values + 1),
                       g_marshal_value_peek_string (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:BOXED,BOXED (gtkextra-marshal.list:4) */
void
gtksheet_BOOLEAN__BOXED_BOXED (GClosure     *closure,
                               GValue       *return_value G_GNUC_UNUSED,
                               unsigned int  n_param_values,
                               const GValue *param_values,
                               void         *invocation_hint G_GNUC_UNUSED,
                               void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__BOXED_BOXED) (void *data1,
                                                         void *arg_1,
                                                         void *arg_2,
                                                         void *data2);
  register GMarshalFunc_BOOLEAN__BOXED_BOXED callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__BOXED_BOXED) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_boxed (param_values + 1),
                       g_marshal_value_peek_boxed (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:BOXED,DOUBLE,DOUBLE (gtkextra-marshal.list:5) */
void
gtksheet_BOOLEAN__BOXED_DOUBLE_DOUBLE (GClosure     *closure,
                                       GValue       *return_value G_GNUC_UNUSED,
                                       unsigned int  n_param_values,
                                       const GValue *param_values,
                                       void         *invocation_hint G_GNUC_UNUSED,
                                       void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__BOXED_DOUBLE_DOUBLE) (void *data1,
                                                             void *arg_1,
                                                             double      arg_2,
                                                             double      arg_3,
                                                             void *data2);
  register GMarshalFunc_BOOLEAN__BOXED_DOUBLE_DOUBLE callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 4);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__BOXED_DOUBLE_DOUBLE) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_boxed (param_values + 1),
                       g_marshal_value_peek_double (param_values + 2),
                       g_marshal_value_peek_double (param_values + 3),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:POINTER,POINTER (gtkextra-marshal.list:6) */
void
gtksheet_BOOLEAN__POINTER_POINTER (GClosure     *closure,
                                   GValue       *return_value G_GNUC_UNUSED,
                                   unsigned int  n_param_values,
                                   const GValue *param_values,
                                   void         *invocation_hint G_GNUC_UNUSED,
                                   void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__POINTER_POINTER) (void *data1,
                                                         void *arg_1,
                                                         void *arg_2,
                                                         void *data2);
  register GMarshalFunc_BOOLEAN__POINTER_POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__POINTER_POINTER) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_pointer (param_values + 1),
                       g_marshal_value_peek_pointer (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:POINTER,BOXED (gtkextra-marshal.list:7) */
void
gtksheet_BOOLEAN__POINTER_BOXED (GClosure     *closure,
                                 GValue       *return_value G_GNUC_UNUSED,
                                 unsigned int  n_param_values,
                                 const GValue *param_values,
                                 void         *invocation_hint G_GNUC_UNUSED,
                                 void         *marshal_data)
{
  typedef int (*GMarshalFunc_BOOLEAN__POINTER_BOXED) (void *data1,
                                                       void *arg_1,
                                                       void *arg_2,
                                                       void *data2);
  register GMarshalFunc_BOOLEAN__POINTER_BOXED callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__POINTER_BOXED) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_pointer (param_values + 1),
                       g_marshal_value_peek_boxed (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:POINTER,STRING (gtkextra-marshal.list:8) */
void
gtksheet_BOOLEAN__POINTER_STRING (GClosure     *closure,
                                  GValue       *return_value G_GNUC_UNUSED,
                                  unsigned int  n_param_values,
                                  const GValue *param_values,
                                  void         *invocation_hint G_GNUC_UNUSED,
                                  void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__POINTER_STRING) (void *data1,
                                                        void *arg_1,
                                                        void *arg_2,
                                                        void *data2);
  register GMarshalFunc_BOOLEAN__POINTER_STRING callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__POINTER_STRING) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_pointer (param_values + 1),
                       g_marshal_value_peek_string (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:POINTER (gtkextra-marshal.list:9) */
void
gtksheet_BOOLEAN__POINTER (GClosure     *closure,
                           GValue       *return_value G_GNUC_UNUSED,
                           unsigned int  n_param_values,
                           const GValue *param_values,
                           void         *invocation_hint G_GNUC_UNUSED,
                           void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__POINTER) (void *data1,
                                                     void *arg_1,
                                                     void *data2);
  register GMarshalFunc_BOOLEAN__POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 2);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__POINTER) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_pointer (param_values + 1),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:BOXED (gtkextra-marshal.list:10) */
void
gtksheet_BOOLEAN__BOXED (GClosure     *closure,
                         GValue       *return_value G_GNUC_UNUSED,
                         unsigned int  n_param_values,
                         const GValue *param_values,
                         void         *invocation_hint G_GNUC_UNUSED,
                         void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__BOXED) (void *data1,
                                                   void *arg_1,
                                                   void *data2);
  register GMarshalFunc_BOOLEAN__BOXED callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 2);

  if (G_CCLOSURE_SWAP_DATA (closure)) {

      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__BOXED) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_boxed (param_values + 1),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* BOOL:INT,INT (gtkextra-marshal.list:11) */
void
gtksheet_BOOLEAN__INT_INT (GClosure     *closure,
                           GValue       *return_value G_GNUC_UNUSED,
                           unsigned int  n_param_values,
                           const GValue *param_values,
                           void         *invocation_hint G_GNUC_UNUSED,
                           void         *marshal_data)
{
  typedef int  (*GMarshalFunc_BOOLEAN__INT_INT) (void *data1,
                                                     int  arg_1,
                                                     int  arg_2,
                                                     void *data2);
  register GMarshalFunc_BOOLEAN__INT_INT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;
  int  v_return;

  g_return_if_fail (return_value != NULL);
  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {

      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_BOOLEAN__INT_INT) (marshal_data ? marshal_data : cc->callback);

  v_return = callback (data1,
                       g_marshal_value_peek_int (param_values + 1),
                       g_marshal_value_peek_int (param_values + 2),
                       data2);

  g_value_set_boolean (return_value, v_return);
}

/* VOID:INT (gtkextra-marshal.list:12) */

/* VOID:INT,STRING (gtkextra-marshal.list:13) */
void
gtksheet_VOID__INT_STRING (GClosure     *closure,
                           GValue       *return_value G_GNUC_UNUSED,
                           unsigned int  n_param_values,
                           const GValue *param_values,
                           void         *invocation_hint G_GNUC_UNUSED,
                           void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__INT_STRING) (void *data1,
                                                 int  arg_1,
                                                 void *arg_2,
                                                 void *data2);
  register GMarshalFunc_VOID__INT_STRING callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__INT_STRING) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_int (param_values + 1),
            g_marshal_value_peek_string (param_values + 2),
            data2);
}

/* VOID:BOXED (gtkextra-marshal.list:14) */

/* VOID:VOID (gtkextra-marshal.list:15) */

/* VOID:BOOL (gtkextra-marshal.list:16) */

/* VOID:POINTER (gtkextra-marshal.list:17) */

/* VOID:INT,BOXED (gtkextra-marshal.list:18) */
void
gtksheet_VOID__INT_BOXED (GClosure     *closure,
                          GValue       *return_value G_GNUC_UNUSED,
                          unsigned int  n_param_values,
                          const GValue *param_values,
                          void         *invocation_hint G_GNUC_UNUSED,
                          void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__INT_BOXED) (void *data1,
                                                int  arg_1,
                                                void *arg_2,
                                                void *data2);
  register GMarshalFunc_VOID__INT_BOXED callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__INT_BOXED) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_int (param_values + 1),
            g_marshal_value_peek_boxed (param_values + 2),
            data2);
}

/* VOID:INT,INT (gtkextra-marshal.list:19) */
void
gtksheet_VOID__INT_INT (GClosure     *closure,
                        GValue       *return_value G_GNUC_UNUSED,
                        unsigned int  n_param_values,
                        const GValue *param_values,
                        void         *invocation_hint G_GNUC_UNUSED,
                        void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__INT_INT) (void *data1,
                                              int  arg_1,
                                              int  arg_2,
                                              void *data2);
  register GMarshalFunc_VOID__INT_INT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__INT_INT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_int (param_values + 1),
            g_marshal_value_peek_int (param_values + 2),
            data2);
}

/* VOID:INT,POINTER (gtkextra-marshal.list:20) */
void
gtksheet_VOID__INT_POINTER (GClosure     *closure,
                            GValue       *return_value G_GNUC_UNUSED,
                            unsigned int  n_param_values,
                            const GValue *param_values,
                            void         *invocation_hint G_GNUC_UNUSED,
                            void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__INT_POINTER) (void *data1,
                                                  int   arg_1,
                                                  void *arg_2,
                                                  void *data2);
  register GMarshalFunc_VOID__INT_POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__INT_POINTER) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_int (param_values + 1),
            g_marshal_value_peek_pointer (param_values + 2),
            data2);
}

/* VOID:POINTER,POINTER (gtkextra-marshal.list:21) */
void
gtksheet_VOID__POINTER_POINTER (GClosure     *closure,
                                GValue       *return_value G_GNUC_UNUSED,
                                unsigned int  n_param_values,
                                const GValue *param_values,
                                void         *invocation_hint G_GNUC_UNUSED,
                                void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__POINTER_POINTER) (void *data1,
                                                      void *arg_1,
                                                      void *arg_2,
                                                      void *data2);
  register GMarshalFunc_VOID__POINTER_POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {

      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
  }
  callback = (GMarshalFunc_VOID__POINTER_POINTER) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_pointer (param_values + 1),
            g_marshal_value_peek_pointer (param_values + 2),
            data2);
}

/* VOID:BOXED,POINTER (gtkextra-marshal.list:22) */
void
gtksheet_VOID__BOXED_POINTER (GClosure     *closure,
                              GValue       *return_value G_GNUC_UNUSED,
                              unsigned int  n_param_values,
                              const GValue *param_values,
                              void         *invocation_hint G_GNUC_UNUSED,
                              void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__BOXED_POINTER) (void *data1,
                                                    void *arg_1,
                                                    void *arg_2,
                                                    void *data2);
  register GMarshalFunc_VOID__BOXED_POINTER callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__BOXED_POINTER) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_boxed (param_values + 1),
            g_marshal_value_peek_pointer (param_values + 2),
            data2);
}

/* VOID:BOXED,BOXED (gtkextra-marshal.list:23) */
void
gtksheet_VOID__BOXED_BOXED (GClosure     *closure,
                            GValue       *return_value G_GNUC_UNUSED,
                            unsigned int  n_param_values,
                            const GValue *param_values,
                            void         *invocation_hint G_GNUC_UNUSED,
                            void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__BOXED_BOXED) (void *data1,
                                                  void *arg_1,
                                                  void *arg_2,
                                                  void *data2);
  register GMarshalFunc_VOID__BOXED_BOXED callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__BOXED_BOXED) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_boxed (param_values + 1),
            g_marshal_value_peek_boxed (param_values + 2),
            data2);
}

/* VOID:OBJECT (gtkextra-marshal.list:24) */

/* VOID:OBJECT,OBJECT (gtkextra-marshal.list:25) */
void
gtksheet_VOID__OBJECT_OBJECT (GClosure     *closure,
                              GValue       *return_value G_GNUC_UNUSED,
                              unsigned int  n_param_values,
                              const GValue *param_values,
                              void         *invocation_hint G_GNUC_UNUSED,
                              void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__OBJECT_OBJECT) (void *data1,
                                                    void *arg_1,
                                                    void *arg_2,
                                                    void *data2);
  register GMarshalFunc_VOID__OBJECT_OBJECT callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__OBJECT_OBJECT) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_object (param_values + 1),
            g_marshal_value_peek_object (param_values + 2),
            data2);
}

/* VOID:DOUBLE,DOUBLE,DOUBLE,DOUBLE (gtkextra-marshal.list:26) */
void
gtksheet_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE (GClosure     *closure,
                                            GValue       *return_value G_GNUC_UNUSED,
                                            unsigned int  n_param_values,
                                            const GValue *param_values,
                                            void         *invocation_hint G_GNUC_UNUSED,
                                            void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE) (void *data1,
                                                              double    arg_1,
                                                              double    arg_2,
                                                              double    arg_3,
                                                              double    arg_4,
                                                              void     *data2);
  register GMarshalFunc_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 5);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {

      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
  }

  callback = (GMarshalFunc_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_double (param_values + 1),
            g_marshal_value_peek_double (param_values + 2),
            g_marshal_value_peek_double (param_values + 3),
            g_marshal_value_peek_double (param_values + 4),
            data2);
}

/* VOID:ENUM,INT,BOOLEAN (gtkextra-marshal.list:27) */
void
gtksheet_VOID__ENUM_INT_BOOLEAN (GClosure     *closure,
                                 GValue       *return_value G_GNUC_UNUSED,
                                 unsigned int  n_param_values,
                                 const GValue *param_values,
                                 void         *invocation_hint G_GNUC_UNUSED,
                                 void         *marshal_data)
{
  typedef void (*GMarshalFunc_VOID__ENUM_INT_BOOLEAN) (void *data1,
                                                       int   arg_1,
                                                       int   arg_2,
                                                       int   arg_3,
                                                       void *data2);
  register GMarshalFunc_VOID__ENUM_INT_BOOLEAN callback;
  register GCClosure *cc = (GCClosure*) closure;
  register void *data1;
  register void *data2;

  g_return_if_fail (n_param_values == 4);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
      data1 = closure->data;
      data2 = g_value_peek_pointer (param_values + 0);
  }
  else {
      data1 = g_value_peek_pointer (param_values + 0);
      data2 = closure->data;
    }
  callback = (GMarshalFunc_VOID__ENUM_INT_BOOLEAN) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_marshal_value_peek_enum (param_values + 1),
            g_marshal_value_peek_int (param_values + 2),
            g_marshal_value_peek_boolean (param_values + 3),
            data2);
}
