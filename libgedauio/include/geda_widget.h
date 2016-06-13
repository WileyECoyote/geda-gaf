
#ifndef __GEDA_WIDGET_H__
#define __GEDA_WIDGET_H__

typedef struct {
  GQuark         path_quark;
  GtkAccelGroup *accel_group;
  GClosure      *closure;
} AccelPath;

typedef struct
{
  GObject      *object;
  unsigned int  key;
  unsigned int  modifiers;
   char        *signal;
} AccelGroupParserData;

void        geda_widget_buildable_finish_accelerator (GtkWidget *widget,
                                                      GtkWidget *toplevel,
                                                      void      *user_data);

const char *geda_widget_get_accel_path               (GtkWidget *widget,
                                                      bool      *locked);

void        geda_widget_set_pointer_position         (GtkWidget *widget,
                                                      int        x,
                                                      int        y);

#endif /* __GEDA_WIDGET_H__ */