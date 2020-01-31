
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

void         geda_widget_buildable_finish_accelerator (GtkWidget *widget,
                                                       GtkWidget *toplevel,
                                                       void      *user_data);

const char  *geda_widget_get_accel_path               (GtkWidget *widget,
                                                       bool      *locked);

unsigned int geda_widget_get_allocated_width          (GtkWidget *widget);

GtkWidgetAuxInfo
            *geda_widget_get_aux_info                 (GtkWidget      *widget,
                                                       bool            create);

void         geda_widget_hide_all                     (GtkWidget      *widget);

void         geda_widget_modify_bg                    (GtkWidget      *widget,
                                                       GtkStateType    state,
                                                       const GdkColor *color);

void         geda_widget_modify_color                 (GtkWidget      *widget,
                                                       GtkRcFlags      component,
                                                       GtkStateType    state,
                                                       const GdkColor *color);

void         geda_widget_modify_color_component       (GtkWidget      *widget,
                                                       GtkRcFlags      component,
                                                       GtkStateType    state,
                                                       const GdkColor *color);

void         geda_widget_modify_fg                    (GtkWidget      *widget,
                                                       GtkStateType    state,
                                                       const GdkColor *color);

void         geda_widget_modify_insensitive_bg        (GtkWidget      *widget,
                                                       const GdkColor *color);

void         geda_widget_modify_insensitive_fg        (GtkWidget      *widget,
                                                       const GdkColor *color);

void         geda_widget_modify_normal_bg             (GtkWidget      *widget,
                                                       const GdkColor *color);

void         geda_widget_modify_normal_fg             (GtkWidget      *widget,
                                                       const GdkColor *color);

void         geda_widget_set_item_active              (GtkWidget      *widget,
                                                       bool            state);

void         geda_widget_set_pointer_position         (GtkWidget      *widget,
                                                       int             x,
                                                       int             y);
#endif /* __GEDA_WIDGET_H__ */