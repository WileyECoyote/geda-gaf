

#ifndef __GEDA_SEPARATOR_H__
#define __GEDA_SEPARATOR_H__

#define GEDA_TYPE_SEPARATOR            (geda_separator_get_type ())
#define GEDA_SEPARATOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_SEPARATOR, GedaSeparator))
#define GEDA_SEPARATOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_SEPARATOR, GedaSeparatorClass))
#define GEDA_IS_SEPARATOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_SEPARATOR))
#define GEDA_IS_SEPARATOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_SEPARATOR))
#define GEDA_SEPARATOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_SEPARATOR, GedaSeparatorClass))

typedef struct _GedaSeparator       GedaSeparator;
typedef struct _GedaSeparatorClass  GedaSeparatorClass;

struct _GedaSeparator
{
  GtkWidget widget;
  int       orientation;
};

struct _GedaSeparatorClass
{
  GtkWidgetClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_separator_get_type (void) G_GNUC_CONST;

GtkWidget *geda_separator_new      (int orientation);
GtkWidget *geda_hseparator_new     (void);
GtkWidget *geda_vseparator_new     (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_SEPARATOR_H__ */
