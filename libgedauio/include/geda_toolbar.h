
#ifndef __GEDA_TOOLBAR_H__
#define __GEDA_TOOLBAR_H__

#define GEDA_TYPE_TOOLBAR            (geda_toolbar_get_type ())
#define GEDA_TOOLBAR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_TOOLBAR, GedaToolbar))
#define GEDA_TOOLBAR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_TOOLBAR, GedaToolbarClass))
#define GEDA_IS_TOOLBAR(obj)         (is_a_geda_toolbar((GedaToolbar*)obj))
#define GEDA_IS_TOOLBAR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_TOOLBAR))
#define GEDA_TOOLBAR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_TOOLBAR, GedaToolbarClass))

typedef struct _GedaToolbar       GedaToolbar;
typedef struct _GedaToolbarClass  GedaToolbarClass;

struct _GedaToolbar
{
  GtkToolbar parent;
  GedaType   instance_type;
  int        orientation;
};

struct _GedaToolbarClass
{
  GtkToolbarClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_toolbar_get_type (void) GEDA_CONST;
bool       is_a_geda_toolbar     (GedaToolbar *toolbar);

GtkWidget *geda_toolbar_new      (int orientation);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_TOOLBAR_H__ */
