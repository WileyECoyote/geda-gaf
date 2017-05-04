

#ifdef __cplusplus
extern "C" {
#endif

GList *
geda_container_focus_sort (GtkContainer     *container,
                           GList            *children,
                           GtkDirectionType  direction,
                           GtkWidget        *old_focus);

#define geda_container_add(c, w) gtk_container_add((GtkContainer*)c, (GtkWidget*)w)
#define geda_container_remove(c, w) gtk_container_remove((GtkContainer*)c, (GtkWidget*)w)

#define geda_container_get_children(c) gtk_container_get_children((GtkContainer*)c)

#define geda_set_container_border_width(c, w) gtk_container_set_border_width((GtkContainer*)c, w)
#define geda_container_set_focus_chain(c, fc) gtk_container_set_focus_chain ((GtkContainer*)c, fc);

#define geda_container_forall(c, f, d )gtk_container_forall ((GtkContainer*)c, (GtkCallback)f, (void*)d)
#define geda_container_foreach(c, f, d )gtk_container_forall ((GtkContainer*)c, (GtkCallback)f, (void*)d)

#ifdef __cplusplus
}
#endif /* __cplusplus */