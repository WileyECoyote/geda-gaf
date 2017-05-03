

#ifdef __cplusplus
extern "C" {
#endif

GList *
geda_container_focus_sort (GtkContainer     *container,
                           GList            *children,
                           GtkDirectionType  direction,
                           GtkWidget        *old_focus);

#define geda_container_add(c, w) gtk_container_add((GtkContainer*)c, (GtkWidget*)w)

#ifdef __cplusplus
}
#endif /* __cplusplus */