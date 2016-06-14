

#ifdef __cplusplus
extern "C" {
#endif

GList *
geda_container_focus_sort (GtkContainer     *container,
                           GList            *children,
                           GtkDirectionType  direction,
                           GtkWidget        *old_focus);

#ifdef __cplusplus
}
#endif /* __cplusplus */