/* g_netlist.c */
void g_set_project_current(GedaToplevel *pr_current);
SCM g_get_verbosity (void);
SCM g_get_version (void);
SCM g_get_backend_arguments (void);
SCM g_get_input_files(void);
SCM g_get_packages(SCM level);
SCM g_get_non_unique_packages(SCM level);
SCM g_get_pins(SCM uref);
SCM g_get_all_nets(SCM scm_level);
SCM g_get_all_unique_nets(SCM scm_level);
SCM g_get_all_connections(SCM scm_netname);
SCM g_get_nets(SCM scm_uref, SCM scm_pin);
SCM g_get_pins_nets(SCM scm_uref);
SCM g_get_all_package_attributes(SCM scm_uref, SCM scm_wanted_attrib);
SCM g_get_attribute_by_pinseq(SCM scm_uref, SCM scm_pinseq, SCM scm_wanted_attrib);
SCM g_get_attribute_by_pinnumber(SCM scm_uref, SCM scm_pin, SCM scm_wanted_attrib);
SCM g_get_toplevel_attribute(SCM scm_wanted_attrib);
SCM g_graphical_objs_in_net_with_attrib_get_attrib(SCM scm_netname, SCM scm_has_attribute, SCM scm_wanted_attribute);

/* g_rc.c */
SCM g_rc_gnetlist_version(SCM version);
SCM g_rc_net_naming_priority(SCM mode);
SCM g_rc_hierarchy_traversal(SCM mode);
SCM g_rc_hierarchy_uref_mangle(SCM mode);
SCM g_rc_hierarchy_netname_mangle(SCM mode);
SCM g_rc_hierarchy_netattrib_mangle(SCM mode);
SCM g_rc_hierarchy_netname_separator(SCM name);
SCM g_rc_hierarchy_netattrib_separator(SCM name);
SCM g_rc_hierarchy_uref_separator(SCM name);
SCM g_rc_hierarchy_netattrib_order(SCM mode);
SCM g_rc_hierarchy_netname_order(SCM mode);
SCM g_rc_hierarchy_uref_order(SCM mode);
SCM g_rc_unnamed_netname(SCM name);
SCM g_rc_unnamed_busname(SCM name);

/* g_register.c */
void g_register_funcs(void);
SCM  g_quit(void);

/* globals.c */
/* gnetlist.c */
void gnetlist_quit(void);
int  main(int argc, char *argv[]);

/* i_vars.c */
void i_vars_set(GedaToplevel *pr_current);
void i_vars_init_gnetlist_defaults (void);
void i_vars_free_strings (void);
/* parsecmd.c */
void    usage(char *cmd);
int     parse_commandline(int argc, char *argv[], char **output_filename);

/* s_cpinlist.c */
CPINLIST *s_cpinlist_add(CPINLIST *ptr);
void      s_cpinlist_destroy_or_report(CPINLIST *pinlist, GedaList *list);
void      s_cpinlist_print(CPINLIST *ptr);
CPINLIST *s_cpinlist_return_head(CPINLIST *tail);
CPINLIST *s_cpinlist_return_tail(CPINLIST *head);
CPINLIST *s_cpinlist_search_pin(CPINLIST *ptr, char *pin_number);

/* s_hierarchy.c */
void   s_hierarchy_traverse(GedaToplevel *pr_current,GedaObject *o_current, NETLIST *netlist);
void   s_hierarchy_post_process(GedaToplevel *pr_current, NETLIST *head);
GList *s_hierarchy_remove_urefconn(NETLIST *head, char *uref_disable);
void   s_hierarchy_remove_compsite_all(NETLIST *head, GedaList *removed);
char  *s_hierarchy_create_uref(GedaToplevel *pr_current, char *basename, char *hierarchy_tag);
char  *s_hierarchy_create_netname(GedaToplevel *pr_current, char *basename, char *hierarchy_tag);
char  *s_hierarchy_create_netattrib(GedaToplevel *pr_current, char *basename, char *hierarchy_tag);
void   s_hierarchy_remove_uref_mangling(GedaToplevel *pr_current, NETLIST *head);
char  *s_hierarchy_return_baseuref(GedaToplevel *pr_current, char *uref);
int    s_hierarchy_graphical_search(GedaObject*o_current, int count);

/* s_misc.c */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);

/* s_net.c */
NET  *s_net_add(NET *ptr);
void  s_net_destroy_or_report(NET *ptr, GedaList *list);
int   s_net_find(NET *net_head, NET *node);
void  s_net_print(NET *ptr);
char *s_net_return_connected_string(GedaToplevel *pr_current, GedaObject *object, char *hierarchy_tag);
NET  *s_net_return_head(NET *tail);
NET  *s_net_return_tail(NET *head);
char *s_net_name_search(GedaToplevel *pr_current, NET *net_head);
char *s_net_name(GedaToplevel *pr_current, NETLIST *netlist_head, NET *net_head, char *hierarchy_tag, int type);

/* s_netattrib.c */
char *s_netattrib_pinnum_get_connected_string (const char *pinnum) GEDA_WARN_UNUSED_RESULT;
const char *s_netattrib_connected_string_get_pinnum (const char *str);
char *s_netattrib_extract_netname(char *value);
void  s_netattrib_create_pins(GedaToplevel *pr_current,GedaObject *o_current, NETLIST *netlist, char *value, char *hierarchy_tag);
void  s_netattrib_handle(GedaToplevel *pr_current,GedaObject *o_current, NETLIST *netlist, char *hierarchy_tag);
char *s_netattrib_net_search(GedaObject *o_current, const char *wanted_pin);
char *s_netattrib_return_netname(GedaToplevel *pr_current,GedaObject *o_current, char *pinnumber, char *hierarchy_tag);

/* s_netlist.c */
NETLIST *s_netlist_add(NETLIST *netlist);
void     s_netlist_destroy_or_report(NETLIST *netlist, GedaList *list);
void     s_netlist_name_named_nets (GedaToplevel *pr_current, NETLIST *named_netlist, NETLIST *unnamed_netlist);
char    *s_netlist_netname_of_netid (GedaToplevel *pr_current, NETLIST *netlist_head, int net_id);
void     s_netlist_post_process(GedaToplevel *pr_current, NETLIST *head);
void     s_netlist_print(NETLIST *netlist);
NETLIST *s_netlist_return_head(NETLIST *tail);
NETLIST *s_netlist_return_tail(NETLIST *head);

/* s_rename.c */
void s_rename_init(void);
void s_rename_destroy_all(void);
void s_rename_next_set(void);
int  s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
void s_rename_all(GedaToplevel *pr_current, NETLIST *netlist_head);
SCM  g_get_renamed_nets(SCM scm_level);

/* s_traverse.c */
void      s_traverse_process(GedaToplevel *pr_current);
void      s_traverse_sheet(GedaToplevel *pr_current, const GList *obj_list);
void      s_traverse_hierarchy_sheet (GedaToplevel *pr_current, NETLIST *netlist);
CPINLIST *s_traverse_component(GedaToplevel *pr_current,GedaObject *component, char *hierarchy_tag);
NET      *s_traverse_net(GedaToplevel *pr_current, NET *nets, int starting, GedaObject *object, char *hierarchy_tag, int type);

/* vams_misc.c */
SCM vams_get_attribs_list(GedaObject *object);
SCM vams_get_package_attributes(SCM scm_uref);
