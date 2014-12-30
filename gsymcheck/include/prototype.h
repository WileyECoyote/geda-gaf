/* g_rc.c */
SCM g_rc_gsymcheck_version(SCM version);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* gsymcheck.c */
void gsymcheck_quit(void);
void main_prog(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(GedaToplevel *pr_current);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
/* s_check.c */
int s_check_all(GedaToplevel *pr_current);
int s_check_symbol(GedaToplevel *pr_current, Page *p_current, const GList *obj_list);
bool s_check_list_has_item(char **list , char *item);
void s_check_symbol_structure(const GList *obj_list, SYMCHECK *s_current);
void s_check_text (const GList *obj_list, SYMCHECK *s_current);
void s_check_graphical(const GList *obj_list, SYMCHECK *s_current);
void s_check_device(const GList *obj_list, SYMCHECK *s_current);
void s_check_pinseq(const GList *obj_list, SYMCHECK *s_current);
void s_check_pinnumber(const GList *obj_list, SYMCHECK *s_current);
void s_check_pin_ongrid(const GList *obj_list, SYMCHECK *s_current);
void s_check_slotdef(const GList *obj_list, SYMCHECK *s_current);
void s_check_oldpin(const GList *obj_list, SYMCHECK *s_current);
void s_check_oldslot(const GList *obj_list, SYMCHECK *s_current);
void s_check_nets_buses(const GList *obj_list, SYMCHECK *s_current);
void s_check_connections(const GList *obj_list, SYMCHECK *s_current);
void s_check_missing_attribute(Object *object, char *attribute, SYMCHECK *s_current);
void s_check_missing_attributes(const GList *obj_list, SYMCHECK *s_current);
void s_check_pintype(const GList *obj_list, SYMCHECK *s_current);
/* s_log.c */
void s_log_update (const char *log_domain, GLogLevelFlags log_level, const char *buf);
/* s_symstruct.c */
SYMCHECK *s_symstruct_init(void);
void s_symstruct_print(SYMCHECK *s_current);
void s_symstruct_free(SYMCHECK *s_current);
