/* g_rc.c */
SCM  g_rc_gsymcheck_version(SCM version);
SCM  g_rc_known_devices(SCM stringlist);
SCM  g_rc_valid_attributes(SCM stringlist);

/* g_register.c */
void g_register_funcs(void);
SCM  g_quit(void);

/* gsymcheck.c */
void gsymcheck_quit(void);

/* i_vars.c */
void i_vars_release_all(void);
void i_vars_set_known_devices(SYMCHECK *s_current);
void i_vars_set_valid_attributes(SYMCHECK *s_current);
void i_vars_set(GedaToplevel *pr_current);

/* parsecmd.c */
void usage(char *cmd);
int  parse_commandline(int argc, char *argv[]);

/* s_check.c */
int  s_check_all(GedaToplevel *pr_current);

/* s_log.c */
void s_log_update (const char *log_domain, GLogLevelFlags log_level, const char *buf);

/* s_symstruct.c */
SYMCHECK *s_symstruct_init(void);
void s_symstruct_print(SYMCHECK *s_current);
void s_symstruct_reset(SYMCHECK *s_current);
void s_symstruct_free(SYMCHECK *s_current);
