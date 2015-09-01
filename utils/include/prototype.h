/*! \file
 * Function prototypes for the gschlas utility.
 */
/* g_rc.c */
SCM g_rc_gschlas_version(SCM version);

/* g_register.c */
void g_register_funcs(void);
SCM  g_quit(void);

/* globals.c */

/* gschlas.c */
void gschlas_quit(void);

/* i_vars.c */
void i_vars_set(GedaToplevel *pr_current);

/* parsecmd.c */
void usage(char *cmd);
int  parse_commandline(int argc, char *argv[]);

/* s_util.c */
void s_util_embed(GedaToplevel *pr_current, int embed_mode);
