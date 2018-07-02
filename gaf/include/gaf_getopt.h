
#ifndef GAF_GETOPT_H
#define GAF_GETOPT_H

//extern int  gaf_opterr;		/* if error message should be printed */
extern int  gaf_optind;		/* index into parent argv vector */
//extern int  gaf_optopt;		/* character checked for validity */
//extern int  gaf_optreset;  	/* reset getopt  */

extern char *gaf_optarg;	/* argument associated with option */

typedef struct gaf_option	/* specification for a long form option...	*/
{
  const char *name;		/* option name, without leading hyphens */
  int         has_arg;		/* does it take an argument?		*/
  int        *flag;		/* where to save its status, or NULL	*/
  int         val;		/* its associated status value		*/
} gaf_option;

#ifdef __cplusplus
extern "C" {
#endif

int gaf_getopt(int nargc, char * const nargv[], const char *ostr) ;

int gaf_getopt_long(int nargc, char * const *nargv, const char *options,
                    const gaf_option *long_options, int *idx);

int getopt_long_only(int nargc, char * const *nargv, const char *options,
                     const gaf_option *long_options, int *idx);

#ifdef __cplusplus
}
#endif

#endif /* GAF_GETOPT_H */