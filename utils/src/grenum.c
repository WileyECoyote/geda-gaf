/* $Id$ indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 */
/*  This is grenum, an advanced refdes renumber utility for gEDA's gschem.
 *
 *  Copyright (C) 2005-2014  Levente Kovacs
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * Levente.Kovacs@interware.hu
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <geda/geda_standard.h>

#include <version.h>

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <grenum.h>

int main(int argc, char *argv[])
{
  char buff[BUFFSIZE], infilename[FILENAMESIZE], outfilename[FILENAMESIZE];
  unsigned char flags;
  int c, pages;
  unsigned int i;

#ifdef HAVE_GETOPT_LONG

  int opt_idx;

  const struct option long_opts[]={
    {"version",  no_argument, NULL, 'v'},
    {"help",     no_argument, NULL, 'h'},
    {"pagejump", no_argument, NULL, 'p'},
    {0,0,0,0}};

#endif

  const char *opt_options = "vhp";

  struct refdes_ refdes, refdes_db[MAX_PREFIX_COUNT];

  flags=0x00; /* Clear all flags */

  while(1) {

#ifdef HAVE_GETOPT_LONG
    c = getopt_long(argc, argv, opt_options, long_opts, &opt_idx);
#else
    c = getopt(argc, argv, opt_options);
#endif

    if (c == -1)
      break;
    switch(c) {

      case 'h':
        printhelp();
        return 0;
      case 'v':
        printver();
        return 0;
      case 'p':
        flags|=PageJUMP; /* Set the pagejump flag */
        break;
    }
  }

  if (optind==argc) {

    printf("grenum: no input file\n");
    printhelp();
    return NO_INPUT_FILE;
  }

  /* Zero all the strings in the database. */
  for (c = 0; c < MAX_PREFIX_COUNT; ++c) {
    refdes_db[c].prefix[0]='\0';
    refdes_db[c].value=COUNT_START;
  }

  for (pages = 1; optind < argc; ++optind, ++pages) {

    FILE *infile, *outfile;
    int ret;

    if ((flags&PageJUMP)==PageJUMP) { /* pagejumps */

      for (c = 0; c < MAX_PREFIX_COUNT; ++c) {
        /* Reset the counters according to page numbers */
        refdes_db[c].value=Page_JMP*pages+COUNT_START;
      }
    }

    /* Copy the filename to the buffer */
    strcpy(&infilename[0], argv[optind]);

    /* Open file, use r+ for read only */
    if ((infile=fopen(infilename, "r")) == NULL) {
      fprintf(stderr, "grenum: unable to open input file, %s.\n", strerror(errno));
      return FILE_OP_ERROR;
    }

    strcpy(&outfilename[0],&infilename[0]);

    if ((outfile=fopen(strcat(&outfilename[0],".tmp"),"wb"))==NULL) {
      fprintf(stderr, "grenum: could not create tmp file, %s.\n", strerror(errno));
      /* Close the file */
      fclose(infile);
      return FILE_OP_ERROR;
    }

    printf("grenum: processing file %s\n",&infilename[0]);

    /* Read one line. */
    while((ret=get_refdes_from_file(infile, &refdes, buff))!=END_OF_FILE) {

    /*Process starts here */

#ifdef DEBUG
      printf("%s\n",&buff[0]); /* Print out what is read */
#endif

      switch(ret) {
        case NOT_REFDES_LINE:
          if (fputs(buff,outfile)==-1) {
            fprintf(stderr, "grenum: could not write to tmp file, %s.\n", strerror(errno));
            /* Close the files */
            fclose(infile);
            fclose(outfile);
            return FILE_OP_ERROR;
          }
          continue;

        case REFDES_WITH_VALUE: /* Compare the maximum value, search for gaps, and set the refes_db.value to the next free value */
          c = refdes_lookup(refdes_db, &refdes);
          switch(c) {

            case REFDES_NOT_FOUND: /* No such prefix */
              strcpy(&refdes_db[refdes.prefixes].prefix[0],&refdes.prefix[0]); /* Register the prefix to the database */
              refdes_db[refdes.prefixes+1].prefix[0]='\0';
            refdes_db[refdes.prefixes].value=refdes.value; /* Renumber... Finally :-)*/
            break;

            case MAX_PREFIX_COUNT: /* Out of memory */
              printf("grenum: out of memory. Too much refdes prefixes.\n");
              /* Close the files */
              fclose(infile);
              fclose(outfile);
              return OUT_OF_MEMORY;

            default:
              if (refdes.value-refdes_db[c].value==1) {

                /* If we have the next value, don't do anything, just update the database. */
                refdes_db[c].value=refdes.value;
                break;
              }

              /* Now we have a hole in numbering. Let's see if it'll be fixed, and seek for the maximum value. eg. R1,R2,R5,R3. So, we have to search for R3,R4, and set the db to R3. */

              for (i=refdes_db[c].value+1; i<refdes.value; ++i) {
                if (seek_value(c, infile, i, refdes_db)==VALUE_NOT_FOUND) {
                  refdes_db[c].value=i-1;
                  break;
                }
              }

              if (i!=refdes.value) {
                flags|=GAP_DETECTED;
              }
              else {
                flags|=~GAP_DETECTED;
              }
              break;
          }
          break; /* continue our job */
            case REFDES_WITHOUT_VALUE:
              c=refdes_lookup(refdes_db, &refdes);
              switch(c) {
                case -1: /* No such prefix */
                  strcpy(&refdes_db[refdes.prefixes].prefix[0],&refdes.prefix[0]);
                  refdes.value=++refdes_db[refdes.prefixes].value;
                  refdes_db[refdes.prefixes+1].prefix[0]='\0';
              break;
                case MAX_PREFIX_COUNT:
                  printf("grenum: out of memory. Too much refdes prefixes.\n");
                  fclose(infile); /* Close the files */
                  fclose(outfile);
                  return OUT_OF_MEMORY;
                default:
                  if ((flags&GAP_DETECTED)==GAP_DETECTED) {
                    for (i=refdes_db[c].value+1; seek_value(c, infile, i, refdes_db)!=VALUE_NOT_FOUND; ++i);
                    refdes.value=refdes_db[c].value=i;
                  }
                  else {
                    refdes.value=++refdes_db[c].value; /* renumber */
                  }
                   break;
              }
              sprintf(buff, "refdes=%s%d\n", &refdes.prefix[0], refdes.value);
              break;
                case REFDES_ERROR: /* e.g. awdf#$%WSf82f8 :-) No "=" signal in the refdes string. */
                  printf("grenum: parse error\n");
                  fclose(infile);
                  fclose(outfile);
                  return PARSE_ERROR;
      }

      { /* Finally, write the refdes line to the output file */
        if (fputs(buff,outfile)==-1) {
          fprintf(stderr, "grenum: could not write to tmp file, %s.\n", strerror(errno));
        }

        fclose(infile);
        fclose(outfile);
        return FILE_OP_ERROR;
      }
    } /* Process ends here */

    fclose(outfile);

    /* buff has the original infilename */
    strcpy(&buff[0],&infilename[0]);

    /* The next few lines implements the copy program */
    fseek(infile,0L,SEEK_SET); /* Go to the begining of the infile */
    outfile=fopen(strcat(&buff[0],".save"),"wb");

    if (outfile==NULL) {
      fprintf(stderr, "grenum: could not create backup file, %s.\n", strerror(errno));
      fclose(infile);
      return FILE_OP_ERROR;
    }

    while(fgets(&buff[0],BUFFSIZE,infile)!=NULL)  { /* Read one line. */

      if (fputs(&buff[0],outfile)==-1) {
        fprintf(stderr, "grenum: could not write to backup file, %s.\n", strerror(errno));
        fclose(infile);
        fclose(outfile);
        return FILE_OP_ERROR;
      }
    }

    fclose(infile);
    fclose(outfile);

    /* Move the tmpfile to the original */
    rename(outfilename, infilename);
  }
  printf("grenum: file(s) successfully processed\n");
  return OK; /*Everything is okay*/
}

int get_refdes_from_file(FILE *fp, struct refdes_ *refdes, char *buff)
{
  /* Read one line from file, and return the following things:
   *
   * END_OF_FILE if file reaches its end. The content of buff is unknown!
   *
   * NOT_REFDES_LINE if the current line is not a refdes. The line will
   * saved in buff.
   *
   * Return according to parse_refdes(). The buff will contain the current line too.
   */

  if (fgets(buff, BUFFSIZE, fp)==NULL)
    return END_OF_FILE;

  if (strstr(buff, "refdes=")==NULL)
    return NOT_REFDES_LINE;

  return parse_refdes(refdes, buff);
}

int seek_value(int prefix, FILE *fp, unsigned int value, struct refdes_ *db)
{
  fpos_t filepos;
  int ret;
  struct refdes_ refdes;
  char buff[BUFFSIZE];

  /* First of all, save the file pos. */
  fgetpos(fp, &filepos);

  rewind(fp); /* Rewind */

  while((ret=get_refdes_from_file(fp, &refdes, buff))!=END_OF_FILE) {

    if (ret==REFDES_WITH_VALUE && prefix==refdes_lookup(db, &refdes) && refdes.value==value)
    {
      fsetpos(fp,&filepos);
      return VALUE_FOUND;
    }
  }

  fsetpos(fp,&filepos);
  return VALUE_NOT_FOUND;
}

int parse_refdes(struct refdes_ *refdes, char *ref_str)
{
  int i;
  char buff[BUFFSIZE],*cpr,*cp;

  /*
   * This function parses the refdes line from the .sch file. It takes a pointer to the
   * complete refdes definition string, and a pointer which points to a refdes structure
   * where it'll store the info.
   *
   * parse_refdes() will return
   *
   * REFDES_WITH_VALUE if there was a prefix with renumbered value
   * (for example R1,IC3,U5);
   *
   * REFDES_WITHOUT_VALUE if there was a "?" mark found, and it has to be
   * renumbered (e.g. U?);
   *
   * REFDES_ERROR, if there was some uncool thing.
   *
   * The refdes structure is filled with the prefix and the value.
   *
   * Note that if a "?" is found, the value member remains untouched.
   */

  /* seek for the "=" */
  cpr=strstr(ref_str,"=");

  if (cpr==NULL) /* Should not happen */
    return REFDES_ERROR;

  cp=strstr(ref_str,"?");

  /* refdes=U1    refdes=IC?
   *      |             |
   *    *cpr            cp
   */
  if (cp!=NULL) {

    /* Not renumbered yet */
    strncpy(&refdes->prefix[0], cpr+1,cp-cpr-1); /* Copy the prefix to the refdes structure */
    refdes->prefix[cp-cpr-1]='\0';

#ifdef DEBUG
    printf("Prefix=%s\n",&refdes->prefix[0]);
#endif

    return REFDES_WITHOUT_VALUE;
  }

  /* No "?". Copy the prefix */
  for (cp=cpr+1,i=0;(*cp != '\n' && *cp >= 'A' && *cp <= 'z'); ++i,++cp)
    buff[i]=*cp;  /* Fill the buffer from char to char */

  /* Terminate with NULL to be a string */
  buff[i]='\0';

#ifdef DEBUG
  printf("Prefix=%s\n",&buff[0]);
#endif

  /* Copy to refdes structure */
  strcpy(&refdes->prefix[0],&buff[0]);

  for (i = 0; (*cp != '\n' && *cp >= '0' && *cp <=  '9'); ++cp, ++i) {
    /* Fill the buffer from char to char */
    buff[i]=*cp;
  }

  /* Terminate with NULL to be a string */
  buff[i]='\0';

#ifdef DEBUG
    printf("Value=%s\n",&buff[0]);
#endif

  refdes->value=abs(atoi(&buff[0]));

  return REFDES_WITH_VALUE;
}

int refdes_lookup(struct refdes_ *db, struct refdes_ *ref)
{
  int c=0;

  for (c=0;c<MAX_PREFIX_COUNT;++c) {

    if (strcmp(ref->prefix,(*(db+c)).prefix)==0)
      break;
    else if ((*(db+c)).prefix[0]=='\0')
    {
      ref->prefixes=c;
      return REFDES_NOT_FOUND;
    }
  }
  return c;
}

void printhelp()
{

#ifdef HAVE_GETOPT_LONG
  const char *v_opt="-v | --version";
  const char *h_opt="-h | --help";
  const char *p_opt="-p | --pagejump";
#else
  const char *v_opt="-v";
  const char *h_opt="-h";
  const char *p_opt="-p";
#endif

  printver();
  printf("Usage: grenum [%s] [%s] [%s] file1.sch file2.sch ...\n\n",
  v_opt, h_opt, p_opt);
  printf("\t%s\tprints version info\n\t%s\tprints this help\n\t%s\tsets pagejump mode on\n",
  v_opt, h_opt, p_opt);
  printf("For more information read the README file and/or the manual.\n");
}

void printver()
{
  printf("This is grenum, an advanced refdes renumber utility for gEDA's gschem.\n");
  printf("Version %s.  gEDA/gaf version %s.%s\n", GRVERSION,
          PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION);
  printf("Compiled on %s at %s\n",COMP_DATE,COMP_TIME);
}
