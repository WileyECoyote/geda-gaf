/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_stat.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2014-2016 Wiley Edward Hill
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: July, 25, 2014
 */
#ifndef __GEDA_STAT__
#  define __GEDA_STAT__

#  ifdef HAVE_SYS_STAT_H

#    include <sys/stat.h>

#  else

#    define S_ISUID       0004000           /* set user id on execution */
#    define S_ISGID       0002000           /* set group id on execution */

#    define S_IRWXU       0000700           /* RWX mask for owner */
#    define S_IRUSR       0000400           /* R for owner */
#    define S_IWUSR       0000200           /* W for owner */
#    define S_IXUSR       0000100           /* X for owner */

#    define S_IREAD    S_IRUSR
#    define S_IWRITE   S_IWUSR
#    define S_IEXEC    S_IXUSR

#    define S_IRWXG       0000070           /* RWX mask for group */
#    define S_IRGRP       0000040           /* R for group */
#    define S_IWGRP       0000020           /* W for group */
#    define S_IXGRP       0000010           /* X for group */

#    define S_IRWXO       0000007           /* RWX mask for other */
#    define S_IROTH       0000004           /* R for other */
#    define S_IWOTH       0000002           /* W for other */
#    define S_IXOTH       0000001           /* X for other */

#    define S_IFMT        0170000           /* type of file */
#    define S_IFIFO       0010000           /* named pipe (fifo) */
#    define S_IFCHR       0020000           /* character special */
#    define S_IFDIR       0040000           /* directory */
#    define S_IFBLK       0060000           /* block special */
#    define S_IFREG       0100000           /* regular */
#    define S_IFLNK       0120000           /* symbolic link */
#    define S_IFSOCK      0140000           /* socket */

#    define S_ISDIR(m)    ((m & 0170000) == 0040000)      /* directory */
#    define S_ISCHR(m)    ((m & 0170000) == 0020000)      /* char special */
#    define S_ISBLK(m)    ((m & 0170000) == 0060000)      /* block special */
#    define S_ISREG(m)    ((m & 0170000) == 0100000)      /* regular file */
#    define S_ISFIFO(m)   ((m & 0170000) == 0010000)      /* fifo */

#    define S_ISLNK(m)    ((m & 0170000) == 0120000)      /* symbolic link */
#    define S_ISSOCK(m)   ((m & 0170000) == 0140000)      /* socket */

#  endif

/* Even given the above, there is still a chance things did not get defined,
 * such as the case when __MINGW32__ is defined because stat.h exist but does
 * not define some of these:
 */

/* Group */
#  ifndef S_IRWXG
#    define S_IRWXG       0000070           /* RWX mask for group */
#  endif

#  ifndef S_IRGRP
#    define S_IRGRP       0000040           /* R for group */
#  endif

#  ifndef S_IWGRP
#    define S_IWGRP       0000020           /* W for group */
#  endif

#  ifndef S_IXGRP
#    define S_IXGRP       0000010           /* X for group */
#  endif

/* Other */
#  ifndef S_IRWXO
#    define S_IRWXO       0000007           /* RWX mask for other */
#  endif

#  ifndef S_IROTH
#    define S_IROTH       0000004           /* R for other */
#  endif

#  ifndef S_IWOTH
#    define S_IWOTH       0000002           /* W for other */
#  endif

#  ifndef S_IXOTH
#    define S_IXOTH       0000001           /* X for other */
#  endif

#endif /* ifndef __GEDA_STAT__ */
