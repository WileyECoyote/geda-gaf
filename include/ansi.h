/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: ansi.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2015 Wiley Edward Hill
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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
 * 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: March, 01, 2015
 */
/*
 *     printf("%sred%s\n", FG_RED, RESET);
 *
 *     or better:
 *
 *     printf("The quick " FG_RED "red" RESET " fox jumped over the " FG_YELLOW "yellow" RESET " dog\n");
 */
#pragma once
#ifndef ANSI_COLORS_H
#define ANSI_COLORS_H

#define ATT_ALL_OFF     "\x1B[0m"   /* All attributes off (can be omitted ) */
#define ATT_BOLD        "\x1B[1m"   /* Bold, appears bright */
#define ATT_FAINT       "\x1B[2m"   /* Faint, not widely supported */
#define ATT_ITALIC      "\x1B[3m"   /* Italic, not widely supported, sometimes inverse */
#define ATT_UNDERSCORE  "\x1B[4m"   /* Underscore, monomchrome adapters only! */
#define ATT_BLINKING    "\x1B[5m"   /* Blinking */
#define ATT_RAPID       "\x1B[6m"   /* Rapid Blinking, not widely supported */
#define ATT_REVERSE     "\x1B[7m"   /* Reverse video - depends on previous colors */
#define ATT_HIDDEN      "\x1B[8m"   /* Hidden */
#define ATT_CROSSED     "\x1B[9m"   /* Crossed out, not widely supported */
#define ATT_PRIMARY     "\x1B[10m"  /* Primary, default font */
/*11–19 nth alternate font */
#define ATT_FRAKTUR     "\x1B[20m"  /* Fraktur, rarely supported */
#define ATT_BOLD_OFF    "\x1B[21m"  /* Bold off not widely supported; double underline, rarely supported */
#define ATT_NORMAL      "\x1B[22m"  /* Neither bold nor faint */
#define ATT_NOT_IF      "\x1B[23m"  /* Not italic, not Fraktur */
#define ATT_UL_OFF      "\x1B[24m"  /* Not singly or doubly underlined */
#define ATT_BLINK_OFF   "\x1B[25m"  /* */
#define ATT_RESERVED    "\x1B[26m"  /* Not used */
#define ATT_POSITIVE    "\x1B[27m"  /* Not reversed */
#define ATT_REVEAL      "\x1B[28m"  /* Conceal off */
#define ATT_CROSS_OFF   "\x1B[28m"  /* Not crossed out */
/*30 - 37 Set foreground text color = 30 + x */
#define FG_BLACK        "\x1B[30m"
#define FG_RED          "\x1B[31m"
#define FG_GREEN        "\x1B[32m"
#define FG_YELLOW       "\x1B[33m"
#define FG_BLUE         "\x1B[34m"
#define FG_MAGENTA      "\x1B[35m"
#define FG_CYAN         "\x1B[36m"
#define FG_GRAY         "\x1B[37m"     /* Maybe white */
#define FG_EXCOLOR      "\x1B[38m"
#define FG_DEFAULT      "\x1B[39m"

#define FG_D_GRAY       "\x1B[1;30m"  /* Light Black */
#define FG_L_RED        "\x1B[1;31m"
#define FG_L_GREEN      "\x1B[1;32m"
#define FG_L_BLUE       "\x1B[1;34m"
#define FG_L_MAGENTA    "\x1B[1;35m"
#define FG_L_CYAN       "\x1B[1;36m"
#define FG_WHITE        "\x1B[1;37m"

#define BG_BLACK        "\x1B[40m"
#define BG_RED          "\x1B[41m"
#define BG_GREEN        "\x1B[42m"
#define BG_YELLOW       "\x1B[43m"
#define BG_BLUE         "\x1B[44m"
#define BG_MAGENTA      "\x1B[45m"
#define BG_CYAN         "\x1B[46m"
#define BG_WHITE        "\x1B[47m"
#define BG_EXCOLOR      "\x1B[38m"
#define BG_DEFAULT      "\x1B[39m"

#define ATT_FRAMED      "\x1B[51m"  /*  */
#define ATT_ENCIRCLED   "\x1B[52m"  /*  */
#define ATT_OVER_BAR    "\x1B[53m"  /* Over-lined */
#define ATT_NOT_FC      "\x1B[54m"  /* Not framed or encircled */
#define ATT_OBAR_OFF    "\x1B[55m"  /* Not Over-lined */

#define RESET           "\033[0m"
#endif