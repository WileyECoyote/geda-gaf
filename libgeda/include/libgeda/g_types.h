/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: g_types.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */

#ifndef _G_TYPES_H_INCL
#define _G_TYPES_H_INCL

/*! \brief Geda TopLevel Data Structure */
typedef struct _GedaToplevel     GedaToplevel;

/*! \brief Geda Page Data Structure */
typedef struct _GedaPage         Page;

/* Object types */

/*! \brief Geda Base Object Structure */
typedef struct _GedaObject       GedaObject;

/*! \brief Geda Arc Object Structure */
typedef struct _GedaArc          GedaArc;

/*! \brief Geda Box Object Structure */
typedef struct _GedaBox          GedaBox;

/*! \brief Geda Bus Object Structure */
typedef struct _GedaBus          GedaBus;

/*! \brief Geda Circle Object Structure */
typedef struct _GedaCircle       GedaCircle;

/*! \brief Geda Complex Object Structure */
typedef struct _GedaComplex      GedaComplex;

/*! \brief Geda Line Circle Object Structure */
typedef struct _GedaLine         GedaLine;

/*! \brief Geda Net Circle Object Structure */
typedef struct _GedaNet          Net;

/*! \brief Geda Path Circle Object Structure */
typedef struct _GedaPath         Path;

/*! \brief Geda Picture Circle Object Structure */
typedef struct _GedaPicture      Picture;

/*! \brief Geda Pin Circle Object Structure */
typedef struct _GedaPin          Pin;

/*! \brief Geda Text Circle Object Structure */
typedef struct _GedaText         Text;

#endif
