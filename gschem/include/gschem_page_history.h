/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_page_history.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 * Date Contributed: March, 31, 2016
 */
/*!
 * \file gschem_page_history.h
 *
 * \brief
 */

/*! \class GschemPageHistory gschem_page_history.h "gschem_page_history.h"
 *  \brief Page History Object
 */

typedef struct _GschemPageHistory GschemPageHistory;

struct _GschemPageHistory
{
  PageList *pages_forw;
  PageList *pages_back;
};

void
gschem_page_history_free (GschemPageHistory *history);

Page*
gschem_page_history_get_back (GschemPageHistory *history);

Page*
gschem_page_history_get_forward (GschemPageHistory *history);

GschemPageHistory*
gschem_page_history_new (void);

void
gschem_page_history_push_back (GschemPageHistory *history, Page *page);

void
gschem_page_history_remove_page (GschemPageHistory *history, Page *page);

void
gschem_page_history_seed_back (GschemPageHistory *history, GList *pages);
