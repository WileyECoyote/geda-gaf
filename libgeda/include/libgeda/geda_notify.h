/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_notify.h
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
 * 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: September, 2nd, 2014
 */
#ifndef __GEDA_NOTIFY_H__
#define __GEDA_NOTIFY_H__

BEGIN_DECLS

typedef struct st_change_notify change_notify;

/*------------------------------------------------------------------
 *                     Notification Handlers
 *------------------------------------------------------------------*/

struct st_change_notify {
  ChangeNotifyFunc pre_change_func;
  ChangeNotifyFunc change_func;
  void *user_data;
};

#define GEDA_TYPE_NOTIFY_LIST            (geda_notify_list_get_type())
#define GEDA_NOTIFY_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_NOTIFY_LIST, GedaNotifyList))
#define GEDA_NOTIFY_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_NOTIFY_LIST, GedaNotifyListClass))
#define GEDA_IS_NOTIFY_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GEDA_TYPE_NOTIFY_LIST))
#define GEDA_IS_NOTIFY_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_NOTIFY_LIST))
#define GEDA_NOTIFY_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_NOTIFY_LIST, GedaNotifyListClass))


typedef struct _GedaNotifyList      GedaNotifyList;
typedef struct _GedaNotifyListClass GedaNotifyListClass;

struct _GedaNotifyList {
  GObject parent;
  GList  *glist;
  int     freeze_count;
};

struct _GedaNotifyListClass {
  GObjectClass parent;
};

GedaType  geda_notify_list_get_type                (void) GEDA_CONST;

GedaNotifyList *geda_notify_list_new               (void);

int       geda_notify_list_is_frozen               (GedaNotifyList *list);
void      geda_notify_list_freeze                  (GedaNotifyList *list);
void      geda_notify_list_thaw                    (GedaNotifyList *list);

void      geda_notify_list_add                     (GedaNotifyList *list, void *item);
void      geda_notify_list_add_glist               (GedaNotifyList *list, GList *items);
GList    *geda_notify_list_copy_glist              (GedaNotifyList *list);
int       geda_notify_list_in_list                 (GedaNotifyList *list, void *func);
void      geda_notify_list_remove                  (GedaNotifyList *list, void *item);

/*void geda_notify_list_remove_glist(GedaNotifyList *list, GList *items); */ /* Undemanded as yet */
void      geda_notify_list_remove_all              (GedaNotifyList *list);


#define geda_notify_list_get_glist(list) ((list->glist) ? ((GList *)(g_list_first (list->glist))) : NULL)

END_DECLS

#endif /* __GEDA_NOTIFY_H__ */
