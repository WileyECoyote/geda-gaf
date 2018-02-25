/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_uio_macros.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2015-2018 Wiley Edward Hill
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 * Date: January 06, 2015
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
#ifndef __GEDA_UIO_MACROS_H__
#define __GEDA_UIO_MACROS_H__

/* Macros for GedaFileChooser and GedaImageChooser */
#define  geda_chooser_set_do_overwrite_confirmation(c, v) \
         gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER(c), v)

#define  geda_chooser_get_preview_widget_active(c) \
         gtk_file_chooser_get_preview_widget_active (GTK_FILE_CHOOSER(c))

/* Note must include geda_image_chooser.h for this to work, but not the other way
 * around, GedaFileChooser can also just use GtkFileChooser version */
#define  geda_chooser_set_preview_widget_active(c, v) \
         if (GEDA_IS_IMAGE_CHOOSER(c)) \
           geda_image_chooser_set_preview_active (GTK_WIDGET(c), v); \
         else \
           gtk_file_chooser_set_preview_widget_active (GTK_FILE_CHOOSER(c), v);

#endif /* __GEDA_UIO_MACROS_H__ */
