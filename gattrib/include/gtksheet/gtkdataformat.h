/* gtkdataformat - data formatter
 * Copyright 2011  Fredy Paquet <fredy@opag.ch>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#ifndef __GTK_DATA_FORMAT_H__
#define __GTK_DATA_FORMAT_H__

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

char *gtk_data_format        (const char *str, const char *dataformat);
char *gtk_data_format_remove (const char *str, const char *dataformat);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_DATA_FORMAT_H__ */
