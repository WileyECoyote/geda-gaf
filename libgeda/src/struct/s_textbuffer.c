/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include "../../../config.h"

#include <stdio.h>
#include <glib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda_priv.h>

/*!
 * \brief Clean up a managed text buffer
 * \par Function description
 *  Cleans up all of the resources associated with a given TextBuffer.
 *  Should be called thus:
 *
 * \code
 *  tb = geda_struct_textbuffer_free (tb);
 * \endcode
 */
TextBuffer *geda_struct_textbuffer_free (TextBuffer *tb)
{
  if (tb == NULL) return NULL;

  GEDA_FREE (tb->line);
  tb->line = NULL;
  GEDA_FREE (tb);

  return NULL;
}

/*!
 * \brief Get the line count from the text buffer
 * \par Function description
 *  Returns the number of calls to geda_struct_textbuffer_next,
 *  which would include geda_struct_textbuffer_next_line. The
 *  return value is not accurate if geda_struct_textbuffer_seek
 *  has been called!
 */
int geda_struct_textbuffer_get_line_count (TextBuffer *tb)
{
  if (tb == NULL) return 0;
  return tb->line_count;
}

/*!
 * \brief Create a new managed text buffer.
 * \par Function description
 *  Allocates and initializes a new TextBuffer to manage the given
 *  data buffer.
 *
 *  If the size argument is negative, assumes that data is
 *  null-terminated.
 *
 * \param data The address of the buffer to be managed.
 * \param size The length of the buffer.
 *
 * \returns Pointer to a new TextBuffer struct.
 */
TextBuffer *geda_struct_textbuffer_new (const char *data, const int size)
{
  TextBuffer  *tb;
  unsigned int realsize;

  g_return_val_if_fail ((data != NULL), NULL);

  if (size < 0)
    realsize = strlen(data);
  else
    realsize = size;

  tb = GEDA_MEM_ALLOC0 (sizeof(TextBuffer));

  tb->buffer     = data;
  tb->size       = realsize;
  tb->linesize   = TEXT_BUFFER_LINE_SIZE;
  tb->line       = GEDA_MEM_ALLOC(tb->linesize);
  tb->offset     = 0;
  tb->line_count = 0;

  return tb;
}

/*!
 * \brief Fetch a number of characters from a text buffer
 * \par Function description
 *  Get some number of characters from a TextBuffer, starting at the
 *  current position.  If the end of the buffer has been reached (and
 *  thus no more characters remain) returns null.  If \a count is -1,
 *  obtains all characters up to and including the next newline.
 *
 *  A newline is detected as '\\n', or '\\r' together with its
 *  immediately following '\\n', or '\\r', in that order.  All newlines
 *  are collapsed into a single '\\n'.
 *
 *  The returned character array should be considered highly volatile,
 *  and is only valid until the next call to geda_struct_textbuffer_next() or
 *  geda_struct_textbuffer_next_line().
 *
 * \param tb    TextBuffer to read from.
 * \param count Maximum number of characters to read.
 *
 * \returns Character array, or NULL if no characters left.
 */
const char *geda_struct_textbuffer_next (TextBuffer *tb, const int count)
{
  bool eol = FALSE;

  g_return_val_if_fail (tb != NULL, NULL);

  if (tb->offset >= tb->size)
    return NULL;

  const char *src = tb->buffer + tb->offset;
        char *dest = tb->line;
  const char *buf_end = tb->buffer + tb->size;

  while (1) {

    unsigned int len;
    char c;

    if (src >= buf_end) break;
    if (count >= 0 && dest - tb->line >= count) break;
    if (count < 0 && eol) break;

    /* Expand line buffer, if necessary, leaving space for a null */
    len = dest - tb->line + 2;
    if (len >= tb->linesize) {
      tb->linesize += TEXT_BUFFER_LINE_SIZE;
      tb->line = g_realloc(tb->line, tb->linesize);
    }

    eol = FALSE;
    c = *src;
    if (c == '\n') {
      *dest = '\n';
      eol = TRUE;
    }
    else if (c == '\r') {
      *dest = '\n';
      eol = TRUE;
      /* Peek ahead to absorb a '\n' */
      src++;
      if (src >= buf_end || *src != '\n') src--;
    }
    else {
      *dest = c;
    }

    src++;
    dest++;
  }

  *dest = 0;
  tb->offset = src - tb->buffer;

  tb->line_count++;

  return tb->line;
}

/*!
 * \brief Fetch the next line from a text buffer
 * \par Function description
 *  Get the next line of characters from a TextBuffer, starting from
 *  the current position.  If the end of the buffer has been reached
 *  (and thus no more characters remain) returns null.
 *
 *  The returned character array should be considered highly volatile,
 *  and is only valid until the next call to geda_struct_textbuffer_next()
 *  or geda_struct_textbuffer_next_line().
 *
 * \param tb    TextBuffer to read from.
 *
 * \returns     Character array, or NULL if no characters left.
 */
const char *geda_struct_textbuffer_next_line (TextBuffer *tb)
{
  return geda_struct_textbuffer_next (tb, -1);
}

/*!
 * \brief Change the current position within a text buffer
 * \par Function description
 *  Changes where the next call to geda_struct_textbuffer_next will
 *  start reading. If offset is negative, the offset is calculated
 *  relative to the current buffer position.
 *
 * \param tb     A TextBuffer to seek within.
 * \param offset A new position within the buffer.
 */
void geda_struct_textbuffer_seek (TextBuffer *tb, int offset)
{
  if (tb) {

    int index;

    /* if requested offset is greater than the size of the
     * buffer then set index to the end of the buffer */
    if (offset > tb->size) {
      index = tb->size;
    }
    else if (offset < 0) {

      index = tb->offset - offset;

      if (index < 0) {
        index = 0;
      }
    }
    else {
      index = offset;
    }

    tb->offset = index;
  }
}
