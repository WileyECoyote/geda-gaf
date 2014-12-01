/* The declaration for gschem's interface to libgedadraw is include as a seperate file
 * so that headers for the underlying systems within libgedadraw need not be include by
 * all of gschem.
 * Current, only o_redraw.c needs to include this file
 */

/* o_draw.c */
void       o_draw_object                (GschemToplevel *w_current, GedaDrawData *draw_data, GdkColor *color);