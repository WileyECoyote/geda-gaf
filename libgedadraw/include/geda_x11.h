#define geda_draw_text geda_x11_draw_text
#define geda_draw_line geda_x11_draw_line

typedef struct st_geda_draw_data GedaDrawData;

struct st_geda_draw_data {

  cairo_surface_t *surface;

  GC        gc;
  Display  *display;
  Drawable  drawable;
  int       screen;

  Object   *object;

  XColor    color;
};

int geda_x11_draw_line (GedaDrawData *draw_data, int x1, int y1, int x2, int y2);
int geda_x11_draw_text (GedaDrawData *draw_data, int x, int y);
