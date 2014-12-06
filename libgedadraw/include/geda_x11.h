#define geda_draw_box    geda_x11_draw_box
#define geda_draw_circle geda_x11_draw_circle
#define geda_draw_line   geda_x11_draw_line
#define geda_draw_net    geda_x11_draw_net
#define geda_draw_text   geda_x11_draw_text

typedef struct st_geda_draw_data GedaDrawData;

struct st_geda_draw_data {

  cairo_surface_t *surface;

  GC        gc;
  Display  *display;
  Drawable  drawable;
  int       screen;

  Object     *object;
  const char *font_name;
  double      scale;

  XColor      color;
  Colormap    colormap;
};

int geda_x11_draw_box    (GedaDrawData *draw_data, int x, int y, int width, int height);
int geda_x11_draw_circle (GedaDrawData *draw_data, int x, int y, int radius);
int geda_x11_draw_line   (GedaDrawData *draw_data, int x1, int y1, int x2, int y2);
int geda_x11_draw_net    (GedaDrawData *draw_data, int x1, int y1, int x2, int y2);
int geda_x11_draw_text   (GedaDrawData *draw_data, int x, int y);
