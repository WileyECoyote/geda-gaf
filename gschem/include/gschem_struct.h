
typedef struct st_session Session;
typedef struct st_session_menu_data SessionMenuData;
typedef struct st_stretch STRETCH;

struct st_session {
  int   page_count;
  char *session_file;
  char *session_name;
};

struct st_session_menu_data {
  GschemToplevel *w_current;
  Session        *session;
};

struct st_stretch
{
  Object *object;
  int whichone;
};