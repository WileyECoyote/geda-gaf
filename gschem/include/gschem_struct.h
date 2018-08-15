
typedef struct st_idle_hook_data IdleHookData;
typedef struct st_idle_task_data IdleTaskData;
typedef struct st_session Session;
typedef struct st_session_menu_data SessionMenuData;
typedef struct st_stretch STRETCH;

struct st_idle_hook_data {
  unsigned int    source_id;
  GschemToplevel *w_current;

  union { GList      *list;
          GedaObject *object;
          Page       *page;
        } data;

  char           *name;
  unsigned int    type;
  unsigned int    hook;
};

struct st_idle_task_data {
  unsigned int    source_id;
  GschemToplevel *w_current;
  void           *data;
  int             retry;
};

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
  GedaObject *object;
  int whichone;
};