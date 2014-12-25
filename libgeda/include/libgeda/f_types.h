

extern void (*x_log_update_func)(const char *, GLogLevelFlags,  const char *);

/* Used by g_rc_parse_handler() */
typedef void (*ConfigParseErrorFunc)(GError **, void *);

typedef int (*BoundsFunc)(Object *);

/*! \brief Type of callback function for calculating text bounds */
typedef int(*RenderedBoundsFunc)(void *, Object *, int *, int *, int *, int *);

/*! \brief Type of callback function for object damage notification */
typedef int(*ChangeNotifyFunc)(void *, Object *);

/*! \brief Type of callback function for notification when a new Object is created */
typedef void(*NewObjectFunc)(Object *, void *);

/*! \brief Type of callback function for notification when a new GedaToplevel is created */
typedef void(*NewPageFunc)(Page *, void *);

/*! \brief Type of callback function for notification when a new GedaToplevel is created */
typedef void(*NewToplevelFunc)(GedaToplevel *, void *);

/*! \brief Type of callback function for notification when an object's attributes change */
typedef void(*AttribsChangedFunc)(void *, Object *);

/*! \brief Type of callback function for notification when an object's connections change */
typedef void(*ConnsChangedFunc)(void *, Object *);

/*! \brief Type of callback function for querying loading of backups  */
typedef bool(*LoadBackupQueryFunc)( char *, void *);

/*! \brief Type of callback function for object damage notification */
typedef void (*WeakNotifyFunc)(void *, void *);