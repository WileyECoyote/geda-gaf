/*! \typedef LogFunc
 *  \brief Type of log handler function used by libgeda logging utilities */
typedef void (*LogFunc) (const char *, GLogLevelFlags, const char *, void *);

/*! \typedef LogUpdateFunc
 *  \brief Type of callback function  used by geda_utility_log_set_update_func */
typedef void (*LogUpdateFunc)(const char *, GLogLevelFlags,  const char *);

/*! \typedef ConfigParseErrorFunc
 *  \brief Type of callback function used by g_rc_parse_handler */
typedef void (*ConfigParseErrorFunc)(GError **, void *);

/*! \typedef ObjectBoundsFunc
 *  \brief Type of callback function for getting the bounds of a GedaObject */
typedef int (*ObjectBoundsFunc)(GedaObject *);

/*! \typedef RenderedBoundsFunc
 *  \brief Type of callback function for calculating text bounds */
typedef int(*RenderedBoundsFunc)(void *, GedaObject *, int *, int *, int *, int *);

/*! \typedef ChangeNotifyFunc
 *  \brief Type of callback function for object damage notification */
typedef int(*ChangeNotifyFunc)(void *, GedaObject *);

/*! \typedef NewObjectFunc
 *  \brief Type of callback function for notification when a new GedaObject is created */
typedef void(*NewObjectFunc)(GedaObject *, void *);

/*! \typedef NewPageFunc
 *  \brief Type of callback function for notification when a new GedaToplevel is created */
typedef void(*NewPageFunc)(Page *, void *);

/*! \typedef NewToplevelFunc
 *  \brief Type of callback function for notification when a new GedaToplevel is created */
typedef void(*NewToplevelFunc)(GedaToplevel *, void *);

/*! \typedef AttribsChangedFunc
 *  \brief Type of callback function for notification when an object's attributes change */
typedef void(*AttribsChangedFunc)(void *, GedaObject *);

/*! \typedef ConnsChangedFunc
 *  \brief Type of callback function for notification when an object's connections change */
typedef void(*ConnsChangedFunc)(void *, GedaObject *);

/*! \typedef LoadBackupQueryFunc
 *  \brief Type of callback function for querying loading of backups  */
typedef bool(*LoadBackupQueryFunc)( char *, void *);

/*! \typedef WeakNotifyFunc
 *  \brief Type of callback function for object damage notification */
typedef void (*WeakNotifyFunc)(void *, void *);
