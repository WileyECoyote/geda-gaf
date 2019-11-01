#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include <gtk/gtk.h>

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_dockbox.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaDockBox - A Widget container to hold Handleboxes
 * \par
 * \defgroup GedaDockBox Docking Widget Implementation Module
 * @{
 */

static void *geda_dock_box_parent_class = NULL;

/* Table of pointers to GedaDockBox instances */
static GHashTable *dockbox_hash = NULL;

/*! \internal gobject_class->finalize */
static void geda_dock_box_finalize (GObject *object)
{
  GedaDockBox *dockbox = (GedaDockBox*)object;

  if (dockbox->docks) {
    g_list_free(dockbox->docks);
    dockbox->docks = NULL;
  }

  if (g_hash_table_remove (dockbox_hash, object)) {
    if (!g_hash_table_size (dockbox_hash)) {
      g_hash_table_destroy (dockbox_hash);
      dockbox_hash = NULL;
    }
  }

  G_OBJECT_CLASS (geda_dock_box_parent_class)->finalize (object);
}

/*!
 * \brief GedaDockBoxClass Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  g_class     GedaDockBoxClass class being initializing
 * \param [in]  class_data  Associated GedaDockBoxClass structure
 */
static void geda_dock_box_class_init(void *g_class, void *class_data)
{
  GObjectClass       *object_class;
  GedaDockBoxClass   *class;

  class           = (GedaDockBoxClass*)g_class;
  object_class    = (GObjectClass*)class;

  object_class->finalize            = geda_dock_box_finalize;

  geda_dock_box_parent_class = g_type_class_peek_parent(class);
}

/*!
 * \brief Type instance initializer for GedaDockBox
 * \par Function Description
 *  Type instance initializer for GedaDockBox, initializes a new empty
 *  GedaDockBox object.
 *
 * \param [in] instance The GedaDockBox structure being initialized,
 * \param [in] g_class  The GedaDockBox class we are initializing.
 */
static void geda_dock_box_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaDockBox *dockbox = (GedaDockBox*)instance;

  dockbox->docks = NULL;

  if (!dockbox_hash) {
    dockbox_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (dockbox_hash, instance, instance);
}

/**
 * \defgroup GedaDockBoxFunctions Handle Box Public Functions
 * @{
 *  \par Begin Public Accessors
 */

/*!
 * \brief Retrieve GedaDockBox's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaDockBox Type identifier. When
 *  first called, the function registers a #GedaDockBox in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaDockBox and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaDockBox.
 */
GedaType geda_dock_box_get_type (void)
{
  static volatile GedaType geda_dock_box_type = 0;

  if (g_once_init_enter (&geda_dock_box_type)) {

    static const GTypeInfo info = {
      sizeof(GedaDockBoxClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_dock_box_class_init,        /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaDockBox),
      0,                               /* n_preallocs         */
      geda_dock_box_instance_init      /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaDockBox");
    type   = g_type_register_static (GTK_TYPE_BOX, string, &info, 0);

    g_once_init_leave (&geda_dock_box_type, type);
  }

  return geda_dock_box_type;
}

/*!
 * \brief Check if an object is a GedaDockBox
 * \par Function Description
 *  Determines if \a dockbox is valid by verifying \a dockbox
 *  is included in the hash table of GedaDockBox objects.
 *
 * \return TRUE if \a dockbox is a valid GedaDockBox
 */
bool is_a_geda_dock_box (GedaDockBox *dockbox)
{
  if ((dockbox != NULL) && (dockbox_hash != NULL)) {
    return g_hash_table_lookup(dockbox_hash, dockbox) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Get a New GedaDockBox Object
 * \par Function Description
 *  Creates and returns a new GedaDockBox instance
 */
GtkWidget *geda_dock_box_new (int orientation)
{
  GtkWidget *dockbox;

  if (orientation == GTK_ORIENTATION_HORIZONTAL) {
    dockbox = g_object_new (GEDA_TYPE_DOCK_BOX,
                           "orientation",
                            GTK_ORIENTATION_VERTICAL,
                            NULL);
  }
  else if (orientation == GTK_ORIENTATION_VERTICAL) {
    dockbox = g_object_new (GEDA_TYPE_DOCK_BOX,
                           "orientation",
                            GTK_ORIENTATION_HORIZONTAL,
                            NULL);
  }
  else {
    dockbox = NULL;
  }

  if (dockbox) {
    ((GedaDockBox*)dockbox)->orientation = orientation;
  }

  return dockbox;
}

static GtkBox *geda_dock_box_new_box(GedaDockBox *dockbox)
{
  GtkWidget *box;

  if (dockbox->orientation == GTK_ORIENTATION_HORIZONTAL) {
    box = gtk_hbox_new (FALSE, 0);
  }
  else {
    box = gtk_vbox_new (FALSE, 0);
  }

  gtk_box_pack_start ((GtkBox*)dockbox, box, FALSE, FALSE, 0);

  gtk_widget_show(box);

  return (GtkBox*)box;
}

static GtkBox *geda_dock_box_get_location (GedaDockBox *dockbox, int location)
{
  GtkBox *box;

  if (!dockbox->docks) {

    box = geda_dock_box_new_box (dockbox);

    dockbox->docks = g_list_append(NULL, box);
  }
  else {

    int count;

    count = g_list_length(dockbox->docks) - 1;

    if (location > count) {
      box = geda_dock_box_new_box (dockbox);
      /* For now, assume consecutive */
      dockbox->docks = g_list_append(dockbox->docks, box);
    }
    else {
      box = g_list_nth_data (dockbox->docks, location);
    }
  }

  return box;
}

/*!
 * \brief Add a GedaHandleBox to a GedaDockBox
 * \par Function Description
 *  Packs the \a child handlebox into a subcontainer of the \a dockbox.
 *  If dockbox has no subcontainer, the location is ignore and the child
 *  is added to a new subcontainer. If the dockbox has a subcontainer then
 *  the child will be added at the index given by location, which would be
 *  a new subcontainer if the given location is greater than the current
 *  number of subcontainers.
 */
void geda_dock_box_add (GedaDockBox *dockbox, GtkWidget *child, int location)
{
  GtkBox *box;

  g_return_if_fail (GEDA_IS_DOCK_BOX (dockbox));

  box = geda_dock_box_get_location(dockbox, location);

  gtk_box_pack_start (box, child, FALSE, FALSE, 0);
}

/*!
 * \brief Add a GedaHandleBox to a GedaDockBox Widget
 * \par Function Description
 *  Widget version of #geda_dock_box_add.
 */
void geda_dock_widget_add (GtkWidget *dockbox, GtkWidget *child, int location)
{
  GtkBox *box;

  g_return_if_fail (GEDA_IS_DOCK_BOX (dockbox));

  box = geda_dock_box_get_location((GedaDockBox*)dockbox, location);

  gtk_box_pack_start (box, child, FALSE, FALSE, 0);
}

/** @} end group GedaDockBoxFunctions */
/** @} end group GedaDockBox */
