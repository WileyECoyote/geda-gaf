#ifndef __GEDA_MENU_ENUM_H__
#define __GEDA_MENU_ENUM_H__

/*!
 * \typedef PackDirection
 * PACK_DIRECTION_LTR: Widgets are packed left-to-right
 * PACK_DIRECTION_RTL: Widgets are packed right-to-left
 * PACK_DIRECTION_TTB: Widgets are packed top-to-bottom
 * PACK_DIRECTION_BTT: Widgets are packed bottom-to-top
 *
 * Determines how widgets should be packed inside menubars
 * and menuitems contained in menubars.
 */
typedef enum
{
  PACK_DIRECTION_LTR,
  PACK_DIRECTION_RTL,
  PACK_DIRECTION_TTB,
  PACK_DIRECTION_BTT
} PackDirection;

/*!
 * \typedef MenuDirection
 * Menu keyboard movement types */
typedef enum
{
  MENU_DIR_PARENT,
  MENU_DIR_CHILD,
  MENU_DIR_NEXT,
  MENU_DIR_PREV
} MenuDirection;

/*!
 * \typedef SubmenuDirection
 * Directions for submenus */
typedef enum
{
  SUBMENU_DIR_LEFT,
  SUBMENU_DIR_RIGHT
} SubmenuDirection;

/*!
 * \typedef SubmenuPlacement
 * Placement of submenus */
typedef enum
{
  MENU_TOP_BOTTOM,
  MENU_LEFT_RIGHT
} SubmenuPlacement;

#endif /* __GEDA_MENU_ENUM_H__ */