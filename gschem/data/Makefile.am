theme=hicolor

# --------------------------------------------------------------------
# These icons get installed to a gEDA-specific icons
# directory. They're used within gschem.
icondir = $(GEDADATADIR)/icons/$(theme)
iconsvg = gschem-icons.svg

action_icons = \
	attribute-attach \
	attribute-detach \
	attribute-reset \
	break \
	clone \
	close-path \
	symbol-datasheet \
	deselect \
	extend \
	insert-attribute \
	insert-arc \
	insert-circle \
	insert-box \
	insert-bus \
	insert-line \
	insert-net \
	insert-path \
	insert-pin \
	insert-symbol \
	insert-text \
	insert-net \
	multi-clone \
	process-stop \
	select \
	show-both \
	show-hidden \
	show-inherited \
	show-name \
	show-netnames \
	show-value \
	unselect-all \
	zoom-extents \
	zoom-fit \
	zoom-in \
	zoom-mag \
	zoom-out \
	zoom-limits \
	zoom-selection

action_icon_files = \
	$(action_icons:%=%-16.png) \
	$(action_icons:%=%-22.png) \
	attribute-visibility-28.png \
	attribute-visibility-22.png

regen-svg-icons: $(iconsvg)
	if test -x "`which inkscape`"; then \
	  for f in $(action_icon_files); do \
	    id=`echo $$f | sed -e's:.png::'`; \
	    inkscape --export-use-hints \
	             --export-id=$$id \
	             --export-png=$(srcdir)/$$f \
	             $(srcdir)/$(iconsvg); \
	  done; \
	fi

icons = \
	$(action_icon_files:%=actions,%)

# --------------------------------------------------------------------
# The application icon gets installed to the system icons
# directory. It's used by e.g. the desktop environment to make buttons
# that launch gEDA look nice.
themedir=$(xdgdatadir)/icons/$(theme)

app_icon = geda-gschem

app_icon_files = \
	$(app_icon:%=%-48.png) \
	$(app_icon:%=%.svg)
#	$(app_icon:%=%-16.png)
#	$(app_icon:%=%-22.png)
#	$(app_icon:%=%-24.png)
#	$(app_icon:%=%-32.png)
#

theme_icons = \
	$(app_icon_files:%=apps,%)

# --------------------------------------------------------------------
# We also install a .desktop file.
xdgdatadir=@XDGDATADIR@

desktopdir=$(xdgdatadir)/applications
desktop_in_files = geda-gschem.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)

registrydir=$(xdgdatadir)/application-registry
registry_DATA=gschem.applications

DOMAIN=geda-gschem

@DESKTOP_I18N_LANGS_RULE@
@DESKTOP_I18N_DESKTOP_RULE@

# --------------------------------------------------------------------
# Special rules for installing icon files
install_icons_exec = \
	$(ICON_THEME_INSTALLER) \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(icondir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"

install_theme_icons_exec = \
	$(ICON_THEME_INSTALLER) \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(themedir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"

install-data-local:
	$(install_icons_exec) -i $(icons)
	$(install_theme_icons_exec) -i $(theme_icons)

install-data-hook:
if ENABLE_UPDATE_XDG_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)" 2>/dev/null
endif

uninstall-hook:
if ENABLE_UPDATE_XDG_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)" 2>/dev/null
endif
	$(install_icons_exec) -u $(icons)
	$(install_theme_icons_exec) -u $(theme_icons)


EXTRA_DIST           =	$(iconsvg)           \
			$(action_icon_files) \
			$(app_icon_files)    \
			$(desktop_in_files)  \
			$(desktop_DATA)      \
			$(registry_DATA)

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~ core FILE $(desktop_DATA)
MAINTAINERCLEANFILES = *.log *~ core FILE Makefile.in $(desktop_DATA)
