theme              =  hicolor
xdgdatadir         = @XDGDATADIR@
desktopdir         = $(xdgdatadir)/applications
themedir           = $(xdgdatadir)/icons/$(theme)
desktop_in_files   = geda-gattrib.desktop.in
desktop_DATA       = $(desktop_in_files:.desktop.in=.desktop)

registrydir=$(xdgdatadir)/application-registry
registry_DATA=gattrib.applications

app_icon   = geda-gattrib

app_icon_files = \
	$(app_icon:%=%-48.png) \
	$(app_icon:%=%.svg)
#	$(app_icon:%=%-16.png)
#	$(app_icon:%=%-22.png)
#	$(app_icon:%=%-24.png)
#	$(app_icon:%=%-32.png)
#
theme_icons        = $(app_icon_files:%=apps,%)

DOMAIN             = geda-gattrib

@DESKTOP_I18N_LANGS_RULE@
@DESKTOP_I18N_DESKTOP_RULE@

EXTRA_DIST         = $(app_icon_files) $(desktop_in_files) \
		     $(desktop_DATA) $(registry_DATA)

install_icon_exec = \
	$(ICON_THEME_INSTALLER) \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(themedir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"

install-data-local:
	$(install_icon_exec) -i $(theme_icons)


install-data-hook:
if ENABLE_UPDATE_XDG_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)" 2>/dev/null
endif

uninstall-hook:
if ENABLE_UPDATE_XDG_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)" 2>/dev/null
endif
	$(install_icon_exec) -u $(theme_icons)

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
MAINTAINERCLEANFILES = *.log *~ core FILE Makefile.in
DISTCLEANFILES       = $(desktop_DATA)

