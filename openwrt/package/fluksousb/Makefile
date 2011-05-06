# Copyright (c) 2008 jokamajo.org
# $Id$

include $(TOPDIR)/rules.mk

PKG_NAME:=fluksousb
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)
PKG_INSTALL_DIR:=$(PKG_BUILD_DIR)/ipkg-install

include $(INCLUDE_DIR)/package.mk

define Package/fluksousb
  SECTION:=utils
  CATEGORY:=Utilities
  DEPENDS:=+msg-flukso +kmod-usb-serial-ftdi
  TITLE:=Meta package to install all dependencies to use a fluksoUSB board
  URL:=http://mysmartgrid.de/
endef

define Package/fluksousb/Description
	This meta package depends on all necessary packages for using a fluksoUSB board
endef

define Build/Compile
endef

define Package/fluksousb/install
	$(INSTALL_DIR) $(1)/usr/share/doc/fluksousb/
	$(CP) README.TXT $(1)/usr/share/doc/fluksousb/README.TXT
endef

define Package/fluksousb/postinst
	#!/bin/sh

	echo 'using ttyUSB0 instead of ttyS0'
	sed -i -e 's/ttyS0/ttyUSB0/g' /etc/init.d/flukso /usr/share/lua/flukso/flukso.lua
endef

$(eval $(call BuildPackage,fluksousb))
