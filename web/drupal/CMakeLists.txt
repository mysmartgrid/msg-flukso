# -*- mode: cmake; -*-
project(msg-web)
cmake_minimum_required(VERSION 2.6.2 FATAL_ERROR)
if(POLICY CMP0011)
  cmake_policy(SET CMP0011 NEW)
endif(POLICY CMP0011)

set(MSG-WEB_MAJOR_VERSION 0)
set(MSG-WEB_MINOR_VERSION 1)
set(MSG-WEB_SUB_VERSION   0)
set(VERSION_SEP "-")
set(MSG-WEB_SHORT_VERSION "${MSG-WEB_MAJOR_VERSION}${VERSION_SEP}${MSG-WEB_MINOR_VERSION}")
set(MSG-WEB_VERSION "${MSG-WEB_SHORT_VERSION}${VERSION_SEP}${MSG-WEB_SUB_VERSION}")
set(MSG-WEB_RPM_VERSION "${MSG-WEB_MAJOR_VERSION}.${MSG-WEB_MINOR_VERSION}.${MSG-WEB_SUB_VERSION}")
set(VERSION_DATE "2012-09-14")

###########################################################
# Where are the additional libraries installed? Note: provide includes
# path here, subsequent checks will resolve everything else
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/modules")

set(DEST "var/www/flukso/sites/all")

install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
#install(DIRECTORY modules/custom/ DESTINATION ${DEST}/modules)
install(DIRECTORY libraries/ DESTINATION ${DEST}/libraries)
install(DIRECTORY themes/ DESTINATION ${DEST}/themes)

SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA 
  ${CMAKE_SOURCE_DIR}/prerm
  ${CMAKE_SOURCE_DIR}/preinst
  ${CMAKE_SOURCE_DIR}/postrm
  ${CMAKE_SOURCE_DIR}/postinst
  )


# enable unit testing
include(CTest)
enable_testing()

### print some output for the user
message("")
message("        ***** Configuration parameters *****")
message("             prefix: ${CMAKE_INSTALL_PREFIX}")

# CPACK packaging
INCLUDE(InstallRequiredSystemLibraries)
set(CPACK_SOURCE_GENERATOR "TGZ;TBZ2")

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "MySmartGrid Drupal Webinterface")
set(CPACK_PACKAGE_VENDOR              "Fraunhofer ITWM")
set(CPACK_PACKAGE_CONTACT             "Kai Krueger <kai.krueger@itwm.fhg.de>")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER   "Kai Krueger <kai.krueger@itwm.fhg.de>")
set(CPACK_PACKAGE_DESCRIPTION_FILE    "${CMAKE_CURRENT_SOURCE_DIR}/README")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYRIGHT")
set(CPACK_PACKAGE_VERSION_MAJOR ${V_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${V_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${V_PATCH})
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${PROJECT_NAME}-${V_MAJOR}.${V_MINOR}")

set(CPACK_SOURCE_IGNORE_FILES "/CVS/;/.svn/;.swp$;.#;/#;.*~;cscope.*;build.*")

IF(WIN32 AND NOT UNIX)
  set(CPACK_GENERATOR "NSIS")
  set(CPACK_NSIS_PACKAGE_NAME "Gridlicense Demo Tools")
  # set(CPACK_PACKAGE_EXECUTABLES "gui4" "Grid License Demo Frontend")
  set(CPACK_PACKAGE_DEFAULT_LOCATION ${INSTALL_DIR})

  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\gui4.exe")
  set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} Gridlicense Demo Tools")
  set(CPACK_NSIS_HELP_LINK    "http://www.itwm.fraunhofer.de/")
  set(CPACK_NSIS_URL_INFO_ABOUT "http://www.itwm.fraunhofer.de/abteilungen/hpc.html")
  set(CPACK_NSIS_CONTACT "Kai Krueger <kai.krueger@itwm.fhg.de>")
  set(CPACK_NSIS_MODIFY_PATH ON)
  # set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "${CMAKE_PROJECT_NAME}-${EWS_VERSION}")
  # set(CPACK_NSIS_MENU_LINKS "${EWS_WEBSITE}" "Homepage for ${EWS_APP_NAME}")
  set(CPACK_NSIS_DEFINES "  !define MUI_WELCOMEPAGE_TITLE_3LINES 1
    !define MUI_FINISHPAGE_TITLE_3LINES 1")
else(WIN32 AND NOT UNIX)
  #set(CPACK_SET_DESTDIR On)
  set(CPACK_SOURCE_STRIP_FILES "")
  set(CPACK_GENERATOR "TGZ;TBZ2;TZ;DEB;RPM")
  set(CPACK_RPM_PACKAGE_RELOCATABLE 1)
endif(WIN32 AND NOT UNIX)
include(CPack)

