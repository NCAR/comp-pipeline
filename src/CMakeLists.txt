add_subdirectory(common)
add_subdirectory(level1)
add_subdirectory(level2)

configure_file(comp_find_code_version.pro.in comp_find_code_version.pro)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/comp_find_code_version.pro DESTINATION src)

file(GLOB PRO_FILES "*.pro")
install(FILES ${PRO_FILES} DESTINATION src)

file(GLOB SAV_FILES "*.sav")
install(FILES ${SAV_FILES} DESTINATION src)

install(FILES epochs.cfg DESTINATION src)
