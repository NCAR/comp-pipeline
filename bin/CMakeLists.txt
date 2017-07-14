configure_file(comp_verify.sh.in comp_verify.sh @ONLY)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/comp_verify.sh DESTINATION bin)
