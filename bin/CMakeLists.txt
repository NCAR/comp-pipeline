configure_file(comp_verify_dates.sh.in comp_verify_dates.sh @ONLY)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/comp_verify_dates.sh DESTINATION bin)
install(PROGRAMS comp_verify.sh DESTINATION bin)
