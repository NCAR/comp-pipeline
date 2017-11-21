configure_file(comp_verify_dates.sh.in comp_verify_dates.sh @ONLY)
configure_file(comp_run_pipeline.sh.in comp_run_pipeline.sh @ONLY)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/comp_verify_dates.sh
		 ${CMAKE_CURRENT_BINARY_DIR}/comp_run_pipeline.sh
        DESTINATION bin)
install(PROGRAMS comp_verify.sh DESTINATION bin)
