configure_file(comp_verify_dates.sh.in comp_verify_dates.sh @ONLY)
configure_file(comp_run_pipeline.sh.in comp_run_pipeline.sh @ONLY)
configure_file(comp.in comp @ONLY)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/comp_verify_dates.sh
		 ${CMAKE_CURRENT_BINARY_DIR}/comp_run_pipeline.sh
                 ${CMAKE_CURRENT_BINARY_DIR}/comp
        DESTINATION bin)
install(PROGRAMS comp_verify.sh DESTINATION bin)
