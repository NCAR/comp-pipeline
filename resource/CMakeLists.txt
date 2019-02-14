add_subdirectory(atlas)

file(GLOB CONTINUUM_CORRECTION_FILES "wave_cal_*.txt")
install(FILES ${CONTINUUM_CORRECTION_FILES} DESTINATION resource)
