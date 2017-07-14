file(GLOB SH_FILES "*.sh")
install(PROGRAMS
          ${SH_FILES}
        DESTINATION scripts)
