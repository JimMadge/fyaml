set(SOURCES
yaml.F90
example.F90
)
add_executable(yaml ${SOURCES})

install(TARGETS yaml DESTINATION ${CMAKE_INSTALL_BINDIR})
