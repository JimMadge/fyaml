project(yaml_module Fortran)
cmake_minimum_required(VERSION 3.0)
include(GNUInstallDirs)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

set(CMAKE_Fortran_FLAGS_Debug "-Wall -g -fbacktrace")
set(CMAKE_Fortran_FLAGS_Release "-O3")
If(CMAKE_BUILD_TYPE MATCHES "Debug")
        message("Debug build")
        message("CMAKE_Fortran_FLAGS_Debug is ${CMAKE_Fortran_FLAGS_Debug}")
endif()
If(CMAKE_BUILD_TYPE MATCHES "Release")
        message("Release build")
        message("CMAKE_Fortran_FLAGS_Release is ${CMAKE_Fortran_FLAGS_Release}")
endif()

add_subdirectory(src)
