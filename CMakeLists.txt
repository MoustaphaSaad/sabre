cmake_minimum_required(VERSION 3.16)

project(sabre LANGUAGES CXX)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/$<CONFIG>")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/$<CONFIG>")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/$<CONFIG>")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_INSTALL_RPATH "$ORIGIN")
set(CMAKE_BUILD_RPATH "$ORIGIN")
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

option(SABRE_BUILD_EXAMPLES    "Build example applications and playgrounds that showcase the sabre library." ON)
option(SABRE_BUILD_TESTS       "Build sabre unit tests."                                                     ON)
option(SABRE_UNITY_BUILD       "Combine all sabre source files into one jumbo build."                        ON)

option(MN_BUILD_EXAMPLES OFF)
option(MN_BUILD_TESTS OFF)
add_subdirectory(external/mn EXCLUDE_FROM_ALL)

add_subdirectory(sabre)

if (SABRE_BUILD_EXAMPLES)
	add_subdirectory(playground)
endif (SABRE_BUILD_EXAMPLES)