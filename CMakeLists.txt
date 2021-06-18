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

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

option(SABRE_BUILD_EXAMPLES    "Build example applications and playgrounds that showcase the sabre library." ON)
option(SABRE_BUILD_TESTS       "Build sabre unit tests."                                                     ON)
option(SABRE_UNITY_BUILD       "Combine all sabre source files into one jumbo build."                        ON)

# external dependencies
include(CPM)
set(MN_BUILD_EXAMPLES OFF CACHE INTERNAL "")
set(MN_BUILD_TESTS OFF CACHE INTERNAL "")
CPMAddPackage(
	NAME             mn
	GIT_REPOSITORY   https://github.com/MoustaphaSaad/mn.git
	GIT_TAG          13c8afb86c75f53b3ad99674670a9c82d59a742d
	GIT_SHALLOW      TRUE
	EXCLUDE_FROM_ALL TRUE
)

add_subdirectory(sabre)
add_subdirectory(sabrec)

if (SABRE_BUILD_EXAMPLES)
	add_subdirectory(playground)
endif (SABRE_BUILD_EXAMPLES)

if (SABRE_BUILD_TESTS)
	CPMAddPackage(
		NAME             doctest
		GIT_REPOSITORY   https://github.com/onqtam/doctest.git
		GIT_TAG          2.4.6
		GIT_SHALLOW      TRUE
		EXCLUDE_FROM_ALL TRUE
	)
	add_subdirectory(unittest)
endif (SABRE_BUILD_TESTS)