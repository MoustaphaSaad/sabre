cmake_minimum_required(VERSION 3.16)

set(MASTER_PROJECT OFF)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
	set(MASTER_PROJECT ON)
endif ()

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

option(SABRE_BUILD_EXAMPLES    "Build example applications and playgrounds that showcase the sabre library." ${MASTER_PROJECT})
option(SABRE_BUILD_TESTS       "Build sabre unit tests."                                                     ${MASTER_PROJECT})
option(SABRE_LOG_METRICS       "Measures and logs different performance metrics."                            ${MASTER_PROJECT})
option(SABRE_INSTALL           "Generates the install target"                                                ${MASTER_PROJECT})

# Treat all compiler warnings as errors and increase warning level.
if(MSVC)
    add_compile_options(-W4 -WX)
endif()

# external dependencies
include(CPM)
CPMAddPackage(
	NAME             mn
	GIT_REPOSITORY   https://github.com/MoustaphaSaad/mn.git
	GIT_TAG          v5.2.0
)

add_subdirectory(std)
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

include(InstallRequiredSystemLibraries)
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
include(CPack)