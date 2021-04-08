cmake_minimum_required(VERSION 3.16)

# list the header files
set(HEADER_FILES
	include/sabre/Dummy.h
)

# list the source files
set(SOURCE_FILES
	src/sabre/Dummy.cpp
)

add_library(sabre)
add_library(MoustaphaSaad::sabre ALIAS sabre)

target_sources(sabre
	PRIVATE ${HEADER_FILES}
			${SOURCE_FILES})

if (SABRE_UNITY_BUILD)
	set_target_properties(sabre
		PROPERTIES UNITY_BUILD_BATCH_SIZE 0
				   UNITY_BUILD true)
endif()

target_link_libraries(sabre
	PUBLIC
		MoustaphaSaad::mn
)

# enable C++17
# disable any compiler specifc extensions
# add d suffix in debug mode
target_compile_features(sabre PUBLIC cxx_std_17)
set_target_properties(sabre PROPERTIES
	CXX_EXTENSIONS OFF
	DEBUG_POSTFIX d
)

# generate exports header file
include(GenerateExportHeader)
generate_export_header(sabre
	EXPORT_FILE_NAME ${CMAKE_CURRENT_SOURCE_DIR}/include/sabre/Exports.h
)

# list include directories
target_include_directories(sabre
	PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}/include
	PRIVATE
	${CMAKE_CURRENT_SOURCE_DIR}/src
)
