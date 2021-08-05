cmake_minimum_required(VERSION 3.16)

add_executable(sabrec
	main.cpp
)

target_link_libraries(sabrec
	MoustaphaSaad::sabre
)

add_dependencies(sabrec sabre_std)

if (SABRE_INSTALL)
	install(TARGETS sabrec)
	install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../std DESTINATION "${CMAKE_INSTALL_BINDIR}")

	# God i hate cmake
	# install(CODE [[
	# 	file(GET_RUNTIME_DEPENDENCIES
	# 		EXECUTABLES $<TARGET_FILE:sabrec>
	# 		RESOLVED_DEPENDENCIES_VAR resolved_deps
	# 		UNRESOLVED_DEPENDENCIES_VAR unresolved_deps
	# 		DIRECTORIES "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
	# 		PRE_EXCLUDE_REGEXES
	# 			"api-ms-*"
	# 		POST_EXCLUDE_REGEXES
	# 			".*system32/.*\\.dll"
	# 	)
	# 	foreach(resolved_file ${resolved_deps})
	# 		file(INSTALL
	# 			DESTINATION "${CMAKE_INSTALL_PREFIX}/bin"
	# 			TYPE SHARED_LIBRARY
	# 			FILES "${resolved_file}"
	# 		)
	# 	endforeach()
	# 	foreach(unresolved_file ${unresolved_deps})
	# 		message(WARNING "Unresolved dependency: ${unresolved_file}")
	# 	endforeach()
	# ]])
endif (SABRE_INSTALL)