cmake_minimum_required(VERSION 3.16)

add_executable(sabrec
	main.cpp
)

target_link_libraries(sabrec
	MoustaphaSaad::sabre
)

get_target_property(SABREC_OUTPUT_DIR sabrec RUNTIME_OUTPUT_DIRECTORY)
add_custom_target(
	deploy_std
	COMMAND ${CMAKE_COMMAND} -E copy_directory
			${CMAKE_CURRENT_SOURCE_DIR}/../std/
			${SABREC_OUTPUT_DIR}/std/
)
add_dependencies(sabrec deploy_std)

if (SABRE_INSTALL)
	install(TARGETS sabrec)
	install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../std DESTINATION "${CMAKE_INSTALL_BINDIR}")
endif (SABRE_INSTALL)