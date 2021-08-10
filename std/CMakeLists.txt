cmake_minimum_required(VERSION 3.19)

add_custom_target(
	sabre_std
	COMMAND ${CMAKE_COMMAND} -E copy_directory
			${CMAKE_CURRENT_SOURCE_DIR}/std/
			${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/std
)