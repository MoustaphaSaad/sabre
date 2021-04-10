cmake_minimum_required(VERSION 3.16)

# list source files
set(SOURCE_FILES
	unittest_main.cpp
	unittest_sabre.cpp
)

# add executable target
add_executable(unittest
	${SOURCE_FILES}
)

target_link_libraries(unittest
	PRIVATE
		MoustaphaSaad::sabre
		doctest
)

target_compile_definitions(unittest
	PRIVATE
	DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/data"
)