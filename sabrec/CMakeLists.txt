cmake_minimum_required(VERSION 3.16)

add_executable(sabrec
	main.cpp
)

target_link_libraries(sabrec
	MoustaphaSaad::sabre
)