cmake_minimum_required(VERSION 3.16)

add_executable(sabre_playground
	playground.cpp
)

target_link_libraries(sabre_playground
	MoustaphaSaad::mn
)