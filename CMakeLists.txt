cmake_minimum_required(VERSION 3.21)
project(clox C)

set(CMAKE_C_STANDARD 99)

add_executable(clox main.c common.h chunk.h chunk.c memory.h memory.c debug.h debug.c value.h value.c vm.h vm.c)
