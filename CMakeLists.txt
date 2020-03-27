cmake_minimum_required(VERSION 3.0)

project(calc)

file(GLOB SOURCES LIST_DIRECTORIES false *.cpp)
set (CMAKE_CXX_STANDARD 17)
add_executable(${PROJECT_NAME} ${SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE
${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
