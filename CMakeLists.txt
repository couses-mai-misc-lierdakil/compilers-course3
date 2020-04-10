cmake_minimum_required(VERSION 3.0)

project(calc)

file(COPY grammar.xy DESTINATION ${CMAKE_BINARY_DIR})

ADD_CUSTOM_COMMAND(OUTPUT lexer.cpp lexer.h parser.cpp parser.h
  tokenType.h parser.txt dfa.gv nfa.gv
COMMAND parser-generator grammar.xy
DEPENDS grammar.xy)

file(GLOB SOURCES LIST_DIRECTORIES false *.cpp)
set (CMAKE_CXX_STANDARD 17)
add_executable(${PROJECT_NAME} ${SOURCES} lexer.cpp parser.cpp)
target_include_directories(${PROJECT_NAME} PRIVATE
${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
