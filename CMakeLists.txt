cmake_minimum_required(VERSION 2.8.11)
enable_testing()

project(FortranSerialization Fortran)

set(TARGET     ${CMAKE_CURRENT_BINARY_DIR}/serialization.f90)
set(SOURCE     ${CMAKE_CURRENT_SOURCE_DIR}/src/serialization.f90)
set(TARGET_LIB serialization)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR})

add_custom_command(
  OUTPUT  ${TARGET}
  COMMAND ${CMAKE_Fortran_COMPILER} -E -P -cpp ${SOURCE} -o ${TARGET}
  COMMAND sed -i -e 's/\;\;/\\n/g' ${TARGET}
  DEPENDS ${SOURCE}
  )
add_custom_target(generate_module SOURCES ${TARGET})
add_library(${TARGET_LIB} ${TARGET})

add_subdirectory(test)
add_test(NAME save_load_test  COMMAND $<TARGET_FILE:xtest1>)
add_test(NAME serialize_test  COMMAND $<TARGET_FILE:xtest2>)
