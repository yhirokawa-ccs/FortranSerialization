cmake_minimum_required(VERSION 2.8.11)

project(FortranSerialization)

set(TARGET ${CMAKE_CURRENT_BINARY_DIR}/serialization.f90)
set(SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/src/serialization.f90)

add_custom_command(
  OUTPUT  ${TARGET}
  COMMAND ${CMAKE_C_COMPILER} -E -P -cpp ${SOURCE} -o ${TARGET}
  COMMAND sed -i -e 's/\;\;/\\n/g' ${TARGET}
  DEPENDS ${SOURCE}
  )

add_custom_target(generate_module ALL SOURCES ${TARGET})
