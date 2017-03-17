add_executable(xtest1 test1.f90)
target_link_libraries(xtest1 ${TARGET_LIB})

add_executable(xtest2 test2.f90)
target_link_libraries(xtest2 ${TARGET_LIB})
