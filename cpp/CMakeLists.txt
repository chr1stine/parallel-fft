cmake_minimum_required(VERSION 2.6)

project(FFT)

# Locate GTest
find_package(GTest REQUIRED)
# # Link runTests with what we want to test and the GTest and pthread library
add_executable(runUnitTests tests/unit.cpp src/fft.cpp src/utils.cpp)
target_link_libraries(runUnitTests ${GTEST_LIBRARIES} pthread)

# Locate benchmark
find_package(benchmark REQUIRED)
add_executable(runPerfomanceTests tests/perf.cpp src/fft.cpp src/utils.cpp)
target_link_libraries(runPerfomanceTests benchmark::benchmark pthread)

# build main
add_executable(main src/main.cpp src/fft.cpp src/utils.cpp)
target_link_libraries(main pthread)