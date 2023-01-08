cmake_minimum_required(VERSION 3.10)

#Name your project here
project(pymol-hoppy-cpp)

find_package(
  Python3
  COMPONENTS Interpreter Development
)

set(CMAKE_FIND_LIBRARY_PREFIXES "_" ${CMAKE_FIND_LIBRARY_PREFIXES})
set(CMAKE_FIND_LIBRARY_SUFFIXES ".cpython-${Python3_VERSION_MAJOR}${Python3_VERSION_MINOR}-x86_64-linux-gnu.so" ${CMAKE_FIND_LIBRARY_SUFIXES})

message(CMAKE_FIND_LIBRARY_PREFIXES="${CMAKE_FIND_LIBRARY_PREFIXES}")
message(CMAKE_FIND_LIBRARY_SUFFIXES="${CMAKE_FIND_LIBRARY_SUFFIXES}")

find_library(
  PYMOL_CMD
  cmd
  PATHS "${Python3_SITELIB}"
  PATH_SUFFIXES "pymol"
  NO_DEFAULT_PATH
  REQUIRED
)
	
message(PYMOL_CMD="${PYMOL_CMD}")
message(Python3_SITELIB="${Python3_SITELIB}")
message(Python3_VERSION="${Python3_VERSION}")
