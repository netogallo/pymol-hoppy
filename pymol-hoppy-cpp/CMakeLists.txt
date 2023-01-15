cmake_minimum_required(VERSION 3.10)

#Name your project here
project(pymol-hoppy-cpp)

set(CMAKE_CXX_COMPILER "g++")
#set( CMAKE_CXX_STANDARD 17 )
#set( CMAKE_CXX_STANDARD_REQUIRED OFF )
#set( CMAKE_POSITION_INDEPENDENT_CODE ON )

# string(
#   REGEX REPLACE "-std=c++11"
#   ""
#   CMAKE_CXX_FLAGS
#   ${CMAKE_CXX_FLAGS}
# )

add_compile_options(-O2 -dynamic -shared -fPIC)

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

add_library(pymol SHARED IMPORTED)
set_property(TARGET pymol PROPERTY IMPORTED_LOCATION ${PYMOL_CMD})

# find_program(
#   GHC_PKG
#   ghc-pkg
# )

# execute_process(
#   COMMAND ${GHC_PKG} field ghc include-dirs
#   OUTPUT_VARIABLE GHC_INCLUDE
# )

# string(
#   REGEX REPLACE "include-dirs:[ \t\r\n]*"
#   ""
#   GHC_INCLUDE
#   ${GHC_INCLUDE}
# )

set(GHC_INCLUDE "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/include/")

string(
  REGEX REPLACE "\n"
  ""
  GHC_INCLUDE
  ${GHC_INCLUDE}
)

find_path(
  PYMOL_LAYER0
  PyMOLGlobals.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES layer0
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_LAYER1
  P.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES layer1
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_LAYER2
  AtomInfo.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES layer2
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_LAYER3
  AtomIterators.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES layer3
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_LAYER4
  Cmd.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES layer4
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_LAYER5
  PyMOL.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES layer5
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_OV
  ov_types.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES ov/src
  NO_DEFAULT_PATH
  REQUIRED
)

find_path(
  PYMOL_INCLUDE
  pymol/functional.h
  PATHS ENV PYMOL_SRC_DIR
  PATH_SUFFIXES include
  NO_DEFAULT_PATH
  REQUIRED
)

add_library(
  HsPymolCpp SHARED
  cpp/gen/layer3/gen_AtomIterators.cpp
  cpp/gen/gen_PymolSupport.cpp
  cpp/gen/gen_std.cpp
  cpp/Support.cpp
)

set_target_properties(HsPymolCpp PROPERTIES LINKER_LANGUAGE CXX)

target_include_directories(
  HsPymolCpp
  PUBLIC "cpp/" ${GHC_INCLUDE} ${PYMOL_LAYER0} ${PYMOL_LAYER1} ${PYMOL_LAYER2} ${PYMOL_LAYER3} ${PYMOL_LAYER4} ${PYMOL_LAYER5} ${PYMOL_OV} ${PYMOL_INCLUDE}
)

target_link_libraries(
  HsPymolCpp
  pymol
)

install(
  TARGETS HsPymolCpp
)


message(PYMOL_CMD="${PYMOL_CMD}")
message(Python3_SITELIB="${Python3_SITELIB}")
message(Python3_VERSION="${Python3_VERSION}")
message(GHC="${GHC}")
message(GHC_INCLUDE="${GHC_INCLUDE}")
message(PYMOL_LAYER0="${PYMOL_LAYER0}")
message(COMPILE_OPTIONS="${COMPILE_OPTIONS}")
