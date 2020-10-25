cmake_minimum_required(VERSION 3.0)

project(fluidbox LANGUAGES Fortran)

set(source_files
  src/const_mod.F90
  src/fdm/mesh_mod.F90
  src/fdm/state_mod.F90
)

add_library(fluidbox ${source_files})