cmake_minimum_required(VERSION 3.0)

project(fluidbox LANGUAGES Fortran)

if (DEFINED ENV{NETCDF} AND (NOT DEFINED ENV{NETCDF_ROOT}))
  set(ENV{NETCDF_ROOT} $ENV{NETCDF})
endif ()
if (DEFINED ENV{NETCDF_ROOT})
  include_directories("$ENV{NETCDF_ROOT}/include")
  link_directories("$ENV{NETCDF_ROOT}/lib")
else ()
  find_package(PkgConfig)
  if (PKG_CONFIG_FOUND)
    pkg_search_module(NETCDF REQUIRED netcdf)
    if (NETCDF_FOUND)
      include_directories(${NETCDF_INCLUDEDIR})
      link_directories(${NETCDF_LIBDIR})
    else ()
      message(FATAL_ERROR "Pkg-config could not find netcdf library!")
    endif ()
  else ()
    message(FATAL_ERROR "Unable to find pkg-config library!")
  endif ()
endif ()

add_subdirectory(lib/flogger)
add_subdirectory(lib/string)
add_subdirectory(lib/container)
add_subdirectory(lib/fiona)

set(source_files
  src/const_mod.F90
  src/namelist_mod.F90
  src/formula_mod.F90
  src/fdm/mesh_mod.F90
  src/fdm/static_mod.F90
  src/fdm/state_mod.F90
  src/fdm/test_mod.F90
  src/fdm/vert_coord/vert_coord_mod.F90
  src/fdm/vert_coord/hybrid_coord_mod.F90
  src/fdm/vert_coord/hybrid_coord_test_mod.F90
  src/fdm/vert_coord/hybrid_coord_ecmwf_mod.F90
  src/fdm/vert_coord/sigma_coord_mod.F90
  src/tests/schar_mountain_test_mod.F90
)

add_library(fluidbox ${source_files})
target_link_libraries(fluidbox
  flogger
  fortran_string
  fortran_container
  fiona
  netcdff
)
