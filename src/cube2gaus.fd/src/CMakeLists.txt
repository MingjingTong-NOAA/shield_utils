set(lib_src
    lib/fv3_da_out_api.F90
    lib/c2l/affinity.c
    lib/c2l/cub2latlon.F90
    lib/c2l/fill_corner.F90
    lib/c2l/flow_proj.F90
    lib/c2l/fv_timing.F90
    lib/c2l/ghost_cubsph.F90
    lib/c2l/grid_utils.F90
    lib/core/fv3_da_ctrl.F90
    lib/core/fv3_da.F90
    lib/core/interp_res_ncio.F90
    lib/core/interp_res_nemsio.F90
    lib/util/io_nc_util.F90)

add_library(libdaio ${lib_src})

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -i4 -fp-model precise")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
endif()

set(exe_name fv3_c2g_atms.x)
add_executable(${exe_name} main.F90)

target_include_directories(libdaio PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(
  libdaio
  PUBLIC
  NetCDF::NetCDF_Fortran
  nemsio::nemsio)

target_link_libraries(${exe_name} PRIVATE libdaio)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
