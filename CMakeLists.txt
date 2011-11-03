##
##  CMakeFile for GWtool
##  AF, 27/10/2011
##
##  To compile, from the directory that contains this file, do:
##    $ mkdir build; cd build
##    $ cmake ..
##    $ make
##


cmake_minimum_required( VERSION 2.6 FATAL_ERROR )

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE Release CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE )
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( GWtool Fortran )


# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )


# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/mod" )
set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/bin" )



# Find dependencies:
find_package( LibSUFR REQUIRED )
set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES}" )  # will be transferred to CompilerFlags



# Set source files:
#include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )


# Create the file code_version.f90, which contains the svn version number and date:
if( CREATE_VERSION )
  
  # Code version generator:
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_SOURCE_DIR}/src/code_version.f90
    COMMAND cd $(CMAKE_SOURCE_DIR)
    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh $(CMAKE_SOURCE_DIR) ${Fortran_COMPILER_NAME}
    )
  
  # Tell CMake the source won't be available until build time:
  SET_SOURCE_FILES_PROPERTIES( ${CMAKE_SOURCE_DIR}/src/code_version.f90 PROPERTIES GENERATED 1 )

endif( CREATE_VERSION )




# Create executable targets:
add_executable(        isco  src/isco.f90 )
target_link_libraries( isco  ${LibSUFR_LIBRARIES} )

add_executable(        m1m2-mceta  src/m1m2-mceta.f90 )
target_link_libraries( m1m2-mceta  ${LibSUFR_LIBRARIES} )

add_executable(        mceta-m1m2  src/mceta-m1m2.f90 )
target_link_libraries( mceta-m1m2  ${LibSUFR_LIBRARIES} )

add_executable(        tcoal  src/tcoal.f90 )
target_link_libraries( tcoal  ${LibSUFR_LIBRARIES} )



# Set module directory:
set_target_properties( isco m1m2-mceta mceta-m1m2 tcoal
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )


# Install to destination:
install( TARGETS isco m1m2-mceta mceta-m1m2 tcoal RUNTIME DESTINATION bin )

# - use DIRECTORY rather than TARGETS so that I don't need to specify them all again...
#install( DIRECTORY usr/bin/ 
#    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE 
#    DESTINATION ${CMAKE_INSTALL_PREFIX}/bin )


