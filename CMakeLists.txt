##
##  CMakeFile for GWtools
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
project( GWtools Fortran )


# Use CPack for packaging:
#include( CPack )
#set( CPACK_INSTALL_PREFIX_PATH ${CMAKE_INSTALL_PREFIX_PATH} )

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
include( CompilerFlags )


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

add_executable(        m1_m2-mc_eta  src/m1_m2-mc_eta.f90 )
target_link_libraries( m1_m2-mc_eta  ${LibSUFR_LIBRARIES} )

add_executable(        mc_eta-m1_m2  src/mc_eta-m1_m2.f90 )
target_link_libraries( mc_eta-m1_m2  ${LibSUFR_LIBRARIES} )

add_executable(        tcoal  src/tcoal.f90 )
target_link_libraries( tcoal  ${LibSUFR_LIBRARIES} )



# Set module directory:
set_target_properties( isco m1_m2-mc_eta mc_eta-m1_m2 tcoal
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )


# Install to destination:
# - use DIRECTORY rather than TARGETS so that I don't need to specify them all again...
install( DIRECTORY usr/bin/ 
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE 
    DESTINATION ${CMAKE_INSTALL_PREFIX}/bin )


