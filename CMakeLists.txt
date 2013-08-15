##
##  CMakeFile for GWtool
##  AF, 2011-10-27
##
##  Copyright 2007-2013 AstroFloyd - astrofloyd.org
##  
##  This file is part of the GWtool package,
##  see: http://gwtool.sf.net/
##  
##  GWtool is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  GWtool is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with GWtool.  If not, see <http://www.gnu.org/licenses/>.
##
##  To install this software on your system, see the INSTALL file
##  in the root or doc/ directory of this package.
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
#set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib${LIB_SUFFIX}" )
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
add_executable(        binary_contact_gw  src/binary_contact_gw.f90 )
target_link_libraries( binary_contact_gw  ${LibSUFR_LIBRARIES} )

add_executable(        eta2q  src/eta2q.f90 )
target_link_libraries( eta2q  ${LibSUFR_LIBRARIES} )

add_executable(        isco  src/isco.f90 )
target_link_libraries( isco  ${LibSUFR_LIBRARIES} )

add_executable(        ligo_mcmc_injection  src/ligo_mcmc_injection.f90 )
target_link_libraries( ligo_mcmc_injection  ${LibSUFR_LIBRARIES} )

add_executable(        ligo_xml2screen  src/ligo_xml2screen.f90 )
target_link_libraries( ligo_xml2screen  ${LibSUFR_LIBRARIES} )

add_executable(        m1m2-mceta  src/m1m2-mceta.f90 )
target_link_libraries( m1m2-mceta  ${LibSUFR_LIBRARIES} )

add_executable(        match_wfs  src/match_wfs.f90 src/wf_routines.f90 )
target_link_libraries( match_wfs  ${LibSUFR_LIBRARIES} )

add_executable(        mceta-m1m2  src/mceta-m1m2.f90 )
target_link_libraries( mceta-m1m2  ${LibSUFR_LIBRARIES} )

add_executable(        q2eta  src/q2eta.f90 )
target_link_libraries( q2eta  ${LibSUFR_LIBRARIES} )

add_executable(        tcoal  src/tcoal.f90 )
target_link_libraries( tcoal  ${LibSUFR_LIBRARIES} )



# Set module directory:
set_target_properties( binary_contact_gw eta2q isco ligo_mcmc_injection ligo_xml2screen m1m2-mceta match_wfs mceta-m1m2 q2eta tcoal
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )


# Install to destination:
install( TARGETS       binary_contact_gw eta2q isco ligo_mcmc_injection ligo_xml2screen m1m2-mceta match_wfs mceta-m1m2 q2eta tcoal
  RUNTIME DESTINATION bin )

