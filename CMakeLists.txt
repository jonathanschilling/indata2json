cmake_minimum_required(VERSION 3.4...3.18)

# Define the project name and language
project(indata2json 
    LANGUAGES Fortran
    DESCRIPTION "Converter for VMEC inputs (INDATA namelist) to JSON")

# Set the Fortran compilers and flags
message(STATUS "Detected platform: ${CMAKE_SYSTEM_NAME}")
# needed for, e.g., LIBSTELL/Sources/Miscel/getcarg.f90
if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(FLAGS_PLATFORM "-DLINUX")
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin") # macOS
    set(FLAGS_PLATFORM "-DMACOSX")
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}. Only Linux and macOS are supported.")
endif()

set(CMAKE_Fortran_FLAGS "-cpp -lgfortran ${FLAGS_PLATFORM}")
set(CMAKE_Fortran_FLAG_F77 "-std=legacy")

# Define the sources
set(SOURCES
    json-fortran/json.f90
    LIBSTELL/Sources/Miscel/getcarg.f90
    LIBSTELL/Sources/Miscel/tolower.f90
    LIBSTELL/Sources/Modules/safe_open_mod.f90
    LIBSTELL/Sources/Modules/stel_kinds.f
    LIBSTELL/Sources/Modules/stel_constants.f
    LIBSTELL/Sources/Modules/vparams.f
    LIBSTELL/Sources/Modules/vsvd0.f
    src/vmec_input.f
    src/nonzerolen.f90
    src/indata2json.f90
)

# Create libraries and handle inter-dependencies
add_library(json STATIC json-fortran/json.f90)

add_library(getcarg STATIC LIBSTELL/Sources/Miscel/getcarg.f90)

add_library(tolower STATIC LIBSTELL/Sources/Miscel/tolower.f90)

add_library(safe_open_mod STATIC LIBSTELL/Sources/Modules/safe_open_mod.f90)

add_library(stel_kinds STATIC LIBSTELL/Sources/Modules/stel_kinds.f)
target_compile_options(stel_kinds PRIVATE ${CMAKE_Fortran_FLAG_F77})

add_library(stel_constants STATIC LIBSTELL/Sources/Modules/stel_constants.f)
target_compile_options(stel_constants PRIVATE ${CMAKE_Fortran_FLAG_F77})
target_link_libraries(stel_constants PRIVATE stel_kinds)

add_library(vparams STATIC LIBSTELL/Sources/Modules/vparams.f)
target_compile_options(vparams PRIVATE ${CMAKE_Fortran_FLAG_F77})
target_link_libraries(vparams PRIVATE stel_kinds stel_constants)

add_library(vsvd0 STATIC LIBSTELL/Sources/Modules/vsvd0.f)
target_compile_options(vsvd0 PRIVATE ${CMAKE_Fortran_FLAG_F77})
target_link_libraries(vsvd0 PRIVATE stel_kinds)

add_library(vmec_input STATIC src/vmec_input.f)
target_compile_options(vmec_input PRIVATE ${CMAKE_Fortran_FLAG_F77})
target_link_libraries(vmec_input PRIVATE vparams vsvd0)

add_library(nonzerolen STATIC src/nonzerolen.f90)
target_compile_options(nonzerolen PRIVATE PRIVATE ${CMAKE_Fortran_FLAG_F77})
target_link_libraries(nonzerolen PRIVATE stel_kinds stel_constants)

# Add the final executable
add_executable(indata2json src/indata2json.f90)

# Link the libraries to the executable
target_link_libraries(indata2json
    json
    getcarg
    safe_open_mod
    stel_kinds
    stel_constants
    vparams
    vsvd0
    tolower
    nonzerolen
    vmec_input
)
