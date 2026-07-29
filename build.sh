#! /bin/sh
# This creates the standalone fortran source file
# assumes you have uv installed
# assumes you are running this script from the project root

# ensure that the build directory exists
mkdir -p build/staging

# Converts fypp sources to f90
uvx fypp src/templates/red_vector.fypp build/staging/red_vector.f90
uvx fypp src/templates/red_random.fypp build/staging/red_random.f90


cat build/staging/red_vector.f90 build/staging/red_random.f90 src/red_string.f90 > build/redstart.f90
