# Redstart
A Fortran library for general use.

## Structure

This library is composed of multiple files containing a module each, all of which will have a name starting with red_.
The name of the file will be the same as the module with .f90 appended at the end.

The standard method of distribution for this library is one amalgamated file: redstart.f90. This reflects the design
of this library, the goal is to make something that is very simple for end users to simply copy and paste into their
project.

## Using Redstart

1. Copy the amalgamted file (redstart.f90) found in build/ to your project
2. Add red_* modules to your source files for import
3. Point your compiler at the redstart.f90 source file during compilation
4. There is no step 4.

## Modules

### red_vector

This is a module which provides vectors for fortran primitives (e.g. logical, integer, real, ...).



