# Redstart
![redstart](Phoenicurus-phoenicurus.jpg)
A Fortran library for general use.

## Structure

This library is composed of multiple files containing a module each, all of which will have a name starting with red_.
The name of the file will be the same as the module with .f90 appended at the end.


The standard method of distribution for this library is one amalgamated file: redstart.f90. This reflects the design
of this library, the goal is to make something that is very simple for end users to simply copy and paste into their
project.

## Using Redstart

1. Clone the repo.
2. Run build.sh.
3. Copy the amalgamted file (redstart.f90) found in build/ to your project.
4. Add the modules to your source files for import.
5. Point your compiler at the redstart.f90 source file during compilation.
6. There is no step 6.

## Modules

### red_errors

Provides the type red_error that is used for error reporting in different modules of this system.

### red_vector

This is a module which provides vectors for fortran primitives (e.g. logical, integer, real, ...).

The original source is in the `src/templates` directory, as this uses fypp to generate the same vector
interface for different types.

### red_random

This module provides an ergonomic wrapper around the default rng found in Fortran. It allows for specification
of custom ranges, and allows for generation of random integers as well.

This program is an fypp template, as it generates shuffling functions for each of the different types of vectors
available from red_vector.

### red_string

Gives a series of string utilities for convenience.

### red_ioutil

Convenient wrapper around basic input and output, to screen or file.

### red_datetime

Module for working with dates and times. Can handle a few different formats,
date arithmetic, and stopwatch functionality.

### red_logger

Basic logging module, pulls in red_datetime for timestamps. Capable of generating multiple logs that can write
to either console, file, or both at different log levels.
