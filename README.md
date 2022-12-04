# PROFOIL

PROFOIL is a computer program for the inverse design of isolated airfoils.  

## Compiling PROFOIL and Testing

The script _profoil-gfortran.sh_ in the source directory **./src** can
be run to compile the code 
and run a series of test cases.  The results are output to the
**./src/test/results-gfortran** folder that can be compared with the
output files in this repository.  The script can be run using:

```sh
source profoil-gfortran.sh
```

## User Guide

The user guide for PROFOIL is here **./doc/PROFOIL-User-Guide-2022.pdf**.

## PROFOIL-UI

Complete the installation of the User Interface (UI) by following the install instructions for PROFOIL-UI here:

https://github.com/kjayawar/PROFOIL-UI

## License

PROFOIL is released under the MIT License.  Three subroutines (draw, qip, and thicke) were adapted from NASA TM-80210.
