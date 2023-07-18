#!/bin/bash
#

currentDate=`date`

#======================================================
# The test directory should exist and include PROFOIL .in files to run.
mkdir -p test

#======================================================
# Build profoil using gfortran
mkdir -p temp
rm -vf temp/*.o temp/*.f temp/*.inc
cp -avf *.f PROFOIL.INC temp/
cd temp/
pwd
for FILE in `ls -1 *.f`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
gfortran -o profoil *.o
rm -vf *.o
rm -vf *.f
rm -vf *.INC
mv -vf profoil ../test
cd ../
pwd
rmdir temp
# The build above does create some fortran "Warnings" for draw.f and thicke.f.


#======================================================
# Run profoil for all files in the test directory.
# Use the output to create "profoil_test_*" files for comparison.
cd test/
mkdir -p results-gfortran
pwd
rm -vf results-gfortran/profoil_test_long.txt
touch  results-gfortran/profoil_test_long.txt
rm -vf results-gfortran/profoil_test_short.txt
touch  results-gfortran/profoil_test_short.txt
rm -vf results-gfortran/profoil_test_tra.txt
touch  results-gfortran/profoil_test_tra.txt

echo $currentDate >> results-gfortran/profoil_test_long.txt
echo $currentDate >> results-gfortran/profoil_test_short.txt
echo $currentDate >> results-gfortran/profoil_test_tra.txt

TRAFILE=profoil_tra.txt
for FILE in `ls -1 *.in`;
do
  cp -avf $FILE profoil.in
  dos2unix profoil.in profoil.in
  ./profoil | tee profoil_out.txt
  echo "************************************"            >> results-gfortran/profoil_test_long.txt
  echo "Running file: $FILE"                             >> results-gfortran/profoil_test_long.txt
  echo "************************************"            >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt
  cat -n profoil.in                                      >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt
  echo "************************************"            >> results-gfortran/profoil_test_long.txt
  echo "Output from file: $FILE"                         >> results-gfortran/profoil_test_long.txt
  echo "************************************"            >> results-gfortran/profoil_test_long.txt
  cat profoil_out.txt                                    >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt
  echo "============================================"    >> results-gfortran/profoil_test_long.txt
  echo "Dump File: $FILE"                                >> results-gfortran/profoil_test_long.txt
  echo "============================================"    >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt
  cat profoil.dmp                                        >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt
  echo "============================================"    >> results-gfortran/profoil_test_long.txt
  echo "Coordinates: $FILE"                              >> results-gfortran/profoil_test_long.txt
  echo "============================================"    >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt
  cat fort.95                                            >> results-gfortran/profoil_test_long.txt
  echo " "                                               >> results-gfortran/profoil_test_long.txt

  echo "============================================"    >> results-gfortran/profoil_test_short.txt
  echo "Dump File: $FILE"                                >> results-gfortran/profoil_test_short.txt
  echo "============================================"    >> results-gfortran/profoil_test_short.txt
  echo " "                                               >> results-gfortran/profoil_test_short.txt
  cat profoil.dmp                                        >> results-gfortran/profoil_test_short.txt
  echo " "                                               >> results-gfortran/profoil_test_short.txt
  echo "============================================"    >> results-gfortran/profoil_test_short.txt
  echo "Coordinates: $FILE"                              >> results-gfortran/profoil_test_short.txt
  echo "============================================"    >> results-gfortran/profoil_test_short.txt
  echo " "                                               >> results-gfortran/profoil_test_short.txt
  cat fort.95                                            >> results-gfortran/profoil_test_short.txt
  echo " "                                               >> results-gfortran/profoil_test_short.txt

  if [ -f "$TRAFILE" ]; then
	  echo "************************************"    >> results-gfortran/profoil_test_tra.txt
	  echo "Running file: $FILE"                     >> results-gfortran/profoil_test_tra.txt
	  echo "************************************"    >> results-gfortran/profoil_test_tra.txt
	  echo " "                                       >> results-gfortran/profoil_test_tra.txt
	  echo "************************************"    >> results-gfortran/profoil_test_tra.txt
	  echo "Output from file: profoil_tra.txt   "    >> results-gfortran/profoil_test_tra.txt
	  echo "Eppler style output                 "    >> results-gfortran/profoil_test_tra.txt
	  echo "************************************"    >> results-gfortran/profoil_test_tra.txt
	  echo " "                                       >> results-gfortran/profoil_test_tra.txt
	  cat profoil_tra.txt                            >> results-gfortran/profoil_test_tra.txt
	  echo " "                                       >> results-gfortran/profoil_test_tra.txt
	  echo "************************************"    >> results-gfortran/profoil_test_tra.txt
  fi

  rm -vf profoil_out.txt
  rm -vf profoil_tra.txt
  rm -vf profoil.in
  rm -vf fort.95
done

#======================================================
# clean up files not being used
rm -vf profoil.in
rm -vf profoil.dmp
rm -vf profoil.vel
rm -vf profoil.xy
rm -vf profoil_out.txt
rm -vg profoil_tra.txt
rm -vf fort.20
rm -vf fort.30
rm -vf fort.50
rm -vf fort.70
rm -vf fort.80
rm -vf fort.90
rm -vf fort.95
cd ../
pwd
echo $currentDate
echo "Done"
