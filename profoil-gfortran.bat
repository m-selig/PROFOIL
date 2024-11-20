@echo off
setlocal

echo Creating directories if they do not exist...
if not exist test md test
if not exist temp md temp

echo Navigating to temp directory...
cd temp || (echo Failed to change directory to temp & exit /b 1)

echo Deleting specific files in temp directory...
del /S /Q *.o
del /S /Q *.f
del /S /Q *.inc

echo Navigating back to parent directory...
cd .. || (echo Failed to change directory to parent & exit /b 1)

echo Copying necessary files to temp directory...
copy "*.f" "temp/" || (echo Failed to copy *.f files & exit /b 1)
copy "PROFOIL.INC" "temp/" || (echo Failed to copy PROFOIL.INC & exit /b 1)

echo Navigating to temp directory...
cd temp || (echo Failed to change directory to temp & exit /b 1)

echo Compiling .f files...
for %%f in (*.f) do (
    echo Compiling %%f...
    gfortran -c "%%f" -o "%%~nf.o"
    if errorlevel 1 (
        echo Errors compiling %%f
        exit /b 1
    )
)

echo Linking object files to create executable...
gfortran -o profoil *.o
if errorlevel 1 (
    echo Errors linking object files
    exit /b 1
)

echo Cleaning up object and source files...
del /S /Q *.o
del /S /Q *.f
del /S /Q *.INC

echo Moving executable to test directory...
move /Y profoil.exe ../test || (echo Failed to move profoil.exe & exit /b 1)

echo Navigating back to parent directory...
cd .. || (echo Failed to change directory to parent & exit /b 1)

echo Removing temp directory...
rmdir /S /Q temp

cd test
if not exist results-gfortran md results-gfortran
cd

echo %DATE% %TIME% > "results-gfortran/profoil_test_long.txt"
echo %DATE% %TIME% > "results-gfortran/profoil_test_short.txt"
echo %DATE% %TIME% > "results-gfortran/profoil_test_tra.txt"

set TRAFILE=profoil_tra.txt

for %%F in (*.in) do (
    copy /Y %%F profoil.in
    @REM this line of code below doesn't output the .tra file and .dump file
    @REM only the .dmp and the fort.number file
    @REM need some modification since MS-DOS doesn't have tee command like UNIX
    profoil.exe > profoil_out.txt
    echo ************************************            >> "results-gfortran/profoil_test_long.txt"
    echo Running file: %%F                               >> "results-gfortran/profoil_test_long.txt"
    echo ************************************            >> "results-gfortran/profoil_test_long.txt"
    echo.                                                >> "results-gfortran/profoil_test_long.txt"
    type profoil.in >> "results-gfortran/profoil_test_long.txt" || (echo Failed to locate profoil.in & exit /b 1)
    echo.                                                >> "results-gfortran/profoil_test_long.txt"
    echo ************************************            >> "results-gfortran/profoil_test_long.txt"
    echo Output from file: %%F                           >> "results-gfortran/profoil_test_long.txt"
    echo ************************************            >> "results-gfortran/profoil_test_long.txt"
    type profoil_out.txt >> "results-gfortran/profoil_test_long.txt" || (echo Failed to locate profoil_out.txt & exit /b 1)
    echo.                                                >> "results-gfortran/profoil_test_long.txt"
    echo ============================================    >> "results-gfortran/profoil_test_long.txt"
    echo Dump File: %%F                                  >> "results-gfortran/profoil_test_long.txt"
    echo ============================================    >> "results-gfortran/profoil_test_long.txt"
    echo.                                                >> "results-gfortran/profoil_test_long.txt"
    type profoil.dump >> "results-gfortran/profoil_test_long.txt" || (echo Failed to locate profoil.dump)                                
    echo.                                                >> "results-gfortran/profoil_test_long.txt"
    echo ============================================    >> "results-gfortran/profoil_test_long.txt"
    echo Coordinates: %%F                                >> "results-gfortran/profoil_test_long.txt"
    echo ============================================    >> "results-gfortran/profoil_test_long.txt"
    echo.                                                >> "results-gfortran/profoil_test_long.txt"
    type fort.95 >> "results-gfortran/profoil_test_long.txt" || (echo Failed to locate fort.95 & exit /b 1)   
    echo.                                                >> "results-gfortran/profoil_test_long.txt"

    echo ============================================    >> "results-gfortran/profoil_test_short.txt"
    echo Dump File: %%F                                  >> "results-gfortran/profoil_test_short.txt"
    echo ============================================    >> "results-gfortran/profoil_test_short.txt"
    echo.                                                >> "results-gfortran/profoil_test_short.txt"
    type profoil.dmp >> "results-gfortran/profoil_test_short.txt" || (echo Failed to locate profoil.dmp & exit /b 1) 
    echo.                                                >> "results-gfortran/profoil_test_short.txt"
    echo ============================================    >> "results-gfortran/profoil_test_short.txt"
    echo Coordinates: %%F                                >> "results-gfortran/profoil_test_short.txt"
    echo ============================================    >> "results-gfortran/profoil_test_short.txt"
    echo.                                                >> "results-gfortran/profoil_test_short.txt"
    type fort.95 >> "results-gfortran/profoil_test_long.txt" || (echo Failed to locate fort.95 & exit /b 1) 
    echo.                                                >> "results-gfortran/profoil_test_short.txt"

    @REM the .tra files doesn't exist currently need some modification

    if exist %TRAFILE% (
        echo ************************************    >> "results-gfortran/profoil_test_tra.txt"
        echo Running file: %%F                       >> "results-gfortran/profoil_test_tra.txt"
        echo ************************************    >> "results-gfortran/profoil_test_tra.txt"
        echo.                                        >> "results-gfortran/profoil_test_tra.txt"
        echo ************************************    >> "results-gfortran/profoil_test_tra.txt"
        echo Output from file: profoil_tra.txt       >> "results-gfortran/profoil_test_tra.txt"
        echo Eppler style output                     >> "results-gfortran/profoil_test_tra.txt"
        echo ************************************    >> "results-gfortran/profoil_test_tra.txt"
        echo.                                        >> "results-gfortran/profoil_test_tra.txt"
        type profoil_tra.txt                         >> "results-gfortran/profoil_test_tra.txt"
        echo.                                        >> "results-gfortran/profoil_test_tra.txt"
        echo ************************************    >> "results-gfortran/profoil_test_tra.txt"
    )

    del /S /Q profoil_out.txt
    del /S /Q profoil_tra.txt
    del /S /Q profoil.in
    del /S /Q fort.95
)

del /S /Q profoil.in
del /S /Q profoil.dmp
del /S /Q profoil.vel
del /S /Q profoil.xy
del /S /Q profoil_out.txt
del /S /Q profoil_tra.txt
del /S /Q fort.20
del /S /Q fort.30
del /S /Q fort.50
del /S /Q fort.70
del /S /Q fort.80
del /S /Q fort.90
del /S /Q fort.95

cd ..
cd
echo %DATE% %TIME%
echo Done.
endlocal
