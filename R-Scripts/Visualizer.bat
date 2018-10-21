@echo off


:parseargs
IF NOT "%1"=="" (
	IF "%1"=="-c" (
		SET pathToCsvFiles=%2
		SHIFT
	)
	IF "%1"=="-l" (
		SET pathToLibDir=%2
		SHIFT
	)
	IF "%1"=="-r" (
		SET pathToRscript=%2
		SHIFT
	)
	IF "%1"=="-v" (
		SET pathToVD=%2
		SHIFT
	)
	IF "%1"=="-h" (
		CALL :printUsage
		GOTO :end
	)
	IF "%1"=="-g" (
		SET granularity=%2
		SHIFT
	)
	IF "%1"=="-i" (
		SET INSTALL="TRUE"
	)
	SHIFT
	GOTO :parseargs
)

SET currentDirectory=%cd%
echo Current directory: %currentDirectory%
echo %pathToVM%
IF NOT "%INSTALL%"=="" ( 
	if "%INSTALL%"==""TRUE"" (
		:: Perform installation
		echo %pathToRscript% %currentDirectory%\InstallationWrapper.R %pathToLibDir% %currentDirectory%
		%pathToRscript% %currentDirectory%\InstallationWrapper.R %pathToLibDir% %currentDirectory%
	)
)

if "%errorlevel%" GTR 0 (
	echo [Error] R-script execution failed while installing!
	set /p temp= "Press enter to continue...
	exit 1
)

:: Execute the R-scripts and pass arguments to it
echo %pathToRscript% %currentDirectory%\VisualizationWrapper.R "%pathToCsvFiles%" "%currentDirectory%" "%pathToLibDir%" "%pathToVD%" "%granularity%"
%pathToRscript% %currentDirectory%\VisualizationWrapper.R %pathToCsvFiles% %currentDirectory% %pathToLibDir% %pathToVD% %granularity%

if %errorlevel% GTR 0 (
	echo [Error] R-script execution failed!
	set /p temp="Press enter to continue..."
	exit 1
)

:: Copy the images for the legend
mkdir %pathToCsvFiles%\Resources
copy %currentDirectory%\Resources\*.png %pathToCsvFiles%\Resources\

:: Now, invoke PDFLaTeX
cd %pathToCsvFiles%
pdflatex StarPlot.tex
pdfcrop StarPlot.pdf StarPlot.pdf
pdflatex TextPlot.tex	
pdfcrop TextPlot.pdf TextPlot.pdf
:: Remove temporary files that are no longer needed
::rm StarPlot_1.pdf
::rm TextPlot_1.pdf
DEL Rplots.pdf
DEL *.aux 
DEL *.log

:: print usage function
:printUsage
echo "Usage: ./Visualizer.sh -c <PathToDirectoryWithCsvFiles> -l <PathToLibDir> [-r <PathToRscript>] [-v <pathToVD>] [-i] [-h] [-g <granularity>]";
echo "-c: PathToDirectoryWithCsvFiles is the path to the directory containing the performance models as .csv-files.";
echo "-l: PathToLibDir is the path where the libraries should be stored.";
echo "-r: PathToRscript is the path to the Rscript.exe file (only needed on Windows) - Default: Rscript";
echo "-v: PathToVD is the path to the .txt file containing the value domain of each feature.";
echo "-i:PerformInstallation tells the script whether the installation of the libraries should be performed or not.";
echo "-h: Print usage.";
echo "-g: Granularity of the resulting plot. Can either be fine(default) or coarse.";

:end