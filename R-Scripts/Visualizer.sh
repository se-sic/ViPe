#!/bin/bash

function printUsage {
	echo "Usage: ./Visualizer.sh <PathToDirectoryWithCsvFiles> <PathToLibDir> [PathToRscript] [PerformInstallation]";
    echo "PathToDirectoryWithCsvFiles is the path to the directory containing the performance models as .csv-files.";
    echo "PathToLibDir is the path where the libraries should be stored.";
    echo "PathToRscript is the path to the Rscript.exe file (only needed on Windows) - Default: Rscript";
    echo "PathToVM is the path to the .txt file containing the value domain of each feature (only required if installation has to be performed too). Can either be a valid path, or NONE to indicate that no file exists.";
    echo "PerformInstallation tells the script whether the installation of the libraries should be performed or not.";
}

for i in "$@"
do
case $i in 
	-c)
	pathToCsvFiles="$2"
	shift
	shift
	;;
	-l)
	pathToLibDir="$2"
	shift
	shift
	;;
	-r)
	pathToRscript="$2"
	shift
	shift
	;;
	-v)
	pathToVD="$2"
	shift
	shift
	;;
	-i)
	INSTALL="TRUE"
	shift
	;;
	-g)
	granularity="$2"
	shift
	shift
	;;
	-h)
	printUsage
	read -p "Press enter to continue...";
	exit 0
	;;
	*)
	printUsage
	read -p "Press enter to continue...";
	exit 0
	;;
esac
done

mkdir -p $pathToLibDir
# Detect the platform the script is running on
platform='unknown'
unamestr=`uname`

if [[ "$unamestr" == 'Linux' ]]; then
  platform='linux'
else 
  platform='windows'
fi

if [[ "$platform" == 'windows' ]]; then
  currentDirectory=`dirname $BASH_SOURCE`
  currentDirectory=${currentDirectory//\\//}
else 
  currentDirectory=$(dirname `realpath $0`)
fi

echo "Current directory: $currentDirectory";

if [[ "$INSTALL" == 'TRUE' ]]
then
	# Perform installation
	echo "$pathToRscript" ${currentDirectory}/InstallationWrapper.R "${pathToLibDir}" "${currentDirectory}"
	"$pathToRscript" ${currentDirectory}/InstallationWrapper.R "${pathToLibDir}" "${currentDirectory}"
fi

if [ "$?" != "0" ]; then
	echo "[Error] R-script execution failed while installing!";
	read -p "Press enter to continue...";
	exit 1
fi

# Execute the R-scripts and pass arguments to it
echo $pathToRscript ${currentDirectory}/VisualizationWrapper.R "${pathToCsvFiles}" "${currentDirectory}" "${pathToLibDir}" "${pathToVD}" "${granularity}"
"$pathToRscript" ${currentDirectory}/VisualizationWrapper.R "${pathToCsvFiles}" "${currentDirectory}" "${pathToLibDir}" "${pathToVM}" "${pathToVD}" "${granularity}"

if [ "$?" != "0" ]; then
	echo "[Error] R-script execution failed!";
	read -p "Press enter to continue...";
	exit 1
fi

# Copy the images for the legend
mkdir -p ${pathToCsvFiles}/Resources
cp ${currentDirectory}/Resources/*.png ${pathToCsvFiles}/Resources/

# Now, invoke PDFLaTeX
cd $pathToCsvFiles
pdflatex StarPlot.tex
pdfcrop StarPlot.pdf StarPlot.pdf
pdflatex TextPlot.tex	
pdfcrop TextPlot.pdf TextPlot.pdf

# Remove temporary files that are no longer needed
#rm StarPlot_1.pdf
#rm TextPlot_1.pdf
rm Rplot.pdf 2> /dev/null
rm *.aux 
rm *.log
