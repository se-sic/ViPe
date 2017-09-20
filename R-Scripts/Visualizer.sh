#!/bin/bash

function printUsage {
	echo "Usage: ./Visualizer.sh <PathToDirectoryWithCsvFiles> [PathToRscript] [PathToScriptDirectory]";
	echo "PathToDirectoryWithCsvFiles is the path to the directory containing the performance models as .csv-files.";
	echo "PathToRscript is the path to the Rscript.exe file (only needed on Windows)";
	echo "PathToScriptDirectory is the path to the directory where the scripts are stored."
}

if [[ "$#" -lt "1" || "$#" -gt "3"  ]]
then
	printUsage
	exit
fi

pathToCsvFiles="$1";

if [ "$#" -lt "2" ]
then
	pathToRscript="Rscript"
else 
	pathToRscript="$2";
fi

if [ "$#" -lt "3" ]
then
	
else
fi
currentDirectory="$(pwd)"
# BACH_SOURCE[0]

echo $pathToRscript ${currentDirectory}/VisualizationWrapper.R "${pathToCsvFiles}" "${currentDirectory}"
$pathToRscript ${currentDirectory}/VisualizationWrapper.R "${pathToCsvFiles}" "${currentDirectory}"
