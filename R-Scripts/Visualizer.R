library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

# The path to the working directory
path <- "/localhome/Repositories/ViPe/Examples/"
setwd(path);

performanceModels <- NULL;

# Find all csv files in the current directory
csvFiles <- list.files(".", pattern="\\.csv$", recursive = F);

# Parse the performance models
for (i in 1:length(csvFiles)) {
  file <- csvFiles[i];
  name <- strsplit(file, "\\.")[[1]][1];
  
  # Add the performance models
  performanceModel <- read.csv(file, header=TRUE, sep=";");
  performanceModel <- cbind(rep(name, nrow(performanceModel)), performanceModel);
  # Adjust the name of the column
  colnames(performanceModel)[1] <- "Group"
  if (i == 1) {
    performanceModels <- performanceModel;
  } else {
    performanceModels <- rbind(performanceModels, performanceModel)
  }
}

# Find the maximum and minimum value
maximumValue <- max(performanceModels[-1])
minimumValue <- min(performanceModels[-1])

performanceModels[-1] <- (performanceModels[-1] - minimumValue)  / (maximumValue - minimumValue)
browser();
ggradar(performanceModels, axis.labels = seq(1, ncol(performanceModels) - 1), font.radar = "mono", legend.title = "Performance Models")