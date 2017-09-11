visualize <- function(pathToExampleFiles, pathOfSourceFiles) {

  library(ggplot2)
  #library(ggradar)
  
  suppressPackageStartupMessages(library(dplyr))
  library(scales)
  
  # The path to the working directory
  #path <- "C:/Users/chris_000/Desktop/Uni/ViPe/Examples/"
  setwd(pathToExampleFiles);

  performanceModels <- NULL;
  
  # Find all csv files in the current directory
  csvFiles <- list.files(".", pattern="\\.csv$", recursive = F);
  
  # Parse the performance models
  for (i in 1:length(csvFiles)) {
    file <- csvFiles[i];
    name <- strsplit(file, "\\.")[[1]][1];
    
    # Add the performance models
    performanceModel <- read.csv(file, header=TRUE, sep=";", check.names = FALSE);
    
    # Replace the feature interactions by numbers
    #performanceModel <- cbind(rep(name, nrow(performanceModel)), performanceModel);
    
    # Adjust the name of the column
    colnames(performanceModel)[1] <- "Group"
    if (i == 1) {
      performanceModels <- performanceModel;
    } else {
      performanceModels <- rbind(performanceModels, performanceModel)
    }
  }
  
  # Find the maximum and minimum value
  maximumValue <- max(max(performanceModels[-1]), abs(min(performanceModels[-1])))
  minimumValue <- -maximumValue;
  
  performanceModels[-1] <- performanceModels[-1]  / maximumValue
  #colnames(performanceModels) <- gsub("Â", "",colnames(performanceModels))
  
  source("C:/Users/chris_000/Desktop/Uni/ViPe/R-Scripts/ggtext.R")
  ggtext(performanceModels, text.font = "sans", text.size=12, pathOfSourceFiles = pathOfSourceFiles)
  source("C:/Users/chris_000/Desktop/Uni/ViPe/R-Scripts/ggradar.R")
  p <- ggradar(performanceModels, axis.label.size=3, grid.label.size=5, font.radar = "sans", legend.title = "Performance Models", values.radar = c("", "0%", ""), grid.min = -1, grid.mid = 0, grid.max = 1, legend.text.size=10, pathOfSourceFiles = pathOfSourceFiles)
  ggsave("StarPlot.pdf", height=8.5, width=11, p)
}