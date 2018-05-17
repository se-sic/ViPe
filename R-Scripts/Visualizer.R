meanNormalization <- function(dataToNormalize) {
  result <- NULL;
  
  minimumValue <- min(dataToNormalize);
  maximumValue <- max(dataToNormalize);
  meanValue <- mean(apply(dataToNormalize, 1, mean));
  
  result <- (dataToNormalize - meanValue) / (maximumValue - minimumValue);
  
  return(result);
}

visualize <- function(pathToExampleFiles, pathOfSourceFiles, pathToLibrary, doMeanNormalization=FALSE
                      , valueDomain="") {
  library("ggplot2", lib.loc=pathToLibrary)
  library("labeling", lib.loc=pathToLibrary)
  library("digest", lib.loc=pathToLibrary)
  #library(ggradar)
  
  suppressPackageStartupMessages(library("dplyr", lib.loc=pathToLibrary))
  library("scales", lib.loc=pathToLibrary)
  
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
  
  # A list containing the alternatives
  alternatives <- list()
  hasAlternatives <- FALSE
  
  # consider adjusting influences to the value range of the configuration
  if (valueDomain != "" && valueDomain != "NONE") {
    vdFile <- file(valueDomain, open='r')
    lines <- readLines(vdFile)
    
    options <- vector(mode="character", length=0)
    values <- vector(mode="numeric", length=0)
    for (i in 1:length(lines)) {
      keyAndValue = unlist(strsplit(lines[i], "[=]"))
      
      # One line can consist of multiple alternative configuration options
      allOptions <- unlist(strsplit(keyAndValue[1], "[,]"))
      
      # Add all options except the first one to the list
      # This will be used to replace the other option names
      # by the first one.
      if (length(allOptions) > 1) {
        hasAlternatives <- TRUE
        for (j in 2:length(allOptions)) {
          alternatives[[allOptions[j]]] <- allOptions[1]
        }
      }
      
      for (j in 1:length(allOptions)) {
        options <- c(options, allOptions[j])
        valueRange <- unlist(strsplit(unlist(strsplit(keyAndValue[2], "[[]"))[2], "[]]"))[1]
        UpperAndLower <- unlist(strsplit(valueRange, "[,]"))
        valueAdjust <- as.numeric(UpperAndLower[2]) - as.numeric(UpperAndLower[1])
        values <- c(values, valueAdjust)
      }
      
    }
    
    names(values) <- options
    
    for(i in 2:length(performanceModels)) {
      interaction <- colnames(performanceModels)[i]
      
      if (grepl(interaction, pattern="\\*") || grepl(interaction, pattern="log")) {
        
        variables <- unlist(strsplit(interaction, "[*]"))
        valueWeight <- 1
        
        for (x in 1:length(variables)) {
          
          if (!grepl(variables[x], pattern="log\\(")) {
            
            valueWeight <- valueWeight * values[gsub("^\\s+|\\s+$", "", variables[x])]
            
          } else {
            
            valueWeight <- valueWeight * log10(values[gsub("log\\(|\\)$", "",  variables[x])])
            
          }
          
        }
        for (j in 1:nrow(performanceModels)) {
          
          performanceModels[[i]][j] <- valueWeight * performanceModels[[i]][j] 
          
        }
      } else {
        
        valueWeight <- values[interaction]
        for (j in 1:nrow(performanceModels)) {
          
          performanceModels[[i]][j] <- valueWeight * performanceModels[[i]][j] 
          
        }
      }
    } 
    
  }
  
  if (hasAlternatives) {
    # TODO: Combine the alternatives here
    # 1. Replace the names
    allTerms <- names(performanceModels)[-1]
    for (i in 1:length(alternatives)) {
      wordToReplace <- names(alternatives)[i]
      replaceBy <- alternatives[[wordToReplace]]
      allTerms <- gsub(wordToReplace, replaceBy, allTerms, fixed=TRUE)
    }
    
    
    # 2. Search for common names 
    
    # Extract the duplicates
    # TODO: Order?
    duplicates <- allTerms[duplicated(allTerms)]
    
    if (length(duplicates) == 0) {
      hasAlternatives <- FALSE
    } else {
      # 3. Create a new list of lists of vectors that stores the values from the common columns in a vector
      newPerformanceModels <- list()
      columnNames <- unique(allTerms)
      newPerformanceModels[]
      for (i in 2:length(columnNames)) {
        newPerformanceModels[[i]] <- list()
      }
      names(newPerformanceModels) <- columnNames
      
      for (model in 1:length(performanceModels[[1]])) {
        # Set the name of the group
        newPerformanceModels[[1]][[model]] <- performanceModels[[1]][model]
        handled <- c()
        # Set the other values
        for (i in 2:length(newPerformanceModels)) {
          currentColumn <- columnNames[i]
          if (currentColumn %in% duplicates && !(currentColumn %in% handled)) {
            handled <- c(handled, currentColumn)
            indices <- currentColumn == allTerms
            indices <- which(indices)
            values <- c()
            for (index in 1:length(indices)) {
              values <- c(values, performanceModels[[indices[index] + 1]][model])
            }
          } else {
            values <- c(performanceModels[[currentColumn]][model])
          }
          if (length(values) > 0) {
            newPerformanceModels[[currentColumn]][[model]] <- values
          }
        }
      }
    }
  }
  
  # TODO: Adjust the following lines
  if (doMeanNormalization) {
    performanceModels[-1] <- meanNormalization(performanceModels[-1]);
  } else {
    # Find the maximum and minimum value
    maximumValue <- max(max(performanceModels[-1]), abs(min(performanceModels[-1])))
    minimumValue <- -maximumValue;
    
    performanceModels[-1] <- performanceModels[-1]  / maximumValue
  }
  
  # TODO: Adjust the following scripts
  
  if (length(unique(performanceModels$Group)) < 3) {
    source(paste(pathOfSourceFiles, "ggtext.R", sep=""))
    ggtext(performanceModels, text.font = "sans", text.size=14, pathOfSourceFiles = pathOfSourceFiles, pathToLibrary=pathToLibrary) 
  }
  source(paste(pathOfSourceFiles, "ggradar.R", sep=""))
  p <- ggradar(performanceModels, axis.label.size=3, grid.label.size=7, legend.text.size=14, font.radar = "sans", values.radar = c("", "", ""), grid.min = -1, grid.mid = 0, grid.max = 1, pathOfSourceFiles = pathOfSourceFiles, pathToLibrary = pathToLibrary)
  ggsave("StarPlot_1.pdf", height=8.5, width=11, p)
}
