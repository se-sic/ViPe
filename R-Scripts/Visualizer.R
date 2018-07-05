meanNormalization <- function(dataToNormalize) {
  result <- NULL;
  
  minimumValue <- min(dataToNormalize);
  maximumValue <- max(dataToNormalize);
  meanValue <- mean(apply(dataToNormalize, 1, mean));
  
  result <- (dataToNormalize - meanValue) / (maximumValue - minimumValue);
  
  return(result);
}

visualize <- function(pathToExampleFiles, pathOfSourceFiles, pathToLibrary, doMeanNormalization=FALSE
                      , valueDomain="", granularity = "fine") {
  library("ggplot2", lib.loc=pathToLibrary)
  library("labeling", lib.loc=pathToLibrary)
  library("digest", lib.loc=pathToLibrary)
  #library(ggradar)
  print(granularity)
  
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
  
  # prepare varibale to store compute polynominal values
  polynomGroups <- list()
  
  # A list containing the alternatives
  alternatives <- list()
  hasAlternatives <- FALSE
  
  # consider adjusting influences to the value range of the configuration
  if (valueDomain != "" && valueDomain != "NONE") {
    vdFile <- file(valueDomain, open='r')
    lines <- readLines(vdFile)
    
    options <- vector(mode="character", length=0)
    values <- list()
    completeValueRange <- list()
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
        allValues <- unlist(strsplit(valueRange, "[,]"))
        valueAdjust <- as.numeric(allValues[length(allValues)]) - as.numeric(allValues[1])
        completeValueRange[[length(completeValueRange) + 1]] <- allValues
        values <- c(values, valueAdjust)
        
      }
      
    }
    
    names(values) <- options
    names(completeValueRange) <- options
    
    # handle polynominals start
    polynominals <- list()
    if (granularity == "coarse") {
    # compute polynominals
    toIgnore <- vector()
    for(i in 2:length(performanceModels)) {
      poly <- colnames(performanceModels)[i]
      candidate <- c(poly)
      
      for(j in (i+1):length(performanceModels)) {
        if (!(j %in% toIgnore) & j!=i) {
          variablesOther <- unlist(strsplit(colnames(performanceModels)[j], "[*]"))
          variablesCandidate <- unlist(strsplit(poly, "[*]"))
          
          if (setequal(variablesOther, variablesCandidate)) {
            candidate <- c(candidate, colnames(performanceModels)[j])
            toIgnore <- c(toIgnore, j)
          }
        }
      }
      if (length(candidate) > 1) {
        polynominals[[length(polynominals) + 1]] = candidate
      } else if(!(i %in% toIgnore)) {
        # also add all polynominals with at least one numeric options even if there are no multiple occurrences of similar polys
        uniqueVariables <- unique(unlist(strsplit(candidate[1], "[*]")))
        oneNumeric <- FALSE
        for(var in uniqueVariables) {
          # if a option dosent have more than 2 possible values it is a binary option
          if(length(completeValueRange[[var]]) >= 3) {
            oneNumeric <- TRUE
          }
        }
        
        if(oneNumeric) {
          polynominals[[length(polynominals) + 1]] = candidate
        }
      }
    }
    
    #clean up and remove polynominals from the other data into a separate list
    listOfPolynominals <- list()
    for (i in 1:length(polynominals)) {
      polyPartNames <- polynominals[i]
      polynom <- list()
      for (j in 1:length(polyPartNames[[1]])) {
        tmp <- polyPartNames[[1]][j]
        polynom[j] <- performanceModels[tmp]
        performanceModels[tmp] <- NULL
      }
      polyDf <- data.frame(matrix(unlist(polynom), nrow=length(polyPartNames)))
      TMP <- unlist(performanceModels[["Group"]])
      colnames(polyDf) <- performanceModels[["Group"]]
      rownames(polyDf) <- polyPartNames[[1]]
      listOfPolynominals[[i]] = polyDf
    }
    browser();
    
    # compute all values for each polynominal
    groupNames <- vector()
    #for each polynomGroup for each case study compute all possible values
    for (i in 1:length(listOfPolynominals)) {
      groupValues <- list()
      currentPoly <- listOfPolynominals[[i]]
      uniqueVariables <- unique(unlist(strsplit(rownames(currentPoly)[1], "[*]")))
      groupNames <- c(groupNames, paste(uniqueVariables, collapse='*', sep="*"))
      # for each case study
      for (j in 1:ncol(currentPoly)) {
        
        selectedValues <- vector()
        #compute all possible value combinations
        for(variable in uniqueVariables) {
          if (length(selectedValues) == 0) {
            selectedValues <- unlist(completeValueRange[variable])
          } else {
            combinations <- vector()
            for (previousSelection in selectedValues) {
              for (newValue in completeValueRange[variable]) {
                combinations <- c(combinations, paste(previousSelection, newValue, sep="*"))
              }
            }
            selectedValues <- combinations
          }
        }
        
        browser();
        # compute the values of the polynom group
        computedValues <- vector()
        for (val in selectedValues) {
          computedVal <- 0
          for (z in 1:length(rownames(currentPoly))) {
            coeff <- currentPoly[[j]][z]
            assignedValues <- unlist(strsplit(val, "[*]"))
            names(assignedValues) <- uniqueVariables
            #neutral value for multiplication
            temp <- 1
            for (option in unlist(strsplit(rownames(currentPoly)[z], "[*]"))) {
              temp <- temp * as.numeric(assignedValues[option])
            }
            temp <- temp * coeff
            computedVal <- computedVal + temp
          }
          computedValues <- c(computedValues, computedVal)
        }
        groupValues[[length(groupValues) + 1]] <- computedValues
      }
      browser();
      names(groupValues) <- colnames(currentPoly)
      polynomGroups[[length(polynomGroups) + 1]] <- groupValues
    }
    names(polynomGroups) <- groupNames
    
    }
    # handle polynominals end
    
    for(i in 2:length(performanceModels)) {
      interaction <- colnames(performanceModels)[i]
      
      if (grepl(interaction, pattern="\\*") || grepl(interaction, pattern="log")) {
        
        variables <- unlist(strsplit(interaction, "[*]"))
        valueWeight <- 1
        
        for (x in 1:length(variables)) {
          
          if (!grepl(variables[x], pattern="log\\(")) {
            
            valueWeight <- valueWeight * as.numeric(values[gsub("^\\s+|\\s+$", "", variables[x])])
            
          } else {
            
            valueWeight <- valueWeight * log10(as.numeric(values[gsub("log\\(|\\)$", "",  variables[x])]))
            
          }
          
        }
        for (j in 1:nrow(performanceModels)) {
          
          performanceModels[[i]][j] <- valueWeight * performanceModels[[i]][j] 
          
        }
      } else {
        
        valueWeight <- as.numeric(values[interaction])
        for (j in 1:nrow(performanceModels)) {
          
          performanceModels[[i]][j] <- valueWeight * performanceModels[[i]][j] 
          
        }
      }
    } 
    
  }
  alternativeList<- NULL
  
  if (hasAlternatives) {
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
      # 3. Replace the values from the affected columns
      alternatives <- list()
      columnNames <- allTerms
      columnsToRemove <- c()
      
      for (i in 1:nrow(performanceModels)) {
        handled <- c()
        alternativeList[[i]] <- list()
        for (j in 1:ncol(performanceModels)) {
          currentColumn <- columnNames[j]
          if (currentColumn %in% duplicates && !(currentColumn %in% handled)) {
            handled <- c(handled, currentColumn)
            indices <- currentColumn == columnNames
            indices <- which(indices)
            columnsToRemove <- c(columnsToRemove, indices[-1] + 1)
            values <- c()
            for (k in 1:length(indices)) {
              values <- c(values, performanceModels[i, indices[k] + 1])
            }
            performanceModels[i, indices[k]] <- mean(values)
            alternativeList[[i]][[currentColumn]] <- values
          }
        }
      }
      # 4. Remove the columns
      performanceModels <- performanceModels[,-columnsToRemove]
    }
  }
  

  if (doMeanNormalization) {
    performanceModels[-1] <- meanNormalization(performanceModels[-1]);
    
    if (hasAlternatives) {
      # TODO
    }
    
  } else {
    browser();
    # Find the maximum value
    maximumValue <- max(max(performanceModels[-1]), abs(min(performanceModels[-1])))
    for(group in polynomGroups) {
      for(caseStudy in group) {
        for(value in caseStudy) {
          if(abs(value) > maximumValue) {
            maximumValue <- abs(value)
          }
        }
      }
    }
    
    for(alternative in alternativeList) {
      for(values in alternative) {
        for(value in values) {
          if(abs(value) > maximumValue) {
            maximumValue <- abs(value)
          }
        }
      }
    }
    
    
    browser();
    # divide all polys by ma number
	  if (length(polynomGroups)> 0) {
      for(z in 1:length(polynomGroups)) {
        for(i in 1:length(polynomGroups[[z]])) {
          vec <- vector()
          for (j in 1:length(polynomGroups[[z]][[i]])) {
            vec <- c(vec, polynomGroups[[z]][[i]][j] / maximumValue) 
          }
          polynomGroups[[z]][[i]] <- vec
        }
      }
	  }
    
    performanceModels[-1] <- performanceModels[-1]  / maximumValue
    
    # readd the mean of each of poly to the performance models
    for(z in 1:length(polynomGroups)) {
      means <- vector()
      for(caseStudy in polynomGroups[[z]]) {
        means <- c(means, mean(caseStudy))
      }
      performanceModels[[names(polynomGroups)[z]]] <- means
    }
    
    
    if (hasAlternatives) {
      for (i in 1:length(alternativeList)) {
        for (j in 1:length(alternativeList[[i]])){
          alternativeList[[i]][[j]] <- alternativeList[[i]][[j]] / maximumValue
        }
      }
    }
    browser();
    
  }
  
  if (length(unique(performanceModels$Group)) < 3) {
    source(paste(pathOfSourceFiles, "ggtext.R", sep=""))
    ggtext(performanceModels, text.font = "sans", text.size=14, pathOfSourceFiles = pathOfSourceFiles, pathToLibrary=pathToLibrary) 
  }
  source(paste(pathOfSourceFiles, "ggradar.R", sep=""))
  p <- ggradar(performanceModels, axis.label.size=3, grid.label.size=7, legend.text.size=14, font.radar = "sans", values.radar = c("", "", ""), grid.min = -1, grid.mid = 0, grid.max = 1, pathOfSourceFiles = pathOfSourceFiles, pathToLibrary = pathToLibrary, alternatives=alternativeList, polynoms=polynomGroups)
  ggsave("StarPlot_1.pdf", height=8.5, width=11, p)
  
}
