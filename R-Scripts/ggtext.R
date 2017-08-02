#' ggtext
#' @author Christian Kaltenecker
#' @export ggtext

ggtext <- function(plot.data,
                   text.font = "Circular Air Light",
                   text.colour = "black",
                   text.size = 4,
                   curve.colour = "blue") 
{
  # PREPARATION
  
  # This library is based on the ggplot-library
  library(ggplot2)
  
  # Retrieve the location of the script
  script.dir <- dirname(sys.frame(1)$ofile)
  # Load the script containing commonly used functions
  source(paste(script.dir, "common.R", sep="/"))
  
  plot.data <- as.data.frame(plot.data)
  
  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"
  
  # (TODO) Check if data makes sense
  
  # DECLARATION OF INTERNAL FUNCTIONS
  
  GetTermIfAvailable <- function(column) {
    termName <- breakLine(colnames(column)[1]);
    result <- c();
    for (j in 1:nrow(column)) {
      # Return empty string
      if (column[j,] == 0) {
        result <- c(result, "");
      } else {
        result <- c(result, termName);
      }
    }
    return(result);
  }
  
  ComputeStrings <- function(plot.data) {
    allTerms <- list();
    
    for (j in 1:nrow(plot.data[1])) {
      allTerms <- c(allTerms, list(c()));
    }
    
    for (i in 2:ncol(plot.data)) {
      terms <- GetTermIfAvailable(plot.data[i]);
      
      for (j in 1:nrow(plot.data[i])) {
        allTerms[[j]]$label <- c(allTerms[[j]]$label, terms[j]);
      }
    }
    
    return(allTerms);
  }
  
  ComputeTextSizes <- function(allTerms, text.font) {
    result <- c();
    for (j in 1:length(allTerms[[1]][[1]])) {
      for (i in 1:length(allTerms)) {
       if (allTerms[[i]][[1]][j] != "")  {
         specs <- NULL;
         specs$width <- strwidth(allTerms[[i]][[1]][j], "inches", family=text.font);
         specs$height <- strheight(allTerms[[i]][[1]][j], "inches", family=text.font) / 4;
         result <- c(result, list(specs));
         break;
       }
      }
    }
    
    return(result);
  }
  
  ComputePosition <- function(allTerms, distanceBetweenText, textSizes) {
    result <- c();
    
    # Find the maximum height
    maxHeight <- 0;
    for (i in 1:length(textSizes)) {
      maxHeight <- max(maxHeight, textSizes[[i]]$height);
    }
    
    currentOffset <- NULL;
    currentOffset$y <- 0;
    for (i in 1:length(allTerms)) {
      currentOffset$x <- 0;
      positions <- NULL;
      for (j in 1:length(textSizes)) {
        positions$x <- c(positions$x, currentOffset$x);
        currentOffset$x <- currentOffset$x + textSizes[[j]]$width;
        positions$y <- c(positions$y, currentOffset$y);
      }
      currentOffset$y <- currentOffset$y - maxHeight;
      result <- c(result, list(positions));
    }
    
    return(result);
  }
  
  ComputeTextCoordinates <- function(allTerms, distanceBetweenText, text.font) {
    # Retrieve the text sizes
    textSizes <- ComputeTextSizes(allTerms, text.font);
    # Compute the positions of the terms
    positions <- ComputePosition(allTerms, distanceBetweenText, textSizes);

    for (i in 1:length(positions)) {
      allTerms[[i]]$x <- positions[[i]]$x;
      allTerms[[i]]$y <- positions[[i]]$y;
    }
    
    return(allTerms);
  }
  
  # CONVERSION OF THE DATA (if needed)
  allTerms <- NULL;
  allTerms <- ComputeStrings(plot.data);
  
  # Calculating the positions of the strings
  allTerms <- ComputeTextCoordinates(allTerms, 1, text.font);

  # PLOTING
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=20) + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  theme_clear <- theme_clear + theme(legend.position="none")
  
  base <- ggplot() + xlab(NULL) + ylab(NULL) + theme_clear;
  
  # Adding the text
  for (i in 1:length(allTerms)) {
    browser();
    base <- base  + 
      geom_text(data=as.data.frame(allTerms[[i]]),
                aes(x=x,y=y,label=label), family=text.font, size=text.size)
  }
  
  
  return(base);
}