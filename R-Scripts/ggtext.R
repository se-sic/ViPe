#' ggtext
#' @author Christian Kaltenecker
#' @export ggtext

ggtext <- function(plot.data,
                   font = "Circular Air Light",
                   text.colour = "black",
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
  
  ComputeTextSizes <- function(allTerms) {
    result <- c();
    for (j in 1:length(allTerms[[1]][[1]])) {
      for (i in 1:length(allTerms)) {
       if (allTerms[[i]][j] != "")  {
         specs <- NULL;
         specs$width <- strwidth(allTerms[[i]][[1]][j], "inches");
         specs$height <- strheight(allTerms[[i]][[1]][j], "inches");
         result <- c(result, list(specs));
         break;
       }
      }
    }
    
    return(result);
  }
  
  ComputeTextCoordinates <- function(allTerms, distanceBetweenText) {
    # Retrieve the text sizes
    textSizes <- ComputeTextSizes(allTerms);
    
    
  }
  
  # CONVERSION OF THE DATA (if needed)
  allTerms <- NULL;
  allTerms <- ComputeStrings(plot.data);
  
  # Calculating the positions of the strings
  ComputeTextCoordinates(allTerms, 20);

  # PLOTING
  base <- ggplot();
  
  # Adding the text
  base <- base #+ 
    #geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
    #                       aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar)
  
  
}