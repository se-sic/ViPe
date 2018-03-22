#' ggelephant
#' @author Alexander Grebhahn, Christian Kaltenecker

ggelephant <- function(plot.data, 
                       width = 50,
                       height = 5,
                       lineHeight = 1.5,
                       hideTermsWithInfluencesLowerOrEqualThan = 0.01, 
                       pathOfSourceFiles,
                       pathToLibrary) {
  
  # LIBRARIES
  library("ggplot2", lib.loc=pathToLibrary)
  library("gridExtra", lib.loc=pathToLibrary)
  library("grid", lib.loc=pathToLibrary)
  library("cowplot", lib.loc=pathToLibrary)
  # Load the script containing commonly used functions
  source(paste(pathOfSourceFiles, "common.R", sep=""))
  
  # PREPARATION
  
  # DEFINITION OF INTERNAL FUNCTIONS
  
  normalize <- function(vectorToNormalize, hideTermsWithInfluencesLowerThan) {
    # First, build the sum
    vectorToNormalize <- abs(vectorToNormalize)
    # Normalize
    sumOfVector <- sum(vectorToNormalize)
    result <- vectorToNormalize / sumOfVector
    # Filter the values that are too low
    result <- result[1,result[1,] > hideTermsWithInfluencesLowerThan]
    # Adjust the scale
    sumOfVector <- sum(result)
    result <- result / sumOfVector
    return(result)
  }
  
  GenerateTexFile <- function(filePath, pathToOutputFile, titles) {
    # Defining the header and the constants
    content <- c(
    "\\documentclass{standalone}",
    "",
    "\\usepackage{color}",
    "\\usepackage{graphicx}",
    "\\usepackage{tikz}",
    "\\usetikzlibrary{positioning, calc}",
    "",
    "",
    "\\begin{document}",
    "\t%Constants",
    "\t\\newcommand{\\picHeight}{400px}",
    "\t\\newcommand{\\picWidth}{520px}",
    "",
    "\t\\newcommand{\\xCoord}{17.8}",
    "\t\\newcommand{\\yCoord}{-5.16}",
    "\t\\newcommand{\\yOffset}{1.38}",
    "",
    "\t\\newcommand{\\size}{\\Large}",
    "");
    
    
    
    # The footer
    content <- c(content, 
                 c("\t\\end{tikzpicture}",
                   "\\end{document}"
                 ));
    
    # Write the whole content in the specified file
    fileConn <- file(filePath);
    
    writeLines(content, fileConn);
    
    close(fileConn);
  }
  
  # IMPLEMENTATION OF THE FUNCTION ggelephant
  
  # Repeat the whole procedure for every influence model
  for (i in 1:nrow(plot.data)) {
    title <- plot.data[i,1]
    
    # First, identify the relative influences
    influences <- plot.data[i,][-1]
    
    normalizedInfluences <- sort(normalize(influences, hideTermsWithInfluencesLowerOrEqualThan), decreasing = TRUE)
    
    # Retrieve the variable names
    var.names <- colnames(normalizedInfluences)
    
    # Compute the coordinates of the rectangles and the coordinates of the lines
    currentMin <- 0
    coords <- NULL
    lineCoords <- NULL;
    counter <- ncol(normalizedInfluences)
    for (influence in normalizedInfluences) {
      
      
      coords$xMin <- c(coords$xMin, currentMin * width)
      coords$xMax <- c(coords$xMax, (currentMin + influence) * width)
      coords$yMin <- c(coords$yMin, 0)
      coords$yMax <- c(coords$yMax, height)
      
      lineCoords$Group <- c(lineCoords$Group, counter)
      lineCoords$x <- c(lineCoords$x, (currentMin + influence / 2) * width)
      lineCoords$y <- c(lineCoords$y, 0)
      
      lineCoords$Group <- c(lineCoords$Group, counter)
      lineCoords$x <- c(lineCoords$x, (currentMin + influence / 2) * width)
      lineCoords$y <- c(lineCoords$y, counter * -lineHeight)
      
      lineCoords$Group <- c(lineCoords$Group, counter)
      lineCoords$x <- c(lineCoords$x, width)
      lineCoords$y <- c(lineCoords$y, counter * -lineHeight)
      
      currentMin <- currentMin + influence
      counter <- counter - 1
    }
    
    # PLOTTING
    coords <- as.data.frame(coords)
    lineCoords <- as.data.frame(lineCoords)
    
    linePlot <- ggplot(coords) + theme_nothing();
    
    # Plot the lines
    linePlot <- linePlot +
      geom_line(data=lineCoords, aes(x=x, y=y, group=Group), color="grey", size=0.5)
    
    # Plot the rectangles
    linePlot <- linePlot + 
      geom_rect(aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax), size=2, color="black", fill="white")
    
    ggsave(paste("ElephantPlot_", i, ".pdf", sep=""), height=10, width=10, linePlot)
  }
}