#' ggelephant
#' @author Alexander Grebhahn, Christian Kaltenecker

ggelephant <- function(plot.data, 
                       width = 50,
                       height = 2,
                       lineHeight = 1.5,
                       textLineOffset = 0.2,
                       hideTermsWithInfluencesLowerOrEqualThan = 0.01, 
                       pathOfSourceFiles,
                       pathToLibrary) {
  
  # LIBRARIES
  library("ggplot2", lib.loc=pathToLibrary)
  library("gridExtra", lib.loc=pathToLibrary)
  library("grid", lib.loc=pathToLibrary)
  library("cowplot", lib.loc=pathToLibrary)
  library("stringr", lib.loc=pathToLibrary)
  library("grDevices", lib.loc=pathToLibrary)
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
  
  GenerateTexFile <- function(filePath, pathToOutputFile, alignedText, titles) {
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
    paste("\t\\newcommand{\\xCoord}{", 0.356 * width, "}", sep=""),
    paste("\t\\newcommand{\\yCoord}{", -1.465 * height, "}", sep=""),
    paste("\t\\newcommand{\\yOffset}{", 1.143 * lineHeight, "}", sep=""),
    paste("\t\\newcommand{\\lineOffset}{", 0.7 * textLineOffset, "}", sep=""),
    "",
    "\t\\newcommand{\\size}{\\Huge}",
    "");
    
    
    # Defining macros
    prefixes <- c(letters, LETTERS)
    lineOffsetCounter <- 0
    for (i in 1:length(titles)) {
      numberLines <- str_count(titles[i], fixed("*"))
      lineOffsetCounter <- lineOffsetCounter + numberLines
      content <- c(content,
                   paste("\t\\pgfmathsetmacro\\", prefixes[i], "y{\\yCoord - ", i, " * \\yOffset - ", lineOffsetCounter, " * \\lineOffset}", sep=""))
      lineOffsetCounter <- lineOffsetCounter + numberLines
      
    }
    
    content <- c(content, 
                 "\t\\begin{tikzpicture}",
                 paste("\t\t\\node[inner sep=0, anchor=north west] (pic) at (0,0) {\\includegraphics[width=\\picWidth, height=\\picHeight]{", pathToOutputFile, "}};", sep="")
                 );
    
    # Draw nodes in tikzpicture
    for (i in 1:length(titles)) {
      content <- c(content, 
                   paste("\t\t\\node[inner sep=0, anchor=west, align=left] at (\\xCoord, \\", prefixes[i], "y) {\\size $ ", gsub("\\\\", "\\\\\\size ", alignedText[i], fixed=TRUE), " $};", sep="")
                  )
    }
    
    
    # The footer
    content <- c(content, 
                 "\t\\end{tikzpicture}",
                 "\\end{document}"
               )
    
    # Write the whole content in the specified file
    WriteToFile(filePath, content)
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
      coords$influence <- c(coords$influence, influence)
      
      currentMin <- currentMin + influence
    }
    
    # Compute the coordinates for the lines
    currentMin <- 0
    currentHeight <- 0
    for (counter in length(var.names):1) {
      lineCoords$Group <- c(lineCoords$Group, counter)
      influence <- normalizedInfluences[1,counter]
      
      numberLines <- str_count(var.names[counter], fixed("*"))
      
      xTo <- width - (currentMin + influence / 2) * width
      currentHeight <- currentHeight -lineHeight - textLineOffset * numberLines
      yTo <- currentHeight
      currentHeight <- currentHeight - textLineOffset * numberLines
      lineCoords$x <- c(lineCoords$x, xTo)
      lineCoords$y <- c(lineCoords$y, 0)
      
      lineCoords$Group <- c(lineCoords$Group, counter)
      lineCoords$x <- c(lineCoords$x, xTo)
      lineCoords$y <- c(lineCoords$y, yTo)
      
      lineCoords$Group <- c(lineCoords$Group, counter)
      lineCoords$x <- c(lineCoords$x, width)
      lineCoords$y <- c(lineCoords$y, yTo)
      
      currentMin <- currentMin + influence
    }
    
    alignedText <- breakText(var.names)
    
    # PLOTTING
    coords <- as.data.frame(coords)
    lineCoords <- as.data.frame(lineCoords)
    
    linePlot <- ggplot(coords) + theme_nothing()
    
    # Plot the lines
    linePlot <- linePlot +
      geom_line(data=lineCoords, aes(x=x, y=y, group=Group), color="grey", size=1)
    
    # Plot the rectangles
    linePlot <- linePlot + 
      geom_rect(aes(xmin=xMin, xmax=xMax, ymin=yMin, ymax=yMax, alpha=1 - influence), size=2, color="black", fill="grey30")
    
    ggsave(paste("ElephantPlot__", i, ".pdf", sep=""), height=10, width=10, linePlot)
    
    GenerateTexFile(paste("ElephantPlot_", i , ".tex", sep=""), paste("ElephantPlot__", i, ".pdf", sep=""), rev(alignedText), rev(var.names))
  }
  
  # Write the number of generated performance models in a file for the other scripts
  WriteToFile("ElephantPlot_Number.txt", c(as.character(i)))
  
  dev.off()
  graphics.off()
}