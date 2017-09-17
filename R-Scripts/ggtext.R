#' ggtext
#' @author Christian Kaltenecker
#' @export ggtext

ggtext <- function(plot.data,
                   text.font = "Circular Air Light",
                   text.size = 8,
                   curve.colour = "blue",
                   group.point.size = 6,
                   label.size = 4,
                   colours = c("#4045FF", "#FFB400"),
                   legend.title="",
                   legend.text.size = text.size,
                   legend.labels=c("Perf1 (left)", "Perf2 (right)"),
                   pathOfSourceFiles) 
{
  # PREPARATION
  
  # This library is based on the ggplot-library
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(cowplot)
  
  # Retrieve the location of the script
  #script.dir <- dirname(sys.frame(1)$ofile)
  # Load the script containing commonly used functions
  source(paste(pathOfSourceFiles, "common.R", sep=""))
  
  plot.data <- as.data.frame(plot.data)
  
  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"
  
  # (TODO) Check if data makes sense
  
  # DECLARATION OF INTERNAL FUNCTIONS
  
  GetTermIfAvailable <- function(column) {
    result <- c();
    for (j in 1:nrow(column)) {
      rightAlign <- FALSE;
      if (j == 1) {
        rightAlign <- TRUE;
      }
      termName <- breakLine(prepareLine(colnames(column)[1]), rightAlign);
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
  
  ComputeTextSizes <- function(allTerms, text.font, text.size) {
    result <- c();
    for (j in 1:length(allTerms[[1]][[1]])) {
      for (i in 1:length(allTerms)) {
       if (allTerms[[i]][[1]][j] != "")  {
         specs <- NULL;
         specs$width <- strwidth(allTerms[[i]][[1]][j], font=text.size, units='in', family=text.font);
         specs$height <- strheight(allTerms[[i]][[1]][j], font=text.size, units='in', family=text.font);
         result <- c(result, list(specs));
         break;
       }
      }
    }
    
    return(result);
  }
  
  ComputePointCoordinates <- function(plot.data, colours) {
    # Compute the coordinates of the points
    result <- NULL;
    for (i in 1:nrow(plot.data)) {
      counter <- ncol(plot.data)-1;
      for (j in 2:ncol(plot.data)) {
        result <- rbind(result, data.frame(y = counter, x = plot.data[i,j], colour=colours[i], group = plot.data[i,1]))
        counter <- counter - 1;
      }
    }
    return(result);
  }
  
  GenerateLayoutMatrix <- function(plot.data) {
    numberColumns <- ncol(plot.data) - 1;
    result <- c(1, seq(4,numberColumns+3));
    result <- c(result, c(2, rep(numberColumns + 4, numberColumns)));
    result <- c(result, c(3, seq(numberColumns + 5, 2 * numberColumns + 4)));
    result <- matrix(result, nrow=numberColumns + 1);
    return(result);
  }
  
  GenerateHelpLineCoordinates <- function(maximumX, maximumY) {
    result <- NULL;
    
    for (i in 1:maximumY) {
      result <- rbind(result, data.frame(x=-maximumX, y=i, xend=maximumX, yend=i))
    }
    
    return(result);
  }
  
GenerateVerticalBarCoordinates <- function(xmin, xmax, ymin, ymax) {
  result <- NULL;
  
  for (i in seq(ymin, ymax, by=2)) {
    result <- rbind(result, data.frame(xmin=xmin, xmax=xmax, ymin=i - 0.5, ymax=i + 0.5))
  }
  
  return(result);
}

GenerateTexFile <- function(filePath, pathToOutputFile, allTerms) {
  content <- c(
    "\\documentclass{standalone}",
    "",
    "\\usepackage{tikz}",
    "\\usepackage{graphicx}",
    "\\usetikzlibrary{positioning, calc}",
    "", 
    "\\begin{document}",
    "\t\\newcommand{\\picWidth}{100px}",
    "\t\\newcommand{\\picHeight}{375px}",
    "\t\\newcommand{\\topOffset}{-0.57}",
    "\t\\newcommand{\\leftOffset}{0.12}",
    "\t\\newcommand{\\plotWidth}{3.21}",
    "\t\\newcommand{\\plotHeight}{11.43}",
    "\t\\newcommand{\\margin}{0.13}",
    "",
    "\t\\newcommand{\\textsize}{\\tiny}",
    "",
    "\t\\begin{tikzpicture}[every node/.append style={text=black, font=\\textsize}]",
    paste("\t\t\\node[inner sep=0, anchor=north west] (centralImage) at (0,0) {\\includegraphics[width=\\picWidth, height=\\picHeight]{", pathToOutputFile,"}};", sep ="")
  );
  
  for (i in 1:length(allTerms)) {
    maxLength <- length(allTerms[[i]]$label);
    
    if (i == 1) {
      nodeProp <- "anchor=east, align=right";
      xCoord <- "\\leftOffset - \\margin";
    } else {
      nodeProp <- "anchor=west, align=left";  
      xCoord <- "\\leftOffset + \\plotWidth + \\margin"
    }
    
    yCoord <- paste("\\topOffset - \\plotHeight / ", maxLength * 2, sep="");
    
    for (j in 1:maxLength) {
      multiplicationFactor <- 1 + 2*(j-1);
      if (allTerms[[i]]$label[j] != "") {
        content <- c(content, c(
          paste("\t\t\\node[inner sep=0, ", nodeProp, "] at (", xCoord, ", ", yCoord, " * ", multiplicationFactor, ") {$", allTerms[[i]]$label[j], "$};", sep="")
        ));
      }
    }
  }
  
  
  content <- c(content, c(
    "\t\\end{tikzpicture}",
    "\\end{document}"
  ));
  
  fileConn <- file(filePath);
  
  writeLines(content, fileConn);
  
  close(fileConn);
}
  
  graphics.off();
  
  plots <- list();
  
  totalDistance <- 1;
  
  # CONVERSION OF THE DATA (if needed)
  allTerms <- NULL;
  allTerms <- ComputeStrings(plot.data);

  # Compute the size of the textes
  textSizes <- ComputeTextSizes(allTerms, text.font, text.size)

  # Create the plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=20) +
    theme(legend.position="none",
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  # Add the leftrightarrow with the plus and minus sign
  plusPos <- data.frame(x=5, y=0);
  leftArrowPos <- data.frame(x=0, y=0, xend=-4.5, yend=0);
  rightArrowPos <- data.frame(x=0, y=0, xend=4.5, yend=0);
  minusPos <- data.frame(x=-5, y=-0.7);
  anchorPos <- rbind(data.frame(x=5,y=5), data.frame(x=-5, y=-2));
  leftRightArrow <- ggplot() + theme_clear +
    geom_segment(data=leftArrowPos, mapping=aes(x=x, y=y, xend=xend, yend=yend), size=1, colour="black", arrow=arrow(length = unit(0.5, "cm"))) +
    geom_segment(data=rightArrowPos, mapping=aes(x=x, y=y, xend=xend, yend=yend), size=1, colour="black", arrow=arrow(length = unit(0.5, "cm"))) +
    geom_text(data=plusPos, parse=TRUE, mapping=aes(x=x, y=y, label="'' + ''"), size=8, colour="green") + 
    geom_text(data=minusPos, parse=TRUE, mapping=aes(x=x, y=y, label="'' - ''"), size=8, colour="indianred1") +
    geom_point(data=anchorPos, mapping=aes(x=x,y=y), alpha=0, colour="white");
  plots <- c(plots, list(leftRightArrow))
  
  # Add the text and the plot
  
  # Retrieve line and point data
  lineData <- ComputePointCoordinates(plot.data, colours);
  eqZero <- lineData[lineData[,2]==0,];
  nonZero <- lineData[lineData[,2]!=0,];
  
  maximumX <- max(lineData[,1]);
  maximumY <- max(abs(lineData[,2]));
  maxLine <- rbind(data.frame(x=0.5, y=maximumY), data.frame(x=maximumX + 0.5, y=maximumY));
  maxLabel <- data.frame(x=2, y=maximumY, label="+");
  
  midLine <- rbind(data.frame(x=0.5, y=0), data.frame(x=maximumX + 0.5, y=0));
  midLabel <- data.frame(x=2, y=0, label="0");
  
  minLine <- rbind(data.frame(x=0.5, y=-maximumY), data.frame(x=maximumX + 0.5, y=-maximumY));
  minLabel <- data.frame(x=2, y=-maximumY, label="-");
  
  # Retrieve rectangle data
  negRect <- data.frame(xmin=-1, xmax=0, ymin=0.5, ymax=maximumX + 0.5);
  posRect <- data.frame(xmin=0, xmax=1, ymin=0.5, ymax=maximumX + 0.5);
  
  # Help lines on the y-axis
  #helpLineCoords <- GenerateHelpLineCoordinates(maximumY, maximumX);
  
  # Help bars on the y-axis
  helpBarCoords <- GenerateVerticalBarCoordinates(-1, 1, 1, maximumX);

  linePlot <- ggplot(data=lineData) + theme_nothing();
  linePlot <- linePlot + 
    #geom_segment(data=helpLineCoords, mapping=aes(x=x, y=y, xend=xend, yend=yend), colour="gray") + # add help lines in the background
    # Add the green and red backgroud colour
    geom_rect(data=negRect, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="indianred1", alpha=0.25) +
    geom_rect(data=posRect, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="lightgreen", alpha=0.25) +
    # Add the vertical bars
    geom_rect(data=helpBarCoords, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="gray", alpha=0.4) +
    # Maximim, middle and minimum line and the according labels
    geom_line(data=maxLine, mapping=aes(y=x,x=y), linetype="dashed", colour="black") +
    geom_line(data=midLine, mapping=aes(y=x,x=y), linetype="dashed", colour="black") +
    geom_line(data=minLine, mapping=aes(y=x,x=y), linetype="dashed", colour="black") +
    
    geom_path(data=lineData, mapping=aes(x=x,y=y, colour=colour, group = group), size=2) +
    
    geom_point(data=eqZero,aes(x=x,y=y, colour=colour, group = group), shape=21, fill="white", size=group.point.size) +
    geom_point(data=nonZero,aes(x=x,y=y, colour=colour, group=group), size=group.point.size) +
    scale_colour_manual(labels=plot.data[,1], values=c(colours[1], colours[2])) #+
    #theme(plot.margin = unit(c(1,15,1,15), "lines")); # Make room for the grobs
  
  # Include the legend
  linePlot  <- linePlot + labs(colour=legend.title,size=legend.text.size) +
    theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20,
                                                                                    family = text.font)) +
    theme(legend.text = element_text(size = legend.text.size), legend.position="bottom") +
    theme(legend.box.background = element_rect(), legend.box.margin = margin(1,1,1,1)) + # add the box around the legend
    theme(legend.key.height=unit(2,"line")) +
    theme(text=element_text(family=text.font)) +
    labs(x=NULL, y=NULL)
  
  # # Add the text on the left and the right side, respectively
  # counter <- length(allTerms[[1]]$label);
  # 
  # for (j in 1:length(allTerms[[1]]$label)) {
  #   tmpLabel <- allTerms[[1]]$label[j];
  #   xCoord <- maximumY + 0.03
  # 
  #   xCoord <- -xCoord;
  #   
  #   yCoord <- counter;
  #   counter <- counter - 1;
  #   
  #   linePlot <- linePlot + 
  #     annotation_custom(
  #       grob = textGrob(label = allTerms[[1]]$label[j], hjust = 1, gp = gpar(col="black", fontsize=text.size)),
  #       ymin = yCoord,      # Vertical position of the textGrob
  #       ymax = yCoord,
  #       xmin = xCoord,         # Note: The grobs are positioned outside the plot area
  #       xmax = xCoord)
  # }
  
  
  # counter <- length(allTerms[[2]]$label);
  # for (j in 1:length(allTerms[[2]]$label)) {
  #   tmpLabel <- allTerms[[2]]$label[j];
  #   xCoord <- maximumY + 0.03;
  #   
  #   yCoord <- counter;
  #   counter <- counter - 1;
  #   
  #   linePlot <- linePlot + 
  #     annotation_custom(
  #       grob = textGrob(label = allTerms[[2]]$label[j], hjust = 0, gp = gpar(col="black", fontsize=text.size)),
  #       ymin = yCoord,      # Vertical position of the textGrob
  #       ymax = yCoord,
  #       xmin = xCoord,         # Note: The grobs are positioned outside the plot area
  #       xmax = xCoord)
  #   
  # }
  # 
  # # Code to override clipping
  # gt <- ggplot_gtable(ggplot_build(linePlot))
  # gt$layout$clip[gt$layout$name == "panel"] <- "off"
  
  #plots <- c(plots, list(gt));
  
  #p <- grid.arrange(leftRightArrow, gt, ncol=1, heights=c(2/20,18/20))
  #ggsave("TextPlot.pdf", height=8.5, width=11, p);
  #ggsave("TextPlot_1.pdf", height=0.7, width=4, leftRightArrow)
  
  ggsave("TextPlot_1.pdf", height=15, width=4, linePlot)
  GenerateTexFile("TextPlot.tex", "TextPlot_1.pdf", allTerms)
  
  dev.off();
  graphics.off();
}