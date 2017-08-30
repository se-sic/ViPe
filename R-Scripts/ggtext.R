#' ggtext
#' @author Christian Kaltenecker
#' @export ggtext

ggtext <- function(plot.data,
                   text.font = "Circular Air Light",
                   text.size = 4,
                   curve.colour = "blue",
                   group.point.size = 6,
                   label.size = 4,
                   colours = c("#FF5A5F", "#FFB400")) 
{
  # PREPARATION
  
  # This library is based on the ggplot-library
  library(ggplot2)
  library(gridExtra)
  library(grid)
  
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
      counter <- 1;
      for (j in 2:ncol(plot.data)) {
        result <- rbind(result, data.frame(x = counter, y = plot.data[i,j], group = i, colour=colours[i]))
        counter <- counter + 1;
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
  
  graphics.off();
  
  plots <- list();
  
  totalDistance <- 1;
  
  # CONVERSION OF THE DATA (if needed)
  allTerms <- NULL;
  allTerms <- ComputeStrings(plot.data);
  
  # Compute the size of the textes
  textSizes <- ComputeTextSizes(allTerms, text.font, text.size)

  # PLOTING
  
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
  
  # Add an empty plot
  plots <- c(plots, list(ggplot() + theme_clear))
  
  # Add the leftrightarrow with the plus and minus sign
  plusPos <- data.frame(x=-5, y=0);
  leftArrowPos <- data.frame(x=0, y=0, xend=-4, yend=0);
  rightArrowPos <- data.frame(x=0, y=0, xend=4, yend=0);
  minusPos <- data.frame(x=5, y=0);
  anchorPos <- rbind(data.frame(x=5,y=5), data.frame(x=-5, y=-2));
  leftRightArrow <- ggplot() + theme_clear +
    geom_segment(data=leftArrowPos, mapping=aes(x=x, y=y, xend=xend, yend=yend), size=1, colour="black", arrow=arrow(length = unit(0.5, "cm"))) +
    geom_segment(data=rightArrowPos, mapping=aes(x=x, y=y, xend=xend, yend=yend), size=1, colour="black", arrow=arrow(length = unit(0.5, "cm"))) +
    geom_text(data=plusPos, parse=TRUE, mapping=aes(x=x, y=y, label="'' + ''"), size=8, colour="green") + 
    geom_text(data=minusPos, parse=TRUE, mapping=aes(x=x, y=y, label="'' - ''"), size=8, colour="indianred1") +
    geom_point(data=anchorPos, mapping=aes(x=x,y=y), alpha=0, colour="white");
  plots <- c(plots, list(leftRightArrow))
  
  # Add an empty plot
  plots <- c(plots, list(ggplot() + theme_clear))
  
  # Add the text and the plot
  for (i in 1:length(allTerms)) {
    
    if (i > 1) {
      # Retrieve line and point data
      lineData <- ComputePointCoordinates(plot.data, colours);
      eqZero <- lineData[lineData[,2]==0,];
      nonZero <- lineData[lineData[,2]!=0,];
      
      maximumX <- max(lineData[,1]);
      maximumY <- max(abs(lineData[,2]));
      maxLine <- rbind(data.frame(x=1, y=maximumY), data.frame(x=maximumX, y=maximumY));
      maxLabel <- data.frame(x=2, y=maximumY, label="+");
      
      midLine <- rbind(data.frame(x=1, y=0), data.frame(x=maximumX, y=0));
      midLabel <- data.frame(x=2, y=0, label="0");
      
      minLine <- rbind(data.frame(x=1, y=-maximumY), data.frame(x=maximumX, y=-maximumY));
      minLabel <- data.frame(x=2, y=-maximumY, label="-");
      
      # Add a plot for the labels
      # labelPlot <- ggplot() + theme_clear;
      # labelPlot <- labelPlot + 
      #   geom_text(data = maxLabel, mapping=aes(x=x, y=y, label=label), size=label.size, colour="black") +
      #   geom_text(data = midLabel, mapping=aes(x=x, y=y, label=label), size=label.size, colour="black") +
      #   geom_text(data = minLabel, mapping=aes(x=x, y=y, label=label), size=label.size, colour="black");
      # plots <- c(plots, list(labelPlot));
      
      linePlot <- ggplot() + theme_clear;
      linePlot <- linePlot + 
        # Maximim, middle and minimum line and the according labels
        geom_line(data=maxLine, mapping=aes(x=x,y=y), linetype="dashed", colour="lightgreen") +
        geom_line(data=midLine, mapping=aes(x=x,y=y), linetype="dashed", colour="gray") +
        geom_line(data=minLine, mapping=aes(x=x,y=y), linetype="dashed", colour="indianred1") +
        
        geom_line(data=lineData, mapping=aes(x=x,y=y,group=group, colour=colour), size=2) +
        
        geom_point(data=eqZero,aes(x=x,y=y,group=group, colour=colour), shape=21, fill="white", size=group.point.size) +
        geom_point(data=nonZero,aes(x=x,y=y,group=group, colour=colour), size=group.point.size) +
        scale_colour_manual(values=c(colours[1], colours[2])) +
        # Rotate the plot
        coord_flip() +
        scale_x_reverse() + 
        scale_y_reverse();
      
      plots <- c(plots, list(linePlot));
    }
    
    # Add one empty plot
    #plots <- c(plots, list(ggplot() + theme_clear));
    
    for (j in 1:length(allTerms[[i]]$label)) {
      tmpLabel <- allTerms[[i]]$label[j];
      dataframe <- data.frame(x=0 , y=0, label=tmpLabel)
      tmpPlot <- ggplot() + theme_clear;
      tmpPlot <- tmpPlot + 
        geom_text(data=dataframe, mapping=aes(x=x,y=y,label=label), family = text.font, colour="black", size = text.size, hjust=2-i); #, angle=-90 
      plots <- c(plots, list(tmpPlot));
    }
    
  }
  
  numberTerms <- ncol(plot.data) - 1;
  rationPlots_x <- c(3/10, 6/10, 3/10); #c(maxWidth, maxWidth)#, distanceBetweenText, maxWidth);
  rationPlots_y <- c(2/30, rep(1/numberTerms * 28/30, numberTerms));
  layoutMatrix <- GenerateLayoutMatrix(plot.data);
  browser();
  do.call(grid.arrange, c(plots,list(widths =rationPlots_x, heights = rationPlots_y, layout_matrix = layoutMatrix)))#, nrow=nrow(plot.data) + 1, ncol=numberTerms)))
  
  browser();
  
  dev.off();
  graphics.off();
  #return(base);
}