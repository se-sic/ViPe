#' common.R
#' This script contains functions that are used by multiple scripts.
#' @author Christian Kaltenecker
#' @export ggtext

breakLine <- function(line, rightAlign=FALSE, doRecursiveCall=FALSE) {
  #Breaks a line at the given minimum length if a certain symbol ('·') is parsed
  #Args:
  #   line: the line to split
  #   minimumLengthToSplit: the minimum length to split the line
  #   doRecursiveCall: a flag if recursive calls with smaller minimum lengths should be triggered
  result <- "";
  i <- 1;
  found <- FALSE;
  while (i <= nchar(line) && !found) {
    character <- substring(line, i, i);
    if (character == '*') {
      left <- substring(line,0,i-1);
      right <- breakLine(substring(line,i+1,nchar(line)), rightAlign, TRUE);
      if (doRecursiveCall) {
        if (rightAlign) {
          result <- paste(paste(left, " ×", sep=""), right, sep="\n");
        } else {
          result <- paste(left, paste("× ", right, sep=""), sep="\n");
        }
      } else {
        if (rightAlign) {
          result <- paste(paste(left, " ×", sep=""), "\n", right, "   ", sep="");
        } else {
          result <- paste("   ", left, "\n", paste("× ", right, sep=""), sep="");
        }
      }
      found <- TRUE;
    }
    i <- i + 1;
  }
  if (result == "") {
    return(line);
  }
  return(result);
}

getExponent <- function(exponent) {
  result <- "";
  divFactor <- 10;
  repeat {
    
    rest <- exponent %% divFactor;
    
    toAdd <- switch (rest+1,
                    "°",
                    "¹",
                    "²",
                    "³",
                    "4",
                    "5",
                    "6",
                    "7",
                    "8",
                    "9"
                    )
    result <- paste(toAdd, result, sep="")
    
    if (floor(exponent / divFactor) == 0) {
      break;
    } else {
      exponent <- exponent / divFactor;
    }
  }
  
  return(result);
}

prepareLine <- function(line) {
  #
  
  strings <- strsplit(line,"\\*");
  counter <- NULL;
  
  for (i in 1:length(strings[[1]])) {
    name <- strings[[1]][i];
    if (name %in% names(counter)) {
      counter[[name]] <- counter[[name]] + 1;
    } else {
      counter[[name]] <- 1;
    }
  }
  
  result <- "";
  
  for (i in 1:length(names(counter))) {
    name <- names(counter)[i];
    result <- paste(result, name, sep="");
    if (counter[[name]] > 1) {
      #result <- paste(result, "^", counter[[name]] , sep="");
      result <- paste(result, getExponent(counter[[name]]), sep="");
    }
    if (i < length(names(counter))) {
      result <- paste(result, "*", sep="");
    }
  }
  
  return(result);
}

breakText <- function(text, rightAlign=FALSE) {
  #Breaks the given text at the minimum length if a certain symbole ('·') is parsed
  #Args:
  #   text: the text to break
  #   minimumLengthToSplit: the minimum length for the text to split
  result <- c();
  for (i in 1:length(text)) {
    result <- c(result, breakLine(prepareLine(text[i]), rightAlign=rightAlign));
  }
  return(result);
}