#' common.R
#' This script contains functions that are used by multiple scripts.
#' @author Christian Kaltenecker
#' @export ggtext

breakLine <- function(line, minimumLengthToSplit=15, doRecursiveCall=TRUE) {
  #Breaks a line at the given minimum length if a certain symbol ('·') is parsed
  #Args:
  #   line: the line to split
  #   minimumLengthToSplit: the minimum length to split the line
  #   doRecursiveCall: a flag if recursive calls with smaller minimum lengths should be triggered
  result <- "";
  i <- minimumLengthToSplit;
  found <- FALSE;
  while (i <= nchar(line) && !found) {
    character <- substring(line, i, i);
    if (character == '·') {
      left <- substring(line,0,i-1);
      right <- breakLine(substring(line,i,nchar(line)), minimumLengthToSplit,FALSE);
      result <- paste(left, right, sep="\n");
      found <- TRUE;
    }
    i <- i + 1;
  }
  if (!found) {
    if (minimumLengthToSplit != 5 && doRecursiveCall) {
      return(breakLine(line,5,doRecursiveCall = FALSE));
    } else {
      return(line);
    }
  }
  
  return(result);
}

breakText <- function(text, minimumLengthToSplit=15) {
  #Breaks the given text at the minimum length if a certain symbole ('·') is parsed
  #Args:
  #   text: the text to break
  #   minimumLengthToSplit: the minimum length for the text to split
  result <- c();
  for (i in 1:length(axis.labels)) {
    stringLength <- nchar(text[i]);
    if (stringLength <= minimumLengthToSplit) {
      result <- c(result, text[i]);
    } else {
      result <- c(result, breakLine(text[i], minimumLengthToSplit));
    }
  }
  return(result);
}