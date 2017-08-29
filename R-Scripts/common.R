#' common.R
#' This script contains functions that are used by multiple scripts.
#' @author Christian Kaltenecker
#' @export ggtext

breakLine <- function(line, doRecursiveCall=TRUE) {
  #Breaks a line at the given minimum length if a certain symbol ('·') is parsed
  #Args:
  #   line: the line to split
  #   minimumLengthToSplit: the minimum length to split the line
  #   doRecursiveCall: a flag if recursive calls with smaller minimum lengths should be triggered
  result <- line;
  i <- 1;
  while (i <= nchar(line)) {
    character <- substring(line, i, i);
    if (character == '·') {
      left <- substring(line,0,i-1);
      right <- breakLine(substring(line,i+1,nchar(line)));
      result <- paste(left, paste("×", right, sep=""), sep="\n");
    }
    i <- i + 1;
  }
  
  return(result);
}

breakText <- function(text) {
  #Breaks the given text at the minimum length if a certain symbole ('·') is parsed
  #Args:
  #   text: the text to break
  #   minimumLengthToSplit: the minimum length for the text to split
  result <- c();
  for (i in 1:length(text)) {
    stringLength <- nchar(text[i]);
    result <- c(result, breakLine(text[i]));
  }
  return(result);
}