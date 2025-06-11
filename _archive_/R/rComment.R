rComment <- function(x,
                     length = 78,
                     commentPrefix = "### ") {
  
  maxLength <-
    length - nchar(commentPrefix);
  
  x <- unlist(strsplit(x, "\n", fixed=TRUE));
  
  x <- unlist(trimws(x, which="right"));
  
  x <- unlist(strwrap(x, width = maxLength));
  
  x <-
    paste0(
      commentPrefix,
      x
    );
  
  return(x);
  
}
