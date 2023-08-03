#' Zap newlines from a string
#' 
#' Collapse a character vector into a single value, and replace newlines with
#' spaces (and replace double spaces with single spaces).
#'
#' @param x The character value or vector
#'
#' @return The resulting character vector
#' @export
#'
#' @examples metabefor::zapNewLines(
#'   "So here we start,
#' and here we end"
#' );
zapNewlines <- function(x) {
  
  x <- paste0(x, collapse=" ");
  x <- gsub("  ", " ", x, fixed = TRUE);
  
  return(
    gsub(
      " ?\n ?",
      " ",
      x
    )
  );
  
}