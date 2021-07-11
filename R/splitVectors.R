#' In a list of vectors, split vectors to multiple list elements
#'
#' @param x The list of vectors
#' @param sep The separator to use
#'
#' @return The list with the split vectors
#' @export
#'
#' @examples metabefor::splitVectors(
#'   list(
#'     c(1, 2, 3),
#'     4,
#'     c(5, 6)
#'   )
#' );
splitVectors <- function(x,
                         sep = "_",
                         fieldname_regex_alwaysFlatten = NULL) {
  
  if (!is.null(fieldname_regex_alwaysFlatten)) {
    vectorsToFlatten <-
      grep(fieldname_regex_alwaysFlatten, names(x), value=TRUE);
    x[vectorsToFlatten] <-
      flattenNodeValues(x[vectorsToFlatten]);
  }
  
  res <- unlist(x);
  
  newNames <- names(res);

  newerNames <-
    gsub(
      paste0("(",
             paste0(names(x), collapse="|"),
             ")"),
      paste0("\\1", sep),
      newNames
    );
  
  ### For elements that weren't vectors, 
  names(res) <-
    ifelse(
      grepl(
        paste0(
          "^", names(x), "$",
          collapse="|"
        ),
        newNames
      ),
      newNames,
      newerNames
    );

  return(res);
    
}
