#' In a list of vectors, pad short vectors
#'
#' @param x The list of vectors
#' @param padWith The element to pad with
#'
#' @return The list with the padded vectors
#' @export
#'
#' @examples metabefor::padVectors(
#'   list(
#'     c(1, 2, 3),
#'     4,
#'     c(5, 6)
#'   )
#' );
padVectors <- function(x,
                       padWith = NA,
                       fieldname_regex_neverExplode = NULL) {
  
  if (!is.null(fieldname_regex_neverExplode)) {
    vectorsToFlatten <-
      grep(fieldname_regex_neverExplode, names(x), value=TRUE);
    x[vectorsToFlatten] <-
      flattenNodeValues(x[vectorsToFlatten]);
  }
  
  vectorLengths <-
    unlist(lapply(x, length));
  
  longerVectors <- which(vectorLengths>1);
  
  longVectorLengths <- unique(vectorLengths[longerVectors]);
  
  if (length(unique(longVectorLengths)) > 1) {
    
    maxVectorLength <- max(vectorLengths);
    
    vectorsToGrow <- which(vectorLengths > 1 &
                             vectorLengths < maxVectorLength);
    
    elementsToAdd <-
      lapply(
        maxVectorLength - vectorLengths,
        rep,
        x = NA
      );

    for (i in seq_along(vectorsToGrow)) {
      
      x[[vectorsToGrow[i]]] <-
        c(x[[vectorsToGrow[i]]],
          elementsToAdd[[vectorsToGrow[i]]]);
      
    }
  }
  
  return(x);
    
}
